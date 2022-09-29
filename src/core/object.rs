use std::f32::consts::FRAC_PI_4;

use bevy::app::PluginGroupBuilder;
use bevy::prelude::*;
use bevy::reflect::FromReflect;
use bevy_mod_outline::{Outline, OutlineBundle};
use bevy_mod_raycast::Ray3d;
use bevy_mod_raycast::{RayCastMesh, RayCastSource};
use bevy_rapier3d::prelude::*;
use bevy_renet::renet::RenetClient;
use bevy_scene_hook::SceneHook;
use derive_more::From;
use iyes_loopless::prelude::*;
use leafwing_input_manager::prelude::ActionState;
use serde::{Deserialize, Serialize};
use tap::TapOptional;

use super::network::network_event::client_event::EventReceived;
use super::{
    asset_metadata,
    city::City,
    control_action::ControlAction,
    game_state::GameState,
    game_world::{parent_sync::ParentSync, GameEntity},
    network::{network_event::client_event::ClientEventPlugin, SERVER_ID},
    preview::PreviewCamera,
};

pub(super) struct ObjectPlugins;

impl PluginGroup for ObjectPlugins {
    fn build(&mut self, group: &mut PluginGroupBuilder) {
        group
            .add(ClientEventPlugin::<ObjectPicked>::default())
            .add(ClientEventPlugin::<ObjectMoved>::default())
            .add(ClientEventPlugin::<ObjectSpawned>::default())
            .add(ObjectPlugin);
    }
}

struct ObjectPlugin;

impl Plugin for ObjectPlugin {
    fn build(&self, app: &mut App) {
        app.register_type::<Picked>()
            .register_type::<ObjectPath>()
            .add_system(Self::spawn_scene_system.run_in_state(GameState::City))
            .add_system(Self::cursor_object_spawn_system.run_in_state(GameState::City))
            .add_system(Self::movement_system.run_in_state(GameState::City))
            .add_system(Self::pick_confirmation_system.run_unless_resource_exists::<RenetClient>())
            .add_system(
                Self::confirm_system
                    .run_in_state(GameState::City)
                    .run_if(is_placement_confirmed),
            )
            .add_system(
                Self::cancel_system
                    .run_in_state(GameState::City)
                    .run_if(is_placement_canceled),
            )
            .add_system(
                Self::ray_system
                    .chain(Self::picking_system)
                    .chain(Self::outline_system)
                    .run_in_state(GameState::City)
                    .run_if(is_moving_object),
            )
            .add_system(Self::apply_movement_system.run_unless_resource_exists::<RenetClient>())
            .add_system_to_stage(
                CoreStage::PostUpdate,
                Self::despawn_cursor_object_system.run_in_state(GameState::City),
            )
            .add_system(Self::spawn_object_system.run_unless_resource_exists::<RenetClient>());
    }
}

const ROTATION_STEP: f32 = -FRAC_PI_4;

impl ObjectPlugin {
    fn spawn_scene_system(
        mut commands: Commands,
        asset_server: Res<AssetServer>,
        spawned_objects: Query<(Entity, &ObjectPath), Added<ObjectPath>>,
    ) {
        for (entity, object_path) in &spawned_objects {
            let scene_path = asset_metadata::scene_path(&object_path.0);
            let scene_handle: Handle<Scene> = asset_server.load(&scene_path);

            commands
                .entity(entity)
                .insert(scene_handle)
                .insert(GlobalTransform::default())
                .insert(SceneHook::new(|entity, commands| {
                    if entity.contains::<Handle<Mesh>>() {
                        commands
                            .insert_bundle(OutlineBundle {
                                outline: Outline {
                                    visible: false,
                                    colour: Color::rgba(1.0, 1.0, 1.0, 0.3),
                                    width: 2.0,
                                },
                                ..Default::default()
                            })
                            .insert(RayCastMesh::<ObjectPath>::default());
                    }
                }))
                .insert_bundle(VisibilityBundle::default());
        }
    }

    fn ray_system(
        ray_sources: Query<&RayCastSource<ObjectPath>>,
        parents: Query<(&Parent, Option<&ObjectPath>)>,
    ) -> Option<Entity> {
        for source in &ray_sources {
            if let Some((child_entity, _)) = source.intersect_top() {
                let entity = find_parent_object(child_entity, &parents)
                    .expect("object entity should have a parent");
                return Some(entity);
            }
        }

        None
    }

    fn picking_system(
        In(entity): In<Option<Entity>>,
        mut pick_events: EventWriter<ObjectPicked>,
        action_state: Res<ActionState<ControlAction>>,
    ) -> Option<Entity> {
        if let Some(entity) = entity {
            if action_state.just_pressed(ControlAction::Confirm) {
                pick_events.send(ObjectPicked(entity));
                None
            } else {
                Some(entity)
            }
        } else {
            None
        }
    }

    fn outline_system(
        In(entity): In<Option<Entity>>,
        mut previous_entity: Local<Option<Entity>>,
        mut outlines: Query<&mut Outline>,
        children: Query<&Children>,
    ) {
        if *previous_entity == entity {
            return;
        }

        if let Some(entity) = entity {
            set_outline_recursive(entity, true, &mut outlines, &children);
        }

        if let Some(entity) = *previous_entity {
            set_outline_recursive(entity, false, &mut outlines, &children);
        }

        *previous_entity = entity;
    }

    fn pick_confirmation_system(
        mut commands: Commands,
        mut pick_events: EventReader<EventReceived<ObjectPicked>>,
    ) {
        for EventReceived { client_id, event } in pick_events.iter().copied() {
            commands.entity(event.0).insert(Picked(client_id));
        }
    }

    fn cursor_object_spawn_system(
        mut commands: Commands,
        client: Option<Res<RenetClient>>,
        mut picked_objects: Query<
            (
                Entity,
                &Parent,
                &Picked,
                &Handle<Scene>,
                &mut Visibility,
                &Transform,
            ),
            Added<Picked>,
        >,
    ) {
        let client_id = client.map(|client| client.client_id()).unwrap_or(SERVER_ID);
        for (entity, parent, picked, scene_handle, mut visibility, transform) in &mut picked_objects
        {
            if picked.0 == client_id {
                visibility.is_visible = false;
                commands.entity(parent.get()).with_children(|parent| {
                    parent
                        .spawn_bundle(SceneBundle {
                            scene: scene_handle.clone(),
                            transform: *transform,
                            ..Default::default()
                        })
                        .insert(CursorObject::Moving(entity));
                });
            }
        }
    }

    fn movement_system(
        windows: Res<Windows>,
        rapier_ctx: Res<RapierContext>,
        action_state: Res<ActionState<ControlAction>>,
        camera: Query<(&GlobalTransform, &Camera), Without<PreviewCamera>>,
        mut cursor_objects: Query<&mut Transform, With<CursorObject>>,
    ) {
        if let Ok(mut transform) = cursor_objects.get_single_mut() {
            if let Some(cursor_pos) = windows
                .get_primary()
                .and_then(|window| window.cursor_position())
            {
                let (&camera_transform, camera) = camera.single();
                let ray = Ray3d::from_screenspace(cursor_pos, camera, &camera_transform)
                    .expect("ray should be created from screen coordinates");

                let toi = rapier_ctx
                    .cast_ray(
                        ray.origin(),
                        ray.direction(),
                        f32::MAX,
                        false,
                        QueryFilter::new(),
                    )
                    .map(|(_, toi)| toi)
                    .unwrap_or_default();

                transform.translation = ray.origin() + ray.direction() * toi;
                if action_state.just_pressed(ControlAction::RotateObject) {
                    transform.rotate_y(ROTATION_STEP);
                }
            }
        }
    }

    fn cancel_system(mut commands: Commands, moving_objects: Query<Entity, With<CursorObject>>) {
        if let Ok(entity) = moving_objects.get_single() {
            commands.entity(entity).despawn();
        }
    }

    fn confirm_system(
        mut move_events: EventWriter<ObjectMoved>,
        mut spawn_events: EventWriter<ObjectSpawned>,
        cursor_objects: Query<(&Transform, &CursorObject)>,
    ) {
        if let Ok((transform, cursor_object)) = cursor_objects.get_single() {
            match cursor_object {
                CursorObject::Moving(_) => move_events.send(ObjectMoved {
                    translation: transform.translation,
                    rotation: transform.rotation,
                }),
                CursorObject::Spawning(object_path) => spawn_events.send(ObjectSpawned {
                    object_path: object_path.clone(),
                    translation: transform.translation,
                    rotation: transform.rotation,
                }),
            }
        }
    }

    fn apply_movement_system(
        mut commands: Commands,
        mut move_events: EventReader<EventReceived<ObjectMoved>>,
        mut picked_objects: Query<(Entity, &mut Transform, &Picked)>,
    ) {
        for EventReceived { client_id, event } in move_events.iter().copied() {
            if let Some((entity, mut transform, ..)) = picked_objects
                .iter_mut()
                .find(|(.., picked)| picked.0 == client_id)
                .tap_none(|| error!("unable to map received entity"))
            {
                transform.translation = event.translation;
                commands.entity(entity).remove::<Picked>();
            }
        }
    }

    fn despawn_cursor_object_system(
        mut commands: Commands,
        pick_removals: RemovedComponents<Picked>,
        cursor_objects: Query<(Entity, &CursorObject)>,
        mut visibility: Query<&mut Visibility>,
    ) {
        if let Ok((cursor_entity, cursor_object)) = cursor_objects.get_single() {
            if let CursorObject::Moving(moving_entity) = *cursor_object {
                if let Some(object_entity) = pick_removals
                    .iter()
                    .find(|&object_entity| object_entity == moving_entity)
                {
                    commands.entity(cursor_entity).despawn_recursive();
                    let mut visibility = visibility
                        .get_mut(object_entity)
                        .expect("object should have visibility");
                    visibility.is_visible = true;
                }
            }
        }
    }

    fn spawn_object_system(
        mut commands: Commands,
        mut spawn_events: EventReader<EventReceived<ObjectSpawned>>,
        visible_cities: Query<Entity, (With<City>, With<Visibility>)>,
    ) {
        for event in spawn_events.iter().map(|event| event.event.clone()) {
            commands
                .spawn_bundle(ObjectBundle {
                    path: ObjectPath(event.object_path),
                    transform: Transform::default()
                        .with_translation(event.translation)
                        .with_rotation(event.rotation),
                    ..Default::default()
                })
                .insert(ParentSync(visible_cities.single()));
        }
    }
}

/// Iterates up the hierarchy until it finds a parent with an [`ObjectPath`] component if exists.
fn find_parent_object(
    entity: Entity,
    parents: &Query<(&Parent, Option<&ObjectPath>)>,
) -> Option<Entity> {
    let (parent, object_path) = parents.get(entity).unwrap();
    if object_path.is_some() {
        return Some(entity);
    }

    find_parent_object(parent.get(), parents)
}

fn set_outline_recursive(
    entity: Entity,
    visible: bool,
    outlines: &mut Query<&mut Outline>,
    children: &Query<&Children>,
) {
    if let Ok(mut outline) = outlines.get_mut(entity) {
        outline.visible = visible;
    }

    if let Ok(entity_children) = children.get(entity) {
        for &child in entity_children {
            set_outline_recursive(child, visible, outlines, children);
        }
    }
}

fn is_placement_canceled(action_state: Res<ActionState<ControlAction>>) -> bool {
    action_state.just_pressed(ControlAction::Cancel)
}

fn is_placement_confirmed(action_state: Res<ActionState<ControlAction>>) -> bool {
    action_state.just_pressed(ControlAction::Confirm)
}

fn is_moving_object(moving_objects: Query<(), With<CursorObject>>) -> bool {
    moving_objects.is_empty()
}

#[derive(Clone, Copy, Debug, Serialize, Deserialize)]
pub(super) struct ObjectPicked(pub(super) Entity);

#[derive(Clone, Copy, Debug, Serialize, Deserialize)]
pub(super) struct ObjectMoved {
    translation: Vec3,
    rotation: Quat,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub(super) struct ObjectSpawned {
    object_path: String,
    translation: Vec3,
    rotation: Quat,
}

#[derive(Component, Default, Reflect)]
#[reflect(Component)]
pub(super) struct Picked(u64);

/// Marks an entity as an object that should be moved with cursor to preview spawn position.
#[derive(Component)]
pub(crate) enum CursorObject {
    Moving(Entity),
    Spawning(String),
}

#[derive(Bundle, Default)]
pub(crate) struct ObjectBundle {
    pub(crate) path: ObjectPath,
    pub(crate) transform: Transform,
    pub(crate) game_entity: GameEntity,
}

/// Contains path to a an object metadata file.
#[derive(Clone, Component, Debug, Default, From, FromReflect, Reflect)]
#[reflect(Component)]
pub(crate) struct ObjectPath(String);

#[cfg(test)]
mod tests {
    use bevy::{asset::AssetPlugin, core::CorePlugin, ecs::system::SystemState};
    use bevy_mod_raycast::IntersectionData;

    use super::*;

    #[test]
    fn parent_search() {
        let mut world = World::new();
        let child_entity = world.spawn().id();
        let parent_entity = world
            .spawn()
            .insert(ObjectPath::default())
            .push_children(&[child_entity])
            .id();

        // Assign a parent, as an outline object is always expected to have a parent object.
        world.spawn().push_children(&[parent_entity]);

        let mut system_state: SystemState<Query<(&Parent, Option<&ObjectPath>)>> =
            SystemState::new(&mut world);

        let entity = find_parent_object(child_entity, &system_state.get(&world))
            .expect("object should have a parent");
        assert_eq!(entity, parent_entity);
    }

    #[test]
    fn recursive_outline() {
        let mut world = World::new();
        let child_entity1 = world.spawn().insert(Outline::default()).id();
        let child_entity2 = world
            .spawn()
            .insert(Outline::default())
            .push_children(&[child_entity1])
            .id();
        let root_entity = world
            .spawn()
            .insert(Outline::default())
            .push_children(&[child_entity2])
            .id();

        let mut system_state: SystemState<(Query<&mut Outline>, Query<&Children>)> =
            SystemState::new(&mut world);

        const VISIBLE: bool = false;
        let (mut outlines, children) = system_state.get_mut(&mut world);
        set_outline_recursive(root_entity, VISIBLE, &mut outlines, &children);

        assert_eq!(
            world.get::<Outline>(child_entity1).unwrap().visible,
            VISIBLE
        );
        assert_eq!(
            world.get::<Outline>(child_entity2).unwrap().visible,
            VISIBLE
        );
        assert_eq!(world.get::<Outline>(root_entity).unwrap().visible, VISIBLE);
    }

    #[test]
    fn spawning() {
        let mut app = App::new();
        app.add_plugin(TestMovingObjectPlugin);

        let object_entity = app.world.spawn().insert(ObjectPath(String::default())).id();

        app.update();

        let object_entity = app.world.entity(object_entity);
        assert!(object_entity.contains::<Handle<Scene>>());
        assert!(object_entity.contains::<GlobalTransform>());
        assert!(object_entity.contains::<Visibility>());
        assert!(object_entity.contains::<ComputedVisibility>());
    }

    #[test]
    fn confirmation() {
        let mut app = App::new();
        app.add_plugin(TestMovingObjectPlugin);

        let moving_entity = app.world.spawn().insert(CursorObject).id();
        let mut action_state = app.world.resource_mut::<ActionState<ControlAction>>();
        action_state.press(ControlAction::Confirm);

        app.update();

        assert!(!app.world.entity(moving_entity).contains::<CursorObject>());
    }

    #[test]
    fn cancellation() {
        let mut app = App::new();
        app.add_plugin(TestMovingObjectPlugin);

        let moving_entity = app.world.spawn().insert(CursorObject).id();
        let mut action_state = app.world.resource_mut::<ActionState<ControlAction>>();
        action_state.press(ControlAction::Cancel);

        app.update();

        assert!(app.world.get_entity(moving_entity).is_none());
    }

    #[test]
    fn hovering() {
        let mut app = App::new();
        app.add_plugin(CorePlugin)
            .add_plugin(TestMovingObjectPlugin);

        let outline_entity = app
            .world
            .spawn()
            .insert(Outline {
                visible: false,
                ..Default::default()
            })
            .insert(ObjectPath::default())
            .id();
        app.world.spawn().push_children(&[outline_entity]);

        let mut ray_source = RayCastSource::<ObjectPath>::default();
        ray_source.intersections_mut().push((
            outline_entity,
            IntersectionData::new(Vec3::default(), Vec3::default(), 0.0, None),
        ));
        let ray_entity = app.world.spawn().insert(ray_source).id();

        app.update();

        assert!(app.world.get::<Outline>(outline_entity).unwrap().visible);

        let next_outline_entity = app
            .world
            .spawn()
            .insert(Outline {
                visible: false,
                ..Default::default()
            })
            .insert(ObjectPath::default())
            .id();
        app.world.spawn().push_children(&[next_outline_entity]);
        let mut ray_source = app
            .world
            .get_mut::<RayCastSource<ObjectPath>>(ray_entity)
            .unwrap();
        ray_source.intersections_mut().clear();
        ray_source.intersections_mut().push((
            next_outline_entity,
            IntersectionData::new(Vec3::default(), Vec3::default(), 0.0, None),
        ));

        app.update();

        assert!(!app.world.get::<Outline>(outline_entity).unwrap().visible);
        assert!(
            app.world
                .get::<Outline>(next_outline_entity)
                .unwrap()
                .visible
        );
    }

    #[test]
    fn no_hovering() {
        let mut app = App::new();
        app.add_plugin(CorePlugin)
            .add_plugin(TestMovingObjectPlugin);

        let outline_entity = app
            .world
            .spawn()
            .insert(Outline {
                visible: false,
                ..Default::default()
            })
            .insert(ObjectPath::default())
            .id();

        app.world
            .spawn()
            .insert(RayCastSource::<ObjectPath>::default());

        app.update();

        let outline = app.world.get::<Outline>(outline_entity).unwrap();
        assert!(!outline.visible);
    }

    #[test]
    fn selection() {
        let mut app = App::new();
        app.add_plugin(CorePlugin)
            .add_plugin(TestMovingObjectPlugin);

        let outline_entity = app
            .world
            .spawn()
            .insert(Outline {
                visible: false,
                ..Default::default()
            })
            .insert(ObjectPath::default())
            .id();
        app.world.spawn().push_children(&[outline_entity]);

        let mut ray_source = RayCastSource::<ObjectPath>::default();
        ray_source.intersections_mut().push((
            outline_entity,
            IntersectionData::new(Vec3::default(), Vec3::default(), 0.0, None),
        ));
        app.world.spawn().insert(ray_source);

        app.world
            .resource_mut::<ActionState<ControlAction>>()
            .press(ControlAction::Confirm);

        app.update();

        let outline_entity = app.world.entity(outline_entity);
        assert!(!outline_entity.get::<Outline>().unwrap().visible);
        assert!(outline_entity.contains::<CursorObject>());
    }

    struct TestMovingObjectPlugin;

    impl Plugin for TestMovingObjectPlugin {
        fn build(&self, app: &mut App) {
            app.add_loopless_state(GameState::City)
                .init_resource::<RapierContext>()
                .init_resource::<Windows>()
                .init_resource::<ActionState<ControlAction>>()
                .add_plugin(AssetPlugin)
                .add_plugin(ObjectPlugin);
        }
    }
}
