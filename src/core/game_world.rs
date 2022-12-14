pub(crate) mod parent_sync;
pub(super) mod save_rules;

use std::fs;

use anyhow::{Context, Result};
use bevy::{
    ecs::archetype::ArchetypeId,
    prelude::*,
    reflect::TypeRegistry,
    scene::{serde::SceneDeserializer, DynamicEntity},
};
use iyes_loopless::prelude::*;
use ron::Deserializer;
use serde::de::DeserializeSeed;

use super::{error_message, game_paths::GamePaths, game_state::GameState};
use parent_sync::ParentSyncPlugin;
use save_rules::SaveRules;

#[derive(SystemLabel)]
pub(crate) enum GameWorldSystem {
    Saving,
    Loading,
}

pub(super) struct GameWorldPlugin;

impl Plugin for GameWorldPlugin {
    fn build(&self, app: &mut App) {
        app.add_plugin(ParentSyncPlugin)
            .register_type::<GameEntity>()
            .init_resource::<SaveRules>()
            .add_event::<GameSave>()
            .add_event::<GameLoad>()
            .add_system(Self::main_menu_transition_system.run_if_resource_removed::<GameWorld>())
            .add_system(
                Self::saving_system
                    .pipe(error_message::err_message_system)
                    .run_on_event::<GameSave>()
                    .label(GameWorldSystem::Saving),
            )
            .add_system(
                Self::loading_system
                    .pipe(error_message::err_message_system)
                    .run_on_event::<GameLoad>()
                    .label(GameWorldSystem::Loading),
            );
    }
}

impl GameWorldPlugin {
    fn main_menu_transition_system(mut commands: Commands) {
        commands.insert_resource(NextState(GameState::MainMenu));
    }

    /// Saves world to disk with the name from [`GameWorld`] resource.
    fn saving_system(
        world: &World,
        game_world: Res<GameWorld>,
        game_paths: Res<GamePaths>,
        registry: Res<AppTypeRegistry>,
        save_rules: Res<SaveRules>,
    ) -> Result<()> {
        let world_path = game_paths.world_path(&game_world.world_name);

        fs::create_dir_all(&game_paths.worlds)
            .with_context(|| format!("unable to create {world_path:?}"))?;

        let scene = save_to_scene(world, &registry, &save_rules);
        let bytes = scene
            .serialize_ron(&registry)
            .expect("game world should be serialized");

        fs::write(&world_path, bytes)
            .with_context(|| format!("unable to save game to {world_path:?}"))
    }

    /// Loads world from disk with the name from [`GameWorld`] resource.
    fn loading_system(
        mut commands: Commands,
        mut scene_spawner: ResMut<SceneSpawner>,
        mut scenes: ResMut<Assets<DynamicScene>>,
        game_world: Res<GameWorld>,
        game_paths: Res<GamePaths>,
        registry: Res<AppTypeRegistry>,
    ) -> Result<()> {
        let world_path = game_paths.world_path(&game_world.world_name);

        let bytes =
            fs::read(&world_path).with_context(|| format!("unable to load {world_path:?}"))?;
        let mut deserializer = Deserializer::from_bytes(&bytes)
            .with_context(|| format!("unable to parse {world_path:?}"))?;
        let scene_deserializer = SceneDeserializer {
            type_registry: &registry.read(),
        };
        let scene = scene_deserializer
            .deserialize(&mut deserializer)
            .with_context(|| format!("unable to deserialize {world_path:?}"))?;

        scene_spawner.spawn_dynamic(scenes.add(scene));
        commands.insert_resource(NextState(GameState::World));

        Ok(())
    }
}

/// Iterates over a world and serializes all components that implement [`Reflect`]
/// and not filtered using [`SaveRules`]
fn save_to_scene(world: &World, registry: &TypeRegistry, save_rules: &SaveRules) -> DynamicScene {
    let mut scene = DynamicScene::default();
    let registry = registry.read();
    for archetype in world
        .archetypes()
        .iter()
        .filter(|archetype| archetype.id() != ArchetypeId::EMPTY)
        .filter(|archetype| archetype.id() != ArchetypeId::INVALID)
        .filter(|archetype| save_rules.persistent_archetype(archetype))
    {
        let entities_offset = scene.entities.len();
        for archetype_entity in archetype.entities() {
            scene.entities.push(DynamicEntity {
                entity: archetype_entity.entity().index(),
                components: Vec::new(),
            });
        }

        for component_id in archetype
            .components()
            .filter(|&component_id| save_rules.persistent_component(archetype, component_id))
        {
            let component_info = unsafe { world.components().get_info_unchecked(component_id) };
            let type_name = component_info.name();
            let reflect_component = component_info
                .type_id()
                .and_then(|type_id| registry.get(type_id))
                .and_then(|registration| registration.data::<ReflectComponent>())
                .unwrap_or_else(|| panic!("non-ignored component {type_name} should be registered and have reflect(Component)"));

            for (index, archetype_entity) in archetype.entities().iter().enumerate() {
                let component = reflect_component
                    .reflect(world, archetype_entity.entity())
                    .unwrap_or_else(|| panic!("object should have {type_name}"));

                scene.entities[entities_offset + index]
                    .components
                    .push(component.clone_value());
            }
        }
    }

    scene
}

/// Marks entity as important for game.
///
/// Such entitites will be serialized / replicated.
#[derive(Component, Default, Reflect)]
#[reflect(Component)]
pub(crate) struct GameEntity;

/// Event that indicates that game is about to be saved to the file name based on [`GameWorld`] resource.
#[derive(Default)]
pub(crate) struct GameSave;

/// Event that indicates that game is about to be loaded from the file name based on [`GameWorld`] resource.
#[derive(Default)]
pub(crate) struct GameLoad;

/// Contains meta-information about the currently loaded world.
#[derive(Default, Deref, Resource)]
pub(crate) struct GameWorld {
    pub(crate) world_name: String,
}

impl GameWorld {
    pub(crate) fn new(world_name: String) -> Self {
        Self { world_name }
    }
}

#[cfg(test)]
mod tests {
    use bevy::{asset::AssetPlugin, core::CorePlugin, scene::ScenePlugin};

    use super::*;
    use crate::core::city::{City, CityBundle};

    #[test]
    fn saving_and_loading() {
        const WORLD_NAME: &str = "Test world";
        let mut app = App::new();
        app.register_type::<Camera>()
            .register_type::<City>()
            .init_resource::<GamePaths>()
            .insert_resource(GameWorld::new(WORLD_NAME.to_string()))
            .add_plugin(CorePlugin::default())
            .add_plugin(AssetPlugin::default())
            .add_plugin(ScenePlugin)
            .add_plugin(TransformPlugin)
            .add_plugin(GameWorldPlugin);

        const TRANSFORM: Transform = Transform::IDENTITY;
        let non_game_entity = app.world.spawn(TRANSFORM).id();
        let game_world_entity = app
            .world
            .spawn((
                SpatialBundle {
                    transform: TRANSFORM,
                    ..Default::default()
                },
                Camera::default(),
                GameEntity,
            ))
            .id();
        let non_game_city = app.world.spawn((SpatialBundle::default(), City)).id();
        let city = app
            .world
            .spawn((SpatialBundle::default(), CityBundle::default()))
            .push_children(&[game_world_entity])
            .id();

        app.world.send_event_default::<GameSave>();

        app.update();

        app.world.entity_mut(non_game_entity).despawn();
        app.world.entity_mut(non_game_city).despawn();
        app.world.entity_mut(city).despawn_recursive();

        app.world.send_event_default::<GameLoad>();

        app.update();
        app.update();

        assert_eq!(
            app.world.resource::<NextState<GameState>>().0,
            GameState::World
        );
        assert_eq!(
            *app.world.query::<&Transform>().single(&app.world),
            TRANSFORM,
        );
        assert!(
            app.world
                .query_filtered::<(), (With<City>, Without<Transform>)>()
                .get_single(&app.world)
                .is_ok(),
            "loaded city shouldn't contain transform"
        );
        assert!(
            app.world.query::<&Camera>().get_single(&app.world).is_err(),
            "camera component shouldn't be saved"
        );
    }
}
