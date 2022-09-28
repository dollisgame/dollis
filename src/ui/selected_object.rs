use bevy::{asset::HandleId, prelude::*};
use iyes_loopless::prelude::*;

use crate::core::{
    asset_metadata,
    city::City,
    game_state::GameState,
    game_world::parent_sync::ParentSync,
    object::CursorObject,
};

pub(super) struct SelectedObjectPlugin;

impl Plugin for SelectedObjectPlugin {
    fn build(&self, app: &mut App) {
        app.add_system_to_stage(
            // Should run in state before `Self::removal_selection_system` to flush spawn command,
            // otherwise `MovingObject` will be missing and it will be detected as removal.
            CoreStage::PreUpdate,
            Self::spawn_selection_system
                .run_in_state(GameState::City)
                .run_if_resource_added::<SelectedObject>(),
        )
        .add_system(
            Self::remove_selection_system
                .run_in_state(GameState::City)
                .run_if_resource_exists::<SelectedObject>(),
        );
    }
}

impl SelectedObjectPlugin {
    fn spawn_selection_system(
        mut commands: Commands,
        asset_server: Res<AssetServer>,
        selected_object: Res<SelectedObject>,
        visible_cities: Query<Entity, (With<City>, With<Visibility>)>,
    ) {
        let metadata_path = asset_server
            .get_handle_path(selected_object.0)
            .expect("selected object metadata should have a path");

        let scene_path = asset_metadata::scene_path(metadata_path.path());

        commands
            .spawn_bundle(SceneBundle {
                scene: asset_server.load(&scene_path),
                ..Default::default()
            })
            .insert(CursorObject::Spawning(scene_path))
            .insert(ParentSync(visible_cities.single()));
    }

    fn remove_selection_system(
        mut commands: Commands,
        moving_objects: Query<(), With<CursorObject>>,
    ) {
        if moving_objects.is_empty() {
            commands.remove_resource::<SelectedObject>();
        }
    }
}

/// Resource that represents object selection in an object placement menu.
#[derive(Clone, Copy)]
pub(super) struct SelectedObject(pub(crate) HandleId);

#[cfg(test)]
mod tests {
    use bevy::{asset::AssetPlugin, core::CorePlugin, utils::Uuid};

    use crate::core::{asset_metadata::AssetMetadata, object::ObjectPath};

    use super::*;

    #[test]
    fn spawning_selection() {
        let mut app = App::new();
        app.add_loopless_state(GameState::City)
            .add_plugin(CorePlugin)
            .add_plugin(AssetPlugin)
            .add_plugin(SelectedObjectPlugin);

        app.update();

        let city = app
            .world
            .spawn()
            .insert(City)
            .insert(Visibility::default())
            .id();

        let asset_server = app.world.resource::<AssetServer>();
        let dummy_handle: Handle<AssetMetadata> = asset_server.load("dummy.toml");
        app.world.insert_resource(SelectedObject(dummy_handle.id));

        app.update();

        let parent_sync = app
            .world
            .query_filtered::<&ParentSync, (With<CursorObject>, With<ObjectPath>)>()
            .single(&app.world);
        assert_eq!(parent_sync.0, city);
    }

    #[test]
    fn removing_selection() {
        let mut app = App::new();
        app.add_loopless_state(GameState::City)
            .insert_resource(SelectedObject(HandleId::Id(Uuid::nil(), 0)))
            .add_plugin(SelectedObjectPlugin);

        app.update();

        assert!(
            app.world.get_resource::<SelectedObject>().is_none(),
            "selection should be removed when there is no moving object"
        );
    }
}
