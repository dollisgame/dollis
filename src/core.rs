pub(super) mod action;
pub(super) mod asset_metadata;
pub(super) mod city;
pub(super) mod cli;
mod developer;
pub(super) mod doll;
pub(super) mod error_message;
pub(super) mod family;
pub(super) mod family_editor;
pub(super) mod game_paths;
pub(super) mod game_state;
pub(super) mod game_world;
mod ground;
pub(super) mod input_events;
pub(super) mod lot;
pub(super) mod network;
pub(super) mod object;
mod orbit_camera;
pub(super) mod picking;
pub(super) mod preview;
pub(super) mod settings;
pub(super) mod task;
mod video;
mod wall;

use bevy::{app::PluginGroupBuilder, prelude::*};

use action::ActionPlugin;
use asset_metadata::AssetMetadataPlugin;
use city::CityPlugin;
use cli::CliPlugin;
use developer::DeveloperPlugin;
use doll::DollPlugin;
use family::FamilyPlugin;
use family_editor::FamilyEditorPlugin;
use game_paths::GamePathsPlugin;
use game_state::GameStatePlugin;
use game_world::GameWorldPlugin;
use ground::GroundPlugin;
use lot::LotPlugin;
use network::NetworkPlugin;
use object::ObjectPlugin;
use orbit_camera::OrbitCameraPlugin;
use picking::PickingPlugin;
use preview::PreviewPlugin;
use settings::SettingsPlugin;
use task::TaskPlugin;
use video::VideoPlugin;
use wall::WallPlugin;

pub(super) struct CorePlugins;

impl PluginGroup for CorePlugins {
    fn build(self) -> PluginGroupBuilder {
        PluginGroupBuilder::start::<Self>()
            .add(NetworkPlugin) // Should be built first to register server fixed timestep.
            .add(AssetMetadataPlugin)
            .add(GameStatePlugin)
            .add(CityPlugin)
            .add(CliPlugin)
            .add(DollPlugin)
            .add(LotPlugin)
            .add(GroundPlugin)
            .add(ActionPlugin)
            .add(DeveloperPlugin)
            .add(FamilyPlugin)
            .add(FamilyEditorPlugin)
            .add(GamePathsPlugin)
            .add(PickingPlugin)
            .add(PreviewPlugin)
            .add(OrbitCameraPlugin)
            .add(SettingsPlugin)
            .add(VideoPlugin)
            .add(TaskPlugin)
            .add(GameWorldPlugin)
            .add(ObjectPlugin)
            .add(WallPlugin)
    }
}
