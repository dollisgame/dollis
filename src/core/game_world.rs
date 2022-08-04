use anyhow::{Context, Result};
use bevy::{
    ecs::archetype::ArchetypeId,
    prelude::*,
    reflect::{
        serde::{ReflectDeserializer, ReflectSerializer},
        TypeRegistry, TypeRegistryInternal,
    },
    utils::HashMap,
};
use derive_more::Display;
use iyes_loopless::prelude::IntoConditionalSystem;
use serde::de::DeserializeSeed;
use std::{
    any::{type_name, TypeId},
    fs,
};

use super::{errors::log_err_system, game_paths::GamePaths, game_state::InGameOnly};

struct GameWorldPlugin;

impl Plugin for GameWorldPlugin {
    fn build(&self, app: &mut App) {
        app.add_event::<GameSaved>()
            .add_event::<GameLoaded>()
            .add_system(Self::world_saving_system.chain(log_err_system).run_on_event::<GameSaved>())
            .add_system(iyes_loopless::condition::IntoConditionalExclusiveSystem::run_on_event::<GameLoaded>(Self::world_loading_system).at_start());
    }
}

impl GameWorldPlugin {
    fn world_saving_system(
        world: &World,
        world_name: Res<WorldName>,
        game_paths: Res<GamePaths>,
    ) -> Result<()> {
        let world_path = game_paths.world_path(&world_name.0);

        fs::create_dir_all(&game_paths.worlds)
            .with_context(|| format!("Unable to create {world_path:?}"))?;

        let bytes = rmp_serde::to_vec(&serialize_game_world(world).values().collect::<Vec<_>>())
            .context("Unable to serlialize world")?;

        fs::write(&world_path, bytes)
            .with_context(|| format!("Unable to save game to {world_path:?}"))
    }

    fn world_loading_system(world: &mut World) {
        let world_name = world.resource::<WorldName>();
        let game_paths = world.resource::<GamePaths>();
        let world_path = game_paths.world_path(&world_name.0);

        let bytes = match fs::read(&world_path) {
            Ok(bytes) => bytes,
            Err(error) => {
                error!("Unable to load world from {world_path:?}: {error:#}");
                return;
            }
        };

        match rmp_serde::from_slice::<Vec<Vec<Vec<u8>>>>(&bytes) {
            Ok(components) => deserialize_game_world(world, components),
            Err(error) => error!("Unable to deserialize game world: {error:#}"),
        }
    }
}

/// Iterates over a world and serializes all components that implement [`Reflect`]
/// on entities that have [`InGameOnly`] component.
fn serialize_game_world(world: &World) -> HashMap<Entity, Vec<Vec<u8>>> {
    let mut components = HashMap::new();
    let type_registry = world.resource::<TypeRegistry>().read();
    for archetype in world.archetypes().iter() {
        if matches!(
            archetype.id(),
            ArchetypeId::EMPTY | ArchetypeId::RESOURCE | ArchetypeId::INVALID
        ) {
            continue;
        }

        if archetype
            .components()
            .filter_map(|component_id| {
                // SAFETY: `component_id` retrieved from the world.
                unsafe { world.components().get_info_unchecked(component_id) }.type_id()
            })
            .all(|type_id| type_id != TypeId::of::<InGameOnly>())
        {
            // Not an ingame entity
            continue;
        }

        for reflect_component in archetype
            .components()
            .filter_map(|component_id| {
                // SAFETY: `component_id` retrieved from the world.
                unsafe { world.components().get_info_unchecked(component_id) }.type_id()
            })
            .filter_map(|type_id| type_registry.get(type_id))
            .filter_map(|registration| registration.data::<ReflectComponent>())
        {
            for entity in archetype.entities() {
                if let Some(reflect) = reflect_component.reflect(world, *entity) {
                    let serializer = ReflectSerializer::new(reflect, &type_registry);
                    match rmp_serde::to_vec(&serializer) {
                        Ok(bytes) => {
                            let entry: &mut Vec<Vec<u8>> = components.entry(*entity).or_default();
                            entry.push(bytes);
                        }
                        Err(error) => error!("Unable to serialize component: {error:#}"),
                    }
                }
            }
        }
    }

    components
}

fn deserialize_game_world(world: &mut World, components: Vec<Vec<Vec<u8>>>) {
    // Temorary take resources to avoid borrowing issues
    let type_registry = world
        .remove_resource::<TypeRegistry>()
        .unwrap_or_else(|| panic!("Unable to extract {}", type_name::<TypeRegistry>()));
    let read_registry = type_registry.read();

    for entity_components in components {
        let entity = world.spawn().id();
        for component in entity_components {
            if let Err(error) = deserialize_component(world, &read_registry, entity, &component) {
                error!("{error:#}");
            }
        }
    }

    drop(read_registry);
    world.insert_resource(type_registry);
}

fn deserialize_component(
    world: &mut World,
    read_registry: &TypeRegistryInternal,
    entity: Entity,
    component: &[u8],
) -> Result<()> {
    let reflect_deserializer = ReflectDeserializer::new(read_registry);
    let mut deserializer = rmp_serde::Deserializer::from_read_ref(&component);

    let reflect = reflect_deserializer
        .deserialize(&mut deserializer)
        .context("Unable to deserialize component")?;

    let registration = read_registry
        .get_with_name(reflect.type_name())
        .with_context(|| format!("Unable to get registration for {}", reflect.type_name()))?;

    let reflect_component = registration
        .data::<ReflectComponent>()
        .with_context(|| format!("Unable to reflect component for {}", reflect.type_name()))?;

    reflect_component.insert(world, entity, &*reflect);

    Ok(())
}

/// Event that indicates that game is about to be saved to the file name based on [`WorldName`].
#[derive(Display)]
struct GameSaved;

/// Event that indicates that game is about to be loaded from the file name based on [`WorldName`].
#[derive(Display)]
struct GameLoaded;

/// The name of the current world.
struct WorldName(String);

#[cfg(test)]
mod tests {
    use anyhow::{Context, Result};
    use bevy::core::CorePlugin;

    use super::*;
    use crate::core::game_paths::GamePaths;

    #[test]
    fn saving_and_loading() -> Result<()> {
        const WORLD_NAME: &str = "Test world";
        let mut app = App::new();
        app.init_resource::<GamePaths>()
            .insert_resource(WorldName(WORLD_NAME.to_string()))
            .add_plugin(CorePlugin)
            .add_plugin(TransformPlugin)
            .add_plugin(GameWorldPlugin);

        let game_paths = app.world.resource::<GamePaths>();
        let world_path = game_paths.world_path(WORLD_NAME);
        assert!(
            !world_path.exists(),
            "File {world_path:?} shouldn't exists after the plugin initialization"
        );

        const TRANSFORM: Transform = Transform::identity();
        let ingame_entity = app.world.spawn().insert(TRANSFORM).insert(InGameOnly).id();
        let other_entity = app.world.spawn().insert(Transform::identity()).id();

        let mut save_events = app.world.resource_mut::<Events<GameSaved>>();
        save_events.send(GameSaved);

        app.update();

        app.world.entity_mut(ingame_entity).despawn();
        app.world.entity_mut(other_entity).despawn();

        let mut save_events = app.world.resource_mut::<Events<GameLoaded>>();
        save_events.send(GameLoaded);

        app.update();

        assert_eq!(
            *app.world.query::<&Transform>().single(&app.world),
            TRANSFORM,
            "Loaded transform should be equal to the saved"
        );

        fs::remove_file(&world_path)
            .with_context(|| format!("Unable to remove {world_path:?} after test"))
    }
}