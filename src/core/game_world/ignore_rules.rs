use bevy::{
    ecs::{archetype::Archetype, component::ComponentId},
    prelude::*,
    utils::{HashMap, HashSet},
};

use super::parent_sync::ParentSync;
use crate::core::{
    city::City,
    family::{Budget, Family},
    game_world::GameEntity,
    object::{ObjectPath, Picked},
};

/// Contains [`ComponentId`]'s that used to decide
/// if a component should be serialized.
pub(crate) struct IgnoreRules {
    /// Components that should be serialized.
    pub(crate) serializable: HashSet<ComponentId>,
    /// Ignore a key component if its value is present in an archetype.
    pub(crate) ignored_if_present: HashMap<ComponentId, ComponentId>,
    /// ID of [`GameWorld`] component, only entities with this components should be serialized.
    pub(crate) game_entity_id: ComponentId,
}

impl FromWorld for IgnoreRules {
    fn from_world(world: &mut World) -> Self {
        let serializable = HashSet::from([
            world.init_component::<Transform>(),
            world.init_component::<Name>(),
            world.init_component::<City>(),
            world.init_component::<Family>(),
            world.init_component::<Budget>(),
            world.init_component::<ObjectPath>(),
            world.init_component::<GameEntity>(),
            world.init_component::<ParentSync>(),
            world.init_component::<Picked>(),
        ]);

        let ignored_if_present = HashMap::from([(
            world.init_component::<Transform>(),
            world.init_component::<City>(),
        )]);

        let game_entity_id = world.init_component::<GameEntity>();

        Self {
            serializable,
            ignored_if_present,
            game_entity_id,
        }
    }
}

impl IgnoreRules {
    /// Returns `true` if an archetype should be ignored.
    pub(crate) fn ignored_archetype(&self, archetype: &Archetype) -> bool {
        !archetype.contains(self.game_entity_id)
    }

    /// Returns `true` if a component of an archetype should be ignored.
    pub(crate) fn ignored_component(
        &self,
        archetype: &Archetype,
        component_id: ComponentId,
    ) -> bool {
        if let Some(conditional_id) = self.ignored_if_present.get(&component_id) {
            if archetype.contains(*conditional_id) {
                return true;
            }
        }

        !self.serializable.contains(&component_id)
    }
}
