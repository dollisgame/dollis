use bevy::prelude::*;
use derive_more::Display;
use iyes_loopless::prelude::IntoConditionalSystem;

use super::game_world::GameWorld;

pub(super) struct DollPlugin;

impl Plugin for DollPlugin {
    fn build(&self, app: &mut App) {
        app.register_type::<FirstName>()
            .register_type::<LastName>()
            .add_system(Self::name_update_system.run_if_resource_exists::<GameWorld>());
    }
}

impl DollPlugin {
    fn name_update_system(
        mut changed_names: Query<
            (&mut Name, &FirstName, &LastName),
            Or<(Changed<FirstName>, Changed<LastName>)>,
        >,
    ) {
        for (mut name, first_name, last_name) in &mut changed_names {
            name.set(format!("{first_name} {last_name}"));
        }
    }
}

#[derive(Component, Default, Deref, DerefMut, Display, Reflect)]
#[reflect(Component)]
pub(crate) struct FirstName(pub(crate) String);

#[derive(Component, Default, Deref, DerefMut, Display, Reflect)]
#[reflect(Component)]
pub(crate) struct LastName(pub(crate) String);

#[derive(Bundle, Default)]
pub(crate) struct DollBundle {
    name: Name,
    first_name: FirstName,
    last_name: LastName,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn name_update() {
        let mut app = App::new();
        app.init_resource::<GameWorld>().add_plugin(DollPlugin);

        const FIRST_NAME: &str = "First";
        const LAST_NAME: &str = "Last";
        let doll_entity = app
            .world
            .spawn()
            .insert(Name::default())
            .insert(FirstName(FIRST_NAME.to_string()))
            .insert(LastName(LAST_NAME.to_string()))
            .id();

        app.update();

        let name = app.world.get::<Name>(doll_entity).unwrap();
        assert_eq!(name.as_str(), format!("{FIRST_NAME} {LAST_NAME}"));
    }
}
