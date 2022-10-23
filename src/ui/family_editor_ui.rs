use std::mem;

use anyhow::{ensure, Result};
use bevy::prelude::*;
use bevy_egui::{
    egui::{epaint::WHITE_UV, Align2, Area, ImageButton, TextEdit, TextureId, Window},
    EguiContext,
};
use iyes_loopless::prelude::*;
use leafwing_input_manager::prelude::ActionState;

use super::{
    modal_window::{ModalUiExt, ModalWindow},
    ui_action::UiAction,
    UI_MARGIN,
};
use crate::core::{
    doll::{DollBundle, FirstName, LastName},
    error_message,
    family::{Family, FamilySave, FamilySaved, FamilySystems},
    family_editor::{EditableDoll, EditableFamily, FamilyEditor},
    game_state::GameState,
};

pub(super) struct FamilyEditorUiPlugin;

impl Plugin for FamilyEditorUiPlugin {
    fn build(&self, app: &mut App) {
        app.add_system(Self::personality_window_system.run_in_state(GameState::FamilyEditor))
            .add_system(Self::dolls_panel_system.run_in_state(GameState::FamilyEditor))
            .add_system(
                Self::confirm_cancel_system
                    .chain(error_message::err_message_system)
                    .run_in_state(GameState::FamilyEditor),
            )
            .add_system(
                Self::save_family_dialog_system
                    .run_if_resource_exists::<SaveFamilyDialog>()
                    .before(FamilySystems::SaveSystem),
            )
            .add_system(Self::confirmation_system.run_on_event::<FamilySaved>());
    }
}

impl FamilyEditorUiPlugin {
    fn personality_window_system(
        mut egui: ResMut<EguiContext>,
        mut editable_dolls: Query<(&mut FirstName, &mut LastName), With<EditableDoll>>,
    ) {
        if let Ok((mut first_name, mut last_name)) = editable_dolls.get_single_mut() {
            Window::new("Personality")
                .anchor(Align2::LEFT_TOP, (0.0, 0.0))
                .resizable(false)
                .show(egui.ctx_mut(), |ui| {
                    ui.add(TextEdit::singleline(&mut first_name.0).hint_text("First name"));
                    ui.add(TextEdit::singleline(&mut last_name.0).hint_text("Last name"));
                });
        }
    }

    fn dolls_panel_system(
        mut commands: Commands,
        mut egui: ResMut<EguiContext>,
        mut editable_families: Query<&mut Family, With<EditableFamily>>,
        editable_dolls: Query<Entity, With<EditableDoll>>,
        family_editors: Query<Entity, With<FamilyEditor>>,
    ) {
        Window::new("Dolls")
            .resizable(false)
            .title_bar(false)
            .anchor(Align2::LEFT_BOTTOM, (0.0, 0.0))
            .show(egui.ctx_mut(), |ui| {
                ui.horizontal(|ui| {
                    let mut family = editable_families.single_mut();
                    let current_entity = editable_dolls.get_single();
                    for &entity in family.iter() {
                        if ui
                            .add(
                                ImageButton::new(TextureId::Managed(0), (64.0, 64.0))
                                    .uv([WHITE_UV, WHITE_UV]).selected(matches!(current_entity, Ok(current_member) if entity == current_member)),
                            )
                            .clicked()
                        {
                            if let Ok(current_entity) = current_entity {
                                commands.entity(current_entity).remove::<EditableDoll>();
                            }
                            commands.entity(entity).insert(EditableDoll);
                        }
                    }
                    if ui.button("+").clicked() {
                        if let Ok(current_entity) = current_entity {
                            commands.entity(current_entity).remove::<EditableDoll>();
                        }
                        let new_member = commands.entity(family_editors.single()).add_children(|parent| parent.spawn_bundle(DollBundle::default()).insert(EditableDoll).id());
                        family.push(new_member);
                    }
                });
            });
    }

    fn confirm_cancel_system(
        mut commands: Commands,
        mut egui: ResMut<EguiContext>,
        editable_families: Query<&Family, With<EditableFamily>>,
        names: Query<(&FirstName, &LastName)>,
    ) -> Result<()> {
        let mut is_confirmed = false;
        Area::new("Confrirm cancel")
            .anchor(Align2::RIGHT_BOTTOM, (-UI_MARGIN, -UI_MARGIN))
            .show(egui.ctx_mut(), |ui| {
                if ui.button("Cancel").clicked() {
                    commands.insert_resource(NextState(GameState::World));
                }
                is_confirmed = ui.button("Confirm").clicked();
            });

        if is_confirmed {
            let family = editable_families.single();
            ensure!(!family.is_empty(), "family cannot be empty");
            for (index, &member) in family.iter().enumerate() {
                let (first_name, last_name) = names
                    .get(member)
                    .expect("family member should have a first and a last name");
                ensure!(
                    !first_name.is_empty(),
                    "family member {index} do not have a first name"
                );
                ensure!(
                    !last_name.is_empty(),
                    "family member {index} do not have a last name"
                );
            }
            commands.init_resource::<SaveFamilyDialog>();
        }

        Ok(())
    }

    fn save_family_dialog_system(
        mut commands: Commands,
        mut save_events: EventWriter<FamilySave>,
        mut save_dialog: ResMut<SaveFamilyDialog>,
        mut action_state: ResMut<ActionState<UiAction>>,
        mut egui: ResMut<EguiContext>,
        mut editable_families: Query<(Entity, &mut Name), With<EditableFamily>>,
    ) {
        let mut is_open = true;
        ModalWindow::new("Save family")
            .open(&mut is_open, &mut action_state)
            .show(egui.ctx_mut(), |ui| {
                ui.text_edit_singleline(&mut save_dialog.family_name);
                if ui.button("Ok").clicked() {
                    let (family_entity, mut name) = editable_families.single_mut();
                    name.set(mem::take(&mut save_dialog.family_name));
                    save_events.send(FamilySave(family_entity));
                    ui.close_modal();
                }
            });

        if !is_open {
            commands.remove_resource::<SaveFamilyDialog>();
        }
    }

    fn confirmation_system(mut commands: Commands) {
        commands.insert_resource(NextState(GameState::World));
    }
}

struct SaveFamilyDialog {
    family_name: String,
}

impl FromWorld for SaveFamilyDialog {
    fn from_world(world: &mut World) -> Self {
        let family = world
            .query_filtered::<&Family, With<EditableFamily>>()
            .single(world);
        let first_member = *family.first().expect("family shouldn't be empty");
        let last_name = world
            .get::<LastName>(first_member)
            .expect("family members should have last name");

        Self {
            family_name: last_name.to_string(),
        }
    }
}