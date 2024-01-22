//! This example illustrates the use of [`EventBasedStates`].
//! 
//! These are [`States`], just like the ones in the `state` example, but with one
//! caveat: you can only modify them through a set of strongly typed events.
//! 
//! This allows you to define the actual control flow of the app in the event
//! handler alongisde the type definition, and then issue those events from anywhere
//! else in your codebase. Events that aren't applicable at a given time just get ignored,
//! and there is no need to worry about potential breakage or drift between areas that modify
//! the state.
//!
//! In this case, we're transitioning from a `Menu` state to an `InGame` state, with the addition of
//! two layers of TurboMode. When you tap the `T` key, the game will move to the first TurboMode.
//! And when you tap it again, it'll move to the second TurboMode. The third tap will return to normal gameplay.
//! 
//! We're also using functionality provided by [`ComputedStates`] - as illustrated in the `computed_states` example,
//! but will not be focusing on explaing it here.

use bevy::prelude::*;

#[derive(Debug, Clone, Copy, Default, Eq, PartialEq, Hash, States)]
#[event(AppStateEvent, process_app_state)]
enum AppState {
    #[default]
    Menu,
    InGame { turbo: u8 },
}

#[derive(Event)]
enum AppStateEvent {
    EnterGame,
    ToggleTurbo
}

fn process_app_state(mut current: Option<AppState>, event: &AppStateEvent) -> Option<AppState>{
    match event {
        AppStateEvent::EnterGame => match &current {
            Some(AppState::InGame { .. }) => {},
            _ => {
                current = Some(AppState::InGame { turbo: 0 });
            }
        },
        AppStateEvent::ToggleTurbo => match &current {
            Some(AppState::InGame { turbo }) => {
                current = Some(AppState::InGame { turbo: if *turbo < 2 { turbo + 1 } else { 0 } })
            }
            _ => {}
        },
    }
    current
}

#[derive(Debug, Clone, Copy, Default, Eq, PartialEq, Hash)]
struct InGame;

impl ComputedStates for InGame {
    type SourceStates = AppState;

    fn compute(
        sources: <<Self as ComputedStates>::SourceStates as StateSet>::Optionals,
    ) -> Option<Self> {
        match sources {
            Some(AppState::InGame { .. }) => Some(InGame),
            _ => None,
        }
    }
}

fn main() {
    App::new()
        .add_plugins(DefaultPlugins)
        .init_event_state::<AppState>() // Alternatively we could use .insert_state(AppState::Menu)
        .add_computed_state::<InGame>()
        .add_systems(Startup, setup)
        // This system runs when we enter `AppState::Menu`, during the `StateTransition` schedule.
        // All systems from the exit schedule of the state we're leaving are run first,
        // and then all systems from the enter schedule of the state we're entering are run second.
        .add_systems(OnEnter(AppState::Menu), setup_menu)
        // By contrast, update systems are stored in the `Update` schedule. They simply
        // check the value of the `State<T>` resource to see if they should run each frame.
        .add_systems(Update, menu.run_if(in_state(AppState::Menu)))
        .add_systems(OnExit(AppState::Menu), cleanup_menu)
        .add_systems(OnEnter(InGame), setup_game)
        .add_systems(
            Update,
            (movement, change_color, toggle_turbo).run_if(in_state(InGame)),
        )
        .add_systems(Update, log_transitions)
        .run();
}

#[derive(Resource)]
struct MenuData {
    button_entity: Entity,
}

const NORMAL_BUTTON: Color = Color::rgb(0.15, 0.15, 0.15);
const HOVERED_BUTTON: Color = Color::rgb(0.25, 0.25, 0.25);
const PRESSED_BUTTON: Color = Color::rgb(0.35, 0.75, 0.35);

fn setup(mut commands: Commands) {
    commands.spawn(Camera2dBundle::default());
}

fn setup_menu(mut commands: Commands) {
    let button_entity = commands
        .spawn(NodeBundle {
            style: Style {
                // center button
                width: Val::Percent(100.),
                height: Val::Percent(100.),
                justify_content: JustifyContent::Center,
                align_items: AlignItems::Center,
                ..default()
            },
            ..default()
        })
        .with_children(|parent| {
            parent
                .spawn(ButtonBundle {
                    style: Style {
                        width: Val::Px(150.),
                        height: Val::Px(65.),
                        // horizontally center child text
                        justify_content: JustifyContent::Center,
                        // vertically center child text
                        align_items: AlignItems::Center,
                        ..default()
                    },
                    background_color: NORMAL_BUTTON.into(),
                    ..default()
                })
                .with_children(|parent| {
                    parent.spawn(TextBundle::from_section(
                        "Play",
                        TextStyle {
                            font_size: 40.0,
                            color: Color::rgb(0.9, 0.9, 0.9),
                            ..default()
                        },
                    ));
                });
        })
        .id();
    commands.insert_resource(MenuData { button_entity });
}

fn menu(
    mut events: EventWriter<AppStateEvent>,
    mut interaction_query: Query<
        (&Interaction, &mut BackgroundColor),
        (Changed<Interaction>, With<Button>),
    >,
) {
    for (interaction, mut color) in &mut interaction_query {
        match *interaction {
            Interaction::Pressed => {
                *color = PRESSED_BUTTON.into();
                events.send(AppStateEvent::EnterGame);
            }
            Interaction::Hovered => {
                *color = HOVERED_BUTTON.into();
            }
            Interaction::None => {
                *color = NORMAL_BUTTON.into();
            }
        }
    }
}

fn cleanup_menu(mut commands: Commands, menu_data: Res<MenuData>) {
    commands.entity(menu_data.button_entity).despawn_recursive();
}

fn setup_game(mut commands: Commands, asset_server: Res<AssetServer>) {
    commands.spawn(SpriteBundle {
        texture: asset_server.load("branding/icon.png"),
        ..default()
    });
}

const SPEED: f32 = 100.0;
fn movement(
    app_state: Res<State<AppState>>,
    time: Res<Time>,
    input: Res<ButtonInput<KeyCode>>,
    mut query: Query<&mut Transform, With<Sprite>>,
) {
    for mut transform in &mut query {
        let mut direction = Vec3::ZERO;
        if input.pressed(KeyCode::ArrowLeft) {
            direction.x -= 1.0;
        }
        if input.pressed(KeyCode::ArrowRight) {
            direction.x += 1.0;
        }
        if input.pressed(KeyCode::ArrowUp) {
            direction.y += 1.0;
        }
        if input.pressed(KeyCode::ArrowDown) {
            direction.y -= 1.0;
        }

        if direction != Vec3::ZERO {
            let speed = SPEED * match app_state.get() {
                AppState::Menu => 0.0,
                AppState::InGame { turbo } => (*turbo as f32) + 1.0,
            };
            transform.translation += direction.normalize() * speed * time.delta_seconds();
        }
    }
}

fn toggle_turbo(mut events: EventWriter<AppStateEvent>, input: Res<ButtonInput<KeyCode>>) {
    if input.just_pressed(KeyCode::KeyT) {
        events.send(AppStateEvent::ToggleTurbo);
    }
}

fn change_color(time: Res<Time>, mut query: Query<&mut Sprite>,
    app_state: Res<State<AppState>>,) {
        let time_mod = 0.5 + match app_state.get() {
            AppState::Menu => 0.0,
            AppState::InGame { turbo } => (*turbo as f32) * 2.0,
        };
    for mut sprite in &mut query {
        sprite
            .color
            .set_b((time.elapsed_seconds() * time_mod).sin() + 2.0);
    }
}

/// print when an `AppState` transition happens
/// also serves as an example of how to use `StateTransitionEvent`
fn log_transitions(mut transitions: EventReader<StateTransitionEvent<AppState>>) {
    for transition in transitions.read() {
        info!(
            "transition: {:?} => {:?}",
            transition.before, transition.after
        );
    }
}
