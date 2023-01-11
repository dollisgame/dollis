use std::f32::consts::PI;

use bevy::{
    prelude::*,
    render::{mesh::Indices, render_resource::PrimitiveTopology},
};
use itertools::{Itertools, MinMaxResult};

pub(super) struct WallPlugin;

impl Plugin for WallPlugin {
    fn build(&self, app: &mut App) {
        app.add_startup_system(Self::spawn_system)
            .add_system(Self::mesh_update_system);
    }
}

impl WallPlugin {
    fn spawn_system(
        mut commands: Commands,
        mut meshes: ResMut<Assets<Mesh>>,
        mut materials: ResMut<Assets<StandardMaterial>>,
    ) {
        commands.spawn(WallBundle {
            vertices: WallVertices(vec![
                (Vec2::ZERO, Vec2::X * 4.0),
                (Vec2::ZERO, Vec2::ONE * 4.0),
                (Vec2::ZERO, Vec2::Y * 4.0),
                // (Vec2::Y * 4.0, -Vec2::ONE * 8.0),
                // (Vec2::Y * 4.0, -Vec2::ONE * 8.0),
            ]),
            pbr_bundle: PbrBundle {
                mesh: meshes.add(Mesh::new(PrimitiveTopology::TriangleList)),
                material: materials.add(StandardMaterial::default()),
                transform: Transform::from_xyz(1.0, 0.0, 0.0),
                ..Default::default()
            },
        });
    }

    fn mesh_update_system(
        mut meshes: ResMut<Assets<Mesh>>,
        walls: Query<(&WallVertices, &Handle<Mesh>), Changed<WallVertices>>,
    ) {
        for (vertices, mesh_handle) in &walls {
            let mesh = meshes
                .get_mut(mesh_handle)
                .expect("associated mesh handle should be valid");

            let mut positions = Vec::new();
            let mut indices = Vec::new();
            for &(a, b) in vertices.iter() {
                let last_index: u32 = positions
                    .len()
                    .try_into()
                    .expect("vertex index should fit u32");

                let width = width_vec(a, b);
                let a_edges = minmax_angles(a, b, vertices);
                let (left_a, right_a) = offset_points(a, b, a_edges, width);

                let b_edges = minmax_angles(b, a, vertices);
                let (left_b, right_b) = offset_points(b, a, b_edges, width);

                positions.push(Vec3::new(left_a.x, 0.0, left_a.y));
                positions.push(Vec3::new(right_a.x, 0.0, right_a.y));
                positions.push(Vec3::new(right_b.x, 0.0, right_b.y));
                positions.push(Vec3::new(left_b.x, 0.0, left_b.y));

                const HEIGHT: f32 = 2.0;
                positions.push(Vec3::new(left_a.x, HEIGHT, left_a.y));
                positions.push(Vec3::new(right_a.x, HEIGHT, right_a.y));
                positions.push(Vec3::new(right_b.x, HEIGHT, right_b.y));
                positions.push(Vec3::new(left_b.x, HEIGHT, left_b.y));

                // Top
                indices.push(last_index + 4);
                indices.push(last_index + 5);
                indices.push(last_index + 7);
                indices.push(last_index + 5);
                indices.push(last_index + 6);
                indices.push(last_index + 7);

                // Left
                indices.push(last_index + 2);
                indices.push(last_index + 5);
                indices.push(last_index + 1);
                indices.push(last_index + 2);
                indices.push(last_index + 6);
                indices.push(last_index + 5);

                // Right
                indices.push(last_index + 0);
                indices.push(last_index + 4);
                indices.push(last_index + 3);
                indices.push(last_index + 4);
                indices.push(last_index + 7);
                indices.push(last_index + 3);

                match a_edges {
                    MinMaxResult::OneElement(_) => (),
                    MinMaxResult::NoElements => {
                        // Back
                        indices.push(last_index + 1);
                        indices.push(last_index + 4);
                        indices.push(last_index + 0);
                        indices.push(last_index + 1);
                        indices.push(last_index + 5);
                        indices.push(last_index + 4);
                    }
                    MinMaxResult::MinMax(_, _) => {
                        // Inside triangle to fill the gap between 3+ walls
                        positions.push(Vec3::new(a.x, HEIGHT, a.y));
                        indices.push(last_index + 4);
                        indices.push(positions.len() as u32 - 1);
                        indices.push(last_index + 5);
                    }
                }

                match b_edges {
                    MinMaxResult::OneElement(_) => (),
                    MinMaxResult::NoElements => {
                        // Front
                        indices.push(last_index + 3);
                        indices.push(last_index + 7);
                        indices.push(last_index + 2);
                        indices.push(last_index + 7);
                        indices.push(last_index + 6);
                        indices.push(last_index + 2);
                    }
                    MinMaxResult::MinMax(_, _) => {
                        // Inside triangle to fill the gap between 3+ walls
                        positions.push(Vec3::new(a.x, HEIGHT, a.y));
                        indices.push(last_index + 7);
                        indices.push(positions.len() as u32 - 1);
                        indices.push(last_index + 6);
                    }
                }
            }

            mesh.insert_attribute(Mesh::ATTRIBUTE_POSITION, positions);
            mesh.set_indices(Some(Indices::U32(indices)));
        }
    }
}

fn width_vec(start: Vec2, end: Vec2) -> Vec2 {
    const WIDTH: f32 = 0.25;
    const HALF_WIDTH: f32 = WIDTH / 2.0;
    (end - start).perp().normalize() * HALF_WIDTH
}

fn offset_points(
    start: Vec2,
    end: Vec2,
    edges: MinMaxResult<(Vec2, Vec2)>,
    width: Vec2,
) -> (Vec2, Vec2) {
    match edges {
        MinMaxResult::NoElements => (start - width, start + width),
        MinMaxResult::OneElement((a, b)) => (
            wall_intersection(start, end, b, a, width),
            wall_intersection(start, end, a, b, -width),
        ),
        MinMaxResult::MinMax((min_a, min_b), (max_a, max_b)) => (
            wall_intersection(start, end, min_b, min_a, width),
            wall_intersection(start, end, max_a, max_b, -width),
        ),
    }
}

fn minmax_angles(a: Vec2, b: Vec2, vertices: &[(Vec2, Vec2)]) -> MinMaxResult<(Vec2, Vec2)> {
    let direction = b - a;
    vertices
        .iter()
        .filter_map(|&(p1, p2)| {
            if p1 == a && p2 != b {
                Some((p1, p2))
            } else if p2 == a && p1 != b {
                Some((p2, p1))
            } else {
                None
            }
        })
        .minmax_by_key(|&(p1, p2)| {
            let angle = (p2 - p1).angle_between(direction);
            if angle < 0.0 {
                angle + PI
            } else {
                angle
            }
        })
}

fn wall_intersection(start: Vec2, end: Vec2, a: Vec2, b: Vec2, width: Vec2) -> Vec2 {
    let current_line = Line::with_offset(start, end, width);
    let line = Line::with_offset(a, b, width_vec(a, b));
    current_line
        .intersection(line)
        .unwrap_or_else(|| start + width)
}

#[derive(Clone, Copy, PartialEq, Debug)]
struct Line {
    a: f32,
    b: f32,
    c: f32,
}

impl Line {
    #[must_use]
    fn new(p1: Vec2, p2: Vec2) -> Self {
        let a = p2.y - p1.y;
        let b = p1.x - p2.x;
        let c = a * p1.x + b * p1.y;
        Self { a, b, c }
    }

    #[must_use]
    fn with_offset(p1: Vec2, p2: Vec2, offset: Vec2) -> Self {
        Self::new(p1 + offset, p2 + offset)
    }

    fn intersection(self, rhs: Self) -> Option<Vec2> {
        let det = self.a * rhs.b - rhs.a * self.b;
        if det == 0.0 {
            None
        } else {
            Some(Vec2 {
                x: (self.b * rhs.c - rhs.b * self.c) / det,
                y: (rhs.a * self.c - self.a * rhs.c) / det,
            })
        }
    }
}

#[derive(Bundle)]
struct WallBundle {
    vertices: WallVertices,
    pbr_bundle: PbrBundle,
}

/// Stores a handle for the lot line material.
#[derive(Resource)]
struct WallMaterial(Handle<StandardMaterial>);

impl FromWorld for WallMaterial {
    fn from_world(world: &mut World) -> Self {
        let mut materials = world.resource_mut::<Assets<StandardMaterial>>();
        let handle = materials.add(StandardMaterial::default());
        Self(handle)
    }
}
#[derive(Clone, Component, Default, Deref, DerefMut, Reflect)]
#[reflect(Component)]
pub(super) struct WallVertices(Vec<(Vec2, Vec2)>);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn intersecting_lines() {
        let line_a = Line::new(Vec2::X, Vec2::ONE);
        let line_b = Line::new(Vec2::ZERO, Vec2::X * 2.0);

        assert_eq!(line_a.intersection(line_b), Some(Vec2::X));
    }

    #[test]
    fn parallel_lines() {
        let line_a = Line::new(Vec2::X, Vec2::ONE);
        let line_b = Line::new(Vec2::X * 2.0, Vec2::ONE * 2.0);

        assert_eq!(line_a.intersection(line_b), None);
    }
}
