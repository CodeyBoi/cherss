use std::time::Duration;

use chessboard::Chessboard;
use sdl2::{event::Event, keyboard::Keycode, pixels::Color, render::Canvas, video::Window};

mod bot;
mod chessboard;
mod piece;

fn main() {
    let ctx = sdl2::init().unwrap();
    let video_subsystem = ctx.video().unwrap();

    let window = video_subsystem
        .window("Chess Engine", 800, 800)
        .position_centered()
        .build()
        .unwrap();

    let mut canvas = window.into_canvas().build().unwrap();

    let mut chess = Chessboard::default().into_game();

    canvas.clear();
    chess.render(&mut canvas);
    canvas.present();

    let mut event_pump = ctx.event_pump().unwrap();

    'main_loop: loop {
        for event in event_pump.poll_iter() {
            match event {
                Event::Quit { .. }
                | Event::KeyDown {
                    keycode: Some(Keycode::Escape),
                    ..
                } => {
                    break 'main_loop;
                }
                Event::KeyDown {
                    keycode: Some(keycode),
                    ..
                } => {
                    chess.handle_keypress(keycode);
                    canvas.clear();
                    chess.render(&mut canvas);
                    canvas.present();
                }
                event @ Event::MouseButtonDown {
                    mouse_btn,
                    x,
                    y,
                    clicks,
                    ..
                } => {
                    let size = canvas.output_size().unwrap();
                    let tile_size = size.0.min(size.1) / chessboard::SIZE as u32;
                    let (row, col) = (7 - (y / tile_size as i32), x / tile_size as i32);

                    chess.handle_click(row as u8, col as u8);

                    canvas.set_draw_color(Color::WHITE);
                    canvas.clear();
                    chess.render(&mut canvas);
                    canvas.present();
                }
                _ => {}
            }
        }

        // GAME LOOP CODE HERE

        ::std::thread::sleep(Duration::new(0, 1_000_000_000u32 / 60));
    }
}

fn render_chessboard(canvas: &mut Canvas<Window>, chessboard: &Chessboard) {}
