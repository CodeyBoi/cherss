use std::time::Duration;

use sdl2::{event::Event, keyboard::Keycode, mouse::MouseButton};

use crate::{chessboard, chessgame::ChessGame};

pub fn _run_sdl(mut chess: ChessGame) {
    let ctx = sdl2::init().unwrap();
    let video_subsystem = ctx.video().unwrap();

    let window = video_subsystem
        .window("Chess Engine", 800, 800)
        .position_centered()
        .build()
        .unwrap();

    let mut canvas = window.into_canvas().build().unwrap();

    canvas.clear();
    chess._sdl_render(&mut canvas);
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
                } => chess._sdl_handle_keypress(keycode),
                Event::MouseButtonDown {
                    mouse_btn: MouseButton::Left,
                    x,
                    y,
                    ..
                } => {
                    let size = canvas.output_size().unwrap();
                    let tile_size = size.0.min(size.1) / chessboard::SIZE as u32;
                    let (row, col) = (7 - (y / tile_size as i32), x / tile_size as i32);

                    chess.handle_click(row as u8, col as u8);
                }
                _ => {}
            }
        }

        chess.make_bot_move();

        canvas.clear();
        chess._sdl_render(&mut canvas);
        canvas.present();

        ::std::thread::sleep(Duration::new(0, 1_000_000_000u32 / 60));
    }
}
