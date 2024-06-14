use crate::player::Player;
use sdl2::{keyboard::Keycode, pixels::Color, rect::Rect, render::Canvas, video::Window};

use crate::{
    chessboard::{Chessboard, Position, SIZE},
    piece::ChessColor,
};

pub struct ChessGame {
    board: Chessboard,
    active_tile: Option<Position>,
}

impl ChessGame {
    pub fn new(board: Chessboard) -> Self {
        Self {
            board,
            active_tile: None,
        }
    }

    pub fn render(&mut self, canvas: &mut Canvas<Window>) {
        self.draw_chessboard(canvas);
        self.draw_highlights(canvas);
        self.draw_pieces(canvas);
    }

    fn draw_chessboard(&self, canvas: &mut Canvas<Window>) {
        let size = canvas.output_size().unwrap();
        let tile_size = size.0.min(size.1) / SIZE as u32;
        for (row, tile_row) in self.board.board.iter().enumerate() {
            for (col, _) in tile_row.iter().enumerate() {
                let row = 7 - row;
                let color = if (row + col) % 2 == 0 {
                    Color::RGB(240, 217, 181)
                } else {
                    Color::RGB(181, 136, 99)
                };
                canvas.set_draw_color(color);

                canvas
                    .fill_rect(Rect::new(
                        col as i32 * tile_size as i32,
                        row as i32 * tile_size as i32,
                        tile_size,
                        tile_size,
                    ))
                    .unwrap();
            }
        }
    }

    fn draw_highlights(&mut self, canvas: &mut Canvas<Window>) {
        let size = canvas.output_size().unwrap();
        let tile_size = size.0.min(size.1) / SIZE as u32;

        // Highlight last move
        if let Some(prev_move) = self.board.history.last() {
            for Position { row, col } in [prev_move.from, prev_move.to] {
                let row = 7 - row;
                let color = if (row + col) % 2 == 0 {
                    Color::RGB(205, 210, 106)
                } else {
                    Color::RGB(170, 162, 58)
                };
                canvas.set_draw_color(color);
                canvas
                    .fill_rect(Rect::new(
                        col as i32 * tile_size as i32,
                        row as i32 * tile_size as i32,
                        tile_size,
                        tile_size,
                    ))
                    .unwrap();
            }
        }

        // Highlight all moves the active piece can make
        if let Some(active_tile) = self.active_tile {
            let color = Color::RGBA(55, 180, 55, 75);
            let margin = 40;
            canvas.set_draw_color(color);
            let (row, col) = (7 - active_tile.row, active_tile.col);
            canvas
                .fill_rect(Rect::new(
                    col as i32 * tile_size as i32,
                    row as i32 * tile_size as i32,
                    tile_size,
                    tile_size,
                ))
                .unwrap();

            for pos @ Position { row, col } in self.board.moves(active_tile) {
                let row = 7 - row;
                // Highlight pieces that can be captured
                if self.board.at(pos).is_some() {
                    for i in 0..5 {
                        canvas
                            .draw_rect(Rect::new(
                                col as i32 * tile_size as i32 + i,
                                row as i32 * tile_size as i32 + i,
                                tile_size - i as u32 * 2,
                                tile_size - i as u32 * 2,
                            ))
                            .unwrap();
                    }
                // Highlight other tiles
                } else {
                    canvas
                        .fill_rect(Rect::new(
                            col as i32 * tile_size as i32 + margin,
                            row as i32 * tile_size as i32 + margin,
                            tile_size - margin as u32 * 2,
                            tile_size - margin as u32 * 2,
                        ))
                        .unwrap();
                }
            }
        }
    }

    fn draw_pieces(&self, canvas: &mut Canvas<Window>) {
        let size = canvas.output_size().unwrap();
        let tile_size = size.0.min(size.1) / SIZE as u32;
        let margin = 20;
        for (row, tile_row) in self.board.board.iter().enumerate() {
            for (col, piece) in tile_row.iter().enumerate() {
                let row = 7 - row;
                if let Some(piece) = piece {
                    let color = match piece.color {
                        ChessColor::White => Color::RED,
                        ChessColor::Black => Color::BLUE,
                    };
                    canvas.set_draw_color(color);
                    canvas
                        .fill_rect(Rect::new(
                            col as i32 * tile_size as i32 + margin,
                            row as i32 * tile_size as i32 + margin,
                            tile_size - margin as u32 * 2,
                            tile_size - margin as u32 * 2,
                        ))
                        .unwrap();
                }
            }
        }
    }

    pub fn handle_click(&mut self, row: u8, col: u8) {
        let pos = Position::new(row as i8, col as i8);

        // If we have an active tile, try to move
        if let Some(last_interact) = self.active_tile {
            // If move is successful, reset active tile
            if let Ok(()) = self.board.make_move(last_interact, pos) {
                if let Player::Bot(strat) = self.board.current_player() {
                    if let Some((from, to)) = strat.choose_move(&mut self.board) {
                        self.board
                            .make_move(from, to)
                            .expect("bot should't be able to choose an invalid move");
                    }
                }
                self.active_tile = None;
                return;
            }
        }

        // Check if we pressed a piece and in that case set that tile as the active tile
        if let Some(piece) = self.board.at(pos) {
            if piece.color == self.board.current_turn_color() {
                self.active_tile = Some(pos);
            }
        }
        // If we clicked an empty tile, clear active tile
        else {
            self.active_tile = None;
        }
    }

    pub fn handle_keypress(&mut self, keycode: Keycode) {
        match keycode {
            Keycode::U => {
                self.board.undo();
                self.active_tile = None;
            }
            Keycode::R => {
                self.board = Chessboard::default();
                self.active_tile = None;
            }
            _ => {}
        }
    }
}
