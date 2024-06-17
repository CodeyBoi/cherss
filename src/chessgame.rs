use crate::{
    chessboard,
    player::Player,
    tui::{Tile, TileStatus},
};
use crossterm::event::{KeyCode, MouseButton, MouseEvent, MouseEventKind};
use ratatui::{
    layout::{Constraint, Layout, Rect},
    Frame,
};
use sdl2::{
    keyboard::Keycode, pixels::Color, rect::Rect as SDLRect, render::Canvas, video::Window,
};

use crate::{
    chessboard::{Chessboard, Position, SIZE},
    piece::ChessColor,
};

pub struct ChessGame {
    board: Chessboard,
    active_tile: Option<Position>,
    pub bots_active: bool,
    pub is_running: bool,
    pub tile_size: u16,
}

impl ChessGame {
    pub fn new(board: Chessboard) -> Self {
        Self {
            board,
            active_tile: None,
            bots_active: true,
            is_running: true,
            tile_size: 8,
        }
    }

    pub fn render(&mut self, frame: &mut Frame) {
        let app_area = frame.size();
        let board_area = Rect {
            height: self.tile_size * SIZE as u16 / 2,
            width: self.tile_size * SIZE as u16,
            ..app_area
        };
        let binding =
            Layout::vertical([Constraint::Length(self.tile_size / 2); SIZE]).split(board_area);
        let rows = binding.iter().rev();
        let tiles = rows
            .flat_map(|row| {
                Layout::horizontal([Constraint::Length(self.tile_size); SIZE])
                    .split(*row)
                    .to_vec()
            })
            .collect::<Vec<_>>();

        let mut is_movable = [false; SIZE * SIZE];
        if let Some(active_tile) = self.active_tile {
            for pos in self.board.moves(active_tile) {
                let idx = (pos.row * SIZE as i8 + pos.col) as usize;
                is_movable[idx] = true;
            }
        }

        for (i, tile_area) in tiles.iter().enumerate() {
            let pos = Position::new((i / SIZE) as i8, (i % SIZE) as i8);
            let piece = self.board.at(pos);
            let status = if is_movable[i] {
                if self.board.at(pos).is_some() {
                    TileStatus::CanCaptureAt
                } else {
                    TileStatus::CanMoveTo
                }
            } else if Some(pos) == self.active_tile {
                TileStatus::Active
            } else {
                TileStatus::Default
            };
            let color = if (i + pos.row as usize) % 2 == 0 {
                ChessColor::Black
            } else {
                ChessColor::White
            };
            let tile = Tile {
                piece,
                status,
                color,
            };
            frame.render_widget(&tile, *tile_area);
        }
    }

    pub fn handle_key_press(&mut self, code: KeyCode) {
        use KeyCode as K;
        match code {
            K::Char('Q' | 'q') => {
                self.is_running = false;
            }
            K::Char('U' | 'u') => {
                self.active_tile = None;
                self.board.undo();
                self.board.undo();
            }
            K::Char('R' | 'r') => {
                self.board = Chessboard::default();
                self.active_tile = None;
            }
            K::Char('B' | 'b') => {
                self.bots_active = !self.bots_active;
            }
            _ => {}
        }
    }

    pub fn handle_mouse_event(&mut self, event: MouseEvent) {
        if let MouseEvent {
            kind: MouseEventKind::Down(MouseButton::Left),
            row: m_row,
            column: m_col,
            ..
        } = event
        {
            let (row, col) = (
                chessboard::SIZE as u16 - 1 - (m_row * 2 / self.tile_size),
                m_col / self.tile_size,
            );
            print!("{row},{col}\r");
            self.handle_click(row as u8, col as u8);
        }
    }

    pub fn set_tile_size(&mut self, size: u16) {
        self.tile_size = size;
    }

    pub fn _sdl_render(&mut self, canvas: &mut Canvas<Window>) {
        self._sdl_draw_chessboard(canvas);
        self._sdl_draw_highlights(canvas);
        self._sdl_draw_pieces(canvas);
    }

    fn _sdl_draw_chessboard(&self, canvas: &mut Canvas<Window>) {
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
                    .fill_rect(SDLRect::new(
                        col as i32 * tile_size as i32,
                        row as i32 * tile_size as i32,
                        tile_size,
                        tile_size,
                    ))
                    .unwrap();
            }
        }
    }

    fn _sdl_draw_highlights(&mut self, canvas: &mut Canvas<Window>) {
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
                    .fill_rect(SDLRect::new(
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
                .fill_rect(SDLRect::new(
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
                            .draw_rect(SDLRect::new(
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
                        .fill_rect(SDLRect::new(
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

    fn _sdl_draw_pieces(&self, canvas: &mut Canvas<Window>) {
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
                        .fill_rect(SDLRect::new(
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

    pub fn make_bot_move(&mut self) {
        if !self.bots_active {
            return;
        }

        if let Player::Bot(strat) = self.board.current_player() {
            if let Some((from, to)) = strat.choose_move(&mut self.board) {
                self.board
                    .make_move(from, to)
                    .expect("bot should't be able to choose an invalid move");
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
                        if let Err(e) = self.board.make_move(from, to) {
                            println!(
                                "error: bot tried making illegal move {}->{}: {:?}",
                                from, to, e,
                            );
                        }
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

    pub fn _sdl_handle_keypress(&mut self, keycode: Keycode) {
        match keycode {
            Keycode::U => {
                self.active_tile = None;
                self.board.undo();
                self.board.undo();
            }
            Keycode::R => {
                self.board = Chessboard::default();
                self.active_tile = None;
            }
            Keycode::B => {
                self.bots_active = !self.bots_active;
            }
            _ => {}
        }
    }
}
