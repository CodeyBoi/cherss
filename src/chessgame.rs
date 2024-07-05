use crate::{
    chess::{Chess, Coords, Move, Position},
    chessboard::ChessResult,
    player::Player,
    tui::{Tile, TileStatus},
};
use crossterm::event::{KeyCode, MouseButton, MouseEvent, MouseEventKind};
use ratatui::{
    layout::{Constraint, Layout, Rect},
    widgets::Widget,
};

use crate::chessboard::SIZE;

pub struct ChessGame {
    board: Box<dyn Chess>,
    active_tile: Option<Position>,
    pub bots_active: bool,
    pub is_running: bool,
    pub tile_size: u16,
}

impl Widget for &mut ChessGame {
    fn render(self, area: Rect, buf: &mut ratatui::prelude::Buffer)
    where
        Self: Sized,
    {
        let rows = Layout::vertical([Constraint::Length(self.tile_size / 2); SIZE]).split(area);
        let tiles = rows
            .iter()
            .rev()
            .flat_map(|row| {
                Layout::horizontal([Constraint::Length(self.tile_size); SIZE])
                    .split(*row)
                    .to_vec()
            })
            .collect::<Vec<_>>();

        let is_movable = {
            let mut m = [false; SIZE * SIZE];
            if let Some(active_tile) = self.active_tile {
                for Move(_, to) in self.board.moves_from(active_tile) {
                    m[to.0 as usize] = true;
                }
            }
            m
        };

        for (i, (tile_area, &is_movable)) in tiles.iter().zip(is_movable.iter()).enumerate() {
            let pos = Position(i as u8);
            let piece = self.board.piece_at(pos);
            let status = if is_movable {
                if self.board.piece_at(pos).is_some() {
                    TileStatus::CanCaptureAt
                } else {
                    TileStatus::CanMoveTo
                }
            } else if Some(pos) == self.active_tile {
                TileStatus::Active
            } else {
                TileStatus::Default
            };
            let color = pos.tile_color();
            let tile = Tile {
                piece,
                status,
                color,
            };
            tile.render(*tile_area, buf);
        }
    }
}

impl ChessGame {
    pub fn new(board: Box<dyn Chess>) -> Self {
        Self {
            board,
            active_tile: None,
            bots_active: false,
            is_running: true,
            tile_size: 10,
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
                if let ChessResult::Undecided = self.board.game_result() {
                    self.board.undo();
                }
                self.board.undo();
            }
            K::Char('R' | 'r') => {
                // self.board =  default().board;
                self.active_tile = None;
            }
            K::Char('B' | 'b') => {
                self.bots_active = !self.bots_active;
            }
            K::Char('T' | 't') => {
                self.make_bot_move();
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
                SIZE as u16 - 1 - (m_row * 2 / self.tile_size),
                // m_row * 2 / self.tile_size,
                m_col / self.tile_size,
            );
            let (row, col) = (row as u8, col as u8);

            if row < SIZE as u8 && col < SIZE as u8 {
                let pos = Coords::new(col as i8, row as i8).to_pos();
                self.handle_click(pos);
            }
        }
    }

    pub fn set_tile_size(&mut self, size: u16) {
        self.tile_size = size;
    }

    pub fn make_bot_move(&mut self) {
        if let Player::Bot(strat) = self.board.current_player() {
            if let Some(Move(from, to)) = strat.choose_move(&self.board) {
                if let Err(e) = self.board.make_move(Move(from, to)) {
                    println!(
                        "error: bot tried making illegal move {}->{}: {:?}",
                        from, to, e,
                    );
                }
            }
        }
    }

    pub fn handle_click(&mut self, pos: Position) {
        println!("{}", pos);

        // If we have an active tile, try to move
        if let Some(last_interact) = self.active_tile {
            // If move is successful, reset active tile
            if self.board.make_move(Move(last_interact, pos)).is_ok() {
                self.active_tile = None;
                return;
            }
        }

        // Check if we pressed a piece and in that case set that tile as the active tile
        if let Some(piece) = self.board.piece_at(pos) {
            if piece.color == self.board.current_turn() {
                self.active_tile = Some(pos);
            }
        }
        // If we clicked an empty tile, clear active tile
        else {
            self.active_tile = None;
        }
    }
}
