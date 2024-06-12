use std::{fmt::Display, str::FromStr};

use sdl2::{
    keyboard::Keycode, pixels::Color as SDLColor, rect::Rect, render::Canvas, video::Window,
};

use crate::piece::{Color, Piece, PieceType};

pub const SIZE: usize = 8;

pub struct ChessGame {
    board: Chessboard,
    active_tile: Option<Position>,
}

impl ChessGame {
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
                    SDLColor::RGB(240, 217, 181)
                } else {
                    SDLColor::RGB(181, 136, 99)
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
                    SDLColor::RGB(205, 210, 106)
                } else {
                    SDLColor::RGB(170, 162, 58)
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
            let color = SDLColor::RGBA(55, 180, 55, 75);
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
                        Color::White => SDLColor::RED,
                        Color::Black => SDLColor::BLUE,
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
                self.active_tile = None;
                println!("{}", self.board.to_fen());
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

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Position {
    row: i8,
    col: i8,
}

pub struct ChessMove {
    from: Position,
    to: Position,
    captured_piece: Option<Piece>,
}

impl Position {
    pub fn new(row: i8, col: i8) -> Self {
        Self { row, col }
    }

    pub fn in_bounds(&self) -> bool {
        0 <= self.row
            && self.row < SIZE.try_into().unwrap()
            && 0 <= self.col
            && self.col < SIZE.try_into().unwrap()
    }
}

impl Display for Position {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut s = String::with_capacity(2);
        s.push((self.col as u8 + b'a') as char);
        s.push((self.row as u8 + b'1') as char);
        f.write_str(&s)
    }
}

#[derive(Debug)]
pub enum ChessMoveError {
    OutOfBounds,
    MissingPiece,
    SameColorCapture,
    IllegalMove,
}

#[derive(Debug)]
pub enum ParseFENError {
    IncompleteString,
    InvalidTurn,
}

#[derive(Clone, Copy)]
pub enum CastlingSide {
    KingSide,
    QueenSide,
}

#[derive(Clone, Copy)]
pub struct CastlingRight {
    side: CastlingSide,
    color: Color,
}

impl CastlingRight {
    const STANDARD: [Self; 4] = [
        Self {
            side: CastlingSide::KingSide,
            color: Color::White,
        },
        Self {
            side: CastlingSide::QueenSide,
            color: Color::White,
        },
        Self {
            side: CastlingSide::KingSide,
            color: Color::Black,
        },
        Self {
            side: CastlingSide::QueenSide,
            color: Color::Black,
        },
    ];

    pub fn new(color: Color, side: CastlingSide) -> Self {
        Self { color, side }
    }
}

pub struct Chessboard {
    pub board: [[Option<Piece>; SIZE]; SIZE],
    history: Vec<ChessMove>,
    first_turn_color: Color,
    castling_rights: Vec<CastlingRight>,
    en_passant_target: Option<Position>,
    start_turn_number: u32,
}

impl Chessboard {
    pub fn new() -> Self {
        Self {
            board: [[None; SIZE]; SIZE],
            history: Vec::new(),
            first_turn_color: Color::White,
            castling_rights: CastlingRight::STANDARD.to_vec(),
            en_passant_target: None,
            start_turn_number: 0,
        }
    }

    pub fn from_fen(fen: &str) -> Self {
        let mut board = Self::new();
        board.load_fen(fen).unwrap();
        board
    }

    pub fn load_fen(&mut self, fen: &str) -> Result<(), ParseFENError> {
        let mut input = fen.split_ascii_whitespace();
        let fen_board = input.next().expect("FEN string should contain text");

        let (mut row, mut col) = (7, 0);
        for c in fen_board.chars() {
            if let Some(d) = c.to_digit(10) {
                for _ in 0..d {
                    self.board[row][col] = None;
                    col += 1;
                }
            } else if let piece @ Some(_) = Piece::from_fen(c) {
                self.board[row][col] = piece;
                col += 1;
            } else if c == '/' {
                (row, col) = (row - 1, 0);
            } else {
                panic!("Invalid character in FEN string: '{}'", c);
            }
        }

        self.first_turn_color = match input.next() {
            Some(current_turn) => match current_turn {
                "w" => Color::White,
                "b" => Color::Black,
                _ => return Err(ParseFENError::InvalidTurn),
            },
            None => return Err(ParseFENError::IncompleteString),
        };

        self.castling_rights = input
            .next()
            .unwrap()
            .chars()
            .filter_map(|c| match c {
                'K' => Some(CastlingRight::new(Color::White, CastlingSide::KingSide)),
                'Q' => Some(CastlingRight::new(Color::White, CastlingSide::QueenSide)),
                'k' => Some(CastlingRight::new(Color::Black, CastlingSide::KingSide)),
                'q' => Some(CastlingRight::new(Color::Black, CastlingSide::QueenSide)),
                _ => None,
            })
            .collect::<Vec<_>>();

        self.en_passant_target = Position::from_str(input.next().unwrap()).ok();

        input.next(); // discard halfmove clock

        self.start_turn_number = input
            .next()
            .unwrap()
            .parse()
            .expect("Fullmove number could not be parsed");

        Ok(())
    }

    pub fn to_fen(&self) -> String {
        let mut fen = String::new();
        let mut empty_tiles = 0;
        for row in self.board.iter().rev() {
            for piece in row {
                if let Some(piece) = piece {
                    if empty_tiles > 0 {
                        fen.push_str(&empty_tiles.to_string());
                        empty_tiles = 0;
                    }
                    fen.push(piece.to_fen());
                } else {
                    empty_tiles += 1;
                }
            }
            if empty_tiles > 0 {
                fen.push_str(&empty_tiles.to_string());
                empty_tiles = 0;
            }
            fen.push('/');
        }
        fen.pop(); // pop last slash char
        fen.push(' ');

        fen.push(match self.current_turn_color() {
            Color::White => 'w',
            Color::Black => 'b',
        });
        fen.push(' ');

        let castling_string = self
            .castling_rights
            .iter()
            .map(|cr| {
                let c = match cr.side {
                    CastlingSide::KingSide => 'K',
                    CastlingSide::QueenSide => 'Q',
                };
                match cr.color {
                    Color::White => c,
                    Color::Black => c.to_ascii_lowercase(),
                }
            })
            .collect::<String>();
        if castling_string.is_empty() {
            fen.push('-');
        } else {
            fen.push_str(&castling_string);
        }
        fen.push(' ');

        if let Some(pos) = self.en_passant_target {
            fen.push_str(pos.to_string().as_str());
        } else {
            fen.push('-');
        }
        fen.push(' ');

        fen.push_str("0 "); // halfmove clock

        fen.push_str(self.turn_number().to_string().as_str());

        fen
    }

    pub fn into_game(self) -> ChessGame {
        ChessGame {
            board: self,
            active_tile: None,
        }
    }

    pub fn make_move(&mut self, from: Position, to: Position) -> Result<(), ChessMoveError> {
        let legal_moves = self.moves(from);
        if !legal_moves.iter().any(|m| *m == to) {
            return Err(ChessMoveError::IllegalMove);
        }
        self.make_move_unchecked(from, to)
    }

    fn make_move_unchecked(&mut self, from: Position, to: Position) -> Result<(), ChessMoveError> {
        if !from.in_bounds() || !to.in_bounds() {
            return Err(ChessMoveError::OutOfBounds);
        }

        match (self.at(from), self.at(to)) {
            (Some(piece), other) => {
                if let Some(other_piece) = other {
                    if piece.color == other_piece.color {
                        return Err(ChessMoveError::SameColorCapture);
                    }
                }

                self.board[to.row as usize][to.col as usize] = Some(piece);
                self.board[from.row as usize][from.col as usize] = None;
                self.history.push(ChessMove {
                    from,
                    to,
                    captured_piece: other,
                });

                Ok(())
            }
            (None, _) => Err(ChessMoveError::MissingPiece),
        }
    }

    pub fn turn_number(&self) -> u32 {
        self.moves_made() / 2
    }

    fn moves_made(&self) -> u32 {
        self.start_turn_number * 2
            + (self.first_turn_color == Color::Black) as u32
            + self.history.len() as u32
    }

    pub fn current_turn_color(&self) -> Color {
        if self.moves_made() % 2 == 0 {
            Color::White
        } else {
            Color::Black
        }
    }

    pub fn undo(&mut self) {
        let Some(ChessMove {
            from,
            to,
            captured_piece,
        }) = self.history.pop()
        else {
            // No moves have been made, do nothing
            return;
        };
        self.board[from.row as usize][from.col as usize] = self.at(to);
        self.board[to.row as usize][to.col as usize] = captured_piece;
    }

    pub fn at(&self, pos: Position) -> Option<Piece> {
        if pos.in_bounds() {
            self.board[pos.row as usize][pos.col as usize]
        } else {
            None
        }
    }

    fn is_move_valid(&mut self, from: Position, to: Position) -> bool {
        let current_turn = self.current_turn_color();
        if self.make_move_unchecked(from, to).is_err() {
            return false;
        }

        // Check if any opposing pieces can capture the king
        for (row, row_tiles) in self.board.iter().enumerate() {
            for (col, piece) in row_tiles.iter().enumerate() {
                if piece.is_none() {
                    continue;
                }
                if let Some(Piece { color, .. }) = piece {
                    if *color == current_turn {
                        // Current piece is the same color as the piece that just moved, skip
                        continue;
                    }
                }

                // Check if any possible move can capture the king
                let pos = Position::new(row as i8, col as i8);
                for mov in &self.generate_moves(pos) {
                    if let Some(Piece {
                        color,
                        piece: PieceType::King,
                    }) = self.at(*mov)
                    {
                        if color == current_turn {
                            self.undo();
                            return false;
                        }
                    }
                }
            }
        }

        self.undo();
        true
    }

    pub fn moves(&mut self, pos: Position) -> Vec<Position> {
        self.generate_moves(pos)
            .iter()
            .cloned()
            .filter(|to| self.is_move_valid(pos, *to))
            .collect()
    }

    fn generate_moves(&self, pos: Position) -> Vec<Position> {
        use PieceType as P;
        match self.at(pos) {
            Some(piece) => match piece.piece {
                P::Pawn => self.pawn_moves(piece.color, pos),
                P::Knight => self.knight_moves(piece.color, pos),
                P::Bishop => self.bishop_moves(piece.color, pos),
                P::Rook => self.rook_moves(piece.color, pos),
                P::Queen => self.queen_moves(piece.color, pos),
                P::King => self.king_moves(piece.color, pos),
            },
            None => Vec::new(),
        }
    }

    fn pawn_moves(&self, color: Color, pos: Position) -> Vec<Position> {
        // TODO: Add en passant and promotion
        let row_offset = match color {
            Color::White => 1,
            Color::Black => -1,
        };
        let mut moves = Vec::new();
        let new_pos = Position::new(pos.row + row_offset, pos.col);
        if self.at(new_pos).is_none() {
            moves.push(new_pos);

            // Check for first move
            let has_moved =
                (color == Color::White && pos.row != 1) || (color == Color::Black && pos.row != 6);
            let new_pos = Position::new(pos.row + row_offset * 2, pos.col);
            if !has_moved && self.at(new_pos).is_none() {
                moves.push(new_pos);
            }
        }

        // Check for possible captures
        for col_offset in [-1, 1] {
            let new_pos = Position::new(pos.row + row_offset, pos.col + col_offset);
            if let Some(piece) = self.at(new_pos) {
                if piece.color != color {
                    moves.push(new_pos);
                }
            }
            // Check for en passant
            else if let Some(m) = self.history.last() {
                let above = Position::new(new_pos.row + 1, new_pos.col);
                let below = Position::new(new_pos.row - 1, new_pos.col);

                match (color, self.at(above), self.at(below)) {
                    (
                        Color::White,
                        None,
                        Some(Piece {
                            color: Color::Black,
                            piece: PieceType::Pawn,
                        }),
                    ) => {
                        if m.from == above && m.to == below {
                            moves.push(new_pos);
                        }
                    }
                    (
                        Color::Black,
                        Some(Piece {
                            color: Color::White,
                            piece: PieceType::Pawn,
                        }),
                        None,
                    ) => {
                        if m.from == below && m.to == above {
                            moves.push(new_pos);
                        }
                    }
                    _ => {}
                }
            }
        }
        moves
    }

    fn knight_moves(&self, color: Color, pos: Position) -> Vec<Position> {
        self.moves_with_offsets(
            color,
            pos,
            vec![
                (-1, -2),
                (1, -2),
                (-2, -1),
                (2, -1),
                (-2, 1),
                (2, 1),
                (-1, 2),
                (1, 2),
            ],
        )
    }

    fn bishop_moves(&self, color: Color, pos: Position) -> Vec<Position> {
        self.moves_with_directions(color, pos, vec![(1, 1), (1, -1), (-1, 1), (-1, -1)])
    }

    fn rook_moves(&self, color: Color, pos: Position) -> Vec<Position> {
        self.moves_with_directions(color, pos, vec![(0, 1), (1, 0), (0, -1), (-1, 0)])
    }

    fn queen_moves(&self, color: Color, pos: Position) -> Vec<Position> {
        self.moves_with_directions(
            color,
            pos,
            vec![
                (0, 1),
                (1, 0),
                (0, -1),
                (-1, 0),
                (1, 1),
                (1, -1),
                (-1, 1),
                (-1, -1),
            ],
        )
    }

    fn king_moves(&self, color: Color, pos: Position) -> Vec<Position> {
        // TODO: Add castling

        self.moves_with_offsets(
            color,
            pos,
            vec![
                (0, 1),
                (1, 0),
                (0, -1),
                (-1, 0),
                (1, 1),
                (1, -1),
                (-1, 1),
                (-1, -1),
            ],
        )
    }

    fn moves_with_directions(
        &self,
        color: Color,
        pos: Position,
        directions: Vec<(i8, i8)>,
    ) -> Vec<Position> {
        let mut moves = Vec::new();
        for (d_row, d_col) in directions {
            for i in 1.. {
                let new_pos = Position::new(pos.row + d_row * i, pos.col + d_col * i);

                if !new_pos.in_bounds() {
                    break;
                }

                match self.at(new_pos) {
                    Some(Piece {
                        color: other_color, ..
                    }) => {
                        if color != other_color {
                            moves.push(new_pos);
                        }
                        break;
                    }
                    None => moves.push(new_pos),
                }
            }
        }
        moves
    }

    fn moves_with_offsets(
        &self,
        color: Color,
        pos: Position,
        offsets: Vec<(i8, i8)>,
    ) -> Vec<Position> {
        let mut moves = Vec::new();
        for (d_row, d_col) in offsets {
            let new_pos = Position::new(pos.row + d_row, pos.col + d_col);

            if !new_pos.in_bounds() {
                continue;
            }

            match self.at(new_pos) {
                Some(Piece {
                    color: other_color, ..
                }) => {
                    if color != other_color {
                        moves.push(new_pos);
                    }
                }
                None => moves.push(new_pos),
            }
        }
        moves
    }
}

#[derive(Debug)]
pub struct ParsePositionError;

impl FromStr for Position {
    type Err = ParsePositionError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let lower = s.trim().to_lowercase();

        if lower.len() != 2 {
            return Err(ParsePositionError);
        }

        let mut chars = lower.chars();
        let (Some(file), Some(rank)) = (chars.next(), chars.next()) else {
            return Err(ParsePositionError);
        };

        if !('a'..='h').contains(&file) {
            return Err(ParsePositionError);
        }

        if !('1'..='8').contains(&rank) {
            return Err(ParsePositionError);
        }

        let col = file as u8 - b'a';
        let row = rank as u8 - b'1';
        Ok(Self::new(row as i8, col as i8))
    }
}

impl Default for Chessboard {
    fn default() -> Self {
        Self::from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 0")
    }
}
