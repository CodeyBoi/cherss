use std::{fmt::Display, str::FromStr};

use crate::{
    chessgame::ChessGame,
    piece::{ChessColor, Piece, PieceType},
    player::{BotStrategy, Player},
};

pub const SIZE: usize = 8;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Position {
    pub row: i8,
    pub col: i8,
}

#[derive(Debug)]
pub struct ChessMove {
    pub from: Position,
    pub to: Position,
    pub captured_piece: Option<Piece>,
    pub en_passant_capture_pos: Option<Position>,
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
    color: ChessColor,
}

impl CastlingRight {
    const STANDARD: [Self; 4] = [
        Self {
            side: CastlingSide::KingSide,
            color: ChessColor::White,
        },
        Self {
            side: CastlingSide::QueenSide,
            color: ChessColor::White,
        },
        Self {
            side: CastlingSide::KingSide,
            color: ChessColor::Black,
        },
        Self {
            side: CastlingSide::QueenSide,
            color: ChessColor::Black,
        },
    ];

    pub fn new(color: ChessColor, side: CastlingSide) -> Self {
        Self { color, side }
    }
}

pub struct Chessboard {
    pub board: [[Option<Piece>; SIZE]; SIZE],
    pub history: Vec<ChessMove>,
    first_turn_color: ChessColor,
    castling_rights: Vec<CastlingRight>,
    en_passant_target: Option<Position>,
    start_turn_number: u32,
    white_king: Position,
    white_player: Player,
    black_king: Position,
    black_player: Player,
}

impl Chessboard {
    pub fn new() -> Self {
        Self {
            board: [[None; SIZE]; SIZE],
            history: Vec::new(),
            first_turn_color: ChessColor::White,
            castling_rights: CastlingRight::STANDARD.to_vec(),
            en_passant_target: None,
            start_turn_number: 0,
            white_king: Position::new(0, 0),
            white_player: Player::Human,
            black_king: Position::new(0, 0),
            black_player: Player::Bot(BotStrategy::Random),
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

        let (mut row, mut col) = (SIZE - 1, 0);
        for c in fen_board.chars() {
            if let Some(d) = c.to_digit(10) {
                for _ in 0..d {
                    self.board[row][col] = None;
                    col += 1;
                }
            } else if let Some(piece) = Piece::from_fen(c) {
                if piece.piece == PieceType::King {
                    match piece.color {
                        ChessColor::White => self.white_king = Position::new(row as i8, col as i8),
                        ChessColor::Black => self.black_king = Position::new(row as i8, col as i8),
                    }
                }
                self.board[row][col] = Some(piece);
                col += 1;
            } else if c == '/' {
                (row, col) = (row - 1, 0);
            } else {
                panic!("Invalid character in FEN string: '{}'", c);
            }
        }

        self.first_turn_color = match input.next() {
            Some(current_turn) => match current_turn {
                "w" => ChessColor::White,
                "b" => ChessColor::Black,
                _ => return Err(ParseFENError::InvalidTurn),
            },
            None => return Err(ParseFENError::IncompleteString),
        };

        self.castling_rights = input
            .next()
            .unwrap()
            .chars()
            .filter_map(|c| match c {
                'K' => Some(CastlingRight::new(
                    ChessColor::White,
                    CastlingSide::KingSide,
                )),
                'Q' => Some(CastlingRight::new(
                    ChessColor::White,
                    CastlingSide::QueenSide,
                )),
                'k' => Some(CastlingRight::new(
                    ChessColor::Black,
                    CastlingSide::KingSide,
                )),
                'q' => Some(CastlingRight::new(
                    ChessColor::Black,
                    CastlingSide::QueenSide,
                )),
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
            ChessColor::White => 'w',
            ChessColor::Black => 'b',
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
                    ChessColor::White => c,
                    ChessColor::Black => c.to_ascii_lowercase(),
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
        ChessGame::new(self)
    }

    pub fn make_move(&mut self, from: Position, to: Position) -> Result<(), ChessMoveError> {
        let legal_moves = self.moves(from);
        if !legal_moves.iter().any(|m| *m == to) {
            return Err(ChessMoveError::IllegalMove);
        }
        self.make_move_unchecked(from, to)
    }

    pub(crate) fn make_move_unchecked(
        &mut self,
        from: Position,
        to: Position,
    ) -> Result<(), ChessMoveError> {
        // TODO: Add promotion
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

                let mut captured_piece = other;
                let mut en_passant_pos = None;

                // Update king positions
                if piece.piece == PieceType::King {
                    match piece.color {
                        ChessColor::White => self.white_king = to,
                        ChessColor::Black => self.black_king = to,
                    }
                }

                let row_offset = match piece.color {
                    ChessColor::White => -1,
                    ChessColor::Black => 1,
                };

                // Remove opposing pawn if en passant capture was made
                if let Some(en_passant_target) = self.en_passant_target {
                    // If a pawn got to en passant target then move must have been en passant
                    if piece.piece == PieceType::Pawn && to == en_passant_target {
                        let pos = Position::new(to.row + row_offset, to.col);
                        captured_piece = self.at(pos);
                        en_passant_pos = Some(pos);
                        self.board[pos.row as usize][pos.col as usize] = None;
                    }
                }

                // Update en passant target
                self.en_passant_target =
                    if piece.piece == PieceType::Pawn && from.row.abs_diff(to.row) == 2 {
                        Some(Position::new(to.row + row_offset, to.col))
                    } else {
                        None
                    };

                self.board[to.row as usize][to.col as usize] = Some(piece);
                self.board[from.row as usize][from.col as usize] = None;
                self.history.push(ChessMove {
                    from,
                    to,
                    captured_piece,
                    en_passant_capture_pos: en_passant_pos,
                });

                Ok(())
            }
            (None, _) => Err(ChessMoveError::MissingPiece),
        }
    }

    pub fn pieces(&self) -> Vec<(Piece, Position)> {
        self.board
            .iter()
            .enumerate()
            .flat_map(|(row, row_pieces)| {
                row_pieces
                    .iter()
                    .enumerate()
                    .filter_map(move |(col, piece)| {
                        piece
                            .as_ref()
                            .map(|piece| (*piece, Position::new(row as i8, col as i8)))
                    })
            })
            .collect::<Vec<_>>()
    }

    pub fn all_moves(&mut self) -> Vec<(Position, Position)> {
        let current_turn = self.current_turn_color();
        self.pieces()
            .iter()
            .flat_map(|&(piece, pos)| {
                self.moves(pos)
                    .iter()
                    .filter_map(|&m| {
                        if piece.color == current_turn {
                            Some((pos, m))
                        } else {
                            None
                        }
                    })
                    .collect::<Vec<_>>()
            })
            .collect::<Vec<_>>()
    }

    pub fn turn_number(&self) -> u32 {
        self.moves_made() / 2
    }

    fn moves_made(&self) -> u32 {
        self.start_turn_number * 2
            + (self.first_turn_color == ChessColor::Black) as u32
            + self.history.len() as u32
    }

    pub fn current_turn_color(&self) -> ChessColor {
        if self.moves_made() % 2 == 0 {
            ChessColor::White
        } else {
            ChessColor::Black
        }
    }

    pub fn current_player(&self) -> Player {
        match self.current_turn_color() {
            ChessColor::White => self.white_player,
            ChessColor::Black => self.black_player,
        }
    }

    pub fn undo(&mut self) {
        let Some(ChessMove {
            from,
            to,
            captured_piece,
            en_passant_capture_pos: en_passant_pos,
        }) = self.history.pop()
        else {
            // No moves have been made, do nothing
            return;
        };

        self.board[from.row as usize][from.col as usize] = self.at(to);
        self.board[to.row as usize][to.col as usize] = None;
        let return_pos = en_passant_pos.unwrap_or(to);
        self.board[return_pos.row as usize][return_pos.col as usize] = captured_piece;

        if let Some(piece) = self.at(from) {
            // Update king position
            if piece.piece == PieceType::King {
                match piece.color {
                    ChessColor::White => self.white_king = from,
                    ChessColor::Black => self.black_king = from,
                }
            }
        }

        // Update en passant target
        self.en_passant_target = if let Some(last_move) = self.history.last() {
            if let Some(piece) = self.at(last_move.to) {
                if piece.piece == PieceType::Pawn
                    && last_move.from.row.abs_diff(last_move.to.row) == 2
                {
                    let row_offset = match piece.color {
                        ChessColor::White => -1,
                        ChessColor::Black => 1,
                    };
                    Some(Position::new(
                        last_move.to.row + row_offset,
                        last_move.to.col,
                    ))
                } else {
                    None
                }
            } else {
                None
            }
        } else {
            None
        };
    }

    pub fn at(&self, pos: Position) -> Option<Piece> {
        if pos.in_bounds() {
            self.board[pos.row as usize][pos.col as usize]
        } else {
            None
        }
    }

    fn is_king_in_check(&mut self, color: ChessColor) -> bool {
        let pos = match color {
            ChessColor::White => self.white_king,
            ChessColor::Black => self.black_king,
        };

        if self
            .rook_moves(color, pos)
            .iter()
            .any(|m| match self.at(*m) {
                Some(piece) => {
                    piece.color != color
                        && (piece.piece == PieceType::Rook || piece.piece == PieceType::Queen)
                }
                None => false,
            })
        {
            return true;
        }

        if self
            .bishop_moves(color, pos)
            .iter()
            .any(|m| match self.at(*m) {
                Some(piece) => {
                    piece.color != color
                        && (piece.piece == PieceType::Bishop || piece.piece == PieceType::Queen)
                }
                None => false,
            })
        {
            return true;
        }

        if self
            .pawn_moves(color, pos)
            .iter()
            .any(|m| match self.at(*m) {
                Some(piece) => piece.color != color && piece.piece == PieceType::Pawn,
                None => false,
            })
        {
            return true;
        }

        if self
            .knight_moves(color, pos)
            .iter()
            .any(|m| match self.at(*m) {
                Some(piece) => piece.color != color && piece.piece == PieceType::Knight,
                None => false,
            })
        {
            return true;
        }

        if self
            .king_moves(color, pos)
            .iter()
            .any(|m| match self.at(*m) {
                Some(piece) => piece.color != color && piece.piece == PieceType::King,
                None => false,
            })
        {
            return true;
        }

        false
    }

    fn try_move(&mut self, from: Position, to: Position) -> bool {
        let current_turn = self.current_turn_color();
        self.make_move_unchecked(from, to).unwrap();
        let ret = !self.is_king_in_check(current_turn);
        self.undo();
        ret
    }

    pub fn moves(&mut self, from: Position) -> Vec<Position> {
        self.generate_moves(from)
            .iter()
            .cloned()
            .filter(|&to| self.try_move(from, to))
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

    fn pawn_moves(&self, color: ChessColor, pos: Position) -> Vec<Position> {
        let row_offset = match color {
            ChessColor::White => 1,
            ChessColor::Black => -1,
        };
        let mut moves = Vec::new();
        let new_pos = Position::new(pos.row + row_offset, pos.col);
        if self.at(new_pos).is_none() {
            moves.push(new_pos);

            // Check for first move
            let has_moved = (color == ChessColor::White && pos.row != 1)
                || (color == ChessColor::Black && pos.row != 6);
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
            else if let Some(pos) = self.en_passant_target {
                if pos == new_pos {
                    moves.push(new_pos);
                }
            }
        }
        moves
    }

    const DIAGONAL_DIRECTIONS: [(i8, i8); 4] = [(1, 1), (1, -1), (-1, 1), (-1, -1)];
    const CARDINAL_DIRECTIONS: [(i8, i8); 4] = [(0, 1), (1, 0), (0, -1), (-1, 0)];
    const KNIGHT_MOVES: [(i8, i8); 8] = [
        (-1, -2),
        (1, -2),
        (-2, -1),
        (2, -1),
        (-2, 1),
        (2, 1),
        (-1, 2),
        (1, 2),
    ];

    fn knight_moves(&self, color: ChessColor, pos: Position) -> Vec<Position> {
        self.moves_with_offsets(color, pos, Self::KNIGHT_MOVES.to_vec())
    }

    fn bishop_moves(&self, color: ChessColor, pos: Position) -> Vec<Position> {
        self.moves_with_directions(color, pos, Self::DIAGONAL_DIRECTIONS.to_vec())
    }

    fn rook_moves(&self, color: ChessColor, pos: Position) -> Vec<Position> {
        self.moves_with_directions(color, pos, Self::CARDINAL_DIRECTIONS.to_vec())
    }

    fn queen_moves(&self, color: ChessColor, pos: Position) -> Vec<Position> {
        self.moves_with_directions(
            color,
            pos,
            [Self::CARDINAL_DIRECTIONS, Self::DIAGONAL_DIRECTIONS].concat(),
        )
    }

    fn king_moves(&self, color: ChessColor, pos: Position) -> Vec<Position> {
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
        color: ChessColor,
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
        color: ChessColor,
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
