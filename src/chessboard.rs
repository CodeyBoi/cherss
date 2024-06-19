use core::panic;
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
    pub const fn new(row: i8, col: i8) -> Self {
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
    GameHasEnded,
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
pub struct CastlingRights {
    queen_side: bool,
    king_side: bool,
}

impl CastlingRights {
    pub fn new() -> Self {
        Self {
            queen_side: false,
            king_side: false,
        }
    }
}

impl Default for CastlingRights {
    fn default() -> Self {
        Self {
            queen_side: true,
            king_side: true,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum ChessResult {
    Undecided,
    Draw,
    Win(ChessColor),
}

pub struct Chessboard {
    pub board: [[Option<Piece>; SIZE]; SIZE],
    pub history: Vec<ChessMove>,
    first_turn_color: ChessColor,
    white_initial_castling_rights: CastlingRights,
    black_initial_castling_rights: CastlingRights,
    en_passant_target: Option<Position>,
    start_turn_number: u32,
    white_king: Position,
    white_player: Player,
    black_king: Position,
    black_player: Player,
    halfmove_clock: u8,
    result: ChessResult,
}

impl Chessboard {
    pub fn new() -> Self {
        Self {
            board: [[None; SIZE]; SIZE],
            history: Vec::new(),
            first_turn_color: ChessColor::White,
            white_initial_castling_rights: CastlingRights::default(),
            black_initial_castling_rights: CastlingRights::default(),
            en_passant_target: None,
            start_turn_number: 0,
            white_king: Position::new(0, 0),
            white_player: Player::Human,
            black_king: Position::new(0, 0),
            black_player: Player::Human,
            halfmove_clock: 0,
            result: ChessResult::Undecided,
        }
    }

    pub fn with_players(white_player: Player, black_player: Player) -> Self {
        Self {
            white_player,
            black_player,
            ..Default::default()
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

        (
            self.white_initial_castling_rights,
            self.black_initial_castling_rights,
        ) = {
            let mut crs = (CastlingRights::new(), CastlingRights::new());
            for c in input.next().unwrap().chars() {
                match c {
                    'Q' => crs.0.queen_side = true,
                    'K' => crs.0.king_side = true,
                    'q' => crs.1.queen_side = true,
                    'k' => crs.1.king_side = true,
                    _ => {}
                }
            }
            crs
        };

        self.en_passant_target = Position::from_str(input.next().unwrap()).ok();

        self.halfmove_clock = input
            .next()
            .unwrap()
            .parse()
            .expect("Halfmove clock should consist of digits");

        self.start_turn_number = input
            .next()
            .unwrap()
            .parse()
            .expect("Fullmove number should consist of digits");

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

        let castling_string = {
            let mut s = String::new();
            if self.white_initial_castling_rights.king_side {
                s.push('K');
            }
            if self.white_initial_castling_rights.queen_side {
                s.push('Q');
            }
            if self.black_initial_castling_rights.king_side {
                s.push('k');
            }
            if self.black_initial_castling_rights.queen_side {
                s.push('q');
            }
            s
        };
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

        fen.push_str(&format!("{} ", self.halfmove_clock)); // halfmove clock

        fen.push_str(self.turn_number().to_string().as_str());

        fen
    }

    pub fn into_game(self) -> ChessGame {
        ChessGame::new(self)
    }

    pub fn make_move(&mut self, from: Position, to: Position) -> Result<(), ChessMoveError> {
        if self.result != ChessResult::Undecided {
            return Err(ChessMoveError::GameHasEnded);
        }

        let legal_moves = self.moves(from);
        if !legal_moves.iter().any(|m| *m == to) {
            return Err(ChessMoveError::IllegalMove);
        }
        self.move_piece(from, to)?;

        // Reset halfmove clock if piece captured or pawn advanced
        if let Some(ChessMove {
            captured_piece: Some(_),
            ..
        }) = self.history.last()
        {
            self.halfmove_clock = 0;
        } else if self
            .at(to)
            .is_some_and(|piece| piece.piece == PieceType::Pawn)
        {
            self.halfmove_clock = 0;
        } else {
            self.halfmove_clock += 1;
        }

        if self.halfmove_clock >= 50 {
            self.result = ChessResult::Draw;
        }

        // Check for checkmate
        let current_turn = self.current_turn_color();
        if self.all_moves().is_empty() {
            if self.is_king_in_check(current_turn) {
                self.result = ChessResult::Win(current_turn);
            } else {
                self.result = ChessResult::Draw;
            }
        }

        Ok(())
    }

    pub(crate) fn move_piece(
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
                if piece.color == current_turn {
                    self.moves(pos).iter().map(|&m| (pos, m)).collect()
                } else {
                    Vec::new()
                }
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

    fn castling_rights(&self) -> (CastlingRights, CastlingRights) {
        const WHITE_KING_START: Position = Position::new(0, 4);
        const WHITE_LROOK_START: Position = Position::new(0, 0);
        const WHITE_RROOK_START: Position = Position::new(0, 7);
        const BLACK_KING_START: Position = Position::new(7, 4);
        const BLACK_LROOK_START: Position = Position::new(7, 0);
        const BLACK_RROOK_START: Position = Position::new(7, 7);

        let mut crs = (
            self.white_initial_castling_rights,
            self.black_initial_castling_rights,
        );
        for ChessMove { from, to, .. } in self.history.iter() {
            if *from == WHITE_KING_START {
                crs.0 = CastlingRights::new();
            } else if *from == WHITE_LROOK_START || *to == WHITE_LROOK_START {
                crs.0.queen_side = false;
            } else if *from == WHITE_RROOK_START || *to == WHITE_RROOK_START {
                crs.0.king_side = false;
            } else if *from == BLACK_KING_START {
                crs.1 = CastlingRights::new();
            } else if *from == BLACK_LROOK_START || *to == BLACK_LROOK_START {
                crs.1.queen_side = false;
            } else if *from == BLACK_RROOK_START || *to == BLACK_RROOK_START {
                crs.1.king_side = false;
            }
        }
        crs
    }

    pub fn at(&self, pos: Position) -> Option<Piece> {
        if pos.in_bounds() {
            self.board[pos.row as usize][pos.col as usize]
        } else {
            None
        }
    }

    fn is_attacked(&self, pos: Position, color: ChessColor) -> bool {
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

    fn is_king_in_check(&self, color: ChessColor) -> bool {
        let pos = match color {
            ChessColor::White => self.white_king,
            ChessColor::Black => self.black_king,
        };
        self.is_attacked(pos, color)
    }

    fn try_move(&mut self, from: Position, to: Position) -> bool {
        let current_turn = self.current_turn_color();

        match self.move_piece(from, to) {
            Ok(_) => {}
            // This means the move is invalid
            Err(_) => return false,
        };
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
        self.moves_with_offsets(color, pos, Self::KNIGHT_MOVES.as_slice())
    }

    fn bishop_moves(&self, color: ChessColor, pos: Position) -> Vec<Position> {
        self.moves_with_directions(color, pos, Self::DIAGONAL_DIRECTIONS.as_slice())
    }

    fn rook_moves(&self, color: ChessColor, pos: Position) -> Vec<Position> {
        self.moves_with_directions(color, pos, Self::CARDINAL_DIRECTIONS.as_slice())
    }

    fn queen_moves(&self, color: ChessColor, pos: Position) -> Vec<Position> {
        self.moves_with_directions(
            color,
            pos,
            [Self::CARDINAL_DIRECTIONS, Self::DIAGONAL_DIRECTIONS]
                .concat()
                .as_slice(),
        )
    }

    fn king_moves(&self, color: ChessColor, pos: Position) -> Vec<Position> {
        let mut moves = self.moves_with_offsets(
            color,
            pos,
            [
                (0, 1),
                (1, 0),
                (0, -1),
                (-1, 0),
                (1, 1),
                (1, -1),
                (-1, 1),
                (-1, -1),
            ]
            .as_slice(),
        );

        // Check if castling is possible
        let color = self.current_turn_color();
        let castling_rights = match color {
            ChessColor::White => self.castling_rights().0,
            ChessColor::Black => self.castling_rights().1,
        };
        if castling_rights.queen_side
            && (1..=3).all(|i| {
                let new_pos = Position::new(pos.row, pos.col - i);
                self.at(new_pos).is_none() && (!self.is_attacked(new_pos, color) || i == 3)
            })
        {
            moves.push(Position::new(pos.row, pos.col - 2));
        }

        if castling_rights.king_side
            && (1..=2).all(|i| {
                let new_pos = Position::new(pos.row, pos.col + i);
                self.at(new_pos).is_none() && !self.is_attacked(new_pos, color)
            })
        {
            moves.push(Position::new(pos.row, pos.col + 2));
        }

        moves
    }

    fn moves_with_directions(
        &self,
        color: ChessColor,
        pos: Position,
        directions: &[(i8, i8)],
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
        offsets: &[(i8, i8)],
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
