use std::{
    fmt::Display,
    ops::{Add, Mul, Neg, Sub},
};

use crate::piece::{ChessColor, PieceType};

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Move(pub Position, pub Position);

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Position(pub u8);

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Coords {
    pub file: i8,
    pub rank: i8,
}

impl Position {
    pub const fn file(self) -> u8 {
        self.0 % 8
    }

    pub const fn rank(self) -> u8 {
        self.0 / 8
    }

    pub const fn to_coords(self) -> Coords {
        Coords::new(self.file() as i8, self.rank() as i8)
    }

    pub const fn is_in_bounds(self) -> bool {
        self.0 < 64
    }

    pub const fn translated(self, file_offset: i8, rank_offset: i8) -> Option<Self> {
        let new_file = self.file() as i8 + file_offset;
        if new_file < 0 || new_file > 7 {
            return None;
        }
        let offset = rank_offset * 8 + file_offset;
        Some(Self(self.0 + offset as u8))
    }

    pub fn translate_by_coords(self, offset: Coords) -> Option<Self> {
        let coords = self.to_coords() + offset;
        if coords.is_in_bounds() {
            Some(coords.to_pos())
        } else {
            None
        }
    }

    pub fn from_uci(uci: &str) -> Result<Self, ParseMoveError> {
        let mut chars = uci.chars();
        let col = chars.next().ok_or(ParseMoveError::InvalidLength)?;
        let col = if ('a'..='h').contains(&col) {
            col as u8 - b'a'
        } else {
            return Err(ParseMoveError::InvalidFile);
        };

        let row = chars.next().ok_or(ParseMoveError::InvalidLength)?;
        let row = if ('1'..='8').contains(&row) {
            row as u8 - b'1'
        } else {
            return Err(ParseMoveError::InvalidFile);
        };
        Ok(Position(row * 8 + col))
    }
}

impl Coords {
    pub const KNIGHT_OFFSETS: [Coords; 8] = [
        Coords::new(-1, -2),
        Coords::new(1, -2),
        Coords::new(-2, -1),
        Coords::new(2, -1),
        Coords::new(-2, 1),
        Coords::new(2, 1),
        Coords::new(-1, 2),
        Coords::new(1, 2),
    ];
    pub const DIAGONAL_OFFSETS: [Coords; 4] = [
        Coords::new(-1, -1),
        Coords::new(1, -1),
        Coords::new(-1, 1),
        Coords::new(1, 1),
    ];
    pub const CARDINAL_OFFSETS: [Coords; 4] = [
        Coords::new(0, -1),
        Coords::new(-1, 0),
        Coords::new(1, 0),
        Coords::new(0, 1),
    ];

    pub const fn new(file: i8, rank: i8) -> Self {
        Self { file, rank }
    }

    pub const fn is_in_bounds(self) -> bool {
        0 <= self.file && self.file < 8 && 0 <= self.rank && self.rank < 8
    }

    pub const fn tile_color(self) -> ChessColor {
        if (self.file + self.rank) % 2 == 0 {
            ChessColor::Black
        } else {
            ChessColor::White
        }
    }

    pub const fn to_pos(self) -> Position {
        Position((self.rank * 8 + self.file) as u8)
    }
}

impl Display for Coords {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut s = String::with_capacity(2);
        s.push((self.rank as u8 + b'a') as char);
        s.push((self.file as u8 + b'1') as char);
        f.write_str(&s)
    }
}

impl Add for Coords {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self::new(self.file + rhs.file, self.rank + rhs.rank)
    }
}

impl Sub for Coords {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Self::new(self.file - rhs.file, self.rank - rhs.rank)
    }
}

impl Mul<Coords> for i8 {
    type Output = Coords;

    fn mul(self, rhs: Coords) -> Self::Output {
        Coords::new(self * rhs.file, self * rhs.rank)
    }
}

impl Neg for Coords {
    type Output = Self;

    fn neg(self) -> Self::Output {
        Self::new(-self.file, -self.rank)
    }
}

pub enum ParseMoveError {
    InvalidLength,
    InvalidFile,
    InvalidRank,
    InvalidPromotion,
}

impl Move {
    fn from_uci(uci: &str) -> Result<Self, ParseMoveError> {
        let from = Position::from_uci(&uci[0..2])?;
        let to = Position::from_uci(&uci[0..2])?;
        Ok(Move(from, to))
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

pub trait Chess {
    fn make_move(&mut self, chess_move: Move) -> Result<(), ChessMoveError>;
    fn generate_moves(&self, moves: &mut Vec<Move>, piece: PieceType, color: ChessColor);
}