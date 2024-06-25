use std::ops::Not;

use lazy_static::lazy_static;

use crate::piece::{ChessColor, Piece};

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct BitBoard(u64);

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Position(u8);

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct ChessMove(u8, u8);

lazy_static! {
    static ref BIT_INDEX_LOOKUP: Vec<Vec<u8>> = {
        let mut idxs = Vec::new();
        for i in 0..16 {
            let mut inner_idxs: Vec<u8> = Vec::new();
            for j in 0..4 {
                let mask = 1 << j;
                if i & mask != 0 {
                    inner_idxs.push(3 - j);
                }
            }
            idxs.push(inner_idxs);
        }
        idxs
    };
}

impl BitBoard {
    pub fn set(self, idx: u8) -> Self {
        Self(self.0 | (1 << idx))
    }

    pub fn clear(self, idx: u8) -> Self {
        Self(self.0 & !(1 << idx))
    }

    pub fn union(self, rhs: Self) -> Self {
        Self(self.0 | rhs.0)
    }

    pub fn intersection(self, rhs: Self) -> Self {
        Self(self.0 & rhs.0)
    }

    pub fn serialize(self) -> Vec<u8> {
        let mut idxs = Vec::new();
        for i in (0..16).rev() {
            let mask = 0xf << (15 - i);
            let lookup_idx = self.0 & mask;
            let idx_offset = i * 4;
            for idx in &BIT_INDEX_LOOKUP[lookup_idx as usize] {
                idxs.push(idx + idx_offset);
            }
        }
        idxs
    }
}

impl Not for BitBoard {
    type Output = Self;

    fn not(self) -> Self::Output {
        Self(!self.0)
    }
}

struct Chessboard {
    pieces: [BitBoard; 6],
    colors: [BitBoard; 2],
}

impl Chessboard {
    pub fn get(&self, piece: Piece) -> BitBoard {
        let idx = piece.piece as usize;
        self.pieces[idx].intersection(self.get_color_map(piece.color))
    }

    fn get_color_map(&self, color: ChessColor) -> BitBoard {
        self.colors[color as usize]
    }

    fn generate_moves(&self, pos: Position, color: ChessColor) -> Vec<Position> {
        let valid_move_tiles = !self.get_color_map(color);

        todo!()
    }
}
