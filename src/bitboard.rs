use std::ops::{Index, Not};

use crate::{
    chess::{Chess, ChessMoveError, Coords, Move, Position},
    piece::{ChessColor, PieceType},
};

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct BitBoard(pub u64);

impl BitBoard {
    const FULL: Self = Self(!0);

    const A_FILE: Self = Self(0x8080_8080_8080_8080);
    const B_FILE: Self = Self(0x4040_4040_4040_4040);
    const G_FILE: Self = Self(0x0202_0202_0202_0202);
    const H_FILE: Self = Self(0x0101_0101_0101_0101);

    const RANK_2: Self = Self(0x00ff_0000_0000_0000);
    const RANK_7: Self = Self(0x0000_0000_0000_ff00);

    pub const fn set(self, idx: u8) -> Self {
        Self(self.0 | (1 << idx))
    }

    pub const fn clear(self, idx: u8) -> Self {
        Self(self.0 & !(1 << idx))
    }

    pub const fn union(self, rhs: Self) -> Self {
        Self(self.0 | rhs.0)
    }

    pub const fn intersection(self, rhs: Self) -> Self {
        Self(self.0 & rhs.0)
    }

    pub fn serialize_into(self, v: &mut Vec<Position>) {
        let mut value = self.0;
        while value != 0 {
            v.push(Position(self.0.trailing_zeros() as u8));
            value = self.clear_least_significant_set_bit().0;
        }
    }

    pub fn serialized(self) -> Vec<Position> {
        let mut v = Vec::new();
        self.serialize_into(&mut v);
        v
    }

    pub const fn translated(self, offset: Coords) -> Self {
        let shift_offset = offset.rank * 8 + offset.file;
        if shift_offset > 0 {
            Self(self.0 >> shift_offset)
        } else {
            Self(self.0 << -shift_offset)
        }
    }

    pub const fn isolate_least_significant_set_bit(self) -> Self {
        Self(self.0 & self.0.wrapping_neg())
    }

    pub const fn clear_least_significant_set_bit(self) -> Self {
        Self(self.0 & (self.0.wrapping_sub(1)))
    }
}

struct Pieces([BitBoard; 6]);
struct ColorMaps([BitBoard; 2]);

impl Index<PieceType> for Pieces {
    type Output = BitBoard;

    fn index(&self, index: PieceType) -> &Self::Output {
        &self.0[index as usize]
    }
}

impl Index<ChessColor> for ColorMaps {
    type Output = BitBoard;

    fn index(&self, index: ChessColor) -> &Self::Output {
        &self.0[index as usize]
    }
}

impl Not for BitBoard {
    type Output = Self;

    fn not(self) -> Self::Output {
        Self(!self.0)
    }
}

struct Chessboard {
    pieces: Pieces,
    colors: ColorMaps,
}

impl Chessboard {
    pub fn get(&self, piece: PieceType, color: ChessColor) -> BitBoard {
        self.pieces[piece].intersection(self.colors[color])
    }

    fn make_move(&mut self, chess_move: Move) -> Result<(), ChessMoveError> {
        todo!()
    }

    fn generate_moves(&self, moves: &mut Vec<Move>, piece: PieceType, color: ChessColor) {
        use PieceType as P;
        let pieces = self.get(piece, color);
        match piece {
            P::Pawn => {
                self.generate_pawn_moves(moves, pieces, color);
            }
            P::Knight => {
                self.generate_knight_moves(moves, pieces, color);
            }
            P::Bishop => {
                self.generate_bishop_moves(moves, pieces, color);
            }
            P::Rook => {
                self.generate_rook_moves(moves, pieces, color);
            }
            P::Queen => {
                self.generate_queen_moves(moves, pieces, color);
            }
            P::King => {
                self.generate_king_moves(moves, pieces, color);
            }
        };
    }

    fn add_moves(moves: &mut Vec<Move>, offset: Coords, positions: &[Position]) {
        for pos in positions {
            let origin_pos = pos
                .translate_by_coords(-offset)
                .expect("`add_moves` assumes no horizontal wrapping");
            moves.push(Move(origin_pos, *pos));
        }
    }

    fn generate_pawn_moves(&self, moves: &mut Vec<Move>, pieces: BitBoard, color: ChessColor) {
        // We can only capture opposing pieces
        let opposing_pieces = self.colors[!color];

        let (rank_offset, double_move_mask) = match color {
            ChessColor::White => (1, BitBoard::RANK_2),
            ChessColor::Black => (-1, BitBoard::RANK_7),
        };

        // We cannot move into any other pieces (except when capturing)
        let valid_moves = !self.colors[color].union(opposing_pieces);
        let offset_dir = Coords::new(0, rank_offset);
        let move_one_tile = pieces.translated(offset_dir).intersection(valid_moves);
        Self::add_moves(moves, offset_dir, &move_one_tile.serialized());

        // Start checking from `move_one_tile` to check that no pieces are blocking
        let move_two_tiles = move_one_tile
            .translated(offset_dir)
            .intersection(valid_moves)
            // Filter out pieces which didn't start at the correct rank
            .intersection(double_move_mask.translated(2 * offset_dir));
        Self::add_moves(moves, 2 * offset_dir, &move_two_tiles.serialized());

        let capture_offset = Coords::new(-1, rank_offset);
        let left_captures = pieces
            // Filter out pawns which will wrap horizontally
            .intersection(!BitBoard::A_FILE)
            .translated(capture_offset)
            // We can only capture on squares with an opposing piece
            .intersection(opposing_pieces);
        Self::add_moves(moves, capture_offset, &left_captures.serialized());

        // Same reasoning as above
        let capture_offset = Coords::new(1, rank_offset);
        let right_captures = pieces
            .intersection(!BitBoard::H_FILE)
            .translated(capture_offset)
            .intersection(opposing_pieces);
        Self::add_moves(moves, capture_offset, &right_captures.serialized());
    }

    fn generate_knight_moves(&self, moves: &mut Vec<Move>, pieces: BitBoard, color: ChessColor) {
        // We cannot move to or capture our own pieces
        let valid_tiles = !self.colors[color];

        for offset in Coords::KNIGHT_OFFSETS {
            // Filter out moves which will go OOB horisontally (we don't have to worry about vertical
            // OOB as they will be shifted away)
            let mask = match offset.file {
                -2 => !(BitBoard::A_FILE.union(BitBoard::B_FILE)),
                -1 => !BitBoard::A_FILE,
                1 => !BitBoard::H_FILE,
                2 => !(BitBoard::G_FILE.union(BitBoard::H_FILE)),
                _ => BitBoard::FULL,
            };
            let possible_moves = pieces
                .intersection(mask)
                .translated(offset)
                .intersection(valid_tiles);
            Self::add_moves(moves, offset, &possible_moves.serialized());
        }
    }

    fn generate_bishop_moves(&self, moves: &mut Vec<Move>, pieces: BitBoard, color: ChessColor) {
        todo!()
    }

    fn generate_rook_moves(&self, moves: &mut Vec<Move>, pieces: BitBoard, color: ChessColor) {
        todo!()
    }

    fn generate_queen_moves(&self, moves: &mut Vec<Move>, pieces: BitBoard, color: ChessColor) {
        todo!()
    }

    fn generate_king_moves(&self, moves: &mut Vec<Move>, pieces: BitBoard, color: ChessColor) {
        todo!()
    }
}

impl Chess for Chessboard {
    fn make_move(&mut self, chess_move: Move) -> Result<(), ChessMoveError> {
        self.make_move(chess_move)
    }

    fn generate_moves(&self, moves: &mut Vec<Move>, piece: PieceType, color: ChessColor) {
        self.generate_moves(moves, piece, color);
    }
}
