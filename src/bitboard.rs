use std::{
    fmt::{Display, Write},
    ops::{Deref, Index, IndexMut, Not},
};

use lazy_static::lazy_static;

use crate::{
    chess::{Chess, ChessMoveError, Coords, Dir, Move, Position},
    piece::{ChessColor, Piece, PieceType, PIECE_TYPES},
};

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct BitBoard(pub u64);

type AttackPatterns = [BitBoard; 64];

impl Index<Position> for AttackPatterns {
    type Output = BitBoard;

    fn index(&self, index: Position) -> &Self::Output {
        &self[index.0 as usize]
    }
}

lazy_static! {
    static ref RAYS_E: AttackPatterns = BitBoard::generate_attack_rays(Dir::E);
    static ref RAYS_NE: AttackPatterns = BitBoard::generate_attack_rays(Dir::NE);
    static ref RAYS_N: AttackPatterns = BitBoard::generate_attack_rays(Dir::N);
    static ref RAYS_NW: AttackPatterns = BitBoard::generate_attack_rays(Dir::NW);
    static ref RAYS_W: AttackPatterns = BitBoard::generate_attack_rays(Dir::W);
    static ref RAYS_SW: AttackPatterns = BitBoard::generate_attack_rays(Dir::SW);
    static ref RAYS_S: AttackPatterns = BitBoard::generate_attack_rays(Dir::S);
    static ref RAYS_SE: AttackPatterns = BitBoard::generate_attack_rays(Dir::SE);
}

impl BitBoard {
    const FULL: Self = Self(!0);

    const A_FILE: Self = Self(0x8080_8080_8080_8080);
    const B_FILE: Self = Self(0x4040_4040_4040_4040);
    const G_FILE: Self = Self(0x0202_0202_0202_0202);
    const H_FILE: Self = Self(0x0101_0101_0101_0101);

    const RANK_1: Self = Self(0xff00_0000_0000_0000);
    const RANK_2: Self = Self(0x00ff_0000_0000_0000);
    const RANK_7: Self = Self(0x0000_0000_0000_ff00);
    const RANK_8: Self = Self(0x0000_0000_0000_00ff);

    pub const fn zero() -> Self {
        Self(0)
    }

    pub const fn set(self, idx: u8) -> Self {
        Self(self.0 | (1 << idx))
    }

    pub const fn get(self, idx: u8) -> bool {
        self.0 & (1 << idx) != 0
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

    pub const fn clear_most_significant_set_bit(self) -> Self {
        Self(self.0 & !(1 << (63 - self.0.leading_zeros())))
    }

    fn generate_attack_ray(position: Position, direction: Coords) -> BitBoard {
        let mut board = BitBoard::zero();
        let mut pos = position.to_coords();
        loop {
            pos = pos + direction;
            if !pos.is_in_bounds() {
                break;
            }
            board = board.set(pos.to_pos().0);
        }
        board
    }

    fn generate_attack_rays(direction: Coords) -> AttackPatterns {
        let mut boards = [BitBoard::zero(); 64];
        for (i, board) in boards.iter_mut().enumerate() {
            *board = Self::generate_attack_ray(Position(i as u8), direction);
        }
        boards
    }
}

impl Not for BitBoard {
    type Output = Self;

    fn not(self) -> Self::Output {
        Self(!self.0)
    }
}

impl Display for BitBoard {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for i in 0..64 {
            if self.get(i) {
                f.write_char('1')?;
            } else {
                f.write_char('.')?;
            }
            f.write_char(' ')?;
            if i % 8 == 7 {
                f.write_char('\n')?;
            }
        }
        Ok(())
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

impl IndexMut<PieceType> for Pieces {
    fn index_mut(&mut self, index: PieceType) -> &mut Self::Output {
        &mut self.0[index as usize]
    }
}

impl Index<ChessColor> for ColorMaps {
    type Output = BitBoard;

    fn index(&self, index: ChessColor) -> &Self::Output {
        &self.0[index as usize]
    }
}

impl IndexMut<ChessColor> for ColorMaps {
    fn index_mut(&mut self, index: ChessColor) -> &mut Self::Output {
        &mut self.0[index as usize]
    }
}

pub struct Chessboard {
    pieces: Pieces,
    colors: ColorMaps,
}

impl Chessboard {
    pub fn get(&self, piece: PieceType, color: ChessColor) -> BitBoard {
        self.pieces[piece].intersection(self.colors[color])
    }

    pub fn piece_at(&self, pos: Position) -> Option<Piece> {
        let mask = pos.to_mask();
        let color = if self.colors[ChessColor::White].intersection(mask).0 != 0 {
            ChessColor::White
        } else if self.colors[ChessColor::Black].intersection(mask).0 != 0 {
            ChessColor::Black
        } else {
            return None;
        };

        let piece = PIECE_TYPES
            .iter()
            .find(|&&piece| self.pieces[piece].intersection(mask).0 != 0)?;

        Some(Piece::new(color, *piece))
    }

    fn occupancy(&self) -> BitBoard {
        self.colors[ChessColor::White].union(self.colors[ChessColor::Black])
    }

    fn make_move(&mut self, Move(from, to): Move) -> Result<(), ChessMoveError> {
        let piece = self.piece_at(from).ok_or(ChessMoveError::MissingPiece)?;
        let mut legal_moves = Vec::new();
        self.generate_moves_by_piece(&mut legal_moves, piece.piece, piece.color);

        if !legal_moves.iter().any(|&Move(f, t)| f == from && t == to) {
            return Err(ChessMoveError::IllegalMove);
        }

        self.move_piece(Move(from, to))?;

        Ok(())
    }

    fn move_piece(&mut self, Move(from, to): Move) -> Result<(), ChessMoveError> {
        if !from.is_in_bounds() || !to.is_in_bounds() {
            return Err(ChessMoveError::OutOfBounds);
        }

        match (self.piece_at(from), self.piece_at(to)) {
            (Some(piece), capture) => {
                if capture.is_some_and(|o| o.color == piece.color) {
                    return Err(ChessMoveError::SameColorCapture);
                }

                self.pieces[piece.piece] = self.pieces[piece.piece].clear(from.0).set(to.0);
                self.colors[piece.color] = self.colors[piece.color].clear(from.0).set(to.0);

                if let Some(cap) = capture {
                    self.pieces[cap.piece] = self.pieces[cap.piece].clear(to.0);
                    self.colors[cap.color] = self.colors[cap.color].clear(to.0);
                }

                Ok(())
            }
            (None, _) => Err(ChessMoveError::MissingPiece),
        }
    }

    fn generate_moves_from(&self, moves: &mut Vec<Move>, origin: Position) {
        let Piece { color, piece } = match self.piece_at(origin) {
            Some(p) => p,
            None => return,
        };
        self._generate_moves(moves, piece, color, origin.to_mask());
    }

    fn generate_moves_by_piece(&self, moves: &mut Vec<Move>, piece: PieceType, color: ChessColor) {
        let piece_map = self.get(piece, color);
        self._generate_moves(moves, piece, color, piece_map);
    }

    fn _generate_moves(
        &self,
        moves: &mut Vec<Move>,
        piece: PieceType,
        color: ChessColor,
        piece_map: BitBoard,
    ) {
        use PieceType as P;
        match piece {
            P::Pawn => {
                self.generate_pawn_moves(moves, piece_map, color);
            }
            P::Knight => {
                self.generate_knight_moves(moves, piece_map, color);
            }
            P::Bishop => {
                self.generate_bishop_moves(moves, piece_map, color);
            }
            P::Rook => {
                self.generate_rook_moves(moves, piece_map, color);
            }
            P::Queen => {
                self.generate_queen_moves(moves, piece_map, color);
            }
            P::King => {
                self.generate_king_moves(moves, piece_map, color);
            }
        };
    }

    fn add_moves(moves: &mut Vec<Move>, offset: Coords, positions: &[Position]) {
        for pos in positions {
            let origin = pos
                .translate_by_coords(-offset)
                .expect("`add_moves` assumes no horizontal wrapping");
            moves.push(Move(origin, *pos));
        }
    }

    fn add_moves_with_origin(moves: &mut Vec<Move>, origin: Position, positions: &[Position]) {
        for pos in positions {
            moves.push(Move(origin, *pos));
        }
    }

    fn generate_pawn_moves(&self, moves: &mut Vec<Move>, pieces: BitBoard, color: ChessColor) {
        // We can only capture opposing pieces
        let opposing_pieces = self.colors[!color];

        let (rank_offset, double_move_mask) = match color {
            ChessColor::White => (Dir::N_OFFSET, BitBoard::RANK_2),
            ChessColor::Black => (Dir::S_OFFSET, BitBoard::RANK_7),
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

        let capture_offset = Coords::new(Dir::W_OFFSET, rank_offset);
        let west_captures = pieces
            // Filter out pawns which would wrap horizontally
            .intersection(!BitBoard::A_FILE)
            .translated(capture_offset)
            // We can only capture on squares with an opposing piece
            .intersection(opposing_pieces);
        Self::add_moves(moves, capture_offset, &west_captures.serialized());

        // Same reasoning as above
        let capture_offset = Coords::new(Dir::E_OFFSET, rank_offset);
        let east_captures = pieces
            .intersection(!BitBoard::H_FILE)
            .translated(capture_offset)
            .intersection(opposing_pieces);
        Self::add_moves(moves, capture_offset, &east_captures.serialized());
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

    fn generate_sliding_attack_board(
        &self,
        rays: &[BitBoard],
        is_positive_direction: bool,
    ) -> BitBoard {
        let occupancy = self.occupancy();

        let mut possible_moves = BitBoard::zero();
        for ray_pattern in rays {
            let blockers = ray_pattern.intersection(occupancy);
            let blockers_past_first = if is_positive_direction {
                blockers.clear_least_significant_set_bit()
            } else {
                blockers.clear_most_significant_set_bit()
            };
            possible_moves = possible_moves.union(ray_pattern.intersection(!blockers_past_first));
        }
        possible_moves
    }

    fn generate_sliding_moves(
        &self,
        moves: &mut Vec<Move>,
        pieces: BitBoard,
        color: ChessColor,
        attack_patterns: &[&AttackPatterns],
        is_positive_direction: bool,
    ) {
        let valid_tiles = !self.colors[color];
        for piece in pieces.serialized() {
            let rays = attack_patterns
                .iter()
                .map(|rays| rays[piece])
                .collect::<Vec<_>>();
            let attack_board =
                self.generate_sliding_attack_board(rays.as_slice(), is_positive_direction);
            let possible_moves = attack_board.intersection(valid_tiles);
            Self::add_moves_with_origin(moves, piece, &possible_moves.serialized());
        }
    }

    fn generate_bishop_moves(&self, moves: &mut Vec<Move>, pieces: BitBoard, color: ChessColor) {
        self.generate_sliding_moves(
            moves,
            pieces,
            color,
            [RAYS_NE.deref(), RAYS_NW.deref()].as_slice(),
            true,
        );
        self.generate_sliding_moves(
            moves,
            pieces,
            color,
            [RAYS_SE.deref(), RAYS_SW.deref()].as_slice(),
            false,
        );
    }

    fn generate_rook_moves(&self, moves: &mut Vec<Move>, pieces: BitBoard, color: ChessColor) {
        self.generate_sliding_moves(
            moves,
            pieces,
            color,
            [RAYS_N.deref(), RAYS_E.deref()].as_slice(),
            true,
        );
        self.generate_sliding_moves(
            moves,
            pieces,
            color,
            [RAYS_S.deref(), RAYS_W.deref()].as_slice(),
            false,
        );
    }

    fn generate_queen_moves(&self, moves: &mut Vec<Move>, pieces: BitBoard, color: ChessColor) {
        self.generate_bishop_moves(moves, pieces, color);
        self.generate_rook_moves(moves, pieces, color);
    }

    fn generate_king_moves(&self, moves: &mut Vec<Move>, piece: BitBoard, color: ChessColor) {
        // We cannot move to or capture our own pieces
        let valid_tiles = !self.colors[color];

        for offset in [Dir::CARDINAL, Dir::DIAGONAL].concat() {
            // Filter out moves which will go OOB horisontally (we don't have to worry about vertical
            // OOB as they will be shifted away)
            let mask = match offset.file {
                Dir::W_OFFSET => !BitBoard::A_FILE,
                Dir::E_OFFSET => !BitBoard::H_FILE,
                _ => BitBoard::FULL,
            };
            let possible_moves = piece
                .intersection(mask)
                .translated(offset)
                .intersection(valid_tiles);

            // TODO: Filter moves which put king in check

            Self::add_moves(moves, offset, &possible_moves.serialized());
        }
    }
}

impl Default for Chessboard {
    fn default() -> Self {
        use BitBoard as B;
        const PAWNS: B = B::RANK_2.union(B::RANK_7);
        const KNIGHTS: B = B::zero().set(1).set(6).set(57).set(62);
        const BISHOPS: B = B::zero().set(2).set(5).set(58).set(61);
        const ROOKS: B = B::zero().set(0).set(7).set(56).set(63);
        const QUEENS: B = B::zero().set(3).set(59);
        const KINGS: B = B::zero().set(4).set(60);

        const WHITE: B = B::RANK_1.union(B::RANK_2);
        const BLACK: B = B::RANK_7.union(B::RANK_8);

        let mut pieces = Pieces([BitBoard::zero(); 6]);
        pieces[PieceType::Pawn] = PAWNS;
        pieces[PieceType::Knight] = KNIGHTS;
        pieces[PieceType::Bishop] = BISHOPS;
        pieces[PieceType::Rook] = ROOKS;
        pieces[PieceType::Queen] = QUEENS;
        pieces[PieceType::King] = KINGS;

        let mut colors = ColorMaps([BitBoard::zero(); 2]);
        colors[ChessColor::White] = WHITE;
        colors[ChessColor::Black] = BLACK;

        Self { pieces, colors }
    }
}

impl Chess for Chessboard {
    fn make_move(&mut self, chess_move: Move) -> Result<(), ChessMoveError> {
        self.make_move(chess_move)
    }

    fn generate_moves_from(&self, moves: &mut Vec<Move>, origin: Position) {
        self.generate_moves_from(moves, origin);
    }

    fn generate_moves_by_piece(&self, moves: &mut Vec<Move>, piece: PieceType, color: ChessColor) {
        self.generate_moves_by_piece(moves, piece, color);
    }
}
