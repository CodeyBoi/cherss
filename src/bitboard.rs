use core::fmt;
use std::{
    fmt::{Display, Write},
    ops::{Deref, Index, IndexMut, Not},
};

use lazy_static::lazy_static;

use crate::{
    chess::{Chess, ChessMoveError, Coords, Dir, Move, Position},
    chessboard::ChessResult,
    chessgame::ChessGame,
    piece::{ChessColor, Piece, PieceType, PIECE_TYPES},
    player::Player,
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

impl Deref for BitBoard {
    type Target = u64;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl fmt::Debug for BitBoard {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fmt::Display::fmt(&self, f)
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
    static ref ATTACK_LOOKUP: [[u8; 8]; 64] = BitBoard::generate_attack_lookups();
}

impl BitBoard {
    const FULL: Self = Self(!0);

    const FILE_A: Self = Self(0x0101_0101_0101_0101);
    const FILE_B: Self = Self(0x0202_0202_0202_0202);
    const FILE_G: Self = Self(0x4040_4040_4040_4040);
    const FILE_H: Self = Self(0x8080_8080_8080_8080);

    const RANK_1: Self = Self(0x0000_0000_0000_00ff);
    const RANK_2: Self = Self(0x0000_0000_0000_ff00);
    const RANK_7: Self = Self(0x00ff_0000_0000_0000);
    const RANK_8: Self = Self(0xff00_0000_0000_0000);

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
        let mut board = self;
        while board != BitBoard::zero() {
            v.push(Position(board.0.trailing_zeros() as u8));
            board = board.with_lsb_cleared();
        }
    }

    pub fn serialized(self) -> Vec<Position> {
        let mut v = Vec::new();
        self.serialize_into(&mut v);
        v
    }

    pub fn translated(self, offset: Coords) -> Self {
        // Filter out cells which would wrap horizontally
        let mask = {
            let cols = (0..offset.file.abs()).fold(Self::zero(), |acc, i| {
                acc.union(BitBoard(Self::FILE_A.0 << i))
            });
            if offset.file > 0 {
                !BitBoard(cols.0 << (8 - offset.file))
            } else {
                !cols
            }
        };
        let shift_offset = offset.rank * 8 + offset.file;
        if shift_offset > 0 {
            Self(self.intersection(mask).0 << shift_offset)
        } else {
            Self(self.intersection(mask).0 >> -shift_offset)
        }
    }

    pub const fn isolate_lsb(self) -> Self {
        Self(self.0 & self.0.wrapping_neg())
    }

    pub const fn with_lsb_cleared(self) -> Self {
        Self(self.0 & (self.0.wrapping_sub(1)))
    }

    pub const fn with_msb_cleared(self) -> Self {
        if self.0 == 0 {
            self
        } else {
            Self(self.0 & !(1 << (63 - self.0.leading_zeros())))
        }
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

    // Index into this attack lookup is the middle 6 bits of the rank (or file). The edges don't matter, as there are no tiles behind them.
    fn generate_attack_lookups() -> [[u8; 8]; 64] {
        let mut table = [[0; 8]; 64];
        for occupancy in 0u8..64 {
            for pos in 0u8..8 {
                table[occupancy as usize][pos as usize] =
                    Self::generate_attack_lookup(occupancy, pos);
            }
        }
        table
    }

    fn generate_attack_lookup(occupancy: u8, piece_pos: u8) -> u8 {
        // Occupancy at the edges don't matter, as there are no tiles behind them.
        let occupancy = occupancy << 1;
        let mut res = 0x0;
        // Look east
        for pos in piece_pos + 1..8 {
            let mask = 1 << pos;
            res |= mask;
            if occupancy & mask != 0 {
                break;
            }
        }

        // Look west
        for pos in (0..piece_pos).rev() {
            let mask = 1 << pos;
            res |= mask;
            if occupancy & mask != 0 {
                break;
            }
        }

        eprintln!("           {}V", " ".repeat(7 - piece_pos as usize));
        eprintln!("occupancy: {:08b}\n   attack: {:08b}", occupancy, res);
        res
    }

    fn from_pos_list(cells: &[Position]) -> Self {
        cells.iter().fold(Self::zero(), |acc, pos| acc.set(pos.0))
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
        f.write_char('\n')?;
        for rank in (0..8).rev() {
            for file in 0..8 {
                if self.get(rank * 8 + file) {
                    f.write_char('X')?;
                } else {
                    f.write_char('·')?;
                }
                f.write_char(' ')?;
            }
            f.write_char('\n')?;
        }
        Ok(())
    }
}

struct Pieces([BitBoard; 6]);
struct ColorMaps([BitBoard; 2]);

impl Pieces {
    pub fn new() -> Self {
        Self([BitBoard::zero(); 6])
    }
}

impl ColorMaps {
    pub fn new() -> Self {
        Self([BitBoard::zero(); 2])
    }
}

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
    history: Vec<Move>,
    result: ChessResult,
    white_player: Player,
    black_player: Player,
    current_turn: ChessColor,
}

impl Chessboard {
    pub fn get(&self, piece: PieceType, color: ChessColor) -> BitBoard {
        self.pieces[piece].intersection(self.colors[color])
    }

    pub fn with_players(white_player: Player, black_player: Player) -> Self {
        Self {
            white_player,
            black_player,
            ..Default::default()
        }
    }

    pub fn with_pieces(initial_pieces: &[(Position, Piece)]) -> Self {
        let mut colors = ColorMaps::new();
        let mut pieces = Pieces::new();

        for (position, piece) in initial_pieces {
            colors[piece.color] = colors[piece.color].set(position.0);
            pieces[piece.piece] = pieces[piece.piece].set(position.0);
        }

        Self {
            pieces,
            colors,
            ..Default::default()
        }
    }

    pub fn into_game(self) -> ChessGame {
        ChessGame::new(Box::new(self))
    }

    fn occupancy(&self) -> BitBoard {
        self.colors[ChessColor::White].union(self.colors[ChessColor::Black])
    }

    pub fn make_move(&mut self, Move(from, to): Move) -> Result<(), ChessMoveError> {
        BitBoard::generate_attack_lookups();
        let Piece { color, piece } = self.piece_at(from).ok_or(ChessMoveError::MissingPiece)?;

        let legal_moves = self.moves_by_piece(piece, color);

        if !legal_moves.contains(&Move(from, to)) {
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

                if let Some(cap) = capture {
                    self.pieces[cap.piece] = self.pieces[cap.piece].clear(to.0);
                    self.colors[cap.color] = self.colors[cap.color].clear(to.0);
                }

                self.pieces[piece.piece] = self.pieces[piece.piece].clear(from.0).set(to.0);
                self.colors[piece.color] = self.colors[piece.color].clear(from.0).set(to.0);

                Ok(())
            }
            (None, _) => Err(ChessMoveError::MissingPiece),
        }
    }

    fn generate_moves_by_piece(&self, moves: &mut Vec<Move>, piece: PieceType, color: ChessColor) {
        let piece_map = self.get(piece, color);
        self.generate_moves(moves, piece, color, piece_map);
    }

    fn generate_moves(
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
        let valid_moves = !self.occupancy();
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
            .intersection(!BitBoard::FILE_A)
            .translated(capture_offset)
            // We can only capture on squares with an opposing piece
            .intersection(opposing_pieces);
        Self::add_moves(moves, capture_offset, &west_captures.serialized());

        // Same reasoning as above
        let capture_offset = Coords::new(Dir::E_OFFSET, rank_offset);
        let east_captures = pieces
            .intersection(!BitBoard::FILE_H)
            .translated(capture_offset)
            .intersection(opposing_pieces);
        Self::add_moves(moves, capture_offset, &east_captures.serialized());
    }

    fn generate_knight_moves(&self, moves: &mut Vec<Move>, pieces: BitBoard, color: ChessColor) {
        // We cannot move to or capture our own pieces
        let valid_tiles = !self.colors[color];

        for offset in Coords::KNIGHT_OFFSETS {
            let possible_moves = pieces.translated(offset).intersection(valid_tiles);
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
                blockers.with_lsb_cleared()
            } else {
                blockers.with_msb_cleared()
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
                Dir::W_OFFSET => !BitBoard::FILE_A,
                Dir::E_OFFSET => !BitBoard::FILE_H,
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

        let pieces = {
            let mut p = Pieces([BitBoard::zero(); 6]);
            p[PieceType::Pawn] = PAWNS;
            p[PieceType::Knight] = KNIGHTS;
            p[PieceType::Bishop] = BISHOPS;
            p[PieceType::Rook] = ROOKS;
            p[PieceType::Queen] = QUEENS;
            p[PieceType::King] = KINGS;
            p
        };

        let colors = {
            let mut c = ColorMaps([BitBoard::zero(); 2]);
            c[ChessColor::White] = WHITE;
            c[ChessColor::Black] = BLACK;
            c
        };

        Self {
            pieces,
            colors,
            history: Vec::new(),
            result: ChessResult::Undecided,
            white_player: Player::Human,
            black_player: Player::Bot(crate::player::BotStrategy::Random),
            current_turn: ChessColor::White,
        }
    }
}

impl Chess for Chessboard {
    fn make_move(&mut self, chess_move: Move) -> Result<(), ChessMoveError> {
        let ret = self.make_move(chess_move);
        if ret.is_ok() {
            self.current_turn = !self.current_turn;
        }
        ret
    }

    fn moves_from(&self, origin: Position) -> Vec<Position> {
        let Piece { color, piece } = match self.piece_at(origin) {
            Some(p) => p,
            None => return Vec::new(),
        };
        let mut moves = Vec::new();
        if origin.is_in_bounds() {
            self.generate_moves(&mut moves, piece, color, origin.to_mask());
        }
        moves.iter().map(|m| m.1).collect()
    }

    fn moves_by_piece(&self, piece: PieceType, color: ChessColor) -> Vec<Move> {
        let mut moves = Vec::new();
        let piece_map = self.get(piece, color);
        self.generate_moves(&mut moves, piece, color, piece_map);
        moves
    }

    fn all_moves(&self) -> Vec<Move> {
        let color = self.current_turn();
        let mut moves = Vec::new();
        for piece in PIECE_TYPES {
            self.generate_moves_by_piece(&mut moves, piece, color);
        }
        moves
    }

    fn piece_at(&self, pos: Position) -> Option<Piece> {
        if !pos.is_in_bounds() {
            return None;
        }

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

    fn game_result(&self) -> ChessResult {
        self.result
    }

    fn current_turn(&self) -> ChessColor {
        self.current_turn
    }

    fn current_player(&self) -> Player {
        if self.current_turn() == ChessColor::White {
            self.white_player
        } else {
            self.black_player
        }
    }

    fn history(&self) -> &[Move] {
        self.history.as_slice()
    }

    fn undo(&mut self) {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use Position as P;

    #[test]
    fn test_translate() {
        assert_eq!(
            BitBoard::FILE_B.translated(Coords::new(5, 1)),
            BitBoard::FILE_G.clear(6),
        );
        assert_eq!(
            BitBoard::RANK_1.translated(Coords::new(0, 1)),
            BitBoard::RANK_2,
        );
        assert_eq!(
            BitBoard::RANK_2.translated(Coords::new(2, -1)),
            BitBoard::RANK_1.clear(0).clear(1),
        );
    }

    #[test]
    fn test_generate_pawn_moves() {
        let mut board = Chessboard::default();

        let moves = BitBoard::from_pos_list(&board.moves_from(P::uci("c2").unwrap()));
        let expected = BitBoard::from_pos_list(&[P::uci("c3").unwrap(), P::uci("c4").unwrap()]);

        assert_eq!(moves, expected);

        let move1 = Move(P::uci("e2").unwrap(), P::uci("e4").unwrap());

        board.move_piece(move1).unwrap();
        board
            .move_piece(Move(P::uci("f7").unwrap(), P::uci("f5").unwrap()))
            .unwrap();

        let moves = BitBoard::from_pos_list(&board.moves_from(move1.1));
        let expected = BitBoard::from_pos_list(&[P::uci("e5").unwrap(), P::uci("f5").unwrap()]);

        assert_eq!(moves, expected);

        board
            .move_piece(Move(P::uci("f5").unwrap(), P::uci("e5").unwrap()))
            .unwrap();

        let moves = BitBoard::from_pos_list(&board.moves_from(move1.1));
        assert_eq!(moves, BitBoard::zero());
    }

    #[test]
    fn test_generate_knight_moves() {
        let mut board = Chessboard::default();

        let moves = BitBoard::from_pos_list(&board.moves_from(P::uci("b1").unwrap()));
        let expected = BitBoard::from_pos_list(&[P::uci("a3").unwrap(), P::uci("c3").unwrap()]);

        assert_eq!(moves, expected);

        let move1 = Move(P(0o01), P(0o57));

        board.move_piece(move1).unwrap();

        let moves = BitBoard::from_pos_list(&board.moves_from(move1.1));
        let expected = BitBoard::from_pos_list(&[
            P::uci("g8").unwrap(),
            P::uci("f7").unwrap(),
            P::uci("f5").unwrap(),
            P::uci("g4").unwrap(),
        ]);

        assert_eq!(moves, expected);
    }
}
