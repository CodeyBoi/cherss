use core::fmt;
use std::{
    fmt::{Display, Write},
    ops::{Deref, Index, IndexMut, Not},
};

use lazy_static::lazy_static;

use crate::{
    chess::{Chess, ChessMoveError, Coords, Dir, Line, LineOrientation, Move, Position},
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
    static ref KNIGHT_LOOKUP: [BitBoard; 64] = BitBoard::generate_knight_lookup();
}

impl BitBoard {
    const FULL: Self = Self(!0);

    const FILE_A: Self = Self(0x0101_0101_0101_0101);

    const RANK_1: Self = Self(0x0000_0000_0000_00ff);
    const RANK_2: Self = Self(0x0000_0000_0000_ff00);
    const RANK_7: Self = Self(0x00ff_0000_0000_0000);
    const RANK_8: Self = Self(0xff00_0000_0000_0000);

    const DIAGONAL: Self = Self(0x8040_2010_0804_0201);
    const ANTI_DIAGONAL: Self = Self(0x0102_0408_1020_4080);

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

    pub const fn intersect(self, rhs: Self) -> Self {
        Self(self.0 & rhs.0)
    }

    pub const fn is_empty(self) -> bool {
        self.0 == 0
    }

    pub const fn from_rank(rank: u8) -> Self {
        Self(Self::RANK_1.0 << (rank * 8))
    }

    pub const fn from_file(file: u8) -> Self {
        Self(Self::FILE_A.0 << file)
    }

    pub const fn diagonal(offset: i8) -> Self {
        if offset >= 0 {
            Self(Self::DIAGONAL.0 << (offset.abs() * 8))
        } else {
            Self(Self::DIAGONAL.0 >> (offset.abs() * 8))
        }
    }

    pub const fn anti_diagonal(offset: i8) -> Self {
        if offset >= 0 {
            Self(Self::ANTI_DIAGONAL.0 << (offset.abs() * 8))
        } else {
            Self(Self::ANTI_DIAGONAL.0 >> (offset.abs() * 8))
        }
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

    pub fn translate(self, file: i8, rank: i8) -> Self {
        let shift_offset = rank * 8 + file;
        if file == 0 {
            return if shift_offset > 0 {
                Self(self.0 << shift_offset)
            } else {
                Self(self.0 >> -shift_offset)
            };
        }
        // Filter out cells which would wrap horizontally
        let mask = {
            let cols = (0..file.abs()).fold(Self::zero(), |acc, i| {
                acc.union(BitBoard(Self::FILE_A.0 << i))
            });
            if file > 0 {
                !BitBoard(cols.0 << (8 - file))
            } else {
                !cols
            }
        };
        if shift_offset > 0 {
            Self(self.intersect(mask).0 << shift_offset)
        } else {
            Self(self.intersect(mask).0 >> -shift_offset)
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

    fn fill(self, rhs: Self) -> Self {
        Self(self.wrapping_mul(rhs.0))
    }

    fn fill_north(self) -> Self {
        self.fill(Self::FILE_A)
    }

    fn get_line(self, line: Line) -> u8 {
        use Line as L;
        // These lines are extracted as per https://www.chessprogramming.org/Kindergarten_Bitboards
        match line {
            L::Rank(rank) => self.translate(0, -(rank as i8)).intersect(Self::RANK_1),
            L::File(file) => self
                .translate(-(file as i8), 0)
                .intersect(BitBoard::FILE_A)
                .fill(Self::ANTI_DIAGONAL)
                .translate(0, -7),
            L::Diagonal(offset) => self
                .intersect(Self::DIAGONAL.translate(0, offset))
                .fill_north()
                .translate(0, -7),
            L::AntiDiagonal(offset) => self
                .intersect(Self::ANTI_DIAGONAL.translate(0, offset))
                .fill_north()
                .translate(0, -7),
        }
        .0 as u8
    }

    /// Constructs a BitBoard given an occupancy line and its location. The inverse of get_line().
    fn from_line(line: u8, location: Line) -> Self {
        use Line as L;
        // board starts out with the occupancy in the first rank
        let board = Self(line as u64);
        match location {
            L::Rank(rank) => board.translate(0, rank as i8),
            L::File(file) => board
                .fill(Self::ANTI_DIAGONAL)
                .intersect(Self::from_file(7))
                .translate(-7i8.saturating_sub_unsigned(file), 0),
            L::Diagonal(offset) => board
                .fill_north()
                .intersect(Self::DIAGONAL.translate(0, offset)),
            L::AntiDiagonal(offset) => board
                .fill_north()
                .intersect(Self::ANTI_DIAGONAL.translate(0, offset)),
        }
    }

    // Index into this attack lookup is the middle 6 bits of the rank/file/diagonal. The edges don't matter, as there are no tiles behind them.
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

        // eprintln!("           {}V", " ".repeat(7 - piece_pos as usize));
        // eprintln!("occupancy: {:08b}\n   attack: {:08b}", occupancy, res);
        res
    }

    fn generate_knight_lookup() -> [Self; 64] {
        let mut res = [BitBoard::zero(); 64];
        for i in 0..64 {
            let pos = Position(i as u8);
            let mask = pos.to_mask();
            res[i] = Coords::KNIGHT_OFFSETS
                .iter()
                .fold(BitBoard::zero(), |acc, offset| {
                    acc.union(mask.translate(offset.file, offset.rank))
                });
        }
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
        self.pieces[piece].intersect(self.colors[color])
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
                for pos in piece_map.serialized() {
                    Self::add_moves_with_origin(
                        moves,
                        pos,
                        &self
                            .generate_knight_moves(pos.to_mask(), color)
                            .serialized(),
                    );
                }
            }
            P::Bishop => {
                for pos in piece_map.serialized() {
                    Self::add_moves_with_origin(
                        moves,
                        pos,
                        &self
                            .generate_bishop_moves(pos.to_mask(), color)
                            .serialized(),
                    );
                }
            }
            P::Rook => {
                for pos in piece_map.serialized() {
                    Self::add_moves_with_origin(
                        moves,
                        pos,
                        &self.generate_rook_moves(pos.to_mask(), color).serialized(),
                    );
                }
            }
            P::Queen => {
                for pos in piece_map.serialized() {
                    Self::add_moves_with_origin(
                        moves,
                        pos,
                        &self.generate_queen_moves(pos.to_mask(), color).serialized(),
                    );
                }
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
        let move_one_tile = pieces.translate(0, rank_offset).intersect(valid_moves);
        Self::add_moves(moves, offset_dir, &move_one_tile.serialized());

        // Start checking from `move_one_tile` to check that no pieces are blocking
        let move_two_tiles = move_one_tile
            .translate(0, rank_offset)
            .intersect(valid_moves)
            // Filter out pieces which didn't start at the correct rank
            .intersect(double_move_mask.translate(0, 2 * rank_offset));
        Self::add_moves(moves, 2 * offset_dir, &move_two_tiles.serialized());

        let capture_offset = Coords::new(Dir::W_OFFSET, rank_offset);
        let west_captures = pieces
            .translate(Dir::W_OFFSET, rank_offset)
            // We can only capture on squares with an opposing piece
            .intersect(opposing_pieces);
        Self::add_moves(moves, capture_offset, &west_captures.serialized());

        // Same reasoning as above
        let capture_offset = Coords::new(Dir::E_OFFSET, rank_offset);
        let east_captures = pieces
            .translate(Dir::E_OFFSET, rank_offset)
            .intersect(opposing_pieces);
        Self::add_moves(moves, capture_offset, &east_captures.serialized());
    }

    fn generate_knight_moves(&self, pieces: BitBoard, color: ChessColor) -> BitBoard {
        // We cannot move to or capture our own pieces
        let valid_tiles = !self.colors[color];
        pieces
            .serialized()
            .iter()
            .fold(BitBoard::zero(), |acc, pos| {
                acc.union(KNIGHT_LOOKUP[pos.0 as usize].intersect(valid_tiles))
            })
    }

    fn generate_sliding_moves(
        &self,
        occupancy: BitBoard,
        position: Position,
        directions: &[LineOrientation],
    ) -> BitBoard {
        directions.iter().fold(BitBoard::zero(), |acc, dir| {
            let (location, line_index) = position.containing_line(*dir);
            let line_occupancy = occupancy.get_line(location);
            let possible_line_moves =
                ATTACK_LOOKUP[((line_occupancy & 0b01111110) >> 1) as usize][line_index as usize];
            BitBoard::from_line(possible_line_moves, location).union(acc)
        })
    }

    fn generate_bishop_moves(&self, pieces: BitBoard, color: ChessColor) -> BitBoard {
        let occupancy = self.occupancy();
        let valid_tiles = !self.colors[color];
        pieces
            .serialized()
            .iter()
            .fold(BitBoard::zero(), |acc, pos| {
                self.generate_sliding_moves(
                    occupancy,
                    *pos,
                    &[LineOrientation::Diagonal, LineOrientation::AntiDiagonal],
                )
                .union(acc)
            })
            .intersect(valid_tiles)
    }

    fn generate_rook_moves(&self, pieces: BitBoard, color: ChessColor) -> BitBoard {
        let occupancy = self.occupancy();
        let valid_tiles = !self.colors[color];
        pieces
            .serialized()
            .iter()
            .fold(BitBoard::zero(), |acc, pos| {
                self.generate_sliding_moves(
                    occupancy,
                    *pos,
                    &[LineOrientation::Rank, LineOrientation::File],
                )
                .union(acc)
            })
            .intersect(valid_tiles)
    }

    fn generate_queen_moves(&self, pieces: BitBoard, color: ChessColor) -> BitBoard {
        self.generate_bishop_moves(pieces, color)
            .union(self.generate_rook_moves(pieces, color))
    }

    fn generate_king_moves(&self, moves: &mut Vec<Move>, piece: BitBoard, color: ChessColor) {
        // We cannot move to or capture our own pieces
        let valid_tiles = !self.colors[color];

        for offset in [Dir::CARDINAL, Dir::DIAGONAL].concat() {
            // Filter out moves which will go OOB horisontally (we don't have to worry about vertical
            // OOB as they will be shifted away)
            let mask = match offset.file {
                Dir::W_OFFSET => !BitBoard::FILE_A,
                Dir::E_OFFSET => !BitBoard::from_file(7),
                _ => BitBoard::FULL,
            };
            let possible_moves = piece
                .intersect(mask)
                .translate(offset.file, offset.rank)
                .intersect(valid_tiles);

            // TODO: Filter moves which put king in check

            Self::add_moves(moves, offset, &possible_moves.serialized());
        }
    }

    fn get_attack_map(&self, color: ChessColor) -> BitBoard {
        let mut attack_map = BitBoard::zero();

        let pieces = self.colors[color];

        // Handle pawns
        let rank_offset = match color {
            ChessColor::White => Dir::N_OFFSET,
            ChessColor::Black => Dir::S_OFFSET,
        };

        let pawns = pieces.intersect(self.pieces[PieceType::Pawn]);
        attack_map = attack_map.union(pawns.translate(Dir::W_OFFSET, rank_offset));
        attack_map = attack_map.union(pieces.translate(Dir::E_OFFSET, rank_offset));

        let knights = pieces.intersect(self.pieces[PieceType::Knight]);
        attack_map = knights.serialized().iter().fold(attack_map, |acc, pos| {
            acc.union(KNIGHT_LOOKUP[pos.0 as usize])
        });

        let knights = pieces.intersect(self.pieces[PieceType::Knight]);
        attack_map = knights.serialized().iter().fold(attack_map, |acc, pos| {
            acc.union(KNIGHT_LOOKUP[pos.0 as usize])
        });

        attack_map
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

        // for line in [
        //     BitBoard::FILE_A,
        //     BitBoard::DIAGONAL,
        //     BitBoard::ANTI_DIAGONAL,
        // ] {
        //     for i in 0..8 {
        //         let val = 1u64 << i;
        //         let board = BitBoard(val as u64);
        //         let result = BitBoard(board.0.wrapping_mul(line.0));
        //         eprintln!("val={val}, {}{}", board, result);
        //     }
        // }

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
        let color = if self.colors[ChessColor::White].intersect(mask).0 != 0 {
            ChessColor::White
        } else if self.colors[ChessColor::Black].intersect(mask).0 != 0 {
            ChessColor::Black
        } else {
            return None;
        };

        let piece = PIECE_TYPES
            .iter()
            .find(|&&piece| self.pieces[piece].intersect(mask).0 != 0)?;

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
            BitBoard::from_file(1).translate(5, 1),
            BitBoard::from_file(6).clear(6),
        );
        assert_eq!(BitBoard::RANK_1.translate(0, 1), BitBoard::RANK_2);
        assert_eq!(
            BitBoard::RANK_2.translate(2, -1),
            BitBoard::RANK_1.clear(0).clear(1),
        );
    }

    #[test]
    fn test_line_functions() {
        const BOARD: BitBoard = BitBoard::from_file(1)
            .union(BitBoard::RANK_8)
            .set(32)
            .set(34)
            .set(53)
            .set(43);

        let tests = [
            (Line::Rank(5), 0b0000_1010),
            (Line::Rank(6), 0b0010_0010),
            (Line::File(2), 0b1001_0000),
            (Line::File(3), 0b1010_0000),
            (Line::Diagonal(0), 0b1000_0010),
            (Line::Diagonal(-2), 0b0000_0000),
            (Line::Diagonal(2), 0b0010_1110),
            (Line::AntiDiagonal(0), 0b0000_0011),
            (Line::AntiDiagonal(-3), 0b0000_0011),
            (Line::AntiDiagonal(1), 0b0000_1010),
        ];

        for (location, expected) in tests {
            assert_eq!(BOARD.get_line(location), expected);
            assert_eq!(
                BitBoard::from_line(expected, location),
                BOARD.intersect(location.to_mask()),
            );
        }
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

    #[test]
    fn test_generate_bishop_moves() {
        let mut board = Chessboard::default();

        let moves = BitBoard::from_pos_list(&board.moves_from(P::uci("c1").unwrap()));
        let expected = BitBoard::zero();

        assert_eq!(moves, expected);

        let move1 = Move(P(0o02), P(0o20));

        board.move_piece(move1).unwrap();

        let moves = BitBoard::from_pos_list(&board.moves_from(move1.1));
        let expected = BitBoard::DIAGONAL.translate(1, 3).clear(0o75);

        assert_eq!(moves, expected);
    }

    #[test]
    fn test_generate_rook_moves() {
        let mut board = Chessboard::default();

        let moves = BitBoard::from_pos_list(&board.moves_from(P::uci("a1").unwrap()));
        let expected = BitBoard::zero();

        assert_eq!(moves, expected);

        let move1 = Move(P(0o00), P(0o20));

        board.move_piece(move1).unwrap();

        let moves = BitBoard::from_pos_list(&board.moves_from(move1.1));
        let expected = BitBoard::FILE_A
            .translate(0, 3)
            .clear(0o70)
            .union(BitBoard::RANK_1.translate(1, 2));

        assert_eq!(moves, expected);

        let move1 = Move(P(0o20), P(0o44));
        board.move_piece(move1).unwrap();
        let moves = BitBoard::from_pos_list(&board.moves_from(move1.1));
        let expected = BitBoard::from_file(4)
            .translate(0, 2)
            .clear(0o74)
            .union(BitBoard::from_rank(4))
            .clear(move1.1 .0);

        assert_eq!(moves, expected);
    }
}
