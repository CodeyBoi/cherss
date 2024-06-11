#[derive(Clone, Copy)]
pub struct Piece {
    pub color: Color,
    pub piece: PieceType,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum PieceType {
    Pawn,
    Knight,
    Bishop,
    Rook,
    Queen,
    King,
}

impl PieceType {
    pub fn to_char(&self) -> char {
        match self {
            PieceType::Pawn => ' ',
            PieceType::Knight => 'N',
            PieceType::Bishop => 'B',
            PieceType::Rook => 'R',
            PieceType::Queen => 'Q',
            PieceType::King => 'K',
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Color {
    White,
    Black,
}

impl Piece {
    pub fn new(color: Color, piece: PieceType) -> Self {
        Self { color, piece }
    }
}
