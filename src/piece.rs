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
    pub fn to_char(self) -> char {
        match self {
            PieceType::Pawn => 'P',
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

    pub fn from_fen(fen: char) -> Option<Self> {
        let color = if fen.is_uppercase() {
            Color::White
        } else {
            Color::Black
        };
        let piece = match fen.to_ascii_lowercase() {
            'p' => PieceType::Pawn,
            'n' => PieceType::Knight,
            'b' => PieceType::Bishop,
            'r' => PieceType::Rook,
            'q' => PieceType::Queen,
            'k' => PieceType::King,
            _ => return None,
        };
        Some(Self::new(color, piece))
    }

    pub fn to_fen(self) -> char {
        let c = self.piece.to_char();
        if self.color == Color::White {
            c
        } else {
            c.to_ascii_lowercase()
        }
    }
}
