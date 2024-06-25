use ratatui::{
    buffer::Buffer,
    layout::Rect,
    style::{Color, Style, Stylize},
    widgets::Widget,
};

#[derive(Clone, Copy, Debug)]
pub struct Piece {
    pub color: ChessColor,
    pub piece: PieceType,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
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

    pub fn value(self) -> u8 {
        match self {
            PieceType::Pawn => 1,
            PieceType::Knight => 3,
            PieceType::Bishop => 3,
            PieceType::Rook => 5,
            PieceType::Queen => 9,
            PieceType::King => u8::MAX,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum ChessColor {
    White,
    Black,
}

const PAWN_RENDER: [&str; 4] = [r"   ()   ", r"   )(   ", r"  (cc)  ", r" /cccc\ "];

const KNIGHT_RENDER: [&str; 4] = [r" ^^cc\  ", r"//│cc│  ", r"  │cc│  ", r" /cccc\ "];

const BISHOP_RENDER: [&str; 4] = [r"   ()   ", r"  (──)  ", r"  /cc\  ", r" /cccc\ "];

const ROOK_RENDER: [&str; 4] = [r" ┌┐┌┐┌┐ ", r" └┐cc┌┘ ", r"  │cc│  ", r" /cccc\ "];

const QUEEN_RENDER: [&str; 4] = [r"  WWWW  ", r"  )cc(  ", r"  /cc\  ", r" /cccc\ "];

const KING_RENDER: [&str; 4] = [r"  ═╡╞═  ", r"  )cc(  ", r"  /cc\  ", r" /cccc\ "];

impl Piece {
    pub fn new(color: ChessColor, piece: PieceType) -> Self {
        Self { color, piece }
    }

    pub fn from_fen(fen: char) -> Option<Self> {
        let color = if fen.is_uppercase() {
            ChessColor::White
        } else {
            ChessColor::Black
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
        if self.color == ChessColor::White {
            c
        } else {
            c.to_ascii_lowercase()
        }
    }
}

impl Widget for Piece {
    fn render(self, area: Rect, buf: &mut Buffer)
    where
        Self: Sized,
    {
        use PieceType as P;

        let fill_str = match self.color {
            ChessColor::White => "'",
            ChessColor::Black => "▒",
        };

        let render_str = match self.piece {
            P::Pawn => PAWN_RENDER,
            P::Knight => KNIGHT_RENDER,
            P::Bishop => BISHOP_RENDER,
            P::Rook => ROOK_RENDER,
            P::Queen => QUEEN_RENDER,
            P::King => KING_RENDER,
        };

        for (i, line) in render_str.iter().enumerate() {
            let line = line.replace('c', fill_str);
            let (x, y) = (area.x, area.y + i as u16);
            buf.set_string(x, y, line, Style::new().black());
        }
    }
}
