use std::ops::DerefMut;

use crossterm::style::SetStyle;
use ratatui::{
    buffer::Buffer,
    layout::Rect,
    style::{palette::material::WHITE, Style, Stylize},
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
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum ChessColor {
    White,
    Black,
}

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
        let (x, y) = (area.x + area.width / 2, area.y + area.height / 2);
        buf.set_string(
            x,
            y,
            self.to_fen().to_string(),
            Style::new().white().on_black(),
        );
    }
}
