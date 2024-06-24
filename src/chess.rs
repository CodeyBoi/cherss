use crate::piece::ChessColor;

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Move(u8, u8);

pub enum ParseMoveError {
    InvalidLength,
    InvalidFile,
    InvalidRank,
    InvalidPromotion,
}

pub fn pos_from_uci(uci: &str) -> Result<u8, ParseMoveError> {
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
    Ok(row * 8 + col)
}

impl Move {
    fn from_uci(uci: &str) -> Result<Self, ParseMoveError> {
        let from = pos_from_uci(&uci[0..2])?;
        let to = pos_from_uci(&uci[0..2])?;
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
    fn make_move(chess_move: Move) -> Result<(), ChessMoveError>;
    fn gen_moves(pos: u8, color: ChessColor);
}
