use rand::seq::SliceRandom as _;

use crate::chessboard::{Chessboard, Position};

#[derive(Clone, Copy)]
pub enum Player {
    Bot(BotStrategy),
    Human,
}

#[derive(Clone, Copy)]
pub enum BotStrategy {
    Random,
    DarkSquares,
}

impl BotStrategy {
    pub fn choose_move(self, board: &mut Chessboard) -> Option<(Position, Position)> {
        match self {
            BotStrategy::Random => Self::random(board),
            BotStrategy::DarkSquares => Self::dark_squares(board),
        }
    }

    pub fn random(board: &mut Chessboard) -> Option<(Position, Position)> {
        let moves = board.all_moves();
        moves[..].choose(&mut rand::thread_rng()).cloned()
    }

    pub fn dark_squares(board: &mut Chessboard) -> Option<(Position, Position)> {
        let moves = board.all_moves();
        moves
            .iter()
            .find(|&&pos| (pos.1.row + pos.1.col) % 2 == 0 && (pos.0.row + pos.0.col) % 2 == 1)
            .or_else(|| moves.iter().find(|&&pos| (pos.1.row + pos.1.col) % 2 == 0))
            .cloned()
            .or_else(|| Self::random(board))
    }
}
