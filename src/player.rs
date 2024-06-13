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
}

impl BotStrategy {
    pub fn choose_move(self, board: &mut Chessboard) -> Option<(Position, Position)> {
        match self {
            BotStrategy::Random => Self::random(board),
        }
    }

    pub fn random(board: &mut Chessboard) -> Option<(Position, Position)> {
        let moves = board.all_moves();
        moves[..].choose(&mut rand::thread_rng()).cloned()
    }
}
