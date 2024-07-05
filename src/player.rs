use rand::seq::SliceRandom as _;

use crate::{
    bitboard::Chessboard,
    chess::{Chess, Coords, Move},
    piece::ChessColor,
};

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
    pub fn choose_move(self, board: &Box<dyn Chess>) -> Option<Move> {
        match self {
            BotStrategy::Random => Self::random(board),
            BotStrategy::DarkSquares => Self::dark_squares(board),
        }
    }

    pub fn random(board: &Box<dyn Chess>) -> Option<Move> {
        let moves = board.all_moves();
        moves[..].choose(&mut rand::thread_rng()).cloned()
    }

    fn dark_squares(board: &Box<dyn Chess>) -> Option<Move> {
        let moves = board.all_moves();
        moves
            .iter()
            .find(|&&Move(from, to)| {
                from.tile_color() == ChessColor::White && to.tile_color() == ChessColor::Black
            })
            .or_else(|| {
                moves
                    .iter()
                    .find(|&&Move(_, to)| to.tile_color() == ChessColor::Black)
            })
            .cloned()
            .or_else(|| Self::random(board))
    }

    fn mirror(board: &Box<dyn Chess>) -> Option<Move> {
        // let moves = board.all_moves();
        // if let Some(last_move) = board.history().last() {}

        todo!()
    }
}
