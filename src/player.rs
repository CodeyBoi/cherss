use rand::seq::SliceRandom as _;

use crate::{chess::Coords, chessboard::Chessboard, piece::ChessColor};

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
    pub fn choose_move(self, board: &mut Chessboard) -> Option<(Coords, Coords)> {
        match self {
            BotStrategy::Random => Self::random(board),
            BotStrategy::DarkSquares => Self::dark_squares(board),
        }
    }

    pub fn random(board: &mut Chessboard) -> Option<(Coords, Coords)> {
        let moves = board.all_moves();
        moves[..].choose(&mut rand::thread_rng()).cloned()
    }

    fn dark_squares(board: &mut Chessboard) -> Option<(Coords, Coords)> {
        let moves = board.all_moves();
        moves
            .iter()
            .find(|&&(from, to)| {
                from.tile_color() == ChessColor::White && to.tile_color() == ChessColor::Black
            })
            .or_else(|| {
                moves
                    .iter()
                    .find(|&&(_, to)| to.tile_color() == ChessColor::Black)
            })
            .cloned()
            .or_else(|| Self::random(board))
    }

    fn mirror(board: &mut Chessboard) -> Option<(Coords, Coords)> {
        let moves = board.all_moves();
        if let Some(last_move) = board.history.last() {}

        todo!()
    }
}
