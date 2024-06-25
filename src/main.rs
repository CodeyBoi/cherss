use bitboard::Chessboard;
use player::{BotStrategy, Player};
use tui::App;

mod bitboard;
mod chess;
mod chessboard;
mod chessgame;
mod piece;
mod player;
mod sdl;
mod tui;

fn main() {
    let chess = chessboard::Chessboard::with_players(
        Player::Bot(BotStrategy::Random),
        Player::Bot(BotStrategy::Random),
    )
    .into_game();

    let _ = Chessboard::default();

    App::run(chess).unwrap();
}
