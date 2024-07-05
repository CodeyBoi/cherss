use std::io::stdout;

use crossterm::{
    execute,
    terminal::{disable_raw_mode, LeaveAlternateScreen},
};
use player::{BotStrategy, Player};
use tui::App;

mod bitboard;
mod chess;
mod chessboard;
mod chessgame;
mod piece;
mod player;
mod tui;

fn main() {
    let chess = bitboard::Chessboard::with_players(Player::Human, Player::Human).into_game();

    let _ = chessboard::Chessboard::default();

    if let Err(e) = App::run(chess) {
        disable_raw_mode().unwrap();
        execute!(stdout(), LeaveAlternateScreen).unwrap();
        println!("{}", e);
    }
}
