use std::{
    io::{self, stdout, Stdout},
    time::{Duration, Instant},
};

use ratatui::{prelude::*, widgets::*};

use crossterm::{
    cursor,
    event::{self, DisableMouseCapture, EnableMouseCapture, Event, KeyEvent, KeyEventKind},
    execute,
    terminal::{
        disable_raw_mode, enable_raw_mode, is_raw_mode_enabled, EnterAlternateScreen,
        LeaveAlternateScreen,
    },
};
use ratatui::{backend::CrosstermBackend, layout::Rect, style::Stylize, Terminal};

use crate::chessgame::ChessGame;
use crate::{
    chessboard::{self, SIZE},
    piece::{ChessColor, Piece},
};

pub type Tui = Terminal<CrosstermBackend<Stdout>>;

pub enum TileStatus {
    Default,
    Active,
    CanMoveTo,
    CanCaptureAt,
}

pub(crate) struct Tile {
    pub piece: Option<Piece>,
    pub status: TileStatus,
    pub color: ChessColor,
}

impl Widget for &Tile {
    fn render(self, area: Rect, buf: &mut Buffer)
    where
        Self: Sized,
    {
        use TileStatus as T;

        (if let ChessColor::White = self.color {
            Block::new().black().on_gray()
        } else {
            Block::new().white().on_dark_gray()
        })
        .render(area, buf);

        match self.status {
            T::Active => {
                Block::new().on_green().render(area, buf);
            }
            T::CanMoveTo => {
                let radius = 1;
                let a = Rect {
                    x: area.x + area.width / 2 - radius,
                    y: area.y + (area.height - radius) / 2,
                    width: radius * 2,
                    height: radius,
                };
                Block::new().on_green().render(a, buf);
            }
            T::CanCaptureAt => {
                Block::bordered().green().render(area, buf);
            }
            _ => {}
        }

        if let Some(piece) = self.piece {
            piece.render(area, buf);
        }
    }
}

pub struct App {
    tui: Tui,
    chess: ChessGame,
}

impl App {
    pub fn run(chess: ChessGame) -> io::Result<()> {
        let tui = Terminal::new(CrosstermBackend::new(stdout()))?;

        let mut app = App { tui, chess };

        let term_size = app.tui.size()?;
        let tile_size = 10;
        app.chess.set_tile_size(tile_size);
        let size = Rect {
            x: 0,
            y: 0,
            width: tile_size * SIZE as u16,
            height: tile_size * SIZE as u16 / 2,
        };

        if term_size.width < size.width || term_size.height < size.height {
            println!(
                "Please set terminal size to atleast {}x{}.",
                size.width, size.height
            );
            return Ok(());
        }

        init_terminal()?;

        loop {
            let start = Instant::now();
            if !app.chess.is_running {
                break;
            }

            app.handle_events().expect("error when handling events");
            app.chess.make_bot_move();

            app.tui.draw(|frame| {
                app.chess.render(frame);
                frame.set_cursor(0, 0);
            })?;

            std::thread::sleep(
                Duration::from_micros(1_000_000 / 10).saturating_sub(start.elapsed()),
            );
        }
        Ok(())
    }

    fn handle_events(&mut self) -> io::Result<()> {
        while event::poll(Duration::from_secs(0))? {
            match event::read()? {
                Event::Mouse(event) => {
                    self.chess.handle_mouse_event(event);
                }
                Event::Key(KeyEvent {
                    kind: KeyEventKind::Press,
                    code,
                    ..
                }) => {
                    self.chess.handle_key_press(code);
                }
                Event::Resize(w, h) => {
                    self.chess
                        .set_tile_size(w.min(h * 2) / chessboard::SIZE as u16);
                }
                _ => {}
            }
        }
        Ok(())
    }
}

fn init_terminal() -> io::Result<()> {
    enable_raw_mode()?;
    execute!(stdout(), EnterAlternateScreen)?;
    execute!(stdout(), EnableMouseCapture, cursor::Hide)?;

    Ok(())
}

fn restore() -> io::Result<()> {
    if is_raw_mode_enabled()? {
        execute!(stdout(), DisableMouseCapture, cursor::Show)?;
        execute!(stdout(), LeaveAlternateScreen)?;
        disable_raw_mode()?;
    }

    Ok(())
}

impl Drop for App {
    fn drop(&mut self) {
        restore().expect("restore function should not fail");
    }
}
