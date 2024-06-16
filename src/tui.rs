use std::{
    io::{self, stdout, Stdout},
    time::{Duration, Instant},
};

use crossterm::{
    cursor,
    event::{
        self, DisableMouseCapture, EnableMouseCapture, Event, KeyCode, KeyEvent, KeyEventKind,
        MouseButton, MouseEvent, MouseEventKind,
    },
    execute,
    terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
    ExecutableCommand,
};
use ratatui::{
    backend::CrosstermBackend,
    layout::Rect,
    style::Stylize,
    widgets::{canvas::Rectangle, Block},
    Terminal,
};

use crate::{
    chessboard::{self, SIZE},
    chessgame::ChessGame,
};

pub type Tui = Terminal<CrosstermBackend<Stdout>>;

pub struct App {
    tui: Tui,
    chess: ChessGame,
}

impl App {
    pub fn run(chess: ChessGame) -> io::Result<()> {
        let tui = init_terminal()?;
        let mut app = App { tui, chess };

        let term_size = app.tui.size()?;
        let tile_size = 10;
        app.chess.set_tile_size(tile_size);

        loop {
            let start = Instant::now();
            if !app.chess.is_running {
                break;
            }

            app.handle_events().expect("error when handling events");
            app.chess.make_bot_move();

            app.tui.draw(|frame| {
                frame.render_widget(Block::bordered().white().on_magenta(), frame.size());
                app.chess.render(frame);
                frame.set_cursor(0, 0);
            })?;

            std::thread::sleep(
                Duration::from_micros(1_000_000 / 10).saturating_sub(start.elapsed()),
            );
        }

        app.tui.resize(term_size)?;
        restore()?;
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

fn init_terminal() -> io::Result<Tui> {
    execute!(
        stdout(),
        EnterAlternateScreen,
        EnableMouseCapture,
        cursor::Hide,
    )?;
    enable_raw_mode()?;
    let terminal = Terminal::new(CrosstermBackend::new(stdout()))?;

    Ok(terminal)
}

fn restore() -> io::Result<()> {
    execute!(
        stdout(),
        LeaveAlternateScreen,
        DisableMouseCapture,
        cursor::Show,
    )?;
    disable_raw_mode()?;

    Ok(())
}
