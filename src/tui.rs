use std::{
    io::{self, stdout, Stdout},
    time::{Duration, Instant},
};

use ratatui::{layout::Position, prelude::*, widgets::*};

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
    chessboard::SIZE,
    piece::{ChessColor, Piece},
};

pub type Tui = Terminal<CrosstermBackend<Stdout>>;

#[derive(Clone, Copy)]
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

struct TileColors {
    bg: Color,
    active: Color,
    highlight: Color,
}

impl TileColors {
    const LIGHT: TileColors = TileColors {
        bg: Color::Rgb(240, 217, 181),
        active: Color::Rgb(205, 209, 107),
        highlight: Color::Rgb(130, 151, 105),
    };
    const DARK: TileColors = TileColors {
        bg: Color::Rgb(181, 136, 99),
        active: Color::Rgb(170, 162, 59),
        highlight: Color::Rgb(100, 111, 64),
    };
}

impl Widget for &Tile {
    fn render(self, area: Rect, buf: &mut Buffer)
    where
        Self: Sized,
    {
        use TileStatus as T;

        let colors = match self.color {
            ChessColor::White => TileColors::LIGHT,
            ChessColor::Black => TileColors::DARK,
        };
        Block::new()
            .bg(match self.status {
                T::Active => colors.active,
                _ => colors.bg,
            })
            .render(area, buf);

        match self.status {
            T::CanMoveTo => {
                let radius = 1;
                let a = Rect {
                    x: area.x + area.width / 2 - radius,
                    y: area.y + (area.height - radius) / 2,
                    width: radius * 2,
                    height: radius,
                };
                Block::new().bg(colors.highlight).render(a, buf);
            }
            T::CanCaptureAt => {
                Block::bordered().fg(colors.highlight).render(area, buf);
            }
            _ => {}
        }

        if let Some(piece) = self.piece {
            piece.render(area.inner(&Margin::new(1, 1)), buf);
        }
    }
}

pub struct App {
    tui: Tui,
    chess: ChessGame,
    board_area: Rect,
    history_area: Rect,
}

impl App {
    pub fn run(mut chess: ChessGame) -> io::Result<()> {
        let tui = Terminal::new(CrosstermBackend::new(stdout()))?;

        let app_area = tui.size()?;
        let tile_size = 10;
        let history_width = 15;
        chess.set_tile_size(tile_size);
        let (board_width, board_height) = (tile_size * SIZE as u16, tile_size * SIZE as u16 / 2);

        if app_area.width < board_width || app_area.height < board_height {
            println!(
                "Please set terminal size to atleast {}x{} (current term size {}x{})",
                board_width, board_height, app_area.width, app_area.height
            );
            return Ok(());
        }

        let rows = Layout::vertical([
            Constraint::Length(2),
            Constraint::Length(board_height),
            Constraint::Length(2),
        ])
        .split(app_area);
        let columns = Layout::horizontal([
            Constraint::Length(board_width),
            Constraint::Length(history_width),
        ])
        .split(rows[1]);

        let (board_area, history_area) = (columns[0], columns[1]);

        // let board_area = Rect {
        //     x: app_area.x + (app_area.width - board_width) / 2,
        //     y: app_area.y + (app_area.height - board_height) / 2,
        //     width: board_width,
        //     height: board_height,
        // };

        let mut app = App {
            tui,
            chess,
            board_area,
            history_area,
        };

        init_terminal()?;

        loop {
            let start = Instant::now();
            if !app.chess.is_running {
                break;
            }

            app.handle_events().expect("error when handling events");
            if app.chess.bots_active {
                app.chess.make_bot_move();
            }

            app.render();

            std::thread::sleep(
                Duration::from_micros(1_000_000 / 60).saturating_sub(start.elapsed()),
            );
        }
        Ok(())
    }

    fn handle_events(&mut self) -> io::Result<()> {
        while event::poll(Duration::from_secs(0))? {
            match event::read()? {
                Event::Mouse(event) => {
                    if self
                        .board_area
                        .contains(Position::new(event.column, event.row))
                    {
                        let (column, row) = (
                            event.column.wrapping_sub(self.board_area.x),
                            event.row.wrapping_sub(self.board_area.y),
                        );
                        let event = event::MouseEvent {
                            column,
                            row,
                            ..event
                        };
                        self.chess.handle_mouse_event(event);
                    }
                }
                Event::Key(KeyEvent {
                    kind: KeyEventKind::Press,
                    code,
                    ..
                }) => {
                    self.chess.handle_key_press(code);
                    self.render();
                }
                _ => {}
            }
        }
        Ok(())
    }

    fn render(&mut self) {
        let _ = self.tui.draw(|frame| {
            frame.render_widget(&mut self.chess, self.board_area);
            let style = Style::new().fg(Color::White);
            let mut history_items: Vec<_> = self
                .chess
                .board
                .history()
                .chunks(2)
                .enumerate()
                .map(|(i, moves)| {
                    let mut text = format!("{}. {}-{}", i + 1, moves[0].0, moves[0].1);
                    if moves.len() >= 2 {
                        text.push_str(&format!(" {}-{}", moves[1].0, moves[1].1));
                    }
                    ListItem::new(Line::styled(text, style))
                })
                .collect();
            if self.chess.board.history().len() % 2 == 0 {
                history_items.push(ListItem::new(Line::styled(
                    format!("{}.", self.chess.board.history().len() / 2 + 1),
                    style,
                )));
            }
            let history_widget = List::new(history_items);
            frame.render_widget(history_widget, self.history_area);

            frame.set_cursor(0, 0);
        });
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
