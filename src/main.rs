use crossbeam::channel::{select, Receiver, Sender};
use std::collections::{HashMap, VecDeque};
use std::io::{stdout, Stdout, StdoutLock, Write};
use std::time::{Duration, Instant};
use std::{
    io::{self, BufRead},
    str::FromStr,
};

use anyhow::{Context, Result};
use log::Level;
use pad::PadStr;
use termion::color;
use termion::event::Key;
use termion::input::TermRead;
use termion::raw::{IntoRawMode, RawTerminal};
use termion::{async_stdin, style};

const MAX_RECS: usize = 1_000_000_000;

#[derive(Clone, Debug)]
struct Time {
    parts: [String; 4],
}

impl Time {
    fn from_str(text: &str) -> Result<Self> {
        let mut splitter = text.splitn(3, ':');
        let h = splitter.next().context("Can't parse hours")?;
        let m = splitter.next().context("Can't parse minutes")?;
        let s = splitter.next().context("Can't parse seconds")?;

        let mut s_splitter = s.split('.');

        let s = s_splitter.next().context("Can't parse seconds from s.ms")?;
        let ms = s_splitter
            .next()
            .context("Can't parse milliseconds from s.ms")?;

        let parts = [h, m, s, ms].map(String::from);

        Ok(Self { parts })
    }

    fn now() -> Self {
        let now = chrono::offset::Local::now();
        let h = now.format("%H");
        let m = now.format("%M");
        let s = now.format("%S");
        let ms = now.format("%.3f");

        let parts = [h, m, s, ms].map(|x| x.to_string());

        Self { parts }
    }

    fn render(&self, other: Option<&Self>) -> String {
        fn part_color(equal: bool) -> String {
            if equal {
                color::Fg(color::LightBlack).to_string()
            } else {
                color::Fg(color::Reset).to_string()
            }
        }
        if let Some(other) = other {
            format!(
                "{}{}:{}{}:{}{}.{}{}",
                part_color(self.parts[0] == other.parts[0]),
                self.parts[0],
                part_color(self.parts[1] == other.parts[1]),
                self.parts[1],
                part_color(self.parts[2] == other.parts[2]),
                self.parts[2],
                part_color(self.parts[3] == other.parts[3]),
                self.parts[3]
            )
        } else {
            format!(
                "{}:{}:{}.{}",
                self.parts[0], self.parts[1], self.parts[2], self.parts[3]
            )
        }
    }
}

#[derive(Clone, Debug)]
struct Record {
    level: Level,
    time: Time,
    path: Option<String>,
    message: String,
}

impl Record {
    fn from_text(text: &str) -> Result<Self> {
        let mut splitter = text.splitn(4, ' ');
        let _date = splitter.next().context("Can't parse month")?;
        let time = splitter.next().context("Can't parse time")?;
        let level_str = splitter.next().context("Can't parse level")?;
        let level = Level::from_str(level_str)
            .map_err(|err| anyhow::anyhow!(err))
            .context("Can't parse level")?;
        let message = splitter.next().context("Can't parse message")?;

        if let Some(_) = splitter.next() {
            return Err(anyhow::anyhow!("You have some string parts unhandled!"));
        }

        let separator_idx = message.find(" - ");

        let (path, msg) = if let Some(separator_idx) = separator_idx {
            let (path, msg) = message.split_at(separator_idx);
            let msg = &msg[3..];
            (Some(path.into()), msg)
        } else {
            (None, message)
        };

        Ok(Self {
            level,
            time: Time::from_str(time)?,
            path,
            message: msg.into(),
        })
    }

    fn from_msg(msg: &str) -> Self {
        Self {
            level: Level::Trace,
            time: Time::now(),
            path: None,
            message: msg.into()
        }
    }

    fn render(
        &self,
        stdou: &mut RawTerminal<Stdout>,
        mut max_path_len: usize,
        mut duplicate_count: usize,
        other: &Option<Self>,
        highlight: bool,
        show_dups: bool,
    ) -> (usize, usize) {
        let level_color = if highlight {
            color::Fg(color::LightWhite).to_string()
        } else {
            match self.level {
                Level::Error => color::Fg(color::Red).to_string(),
                Level::Warn => color::Fg(color::Yellow).to_string(),
                Level::Info => color::Fg(color::Blue).to_string(),
                Level::Debug => color::Fg(color::LightBlack).to_string(),
                Level::Trace => color::Fg(color::LightBlack).to_string(),
            }
        };
        let level_string = format!("{}{:5}", level_color, self.level);
        let time_string = self.time.render(other.as_ref().map(|other| &other.time));

        let path = if let Some(path) = &self.path {
            max_path_len = path.len().max(max_path_len);
            path.pad_to_width_with_alignment(max_path_len, pad::Alignment::Right)
        } else {
            " ".repeat(max_path_len)
        };

        let (w, h) = termion::terminal_size().expect("Can't get terminal size");

        let space_for_msg = (w as usize) - 20 - max_path_len;

        let trunc_text = " ...";
        let trunc_indicator = format!(
            "{}{}{}",
            color::Fg(color::LightBlack),
            trunc_text,
            color::Fg(color::Reset)
        );

        let msg_len = self.message.len();

        let msg = if msg_len > space_for_msg {
            let mut new_msg = self.message.clone();
            new_msg.truncate(space_for_msg - trunc_text.len());
            new_msg += &trunc_indicator;
            new_msg
        } else {
            self.message.clone()
        };

        let msg_color = if let Some(other) = other {
            if other.message == self.message && show_dups {
                color::Fg(color::LightBlack).to_string()
            } else {
                color::Fg(color::Reset).to_string()
            }
        } else {
            color::Fg(color::Reset).to_string()
        };

        let view = format!(
            "{}{} {} {}{} {}{}{}",
            if highlight {
                style::Bold.to_string()
            } else {
                style::NoBold.to_string()
            },
            level_string,
            time_string,
            if highlight {
                color::Fg(color::LightWhite).to_string()
            } else {
                color::Fg(color::LightBlack).to_string()
            },
            path,
            msg_color,
            msg,
            style::NoBold,
        );

        if let Some(other) = other {
            if other.message != self.message {
                write!(stdou, "{}\r\n", view).unwrap();
                duplicate_count = 1;
            }
            if other.message == self.message {
                if show_dups {
                    write!(stdou, "{}\r\n", view).unwrap();
                }
                duplicate_count += 1;
                let stdout = stdout();
                let mut stdout = stdout.lock().into_raw_mode().unwrap();
                write!(stdout, "{}", termion::cursor::Save).unwrap();
                write!(
                    stdout,
                    "{}[{:1X}]",
                    termion::cursor::Goto(w - 2, h - 1),
                    duplicate_count
                )
                .unwrap();
                write!(stdout, "{}", termion::cursor::Restore).unwrap();
                stdout.flush().unwrap();
            }
        } else {
            duplicate_count = 1;
            write!(stdou, "{}\r\n", view).unwrap();
        }

        (max_path_len, duplicate_count)
    }

    fn inspect(&self, stdout: &mut RawTerminal<StdoutLock>) -> Result<()> {
        let mut indent = 0;
        let tab_size = 4;

        let mut lines: Vec<(String, usize)> = vec![];
        let mut buf = String::new();
        let mut len = 0;
        let time_ren = self.time.render(None);
        buf += &format!("{}{} ", color::Fg(color::Red), time_ren);
        len += 13;

        #[derive(PartialEq)]
        enum Glyph {
            Alpha,
            Digit,
            Punc,
            NewLineReason,
        }
        let mut last_glyph = Glyph::Alpha;

        let mut is_quoted_string = false;
        let mut last_quoted_str = false;

        for c in self.message.chars() {
            if c == '}' {
                buf += " ";
                len += 1;
                lines.push((buf, len));
                indent -= tab_size;
                buf = " ".repeat(indent);
                len = indent;
            }

            if c == '"' {
                is_quoted_string ^= true;
            }

            let cur_glyph = if c == ',' || c == '{' {
                Glyph::NewLineReason
            } else if c.is_digit(10) {
                Glyph::Digit
            } else if c.is_ascii_punctuation() && c != '_' {
                Glyph::Punc
            } else {
                Glyph::Alpha
            };

            if !is_quoted_string && cur_glyph != last_glyph {
                buf += &format!(
                    "{}",
                    match cur_glyph {
                        Glyph::Alpha => {
                            if last_glyph == Glyph::NewLineReason {
                                color::Fg(color::LightWhite).to_string()
                            } else {
                                color::Fg(color::Green).to_string()
                            }
                        }
                        Glyph::Digit => color::Fg(color::Magenta).to_string(),
                        Glyph::Punc => color::Fg(color::LightBlack).to_string(),
                        Glyph::NewLineReason => color::Fg(color::LightBlack).to_string(),
                    }
                );
            }

            if is_quoted_string != last_quoted_str {
                buf += &format!("{}", color::Fg(color::LightYellow));
            }

            last_quoted_str = is_quoted_string;

            last_glyph = cur_glyph;

            buf += &c.to_string();
            len += 1;

            if c == ',' {
                buf += " ";
                len += 1;
                lines.push((buf, len));
                buf = " ".repeat(indent);
                len = indent;
            }
            if c == '{' {
                indent += tab_size;
                buf += " ";
                len += 1;
                lines.push((buf, len));
                buf = " ".repeat(indent);
                len = indent;
            }
        }

        lines.push((buf, len));

        let longest_line = lines.iter().map(|(_s, len)| *len).max().unwrap_or_default();

        let mut y = 2;

        let indent_base = 20;

        let border_color = color::Fg(color::White);

        write!(stdout, "{}", termion::cursor::Goto(indent_base as u16, y)).unwrap();
        write!(stdout, "{}╭─{}─╮ ", border_color, "─".repeat(longest_line)).unwrap();
        y += 1;

        for (mut line, len) in lines {
            line = line.replace(
                "Rejected",
                &format!(
                    "{}{}Rejected{}",
                    color::Fg(color::Yellow),
                    style::Bold,
                    style::NoBold
                ),
            );
            line = line.replace(
                "PendingNew",
                &format!(
                    "{}{}PendingNew{}",
                    color::Fg(color::Cyan),
                    style::Bold,
                    style::NoBold
                ),
            );
            let pad = longest_line.saturating_sub(len);
            let padding = " ".repeat(pad);
            write!(stdout, "{}", termion::cursor::Goto(indent_base as u16, y)).unwrap();
            write!(
                stdout,
                "{}│ {}{} {}│ ",
                border_color, line, padding, border_color,
            )
            .unwrap();
            y += 1;
        }

        write!(stdout, "{}", termion::cursor::Goto(indent_base as u16, y)).unwrap();
        write!(stdout, "╰─{}─╯ ", "─".repeat(longest_line)).unwrap();

        Ok(())
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
enum Module {
    CryptoConnector,
    Domain,
    MonitorConnector,
}

impl Module {
    fn contained_in(&self, text: &str) -> bool {
        text.contains(match self {
            Module::CryptoConnector => "crypto_connector",
            Module::Domain => "domain",
            Module::MonitorConnector => "monitor",
        })
    }
}

struct Ui {
    modules: HashMap<Module, bool>,
    is_inspecting: bool,
    offset: usize,
    lag: usize,
    dirty: bool,
    mem: usize,
    decimator: usize,
}

impl Ui {
    fn new() -> Self {
        let modules = [
            (Module::CryptoConnector, true),
            (Module::Domain, true),
            (Module::MonitorConnector, true),
        ];
        Self {
            modules: modules.into_iter().collect(),
            is_inspecting: false,
            offset: 0,
            lag: 0,
            dirty: true,
            mem: 0,
            decimator: 0,
        }
    }

    fn want_to_show_rec(&self, rec: &Record) -> bool {
        if let Some(path) = &rec.path {
            !self
                .modules
                .iter()
                .any(|(key, val)| !*val && key.contained_in(path))
        } else {
            true
        }
    }
}

fn handle_key(ui: &mut Ui, key: &Key) {
    match key {
        Key::Char('c' | 'd' | 'm') => {
            ui.dirty = true;
            let module = match key {
                Key::Char('c') => Module::CryptoConnector,
                Key::Char('d') => Module::Domain,
                Key::Char('m') => Module::MonitorConnector,
                _ => unreachable!(),
            };
            ui.modules
                .entry(module)
                .and_modify(|enabled| *enabled ^= true);
        }
        Key::Char('i') => {
            ui.dirty = true;
            ui.is_inspecting ^= true;

            if !ui.is_inspecting {
                return;
            }

            ui.offset = 0;
            ui.lag = 0;
        }
        Key::Up => {
            ui.dirty = true;
            let (_w, h) = termion::terminal_size().expect("Can't get terminal size");
            if ui.offset as u16 == (h - 2) {
                ui.lag += 1;
            } else {
                ui.offset = ui.offset + 1;
            }
        }
        Key::Down => {
            ui.dirty = true;
            if ui.offset == 0 {
                ui.lag = ui.lag.saturating_sub(1);
            } else {
                ui.offset = ui.offset.saturating_sub(1)
            }
        }
        _ => {}
    }
}

fn handle_msg(
    mut stdout: &mut RawTerminal<Stdout>,
    state: &mut State,
    ui: &Ui,
    records: &mut VecDeque<Record>,
    text: String,
) -> bool {
    let rec = match Record::from_text(&text) {
        Ok(rec) => {
            rec
        }
        Err(_err) => {
            Record::from_msg(&text)
        }
    };

    if records.len() == MAX_RECS {
        records.pop_front();
    }
    records.push_back(rec.clone());

    if ui.is_inspecting {
        return false;
    }

    if ui.want_to_show_rec(&rec) {
        let (max_path_len, new_dup_cnt) = rec.render(
            &mut stdout,
            state.max_path_len,
            state.duplicate_cnt,
            &state.last_rec,
            false,
            false,
        );
        state.max_path_len = max_path_len;
        state.duplicate_cnt = new_dup_cnt;
    }

    state.last_rec = Some(rec);

    return true;
}

struct State {
    max_path_len: usize,
    duplicate_cnt: usize,
    last_rec: Option<Record>,
}

fn main_thread(krx: Receiver<Key>, mrx: Receiver<String>, srx: Receiver<()>) -> Result<()> {
    let mut stdout = stdout().into_raw_mode().unwrap();

    write!(stdout, "{}", termion::clear::All).unwrap();
    write!(stdout, "{}", termion::cursor::Goto(1, 1)).unwrap();


    let mut state = State {
        max_path_len: 0,
        duplicate_cnt: 1,
        last_rec: None,
    };

    let mut ui = Ui::new();

    let mut records = VecDeque::<Record>::new();

    loop {
        select! {
            recv(krx) -> key => {
                if let Ok(key) = key {
                    handle_key(&mut ui, &key);
                } else {
                    break Ok(());
                }
            },
            recv(mrx) -> text => {
                if let Ok(text) = text {
                    let has_rendered = handle_msg(&mut stdout, &mut state, &ui, &mut records, text);
                    if has_rendered {
                        ui.dirty = true;
                    }
                    ui.lag += 1;
                } else {
                    break Ok(());
                }
            },
            recv(srx) -> _shutdown_msg => {
                break Ok(());
            }
        }

        if ui.dirty {
            draw_ui(&mut ui, &records, &mut stdout);
            ui.dirty = false;
        }
        stdout.flush().unwrap();
    }
}

fn handle_stdin() -> Result<()> {
    let stdin = async_stdin();
    let mut keys = stdin.keys();

    let (ktx, krx) = crossbeam::channel::unbounded();
    let (mtx, mrx) = crossbeam::channel::unbounded();
    let (stx, srx) = crossbeam::channel::unbounded::<()>();
    let (stx_2, srx_2) = crossbeam::channel::unbounded::<()>();

    let stx_r = stx.clone();

    let reader_handle = std::thread::Builder::new()
        .name("stdin reader".into())
        .spawn(move || {
            stdin_reader(mtx, srx, stx_r);
        })
        .expect("Can't spawn reader thread");

    let main_thread_handle = std::thread::Builder::new()
        .name("main thread".into())
        .spawn(move || main_thread(krx, mrx, srx_2))
        .expect("Can't spawn keyb thread");

    loop {
        if let Some(Ok(key)) = keys.next() {
            if let Key::Char('q') | Key::Esc = key {
                break;
            }
            if let Err(_err) = ktx.send(key) {
                break;
            }
        }
        std::thread::sleep(Duration::from_millis(20));
    }
    stx.send(()).expect("Can't send shutdown message");
    stx_2.send(()).expect("Can't send shutdown message (2)");

    reader_handle.join().expect("Can't join reader handle");
    main_thread_handle.join().expect("Can't join reader handle")
}

fn draw_ui(ui: &mut Ui, records: &VecDeque<Record>, stdou: &mut RawTerminal<Stdout>) {
    let stdout = stdout();
    let mut stdout = stdout.lock().into_raw_mode().unwrap();
    write!(stdout, "{}", termion::cursor::Save).unwrap();

    if ui.is_inspecting {
        write!(stdout, "{}", termion::clear::All).unwrap();

        let (_w, h) = termion::terminal_size().expect("Can't get terminal size");
        let mut max_path_len = 0;

        write!(stdout, "{}", termion::cursor::Goto(1, 1)).unwrap();

        let mut idx = (records.len() - 1).saturating_sub(ui.lag);
        let mut recs: Vec<Record> = vec![];

        while recs.len() < (h - 1) as usize {
            let rec = &records[idx];
            if ui.want_to_show_rec(&rec) {
                recs.push(rec.clone());
            }
            if idx == 0 {
                break;
            }
            idx -= 1;
        }

        recs.reverse();
        let target_idx = recs.len().saturating_sub(ui.offset).saturating_sub(1);
        let mut dup_count = 0;
        let mut last_rec = None;
        for (idx, rec) in (&recs).iter().enumerate() {
            let highlight = target_idx == idx;
            (max_path_len, dup_count) =
                rec.render(stdou, max_path_len, dup_count, &last_rec, highlight, true);
            last_rec = Some(rec.clone());
        }

        if let Err(err) = recs[target_idx].inspect(&mut stdout) {
            write!(
                stdout,
                "{}Can't inspect: {}",
                termion::cursor::Goto(1, 2),
                err
            )
            .unwrap();
        }

        let mark_position = h.saturating_sub(ui.offset as u16).max(1) as u16 - 1 - (h.saturating_sub(recs.len() as u16 + 1));

        let (w, _h) = termion::terminal_size().expect("Can't get terminal size");
        write!(
            stdout,
            "{}{}<<<",
            termion::cursor::Goto(w - 2, mark_position),
            color::Fg(color::Cyan)
        )
        .unwrap();
    }

    let mut status: String = ui
        .modules
        .iter()
        .map(|(module, state)| {
            let modline = format!(
                "{}[{:?}] ",
                match state {
                    true => color::Fg(color::Reset).to_string(),
                    false => color::Fg(color::LightBlack).to_string(),
                },
                module
            );
            modline
        })
        .collect();
    status += &format!("({}) ", ui.lag);
    ui.decimator += 1;
    if ui.decimator % 1024 == 0 {
        ui.decimator = 1;
        let me = procfs::process::Process::myself().unwrap();
        let mems = me.statm().unwrap();
        ui.mem = mems.resident as usize * 4096 / 1024 / 1024;
    }
    status += &format!("RAM: [{} MB]", ui.mem);
    write!(stdout, "{}{}", termion::cursor::Goto(1, 1), status).unwrap();
    write!(stdout, "{}", termion::cursor::Restore).unwrap();
    stdout.flush().unwrap();
}

fn stdin_reader(mtx: Sender<String>, srx: Receiver<()>, stx: Sender<()>) {
    let stdin = io::stdin();
    let lines = stdin.lock().lines();

    for text in lines {
        if srx.try_recv().is_ok() {
            break;
        }
        if let Ok(text) = text {
            if let Err(_err) = mtx.send(text) {
                break;
            }
        }
    }
    stx.send(())
        .expect("stdin_reader can't send shutdown message");
}

fn main() -> Result<()> {
    let exit_reason = handle_stdin();
    if let Err(err) = exit_reason {
        println!("{}Exit reason: {}", color::Fg(color::Reset), err);
    }
    Ok(())
}
