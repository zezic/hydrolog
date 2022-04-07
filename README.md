# Hydrolog

Rusty debug logs inspector

## Installing

```bash
git clone git@github.com:zezic/hydrolog.git
cd hydrolog
cargo install --path .
```

## Usage

```bash
tail -f your_file.log | hydrolog
```

## Hotkeys

- <kbd>Q</kbd> – Quit

- <kbd>C</kbd> – Toggle Crypto Connector Logs
- <kbd>M</kbd> – Toggle Monitor Connector Logs
- <kbd>D</kbd> – Toggle Domain Logs

- <kbd>I</kbd> – Toggle Inspector Mode
    - <kbd>↑</kbd> - Navigate Back (Up)
    - <kbd>↓</kbd> - Navigate Forward (Down)

## Todo

- [ ] Handle the end of stream, do not hang or crash
- [ ] Support fast (PgUp/PgDown) navigation
- [ ] Allow to adjust the inspector window geometry
- [ ] Support scroll inside inspector for long data structs
- [ ] Do not pollute the terminal emulator scrollback buffer with the UI

## Notes

Expect bugs.