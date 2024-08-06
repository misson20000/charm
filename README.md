# Charm

A structure-first hex editor focused on improving the user experience in manually annotating binary file structure.

![Screenshot of charm](doc/screenshot.png?raw=true)

## Current Features

- Full 64-bit addressing
- Asynchronous I/O
- Guaranteed correct undo
- Dark mode and light mode
- Written in Rust

## Near-term Features

- Non-destructive editing

## Planned Features

- Arrays and reusable types
- Python analyzer API
- Bit-level addressing (also already halfway there)

## Building

### Ubuntu

1. Install dependencies:

```
sudo apt-get install libgtk-4-dev libadwaita-1-dev libgdk-pixbuf2.0-dev libatk1.0-dev libcairo2-dev libjpeg8-dev libpango1.0-dev libgif-dev build-essential g++
```

2. Build:

```
cargo build
```

On certain versions of Ubuntu, your version of GTK may be too old for the default configuration, which depends on GTK 4.8. You can disable GTK 4.8 features and build like so, but you may have worse scrolling behavior on Wayland.

```
cargo build --no-default-features -F gtk
```

### NixOS

```
nix-shell -p gtk4 gdk-pixbuf atk cairo libjpeg8 pango giflib pkg-config libadwaita
```
