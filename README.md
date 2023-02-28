# charm

A hex editor that "isn't bad". 

![Screenshot of charm](doc/screenshot.png?raw=true)

## Building

### Ubuntu

1. Install dependencies:

```
sudo apt-get install libgtk-3-dev libgdk-pixbuf2.0-dev libatk1.0-dev libcairo2-dev libjpeg8-dev libpango1.0-dev libgif-dev build-essential g++
```

2. Build:

```
cargo build -F gtk
```
