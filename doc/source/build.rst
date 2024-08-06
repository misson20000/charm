Building & Installing
=====================

Linux
-----

Linux is currently the only platform supported by Charm. You will need the following dependencies installed:

- A Rust toolchain
- GTK 4 (with development headers)
- libadwaita (with development headers)
  
Ubuntu::
  
  sudo apt-get install libgtk-4-dev libadwaita-1-dev libgdk-pixbuf2.0-dev libatk1.0-dev libcairo2-dev libjpeg8-dev libpango1.0-dev libgif-dev build-essential g++

NixOS::
  
  nix-shell -p gtk4 gdk-pixbuf atk cairo libjpeg8 pango giflib pkg-config libadwaita

Once all dependencies are satisfied, you should be able to build and run Charm as any other Rust application::

  git clone https://github.com/misson20000/charm.git
  cd charm
  cargo build
  cargo run

.. warning::

   Certain Ubuntu releases ship GTK versions that may be too old for the default build configuration configuration, which depends on GTK 4.8. You can disable GTK 4.8 features and build like so, but you may have worse scrolling behavior on Wayland::
   
     cargo build --no-default-features -F gtk

MacOS
-----

At least one person has successfully built and run Charm natively for MacOS. I don't know how they did it.

Windows
-------

It should be possible to build and run Charm natively for Windows, but it has not yet been done.
