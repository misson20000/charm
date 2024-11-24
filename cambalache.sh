#!/bin/bash
cargo build
g-ir-compiler gir/Charm-0.1.gir -o gir/Charm-0.1.typelib
mkdir -p $HOME/.cambalache/catalogs/
cp gir/charm-cambalache-catalog.xml $HOME/.cambalache/catalogs/
export GI_TYPELIB_PATH=$PWD/gir/
export LD_PRELOAD=target/debug/libcharm.so
cambalache src/view/charm.cmb
