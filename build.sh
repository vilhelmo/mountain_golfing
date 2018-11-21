#!/usr/bin/env bash

ca65 mountain_golfing.asm -o mountain_golfing.o --debug-info

ld65 mountain_golfing.o -C nesfile.ini -o mountain_golfing.nes --dbgfile mountain_golfing.dbg
