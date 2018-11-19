#!/usr/bin/env bash

ca65 mountain_golfing.s -o mountain_golfing.o 

ld65 mountain_golfing.o -C nesfile.ini -o mountain_golfing.nes