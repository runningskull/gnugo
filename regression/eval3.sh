#!/bin/sh
nice -n 19 ./regress.pl \
  --goprog "../interface/gnugo.exe --mode gtp --quiet" \
  --verbose 1  \
  -a 1
