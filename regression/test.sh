#!/bin/sh

GNUGO=../interface/gnugo

gtpfile=$1
options=$2   # May be omitted
options2=$3  # May be omitted  FIXME: Do multiple options nicer.

$GNUGO --quiet $options $options2 --mode gtp <$gtpfile | egrep '^[=?][0-9]+' | cut -c 2-
