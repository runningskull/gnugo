#!/bin/sh

GNUGO=../interface/gnugo

tstfile=$1
options=$2   # May be omitted
options2=$3  # May be omitted  FIXME: Do multiple options better.

$GNUGO --quiet $options $options2 --mode gtp <$tstfile |\
	awk -f regress.awk tst=$tstfile verbose=1
