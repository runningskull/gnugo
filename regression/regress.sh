#!/bin/sh

GNUGO=../interface/gnugo

srcdir=$1
tstfile=$2
options=$3   # May be omitted
options2=$4  # May be omitted  FIXME POST3.0: Do multiple options nicer.

$GNUGO --quiet $options $options2 --mode gtp <$tstfile |\
	awk -f $srcdir/regress.awk tst=$tstfile
