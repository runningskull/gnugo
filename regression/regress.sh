#!/bin/sh

if test ! "$GNUGO" ; then
	GNUGO=../interface/gnugo
fi

srcdir=$1
tstfile=$2
options=$3   # May be omitted
options2=$4  # May be omitted
options3=$5  # May be omitted
options4=$6  # May be omitted  FIXME: Do multiple options nicer.

$GNUGO --quiet $options $options2 $options3 $options4 --mode gtp <$tstfile |\
	awk -f $srcdir/regress.awk tst=$tstfile
