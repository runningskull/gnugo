#!/bin/sh

if test ! "$GNUGO"; then
	GNUGO=../interface/gnugo
fi

tstfile=$1
options=$2   # May be omitted
options2=$3  # May be omitted
options3=$4  # May be omitted
options4=$5  # May be omitted  FIXME: Do multiple options better.

$GNUGO --quiet $options $options2 $options3 $options4 --mode gtp <$tstfile |\
	awk -f regress.awk tst=$tstfile verbose=1
