#!/bin/sh
nice -n 19 ./regress.pl \
  --goprog "../interface/gnugo.exe --mode gtp --quiet" \
  --verbose 1 \
	reading.tst \
	owl.tst  \
	owl_rot.tst  \
	ld_owl.tst  \
	optics.tst  \
	life.tst \
	filllib.tst  \
	atari_atari.tst  \
	connection.tst  \
	blunder.tst  \
	strategy.tst  \
	strategy2.tst  \
	endgame.tst  \
	heikki.tst  \
	neurogo.tst  \
	arb.tst  \
	rosebud.tst  \
	golife.tst  \
	arion.tst  \
	viking.tst  \
	ego.tst  \
	dniwog.tst  \
	strategy3.tst \
	strategy4.tst \
	nicklas1.tst  \
	nicklas2.tst  \
	nicklas3.tst  \
	nicklas4.tst  \
	nicklas5.tst  \
	manyfaces.tst  \
	niki.tst  \
	trevor.tst \
	tactics.tst
