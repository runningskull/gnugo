#terrible connection mistake.
loadsgf games/trevor/trevor_01.sgf 28
1 gg_genmove white
#? [G5]

#probably a screwed up fuseki library!
loadsgf games/trevor/trevor_02.sgf
2 gg_genmove white
#? [!PASS]*

# underlying reading problem
loadsgf games/trevor/trevor_03.sgf 28
3 gg_genmove white
#? [!E3]*

#simple edge block missed.
loadsgf games/trevor/trevor_04.sgf 30
4 gg_genmove white
#? [G1]*

#E5 is the only move.  Is GnuGo playing somewhat randomly here?
loadsgf games/trevor/trevor_05.sgf 6
5 gg_genmove white
#? [E5]*

#locally B9 is terrible.  There are other big moves on the board.
loadsgf games/trevor/trevor_06.sgf 22
6 gg_genmove white
#? [B7|C6|F5]

#B5 is certainly better than C5 here.  This is a terrible
#game for white, but it's hard to narrow down the obvious
#blunders.
loadsgf games/trevor/trevor_07.sgf 32
7 gg_genmove white
#? [!C5]*

#Pattern missing here?  B8 is almost always better than B9.
loadsgf games/trevor/trevor_08.sgf 24
8 gg_genmove white
#? [!B9]

#E5 (or some defence of D4 worm) is necessary.
loadsgf games/trevor/trevor_09.sgf
9 gg_genmove white
#? [E5]*

#This is nearly trivial, it would seem!
loadsgf games/trevor/trevor_10.sgf
10 gg_genmove white
#? [E6]

#Yikes, gnugo can't play into here.  See also the next problem.
loadsgf games/trevor/trevor_11.sgf
11 gg_genmove white
#? [!B2]*

#J2 is terrible.  H1 leads at least to a Ko.
loadsgf games/trevor/trevor_12.sgf
12 gg_genmove black
#? [H1]*
