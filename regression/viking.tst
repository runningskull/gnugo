# This test suite is based on games against the program Viking by
# Magnus Persson.

loadsgf games/viking1.sgf 15
1 gg_genmove white 
#? [O15]*

loadsgf games/viking1.sgf 23
2 gg_genmove white 
#? [E2]

loadsgf games/viking1.sgf 65
3 gg_genmove white 
#? [M15|B7]*

loadsgf games/viking1.sgf 135
4 gg_genmove white 
#? [B17|B18]*

loadsgf games/viking1.sgf 187
5 gg_genmove white 
#? [S5]

loadsgf games/viking1.sgf 189
6 gg_genmove white 
#? [B17|B18]

# Locally S5 is the only move.
loadsgf games/viking3.sgf 79
7 gg_genmove white
#? [!T7]

# A1 is of course inferior to B3
loadsgf games/viking3.sgf 93
8 gg_genmove white
#? [!A1]

# The position cries out for a move at J13.
loadsgf games/viking3.sgf 103
9 gg_genmove white
#? [J13]

# K15 leaves too many cutting points.
loadsgf games/viking3.sgf 113
10 gg_genmove white
#? [K16]*

loadsgf games/viking3.sgf 113
11 gg_genmove white
#? [!G19]

loadsgf games/viking3.sgf 137
12 gg_genmove white
#? [S14|S15]

# J9 is an overplay since it can be cut off.
#CATEGORY=CONNECTION
# J10 is not much better either, as black can cut J10/K11 off with M11.
# This is a non-transitivity connection problem.
# L10 looks best, but anything repairing the connection K11/N9 should
# make us happy. /ab
loadsgf games/viking3.sgf 213
13 gg_genmove white
#? [L10|L11|M11|N10|K7]*

