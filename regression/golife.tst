# This regression file covers mistakes made in the 9x9 Computer Go
# Ladder game between GNU Go 2.7.187 (white) and GoLife I (black) on
# November 2, 2000.

loadsgf games/golife.sgf 16
1 reg_genmove white
#? [C7]

loadsgf games/golife.sgf 26
2 reg_genmove white
#? [H5]

# In this position the monkey jump can be cut with ko and is smaller
# than a move at the bottom anyway.
loadsgf games/golife.sgf 30
3 reg_genmove white
#? [E3|E2]*

# # H5 is nice but not the only move. Securing the connection to A3 or
# # expanding around E2 or E3 is also okay. Testcase removed.
# loadsgf games/golife.sgf 32
# 4 reg_genmove white
# #? [H5]

# GNU Go did play this connection. The test is to make sure it never forgets it.
loadsgf games/golife.sgf 34
5 reg_genmove white
#? [H5]

# G6 is out of the question here. It's harder to decide between B2 and F4.
loadsgf games/golife.sgf 38
6 reg_genmove white
#? [B2|F4]*

# See also test case owl:192
# F3 does kill black.
#
#    A B C D E F G H J
#  9 . . . . . . X O . 9          O=white
#  8 X X . O . O X X O 8          X=black
#  7 O X O . . O O O . 7
#  6 O O O X X X O . O 6
#  5 . . X . X O . O X 5
#  4 . . X 3 4 X O O . 4
#  3 O . X . 2 1 O . . 3
#  2 . O . X X X X O . 2
#  1 . . . 7 . 6 5 . . 1
#    A B C D E F G H J
#
loadsgf games/golife.sgf 46
7 reg_genmove white
#? [F3]*

# Connecting the one point ko at G5 is too small. Either C2 or G1 looks
# largest.
loadsgf games/golife.sgf 48
8 reg_genmove white
#? [C2|G1]

loadsgf games/golife.sgf 56
9 reg_genmove white
#? [C1]

loadsgf games/golife.sgf 60
10 reg_genmove white
#? [D7]

