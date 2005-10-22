# This regression file covers mistakes made in the Computer Go Ladder
# game between GNU Go 2.7.218 (white) and Ego (black) on
# February 8, 2001.

# An extension to K4 or K3 is urgent here.
loadsgf games/ego.sgf 12
1 reg_genmove white
#? [K4|K3]

# G3 looks good here but is maybe not the only move. E5 is clearly
# suboptimal, however.
loadsgf games/ego.sgf 30
2 restricted_genmove white E5 G3
#? [G3]

#CATEGORY=PATTERN_TUNING
#DESCRIPTION=K17 misses H18 threat.
#SEVERITY=8
# K18 is unreasonable. H18 is the only move locally.
loadsgf games/ego.sgf 52
3 reg_genmove white
#? [H18]

#CATEGORY=PATTERN_TUNING
#DESCRIPTION=Possible replacement pattern appropriate?
#SEVERITY=8
# The descent at G19 is bad shape because it leaves much aji around
# E18 and gives black an endgame sente move at H19.
loadsgf games/ego.sgf 54
4 restricted_genmove white G19 H19 K17 F18
#? [F18]*

#CATEGORY=
#DESCRIPTION=Why is B15 so bad?  -Trevor
#SEVERITY=0
# D7 is a good move.
loadsgf games/ego.sgf 68
5 reg_genmove white
#? [!B15]

loadsgf games/ego.sgf 86
6 restricted_genmove white M5 N5
#? [M5]

# Unreasonable monkey jump
loadsgf games/ego.sgf 96
7 restricted_genmove white Q1 P2
#? [P2]

#CATEGORY=BORDER_PROTECTION
#DESCRIPTION=Tough moyo protection.
#SEVERITY=5
loadsgf games/ego.sgf 112
8 reg_genmove white
#? [F12]

loadsgf games/ego.sgf 180
9 reg_genmove white
#? [S19]

loadsgf games/ego.sgf 190
10 reg_genmove white
#? [S18]


#CATEGORY=KO_READING
#DESCRIPTION=Tough - Black pushthrough is troublesome.
#SEVERITY=3
loadsgf games/ego.sgf 198
11 reg_genmove white
#? [T18|R16]*

# The correct move is the double sente at R10, but for the time being
# we also accept C12.
loadsgf games/ego.sgf 252
12 reg_genmove white
#? [R10|C12]

