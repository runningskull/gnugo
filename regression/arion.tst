# NNGS game of GNU Go 2.7.203 on December 28, 2000

loadsgf games/arion.sgf 33
1 gg_genmove black 1
#? [Q6]

loadsgf games/arion.sgf 63
2 reg_genmove black
#? [Q18|S16]

# J3 is maybe not the largest move on the board but it is very clearly
# sounder than a move near P6.
loadsgf games/arion.sgf 69
3 restricted_genmove black J3 O6 P7 O5 O7 N6
#? [J3]

loadsgf games/arion.sgf 139
4 reg_genmove black
#? [F9]

# F15 is a blunder, leading to shortage of liberties and a combination
# attack. To make this clearer, first fill out the remaining dame points.
loadsgf games/arion.sgf 213
play black P13
play white D13
play black E4
5 reg_genmove black
#? [D17|E17|F17|H17|J17|K17|D18|E18|F18|G18|H18|J18|K18]

# F19 seems to be best way to live from a yose perspective.
loadsgf games/arion.sgf 225
6 reg_genmove black
#? [E19|F19|F18]
