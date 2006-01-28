# =============================
# Private test set nicklas5.tst
# These tests are from 19x19 games.
# None of these tests passes with version 2.7.222.
# I think these problems are "non trivial".
# =============================

loadsgf games/nicklas/nicklas8.sgf 116
802 reg_genmove black
#? [O12]

loadsgf games/nicklas/nicklas8.sgf 134
803 reg_genmove black
#? [F7|G7]*

loadsgf games/nicklas/nicklas8.sgf 140
804 reg_genmove black
#? [D6]*

# Locally K18 is superior to L17.
loadsgf games/nicklas/nicklas12.sgf 69
1202 restricted_genmove black K18 L17
#? [K18]

# I guess K18, giving up the O17 stones and attacking H17 in exchange,
# is playable. /ab
loadsgf games/nicklas/nicklas12.sgf 71
1203 reg_genmove black
#? [K18|N18|N19|M19]*


# tm - Looks like a semeai problem.  E18 is good except for semeai.
loadsgf games/nicklas/nicklas12.sgf 89
1204 reg_genmove black
#? [H15]*

# This is presumably a semeai problem.
# W L8 is alive in semeai. /ab
loadsgf games/nicklas/nicklas12.sgf 182
1211 dragon_status L8
#? [alive]

# This is presumably a semeai problem.
# As W L8 is alive in semeai with sufficiently many liberties ahead of
# B G13, I do not think the black P13 dragon can make eyes. /ab
loadsgf games/nicklas/nicklas12.sgf 199
1212 dragon_status P13
#? [dead]

loadsgf games/nicklas/nicklas12.sgf 263
1217 reg_genmove black
#? [F3]*
