# =============================
# Private test set nicklas3.tst
# These tests are from 9x9 games.
# None of these tests passes with version 2.7.222.
# I think these problems are "non trivial".
# =============================

loadsgf games/nicklas/nicklas4.sgf 73
401 reg_genmove black
#? [A5]

loadsgf games/nicklas/nicklas6.sgf 25
602 reg_genmove black
#? [C6]

# J6 gives a gote seki.
# However, playing sente moves against the group, starting with H9,
# and then taking gote at H1 gives the same end result. Thus H9 is
# also okay.
loadsgf games/nicklas/nicklas14.sgf 48
1403 reg_genmove white
#? [J6|H9]*
