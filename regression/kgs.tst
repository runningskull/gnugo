loadsgf games/kgs/yagr.sgf 204
10 reg_genmove black
#? [A11]*

loadsgf games/kgs/yagr.sgf 206
20 reg_genmove black
#? [C10]*

loadsgf games/kgs/yagr2.sgf 65
30 reg_genmove black
#? [!P18|P17|O17]

loadsgf games/kgs/yagr3.sgf 78
40 restricted_genmove white A7 A6
#? [A6]*

loadsgf games/kgs/yagr4.sgf 38
50 reg_genmove black
#? [D15]*

loadsgf games/kgs/yagr4.sgf 184
60 restricted_genmove black J4 J8
#? [J8]

# Given the stage of the game, B2 is huge.
loadsgf games/kgs/yagr5.sgf 153
70 reg_genmove white
#? [B2]*

# Black could win the semeai, white must defend
loadsgf games/kgs/20040507-GnuGoCVS-read.sgf 212
80 reg_genmove white
#? [L1|O2|Q1]

# T8 only move. T7 only gives ko, but the owl code is missing
# it, because the tactical reading is proposing a bogus lunch
# defense (the self atari at T12 also results in a ko for the
# tactical reading routines, but fails in the owl context)
loadsgf games/kgs/20040507-GnuGoCVS-read.sgf 268
90 reg_genmove white
#? [T8]

# Semeai related problem. The status of D3 is revised to ALIVE
# because black can't win the semeai C3 vs. D3 (both live after
# B:C4 W:C1). The owl attack at C1 is then discarded, since the
# dragon isn't critical.
loadsgf games/kgs/20040516-GoBucks-GnuGoCVS.sgf 242
100 reg_genmove black
#? [C1]

loadsgf games/kgs/20040517-siRNA-GnuGoCVS.sgf 152
110 reg_genmove black
#? [G13]

# P18 is in danger now.
loadsgf games/kgs/20040517-siRNA-GnuGoCVS.sgf 192
120 reg_genmove black
#? [S18]

# White missed, P18 can still be saved.
loadsgf games/kgs/20040517-siRNA-GnuGoCVS.sgf 198
130 reg_genmove black
#? [S18]
