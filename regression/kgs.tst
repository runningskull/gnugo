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
#? [L1|O2|Q1]*

# T8 only move. T7 only gives ko, but the owl code is missing
# it, because the tactical reading is proposing a bogus lunch
# defense (the self atari at T12 also results in a ko for the
# tactical reading routines, but fails in the owl context)
loadsgf games/kgs/20040507-GnuGoCVS-read.sgf 268
90 reg_genmove white
#? [T8]*

# Semeai related problem. The status of D3 is revised to ALIVE
# because black can't win the semeai C3 vs. D3 (both live after
# B:C4 W:C1). The owl attack at C1 is then discarded, since the
# dragon isn't critical.
loadsgf games/kgs/20040516-GoBucks-GnuGoCVS.sgf 242
100 reg_genmove black
#? [C1]*

loadsgf games/kgs/20040517-siRNA-GnuGoCVS.sgf 152
110 reg_genmove black
#? [G13]*

# P18 is in danger now.
loadsgf games/kgs/20040517-siRNA-GnuGoCVS.sgf 192
120 reg_genmove black
#? [S18]*

# White missed, P18 can still be saved.
loadsgf games/kgs/20040517-siRNA-GnuGoCVS.sgf 198
130 reg_genmove black
#? [S18]*

# Awful tenuki.
loadsgf games/kgs/2004-04-10-GnuGoCVS-inuyasha34.sgf 56
140 reg_genmove white
#? [P14|N15|G12]*

# Another tenuki from same fight - lots of cutting points are opportunities
# for escape.
loadsgf games/kgs/2004-04-10-GnuGoCVS-inuyasha34.sgf 64
150 reg_genmove white
#? [M14|Q13]*

# Later, ignores a fight and misreads a ladder - anything related to the
# fight or the ladder is better than the move made in the game.
loadsgf games/kgs/2004-04-10-GnuGoCVS-inuyasha34.sgf 94
160 reg_genmove white
#? [G12|H8|J7|G7]*

# Again the move made in the game doesn't help the fight in the slightest
# nor resolve the ladder.
loadsgf games/kgs/2004-04-10-GnuGoCVS-inuyasha34.sgf 96
170 reg_genmove white
#? [G12|H8|G7]*

loadsgf games/kgs/geluba-yagr.sgf 164
180 reg_genmove black
#? [C4|C3|D3]*

# Only way to control damage: sacrifice the three stones at E3
loadsgf games/kgs/geluba-yagr.sgf 166
190 reg_genmove black
#? [C4]*

# Don't kill your own group.
loadsgf games/kgs/yagr-ituyosi.sgf 160
200 restricted_genmove white K4 M4 L4
#? [!K4]*

# An "owl attack captures tail" problem
loadsgf games/kgs/yagr-gab9.2.sgf 158
210 reg_genmove white
#? [Q13]*

# Connect the monkey jump instead of running nowhere!
loadsgf games/kgs/yagr-Sapojnik.sgf 80
220 reg_genmove white
#? [T10]*

# Win the semeai!
loadsgf games/kgs/yagr-Kazik.sgf 149
230 reg_genmove white
#? [J12|J11|L10|L9|J19]*

loadsgf games/kgs/yagr-yudeta.sgf 257
240 reg_genmove white
#? [B7]*

# In the game, GNU Go ran on the first line until the very end :((
loadsgf games/kgs/yagr-yudeta.sgf 78
250 reg_genmove white
#? [P1]*

# Huge semeai.
loadsgf games/kgs/malitourne-yagr.sgf 240
260 reg_genmove black
#? [T10|Q11|O11]*

# Same problem as test 250.
loadsgf games/kgs/malitourne-yagr.sgf 80
270 restricted_genmove white O1 T1
#? [T1]*

loadsgf games/kgs/yagr-hasenhirn.sgf 71
280 reg_genmove white
#? [G1]*

# A ladder desaster.
loadsgf games/kgs/yagr-czarny.sgf 16
290 reg_genmove white
#? [B2]*

loadsgf games/kgs/yagr-czarny.sgf 24
300 restricted_genmove white H4 H5
#? [H4]*

# N15 as played in the game is off-topic.
loadsgf games/kgs/yagr-awf.sgf 191
310 reg_genmove white
#? [F11|H10|J15|K12|F19|D18]

# Probably owl cut problem. At least have to try to win the semeai.
loadsgf games/kgs/yagr-Mythenmetz.sgf 156
320 reg_genmove white
#? [K12]*


