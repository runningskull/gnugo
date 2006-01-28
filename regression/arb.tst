# ==============
# GNU Go mistakes in games against or observed by Teun
# ==============

loadsgf games/arb/game01.sgf 53
101 reg_genmove black
#? [(C2|D2|F2|G2|F1|D1|E1)]

loadsgf games/arb/game01.sgf 54
102 reg_genmove white
#? [D2|F2|A2]

loadsgf games/arb/game02.sgf 69
103 reg_genmove black
#? [PASS]

loadsgf games/arb/game02.sgf 71
104 reg_genmove black
#? [A7|B7]

loadsgf games/arb/dumonteil-1.sgf 184
150 reg_genmove white
#? [J5]

# Why is this move so urgent? /gf
# tm - F14 is not necessary - removing this bad problem.
#loadsgf games/arb/dumonteil-2.sgf 229
#200 reg_genmove black
##? [F14]

loadsgf games/arb/dumonteil-2.sgf 247
201 reg_genmove black
#? [H13]

loadsgf games/arb/dumonteil-2.sgf 251
202 restricted_genmove black B19 D15 F19 F16
#? [F19|F16]

loadsgf games/arb/dumonteil-3.sgf 193
203 reg_genmove black
#? [T7]*

# the root of the problem at test 203 lies at move 185
loadsgf games/arb/dumonteil-3.sgf 185
204 reg_genmove black
#? [S7]*

loadsgf games/arb/dumonteil-4.sgf 189
210 reg_genmove black
#? [D8]*

# Ko mistake observed in gnugo-2.7.231 --level 8 -M 32
loadsgf games/arb/dumonteil-5.sgf 83
220 reg_genmove black
#? [!F6]

# Ko mistake observed in gnugo-2.7.231 --level 8 -M 32
loadsgf games/arb/dumonteil-5.sgf 125
221 reg_genmove black
#? [J9]

loadsgf games/arb/dumonteil-6.sgf 33
231 reg_genmove black
#? [F5]*

# Defend against a combination attack.
loadsgf games/arb/game03.sgf 109
232 reg_genmove black
#? [M9|M10|L10|L11]

loadsgf games/arb/game04.sgf 43
233 reg_genmove black
#? [J9]
