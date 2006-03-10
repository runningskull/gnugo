# numbering is continued from strategy.tst.

loadsgf games/strategy14a.sgf
# modification of move 17 in strategy14.sgf: Ladder works for W, urgent
# move in lower left removed.
51 reg_genmove white
#? [P3]*

loadsgf games/strategy14.sgf 33
52 reg_genmove white
#? [H5]*

loadsgf games/strategy14.sgf 53
53 reg_genmove white
#? [S15]*

loadsgf games/strategy14.sgf 81
54 reg_genmove white
#? [P6]

loadsgf games/strategy14.sgf 125
55 reg_genmove white
#? [C12]*

loadsgf games/strategy14.sgf 211
56 restricted_genmove white C9 A7
#? [A7]

loadsgf games/strategy15.sgf 2
57 reg_genmove black
#? [!PASS]

loadsgf games/strategy16.sgf 2
58 reg_genmove black
#? [!PASS]

# incident 285
# Seki mistake
loadsgf games/incident278.sgf 322
59 reg_genmove black
#? [B1]

# incident 257
# This result appears to be wrong. B can get a ko by playing D10.
loadsgf games/incident256.sgf 215
60 reg_genmove black
#? [H7]*

# incident 259
loadsgf games/incident258.sgf 251
61 reg_genmove black
#? [M2]

# incident 169 (from the disastrous Indigo game)
# D6 is unreasonable. Test it against an arbitrary reasonable move.
loadsgf games/incident169.sgf 18
62 restricted_genmove white D6 H3
#? [H3]

# incident 170 (from the disastrous Indigo game)
# E6 is unreasonable. Test it against an arbitrary reasonable move.
loadsgf games/incident169.sgf 32
63 restricted_genmove white E6 L6
#? [L6]

# incident 171 (from the disastrous Indigo game)
loadsgf games/incident169.sgf 60
64 reg_genmove white
#? [B13|C13|C12|E14|D14]

# incident 173 (from the disastrous Indigo game)
# M17 connects on the edge. L16 does too but allows black to get
# somewhat stronger. L13 is not completely out of the question since
# it successfully cuts black. It might be a little too early though.
# Also see connection:23-25.
loadsgf games/incident169.sgf 110
65 reg_genmove white
#? [M17]*

# incident 174 (from the disastrous Indigo game)
loadsgf games/incident169.sgf 132
66 reg_genmove white
#? [N11]*

# incident 175 (from the disastrous Indigo game)
loadsgf games/incident169.sgf 154
67 reg_genmove white
#? [F14]*

# incident 177 (from the disastrous Indigo game)
# D13 doesn't suffice to kill the whole black dragon because after
# black F14, E13 and M16 are miai to make a second eye.
loadsgf games/incident169.sgf 184
68 reg_genmove white
#? [F14]*

# incident 183 (from the disastrous Indigo game)
loadsgf games/incident169.sgf 300
69 reg_genmove white
#? [PASS]

loadsgf games/strategy17.sgf
# defense of K16 is urgent
# however it may be difficult to save both K16 and M18 so if the 
# moves listed are not too undervalued we may accept this result
70 reg_genmove white
#? [M15|J15|N16|J16]*

# E6 is the only move.
loadsgf games/strategy18.sgf
71 reg_genmove black
#? [E6]

# Semeai problem.
loadsgf games/strategy19.sgf 94
72 reg_genmove white
#? [A8]

# I have added R17, as it seems to kill the huge black dragon --arend
# added P15, because it also seems likely to kill the dragon,
# and gnugo 3.3.18-pre3 finds it at level 12 --evand
# Note: E7 doesn't work because of B:D9.
loadsgf games/semeai2.sgf 116
73 reg_genmove white
#? [F7|R17|P15]*

# I have added R17, as it seems to kill the huge black dragon --arend
# added P15, because it also seems likely to kill the dragon,
# and gnugo 3.3.18-pre3 finds it at level 12 --evand
loadsgf games/semeai2.sgf 118
74 reg_genmove white
#? [F7|R17|P15]

loadsgf games/semeai2.sgf 150
# If Q11 is not undervalued F7 is acceptable (though not correct)
75 reg_genmove white
#? [Q11]*

loadsgf games/semeai2.sgf 152
# If Q11 is not undervalued F7 is acceptable (though not correct)
76 reg_genmove white
#? [Q11]*

loadsgf games/strategy21.sgf 86
77 restricted_genmove black N4 H3 G3 J2 H2 G2
#? [H3]

# Compare reading test 131.
loadsgf games/strategy21.sgf 96
78 reg_genmove black
#? [H2]*

loadsgf games/strategy14a.sgf
# see test 51
79 reg_genmove black
#? [N3]


#CATEGORY=SEMEAI
#DESCRIPTION=
#SEVERITY=
# This is essentially a semeai problem
loadsgf games/strategy22.sgf
80 reg_genmove white
#? [P4|Q4|Q3|S8]

# Looks to me like B12 is also effective, although much more
# complicated. /gf
loadsgf games/strategy23.sgf
81 reg_genmove white
#? [E13|D13]*

loadsgf games/dragon1.sgf 206
82 reg_genmove black
#? [L5]

loadsgf games/heikki/heikki01.sgf 17
# P15 is a joseki, but GNU should at least see that P14 is urgent
83 reg_genmove black
#? [P14]

loadsgf games/heikki/heikki01.sgf 35
# J18 makes ko
# But Q13 seems larger -arend
84 reg_genmove black
#? [Q13]*

loadsgf games/strategy24.sgf
85 reg_genmove white
#? [O2]*

# A very tricky semeai problem.
# Maybe we should move this and other semeai problems to a separate
# test suite.
loadsgf games/semeai3.sgf 240
86 reg_genmove black
#? [J14]*

# Moved to atari_atari.tst
# # Atari_atari makes a mistake.
# loadsgf games/atari_atari01.sgf 235
# 87 reg_genmove black
# #? [!N3]

loadsgf games/strategy25.sgf 45
88 reg_genmove black
#? [B7]*

loadsgf games/strategy25.sgf 55
89 reg_genmove black
#? [(D16|B7|G13)]

# Connecting with ko at B14 looks best. Cutting at D17 might be
# considered. B17 (game move) is inferior.
loadsgf games/strategy25.sgf 61
90 reg_genmove black
#? [B14|D17]*

# Connecting with ko at B14 is still best. The game move at B13 is
# nothing but horrible. G13 would of course be superior if it killed
# white, but this looks unlikely.
# tm: Whether G13 kill or not, sealing white in is best.  G13 is fine.
# tm added G13 (3.1.14)
loadsgf games/strategy25.sgf 63
91 reg_genmove black
#? [B14|G13]

loadsgf games/strategy25.sgf 67
92 reg_genmove black
#? [G13]

#CATEGORY=SEMEAI
#DESCRIPTION=
#SEVERITY=
loadsgf games/strategy25.sgf 75
93 reg_genmove black
#? [D11]*

# A2 is the best move to live in the corner. B3 and A4 also live but
# the two B6 stones must then be sacrificed. C5 only lives with ko. B5
# dies right out.
loadsgf games/strategy25.sgf 119
94 reg_genmove black
#? [A2]

# The game move at P13 is a suicidal blunder.
loadsgf games/strategy25.sgf 249
95 restricted_genmove black P13 O15
#? [O15]

loadsgf games/strategy26.sgf 195
96 restricted_genmove black J5 O1
#? [J5]

loadsgf games/strategy26.sgf 209
97 reg_genmove black
#? [T8]

# Under no circumstances play the snapback at Q1!
loadsgf games/strategy26.sgf 237
98 restricted_genmove black T6 Q1 S1 L5 N7
#? [T6]

# It's pointless to play L5 in order to tactically save Q2. We need to
# classify the Q2 dragon as inessential.
loadsgf games/strategy26.sgf 251
99 reg_genmove black
#? [M13]

# See also owl:254.
loadsgf games/strategy26.sgf 257
100 reg_genmove black
#? [M16]
