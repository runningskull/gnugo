# This is a collection of tests from other sets which may have a semeai
# connection. This test should not be invoked with make all_batches since
# these tests are repeated elsewhere.

# Arb 104
loadsgf games/arb/game02.sgf 71
104 reg_genmove black
#? [A7|B7]

# Arend 30
#CATEGORY=BLUNDER
# GNU Go played F8.
loadsgf games/arend/gnugo-gnugo7.sgf 79
30 reg_genmove black
#? [C13]

# Buzco 6
loadsgf games/buzco1.sgf 82
# tm - A12 is not unthinkable. (3.1.15)
6 reg_genmove black
#? [E8|E9|A12]

# Dniwog 5
#CATEGORY=OWL_TUNING
#DESCRIPTION=W group needs help & 2 black stones attacking.
#SEVERITY=8
loadsgf games/dniwog.sgf 78
5 reg_genmove white
#? [H4|G4]*

# Golife 1
loadsgf games/golife.sgf 16
1 reg_genmove white
#? [C7]*

# Golife 2
loadsgf games/golife.sgf 26
2 reg_genmove white
#? [H5]*

# Manyfaces 7
#CATEGORY=OWL_TUNING
#DESCRIPTION=Q5 dragon is under severe pressure.
#SEVERITY=8
loadsgf games/mfgg2.sgf 45
7 reg_genmove black
#? [R2|S7]

# Neurogo 11
# Semeai problem.
loadsgf games/FSF-neurogo.sgf 148
11 reg_genmove white
#? [N5]

# Neurogo 12
loadsgf games/FSF-neurogo.sgf 152
12 reg_genmove white
#? [B10]

# Nicklas1 1213
loadsgf games/nicklas/nicklas12.sgf 203
1213 reg_genmove black
#? [N4]

# Nicklas1 1405
loadsgf games/nicklas/nicklas14.sgf 55
1405 reg_genmove black
#? [B7]

# Nicklas1 1406
# Semeai problem
loadsgf games/nicklas/nicklas14.sgf 57
1406 reg_genmove black
#? [B6]

# Nicklas1 501
loadsgf games/nicklas/nicklas5.sgf 23
501 reg_genmove black
#? [G7]

# Nicklas2 1407
#CATEGORY=OWL_TUNING
#DESCRIPTION=PASS here is unthinkable, though RESIGN might be appropriate!
#SEVERITY=5
# One eye wins against no eye
#Note: Owl & Dragon status is both critical for the W group.  Why
#  would B pass when there's a critical group on the board? Owl & 
#  Dragon status for all of the B stones is Dead.  Can a Critical
#  group kill another group???
loadsgf games/nicklas/nicklas14.sgf 61
1407 reg_genmove black
#? [A6]*

# Nicklas2 904
# E1 loses some endgame points since black must capture later anyhow, 
# but it saves the group. It could be regarded as acceptable at this 
# point.
loadsgf games/nicklas/nicklas9.sgf 50
904 reg_genmove black
#? [B1|E1]

# Strategy 1
loadsgf games/strategy1.sgf
1 reg_genmove white
#? [Q13]*

# Strategy 37
loadsgf games/nicklas/nicklas8.sgf 84
37 reg_genmove black
#? [!A17|A18|C19]

# Strategy 44
#CATEGORY=STRATEGY
#DESCRIPTION=L4 is awkward.
#SEVERITY=2
#    2.7.179: Whether or not the recommended move at G2 is found, the 
#    move at A6 is egregiously bad. In this situation an owl critical 
#    dragon adjoins an owl dead one. We need to revisit our policy for 
#    such situations. The reading code thinks the worm at B4 can be 
#    defended, so this is an owl lunch.
#
#    2.7.180: After revision of semeai.c, the move at A6 is no longer
#    found. Now GNU plays at M16 on the top. Arguably stabilizing
#    the bottom at G2 or (worse but still OK) at J2 is better but
#    I would class this result as ACCEPTABLE.
#
#    3.1.9: Best move here is tough.  GNU Go correctly tries
#    to stabilize G3 group.
#
# See also reading:158
loadsgf games/strategy11.sgf 77
44 reg_genmove black
#? [G2]

# Strategy2 66
# incident 174 (from the disastrous Indigo game)
loadsgf games/incident169.sgf 132
66 reg_genmove white
#? [N11]*

# Strategy2 72
# Semeai problem.
loadsgf games/strategy19.sgf 94
72 reg_genmove white
#? [A8]*

# Strategy2 73
# I have added R17, as it seems to kill the huge black dragon --arend
loadsgf games/semeai2.sgf 116
73 reg_genmove white
#? [F7|R17]

# Strategy2 80
# This is essentially a semeai problem
loadsgf games/strategy22.sgf
80 reg_genmove white
#? [P4|Q4|Q3]*

# Strategy2 86
# A very tricky semeai problem.
# Maybe we should move this and other semeai problems to a separate
# test suite.
loadsgf games/semeai3.sgf 240
86 reg_genmove black
#? [J14]*

# Strategy2 93
loadsgf games/strategy25.sgf 75
93 reg_genmove black
#? [D11]*

# Strategy3 109
# Important semeai example.
loadsgf games/strategy27.sgf 216
109 reg_genmove white
#? [A9|C7]*

# Strategy3 110
# Important semeai example.
loadsgf games/strategy27.sgf 220
110 reg_genmove white
#? [A8]

# Strategy3 113
loadsgf games/me.sgf 150
113 reg_genmove black
#? [P1]

# Strategy3 124
# C5 seems to lead to seki. Semeai mistake.
loadsgf games/incident269.sgf 276
124 reg_genmove black
#? [C5]*

# Strategy3 126
# The move valuation must become aware of the distinction between
# attack and defense with or without ko.
loadsgf games/strategy29.sgf 138
126 reg_genmove white
#? [B19]

# Strategy3 128
# Semeai problem.
loadsgf games/strategy30.sgf 171
128 reg_genmove black
#? [O8]*

# Strategy3 129
# Semeai problem.
loadsgf games/strategy30.sgf 201
129 reg_genmove black
#? [Q9]*

# Strategy3 139
# Urgent semeai move, worth more than 150 points.
loadsgf games/strategy33.sgf 189
139 reg_genmove black
#? [M18]

# Strategy3 145
# Incident 209. Semeai mistake.
loadsgf games/incident209.sgf 193
145 reg_genmove white
#? [S14|T14]

# Strategy3 146
# Incident 210. Semeai mistake.
loadsgf games/incident209.sgf 259
146 reg_genmove white
#? [R11]

# Strategy4 156
loadsgf games/ssstator.sgf 133
156 reg_genmove black
#? [B14|B16]

# Strategy4 163
# First live then attack. This should probably be analyzed as a semeai.
loadsgf games/strategy39.sgf 158
163 reg_genmove white
#? [O7]*

# Strategy4 168
# Safety must come first. This is almost a whole-board semeai.
loadsgf games/strategy40.sgf 30
168 reg_genmove black
#? [A3|A4]

# Strategy4 179
#DESCRIPTION=Tenuki also seems OK here.
#Added E2 (3.1.14) -trevor
loadsgf games/xxlin.sgf 20
179 reg_genmove black
#? [P3|O2|P2|O4|E2]

# Strategy4 185
# We may want to move this to a dedicated seki or semeai test suite in
# the future.
loadsgf games/seki02.sgf
185 reg_genmove black
#? [E7]

# Strategy4 199
#Tenuki at S18 is not unthinkable. (3.1.14) -trevor
loadsgf games/bretz.sgf 164
199 reg_genmove black
#? [N5|S18]

# Strategy4 200
#Tenuki at S18 is not unthinkable. (3.1.14) -trevor
loadsgf games/bretz.sgf 212
200 reg_genmove black
#? [P6|P7|Q7|S18]

# Strategy4 206
#CATEGORY=SEMEAI
#DESCRIPTION=K6 does take K8 to no eyes, but...
#SEVERITY=8
#GNU Go misses that K10 (large B group) has way few liberties
#than O9 group, hense missing importance of cutting off K8.
loadsgf games/strategy45.sgf
206 reg_genmove black
#? [L8]*

# Trevora 530
#Have to try.
loadsgf games/trevor/auto/a035.sgf 42
530 reg_genmove white
#? [H4]*


