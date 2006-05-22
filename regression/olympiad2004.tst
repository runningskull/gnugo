# Q12 might be playable after all but it's very unorthodox and it
# would be preferrable if GNU Go played some more normal fuseki move.
loadsgf games/olympiad2004/19x19/gnu-ind.sgf 5
1 restricted_genmove black Q12 E3 E4
#? [E3|E4]

# G14 looks more natural than the played move at G12.
loadsgf games/olympiad2004/19x19/gnu-ind.sgf 65
2 restricted_genmove black G14 G12
#? [G14]

# Q10 looks too small and overly conservative. There are many
# larger moves on the board.
loadsgf games/olympiad2004/19x19/gnu-ind.sgf 79
3 reg_genmove black
#? [!Q10]

# N16 does connect but completely misses the point. GNU Go would win
# big by just making sure that the thrashing white group in the center
# doesn't spring to life.
loadsgf games/olympiad2004/19x19/gnu-ind.sgf 127
4 reg_genmove black
#? [G10|G9|G8|G7|J7|J6|K7|H7]*

# M15 is again too small. Subduing the big thrashing white dragon must
# have priority.
loadsgf games/olympiad2004/19x19/gnu-ind.sgf 135
5 reg_genmove black
#? [F11|F10|G10|E10|E9|E11|G7|F8]

# The focus must still be on the left. N18 is big but not big enough.
loadsgf games/olympiad2004/19x19/gnu-ind.sgf 141
6 reg_genmove black
#? [D11|H9|F10]*

# P18 looks at best like a questionable followup to N18. Playing on
# the left is urgent.
loadsgf games/olympiad2004/19x19/gnu-ind.sgf 143
7 reg_genmove black
#? [D14|D11]*

# Better capture than connect.
loadsgf games/olympiad2004/19x19/gnu-ind.sgf 161
8 restricted_genmove black D10 C8
#? [C8]

# E15 only threatens to make an eye in gote. Locally F15 is a better
# option but most natural is to block at B15 to secure two eyes and
# limit white's thrashing potential.
loadsgf games/olympiad2004/19x19/gnu-ind.sgf 179
9 reg_genmove black
#? [B15]*

# Regardless whether the semeai still can be won after C14, blocking
# at B15 is the natural and much easier way to live locally and
# definitely win the semeai.
loadsgf games/olympiad2004/19x19/gnu-ind.sgf 183
10 reg_genmove black
#? [B15]

# T17 is totally meritless compared to any of the local alternatives.
loadsgf games/olympiad2004/19x19/gnu-ind.sgf 203
11 restricted_genmove black T17 S19 R19 O14 O13 S14
#? [S19|R19|O14|O13|S14]*

# L9 is zero points gote.
loadsgf games/olympiad2004/19x19/gnu-ind.sgf 223
12 restricted_genmove black L9 F7
#? [F7]*

# A7 solves the tactical problem as well as B10 but also splits the
# eyespace into two parts.
loadsgf games/olympiad2004/19x19/gnu-ind.sgf 235
13 reg_genmove black
#? [A7]*

# T14 is bigger than T4/T5.
loadsgf games/olympiad2004/19x19/gnu-ind.sgf 249
14 reg_genmove black
#? [T14]*

# G18 is locally bigger than J19.
loadsgf games/olympiad2004/19x19/gnu-ind.sgf 281
15 restricted_genmove black J19 G18
#? [G18]*

# Since white can get a ko after C11, it is necessary to first settle
# the semeai.
loadsgf games/olympiad2004/19x19/gnu-ind.sgf 287
16 reg_genmove black
#? [A7|A6|E4|A5]

# The semeai still needs settling. White can live or get a ko for the
# life and death of both by the combination of A1 and A7, followed by
# D4.
loadsgf games/olympiad2004/19x19/gnu-ind.sgf 289
17 reg_genmove black
#? [A7|A6|E4|A5]

# Force black into bad shape or allowing a ponnuki.
loadsgf games/olympiad2004/19x19/int-gnu.sgf 8
101 reg_genmove white
#? [E2]*

# Force black into bad shape or allowing a ponnuki.
loadsgf games/olympiad2004/19x19/int-gnu.sgf 10
102 reg_genmove white
#? [E2]

# C18 is mandatory after D18
loadsgf games/olympiad2004/19x19/int-gnu.sgf 18
103 reg_genmove white
#? [C18]

# Must at least settle the shape (in sente) before tenuki.
loadsgf games/olympiad2004/19x19/int-gnu.sgf 20
104 reg_genmove white
#? [D14]*

# Can't tenuki before connecting. Forcing with T17 is also okay.
loadsgf games/olympiad2004/19x19/int-gnu.sgf 28
105 reg_genmove white
#? [O17|N17|T17]

# K4 is overconcentrated. Which moves more than D14 are good here?
loadsgf games/olympiad2004/19x19/int-gnu.sgf 42
106 restricted_genmove white D14 K4
#? [D14]*

# With the black stone at R9, R6 is locally better than Q6.
loadsgf games/olympiad2004/19x19/int-gnu.sgf 46
107 restricted_genmove white Q6 R6
#? [R6]*

# P3 does not look urgent here.
loadsgf games/olympiad2004/19x19/int-gnu.sgf 50
108 reg_genmove white
#? [!P3]

# First live on the left edge.
loadsgf games/olympiad2004/19x19/int-gnu.sgf 60
109 reg_genmove white
#? [C10]*

# B10 not sufficient to live.
loadsgf games/olympiad2004/19x19/int-gnu.sgf 62
110 reg_genmove white
#? [!B10]

# B11 not sufficient to live.
loadsgf games/olympiad2004/19x19/int-gnu.sgf 64
111 reg_genmove white
#? [!B11]

# T13 is an overplay. T14 works better and also solves the problem
# with bad aji in the upper right corner.
loadsgf games/olympiad2004/19x19/int-gnu.sgf 96
112 reg_genmove white
#? [T14]*

# Q10 does not make much good. More interesting to play some move in
# the center.
loadsgf games/olympiad2004/19x19/int-gnu.sgf 104
113 reg_genmove white
#? [!Q10]

# K19 is an overplay. After black M18 white has to abandon K19 or risk
# a ko in the corner.
loadsgf games/olympiad2004/19x19/int-gnu.sgf 110
114 restricted_genmove white K19 M18
#? [M18]*

# T18 dies right out if white finds the killing move at R19. P19 and
# R19 both live, but K19 and M19 must be sacrificed. N19 lives with
# everything except T17 and possibly T19, which are less important.
loadsgf games/olympiad2004/19x19/int-gnu.sgf 114
115 reg_genmove white
#? [N19]*

# M3 is not necessary. Blocking in the center is big.
loadsgf games/olympiad2004/19x19/int-gnu.sgf 132
116 reg_genmove white
#? [H8]*

# G10 is locally clearly better than G9 as it threatens to cut.
loadsgf games/olympiad2004/19x19/int-gnu.sgf 162
117 restricted_genmove white G9 G10
#? [G10]*

# Can't just give up the ko. T16 and L17 should be big enough ko
# threats. G5 might be barely big enough as well.
loadsgf games/olympiad2004/19x19/int-gnu.sgf 170
118 reg_genmove white
#? [T16|L17|M17|G5]*

# O15 is bigger than H4.
loadsgf games/olympiad2004/19x19/int-gnu.sgf 178
119 restricted_genmove white H4 O15
#? [O15]

# N13 is worthless.
loadsgf games/olympiad2004/19x19/int-gnu.sgf 194
120 restricted_genmove white N13 M12
#? [M12]*

# H6 is completely worthless. The other atari is clearly better.
loadsgf games/olympiad2004/19x19/int-gnu.sgf 196
121 restricted_genmove white H6 G5
#? [G5]

# The ko can at least be captured once before connecting.
loadsgf games/olympiad2004/19x19/int-gnu.sgf 202
122 restricted_genmove white C6 E7
#? [C6]*

# We are ahead. Even if owl misreads, the thrashing dragon
# suppression should generate a move at L3 or K3.
# See also owl1:355,356.
loadsgf games/olympiad2004/19x19/int-gnu.sgf 242
123 reg_genmove white
#? [K3|L3|L1]

# More important to build towards center than closing off edge.
loadsgf games/olympiad2004/19x19/gnu-int.sgf 67
201 restricted_genmove black C11 E11 E12
#? [E11]*

# Really bad shape at G7.
loadsgf games/olympiad2004/19x19/gnu-int.sgf 113
202 restricted_genmove black G7 G8
#? [G8]

# O10 protects against both cuts!
loadsgf games/olympiad2004/19x19/gnu-int.sgf 205
203 restricted_genmove black M13 O10
#? [O10]*

# Black will break through somewhere after D4.
loadsgf games/olympiad2004/9x9/gnu-int.sgf 9
301 reg_genmove black
#? [D4]*

# H6 is bigger than J7.
loadsgf games/olympiad2004/9x9/int-gnu.sgf 42
401 reg_genmove white
#? [H6]*

# Better to allow G7 to get cut off than E3.
loadsgf games/olympiad2004/9x9/gnu-mag.sgf 9
501 reg_genmove black
#? [F3]

# Hane at A6 leads nowhere. The clamp at B5 has more potential.
# The cut at C6 is captured right out in a ladder.
loadsgf games/olympiad2004/9x9/gnu-mag.sgf 25
502 restricted_genmove black A6 B5 C6
#? [B5]*

# Wrong atari. F6 is better than E7.
loadsgf games/olympiad2004/9x9/gnu-mag.sgf 35
503 restricted_genmove black E7 F6
#? [F6]*

# F6 really bad shape.
loadsgf games/olympiad2004/9x9/gnu-dbg.sgf 11
601 restricted_genmove black F7 F6 H4 G3
#? [F7|H4|G3]

# G6 about as bad shape as one can get. E6 has more promise.
loadsgf games/olympiad2004/9x9/gnu-dbg.sgf 23
602 reg_genmove black
#? [E6]*

# Always connect from the secure end. A1 goes before A4.
loadsgf games/olympiad2004/9x9/gnu-dbg.sgf 63
603 reg_genmove black
#? [A1]*
