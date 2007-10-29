#--------------------------------------------------------------
# Not sure it's the best move on the board, but it should be
# considered. Unless I missed something, it looks like a good
# reducing move.
#CATEGORY=STRATEGY
loadsgf games/nando/auto001.sgf
1 reg_genmove white
#? [M12|N11]*

# Uh ?!
#CATEGORY=OWL
loadsgf games/nando/auto002.sgf
2 owl_does_defend A14 B13
#? [0]*

#CATEGORY=OPTICS
loadsgf games/nando/auto003.sgf
3 owl_defend B11
#? [1 A12]

#CATEGORY=CONNECTION
loadsgf games/nando/auto004.sgf
4 disconnect S7 S11
#? [1 (S10|T9)]
5 same_dragon S7 S11
#? [0]*

# Problem with multiple lunches
#CATEGORY=OWL
loadsgf games/nando/auto005.sgf
6 owl_does_defend S2 Q2
#? [0]

#CATEGORY=ATARI_ATARI
loadsgf games/nando/auto006.sgf
7 reg_genmove black
#? [N16]
8 reg_genmove white
#? [M16|N16]

# Beware of the double snapback.
#CATEGORY=ATARI_ATARI
loadsgf games/nando/auto007.sgf
white A1
black B5
9 reg_genmove white
#? [E1|E2|F1|G1|G2]

# GG (as of 3.3.15) doesn't even try a single move. On the other
# side, GG doesn't have (yet) any mechanism for detecting a
# "connect either" combination.
#CATEGORY=OWL
loadsgf games/nando/auto008.sgf
10 owl_defend D12
#? [1 (C11|F11|C12)]*

#CATEGORY=OWL/OPTICS
loadsgf games/nando/auto009.sgf
11 owl_attack S8
#? [1 (S2|T2|S1|T4|T3)]

#CATEGORY=OWL/OPTICS
# Note that K12 connects out (sacrificing L11 if necessary).
loadsgf games/nando/auto010.sgf
12 owl_attack L17
#? [0]*

#CATEGORY=OWL/OPTICS
# (If white B2 in reply to B1, Black A1 kills cleanly. /ab)
loadsgf games/nando/auto011.sgf
13 owl_attack A4
#? [1 B1]*

# W should better counter-attack
#CATEGORY=CONNECTION
loadsgf games/nando/auto012.sgf 90
14 reg_genmove white
#? [P7]*

# Still the better choice, S7 only strengthens B.
#CATEGORY=CONNECTION
loadsgf games/nando/auto012.sgf 92
15 reg_genmove white
#? [P7]*

# D5 is just stupid
# (but A5 doesn't work. /ab)
#CATEGORY=OWL
loadsgf games/nando/auto012.sgf 124
17 reg_genmove white
#? [B4|B5]*

# Non-transitivity problem.
#CATEGORY=CONNECTION
loadsgf games/nando/auto013.sgf
18 disconnect D6 H6
#? [1 E6]
19 same_dragon D6 H6
#? [0]*

# S8, T12 and a lack of liberties are too many weaknesses.
# W must defend.
#CATEGORY=ATARI_ATARI
loadsgf games/nando/auto014.sgf
black S16
20 reg_genmove white
#? [S8|S9|T12]
# With another move ordering
loadsgf games/nando/auto014.sgf
black S8
white T9
black S16
21 reg_genmove white
#? [S9|T12]

# Another non-transitivity problem.
#CATEGORY=CONNECTION
loadsgf games/nando/auto015.sgf
22 disconnect L3 N2
#? [1 M3]
23 same_dragon L3 N2
#? [0]

#CATEGORY=OWL
loadsgf games/nando/auto016.sgf
24 owl_defend P17
#? [1 M13]

#CATEGORY=BLUNDER
loadsgf games/nando/auto017.sgf
white A8
black A10
25 reg_genmove white
#? [!P16]

# Another lunch "mirage" at N16/O15.
#CATEGORY=OWL
loadsgf games/nando/auto018.sgf
26 owl_does_defend P16 R18
#? [0]

# Move valuation looks way off. No idea what is happening here.
#CATEGORY=?
loadsgf games/nando/auto019.sgf
27 reg_genmove white
#? [D6]*

# Non-transitivity problem (or is it ok to amalgamate by ko ?)
#CATEGORY=CONNECTION
loadsgf games/nando/auto020.sgf
28 disconnect S3 S7
#? [2 S6]
29 same_dragon S3 S7
#? [0]*

# W still can either connect or make 2 eyes
#CATEGORY=OWL
loadsgf games/nando/auto020.sgf
black S6
30 owl_defend S11
#? [1 T6]*

#CATEGORY=CONNECTION
loadsgf games/nando/auto021.sgf
31 connect E18 H19
#? [0]

# O12 and Q14 are amalgamated per pattern CC502. The OWL engine
# thinks that the dragon can simply escape by playing O13 in
# response to black P13. And unfortunately, no other safeguards
# prevent the blunder (as of 3.5.5)
#CATEGORY=ATARI_ATARI (?)
loadsgf games/nando/auto022.sgf 206
32 reg_genmove white
#? [N13|O13]

# After W:H11 B:K13, White can save the day with K12, but GNU Go 3.7.5
# thinks that the move is unsafe (see reading:220)
loadsgf games/nando/auto023.sgf 170
33 reg_genmove white
#? [H11]

loadsgf games/nando/auto026.sgf 118
34 restricted_genmove white S12 S8
#? [S8]*

# Apparently, a ko depth issue (3.7.7)
play white T8
35 attack T8
#? [3 S8]*

loadsgf games/nando/auto027.sgf 165
36 disconnect L8 N7
#? [1 L7]*


#--------------------------------------------------------------
# Cascade of chain breakings
#CATEGORY=TACTICAL_READING
loadsgf games/nando/meijin27_g1.sgf 105
110 owl_defend B16
#? [0]
111 attack C18
#? [0]

# Connection overvaluation. This dragon has miai options to
# connect to strong dragons, so it isn't urgent to do it now.
#CATEGORY=STRATEGY
loadsgf games/nando/meijin27_g1.sgf 186
112 reg_genmove white
#? [!P7]

#--------------------------------------------------------------
# After B:P2 W:Q2, the connection code (as of 3.3.15) overvalues
# B:P3 (-1.95) and consequently rejects the correct move B:Q1
# at candidate selection stage
#CATEGORY=CONNECTION
loadsgf games/nando/meijin27_g4.sgf 23
140 disconnect O2 Q3
#? [1 P2]*

# More an amalgamation problem. GG knows how to cut, but still
# thinks it's the same dragon.
# (Non-transitivity problem. /ab)
#CATEGORY=CONNECTION
loadsgf games/nando/meijin27_g4.sgf 128
141 disconnect P2 P4
#? [1 P3]
142 same_dragon P2 P4
#? [0]*

#--------------------------------------------------------------
# Not really a connection problem, rather a tactical reading one
# see test 151
#CATEGORY=CONNECTION
loadsgf games/nando/meijin27_g5.sgf 48
150 disconnect P3 Q4
#? [1 P4]*

#CATEGORY=TACTICAL_READING
loadsgf games/nando/meijin27_g5.sgf 48
black P4
151 attack P4
#? [0]*

#--------------------------------------------------------------
# C4 should be played only when W already has a stone around D10
#CATEGORY=FUSEKI
loadsgf games/nando/kisei27_g1.sgf 14
210 reg_genmove white
#? [D3]

# D5 is exceedingly urgent. GG should _never_ tenuki !
#CATEGORY=FUSEKI
loadsgf games/nando/kisei27_g1.sgf 16
211 reg_genmove white
#? [D5]

# Connection is mandatory.
#CATEGORY=FUSEKI
loadsgf games/nando/kisei27_g1.sgf 20
212 reg_genmove white
#? [E3]

#CATEGORY=CONNECTION
loadsgf games/nando/kisei27_g1.sgf 26
213 disconnect C4 C6
#? [0]*

#---------------------- End of Tests --------------------------
