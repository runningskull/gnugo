#--------------------------------------------------------------
# Not sure it's the best move on the board, but it should be
# considered. Unless I missed something, it looks like a good
# reducing move.
#CATEGORY=STRATEGY
loadsgf games/nando/auto001.sgf
1 gg_genmove white
#? [M12]*

# Uh ?!
#CATEGORY=OWL
loadsgf games/nando/auto002.sgf
2 owl_does_defend A14 B13
#? [0]*

#CATEGORY=OPTICS
loadsgf games/nando/auto003.sgf
3 owl_defend B11
#? [1 A12]*

#CATEGORY=CONNECTION
loadsgf games/nando/auto004.sgf
4 disconnect S7 S11
#? [1 S10|T9]*
5 same_dragon S7 S11
#? [0]*

# Problem with multiple lunches
#CATEGORY=OWL
loadsgf games/nando/auto005.sgf
6 owl_does_defend S2 Q2
#? [0]*

#CATEGORY=ATARI_ATARI (?)
loadsgf games/nando/auto006.sgf
7 gg_genmove black
#? [N16]*
8 gg_genmove white
#? [M16|N16]*

# Beware of the double snapback.
#CATEGORY=TACTICAL_READING
loadsgf games/nando/auto007.sgf
white A1
black B5
9 gg_genmove white
#? [E1|E2|F1|G1|G2]*

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
#? [1 (S2|T2|S1)]*

#CATEGORY=OWL/OPTICS
loadsgf games/nando/auto010.sgf
12 owl_defend L17
#? [1 (K19|L18|K18|H19)]*

#CATEGORY=OWL/OPTICS
# (If white B2 in reply to B1, Black A1 kills cleanly. /ab)
loadsgf games/nando/auto011.sgf
13 owl_attack A4
#? [1 B1]*

# W should better counter-attack
#CATEGORY=CONNECTION
loadsgf games/nando/auto012.sgf 90
14 gg_genmove white
#? [P7]*

# Still the better choice, S7 only strengthens B.
#CATEGORY=CONNECTION
loadsgf games/nando/auto012.sgf 92
15 gg_genmove white
#? [P7]*

# Either the reverse followup or the constraint of EE106 is
# wrong IMHO
#CATEGORY=ENDGAME
loadsgf games/nando/auto012.sgf 112
16 gg_genmove white
#? [!D1]*

# D5 is just stupid
# (but A5 doesn't work. /ab)
#CATEGORY=OWL
loadsgf games/nando/auto012.sgf 124
17 gg_genmove white
#? [B4|B5]*

#--------------------------------------------------------------
# Cascade of chain breakings
#CATEGORY=TACTICAL_READING
loadsgf games/nando/meijin27_g1.sgf 105
110 owl_defend B16
#? [0]*
111 attack C18
#? [0]*

# Connection overvaluation. This dragon has miai options to
# connect to strong dragons, so it isn't urgent to do it now.
#CATEGORY=STRATEGY
loadsgf games/nando/meijin27_g1.sgf 186
112 gg_genmove white
#? [!P7]*

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
210 gg_genmove white
#? [D3]*

# D5 is exceedingly urgent. GG should _never_ tenuki !
#CATEGORY=FUSEKI
loadsgf games/nando/kisei27_g1.sgf 16
211 gg_genmove white
#? [D5]*

# Connection is mandatory.
#CATEGORY=FUSEKI
loadsgf games/nando/kisei27_g1.sgf 20
212 gg_genmove white
#? [E3]*

#CATEGORY=CONNECTION
loadsgf games/nando/kisei27_g1.sgf 26
213 disconnect C4 C6
#? [0]*

#---------------------- End of Tests --------------------------
