#--------------------------------------------------------------
# Not sure it's the best move on the board, but it should be
# considered. Unless I missed something, it looks like a good
# reducing move.
#CATEGORY=STRATEGY
loadsgf games/nando/auto001.sgf
1 gg_genmove white
#? [M12]

# Uh ?!
#CATEGORY=OWL
loadsgf games/nando/auto002.sgf
2 owl_does_defend A14 B13
#? [0]

#CATEGORY=OPTICS
loadsgf games/nando/auto003.sgf
3 owl_defend B11
#? [1 A12]

#CATEGORY=CONNECTION
loadsgf games/nando/auto004.sgf
4 disconnect S7 S11
#? [1 S10|T9]
5 same_dragon S7 S11
#? [0]

# Problem with multiple lunches
#CATEGORY=OWL
loadsgf games/nando/auto005.sgf
6 owl_does_defend S2 Q2
#? [0]

#CATEGORY=ATARI_ATARI (?)
loadsgf games/nando/auto006.sgf
7 gg_genmove black
#? [N16]
8 gg_genmove white
#? [M16|N16]

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
112 gg_genmove white
#? [!P7]

#--------------------------------------------------------------
# After B:P2 W:Q2, the connection code (as of 3.3.15) overvalues
# B:P3 (-1.95) and consequently rejects the correct move B:Q1
# at candidate selection stage
#CATEGORY=CONNECTION
loadsgf games/nando/meijin27_g4.sgf 23
140 disconnect O2 Q3
#? [1 P2]

# More an amalgamation problem. GG knows how to cut, but still
# thinks it's the same dragon.
#CATEGORY=CONNECTION
loadsgf games/nando/meijin27_g4.sgf 128
141 disconnect P2 P4
#? [1 P3]*
142 same_dragon P2 P4
#? [0]

#--------------------------------------------------------------
# Not really a connection problem, rather a tactical reading one
# see test 151
#CATEGORY=CONNECTION
loadsgf games/nando/meijin27_g5.sgf 48
150 disconnect P3 Q4
#? [1 P4]

#CATEGORY=TACTICAL_READING
loadsgf games/nando/meijin27_g5.sgf 48
black P4
151 attack P4
#? [0]

#--------------------------------------------------------------
# C4 should be played only when W already has a stone around D10
#CATEGORY=FUSEKI
loadsgf games/nando/kisei27_g1.sgf 14
210 gg_genmove white
#? [D3]

# D5 is exceedingly urgent. GG should _never_ tenuki !
#CATEGORY=FUSEKI
loadsgf games/nando/kisei27_g1.sgf 16
211 gg_genmove white
#? [D5]

# Connection is mandatory.
#CATEGORY=FUSEKI
loadsgf games/nando/kisei27_g1.sgf 20
212 gg_genmove white
#? [E3]

#CATEGORY=CONNECTION
loadsgf games/nando/kisei27_g1.sgf 26
213 disconnect C4 C6
#? [0]

#---------------------- End of Tests --------------------------
