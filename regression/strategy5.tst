# Reset applicable counters
reset_connection_node_counter
reset_owl_node_counter
reset_reading_node_counter
reset_trymove_counter

loadsgf games/wing-yuhiko-gnugo.sgf 12
221 restricted_genmove black S4 Q7
#? [S4]

# This might be worth a J pattern.
loadsgf games/wing-yuhiko-gnugo.sgf 30
222 reg_genmove black
#? [R14]*

loadsgf games/wing-yuhiko-gnugo.sgf 46
223 reg_genmove black
#? [P16]*

#CATEGORY=STRATEGY
loadsgf games/wing-yuhiko-gnugo.sgf 62
224 reg_genmove black
#? [N15]

#CATEGORY=STRATEGY
loadsgf games/wing-yuhiko-gnugo.sgf 86
225 reg_genmove black
#? [O12]

#CATEGORY=CONNECTION
loadsgf games/olympiad2002/game2-19-gnugo-goint-1-0.sgf 54
226 reg_genmove white
#? [F5]*

#CATEGORY=OWL
# F1 was played in the game as an owl defense of E4.
loadsgf games/olympiad2002/game2-19-gnugo-goint-1-0.sgf 66
227 reg_genmove white
#? [!F1]

#CATEGORY=STRATEGY
# Don't run away with an unimportant, weak stone.
loadsgf games/olympiad2002/game2-19-gnugo-goint-1-0.sgf 90
228 reg_genmove white
#? [!N12]

loadsgf games/olympiad2002/game1-19-go4-gnugo-1-0.sgf 41
229 reg_genmove black
#? [D7]

loadsgf games/olympiad2002/game1-19-go4-gnugo-1-0.sgf 53
230 reg_genmove black
#? [H9]

loadsgf games/olympiad2002/game1-19-go4-gnugo-1-0.sgf 107
231 reg_genmove black
#? [J2|J3]*

# If tuning this position be sure GNU Go plays subsequent moves acceptably
loadsgf games/olympiad2002/game1-19-goint-gnugo-1-0.sgf 17
232 reg_genmove black
#? [D11|E11]

loadsgf games/olympiad2002/game1-19-goint-gnugo-1-0.sgf 23
233 reg_genmove black
#? [E13]*

loadsgf games/olympiad2002/game1-19-goint-gnugo-1-0.sgf 29
234 reg_genmove black
#? [H14]

loadsgf games/olympiad2002/game1-19-goint-gnugo-1-0.sgf 47
235 reg_genmove black
#? [G12|J13]

loadsgf games/olympiad2002/game1-19-goint-gnugo-1-0.sgf 91
236 reg_genmove black
#? [R12]*

# Not a valid problem
# loadsgf games/nngs/gnugo-3.3.6-evand-200208290255.sgf 73
# 272 reg_genmove white
# #? [C7]*

# Black has a combination attack which must be defended correctly against.
# F8 and F7 both fail.
loadsgf games/strategy47.sgf
273 reg_genmove white
#? [E8|E9|F9|E7|F6|E6|D7|C7]

# Black has a combination attack which must be defended correctly against.
# In particular L5 fails.
loadsgf games/strategy48.sgf
274 reg_genmove white
#? [!L5|M5|N5]

# Semeai problem. White can make seki, black can kill.
loadsgf games/strategy49.sgf
275 reg_genmove white
#? [A18]
276 reg_genmove black
#? [A18]

# R19 defends everything.
loadsgf games/strategy50.sgf
277 reg_genmove white
#? [R19]

loadsgf games/vaughn.sgf 44
278 reg_genmove black
#? [!J11]*

# Really, anything is better than A18. (B13 and B12 are quite
# ineffective too but can at least not be repeated...)
loadsgf games/reading41.sgf 130
279 reg_genmove black
#? [!A18]

############ End of Tests #################

# Report number of nodes visited by the tactical reading
10000 get_reading_node_counter
#? [0]&

# Report number of nodes visited by the owl code
10001 get_owl_node_counter
#? [0]&

# Report number of nodes visited by the life code
10002 get_connection_node_counter
#? [0]&

# Report number of trymoves/trykos visited by the test
10003 get_trymove_counter
#? [0]&
