# Reset applicable counters
reset_life_node_counter
reset_owl_node_counter
reset_reading_node_counter
reset_trymove_counter

loadsgf games/wing-yuhiko-gnugo.sgf 12
221 gg_genmove black
#? [S4|C11]*

# This might be worth a J pattern.
loadsgf games/wing-yuhiko-gnugo.sgf 30
222 gg_genmove black
#? [R14]*

loadsgf games/wing-yuhiko-gnugo.sgf 46
223 gg_genmove black
#? [P16]*

#CATEGORY=STRATEGY
loadsgf games/wing-yuhiko-gnugo.sgf 62
224 gg_genmove black
#? [N15]*

#CATEGORY=STRATEGY
loadsgf games/wing-yuhiko-gnugo.sgf 86
225 gg_genmove black
#? [O12]*

#CATEGORY=CONNECTION
loadsgf games/olympiad2002/game2-19-gnugo-goint-1-0.sgf 54
226 gg_genmove white
#? [F5]*

#CATEGORY=OWL
# F1 was played in the game as an owl defense of E4.
loadsgf games/olympiad2002/game2-19-gnugo-goint-1-0.sgf 66
227 gg_genmove white
#? [!F1]*

#CATEGORY=STRATEGY
# Don't run away with an unimportant, weak stone.
loadsgf games/olympiad2002/game2-19-gnugo-goint-1-0.sgf 90
228 gg_genmove white
#? [!N12]*

loadsgf games/olympiad2002/game1-19-go4-gnugo-1-0.sgf 41
229 gg_genmove black
#? [D7]

loadsgf games/olympiad2002/game1-19-go4-gnugo-1-0.sgf 53
230 gg_genmove black
#? [H9]

loadsgf games/olympiad2002/game1-19-go4-gnugo-1-0.sgf 107
231 gg_genmove black
#? [J2|J3]

# If tuning this position be sure GNU Go plays subsequent moves acceptably
loadsgf games/olympiad2002/game1-19-goint-gnugo-1-0.sgf 17
232 gg_genmove black
#? [D11|E11]

loadsgf games/olympiad2002/game1-19-goint-gnugo-1-0.sgf 23
233 gg_genmove black
#? [E13]

loadsgf games/olympiad2002/game1-19-goint-gnugo-1-0.sgf 29
234 gg_genmove black
#? [H14]

loadsgf games/olympiad2002/game1-19-goint-gnugo-1-0.sgf 47
235 gg_genmove black
#? [G12|J13]

loadsgf games/olympiad2002/game1-19-goint-gnugo-1-0.sgf 91
236 gg_genmove black
#? [R12]




############ End of Tests #################

# Report number of nodes visited by the tactical reading
10000 get_reading_node_counter
#? [0]&

# Report number of nodes visited by the owl code
10001 get_owl_node_counter
#? [0]&

# Report number of nodes visited by the life code
10002 get_life_node_counter
#? [0]&

# Report number of trymoves/trykos visited by the test
10003 get_trymove_counter
#? [0]&
