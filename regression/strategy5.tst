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
