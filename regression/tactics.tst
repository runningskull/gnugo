# Reset applicable counters
reset_connection_node_counter
reset_owl_node_counter
reset_reading_node_counter
reset_trymove_counter

loadsgf games/tactics01.sgf
1 reg_genmove black
#? [C15|D15|B13|B11|B14]*

loadsgf games/tactics02.sgf
2 reg_genmove black
#? [S11|S12]

loadsgf games/tactics03.sgf
3 reg_genmove black
#? [M18]

# This is in part an atari_atari problem. The best local move is K18
# though J18 or G18 might be acceptable.
loadsgf games/rosebud4.sgf 138
4 reg_genmove white
#? [K18]*

loadsgf games/tactics04.sgf 206
5 reg_genmove white
#? [N16]



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
