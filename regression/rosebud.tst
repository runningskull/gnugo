# Reset applicable counters
reset_connection_node_counter
reset_owl_node_counter
reset_reading_node_counter
reset_trymove_counter

loadsgf games/rosebud.sgf 10
1 reg_genmove black
#? [E16]*

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
