# Reset applicable counters
reset_connection_node_counter
reset_owl_node_counter
reset_reading_node_counter
reset_trymove_counter

# Apparently, T9 gets overvalued because the effect on S7 and T6 is
# counted twice (territorially and strategically)
loadsgf games/tactics05.sgf
10 gg_genmove white
#? [!T9]

loadsgf games/tactics06.sgf
20 owl_defend C19
#? [1 A17]


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
