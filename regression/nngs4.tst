# Reset applicable counters
reset_connection_node_counter
reset_owl_node_counter
reset_reading_node_counter
reset_trymove_counter


loadsgf games/nngs/gnugo-3.3.10-niki-200210281349.sgf 10
10 gg_genmove white
#? [E18]*

loadsgf games/nngs/gnugo-3.3.10-niki-200210281349.sgf 26
20 gg_genmove white
#? [Q4|Q6]*

loadsgf games/nngs/gnugo-3.3.10-niki-200210281349.sgf 36
30 gg_genmove white
#? [C5|C6]

loadsgf games/nngs/gnugo-3.3.10-niki-200210281349.sgf 46
40 gg_genmove white
#? [H3|F4]*


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
