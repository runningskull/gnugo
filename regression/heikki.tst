# Reset applicable counters
reset_connection_node_counter
reset_owl_node_counter
reset_reading_node_counter
reset_trymove_counter

# Gnugo concluded that a group could live in ko,
# and did not bother defending, taking a large opening 
# point instead. The group died later.

loadsgf games/heikki/heikki01.sgf 33
10 reg_genmove black
#? [M18]
# It looks to me like white J18 would kill the black stones without ko. /gf
11 owl_attack L18
#? [1 J18]

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

