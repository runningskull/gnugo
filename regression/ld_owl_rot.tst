reset_reading_node_counter
reset_owl_node_counter
reset_life_node_counter

loadsgf rot1/games/life_and_death/tripod2.sgf 
159 dragon_status R17
#? [critical (Q18|T16) S19]*

loadsgf rot1/games/life_and_death/tripod6.sgf 
179 owl_attack C3
#? [3 A3]*

loadsgf rot1/games/life_and_death/tripod6.sgf 
182 owl_defend C17
#? [2 B19]*

loadsgf rot1/games/life_and_death/tripod7.sgf 
190 dragon_status R17
#? [critical (S17|Q18|S15|T15|T16) T18]*


# Report number of nodes visited by the tactical reading
10000 get_reading_node_counter
#?[0]&

# Report number of nodes visited by the owl code
10001 get_owl_node_counter
#?[0]&

# Report number of nodes visited by the life code
10002 get_life_node_counter
#?[0]&

