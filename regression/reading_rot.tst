reset_reading_node_counter

loadsgf rot0/games/reading06.sgf 
65 defend E2
#? [0]*

loadsgf rot1/games/incident42.sgf 58
increase_depths
trymove B B5
27 defend E4
#? [1 A5]*
popgo
decrease_depths

loadsgf rot1/games/incident42.sgf 88
increase_depths
trymove B B5
28 defend E4
#? [1 A5]*
popgo
decrease_depths

loadsgf rot1/games/incident42.sgf 89
92 defend E4
#? [1 A5]*

loadsgf rot2/games/incident67.sgf 21
13 defend G7
#? [1 J8]*


# Report number of nodes visited by the tactical reading
10000 get_reading_node_counter
#?[0]&

