reset_reading_node_counter
reset_owl_node_counter

loadsgf rot1/games/incident258.sgf 147
66 owl_defend R16
#? [1 S15]

loadsgf rot1/games/incident96.sgf 71
138 owl_defend M10
#? [1 J12]

loadsgf rot1/games/strategy26.sgf 285
160 owl_defend Q1
#? [1 S4]

loadsgf rot2/games/strategy20.sgf 236
123 owl_defend O2
#? [0]*

loadsgf rot2/games/golife.sgf 46
192 owl_attack E5
#? [1 G6]

loadsgf rot3/games/owl24.sgf 127
189 owl_defend E14
#? [1 D16]

loadsgf rot6/games/incident96.sgf 71
137 owl_attack B10
#? [1 (E12|B13)]*


# Report number of nodes visited by the tactical reading
10000 get_reading_node_counter
#?[0]&

# Report number of nodes visited by the owl code
10001 get_owl_node_counter
#?[0]&

