reset_reading_node_counter
reset_owl_node_counter

loadsgf rot0/games/strategy25.sgf 63
150 owl_attack P16
#? [0]*

loadsgf rot1/games/incident263.sgf 32
70 owl_attack H16
#? [1 (L18|K18|G18)]*

loadsgf rot1/games/strategy33.sgf 15
193 owl_attack D4
#? [0]

loadsgf rot1/games/FSF-neurogo.sgf 286
215 owl_attack L5
#? [1 M2]

loadsgf rot5/games/incident256.sgf 157
65 owl_defend J14
#? [1 L12]*

loadsgf rot5/games/strategy25.sgf 197
152 owl_attack C9
#? [1 B6]*


# Report number of nodes visited by the tactical reading
10000 get_reading_node_counter
#?[0]&

# Report number of nodes visited by the owl code
10001 get_owl_node_counter
#?[0]&

