# Reset applicable counters
reset_connection_node_counter
reset_owl_node_counter
reset_reading_node_counter
reset_trymove_counter

loadsgf games/endgame1.sgf 1
101 reg_genmove black
#? [F9]

loadsgf games/endgame1.sgf 2
102 reg_genmove white
#? [E9]

loadsgf games/endgame1.sgf 3
103 reg_genmove black
#? [G9]

loadsgf games/endgame1.sgf 4
104 reg_genmove white
#? [E8|D8|E7]

loadsgf games/endgame1.sgf 5
105 reg_genmove black
#? [J2|J3|A7]

loadsgf games/endgame1.sgf 6
106 reg_genmove white
#? [G1|H1|F3]

# J3 is 0 points double sente.
loadsgf games/endgame1.sgf 7
107 reg_genmove black
#? [J3|A7]

# tm - H1 also seems fine.
loadsgf games/endgame1.sgf 8
108 reg_genmove white
#? [G1|H1]

loadsgf games/endgame1.sgf 9
109 reg_genmove black
#? [A7]

loadsgf games/endgame1.sgf 10
110 reg_genmove white
#? [A8]

loadsgf games/endgame1.sgf 11
111 reg_genmove black
#? [A6]

loadsgf games/endgame1.sgf 12
112 reg_genmove white
#? [B8|B9|C8]

loadsgf games/endgame1.sgf 13
113 reg_genmove black
#? [D2|C5]

loadsgf games/endgame1.sgf 14
114 reg_genmove white
#? [E1]

loadsgf games/endgame1.sgf 15
115 reg_genmove black
#? [C5]

loadsgf games/endgame1.sgf 16
116 reg_genmove white
#? [D5]

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
