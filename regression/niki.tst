# Reset life node counter
reset_life_node_counter
# Reset owl node counter
reset_owl_node_counter
# Reset reading node counter
reset_reading_node_counter

loadsgf games/niki.sgf 24
1 gg_genmove white
#? [E17]

loadsgf games/niki.sgf 40
2 gg_genmove white
#? [E7]*

loadsgf games/niki.sgf 68
3 gg_genmove white
#? [E12]*

loadsgf games/niki.sgf 92
4 gg_genmove white
#? [Q8]

loadsgf games/niki.sgf 94
5 gg_genmove white
#? [S9]*

loadsgf games/niki.sgf 106
6 gg_genmove white
#? [!S18]

loadsgf games/niki.sgf 110
7 gg_genmove white
#? [J12]*

loadsgf games/niki.sgf 124
8 gg_genmove white
#? [G16]*
9 gg_genmove black
#? [F15]*

loadsgf games/niki.sgf 128
10 gg_genmove white
#? [S5]*

loadsgf games/niki.sgf 158
11 gg_genmove white
#? [M6|M6|P7]*

loadsgf games/niki.sgf 212
12 gg_genmove white
#? [H12]*

loadsgf games/niki.sgf 226
13 gg_genmove white
#? [F8]

# Report number of nodes visited by the tactical reading
10000 get_reading_node_counter
#? [0]&

# Report number of nodes visited by the owl code
10001 get_owl_node_counter
#? [0]&

# Report number of nodes visited by the life code
10002 get_life_node_counter
#? [0]&
