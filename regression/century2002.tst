# Reset applicable counters
reset_connection_node_counter
reset_owl_node_counter
reset_reading_node_counter
reset_trymove_counter

loadsgf games/century21-2002/golois-gnugo.sgf 35
10 gg_genmove black
#? [R15]

loadsgf games/century21-2002/golois-gnugo.sgf 69
20 gg_genmove black
#? [!A6]

# same bug as nngs2:160
loadsgf games/century21-2002/golois-gnugo.sgf 183
30 gg_genmove black
#? [D15|C15]

# B15 and D13 are both locally better. C13 fills a dame.
loadsgf games/century21-2002/golois-gnugo.sgf 239
35 gg_genmove black
#? [!C13]

loadsgf games/century21-2002/goint-gnugo.sgf 37
40 gg_genmove black 
#? [J3]

loadsgf games/century21-2002/goint-gnugo.sgf 47
50 gg_genmove black 
#? [F7]*

loadsgf games/century21-2002/goint-gnugo.sgf 55
55 gg_genmove black 
#? [F7]*

loadsgf games/century21-2002/goint-gnugo.sgf 111
60 gg_genmove black 
#? [Q14]*

loadsgf games/century21-2002/goint-gnugo.sgf 119
70 gg_genmove black 
#? [J11|K11]*

loadsgf games/century21-2002/goint-gnugo.sgf 161
80 owl_attack D17
#? [1 C17]*

loadsgf games/century21-2002/goint-gnugo.sgf 209
85 gg_genmove black 
#? [N9]*

# A17 and A18 produce 10,000 year ko. But A17 is better
# since B has the option of making seki. B19 is equivalent
# to A18 since white gets the choice of ko.
loadsgf games/century21-2002/goint-gnugo.sgf 237
90 gg_genmove black
#? [A17]*

# A19 is horrible, dying in gote. Before that white couldn't kill without ko.
loadsgf games/century21-2002/goint-gnugo.sgf 247
95 gg_genmove black
#? [!A19]

# locally B7 is better
loadsgf games/century21-2002/manyfaces-gnugo.sgf 27
100 gg_genmove black
#? [!B6]

loadsgf games/century21-2002/manyfaces-gnugo.sgf 29
110 gg_genmove black
#? [C8]*

loadsgf games/century21-2002/manyfaces-gnugo.sgf 43
120 gg_genmove black
#? [L3]

loadsgf games/century21-2002/manyfaces-gnugo.sgf 45
130 gg_genmove black
#? [N4]

loadsgf games/century21-2002/manyfaces-gnugo.sgf 57
140 gg_genmove black
#? [J3|R4|P5]*

loadsgf games/century21-2002/manyfaces-gnugo.sgf 157
150 gg_genmove black
#? [B18]

# avoid shortage of liberties
loadsgf games/century21-2002/manyfaces-gnugo.sgf 177
160 gg_genmove black
#? [B7]*

loadsgf games/century21-2002/manyfaces-gnugo.sgf 235
170 gg_genmove black
#? [J10]*

# prevent the seki
loadsgf games/century21-2002/manyfaces-gnugo.sgf 253
180 gg_genmove black
#? [T9]*

loadsgf games/century21-2002/gnugo-katsunari.sgf 26
190 gg_genmove white
#? [D3]*

loadsgf games/century21-2002/gnugo-katsunari.sgf 36
200 gg_genmove white
#? [F6]*

loadsgf games/century21-2002/gnugo-katsunari.sgf 58
210 gg_genmove white
#? [!A15]*

# a move in the center is bigger than this monkey jump
loadsgf games/century21-2002/gnugo-katsunari.sgf 76
220 gg_genmove white
#? [!T3]*

loadsgf games/century21-2002/gnugo-katsunari.sgf 96
240 gg_genmove white
#? [F18]

loadsgf games/century21-2002/gnugo-katsunari.sgf 156
260 gg_genmove white
#? [S12|Q12]*

loadsgf games/century21-2002/gnugo-katsunari.sgf 202
270 gg_genmove white
#? [O7]*

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
