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

# upper left needs attention
loadsgf games/nngs/evand-gnugo-3.3.11-200211151742.sgf 12
50 gg_genmove black
#? [J17|E14|F15|C18|B18]*

loadsgf games/nngs/evand-gnugo-3.3.11-200211151742.sgf 16
60 gg_genmove black
#? [E14|B18|C18|G18]*

loadsgf games/nngs/evand-gnugo-3.3.11-200211151742.sgf 142
70 owl_defend J11
#? [1 S12]

loadsgf games/nngs/gnugo-3.3.12-jimm-200211261724.sgf 20
80 restricted_genmove white M7 J6
#? [J6]*

loadsgf games/nngs/gnugo-3.3.12-jimm-200211261724.sgf 170
90 gg_genmove white
#? [D12|C11|E14]*

loadsgf games/nngs/leftd-gnugo-3.3.12-200211292017.sgf 184
100 gg_genmove black
#? [F18|G18]*

loadsgf games/nngs/saphir-gnugo-3.3.12-200212031818.sgf 24
110 gg_genmove black
#? [D17]

loadsgf games/nngs/saphir-gnugo-3.3.12-200212031818.sgf 38
120 gg_genmove black
#? [C13]

loadsgf games/nngs/saphir-gnugo-3.3.12-200212031818.sgf 40
130 gg_genmove black
#? [E14]

loadsgf games/nngs/saphir-gnugo-3.3.12-200212031818.sgf 40
140 restricted_genmove black N17 R5 R4
#? [R5|R4]

loadsgf games/nngs/saphir-gnugo-3.3.12-200212031818.sgf 94
150 gg_genmove black
#? [N3|M3]

loadsgf games/nngs/saphir-gnugo-3.3.12-200212031818.sgf 100
160 gg_genmove black
#? [K8|L8]

loadsgf games/nngs/saphir-gnugo-3.3.12-200212031818.sgf 168
170 gg_genmove black
#? [S6]

loadsgf games/nngs/saphir-gnugo-3.3.12-200212031818.sgf 168
180 gg_genmove black
#? [K8|L8]

loadsgf games/nngs/saphir-gnugo-3.3.12-200212031818.sgf 184
190 gg_genmove black
#? [T8]

loadsgf games/nngs/saphir-gnugo-3.3.12-200212031818.sgf 192
200 gg_genmove black
#? [T6]

# Make a ko threat. How about T8 or T10?
loadsgf games/nngs/saphir-gnugo-3.3.12-200212031818.sgf 202
210 gg_genmove black
#? [!S5]

loadsgf games/nngs/saphir-gnugo-3.3.12-200212031818.sgf 204
220 gg_genmove black
#? [!T2]

# Kill the whole thing, not just a piece.
loadsgf games/nngs/saphir-gnugo-3.3.12-200212031818.sgf 221
230 gg_genmove black
#? [Q18]

loadsgf games/nngs/saphir-gnugo-3.3.12-200212031818.sgf 242
240 gg_genmove black
#? [B4]

loadsgf games/nngs/saphir-gnugo-3.3.12-200212031818.sgf 246
250 gg_genmove black
#? [F7]

loadsgf games/nngs/saphir-gnugo-3.3.12-200212031818.sgf 258
260 gg_genmove black
#? [A4]

loadsgf games/nngs/saphir-gnugo-3.3.12-200212031818.sgf 292
270 gg_genmove black
#? [!P5]


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
