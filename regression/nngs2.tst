# Reset applicable counters
reset_life_node_counter
reset_owl_node_counter
reset_reading_node_counter
reset_trymove_counter


loadsgf games/nngs/gnugo-3.3.2-Zahlman-200205221717.sgf 207
1 gg_genmove white
#? [B1|A2]*

loadsgf games/nngs/Jion-gnugo-3.3.2-200205241928.sgf 22
10 gg_genmove black
#? [!H6]

loadsgf games/nngs/Jion-gnugo-3.3.2-200205241928.sgf 96
20 gg_genmove black
#? [H11]*

# K9 is harmful. After B:M16, W cannot kill J11 since E17 and N12 are miai
loadsgf games/nngs/Jion-gnugo-3.3.2-200205241928.sgf 112
30 gg_genmove black
#? [L16|M16]*

loadsgf games/nngs/Jion-gnugo-3.3.2-200205241928.sgf 158
40 gg_genmove black
#? [!K18]

# semeai problem
loadsgf games/nngs/Jion-gnugo-3.3.2-200205241928.sgf 162
50 gg_genmove black
#? [K2]

# locally O1 is better
loadsgf games/nngs/Jion-gnugo-3.3.2-200205241928.sgf 166
60 gg_genmove black
#? [!P1]*

loadsgf games/nngs/Jion-gnugo-3.3.2-200205241928.sgf 176
70 gg_genmove black
#? [!L18]

# semeai problem
loadsgf games/nngs/tobyiii-gnugo-3.3.2-200206041432.sgf 162
80 gg_genmove black
#? [O4|P1]*

#CATEGORY=BLUNDER (incorrectly rejected move)
loadsgf games/nngs/tobyiii-gnugo-3.3.2-200206041432.sgf 166
90 gg_genmove black
#? [N1]*

loadsgf games/nngs/lindq-gnugo-3.3.4-200207051636.sgf 214
100 gg_genmove black
#? [H5]*


############ End of Tests #################

# Report number of nodes visited by the tactical reading
10000 get_reading_node_counter
#? [0]&

# Report number of nodes visited by the owl code
10001 get_owl_node_counter
#? [0]&

# Report number of nodes visited by the life code
10002 get_life_node_counter
#? [0]&

# Report number of trymoves/trykos visited by the test
10003 get_trymove_counter
#? [0]&
