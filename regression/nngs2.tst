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
#? [!P1]

loadsgf games/nngs/Jion-gnugo-3.3.2-200205241928.sgf 176
70 gg_genmove black
#? [!L18]*

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
#? [H5]

loadsgf games/nngs/silverado-gnugo-3.3.5-200208061810.sgf 34
110 gg_genmove black
#? [B14|B13]

loadsgf games/nngs/silverado-gnugo-3.3.5-200208061810.sgf 42
120 gg_genmove black
#? [E12|D13]*

# See also connection:93
loadsgf games/nngs/silverado-gnugo-3.3.5-200208061810.sgf 100
130 gg_genmove black
#? [S13]*

loadsgf games/nngs/silverado-gnugo-3.3.5-200208061810.sgf 130
140 gg_genmove black
#? [P5]*

loadsgf games/nngs/silverado-gnugo-3.3.5-200208061810.sgf 138
150 gg_genmove black
#? [M3|L3]*

# same bug as century-2002:30
loadsgf games/nngs/gnugo-3.3.2-Programmer-200206021522.sgf 52
160 gg_genmove black
#? [J14]*

# R9 is clearly worse than Q9.
loadsgf games/nngs/kumu-gnugo-3.2-200205070903.sgf 116
170 gg_genmove black
#? [Q9]*

# G17 is clearly worse than G16.
loadsgf games/nngs/gnugo-3.3.2-mr23-200205120953.sgf 132
180 gg_genmove white
#? [G16]*

loadsgf games/nngs/joshj-gnugo-3.3.2-200205310709.sgf 86
190 gg_genmove black
#? [!S8]

loadsgf games/nngs/joshj-gnugo-3.3.2-200205310709.sgf 124
200 gg_genmove black
#? [G2]

loadsgf games/nngs/joshj-gnugo-3.3.2-200205310709.sgf 148
210 gg_genmove black
#? [F15]*

loadsgf games/nngs/huck-gnugo-3.3.6-200209060837.sgf 54
220 gg_genmove black
#? [S9]*

loadsgf games/nngs/huck-gnugo-3.3.6-200209060837.sgf 158
230 gg_genmove black
#? [G17]*

loadsgf games/nngs/huck-gnugo-3.3.6-200209060837.sgf 160
240 gg_genmove black
#? [F17]

loadsgf games/nngs/huck-gnugo-3.3.6-200209060837.sgf 196
250 gg_genmove black
#? [T18]*

loadsgf games/nngs/huck-gnugo-3.3.6-200209060837.sgf 204
260 gg_genmove black
#? [S18]*

loadsgf games/nngs/huck-gnugo-3.3.6-200209060837.sgf 206
270 gg_genmove black
#? [S19]*

loadsgf games/nngs/huck-gnugo-3.3.6-200209060837.sgf 216
280 gg_genmove black
#? [!A13]*

# B16 is maybe not playable.
# C17 is in any case better than the game move C18.
loadsgf games/nngs/huck-gnugo-3.3.6-200209060837.sgf 220
290 gg_genmove black
#? [C17|B16]*

# Hey, Q19 is dead and Q13 doesn't threaten anything.
loadsgf games/nngs/huck-gnugo-3.3.6-200209060837.sgf 232
300 gg_genmove black
#? [!Q13]*

loadsgf games/nngs/huck-gnugo-3.3.6-200209060837.sgf 240
310 gg_genmove black
#? [M11]

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
