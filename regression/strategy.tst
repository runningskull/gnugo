# Reset applicable counters
reset_life_node_counter
reset_owl_node_counter
reset_reading_node_counter
reset_trymove_counter

loadsgf games/strategy1.sgf
1 gg_genmove white
#? [Q13]

loadsgf games/strategy2.sgf 5
2 gg_genmove black
#? [!G2|D2]

loadsgf games/strategy2.sgf 9
3 gg_genmove black
#? [!H1|H2]

loadsgf games/strategy2.sgf 11
4 gg_genmove black
#? [!F1]

loadsgf games/strategy3.sgf 14
5 gg_genmove white
#? [R17|P17]

# incident 104
# Probably requires a dynamic connection analysis to solve.
loadsgf games/incident104.sgf 63
6 gg_genmove white
#? [E10]*

# incident 106
loadsgf games/incident104.sgf 215
7 gg_genmove white
#? [S19]

# incident 107
loadsgf games/incident107.sgf 216
8 gg_genmove black
#? [N1]

# incident 109
loadsgf games/incident108b.sgf 172
9 gg_genmove black
#? [P15]

# incident 110
loadsgf games/incident108b.sgf 176
10 gg_genmove black
#? [Q7]

# incident 111
loadsgf games/incident108b.sgf 178
11 gg_genmove black
#? [P15]*

# incident 112
# Suboptimal to make eye at L4.
loadsgf games/incident108b.sgf 270
12 gg_genmove black
#? [N4]

# incident 113
loadsgf games/incident108b.sgf 292
13 gg_genmove black
#? [N18]

# incident 121
# E9 is not necessarily the best move since upper left weak too.
# But the game move at R7 is not urgent!
loadsgf games/incident121.sgf 24
14 gg_genmove black
#? [E9]

# incident 123
# Again, E9 is not necessarily the best move.
loadsgf games/incident121.sgf 60
15 gg_genmove black
#? [E9]*

# incident 132
loadsgf games/incident121.sgf 116
16 gg_genmove black
#? [H12]

# incident 165
loadsgf games/incident165.sgf 108
17 gg_genmove black
#? [N11]*

# incident 166a
# E5 is an overplay
# See reading test 124
loadsgf games/incident165.sgf 142
18 gg_genmove black
#? [E4]*

loadsgf games/strategy4.sgf
19 gg_genmove black
#? [!PASS]

# incident 290
loadsgf games/incident290.sgf 30
20 gg_genmove black
#? [Q11]*

# incident 291
loadsgf games/incident291.sgf 54
21 gg_genmove black
#? [S13]

# incident 292
loadsgf games/incident291.sgf 70
22 gg_genmove black
#? [K3]

# incident 294
loadsgf games/incident291.sgf 88
23 gg_genmove black
#? [!K5]

# incident 295
# endgame mistake
loadsgf games/incident291.sgf 250
24 gg_genmove black
#? [F4]

# Moved to filllib.tst
# # incident 296
# # liberty filling mistake
# loadsgf games/incident291.sgf 274
# 25 gg_genmove black
# #? [PASS]

loadsgf games/strategy5.sgf 12
26 gg_genmove black
#? [D3]

loadsgf games/strategy5.sgf 14
27 gg_genmove black
#? [C3]*

loadsgf games/strategy5.sgf 20
28 gg_genmove black
#? [!F2]

loadsgf games/strategy5.sgf 40
29 gg_genmove black
#? [R4]

loadsgf games/strategy5.sgf 44
30 gg_genmove black
#? [!T7]

# This is a problem with the semeai analyzer and revise_semeai().
loadsgf games/strategy6.sgf 274
31 gg_genmove black
#? [!(K19|D17|E16|E15)]

loadsgf games/strategy7.sgf 23
32 gg_genmove black
#? [A4]

loadsgf games/strategy8.sgf 283
33 gg_genmove white
#? [O7]

loadsgf games/nicklas/nicklas8.sgf 72
34 gg_genmove black
#? [E17]*

loadsgf games/nicklas/nicklas8.sgf 80
35 gg_genmove black
#? [!B17]

loadsgf games/nicklas/nicklas8.sgf 82
36 gg_genmove black
#? [!D18]

loadsgf games/nicklas/nicklas8.sgf 84
37 gg_genmove black
#? [!A17|A18|C19]

# The life code solves this mistake.
loadsgf games/nicklas/nicklas8.sgf 86
38 gg_genmove black
#? [!B19]

loadsgf games/nicklas/nicklas8.sgf 96
39 gg_genmove black
#? [P16]

loadsgf games/nicklas/nicklas8.sgf 98
40 gg_genmove black
#? [E5]*

loadsgf games/nicklas/nicklas8.sgf 106
41 gg_genmove black
#? [N16]*

loadsgf games/strategy9.sgf 232
42 gg_genmove black
#? [C17]

# A14 works but is inferior shape since it leaves more ko threats.
loadsgf games/strategy10.sgf
43 gg_genmove white
#? [B13|B14|A14]

loadsgf games/strategy11.sgf 77
44 gg_genmove black
#? [G2]*

# G17/G18 and C1 are both huge.
loadsgf games/strategy11.sgf 127
45 gg_genmove black
#? [G17|G18|C1]

loadsgf games/strategy11.sgf 245
46 gg_genmove black
#? [E1]

loadsgf games/strategy12.sgf 9
47 gg_genmove black
#? [!N17]

loadsgf games/strategy12.sgf 19
48 gg_genmove black
#? [B17]*

loadsgf games/strategy12.sgf 21
# D16 is the strongest but these are acceptable
49 gg_genmove black
#? [C17|D16|B17]

# Q9 is clearly better than Q11 because it stops a black connection
# along the edge.
loadsgf games/strategy13.sgf
50 gg_genmove white
#? [Q9|Q11]*



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
