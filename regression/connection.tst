# Reset applicable counters
reset_reading_node_counter

loadsgf games/connection1.sgf
1 same_dragon G17 K17
#? [0]*
2 same_dragon R12 R9
#? [0]

loadsgf games/strategy11.sgf 87
3 same_dragon A6 B4
#? [0]

loadsgf games/incident104.sgf 63
4 same_dragon C14 E14
#? [0]

loadsgf games/strategy13.sgf
5 same_dragon S11 R12
#? [0]*

# incident 213
loadsgf games/incident211.sgf 40
6 same_dragon G8 J6
#? [0]*

loadsgf games/strategy21.sgf 86
7 same_dragon F4 J3
#? [0]

loadsgf games/owl19.sgf
# A problem with rot2 rotated owl tests 128 and 129 reduces to
# this dragon amalgamation problem.
8 same_dragon A15 C16
#? [1]
9 same_dragon P19 Q17
#? [1]

# This definitely requires dynamic connection reading.
loadsgf games/incident240.sgf 69
10 same_dragon C10 E9
#? [0]

loadsgf games/connection2.sgf 177
11 same_dragon G6 J7
#? [0]*
12 same_dragon G8 J7
#? [0]*

loadsgf games/strategy29.sgf 204
13 same_dragon O6 Q8
#? [0]
14 same_dragon O6 R6
#? [0]

loadsgf games/strategy33.sgf 159
15 same_dragon G7 F9
#? [0]*

loadsgf games/doublecut.sgf
16 same_dragon E16 G14
#? [0]

loadsgf games/doublecut2.sgf 151
17 same_dragon D13 F12
#? [0]*

loadsgf games/arion.sgf 63
18 same_dragon N17 P17
#? [1]*

loadsgf games/dublin1.sgf 71
19 same_dragon B15 E16
#? [0]*

loadsgf games/nicklas/nicklas12.sgf 69
20 connect K17 N17
#? [1 K18]*


# Report number of nodes visited by the tactical reading
10000 get_reading_node_counter
#? [0]&
