# Reset applicable counters
reset_life_node_counter
reset_owl_node_counter
reset_reading_node_counter
reset_trymove_counter

loadsgf games/mfgg1.sgf 43
1 genmove_black
#? [S13]

loadsgf games/mfgg1.sgf 49
2 genmove_black
#? [S16]

loadsgf games/mfgg1.sgf 65
3 genmove_black
#? [S12]*

loadsgf games/mfgg1.sgf 97
4 genmove_black
#? [!T6]

loadsgf games/mfgg2.sgf 19
5 genmove_black
#? [M5|M15|R3|R17]*

loadsgf games/mfgg2.sgf 23
6 genmove_black
#? [R3]*

loadsgf games/mfgg2.sgf 45
7 genmove_black
#? [R2|S7]*

loadsgf games/mfgg2.sgf 117
8 genmove_black
#? [T2]*

loadsgf games/mfgg3.sgf 42
9 genmove_black
#? [!P12]

loadsgf games/mfgg3.sgf 44
10 genmove_black
#? [N3]*


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
