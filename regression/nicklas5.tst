# =============================
# Private test set nicklas5.tst
# These tests are from 19x19 games.
# None of these tests passes with version 2.7.222.
# I think these problems are "non trivial".
# =============================

# Reset applicable counters
reset_life_node_counter
reset_owl_node_counter
reset_reading_node_counter
reset_trymove_counter


loadsgf games/nicklas/nicklas8.sgf 116
802 gg_genmove black
#? [O12]*

loadsgf games/nicklas/nicklas8.sgf 134
803 gg_genmove black
#? [F7|G7]*

loadsgf games/nicklas/nicklas8.sgf 140
804 gg_genmove black
#? [D6]*

# Locally K18 is superior to L17.
loadsgf games/nicklas/nicklas12.sgf 69
1202 gg_genmove black
#? [!L17]*

loadsgf games/nicklas/nicklas12.sgf 71
1203 gg_genmove black
#? [N18|N19|M19]*

loadsgf games/nicklas/nicklas12.sgf 89
1204 gg_genmove black
#? [H15]*

loadsgf games/nicklas/nicklas12.sgf 182
1211 dragon_status L8
#? [critical]*

loadsgf games/nicklas/nicklas12.sgf 199
1212 dragon_status P13
#? [critical]*

loadsgf games/nicklas/nicklas12.sgf 263
1217 gg_genmove black
#? [F3]*

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
