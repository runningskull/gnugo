# =============================
# Private test set nicklas4.tst
# These tests are from 19x19 games.
# None of these tests passes with version 2.7.222.
# I think these problems are "simple".
# =============================

# Reset applicable counters
reset_life_node_counter
reset_owl_node_counter
reset_reading_node_counter
reset_trymove_counter

loadsgf games/nicklas/nicklas8.sgf 146
805 gg_genmove black
#? [C5|B4]*

loadsgf games/nicklas/nicklas8.sgf 212
809 gg_genmove black
#? [B10]*

loadsgf games/nicklas/nicklas8.sgf 242
812 gg_genmove black
#? [K12]*

loadsgf games/nicklas/nicklas11.sgf 56
1102 gg_genmove black
#? [P9]*

loadsgf games/nicklas/nicklas11.sgf 172
1103 gg_genmove black
#? [C3]*

loadsgf games/nicklas/nicklas12.sgf 29
1201 gg_genmove black
#? [P9]*

loadsgf games/nicklas/nicklas12.sgf 95
1205 gg_genmove black
#? [E16]*

1215 dragon_status A4
#? [dead]*

loadsgf games/nicklas/nicklas15.sgf 185
1501 gg_genmove black
#? [D17]*


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
