# =============================
# Private test set nicklas2.tst
# These tests are from 9x9 games.
# None of these tests passes with version 2.7.222.
# I think these problems are "simple".
# =============================

# Reset applicable counters
reset_life_node_counter
reset_owl_node_counter
reset_reading_node_counter
reset_trymove_counter

loadsgf games/nicklas/nicklas1.sgf 29
102 gg_genmove black
#? [D4]*

loadsgf games/nicklas/nicklas6.sgf 23
601 gg_genmove black
#? [D8|C8|D6|D4]*

loadsgf games/nicklas/nicklas7.sgf 29
701 gg_genmove black
#? [F1]*

loadsgf games/nicklas/nicklas9.sgf 28
902 dragon_status E9
#? [critical]*

903 dragon_status G7
#? [critical H8 H8]

# E1 loses some endgame points since black must capture later anyhow, 
# but it saves the group. It could be regarded as acceptable at this 
# point.
loadsgf games/nicklas/nicklas9.sgf 50
904 gg_genmove black
#? [B1|E1]

loadsgf games/nicklas/nicklas10.sgf 18
1001 gg_genmove black
#? [B7]

loadsgf games/nicklas/nicklas13.sgf 49
1301 gg_genmove black
#? [B8|B9]

loadsgf games/nicklas/nicklas14.sgf 31
1401 gg_genmove black
#? [B3]*

loadsgf games/nicklas/nicklas14.sgf 43
1402 gg_genmove black
#? [J8|J6]*

# One eye wins against no eye
loadsgf games/nicklas/nicklas14.sgf 61
1407 gg_genmove black
#? [A6]*

loadsgf games/nicklas/nicklas17.sgf 47
1701 gg_genmove black
#? [C9]*

loadsgf games/nicklas/nicklas18.sgf 17
1802 gg_genmove black
#? [E2]

loadsgf games/nicklas/nicklas18.sgf 45
1803 gg_genmove black
#? [A8]*

loadsgf games/nicklas/nicklas21.sgf 72
2102 gg_genmove black
#? [PASS]*

loadsgf games/nicklas/nicklas21.sgf 74
2103 gg_genmove black
#? [PASS]*

loadsgf games/nicklas/nicklas21.sgf 76
2104 gg_genmove black
#? [PASS]

loadsgf games/nicklas/nicklas22.sgf 42
2201 gg_genmove black
#? [C2]

loadsgf games/nicklas/nicklas22.sgf 50
2202 gg_genmove black
#? [B1]

loadsgf games/nicklas/nicklas24.sgf 41
2401 gg_genmove black
#? [G3]*



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
