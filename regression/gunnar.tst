# Reset applicable counters
reset_connection_node_counter
reset_owl_node_counter
reset_reading_node_counter
reset_trymove_counter

# M19 is a serious blunder.
loadsgf games/nngs/gnugo-3.3.10-viking4-200210261703.sgf 181
1 gg_genmove white
#? [M17]*

# D19 is locally worse than both E17 and E18.
loadsgf games/nngs/gnugo-3.3.10-rcde05-200210280427.sgf 83
2 gg_genmove white
#? [!D19]*

# Just connect at K17. Don't make it difficult.
loadsgf games/nngs/gnugo-3.3.10-rcde05-200210280427.sgf 157
3 gg_genmove white
#? [K17]*

# L17 is horribly overconcentrated
loadsgf games/nngs/gnugo-3.3.10-rcde05-200210300235.sgf 27
4 gg_genmove white
#? [!L17]*

# G1 is the wrong way to defend G2. Both F2 and K2/K3 are locally better.
# But what's really urgent is a move around F12.
# An invasion around R8 would also be big but must wait until the left
# edge has been settled.
loadsgf games/nngs/gnugo-3.3.10-rcde05-200210300235.sgf 51
5 gg_genmove white
#? [F12]*

# B17 is unreasonable. Locally E16 only move.
loadsgf games/nngs/gnugo-3.3.11-ccwills-200211031528.sgf 35
6 gg_genmove white
#? [!B17]

############ End of Tests #################

# Report number of nodes visited by the tactical reading
10000 get_reading_node_counter
#? [0]&

# Report number of nodes visited by the owl code
10001 get_owl_node_counter
#? [0]&

# Report number of nodes visited by the connection code
10002 get_connection_node_counter
#? [0]&

# Report number of trymoves/trykos visited by the test
10003 get_trymove_counter
#? [0]&
