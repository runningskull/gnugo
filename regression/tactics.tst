# Reset applicable counters
reset_connection_node_counter
reset_owl_node_counter
reset_reading_node_counter
reset_trymove_counter

loadsgf games/tactics01.sgf
1 gg_genmove black
#? [C15|D15|B13|B11|B14]*

loadsgf games/tactics02.sgf
2 gg_genmove black
#? [S11|S12]

loadsgf games/tactics03.sgf
3 gg_genmove black
#? [M18]*

# This is in part an atari_atari problem. The best local move is K18
# though J18 or G18 might be acceptable.
loadsgf games/rosebud4.sgf 138
4 gg_genmove white
#? [K18]*

loadsgf games/tactics04.sgf 206
5 gg_genmove white
#? [N16]

# attack_either and defend_both tests could go in a separate test suite,
# once there is more of them

loadsgf games/nngs/Lazarus-gnugo-3.1.34-200204280120.sgf 32
trymove black D12
trymove white C13
trymove white C12
trymove black C11
101 attack_either C11 D12
#? [1 D12]*
popgo
popgo
popgo
popgo

# This is subtle. White N3 works as defense to L4.
loadsgf games/mertin13x13/katsunari-gnugo2.W+4.sgf 126
trymove black L4
102 defend_both N7 K4
#? [0]*
popgo



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
