# Reset applicable counters
reset_connection_node_counter
reset_owl_node_counter
reset_reading_node_counter
reset_trymove_counter

# M19 is a serious blunder.
loadsgf games/nngs/gnugo-3.3.10-viking4-200210261703.sgf 181
1 gg_genmove white
#? [M17]

# D19 is locally worse than both E17 and E18.
loadsgf games/nngs/gnugo-3.3.10-rcde05-200210280427.sgf 83
2 restricted_genmove white D19 E18 E17
#? [E17|E18]

# Just connect at K17. Don't make it difficult.
loadsgf games/nngs/gnugo-3.3.10-rcde05-200210280427.sgf 157
3 gg_genmove white
#? [K17]*

# L17 is horribly overconcentrated
loadsgf games/nngs/gnugo-3.3.10-rcde05-200210300235.sgf 27
4 gg_genmove white
#? [!L17]

# G1 is the wrong way to defend G2. Both F2 and K2/K3 are locally better.
# But what's really urgent is a move around F12.
# An invasion around R8 would also be big but must wait until the left
# edge has been settled.
loadsgf games/nngs/gnugo-3.3.10-rcde05-200210300235.sgf 51
5 gg_genmove white
#? [F12]*

# B17 is unreasonable. Locally E16 only move.
loadsgf games/nngs/gnugo-3.3.11-ccwills-200211031528.sgf 35
6 restricted_genmove white B17 E16
#? [E16]

# O14 adds nothing at all to the position.
loadsgf games/nngs/niko-gnugo-3.3.11-200211201651.sgf 216
7 gg_genmove black
#? [!O14]

# O13 is completely wasted.
loadsgf games/nngs/niko-gnugo-3.3.11-200211201651.sgf 222
8 gg_genmove black
#? [!O13]*

# Critical to keep black split up.
loadsgf games/gunnar/gunnar1.sgf 34
9 gg_genmove white
#? [F13]*

# Q15 was supposed to attack either Q14 and Q16 but obviously doesn't work.
# R12 is probably better than R14 but both get the point.
loadsgf games/gunnar/gunnar1.sgf 96
10 gg_genmove white
#? [R12|R14]*

# L8 doesn't suffice for local life.
loadsgf games/gunnar/gunnar1.sgf 126
11 gg_genmove white
#? [K9]

# C13 is nonsense. Just connect with B13.
loadsgf games/gunnar/gunnar2.sgf 72
12 gg_genmove white
#? [B13]

# Q5 is 3 points reverse sente. Q12 is of similar size.
# E4 is only one point reverse sente.
loadsgf games/gunnar/gunnar2.sgf 164
13 gg_genmove white
#? [Q5|Q12]

# Necessary to block at Q11. O13 is a rather bad idea.
loadsgf games/gunnar/gunnar2.sgf 168
14 gg_genmove white
#? [Q11]*

# O6 is clearly locally better than P6.
loadsgf games/gunnar/gunnar3.sgf 140
15 restricted_genmove white O6 P6
#? [O6]*

# T5 is huge. See next test case.
loadsgf games/gunnar/gunnar3.sgf 190
16 gg_genmove white
#? [T5]*

# T6 allows a combination attack. Necessary to fall back.
loadsgf games/gunnar/gunnar3.sgf 192
17 gg_genmove white
#? [S7]*

# Q15 is just swallowed.
loadsgf games/gunnar/gunnar4.sgf 180
18 restricted_genmove white P15 Q15
#? [P15]

# No reason to play ko with L3.
loadsgf games/gunnar/gunnar4.sgf 198
19 gg_genmove white
#? [L4]*

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
