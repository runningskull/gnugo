# Reset owl node counter
reset_owl_node_counter
# Reset reading node counter
reset_reading_node_counter

# Extremely bad misread. (Made GNU Go play at L9 with the last move before
# that position.)
loadsgf games/wing-yuhiko-gnugo.sgf 115
263 owl_defend M13
#? [1 R11]*

# Need another vital attack pattern here?
loadsgf games/wing-yuhiko-gnugo.sgf 220
264 owl_attack D18
#? [2 A18]*

# 3.3.2 comes up with R14, which of course doesn't work. T15 might
# work, but is so much worse than S16 that there's no reason to allow
# it.
# See also manyfaces:2.
loadsgf games/mfgg1.sgf 49
265 owl_attack S15
#? [1 S16]*

loadsgf games/unfinished.sgf 214
266 owl_defend C15
#? [1 B17]

loadsgf games/unfinished.sgf 214
267 owl_defend Q2
#? [2 R3]


########### end of tests #####################

# Report number of nodes visited by the tactical reading
10000 get_reading_node_counter
#? [0]&

# Report number of nodes visited by the owl code
10001 get_owl_node_counter
#? [0]&
