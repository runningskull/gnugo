# Reset applicable counters
reset_connection_node_counter
reset_owl_node_counter
reset_reading_node_counter
reset_trymove_counter

################################################################
# endgame2.sgf is a refinement of the endgame problems in ego.sgf

loadsgf games/endgame2.sgf 1
201 reg_genmove white
#? [R10]*

loadsgf games/endgame2.sgf 2
202 reg_genmove black
#? [Q9]

loadsgf games/endgame2.sgf 3
203 reg_genmove white
#? [A7]

# H19 and A12 are both 1 point sente.
loadsgf games/endgame2.sgf 4
204 reg_genmove black
#? [H19|A12]

loadsgf games/endgame2.sgf 5
205 reg_genmove white
#? [F18|E18]

loadsgf games/endgame2.sgf 6
206 reg_genmove black
#? [A12]

loadsgf games/endgame2.sgf 7
207 reg_genmove white
#? [A11]

loadsgf games/endgame2.sgf 8
208 reg_genmove black
#? [N1]*

loadsgf games/endgame2.sgf 9
209 reg_genmove white
#? [A6|O1]

# A5 is sente because the followup is bigger than black's capture at O2.
loadsgf games/endgame2.sgf 10
210 reg_genmove black
#? [A5]

# A5 is sente because the followup is bigger than black's capture at O2.
loadsgf games/endgame2.sgf 11
211 reg_genmove white
#? [A5]

loadsgf games/endgame2.sgf 12
212 reg_genmove black
#? [A5]

loadsgf games/endgame2.sgf 13
213 reg_genmove white
#? [Q8|C13]

loadsgf games/endgame2.sgf 14
214 reg_genmove black
#? [R8|C12|C13]

loadsgf games/endgame2.sgf 15
215 reg_genmove white
#? [C13]

loadsgf games/endgame2.sgf 16
216 reg_genmove black
#? [D13]

loadsgf games/endgame2.sgf 17
217 reg_genmove white
#? [C12]

loadsgf games/endgame2.sgf 18
218 reg_genmove black
#? [H1]

loadsgf games/endgame2.sgf 19
219 reg_genmove white
#? [T17]

#################################################################
# endgame3.sgf features a subtle "eventual shortage of liberties"
# position, causing E5 to be worth a whole point.

loadsgf games/endgame3.sgf
301 reg_genmove black
#? [E5]*
302 reg_genmove white
#? [E5]

#################################################################
# endgame4.sgf is a question of ending with sente or not. Black E5
# does defend the black F3 string, but turns out to lose a point in
# the end.

loadsgf games/endgame4.sgf
401 reg_genmove black
#? [C4]

# The basic 2/3 pt ko capture is undervalued.
loadsgf games/endgame5.sgf
501 reg_genmove black
#? [E5]

# An endgame sente pattern.
loadsgf games/endgame6.sgf 1
601 reg_genmove black
#? [J8]

loadsgf games/endgame6.sgf 1
602 reg_genmove white
#? [J8]

loadsgf games/endgame6.sgf 2
603 reg_genmove black
#? [C1]*

loadsgf games/endgame6.sgf 2
604 reg_genmove white
#? [D1]*

# G5 is one point in sente or 2 points in gote for white. E3 is bigger.
loadsgf games/endgame7.sgf 1
701 reg_genmove black
#? [E3]

loadsgf games/endgame7.sgf 1
702 reg_genmove white
#? [E3]

loadsgf games/endgame7.sgf 2
703 reg_genmove black
#? [G5]

loadsgf games/endgame7.sgf 2
704 reg_genmove white
#? [G5]

loadsgf games/nngs/gnugo-3.3.11-bconwil-200211202359.sgf 242
801 reg_genmove white
#? [S14]

802 reg_genmove black
#? [S14]*

# No point for F5.
loadsgf games/endgame8.sgf
803 reg_genmove black
#? [J7]
804 reg_genmove white
#? [J7]

loadsgf games/nngs/gnugo-3.3.17-Wiedemann-200303251932.sgf 151
810 genmove white
#? [F5|F4]

loadsgf games/nngs/gnugo-3.3.17-Wiedemann-200303251932.sgf 203
820 reg_genmove white
#? [H4|M10|K19]*

loadsgf games/paul.sgf 205
830 restricted_genmove white M19 B13
#? [B13]

loadsgf games/nngs/gnugo-3.3.17-Shindou-200304162217.sgf 110
840 reg_genmove white
#? [L15]*

loadsgf games/nngs/gnugo-3.3.17-Shindou-200304162217.sgf 140
850 restricted_genmove white K4 J4 L4 P5
#? [K4|J4|L4]*

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
