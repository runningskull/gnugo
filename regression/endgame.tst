# Reset applicable counters
reset_life_node_counter
reset_owl_node_counter
reset_reading_node_counter
reset_trymove_counter

# endgame1.sgf is problem 1 in Get Strong at the Endgame
loadsgf games/endgame1.sgf 1
101 gg_genmove black
#? [F9]

loadsgf games/endgame1.sgf 2
102 gg_genmove white
#? [E9]

loadsgf games/endgame1.sgf 3
103 gg_genmove black
#? [G9]

loadsgf games/endgame1.sgf 4
104 gg_genmove white
#? [E8|D8]

loadsgf games/endgame1.sgf 5
105 gg_genmove black
#? [J2|J3|A7]

loadsgf games/endgame1.sgf 6
106 gg_genmove white
#? [G1|H1|F3]

# J3 is 0 points double sente.
loadsgf games/endgame1.sgf 7
107 gg_genmove black
#? [J3|A7]*

loadsgf games/endgame1.sgf 8
108 gg_genmove white
#? [G1]

loadsgf games/endgame1.sgf 9
109 gg_genmove black
#? [A7]

loadsgf games/endgame1.sgf 10
110 gg_genmove white
#? [A8]

loadsgf games/endgame1.sgf 11
111 gg_genmove black
#? [A6]

loadsgf games/endgame1.sgf 12
112 gg_genmove white
#? [B8|B9|C8]

loadsgf games/endgame1.sgf 13
113 gg_genmove black
#? [D2|C5]

loadsgf games/endgame1.sgf 14
114 gg_genmove white
#? [E1]

loadsgf games/endgame1.sgf 15
115 gg_genmove black
#? [C5]

loadsgf games/endgame1.sgf 16
116 gg_genmove white
#? [D5]

################################################################
# endgame2.sgf is a refinement of the endgame problems in ego.sgf

loadsgf games/endgame2.sgf 1
201 gg_genmove white
#? [R10]*

loadsgf games/endgame2.sgf 2
202 gg_genmove black
#? [Q9]

loadsgf games/endgame2.sgf 3
203 gg_genmove white
#? [A7]

# H19 and A12 are both 1 point sente.
loadsgf games/endgame2.sgf 4
204 gg_genmove black
#? [H19|A12]

loadsgf games/endgame2.sgf 5
205 gg_genmove white
#? [F18|E18]

loadsgf games/endgame2.sgf 6
206 gg_genmove black
#? [A12]

loadsgf games/endgame2.sgf 7
207 gg_genmove white
#? [A11]

loadsgf games/endgame2.sgf 8
208 gg_genmove black
#? [N1]*

loadsgf games/endgame2.sgf 9
209 gg_genmove white
#? [A6|O1]

# A5 is sente because the followup is bigger than black's capture at O2.
loadsgf games/endgame2.sgf 10
210 gg_genmove black
#? [A5]

# A5 is sente because the followup is bigger than black's capture at O2.
loadsgf games/endgame2.sgf 11
211 gg_genmove white
#? [A5]

loadsgf games/endgame2.sgf 12
212 gg_genmove black
#? [A5]

loadsgf games/endgame2.sgf 13
213 gg_genmove white
#? [Q8|C13]

loadsgf games/endgame2.sgf 14
214 gg_genmove black
#? [R8|C12|C13]

loadsgf games/endgame2.sgf 15
215 gg_genmove white
#? [C13]

loadsgf games/endgame2.sgf 16
216 gg_genmove black
#? [D13]

loadsgf games/endgame2.sgf 17
217 gg_genmove white
#? [C12]

loadsgf games/endgame2.sgf 18
218 gg_genmove black
#? [H1]

loadsgf games/endgame2.sgf 19
219 gg_genmove white
#? [T17]

#################################################################
# endgame3.sgf features a subtle "eventual shortage of liberties"
# position, causing E5 to be worth a whole point.

loadsgf games/endgame3.sgf
301 gg_genmove black
#? [E5]*
302 gg_genmove white
#? [E5]*


#################################################################
# endgame4.sgf is a question of ending with sente or not. Black E5
# does defend the black F3 string, but turns out to lose a point in
# the end.

loadsgf games/endgame4.sgf
401 gg_genmove black
#? [C4]*


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
