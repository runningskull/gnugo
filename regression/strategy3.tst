# Reset applicable counters
reset_connection_node_counter
reset_owl_node_counter
reset_reading_node_counter
reset_trymove_counter

# numbering is continued from strategy2.tst.

loadsgf games/strategy26.sgf 259
101 gg_genmove black
#? [M16]

# Playing N18 instead of O19 leaves the threat to make a point on the edge.
loadsgf games/strategy26.sgf 265
102 gg_genmove black
#? [N18]

# A15 and C15 both fail badly. C16 allows white to make seki.
loadsgf games/strategy26.sgf 285
103 gg_genmove black
#? [B16]

# Urgent response.
loadsgf games/strategy27.sgf 12
104 gg_genmove white
#? [P4]

loadsgf games/strategy27.sgf 62
105 gg_genmove white
#? [J2]

loadsgf games/strategy27.sgf 152
106 gg_genmove white
#? [F10]

loadsgf games/strategy27.sgf 190
107 gg_genmove white
#? [F1]

loadsgf games/strategy27.sgf 208
108 gg_genmove white
#? [C15]

#CATEGORY=SEMEAI
#DESCRIPTION=
#SEVERITY=
# Important semeai example.
loadsgf games/strategy27.sgf 216
109 gg_genmove white
#? [A9|C7]*

# Important semeai example.
loadsgf games/strategy27.sgf 220
110 gg_genmove white
#? [A8]

loadsgf games/me.sgf 44
111 gg_genmove black
#? [K3]*

loadsgf games/me.sgf 84
112 gg_genmove black
#? [M12]

loadsgf games/me.sgf 150
113 gg_genmove black
#? [P1]

loadsgf games/poka.sgf 42
114 gg_genmove white
#? [D7]*

loadsgf games/poka.sgf 52
115 gg_genmove white
#? [G2]

# incident 281
# M4 and N5 are okay. O5 is a bad mistake.
loadsgf games/incident278.sgf 212
116 gg_genmove black
#? [M4|N5]

# incident 282
loadsgf games/incident278.sgf 252
117 gg_genmove black
#? [M2]

loadsgf games/incident240.sgf 65
118 gg_genmove black
#? [A8]

# incident 241
loadsgf games/incident240.sgf 69
119 gg_genmove black
#? [D9|J3]*

# incident 221
loadsgf games/incident221.sgf 35
120 gg_genmove black
#? [C7]

# incident 222
loadsgf games/incident221.sgf 39
121 gg_genmove black
#? [C7]

# incident 223
# see also owl:178
loadsgf games/incident223.sgf 228
122 gg_genmove black
#? [(R19|T18)]

# incident 275
loadsgf games/incident272.sgf 250
123 gg_genmove black
#? [P4]

# incident 271
# C5 seems to lead to seki. Semeai mistake.
loadsgf games/incident269.sgf 276
124 gg_genmove black
#? [C5]*

# incident 158
loadsgf games/incident156.sgf 232
125 gg_genmove black
#? [K17|J17|K18|L18|K19|L19|M19|H16]

# The move valuation must become aware of the distinction between
# attack and defense with or without ko.
loadsgf games/strategy29.sgf 138
126 gg_genmove white
#? [B19]

# incident 74
loadsgf games/incident74.sgf 236
127 gg_genmove black
#? [!(S5|T4|T7)]

# Semeai problem.
loadsgf games/strategy30.sgf 171
128 gg_genmove black
#? [O8]

# Semeai problem.
# tm - S12 also works ok.
loadsgf games/strategy30.sgf 201
129 gg_genmove black
#? [Q9|S12]

loadsgf games/strategy31.sgf 34
130 gg_genmove white
#? [!D17]

loadsgf games/strategy31.sgf 44
131 gg_genmove white
#? [!(B18|P17|Q17|R16|R15|R18|Q18|P18|S17|S16|S15|G17)]

loadsgf games/strategy31.sgf 48
132 gg_genmove white
#? [!F4]

# Locally D4 is the only move, but there may be bigger moves elsewhere.
loadsgf games/strategy31.sgf 50
133 gg_genmove white
#? [!(E5|E4)]

# R10 is clearly superior to S10
loadsgf games/strategy31.sgf 158
134 gg_genmove white
#? [!S10]

loadsgf games/strategy31.sgf 188
135 gg_genmove white
#? [M12|N13|M13]*

loadsgf games/strategy32.sgf 19
136 gg_genmove black
#? [E2]

loadsgf games/mf1.sgf 29
137 gg_genmove black
#? [F5]

loadsgf games/manner.sgf 12
138 gg_genmove white
#? [!O16]

# Urgent semeai move, worth more than 150 points.
loadsgf games/strategy33.sgf 189
139 gg_genmove black
#? [M18]

loadsgf games/owl26.sgf 138
140 gg_genmove white
#? [N5|M6]

loadsgf games/strategy34.sgf 51
141 gg_genmove black
#? [C7]

loadsgf games/strategy34.sgf 57
142 gg_genmove black
#? [F7]

loadsgf games/strategy34.sgf 59
143 gg_genmove black
#? [F8]

loadsgf games/strategy34.sgf 61
144 gg_genmove black
#? [F8]

# Incident 209. Semeai mistake.
loadsgf games/incident209.sgf 193
145 gg_genmove white
#? [S14|T14]

# Incident 210. Semeai mistake.
loadsgf games/incident209.sgf 259
146 gg_genmove white
#? [R11]

# G3 is necessary here.
loadsgf games/strategy35.sgf 112
147 gg_genmove white
#? [!G5]

loadsgf games/strategy35.sgf 140
148 gg_genmove white
#? [!K16]

loadsgf games/gwe.sgf 225
# see also reading test 149
149 gg_genmove white
#? [!O1]

# Owl reading mistake, eyespace related.
loadsgf games/gwe.sgf 203
150 gg_genmove white
#? [Q1]*

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
