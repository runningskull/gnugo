# Reset applicable counters
reset_connection_node_counter
reset_owl_node_counter
reset_reading_node_counter
reset_trymove_counter

loadsgf games/gwe.sgf 231
# strictly speaking tenuki is possible here. Q1 is of course bad
# tm - Indeed GNU Go has finally figured out that tenuki is best!
# tm - yes, Q1 is bad.  T5 also wastes a point.
151 reg_genmove white
#? [!(Q1|T5)]*

loadsgf games/gwe.sgf 241
152 reg_genmove white
#? [S8]

loadsgf games/strategy36.sgf
153 reg_genmove black
#? [Q9]

loadsgf games/strategy37.sgf
154 reg_genmove white
#? [E8]

# Incident 190.
loadsgf games/incident189.sgf 186
155 reg_genmove black
#? [D18]*

loadsgf games/ssstator.sgf 133
156 reg_genmove black
#? [B14|B16]

loadsgf games/ssstator.sgf 136
157 reg_genmove white
#? [B14|B16|C18]

# E16 is completely aimless and ineffective.
loadsgf games/strategy38.sgf 51
158 reg_genmove black
#? [!E16]

loadsgf games/strategy38.sgf 117
159 reg_genmove black
#? [R9]*

loadsgf games/strategy38.sgf 133
160 reg_genmove black
#? [K2|L2|L3]

loadsgf games/strategy38.sgf 197
161 reg_genmove black
#? [E12]

# First live then attack. This should probably be analyzed as a semeai.
loadsgf games/strategy39.sgf 156
162 reg_genmove white
#? [O7|Q7|N7]

# First live then attack. This should probably be analyzed as a semeai.
# tm - P8, though not as clean, also kills all the B stones.
loadsgf games/strategy39.sgf 158
163 reg_genmove white
#? [O7|P8]*

#### N4 is clearly superior to O5.
#### tm - N4 has it's weaknesses (at O5) too.  Both clearly kill.
###    removing this problem.
###loadsgf games/strategy39.sgf 172
###164 reg_genmove white
####? [!O5]
###

# K1 is out of the question. K3 is locally better and L6 might kill with ko.
loadsgf games/strategy39.sgf 178
165 reg_genmove white
#? [K3|L6]

loadsgf games/incident94.sgf 114
166 reg_genmove white
#? [D11]

# Tenuki instead of urgent edge joseki move.
loadsgf games/strategy40.sgf 10
167 reg_genmove black
#? [D4]

# Safety must come first. This is almost a whole-board semeai.
loadsgf games/strategy40.sgf 30
168 reg_genmove black
#? [A3|A4]*

# Both J7 and J9 rescue the threatened stones, but J7 is the proper
# choice since it leaves a smaller ko threat behind. (That there's no
# risk for ko in this position is irrelevant. There's still no reason
# to choose J9 over J7.)
loadsgf games/strategy41.sgf
169 reg_genmove black
#? [J7]

# White can't play G19. Locally this move must be at G18.
loadsgf games/strategy42.sgf 174
170 reg_genmove white
#? [!G19]

# T19 is out of the question. Locally T17 is better but the correct
# move is A5.
loadsgf games/strategy42.sgf 188
171 reg_genmove white
#? [A5]

# It's correct to make a ko threat, but N9 is inefficient because it
# doesn't utilize the full ko potential of the position. Locally M8 is
# superior.
loadsgf games/strategy42.sgf 214
172 reg_genmove white
#? [!N9]

#CATEGORY=KO
#DESCRIPTION=GNU Go lacks sufficient Ko understanding
#SEVERITY=4
# There's no way to win the ko by making atari from above. This move
# must be at A9. This is a good example showing why only having the
# difference between a ko where we make the first threat or not is
# insufficient.
loadsgf games/strategy42.sgf 218
173 reg_genmove white
#? [A9]

# Always take the ko at least once.
loadsgf games/strategy42.sgf 248
174 reg_genmove white
#? [S19]

# This example shows that we need to find additional owl attack
# points. Currently only K15 is found to owl attack N19, but clearly
# the J14 move also does this, without sacrificing K13.
loadsgf games/strategy43.sgf 271
175 reg_genmove white
#? [J14]

# Best to capture with snapback.
loadsgf games/strategy44.sgf
176 reg_genmove black
#? [D9]

loadsgf games/xxlin.sgf 8
177 reg_genmove black
#? [E2]

# E2 looks somewhat smaller but is still a good move. I think it
# should be added. /gf
loadsgf games/xxlin.sgf 16
178 reg_genmove black
#? [P4|P3|E2]

#DESCRIPTION=Tenuki also seems OK here.
#Added E2 (3.1.14) -trevor
loadsgf games/xxlin.sgf 20
179 reg_genmove black
#? [P3|O2|P2|O4|E2]*

loadsgf games/xxlin.sgf 30
180 restricted_genmove black M7 Q8 R8 S8
#? [Q8|R8]*

loadsgf games/xxlin.sgf 38
181 reg_genmove black
#? [F3]*

loadsgf games/xxlin.sgf 48
182 reg_genmove black
#? [E7|F8]*

loadsgf games/xxlin.sgf 76
183 reg_genmove black
#? [P10|H9]

loadsgf games/xxlin.sgf 106
184 reg_genmove black
#? [H13|F15]*

# We may want to move this to a dedicated seki or semeai test suite in
# the future.
loadsgf games/seki02.sgf
185 reg_genmove black
#? [E7]

# O8 only move.
loadsgf games/owl29.sgf 134
186 reg_genmove white
#? [O8]

# B can play at C4 if we remember the tesuji B:C4 W:E4 B:B2.

loadsgf games/pooo.sgf 16
187 restricted_genmove black E4 C4 B3 B4 C1
#? [C4|E4]

loadsgf games/pooo.sgf 24
188 reg_genmove black
#? [B6|C4]

# The dragon at A2 is misclassified as inessential.

loadsgf games/pooo.sgf 50
189 reg_genmove black
#? [E14]*

loadsgf games/pooo.sgf 52
190 reg_genmove black
#? [D13]

loadsgf games/pooo.sgf 56
191 reg_genmove black
#? [C12]

loadsgf games/pooo.sgf 74
192 reg_genmove black
#? [A15]*

# (3.1.9) owl status of A11 is unchecked.

loadsgf games/bretz.sgf 26
193 reg_genmove black
#? [F16]*

loadsgf games/bretz.sgf 58
194 reg_genmove black
#? [A13]*

loadsgf games/bretz.sgf 88
195 reg_genmove black
#? [!S14]

loadsgf games/bretz.sgf 100
196 reg_genmove black
#? [M17]*

#Tenuki at S18 is not unthinkable. (3.1.14) -trevor
loadsgf games/bretz.sgf 130
197 reg_genmove black
#? [K3|S18]

#Tenuki at S18 is not unthinkable. (3.1.14) -trevor
loadsgf games/bretz.sgf 148
198 reg_genmove black
#? [C10|S18]

#Tenuki at S18 is not unthinkable. (3.1.14) -trevor
loadsgf games/bretz.sgf 164
199 reg_genmove black
#? [N5|S18]

#Tenuki at S18 is not unthinkable. (3.1.14) -trevor
loadsgf games/bretz.sgf 212
200 reg_genmove black
#? [P6|P7|Q7|S18]

loadsgf games/juka1.sgf 8
201 reg_genmove black
#? [D5]*

loadsgf games/juka1.sgf 14
202 reg_genmove black
#? [B6]*

loadsgf games/juka1.sgf 18
203 reg_genmove black
#? [D5]*

loadsgf games/juka1.sgf 28
204 reg_genmove black
#? [B2]*

loadsgf games/juka1.sgf 48
205 reg_genmove black
#? [!O11]

#CATEGORY=SEMEAI
#DESCRIPTION=K6 does take K8 to no eyes, but...
#SEVERITY=8
#GNU Go misses that K10 (large B group) has way few liberties
#than O9 group, hense missing importance of cutting off K8.
loadsgf games/strategy45.sgf
206 reg_genmove black
#? [L8]*

# T6 fails badly. Locally S9 looks best.
loadsgf games/owl30.sgf 84
207 reg_genmove black
#? [!T6]

loadsgf games/lordofpi.sgf 36
208 reg_genmove black
#? [A5|A2|B1]

# A4 allows W to make seki
loadsgf games/lordofpi.sgf 46
209 reg_genmove black
#? [!A4]

loadsgf games/lordofpi.sgf 52
210 reg_genmove black
#? [H7]

loadsgf games/lordofpi.sgf 56
211 reg_genmove black
#? [H8]

# Pattern LE14 gives an unreasonable value to H3.
loadsgf games/owl31.sgf 28
212 reg_genmove white
#? [G2]

loadsgf games/strategy46.sgf 172
213 reg_genmove white
#? [F1]

loadsgf games/strategy46.sgf 174
214 reg_genmove white
#? [D2]

# CD103a incorrectly thinks E7 defends against a combination attack.
loadsgf games/owl33.sgf 200
215 reg_genmove black
#? [!E7]

# B6 is too slow at this time.
loadsgf games/handtalk/handtalk1.sgf 45
216 reg_genmove black
#? [!B6]

loadsgf games/handtalk/handtalk1.sgf 49
217 reg_genmove black
#? [!R9]

# locally F12 is better. Similar problem at move 51
loadsgf games/handtalk/handtalk1.sgf 55
218 reg_genmove black
#? [!E12]

# lots of good problems in this game
loadsgf games/nngs/gnugo-3.1.30-morlvera-200204041921.sgf 154
219 reg_genmove white
#? [K9]*

loadsgf games/nngs/gnugo-3.1.30-merlin-200204041428.sgf 189
220 reg_genmove white
#? [R6]


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
