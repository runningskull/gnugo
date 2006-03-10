loadsgf games/wing-yuhiko-gnugo.sgf 12
221 restricted_genmove black S4 Q7
#? [S4]

# This might be worth a J pattern.
loadsgf games/wing-yuhiko-gnugo.sgf 30
222 reg_genmove black
#? [R14]*

loadsgf games/wing-yuhiko-gnugo.sgf 46
223 reg_genmove black
#? [P16]

#CATEGORY=STRATEGY
loadsgf games/wing-yuhiko-gnugo.sgf 62
224 reg_genmove black
#? [N15]

#CATEGORY=STRATEGY
loadsgf games/wing-yuhiko-gnugo.sgf 86
225 reg_genmove black
#? [O12]

#CATEGORY=CONNECTION
loadsgf games/olympiad2002/game2-19-gnugo-goint-1-0.sgf 54
226 reg_genmove white
#? [F5]

#CATEGORY=OWL
# F1 was played in the game as an owl defense of E4.
loadsgf games/olympiad2002/game2-19-gnugo-goint-1-0.sgf 66
227 reg_genmove white
#? [!F1]

#CATEGORY=STRATEGY
# Don't run away with an unimportant, weak stone.
loadsgf games/olympiad2002/game2-19-gnugo-goint-1-0.sgf 90
228 reg_genmove white
#? [!N12]

loadsgf games/olympiad2002/game1-19-go4-gnugo-1-0.sgf 41
229 reg_genmove black
#? [D7]

loadsgf games/olympiad2002/game1-19-go4-gnugo-1-0.sgf 53
230 reg_genmove black
#? [H9]

loadsgf games/olympiad2002/game1-19-go4-gnugo-1-0.sgf 107
231 reg_genmove black
#? [J2|J3]*

# If tuning this position be sure GNU Go plays subsequent moves acceptably
loadsgf games/olympiad2002/game1-19-goint-gnugo-1-0.sgf 17
232 reg_genmove black
#? [D11|E11]

loadsgf games/olympiad2002/game1-19-goint-gnugo-1-0.sgf 23
233 reg_genmove black
#? [E13]*

loadsgf games/olympiad2002/game1-19-goint-gnugo-1-0.sgf 29
234 reg_genmove black
#? [H14]

loadsgf games/olympiad2002/game1-19-goint-gnugo-1-0.sgf 47
235 reg_genmove black
#? [G12|J13]

loadsgf games/olympiad2002/game1-19-goint-gnugo-1-0.sgf 91
236 reg_genmove black
#? [R12]*

# Not a valid problem
# loadsgf games/nngs/gnugo-3.3.6-evand-200208290255.sgf 73
# 272 reg_genmove white
# #? [C7]*

# Black has a combination attack which must be defended correctly against.
# F8 and F7 both fail.
loadsgf games/strategy47.sgf
273 reg_genmove white
#? [E8|E9|F9|E7|F6|E6|D7|C7]

# Black has a combination attack which must be defended correctly against.
# In particular L5 fails.
loadsgf games/strategy48.sgf
274 restricted_genmove white L5 M5 N5 M6
#? [M6]*

# Semeai problem. White can make seki, black can kill.
loadsgf games/strategy49.sgf
275 reg_genmove white
#? [A18]
276 reg_genmove black
#? [A18]

# R19 defends everything.
loadsgf games/strategy50.sgf
277 reg_genmove white
#? [R19]

loadsgf games/vaughn.sgf 44
278 reg_genmove black
#? [!J11]*

# Really, anything is better than A18. (B13 and B12 are quite
# ineffective too but can at least not be repeated...)
loadsgf games/reading41.sgf 130
279 restricted_genmove black A18 A16 B13 B12 E11
#? [A16]

loadsgf games/nngs/takeda-gnugo-3.3.23-200307302010.sgf 10
280 reg_genmove black
#? [P3]*

loadsgf games/nngs/gnugo-3.3.23-viking4-200307312134.sgf 37
281 reg_genmove white
#? [E13]*

#take the ko!
loadsgf games/nngs/gnugo-3.3.23-viking4-200307312134.sgf 195
282 reg_genmove white
#? [O13]

loadsgf games/nngs/gnugo-3.3.23-viking4-200307312134.sgf 235
283 reg_genmove white
#? [B9]

loadsgf games/nngs/jypower-gnugo-3.3.22-200307050519.sgf 154
284 reg_genmove black
#? [R12|Q9]

loadsgf games/nngs/jypower-gnugo-3.3.22-200307050519.sgf 162
285 restricted_genmove black S10 O11 O10 Q11 N7
#? [S10]

loadsgf games/self_play/354-34-1.sgf 180
286 reg_genmove white
#? [K5]

loadsgf games/self_play/354-34-3.sgf 194
287 reg_genmove white
#? [C2]*

# By Andreas Braendle: 3.5.4 level 15 plays F4, and doesn't see the
# value of a pincer.
loadsgf games/ab1_fuseki.sgf 32
288 reg_genmove black
#? [R12|R11|Q12|Q11]*

289 restricted_genmove black F4 M16
#? [M16]

# avoid bad exchange at E2
loadsgf games/gg-040225.sgf 220
290 reg_genmove black
#? [S16|S17|T16]

# P7 creates a shortage of liberties.
loadsgf games/kisei28_g7.sgf 189
291 restricted_genmove black P7 L9
#? [L9]*

loadsgf games/semeai/semeai19.sgf 42
292 restricted_genmove black A15 F16 F17 S4 Q3
#? [F16|F17]

293 reg_genmove white
#? [F17]*

# Very important to separate black.
loadsgf games/tso_igobot_2006-01-16.sgf 18
294 restricted_genmove white O6 S6
#? [S6]

# Just play J2 directly.
loadsgf games/CrazyStone1.sgf 34
295 reg_genmove white
#? [J2]*

# Play the proper move.
loadsgf games/CrazyStone2.sgf 17
296 restricted_genmove black F9 G8
#? [G8]

# D9 is quite bad. (Not played in the game since it's the wrong player
# but the move of choice for GNU Go 3.6 and 3.7.7.)
loadsgf games/CrazyStone2.sgf 18
297 restricted_genmove white D8 D9
#? [D8]*

# A5 is an overplay since it can't be defended without losing the
# important eye in the upper left corner. (The list of accepted moves
# is rather generous and can be
loadsgf games/CrazyStone2.sgf 49
298 reg_genmove black
#? [D2|B7]

# Can't play away any more liberties. Start picking the G8 stones off
# the board immediately so that no more than the A7 stones need be
# lost. (There's no time to attack from above.)
loadsgf games/CrazyStone2.sgf 61
299 reg_genmove black
#? [H3]*

# Still possible to save most of the stones.
# See also connection:127.
loadsgf games/CrazyStone2.sgf 77
300 reg_genmove black
#? [H3]*

# B1 is the biggest way to defend the B7 dragon.
loadsgf games/strategy51.sgf
301 reg_genmove black
#? [B1]
