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
#? [1 S16]

loadsgf games/unfinished.sgf 214
266 owl_defend C15
#? [1 B17]

loadsgf games/unfinished.sgf 214
267 owl_defend Q2
#? [2 R3]

loadsgf games/owl36.sgf
268 owl_defend A3
#? [3 A1]

loadsgf games/nngs/lindq-gnugo-3.3.4-200207051636.sgf 192
269 owl_defend D11
#? [1 E9]

# tests 270 - 278 refer to badly connected dragon eyespaces
loadsgf games/owl37.sgf
270 owl_attack A4
#? [1 A5]*

271 owl_defend A4
#? [1 A5]*

loadsgf games/owl38.sgf
272 owl_defend H12
#? [1 F13]

loadsgf games/owl39.sgf 1
273 owl_attack E7
#? [1 G4]*

274 owl_defend E7
#? [1 G4|G7|B6]*

loadsgf games/owl39.sgf
275 owl_attack E7
#? [1 G4]*

276 owl_defend E7
#? [1 G4]*

loadsgf games/owl39a.sgf 1
277 owl_defend F7
#? [1 G4]*

loadsgf games/owl39a.sgf
278 owl_defend F7
#? [1 G4]*

# GNU Go tries to "eat lunch" at B1 instead of recognizing this as seki.
loadsgf games/owl40.sgf
279 owl_defend A3
#? [1 PASS]*

loadsgf games/nngs/gnugo-3.3.8-Joorin-200209251541.sgf 169
280 owl_attack E6
#? [1 F4]*

loadsgf games/nngs/gnugo-3.3.8-evand-200209170730.sgf 227
281 owl_attack O19
#? [1 O12]*

# Not sure whether there is a defense, but J9 certainly can be attacked.
loadsgf games/nngs/gnugo-3.3.8-viking4-200209250907.sgf 77
282 owl_attack J9
#? [1 .*]

loadsgf games/nngs/gnugo-3.3.8-viking4-200209250907.sgf 231
283 owl_defend J9
#? [0]*

loadsgf games/nngs/gnugo-3.3.8-viking4-200209250907.sgf 241
284 owl_defend J9
#? [1 (M7|H7)]

# Looks like a tough problem for GNU Go.
loadsgf games/owl41.sgf 1
285 owl_defend A6
#? [1 B3]*

# Getting into a ko with A3 is not good.
loadsgf games/owl41.sgf
286 owl_attack A6
#? [1 B3]*

loadsgf games/nngs/gnugo-3.3.9-RikiTiki-200210170706.sgf 230
287 owl_attack D4
#? [0]

# Seki.
loadsgf games/nngs/gnugo-3.3.9-RikiTiki-200210170706.sgf 234
288 owl_attack D4
#? [0]

loadsgf games/nngs/gnugo-3.3.9-pogonyshev-200210142137.sgf 62
289 owl_defend H8
#? [1 H6]

loadsgf games/nngs/gnugo-3.3.10-rcde05-200210280427.sgf 253
290 owl_attack M19
#? [1 O19]*
291 owl_defend M19
#? [2 O19]*

loadsgf games/gunnar/gunnar3.sgf 88
292 owl_defend T2
#? [1 T4]*

loadsgf games/nngs/gnugo-3.3.12-RikiTiki-200212042341.sgf 80
293 owl_defend Q6
#? [0]*

loadsgf games/owl42.sgf
294 owl_defend L11
#? [1 M9]*

loadsgf games/owl43.sgf
295 owl_attack C4
#? [1 C3]

loadsgf games/owl44.sgf
296 owl_attack D6
#? [1 E5|D8]*


########### end of tests #####################

# Report number of nodes visited by the tactical reading
10000 get_reading_node_counter
#? [0]&

# Report number of nodes visited by the owl code
10001 get_owl_node_counter
#? [0]&
