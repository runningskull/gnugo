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
280 owl_attack F6
#? [1 F4]

loadsgf games/nngs/gnugo-3.3.8-evand-200209170730.sgf 227
281 owl_attack O19
#? [1 O12]

# Not sure whether there is a defense, but J9 certainly can be attacked.
loadsgf games/nngs/gnugo-3.3.8-viking4-200209250907.sgf 77
282 owl_attack J9
#? [1 .*]

loadsgf games/nngs/gnugo-3.3.8-viking4-200209250907.sgf 231
283 owl_defend J9
#? [0]*

loadsgf games/nngs/gnugo-3.3.8-viking4-200209250907.sgf 241
284 owl_defend J9
#? [1 (M7|H7|M5|L8)]

# Looks like a tough problem for GNU Go.
loadsgf games/owl41.sgf 1
285 owl_defend A6
#? [1 B3]

# Getting into a ko with A3 is not good.
loadsgf games/owl41.sgf
286 owl_attack A6
#? [1 B3]

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
#? [1 O19]
291 owl_defend M19
#? [2 O19]*

loadsgf games/gunnar/gunnar3.sgf 88
292 owl_defend T2
#? [1 T4]

loadsgf games/nngs/gnugo-3.3.12-RikiTiki-200212042341.sgf 80
293 owl_defend Q6
#? [0]*

loadsgf games/owl42.sgf
294 owl_defend L11
#? [1 M9]

loadsgf games/owl43.sgf
295 owl_attack C4
#? [1 C3]

loadsgf games/owl44.sgf
296 owl_attack D6
#? [1 E5|D8]

loadsgf games/nngs/gnugo-3.3.16-ccwills-200302151826.sgf 110
297 owl_defend C8
#? [1 A13]*

loadsgf games/nngs/halti-gnugo-3.3.17-200303162357.sgf 228
298 owl_attack P3
#? [2 Q3]
299 owl_defend P3
#? [1 Q1]

loadsgf games/nngs/wingjk-gnugo-3.3.17-200304070910.sgf
300 owl_defend Q4
#? [1 S5|S2]

loadsgf games/nngs/gnugo-3.3.17-Shindou-200304162217.sgf 126
301 owl_does_defend A12 A13
#? [0]*

loadsgf games/nngs/gnugo-3.3.17-Shindou-200304162217.sgf 148
302 owl_defend C19
#? [0]*

loadsgf games/nngs/gnugo-3.3.17-Shindou-200304162217.sgf 166
303 owl_does_defend A17 A16
#? [0]*

loadsgf games/gunnar/gunnar6.sgf 135
304 owl_defend Q14
#? [0]

loadsgf games/gunnar/gunnar7.sgf
305 owl_defend P8
#? [0]*

# Only topologically false eyes but it still lives!
# See also gunnar:27.
loadsgf games/gunnar/rbm-bobk-200303111815.sgf -L 159
306 owl_attack M17
#? [0]*

loadsgf games/nngs/guestn-gnugo-3.3.17-200304200323.sgf 52
307 owl_attack S10
#? [1 S11]

308 owl_defend S10
#? [1 S11]*

loadsgf games/nngs/gnugo-3.3.18-spewnboy-200304291635.sgf 91
309 owl_attack N5
#? [1 L2|L4]

310 owl_defend N5
#? [1 L2|L4]

loadsgf games/nngs/qly-gnugo-3.3.18-200305022134.sgf 72
311 owl_attack K6
#? [1 E2]*

312 owl_defend K6
#? [1 C3]*

loadsgf games/owl45.sgf
313 owl_attack B18
#? [1 (A18|C19)]

# See also ld_owl:12
loadsgf games/life_and_death/ld3.sgf
white E1
black B1
white D1
314 owl_defend A2
#? [1 F1]

loadsgf games/splee2.sgf 128
315 owl_does_attack Q9 M10
#? [0]*

loadsgf games/nngs/gnugo-3.3.10-niki-200210281349.sgf 136
316 owl_defend L7
#? [0]*

# J5 and J3 fail spectacularly due to shortage of liberties.
loadsgf games/owl46.sgf
317 owl_attack J4
#? [1 (J7|J6|J8)]*

# See also lazarus:10
loadsgf games/nngs/Lazarus-gnugo-3.1.19-200201092246.sgf 258
318 owl_does_defend F10 D10
#? [0]*

# Problem with bogus escape potential in GNU Go 3.3.22.
loadsgf games/owl48.sgf
319 owl_attack S10
#? [1 S6]*


########### end of tests #####################

# Report number of nodes visited by the tactical reading
10000 get_reading_node_counter
#? [0]&

# Report number of nodes visited by the owl code
10001 get_owl_node_counter
#? [0]&
