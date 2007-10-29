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
#? [1 (G4|G7|B6)]

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
#? [0]

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
#? [1 (E5|D8)]

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
#? [1 (S5|S2)]*

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
loadsgf games/gunnar/rbm-bobk-200303111815.sgf 159
306 owl_attack M17
#? [0]*

loadsgf games/nngs/guestn-gnugo-3.3.17-200304200323.sgf 52
307 owl_attack S10
#? [1 S11]

308 owl_defend S10
#? [1 S11]*

loadsgf games/nngs/gnugo-3.3.18-spewnboy-200304291635.sgf 91
309 owl_attack N5
#? [1 (L2|L4)]

310 owl_defend N5
#? [1 (L2|L4)]

loadsgf games/nngs/qly-gnugo-3.3.18-200305022134.sgf 72
311 owl_attack K6
#? [1 E2]

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

loadsgf games/nngs/takeda-gnugo-3.4-200308142205.sgf 60
320 owl_attack R4
#? [0]

loadsgf games/nngs/gnugo-3.4-viking4-200308191053.sgf 79
321 owl_defend F3
#? [0]*

loadsgf games/nngs/gnugo-3.4-viking4-200308191053.sgf 81
322 owl_defend F3
#? [0]

loadsgf games/nngs/gnugo-3.4-viking4-200308191053.sgf 85
323 owl_defend F3
#? [0]

loadsgf games/nngs/gnugo-3.4-viking4-200308191053.sgf 89
324 owl_defend E5
#? [0]

loadsgf games/nngs/ccwills-gnugo-3.4-200308231739.sgf 145
325 owl_attack R18
#? [1 (S18|T15)]*

loadsgf games/nngs/ccwills-gnugo-3.4-200308231739.sgf 146
326 owl_defend R18
#? [3 S18]

loadsgf games/nngs/gnugo-3.5.1-viking4-200309231039.sgf 139
327 owl_attack O18
#? [1 (O19|P19|Q19|Q17|N19)]

loadsgf games/nngs/juka-gnugo-3.5.1-200309161948.sgf
328 owl_attack R6
#? [1 (P1|T4)]
329 owl_defend R6
#? [1 T4]

loadsgf games/nngs/tommmal-gnugo-3.5.1-200310071202.sgf 78
330 owl_attack Q5
#? [1 R1]
331 owl_defend Q5
#? [1 (R1|S1|T2)]

# This is more of a semeai, but at least owl should not say that
# B19 is unattackable.
# See also reading:189.
loadsgf games/nngs/tommmal-gnugo-3.5.1-200310071202.sgf 194
332 owl_attack B19
#? [1 A14]

loadsgf games/nngs/tommmal-gnugo-3.5.1-200310140825.sgf 126
333 owl_defend C17
#? [0]

loadsgf games/nngs/joshj-gnugo-3.5.2gf1-200312171536.sgf 119
334 owl_attack F5
#? [1 (E7|F6)]

loadsgf games/nngs/joshj-gnugo-3.5.2gf1-200312171536.sgf 180
335 owl_defend K12
#? [1 O6]

loadsgf games/nngs/gnugo-3.5.2gf1-G28-200312282240.sgf 203
336 owl_attack M5
#? [1 (N2|O1)]*

# A problem with extremely enthusiastic guess_eye_space() estimation.
# Must be solved with "bulky eye" heuristic in compute_eyes_pessimistic().
loadsgf games/self_play/selfplay1.sgf 206
337 owl_attack E19
#? [1 C18]*

loadsgf games/owl50.sgf
338 owl_defend F18
#? [1 M16]*

loadsgf games/nngs/gnugo-3.5.2gf1-wingjk-200312301242.sgf 90
339 owl_defend O17
#? [0]

# Both dragons could be saved.
loadsgf games/kgs/20040507-GnuGoCVS-read.sgf 224
340 owl_defend B10
#? [1 A11]*
341 owl_defend G13
#? [1 K8]*

loadsgf games/kgs/yagr-r08ert.sgf 139
342 owl_attack S6
#? [(2|3) T2]*

loadsgf games/kgs/yagr-r08ert.sgf 140
343 owl_defend S6
#? [(2|3) T3]*

loadsgf games/disaster.sgf 18
344 owl_attack N3
#? [0]

loadsgf games/kgs/yagr-Mythenmetz.2.sgf 111
345 owl_defend O13
#? [1 N13]*
346 owl_defend Q13
#? [1 N13]*

loadsgf games/owl51.sgf
347 owl_defend G16
#? [1 D10]*

loadsgf games/owl52.sgf
348 owl_defend M11
#? [0]*

# Eye space with a cutting point. 
loadsgf games/olympiad2004/19x19/int-gnu.sgf 115
349 owl_attack T19
#? [1 R19]

loadsgf games/olympiad2004/19x19/gnu-int.sgf 175
350 owl_defend F11
#? [0]

# The problem is that the G19 string is not considered as part of an
# eyespace but as a string that can be tactically defended at F18.
# Unfortunately white F18 gives black a seki.
loadsgf games/scoring/score11.sgf
play white C15
play white A13
play white E14
351 owl_attack E16
#? [1 (PASS|E11|F19|G18)]

loadsgf games/scoring/score14.sgf
352 owl_attack B19
#? [0]*

loadsgf games/owl53.sgf
353 owl_attack G6
#? [0]

loadsgf games/scoring/score39.sgf
354 owl_defend Q13
#? [0]

# See also olympiad2004:123.
loadsgf games/olympiad2004/19x19/int-gnu.sgf 242
355 owl_attack Q3
#? [1 (K3|L3|L1|J2)]
356 owl_defend Q3
#? [1 L3]*

loadsgf games/owl54.sgf
357 owl_defend D3
#? [0]*

loadsgf games/scoring/score55.sgf
358 owl_defend Q18
#? [0]*

loadsgf games/scoring/score53.sgf
359 owl_defend P5
#? [0]*

loadsgf games/scoring/score43.sgf
play black O16
play black N17
360 owl_defend L18
#? [0]*
play white L14
361 owl_attack L18
#? [1 K16]*
362 owl_defend L18
#? [1 M16]

loadsgf games/kgs/GnuGoCVS-john.sgf 166
363 owl_defend F9
#? [1 (F5|G5|G6|H6)]*

loadsgf games/owl54.sgf
364 owl_attack E9
#? [1 (PASS|A7|D6|F8|F9)]*
365 owl_defend E9
#? [0]

loadsgf games/kgs/munchie-GNU.sgf 164
366 owl_attack N2
#? [0]*
367 owl_attack O5
#? [1 O4]*
368 owl_defend O5
#? [1 (O4|N3)]*
369 owl_attack N7
#? [1 (O4|N5)]
370 owl_defend N7
#? [1 O4]*

loadsgf games/kgs/PsyDuck-GNU.sgf 76
371 owl_attack K11
#? [1 M13]*

loadsgf games/kgs/GNU-higher.sgf 267
372 owl_defend A17
#? [1 D18]

# See also nngs2:580.
loadsgf games/nngs/gnugo-3.3.9-nailer-200210192319.sgf 152
373 owl_attack F18
#? [1 (N19|P19|M19|K19|H19|J14|J15)]
374 owl_defend F18
#? [1 K19]*

loadsgf games/kgs/Erro-TheGNUGo.sgf
375 owl_attack R10
#? [1 (S16|S17|S18)]*

# See also gunnar:73.
loadsgf games/kgs/GNU-ivanho.sgf 182
376 owl_defend K18
#? [2 M19]*
377 owl_defend H17
#? [3 M19]*

loadsgf games/kgs/GNU-tadf.sgf 98
378 owl_attack C5
#? [1 A2]*

# See also nngs4:730.
loadsgf games/nngs/GoFuN-gnugo-3.4-200308030626.sgf 194
379 owl_does_attack D18 E15
#? [0]*

loadsgf games/nando/auto024.sgf 77
play black N3
play white M2
play black O2
play white M4
play white N10
play white M10
play white L5
play white M6
380 owl_defend O7
#? [1 P4]*

# Black has easy local life with L10.
loadsgf games/kgs/GNU-ituyosi.sgf 167
381 owl_attack P9
#? [1 (L10|M11|M10)]*
382 owl_defend P9
#? [1 (L10|K7|K5|Q10)]*

# Persistent cache problem. B5 is not included in the active area.
loadsgf games/kgs/tournament8/GNU-AyaBot.sgf 16
383 owl_attack C3
#? [0]
loadsgf games/kgs/tournament8/GNU-AyaBot.sgf 18
384 owl_attack C3
#? [1 (A2|F2)]*

loadsgf games/kgs/TheGNUGo-grasshoper.sgf 288
385 owl_defend G14
#? [1 C11]*

loadsgf games/kgs/TheGNUGo-rcsteppat.sgf 138 
386 owl_defend O17
#? [1 P19]*

loadsgf games/owl55.sgf 20
387 owl_defend F4
#? [1 C3]*

loadsgf games/9x9-2.sgf 28
388 owl_attack C3
#? [1 A2]*

loadsgf games/9x9-3.sgf 37
389 owl_attack E3
#? [0]*

# See also trevord:790.
loadsgf games/trevor/auto/d32.sgf 240
390 owl_attack Q14
#? [3 P14]*
391 owl_defend Q14
#? [1 P14]
392 owl_does_defend T13 Q14
#? [2]*

# See also semeai:139,140.
loadsgf games/semeai/semeai21.sgf
393 owl_attack F9
#? [1 (E9|G7)]
394 owl_defend F9
#? [1 G9]*

# See also 9x9:640.
loadsgf games/cgos/25811.sgf 52
395 owl_attack E9
#? [3 (A7|B9)]*
396 owl_defend E9
#? [2 PASS]*
