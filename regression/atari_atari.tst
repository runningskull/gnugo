# N3 is ineffective
loadsgf games/incident107.sgf 216
1 combination_attack black
#? [N1]

loadsgf games/atari_atari01.sgf 235
2 combination_attack black
#? [PASS]

loadsgf games/arb/game02.sgf 69
3 combination_attack black
#? [PASS]

loadsgf games/incident156.sgf 232
4 combination_attack white
#? [K17]

# K12 doesn't work.
loadsgf games/ego.sgf 246
5 combination_attack black
#? [PASS]

# E8 is bogus
loadsgf games/endgame1.sgf 3
6 combination_attack black
#? [PASS]

# D11 can be answered at C12.
loadsgf games/scoring/score30.sgf
7 combination_attack black
#? [PASS]

loadsgf games/atari_atari02.sgf 140
8 combination_attack black
#? [PASS]

loadsgf games/atari_atari03.sgf 222
9 combination_attack white
#? [A11]

# Both G5 and J4 capture (at least) the G6 string. After endgame
# considerations, J4 turns out to be one point bigger.
loadsgf games/atari_atari04.sgf 279
10 combination_attack white
#? [J4|G5]
