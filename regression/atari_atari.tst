# N3 is ineffective. N2 and N1 differ in that the latter move gives
# white the option to defend with a (disadvantageous) ko. L3 and K2
# are also effective but miss the point and waste good ko threats.
loadsgf games/incident107.sgf 216
1 combination_attack black
#? [N2|N1|L3|K2]

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

# Retired as endgame1.sgf is no longer in CVS. /ab
# # E8 is bogus
# loadsgf games/endgame1.sgf 3
# 6 combination_attack black
# #? [PASS]

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

loadsgf games/viking2.sgf 140
11 combination_attack white
#? [M14]

loadsgf games/viking2.sgf 146
12 combination_attack white
#? [N13|Q12]

loadsgf games/trevor/auto/c24.sgf 62
13 combination_attack black
#? [D8]

loadsgf games/nngs/gnugo-3.1.18-gopriest-200201072104.sgf 104
14 combination_attack black
#? [PASS]

loadsgf games/atari_atari05.sgf 291
15 combination_attack black
#? [B13]

loadsgf games/atari_atari06.sgf
16 combination_attack white
#? [K3]

loadsgf games/nngs/Lazarus-gnugo-3.1.34-200204280120.sgf 194
17 combination_attack white
#? [S10]

loadsgf games/nngs/gnugo-3.3.12-guest-200212171626.sgf 148
18 combination_attack black
#? [R11|R14]

loadsgf games/nngs/gnugo-3.3.10-deye-200210211347.sgf 135
19 combination_attack black
#? [P9|Q7]

# See also nando:25
loadsgf games/nando/auto017.sgf
white P16
20 combination_attack black
#? [L17]*

loadsgf games/nngs/leftd-gnugo-3.3.16-200302252226.sgf 256
21 combination_attack black
#? [Q9|N13|O13]

loadsgf games/nngs/gnugo-3.3.16-ccwills-200303030550.sgf
22 combination_attack black
#? [L10]

loadsgf games/atari_atari07.sgf
23 combination_attack black
#? [C8]

loadsgf games/atari_atari08.sgf 227
24 combination_defend black
#? [!.*(K16|A12|R12|S12|T12|A11|B11|C11|R11|S11|T11|A10|C10|Q10|S10|D9).*]*

# T14 is a blunder.  GNU Go should know how to punish it.
loadsgf games/atari_atari09.sgf
play black T14
25 combination_attack white
#? [R16]

loadsgf games/self_play/354-34-2.sgf 150
26 combination_defend white
#? [E11|E12]*

loadsgf games/filllib14.sgf
play white H5
27 combination_attack black
#? [0]*

# See also nngs:1060. Black has no combination attack at R3.
loadsgf games/nngs/gnugo-3.1.18-goku-200201042350.sgf 52
28 combination_attack black
#? [0]*

loadsgf games/atari_atari10.sgf
29 combination_attack white
#? [K5]
