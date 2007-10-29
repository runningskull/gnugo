loadsgf games/nngs/Temnik-gnugo-3.3.15-200301161937.sgf 220
1 reg_genmove black
#? [Q7|Q6|O7|O6]

loadsgf games/nngs/Temnik-gnugo-3.3.15-200301161937.sgf 222
2 reg_genmove black
#? [Q7|Q6|O7|O5]

loadsgf games/nngs/Temnik-gnugo-3.3.15-200301161937.sgf 228
3 reg_genmove black
#? [O5|N5|N4]

loadsgf games/nngs/Temnik-gnugo-3.3.15-200301161937.sgf 230
4 reg_genmove black
#? [N4|O4|M4]*

loadsgf games/nngs/Temnik-gnugo-3.3.15-200301161937.sgf 232
5 reg_genmove black
#? [M3|M2|N3|L4|M5|M4]*

# Thrashing dragon in semeai (liberty count is close!)
loadsgf games/nngs/uno-gnugo-3.3.16-200302050206.sgf 204
6 reg_genmove black
#? [T4|M5|O4|O2|P1]

# D14 is way more solid than C13. Other moves are conceivable.
loadsgf games/nngs/tommmal-gnugo-3.5.1-200310140825.sgf 78
7 reg_genmove black
#? [D14]

# H10 effectively destroys white's eye shape and development
# potential. The game move at N4 has almost no effect on the thrashing
# dragon.
loadsgf games/nngs/tommmal-gnugo-3.5.1-200310140825.sgf 140
8 reg_genmove black
#? [H10]

# The N19 dragon should possibly be critical.
loadsgf games/nngs/ccwills-gnugo-3.5.2-200312112026.sgf 95
9 reg_genmove black
#? [N12|N11]*

# Don't let the dragon get out. It's in fact probably critical.
loadsgf games/nngs/ccwills-gnugo-3.5.2-200312112026.sgf 125
10 reg_genmove black
#? [J15]

# White can't be allowed to thrash in the corner. (Clearly critical dragon.)
loadsgf games/nngs/ccwills-gnugo-3.5.2-200312112026.sgf 173
11 reg_genmove black
#? [S3]

# H18 is too far to have any effect on the thrashing dragon.
loadsgf games/nngs/gnugo-3.5.4-chpr-200403201748.sgf 204
12 reg_genmove white
#? [K10]*

# G10 is totally inappropriate. F10 is of course locally better but
# the natural constraining move is P11.
loadsgf games/nngs/gnugo-3.5.4-chpr-200403201748.sgf 210
13 reg_genmove white
#? [P11]

# N17 more constraining than L18.
loadsgf games/nngs/gnugo-3.5.4-chpr-200403201748.sgf 274
14 reg_genmove white
#? [N17]*

# More valuable to connect P17 and O15.
loadsgf games/nngs/gnugo-3.5.4-chpr-200403201748.sgf 278
15 reg_genmove white
#? [P16]*

#restrain the dragon
loadsgf games/nngs/gnugo-3.5.5-liza-200404251750.sgf 42
16 reg_genmove white
#? [B8|B9|B10|C8|C9]*

# P19 as in the game is not so attractive. Better to first isolate 
# the dragon completely. 
loadsgf games/kgs/2004-04-10-Shun9137-GnuGoCVS.sgf 243
17 reg_genmove black
#? [L19|K19]*

# GNU Go 3.7.9 tries an illegal, supposedly liberty filling, move at A1.
loadsgf games/cgos/60311.sgf
18 reg_genmove black
#? [C4|A3|A5]

# GNU Go 3.7.9 tries a simple ko violation at C9.
loadsgf games/cgos/58273.sgf
19 reg_genmove black
#? [J9|F9]

# Don't play send-two-return-one with F4, especially not when positional
# superko is used.
# See also reading:228.
loadsgf games/cgos/879.sgf 68
20 reg_genmove white
#? [A9|A8|F2]

# Don't play into ko needlessly with E1.
loadsgf games/cgos/30527.sgf 71
21 reg_genmove black
#? [A3|D1|G9|C5|C6|D8|E7|F5|F4]

# Don't play send-two-return-one with J9, especially not when positional
# superko is used.
loadsgf games/cgos/30527.sgf 73
22 restricted_genmove black G9 J9
#? [G9]

# The ko at J6 is an irrelevant distration. Start with filling the
# safe outer liberties.
loadsgf games/cgos/994.sgf 76
23 reg_genmove white
#? [A5|D7]

# Fill the outer ko at J7 first and there's nothing black can do.
loadsgf games/cgos/994.sgf 78
24 restricted_genmove white J1 J7
#? [J7]*

loadsgf games/cgos/13435.sgf 74
25 reg_genmove white
#? [A5|C7]

loadsgf games/cgos/31446.sgf 88
26 reg_genmove white
#? [E1|A1]*

# Fill a ko before taking another one.
loadsgf games/cgos/37169.sgf 86
27 reg_genmove white
#? [J6]*
loadsgf games/cgos/37169.sgf 88
28 reg_genmove white
#? [A2]*
loadsgf games/cgos/37169.sgf 90
29 reg_genmove white
#? [D3]*

loadsgf games/cgos/26449.sgf 47
30 reg_genmove black
#? [J4|J7]

