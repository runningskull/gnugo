
# M3 is overplay
loadsgf games/nngs/bconwil-gnugo-3.3.11-200211182313.sgf 18
10 reg_genmove black
#? [G3|F4]

# This is of course too hard for GNU Go at the moment.
loadsgf games/nngs/bconwil-gnugo-3.3.11-200211182313.sgf 20
20 reg_genmove black
#? [G3|H3]*

loadsgf games/nngs/gnugo-3.3.12-RikiTiki-200212042341.sgf 36
30 reg_genmove white
#? [K3|J4|M5]

# Definitely not T12 as in the game. /ab
loadsgf games/nngs/gnugo-3.3.12-RikiTiki-200212042341.sgf 50
40 reg_genmove white
#? [R10|S10|P11]*

loadsgf games/nngs/gnugo-3.3.12-RikiTiki-200212042341.sgf 52
50 restricted_genmove white S14 T13
#? [S14]

# The cut at R14 is not dangerous. /ab
loadsgf games/nngs/gnugo-3.3.12-RikiTiki-200212042341.sgf 110
60 reg_genmove white
#? [N8|M7|L7|M9|L9|N7|O9|N10]

loadsgf games/nngs/gnugo-3.3.12-RikiTiki-200212042341.sgf 112
70 reg_genmove white
#? [N7|O9]

# Only move.
loadsgf games/nngs/gnugo-3.3.16-ccwills-200302151826.sgf 74
80 reg_genmove white
#? [L2]

loadsgf games/paul.sgf 89
90 restricted_genmove white P9 O10
#? [O10]

loadsgf games/paul.sgf 91
100 restricted_genmove white O16 O15
#? [O16]

loadsgf games/nngs/gnugo-3.3.18-agh-200304291749.sgf 64
110 restricted_genmove white T7 T8 R8
#? [R8]

loadsgf games/nngs/gnugo-3.3.18-joshj-200304242104.sgf 58
120 restricted_genmove white G19 H17 J17
#? [H17|J17]

# See also arend:24
loadsgf games/arend/gnugo-gnugo6.sgf 20
130 restricted_genmove white P17 G16 F16 E15
#? [P17]

loadsgf games/nngs/guestu-gnugo-3.3.19-200305131943.sgf 39
140 reg_genmove black
#? [D11|H10|G9|E10]*

loadsgf games/nngs/guestu-gnugo-3.3.19-200305131943.sgf 187
150 reg_genmove black
#? [D6|C6]

loadsgf games/nngs/GoFuN-gnugo-3.3.21-200306202102.sgf 6
160 reg_genmove black
#? [O16]

loadsgf games/nngs/GoFuN-gnugo-3.3.21-200306202102.sgf 12
170 reg_genmove black
#? [O14]*

loadsgf games/nngs/GoFuN-gnugo-3.3.21-200306202102.sgf 26
180 restricted_genmove black L17 L18
#? [L17]*

loadsgf games/nngs/GoFuN-gnugo-3.3.21-200306202102.sgf 40
190 reg_genmove black
#? [O1|P1|Q1]*

# Q5 is clearly better, but R5 is still a lot better than everything else.
loadsgf games/nngs/GoFuN-gnugo-3.3.21-200306202102.sgf 44
200 reg_genmove black
#? [Q5|R5]*

# Cut off the white group!
loadsgf games/nngs/GoFuN-gnugo-3.3.21-200306202102.sgf 106
210 reg_genmove black
#? [K2]*

# K11 should kill the white dragon. L11 might kill, too, so we accept it,
# although K11 is better.
loadsgf games/nngs/GoFuN-gnugo-3.3.21-200306202102.sgf 152
220 reg_genmove black
#? [K11|L11]*

loadsgf games/nngs/GoFuN-gnugo-3.3.21-200306202102.sgf 158
230 restricted_genmove black G14 H14 K11 L11
#? [!H14]

loadsgf games/nngs/GoFuN-gnugo-3.3.21-200306202102.sgf 208
240 reg_genmove black
#? [N7]*

loadsgf games/kgs/TheGNUGo-Botkiller3.sgf 165
250 restricted_genmove white D5 E5 D6
#? [D5]*

loadsgf games/kgs/simat-TheGNUGo.sgf 19
260 reg_genmove black
#? [E14]*

loadsgf games/kgs/TheGNUGo-qsdf.sgf 83
270 restricted_genmove white A11 A12 B8
#? [A11|A12]*

loadsgf games/hamete_joseki.sgf 37
280 restricted_genmove black B5 P17 P16 Q12 E6 M3
#? [B5]*

loadsgf games/hamete_joseki.sgf 51
290 reg_genmove black
#? [C5]*

loadsgf games/9x9-1.sgf 23
300 restricted_genmove black D8 E8 F8
#? [D8]*
