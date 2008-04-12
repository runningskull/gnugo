# Q6 is too slow.
loadsgf games/gifu2005/gnugo-katsu.sgf 24
1010 reg_genmove white
#? [C14|C10|C6|D6|F3|F17|R11]*

loadsgf games/gifu2005/gnugo-katsu.sgf 34
1030 restricted_genmove white C10 C12
#? [C10]*

loadsgf games/gifu2005/gnugo-katsu.sgf 36
1040 reg_genmove white
#? [E12|D11]*

# Q12 is urgent. (Posed as restricted_genmove because a move around C12
# is arguable more urgent.)
loadsgf games/gifu2005/gnugo-katsu.sgf 42
1050 restricted_genmove white Q12 E2
#? [Q12]*

loadsgf games/gifu2005/gnugo-katsu.sgf 44
1060 restricted_genmove white C6 E12 D14 D11
#? [E12|D11|D14]*

loadsgf games/gifu2005/gnugo-katsu.sgf 60
1070 reg_genmove white
#? [G12]*

loadsgf games/gifu2005/gnugo-katsu.sgf 62
1080 reg_genmove white
#? [O5]*

loadsgf games/gifu2005/gnugo-katsu.sgf 108
1090 reg_genmove white
#? [M9]*

loadsgf games/gifu2005/gnugo-katsu.sgf 110
1100 reg_genmove white
#? [N9]*

loadsgf games/gifu2005/gnugo-katsu.sgf 132
1110 reg_genmove white
#? [L5]

loadsgf games/gifu2005/gnugo-katsu.sgf 150
1120 reg_genmove white
#? [O2]

#Sente Seki.
loadsgf games/gifu2005/gnugo-katsu.sgf 224
1130 reg_genmove white
#? [S18]*

# Three points in sente.
loadsgf games/gifu2005/gnugo-katsu.sgf 238
1140 restricted_genmove white G8 G9 H12 E5
#? [G8]

loadsgf games/gifu2005/gnugo-katsu.sgf 254
play black S19
1150 reg_genmove white
#? [T14|P12|Q12]*

# Nothing bad can happen in the upper left corner. The B16 cut leads
# nowhere.
# See also reading:233.
loadsgf games/gifu2005/mfg-gnugo.sgf 195
1200 restricted_genmove black B19 B18 B16 C16
#? [C16]*

# Double threat to cut at E17. See also tactics1:108 regarding O6.
loadsgf games/gifu2005/mfg-gnugo.sgf 204
1210 reg_genmove white
#? [E17]*

# Double threat to cut at E17.
loadsgf games/gifu2005/mfg-gnugo.sgf 205
1220 reg_genmove black
#? [E17]

# M6 is worth two points gote.
loadsgf games/gifu2005/mfg-gnugo.sgf 259
1230 reg_genmove black
#? [M6]*
