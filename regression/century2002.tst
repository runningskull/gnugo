loadsgf games/century21-2002/golois-gnugo.sgf 35
10 reg_genmove black
#? [R15]

loadsgf games/century21-2002/golois-gnugo.sgf 69
20 reg_genmove black
#? [!A6]

# same bug as nngs2:160
loadsgf games/century21-2002/golois-gnugo.sgf 183
30 reg_genmove black
#? [D15|C15]

# B15 and D13 are both locally better. C13 fills a dame.
loadsgf games/century21-2002/golois-gnugo.sgf 239
35 restricted_genmove black B15 D13 C13
#? [B15|D13]

loadsgf games/century21-2002/goint-gnugo.sgf 37
40 reg_genmove black 
#? [J3]

loadsgf games/century21-2002/goint-gnugo.sgf 47
50 reg_genmove black 
#? [F7]*

loadsgf games/century21-2002/goint-gnugo.sgf 55
55 reg_genmove black 
#? [F7]*

loadsgf games/century21-2002/goint-gnugo.sgf 111
60 reg_genmove black 
#? [Q14]*

loadsgf games/century21-2002/goint-gnugo.sgf 119
70 reg_genmove black 
#? [J11|K11]*

loadsgf games/century21-2002/goint-gnugo.sgf 161
80 owl_attack D17
#? [1 C17]

# White can make seki in the upper left corner and preventing that is
# the largest move on the board. B17 is the most natural move and the
# one leaving the least aji. Except for the upper left corner, N9 is
# the biggest move.
loadsgf games/century21-2002/goint-gnugo.sgf 209
85 reg_genmove black 
#? [B17]*
86 restricted_genmove black N9 T15 A13 Q7 Q6 N10 N8 L13 G19 G15
#? [N9]

# A17 and A18 produce 10,000 year ko. But A17 is better
# since B has the option of making seki. B19 is equivalent
# to A18 since white gets the choice of ko.
loadsgf games/century21-2002/goint-gnugo.sgf 237
90 reg_genmove black
#? [A17]*

# A19 is horrible, dying in gote. Before that white couldn't kill without ko.
loadsgf games/century21-2002/goint-gnugo.sgf 247
95 reg_genmove black
#? [!A19]

# locally B7 is better
loadsgf games/century21-2002/manyfaces-gnugo.sgf 27
100 reg_genmove black
#? [!B6]

loadsgf games/century21-2002/manyfaces-gnugo.sgf 29
110 reg_genmove black
#? [C8]*

loadsgf games/century21-2002/manyfaces-gnugo.sgf 43
120 reg_genmove black
#? [L3]

loadsgf games/century21-2002/manyfaces-gnugo.sgf 45
130 reg_genmove black
#? [N4]

loadsgf games/century21-2002/manyfaces-gnugo.sgf 57
140 reg_genmove black
#? [J3|R4|P5]

# B18 is better than A18 by leaving one less ko threat behind. At
# this time we accept both moves.
loadsgf games/century21-2002/manyfaces-gnugo.sgf 157
150 reg_genmove black
#? [B18]

# avoid shortage of liberties
loadsgf games/century21-2002/manyfaces-gnugo.sgf 177
160 reg_genmove black
#? [B7]

# Does not work because of white J8.
loadsgf games/century21-2002/manyfaces-gnugo.sgf 235
170 owl_does_defend K10 H8
#? [0]*

# prevent the seki
loadsgf games/century21-2002/manyfaces-gnugo.sgf 253
180 reg_genmove black
#? [T9]

loadsgf games/century21-2002/gnugo-katsunari.sgf 26
190 reg_genmove white
#? [D3]*

loadsgf games/century21-2002/gnugo-katsunari.sgf 36
200 reg_genmove white
#? [F6]

loadsgf games/century21-2002/gnugo-katsunari.sgf 58
210 restricted_genmove white A15 C16
#? [C16]

# a move in the center is bigger than this monkey jump
loadsgf games/century21-2002/gnugo-katsunari.sgf 76
220 reg_genmove white
#? [!T3]

loadsgf games/century21-2002/gnugo-katsunari.sgf 96
240 reg_genmove white
#? [F18]*

loadsgf games/century21-2002/gnugo-katsunari.sgf 156
260 reg_genmove white
#? [S12|Q12|Q13]
261 owl_defend J12
#? [0]

loadsgf games/century21-2002/gnugo-katsunari.sgf 202
270 reg_genmove white
#? [O7]
