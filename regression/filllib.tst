# incident 72
# A15 and A16 are ko threats. These are of no use but don't hurt the
# position either. filllib shouldn't play these but they may get
# played as owl_threaten_attack moves. We include them here to avoid
# false alarms.
loadsgf games/incident72.sgf 255
1 reg_genmove black
#? [T15|T13|T10|T8|T5|T1|Q1|N1|A15|A16]
2 reg_genmove white
#? [A14|A10|A8|A4|A1|E1|G1|J1]

# incident 187
loadsgf games/incident187.sgf 227
3 reg_genmove white
#? [B15|A15|A17|B14]
4 reg_genmove black
#? [B19|A17]*

# incident 216
loadsgf games/incident211.sgf 64
5 reg_genmove black
#? [PASS]
6 reg_genmove white
#? [PASS]

# incident 296
loadsgf games/incident291.sgf 274
7 reg_genmove black
#? [PASS]
8 reg_genmove white
#? [PASS]

loadsgf games/filllib1.sgf 54
9 reg_genmove white
#? [PASS]
10 reg_genmove black
#? [PASS]

loadsgf games/filllib1.sgf 58
11 reg_genmove white
#? [PASS]
12 reg_genmove black
#? [PASS]

loadsgf games/filllib1.sgf 60
13 reg_genmove white
#? [PASS]
14 reg_genmove black
#? [PASS]

loadsgf games/filllib2.sgf 312
15 reg_genmove white
#? [P19]
16 reg_genmove black
#? [R19]

loadsgf games/filllib3.sgf 105
17 reg_genmove white
#? [B11|A13]
18 reg_genmove black
#? [A10]

loadsgf games/filllib4.sgf 252
19 reg_genmove white
#? [A13]
20 reg_genmove black
#? [A13]

# White A7 may give a point if there are sufficient ko threats. The
# variation W A7, B A6, W D8, B A8, W B9, B A7 does not lose a point.
loadsgf games/filllib5.sgf 52
21 reg_genmove white
#? [A7|D9|B9]
22 reg_genmove black
#? [A7]

loadsgf games/filllib6.sgf 261
23 reg_genmove white
#? [P16]
24 reg_genmove black
#? [P16]

# White J3 is best but with superior ko threats, H1 and J5 can also be
# played with the same result.
loadsgf games/filllib7.sgf 72
25 reg_genmove white
#? [J3|H1|J5]
26 reg_genmove black
#? [J5|H7|J7]

loadsgf games/filllib8.sgf 303
27 reg_genmove white
#? [Q16]
28 reg_genmove black
#? [R14]

# It's not a very good idea to throw in to start a ko, but we accept
# it anyway.
loadsgf games/filllib9.sgf 235
29 reg_genmove white
#? [B1|A1]
30 reg_genmove black
#? [A3|A1|B1]

# Q19 for white is non-orthodox but doesn't lose points.
loadsgf games/trevor/auto/d29.sgf 188
31 reg_genmove white
#? [S16|T17|O18|Q19]
32 reg_genmove black
#? [O18|O19|N18|P19|N19]

loadsgf games/filllib10.sgf
33 reg_genmove white
#? [PASS]
34 reg_genmove black
#? [PASS]

loadsgf games/nngs/speciman-gnugo-3.1.32-200204211014.sgf 53
35 reg_genmove black
#? [A8|B8]*
36 reg_genmove white
#? [A5|A8|B8]

loadsgf games/filllib11.sgf
37 reg_genmove white
#? [H5]
38 reg_genmove black
#? [E4]

loadsgf games/nngs/gnugo-3.1.34-guest1-200204242025.sgf 86
39 reg_genmove white
#? [G12|F12|G13]
40 reg_genmove black
#? [K13]

loadsgf games/nngs/gnugo-3.2-merlin-200205071828.sgf 244
41 reg_genmove white
#? [M8]

loadsgf games/nngs/gnugo-3.3.11-rcde05-200211090008.sgf 271
42 reg_genmove white
#? [N19]

loadsgf games/filllib12.sgf
43 reg_genmove black
#? [C6]

loadsgf games/filllib13.sgf
44 reg_genmove black
#? [F4]

#Moved from 9x9:240
loadsgf games/nngs/evand-gnugo-3.5.2gf1-200312150903.sgf
45 reg_genmove black
#? [G9]

loadsgf games/filllib14.sgf
46 reg_genmove white
#? [N3|H5|M1]*

# L1 loses one point since we still need to play K2 and one of L2 and J1.
loadsgf games/filllib15.sgf
47 reg_genmove black
#? [L2|K2|J1]

# F1 is an unsafe backfilling move for J1 and must be preceded by e.g. E2.
loadsgf games/filllib16.sgf
48 reg_genmove white
#? [B4|E2|B2]*

# The liberty filling move at B16 is a blunder but can be played after
# either of C18 and E18.
loadsgf games/filllib17.sgf
49 reg_genmove white
#? [C18|E18]*

# Black must eliminate white's seki potential so that L3 can be played.
loadsgf games/filllib18.sgf
50 reg_genmove black
#? [T2|R2|S1]*

# GNU Go 3.7.9  tries an illegal liberty filling move at J4.
loadsgf games/filllib19.sgf
51 reg_genmove black
#? [D6|B2]
