loadsgf games/kgs/yagr.sgf 204
10 reg_genmove black
#? [A11]*

loadsgf games/kgs/yagr.sgf 206
20 reg_genmove black
#? [C10]*

loadsgf games/kgs/yagr2.sgf 65
30 restricted_genmove black N19 P19 L18 P18 P17 O17
#? [L18]

loadsgf games/kgs/yagr3.sgf 78
40 restricted_genmove white A7 A6
#? [A6]

loadsgf games/kgs/yagr4.sgf 38
50 reg_genmove black
#? [D15]*

loadsgf games/kgs/yagr4.sgf 184
60 restricted_genmove black J4 J8
#? [J8]

# Given the stage of the game, B2 is huge.
loadsgf games/kgs/yagr5.sgf 153
70 reg_genmove white
#? [B2]

# Black could win the semeai, white must defend
loadsgf games/kgs/20040507-GnuGoCVS-read.sgf 212
80 reg_genmove white
#? [L1|O2|Q1]*

# T8 only move. T7 only gives ko, but the owl code is missing
# it, because the tactical reading is proposing a bogus lunch
# defense (the self atari at T12 also results in a ko for the
# tactical reading routines, but fails in the owl context)
loadsgf games/kgs/20040507-GnuGoCVS-read.sgf 268
90 reg_genmove white
#? [T8]

# Semeai related problem. The status of D3 is revised to ALIVE
# because black can't win the semeai C3 vs. D3 (both live after
# B:C4 W:C1). The owl attack at C1 is then discarded, since the
# dragon isn't critical.
loadsgf games/kgs/20040516-GoBucks-GnuGoCVS.sgf 242
100 reg_genmove black
#? [C1]*

loadsgf games/kgs/20040517-siRNA-GnuGoCVS.sgf 152
110 reg_genmove black
#? [G13]

# P18 is in danger now.
loadsgf games/kgs/20040517-siRNA-GnuGoCVS.sgf 192
120 reg_genmove black
#? [S18]*

# White missed, P18 can still be saved.
loadsgf games/kgs/20040517-siRNA-GnuGoCVS.sgf 198
130 reg_genmove black
#? [S18]

# Awful tenuki.
loadsgf games/kgs/2004-04-10-GnuGoCVS-inuyasha34.sgf 56
140 reg_genmove white
#? [P14|N15|G12]*

# Another tenuki from same fight - lots of cutting points are opportunities
# for escape.
loadsgf games/kgs/2004-04-10-GnuGoCVS-inuyasha34.sgf 64
150 reg_genmove white
#? [M14|Q13]*

# Later, ignores a fight and misreads a ladder - anything related to the
# fight or the ladder is better than the move made in the game.
loadsgf games/kgs/2004-04-10-GnuGoCVS-inuyasha34.sgf 94
160 reg_genmove white
#? [G12|H8|J7|G7]

# Again the move made in the game doesn't help the fight in the slightest
# nor resolve the ladder.
loadsgf games/kgs/2004-04-10-GnuGoCVS-inuyasha34.sgf 96
170 reg_genmove white
#? [G12|H8|G7]*

loadsgf games/kgs/geluba-yagr.sgf 164
180 reg_genmove black
#? [C4|C3|D3]

# Only way to control damage: sacrifice the three stones at E3
loadsgf games/kgs/geluba-yagr.sgf 166
190 reg_genmove black
#? [C4]*

# Don't kill your own group.
loadsgf games/kgs/yagr-ituyosi.sgf 160
200 restricted_genmove white K4 M4 L4
#? [M4|L4]*

# An "owl attack captures tail" problem
loadsgf games/kgs/yagr-gab9.2.sgf 158
210 reg_genmove white
#? [Q13]*

# Connect the monkey jump instead of running nowhere!
loadsgf games/kgs/yagr-Sapojnik.sgf 80
220 reg_genmove white
#? [T10]*

# Win the semeai!
loadsgf games/kgs/yagr-Kazik.sgf 149
230 reg_genmove white
#? [J12|J11|L10|L9|J19]*

loadsgf games/kgs/yagr-yudeta.sgf 257
240 reg_genmove white
#? [B7]

# This should be a different game, but I don't know which one.
## In the game, GNU Go ran on the first line until the very end :((
#loadsgf games/kgs/yagr-yudeta.sgf 78
#250 reg_genmove white
##? [P1]*

# Huge semeai.
loadsgf games/kgs/malitourne-yagr.sgf 240
260 reg_genmove black
#? [T10|Q11|O11]*

# Same problem as test 250.
loadsgf games/kgs/yagr-Rayden.sgf 80
270 restricted_genmove white O1 T1
#? [T1]*

loadsgf games/kgs/yagr-hasenhirn.sgf 71
280 reg_genmove white
#? [G1]*

# A ladder desaster.
loadsgf games/kgs/yagr-czarny.sgf 16
290 reg_genmove white
#? [B2]

loadsgf games/kgs/yagr-czarny.sgf 24
300 restricted_genmove white H4 H5
#? [H4]*

# N15 as played in the game is off-topic.
loadsgf games/kgs/yagr-awf.sgf 191
310 reg_genmove white
#? [F11|H10|J15|K12|F19|D18]

# Probably owl cut problem. At least have to try to win the semeai.
loadsgf games/kgs/yagr-Mythenmetz.sgf 156
320 reg_genmove white
#? [K12]*

loadsgf games/kgs/yagr-FKleolio.sgf 211
330 reg_genmove white
#? [H1]*

loadsgf games/kgs/yagr-nigiri.sgf 121
340 restricted_genmove white L2 L4
#? [L4]

# Not L8
loadsgf games/kgs/SURARIN-yagr.sgf 218
350 reg_genmove black
#? [L7|K7]*

loadsgf games/kgs/yagr-digibier.sgf 218
360 reg_genmove white
#? [B18]

loadsgf games/kgs/yagr-justre1.sgf 133
370 restricted_genmove white H11 G12
#? [G12]*

# I think P9 as played in the game is ok. /ab
#loadsgf games/kgs/evand-TheGNUGo.sgf 8
#380 reg_genmove black
##? [P8|P12|Q12]

loadsgf games/kgs/evand-TheGNUGo.sgf 10
390 reg_genmove black
#? [P8|P7]*

#N12 is really bad
loadsgf games/kgs/evand-TheGNUGo.sgf 12
400 reg_genmove black
#? [P8|P7|Q13]

#cut and fight somewhere
loadsgf games/kgs/evand-TheGNUGo.sgf 20
410 reg_genmove black
#? [O8|O11|P11]*

loadsgf games/kgs/evand-TheGNUGo.sgf 36
420 owl_attack R14
#? [0]*

#The M9-O5 exchange is bad
loadsgf games/kgs/evand-TheGNUGo.sgf 70
430 reg_genmove black
#? [K18]

#loadsgf games/kgs/evand-TheGNUGo.sgf 80
#440 reg_genmove black
##? [R18]

loadsgf games/kgs/evand-TheGNUGo.sgf 84
450 reg_genmove black
#? [L15]

#fight the ko
loadsgf games/kgs/evand-TheGNUGo.sgf 98
460 reg_genmove black
#? [M19]*

loadsgf games/kgs/evand-TheGNUGo.sgf 178
470 reg_genmove black
#? [G13]*

#block somehow
loadsgf games/kgs/evand-TheGNUGo.sgf 190
480 reg_genmove black
#? [H9|G9|G8]*

loadsgf games/kgs/evand-TheGNUGo.sgf 242
490 reg_genmove black
#? [H13]

loadsgf games/kgs/evand-TheGNUGo.sgf 270
# Removed P13, too short on liberties to play that.  /pp
500 reg_genmove black
#? [O15]

# P8 seems the most reasonable move here.
loadsgf games/kgs/20040525-fanfan-GnuGoBot.sgf 16
510 reg_genmove black
#? [P8]*

# R9 makes life.
loadsgf games/kgs/20040525-fanfan-GnuGoBot.sgf 34
520 reg_genmove black
#? [R9]

# Black has at least the Q6 threat and should fight the ko.
loadsgf games/kgs/20040525-fanfan-GnuGoBot.sgf 52
530 reg_genmove black
#? [P14]

# The game move A8 is funny. A stone at A5 or A11 would maybe
# have a chance to influence something around.
loadsgf games/kgs/20040525-fanfan-GnuGoBot.sgf 78
540 reg_genmove black
#? [A5|A11]*

# I believe there are better chances to save something with a
# move like Q2 which threatens to make an eye around Q3.
loadsgf games/kgs/20040525-fanfan-GnuGoBot.sgf 96
550 reg_genmove black
#? [Q2]*

# A classic, should be attempted.
loadsgf games/kgs/20040525-fanfan-GnuGoBot.sgf 126
560 reg_genmove black
#? [B1]*

#E6 is bad
loadsgf games/kgs/20050407-tfujii-GNU.sgf 21
570 reg_genmove black
#? [K17|N16|P9|R15|C4|C15|D16]*

loadsgf games/kgs/20050407-tfujii-GNU.sgf 27
580 reg_genmove black
#? [P16]*

loadsgf games/kgs/20050407-tfujii-GNU.sgf 37
590 reg_genmove black
#? [P16]*

loadsgf games/kgs/20050407-tfujii-GNU.sgf 51
600 reg_genmove black
#? [E15]*

loadsgf games/kgs/20050407-tfujii-GNU.sgf 71
610 reg_genmove black
#? [O15|N16]

loadsgf games/kgs/20050407-tfujii-GNU.sgf 73
620 reg_genmove black
#? [N16]*

loadsgf games/kgs/20050407-tfujii-GNU.sgf 133
630 reg_genmove black
#? [F10|G9]*

#I think B2 is at best ko, and GNU Go fails to continue next move
#GNU Go 3.7.3 doesn't seem to play this -- a result of Gunnar's
#experimental code?
loadsgf games/kgs/20050407-tfujii-GNU.sgf 153
640 reg_genmove black
#? [E2]

#G6 is bad.  B4 or D9 is probably best, C3 is at least an improvement
loadsgf games/kgs/20050408-wrf6041-GNU.sgf 11
650 reg_genmove black
#? [B4|D9|C3]*

#something local; N15 is bad
loadsgf games/kgs/20050408-wrf6041-GNU.sgf 27
660 reg_genmove black
#? [H16|J15|C13|E16]*

#M16 is bad
loadsgf games/kgs/20050408-wrf6041-GNU.sgf 33
670 reg_genmove black
#? [J15|C13|B4|C3|H4|S6|S5|G13]*

# D13 clearly best but C12 acceptable. /ab
loadsgf games/kgs/20050408-GNU-mamassang.sgf 30
680 reg_genmove white
#? [D13|C12]*

loadsgf games/kgs/20050408-GNU-mamassang.sgf 34
690 restricted_genmove white K9 R11
#? [R11]*

loadsgf games/kgs/20050408-GNU-mamassang.sgf 46
700 reg_genmove white
#? [C10|D10|M3|M4|K3|R3]*

loadsgf games/kgs/20050408-GNU-mamassang.sgf 80
710 reg_genmove white
#? [B7]*

loadsgf games/kgs/20050408-GNU-mamassang.sgf 160
720 reg_genmove white
#? [A15]

# This requires knowledge that we have no ko threats. Tough to solve, but
# important. /ab
loadsgf games/kgs/20050408-janine-GNU.sgf 16
730 reg_genmove black
#? [R16]*

loadsgf games/kgs/20050408-janine-GNU.sgf 28
740 restricted_genmove black F3 F4
#? [F4]*

#H6 should at least get the J6-5 stones out
loadsgf games/kgs/20050408-janine-GNU.sgf 90
750 reg_genmove black
#? [H6|K7]*

#W S10 is big
loadsgf games/kgs/20050408-janine-GNU.sgf 230
760 reg_genmove black
#? [S10|R10]*

