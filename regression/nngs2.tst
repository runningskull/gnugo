# See also reading:173
loadsgf games/nngs/gnugo-3.3.2-Zahlman-200205221717.sgf 207
1 reg_genmove white
#? [B1|A2]

loadsgf games/nngs/Jion-gnugo-3.3.2-200205241928.sgf 22
10 reg_genmove black
#? [!H6]

loadsgf games/nngs/Jion-gnugo-3.3.2-200205241928.sgf 96
20 reg_genmove black
#? [H11]*

# K9 is harmful. After B:M16, W cannot kill J11 since E17 and N12 are miai
loadsgf games/nngs/Jion-gnugo-3.3.2-200205241928.sgf 112
30 reg_genmove black
#? [L16|M16]*

loadsgf games/nngs/Jion-gnugo-3.3.2-200205241928.sgf 158
40 reg_genmove black
#? [!K18]

# semeai problem
loadsgf games/nngs/Jion-gnugo-3.3.2-200205241928.sgf 162
50 reg_genmove black
#? [K2]

# locally O1 is better
loadsgf games/nngs/Jion-gnugo-3.3.2-200205241928.sgf 166
60 reg_genmove black
#? [!P1]

loadsgf games/nngs/Jion-gnugo-3.3.2-200205241928.sgf 176
70 reg_genmove black
#? [!L18]

# semeai problem
loadsgf games/nngs/tobyiii-gnugo-3.3.2-200206041432.sgf 162
80 reg_genmove black
#? [O4|P1]*

#CATEGORY=BLUNDER (incorrectly rejected move)
loadsgf games/nngs/tobyiii-gnugo-3.3.2-200206041432.sgf 166
90 reg_genmove black
#? [N1]

loadsgf games/nngs/lindq-gnugo-3.3.4-200207051636.sgf 214
100 reg_genmove black
#? [H5]

loadsgf games/nngs/silverado-gnugo-3.3.5-200208061810.sgf 34
110 reg_genmove black
#? [B14|B13]

loadsgf games/nngs/silverado-gnugo-3.3.5-200208061810.sgf 42
120 reg_genmove black
#? [E12|D13]

# See also connection:93
loadsgf games/nngs/silverado-gnugo-3.3.5-200208061810.sgf 100
130 reg_genmove black
#? [S13]*

loadsgf games/nngs/silverado-gnugo-3.3.5-200208061810.sgf 130
140 restricted_genmove black O5 P5 Q5 Q6
#? [P5]*

loadsgf games/nngs/silverado-gnugo-3.3.5-200208061810.sgf 138
150 reg_genmove black
#? [M3|L3]*

# same bug as century-2002:30
loadsgf games/nngs/gnugo-3.3.2-Programmer-200206021522.sgf 52
160 reg_genmove white
#? [J14]

# R9 is clearly worse than Q9.
loadsgf games/nngs/kumu-gnugo-3.2-200205070903.sgf 116
170 reg_genmove black
#? [Q9]*

# G17 is clearly worse than G16.
loadsgf games/nngs/gnugo-3.3.2-mr23-200205120953.sgf 132
180 reg_genmove white
#? [G16]

loadsgf games/nngs/joshj-gnugo-3.3.2-200205310709.sgf 86
190 reg_genmove black
#? [!S8]

loadsgf games/nngs/joshj-gnugo-3.3.2-200205310709.sgf 124
200 reg_genmove black
#? [G2]

loadsgf games/nngs/joshj-gnugo-3.3.2-200205310709.sgf 148
210 reg_genmove black
#? [F15]

loadsgf games/nngs/huck-gnugo-3.3.6-200209060837.sgf 54
220 reg_genmove black
#? [S9]*

loadsgf games/nngs/huck-gnugo-3.3.6-200209060837.sgf 158
230 reg_genmove black
#? [G17]*

loadsgf games/nngs/huck-gnugo-3.3.6-200209060837.sgf 160
240 reg_genmove black
#? [F17]*

# # T18 is not very big, maybe a few points. There are clearly bigger
# # points on the board. Removed.
# loadsgf games/nngs/huck-gnugo-3.3.6-200209060837.sgf 196
# 250 reg_genmove black
# #? [T18]*

# White can't kill or make seki but black may be forced to play
# several moves inside own territory to remove white's stones later.
loadsgf games/nngs/huck-gnugo-3.3.6-200209060837.sgf 204
260 reg_genmove black
#? [S18]*

loadsgf games/nngs/huck-gnugo-3.3.6-200209060837.sgf 206
270 reg_genmove black
#? [S19]

loadsgf games/nngs/huck-gnugo-3.3.6-200209060837.sgf 216
280 reg_genmove black
#? [!A13]*

# B16 is maybe not playable.
# C17 is in any case better than the game move C18.
loadsgf games/nngs/huck-gnugo-3.3.6-200209060837.sgf 220
290 reg_genmove black
#? [C17|B16]

# Hey, Q19 is dead and Q13 doesn't threaten anything.
loadsgf games/nngs/huck-gnugo-3.3.6-200209060837.sgf 232
300 reg_genmove black
#? [!Q13]

loadsgf games/nngs/huck-gnugo-3.3.6-200209060837.sgf 240
310 reg_genmove black
#? [M11]


# problems from gnugo-3.3.8-nailer-200209190045

# C10 is important enough to be saved, D10 only move.
loadsgf games/nngs/gnugo-3.3.8-nailer-200209190045.sgf 34
400 reg_genmove white
#? [D10]

# stones at C10 are critical, don't tenuki.
loadsgf games/nngs/gnugo-3.3.8-nailer-200209190045.sgf 42
410 reg_genmove white
#? [D7]

# don't get into the snap back!
loadsgf games/nngs/gnugo-3.3.8-nailer-200209190045.sgf 115
420 reg_genmove black
#? [M15|M13]

# missed vital snap back after black's mistake.
loadsgf games/nngs/gnugo-3.3.8-nailer-200209190045.sgf 116
430 reg_genmove white
#? [M11]

# all these moves should kill at least some of black stones
loadsgf games/nngs/gnugo-3.3.8-nailer-200209190045.sgf 144
440 reg_genmove white
#? [B18|C18|E16|E18|F16|F17]*

# prevent loss of four stones a couple of moves later. E16 doesn't
# look very good, but it works.
loadsgf games/nngs/gnugo-3.3.8-nailer-200209190045.sgf 170
450 reg_genmove white
#? [E14|D15|E16]*

# black is getting to deep, time to defend.
loadsgf games/nngs/gnugo-3.3.8-nailer-200209190045.sgf 196
460 reg_genmove white
#? [J4|K2|K3|K4|K5|L2]

#CATEGORY=SEMEAI
# K1 wins the semeai. Other moves let black get ko.
loadsgf games/nngs/gnugo-3.3.8-nailer-200209190045.sgf 212
470 reg_genmove white
#? [K1]*

# H4 only forces black to connect. H3 is locally more interesting, or
# a move to secure life.
loadsgf games/nngs/gnugo-3.3.8-viking4-200209250907.sgf 73
480 reg_genmove white
#? [!H4]

# R14 also helps black to connect. More interesting to play S14 or a
# monkey jump.
loadsgf games/nngs/gnugo-3.3.8-viking4-200209250907.sgf 87
490 reg_genmove white
#? [!R14]

# Bigger to kill upper right corner than C9.
loadsgf games/nngs/gnugo-3.3.8-viking4-200209250907.sgf 141
500 reg_genmove white
#? [S18]

# Double sente at E8. Clearly biggest move at this time.
loadsgf games/nngs/gnugo-3.3.8-viking4-200209250907.sgf 189
510 reg_genmove white
#? [E8]*

# Necessary to connect. See also connection:94
loadsgf games/nngs/gnugo-3.3.8-viking4-200209250907.sgf 193
520 reg_genmove white
#? [D9]*

# Semeai.
loadsgf games/nngs/gnugo-3.3.8-viking4-200209250907.sgf 205
530 reg_genmove white
#? [A11|B8]*

# Semeai.
loadsgf games/nngs/gnugo-3.3.8-viking4-200209250907.sgf 211
540 reg_genmove white
#? [B8|B9]*

# gnugo-3.3.9-nailer-200210192319 problems.

loadsgf games/nngs/gnugo-3.3.9-nailer-200210192319.sgf 122
550 reg_genmove white
#? [!H19|F19]

# The next two tests pass and perhaps don't need to be in the regressions.
# loadsgf games/nngs/gnugo-3.3.9-nailer-200210192319.sgf 146
# 560 reg_genmove white
# #? [!L16]
# 
# loadsgf games/nngs/gnugo-3.3.9-nailer-200210192319.sgf 148
# 570 reg_genmove white
# #? [!F19]

# See also owl1:373,374.
loadsgf games/nngs/gnugo-3.3.9-nailer-200210192319.sgf 152
580 reg_genmove white
#? [N19]

# We still can live.
loadsgf games/nngs/gnugo-3.3.9-nailer-200210192319.sgf 224
600 reg_genmove white
#? [B5]
