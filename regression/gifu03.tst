# Urgent to save group in upper right.
loadsgf games/cgf2003/GnuGo-Aya.sgf 48
1 reg_genmove white
#? [O19|M17|K18|N19|P14|Q14|R13|N14]*

# R1 makes seki.
loadsgf games/cgf2003/GnuGo-Aya.sgf 212
2 reg_genmove white
#? [R1]

# Urgent to block. It is okay to sacrifice P16 if black cuts.
loadsgf games/cgf2003/GnuGo-GORO.sgf 26
101 reg_genmove white
#? [O15]*

# P16 can still live.
loadsgf games/cgf2003/GnuGo-GORO.sgf 38
102 reg_genmove white
#? [R11]*

# F18 strange shape, leaves much aji.
loadsgf games/cgf2003/GnuGo-GORO.sgf 50
103 restricted_genmove white F18 F17
#? [F17]

# P2 lives.
loadsgf games/cgf2003/GnuGo-GORO.sgf 90
104 reg_genmove white
#? [P2]*

# G18 misses the point. More important to cover cut at L17.
loadsgf games/cgf2003/GnuGo-GORO.sgf 116
105 restricted_genmove white G18 H17 H16 J16 K16 K17 L16 L17 F17 F16 H18
#? [H17|H16|J16|K16|K17|L16|L17]*

# H1 2 points sente.
loadsgf games/cgf2003/GnuGo-GORO.sgf 174
106 reg_genmove white
#? [H1]

# F5 one point gote. N9, P9, and H19 are bigger.
loadsgf games/cgf2003/GnuGo-GORO.sgf 194
107 reg_genmove white
#? [N9|P9]*

# C15 good reduction. At least something like four points in sente.
loadsgf games/cgf2003/GnuGo-Go4.sgf 105
201 reg_genmove black
#? [C15]*

# B1 is not small but at most about 6 points gote. H7 is closer to 9
# or 10.
loadsgf games/cgf2003/GnuGo-Go4.sgf 115
202 reg_genmove black
#? [H7]*

# O18 is an endgame tesuji.
loadsgf games/cgf2003/GnuGo-Go4.sgf 129
203 restricted_genmove black O18 R19
#? [O18]*

# G18 is not worth anything at all.
loadsgf games/cgf2003/GnuGo-Go4.sgf 141
204 restricted_genmove black G18 J19
#? [J19]

# P5 clearly better than N6.
loadsgf games/cgf2003/GnuGo-Go4.sgf 155
205 restricted_genmove black N6 P5
#? [P5]

# C13 only move. D13 unthinkable.
loadsgf games/cgf2003/GnuGo-Go4.sgf 175
206 reg_genmove black
#? [C13]

# E2 urgent for life in the corner.
loadsgf games/cgf2003/GnuGo-GoInt.sgf 41
301 reg_genmove black
#? [E2]

# C1 suffices to live, as does B6.
loadsgf games/cgf2003/GnuGo-GoInt.sgf 59
302 reg_genmove black
#? [C1|B6]

# P18 is clearly superior to Q18.
loadsgf games/cgf2003/GnuGo-GoInt.sgf 69
303 restricted_genmove black Q18 P18
#? [P18]

# Very uninteresting to save G6.
loadsgf games/cgf2003/GnuGo-GoInt.sgf 119
304 restricted_genmove black G7 J7
#? [J7]*

# E1 dies.
loadsgf games/cgf2003/GnuGo-GoInt.sgf 139
305 reg_genmove black
#? [!E1]

# B15 very ineffective.
loadsgf games/cgf2003/GnuGo-GoInt.sgf 197
306 restricted_genmove black B15 B14
#? [B14]

# H8 and M7 miai for saving the K7 stones
loadsgf games/cgf2003/GnuGo-GoInt.sgf 205
307 restricted_genmove black P9 O8 N7 N9 H8 M7 N6
#? [P9|O8|N9]*

# No value playing N6.
loadsgf games/cgf2003/GnuGo-GoInt.sgf 207
308 restricted_genmove black N6 L3
#? [L3]

# J1 locally better than K1.
loadsgf games/cgf2003/GnuGo-GoInt.sgf 217
309 restricted_genmove black J1 K1
#? [J1]

# A3 wins the semeai.
# Update for 3.5.2: The basic problem is that B6 is thought to be
# tactically dead. See also reading:191.
loadsgf games/cgf2003/GnuGo-GoInt.sgf 237
310 reg_genmove black
#? [A3]*

# The game move K5 is too slow. The defense in upper right corner and
# of H16 is really urgent.
loadsgf games/cgf2003/GnuGo-GoeMate.sgf 27
401 reg_genmove black
#? [R15|O17|K16]

# White is already building the upper side territory, suppressing it and
# expanding own territory is urgent.
loadsgf games/cgf2003/GnuGo-GoeMate.sgf 31
402 reg_genmove black
#? [O17]*

# Capture solidly.
loadsgf games/cgf2003/GnuGo-GoeMate.sgf 85
403 reg_genmove black
#? [R13]*

# GNU Go 3.4 completely missed that it would be valuable to capture Q15.
loadsgf games/cgf2003/GnuGo-GoeMate.sgf 129
404 restricted_genmove black S12 S10
#? [S12]

# P14 is an atari which only helps black. Q14 is the only move.
loadsgf games/cgf2003/GnuGo-Haruka.sgf 10
501 reg_genmove white
#? [Q14]

# Tenuki unreasonable. One of the running groups must be strengthened.
loadsgf games/cgf2003/GnuGo-Haruka.sgf 42
502 reg_genmove white
#? [R9|L10|K12]

# R10 better eye shape and less cutting aji than R9.
loadsgf games/cgf2003/GnuGo-Haruka.sgf 48
503 restricted_genmove white R9 R10
#? [R10]*

# Too late to sacrifice the O18 group.
loadsgf games/cgf2003/GnuGo-Haruka.sgf 54
504 reg_genmove white
#? [L11|L10]*

# Cutting at E8 urgent.
loadsgf games/cgf2003/GnuGo-Haruka.sgf 86
505 reg_genmove white
#? [E8]

# Blocking at G2 very big. Black can jump all the way to J1.
loadsgf games/cgf2003/GnuGo-Haruka.sgf 118
506 restricted_genmove white F6 G2
#? [G2]*

# The push through at G13 is prepared.
loadsgf games/cgf2003/GnuGo-Haruka.sgf 168
507 reg_genmove white
#? [G13]*

## Q14 is not common in the Chinese fuseki. (But is it worth regressing over?)
# dr deleted 2004-12-13 when reinserting pattern Fuseki488 / pre 0x220d654c
# / post 0x235d1a72
# loadsgf games/cgf2003/GnuGo-KCC.sgf 7
# 601 restricted_genmove black R12 O16 Q14
# #? [R12|O16|Q11|P14]

# Making moyo here is much better than the game move F14. The
# important thing is that O5 has some strategic effect on white while
# F14 has none (too far from white).
loadsgf games/cgf2003/GnuGo-KCC.sgf 27
602 reg_genmove black
#? [O5]*

# The corner is worth 20 points.
loadsgf games/cgf2003/GnuGo-KCC.sgf 29
603 reg_genmove black
#? [B16]

# Not D2. Better to attack something.
loadsgf games/cgf2003/GnuGo-KCC.sgf 41
604 reg_genmove black
#? [C8|F12|E12|G13|O7|P7|P6]

loadsgf games/cgf2003/GnuGo-KCC.sgf 69
605 reg_genmove black
#? [O13|O14]*

# O11 lives.
loadsgf games/cgf2003/GnuGo-KCC.sgf 141
606 reg_genmove black
#? [O11]*

# D7 very definite sente.
loadsgf games/cgf2003/GnuGo-KCC.sgf 201
607 restricted_genmove black T14 D7
#? [D7]*

# O19 is not the right move if we want to reduce the aji of O18.
loadsgf games/cgf2003/GnuGo-Katsunari.sgf 71
701 restricted_genmove black O19 M18 L18 L17
#? [M18|L18|L17]

# N6 locally better than M6.
loadsgf games/cgf2003/GnuGo-Katsunari.sgf 107
702 restricted_genmove black M6 N6
#? [N6]*

# C1 locally worse than the moves which save A2.
loadsgf games/cgf2003/GnuGo-Katsunari.sgf 177
703 reg_genmove black
#? [B2|B1|C2]

# Don't let H8 get away.
loadsgf games/cgf2003/GnuGo-Katsunari.sgf 179
704 reg_genmove black
#? [D3|C2]
