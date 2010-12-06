# games/nngs/AdaGeek-gnugo-3.1.18-200201070826.sgf problems:

loadsgf games/nngs/AdaGeek-gnugo-3.1.18-200201070826.sgf 20
100 reg_genmove black
#? [K3]*


#Trying to save L2 is an overplay.
loadsgf games/nngs/AdaGeek-gnugo-3.1.18-200201070826.sgf 22
110 reg_genmove black
#? [E2|H5|J5]*

loadsgf games/nngs/AdaGeek-gnugo-3.1.18-200201070826.sgf 22
111 restricted_genmove black L2 L3
#? [L2]

#new failure (GNU Go 3.7.3 plays L2)
loadsgf games/nngs/AdaGeek-gnugo-3.1.18-200201070826.sgf 24
112 reg_genmove black
#? [H5|J5]*


loadsgf games/nngs/AdaGeek-gnugo-3.1.18-200201070826.sgf 30
120 restricted_genmove black S1 Q4 R5
#? [S1]*


#new failure GNU Go 3.7.3
loadsgf games/nngs/AdaGeek-gnugo-3.1.18-200201070826.sgf 32
122 restricted_genmove black K16 R8 K5
#? [K16|K5]*


loadsgf games/nngs/AdaGeek-gnugo-3.1.18-200201070826.sgf 56
130 reg_genmove black
#? [J6]*


loadsgf games/nngs/AdaGeek-gnugo-3.1.18-200201070826.sgf 72
140 restricted_genmove black M3 M4 O11
#? [O11]


loadsgf games/nngs/AdaGeek-gnugo-3.1.18-200201070826.sgf 80
150 restricted_genmove black J13 H12 H11
#? [H12|H11]


loadsgf games/nngs/AdaGeek-gnugo-3.1.18-200201070826.sgf 84
160 restricted_genmove black P14 P13 P12 O12
#? [O12]


# This test can safely be retired. /ab
#loadsgf games/nngs/AdaGeek-gnugo-3.1.18-200201070826.sgf 86
#170 reg_genmove black
##? [!N13|M5]


#new failure GNU Go 3.7.3
loadsgf games/nngs/AdaGeek-gnugo-3.1.18-200201070826.sgf 122
175 reg_genmove black
#? [L4|P2]*


#GNU Go 3.7.3 plays K9.  Is this acceptable? --evand Yes. /ab.
loadsgf games/nngs/AdaGeek-gnugo-3.1.18-200201070826.sgf 152
180 reg_genmove black
#? [L11|K9]

#
#loadsgf games/nngs/AdaGeek-gnugo-3.1.18-200201070826.sgf 160
#190 reg_genmove black
##? [O11|P12]*


loadsgf games/nngs/AdaGeek-gnugo-3.1.18-200201070826.sgf 162
200 reg_genmove black
#? [P12|O11]*


loadsgf games/nngs/AdaGeek-gnugo-3.1.18-200201070826.sgf 172
210 reg_genmove black
#? [P11]


loadsgf games/nngs/AdaGeek-gnugo-3.1.18-200201070826.sgf 194
220 reg_genmove black
#? [L1|E7|G13|K9]*




# games/nngs/AdaGeek-gnugo-3.1.18-200201071333.sgf problems:

# Not clear what is best, but strengthening one of the weak groups
# is urgent. /ab
loadsgf games/nngs/AdaGeek-gnugo-3.1.18-200201071333.sgf 24
230 reg_genmove black
#? [E9|G7|H2|K6|N5|D6]


loadsgf games/nngs/AdaGeek-gnugo-3.1.18-200201071333.sgf 30
240 restricted_genmove black T6 T5 R4 S4 T4 S3 T3 Q2
#? [R4|S4|Q2]*

# Desperate situation, no clear best move. /ab
#loadsgf games/nngs/AdaGeek-gnugo-3.1.18-200201071333.sgf 56
#250 reg_genmove black
##? [!S15]


loadsgf games/nngs/AdaGeek-gnugo-3.1.18-200201071333.sgf 60
260 reg_genmove black
#? [O13|P14]*


#GNU Go 3.7.3 likes M7
loadsgf games/nngs/AdaGeek-gnugo-3.1.18-200201071333.sgf 92
270 reg_genmove black
#? [F8|H8|G6|H9]*


loadsgf games/nngs/AdaGeek-gnugo-3.1.18-200201071333.sgf 98
280 reg_genmove black
#? [E10]*


loadsgf games/nngs/AdaGeek-gnugo-3.1.18-200201071333.sgf 104
290 reg_genmove black
#? [D12|D11|E12]*


loadsgf games/nngs/AdaGeek-gnugo-3.1.18-200201071333.sgf 112
300 restricted_genmove black B11 C12 C13 A13
#? [C13]


#New failure GNU Go 3.7.3
loadsgf games/nngs/AdaGeek-gnugo-3.1.18-200201071333.sgf 114
302 restricted_genmove black C12 F15 M7 B14
#? [F15|M7|B14]*


#New failure GNU Go 3.7.3
loadsgf games/nngs/AdaGeek-gnugo-3.1.18-200201071333.sgf 116
304 restricted_genmove black B14 E13 M7
#? [B14|M7]


#New failure GNU Go 3.7.3
loadsgf games/nngs/AdaGeek-gnugo-3.1.18-200201071333.sgf 158
306 reg_genmove black
#? [E14|E13|F13]*


#New failure GNU Go 3.7.3
loadsgf games/nngs/AdaGeek-gnugo-3.1.18-200201071333.sgf 240
308 reg_genmove black
#? [L3|M19]




# games/nngs/Lazarus-gnugo-3.1.17-200112301450.sgf problems:

loadsgf games/nngs/Lazarus-gnugo-3.1.17-200112301450.sgf 8
310 reg_genmove black
#? [B9]


loadsgf games/nngs/Lazarus-gnugo-3.1.17-200112301450.sgf 12
320 reg_genmove black
#? [B15|B16]


loadsgf games/nngs/Lazarus-gnugo-3.1.17-200112301450.sgf 14
330 reg_genmove black
#? [B15|B16]


#New failure GNU Go 3.7.3
loadsgf games/nngs/Lazarus-gnugo-3.1.17-200112301450.sgf 52
335 restricted_genmove black S6 R5 R2
#? [S6|R5]*


#Other moves possible.
loadsgf games/nngs/Lazarus-gnugo-3.1.17-200112301450.sgf 64
340 reg_genmove black
#? [R8]*




loadsgf games/nngs/Lazarus-gnugo-3.1.17-200112301450.sgf 82
370 reg_genmove black
#? [!Q1]*

# W misses break-chain (I think!?!) -tm 
loadsgf games/nngs/Lazarus-gnugo-3.1.17-200112301450.sgf 82
371 attack R5
#? [0]*


loadsgf games/nngs/Lazarus-gnugo-3.1.17-200112301450.sgf 126
380 reg_genmove black
#? [J2]*


loadsgf games/nngs/Lazarus-gnugo-3.1.17-200112301450.sgf 152
390 reg_genmove black
#? [N17]


loadsgf games/nngs/Lazarus-gnugo-3.1.17-200112301450.sgf 156
400 reg_genmove black
#? [N14|L13|M13]*


loadsgf games/nngs/Lazarus-gnugo-3.1.17-200112301450.sgf 204
410 reg_genmove black
#? [J9]


#E4 better than C4, if must defend.
# - We must defend. E4 only move. -ab
loadsgf games/nngs/Lazarus-gnugo-3.1.17-200112301450.sgf 234
420 reg_genmove black
#? [E4]




# games/nngs/Lazarus-gnugo-3.1.18-200201072351.sgf problems:

loadsgf games/nngs/Lazarus-gnugo-3.1.18-200201072351.sgf 10
430 reg_genmove black
#? [E18|D17]*


#other moves possible.
loadsgf games/nngs/Lazarus-gnugo-3.1.18-200201072351.sgf 24
440 reg_genmove black
#? [O2]




loadsgf games/nngs/Lazarus-gnugo-3.1.18-200201072351.sgf 84
480 reg_genmove black
#? [G14]*


loadsgf games/nngs/Lazarus-gnugo-3.1.18-200201072351.sgf 86
490 reg_genmove black
#? [J18]*


loadsgf games/nngs/Lazarus-gnugo-3.1.18-200201072351.sgf 90
500 reg_genmove black
#? [J17]


loadsgf games/nngs/Lazarus-gnugo-3.1.18-200201072351.sgf 92
510 reg_genmove black
#? [G14]


loadsgf games/nngs/Lazarus-gnugo-3.1.18-200201072351.sgf 226
530 reg_genmove black
#? [N5]*


loadsgf games/nngs/Lazarus-gnugo-3.1.18-200201072351.sgf 234
540 reg_genmove black
#? [H10]*


loadsgf games/nngs/Lazarus-gnugo-3.1.18-200201072351.sgf 242
550 restricted_genmove black N6 L6
#? [L6]


loadsgf games/nngs/Lazarus-gnugo-3.1.18-200201072351.sgf 272
560 reg_genmove black
#? [S15]*


loadsgf games/nngs/Lazarus-gnugo-3.1.18-200201072351.sgf 276
570 reg_genmove black
#? [F2]*




# games/nngs/NE-gnugo-3.1.18-200201030545.sgf problems:

loadsgf games/nngs/NE-gnugo-3.1.18-200201030545.sgf 20
590 reg_genmove black
#? [G3]*


loadsgf games/nngs/NE-gnugo-3.1.18-200201030545.sgf 40
600 reg_genmove black
#? [H6]


loadsgf games/nngs/NE-gnugo-3.1.18-200201030545.sgf 42
610 reg_genmove black
#? [J8]




# games/nngs/gnugo-3.1.15-goku-200112081829.sgf problems:

loadsgf games/nngs/gnugo-3.1.15-goku-200112081829.sgf 8
620 reg_genmove white
#? [P16]

#
#loadsgf games/nngs/gnugo-3.1.15-goku-200112081829.sgf 38
#630 reg_genmove white
##? [!J16]


loadsgf games/nngs/gnugo-3.1.15-goku-200112081829.sgf 40
640 restricted_genmove white M11 J9
#? [J9]



loadsgf games/nngs/gnugo-3.1.15-goku-200112081829.sgf 116
660 reg_genmove white
#? [H3]*




loadsgf games/nngs/gnugo-3.1.15-goku-200112081829.sgf 146
680 reg_genmove white
#? [P13]


loadsgf games/nngs/gnugo-3.1.15-goku-200112081829.sgf 152
690 reg_genmove white
#? [N13]




# games/nngs/gnugo-3.1.18-AdaGeek-200201061015.sgf problems:

loadsgf games/nngs/gnugo-3.1.18-AdaGeek-200201061015.sgf 47
700 restricted_genmove white K17 K18 J18 J17 K16 J16
#? [K17]


loadsgf games/nngs/gnugo-3.1.18-AdaGeek-200201061015.sgf 57
710 reg_genmove white
#? [F16]*

# Redundant. /ab
#loadsgf games/nngs/gnugo-3.1.18-AdaGeek-200201061015.sgf 59
#720 reg_genmove white
##? [F16]*



# This is an owl problem and should be made explicit as such. /ab
#loadsgf games/nngs/gnugo-3.1.18-AdaGeek-200201061015.sgf 113
#770 reg_genmove white
##? [!K2]



# Retired. /ab
#loadsgf games/nngs/gnugo-3.1.18-AdaGeek-200201061015.sgf 233
#800 reg_genmove white
##? [!N19|M19|L19|S18|K1]


loadsgf games/nngs/gnugo-3.1.18-AdaGeek-200201061015.sgf 241
810 reg_genmove white
#? [J6|J8]*


#J11 is terrible.
loadsgf games/nngs/gnugo-3.1.18-AdaGeek-200201061015.sgf 257
820 reg_genmove white
#? [J13|L9]*





# games/nngs/gnugo-3.1.18-Mikael-200201062258.sgf problems:

loadsgf games/nngs/gnugo-3.1.18-Mikael-200201062258.sgf 42
850 reg_genmove white
#? [H15]


loadsgf games/nngs/gnugo-3.1.18-Mikael-200201062258.sgf 74
860 reg_genmove white
#? [O13]*


loadsgf games/nngs/gnugo-3.1.18-Mikael-200201062258.sgf 138
870 reg_genmove white
#? [A18]*


# games/nngs/gnugo-3.1.18-Rufus-200201051408.sgf problems:

loadsgf games/nngs/gnugo-3.1.18-Rufus-200201051408.sgf 5
875 reg_genmove white
#? [C5]*


# games/nngs/gnugo-3.1.18-Rufus-200201051411.sgf problems:

loadsgf games/nngs/gnugo-3.1.18-Rufus-200201051411.sgf 13
880 reg_genmove white
#? [E6]*


loadsgf games/nngs/gnugo-3.1.18-Rufus-200201051411.sgf 19
890 restricted_genmove white G4 H4 J4
#? [G4]*




# games/nngs/gnugo-3.1.18-Rufus-200201051823.sgf problems:

loadsgf games/nngs/gnugo-3.1.18-Rufus-200201051823.sgf 21
900 reg_genmove white
#? [Q15]


loadsgf games/nngs/gnugo-3.1.18-Rufus-200201051823.sgf 43
910 reg_genmove white
#? [T16]*




# games/nngs/gnugo-3.1.18-Rufus-200201052349.sgf problems:



# games/nngs/gnugo-3.1.18-bnh-200201061916.sgf problems:

loadsgf games/nngs/gnugo-3.1.18-bnh-200201061916.sgf 30
920 reg_genmove white
#? [B14|C15]*


loadsgf games/nngs/gnugo-3.1.18-bnh-200201061916.sgf 34
930 reg_genmove white
#? [S8]


loadsgf games/nngs/gnugo-3.1.18-bnh-200201061916.sgf 36
940 reg_genmove white
#? [L2|M4|O3|S8|P17|B14]




# games/nngs/gnugo-3.1.18-dermicha-200201041355.sgf problems:

loadsgf games/nngs/gnugo-3.1.18-dermicha-200201041355.sgf 28
950 reg_genmove white
#? [F1]




# games/nngs/gnugo-3.1.18-goku-200201042031.sgf problems:

loadsgf games/nngs/gnugo-3.1.18-goku-200201042031.sgf 10
960 reg_genmove white
#? [R16|R17|S16]


loadsgf games/nngs/gnugo-3.1.18-goku-200201042031.sgf 38
970 reg_genmove white
#? [J9]*


loadsgf games/nngs/gnugo-3.1.18-goku-200201042031.sgf 80
980 reg_genmove white
#? [D10]*


loadsgf games/nngs/gnugo-3.1.18-goku-200201042031.sgf 86
990 reg_genmove white
#? [C9]


loadsgf games/nngs/gnugo-3.1.18-goku-200201042031.sgf 162
1000 reg_genmove white
#? [K6]


loadsgf games/nngs/gnugo-3.1.18-goku-200201042031.sgf 180
1010 reg_genmove white
#? [Q16]




# games/nngs/gnugo-3.1.18-goku-200201042350.sgf problems:

loadsgf games/nngs/gnugo-3.1.18-goku-200201042350.sgf 20
1020 reg_genmove white
#? [Q2]


loadsgf games/nngs/gnugo-3.1.18-goku-200201042350.sgf 26
1030 reg_genmove white
#? [L18|J13|J12|K13|R17|R16|C14]


loadsgf games/nngs/gnugo-3.1.18-goku-200201042350.sgf 28
1040 reg_genmove white
#? [L18|J13|J12|K13|R17|R16|C14]


loadsgf games/nngs/gnugo-3.1.18-goku-200201042350.sgf 36
1050 reg_genmove white
#? [J10|H10|G10|F10]*


# See also atari_atari:28.
loadsgf games/nngs/gnugo-3.1.18-goku-200201042350.sgf 52
1060 reg_genmove white
#? [H3|F3]*


loadsgf games/nngs/gnugo-3.1.18-goku-200201042350.sgf 56
1070 reg_genmove white
#? [K17]*


loadsgf games/nngs/gnugo-3.1.18-goku-200201042350.sgf 58
1080 reg_genmove white
#? [K17]*


loadsgf games/nngs/gnugo-3.1.18-goku-200201042350.sgf 92
1090 reg_genmove white
#? [B17|B16]


#Yikes! (B19 original move)
loadsgf games/nngs/gnugo-3.1.18-goku-200201042350.sgf 98
1100 reg_genmove white
#? [C18|D14]*



loadsgf games/nngs/gnugo-3.1.18-goku-200201042350.sgf 146
1110 reg_genmove white
#? [J16]


loadsgf games/nngs/gnugo-3.1.18-goku-200201042350.sgf 154
1120 reg_genmove white
#? [D4|D6]


loadsgf games/nngs/gnugo-3.1.18-goku-200201042350.sgf 158
1130 reg_genmove white
#? [B7|C8]




# games/nngs/gnugo-3.1.18-gopriest-200201072104.sgf problems:

loadsgf games/nngs/gnugo-3.1.18-gopriest-200201072104.sgf 42
1140 owl_defend B13
#? [1 (A11|B11|B10)]


#loadsgf games/nngs/gnugo-3.1.18-gopriest-200201072104.sgf 54
#1150 reg_genmove white
##? [!L7]


loadsgf games/nngs/gnugo-3.1.18-gopriest-200201072104.sgf 62
1160 reg_genmove white
#? [H10|H9|H8]


loadsgf games/nngs/gnugo-3.1.18-gopriest-200201072104.sgf 80
1170 reg_genmove white
#? [K6|K8]


loadsgf games/nngs/gnugo-3.1.18-gopriest-200201072104.sgf 82
1180 reg_genmove white
#? [L9]*


loadsgf games/nngs/gnugo-3.1.18-gopriest-200201072104.sgf 98
1190 owl_defend B13
#? [0]


loadsgf games/nngs/gnugo-3.1.18-gopriest-200201072104.sgf 104
1200 reg_genmove white
#? [PASS]*




# games/nngs/gnugo-3.1.18-guestx-200201071151.sgf problems:

loadsgf games/nngs/gnugo-3.1.18-guestx-200201071151.sgf 12
1210 reg_genmove white
#? [C11|B11]*




# games/nngs/gnugo-3.1.18-jimm-200201050556.sgf problems:

loadsgf games/nngs/gnugo-3.1.18-jimm-200201050556.sgf 24
1220 reg_genmove white
#? [C17]*


loadsgf games/nngs/gnugo-3.1.18-jimm-200201050556.sgf 28
1230 reg_genmove white
#? [H7|B5|E7|R14]*


#Locally, there are better moves than H14
loadsgf games/nngs/gnugo-3.1.18-jimm-200201050556.sgf 42
1240 restricted_genmove white J17 J16 K16 H15 J15 K15 H14 J14
#? [J16|K16|K15]


# Retired. /ab
#loadsgf games/nngs/gnugo-3.1.18-jimm-200201050556.sgf 44
#1250 reg_genmove white
##? [!J11]


loadsgf games/nngs/gnugo-3.1.18-jimm-200201050556.sgf 68
1260 restricted_genmove white F18 G18 L18
#? [G18|F18]


loadsgf games/nngs/gnugo-3.1.18-jimm-200201050556.sgf 90
1270 reg_genmove white
#? [C16|B15]*


loadsgf games/nngs/gnugo-3.1.18-jimm-200201050556.sgf 138
1280 reg_genmove white
#? [D13]



loadsgf games/nngs/gnugo-3.1.18-jimm-200201050556.sgf 248
1300 reg_genmove white
#? [M2]




# games/nngs/gnugo-3.1.18-patch-200201081044.sgf problems:

loadsgf games/nngs/gnugo-3.1.18-patch-200201081044.sgf 16
1310 reg_genmove white
#? [Q15|Q14]


loadsgf games/nngs/gnugo-3.1.18-patch-200201081044.sgf 20
1320 reg_genmove white
#? [O18]*


loadsgf games/nngs/gnugo-3.1.18-patch-200201081044.sgf 161
1420 reg_genmove white
#? [D12]


loadsgf games/nngs/gnugo-3.1.18-patch-200201081044.sgf 163
1430 reg_genmove white
#? [D12]




# games/nngs/gnugo-3.1.18-rpwiegand-200201072335.sgf problems:

loadsgf games/nngs/gnugo-3.1.18-rpwiegand-200201072335.sgf 12
1440 reg_genmove black
#? [H10]


loadsgf games/nngs/gnugo-3.1.18-rpwiegand-200201072335.sgf 13
1450 reg_genmove white
#? [J10]*



loadsgf games/nngs/gnugo-3.1.18-rpwiegand-200201072335.sgf 71
1480 reg_genmove white
#? [B1]



# games/nngs/gnugo-3.1.18-wingjk-200201080120.sgf problems:

#Can't block this way, unless have a stone around K16
loadsgf games/nngs/gnugo-3.1.18-wingjk-200201080120.sgf 18
1500 reg_genmove white
#? [R16]


loadsgf games/nngs/gnugo-3.1.18-wingjk-200201080120.sgf 24
1510 reg_genmove white
#? [S16]*


# Other moves possible, !G6
loadsgf games/nngs/gnugo-3.1.18-wingjk-200201080120.sgf 44
1520 reg_genmove white
#? [F15|F14]*




loadsgf games/nngs/gnugo-3.1.18-wingjk-200201080120.sgf 232
1540 reg_genmove white
#? [M8]*


loadsgf games/nngs/gnugo-3.1.18-wingjk-200201080120.sgf 234
1550 reg_genmove white
#? [M8]


loadsgf games/nngs/gnugo-3.1.18-wingjk-200201080120.sgf 236
1560 reg_genmove white
#? [M8]


loadsgf games/nngs/gnugo-3.1.18-wingjk-200201080120.sgf 244
1570 reg_genmove white
#? [N13]*




# games/nngs/juka-gnugo-3.1.16-200112142153.sgf problems:

loadsgf games/nngs/juka-gnugo-3.1.16-200112142153.sgf 6
1580 reg_genmove black
#? [K6|O6]


#M3 is antisuji!
# L5 best attack here. /ab
loadsgf games/nngs/juka-gnugo-3.1.16-200112142153.sgf 8
1590 reg_genmove black
#? [L5]*


#Egads, don't tenuki here!
loadsgf games/nngs/juka-gnugo-3.1.16-200112142153.sgf 14
1600 reg_genmove black
#? [N5]*



loadsgf games/nngs/juka-gnugo-3.1.16-200112142153.sgf 220
1620 reg_genmove black
#? [B5]*


loadsgf games/nngs/juka-gnugo-3.1.16-200112142153.sgf 228
1630 restricted_genmove black A2 C2
#? [C2]


loadsgf games/nngs/juka-gnugo-3.1.16-200112142153.sgf 238
1640 restricted_genmove black E2 F1
#? [F1]




# games/nngs/makoops-gnugo-3.1.18-200201081434.sgf problems:

loadsgf games/nngs/makoops-gnugo-3.1.18-200201081434.sgf 12
1650 reg_genmove black
#? [S4]*


# Retired. /ab
#loadsgf games/nngs/makoops-gnugo-3.1.18-200201081434.sgf 30
#1660 reg_genmove black
##? [!F18]


#B18 possibly?  !A15
loadsgf games/nngs/makoops-gnugo-3.1.18-200201081434.sgf 50
1670 reg_genmove black
#? [C18]


loadsgf games/nngs/makoops-gnugo-3.1.18-200201081434.sgf 56
1680 reg_genmove black
#? [K17]


loadsgf games/nngs/makoops-gnugo-3.1.18-200201081434.sgf 60
1690 reg_genmove black
#? [K17|L16]*


loadsgf games/nngs/makoops-gnugo-3.1.18-200201081434.sgf 68
1700 reg_genmove black
#? [D8|E9|C9]



#loadsgf games/nngs/makoops-gnugo-3.1.18-200201081434.sgf 164
#1740 reg_genmove black
##? [!H3]


loadsgf games/nngs/makoops-gnugo-3.1.18-200201081434.sgf 178
1750 reg_genmove black
#? [L8|J9]


loadsgf games/nngs/makoops-gnugo-3.1.18-200201081434.sgf 192
1760 reg_genmove black
#? [N7]


loadsgf games/nngs/makoops-gnugo-3.1.18-200201081434.sgf 196
1770 reg_genmove black
#? [M2|M3|M5|O1]


loadsgf games/nngs/makoops-gnugo-3.1.18-200201081434.sgf 202
1780 reg_genmove black
#? [L1]*




# games/nngs/scf-gnugo-3.1.18-200201060027.sgf problems:

loadsgf games/nngs/scf-gnugo-3.1.18-200201060027.sgf 57
1790 restricted_genmove black O16 P14 S14
#? [O16]*


# Now redundant. /ab
#loadsgf games/nngs/scf-gnugo-3.1.18-200201060027.sgf 59
#1800 reg_genmove black
##? [!H5|G4|F6|F7]


loadsgf games/nngs/scf-gnugo-3.1.18-200201060027.sgf 67
1810 reg_genmove black
#? [N15]*



#Something around L6 looks right.
loadsgf games/nngs/scf-gnugo-3.1.18-200201060027.sgf 83
1830 reg_genmove black
#? [L6]*


#locally K12 is better than J13
loadsgf games/nngs/scf-gnugo-3.1.18-200201060027.sgf 97
1840 reg_genmove black
#? [K12]*



loadsgf games/nngs/scf-gnugo-3.1.18-200201060027.sgf 117
1860 restricted_genmove black D19 E18
#? [E18]


# Redundant. /ab
#loadsgf games/nngs/scf-gnugo-3.1.18-200201060027.sgf 125
#1870 reg_genmove black
##? [!D19]


# F6 connects out either way.
loadsgf games/nngs/scf-gnugo-3.1.18-200201060027.sgf 135
1880 reg_genmove black
#? [F6|G7]*


loadsgf games/nngs/scf-gnugo-3.1.18-200201060027.sgf 161
1890 reg_genmove black
#? [H16]


#Hard to tell whether J18 or F19 are better.
loadsgf games/nngs/scf-gnugo-3.1.18-200201060027.sgf 173
1900 reg_genmove black
#? [J18|F19]


loadsgf games/nngs/scf-gnugo-3.1.18-200201060027.sgf 177
1910 reg_genmove black
#? [E18]


loadsgf games/nngs/scf-gnugo-3.1.18-200201060027.sgf 205
1920 restricted_genmove black K11 L11
#? [K11]

#CATEGORY=TERRITORIAL_VALUATION
# Not E9 as played by GNU Go! -ab
loadsgf games/nngs/gnugo-3.1.20-setenza-200201162038.sgf 15
1930 reg_genmove white
#? [K15|L3|K3|F17|O17]*

#Retired. /ab
#loadsgf games/nngs/gnugo-3.1.20-setenza-200201162038.sgf 23
#1940 reg_genmove white
##? [!N5|O5]

# Have to defend M16/O17.
loadsgf games/nngs/gnugo-3.1.20-setenza-200201162038.sgf 29
1950 reg_genmove white
#? [!Q11|S10|N17|M17|N16]

loadsgf games/nngs/gnugo-3.1.20-setenza-200201172131.sgf 15
1955 reg_genmove white
#? [D3]

loadsgf games/nngs/gnugo-3.1.20-setenza-200201172131.sgf 27
1960 reg_genmove white
#? [M15]*

loadsgf games/nngs/gnugo-3.1.20-setenza-200201172131.sgf 33
1970 reg_genmove white
#? [P7|L3]*

loadsgf games/nngs/gnugo-3.1.20-setenza-200201172131.sgf 35
1980 restricted_genmove white G17 F15
#? [F15]

loadsgf games/nngs/gnugo-3.1.20-setenza-200201172131.sgf 39
2000 reg_genmove white
#? [P13|P7|L3|M10]*

loadsgf games/nngs/gnugo-3.1.20-setenza-200201172131.sgf 43
2010 reg_genmove white
#? [P13|M10|O6]*

loadsgf games/nngs/gnugo-3.1.20-setenza-200201172131.sgf 51
2020 reg_genmove white
#? [L2|H6|H7|J6|J7]*

# This is tough. If Black plays J5, then he can win the semeai after
# E3 - E4 - D3 - D2 - D4 - D5 - E5. /ab
loadsgf games/nngs/LordOfPi-gnugo-3.1.20-200201202014.sgf 16
2030 reg_genmove black
#? [J5]*

# I think B can still get a seki (or life) at the bottom if he plays E5. /ab
loadsgf games/nngs/LordOfPi-gnugo-3.1.20-200201202014.sgf 22
2040 reg_genmove black
#? [E5]

#CATEGORY=OWL_TUNING + KO
loadsgf games/nngs/LordOfPi-gnugo-3.1.20-200201202014.sgf 30
2050 reg_genmove black
#? [A2|B2]*

#CATEGORY=OWL_TUNING
loadsgf games/nngs/LordOfPi-gnugo-3.1.20-200201202014.sgf 36
2060 reg_genmove black
#? [C4|C1|F1]
