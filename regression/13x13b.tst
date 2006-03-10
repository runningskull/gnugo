# This is a collection of mistakes by GNU Go in Stefan Mertin's private
# 13x13 - Computer Go tournament in 2001-2002 (see
# http://www.geocities.com/comp_go/13x13.html).
# 
# While the games used in 13x13.tst were played by GNU Go 3.0, this test
# suite covers games played at the end of the tournament by GNU Go 3.2
# at level 15.
# 
# The opponents and results in these games are:
# 
# Game       GNU Go  Result  Opponent
# name       color
# DAGN1.sgf  white   +40.5   Dariush
# DAGN2.sgf  white   +52.5   Dariush
# DAGN3.sgf  white   + 2.5   Dariush
# GNDA1.sgf  black   +39.5   Dariush
# GNDA2.sgf  black   +34.5   Dariush
# GNDA3.sgf  black   +32.5   Dariush
# GAGN1.sgf  white   -29.5   GoAhead
# GAGN2.sgf  white   + 0.5   GoAhead
# GAGN3.sgf  white   +16.5   GoAhead
# GNGA1.sgf  black   + 2.5   GoAhead
# GNGA2.sgf  black   +67.5   GoAhead
# GNGA3.sgf  black   - 5.5   GoAhead
# GHGN1.sgf  white   +33.5   Goliath
# GHGN2.sgf  white   + 6.5   Goliath
# GHGN3.sgf  white   -48.5   Goliath
# GNGH1.sgf  black   + 5.5   Goliath
# GNGH2.sgf  black   +13.5   Goliath
# GNGH3.sgf  black   -59.5   Goliath
# GIGN1.sgf  white   +12.5   Go Intellect
# GIGN2.sgf  white   - 0.5   Go Intellect
# GIGN3.sgf  white   +21.5   Go Intellect
# GNGI1.sgf  black   +31.5   Go Intellect
# GNGI2.sgf  black   +18.5   Go Intellect
# GNGI3.sgf  black   + 2.5   Go Intellect
# GMGN1.sgf  white   + 3.5   GoeMate
# GMGN2.sgf  white   -10.5   GoeMate
# GMGN3.sgf  white   - 6.5   GoeMate
# GNGM1.sgf  black   + 9.5   GoeMate
# GNGM2.sgf  black   -12.5   GoeMate
# GNGM3.sgf  black   - 6.5   GoeMate
# GPGN1.sgf  white   - 6.5   Go Professional
# GPGN3.sgf  white   -15.5   Go Professional
# GPGN5.sgf  white   -16.5   Go Professional
# GNGP1.sgf  black   +27.5   Go Professional
# GNGP3.sgf  black   - 0.5   Go Professional
# GNGP5.sgf  black   - 1.5   Go Professional
# GXGN1.sgf  white   +25.5   Go4++
# GXGN2.sgf  white   -20.5   Go4++
# GXGN3.sgf  white   -14.5   Go4++
# GNGX1.sgf  black   -10.5   Go4++
# GNGX2.sgf  black   +11.5   Go4++
# GNGX3.sgf  black   - 4.5   Go4++
# HTGN1.sgf  white   +10.5   HandTalk
# HTGN2.sgf  white   +23.5   HandTalk
# HTGN3.sgf  white   + 6.5   HandTalk
# GNHT1.sgf  black   + 5.5   HandTalk
# GNHT2.sgf  black   -13.5   HandTalk
# GNHT3.sgf  black   +16.5   HandTalk
# MFGN1.sgf  white   +43.5   Many Faces of Go
# MFGN2.sgf  white   +35.5   Many Faces of Go
# MFGN3.sgf  white   -42.5   Many Faces of Go
# GNMF1.sgf  black   +17.5   Many Faces of Go
# GNMF2.sgf  black   +44.5   Many Faces of Go
# GNMF3.sgf  black   -18.5   Many Faces of Go
# WLGN1.sgf  white   - 1.5   Wulu
# WLGN2.sgf  white   -41.5   Wulu
# WLGN3.sgf  white   + 5.5   Wulu
# GNWL1.sgf  black   +52.5   Wulu
# GNWL2.sgf  black   +20.5   Wulu
# GNWL3.sgf  black   -13.5   Wulu
# 
# The variations in the sgf-files are due to Stefan Mertin.

loadsgf games/mertin13x13/GNDA3.sgf 61
1 reg_genmove black
#? [M4]

loadsgf games/mertin13x13/GAGN1.sgf 12
2 reg_genmove white
#? [C12|G10|G12]

# N5 is locally worse than M5.
loadsgf games/mertin13x13/GAGN1.sgf 28
3 restricted_genmove white M5 N5
#? [M5]

# Hane at K1 is not appropriate.
loadsgf games/mertin13x13/GAGN1.sgf 42
4 restricted_genmove white F4 H2 K1
#? [F4]

# Unreasonable to block at A5.
loadsgf games/mertin13x13/GAGN1.sgf 66
5 restricted_genmove white A5 B4 C4 C5
#? [!A5]

# B4 unreasonable. C4 locally the best move.
loadsgf games/mertin13x13/GAGN1.sgf 68
6 restricted_genmove white B4 C4
#? [C4]*

# H11 allows a combination attack starting with K10.
loadsgf games/mertin13x13/GNGA3.sgf 67
7 reg_genmove black
#? [J11]

# F10 blocks and connects in the best way. F11 is clearly inferior.
loadsgf games/mertin13x13/GNGA3.sgf 81
8 reg_genmove black
#? [F10]

# Most other moves are better than the game move at C6.
loadsgf games/mertin13x13/GHGN1.sgf 26
9 owl_attack D7
#? [0]

# Most other moves are better than the game move at M6.
loadsgf games/mertin13x13/GHGN2.sgf 48
10 restricted_genmove white M4 L6 M6
#? [M4|L6]

# L6 urgent.
loadsgf games/mertin13x13/GHGN2.sgf 52
11 reg_genmove white
#? [L6]

# J5 not joseki.
loadsgf games/mertin13x13/GHGN3.sgf 8
12 restricted_genmove white J5 J6
#? [J6]*

# M13 is a very odd move.
loadsgf games/mertin13x13/GHGN3.sgf 38
13 restricted_genmove white M11 M12 M13 K12
#? [K12]*

# J9 is huge.
loadsgf games/mertin13x13/GNGH2.sgf 23
14 reg_genmove black
#? [J9]

# N11 is meaningless.
loadsgf games/mertin13x13/GNGH2.sgf 71
15 reg_genmove black
#? [B5]

# H2 completely misses the point. Necessary to block at G4.
loadsgf games/mertin13x13/GNGH3.sgf 57
16 reg_genmove black
#? [G4]

# F1 gives seki.
loadsgf games/mertin13x13/GNGH3.sgf 111
17 reg_genmove black
#? [F1]*

# Necessary to defend the bottom.
loadsgf games/mertin13x13/GIGN2.sgf 18
18 reg_genmove white
#? [F3]

# One critical point left at B5. K10 is dame.
loadsgf games/mertin13x13/GIGN2.sgf 60
19 reg_genmove white
#? [B5]

# K10 is unreasonable.
loadsgf games/mertin13x13/GIGN3.sgf 92
20 reg_genmove white
#? [J10]

# J13 fails right out.
loadsgf games/mertin13x13/GIGN3.sgf 94
21 restricted_genmove white K11 J13
#? [K11]

# A6 bad shape.
loadsgf games/mertin13x13/GIGN3.sgf 100
22 reg_genmove white
#? [B6|B7]

# J1 gives ko. K1 kills unconditionally.
loadsgf games/mertin13x13/GNGI1.sgf 95
23 reg_genmove black
#? [K1]

# N11 is clearly the worst way to end the ko. M13 and M9 are both better.
loadsgf games/mertin13x13/GNGI3.sgf 63
24 reg_genmove black
#? [M9|M13]

# M13 lives independently. K13 gives a semeai.
loadsgf games/mertin13x13/GNGI3.sgf 65
25 reg_genmove black
#? [M13]*

# Must connect at D3.
loadsgf games/mertin13x13/GMGN2.sgf 30
26 reg_genmove white
#? [D3]

# B2 inefficient way to live.
loadsgf games/mertin13x13/GMGN2.sgf 92
27 reg_genmove white
#? [C4]

# Taken out -- H9 seems reasonable nevertheless. /ab
## H9 can be cut off.
#loadsgf games/mertin13x13/GMGN3.sgf 30
#28 reg_genmove white
##? [!H9]

# Must connect at F3.
loadsgf games/mertin13x13/GNGM3.sgf 11
29 reg_genmove black
#? [F3]*

loadsgf games/mertin13x13/GPGN1.sgf 62
30 reg_genmove white
#? [N8]*

loadsgf games/mertin13x13/GPGN1.sgf 64
31 reg_genmove white
#? [M7]*

# G5 is dame. H4 is the point needed to win the game.
loadsgf games/mertin13x13/GNGP3.sgf 55
32 reg_genmove black
#? [H4]

# F6 clearly better than E6.
loadsgf games/mertin13x13/GNGP5.sgf 73
33 restricted_genmove black E6 F6
#? [F6]

# E5 is just cut off and captured.
loadsgf games/mertin13x13/GXGN2.sgf 48
34 reg_genmove white
#? [E8|F9|F8|D7]

# Breakin problem.
# D10 captures on too small scale.
loadsgf games/mertin13x13/GXGN2.sgf 52
35 reg_genmove white
#? [D7|E8|F9]*

# Low enclosure better.
loadsgf games/mertin13x13/GXGN3.sgf 12
36 restricted_genmove white J3 J4 H3 H4
#? [J3|H3]*

# Must follow up A6 with A4, which in any case is sente.
loadsgf games/mertin13x13/GNGX1.sgf 51
37 reg_genmove black
#? [A4]

# E8 clearly bigger than E7.
loadsgf games/mertin13x13/HTGN3.sgf 102
38 restricted_genmove white E7 E8
#? [E8]*

# F10 needed for seki.
loadsgf games/mertin13x13/MFGN3.sgf 58
39 reg_genmove white
#? [F10]*

# Must connect at F8.
loadsgf games/mertin13x13/WLGN2.sgf 74
40 reg_genmove white
#? [F8]

loadsgf games/mertin13x13/GNWL1.sgf 61
41 reg_genmove black
#? [J6]

# L7 way overconcentrated.
loadsgf games/mertin13x13/GNWL2.sgf 35
42 restricted_genmove black L7 L13
#? [L13]

# J3 is of course bigger than J1.
loadsgf games/mertin13x13/GNWL3.sgf 109
43 restricted_genmove black J1 J2 J3
#? [J3]*
