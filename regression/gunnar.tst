# M19 is a serious blunder.
loadsgf games/nngs/gnugo-3.3.10-viking4-200210261703.sgf 181
1 reg_genmove white
#? [M17]

# D19 is locally worse than both E17 and E18.
loadsgf games/nngs/gnugo-3.3.10-rcde05-200210280427.sgf 83
2 restricted_genmove white D19 E18 E17
#? [E17|E18]

# Just connect at K17. Don't make it difficult.
loadsgf games/nngs/gnugo-3.3.10-rcde05-200210280427.sgf 157
3 reg_genmove white
#? [K17]*

# L17 is horribly overconcentrated
loadsgf games/nngs/gnugo-3.3.10-rcde05-200210300235.sgf 27
4 restricted_genmove white L17 O18
#? [O18]

# G1 is the wrong way to defend G2. Both F2 and K2/K3 are locally better.
# But what's really urgent is a move around F12.
# An invasion around R8 would also be big but must wait until the left
# edge has been settled.
loadsgf games/nngs/gnugo-3.3.10-rcde05-200210300235.sgf 51
5 reg_genmove white
#? [F12]*

# B17 is unreasonable. Locally E16 only move.
loadsgf games/nngs/gnugo-3.3.11-ccwills-200211031528.sgf 35
6 restricted_genmove white B17 E16
#? [E16]

# O14 adds nothing at all to the position.
loadsgf games/nngs/niko-gnugo-3.3.11-200211201651.sgf 216
7 restricted_genmove black O14 M19
#? [M19]

# O13 is completely wasted.
loadsgf games/nngs/niko-gnugo-3.3.11-200211201651.sgf 222
8 restricted_genmove black M19 O13
#? [M19]

# Critical to keep black split up.
loadsgf games/gunnar/gunnar1.sgf 34
9 reg_genmove white
#? [F13]*

# Q15 was supposed to attack either Q14 or Q16 but obviously doesn't work.
# R12 is probably better than R14 but both get the point.
loadsgf games/gunnar/gunnar1.sgf 96
10 reg_genmove white
#? [R12|R14]

# L8 doesn't suffice for local life.
loadsgf games/gunnar/gunnar1.sgf 126
11 reg_genmove white
#? [K9]

# C13 is nonsense. Just connect with B13.
loadsgf games/gunnar/gunnar2.sgf 72
12 reg_genmove white
#? [B13]

# Q5 is 3 points reverse sente. Q12 is of similar size.
# E4 is only one point reverse sente.
loadsgf games/gunnar/gunnar2.sgf 164
13 reg_genmove white
#? [Q5|Q12]

# Necessary to block at Q11. O13 is a rather bad idea.
loadsgf games/gunnar/gunnar2.sgf 168
14 reg_genmove white
#? [Q11]*

# O6 is clearly locally better than P6.
loadsgf games/gunnar/gunnar3.sgf 140
15 restricted_genmove white O6 P6
#? [O6]

# T5 is huge. See next test case.
loadsgf games/gunnar/gunnar3.sgf 190
16 reg_genmove white
#? [T5]*

# T6 allows a combination attack. Necessary to fall back.
loadsgf games/gunnar/gunnar3.sgf 192
17 reg_genmove white
#? [S7]*

# Q15 is just swallowed.
loadsgf games/gunnar/gunnar4.sgf 180
18 restricted_genmove white P15 Q15
#? [P15]

# No reason to play ko with L3.
loadsgf games/gunnar/gunnar4.sgf 198
19 reg_genmove white
#? [L4]

# H14 and J14 are blunders.
loadsgf games/gunnar/gunnar5.sgf 198
20 reg_genmove black
#? [F16|K14]

# P17 only helps black and leaves white with bad shape. Locally Q17 is
# superior. A move around K16 may be even better to start with.
loadsgf games/nngs/gnugo-3.3.16-rubus-200302281805.sgf 17
21 restricted_genmove white P17 Q17 K16 K17
#? [Q17|K16|K17]*

# Same problem as 21 but with slightly different surroundings.
loadsgf games/nngs/gnugo-3.3.16-rubus-200302281805.sgf 19
22 restricted_genmove white E17 D17 K16 K17
#? [D17|K16|K17]*

# Must not play empty triangle at O5. N5 superior local shape.
loadsgf games/nngs/gnugo-3.3.16-rubus-200302281805.sgf 69
23 restricted_genmove white O4 O5 N4 N5
#? [N5]

# L17 and P9 are both -1 point gote. K3 is just a random dame point.
# See also owl1:300
loadsgf games/gunnar/gunnar7.sgf
24 restricted_genmove black L17 K3
#? [K3]
25 restricted_genmove black P9 K3
#? [K3]*

# J2 is not a relevant cutstone and saving it should not be worth a bonus.
loadsgf games/gunnar/gunnar8.sgf
26 restricted_genmove black K1 H1 J3 J4 F9
#? [F9]

# White cannot make two proper eyes but can make life with
# topologically false eyes. G18 and M19 both live but G18 is
# slightly bigger. M12 is a well timed double sente move which of
# course also makes life. F19, G19, K19, and N19 all die.
# See also owl1:301
loadsgf games/gunnar/rbm-bobk-200303111815.sgf 159
27 reg_genmove white
#? [G18|M12|M19]*

# T9 is maybe okay but it seems to end in ko for the life of one of
# the white dragons. Anything is better than the game move at N1, but
# it looks like S9 is the correct move. For the time being we allow
# also T9, though.
loadsgf games/nngs/gnugo-3.3.17-joshj-200304172202.sgf 226
28 reg_genmove white
#? [S9|T9]

# H5 is the only move. H11 can connect to either of two invincible
# dragons and is therefore not weak at all.
loadsgf games/gunnar/gunnar9.sgf 1
29 reg_genmove white
#? [H5]*

# F18 should be a better and bigger way to live than B18.
loadsgf games/nngs/gnugo-3.3.18-overziel7-200304281000.sgf 110
30 restricted_genmove white F18 B18
#? [F18]

# F18 is clearly bigger and better shape than E17.
loadsgf games/nngs/gnugo-3.3.18-overziel7-200304281000.sgf 120
31 restricted_genmove white F18 E17
#? [F18]*

# C18 looks like the simplest way to live. There may be other effective
# moves which should also be accepted.
loadsgf games/nngs/gnugo-3.3.18-overziel7-200304281000.sgf 124
32 reg_genmove white
#? [C18]*

# Important to strengthen the wall at M11. P10 is not so large.
loadsgf games/nngs/gnugo-3.3.18-overziel7-200304281000.sgf 184
33 restricted_genmove white M11 P10
#? [M11]*

# The game move at P7 is worthless. M4 or M5 take the last point.
loadsgf games/nngs/gnugo-3.3.18-overziel7-200304281000.sgf 234
34 reg_genmove white
#? [M4|M5]

# D5 is a ko threat which loses two points. Better let black connect
# the ko.
loadsgf games/gunnar/gunnar10.sgf
35 reg_genmove white
#? [C7|D7|E7]*

# The break-in code of GNU Go 3.3.21 wants to play J12, H11, or K13.
# Clearly filling ko at B18 is the only worthwhile move here.
loadsgf games/gunnar/gunnar11.sgf
36 reg_genmove white
#? [B18]

# The game move at S8 was found as a bogus endgame backfilling dame
# move. 
loadsgf games/nngs/tommmal-gnugo-3.4-200309021655.sgf 106
37 restricted_genmove black S8 Q15
#? [Q15]*

# The semeai can still be won.
loadsgf games/nngs/tommmal-gnugo-3.5.1-200309081210.sgf 208
38 reg_genmove black
#? [M7|N1]*

# Keep the connection to save everything without ko.
loadsgf games/nngs/gnugo-3.5.1-viking4-200309231039.sgf 123
39 reg_genmove white
#? [N4]*

# A14 is huge, determining the entire upper left part of the board.
# The lower right corner is also critical but much smaller.
# See also owl1:332.
loadsgf games/nngs/tommmal-gnugo-3.5.1-200310071202.sgf 194
40 reg_genmove black
#? [A14]

# 3.5.1 finds an owl attack on H17 at E16 but no defense. The semeai
# code correctly understands the vitality of D13 for both players and
# revises the status to critical. However, the attack point is not
# revised.
loadsgf games/nngs/tommmal-gnugo-3.5.1-200310140825.sgf 142
41 reg_genmove black
#? [D13]

# Make seki with B18. The problem for 3.5.1 is that the semeai code is
# not called since E19 is considered owl alive and B18 owl dead.
loadsgf games/gunnar/gunnar12.sgf
42 reg_genmove white
#? [B19]

# Q19 lets white make seki. T17 dies.
loadsgf games/nngs/Minori-gnugo-3.5.2gf1-200312161433.sgf 130
43 reg_genmove black
#? [R18|S18]*

# T18 still gives seki. T17 dies.
loadsgf games/nngs/Minori-gnugo-3.5.2gf1-200312161433.sgf 132
44 reg_genmove black
#? [T18]*

# The played move here was H13, caused by incorrect delta territory
# but ultimately by a bug in break_through(). Naturally there is no
# way for white to break through G13, H14, G15.
#
# Original problem solved, but there's a better move than just
# filling dames at Q7|Q6|J16|J17|E7|N4 /nn
loadsgf games/nngs/Minori-gnugo-3.5.2gf1-200312161433.sgf 254
45 reg_genmove black
#? [K1]

# Don't leave aji around, capture the white stones solidly.
loadsgf games/nngs/joshj-gnugo-3.5.2gf1-200312171536.sgf 140
46 restricted_genmove black O11 O9 N10 N9 M9
#? [O11]*

# F15 is about 6 points plus some reverse followup. A4 is three points
# reverse sente.
loadsgf games/nngs/gnugo-3.5.2gf1-wingjk-200312301242.sgf 114
47 restricted_genmove white F15 A4
#? [F15]*

# Saving M13 is small compared to stopping a black incursion.
loadsgf games/nngs/gnugo-3.5.2gf1-wingjk-200312301242.sgf 122
48 restricted_genmove white M12 P11
#? [P11]*

# In 3.5.2 the breakin code hallucinates that white A10 would suddenly
# let black break into the territory around F6.
loadsgf games/nngs/gnugo-3.5.2gf1-wingjk-200312301242.sgf 164
49 reg_genmove white
#? [A10]

# R13 very inefficient.
loadsgf games/nngs/gnugo-3.5.4-chpr-200403201748.sgf 132
50 restricted_genmove white R13 O8
#? [O8]*

# S7 kills all black stones in the corner. For GNU Go 3.5.4 this seems
# to be a problem with valuation of the semeai.
loadsgf games/nngs/gnugo-3.5.4-chpr-200403201748.sgf 154
51 reg_genmove white
#? [S7]

# Regardless whether the dragon is thrashing or critical, Q14 is the
# most solid move.
loadsgf games/nngs/gnugo-3.5.4-chpr-200403201748.sgf 216
52 reg_genmove white
#? [Q14]*

# R18 is clearly ineffective.
# See also connection:111
loadsgf games/nngs/gnugo-3.5.4-chpr-200403201748.sgf 290
53 reg_genmove white
#? [S17]

# Playing J4 or F6 inside own territory clearly loses points. In 3.5.9
# this is caused by some break-in confusion.
loadsgf games/gunnar/gunnar13.sgf
54 restricted_genmove white J4 F6 D9
#? [D9]

# White must find a ko threat. In the choice between C13 and D13, the
# former has absolutely no followup potential and in this case also
# happens to lose a point right out.
#
# Notice that C1 must be included among the considered moves to
# provoke generation of ko threats in the first place.
loadsgf games/gunnar/gunnar14.sgf
play black B1
55 restricted_genmove white C13 D13 C1
#? [D13]

# White must find a ko threat. A11 is neither a ko threat, nor worth a
# point.
loadsgf games/gunnar/gunnar15.sgf
play black R1
56 reg_genmove white
#? [P18|E15|O12|N11|T12|T10|S7|T5]

# No territory to contest around J12. Connect ko at Q19 instead.
loadsgf games/gunnar/gunnar16.sgf
57 restricted_genmove black J13 J12 H11 J11 K11 Q19
#? [Q19]

# No territory to contest around J15. Filling ko at Q19 bigger.
loadsgf games/gunnar/gunnar17.sgf
58 reg_genmove white
#? [Q19]

# Do not defend against combination attack inside potential territory
# unless we have to.
loadsgf games/gunnar/gunnar18.sgf
59 reg_genmove black
#? [P13]

# R10 is clearly bigger than R11.
loadsgf games/gunnar/gunnar19.sgf
60 restricted_genmove white R10 R11
#? [R10]*

# Major endgame tesuji.
loadsgf games/gunnar/gunnar20.sgf
61 reg_genmove black
#? [D8]*

# O19 is both bigger and safer than the played move at G14.
loadsgf games/kgs/GnuGoCVS-john.sgf 204
62 reg_genmove white
#? [O19]*

# Q2 more solid than P1.
loadsgf games/kgs/dogo-TheGNUGo.sgf 42
63 restricted_genmove black Q2 P1
#? [Q2]*

# The game move at M1 is just weird. R2 is locally better than Q2.
loadsgf games/kgs/dogo-TheGNUGo.sgf 44
64 restricted_genmove black M1 Q2 R2 M5 R5
#? [R2|M5|R5]*

# P19 is one point reverse sente. There are many larger moves.
loadsgf games/kgs/dogo-TheGNUGo.sgf 146
65 restricted_genmove black P19 Q12 B11 S8
#? [Q12|B11|S8]*

# A10 was an overplay. Black should capture it and not fall back.
loadsgf games/kgs/dogo-TheGNUGo.sgf 162
66 restricted_genmove black B11 B10 B9 A9 A11
#? [B11]

# O16 severely overvalued and D9 undervalued
loadsgf games/kgs/dogo-TheGNUGo.sgf 164
67 restricted_genmove black D9 O16
#? [D9]*

# Capture on a large scale.
loadsgf games/kgs/dogo-TheGNUGo.sgf 170
68 restricted_genmove black F6 D8
#? [D8]

# Stop the main incursion first.
loadsgf games/kgs/dogo-TheGNUGo.sgf 178
69 restricted_genmove black A9 D7
#? [D7]*

# A13 and A18 live, the game move at D14 dies.
loadsgf games/kgs/dogo-TheGNUGo.sgf 190
70 reg_genmove black
#? [A18|A13]

# D5 huge.
loadsgf games/kgs/dogo-TheGNUGo.sgf 198
71 reg_genmove black
#? [D5]

# P11 is sente but loses one point since white can then play Q7 in
# sente.
loadsgf games/kgs/llk-GNU.sgf 228
72 restricted_genmove black P11 F18
#? [F18]

# The ko threat was answered. Now take the ko again instead of a small
# move like M4.
# See also owl1:376,377.
loadsgf games/kgs/GNU-ivanho.sgf 182
73 reg_genmove white
#? [M19]

# No need to fall down to B14. C14 is effective enough and saves more
# of the territory.
loadsgf games/kgs/GNU-ivanho.sgf 210
74 restricted_genmove white B14 C14
#? [C14]

# See also connection:120,121.
loadsgf games/kgs/GNU-higher.sgf 193
75 reg_genmove white
#? [M11]*

# R2 can't live. Necessary to find a ko threat.
loadsgf games/kgs/GNU-HISOKA10.sgf 226
76 restricted_genmove white P1 R2 G17
#? [G17]

# G8 is biggest by far.
loadsgf games/kgs/GNU-marfan.sgf 183
77 reg_genmove white
#? [G8]

# N3 is a solid defense. The game move at M5 fails.
loadsgf games/kgs/GNU-marfan.sgf 221
78 reg_genmove white
#? [N3]*

# K12 is clearly more solid than K11.
loadsgf games/kgs/perler-GNU.sgf 198
79 restricted_genmove black K11 K12
#? [K12]

# K15 is clearly more solid than K14.
loadsgf games/kgs/perler-GNU.sgf 202
80 restricted_genmove black K14 K15
#? [K15]*

# C5 is both bigger and leaves less aji than C4.
loadsgf games/kgs/JMBE-GNU.sgf 198
81 restricted_genmove black C4 C5
#? [C5]

# One point bigger to capture at T6 than to connect at T11.
loadsgf games/kgs/sade-GNU.sgf 253
82 restricted_genmove black T11 T6
#? [T6]*

# M10 is clearly better than N10.
loadsgf games/kgs/sade-GNU.sgf 265
83 restricted_genmove black M10 N10
#? [M10]

# P13 is clearly better than O13.
loadsgf games/kgs/GNU-bassanio.sgf 164
84 restricted_genmove white P13 O13
#? [P13]

# T6 can't be defended. R8 is globally too small but locally
# guaranteed to be better.
loadsgf games/kgs/ben9992000-GNU.sgf 141
85 restricted_genmove black T6 R8
#? [R8]

# B6 unreasonable.
loadsgf games/kgs/haiku-GNU.sgf 93
86 restricted_genmove black C8 B6
#? [C8]

# O5 solves the problem. P4 backfires.
loadsgf games/kgs/melonhead1-GNU.sgf 127
87 restricted_genmove black O5 P4 M4 P3 M3 N3 N2 M5 N6
#? [O5]

# A15 does not solve the atari-atari problem.
loadsgf games/kgs/melonhead1-GNU.sgf 183
88 reg_genmove black
#? [B17]*

# E6 three points reverse sente.
loadsgf games/kgs/Kuksa-GNU.sgf 178
89 reg_genmove black
#? [E6]*

# Black H3 captures the H2 stone with ko. Even if the ko should
# eventually be lost there's no points to lose in trying.
loadsgf games/gunnar/gunnar21.sgf
90 reg_genmove black
#? [H3]

# Break-in weirdness. P6 does not save the T10 stones.
loadsgf games/kgs/GNU-roidesnems.sgf 143
91 restricted_genmove white P6 S4 T4 P5 O5 N5
#? [S4|T4]*

# B15 cuts off the black stone in the corner. G17 is pointlessly submissive.
loadsgf games/kgs/GNU-namascae.sgf 102
92 restricted_genmove white G17 B15
#? [B15]

# C11 blocks off better than D11.
loadsgf games/kgs/GNU-namascae.sgf 110
93 restricted_genmove white C11 D11
#? [C11]*

# Necessary to protect against J14 cut. L16 and N15 are ineffective.
loadsgf games/kgs/GNU-namascae.sgf 154
94 reg_genmove white
#? [H15|K16]*

# Of course O3 isn't an option.
loadsgf games/gunnar/gunnar22.sgf
95 reg_genmove white
#? [P1|S1]

# Connection intransitivity, connecting at D18 lets the top be cut
# off. We accept all moves which solve the connection problem.
loadsgf games/kgs/koketto-GNU.sgf 209
96 reg_genmove black
#? [E16|F15|F14|G15|H15]*

# B8 is overvalued by 3.7.8.
loadsgf games/kgs/GNU-goodluck.sgf 233
97 restricted_genmove white N16 B8
#? [N16]*

# F7 is mostly pointless as strategic defense.
loadsgf games/kgs/GNU-ituyosi2.sgf 243
98 restricted_genmove white A6 B6 F7
#? [A6]*

# T2 is found by endgame move generator in 3.7.9 but has no effect.
loadsgf games/kgs/GNU-ituyosi2.sgf 293
99 restricted_genmove white T2 C11
#? [C11]*

# Letting black cut at S6 gives major trouble.
loadsgf games/kgs/GNU-ituyosi2.sgf 301
100 reg_genmove white
#? [S6]*

# T8 wins the semeai, T7 allows black to make seki.
loadsgf games/kgs/GNU-ituyosi2.sgf 311
101 reg_genmove white
#? [T8]*
