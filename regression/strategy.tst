loadsgf games/strategy1.sgf
1 reg_genmove white
#? [Q13]

loadsgf games/strategy2.sgf 5
2 reg_genmove black
#? [!G2|D2]

# # Current GNU Go would never play those moves. Removed.
# loadsgf games/strategy2.sgf 9
# 3 reg_genmove black
# #? [!H1|H2]
# 
# loadsgf games/strategy2.sgf 11
# 4 reg_genmove black
# #? [!F1]

loadsgf games/strategy3.sgf 14
5 reg_genmove white
#? [R17|P17]

# incident 104
# Probably requires a dynamic connection analysis to solve.
loadsgf games/incident104.sgf 63
6 reg_genmove white
#? [E10]*

# incident 106
loadsgf games/incident104.sgf 215
7 reg_genmove white
#? [S19]

# incident 107
# tm - moved the test out to move 228; at 216, T17 is arguable bigger.
loadsgf games/incident107.sgf 228
8 reg_genmove black
#? [N2|N1]*

# incident 109
loadsgf games/incident108b.sgf 172
9 reg_genmove black
#? [P15]

# incident 110
loadsgf games/incident108b.sgf 176
10 reg_genmove black
#? [Q7]

#CATEGORY=BAD_PROBLEM
#DESCRIPTION=Not clear enough.
#SEVERITY=0
# From STRATEGY:
#    2.7.178 makes the move at P17 which is better than the game
#    move at P18 but probably inferior to the recommended move at P15.
#    It is not totally clear to me that the move at P15 is really
#    best since the 3-3 point is still open. 
# incident 111
loadsgf games/incident108b.sgf 178
11 reg_genmove black
#? [P15]

# incident 112
# Suboptimal to make eye at L4.
loadsgf games/incident108b.sgf 270
12 reg_genmove black
#? [N4]

# incident 113
# See also reading:194
loadsgf games/incident108b.sgf 292
13 reg_genmove black
#? [N18]

# incident 121
# E9 is not necessarily the best move since upper left weak too.
# But the game move at R7 is not urgent!
loadsgf games/incident121.sgf 24
14 reg_genmove black
#? [E9]

# incident 132
loadsgf games/incident121.sgf 116
16 reg_genmove black
#? [H12]


#CATEGORY=PATTERN_TUNING
#DESCRIPTION=N11 may not be biggest, but should be found.
#SEVERITY=2
#    2.7.179: This can surely be fixed with the addition of a
#    pattern. After N11, the strings at M12 and N10 cannot both be 
#    saved. This is not necessarily the biggest move on the board but 
#    at least this move should be found.
# incident 165
# H10 looks fine - tm
loadsgf games/incident165.sgf 108
17 reg_genmove black
#? [N11|H10]


# incident 166a
# Original move was N3, which is pointless.
# E5 is an overplay, see reading test 124.
# F3 seems fine to protect the two cuts -arend
# N11 may well be biggest but is already tested above.
loadsgf games/incident165.sgf 142
18 restricted_genmove black E4 E5 F3 N3
#? [E4|F3]

# # Current GNU Go would never consider passing here. Removed.
# loadsgf games/strategy4.sgf
# 19 reg_genmove black
# #? [!PASS]


#CATEGORY=OWL_TUNING
#DESCRIPTION=S13 is terrible - negative value.
#SEVERITY=8
#    2.7.179: Owl code finds Q11 but it remains undervalued at 17 points.
# incident 290
loadsgf games/incident290.sgf 30
20 reg_genmove black
#? [Q11]

# incident 291
loadsgf games/incident291.sgf 54
21 reg_genmove black
#? [S13]

# incident 292
loadsgf games/incident291.sgf 70
22 reg_genmove black
#? [K3]

# incident 294
loadsgf games/incident291.sgf 88
23 reg_genmove black
#? [!K5]

# incident 295
# endgame mistake
loadsgf games/incident291.sgf 250
24 reg_genmove black
#? [F4]

# Moved to filllib.tst
# # incident 296
# # liberty filling mistake
# loadsgf games/incident291.sgf 274
# 25 reg_genmove black
# #? [PASS]

loadsgf games/strategy5.sgf 12
26 reg_genmove black
#? [D3]


#CATEGORY=PATTERN_TUNING
#DESCRIPTION=Blocking in the corner is better.
#SEVERITY=3
#    This test passed before 2.7.183, but for the wrong reason: 
#    the move at C3 was allegedly an owl attack/defense point for 
#    the dragon at D3, which of course it is not.
#
#    Thus this example should be regarded as having been already
#    broken.
#
loadsgf games/strategy5.sgf 14
27 reg_genmove black
#? [C3]

loadsgf games/strategy5.sgf 20
28 restricted_genmove black G3 F2
#? [G3]

loadsgf games/strategy5.sgf 40
29 reg_genmove black
#? [R4]*

loadsgf games/strategy5.sgf 44
30 restricted_genmove black T7 S4
#? [S4]

# This is a problem with the semeai analyzer and revise_semeai().
# But there is no semeai.
loadsgf games/strategy6.sgf 274
31 reg_genmove black
#? [PASS]

loadsgf games/strategy7.sgf 23
32 reg_genmove black
#? [A4]

loadsgf games/strategy8.sgf 283
33 reg_genmove white
#? [O7]

loadsgf games/nicklas/nicklas8.sgf 72
34 reg_genmove black
#? [E17]

loadsgf games/nicklas/nicklas8.sgf 80
35 restricted_genmove black B17 H17
#? [H17]

loadsgf games/nicklas/nicklas8.sgf 82
36 restricted_genmove black H17 D18
#? [H17]

loadsgf games/nicklas/nicklas8.sgf 84
37 restricted_genmove black A17 A18 C19 H17
#? [H17]

# The life code solves this mistake.
loadsgf games/nicklas/nicklas8.sgf 86
38 restricted_genmove black H17 B19
#? [H17]

loadsgf games/nicklas/nicklas8.sgf 96
39 reg_genmove black
#? [P16]


#CATEGORY=STRATEGY
#DESCRIPTION=L14 is not a terrible way to attack O13 group.
#SEVERITY=2
#Consider retiring this problem, or adding L14.  L14 is not terrible.
#M11 is better than L4
#    2.7.231: Broke again. M11 is overrated at 28 points. The
#    large followup value is responsible.
# I disagree. M11 makes territory while attacking. -arend
loadsgf games/nicklas/nicklas8.sgf 98
40 reg_genmove black
#? [E5|M11]*


#CATEGORY=STRATEGY
#DESCRIPTION=N16 is better than M11.  Could be a tough attack, though.
#SEVERITY=4
#    2.7.179: After fixing strategy 40, the spurious defenses of D19
#    will be removed. Now N16 will be the most valuable move. So 
#    this should be fixed automatically when 40 is fixed.
#
#    2.7.180: this test passes.
#
#    2.7.220: N12 is found as an owl attack point for the S14 dragon.
#    This attack will probably fail. More seriously, M16 is preferred
#    to N16.
#
loadsgf games/nicklas/nicklas8.sgf 106
41 reg_genmove black
#? [N16]

loadsgf games/strategy9.sgf 232
42 reg_genmove black
#? [C17]

# A14 works but is inferior shape since it leaves more ko threats.
loadsgf games/strategy10.sgf
43 reg_genmove white
#? [B13|B14|A14]

#CATEGORY=STRATEGY
#DESCRIPTION=L4 is awkward.
#SEVERITY=2
#    2.7.179: Whether or not the recommended move at G2 is found, the 
#    move at A6 is egregiously bad. In this situation an owl critical 
#    dragon adjoins an owl dead one. We need to revisit our policy for 
#    such situations. The reading code thinks the worm at B4 can be 
#    defended, so this is an owl lunch.
#
#    2.7.180: After revision of semeai.c, the move at A6 is no longer
#    found. Now GNU plays at M16 on the top. Arguably stabilizing
#    the bottom at G2 or (worse but still OK) at J2 is better but
#    I would class this result as ACCEPTABLE.
#
#    3.1.9: Best move here is tough.  GNU Go correctly tries
#    to stabilize G3 group.
#
# See also reading:158
loadsgf games/strategy11.sgf 77
44 reg_genmove black
#? [G2]*

# G17/G18 and C1/H1 are both huge.
loadsgf games/strategy11.sgf 127
45 reg_genmove black
#? [G17|G18|C1|H1]

loadsgf games/strategy11.sgf 245
46 reg_genmove black
#? [E1]

loadsgf games/strategy12.sgf 9
47 reg_genmove black
#? [!N17]

#CATEGORY=FUSEKI_STRATEGY
#DESCRIPTION=B17 better than D17
#SEVERITY=3
loadsgf games/strategy12.sgf 19
48 reg_genmove black
#? [B17]*

loadsgf games/strategy12.sgf 21
# D16 is the strongest but these are acceptable
49 reg_genmove black
#? [C17|D16|B17]*


#CATEGORY=OWL_TUNING
#DESCRIPTION=P9 is pointless compared to Q9.
#SEVERITY=8
# Q9 is clearly better than Q11 because it stops a black connection
# along the edge.
# So much better, that I removed Q11 option -trevor
loadsgf games/strategy13.sgf
50 reg_genmove white
#? [Q9]*
