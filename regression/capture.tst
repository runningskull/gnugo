loadsgf golois/Goemate990902-15.sgf
1 defend S10
#? [1 T7]

loadsgf golois/Goemate990902-5.sgf
2 defend J2
#? [1 M1]

loadsgf golois/Goemate990904-4.sgf
3 defend Q15
#? [0]

# This problem is much harder than it may look. After black H5, white
# can force with the sequence W E1, B F2, W H2, B J2, W H3, B J3.
# After that, white plays H6 and black has to work hard to stop escape. /gf
# Changed point of attack to H6, which looks correct. /db
loadsgf golois/Goemate991109-1.sgf
4 attack G5
#? [1 H6]*

# It looks as if the originally recommended solution [1 S3] is wrong.
# After W:S3 B:S2 W:T2 B:S1 W:O1 B:S5 black wins the semeai. /db
loadsgf golois/Handtalk980821-2.sgf
5 attack R4
#? [1 S3]*

loadsgf golois/Handtalk980821-2.sgf
6 attack O2
#? [1 (O3|N2|P1|S3|O1|S2)]

loadsgf golois/Handtalk980821-2.sgf
7 defend R3
#? [1 (S3|O3|N2|P1|O1|S2)]

loadsgf golois/Handtalk980824-1.sgf
8 defend K5
#? [0]*

loadsgf golois/Handtalk980826-1.sgf
9 defend B4
#? [0]

# P11 seems to be an effective tactical defense too, albeit making
# things much more difficult. /gf
loadsgf golois/Jimmy990711-2.sgf
10 defend P10
#? [1 O11]*

loadsgf golois/knippel.sgf
11 defend C8
#? [0]

loadsgf golois/ko990604.sgf
12 defend B3
#? [0]

loadsgf golois/Prendre990424-1.sgf
13 attack C8
#? [1 D8]*

loadsgf golois/Prendre990430-1.sgf
14 defend H7
#? [0]

loadsgf golois/Prendre990502-1.sgf
15 defend G4
#? [0]

loadsgf golois/Prendre990630-1.sgf
16 defend B12
#? [1 (A12|A15|B17|B18|A17|A18)]

loadsgf golois/test.sgf
17 attack G7
#? [1 G6]

# This test case originally said "defend G5", which must be an error.
# Changed it to defend the weak stone at F5 instead. /gf
# F6 works to defend F3 -trevor
loadsgf golois/web000103.sgf
18 defend F5
#? [1 F6]


