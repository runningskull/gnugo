# See also test 87.
loadsgf games/connection1.sgf
1 same_dragon G17 K17
#? [0]
2 same_dragon R12 R9
#? [0]

loadsgf games/strategy11.sgf 87
3 same_dragon A6 B4
#? [0]

loadsgf games/incident104.sgf 63
4 same_dragon C14 E14
#? [0]

loadsgf games/strategy13.sgf
5 same_dragon S11 R12
#? [0]

# incident 213
loadsgf games/incident211.sgf 40
6 same_dragon G8 J6
#? [0]

loadsgf games/strategy21.sgf 86
7 same_dragon F4 J3
#? [0]

loadsgf games/owl19.sgf
# A problem with rot2 rotated owl tests 128 and 129 reduces to
# this dragon amalgamation problem.
8 same_dragon A15 C16
#? [1]
9 same_dragon P19 Q17
#? [1]

# This definitely requires dynamic connection reading.
loadsgf games/incident240.sgf 69
10 same_dragon C10 E9
#? [0]

loadsgf games/connection2.sgf 177
11 same_dragon G6 J7
#? [0]
12 same_dragon G8 J7
#? [0]

loadsgf games/strategy29.sgf 204
13 same_dragon O6 Q8
#? [0]
14 same_dragon O6 R6
#? [0]

loadsgf games/strategy33.sgf 159
15 same_dragon G7 F9
#? [0]

loadsgf games/doublecut.sgf
16 same_dragon E16 G14
#? [0]

loadsgf games/doublecut2.sgf 151
17 same_dragon D13 F12
#? [0]

loadsgf games/arion.sgf 63
18 same_dragon N17 P17
#? [1]

loadsgf games/dublin1.sgf 72
19 same_dragon B15 E16
#? [0]

loadsgf games/nicklas/nicklas12.sgf 69
20 connect K17 N17
#? [1 K18]*

loadsgf games/trevor/trevor_09.sgf
21 connect D4 G5
#? [1 (E5|D5|C5|C6|E6|C7|D7|E7)]
22 disconnect D4 G5
#? [1 (E5|D5|E6|F6)]

loadsgf games/incident169.sgf 110
23 connect K17 N16
#? [1 (M17|L16)]
24 disconnect L15 L17
#? [1 (M17|L16)]
25 disconnect L14 M12
#? [1 L13]

loadsgf games/nicklas/nicklas16.sgf 38
26 connect C3 C6
#? [1 B4]
27 connect C3 B7
#? [1 B4]

loadsgf games/golife.sgf 38
28 disconnect H5 H7
#? [0]

loadsgf games/incident240.sgf 69
29 disconnect B12 F7
#? [1 D9]*

loadsgf games/strategy13.sgf
30 connect R13 S8
#? [1 (Q11|T10|T9)]
31 disconnect R13 S8
#? [1 Q9]

loadsgf games/incident240.sgf 69
32 disconnect B12 E9
#? [1 D9]

# White can't cut unconditionally but black has to make the first ko threat.
loadsgf games/trevor/auto/a035.sgf 28
33 disconnect B4 C6
#? [2 B5]*

# Monkey jumps and relatives.
loadsgf games/connection3.sgf
34 disconnect B11 A9
#? [0]
35 disconnect L12 H13
#? [0]
36 connect M3 N7
#? [1 (N4|M4|N5|N6)]
37 disconnect M3 N7
#? [1 (N4|M4|N5|M5|M6|N6)]
38 connect B2 G1
#? [1 E1]*
39 disconnect B2 G1
#? [1 (F2|F1|E2|E1|D2|D1|C2|C1|G2|H1|B1)]

# More monkey jumps and relatives.
loadsgf games/connection4.sgf
40 connect B11 A8
#? [1 B9]
41 disconnect B11 A8
#? [1 (B9|A10|B10|B8|A7)]
42 connect L12 H13
#? [1 K12]
43 disconnect L12 H13
#? [1 (K12|J12|J13|H12|G13)]
44 connect M3 N7
#? [0]
45 disconnect M3 N7
#? [1 (N4|M4|N5|M5|M6|M7|N8|M8)]
46 connect B2 G1
#? [1 (B1|C1|C2|D2|E1|F1)]
47 disconnect B2 G1
#? [1 (C1|E1)]

loadsgf games/trevor/auto/a038.sgf 34
48 disconnect C3 G3
#? [1 (D2|F2)]

loadsgf games/trevor/trevor_15.sgf 
49 disconnect D7 G6
#? [1 (E7|G7)]

loadsgf games/trevor/auto/a014.sgf 16
50 disconnect B5 C2
#? [1 B4]

loadsgf games/nngs/Lazarus-gnugo-3.1.19-200201092246.sgf 108
51 connect R14 Q16
#? [1 Q15]
52 connect R14 O15
#? [1 Q15]*
53 connect S7 S4
#? [1 T5]

# D8 might also disconnect.
loadsgf games/nngs/Lazarus-gnugo-3.1.19-200201092246.sgf 164
54 disconnect B6 D10
#? [1 C8]*
55 connect B6 D10
#? [1 (D8|C8)]*
56 connect B6 F9
#? [1 D8]

loadsgf games/nngs/Lazarus-gnugo-3.1.19-200201092246.sgf 192
57 connect K14 K16
#? [1 L16]

loadsgf games/nngs/Lazarus-gnugo-3.1.19-200201092246.sgf 204
58 disconnect D18 F17
#? [0]

loadsgf games/nngs/Lazarus-gnugo-3.1.19-200201092246.sgf 216
59 disconnect D18 E15
#? [1 (D15|F15|B15|D19|E19)]
60 connect D14 B18
#? [1 (B14|D15|F15|B15|D19|E19|C13)]
61 connect O9 Q9
#? [1 (D15|F15|B15|D19|E19)]*

loadsgf games/nngs/Lazarus-gnugo-3.1.19-200201092246.sgf 238
62 connect S18 S14
#? [1 (T15|T17)]

loadsgf games/nngs/Lazarus-gnugo-3.1.19-200201092246.sgf 258
63 connect N13 N17
#? [1 (N16|O17|P16)]

loadsgf games/trevor/auto/b72.sgf 30
64 disconnect G11 K11
#? [1 H11]
65 same_dragon G11 K11
#? [0]

loadsgf games/trevor/auto/b67.sgf 58
66 disconnect M6 L3
#? [1 (M5|M3)]
67 connect M6 L3
#? [1 (M3|M2|M5)]
68 disconnect M6 L1
#? [1 (M5|M3|M2|M1)]
69 connect M6 L1
#? [1 (M2|M3)]
70 same_dragon M6 L3
#? [0]

loadsgf games/nngs/gnugo-3.1.18-goku-200201042350.sgf 52
71 connect D3 G3
#? [1 (F3|H3|E3|G2|F2|H2|E2)]
72 disconnect D3 G3
#? [1 F3]
73 connect D3 J2
#? [1 (F3|H3|E3|G2|F2|H2|E2)]
74 disconnect D3 J2
#? [1 F3]
75 same_dragon D3 G3
#? [0]

loadsgf games/trevor/auto/b20.sgf 38
76 connect D2 G2
#? [1 (E3|F1|F2)]
77 disconnect D2 G2
#? [1 (F2|F1)]
78 same_dragon D2 G2
#? [0]

loadsgf games/nngs/gnugo-3.1.34-viking4-200204240801.sgf 134
79 connect S4 R7
#? [1 S6]
80 connect R2 R7
#? [1 S6]

loadsgf games/handtalk/handtalk13.sgf 106
81 connect L5 R5
#? [1 O5]*

loadsgf games/handtalk/handtalk13.sgf 116
82 same_dragon M7 M10
#? [0]*

loadsgf games/handtalk/handtalk23.sgf 80
83 same_dragon K3 L5
#? [0]
84 same_dragon H6 L5
#? [0]

loadsgf games/handtalk/handtalk23.sgf 82
85 same_dragon K3 G3
#? [0]
86 same_dragon K3 M3
#? [0]

# See also test 2. Possibly some more move works.
loadsgf games/connection1.sgf
87 disconnect R12 R9
#? [1 S10]

# See also trevorb:670.
loadsgf games/trevor/auto/b64.sgf 44
88 disconnect E6 H5
#? [0]

loadsgf games/ego.sgf 191
89 same_dragon M12 Q11
#? [1]

loadsgf games/trevor/auto/b13.sgf 48
90 same_dragon D4 F4
#? [0]

loadsgf games/trevor/auto/c18.sgf 82
91 same_dragon J8 J10
#? [1]

loadsgf games/olympiad2002/game1-19-goint-gnugo-1-0.sgf 91
92 same_dragon P15 Q14
#? [1]

# See also nngs2:130
loadsgf games/nngs/joshj-gnugo-3.3.2-200205310709.sgf 124
93 disconnect K2 L3
#? [3 L2]

# See also nngs2:520
loadsgf games/nngs/gnugo-3.3.8-viking4-200209250907.sgf 193
94 disconnect D8 E10
#? [1 D9]

loadsgf games/reading39.sgf
95 disconnect O4 P3
#? [1 (N1|N2)]

loadsgf games/handtalk/handtalk1.sgf 31
96 same_dragon C16 C13
#? [0]

loadsgf games/nngs/ManyFaces3-gnugo-3.3.11-200211071935.sgf 201
97 disconnect P8 O9
#? [1 O5]

loadsgf games/nngs/gnugo-3.3.10-rcde05-200210280427.sgf 185
98 disconnect N4 Q6
#? [1 O5]

loadsgf games/gunnar/gunnar3.sgf 74
99 disconnect N3 P3
#? [0]*

loadsgf games/nngs/gnugo-3.3.10-jimm-200210222018.sgf 140
100 disconnect L12 M13
#? [(2|3) J15]*

loadsgf games/nngs/Temnik-gnugo-3.3.15-200301161937.sgf 232
101 disconnect O3 L5
#? [1 (M3|N3)]*

loadsgf games/nngs/gnugo-3.3.17-joshj-200304172202.sgf 222
102 disconnect Q8 Q6
#? [1 (S7|S6|S8|R6)]*

# See also owl:114
loadsgf games/incident169.sgf 210
103 connect L2 G4
#? [1 K2]

loadsgf golois/Aya991113-9.sgf
104 connect G2 L4
#? [1 (H2|H1|G1|J1|H3)]

# See also nicklas1:501
# Strictly speaking, this is ko. But this is way beyond the readconnect
# horizon.
loadsgf games/nicklas/nicklas5.sgf 23
105 disconnect F8 G6
#? [1 G7]

loadsgf games/nngs/Lazarus-gnugo-3.1.31-200204140254.sgf 164
trymove black R14
trymove white P16
106 disconnect O14 O17
#? [1 O16]*
popgo
popgo

loadsgf games/gunnar/gunnar11.sgf
107 connect C14 H12
#? [0]

loadsgf games/nngs/gnugo-3.3.21-joshj-200306270402.sgf 172
108 same_dragon E16 F10
#? [0]*

loadsgf games/nngs/gnugo-3.5.1-viking4-200309231039.sgf 123
109 same_dragon O3 N5
#? [0]*

# See also nngs4:290
loadsgf games/nngs/aburry-gnugo-3.3.12-200212162212.sgf 38
110 connect E3 G3
#? [0]

# See also gunnar:53
loadsgf games/nngs/gnugo-3.5.4-chpr-200403201748.sgf 290
trymove W R18
trymove B S17
111 connect O18 R18
#? [0]
popgo
popgo

# See also nngs1:11
loadsgf games/nngs/camel-gnugo-3.1.26-200203022335.sgf 104
trymove B S6
112 connect S3 S6
#? [0]
popgo

# See also blunder:24 and reading:198.
loadsgf games/blunder17.sgf
trymove B P5
113 disconnect L3 M4
#? [1 L4]
popgo

loadsgf games/kgs/TheGNUGo-JVD.sgf 19
trymove white C15
114 connect C14 C18
#? [1 B15]*
popgo

loadsgf games/gunnar/gunnar9.sgf 4
115 disconnect D10 J7
#? [1 H7]

# See also golife:3 and connection:28.
loadsgf games/golife.sgf 30
116 disconnect H4 H7
#? [0]

# See also trevor:15.
loadsgf games/trevor/trevor_15.sgf 
117 disconnect D7 F7
#? [3 E7]

# See also reading:219.
loadsgf games/kgs/llk-GNU.sgf 150
118 same_dragon H12 N13
#? [0]
119 disconnect H12 N13
#? [1 N10]*

# See also gunnar:75.
loadsgf games/kgs/GNU-higher.sgf 193
120 same_dragon L10 N11
#? [0]
121 disconnect L10 N11
#? [1 M11]*

# This test is extremely important.
loadsgf games/kgs/perler-GNU.sgf 246
122 disconnect B18 E15
#? [3 E16]

loadsgf games/kgs/ben9992000-GNU.sgf 189
123 disconnect E10 F12
#? [3 E11]
124 connect E10 G10
#? [1 E11]
125 disconnect E10 G10
#? [3 E11]

# See also trevorc:450.
loadsgf games/trevor/auto/c23.sgf 72
126 disconnect E8 F6
#? [1 E6]*

# See also strategy5:300.
loadsgf games/CrazyStone2.sgf 77
127 same_dragon A9 G6
#? [0]*

# See also nngs4:630
loadsgf games/nngs/evand-gnugo-3.3.21-200306130638.sgf 156
play black C5
128 connect C4 C8
#? [1 B6]*
