# For any of the problems below, if PASS is a solution it is the best
# solution. However the other solutions are acceptable.

############## semeai tests #################
#
# After analyze_semeai [dragon1] [dragon2]
# the results are returned in the form (result1) (result2).
# These are the results of the defense of dragon1 and the attack
# of dragon2 assuming that the dragon1 player moves first. Thus
# a result 1 0 typically means seki, while a 1 1 result means a kill
# and 0 0 means the semeai is lost. In addition to seki, 1 0 may mean
# that both dragons gain independent life.

loadsgf games/semeai/semeai6.sgf
1 analyze_semeai C1 E1
#? [1 0 (F1|F2|F3|F4|F5|PASS)]

2 analyze_semeai E1 C1
#? [1 0 (B5|B4|B3|PASS)]

3 analyze_semeai L1 N1
#? [1 0 (O5|O4|O3|P3|Q2|Q1|PASS)]

4 analyze_semeai N1 L1
#? [1 0 (H2|J3|K3|PASS)]

5 analyze_semeai A14 A12
#? [1 1 (A13|B13|C13|D13)]

6 analyze_semeai A12 A14
#? [1 1 (A15|B15|C15|D15|E15)]

loadsgf games/semeai/semeai7.sgf
7 analyze_semeai C1 E1
#? [1 0 (F1|F2|F3|F4|F5|PASS)]

8 analyze_semeai E1 C1
#? [1 1 (B2|B3)]

9 analyze_semeai L1 N1
#? [1 0 (O5|O4|O3|P3|Q2|Q1)]

10 analyze_semeai N1 L1
#? [1 1 (H2|K3)]

11 analyze_semeai A14 A12
#? [1 1 (A13|B13|D13|PASS)]

12 analyze_semeai A12 A14
#? [0 0 PASS]

loadsgf games/semeai/semeai8.sgf
13 analyze_semeai C1 E1
#? [0 0 PASS]

14 analyze_semeai E1 C1
#? [1 1 (B5|PASS)]

15 analyze_semeai L1 N1
#? [0 0 PASS]

16 analyze_semeai N1 L1
#? [1 1 (K5|PASS)]

17 analyze_semeai A14 A12
#? [0 0 PASS]

18 analyze_semeai A12 A14
#? [1 1 (A15|B15|D15|E15|PASS)]

loadsgf games/semeai/semeai9.sgf

19 analyze_semeai J1 L1
#? [1 1 (M3|N3|O3|P3|Q1)]

20 analyze_semeai L1 J1
#? [1 1 (H1|H2|H3|H4|H5|H6|H7|H8)]

21 analyze_semeai N19 O19
#? [1 1 (T17|T19)]

22 analyze_semeai O19 N19
#? [1 1 (L16|L17|L18|M19)]

loadsgf golois/Goemate990902-1.sgf

23 analyze_semeai G12 G13
#? [1 1 (F15|G15|H15|E13|F13|PASS)]

24 analyze_semeai G13 G12
#? [0 0 PASS]

25 analyze_semeai S8 R8
#? [1 1 S9]

26 analyze_semeai R8 S8
#? [1 1 S9]

# If this semeai is treated as a strictly local
# problem (ignoring the R8 dragon) then R7 can't live.
# But the semeai code treats it as a local problem by
# design!  So it's unclear what the correct answer
# should be.  A similar remark holds with problem 28.

27 analyze_semeai Q7 R7
#? [1 1 S9]

28 analyze_semeai R7 Q7
#? [1 1 S9]

# A6 gives an unfavorable ko while F10 gives seki.
# Since there are no ko threats, and F10 is enough to win, it is preferred.
loadsgf games/semeai/semeaiko1.sgf
29 reg_genmove black
#? [F10]*

loadsgf games/strategy11.sgf 127
30 analyze_semeai B3 G4
#? [1 1 C1]

loadsgf games/strategy11.sgf 127
31 analyze_semeai G4 B3
#? [1 1 C1]

loadsgf games/nicklas/nicklas14.sgf 55
32 analyze_semeai B8 D9
#? [1 0 B6]*

loadsgf games/nicklas/nicklas14.sgf 55
33 analyze_semeai D9 B8
#? [1 1 B7]

# S18 produces a favorable ko. T18 makes seki.
loadsgf games/bretz.sgf 130
34 analyze_semeai N18 Q18
#? [1 1 S18]

# ab added (3.1.22)
loadsgf games/mertin13x13/gnugo-goliath2.W+38.sgf 61
35 analyze_semeai M13 M11
#? [1 1 (PASS|N13|N10|H11)]

# See also reading:166
loadsgf games/nngs/Lazarus-gnugo-3.1.19-200201092246.sgf 66
36 analyze_semeai S9 R11
#? [1 1 R12]*

loadsgf games/semeai/semeai10.sgf
37 analyze_semeai B11 L7
#? [1 1 B12]

loadsgf games/semeai/semeai11.sgf
38 analyze_semeai B9 B8
#? [1 1 (PASS|B1|A9)]

# See also global:3.
loadsgf golois/Aya991113-13.sgf
39 analyze_semeai R12 H9
#? [2 2 H4]

40 analyze_semeai H9 R12
#? [1 1 (J4|H4)]

loadsgf games/semeai/semeai9.sgf
41 analyze_semeai A3 A5
#? [1 1 (PASS|B3|A6|B6|C5|D4|E3|E2|E1)]
42 analyze_semeai A5 A3
#? [0 0 PASS]
43 analyze_semeai A16 A12
#? [1 1 (A15|A11|B11|C14|C13|D12|D11|D10|D9|B9|A9)]
44 analyze_semeai A12 A16
#? [1 1 (A17|B17|C16|D16|E15|E14|F13|F12|F11|F10|F9|F8|B9|A9)]

loadsgf games/semeai/semeai12.sgf
45 analyze_semeai C3 A9
#? [0 0 PASS]

loadsgf games/semeai/semeai9.sgf
46 analyze_semeai Q12 S11
#? [1 0 (PASS|R8|R9|T11|T9|R11|R10)]
47 analyze_semeai S11 Q12
#? [1 0 (PASS|T11|T9|R9|T10|R10|R11)]*

loadsgf games/nngs/gnugo-3.3.20-GoFuN-200306071813.sgf 234
48 analyze_semeai E17 D17
#? [1 0 (D8|F8|G12|G10)]


# The opposite semeai is not very interesting (black clearly can kill white).
loadsgf games/nngs/jypower-gnugo-3.3.17-200304150031.sgf 166
50 analyze_semeai O7 L7
#? [1 1 (J5|J6|K7|J4|G5|G6|H7)]

# See reading:182,183 for comments.
loadsgf games/reading41.sgf 118
51 analyze_semeai C19 B16
#? [1 0 (A19|E11)]
52 analyze_semeai B16 C19
#? [1 3 A17]*

# See reading:184-187 for comments.
loadsgf games/reading41.sgf 130
53 analyze_semeai C19 B16
#? [1 0 E11]
54 analyze_semeai B16 C19
#? [1 2 A16]

#Test 55-60 for nakade moves during semeai
loadsgf games/test3win.sgf
55 analyze_semeai F4 F7
#? [1 1 H6]

loadsgf games/test3lose.sgf
56 analyze_semeai F4 F7
#? [0 0 PASS]

loadsgf games/test4win.sgf
57 analyze_semeai C5 H6
#? [1 1 J4]

# See also reading:195.
loadsgf games/test4lose.sgf
58 analyze_semeai F5 H6
#? [0 0 PASS]

loadsgf games/test5win.sgf
59 analyze_semeai B7 E6
#? [1 1 H5]

loadsgf games/test5lose.sgf
60 analyze_semeai C8 E6
#? [0 0 PASS]

# A15 is a certain seki. A17 gives a ko for life and death of both.
# The ko is very indirect for white, but it's still a ko. B17
# naturally dies right out.
loadsgf games/nngs/tommmal-gnugo-3.4-200308200805.sgf 122
61 analyze_semeai D19 B19
#? [1 0 A15]

# A17 kills cleanly with a bent four in the corner. A15 gives a bad
# ko. See also tests 63-70.
62 analyze_semeai B19 D19
#? [1 1 A17]

# Variations of bent four in the corner positions considered as semeai.
loadsgf games/semeai/semeai13.sgf
63 analyze_semeai D13 B13
#? [0 0 PASS]
64 analyze_semeai B13 D13
#? [1 1 (PASS|A13|E13|E12|C11|C10|B9|A9)]
65 analyze_semeai K13 M13
#? [0 0 PASS]
66 analyze_semeai M13 K13
#? [1 1 (PASS|N13)]
67 analyze_semeai A4 A2
#? [0 0 PASS]
68 analyze_semeai A2 A4
#? [1 1 (PASS|A5|B5|C3|D3)]
69 analyze_semeai M4 N3
#? [0 0 PASS]
70 analyze_semeai N3 M4
#? [1 1 (PASS|L3)]

# A basic seki with varying number of outer liberties.
loadsgf games/semeai/semeai14.sgf
71 analyze_semeai D13 B13
#? [1 0 PASS]
72 analyze_semeai B13 D13
#? [1 0 PASS]
73 analyze_semeai M13 N12
#? [1 0 PASS]
74 analyze_semeai N12 M13
#? [1 0 (PASS|L13)]
75 analyze_semeai J3 L2
#? [1 0 PASS]
76 analyze_semeai L2 J3
#? [1 0 (PASS|N3|K2)]
77 analyze_semeai A5 A3
#? [1 0 PASS]
78 analyze_semeai A3 A5
#? [1 0 (PASS|A6|C6|C1)]
79 analyze_semeai F9 F7
#? [1 0 PASS]
80 analyze_semeai F7 F9
#? [1 0 (PASS|E9|E8|J9|H5)]

loadsgf games/semeai/semeai15.sgf
81 dragon_status H7
#? [dead]
82 dragon_status J7
#? [alive]
83 dragon_status J9
#? [dead]
84 dragon_status E13
#? [dead]
85 dragon_status A13
#? [alive]
86 dragon_status C11
#? [critical A2 A2]

loadsgf games/semeai/semeai16.sgf 222
87 analyze_semeai N19 S18
#? [1 1 (Q19|S19|T18|T16)]
88 analyze_semeai S18 N19
#? [2 0 T12]*
89 reg_genmove black
#? [Q19|S19|T18|T16]*

loadsgf games/semeai/semeai16.sgf 224
90 analyze_semeai N19 S18
#? [1 3 (Q19|S19|T18|T16)]*
91 analyze_semeai S18 N19
#? [2 0 Q17]*
92 reg_genmove black
#? [Q19|S19|T18|T16]

loadsgf games/semeai/semeai16.sgf 226
93 analyze_semeai N19 S18
#? [1 3 S19]*
94 analyze_semeai S18 N19
#? [1 1 S19]
95 reg_genmove black
#? [S19]

loadsgf games/semeai/semeai16.sgf 230
96 analyze_semeai N19 S18
#? [1 0 S19]
97 analyze_semeai S18 N19
#? [1 1 S19]*
98 reg_genmove black
#? [S19]

loadsgf games/nngs/beedee-gnugo-3.5.3-200401140035.sgf 280
99 analyze_semeai R13 Q13
#? [1 0 (L12|K9|M6|L7|S17)]
100 analyze_semeai Q13 R13
#? [1 1 (P10|Q11|S10|T11)]

loadsgf games/semeai/semeai17.sgf 52
101 analyze_semeai D9 C6
#? [1 1 (D6|C7)]
102 analyze_semeai C6 D9
#? [1 1 (D6|C7)]
103 analyze_semeai D9 E9
#? [1 1 (D6|C7)]
104 analyze_semeai E9 D9
#? [1 1 (D6|C7)]
105 analyze_semeai G8 E9
#? [1 1 (D6|C7)]*
106 analyze_semeai E9 G8
#? [1 0 (D6|C7)]*

# Doubtful whether C7 should be accepted even if it kills all white.
107 reg_genmove black
#? [D6|C7]

loadsgf games/semeai/semeai17.sgf 60
108 analyze_semeai G8 H2
#? [1 1 J2]*
109 analyze_semeai H2 G8
#? [1 1 E1]

loadsgf games/semeai/semeai17.sgf 62
110 analyze_semeai G8 H2
#? [1 0 (PASS|F3|E1)]
111 analyze_semeai H2 G8
#? [1 0 (PASS|F3|E1)]

loadsgf games/semeai/semeai17.sgf 64
112 analyze_semeai G8 H2
#? [1 0 PASS]
113 analyze_semeai H2 G8
#? [1 0 PASS]
114 dragon_status G8
#? [alive]
115 dragon_status H2
#? [alive]

loadsgf games/kgs/yagr-nigiri.sgf 214
116 analyze_semeai F19 F16
#? [3 3 E16]*

loadsgf games/semeai/semeai18.sgf
117 analyze_semeai H3 H5
#? [0 0 PASS]

play white H2
118 analyze_semeai H3 H5
#? [1 0 (H1|G1)]

# Black H1 doesn't work. White plays G1 and gets seki or ko.
119 analyze_semeai H5 H3
#? [1 1 G1]

# A14 is strictly correct since C19 allows W an unfavorable ko.
loadsgf games/semeai/semeai19.sgf
120 analyze_semeai C18 C17
#? [0 0 PASS]*

# There is a complication that B18 and C17 are not amalgamated.
# If B plays first C19 gives a favorable ko; A15 allows seki.
# The ko is very unfavorable for W so in 119 a 1 1 result is 
# arguably correct.
loadsgf games/semeai/semeai20.sgf
121 analyze_semeai C17 C18
#? [1 0]*
122 analyze_semeai C18 C17
#? [2 2 C19]*

# There is also an amalgamation problem here.
loadsgf games/semeai/semeai19.sgf 80
123 analyze_semeai K18 N18
#? [1 1 M18]*

loadsgf games/verybad.sgf 104
124 analyze_semeai Q16 Q17
#? [1 1 P19]
125 analyze_semeai Q17 Q16
#? [3 3 T17]*

loadsgf games/verybad.sgf 114
126 analyze_semeai Q16 Q17
#? [2 2 (P19|S17|T17)]
127 analyze_semeai Q17 Q16
#? [2 2 (O15|O14|R13)]

# Take the ko last.
128 restricted_genmove black T15 T17 S17 P19
#? [!T15]*

loadsgf games/verybad.sgf 118
129 analyze_semeai Q16 Q17
#? [3 3 (Q19|S17|T17)]

loadsgf games/owl54.sgf
130 analyze_semeai D3 G2
#? [0 0 PASS]

loadsgf games/kgs/GNU-minautore.sgf 80
131 dragon_status P8
#? [critical T8 (T8|T10|T11|T12)]

# J8 leaves ko aji. (Admittedly a very bad ko for black but still a ko.)
loadsgf games/CrazyStone1.sgf 50
132 reg_genmove white
#? [H9|J4]

# After white H2 there's nothing black can do. Eventually white will
# have to capture a ko but this can be postponed indefinitely so just
# like the bent four in the corner, black is dead without further
# moves, with Japanese rules.
loadsgf games/CrazyStone1.sgf 56
133 analyze_semeai D9 G3
#? [1 1 H2]*
134 analyze_semeai G3 D9
#? [1 1 H2]

# See also strategy3:146
loadsgf games/incident209.sgf 259
135 analyze_semeai R12 Q12
#? [1 0 R11]

# See also kgs:230
loadsgf games/kgs/yagr-Kazik.sgf 149
136 analyze_semeai K19 L19
#? [0 1 (N16|O13|O12|O11)]*

loadsgf games/semeai/semeai21.sgf
137 analyze_semeai A5 A2
#? [1 0 F1]*
138 analyze_semeai A2 A5
#? [1 1 (D1|F1|H1)]*

# See also owl1:393,394.
139 analyze_semeai F9 H9
#? [1 1 G9]
140 analyze_semeai H9 F9
#? [1 1 (E9|G7)]

loadsgf games/semeai/semeai22.sgf 1
141 analyze_semeai A4 E3
#? [2 2 (F1|F2|F3)]
142 analyze_semeai E3 A4
#? [2 2 (A1|A3)]*

# White has no local move but can pass for a good ko.
# See also reading:224-227.
loadsgf games/semeai/semeai22.sgf 7
143 analyze_semeai A4 E3
#? [3 3 (F1|F3)]
144 analyze_semeai E3 A4
#? [2 2 PASS]

# See also 9x9:640.
loadsgf games/cgos/25811.sgf 52
145 analyze_semeai E9 B8
#? [2 2 PASS]*
146 analyze_semeai B8 E9
#? [3 3 (A7|B9)]*

# Underlying amalgation problem, see reading:232.
loadsgf games/semeai/semeai6.sgf
play white A15
play black D13
147 analyze_semeai A12 A14
#? [1 1 (B15|C15|D15|E15)]

# Multiple semeai misunderstandings. First let the outer white stones
# be safe.
loadsgf games/kgs/Hosbodar-GNU.sgf 193
play white N6
play white O7
148 analyze_semeai T7 S9
#? [1 1 T9]

# A big eye is worth lots of liberties. Let's eliminate the tiny ko
# potential for simplicity.
loadsgf games/kgs/Hosbodar-GNU.sgf 197
play black N8
149 analyze_semeai S9 R10
#? [1 1 (PASS|T11|S11|R11|P9|P8|P7|P1|Q1|R1)]
150 analyze_semeai R10 S9
#? [0 0 PASS]
151 dragon_status R10
#? [dead]*
152 dragon_status S9
#? [alive]*
153 dragon_status T7
#? [dead]

# One eye beats no eye, in particular if the one-eyed dragon is shaped
# like a nakade.
loadsgf games/zheng.sgf 2
154 analyze_semeai A4 B2
#? [1 0 B1]
155 reg_genmove white
#? [B1]
