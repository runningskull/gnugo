# incident 66
loadsgf games/incident66.sgf 52
1 owl_attack D8
#? [1 (F8|E8|E9)]
2 owl_defend D8
#? [1 (E9|E8|F8)]

# incident 85
loadsgf games/incident85.sgf 104
3 owl_attack B11
#? [1 (C12|B14)]
4 owl_defend B11
#? [1 C12]

# incident 159
# Recent analysis has shown that white actually cannot live.
# After white B5, black can kill with
# Black C4 - B4 - B2,
# a) W A2 - B1 - C1 - B3 - A3 - A5
# b) W B1 - B2 - B6 - A6
# The attack test has been commented out. (3.5.1)
loadsgf games/incident156.sgf 246
# 5 owl_attack C3
# #? [1 (B4|C4)]
6 owl_defend C3
#? [0]*

# incident 160
loadsgf games/incident156.sgf 250
7 owl_attack C3
#? [1 (B4|B5)]
8 owl_defend C3
#? [1 B5]

loadsgf games/incident156.sgf 249
9 owl_attack C3
#? [1 (B4|B2)]
10 owl_defend C3
#? [1 A2]

# incident 236 (slightly modified)
loadsgf games/incident236.sgf 116
11 owl_attack P5
#? [0]
12 owl_defend P5
#? [1 (PASS|P6)]

# incident 237
loadsgf games/incident237.sgf 42
13 owl_attack A4
#? [0]

# incident 235
loadsgf games/incident235.sgf 110
14 owl_attack Q4
#? [1 (Q1|S2)]
15 owl_defend Q4
#? [1 S2]

# incident 234
loadsgf games/incident234.sgf 164
16 owl_attack C18
#? [1 A18]
17 owl_defend C18
#? [1 (A18|C19|E19)]

# The attack at B2 only gives ko. A2 looks more effective.
loadsgf games/owl01.sgf
18 owl_attack C3
#? [1 A2]
19 owl_defend C3
#? [1 A2]

# incident 131
loadsgf games/incident121.sgf 110
20 owl_attack B3
#? [1 E2]
21 owl_defend B3
#? [1 F2]

# incident 133
loadsgf games/incident121.sgf 122
22 owl_attack P12
#? [1 (PASS|S11|S14|N13|S13)]
23 owl_defend P12
#? [0]

# incident 154
loadsgf games/incident153.sgf 205
24 owl_attack F3
#? [1 (J3|F2|F1)]
25 owl_defend F3
#? [1 F1]

# incident 161
# tm added C9 option to kill.
loadsgf games/incident161.sgf 38
26 owl_attack B8
#? [1 (B7|C7|C9|PASS)]
27 owl_defend B8
#? [0]

# incident 162
loadsgf games/incident161.sgf 42
28 owl_attack B8
#? [1 C7]
29 owl_defend B8
#? [1 C7]
30 owl_attack D8
#? [1 C7]
31 owl_defend D8
#? [1 C7]

# incident 189
loadsgf games/incident189.sgf 180
32 owl_attack J19
#? [0]

# incident 198
loadsgf games/incident198.sgf 54
33 owl_attack G5
#? [1 H5]
34 owl_defend G5
#? [1 H5]

# incident 201
loadsgf games/incident201.sgf 271
35 owl_attack O18
#? [1 P19]
36 owl_defend O18
#? [1 Q19]

# incident 213
loadsgf games/incident211.sgf 40
37 owl_attack G7
#? [1 (PASS|F7|F6)]
38 owl_defend G7
#? [0]

loadsgf games/incident211.sgf 39
39 owl_attack G7
#? [1 G8]
40 owl_defend G7
#? [1 G8]

# incident 218
loadsgf games/incident218.sgf 136
41 owl_attack E4
#? [1 F1]
42 owl_defend E4
#? [1 (F1|D1|B1)]

# incident 239
# reading code finds B1 but owl code is not very ko-aware
loadsgf games/incident239.sgf 58
43 owl_attack A3
#? [1 B1]
44 owl_defend A3
#? [1 A5]

loadsgf games/owl02.sgf
# note the dragons B3 and F2 are not amalgamated
45  owl_attack B3
#? [1 (E3|F1)]
46 owl_defend B3
#? [1 (F1|B1|D2|E3|E1)]
47  owl_attack F2
#? [1 (E3|F1)]
48 owl_defend F2
#? [1 (F1|B1|D2|E1)]

# incident 240
# a good lunch example
loadsgf games/incident240.sgf 65
49 owl_attack C6
#? [0]

loadsgf games/owl03.sgf
# Gets it right if one more liberty (i.e. remove B17)
50 owl_attack D19
#? [1 E19]
51 owl_defend D19
#? [1 (E19|B19)]

loadsgf games/owl04.sgf
52 owl_attack Q7
#? [1 S1]
53 owl_defend Q7
#? [1 S1]

# incident 199
loadsgf games/incident199.sgf 42
54 owl_attack F4
#? [1 (H2|J2)]
55 owl_defend F4
#? [1 (H2|G2)]

# incident 202
# This is possibly more of a semeai problem. Subtle!
loadsgf games/incident201.sgf 297
56 owl_attack M19
#? [1 (Q19|T13)]
57 owl_defend M19
#? [1 (T13|N19)]*
58 owl_attack T19
#? [1 (T13|N19)]
59 owl_defend T19
#? [1 (Q19|T13)]

# incident 91
loadsgf games/incident91.sgf 68
60 owl_attack S3
#? [1 (S2|S1|T2)]
61 owl_defend S3
#? [1 S2]

# incident 75
loadsgf games/incident74.sgf 238
62 owl_attack C4
#? [1 (B4|B5)]

# incident 253
loadsgf games/incident253.sgf 77
63 owl_defend R14
#? [1 R9]

# incident 254
loadsgf games/incident254.sgf 147
64 owl_defend D16
#? [0]

# incident 256
# Dragon amalgamation is a problem here.
loadsgf games/incident256.sgf 157
65 owl_defend O9
#? [1 M11]

# incident 258
# It's possible that this defense does in fact not work, but then
# there's some mistake during the following moves.
loadsgf games/incident258.sgf 147
66 owl_defend C4
#? [1 B5]

# incident 260
loadsgf games/incident258.sgf 283
67 owl_attack B19
#? [1 H19]
68 owl_defend B19
#? [1 D19]

# incident 261
loadsgf games/incident261.sgf 108
69 owl_defend M12
#? [1 N7]

# incident 263
loadsgf games/incident263.sgf 32
70 owl_attack M4
#? [1 (J2|K2|N2)]
71 owl_defend M4
#? [1 (J2|O2)]

# incident 264
loadsgf games/incident263.sgf 116
72 owl_defend D13
#? [1 C14]

# incident 265
# see also test 134
# R18 is refuted by S17
loadsgf games/incident263.sgf 302
73 owl_defend S14
#? [1 (R17|S18|T14)]

# incident 266
loadsgf games/incident263.sgf 304
74 owl_defend S14
#? [1 (R18|S18|T14|R17)]

# incident 268
loadsgf games/incident267.sgf 54
75 owl_defend B6
#? [0]

# incident 289
loadsgf games/incident287.sgf 251
76 owl_attack P12
#? [1 (O10|N13)]

# incident 105
loadsgf games/incident104.sgf 169
77 owl_defend J19
#? [1 J13]

# More of a semeai problem.
loadsgf games/incident114.sgf 82
78 owl_attack D7
#? [0]

# incident 293
# M5 is clearly inferior, as is L5. 
loadsgf games/incident291.sgf 86
79 owl_defend K4
#? [1 (M6|L6|L7)]

# incident 297
loadsgf games/incident297.sgf 95
80 owl_attack G5
#? [1 J2]

loadsgf games/strategy5.sgf 40
81 owl_attack R6
#? [0]

loadsgf games/strategy5.sgf 40
82 owl_defend R6
#? [1 .+]

loadsgf games/strategy7.sgf 23
83 owl_defend B3
#? [1 (A4|A2|B1)]

loadsgf games/nicklas/nicklas8.sgf 78
84 owl_attack C17
#? [1 (B17|B18|C18)]
85 owl_defend C17
#? [1 (B17|B18|C18)]

loadsgf games/nicklas/nicklas8.sgf 96
86 owl_attack Q13
#? [!1 T13]
87 owl_attack P15
#? [1 P16]

loadsgf games/owl05.sgf 238
88 owl_attack D19
#? [1 (F18|H19)]
89 owl_defend D19
#? [1 (F18|H19)]

loadsgf games/owl06.sgf
90 owl_defend C8
#? [0]

loadsgf games/strategy5.sgf 40
91 owl_attack Q18
#? [0]

loadsgf games/strategy11.sgf 93
92 owl_attack B3
#? [1 (A2|H1|C1)]
93 owl_defend B3
#? [1 (A2|C1)]

loadsgf games/strategy11.sgf 245
94 owl_attack B3
#? [1 E1]
95 owl_defend B3
#? [1 (E1|J1)]

loadsgf games/incident42.sgf 284
96 owl_attack N18
#? [1 Q11]
97 owl_defend N18
#? [1 Q11]

loadsgf games/owl07.sgf
98 owl_attack B18
#? [1 (A18|F19|B19)]
99 owl_defend B18
#? [1 A18]

loadsgf games/strategy14.sgf 53
100 owl_attack Q18
#? [1 S16]
101 owl_defend Q18
#? [1 S15]

loadsgf games/strategy14.sgf 81
102 owl_attack Q6
#? [1 P6]
103 owl_defend Q6
#? [1 (P6|S5)]

# The best attack seems to involve a ko. There's also a strange
# variation starting with white A16 that leads to a genuine triple ko.
# In fact this is more of a semeai problem but is included here to
# make sure that the owl code doesn't try to attack with the
# suicidal C9 move.
loadsgf games/strategy14.sgf 211
104 owl_attack B12
#? [!1 C9]

loadsgf games/owl08.sgf
105 owl_attack C4
#? [0]

loadsgf games/owl09.sgf
106 owl_attack F6
#? [1 (B2|E1)]
107 owl_defend F6
#? [1 B2]

loadsgf games/jahy.sgf 36
# B4 is preferred since C4 may become ko
108 owl_attack C3
#? [1 (B4|C4)]
109 owl_defend C3
#? [1 (B5|C4)]*

# incident 287
loadsgf games/incident287.sgf 103
110 owl_defend F3
#? [1 (B2|D1|C3)]

# incident 177 (from the disastrous Indigo game)
loadsgf games/incident169.sgf 184
111 owl_attack A17
#? [1 F14]*
112 owl_defend A17
#? [1 (F14|M16)]

# incident 178 (from the disastrous Indigo game)
# Possibly more of a semeai problem.
loadsgf games/incident169.sgf 210
113 owl_attack L2
#? [1 K2]
114 owl_defend L2
#? [1 K2]

loadsgf games/ko1.sgf
115 owl_attack L5
#? [2 (L1|H1)]*
116 owl_defend L5
#? [1 L1]

loadsgf games/owl10.sgf
117 owl_attack N11
#? [1 (G12|L14|N13)]

loadsgf games/owl11.sgf 126
118 owl_attack H5
#? [0]*

loadsgf games/strategy18.sgf
119 owl_attack G6
#? [1 (D4|E6|F6|F5|H6|J5|H5)]
120 owl_defend G6
#? [1 E6]

loadsgf games/strategy17.sgf
121 owl_attack K19
#? [0]

loadsgf games/strategy20.sgf 236
122 owl_attack B6
#? [1 (A6|A5|A4|A3|B1|C5)]
# The defense at B1 doesn't work due to black A6.
123 owl_defend B6
#? [1 C5]

loadsgf games/strategy21.sgf 96
124 owl_defend F4
#? [1 H2]

loadsgf games/incident108b.sgf 178
125 owl_attack P16
#? [1 P15]

loadsgf games/owl12.sgf
126 owl_attack J12
#? [1 (K7|K8|L8|L7)]
127 owl_defend J12
#? [1 L8]

loadsgf games/owl13.sgf 130
128 owl_attack P19
#? [1 (S18|T18|T19|P17)]
129 owl_defend P19
#? [1 T18]

loadsgf games/owl14.sgf 250
130 owl_attack B11
#? [1 A10]
131 owl_defend B11
#? [1 A10]

loadsgf games/owl15.sgf
132 owl_attack Q3
#? [(1|2) R4]*
133 owl_defend O3
#? [1 M3]

# see also test 73
loadsgf games/incident263.sgf 302
134 owl_attack S14
#? [1 (S17|S18)]

loadsgf games/owl16.sgf
# attack at N17 almost works
135 owl_attack O18
#? [0]

# Actually, white is alive.  If black F14, white B12 lives.
loadsgf games/owl17.sgf
136 owl_attack D15
#? [0]

loadsgf games/incident96.sgf 71
137 owl_attack B4
#? [1 (E2|B1)]
# J5 seems to work but is inferior to E2 since it loses a point and
# some aji.
138 owl_defend B4
#? [1 E2]

loadsgf games/nicklas/nicklas19.sgf 60
139 owl_attack H8
#? [1 (G9|H9|J8|J7)]
140 owl_defend H8
#? [1 (H9|J8|J9)]

loadsgf games/nicklas/nicklas19.sgf 61
141 owl_attack H8
#? [3 J8]
142 owl_defend H8
#? [1 (H9|J8|J9|H7|J7)]

loadsgf games/nicklas/nicklas19.sgf 62
143 owl_attack H8
#? [1 H7]
144 owl_defend H8
#? [2 H7]

loadsgf games/nicklas/nicklas19.sgf 63
145 owl_attack H8
#? [3 H9]
146 owl_defend H8
#? [1 (H9|J7)]

loadsgf games/owl18.sgf 140
147 owl_attack E13
#? [1 A5]
148 owl_defend E13
#? [1 A5]

# J3 cannot be defended.
loadsgf games/FSF-neurogo.sgf 40
149 owl_defend J3
#? [0]

# There might be an attack at G13, but it looks like it fails.
loadsgf games/strategy25.sgf 63
150 owl_attack D15
#? [0]*

# Now G13 works.
loadsgf games/strategy25.sgf 67
151 owl_attack D15
#? [1 G13]

# More moves might work.
loadsgf games/strategy25.sgf 197
152 owl_attack J3
#? [1 F2]
153 owl_defend J3
#? [1 (E2|G3)]

loadsgf games/strategy25.sgf 250
154 owl_attack M19
#? [2 O15]
155 owl_defend M19
#? [1 (O15|P16|Q15|N16)]

loadsgf games/strategy25.sgf 251
156 owl_attack M19
#? [1 O13]*
157 owl_defend M19
#? [3 (P16|Q15)]*

# There might be some attack here, but at least both S16 and S15 fail.
loadsgf games/strategy26.sgf 51
158 owl_attack Q19
#? [0]
159 owl_defend Q19
#? [1 (S15|S19|S17|PASS)]

# A15 and C15 both fail badly. C16 allows white to make seki.
loadsgf games/strategy26.sgf 285
160 owl_defend D19
#? [1 B16]

loadsgf games/strategy27.sgf 190
161 owl_attack F6
#? [0]

loadsgf games/strategy27.sgf 214
162 owl_attack F19
#? [1 D18]
163 owl_defend F19
#? [1 (B18|D18|D17|C19)]

loadsgf games/owl20.sgf
164 owl_attack R3
#? [1 P1]
165 owl_defend R3
#? [1 (R1|S1|S2)]

loadsgf games/owl21.sgf
166 owl_attack Q17
#? [2 P18]*

loadsgf games/owl22.sgf 138
167 owl_defend Q12
#? [0]
168 owl_defend Q9
#? [0]

loadsgf games/poka.sgf 42
169 owl_defend B8
#? [0]

loadsgf games/owl23.sgf
# This isolates the problem in test 4
170 owl_attack B14
#? [0]*

# incident 284
loadsgf games/incident278.sgf 318
171 owl_attack A6
#? [3 (B3|C1|A1)]*
172 owl_defend A6
#? [1 (B3|C1)]

# incident 97
loadsgf games/incident97.sgf 175
173 owl_attack R14
#? [1 S11]
# S11 is superior to S13, but both work.
174 owl_defend R14
#? [1 (S11|S13)]

# Black can do no better than seki with H17.
loadsgf games/incident225.sgf 254
175 owl_attack A19 
#? [0]

loadsgf games/incident240.sgf 103
176 owl_defend C4
#? [3 G1]

# incident 105
loadsgf games/incident104.sgf 169
177 owl_attack J19
#? [1 (G13|J13|G12|G14)]

# see also strategy3:122
loadsgf games/incident223.sgf 228
178 owl_attack R13
#? [1 (R19|T18)]*
179 owl_defend R13
#? [3 R19]*

loadsgf games/incident269.sgf 200
180 owl_attack R19
#? [1 (T16|T17|T18|S18)]

# tm (3.1.17)  T18 lives.  Note the W sente at R16.
181 owl_defend R19
#? [1 T18]

# A17 is a better defense than B18 because B18 gives W two ko threats
loadsgf games/incident144.sgf 242
182 owl_attack C18
#? [2 (A15|A17)]
183 owl_defend C18
#? [3 (A17|B18)]

# incident 212
# There may be a few more defenses.
loadsgf games/incident211.sgf 26
184 owl_attack F8
#? [1 H8]
185 owl_defend F8
#? [1 H8]

loadsgf games/explorer2.sgf 144
186 owl_defend M3
#? [0]

loadsgf games/explorer2.sgf 145
187 owl_attack M3
#? [1 O3]

loadsgf games/owl24.sgf 127
188 owl_attack F15
#? [1 (D16|C16|F14|E15)]
189 owl_defend F15
#? [1 D16]

loadsgf games/reading25.sgf
190 owl_defend C1
#? [0]

loadsgf games/nicklas/nicklas10.sgf 18
191 owl_defend D8
#? [1 B7]

# See also test case golife:7
loadsgf games/golife.sgf 46
192 owl_attack E5
#? [1 F3]*

loadsgf games/strategy33.sgf 15
193 owl_attack Q16
#? [0]

loadsgf games/strategy33.sgf 91
194 owl_attack D15
#? [1 G18]

loadsgf games/strategy33.sgf 119
195 owl_attack B11
#? [1 (B10|A9|C9|A10|A8)]
196 owl_defend B11
#? [1 (B10|A9)]
197 owl_attack D15
#? [1 (F18|G19)]
198 owl_defend D15
#? [1 F18]

loadsgf games/incident221.sgf 39
199 owl_defend H3
#? [0]

loadsgf games/dniwog.sgf 62
200 owl_defend B2
#? [1 D7]

loadsgf games/owl25.sgf 32
201 owl_defend H6
#? [1 H8]

loadsgf games/owl25.sgf 54
202 owl_defend H6
#? [0]

loadsgf games/owl25.sgf 60
203 owl_defend H6
#? [0]

loadsgf games/owl25.sgf 62
204 owl_attack H6
#? [1 (H9|E9)]
205 owl_defend H6
#? [3 H9]

loadsgf games/owl26.sgf 10
206 owl_attack C12
#? [0]

loadsgf games/owl26.sgf 138
207 owl_defend L6
#? [0]*

loadsgf games/incident248.sgf 228
208 owl_attack J2
#? [3 A2]
209 owl_defend J2
#? [1 (A2|F1|J1|A1|B1)]

loadsgf games/incident161.sgf 56
210 owl_attack C3
#? [1 C2]
211 owl_defend C3
#? [1 C2]

# Incident 190.
loadsgf games/incident189.sgf 186
212 owl_attack K13
#? [3 E16]
213 owl_defend K13
#? [1 (D18|E16|M16|E18)]

loadsgf games/owl27.sgf
214 owl_defend N3
#? [1 (L4|N5)]

# K19 only gives ko.
loadsgf games/FSF-neurogo.sgf 286
215 owl_attack J15
#? [1 H18]
216 owl_defend J15
#? [1 (H18|K19)]

loadsgf games/owl28.sgf 199
217 owl_attack C14
#? [1 A16]
218 owl_defend C14
#? [1 A16]

loadsgf games/filllib9.sgf 119
219 owl_defend K19
#? [0]

loadsgf games/filllib9.sgf 121
220 owl_defend K19
#? [0]

# Additionally D7, C8 and some other odd moves work. H9 does not.
loadsgf games/trevor/trevor_24.sgf 33
221 owl_defend H8
#? [1 (D8|E8)]

# E6 almost kills tactically.
loadsgf games/trevor/trevor_27.sgf 36
222 owl_attack E5
#? [1 E6]

loadsgf games/incident169.sgf 224
223 owl_defend R17
#? [3 S16]

loadsgf games/ego.sgf 198
224 owl_attack T17
#? [2 T18]*

loadsgf games/ego.sgf 198
225 owl_defend T17
#? [3 (Q19|S16)]*

loadsgf games/owl29.sgf 132
226 owl_attack J10
#? [2 O8]*

loadsgf games/owl30.sgf 172
227 owl_attack L19
#? [1 (Q19|P17)]

# The cut at D5 fails.
loadsgf games/nicklas/nicklas18.sgf 17
228 owl_attack B5
#? [0]

loadsgf games/incident240.sgf 69
229 owl_attack B12
#? [1 D9]

loadsgf games/owl31.sgf 20
230 owl_defend G7
#? [0]

loadsgf games/owl32.sgf
231 owl_attack A8
#? [1 (C6|A4)]
232 owl_defend A8
#? [3 C6]
233 owl_attack F6
#? [1 H7]*
234 owl_defend F6
#? [1 (J7|H7|H6|G6)]

# There are probably additional moves.
loadsgf games/owl33.sgf 134
235 owl_attack O4
#? [1 (S3|T4)]
236 owl_defend O4
#? [1 (R4|S3|R3|S2|Q5|P5)]

#SEE_ALSO=reading:170
loadsgf games/owl33.sgf 212
237 owl_defend S12
#? [0]

# See also optics:1801-1812
loadsgf games/marginal_ko.sgf
238 owl_attack B19
#? [1 (B17|A19)]
239 owl_defend B19
#? [2 B17]
240 owl_attack G19
#? [0]
241 owl_attack M19
#? [0]
242 owl_attack J15
#? [0]
243 owl_attack A11
#? [0]
244 owl_attack D11
#? [(2|3) F11]
245 owl_defend D11
#? [1 (F11|H8|F8|H5)]
246 owl_attack O9
#? [3 (M6|Q8)]
247 owl_defend O9
#? [1 (M6|Q8)]
248 owl_attack A6
#? [1 (J1|L2)]
249 owl_defend A6
#? [2 (J1|B7|B2)]
250 owl_attack R3
#? [1 (T3|R2)]
251 owl_defend R3
#? [3 T3]
252 owl_attack C2
#? [2 B2]
253 owl_defend C2
#? [1 B2]

# See also strategy2:100.
loadsgf games/strategy26.sgf 257
254 owl_defend O13
#? [1 (M16|O14|N16|N17)]

loadsgf games/owl34.sgf 211
255 owl_defend D13
#? [1 (D14|E13|G16)]

#CATEGORY=CONNECTION_IN_OWL
# Simple mistake. See also 13x13:60 or 13x13:23.
loadsgf games/mertin13x13/gnugo-katsunari1.B+21.sgf 15
256 owl_attack K12
#? [1 J12]

# H3 is the best W move on the board, but it does not capture J3.
loadsgf games/owl35.sgf
257 owl_attack J3
#? [0]*

# This is quite trivial, but at least GNU Go 3.1.31 fails it.
loadsgf games/handtalk/handtalk11.sgf 157
258 owl_defend O2
#? [0]

loadsgf games/handtalk/handtalk12.sgf 29
259 owl_does_attack L2 M3
#? [0]

# Far too many escape ways for F10 as it could be killed.
loadsgf games/handtalk/handtalk12.sgf 41
260 owl_does_attack G9 F10
#? [0]

# A good example where connection handling is important in owl.
loadsgf games/handtalk/handtalk23.sgf 186
261 owl_attack E5
#? [1 (O6|Q7|T4|T5|R4)]*

# See also 13x13:15.
loadsgf games/mertin13x13/gointellect-gnugo2.W+8.sgf 32
262 owl_defend E7
#? [0]*
