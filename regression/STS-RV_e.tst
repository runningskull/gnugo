# This suite of semeai problems came from the package STS-RV
# available at http://gobase.org/reading/preview/Semeai/#STS
#
# It is a very comprehensive semeai problems suite
# compiled by Ricard Vilà. The associated tests are in GTP
# format but it's not a perfect match for GNU Go because
# it uses a custom command called solve-semeaiS.
# (More info at http://trac.gnugo.org/gnugo/ticket/41)
#
# This file instead is based on the normal GNU Go commands
# (analyze_semeai) and enable the execution of the tests
# in the semeais_e.tst file from STS-RV suite of semeai problems.
#
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
# The result [0 1] is not possible as a correct result but GNU Go
# has been known to return that occasionally.
# Result 2 mean success with good ko, 3 success with bad ko.
# A good ko is characterized by the opponent having to make the first
# external ko threat whereas a bad ko is the opposite.
#
# NOTE: In some problem GNU Go suggest a move instead to PASS
# but the result of the semeai analysis is correct, so to avoid
# a fail not relevant for the test, the move is ignored using
# a regular expression: #? [x y (.*)]

loadsgf games/STS-RV/Class01eProblems/_semeai_Ce_001.sgf
1 analyze_semeai C19 D19
#? [1 1 B17]

2 analyze_semeai D19 C19
#? [1 1 B17]*

3 analyze_semeai A6 A5
#? [1 0 (.*)]

4 analyze_semeai A5 A6
#? [1 0 (.*)]

5 analyze_semeai T4 T3
#? [1 1 (S2|R1)]

6 analyze_semeai T3 T4
#? [1 0 (S2|R1)]

7 analyze_semeai T19 R19
#? [1 1 P18]

8 analyze_semeai R19 T19
#? [1 0 P18]

loadsgf games/STS-RV/Class01eProblems/_semeai_Ce_002.sgf
9 analyze_semeai A19 D19
#? [1 0 PASS]

10 analyze_semeai D19 A19
#? [1 0 PASS]

11 analyze_semeai N19 M19
#? [1 0 PASS]

12 analyze_semeai M19 N19
#? [1 0 PASS]

13 analyze_semeai T15 T16
#? [1 0 PASS]*

14 analyze_semeai T16 T15
#? [1 0 PASS]*

15 analyze_semeai A4 A9
#? [1 0 B6]

16 analyze_semeai A9 A4
#? [1 1 B6]

17 analyze_semeai Q1 R1
#? [1 0 O2]

18 analyze_semeai R1 Q1
#? [1 1 O2]

19 analyze_semeai O6 O5
#? [1 0 (.*)]

20 analyze_semeai O5 O6
#? [1 0 PASS]


loadsgf games/STS-RV/Class01eProblems/_semeai_Ce_003.sgf
21 analyze_semeai T9 T10
#? [0 0 PASS]

22 analyze_semeai T10 T9
#? [1 1 (.*)]

23 analyze_semeai F19 M19
#? [1 0 H18]

24 analyze_semeai M19 F19
#? [1 1 F16]

25 analyze_semeai D1 K1
#? [1 0 PASS]*

26 analyze_semeai K1 D1
#? [1 0 PASS]*

27 analyze_semeai D12 C12
#? [1 0 (.*)]

28 analyze_semeai C12 D12
#? [1 0 (.*)]

29 analyze_semeai J14 J11
#? [1 0 (K14|L13)]

30 analyze_semeai J11 J14
#? [1 1 (K14|L13)]

loadsgf games/STS-RV/Class01eProblems/_semeai_Ce_004.sgf
31 analyze_semeai A19 E19
#? [1 0 B18]

32 analyze_semeai E19 A19
#? [1 1 B18]

33 analyze_semeai M19 L19
#? [1 0 (.*)]

34 analyze_semeai L19 M19
#? [1 0 (.*)]

35 analyze_semeai H10 H9
#? [1 0 (.*)]

36 analyze_semeai H9 H10
#? [1 0 PASS]

37 analyze_semeai R13 T16
#? [1 0 PASS]

38 analyze_semeai T16 R13
#? [1 0 PASS]

39 analyze_semeai A3 F3
#? [1 0 C2]

40 analyze_semeai F3 A3
#? [1 1 A4]

41 analyze_semeai M6 O2
#? [1 0 O5]

42 analyze_semeai O2 M6
#? [1 1 O5]

loadsgf games/STS-RV/Class01eProblems/_semeai_Ce_005.sgf
43 analyze_semeai G19 L19
#? [1 0 (.*)]

44 analyze_semeai L19 G19
#? [1 0 (.*)]

45 analyze_semeai A14 A10
#? [1 0 PASS]

46 analyze_semeai A10 A14
#? [1 0 PASS]

47 analyze_semeai M13 Q13
#? [1 0 N10]

48 analyze_semeai Q13 M13
#? [1 1 N10]

49 analyze_semeai F8 E8
#? [1 0 G6]

50 analyze_semeai E8 F8
#? [1 1 G6]

loadsgf games/STS-RV/Class01eProblems/_semeai_Ce_006.sgf
51 analyze_semeai A19 E19
#? [1 0 (.*)]

52 analyze_semeai E19 A19
#? [1 0 PASS]*

53 analyze_semeai L19 K19
#? [1 0 N18]

54 analyze_semeai K19 L19
#? [1 1 N18]

55 analyze_semeai R13 R7
#? [1 0 PASS]*

56 analyze_semeai R7 R13
#? [1 0 PASS]*

57 analyze_semeai F7 F8
#? [1 0 PASS]

58 analyze_semeai F8 F7
#? [1 0 PASS]

loadsgf games/STS-RV/Class01eProblems/_semeai_Ce_007.sgf
59 analyze_semeai G19 K19
#? [1 0 H18]

60 analyze_semeai K19 G19
#? [1 1 H18]

61 analyze_semeai A13 E13
#? [1 0 PASS]*

62 analyze_semeai E13 A13
#? [1 0 PASS]*

63 analyze_semeai M13 P13
#? [1 0 (.*)]

64 analyze_semeai P13 M13
#? [1 0 (.*)]

65 analyze_semeai R6 P5
#? [1 0 S4]

66 analyze_semeai P5 R6
#? [1 1 S4]

loadsgf games/STS-RV/Class01eProblems/_semeai_Ce_008.sgf
67 analyze_semeai Q19 P19
#? [1 0 S17]

68 analyze_semeai P19 Q19
#? [1 1 S17]

69 analyze_semeai F13 F14
#? [1 0 PASS]

70 analyze_semeai F14 F13
#? [1 0 PASS]

71 analyze_semeai A7 A8
#? [1 0 B5]

72 analyze_semeai A8 A7
#? [1 1 B5]

73 analyze_semeai M6 M7
#? [1 0 N4]

74 analyze_semeai M7 M6
#? [1 1 N4]

loadsgf games/STS-RV/Class01eProblems/_semeai_Ce_009.sgf
75 analyze_semeai A19 E19
#? [1 0 (.*)]

76 analyze_semeai E19 A19
#? [1 0 PASS]*

77 analyze_semeai M13 M14
#? [1 0 (N11|O12)]

78 analyze_semeai M14 M13
#? [1 1 (N11|O12)]

79 analyze_semeai F7 F8
#? [1 0 PASS]

80 analyze_semeai F8 F7
#? [1 0 PASS]

loadsgf games/STS-RV/Class01eProblems/_semeai_Ce_010.sgf
81 analyze_semeai G19 E19
#? [0 0 PASS]

82 analyze_semeai E19 G19
#? [1 1 (.*)]

83 analyze_semeai A13 A14
#? [1 0 PASS]*

84 analyze_semeai A14 A13
#? [1 0 PASS]*

85 analyze_semeai M6 M8
#? [1 0 (O5|O4)]

86 analyze_semeai M8 M6
#? [1 1 O5]

loadsgf games/STS-RV/Class01eProblems/_semeai_Ce_011.sgf
87 analyze_semeai M19 L19
#? [1 0 PASS]*

88 analyze_semeai L19 M19
#? [1 0 PASS]*

89 analyze_semeai F13 F14
#? [1 0 H11]

90 analyze_semeai F14 F13
#? [1 1 H11]

91 analyze_semeai A7 A8
#? [1 0 C5]*

92 analyze_semeai A8 A7
#? [1 1 C5]*

loadsgf games/STS-RV/Class01eProblems/_semeai_Ce_012.sgf
93 analyze_semeai A19 E19
#? [1 0 B16]*

94 analyze_semeai E19 A19
#? [2 2 B16]*

95 analyze_semeai O13 O14
#? [1 0 (Q11|Q12)]

96 analyze_semeai O14 O13
#? [2 2 Q12]*
# GNU Go dont realize that w Q12 live seki

97 analyze_semeai G7 G8
#? [1 0 (J5|H5)]

98 analyze_semeai G8 G7
#? [2 2 H5]*
# GNU Go dont realize that w H5 live seki

loadsgf games/STS-RV/Class01eProblems/_semeai_Ce_013.sgf
99 analyze_semeai G19 E19
#? [3 0 J17]*

100 analyze_semeai E19 G19
#? [1 1 J17]

101 analyze_semeai A13 A14
#? [1 0 (.*)]

102 analyze_semeai A14 A13
#? [1 0 PASS]*

103 analyze_semeai O7 O8
#? [1 0 Q5]

104 analyze_semeai O8 O7
#? [1 1 Q5]

loadsgf games/STS-RV/Class01eProblems/_semeai_Ce_014.sgf
105 analyze_semeai O19 N19
#? [1 0 (Q18|P18)]

106 analyze_semeai N19 O19
#? [3 3 P18]*
# GNU Go dont realize that w P18 live seki

107 analyze_semeai G13 G14
#? [1 0 (J12|H11)]

108 analyze_semeai G14 G13
#? [1 1 (J12|H11)]

109 analyze_semeai A7 A8
#? [1 0 (B5|C5)]

110 analyze_semeai A8 A7
#? [2 2 C5]*

loadsgf games/STS-RV/Class01eProblems/_semeai_Ce_015.sgf
111 analyze_semeai A19 E19
#? [1 0 PASS]

112 analyze_semeai E19 A19
#? [1 0 PASS]

113 analyze_semeai O13 O14
#? [1 0 Q11]

114 analyze_semeai O14 O13
#? [1 1 Q11]

115 analyze_semeai G7 G8
#? [0 0 PASS]

116 analyze_semeai G8 G7
#? [1 1 (.*)]

loadsgf games/STS-RV/Class01eProblems/_semeai_Ce_016.sgf
117 analyze_semeai G19 L19
#? [2 2 J17]

118 analyze_semeai L19 G19
#? [1 1 H16]

119 analyze_semeai A4 A5
#? [1 0 C2]

120 analyze_semeai A5 A4
#? [1 1 C2]

121 analyze_semeai O7 O8
#? [0 0 PASS]

122 analyze_semeai O8 O7
#? [1 1 (.*)]

loadsgf games/STS-RV/Class01eProblems/_semeai_Ce_017.sgf
123 analyze_semeai O19 L19
#? [1 0 (Q19|P18)]

124 analyze_semeai L19 O19
#? [1 1 (Q19|P18)]

125 analyze_semeai G13 G14
#? [1 0 (.*)]

126 analyze_semeai G14 G13
#? [1 0 (.*)]

127 analyze_semeai A7 A8
#? [1 0 (B4|C6)]

128 analyze_semeai A8 A7
#? [1 1 (B4|E3)]

loadsgf games/STS-RV/Class01eProblems/_semeai_Ce_018.sgf
129 analyze_semeai A19 E19
#? [1 0 C17]

130 analyze_semeai E19 A19
#? [1 1 B18]

131 analyze_semeai O13 O14
#? [1 0 PASS]

132 analyze_semeai O14 O13
#? [1 0 PASS]

133 analyze_semeai G7 G8
#? [1 0 K5]

134 analyze_semeai G8 G7
#? [1 1 K5]

loadsgf games/STS-RV/Class01eProblems/_semeai_Ce_019.sgf
135 analyze_semeai G19 E19
#? [1 0 K17]

136 analyze_semeai E19 G19
#? [1 1 K17]

137 analyze_semeai A13 A14
#? [1 0 C11]

138 analyze_semeai A14 A13
#? [1 1 C11]

139 analyze_semeai O7 O8
#? [1 0 Q5]

140 analyze_semeai O8 O7
#? [1 1 Q5]

loadsgf games/STS-RV/Class01eProblems/_semeai_Ce_020.sgf
141 analyze_semeai O19 N19
#? [1 0 R18]

142 analyze_semeai N19 O19
#? [1 1 R18]

143 analyze_semeai G13 G14
#? [0 0 PASS]

144 analyze_semeai G14 G13
#? [1 1 (.*)]

145 analyze_semeai A7 A9
#? [1 0 C5]

146 analyze_semeai A9 A7
#? [1 1 C4]

loadsgf games/STS-RV/Class01eProblems/_semeai_Ce_021.sgf
147 analyze_semeai A19 F19
#? [1 0 PASS]

148 analyze_semeai F19 A19
#? [1 0 PASS]

149 analyze_semeai O13 N13
#? [1 0 (.*)]

150 analyze_semeai N13 O13
#? [1 0 (.*)]

151 analyze_semeai G7 G8
#? [1 0 J5]

152 analyze_semeai G8 G7
#? [1 1 J4]

loadsgf games/STS-RV/Class01eProblems/_semeai_Ce_022.sgf
153 analyze_semeai G19 F19
#? [1 0 (.*)]

154 analyze_semeai F19 G19
#? [1 0 PASS]

155 analyze_semeai A13 A8
#? [1 0 B11]

156 analyze_semeai A8 A13
#? [1 1 B11]

157 analyze_semeai O7 O8
#? [1 0 PASS]*

158 analyze_semeai O8 O7
#? [1 0 PASS]*

loadsgf games/STS-RV/Class01eProblems/_semeai_Ce_023.sgf
159 analyze_semeai O19 N19
#? [1 0 (Q18|R17)]

160 analyze_semeai N19 O19
#? [1 1 (Q18|R17)]

161 analyze_semeai G13 G14
#? [1 0 J11]

162 analyze_semeai G14 G13
#? [1 1 J11]

163 analyze_semeai A7 A8
#? [1 0 (.*)]

164 analyze_semeai A8 A7
#? [1 0 (.*)]

loadsgf games/STS-RV/Class01eProblems/_semeai_Ce_024.sgf
165 analyze_semeai A3 A4
#? [1 1 (A5|B5)]

166 analyze_semeai A4 A3
#? [1 1 F1]

167 analyze_semeai T3 T4
#? [1 1 (Q5|R5|S5|T5)]

168 analyze_semeai T4 T3
#? [1 1 (T2|S1|S2)]

169 analyze_semeai A17 A16
#? [1 1 A19]

170 analyze_semeai A16 A17
#? [1 1 D19]

171 analyze_semeai T17 T16
#? [0 0 PASS]

172 analyze_semeai T16 T17
#? [1 1 (.*)]

loadsgf games/STS-RV/Class01eProblems/_semeai_Ce_025.sgf
173 analyze_semeai A3 A4
#? [1 0 PASS]*

174 analyze_semeai A4 A3
#? [1 0 PASS]*

175 analyze_semeai T3 T4
#? [1 1 S2]

176 analyze_semeai T4 T3
#? [1 1 S2]*

177 analyze_semeai A17 A16
#? [1 1 (F19|E16)]

178 analyze_semeai A16 A17
#? [1 1 D19]

179 analyze_semeai T17 T16
#? [1 1 (P19|P18|P17)]

180 analyze_semeai T16 T17
#? [1 1 (S19|S18|T18)]

loadsgf games/STS-RV/Class01eProblems/_semeai_Ce_026.sgf
181 analyze_semeai A3 A4
#? [1 1 (C1|A1)]
# b A1 seem useless but is correct

182 analyze_semeai A4 A3
#? [1 1 A1]

183 analyze_semeai T4 T5
#? [3 3 T1]*

184 analyze_semeai T5 T4
#? [1 1 T1]

185 analyze_semeai A17 A16
#? [1 1 B18]

186 analyze_semeai A16 A17
#? [1 1 (D19|D18)]

187 analyze_semeai T17 T16
#? [1 1 T19]

188 analyze_semeai T16 T17
#? [1 0 T19]*

loadsgf games/STS-RV/Class01eProblems/_semeai_Ce_027.sgf
189 analyze_semeai A3 A4
#? [1 1 A1]

190 analyze_semeai A4 A3
#? [1 1 F1]

191 analyze_semeai T3 T4
#? [1 1 (.*)]

192 analyze_semeai T4 T3
#? [0 0 PASS]

193 analyze_semeai A17 A16
#? [1 1 A19]

194 analyze_semeai A16 A17
#? [1 1 D19]

195 analyze_semeai T17 T16
#? [1 1 (P19|P18)]

196 analyze_semeai T16 T17
#? [1 1 (S19|T18)]

loadsgf games/STS-RV/Class01eProblems/_semeai_Ce_028.sgf
197 analyze_semeai A15 A14
#? [1 0 A17]

198 analyze_semeai A14 A15
#? [1 1 A17]

199 analyze_semeai A2 A1
#? [1 0 A5]

200 analyze_semeai A1 A2
#? [1 1 A3]

201 analyze_semeai K1 H1
#? [1 0 L2]*

202 analyze_semeai H1 K1
#? [1 1 (M1|L2)]*

203 analyze_semeai T10 T11
#? [1 0 PASS]

204 analyze_semeai T11 T10
#? [1 0 PASS]

205 analyze_semeai Q19 R18
#? [1 0 O19]

206 analyze_semeai R18 Q19
#? [1 1 O19]

loadsgf games/STS-RV/Class01eProblems/_semeai_Ce_029.sgf
207 analyze_semeai A9 A8
#? [1 0 A11]

208 analyze_semeai A8 A9
#? [1 1 C13]

209 analyze_semeai Q1 P1
#? [1 0 S1]

210 analyze_semeai P1 Q1
#? [1 1 S1]

211 analyze_semeai T16 T17
#? [1 0 T14]

212 analyze_semeai T17 T16
#? [1 1 T14]

213 analyze_semeai K19 L19
#? [1 0 (.*)]

214 analyze_semeai L19 K19
#? [1 0 PASS]

loadsgf games/STS-RV/Class01eProblems/_semeai_Ce_030.sgf
215 analyze_semeai A15 A14
#? [1 0 A16]

216 analyze_semeai A14 A15
#? [1 1 A16]

217 analyze_semeai A1 D1
#? [1 0 (A2|B2)]

218 analyze_semeai D1 A1
#? [1 1 A2]

219 analyze_semeai K1 L1
#? [1 0 PASS]

220 analyze_semeai L1 K1
#? [1 0 PASS]

221 analyze_semeai T4 T3
#? [1 0 (T7|S5)]

222 analyze_semeai T3 T4
#? [1 1 T7]

223 analyze_semeai N19 M19
#? [1 0 (P19|O19)]

224 analyze_semeai M19 N19
#? [1 1 P19]

loadsgf games/STS-RV/Class01eProblems/_semeai_Ce_031.sgf
225 analyze_semeai A8 A7
#? [1 0 B9]*

226 analyze_semeai A7 A8
#? [1 1 (C12|C13)]*

227 analyze_semeai R1 S1
#? [1 0 (P1|N2)]*

228 analyze_semeai S1 R1
#? [1 1 P1]*

229 analyze_semeai T11 T10
#? [1 0 T14]*

230 analyze_semeai T10 T11
#? [1 1 T14]*

231 analyze_semeai L19 M19
#? [1 0 H18]*

232 analyze_semeai M19 L19
#? [1 1 H18]*

loadsgf games/STS-RV/Class01eProblems/_semeai_Ce_032.sgf
233 analyze_semeai A15 A14
#? [1 0 (A17|B17)]

234 analyze_semeai A14 A15
#? [1 1 D15]

235 analyze_semeai A3 D1
#? [1 0 B5]

236 analyze_semeai D1 A3
#? [1 1 B5]

237 analyze_semeai P1 Q1
#? [1 0 O1]

238 analyze_semeai Q1 P1
#? [1 1 (M2|O1)]

239 analyze_semeai T11 T12
#? [1 0 PASS]

240 analyze_semeai T12 T11
#? [1 0 PASS]

241 analyze_semeai L19 K19
#? [1 0 PASS]

242 analyze_semeai K19 L19
#? [1 0 PASS]

loadsgf games/STS-RV/Class01eProblems/_semeai_Ce_033.sgf
243 analyze_semeai A9 A8
#? [0 0 PASS]

244 analyze_semeai A8 A9
#? [1 1 (.*)]

245 analyze_semeai J1 K1
#? [1 0 (H1|G2|F2)]

246 analyze_semeai K1 J1
#? [1 1 (H1|G2|F2)]

247 analyze_semeai T5 T6
#? [0 0 PASS]

248 analyze_semeai T6 T5
#? [1 1 (.*)]

249 analyze_semeai T17 T18
#? [1 0 S16]

250 analyze_semeai T18 T17
#? [1 1 T14]*

251 analyze_semeai J19 K19
#? [1 0 PASS]

252 analyze_semeai K19 J19
#? [1 0 PASS]
