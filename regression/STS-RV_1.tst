# This suite of semeai problems came from the package STS-RV/Class01eProblems
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
# in the semeais_1.tst file from STS-RV suite of semeai problems.
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

loadsgf games/STS-RV/Class01eProblems/_semeai_C1_001.sgf
1 analyze_semeai J19 L19
#? [0 0 PASS]

2 analyze_semeai L19 J19
#? [1 1 (PASS|F19|G18)]

3 analyze_semeai C10 C9
#? [1 1 (D8|C3|B3|A3)]

4 analyze_semeai C9 C10
#? [1 1 (A10|A12|B11|A8|B8|B12)]

5 analyze_semeai K3 L3
#? [1 1 (M4|N4|R3|R2|R1)]

6 analyze_semeai L3 K3
#? [1 1 (J2|K1|H1|M2|M1|H2)]

7 analyze_semeai Q10 Q11
#? [1 1 (T16|S16|R16|Q15|Q14)]

8 analyze_semeai Q11 Q10
#? [1 1 (S8|T9|T7|R11|S11|T11|S9)]


loadsgf games/STS-RV/Class01eProblems/_semeai_C1_002.sgf
9 analyze_semeai B18 A18
#? [1 1 (D19|D18)]

10 analyze_semeai A18 B18
#? [1 1 A17]

11 analyze_semeai S18 T18
#? [1 1 (Q19|Q18)]

12 analyze_semeai T18 S18
#? [1 1 T17]

13 analyze_semeai B2 A2
#? [1 1 (PASS|D1)]

14 analyze_semeai A2 B2
#? [0 0 PASS]

15 analyze_semeai S2 T2
#? [1 1 (PASS|Q1)]

16 analyze_semeai T2 S2
#? [0 0 PASS]

loadsgf games/STS-RV/Class01eProblems/_semeai_C1_003.sgf
17 analyze_semeai E19 F19
#? [1 1 (D14|C14|C13|C12|C11|B10|B19)]

18 analyze_semeai F19 E19
#? [1 1 A16]

19 analyze_semeai T5 T6
#? [1 1 (R7|Q7|P6|O5|O4|O3|N3|M3|L3|K2)]

20 analyze_semeai T6 T5
#? [1 1 (R3|S4|S2|T3)]

loadsgf games/STS-RV/Class01eProblems/_semeai_C1_004.sgf
21 analyze_semeai C15 C16
#? [1 0 (PASS|C17|C18)]

22 analyze_semeai C16 C15
#? [1 0 (PASS|C12|C13)]

23 analyze_semeai C5 C4
#? [1 0 (C3|C2)]

24 analyze_semeai C4 C5
#? [1 1 (A5|B5)]

25 analyze_semeai T5 T4
#? [1 1 T3]

26 analyze_semeai T4 T5
#? [1 1 T7]

27 analyze_semeai S15 S16
#? [1 0 PASS]

28 analyze_semeai S16 S15
#? [1 0 PASS]

loadsgf games/STS-RV/Class01eProblems/_semeai_C1_005.sgf
29 analyze_semeai A16 A17
#? [1 1 (A18|A19)]

30 analyze_semeai A17 A16
#? [1 1 (A15|A14)]

31 analyze_semeai C4 C3
#? [1 0 (PASS|C1|C2)]

32 analyze_semeai C3 C4
#? [1 0 (PASS|A6|A7|A4|B4)]

33 analyze_semeai S5 S4
#? [1 1 T4]

34 analyze_semeai S4 S5
#? [1 0 (T7|T6)]

35 analyze_semeai T15 T16
#? [1 0 (PASS|T14)]

36 analyze_semeai T16 T15
#? [1 0 (PASS|T18)]

loadsgf games/STS-RV/Class01eProblems/_semeai_C1_006.sgf
37 analyze_semeai A16 A17
#? [1 1 (E19|E18)]

38 analyze_semeai A17 A16
#? [1 1 (A11|B11)]

39 analyze_semeai D4 D3
#? [1 0 (E2|E1|A2|C2|A8)]
#A8 is deprecated but also achieves seki.

40 analyze_semeai D3 D4
#? [1 1 (C6|C7)]

41 analyze_semeai S4 S3
#? [1 0 (T2|R2|T8)]

42 analyze_semeai S3 S4
#? [1 1 T4]

#Unexpected behavior. Black can choose between both living or killing
43 analyze_semeai S15 S16
#? [1 1 S18]

44 analyze_semeai S16 S15
#? [1 0 (T13|S18|R11|R12)]
#R11 R12 deprecated leave more ko threats

45 analyze_semeai N14 N15
#? [1 1 H12]

46 analyze_semeai N15 N14
#? [1 1 H12]

loadsgf games/STS-RV/Class01eProblems/_semeai_C1_007.sgf
47 analyze_semeai A15 A16
#? [1 1 (A18|A17|B18|B17)]

48 analyze_semeai A16 A15
#? [1 1 (C13|C14)]

49 analyze_semeai E2 D2
#? [1 0 (A5|B5|A3|B1)]

50 analyze_semeai D2 E2
#? [1 1 J1]

51 analyze_semeai Q18 R18
#? [1 0 PASS]

52 analyze_semeai R18 Q18
#? [1 0 PASS]

53 analyze_semeai S3 S4
#? [1 1 R6]

54 analyze_semeai S4 S3
#? [1 0 (O2|O1|R2|Q1|S1)]

55 analyze_semeai K14 L14
#? [1 1 (P12|P11|O10|M9)]

56 analyze_semeai L14 K14
#? [1 0 (J15|H15|F14|E13|E12|F10)]

loadsgf games/STS-RV/Class01eProblems/_semeai_C1_008.sgf
57 analyze_semeai A16 A17
#? [1 1 B19]

58 analyze_semeai A17 A16
#? [1 1 (B13|A14)]

59 analyze_semeai B4 B3
#? [0 0 PASS]

60 analyze_semeai B3 B4
#? [1 1 (PASS|C1|C2|D1)]

61 analyze_semeai R2 Q2
#? [1 1 (T3|O1|O2|P1|P3|R1)]

62 analyze_semeai Q2 R2
#? [1 0 T3]

63 analyze_semeai P15 Q15
#? [1 0 (PASS|S17|T16)]

64 analyze_semeai Q15 P15
#? [1 0 (PASS|M15|N15|O15)]

65 analyze_semeai K10 K9
#? [1 0 (PASS|G7|J5|K4|L4|M5|N6|N7)]

66 analyze_semeai K9 K10
#? [1 0 (PASS|G11|G12|H14|J14|K14|M13|N12|N11)]

loadsgf games/STS-RV/Class01eProblems/_semeai_C1_009.sgf
67 analyze_semeai E18 D18
#? [1 1 (B14|C14|A17|B17|C17|B16)]

68 analyze_semeai D18 E18
#? [1 0 (G15|H15|K19|K18|G17|G19|F18|H18)]

69 analyze_semeai A5 A6
#? [1 1 D6]

70 analyze_semeai A6 A5
#? [1 0 (C3|B4)]

71 analyze_semeai S4 S5
#? [0 0 PASS]

72 analyze_semeai S5 S4
#? [1 1 (PASS|O2|O3)]

73 analyze_semeai N16 N15
#? [1 0 (PASS|M11|N10|O10|S14|T15)]

74 analyze_semeai N15 N16
#? [1 0 (PASS|M18|M19)]

loadsgf games/STS-RV/Class01eProblems/_semeai_C1_010.sgf
75 analyze_semeai A15 A16
#? [1 1 (C18|B17|D17)]

76 analyze_semeai A16 A15
#? [1 1 (C18|B17|D17)]

77 analyze_semeai E1 F1
#? [1 1 (PASS|H3|J2)]

78 analyze_semeai F1 E1
#? [0 0 PASS]

loadsgf games/STS-RV/Class01eProblems/_semeai_C1_011.sgf
79 analyze_semeai A15 A16
#? [1 1 (E19|F18|F17)]

80 analyze_semeai A16 A15
#? [1 0 (F12|F11|F10|F9|E8|D8)]

81 analyze_semeai D2 E2
#? [1 1 E1]

82 analyze_semeai E2 D2
#? [1 0 B2]

83 analyze_semeai R3 Q3
#? [1 0 (PASS|M5|N5|O6|O7)]

84 analyze_semeai Q3 R3
#? [1 0 (PASS|R8|S8|T8)]

85 analyze_semeai Q19 P19
#? [1 1 (N17|L16)]

86 analyze_semeai P19 Q19
#? [1 1 (Q17|R16)]

loadsgf games/STS-RV/Class01eProblems/_semeai_C1_012.sgf
87 analyze_semeai E19 D19
#? [1 1 (B17|D17)]

88 analyze_semeai D19 E19
#? [1 1 (G19|J17|K17|L17|M17|G16|G15|G14|F13|E13|D13|C13|B13|A13)]

89 analyze_semeai O5 P5
#? [1 1 (T5|S6)]

90 analyze_semeai P5 O5
#? [1 1 (B3|C3|D3|D4|D5|E6|F5|F4|F3|G3|H3|J3|J1|K4|L4)]

loadsgf games/STS-RV/Class01eProblems/_semeai_C1_013.sgf
91 analyze_semeai E19 D19
#? [0 0 PASS]

92 analyze_semeai D19 E19
#? [1 1 (PASS|A13|B13|C13|D13|E13|F13|G14|G15|G16|G17|J17|L17|M17)]

93 analyze_semeai O5 P5
#? [0 0 PASS]

94 analyze_semeai P5 O5
#? [1 1 (PASS|B3|C3|D3|D4|D5|F5|F4|F3|G3|H3|J3|K4|L4|J1)]

loadsgf games/STS-RV/Class01eProblems/_semeai_C1_014.sgf
95 analyze_semeai E19 D19
#? [1 1 (PASS|B17|D17)]

96 analyze_semeai D19 E19
#? [0 0 PASS]

97 analyze_semeai O5 P5
#? [1 1 (PASS|S6|T5)]

98 analyze_semeai P5 O5
#? [0 0 PASS]

loadsgf games/STS-RV/Class01eProblems/_semeai_C1_015.sgf
99 analyze_semeai E18 F18
#? [1 0 PASS]

100 analyze_semeai F18 E18
#? [1 0 (PASS|E14)]

loadsgf games/STS-RV/Class01eProblems/_semeai_C1_016.sgf
101 analyze_semeai A14 A15
#? [1 1 C17]

102 analyze_semeai A15 A14
#? [1 1 C17]

103 analyze_semeai A6 A5
#? [1 1 B3]

104 analyze_semeai A5 A6
#? [1 1 B3]

105 analyze_semeai T6 T5
#? [0 0 PASS]

106 analyze_semeai T5 T6
#? [1 1 (PASS|O1|O2|O3|O4|O5|O6|P7|Q7|R7|S7|T7)]

107 analyze_semeai T14 T15
#? [1 1 T17]

108 analyze_semeai T15 T14
#? [1 1 T17]

109 analyze_semeai F10 G10
#? [1 1 J11]

110 analyze_semeai G10 F10
#? [1 1 J11]

loadsgf games/STS-RV/Class01eProblems/_semeai_C1_017.sgf
111 analyze_semeai A14 A15
#? [0 0 PASS]

112 analyze_semeai A15 A14
#? [1 1 (PASS|A13|B13|C13|D13|E14|F15|F16|F17|G17|H17|J18|J19)]

113 analyze_semeai A4 A3
#? [1 1 (B2|C3|D2)]

114 analyze_semeai A3 A4
#? [1 1 B2]

115 analyze_semeai T14 T15
#? [1 1 S17]

116 analyze_semeai T15 T14
#? [1 1 S17]

loadsgf games/STS-RV/Class01eProblems/_semeai_C1_018.sgf
117 analyze_semeai T14 T15
#? [0 0 PASS]

118 analyze_semeai T15 T14
#? [1 1 (PASS|L19|L18|M17|N17|N16|N15|O14|P14|Q13|R13|S13|T13)]

119 analyze_semeai A4 A3
#? [1 1 C2]

120 analyze_semeai A3 A4
#? [1 1 C2]

121 analyze_semeai A14 A15
#? [1 1 C17]

122 analyze_semeai A15 A14
#? [1 1 C17]

loadsgf games/STS-RV/Class01eProblems/_semeai_C1_019.sgf
123 analyze_semeai A14 A15
#? [1 1 B17]

124 analyze_semeai A15 A14
#? [1 1 B17]

125 analyze_semeai A4 A3
#? [0 0 PASS]

126 analyze_semeai A3 A4
#? [1 1 (PASS|A6|B6|C6|D6|E6|F5|G4|G3|H3|J3|K2|K1)]

127 analyze_semeai T5 T4
#? [1 1 R3]

128 analyze_semeai T4 T5
#? [1 1 R3]

129 analyze_semeai T13 T15
#? [1 1 (S18|T16|T14)]

130 analyze_semeai T15 T13
#? [1 1 (S18|T16)]

131 analyze_semeai G10 H10
#? [1 1 K9]

132 analyze_semeai H10 G10
#? [1 1 K9]

loadsgf games/STS-RV/Class01eProblems/_semeai_C1_020.sgf
133 analyze_semeai A14 A15
#? [1 1 A18]

134 analyze_semeai A15 A14
#? [1 1 A18]

135 analyze_semeai A4 A3
#? [1 1 (C2|D2)]

136 analyze_semeai A3 A4
#? [1 1 C2]

137 analyze_semeai T5 T4
#? [1 1 (R3|O1)]

138 analyze_semeai T4 T5
#? [1 1 R3]

139 analyze_semeai T13 T15
#? [1 1 (PASS|T17|T18)]

140 analyze_semeai T15 T13
#? [0 0 PASS]

141 analyze_semeai G10 H10
#? [1 1 (PASS|J10)]

142 analyze_semeai H10 G10
#? [0 0 PASS]

loadsgf games/STS-RV/Class01eProblems/_semeai_C1_021.sgf
143 analyze_semeai A14 A15
#? [1 1 A18]

144 analyze_semeai A15 A14
#? [1 0 A18]

145 analyze_semeai A4 A3
#? [0 0 PASS]

146 analyze_semeai A3 A4
#? [1 1 (PASS|A6|B6|C6|D6|E6|F5|G4|H3|J3|K2|K1)]

147 analyze_semeai T5 T4
#? [1 1 (PASS|O1)]

148 analyze_semeai T4 T5
#? [0 0 PASS]

149 analyze_semeai T13 T15
#? [0 0 PASS]

150 analyze_semeai T15 T13
#? [1 1 (PASS|L19|L18|M17|N17|O17|O16|O15|P14|Q13|R13|S12|T12)]

151 analyze_semeai G10 H10
#? [1 1 J10]

152 analyze_semeai H10 G10
#? [1 1 J10]

loadsgf games/STS-RV/Class01eProblems/_semeai_C1_022.sgf
153 analyze_semeai A14 A15
#? [1 0 G19]

154 analyze_semeai A15 A14
#? [1 1 G19]

155 analyze_semeai A4 A2
#? [1 1 C2]

156 analyze_semeai A2 A4
#? [1 1 C2]

157 analyze_semeai T5 T4
#? [1 1 R3]

158 analyze_semeai T4 T5
#? [1 1 (N3|O5|P6|Q6|R6|S6|T6)]

159 analyze_semeai T13 T15
#? [1 1 (S18|T16)]

160 analyze_semeai T15 T13
#? [1 1 (N17|017|O16|O15|O14|R13|T12)]

161 analyze_semeai G10 H10
#? [1 1 (PASS|K9)]

162 analyze_semeai H10 G10
#? [0 0 PASS]

loadsgf games/STS-RV/Class01eProblems/_semeai_C1_023.sgf
163 analyze_semeai A15 A16
#? [1 1 (C18|D17)]

164 analyze_semeai A16 A15
#? [1 0 C18]

165 analyze_semeai E1 F1
#? [1 1 D2]

166 analyze_semeai F1 E1
#? [1 1 D2]

167 analyze_semeai T8 T7
#? [1 1 T6]

168 analyze_semeai T7 T8
#? [1 1 (T10|R10|S11)]
#Check the status of semiai if pass. Should be looser not seki. Checked and OK.

169 analyze_semeai O19 N19
#? [1 1 K18]

170 analyze_semeai N19 O19
#? [1 0 (K18|L17|M18)]

171 analyze_semeai J12 K12
#? [1 1 J10]

172 analyze_semeai K12 J12
#? [1 1 J10]


loadsgf games/STS-RV/Class01eProblems/_semeai_C1_024.sgf
173 analyze_semeai A15 A16
#? [1 1 D17]

174 analyze_semeai A16 A15
#? [1 1 B12]
#Check what is the seki value for the program. Checked: winning= 96, seki = 34 for each. Not very accurate. For black should be 44.

175 analyze_semeai F1 G1
#? [1 1 H2]

176 analyze_semeai G1 F1
#? [1 0 H2]

177 analyze_semeai T8 T7
#? [1 1 S4]

178 analyze_semeai T7 T8
#? [1 1 S11]
#Check the status of semiai if pass. Should be looser not seki. Checked and OK

179 analyze_semeai O19 N19
#? [1 0 Q17]

180 analyze_semeai N19 O19
#? [1 1 Q17]
#Check status if pass. Should be looser for black and seki for white. Checked and OK.

181 analyze_semeai J12 K12
#? [1 1 J10]

182 analyze_semeai K12 J12
#? [1 1 J10]

loadsgf games/STS-RV/Class01eProblems/_semeai_C1_025.sgf
183 analyze_semeai A14 A15
#? [1 1 C11]

184 analyze_semeai A15 A14
#? [1 1 C11]

185 analyze_semeai F1 G1
#? [1 1 J2]

186 analyze_semeai G1 F1
#? [1 1 (J2|B2|B3|D3)]
#Though B2 is also possible it leaves more ko threads and should be avoded

187 analyze_semeai T8 T7
#? [1 1 R4]

188 analyze_semeai T7 T8
#? [1 0 R4]

189 analyze_semeai O19 N19
#? [1 1 L18]

190 analyze_semeai N19 O19
#? [1 0 L18]
#Check status if pass. Should be looser for White and seki for Black.

191 analyze_semeai J12 J13
#? [1 1 J11]

192 analyze_semeai J13 J12
#? [1 1 J11]

loadsgf games/STS-RV/Class01eProblems/_semeai_C1_026.sgf
193 analyze_semeai A14 A15
#? [1 0 (PASS|B17)]

194 analyze_semeai A15 A14
#? [1 0 (PASS|B17)]

195 analyze_semeai F1 G1
#? [1 1 K2]

196 analyze_semeai G1 F1
#? [1 1 D3]

197 analyze_semeai T8 T7
#? [1 1 (T4|S3|T1)]

198 analyze_semeai T7 T8
#? [1 1 (R12|R10|S10)]

199 analyze_semeai O19 N19
#? [1 0 Q17]

200 analyze_semeai N19 O19
#? [1 1 Q17]
#Check status if pass. Should be looser for Black and seki for white.

201 analyze_semeai J12 K12
#? [1 1 H8]

202 analyze_semeai K12 J12
#? [1 1 H8]

loadsgf games/STS-RV/Class01eProblems/_semeai_C1_027.sgf
203 analyze_semeai F19 G19
#? [1 1 B19]

204 analyze_semeai G19 F19
#? [1 1 B19]

205 analyze_semeai D1 E1
#? [1 1 C2]

206 analyze_semeai E1 D1
#? [1 1 C2]

207 analyze_semeai T5 T6
#? [1 1 R4]

208 analyze_semeai T6 T5
#? [1 1 R4]
