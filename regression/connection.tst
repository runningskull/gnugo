# Reset applicable counters
reset_reading_node_counter

loadsgf games/connection1.sgf
1 same_dragon G17 K17
#? [0]*
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
#? [0]*

# incident 213
loadsgf games/incident211.sgf 40
6 same_dragon G8 J6
#? [0]*

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
#? [0]*

loadsgf games/doublecut.sgf
16 same_dragon E16 G14
#? [0]

loadsgf games/doublecut2.sgf 151
17 same_dragon D13 F12
#? [0]*

loadsgf games/arion.sgf 63
18 same_dragon N17 P17
#? [1]*

loadsgf games/dublin1.sgf 71
19 same_dragon B15 E16
#? [0]*

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
#? [1 (M17|L16)]*
24 disconnect L15 L17
#? [1 (M17|L16)]
25 disconnect L14 M12
#? [1 L13]

loadsgf games/nicklas/nicklas16.sgf 38
26 connect C3 C6
#? [1 B4]*
27 connect C3 B7
#? [1 B4]*

loadsgf games/golife.sgf 38
28 disconnect H5 H7
#? [0]

loadsgf games/incident240.sgf 69
29 disconnect B12 F7
#? [1 D9]*

loadsgf games/strategy13.sgf
30 connect R13 S8
#? [1 (Q11|T10|T9)]*
31 disconnect R13 S8
#? [1 Q9]*

loadsgf games/incident240.sgf 69
32 disconnect B12 E9
#? [1 D9]*

loadsgf games/trevor/auto/a035.sgf 28
33 disconnect B4 C6
#? [1 B5]*

# Monkey jumps and relatives.
loadsgf games/connection3.sgf
34 disconnect B11 A9
#? [0]
35 disconnect L12 H13
#? [0]*
36 connect M3 N7
#? [1 (N4|M4|N5|N6)]*
37 disconnect M3 N7
#? [1 (N4|M4|N5|M5|M6)]*
38 connect B2 G1
#? [1 E1]*
39 disconnect B2 G1
#? [1 (F2|F1|E2|E1|D2|D1|C2|C1|G2|H1|B1)]*

# More monkey jumps and relatives.
loadsgf games/connection4.sgf
40 connect B11 A8
#? [1 B9]
41 disconnect B11 A8
#? [1 (B9|A10|B10|B8|A7)]
42 connect L12 H13
#? [1 K12]*
43 disconnect L12 H13
#? [1 (K12|J12|J13|H12|G13)]
44 connect M3 N7
#? [3 M6]*
45 disconnect M3 N7
#? [1 (N4|M4|N5|M5|M6|M7|N8|M8)]*
46 connect B2 G1
#? [1 (B1|C1|C2|D2|E1|F1)]*
47 disconnect B2 G1
#? [1 (C1|E1)]*

loadsgf games/trevor/auto/a038.sgf 34
48 disconnect C3 G3
#? [1 D2|F2]*

loadsgf games/trevor/trevor_15.sgf 
49 disconnect D7 G6
#? [1 (E7|G7)]

loadsgf games/trevor/auto/a014.sgf 16
50 disconnect B5 C2
#? [1 B4]

loadsgf games/nngs/Lazarus-gnugo-3.1.19-200201092246.sgf 108
51 connect R14 Q16
#? [1 Q15]*
52 connect R14 O15
#? [1 Q15]*
53 connect S7 S4
#? [1 T5]*

# D8 might also disconnect.
loadsgf games/nngs/Lazarus-gnugo-3.1.19-200201092246.sgf 164
54 disconnect B6 D10
#? [1 C8]*
55 connect B6 D10
#? [1 D8|C8]*
56 connect B6 F9
#? [1 D8]*

loadsgf games/nngs/Lazarus-gnugo-3.1.19-200201092246.sgf 192
57 connect K14 K16
#? [1 L16]

loadsgf games/nngs/Lazarus-gnugo-3.1.19-200201092246.sgf 204
58 disconnect D18 F17
#? [0]

loadsgf games/nngs/Lazarus-gnugo-3.1.19-200201092246.sgf 216
59 disconnect D18 E15
#? [1 D15|F15|B15|D19|E19]
60 connect D14 B18
#? [1 D15|F15|B15|D19|E19]*
61 connect O9 Q9
#? [1 D15|F15|B15|D19|E19]*

loadsgf games/nngs/Lazarus-gnugo-3.1.19-200201092246.sgf 238
62 connect S18 S14
#? [1 T15|T17]*

loadsgf games/nngs/Lazarus-gnugo-3.1.19-200201092246.sgf 258
63 connect N13 N17
#? [1 N16|O17|P16]*

# Report number of nodes visited by the tactical reading
10000 get_reading_node_counter
#? [0]&
