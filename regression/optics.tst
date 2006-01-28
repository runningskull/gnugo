# Eyes of size 1
loadsgf games/eyes1.sgf
101 eval_eye A1
#? [1 1]
102 eval_eye C1
#? [1 1]
103 eval_eye G1
#? [0 1 F1 F1]
104 eval_eye L1
#? [0 1 K2 K2]
105 eval_eye P1
#? [0 0]
106 eval_eye A4
#? [0 0]
107 eval_eye C6
#? [0 0]
108 eval_eye F7
#? [0 0]
109 eval_eye H9
#? [1 1]
110 eval_eye E9
#? [0 1 D10 D10]
111 eval_eye A10
#? [1 1]

# Eyes of size 2
loadsgf games/eyes2.sgf
201 eval_eye A1
#? [1 1]
202 eval_eye F1
#? [1 1]
203 eval_eye L1
#? [1 1]
204 eval_eye Q1
#? [1 1]
205 eval_eye T1
#? [1 1]
206 eval_eye A4
#? [1 1]
207 eval_eye F3
#? [1 1]
208 eval_eye L3
#? [0 0]
209 eval_eye O4
#? [1 1]
210 eval_eye G6
#? [1 1]
211 eval_eye C7
#? [0 1 D7 D7]
212 eval_eye A7
#? [0 1 B9 B9]
213 eval_eye A12
#? [0 1 A13 A13]
214 eval_eye O9
#? [0 0]
215 eval_eye O13
#? [0 0]

# Eyes of size 3
loadsgf games/eyes3.sgf
301 eval_eye A1
#? [1 2 A1 A1]
302 eval_eye F1
#? [1 2 G1 G1]
303 eval_eye M1
#? [1 1]
304 eval_eye S19
#? [1 2 S18 S18]
305 eval_eye T7
#? [1 1]
306 eval_eye A6
#? [2 2]
307 eval_eye A10
#? [1 2 A9 A9]
308 eval_eye T1
#? [2 2]
309 eval_eye R4
#? [2 2]
310 eval_eye N3
#? [0 1 N5 N5]
311 eval_eye J5
#? [1 1]
312 eval_eye G5
#? [0 1 G6 G6]
313 eval_eye C6
#? [1 2 C6 C6]
314 eval_eye C9
#? [2 2]
315 eval_eye A12
#? [1 1]
316 eval_eye A14
#? [1 1]
317 eval_eye B19
#? [1 2 B18 B18]
318 eval_eye D19
#? [1 2 D18 D18]
319 eval_eye C16
#? [1 1]
320 eval_eye E15
#? [1 1]
321 eval_eye F19
#? [0 1 J18 J18]
322 eval_eye F17
#? [0 2 G17 G17]
323 eval_eye O19
#? [1 1]
324 eval_eye K13
#? [1 1]
325 eval_eye L10
#? [0 0]
326 eval_eye P8
#? [1 1]
327 eval_eye T14
#? [0 0]
328 eval_eye O16
#? [0 1 (N16|N13) N16]

# False margins of various shapes and sizes
loadsgf games/marginal.sgf
1001 eval_eye L19
#? [1 1]
1002 eval_eye O1
#? [1 1]
1003 eval_eye T8
#? [1 1]
1004 eval_eye A12
#? [1 1]
1005 eval_eye F8
#? [1 1]
1006 eval_eye P6
#? [1 1]
1007 eval_eye Q9
#? [1 1]
1008 eval_eye D11
#? [2 2]
1009 eval_eye E6
#? [1 1]
1010 eval_eye K11
#? [0 0]
1011 eval_eye S19
#? [1 1]*
1012 eval_eye C1
#? [1 1]*
1013 eval_eye O16
#? [1 2 G16 L16]*

# incident 73
# This is a problem with make_domains(). The position is a seki.
# FIXME: Is "2 2" correct response for a seki?
loadsgf games/incident73.sgf 210
1101 eval_eye T10
#? [2 2]*

loadsgf games/incident73.sgf 212
1201 eval_eye T10
#? [1 2 T8 T8]*

# incident 97
loadsgf games/incident97.sgf 175
1301 eval_eye S11
#? [1 2 S11 (S11|S13)]

# incident 198
loadsgf games/incident198.sgf 54
1401 eval_eye J4
#? [0 1 H5 H5]

# incident 212
loadsgf games/incident211.sgf 26
1501 eval_eye H8
#? [1 2 H8 H8]*

# incident 238
loadsgf games/incident238.sgf 252
1601 eval_eye E16
#? [1 1]

# Edge and corner eyes different from center eyes.
loadsgf games/eyes_edge.sgf
1701 eval_eye C1
#? [1 1]
1702 eval_eye J2
#? [1 1]
1703 eval_eye T3
#? [1 1]
1704 eval_eye Q19
#? [1 1]
1705 eval_eye A19
#? [1 2 A18 A18]

# Eye spaces with ko contingent margins.
# Either min or max in these depends on ko.
loadsgf games/marginal_ko.sgf
1801 eval_eye A19
#? [0 1 (A19|B17) B17]
1802 eval_eye B17
#? [0 1 (A19|B17) A19]
1803 eval_eye N17
#? [1 1]
1804 eval_eye J12
#? [1 1]
1805 eval_eye A13
#? [0 1 B12 B12]
1806 eval_eye A8
#? [1 1]
1807 eval_eye F7
#? [1 2 F11 (F11|H8|F8)]
1808 eval_eye O7
#? [0 1 (M6|Q8) (M6|Q8)]
1809 eval_eye N12
#? [0 1 (O11 O11)]
1810 eval_eye A1
#? [0 1 B2 B2]
1811 eval_eye H1
#? [1 2 (J1|L2) J1]
1812 eval_eye S1
#? [0 1 R2 R2]
