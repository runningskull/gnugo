# last updated for gnugo 3.3.13 with #USE_BDIST=1 in owl.c

orientation 1
loadsgf games/semeai/semeai6.sgf
6 owl_analyze_semeai A12 A14
#? [ALIVE DEAD (A15|B15|C15|D15)]*

orientation 1
loadsgf games/semeai/semeai8.sgf
18 owl_analyze_semeai A12 A14
#? [ALIVE DEAD (A15|B15|D15|PASS)]*

orientation 2
loadsgf games/semeai/semeai9.sgf
22 owl_analyze_semeai O19 N19
#? [ALIVE DEAD (L15|L17|L18)]*

orientation 1
loadsgf games/nicklas/nicklas14.sgf 55
32 owl_analyze_semeai B8 D9
#? [ALIVE_IN_SEKI ALIVE_IN_SEKI B6]*

