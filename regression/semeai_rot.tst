# last updated for gnugo 3.3.13 with #USE_BDIST=1 in owl.c

orientation 1
loadsgf games/semeai/semeai6.sgf
6 analyze_semeai A12 A14
#? [1 1 (A15|B15|C15|D15)]*

orientation 1
loadsgf games/semeai/semeai8.sgf
18 analyze_semeai A12 A14
#? [1 1 (A15|B15|D15|PASS)]*

orientation 2
loadsgf games/semeai/semeai9.sgf
22 analyze_semeai O19 N19
#? [1 1 (L15|L17|L18)]*

orientation 1
loadsgf games/nicklas/nicklas14.sgf 55
32 analyze_semeai B8 D9
#? [1 0 B6]*

