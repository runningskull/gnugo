# last updated for gnugo 3.3.12, #USE_BDIST=1

orientation 1
loadsgf games/owl02.sgf
46 owl_defend B3
#? [1 (F1|B1|D2|E3)]*

orientation 1
loadsgf games/owl23.sgf
# This isolates the problem in test 4
170 owl_attack B14
#? [0]*

orientation 1
loadsgf games/incident248.sgf 228
208 owl_attack J2
#? [3 A2]

orientation 1
loadsgf games/marginal_ko.sgf
244 owl_attack D11
#? [(2|3) F11]*

