
loadsgf games/handtalk/handtalk1.sgf 113
3 owl_attack q19
#? [2 T15]

loadsgf games/handtalk/handtalk2.sgf 35
4 owl_defend R8
#? [1 P10]*

loadsgf games/handtalk/handtalk2.sgf 70
5 owl_defend P18
#? [2 (S18|O17|N18)]*

# O18 might work, but I don't think so. /ab
loadsgf games/handtalk/handtalk2.sgf 78
6 owl_defend P19
#? [1 (N18|O17)]*

7 analyze_semeai Q17 P19
#? [1 1 (O18|N18|O17|P17)]

8 analyze_semeai P19 Q17
#? [1 1 (N18|O17|S15)]*

loadsgf games/handtalk/handtalk2.sgf 134
9 owl_attack R8
#? [1 (T6|T7)]

loadsgf games/handtalk/handtalk2.sgf 136
10 owl_defend Q4
#? [0]

#loadsgf games/handtalk/handtalk2.sgf 160
#11 owl_defend G15
##? [1 E13]*
