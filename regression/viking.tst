# This test suite is based on games against the program Viking by
# Magnus Persson.

loadsgf games/viking1.sgf 15
1 gg_genmove white 
#? [O15]*

loadsgf games/viking1.sgf 23
2 gg_genmove white 
#? [E2]*

loadsgf games/viking1.sgf 65
3 gg_genmove white 
#? [M15|B7]

loadsgf games/viking1.sgf 135
4 gg_genmove white 
#? [B17|B18]*

loadsgf games/viking1.sgf 187
5 gg_genmove white 
#? [S5]

loadsgf games/viking1.sgf 189
6 gg_genmove white 
#? [B17|B18]
