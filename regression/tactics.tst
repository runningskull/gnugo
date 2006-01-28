loadsgf games/tactics01.sgf
1 reg_genmove black
#? [C15|D15|B13|B11|B14]*

loadsgf games/tactics02.sgf
2 reg_genmove black
#? [S11|S12]

loadsgf games/tactics03.sgf
3 reg_genmove black
#? [M18]

# This is in part an atari_atari problem. The best local move is K18
# though J18 or G18 might be acceptable.
loadsgf games/rosebud4.sgf 138
4 reg_genmove white
#? [K18]*

loadsgf games/tactics04.sgf 206
5 reg_genmove white
#? [N16]
