# This test suite contains tests for boards of size 2x2, 3x3, and 4x4.

# Should play on the empty board. (?)
loadsgf games/tiny/2x2.sgf 1
1 reg_genmove black
#? [A1|A2|B1|B2]

# B2 urgent
loadsgf games/tiny/2x2.sgf 2
2 reg_genmove white
#? [B2]

# Black can make two eyes.
3 reg_genmove black
#? [B2]

# Black can make two eyes.
loadsgf games/tiny/2x2.sgf 3
4 reg_genmove black
#? [B2]


# Taking the center point secures the whole board.
loadsgf games/tiny/3x3a.sgf 1
101 reg_genmove black
#? [B2]

loadsgf games/tiny/3x3a.sgf 3
102 reg_genmove black
#? [B1|B3|C2]

loadsgf games/tiny/3x3a.sgf 5
103 reg_genmove black
#? [C2]

loadsgf games/tiny/3x3b.sgf 2
104 reg_genmove white
#? [B2]

loadsgf games/tiny/3x3b.sgf 3
105 reg_genmove black
#? [B3|B1|C2]

loadsgf games/tiny/3x3b.sgf 5
106 reg_genmove black
#? [C2]


loadsgf games/tiny/4x4a.sgf 1
201 reg_genmove black
#? [B2|C2|B3|C3]

loadsgf games/tiny/4x4a.sgf 2
202 reg_genmove white
#? [B2]

loadsgf games/tiny/4x4a.sgf 3
203 reg_genmove black
#? [B3|C2]

loadsgf games/tiny/4x4a.sgf 4
204 reg_genmove white
#? [B3]

loadsgf games/tiny/4x4a.sgf 5
205 reg_genmove black
#? [B1|B4|C1|C4]

# C4 might also be okay.
loadsgf games/tiny/4x4a.sgf 6
206 reg_genmove white
#? [C1]

# B1, C4, and D2 might also be okay.
loadsgf games/tiny/4x4a.sgf 7
207 reg_genmove black
#? [A3]

# C4 might also be okay.
loadsgf games/tiny/4x4a.sgf 8
208 reg_genmove white
#? [B1]

# C4 and D2 might also be okay.
loadsgf games/tiny/4x4a.sgf 9
209 reg_genmove black
#? [A2]

loadsgf games/tiny/4x4a.sgf 10
210 reg_genmove white
#? [D2]

loadsgf games/tiny/4x4a.sgf 11
211 reg_genmove black
#? [D4]
