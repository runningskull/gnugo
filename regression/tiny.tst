# This test suite contains tests for boards of size 1x1, 2x2, 3x3, and 4x4.

# No legal move on 1x1, so PASS is the only option.
boardsize 1
clear_board
101 reg_genmove black
#? [PASS]


# Should play on the empty board. (?)
loadsgf games/tiny/2x2.sgf 1
201 reg_genmove black
#? [A1|A2|B1|B2]

# B2 urgent
loadsgf games/tiny/2x2.sgf 2
202 reg_genmove white
#? [B2]

# Black can make two eyes.
203 reg_genmove black
#? [B2]

# Black can make two eyes.
loadsgf games/tiny/2x2.sgf 3
204 reg_genmove black
#? [B2]


# Taking the center point secures the whole board.
loadsgf games/tiny/3x3a.sgf 1
301 reg_genmove black
#? [B2]

loadsgf games/tiny/3x3a.sgf 3
302 reg_genmove black
#? [B1|B3|C2]

loadsgf games/tiny/3x3a.sgf 5
303 reg_genmove black
#? [C2]

loadsgf games/tiny/3x3b.sgf 2
304 reg_genmove white
#? [B2]*

loadsgf games/tiny/3x3b.sgf 3
305 reg_genmove black
#? [B3|B1|C2]

loadsgf games/tiny/3x3b.sgf 5
306 reg_genmove black
#? [C2]


loadsgf games/tiny/4x4a.sgf 1
401 reg_genmove black
#? [B2|C2|B3|C3]

loadsgf games/tiny/4x4a.sgf 2
402 reg_genmove white
#? [B2]*

loadsgf games/tiny/4x4a.sgf 3
403 reg_genmove black
#? [B3|C2]

loadsgf games/tiny/4x4a.sgf 4
404 reg_genmove white
#? [B3]

loadsgf games/tiny/4x4a.sgf 5
405 reg_genmove black
#? [B1|B4|C1|C4]

# C4 gives the same result under Chinese rules but is one point worse
# under Japanese rules.
loadsgf games/tiny/4x4a.sgf 6
406 reg_genmove white
#? [C1]

# A3, B1, C4, and D2 all give jigo under Japanese rules. Under Chinese
# rules the results are A3:B+2, B1:B+1, C4:jigo, D2:W+1.
loadsgf games/tiny/4x4a.sgf 7
407 reg_genmove black
#? [A3]*

# Under Chinese rules all of B1, D2, and C4 give B+2. Under Japanese
# rules B1 and D2 give jigo while C4 gives B+1.
loadsgf games/tiny/4x4a.sgf 8
408 reg_genmove white
#? [B1|D2]

# A2, C4, and D2 all give jigo under Japanese rules. Under Chinese
# rules A2 gives B+2, C4 gives jigo, and D2 gives W+1.  
loadsgf games/tiny/4x4a.sgf 9
409 reg_genmove black
#? [A2]

loadsgf games/tiny/4x4a.sgf 10
410 reg_genmove white
#? [D2]

loadsgf games/tiny/4x4a.sgf 11
411 reg_genmove black
#? [D4]
