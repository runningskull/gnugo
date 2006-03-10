# Not the right moment to start cosmic go at G7 as in the game.
loadsgf games/handtalk/handtalk12.sgf 17
1 reg_genmove black
#? [R12|R13|R14|R8|R7|R6]*

# Correct owl defense at P5 gets in 3.1.31 due to negative shape value
# -20 (!) by Pattern Shape13.
loadsgf games/handtalk/handtalk12.sgf 37
2 reg_genmove black
#? [P5|Q2|R2]*

# The sagari at F18 (very much liked by 3.1.26-32 influence code) is bad
# endgame here (as well as in many other situations). Also, defending N7
# seems more urgent.
loadsgf games/handtalk/handtalk12.sgf 55
3 reg_genmove black
#? [!F18]

# Have to try this ko!
loadsgf games/handtalk/handtalk12.sgf 91
4 reg_genmove black
#? [Q2]*

loadsgf games/handtalk/handtalk13.sgf 79
5 reg_genmove black
#? [!P15]

loadsgf games/handtalk/handtalk13.sgf 161
6 restricted_genmove black E16 G13
#? [G13]

loadsgf games/handtalk/handtalk21.sgf 14
7 reg_genmove white
#? [R4]

loadsgf games/handtalk/handtalk21.sgf 24
8 reg_genmove white
#? [!G14]

loadsgf games/handtalk/handtalk21.sgf 46
9 reg_genmove white
#? [J17]*

# Other moves possible, but GNU Go should attack the D8 stone
loadsgf games/handtalk/handtalk23.sgf 26
10 reg_genmove white
#? [E9|F8]

# B9 is quite bad here.
loadsgf games/handtalk/handtalk23.sgf 30
11 reg_genmove white
#? [C11|D10]

# The balance of power between J11 and H8 is the issue at the moment.
# Maybe other moves possible.
loadsgf games/handtalk/handtalk23.sgf 38
12 reg_genmove white
#? [G9|K8|J9|K9]*

loadsgf games/handtalk/handtalk23.sgf 48
13 reg_genmove white
#? [!P14]

loadsgf games/handtalk/handtalk23.sgf 80
14 reg_genmove white
#? [K4]*

loadsgf games/handtalk/handtalk23.sgf 82
15 reg_genmove white
#? [J3]

#CATEGORY=CONNECTION
loadsgf games/handtalk/handtalk23.sgf 96
16 reg_genmove white
#? [J18]*

loadsgf games/handtalk/handtalk23.sgf 162
17 reg_genmove white
#? [B4|B6]*

loadsgf games/handtalk/handtalk23.sgf 186
18 reg_genmove white
#? [S5]

# GNU Go played J8. Make sure it never forgets to do so!
loadsgf games/handtalk/handtalk1.sgf 69
19 reg_genmove black 
#? [J8]

loadsgf games/handtalk/handtalk2.sgf 79
20 reg_genmove black 
#? [N18]

#CATEGORY=BLUNDER
# (Incorrectly rejected move)
loadsgf games/handtalk/handtalk2.sgf 137
21 reg_genmove black 
#? [R6]
