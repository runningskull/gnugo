# This test suite is a collection of blunders. By "blunder" we here
# mean a move which is worse than passing. Also included are moves
# which are okay but which GNU Go incorrectly rejects thinking that
# they would be blunders.

loadsgf games/blunder1.sgf
1 reg_genmove black
#? [!E5]

loadsgf games/blunder2.sgf
2 reg_genmove white
#? [C9|H9]

loadsgf games/blunder3.sgf 229
3 reg_genmove black
#? [!D6]

loadsgf games/blunder4.sgf
4 reg_genmove white
#? [B5|C5|C4|D4|E4|E3|F3|A4|A5]

# This is an "antiblunder". After black A4, white does not have any
# kind of double threat at C6, which is a simple snapback.
loadsgf games/blunder5.sgf
5 reg_genmove black
#? [A4]

loadsgf games/blunder6.sgf 1
6 reg_genmove white
#? [D4]

# A9 is a losing ko threat
loadsgf games/blunder6.sgf 3
7 reg_genmove white
#? [E3]

# Both A3 and A5 are blunders due to a spectacular atari-atari sequence.
loadsgf games/blunder7.sgf
8 reg_genmove black
#? [!(A3|A5)]

# After black D9, white cuts at G8 and gets a ko.
loadsgf games/blunder8.sgf
9 reg_genmove black
#? [G8|G9|H8]

# Clearly, black can't start with J9.
loadsgf games/blunder9.sgf
10 reg_genmove black
#? [G9|F9|C7]

# Black B3 loses at least five stones.
loadsgf games/blunder10.sgf 1
11 reg_genmove black
#? [D4|E4|E5|F4|C6]

# White D4 kills at least six stones.
loadsgf games/blunder10.sgf 2
12 reg_genmove white
#? [D4]*
13 reg_genmove black
#? [D4|E4|E5|F4]

loadsgf games/blunder11.sgf 42
14 reg_genmove black
#? [G4]

loadsgf games/blunder11.sgf 44
15 reg_genmove black
#? [H3]

loadsgf games/blunder12.sgf 252
16 reg_genmove white
#? [R5|Q5|R6|S6|S7|R7]

loadsgf games/blunder12.sgf 258
17 reg_genmove white
#? [R7]*

# A simplification of incident 136. Black E6 is a bad blunder.
loadsgf games/blunder13.sgf
18 reg_genmove black
#? [F5]

loadsgf games/blunder14.sgf
19 reg_genmove white
#? [P18|P19|R18|R19]

# P11 got incorrectly rejected as a blunder.
loadsgf games/handtalk/handtalk13.sgf 123
20 reg_genmove black
#? [P11]

# L9 got incorrectly rejected as a blunder.
loadsgf games/handtalk/handtalk13.sgf 131
21 reg_genmove black
#? [L9]

# R18 is suicidal
loadsgf games/blunder15.sgf
22 reg_genmove black
#? [!R18|R19]

# Don't play an outer liberty and let white have a ko.
loadsgf games/blunder16.sgf
23 reg_genmove black
#? [!F4|F5|F6]

# Don't play the outer liberty and let white make seki.
# See also connection:113 and reading:198.
loadsgf games/blunder17.sgf
24 reg_genmove black
#? [!P5]*

# Don't play an outer liberty and let white make seki or better.
loadsgf games/blunder18.sgf
25 reg_genmove black
#? [!P9|M6|T9]

# Don't play an outer liberty and let black make seki or better.
loadsgf games/blunder19.sgf
26 reg_genmove white
#? [!D19|F17]

# R3 lets black play T2 and get seki.
loadsgf games/blunder20.sgf
27 reg_genmove white
#? [!R3]

# C4 lets black make seki with B1. E1 is even worse.
loadsgf games/blunder22.sgf 1
28 reg_genmove white
#? [!(C4|E1)]*

# C1 gives a ko that white can't win. Pass gives life in seki. B3 dies
# right out.
loadsgf games/blunder22.sgf 5
29 reg_genmove white
#? [!(C1|B3)]

# See also reading:197.
loadsgf games/blunder23.sgf
30 reg_genmove black
#? [!O7]

loadsgf games/atari_atari09.sgf
31 reg_genmove black
#? [!T14]

# E3 and H4 are not blunders, however, H2 is.
loadsgf games/nngs/gnugo-3.4-viking4-200308191053.sgf 283
32 reg_genmove white
#? [E3|H4]

# This needs a detect_semeai_blunder().
loadsgf games/kisei28_g7.sgf 280
33 reg_genmove white
#? [!H6|J1]

# Fills a common liberty in a seki similar to blunder:32.
loadsgf games/kgs/2004-04-28-R-dokuganryu-GnuGoCVS.sgf 302
34 gg_genmove black
#? [!A19|C19]

# An example of transforming a won semeai into a seki
loadsgf games/blunder24.sgf
35 gg_genmove white
#? [!G1]

loadsgf games/blunder25.sgf
36 restricted_genmove white H5 J4 H3 J3 H2 J2
#? [J4|H3|J3|H2|J2]*

