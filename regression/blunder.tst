# Reset applicable counters
reset_life_node_counter
reset_owl_node_counter
reset_reading_node_counter
reset_trymove_counter

loadsgf games/blunder1.sgf
1 gg_genmove black
#? [!E5]

loadsgf games/blunder2.sgf
2 gg_genmove white
#? [C9|H9]

loadsgf games/blunder3.sgf 229
3 gg_genmove black
#? [!D6]

loadsgf games/blunder4.sgf
4 gg_genmove white
#? [B5|C5|C4|D4|E4|E3|F3]

# This is an "antiblunder". After black A4, white does not have any
# kind of double threat at C6, which is a simple snapback.
loadsgf games/blunder5.sgf
5 gg_genmove black
#? [A4]

loadsgf games/blunder6.sgf 1
6 gg_genmove white
#? [D4]

# A9 is a losing ko threat
loadsgf games/blunder6.sgf 3
7 gg_genmove white
#? [E3]

# Both A3 and A5 are blunders due to a spectacular atari-atari sequence.
loadsgf games/blunder7.sgf
8 gg_genmove black
#? [!(A3|A5)]

# After black D9, white cuts at G8 and gets a ko.
loadsgf games/blunder8.sgf
9 gg_genmove black
#? [G8|G9|H8]

# Clearly, black can't start with J9.
loadsgf games/blunder9.sgf
10 gg_genmove black
#? [G9|F9|C7]

# Black B3 loses at least five stones.
loadsgf games/blunder10.sgf 1
11 gg_genmove black
#? [D4|E4|E5|F4|C6]

# White D4 kills at least five stones.
loadsgf games/blunder10.sgf 2
12 gg_genmove white
#? [D4]*
13 gg_genmove black
#? [D4|E4|E5|F4]

loadsgf games/blunder11.sgf 42
14 gg_genmove black
#? [G4]

loadsgf games/blunder11.sgf 44
15 gg_genmove black
#? [H3]

loadsgf games/blunder12.sgf 252
16 gg_genmove white
#? [R5|Q5|R6|S6|S7|R7]

loadsgf games/blunder12.sgf 258
17 gg_genmove white
#? [R7]

# A simplification of incident 136. Black E6 is a bad blunder.
loadsgf games/blunder13.sgf
18 gg_genmove black
#? [F5]

loadsgf games/blunder14.sgf
19 gg_genmove white
#? [P18|P19|R18|R19]
