# GNU Go 3.7.8 plays A1 on levels 11 and up.
loadsgf games/kgs/tournament11/GNU-firstgo.sgf 58
1 reg_genmove white
#? [E12|K4]

# G4 meaningless compared to K3.
loadsgf games/kgs/tournament11/GNU-firstgo.sgf 62
2 restricted_genmove white G4 K3
#? [K3]*

# J13 locally better than L12.
loadsgf games/kgs/tournament11/GNU-firstgo.sgf 104
3 restricted_genmove white J13 L12
#? [J13]*

# G13 accomplishes nothing. Better play the last point at N7.
loadsgf games/kgs/tournament11/GNU-firstgo.sgf 142
4 reg_genmove white
#? [N7]*

# Urgent to defend on the left. D6 looks simplest. Does any other move work?
loadsgf games/kgs/tournament11/GNU-viking5-1.sgf 48
11 reg_genmove white
#? [D6]*

# H11 locally better than K10.
loadsgf games/kgs/tournament11/GNU-viking5-1.sgf 88
12 restricted_genmove white H11 K10
#? [H11]*

# Better to invade a corner straight away. L11 best?
loadsgf games/kgs/tournament11/GNU-CrazyStone.sgf 12
21 restricted_genmove white H2 L11
#? [L11]*

# L7 looks strange. J2 or L11 make more sense.
loadsgf games/kgs/tournament11/GNU-CrazyStone.sgf 18
22 restricted_genmove white J2 L7 L11
#? [J2|L11]*

# Best to fix up the connection along the upper edge with E12. B11 overplay.
loadsgf games/kgs/tournament11/GNU-CrazyStone.sgf 100
23 reg_genmove white
#? [E12]*

# Urgent to live in the upper left corner.
loadsgf games/kgs/tournament11/GNU-CrazyStone.sgf 108
24 reg_genmove white
#? [B12|B13|A11]*

# C1 kills cleanly. C2 doesn't even make ko.
loadsgf games/kgs/tournament11/GNU-viking5-2.sgf 82
31 reg_genmove white
#? [C1]*

# M9 gives a clean tactical capture which looks superior to the game move J12.
loadsgf games/kgs/tournament11/CrazyStone-GNU.sgf 45
41 reg_genmove black
#? [M9]*

# G13 secures two eyes for the top black stones and a trivial
# win of the semeai.
loadsgf games/kgs/tournament11/CrazyStone-GNU.sgf 77
42 reg_genmove black
#? [G13]*

# G13 is still sufficient to get a seki.
loadsgf games/kgs/tournament11/CrazyStone-GNU.sgf 87
43 reg_genmove black
#? [G13]
