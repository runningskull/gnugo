# =============================
# Private test set nicklas4.tst
# These tests are from 19x19 games.
# None of these tests passes with version 2.7.222.
# I think these problems are "simple".
# =============================

loadsgf games/nicklas/nicklas8.sgf 146
805 reg_genmove black
#? [C5|B4]

loadsgf games/nicklas/nicklas8.sgf 212
809 reg_genmove black
#? [B10]

# Changed correct answer from K12 to J12. /gf
# K14 also secures a connection of all important stones but is one
# point worse than J12. K12 loses the tail.
loadsgf games/nicklas/nicklas8.sgf 242
812 reg_genmove black
#? [J12]

# O9 does leave a ko but is not out of the question. /gf
loadsgf games/nicklas/nicklas11.sgf 56
1102 reg_genmove black
#? [P9|O9]

loadsgf games/nicklas/nicklas11.sgf 172
1103 reg_genmove black
#? [C2|C3]

# It seems that GNU Go makes a logical mistake here: After seeing that
# R9 can be defended with S7, and that Q8 can be defended with S7, I
# doubt it checks whether both will be defended by S7. /ab
loadsgf games/nicklas/nicklas12.sgf 29
1201 reg_genmove black
#? [Q9]

# # I have some doubt about the correct move. I might be advisable for B
# # to defend his center group, although W E16 hurts, of course. However,
# # W's attack after W N11, B N12 and next W L9 or similar might be
# # very useful as well.
# # gf This testcase is too unclear, removed. (3.5.3)
# loadsgf games/nicklas/nicklas12.sgf 95
# 1205 reg_genmove black
# #? [E16]*

loadsgf games/nicklas/nicklas12.sgf 215
1215 dragon_status A4
#? [critical A6 (A1|A6)]

# Changed move number from 185 to 186. /gf
loadsgf games/nicklas/nicklas15.sgf 186
1501 reg_genmove black
#? [D17]
