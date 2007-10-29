# =============================
# Private test set nicklas1.tst
# All of these tests passes with version 2.7.222.
#
# Note: This is one of the oldest test files in GNU Go. Some of the
#       negative tests have been removed because a modern GNU Go
#       would never consider those moves.
# =============================

# # D9 is too weird to be considered anymore. Removed.
# loadsgf games/nicklas/nicklas1.sgf 9
# 101 reg_genmove black
# #? [!D9]

# # No longer interesting.
# loadsgf games/nicklas/nicklas2.sgf 5
# 201 reg_genmove black
# #? [!G8|D8|F8]

# # H8 is too weird to be considered anymore. Removed.
# loadsgf games/nicklas/nicklas2.sgf 9
# 202 reg_genmove black
# #? [!H8]

#CATEGORY=OWL_TUNING
#DESCRIPTION=Combination threat to live / connect.
#SEVERITY=4
loadsgf games/nicklas/nicklas2.sgf 23
203 reg_genmove black
#? [J5]

##tm - if B J8, W J5 kills bigger B corner.  
##So, there's no really effective move here.
## GNU Go 3.1.16 gets J5, which is prob. best.
#loadsgf games/nicklas/nicklas2.sgf 39
#204 reg_genmove black
##? [F8]*

loadsgf games/nicklas/nicklas2.sgf 55
205 dragon_status G1
#? [dead]

loadsgf games/nicklas/nicklas3.sgf 49
301 reg_genmove black
#? [H3|H6]

loadsgf games/nicklas/nicklas5.sgf 23
501 reg_genmove black
#? [G7]

loadsgf games/nicklas/nicklas5.sgf 27
502 reg_genmove black
#? [G5]

loadsgf games/nicklas/nicklas6.sgf 31
# semeai problem
603 reg_genmove black
#? [C5]

loadsgf games/nicklas/nicklas6.sgf 41
604 reg_genmove black
#? [A7|J9]

loadsgf games/nicklas/nicklas8.sgf 96
801 reg_genmove black
#? [P16]

# G2 destroys eyeshape but the semeai can't be won.
loadsgf games/nicklas/nicklas8.sgf 162
806 restricted_genmove black G2 D8
#? [D8]

loadsgf games/nicklas/nicklas8.sgf 164
807 restricted_genmove black H1 J2
#? [J2]

loadsgf games/nicklas/nicklas8.sgf 180
808 restricted_genmove black L2 N6
#? [N6]

loadsgf games/nicklas/nicklas8.sgf 224
810 restricted_genmove black O7 C19
#? [O7]

# Moved K15 to K14. /gf ; Added back K14 - both work.
#SEVERITY=7
#CATEGORY=CONNECTION
#DESCRIPTION=Is GnuGo misreading the cut?
loadsgf games/nicklas/nicklas8.sgf 240
811 reg_genmove black
#? [K14|K15]*

loadsgf games/nicklas/nicklas9.sgf 28
901 reg_genmove black
#? [H8]

loadsgf games/nicklas/nicklas10.sgf 30
1002 reg_genmove black
#? [H8]

loadsgf games/nicklas/nicklas10.sgf 56
1003 reg_genmove black
#? [B1]

# A ko threat would have been appropriate, but none exists.
loadsgf games/nicklas/nicklas10.sgf 64
1004 reg_genmove black
#? [PASS]

loadsgf games/nicklas/nicklas10.sgf 66
1005 reg_genmove black
#? [PASS]

loadsgf games/nicklas/nicklas10.sgf 68
1006 reg_genmove black
#? [PASS]

loadsgf games/nicklas/nicklas11.sgf 16
1101 restricted_genmove black D11 C8
#? [C8]

loadsgf games/nicklas/nicklas11.sgf 174
1104 restricted_genmove black A15 C1
#? [C1]*

loadsgf games/nicklas/nicklas11.sgf 180
1105 restricted_genmove black F2 H14
#? [F2]

# Q6 seems largest but we may want to accept some more moves. The
# original formulation !F6 isn't very good since a one point reverse
# sente move isn't all that bad in the position.
loadsgf games/nicklas/nicklas11.sgf 208
1106 reg_genmove black
#? [Q6]*

#CATEGORY=WASTED_MOVE
#DESCRIPTION=GnuGo 3.1.8 gets N2 on next move;  R3 benign ko threat loss.
#SEVERITY=2
loadsgf games/nicklas/nicklas11.sgf 242
1107 reg_genmove black
#? [N2]*

# Thrashing dragon. Q18 is cleanest but R18 and pass are also acceptable.
loadsgf games/nicklas/nicklas11.sgf 258
1108 reg_genmove black
#? [Q18|R18|PASS]

loadsgf games/nicklas/nicklas11.sgf 260
1109 reg_genmove black
#? [Q18]

# Thrashing dragon. O18 is the only interesting restraining move.
loadsgf games/nicklas/nicklas11.sgf 268
1110 reg_genmove black
#? [O18|PASS]

loadsgf games/nicklas/nicklas11.sgf 270
1111 reg_genmove black
#? [N18]

# Q19 is not a fail, it does work as well. /pp
loadsgf games/nicklas/nicklas11.sgf 274
1112 reg_genmove black
#? [N19|Q19]

loadsgf games/nicklas/nicklas12.sgf 103
1206 reg_genmove black
#? [C15]

#CATEGORY=PATTERN_TUNING
#DESCRIPTION=Needless 2 point loss for B.
#SEVERITY=7
loadsgf games/nicklas/nicklas12.sgf 113
1207 restricted_genmove black B18 C18 B17 C17
#? [C17]

loadsgf games/nicklas/nicklas12.sgf 117
1208 reg_genmove black
#? [E19]

# L19 isn't even close to living.
loadsgf games/nicklas/nicklas12.sgf 121
1209 restricted_genmove black L19 B14
#? [B14]

loadsgf games/nicklas/nicklas12.sgf 123
1210 dragon_status D18
#? [dead]

#CATEGORY=SEMEAI
#DESCRIPTION=
#SEVERITY=
loadsgf games/nicklas/nicklas12.sgf 203
1213 reg_genmove black
#? [N4]

# Added A6 among the correct moves. /gf (2.7.203)
loadsgf games/nicklas/nicklas12.sgf 215
1214 reg_genmove black
#? [A1|A6]

# H3 doesn't seem critical. /gf
loadsgf games/nicklas/nicklas12.sgf 259
1216 reg_genmove black
#? [H3]*

loadsgf games/nicklas/nicklas13.sgf 57
1302 reg_genmove black
#? [J3]

loadsgf games/nicklas/nicklas13.sgf 58
1303 dragon_status G8
#? [dead]

loadsgf games/nicklas/nicklas13.sgf 73
1304 reg_genmove black
#? [PASS]

loadsgf games/nicklas/nicklas14.sgf 51
1404 reg_genmove black
#? [J7]

loadsgf games/nicklas/nicklas14.sgf 55
1405 reg_genmove black
#? [B7]

# Semeai problem
loadsgf games/nicklas/nicklas14.sgf 57
1406 reg_genmove black
#? [B6]

loadsgf games/nicklas/nicklas14.sgf 61
1408 dragon_status A8
#? [alive]

loadsgf games/nicklas/nicklas15.sgf 196
1502 reg_genmove black
#? [D14]

loadsgf games/nicklas/nicklas15.sgf 208
1504 restricted_genmove black F16 D15
#? [D15]

loadsgf games/nicklas/nicklas15.sgf 212
1505 reg_genmove black
#? [T14]

loadsgf games/nicklas/nicklas15.sgf 224
1506 reg_genmove black
#? [S4]

# # This is virtually identical with 1506, let's skip it.
# loadsgf games/nicklas/nicklas15.sgf 228
# 1507 reg_genmove black
# #? [!T1]

# # This one too.
# loadsgf games/nicklas/nicklas15.sgf 232
# 1508 reg_genmove black
# #? [!F15]

loadsgf games/nicklas/nicklas15.sgf 258
1509 restricted_genmove black K4 L1 T14
#? [T14]

loadsgf games/nicklas/nicklas15.sgf 284
1510 reg_genmove black
#? [C12]

loadsgf games/nicklas/nicklas15.sgf 296
1511 reg_genmove black
#? [PASS]

loadsgf games/nicklas/nicklas15.sgf 298
1512 reg_genmove black
#? [PASS]

loadsgf games/nicklas/nicklas16.sgf 22
1601 restricted_genmove black D9 D7
#? [D7]

loadsgf games/nicklas/nicklas16.sgf 28
1602 restricted_genmove black H8 B8
#? [B8]

# Missed connection. See also connection:26,27
loadsgf games/nicklas/nicklas16.sgf 38
1603 reg_genmove black
#? [B4]

loadsgf games/nicklas/nicklas16.sgf 62
1604 reg_genmove black
#? [J2]

loadsgf games/nicklas/nicklas16.sgf 64
1605 reg_genmove black
#? [J1]

#CATEGORY=OWL_TUNING
#DESCRIPTION=No reason to give W a chance for ko here.
#SEVERITY=6
#The problem with D5 is the same as in nicklas2:1802
loadsgf games/nicklas/nicklas18.sgf 15
1801 reg_genmove black
#? [D2]

loadsgf games/nicklas/nicklas19.sgf 34
1901 reg_genmove black
#? [B7]

loadsgf games/nicklas/nicklas19.sgf 46
1902 reg_genmove black
#? [B7]

loadsgf games/nicklas/nicklas19.sgf 52
1903 reg_genmove black
#? [A9]

loadsgf games/nicklas/nicklas19.sgf 60
1904 reg_genmove black
#? [H9|J8]

loadsgf games/nicklas/nicklas20.sgf 37
2001 reg_genmove black
#? [G5]

# Is there any way to live in the upper right? If so that's probably better.
loadsgf games/nicklas/nicklas20.sgf 41
2002 reg_genmove black
#? [J5]

# White is thrashing but E1 is not a very meaningful move.
loadsgf games/nicklas/nicklas21.sgf 52
2101 reg_genmove black
#? [PASS|A7|A5|D1|A6|G1]

loadsgf games/nicklas/nicklas23.sgf 26
2301 reg_genmove black
#? [G6|F7|H8]

loadsgf games/nicklas/nicklas23.sgf 40
2302 reg_genmove black
#? [F4]

loadsgf games/nicklas/nicklas23.sgf 52
2303 reg_genmove black
#? [G9]

loadsgf games/nicklas/nicklas23.sgf 60
2304 reg_genmove black
#? [PASS]

loadsgf games/nicklas/nicklas23.sgf 62
2305 reg_genmove black
#? [PASS]
