# =============================
# Private test set nicklas2.tst
# These tests are from 9x9 games.
# None of these tests passes with version 2.7.222.
# I think these problems are "simple".
# =============================

#CATEGORY=ATARI_ATARI
#DESCRIPTION=GnuGo misses followup to D4 push-through.
#SEVERITY=1
#    This is not as clear cut as it may seem. 2.7.228 values the endgame 
#    move at A7 to 6.66 points which seems about correct. The fact that 
#    white D4 threatens both to cut off E5-F6 and to capture D3 through 
#    a chain of threats (which is what happens in the game) is not that
#    easy to understand.  A more general atari_atari module could 
#    perhaps see it, but we are still far from that.  I think this test
#    should be in nicklas3.tst (harder problems).
loadsgf games/nicklas/nicklas1.sgf 29
102 reg_genmove black
#? [D4]

#CATEGORY=PATTERN_TUNING
#DESCRIPTION=I think G9 is not too terrible here. Very difficult to see.-trevor
#SEVERITY=1
#    This test illustrates a general weakness of GNU Go: It's poor 
#    ability to defend its borders.  White can enter blacks domains or
#    capture the E8 group. In the latter case, whites first move is
#    a sacrifice and the capture happens through a chain of threats.
#Added E6, still not thinking G9 is so bad. -trevor
loadsgf games/nicklas/nicklas6.sgf 23
601 reg_genmove black
#? [D8|C8|D6|D4|E6]

#CATEGORY=SEMEAI_MODULE
#DESCRIPTION=Very tough position.  Even if F1 for B, W E7 is tough.
#SEVERITY=1
#    This is a very good test case for a semeai module. Black F1 threatens
#    to live and creates all the necessary liberties to win the semai.
#I disagree; this it tough; a more clear-cut test case would be helpful
#to better isolate the inherent problem. - trevor
loadsgf games/nicklas/nicklas7.sgf 29
701 reg_genmove black
#? [F1]*


#CATEGORY=SEMEAI
#DESCRIPTION=Tough for white to see attacking B, and linking w/ attack stone.
#SEVERITY=3
#    The only way to live is to attack a neighbour dragon, e.g. the one
#    at F8. But does the owl code really do this by default?
#
#    No, this is up to the semeai module to resolve. /gf
#
loadsgf games/nicklas/nicklas9.sgf 28
902 dragon_status E9
#? [critical (.*) (.*)]

903 dragon_status G7
#? [critical (H8|F9) H8]

# E1 loses some endgame points since black must capture later anyhow, 
# but it saves the group. It could be regarded as acceptable at this 
# point.
loadsgf games/nicklas/nicklas9.sgf 50
904 reg_genmove black
#? [B1|E1]

loadsgf games/nicklas/nicklas10.sgf 18
1001 reg_genmove black
#? [H8]

loadsgf games/nicklas/nicklas13.sgf 49
1301 reg_genmove black
#? [B8|B9]

#CATEGORY=TACTICAL_READING
#DESCRIPTION=Fighting Ko here is not unreasonable for B.
#SEVERITY=0
#I would delete this regression test. -trevor
loadsgf games/nicklas/nicklas14.sgf 31
1401 reg_genmove black
#? [B3]

#CATEGORY=OWL_TUNING
#DESCRIPTION=Black must protect his group.  H9 is thinkable also.
#SEVERITY=9
loadsgf games/nicklas/nicklas14.sgf 43
1402 reg_genmove black
#? [J8|J6]

#CATEGORY=SEMEAI
#DESCRIPTION=PASS here is unthinkable, though RESIGN might be appropriate!
#SEVERITY=5
# One eye wins against no eye
#Note: Owl & Dragon status is both critical for the W group.  Why
#  would B pass when there's a critical group on the board? Owl & 
#  Dragon status for all of the B stones is Dead.  Can a Critical
#  group kill another group???
loadsgf games/nicklas/nicklas14.sgf 61
1407 reg_genmove black
#? [A6]


#CATEGORY=PATTERN_TUNING
#DESCRIPTION=Protect those borders!
#SEVERITY=8
#Added A8, for the faint of heart.
# It's black to move!. The business is to kill white, which is most
# solidly done with C9, but I think A5 and maybe some other moves
# work as well. /gf
loadsgf games/nicklas/nicklas17.sgf 47
1701 reg_genmove black
#? [C9|A8]

# Owl reading problem. See also owl:228.
loadsgf games/nicklas/nicklas18.sgf 17
1802 reg_genmove black
#? [E2]


#CATEGORY=WASTED_MOVE
#DESCRIPTION=Um, you're still dead after B1, but sente!
#SEVERITY=1
#SEE_ALSO=trevor.tst?310
loadsgf games/nicklas/nicklas18.sgf 45
1803 reg_genmove black
#? [A6|A8]


#CATEGORY=ENDGAME_TUNING
#DESCRIPTION=OWL & Dragon status for W is all dead.  Why does B play?
#SEVERITY=5
loadsgf games/nicklas/nicklas21.sgf 72
2102 reg_genmove black
#? [PASS]


#CATEGORY=ENDGAME_TUNING
#DESCRIPTION=OWL & Dragon status for W is all dead.  Why does B play?
#SEVERITY=5
#See also nicklas2.tst?2102
loadsgf games/nicklas/nicklas21.sgf 74
2103 reg_genmove black
#? [PASS]

loadsgf games/nicklas/nicklas21.sgf 76
2104 reg_genmove black
#? [PASS]

loadsgf games/nicklas/nicklas22.sgf 42
2201 reg_genmove black
#? [C2]

loadsgf games/nicklas/nicklas22.sgf 50
2202 reg_genmove black
#? [B1]

#CATEGORY=ATARI_ATARI
#DESCRIPTION=Black must defend here - yikes!
#SEVERITY=10
loadsgf games/nicklas/nicklas24.sgf 41
2401 reg_genmove black
#? [G3|G2]
