# CATEGORY=FUSEKI_STRATEGY
# SEVERITY=5
loadsgf games/arend/gnugo-gnugo1.sgf 25
1 gg_genmove black
#? [C4]*

# CATEGORY=KO
# SEVERITY=3
loadsgf games/arend/gnugo-gnugo1.sgf 112
2 gg_genmove white
#? [H15]

# CATEGORY=CONNECTION
# SEVERITY=10
loadsgf games/arend/gnugo-gnugo1.sgf 139
3 gg_genmove black
#? [P6|Q5]*

# These kind of fights might be very difficult to do well
# with current GNU Go methods (How should it choose between the different
# 'correct' alternatives here? - R14 seems clearly best to me).
# But certainly it should not tenuki.
loadsgf games/arend/exper1.sgf 7
4 gg_genmove black
#? [R14|S15|R17]

#CATEGORY=STRATEGY
#DESCRIPTION=GNU Go should escape with its cutting stone.
loadsgf games/arend/exper1.sgf 25
5 gg_genmove black
#? [D7|E7|E6]*

#CATEGORY=STRATEGY
#DESCRIPTION=In this position, GNU Go plays L10 because of an enormous assumed strategic effect on the M16 dragon, which is quite safe.
loadsgf games/arend/gnugo-gnugo2.sgf 57
7 gg_genmove black
#? [C3]

#CATEGORY=BLUNDER
# S 16 can be captured. S17 is better, but another big move
# should be accepted as well
loadsgf games/arend/gnugo-gnugo2.sgf 60
8 gg_genmove white
#? [!S16]

#CATEGORY=TACTICAL_READING
# GNU Go rejects S17 because it is "strategically or tactically unsafe".
# Must be a misread.
loadsgf games/arend/gnugo-gnugo2.sgf 61
9 gg_genmove black
#? [S17]

#CATEGORY=STRATEGY
# J7 is extremely overvalued. This is caused by pattern CB11.
loadsgf games/arend/gnugo-gnugo2.sgf 73
10 gg_genmove black
#? [!J7]

#CATEGORY=STRATEGY
# K6 is overvalued by GNU Go 3.0.0.
loadsgf games/arend/gnugo-gnugo2.sgf 91
11 gg_genmove black
#? [!K6]

#CATEGORY=BLUNDER
loadsgf games/arend/gnugo-gnugo2.sgf 163
12 gg_genmove black
#? [C16|B16]

#CATEGORY=STRATEGY
# GNU Go does not see that P17 helps his group as well, and plays O13.
loadsgf games/arend/gnugo-gnugo4.sgf 27
14 gg_genmove black
#? [P17]*

#CATEGORY=FUSEKI_STRATEGY
# C6 is clearly the wrong side for approaching LL corner.
loadsgf games/arend/gnugo-gnugo4.sgf 31
15 gg_genmove black 1
#? [F3]*


# GNU Go loves this move! (H2 is the point to play here.) It is impossible
# to teach GNU Go not to play there with minor pattern tuning. Problem is
# huge follow_up value; there is no check at all in the move valuation 
# whether a sente move is actually worth something if opponent answers
# (here it is aji-keshi).
# This looses only a few points, but comes up very often.
loadsgf games/arend/gnugo-gnugo4.sgf 45
16 gg_genmove black
#? [!G3]

#CATEGORY=TACTICAL_READING
# GNU Go moves at C9. I do not know what it is afraid of.
loadsgf games/arend/gnugo-gnugo4.sgf 153
17 gg_genmove black
#? [C7]*

# Maybe this problem is suboptimal, because there is a ko involved, and
# GNU Go gets it wrong even without the ko.
loadsgf games/arend/gnugo-gnugo4.sgf 168
18 gg_genmove white
#? [P18|P19|R18|R19|S18|S17|S16|S14|S13|T14|T16]

#CATEGORY=PATTERN_TUNING
# GNU Go often plays this inefficient shape. Should be correctable with an
# additional pattern. Core of the problem is however that GNU Go does not
# understand the essence of the iken tobi -- it can be cut, but Black does
# not mind that, since a cut only strengthens Black.
loadsgf games/arend/gnugo-gnugo5.sgf 39
20 gg_genmove black
#? [!L15]

#CATEGORY=STRATEGY
# This inefficient move is produced by owl. But of course it should not
# be owl in charge of strengthening this big dragon.
loadsgf games/arend/gnugo-gnugo5.sgf 78
21 gg_genmove white
#? [!F9]

# Immediately take the double sente point!
loadsgf games/arend/gnugo-gnugo5.sgf 80
22 gg_genmove white
#? [D14]*

# H10 is quite overvalued here.
loadsgf games/arend/gnugo-gnugo5.sgf 105
23 gg_genmove black
#? [!H10]

# Low Shimari clearly better here
loadsgf games/arend/gnugo-gnugo6.sgf 20
24 gg_genmove white 4
#? [!P16]*

#CATEGORY=ENDGAME_TUNING
# C19 is either double sente, or worth at least 5 pts in reverse sente,
# which is 10 pts. T14 (chosen by GNU Go) is dame.
loadsgf games/arend/gnugo-gnugo6.sgf 174
25 gg_genmove white
#? [C19]

#CATEGORY=FUSEKI_STRATEGY
# D17 is wrong direction; in this position this is really bad.
loadsgf games/arend/gnugo-gnugo7.sgf 17
28 gg_genmove black
#? [C16]

#CATEGORY=FUSKEI_STRATEGY
# Black C15 is sente here, so W C14 is mandatory.
loadsgf games/arend/gnugo-gnugo7.sgf 24
29 gg_genmove white
#? [B14|C14]*

#CATEGORY=BLUNDER
# GNU Go played F8.
loadsgf games/arend/gnugo-gnugo7.sgf 79
30 gg_genmove black
#? [C13]

#CATEGORY=OWL_DEFEND (?)
# B14 can (and should) be saved here
loadsgf games/arend/gnugo-gnugo7.sgf 81
31 gg_genmove black
#? [B13|C13]
 
#CATEGORY=CONNECTION
#SEVERITY=9
loadsgf games/arend/gnugo-gnugo7.sgf 137
32 gg_genmove black
#? [F9|E8]
 
loadsgf games/arend/gnugo-gnugo7.sgf 140
33 gg_genmove white
#? [F9]*
 
#CATEGORY=ENDGAME_TUNING
loadsgf games/arend/gnugo-gnugo7.sgf 173
34 gg_genmove black
#? [!K8]
 
# This is a serious mistake. H17 is not huge, its HUGE.
loadsgf games/arend/gnugo-gnugo8.sgf 32
35 gg_genmove white
#? [H17|J19]

#CATEGORY=ENDGAME_TUNING
# Its just one point difference, but it happens a couple of times every
# game.
loadsgf games/arend/constructed1.sgf
36 gg_genmove black
#? [C5]

#CATEGORY=ENDGAME_TUNING
loadsgf games/arend/constructed2.sgf
37 gg_genmove black
#? [A5]
 
#CATEGORY=STRATEGY
# I think this problem summarizes well two common misjudgements 
# of the influence function:
# 1. Breaking into an opponents moyo is enormously
# undervalued (that is why patterns like LE14--safe jump towards
# oppoents moyo--need to have a fixed minimum value, which often leads
# bad moves)
# 2. O5 considerably weakens the influence of N5. Maybe this is not weighted
# highly enough by GNU Go?
loadsgf games/arend/constructed3.sgf
38 gg_genmove white
#? [O5]

#CATEGORY=STRATEGY
# This is an example for part 1 of my comment of the previous problem.
# This slightly adapted from a game (to make the problem more specific).
loadsgf games/arend/constructed4.sgf
39 gg_genmove black 3
#? [G10|H10|F13]

#CATEGORY=ENDGAME_TUNING
# EB714 is necessary here; without it, GNU Go played B6, only for the
# reason of filling a liberty
loadsgf games/arend/constructed5.sgf
40 gg_genmove white
#? [B7]

#CATEGORY=ENDGAME_TUNING
# EB715 necessary
loadsgf games/arend/constructed6.sgf
41 gg_genmove white
#? [B6]
