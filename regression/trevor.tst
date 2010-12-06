#terrible connection mistake.
loadsgf games/trevor/trevor_01.sgf 28
1 reg_genmove white
#? [G5]

#probably a screwed up fuseki library!
#CATEGORY=JOSEKI_DATABASE
#DIFFICULTY=1
loadsgf games/trevor/trevor_02.sgf
2 reg_genmove white
#? [!PASS]

# underlying reading problem
#Gunnar wrote:
#
#> ./gnugo --quiet -a -w -t -l ../regression/games/unsorted_03.sgf -L 29
#> --decide-string E4 -o vars.sgf
#> 
#> giving the result
#> 
#> | E4 cannot be attacked (874 variations)
#> 
#> So here's the problem. Usually GNU Go is quite strong at tactical
#> reading and it might be expected to read this position correctly.
#> Closer analysis of the vars.sgf file shows that GNU Go is confused by
#> a few meaningless stalling moves from white (like throwin at D5 and
#> atari at F6) and the corner ko potential. This can probably be fixed,
#> but it's not easy and it may involve complex tradeoffs between speed
#> and accuracy.
#
#If you increase the level, it sees the attack but misses that
#it is unconditional. It sees a ko in the corner after
#
#B:G4 W:F2 B:H2 W:H3 B:H4 W:H1 B:J3 etc.
#
#Dan
#CATEGORY=TACTICAL_READING
#DIFFICULTY=8
#PREDECESSOR=reading.tst.154
loadsgf games/trevor/trevor_03.sgf 28
3 reg_genmove white
#? [!E3]

#simple edge block missed.
loadsgf games/trevor/trevor_04.sgf 30
4 reg_genmove white
#? [G1]

#E5 is the only move.  Is GNU Go playing somewhat randomly here?
#Dan said:
#Looking at unsorted test 5, a typical way to solve this would be
#to add an edge Joseki pattern, such as this:
#
#Pattern EJ84
#
#......       Atari before connecting
#..*...
#X.XO..
#.XO...
#......
#......
#------
#
#:8,J
#
#One should also add a pattern or two for followup. A pattern
#with type J is given a weight of 27 on a 19x19 board, which
#means that it is played automatically unless there is something
#really huge on the board.
#
#Unfortunately, this does not work. The problem is that the values of
#J patterns are scaled. From shapes.c:
#
#  if (class & CLASS_J) {
#    TRACE("...joseki standard move\n");
#    add_expand_territory_move(ti, tj);
#    TRACE("...expands territory\n");
#    add_expand_moyo_move(ti, tj);
#    TRACE("...expands moyo\n");
#    set_minimum_move_value(ti, tj, 27 * board_size / 19.0);
#    TRACE("... minimum move value %f\n", 27 * board_size / 19.0);
#  }
#CATEGORY=JOSEKI_DATABASE
#DIFFICULTY=8
loadsgf games/trevor/trevor_05.sgf 6
5 reg_genmove white
#? [E5]

#locally B9 is terrible.  There are other big moves on the board.
loadsgf games/trevor/trevor_06.sgf 22
6 reg_genmove white
#? [B7|C6|F5]

#B5 is certainly better than C5 here.  This is a terrible
#game for white, but it's hard to narrow down the obvious
#blunders.
#Per Dan:  This needs to be fixed. It is probably a
#matter of owl tuning and may require some thought.
#CATEGORY=OWL_TUNING
#DIFFICULTY=6
loadsgf games/trevor/trevor_07.sgf 32
7 reg_genmove white
#? [!C5]

#Pattern missing here?  B8 is almost always better than B9.
loadsgf games/trevor/trevor_08.sgf 24
8 reg_genmove white
#? [!B9]

#E5 (or some defence of D4 worm) is necessary.
#Dan: This should be solved by a reading connection
#analyzer. Tristan Cazenave is working on such a project.
#See also connection:21,22.
#CATEGORY=CONNECTION
loadsgf games/trevor/trevor_09.sgf
9 reg_genmove white
#? [E5]*

#This is nearly trivial, it would seem!
loadsgf games/trevor/trevor_10.sgf
10 reg_genmove white
#? [E6]

#Yikes, gnugo can't play into here.  See also the next problem.
loadsgf games/trevor/trevor_11.sgf
11 reg_genmove white
#? [!B2]*

#J2 is terrible.  H1 leads at least to a Ko.
loadsgf games/trevor/trevor_12.sgf
12 reg_genmove black
#? [H1]

#E2 is obvious.
loadsgf games/trevor/trevor_13.sgf
13 reg_genmove black
#? [E2]

#Must protect double cut at C2 & E2
loadsgf games/trevor/trevor_14.sgf
14 reg_genmove white
#? [!PASS]

# Protecting at E7 is very necessary.
# See also connection:117.
loadsgf games/trevor/trevor_15.sgf 
15 reg_genmove white
#? [E7]

# C1 looks best here. A7 very unorthodox, and at least worse for ko
# threats. Since the lower left is the focus of the next test, this
# test is restricted to the upper left corner.
loadsgf games/trevor/trevor_16.sgf 53
160 restricted_genmove black A7 A8 A9 B8 B9 C7 C8 C9 D8
#? [B8|C8]*

# Protecting lower left group better.
loadsgf games/trevor/trevor_16.sgf 54
161 restricted_genmove white B1 F9
#? [B1]*

# W can live here unconditionally.
# A2 is inferior as black gets seki with B1. /ab
loadsgf games/trevor/trevor_16.sgf 58
162 reg_genmove white
#? [B1]*

# If Black B9, White can't connect at C7.
loadsgf games/trevor/trevor_16.sgf 67
163 reg_genmove black
#? [B9]

# Must connect dragons.
# B6 should be valued much much lower than B5
loadsgf games/trevor/trevor_17.sgf 22
170 reg_genmove white
#? [B5]

# C8 is terrible.
loadsgf games/trevor/trevor_17.sgf 28
171 reg_genmove white
#? [!C8]

# Snapback!
loadsgf games/trevor/trevor_18.sgf 32
180 reg_genmove white
#? [!A8]

# Whoops, don't let go of the dragon's tail!
loadsgf games/trevor/trevor_19.sgf 12
190 reg_genmove white
#? [B7]

# W should connect his two dragons.
loadsgf games/trevor/trevor_20.sgf 14
200 reg_genmove white
#? [D7|D4]*

# W can live on the right pretty easily (i.e. G8)
loadsgf games/trevor/trevor_20.sgf
201 reg_genmove white
#? [!PASS]

# W can live on the right pretty easily (i.e. G8)
# Black really does pass here, but eval.sh reports a PASSED result. (?)
loadsgf games/trevor/trevor_20.sgf
202 reg_genmove black
#? [!PASS]

# Huge negative sacrifice at D8.
loadsgf games/trevor/trevor_21.sgf 22
210 reg_genmove white
#? [!D8]

# In this case, the knight's move connection looks best.
# Black seems not to see the cut coming.
loadsgf games/trevor/trevor_22.sgf 23
220 reg_genmove black
#? [E8]

# F2 is not really sente, nor useful if it were!
loadsgf games/trevor/trevor_23.sgf 27
230 reg_genmove black
#? [C2|B2|C4]


# Connect the dragons!  One eye is not enough, anyway.
# See also owl.tst 221
loadsgf games/trevor/trevor_24.sgf 33
240 reg_genmove black
#? [E8|D8]


# G2 is a gift to white.
loadsgf games/trevor/trevor_25.sgf 31
250 reg_genmove black
#? [!G2]

# G1 is another gift to white.
loadsgf games/trevor/trevor_25.sgf 33
251 reg_genmove black
#? [!G1]


# Should connect further back, at least, but of course
# blocking at C1 is much bigger.
# 3.1.8 reports:
#  Move at C1 strategically or tactically unsafe
#  Move at F9 strategically or tactically unsafe
loadsgf games/trevor/trevor_26.sgf 33
260 reg_genmove black
#? [C1|F9]*

#Here's the problem:
#Top moves:
#1. F9  16.22
#2. A2  8.28
#Checking safety of a black move at F9
#Move at F9 would be a blunder.
loadsgf games/trevor/trevor_26.sgf 39
261 reg_genmove black
#? [F9]*

loadsgf games/trevor/trevor_26.sgf 33
trymove black C1
trymove white C2
trymove black F1
262 defend C2
#? [0]
popgo
popgo
popgo



# Ack, protect the cut!
loadsgf games/trevor/trevor_27.sgf 35
270 reg_genmove black
#? [E6|D6]


# D5 misses a big problem cut.
loadsgf games/trevor/trevor_28.sgf 27
280 reg_genmove black
#? [D4]

# F6 is much better and safer to boot than H6.
# culprit seems to be pattern LE4 (matches H6) - min. value 12.
# Perhaps need to replace H6 w/ F6?
loadsgf games/trevor/trevor_28.sgf 36
281 reg_genmove white
#? [F6]



# Yikes, short of liberties!
loadsgf games/trevor/trevor_29.sgf 54
290 reg_genmove white
#? [!A9]


# Black A7 is a serious threat.  Also, white should at least 
# try to invade.
loadsgf games/trevor/trevor_30.sgf
300 reg_genmove white
#? [!PASS]

# Black A7 is a serious threat.  Also, white should at least 
# try to invade.
loadsgf games/trevor/trevor_30.sgf
301 reg_genmove black
#? [!PASS]

#CATEGORY=TACTICAL_READING
#DESCRIPTION=A7 remains a 7 point threat.
#SEVERITY=6
loadsgf games/trevor/trevor_30.sgf
black G3
302 reg_genmove white
#? [!PASS]*

#CATEGORY=OWL_TUNING
#DESCRIPTION=Black still dead after D1!
#SEVERITY=10
loadsgf games/trevor/trevor_31.sgf
310 reg_genmove black
#? [A8]


loadsgf games/trevor/trevor_32.sgf
320 reg_genmove black
#? [J2|H3]

#duplicate of trevor:340
##loadsgf games/trevor/trevor_33.sgf
##330 owl_defend L9
###? [1 P9]*

#CATEGORY=owl_tuning
#DESCRIPTION=Very simple to threaten eye or run out here.
#SEVERITY=10
#SEE_ALSO=owl.tst@226
loadsgf games/trevor/trevor_34.sgf
340 owl_defend J10
#? [1 P9]

#CATEGORY=owl_tuning
#DESCRIPTION=Very simple Ko missed here.
#SEVERITY=10
#SEE_ALSO=owl.tst#226
loadsgf games/trevor/trevor_35.sgf
350 owl_defend J10
#? [(2|3) L8]

#CATEGORY=
#DESCRIPTION=
#SEVERITY=
loadsgf games/trevor/trevor_36.sgf
360 owl_defend G8
#? [(2|3) J9]*

#CATEGORY=OWL_BUG
#DESCRIPTION=Can safely connect out!
#SEVERITY=10
loadsgf games/trevor/trevor_37.sgf
370 owl_defend C9
#? [1 F8]


#CATEGORY=PATTERN_TUNING
#DESCRIPTION=E3 is certainly better locally
#SEVERITY=2
loadsgf games/trevor/trevor_38.sgf 22
380 reg_genmove white
#? [!E4]

#CATEGORY=READING
#DESCRIPTION=C6 is wrong way to protect the cut.  Loses one point.
#SEVERITY=3
# Added E8. /gf
loadsgf games/trevor/trevor_38.sgf 39
381 reg_genmove black
#? [D7|C7|E8]

#CATEGORY=KO_THREATS
#DESCRIPTION=E9 is terrible for leaving Ko threats.
#SEVERITY=1
loadsgf games/trevor/trevor_38.sgf 41
382 reg_genmove black
#? [E8]


#CATEGORY=PATTERN_TUNING
#DESCRIPTION=White misses cut.
#SEVERITY=10
loadsgf games/trevor/trevor_39.sgf 38
390 reg_genmove white
#? [!PASS]


#CATEGORY=OWL_TUNING
#DESCRIPTION=F8 is tough to find, but not too tough.
#SEVERITY=3
loadsgf games/trevor/trevor_40.sgf 1
400 reg_genmove black
#? [F8]

#CATEGORY=
#DESCRIPTION=White can live!
#SEVERITY=4
loadsgf games/trevor/trevor_40a.sgf 2
401 reg_genmove white
#? [F8]

#CATEGORY=
#DESCRIPTION=White can live!
#SEVERITY=8
loadsgf games/trevor/trevor_40.sgf 2
402 reg_genmove white
#? [!PASS]


#CATEGORY=PATTERN_TUNING
#DESCRIPTION=Black should block, not play B4.
#SEVERITY=5
loadsgf games/trevor/trevor_41.sgf 29
410 reg_genmove black
#? [D5|E5]*

#CATEGORY=BLUNDER
#DESCRIPTION=A3 leaves behind a double-atari.
#SEVERITY=10
loadsgf games/trevor/trevor_41.sgf 35
411 reg_genmove black
#? [!A3]

#CATEGORY=TACTICAL_READING
#DESCRIPTION=D4 worm is more important to save than B2.
#SEVERITY=3
loadsgf games/trevor/trevor_41.sgf 37
412 reg_genmove black
#? [C4]

#CATEGORY=KO
#DESCRIPTION=Terrible confusion on GNU Go's part here.
#SEVERITY=10
#Submitted by:softlife@8848.net (Cai Qiang)
#Cai said: I made a play between GNU Go 3.0.0 and FunGo with 3 
#  handicaps. Gnugo played quite well. And in the end it almost 
#  killed a corner of FunGo, but it played a big blunder:-(. 
#  Attachment is the SGF file.
# gf Added A18 and A16 as acceptable moves. (3.3.16)
loadsgf games/trevor/trevor_42.sgf 211
420 reg_genmove black
#? [G16|A18|A16]

#CATEGORY=TACTICAL_READING
#DESCRIPTION=Minus 1 point, attacking very dead stone.
#SEVERITY=10
loadsgf games/trevor/trevor_42.sgf 191
421 reg_genmove black
#? [!C2]


#CATEGORY=owl_tuning
#DESCRIPTION=Very simple problem.
#SEVERITY=10
loadsgf games/trevor/trevor_43.sgf
430 owl_attack F4
#? [1 G4]

#CATEGORY=owl_tuning
#DESCRIPTION=Simple double atari
#SEVERITY=4
#Note that owl_attack on E3 works, as these stones are treated
#as separate dragons.  Strange.  Is this a valid test?
#So, reg_genmove defends appropriately.
loadsgf games/trevor/trevor_44.sgf
440 owl_attack E6
#? [1 (F4|F5|E4)]

#CATEGORY=owl_tuning
#DESCRIPTION=Double-threat to eye-space missed.
#SEVERITY=10
loadsgf games/trevor/trevor_45.sgf
450 owl_attack E4
#? [1 D4]


#CATEGORY=OWL_TUNING
#DESCRIPTION=Missing simple cut pattern
#SEVERITY=10
#SEE_ALSO=owl:170
loadsgf games/trevor/trevor_46.sgf
460 owl_attack B14
#? [1 E14]

#SEE_ALSO trevor:460
461 reg_genmove black
#? [E14]


#CATEGORY=OWL_TUNING
#DESCRIPTION=Yikes, major Ko misunderstanding.
#SEVERITY=10
#SEE_ALSO=owl:181
loadsgf games/trevor/trevor_47.sgf
470 owl_attack G9
#? [(2|3) G6]

#SEE_ALSO=trevor:470
471 owl_attack A7
#? [(2|3) A3]


#CATEGORY=pattern_tuning
#DESCRIPTION=Of course, B8 is inferior to C9
#SEVERITY=3
#Shouldn't the move valuator include counting of the eye spaces?
loadsgf games/trevor/trevor_48.sgf
480 reg_genmove black
#? [C9]

#CATEGORY=owl_tuning
#DESCRIPTION=Does OWL really need to get this one right?
#SEVERITY=3
#SEE_ALSO=owl:191
#Is it in OWL's charter to get this problem?  
#attack gets it just fine, of course.
481 owl_attack C8
#? [1 B8]*


#CATEGORY=TACTICAL_READING
#DESCRIPTION=See problem this causes w/ owl in Pattern D805
#SEVERITY=7
loadsgf games/trevor/trevor_59.sgf
590 attack A18
#? [0]

loadsgf games/trevor/trevor_60.sgf 2
600 owl_attack E3
#? [1 E2]*


#CATEGORY=OWL
#DESCRIPTION=OWL code misreads cut.
#SEVERITY=10
loadsgf games/trevor/trevor_63.sgf
630 owl_attack G13
#? [1 H13]

#CATEGORY=OWL
#DESCRIPTION=Perhaps a semeai problem???
#SEVERITY=10
#SEE_ALSO=niki:9
loadsgf games/trevor/trevor_64.sgf
640 owl_defend G11
#? [1 (..+)]

#CATEGORY=OWL_TUNING
#DESCRIPTION=KO!
#SEVERITY=6
loadsgf games/trevor/trevor_65.sgf
650 owl_attack G2
#? [(2|3) E1]

#CATEGORY=OWL_TUNING
#DESCRIPTION=short of liberties
#SEVERITY=6
loadsgf games/trevor/trevor_66.sgf
660 owl_attack G2
#? [1 L1]

loadsgf games/trevor/trevor_67.sgf
670 attack G1
#? [(2|3) F1]

loadsgf games/trevor/trevor_67.sgf
671 defend G1
#? [1 (D1|C3|C1)]

loadsgf games/trevor/trevor_68.sgf
680 owl_attack H3
#? [1 H1]

loadsgf games/trevor/trevor_69.sgf
690 owl_attack H2
#? [1 G2]


#CATEGORY=OWL_OPTICS
#DESCRIPTION=Black plays on the incorrect marginal eye space.
#SEVERITY=10
loadsgf games/trevor/trevor_70.sgf
700 owl_attack H1
#? [1 (G5|J5|H5|G2)]

#CATEGORY=OWL_ESCAPE
#DESCRIPTION=black gets the killing move, creating a missed cutting point.
#SEVERITY=9
loadsgf games/trevor/trevor_71.sgf
710 owl_attack H1
#? [1 G5]*


loadsgf games/trevor/trevor_73.sgf
730 owl_attack M2
#? [(2|3) H1]*


loadsgf games/trevor/trevor_74.sgf
740 owl_attack B4
#? [1 A1]

loadsgf games/trevor/trevor_75.sgf
750 disconnect O9 O7
#? [0]


#CATEGORY=OWL_EYES
#DESCRIPTION=Fixed by eye pattern 402
#SEVERITY=10
#Simplified from owl:111
#Note the difficulty here, because the (only) correct vital point
#is also matched by many patterns.
loadsgf games/trevor/owl111a.sgf
1001 owl_defend C6
#? [1 (B8|K9)]


#CATEGORY=OWL
#DESCRIPTION=Escaping to no where.
#SEVERITY=7
#SEE_ALSO=global:16
loadsgf golois/Goemate990903-6.sgf
1010 owl_does_defend O13 O11 
#? [0]

#CATEGORY=OWL
#DESCRIPTION=Why is this an owl_escape?
#SEVERITY=8
loadsgf games/trevor/auto/d32.sgf 240
1020 owl_does_defend T13 S13
#? [0]*


loadsgf games/trevor/auto/d02.sgf 58
1030 attack B14
#? [0]

loadsgf games/trevor/auto/c17.sgf 55
1040 attack H13
#? [(2|3) H12]*

# gf B1 is also an effective attack.
loadsgf games/trevor/auto/c30.sgf 62
1050 owl_attack C3
#? [1 (C2|B1)]


loadsgf games/mertin13x13/katsunari-gnugo2.W+4.sgf 44
1060 owl_does_defend J4 E4
#? [1]


loadsgf games/trevor/trevor_20.sgf  14  
1070 owl_attack G5
#? [0]*


loadsgf games/nngs/theDoor-gnugo-3.3.11-200211130142.sgf 72
1100 reg_genmove black
#? [D15]*

#Bad pattern Shape55
#Fixed with revised constraint:
#   ;weak(a) && oplay_defend(*,B)
loadsgf games/nngs/theDoor-gnugo-3.3.11-200211130142.sgf 208
1110 reg_genmove black
#? [O12]

