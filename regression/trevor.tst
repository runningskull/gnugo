#terrible connection mistake.
loadsgf games/trevor/trevor_01.sgf 28
1 gg_genmove white
#? [G5]

#probably a screwed up fuseki library!
loadsgf games/trevor/trevor_02.sgf
2 gg_genmove white
#? [!PASS]*

# underlying reading problem
loadsgf games/trevor/trevor_03.sgf 28
3 gg_genmove white
#? [!E3]*

#simple edge block missed.
loadsgf games/trevor/trevor_04.sgf 30
4 gg_genmove white
#? [G1]*

#E5 is the only move.  Is GnuGo playing somewhat randomly here?
loadsgf games/trevor/trevor_05.sgf 6
5 gg_genmove white
#? [E5]*

#locally B9 is terrible.  There are other big moves on the board.
loadsgf games/trevor/trevor_06.sgf 22
6 gg_genmove white
#? [B7|C6|F5]

#B5 is certainly better than C5 here.  This is a terrible
#game for white, but it's hard to narrow down the obvious
#blunders.
loadsgf games/trevor/trevor_07.sgf 32
7 gg_genmove white
#? [!C5]*

#Pattern missing here?  B8 is almost always better than B9.
loadsgf games/trevor/trevor_08.sgf 24
8 gg_genmove white
#? [!B9]

#E5 (or some defence of D4 worm) is necessary.
loadsgf games/trevor/trevor_09.sgf
9 gg_genmove white
#? [E5]*

#This is nearly trivial, it would seem!
loadsgf games/trevor/trevor_10.sgf
10 gg_genmove white
#? [E6]

#Yikes, gnugo can't play into here.  See also the next problem.
loadsgf games/trevor/trevor_11.sgf
11 gg_genmove white
#? [!B2]*

#J2 is terrible.  H1 leads at least to a Ko.
loadsgf games/trevor/trevor_12.sgf
12 gg_genmove black
#? [H1]

#E2 is obvious.
loadsgf games/trevor/trevor_13.sgf
13 gg_genmove black
#? [E2]*

#Must protect double cut at C2 & E2
loadsgf games/trevor/trevor_14.sgf
14 gg_genmove white
#? [!PASS]*


#protecting at E7 is very necessary.
loadsgf games/trevor/trevor_15.sgf 
15 gg_genmove white
#? [E7]*

# C1 looks best here.  A7 very unorthodox, and at least
# worse for ko threats.
loadsgf games/trevor/trevor_16.sgf 53
160 gg_genmove black
#? [!A7]*


# Protecting lower left group better.
loadsgf games/trevor/trevor_16.sgf 54
161 gg_genmove white
#? [B1]*

# W can live here unconditionally.
loadsgf games/trevor/trevor_16.sgf 58
162 gg_genmove white
#? [B1]*

# If Black B9, White can't connect at C7.
loadsgf games/trevor/trevor_16.sgf 67
163 gg_genmove black
#? [B9]*

# Must connect dragons.
# B6 should be valued much much lower than B5
# Top moves:
#1. B5  32.93 [CORRECT!]
#2. B6  32.64 [NO, this is terrible, more like B7 & C7]
#3. B7  18.35
#4. C7  18.33
loadsgf games/trevor/trevor_17.sgf 22
170 gg_genmove white
#? [FAIL]*

# C8 is terrible.
loadsgf games/trevor/trevor_17.sgf 28
171 gg_genmove white
#? [!C8]*

# Snapback!
loadsgf games/trevor/trevor_18.sgf 32
180 gg_genmove white
#? [!A8]*

# Whoops, don't let go of the dragon's tail!
loadsgf games/trevor/trevor_19.sgf 12
190 gg_genmove white
#? [B7]*

# W should connect his two dragons.
loadsgf games/trevor/trevor_20.sgf 14
200 gg_genmove white
#? [D7|D4]*

# W can live on the right pretty easily (i.e. G8)
loadsgf games/trevor/trevor_20.sgf
201 gg_genmove white
#? [!PASS]*

# W can live on the right pretty easily (i.e. G8)
# Black really does pass here, but eval.sh reports a PASSED result. (?)
loadsgf games/trevor/trevor_20.sgf
202 gg_genmove black
#? [!PASS]* 

# Huge negative sacrifice at D8.
loadsgf games/trevor/trevor_21.sgf 22
210 gg_genmove white
#? [!D8]*

# In this case, the knight's move connection looks best.
# Black seems not to see the cut coming.
loadsgf games/trevor/trevor_22.sgf 23
220 gg_genmove black
#? [E8]*

# F2 is not really sente, nor useful if it were!
loadsgf games/trevor/trevor_23.sgf 27
230 gg_genmove black
#? [C2|B2]*


# Connect the dragons!  One eye is not enough, anyway.
# gnugo.exe -l games/trevor/trevor_24.sgf -L 33 -t
# shows:
#Top moves:
#1. H9  16.51   (Terrible)
#2. E8  16.51   (much better!)
#Even selects H9, though invoked here, gg picks E8.
#I put FAIL as the match, to force a failure, until H9
#is ranked much lower (about -1, or so!)
loadsgf games/trevor/trevor_24.sgf 33
240 gg_genmove black
#? [FAIL]*



# G2 is a gift to white.
loadsgf games/trevor/trevor_25.sgf 31
250 gg_genmove black
#? [!G2]*

# G1 is another gift to white.
loadsgf games/trevor/trevor_25.sgf 33
251 gg_genmove black
#? [!G1]*


# Should connect further back, at least, but of course
# blocking at C1 is much bigger.
loadsgf games/trevor/trevor_26.sgf 33
260 gg_genmove black
#? [C1|F9]*

# Connecting is safe.  Is black worried about the cut?
loadsgf games/trevor/trevor_26.sgf 39
261 gg_genmove black
#? [F9]*


# Ack, protect the cut!
loadsgf games/trevor/trevor_27.sgf 35
270 gg_genmove black
#? [E6|D6]*


# D5 misses a big problem cut.
loadsgf games/trevor/trevor_28.sgf 27
280 gg_genmove black
#? [D4]*

# F6 is much better and safer to boot than H6.
loadsgf games/trevor/trevor_28.sgf 36
281 gg_genmove white
#? [F6]*



# Yikes, short of liberties!
loadsgf games/trevor/trevor_29.sgf 54
290 gg_genmove white
#? [!A9]*

# Black A7 is a serious threat.  Also, white should at least 
# try to invade.
loadsgf games/trevor/trevor_30.sgf
300 gg_genmove white
#? [!PASS]*

# Black A7 is a serious threat.  Also, white should at least 
# try to invade.
loadsgf games/trevor/trevor_30.sgf
301 gg_genmove black
#? [!PASS]*





