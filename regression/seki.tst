# The theme of this test suite is how to play inside an already living
# group to turn potential territory into a seki. A typical example
# would be this corner group
# 
# |OOOOOO
# |XXXXXO
# |..a.XO
# |....XO
# +------
# 
# where white can make seki in sente by playing at 'a'. There are also
# numerous test cases to check the followup moves to get a seki. In
# many cases one or both players have the option to make ko for life
# and death of the entire corner (whereby black would get territory
# points if winning). Is is generally assumed in this test suite that
# white tries to achieve ko while black tries to avoid it, since the
# ko in most cases is favorable to white.

loadsgf games/seki04.sgf
1 reg_genmove white
#? [B2|B1|A2]
# B2 also secures life but leaves a much bigger ko threat behind.
2 reg_genmove black
#? [A2|B1]

play white B2
# Tenuki lets white make ko with A1
3 reg_genmove black
#? [B1|A2]

play black B1
4 reg_genmove white
#? [A2]

undo
undo
play white B1
5 reg_genmove black
#? [A2]
6 reg_genmove white
#? [A2]

play black A2
7 reg_genmove white
#? [B2]*

undo
# White kills through bent four in the corner.
play black B2
8 reg_genmove white
#? [A2]


loadsgf games/seki05.sgf
# C2 gives a sente seki, while B1 gives a gote seki. Other moves fail.
101 reg_genmove white
#? [C2]
102 reg_genmove black
#? [C2|C1|B2|B1]

play white C2
# Only B1 and A2 (oddly enough) avoids giving white a ko option.
103 reg_genmove black
#? [B1|A2]

play black B1
104 reg_genmove white
#? [B2]

play white B2
105 reg_genmove black
#? [C1]

play black C1
106 reg_genmove white
#? [A2]

play white A2
107 reg_genmove black
#? [D1]

undo
undo
play black A2
108 reg_genmove white
#? [C1]

undo
undo
undo
play black B2
109 reg_genmove white
#? [B1]

undo
play black C1
110 reg_genmove white
#? [B1]

undo
undo
play white B1
111 reg_genmove black
#? [C2]*

undo
play white C1
112 reg_genmove black
#? [B1]


loadsgf games/seki06.sgf
201 reg_genmove white
#? [B2]
202 reg_genmove black
#? [B2]

# Both A2 and B1 lead to seki. Strictly speaking A2 is a better move
# since it leaves black the option to take sente and let white make a
# thousand-year ko. For now we accept both.
play white B2
203 reg_genmove black
#? [A2|B1]

play black A2
204 reg_genmove white
#? [B1]

# White gets a thousand-year ko if black plays tenuki.
play white B1
205 reg_genmove black
#? [C1|C2]

# A1 makes a clean seki with no ko aji.
undo
play white C1
206 reg_genmove black
#? [A1]

undo
undo
play black B1
207 reg_genmove white
#? [A2]

play white A2
208 reg_genmove black
#? [C2]
209 reg_genmove white
#? [A3]

undo
undo
undo
play white B1
210 reg_genmove black
#? [B2]


loadsgf games/seki07.sgf
301 reg_genmove white
#? [B2]
302 reg_genmove black
#? [B2]

play white B2
303 reg_genmove black
#? [A2|C1|C2]

play black A2
304 reg_genmove white
#? [C1]

# Black can start ko with B1 if desperate.
play white C1
305 reg_genmove black
#? [C2]

play black C2
306 reg_genmove white
#? [B1]

# Now black can live with territory.
undo
undo
play white B1
307 reg_genmove black
#? [C1]

undo
undo
play black C1
308 reg_genmove white
#? [A2]

# Tenuki is okay. What remains is a double-sided ko threat (0 points
# double sente).
play white A2
309 reg_genmove black
#? [B1|C2|C9]

play black B1
310 reg_genmove white
#? [C2]

undo
play black C2
311 reg_genmove white
#? [B1]

# A2 also makes seki, but costs a prisoner.
undo
undo
undo
play black C2
312 reg_genmove white
#? [B1|C1]

play white B1
313 reg_genmove black
#? [C1|A2]

play black C1
314 reg_genmove white
#? [A2]

undo
play black A2
315 reg_genmove white
#? [C1]

undo
undo
play white C1
316 reg_genmove black
#? [A2]


loadsgf games/seki08.sgf
401 reg_genmove white
#? [B1]
# Most moves secure territory but C1 and C2 are most natural. We also
# allow B1.
402 reg_genmove black
#? [C1|C2|B1]

# C1 and C2 both secure seki in sente, but C1 is one prisoner better.
# B2 leaves white the option to make ko.
play white B1
403 reg_genmove black
#? [C1|C2]*

play black C1
404 reg_genmove white
#? [A2|D1]

play white A2
405 reg_genmove black
#? [C2]

play black C2
406 reg_genmove white
#? [D1|E1]

undo
undo
play white D1
407 reg_genmove black
#? [C2|E1]

play black C2
408 reg_genmove white
#? [A2]

undo
play black E1
409 reg_genmove white
#? [A2]

play white A2
410 reg_genmove black
#? [C2]

undo
undo
undo
undo

# D1 and A2 also give seki but are one point worse.
play black C2
411 reg_genmove white
#? [C1]

# D1 and A2 are miai to get seki. Black can also opt for a difficult
# ko by playing B2, but this is usually not a good idea. Tenuki
# suffices for seki and leaves a big mutual ko threat.
play white C1
412 reg_genmove black
#? [D1|A2|C9]

play black D1
413 reg_genmove white
#? [A2]

loadsgf games/seki09.sgf
501 reg_genmove white
#? [B1]*
502 reg_genmove black
#? [C1]*

play white B1
503 reg_genmove black
#? [C1]

play black C1
504 reg_genmove white
#? [A2]

loadsgf games/seki10.sgf
601 reg_genmove white
#? [B1]
602 reg_genmove black
#? [C1|B1]

# B2 gives a hopeless ko.
play white B1
603 reg_genmove black
#? [C1]

play black C1
604 reg_genmove white
#? [A2]


loadsgf games/seki11.sgf
701 reg_genmove white
#? [C1]
702 reg_genmove black
#? [C1|B1|B2]

play white C1
703 reg_genmove black
#? [B2]

play black B2
704 reg_genmove white
#? [B1]


loadsgf games/seki12.sgf
801 reg_genmove white
#? [B2]
802 reg_genmove black
#? [B2]

# C2 is the best way to make seki since it leaves ko aji in black's
# favor unless white accepts gote.
play white B2
803 reg_genmove black
#? [C2]

# B1 also makes seki but white definitely ends in gote.
play black C2
804 reg_genmove white
#? [C1]

# After A2 black has the option to start ko with B1, unless white
# takes gote and fills at B1. Sooner or later that may be necessary.
play white C1
805 reg_genmove black
#? [A2]

undo
play white B1
# A2 is one point bigger than C1 regardless of scoring method.
806 reg_genmove black
#? [A2|C1]

play black C1
# A3 is one point bigger than A2 under japanese scoring and a half
# point bigger under chinese scoring.
807 reg_genmove white
#? [A3|A2]

undo
undo
undo

# B1 and A2 give gote seki.
play black C1
808 reg_genmove white
#? [C2]

play white C2
809 reg_genmove black
#? [B1]

# A2 is gote seki and may occasionally be better than allowing black
# to initiate a ko.
play black B1
810 reg_genmove white
#? [A3]

# A1 secures seki, removing white's ko aji.
play white A3
811 reg_genmove black
#? [A1]


loadsgf games/seki13.sgf
901 reg_genmove white
#? [B2]
902 reg_genmove black
#? [B2]

# Black probably have more moves to make seki in sente but C2 looks most
# natural. C1 is also good enough. B1 suffices to make sente seki as well
# but loses one point compared to C2 or C1.
play white B2
903 reg_genmove black
#? [C2|C1]*

play black C2
904 reg_genmove white
#? [B3]

# B1 gives ko or seki in gote. Tenuki also leaves a seki but compared
# to C1 the latter is 1 point double sente.
play white B3
905 reg_genmove black
#? [C1]*

play black C1
906 reg_genmove white
#? [B1]

# Black can play tenuki and still have seki but since A3 is one point
# double sente there is no reason to wait.
play white B1
907 reg_genmove black
#? [A3]*

play black A3
908 reg_genmove white
#? [A2]

# 1 point double sente, just like 907.
undo
909 reg_genmove white
#? [A3]*

play white A3
910 reg_genmove black
#? [A1|A2]

# One point to pick up the A1 stone under Japanese rules.
play black A1
play black C9
911 reg_genmove white
#? [A2]

# After white C1 black can make ko with A2 or seki in gote with D1.
undo
undo
undo
undo
undo
play black B1
912 reg_genmove white
#? [C1]

# White C1 was a mistake. Black makes life with territory at B3.
undo
undo
play white C1
913 reg_genmove black
#? [B3]


# Black should play B2. A2 and B1 leave an unnecessary ko threat.
loadsgf games/seki14.sgf
1001 reg_genmove white
#? [B2]
1002 reg_genmove black
#? [B2]


loadsgf games/seki15.sgf
1101 reg_genmove white
#? [D1]*
1102 reg_genmove black
#? [C1|C2|D2]*

play white D1
1103 reg_genmove black
#? [E2]

play black E2
1104 reg_genmove white
#? [B1]*

play white B1
1105 reg_genmove black
#? [C2]

play black C2
1106 reg_genmove white
#? [C1]


loadsgf games/seki16.sgf
1201 reg_genmove white
#? [B2]
1202 reg_genmove black
#? [B2]

# Black can make seki in sente with most moves but B1 and A2 lose one
# point. Tenuki let's white kill with ko.
play white B2
1203 reg_genmove black
#? [C2|B3|C1|A3]

# B3, A2, and A1 fail to make seki.
play black C2
1204 reg_genmove white
#? [B1|C1|A3]

# A3, B3, and A2 all make seki, but B3 and A2 lose a point.
play white C1
1205 reg_genmove black
#? [A3]*

play black A3
1206 reg_genmove white
#? [B3|A2|B1]

play white B3
1207 reg_genmove black
#? [A2]

play black A2
1208 reg_genmove white
#? [B1]

undo
undo
play white A2
1209 reg_genmove black
#? [B3]*

play black B3
1210 reg_genmove white
#? [B1]

# A3 gains a point compared to A2. B1 is two points worse.
undo
undo
undo
play black B3
1211 reg_genmove white
#? [A3]

# A3 gains a point compared to B3. B1 is two points worse.
undo
play black A2
1212 reg_genmove white
#? [A3]

play white A3
1213 reg_genmove black
#? [B3]

play black B3
1214 reg_genmove white
#? [A1]

undo
undo
undo
undo

# Tenuki leaves seki but loses a point.
play white B1
1215 reg_genmove black
#? [C1|A3|B3|A2|C9]

# B3 and A2 both fail.
play black C1
1216 reg_genmove white
#? [A3]

# A2 also gives seki but loses a point, as does tenuki.
play white A3
1217 reg_genmove black
#? [B3]*

undo
undo
play black A3
1218 reg_genmove white
#? [C1]

# Tenuki is fine.
play white C1
1219 reg_genmove black
#? [B3|A2|C9]

undo
undo
play black B3
1220 reg_genmove white
#? [A2|A3]*

undo
play black A2
1221 reg_genmove black
#? [B3|A3]

# B1 and B3 lose a point.
undo
undo
play white A3
1222 reg_genmove black
#? [C1]*

play black C1
1223 reg_genmove white
#? [B1]

# Only A3 works. If black gets both C1 and A3, white can't do anything
# with two stones (in addition to B2).
undo
undo
undo
play black C1
1224 reg_genmove white
#? [A3]

play white A3
1225 reg_genmove black
#? [B1|C2|B3]

play black B1
1226 reg_genmove white
#? [C2]

play white C2
1227 reg_genmove black
#? [B3]*

# C1 is one point better than C2 and A3.
undo
undo
undo
undo
play black B1
1228 reg_genmove white
#? [C1]*

loadsgf games/FSGCBot-dr.sgf 234
2010 reg_genmove white
#? [A19|B17]

loadsgf games/seki_nakade1.sgf
2020 dragon_status D9
#? [alive]

loadsgf games/seki_nakade2.sgf
2030 dragon_status F9
#? [critical H9 H9]*

play white H9
play white H7
play white G7
play white G9

2050 dragon_status G8
#? [alive]
