# endgame2.sgf is a refinement of the endgame problems in ego.sgf

loadsgf games/endgame2.sgf 1
201 reg_genmove white
#? [R10]*

loadsgf games/endgame2.sgf 2
202 reg_genmove black
#? [Q9]

loadsgf games/endgame2.sgf 3
203 reg_genmove white
#? [A7]

# H19 and A12 are both 1 point sente.
loadsgf games/endgame2.sgf 4
204 reg_genmove black
#? [H19|A12]

loadsgf games/endgame2.sgf 5
205 reg_genmove white
#? [F18|E18]

loadsgf games/endgame2.sgf 6
206 reg_genmove black
#? [A12]

loadsgf games/endgame2.sgf 7
207 reg_genmove white
#? [A11]

loadsgf games/endgame2.sgf 8
208 reg_genmove black
#? [N1]*

loadsgf games/endgame2.sgf 9
209 reg_genmove white
#? [A6|O1]

# A5 is sente because the followup is bigger than black's capture at O2.
loadsgf games/endgame2.sgf 10
210 reg_genmove black
#? [A5]

# A5 is sente because the followup is bigger than black's capture at O2.
loadsgf games/endgame2.sgf 11
211 reg_genmove white
#? [A5]

loadsgf games/endgame2.sgf 12
212 reg_genmove black
#? [A5]

loadsgf games/endgame2.sgf 13
213 reg_genmove white
#? [Q8|C13]

loadsgf games/endgame2.sgf 14
214 reg_genmove black
#? [R8|C12|C13]

loadsgf games/endgame2.sgf 15
215 reg_genmove white
#? [C13]

loadsgf games/endgame2.sgf 16
216 reg_genmove black
#? [D13]

loadsgf games/endgame2.sgf 17
217 reg_genmove white
#? [C12]

loadsgf games/endgame2.sgf 18
218 reg_genmove black
#? [H1]

loadsgf games/endgame2.sgf 19
219 reg_genmove white
#? [T17]

#################################################################
# endgame3.sgf features a subtle "eventual shortage of liberties"
# position, causing E5 to be worth a whole point.

loadsgf games/endgame3.sgf
301 reg_genmove black
#? [E5]*
302 reg_genmove white
#? [E5]

#################################################################
# endgame4.sgf is a question of ending with sente or not. Black E5
# does defend the black F3 string, but turns out to lose a point in
# the end.

loadsgf games/endgame4.sgf
401 reg_genmove black
#? [C4]

# The basic 2/3 pt ko capture is undervalued.
loadsgf games/endgame5.sgf
501 reg_genmove black
#? [E5]

# An endgame sente pattern.
loadsgf games/endgame6.sgf 1
601 reg_genmove black
#? [J8]

loadsgf games/endgame6.sgf 1
602 reg_genmove white
#? [J8]

loadsgf games/endgame6.sgf 2
603 reg_genmove black
#? [C1]*

loadsgf games/endgame6.sgf 2
604 reg_genmove white
#? [D1]*

# G5 is one point in sente or 2 points in gote for white. E3 is bigger.
loadsgf games/endgame7.sgf 1
701 reg_genmove black
#? [E3]

loadsgf games/endgame7.sgf 1
702 reg_genmove white
#? [E3]

loadsgf games/endgame7.sgf 2
703 reg_genmove black
#? [G5]

loadsgf games/endgame7.sgf 2
704 reg_genmove white
#? [G5]

loadsgf games/nngs/gnugo-3.3.11-bconwil-200211202359.sgf 242
801 reg_genmove white
#? [S14]

802 reg_genmove black
#? [S14]*

# No point for F5.
loadsgf games/endgame8.sgf
803 reg_genmove black
#? [J7]
804 reg_genmove white
#? [J7]

loadsgf games/nngs/gnugo-3.3.17-Wiedemann-200303251932.sgf 151
810 reg_genmove white
#? [F5|F4]

loadsgf games/nngs/gnugo-3.3.17-Wiedemann-200303251932.sgf 203
820 reg_genmove white
#? [H4|M10|K19]*

loadsgf games/paul.sgf 205
830 restricted_genmove white M19 B13
#? [B13]

loadsgf games/nngs/gnugo-3.3.17-Shindou-200304162217.sgf 110
840 reg_genmove white
#? [L15]

loadsgf games/nngs/gnugo-3.3.17-Shindou-200304162217.sgf 140
850 restricted_genmove white K4 J4 L4 P5
#? [K4|J4|L4]

# Defending T12 is large and R15 is clearly the best way.
loadsgf games/endgame9.sgf
860 reg_genmove black
#? [R15]

# H1 is one point and N7 is about 0.5.
loadsgf games/endgame10.sgf
870 restricted_genmove black H1 N7
#? [H1]

# K5 is best, although in absence of other worthy moves it doesn't matter.
loadsgf games/endgame11.sgf
880 reg_genmove white
#? [K5]*
# T4 is one point in ko, while T5 and S6 are filling dame only.
890 restricted_genmove white T4 T5 S6
#? [T4]

loadsgf games/nngs/gnugo-3.3.21-joshj-200306270402.sgf 222
900 reg_genmove white
#? [J19|K19|K18]

loadsgf games/kgs/yagr-eddyy.sgf 198
910 reg_genmove white
#? [T2]

loadsgf games/kgs/yagr-gab9.sgf 205
920 reg_genmove white
#? [C4]

# C11 is one point reverse sente.
# E4 is at the very least three points reverse sente, probably five or
# six points reverse sente.
# P4 is three points reverse sente.
# R16/O14 are very very big. O14 is bigger than R16.
loadsgf games/kgs/GNU-merlenoir.sgf 185
930 restricted_genmove white O14 R16
#? [O14]*
931 restricted_genmove white R16 E4
#? [R16]
932 restricted_genmove white E4 P4
#? [E4]
933 restricted_genmove white P4 C11
#? [P4]*

# E4 is at the very least three points reverse sente, probably five or
# six points reverse sente.
# P4 is three points reverse sente.
# R16 is about two points (slightly more) reverse sente.
# K11 is three points gote.
# R1 is about one and a half points reverse sente and in any case
# locally smaller than P4.
loadsgf games/kgs/GNU-merlenoir.sgf 193
940 restricted_genmove white P4 R16
#? [P4]
941 restricted_genmove white R16 K11
#? [R16]*
942 restricted_genmove white P4 R1
#? [P4]*

# R16 is about two points (slightly more) reverse sente.
# H8 is one point reverse sente.
# C1 is between one and two points gote.
loadsgf games/kgs/GNU-merlenoir.sgf 209
950 restricted_genmove white R16 H8
#? [R16]
951 restricted_genmove white H8 C1
#? [H8]*

# J10 is at least four points gote, in reality probably six.
# H8 is one point reverse sente.
loadsgf games/kgs/GNU-merlenoir.sgf 233
960 restricted_genmove white J10 H8
#? [J10]

# No point to gain at the top, fill ko. After white C6, black E6,
# white has to defend and loses the ko.
loadsgf games/endgame12.sgf
970 reg_genmove white
#? [G2]

loadsgf games/nando/auto025.sgf 226
980 restricted_genmove white E4 F4 Q19
#? [F4]

# E5 is about 2.5 points.
loadsgf games/endgame13.sgf 1
990 reg_genmove black
#? [C1]
991 reg_genmove white
#? [C1]
loadsgf games/endgame13.sgf 2
992 reg_genmove black
#? [E5]*
993 reg_genmove white
#? [E5]

# E5 is two points gote, J3 one point sente, and J2 three points gote.
# J2 wins the game by 0.5, all other moves lose.
loadsgf games/endgame14.sgf
1000 reg_genmove black
#? [J2]

# A7 wins but A8 loses.
loadsgf games/endgame15.sgf
1010 reg_genmove white
#? [A7]*
