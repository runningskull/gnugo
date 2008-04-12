# This suite of semeai problems came from the package STS-RV
# available at http://gobase.org/reading/preview/Semeai/#STS
#
# It is a very comprehensive semeai problems suite
# compiled by Ricard Vilà. The associated tests are in GTP
# format but it's not a perfect match for GNU Go because
# it uses a custom command called solve-semeaiS.
# (More info at http://trac.gnugo.org/gnugo/ticket/41)
#
# This file instead is based on the normal GNU Go commands
# (analyze_semeai) and enable the execution of the tests
# in the semeais_Misc.tst file from STS-RV suite of semeai problems.
#
# For any of the problems below, if PASS is a solution it is the best
# solution. However the other solutions are acceptable.

############## semeai tests #################
#
# After analyze_semeai [dragon1] [dragon2]
# the results are returned in the form (result1) (result2).
# These are the results of the defense of dragon1 and the attack
# of dragon2 assuming that the dragon1 player moves first. Thus
# a result 1 0 typically means seki, while a 1 1 result means a kill
# and 0 0 means the semeai is lost. In addition to seki, 1 0 may mean
# that both dragons gain independent life.
# The result [0 1] is not possible as a correct result but GNU Go
# has been known to return that occasionally.
# Result 2 mean success with good ko, 3 success with bad ko.
# A good ko is characterized by the opponent having to make the first
# external ko threat whereas a bad ko is the opposite.
#
# NOTE: In some problem GNU Go suggest a move instead to PASS
# but the result of the semeai analysis is correct, so to avoid
# a fail not relevant for the test, the move is ignored using
# a regular expression: #? [x y (.*)]

loadsgf games/STS-RV/MiscProblems/_semeai_C9_001.sgf
1 analyze_semeai K1 J1
#? [3 3 G2]*

2 analyze_semeai J1 K1
#? [1 1 G1]

3 analyze_semeai A7 A8
#? [1 1 B9]

4 analyze_semeai A8 A7
#? [1 1 A10]*

5 analyze_semeai T9 T10
#? [1 1 (T15|S13|S11)]

6 analyze_semeai T10 T9
#? [2 2 (S11|S13)]*

loadsgf games/STS-RV/MiscProblems/_semeai_C9_002.sgf
7 analyze_semeai T6 T5
#? [1 1 N7]

8 analyze_semeai T5 T6
#? [1 1 T7]*

loadsgf games/STS-RV/MiscProblems/_semeai_C9_003.sgf
9 analyze_semeai F2 G2
#? [1 1 J1]*

10 analyze_semeai G2 F2
#? [1 1 D4]

loadsgf games/STS-RV/MiscProblems/_semeai_C9_004.sgf
11 analyze_semeai K9 J9
#? [1 1 L11]

12 analyze_semeai J9 K9
#? [1 0 L11]*

loadsgf games/STS-RV/MiscProblems/_semeai_C9_005.sgf
13 analyze_semeai B12 B13
#? [1 1 B9]

14 analyze_semeai B13 B12
#? [1 0 B9]*

loadsgf games/STS-RV/MiscProblems/_semeai_C9_006.sgf
15 analyze_semeai D18 D19
#? [1 1 (A18|C19)]

16 analyze_semeai D19 D18
#? [1 1 (B19|C19|A18)]

loadsgf games/STS-RV/MiscProblems/_semeai_C9_007.sgf
17 analyze_semeai P17 P16
#? [1 1 (Q15|R15)]

18 analyze_semeai P16 P17
#? [1 1 N19]

19 analyze_semeai T3 T4
#? [1 1 (T5|S5|R5|Q5|O1)]

20 analyze_semeai T4 T3
#? [1 1 (O1|S2|R2|Q2)]

21 analyze_semeai E19 D19
#? [1 1 (C17|C16)]

22 analyze_semeai D19 E19
#? [1 1 (F17|F16)]

23 analyze_semeai C9 C10
#? [1 1 (D11|A10)]

24 analyze_semeai C10 C9
#? [1 1 (D11|A10)]

loadsgf games/STS-RV/MiscProblems/_semeai_C9_008.sgf
25 analyze_semeai P17 P16
#? [1 1 (P15|R15)]

26 analyze_semeai P16 P17
#? [1 1 (Q18|R18|S18)]

27 analyze_semeai T3 T4
#? [1 1 (R5|Q5)]

28 analyze_semeai T4 T3
#? [1 1 (R2|Q2)]

29 analyze_semeai E19 D19
#? [1 1 (C17|C16)]

30 analyze_semeai D19 E19
#? [1 1 (F17|F16)]

31 analyze_semeai C9 C10
#? [1 1 (D11|A10)]

32 analyze_semeai C10 C9
#? [1 1 (D11|A10)]

loadsgf games/STS-RV/MiscProblems/_semeai_C9_009.sgf
33 analyze_semeai F2 G2
#? [3 3 G1]*

34 analyze_semeai G2 F2
#? [1 1 G1]

loadsgf games/STS-RV/MiscProblems/_semeai_C9_010.sgf
35 analyze_semeai F2 G2
#? [1 1 J1]*

36 analyze_semeai G2 F2
#? [1 1 D4]*

loadsgf games/STS-RV/MiscProblems/_semeai_C9_011.sgf
37 analyze_semeai F2 G2
#? [1 1 J1]*

38 analyze_semeai G2 F2
#? [1 1 D4]

loadsgf games/STS-RV/MiscProblems/_semeai_C9_012.sgf
39 analyze_semeai K9 J9
#? [1 1 (L11|O10|O12)]

40 analyze_semeai J9 K9
#? [1 1 L11]

loadsgf games/STS-RV/MiscProblems/_semeai_C9_013.sgf
41 analyze_semeai B12 B13
#? [1 1 (A9|C19)]

42 analyze_semeai B13 B12
#? [1 0 A9]*

loadsgf games/STS-RV/MiscProblems/_semeai_C9_014.sgf
43 analyze_semeai B12 B13
#? [1 1 (B9|C19)]

44 analyze_semeai B13 B12
#? [1 0 B9]*

loadsgf games/STS-RV/MiscProblems/_semeai_C9_015.sgf
45 analyze_semeai D18 E18
#? [1 1 (D19|F19|H18)]*

46 analyze_semeai E18 D18
#? [1 1 D19]

loadsgf games/STS-RV/MiscProblems/_semeai_C9_016.sgf
47 analyze_semeai C11 C12
#? [1 1 A18]

48 analyze_semeai C12 C11
#? [1 1 A18]

loadsgf games/STS-RV/MiscProblems/_semeai_C9_017.sgf
49 analyze_semeai J19 K19
#? [1 1 (S16|T19)]

50 analyze_semeai K19 J19
#? [1 1 S16]

loadsgf games/STS-RV/MiscProblems/_semeai_C9_018.sgf
51 analyze_semeai K12 K13
#? [1 1 (O14|N14|M14|K14|H12|F11)]

52 analyze_semeai K13 K12
#? [1 1 H12]*

loadsgf games/STS-RV/MiscProblems/_semeai_C9_019.sgf
53 analyze_semeai H18 G18
#? [1 1 E18]*

54 analyze_semeai G18 H18
#? [1 1 E18]*


