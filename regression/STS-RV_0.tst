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
# in the semeais_0.tst file from STS-RV suite of semeai problems.
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

loadsgf games/STS-RV/Class01eProblems/_semeai_C0_001.sgf
1 analyze_semeai A11 A10
#? [1 1 (.*)]
2 analyze_semeai A10 A11
#? [0 0 PASS]

loadsgf games/STS-RV/Class01eProblems/_semeai_C0_002.sgf
3 analyze_semeai L1 N1
#? [1 0 (.*)]

4 analyze_semeai N1 L1
#? [1 0 PASS]

loadsgf games/STS-RV/Class01eProblems/_semeai_C0_003.sgf
5 analyze_semeai H1 K1
#? [1 0 (.*)]

6 analyze_semeai K1 H1
#? [1 0 (.*)]

loadsgf games/STS-RV/Class01eProblems/_semeai_C0_004.sgf
7 analyze_semeai A11 A10
#? [1 1 (A9|B9|C9|D9|E9|F9|G9|H9|J9|K9|L9|M9|N9|O9|P9|Q9|R9|S9|T9)]

8 analyze_semeai A10 A11
#? [1 1 (A12|B12|C12|D12|E12|F12|G12|H12|J12|K12|L12|M12|N12|O12|P12|Q12|R12|S12|T12)]

loadsgf games/STS-RV/Class01eProblems/_semeai_C0_005.sgf
9 analyze_semeai A11 A10
#? [0 0 PASS]

10 analyze_semeai A10 A11
#? [1 1 (.*)]

loadsgf games/STS-RV/Class01eProblems/_semeai_C0_006.sgf
11 analyze_semeai A16 A17
#? [1 1 B18|C18|D18]

12 analyze_semeai A17 A16
#? [1 1 A15|B15|B14]

loadsgf games/STS-RV/Class01eProblems/_semeai_C0_007.sgf
13 analyze_semeai H9 G9
#? [1 0 (.*)]

14 analyze_semeai G9 H9
#? [1 0 (.*)]

loadsgf games/STS-RV/Class01eProblems/_semeai_C0_008.sgf
15 analyze_semeai K13 L13
#? [1 1 (M10|M12)]

16 analyze_semeai L13 K13
#? [1 0 (H11|H12|J13)]

loadsgf games/STS-RV/Class01eProblems/_semeai_C0_009.sgf
17 analyze_semeai K13 L13
#? [1 1 (M10|M11|M12)]

18 analyze_semeai L13 K13
#? [1 1 (H11|H12|J13)]

loadsgf games/STS-RV/Class01eProblems/_semeai_C0_010.sgf
19 analyze_semeai J14 H14
#? [1 1 (.*)]

20 analyze_semeai H14 J14
#? [0 0 PASS]

loadsgf games/STS-RV/Class01eProblems/_semeai_C0_011.sgf
21 analyze_semeai J14 H14
#? [1 1 (G12|G13|G14)]

22 analyze_semeai H14 J14
#? [1 1 (L10|L12|L13)]

loadsgf games/STS-RV/Class01eProblems/_semeai_C0_012.sgf
23 analyze_semeai J14 H14
#? [1 1 (G12|G13|G14)]

24 analyze_semeai H14 J14
#? [1 1 (L10|L11|L12)]

loadsgf games/STS-RV/Class01eProblems/_semeai_C0_013.sgf
25 analyze_semeai A10 A11
#? [1 1 (A12|C12)]

26 analyze_semeai A11 A10
#? [1 1 (A9|C9)]
