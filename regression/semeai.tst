# for any of these problems, if PASS is a solution it is the best
# solution. However the other solutions are acceptable.

loadsgf games/semeai/semeai6.sgf
1 owl_analyze_semeai C1 E1
#? [ALIVE_IN_SEKI ALIVE_IN_SEKI (F1|F2|F3|F4|F5|PASS)]

2 owl_analyze_semeai E1 C1
#? [ALIVE_IN_SEKI ALIVE_IN_SEKI (B5|B4|B3|PASS)]

3 owl_analyze_semeai L1 N1
#? [ALIVE_IN_SEKI ALIVE_IN_SEKI (O5|O4|O3|P3|Q2|Q1|PASS)]

4 owl_analyze_semeai N1 L1
#? [ALIVE_IN_SEKI ALIVE_IN_SEKI (H2|J3|K3|PASS)]

5 owl_analyze_semeai A14 A12
#? [ALIVE DEAD (A13|B13|C13|D13)]

6 owl_analyze_semeai A12 A14
#? [ALIVE DEAD (A15|B15|C15|D15)]

loadsgf games/semeai/semeai7.sgf
7 owl_analyze_semeai C1 E1
#? [ALIVE_IN_SEKI ALIVE_IN_SEKI (F1|F2|F3|F4|F5|PASS)]

8 owl_analyze_semeai E1 C1
#? [ALIVE DEAD (B2|B3)]

9 owl_analyze_semeai L1 N1
#? [ALIVE_IN_SEKI ALIVE_IN_SEKI (O5|O4|O3|P3|Q2|Q1)]

10 owl_analyze_semeai N1 L1
#? [ALIVE DEAD (H2|K3)]

11 owl_analyze_semeai A14 A12
#? [ALIVE DEAD (A13|B13|D13|PASS)]

12 owl_analyze_semeai A12 A14
#? [DEAD ALIVE PASS]

loadsgf games/semeai/semeai8.sgf
13 owl_analyze_semeai C1 E1
#? [DEAD ALIVE PASS]

14 owl_analyze_semeai E1 C1
#? [ALIVE DEAD (B5|PASS)]

15 owl_analyze_semeai L1 N1
#? [DEAD ALIVE PASS]

16 owl_analyze_semeai N1 L1
#? [ALIVE DEAD (K5|PASS)]

17 owl_analyze_semeai A14 A12
#? [DEAD ALIVE PASS]

18 owl_analyze_semeai A12 A14
#? [ALIVE DEAD (A15|B15|D15|PASS)]

loadsgf games/semeai/semeai9.sgf

19 owl_analyze_semeai J1 L1
#? [ALIVE DEAD (M3|N3|O3|P3|Q1)]

20 owl_analyze_semeai L1 J1
#? [ALIVE DEAD (H1|H2|H3|H4|H5|H6|H7|H8)]

21 owl_analyze_semeai N19 O19
#? [ALIVE DEAD (T17|T19)]

22 owl_analyze_semeai O19 N19
#? [ALIVE DEAD (L15|L17|L18)]

loadsgf golois/Goemate990902-1.sgf

23 owl_analyze_semeai G12 G13
#? [ALIVE ALIVE (F13|G13|H13|PASS)]

24 owl_analyze_semeai G13 G12
#? [DEAD DEAD PASS]

25 owl_analyze_semeai S8 R8
#? [ALIVE DEAD S9]

26 owl_analyze_semeai R8 S8
#? [ALIVE DEAD S9]

27 owl_analyze_semeai Q7 R7
#? [ALIVE DEAD S9]

# If this semeai is treated as a strictly local
# problem (ignoring the R8 dragon) then R7 can't live.
# So it's unclear what the correct answer should be.
# A similar remark holds with problem 27.
28 owl_analyze_semeai R7 Q7
#? [ALIVE DEAD S9]
