# for any of these problems, if PASS is a solution it is the best
# solution. However the other solutions are acceptable.

loadsgf games/semeai/semeai6.sgf
1 tactical_analyze_semeai C1 E1
#? [ALIVE_IN_SEKI ALIVE_IN_SEKI (F1|F2|F3|F4|F5|PASS)]

2 tactical_analyze_semeai E1 C1
#? [ALIVE_IN_SEKI ALIVE_IN_SEKI (B5|B4|B3|PASS)]

3 tactical_analyze_semeai L1 N1
#? [ALIVE_IN_SEKI ALIVE_IN_SEKI (O5|O4|O3|P3|Q2|Q1|PASS)]

4 tactical_analyze_semeai N1 L1
#? [ALIVE_IN_SEKI ALIVE_IN_SEKI (H2|J3|K3|PASS)]

5 tactical_analyze_semeai A14 A12
#? [ALIVE DEAD (A13|B13|C13|D13)]

6 tactical_analyze_semeai A12 A14
#? [ALIVE DEAD (A15|B15|C15|D15)]

loadsgf games/semeai/semeai7.sgf
7 tactical_analyze_semeai C1 E1
#? [ALIVE_IN_SEKI ALIVE_IN_SEKI (F1|F2|F3|F4|F5|PASS)]

8 tactical_analyze_semeai E1 C1
#? [ALIVE DEAD (B2|B3)]

9 tactical_analyze_semeai L1 N1
#? [ALIVE_IN_SEKI ALIVE_IN_SEKI (O5|O4|O3|P3|Q2|Q1)]

10 tactical_analyze_semeai N1 L1
#? [ALIVE DEAD (H2|K3)]

11 tactical_analyze_semeai A14 A12
#? [ALIVE DEAD (A13|B13|D13|PASS)]

12 tactical_analyze_semeai A12 A14
#? [DEAD ALIVE PASS]

loadsgf games/semeai/semeai8.sgf
13 tactical_analyze_semeai C1 E1
#? [DEAD ALIVE PASS]

14 tactical_analyze_semeai E1 C1
#? [ALIVE DEAD (B5|PASS)]

15 tactical_analyze_semeai L1 N1
#? [DEAD ALIVE PASS]

16 tactical_analyze_semeai N1 L1
#? [ALIVE DEAD (K5|PASS)]

17 tactical_analyze_semeai A14 A12
#? [DEAD ALIVE PASS]

18 tactical_analyze_semeai A12 A14
#? [ALIVE DEAD (A15|B15|D15|PASS)]

loadsgf games/semeai/semeai9.sgf

19 tactical_analyze_semeai J1 L1
#? [ALIVE DEAD (M3|N3|O3|P3|Q1)]

20 tactical_analyze_semeai L1 J1
#? [ALIVE DEAD (H1|H2|H3|H4|H5|H6|H7|H8)]

21 tactical_analyze_semeai N19 O19
#? [ALIVE DEAD (T17|T19)]

22 tactical_analyze_semeai O19 N19
#? [ALIVE DEAD (L15|L17|L18)]

