# Here is a list of other regressions which should afford
# semeai tuning. The regressions were run with the version
# of November 6, 2001 with three different versions of the
# semeai module. The patch in question was semeai_1_16.5
# which is now in the CVS.
#
#                   Standard  Experimental Patched
#
# Strategy 1            f         P          P
# Strategy 37           p         F          F
# Neurogo 11            p         F          F
# Neurogo 12            p         F          F
# Arb 104               p         F          F
# Golife 1              f         P          P
# Golife 2              f         P          P
# Dniwog 5              f         P          f
# Strategy2 66          p         F          F
# Strategy2 72          f         P          P
# Strategy2 73          F         p          F
# Strategy2 93          f         P          P
# Nicklas1 501          p         F          F
# Nicklas1 1213         f         P          P
# Nicklas1 1405         p         F          F
# Nicklas1 1406         p         F          F
# Nicklas2 904          p         p          F
# Nicklas2 1407         f         P          f
# Manyfaces 7           p         F          F
# Buzco 6               p         F          p
# Strategy3 110         f         P          P
# Strategy3 113         p         F          p
# Strategy3 124         f         P          f
# Strategy3 126         p         F          F
# Arend 30              p         F          F
# Trevora 530           f         P          P
# Strategy4 168         P         f          f
# Strategy4 179         F         F          F (*)
# Strategy4 199         p         F          F
# Strategy4 200         p         F          F
#
# (*) All tests fail, but differently
#
# Here is a supplemental list of problems where a comment in the .tst
# file indicates a semeai:
# 
# Strategy 44
# Strategy2 72
# Strategy2 80
# Strategy2 86
# Strategy3 109
# Strategy3 110
# Strategy3 124
# Strategy3 128
# Strategy3 129
# Strategy3 139
# Strategy3 145
# Strategy3 146
# Strategy4 156
# Strategy4 163
# Strategy4 168
# Strategy4 185
# Strategy4 206

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
#? [ALIVE DEAD (F15|G15|H15|PASS)]*

24 owl_analyze_semeai G13 G12
#? [DEAD ALIVE PASS]

25 owl_analyze_semeai S8 R8
#? [ALIVE DEAD S9]

26 owl_analyze_semeai R8 S8
#? [ALIVE DEAD S9]

# If this semeai is treated as a strictly local
# problem (ignoring the R8 dragon) then R7 can't live.
# But the semeai code treats it as a local problem by
# design!  So it's unclear what the correct answer
# should be.  A similar remark holds with problem 28.

27 owl_analyze_semeai Q7 R7
#? [ALIVE DEAD S9]

28 owl_analyze_semeai R7 Q7
#? [ALIVE DEAD S9]*

# A6 gives an unfavorable ko while F10 gives seki.
# Since there are no ko threats, and F10 is enough to win, it is preferred.
loadsgf games/semeaiko1.sgf
29 gg_genmove black
#? [F10]*

loadsgf games/strategy11.sgf 127
30 owl_analyze_semeai B3 G4
#? [ALIVE DEAD C1]*

loadsgf games/strategy11.sgf 127
31 owl_analyze_semeai G4 B3
#? [ALIVE DEAD C1]*
