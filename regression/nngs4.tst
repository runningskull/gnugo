# Reset applicable counters
reset_connection_node_counter
reset_owl_node_counter
reset_reading_node_counter
reset_trymove_counter


loadsgf games/nngs/gnugo-3.3.10-niki-200210281349.sgf 10
10 reg_genmove white
#? [E18]

loadsgf games/nngs/gnugo-3.3.10-niki-200210281349.sgf 26
20 reg_genmove white
#? [Q4|Q6]

loadsgf games/nngs/gnugo-3.3.10-niki-200210281349.sgf 36
30 reg_genmove white
#? [C5|C6]

loadsgf games/nngs/gnugo-3.3.10-niki-200210281349.sgf 46
40 reg_genmove white
#? [H3|F4]*

# upper left needs attention
loadsgf games/nngs/evand-gnugo-3.3.11-200211151742.sgf 12
50 reg_genmove black
#? [J17|E14|F15|C18|B18]*

loadsgf games/nngs/evand-gnugo-3.3.11-200211151742.sgf 16
60 reg_genmove black
#? [E14|B18|C18|G18]*

loadsgf games/nngs/evand-gnugo-3.3.11-200211151742.sgf 142
70 owl_defend J11
#? [1 (S12|S13)]

loadsgf games/nngs/gnugo-3.3.12-jimm-200211261724.sgf 20
80 restricted_genmove white M7 J6
#? [J6]*

loadsgf games/nngs/gnugo-3.3.12-jimm-200211261724.sgf 170
90 reg_genmove white
#? [D12|C11|E14|B12]

loadsgf games/nngs/leftd-gnugo-3.3.12-200211292017.sgf 184
100 reg_genmove black
#? [F18|G18]*

loadsgf games/nngs/saphir-gnugo-3.3.12-200212031818.sgf 24
110 reg_genmove black
#? [D17]*

loadsgf games/nngs/saphir-gnugo-3.3.12-200212031818.sgf 38
120 reg_genmove black
#? [C13]*

loadsgf games/nngs/saphir-gnugo-3.3.12-200212031818.sgf 40
130 reg_genmove black
#? [E14]*

loadsgf games/nngs/saphir-gnugo-3.3.12-200212031818.sgf 60
140 restricted_genmove black N17 R5 R4
#? [R5|R4]*

loadsgf games/nngs/saphir-gnugo-3.3.12-200212031818.sgf 94
150 reg_genmove black
#? [N3|M3]*

loadsgf games/nngs/saphir-gnugo-3.3.12-200212031818.sgf 100
160 reg_genmove black
#? [K8|L8]*

loadsgf games/nngs/saphir-gnugo-3.3.12-200212031818.sgf 168
170 reg_genmove black
#? [S6]*

loadsgf games/nngs/saphir-gnugo-3.3.12-200212031818.sgf 184
190 reg_genmove black
#? [T8]

loadsgf games/nngs/saphir-gnugo-3.3.12-200212031818.sgf 192
200 reg_genmove black
#? [T6]

# Make a ko threat. How about T8 or T10?
loadsgf games/nngs/saphir-gnugo-3.3.12-200212031818.sgf 202
210 reg_genmove black
#? [!S5]

loadsgf games/nngs/saphir-gnugo-3.3.12-200212031818.sgf 204
220 reg_genmove black
#? [!T2]

# Kill the whole thing, not just a piece.
loadsgf games/nngs/saphir-gnugo-3.3.12-200212031818.sgf 222
230 reg_genmove black
#? [Q18]*

loadsgf games/nngs/saphir-gnugo-3.3.12-200212031818.sgf 242
240 reg_genmove black
#? [B4]*

loadsgf games/nngs/saphir-gnugo-3.3.12-200212031818.sgf 246
250 reg_genmove black
#? [F7]*

loadsgf games/nngs/saphir-gnugo-3.3.12-200212031818.sgf 258
260 reg_genmove black
#? [A4]

loadsgf games/nngs/saphir-gnugo-3.3.12-200212031818.sgf 292
270 reg_genmove black
#? [!P5]

loadsgf games/nngs/aburry-gnugo-3.3.12-200212162212.sgf 32
280 restricted_genmove black F3 H3
#? [F3]*

loadsgf games/nngs/aburry-gnugo-3.3.12-200212162212.sgf 38
290 restricted_genmove black H1 H6 F1
#? [!H1]*

loadsgf games/nngs/aburry-gnugo-3.3.12-200212162212.sgf 42
300 owl_defend B4
#? [A3|D1]*

loadsgf games/nngs/uno-gnugo-3.3.16-200302230528.sgf 22
310 restricted_genmove black H2 D6 D7
#? [D6|D7]*

loadsgf games/nngs/uno-gnugo-3.3.16-200302230528.sgf 40
330 restricted_genmove black R2 M15 H15
#? [M15|H15]*

loadsgf games/nngs/uno-gnugo-3.3.16-200302230528.sgf 76
340 restricted_genmove black B1 D2
#? [D2]

loadsgf games/nngs/uno-gnugo-3.3.16-200302230528.sgf 102
350 reg_genmove black
#? [H10]*

loadsgf games/nngs/uno-gnugo-3.3.16-200302230528.sgf 110
360 reg_genmove black
#? [M10]*

loadsgf games/nngs/uno-gnugo-3.3.16-200302230528.sgf 120
370 owl_defend L18
#? [0]

loadsgf games/nngs/uno-gnugo-3.3.16-200302230528.sgf 128
380 reg_genmove black
#? [J7|K8]*

loadsgf games/nngs/uno-gnugo-3.3.16-200302230528.sgf 138
390 restricted_genmove black B8 A11
#? [A11]*

loadsgf games/nngs/uno-gnugo-3.3.16-200302230528.sgf 142
400 owl_defend J5
#? [0]*

loadsgf games/nngs/uno-gnugo-3.3.16-200302230528.sgf 174
410 restricted_genmove black F2 E1
#? [E1]*

loadsgf games/nngs/joshj-gnugo-3.3.16-200302232226.sgf 65
420 reg_genmove black
#? [G6]*

# G12 allows white to live.
loadsgf games/nngs/joshj-gnugo-3.3.16-200302232226.sgf 185
430 reg_genmove black
#? [H10|G9|G10|G11|M6|L7|K7]*

loadsgf games/nngs/juka-gnugo-3.3.16-200303031959.sgf 20
440 restricted_genmove black E6 B5
#? [E6]

loadsgf games/nngs/juka-gnugo-3.3.16-200303031959.sgf 92
450 reg_genmove black
#? [O8]*

loadsgf games/nngs/juka-gnugo-3.3.16-200303031959.sgf 204
460 reg_genmove black
#? [N10]*

loadsgf games/nngs/looknglass-ggl-200302271041.sgf 99
470 reg_genmove black
#? [R8]

loadsgf games/nngs/evand-gnugo-3.3.17-200303201727.sgf 6
480 gg_genmove black 2
#? [R16]*

loadsgf games/nngs/evand-gnugo-3.3.17-200303201727.sgf 12
485 gg_genmove black 3
#? [D17]*

loadsgf games/nngs/evand-gnugo-3.3.17-200303201727.sgf 28
487 reg_genmove black
#? [P12]*

loadsgf games/nngs/evand-gnugo-3.3.17-200303201727.sgf 38
490 reg_genmove black
#? [M14|N17|M16]*

loadsgf games/nngs/evand-gnugo-3.3.17-200303201727.sgf 52
510 owl_attack R8
#? [0]*

#do something to help the C16 group
loadsgf games/nngs/evand-gnugo-3.3.17-200303201727.sgf 54
520 restricted_genmove black J8 J9 H14 F13 E13 B16 G13 J15 C14
#? [!J8|J9]

loadsgf games/nngs/gnugo-3.3.17-ylamaki-200303262305.sgf 9
530 reg_genmove white
#? [Q2]*

loadsgf games/nngs/gnugo-3.3.17-ylamaki-200303262305.sgf 11
540 reg_genmove white
#? [Q2]*

# New failure (3.3.18)
loadsgf games/nngs/gnugo-3.3.9-RikiTiki-200210170706.sgf 40
550 restricted_genmove white O3 G5
#? [O3]*

# New failure (3.3.18)
loadsgf games/nngs/gnugo-3.3.9-RikiTiki-200210170706.sgf 66
560 restricted_genmove white B13 F7
#? [B13]*

# New failure (3.3.18)
loadsgf games/nngs/gnugo-3.3.9-RikiTiki-200210170706.sgf 86
570 reg_genmove white
#? [F6]

loadsgf games/nngs/gnugo-3.3.9-RikiTiki-200210170706.sgf 88
580 reg_genmove white
#? [E5]*

loadsgf games/nngs/qly-gnugo-3.3.18-200305022134.sgf 70
590 reg_genmove black
#? [E2]*

#New failure (3.3.20)
#gnugo should do something about the O11 dragon
loadsgf games/nngs/gnugo-3.3.10-niki-200210281349.sgf 124
600 reg_genmove white
#? [L9|L12|L10]*

#New failure (3.3.20)
#C13 looks correct here /evand
loadsgf games/nngs/gnugo-3.3.10-niki-200210281349.sgf 144
610 reg_genmove white
#? [C13|D13]

#New failure (3.3.20)
#pull out the stones /evand
loadsgf games/nngs/gnugo-3.3.10-niki-200210281349.sgf 152
620 reg_genmove white
#? [G13|G14]*

loadsgf games/nngs/evand-gnugo-3.3.21-200306130638.sgf 156
630 reg_genmove black
#? [B6]*

#unneeded.  Take a big point somewhere.
loadsgf games/nngs/kit-gnugo-3.3.21-200306181940.sgf 16
640 reg_genmove black
#? [!R7]*

loadsgf games/nngs/kit-gnugo-3.3.21-200306181940.sgf 42
650 reg_genmove black
#? [Q2]*


############ End of Tests #################

# Report number of nodes visited by the tactical reading
10000 get_reading_node_counter
#? [0]&

# Report number of nodes visited by the owl code
10001 get_owl_node_counter
#? [0]&

# Report number of nodes visited by the life code
10002 get_connection_node_counter
#? [0]&

# Report number of trymoves/trykos visited by the test
10003 get_trymove_counter
#? [0]&
