# Reset applicable counters
reset_connection_node_counter
reset_owl_node_counter
reset_reading_node_counter
reset_trymove_counter

loadsgf games/mfgg1.sgf 43
1 reg_genmove black
#? [S13]*

#See also owl1:265.
loadsgf games/mfgg1.sgf 49
2 reg_genmove black
#? [S16]

#CATEGORY=PATTERN_TUNING
#DESCRIPTION=No terribly serious consequences, S11 not optimal.
#SEVERITY=3
loadsgf games/mfgg1.sgf 65
3 reg_genmove black
#? [S12]*

loadsgf games/mfgg1.sgf 97
4 reg_genmove black
#? [!T6]

#CATEGORY=FUSEKI_STRATEGY
#DESCRIPTION=N6 is not terrible - right concept.
#SEVERITY=2
loadsgf games/mfgg2.sgf 19
5 reg_genmove black
#? [M5|M15|R3|R17]

#CATEGORY=FUSEKI_STRATEGY
#DESCRIPTION=E6 really misses the point.
#SEVERITY=9
loadsgf games/mfgg2.sgf 23
6 reg_genmove black
#? [R3]*

#CATEGORY=OWL_TUNING
#DESCRIPTION=Q5 dragon is under severe pressure.
#SEVERITY=8
loadsgf games/mfgg2.sgf 45
7 reg_genmove black
#? [R2|S7]

#CATEGORY=OWL_TUNING
#DESCRIPTION=GNU Go doesn't understand this type of corner position!
#SEVERITY=10
#GG often messes up in this type of corner position.
loadsgf games/mfgg2.sgf 117
8 reg_genmove black
#? [T2]

loadsgf games/mfgg3.sgf 42
9 reg_genmove black
#? [!P12]*

loadsgf games/mfgg3.sgf 44
10 reg_genmove black
#? [N3]*


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
