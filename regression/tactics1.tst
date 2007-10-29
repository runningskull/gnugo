# Apparently, T9 gets overvalued because the effect on S7 and T6 is
# counted twice (territorially and strategically)
loadsgf games/tactics05.sgf
10 restricted_genmove white T9 T8 S9 S8
#? [S8|S9]

loadsgf games/tactics06.sgf
20 owl_defend C19
#? [1 A17]*

# J1 is worth 6 points
loadsgf games/seki03.sgf
30 reg_genmove white
#? [J1]*


# attack_either and defend_both tests could go in a separate test suite,
# once there is more of them

loadsgf games/nngs/Lazarus-gnugo-3.1.34-200204280120.sgf 32
trymove black D12
trymove white C13
trymove white C12
trymove black C11
101 attack_either C11 D12
#? [1]
popgo
popgo
popgo
popgo

# This is subtle. White N3 works as defense to L4.
loadsgf games/mertin13x13/katsunari-gnugo2.W+4.sgf 126
trymove black L4
102 defend_both N7 K4
#? [0]*
popgo

loadsgf games/nngs/Lazarus-gnugo-3.2-200205011927.sgf 86
trymove black G4
trymove white G3
103 attack_either E5 G3
#? [1]
104 attack_either G3 E5
#? [1]
popgo
popgo

# C9 is errouneously thought to be an owl-attack against G13,
# which could indicate a semeai reading problem. On the other
# hand, since G13 is judged owl-defenseless, it seems to me
# that C9 is strangely overvalued.
loadsgf games/tactics07.sgf
105 reg_genmove white
#? [F7]

loadsgf games/tactics08.sgf
106 reg_genmove white
#? [F8|F6]

loadsgf games/edge_defense1.sgf
107 reg_genmove white
#? [B13|B14]

# Obviously P7 is effective.
loadsgf games/gifu2005/mfg-gnugo.sgf 204
trymove white O6
108 defend_both O9 P6
#? [1]*
popgo
