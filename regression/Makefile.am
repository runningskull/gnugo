TST =  13x13.tst arb.tst arend.tst arion.tst atari_atari.tst blunder.tst \
       buzco.tst capture.tst century-2002.tst connect.tst connect_rot.tst \
       connection.tst connection_rot.tst cutstone.tst dniwog.tst ego.tst \
       endgame.tst endgame_rot.tst filllib.tst global.tst golife.tst \
       handtalk.tst heikki.tst lazarus.tst ld_owl.tst ld_owl_rot.tst \
       life.tst manyfaces.tst neurogo.tst newscore.tst nicklas1.tst \
       nicklas2.tst nicklas3.tst nicklas4.tst nicklas5.tst niki.tst \
       nngs.tst nngs1.tst nngs2.tst optics.tst optics_rot.tst owl.tst \
       owl1.tst owl_rot.tst reading.tst reading_rot.tst rosebud.tst \
       score.tst score2.tst score_rot.tst semeai.tst semeai2.tst \
       strategy.tst strategy2.tst strategy3.tst strategy4.tst \
       strategy5.tst strategy_rot.tst tactics.tst trevor.tst trevora.tst \
       trevorb.tst trevorc.tst trevord.tst vie.tst viking.tst

noinst_SCRIPTS = eval.sh regress.sh test.sh eval3.sh

EXTRA_DIST = golois games $(TST) $(noinst_SCRIPTS) regress.awk \
             BREAKAGE regress.pl regress.plx

# Remove these files here... they are created locally
DISTCLEANFILES = *.orig *~

check: first_batch

regression: first_batch

test: all_batches


13x13: 13x13.tst
	$(srcdir)/eval.sh 13x13.tst $(GG_OPTIONS)

arb: arb.tst
	$(srcdir)/eval.sh arb.tst $(GG_OPTIONS)

arend: arend.tst
	$(srcdir)/eval.sh arend.tst $(GG_OPTIONS)

arion: arion.tst
	$(srcdir)/eval.sh arion.tst $(GG_OPTIONS)

atari_atari: atari_atari.tst
	$(srcdir)/eval.sh atari_atari.tst $(GG_OPTIONS)

blunder: blunder.tst
	$(srcdir)/eval.sh blunder.tst $(GG_OPTIONS)

buzco: buzco.tst
	$(srcdir)/eval.sh buzco.tst $(GG_OPTIONS)

capture: capture.tst
	$(srcdir)/eval.sh capture.tst  $(GG_OPTIONS)

century: century-2002.tst
	$(srcdir)/eval.sh century-2002.tst  $(GG_OPTIONS)

connect: connect.tst
	$(srcdir)/eval.sh connect.tst $(GG_OPTIONS)

connect_rot: connect_rot.tst
	$(srcdir)/eval.sh connect_rot.tst $(GG_OPTIONS)

connection: connection.tst
	$(srcdir)/eval.sh connection.tst $(GG_OPTIONS)

connection_rot: connection_rot.tst
	$(srcdir)/eval.sh connection_rot.tst $(GG_OPTIONS)

cutstone: cutstone.tst
	$(srcdir)/eval.sh cutstone.tst $(GG_OPTIONS)

dniwog: dniwog.tst
	$(srcdir)/eval.sh dniwog.tst $(GG_OPTIONS)

ego: ego.tst
	$(srcdir)/eval.sh ego.tst $(GG_OPTIONS)

endgame: endgame.tst
	$(srcdir)/eval.sh endgame.tst $(GG_OPTIONS)

endgame_rot: endgame_rot.tst
	$(srcdir)/eval.sh endgame_rot.tst $(GG_OPTIONS)

filllib: filllib.tst
	$(srcdir)/eval.sh filllib.tst $(GG_OPTIONS)

golife: golife.tst
	$(srcdir)/eval.sh golife.tst $(GG_OPTIONS)

global: global.tst
	$(srcdir)/eval.sh global.tst $(GG_OPTIONS)

handtalk: handtalk.tst
	$(srcdir)/eval.sh handtalk.tst

heikki: heikki.tst
	$(srcdir)/eval.sh heikki.tst $(GG_OPTIONS)

lazarus: lazarus.tst
	$(srcdir)/eval.sh lazarus.tst $(GG_OPTIONS)

ld_owl: ld_owl.tst
	$(srcdir)/eval.sh ld_owl.tst $(GG_OPTIONS)

ld_owl_rot: ld_owl_rot.tst
	$(srcdir)/eval.sh ld_owl_rot.tst $(GG_OPTIONS)

life: life.tst
	$(srcdir)/eval.sh life.tst --life $(GG_OPTIONS)

manyfaces: manyfaces.tst
	$(srcdir)/eval.sh manyfaces.tst $(GG_OPTIONS)

neurogo: neurogo.tst
	$(srcdir)/eval.sh neurogo.tst $(GG_OPTIONS)

newscore: newscore.tst
	$(srcdir)/eval.sh newscore.tst $(GG_OPTIONS)

nicklas: nicklas1.tst nicklas2.tst nicklas3.tst nicklas4.tst nicklas5.tst
	$(srcdir)/eval.sh nicklas1.tst $(GG_OPTIONS)
	$(srcdir)/eval.sh nicklas2.tst $(GG_OPTIONS)
	$(srcdir)/eval.sh nicklas3.tst $(GG_OPTIONS)
	$(srcdir)/eval.sh nicklas4.tst $(GG_OPTIONS)
	$(srcdir)/eval.sh nicklas5.tst $(GG_OPTIONS)

nicklas1: nicklas1.tst
	$(srcdir)/eval.sh nicklas1.tst $(GG_OPTIONS)

nicklas2: nicklas2.tst
	$(srcdir)/eval.sh nicklas2.tst $(GG_OPTIONS)

nicklas3: nicklas3.tst
	$(srcdir)/eval.sh nicklas3.tst $(GG_OPTIONS)

nicklas4: nicklas4.tst
	$(srcdir)/eval.sh nicklas4.tst $(GG_OPTIONS)

nicklas5: nicklas5.tst
	$(srcdir)/eval.sh nicklas5.tst $(GG_OPTIONS)

niki: niki.tst
	$(srcdir)/eval.sh niki.tst $(GG_OPTIONS)

nngs: nngs.tst
	$(srcdir)/eval.sh nngs.tst $(GG_OPTIONS)

nngs1: nngs1.tst
	$(srcdir)/eval.sh nngs1.tst $(GG_OPTIONS)

nngs2: nngs2.tst
	$(srcdir)/eval.sh nngs2.tst $(GG_OPTIONS)

optics: optics.tst
	$(srcdir)/eval.sh optics.tst $(GG_OPTIONS)

optics_rot: optics_rot.tst
	$(srcdir)/eval.sh optics_rot.tst $(GG_OPTIONS)

owl: owl.tst
	$(srcdir)/eval.sh owl.tst $(GG_OPTIONS)

owl1: owl1.tst
	$(srcdir)/eval.sh owl1.tst $(GG_OPTIONS)

owl_rot: owl_rot.tst
	$(srcdir)/eval.sh owl_rot.tst $(GG_OPTIONS)

owl_rotall: owl.tst
	$(srcdir)/eval.sh owl.tst --gtp-initial-orientation 0 $(GG_OPTIONS)
	$(srcdir)/eval.sh owl.tst --gtp-initial-orientation 1 $(GG_OPTIONS)
	$(srcdir)/eval.sh owl.tst --gtp-initial-orientation 2 $(GG_OPTIONS)
	$(srcdir)/eval.sh owl.tst --gtp-initial-orientation 3 $(GG_OPTIONS)
	$(srcdir)/eval.sh owl.tst --gtp-initial-orientation 4 $(GG_OPTIONS)
	$(srcdir)/eval.sh owl.tst --gtp-initial-orientation 5 $(GG_OPTIONS)
	$(srcdir)/eval.sh owl.tst --gtp-initial-orientation 6 $(GG_OPTIONS)
	$(srcdir)/eval.sh owl.tst --gtp-initial-orientation 7 $(GG_OPTIONS)

reading: reading.tst
	$(srcdir)/eval.sh reading.tst $(GG_OPTIONS)

reading_rot: reading_rot.tst
	$(srcdir)/eval.sh reading_rot.tst $(GG_OPTIONS)

rosebud: rosebud.tst
	$(srcdir)/eval.sh rosebud.tst $(GG_OPTIONS)

score: score.tst
	$(srcdir)/eval.sh score.tst $(GG_OPTIONS)

score2: score2.tst
	$(srcdir)/eval.sh score2.tst $(GG_OPTIONS)

score_rot: score_rot.tst
	$(srcdir)/eval.sh score_rot.tst $(GG_OPTIONS)

semeai: semeai.tst
	$(srcdir)/eval.sh semeai.tst $(GG_OPTIONS)

semeai2: semeai2.tst
	$(srcdir)/eval.sh semeai2.tst $(GG_OPTIONS)

strategy: strategy.tst
	$(srcdir)/eval.sh strategy.tst $(GG_OPTIONS)

strategy_rot: strategy_rot.tst
	$(srcdir)/eval.sh strategy_rot.tst $(GG_OPTIONS)

strategy2: strategy2.tst
	$(srcdir)/eval.sh strategy2.tst $(GG_OPTIONS)

strategy3: strategy3.tst
	$(srcdir)/eval.sh strategy3.tst $(GG_OPTIONS)

strategy4: strategy4.tst
	$(srcdir)/eval.sh strategy4.tst $(GG_OPTIONS)

strategy5: strategy5.tst
	$(srcdir)/eval.sh strategy5.tst $(GG_OPTIONS)

tactics: tactics.tst
	$(srcdir)/eval.sh tactics.tst $(GG_OPTIONS)

trevor: trevor.tst
	$(srcdir)/eval.sh trevor.tst $(GG_OPTIONS)

trevora: trevora.tst
	$(srcdir)/eval.sh trevora.tst $(GG_OPTIONS)

trevorb: trevorb.tst
	$(srcdir)/eval.sh trevorb.tst $(GG_OPTIONS)

trevorc: trevorc.tst
	$(srcdir)/eval.sh trevorc.tst $(GG_OPTIONS)

trevord: trevord.tst
	$(srcdir)/eval.sh trevord.tst $(GG_OPTIONS)

viking: viking.tst
	$(srcdir)/eval.sh viking.tst $(GG_OPTIONS)

vie: vie.tst
	$(srcdir)/eval.sh vie.tst $(GG_OPTIONS)


all_batches: first_batch second_batch third_batch fourth_batch fifth_batch

first_batch: 
	$(srcdir)/regress.sh $(srcdir) reading.tst $(GG_OPTIONS)
	$(srcdir)/regress.sh $(srcdir) owl.tst $(GG_OPTIONS)
	$(srcdir)/regress.sh $(srcdir) owl_rot.tst $(GG_OPTIONS)
	$(srcdir)/regress.sh $(srcdir) ld_owl.tst $(GG_OPTIONS)
	$(srcdir)/regress.sh $(srcdir) optics.tst $(GG_OPTIONS)
	$(srcdir)/regress.sh $(srcdir) filllib.tst $(GG_OPTIONS)
	$(srcdir)/regress.sh $(srcdir) atari_atari.tst $(GG_OPTIONS)
	$(srcdir)/regress.sh $(srcdir) connection.tst $(GG_OPTIONS)
	$(srcdir)/regress.sh $(srcdir) blunder.tst $(GG_OPTIONS)
	$(srcdir)/regress.sh $(srcdir) trevora.tst $(GG_OPTIONS)
	$(srcdir)/regress.sh $(srcdir) nngs1.tst $(GG_OPTIONS)
	$(srcdir)/regress.sh $(srcdir) strategy.tst $(GG_OPTIONS)

second_batch: 
	$(srcdir)/regress.sh $(srcdir) endgame.tst $(GG_OPTIONS)
	$(srcdir)/regress.sh $(srcdir) heikki.tst $(GG_OPTIONS)
	$(srcdir)/regress.sh $(srcdir) neurogo.tst $(GG_OPTIONS)
	$(srcdir)/regress.sh $(srcdir) arb.tst $(GG_OPTIONS)
	$(srcdir)/regress.sh $(srcdir) rosebud.tst $(GG_OPTIONS)
	$(srcdir)/regress.sh $(srcdir) golife.tst $(GG_OPTIONS)
	$(srcdir)/regress.sh $(srcdir) arion.tst $(GG_OPTIONS)
	$(srcdir)/regress.sh $(srcdir) viking.tst $(GG_OPTIONS)
	$(srcdir)/regress.sh $(srcdir) ego.tst $(GG_OPTIONS)
	$(srcdir)/regress.sh $(srcdir) dniwog.tst $(GG_OPTIONS)
	$(srcdir)/regress.sh $(srcdir) lazarus.tst $(GG_OPTIONS)
	$(srcdir)/regress.sh $(srcdir) trevorb.tst $(GG_OPTIONS)
	$(srcdir)/regress.sh $(srcdir) strategy2.tst $(GG_OPTIONS)

third_batch: 
	$(srcdir)/regress.sh $(srcdir) nicklas1.tst $(GG_OPTIONS)
	$(srcdir)/regress.sh $(srcdir) nicklas2.tst $(GG_OPTIONS)
	$(srcdir)/regress.sh $(srcdir) nicklas3.tst $(GG_OPTIONS)
	$(srcdir)/regress.sh $(srcdir) nicklas4.tst $(GG_OPTIONS)
	$(srcdir)/regress.sh $(srcdir) nicklas5.tst $(GG_OPTIONS)
	$(srcdir)/regress.sh $(srcdir) manyfaces.tst $(GG_OPTIONS)
	$(srcdir)/regress.sh $(srcdir) niki.tst $(GG_OPTIONS)
	$(srcdir)/regress.sh $(srcdir) trevor.tst $(GG_OPTIONS)
	$(srcdir)/regress.sh $(srcdir) tactics.tst $(GG_OPTIONS)
	$(srcdir)/regress.sh $(srcdir) buzco.tst $(GG_OPTIONS)
	$(srcdir)/regress.sh $(srcdir) nngs.tst $(GG_OPTIONS)
	$(srcdir)/regress.sh $(srcdir) trevorc.tst $(GG_OPTIONS)
	$(srcdir)/regress.sh $(srcdir) strategy3.tst $(GG_OPTIONS)

fourth_batch: 
	$(srcdir)/regress.sh $(srcdir) capture.tst $(GG_OPTIONS)
	$(srcdir)/regress.sh $(srcdir) connect.tst $(GG_OPTIONS)
	$(srcdir)/regress.sh $(srcdir) global.tst $(GG_OPTIONS)
	$(srcdir)/regress.sh $(srcdir) vie.tst $(GG_OPTIONS)
	$(srcdir)/regress.sh $(srcdir) arend.tst $(GG_OPTIONS)
	$(srcdir)/regress.sh $(srcdir) 13x13.tst $(GG_OPTIONS)
	$(srcdir)/regress.sh $(srcdir) semeai.tst $(GG_OPTIONS)
	$(srcdir)/regress.sh $(srcdir) trevord.tst $(GG_OPTIONS)
	$(srcdir)/regress.sh $(srcdir) strategy4.tst $(GG_OPTIONS)

fifth_batch:
	$(srcdir)/regress.sh $(srcdir) owl1.tst $(GG_OPTIONS)
	$(srcdir)/regress.sh $(srcdir) handtalk.tst $(GG_OPTIONS)
	$(srcdir)/regress.sh $(srcdir) nngs2.tst $(GG_OPTIONS)
	$(srcdir)/regress.sh $(srcdir) strategy5.tst $(GG_OPTIONS)
	$(srcdir)/regress.sh $(srcdir) century-2002.tst $(GG_OPTIONS)
