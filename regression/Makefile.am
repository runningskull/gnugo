TST = life.tst owl.tst optics.tst reading.tst strategy.tst \
      ld_owl.tst connection.tst  \
      neurogo.tst arb.tst strategy2.tst strategy3.tst rosebud.tst \
      heikki.tst golife.tst dniwog.tst strategy4.tst \
      nicklas1.tst nicklas2.tst nicklas3.tst nicklas4.tst nicklas5.tst \
      filllib.tst arion.tst endgame.tst viking.tst ego.tst atari_atari.tst \
      score.tst manyfaces.tst cutstone.tst newscore.tst blunder.tst \
      niki.tst trevor.tst tactics.tst buzco.tst \
      capture.tst connect.tst global.tst vie.tst \
      owl_rot.tst score_rot.tst strategy_rot.tst connect_rot.tst arend.tst \
      trevora.tst

noinst_SCRIPTS = eval.sh regress.sh test.sh

EXTRA_DIST = golois games $(TST) $(noinst_SCRIPTS) regress.awk \
             STRATEGY NICKLAS READING BREAKAGE \
	     regress.pl regress.plx

# Remove these files here... they are created locally
DISTCLEANFILES = *.orig *~

check: first_batch

regression: first_batch

test: all_batches

reading: reading.tst
	$(srcdir)/eval.sh reading.tst

owl: owl.tst
	$(srcdir)/eval.sh owl.tst

owl_rot: owl_rot.tst
	$(srcdir)/eval.sh owl_rot.tst

owl_rotall: owl.tst
	$(srcdir)/eval.sh owl.tst --gtp-initial-orientation 0
	$(srcdir)/eval.sh owl.tst --gtp-initial-orientation 1
	$(srcdir)/eval.sh owl.tst --gtp-initial-orientation 2
	$(srcdir)/eval.sh owl.tst --gtp-initial-orientation 3
	$(srcdir)/eval.sh owl.tst --gtp-initial-orientation 4
	$(srcdir)/eval.sh owl.tst --gtp-initial-orientation 5
	$(srcdir)/eval.sh owl.tst --gtp-initial-orientation 6
	$(srcdir)/eval.sh owl.tst --gtp-initial-orientation 7

optics: optics.tst
	$(srcdir)/eval.sh optics.tst

life: life.tst
	$(srcdir)/eval.sh life.tst --life

filllib: filllib.tst
	$(srcdir)/eval.sh filllib.tst

strategy: strategy.tst
	$(srcdir)/eval.sh strategy.tst

strategy_rot: strategy_rot.tst
	$(srcdir)/eval.sh strategy_rot.tst

strategy2: strategy2.tst
	$(srcdir)/eval.sh strategy2.tst

strategy3: strategy3.tst
	$(srcdir)/eval.sh strategy3.tst

strategy4: strategy4.tst
	$(srcdir)/eval.sh strategy4.tst

tactics: tactics.tst
	$(srcdir)/eval.sh tactics.tst

cutstone: cutstone.tst
	$(srcdir)/eval.sh cutstone.tst

ld_owl: ld_owl.tst
	$(srcdir)/eval.sh ld_owl.tst

connection: connection.tst
	$(srcdir)/eval.sh connection.tst

endgame: endgame.tst
	$(srcdir)/eval.sh endgame.tst

blunder: blunder.tst
	$(srcdir)/eval.sh blunder.tst

heikki: heikki.tst
	$(srcdir)/eval.sh heikki.tst

neurogo: neurogo.tst
	$(srcdir)/eval.sh neurogo.tst

arb: arb.tst
	$(srcdir)/eval.sh arb.tst

dniwog: dniwog.tst
	$(srcdir)/eval.sh dniwog.tst

rosebud: rosebud.tst
	$(srcdir)/eval.sh rosebud.tst

golife: golife.tst
	$(srcdir)/eval.sh golife.tst

arion: arion.tst
	$(srcdir)/eval.sh arion.tst

viking: viking.tst
	$(srcdir)/eval.sh viking.tst

ego: ego.tst
	$(srcdir)/eval.sh ego.tst

atari_atari: atari_atari.tst
	$(srcdir)/eval.sh atari_atari.tst

manyfaces: manyfaces.tst
	$(srcdir)/eval.sh manyfaces.tst

score: score.tst
	$(srcdir)/eval.sh score.tst

score_rot: score_rot.tst
	$(srcdir)/eval.sh score_rot.tst

newscore: newscore.tst
	$(srcdir)/eval.sh newscore.tst

nicklas: nicklas1.tst nicklas2.tst nicklas3.tst nicklas4.tst nicklas5.tst
	$(srcdir)/eval.sh nicklas1.tst
	$(srcdir)/eval.sh nicklas2.tst
	$(srcdir)/eval.sh nicklas3.tst
	$(srcdir)/eval.sh nicklas4.tst
	$(srcdir)/eval.sh nicklas5.tst

nicklas1: nicklas1.tst
	$(srcdir)/eval.sh nicklas1.tst

nicklas2: nicklas2.tst
	$(srcdir)/eval.sh nicklas2.tst

nicklas3: nicklas3.tst
	$(srcdir)/eval.sh nicklas3.tst

nicklas4: nicklas4.tst
	$(srcdir)/eval.sh nicklas4.tst

nicklas5: nicklas5.tst
	$(srcdir)/eval.sh nicklas5.tst

niki: niki.tst
	$(srcdir)/eval.sh niki.tst

trevor: trevor.tst
	$(srcdir)/eval.sh trevor.tst

buzco: buzco.tst
	$(srcdir)/eval.sh buzco.tst

capture: capture.tst
	$(srcdir)/eval.sh capture.tst 

connect: connect.tst
	$(srcdir)/eval.sh connect.tst

connect_rot: connect_rot.tst
	$(srcdir)/eval.sh connect_rot.tst

global: global.tst
	$(srcdir)/eval.sh global.tst

vie: vie.tst
	$(srcdir)/eval.sh vie.tst

arend: arend.tst
	$(srcdir)/eval.sh arend.tst

trevora: trevora.tst
	$(srcdir)/eval.sh trevora.tst

all_batches: first_batch second_batch third_batch fourth_batch

first_batch: reading.tst owl.tst owl_rot.tst ld_owl.tst optics.tst filllib.tst atari_atari.tst connection.tst blunder.tst strategy.tst
	$(srcdir)/regress.sh $(srcdir) reading.tst
	$(srcdir)/regress.sh $(srcdir) owl.tst
	$(srcdir)/regress.sh $(srcdir) owl_rot.tst
	$(srcdir)/regress.sh $(srcdir) ld_owl.tst
	$(srcdir)/regress.sh $(srcdir) optics.tst
	$(srcdir)/regress.sh $(srcdir) filllib.tst
	$(srcdir)/regress.sh $(srcdir) atari_atari.tst
	$(srcdir)/regress.sh $(srcdir) connection.tst
	$(srcdir)/regress.sh $(srcdir) blunder.tst
	$(srcdir)/regress.sh $(srcdir) strategy.tst

second_batch: strategy2.tst endgame.tst heikki.tst neurogo.tst arb.tst rosebud.tst golife.tst arion.tst viking.tst ego.tst dniwog.tst
	$(srcdir)/regress.sh $(srcdir) endgame.tst
	$(srcdir)/regress.sh $(srcdir) heikki.tst
	$(srcdir)/regress.sh $(srcdir) neurogo.tst
	$(srcdir)/regress.sh $(srcdir) arb.tst
	$(srcdir)/regress.sh $(srcdir) rosebud.tst
	$(srcdir)/regress.sh $(srcdir) golife.tst
	$(srcdir)/regress.sh $(srcdir) arion.tst
	$(srcdir)/regress.sh $(srcdir) viking.tst
	$(srcdir)/regress.sh $(srcdir) ego.tst
	$(srcdir)/regress.sh $(srcdir) dniwog.tst
	$(srcdir)/regress.sh $(srcdir) strategy2.tst

third_batch: nicklas1.tst nicklas2.tst nicklas3.tst nicklas4.tst nicklas5.tst strategy3.tst manyfaces.tst niki.tst tactics.tst
	$(srcdir)/regress.sh $(srcdir) nicklas1.tst
	$(srcdir)/regress.sh $(srcdir) nicklas2.tst
	$(srcdir)/regress.sh $(srcdir) nicklas3.tst
	$(srcdir)/regress.sh $(srcdir) nicklas4.tst
	$(srcdir)/regress.sh $(srcdir) nicklas5.tst
	$(srcdir)/regress.sh $(srcdir) manyfaces.tst
	$(srcdir)/regress.sh $(srcdir) niki.tst
	$(srcdir)/regress.sh $(srcdir) trevor.tst
	$(srcdir)/regress.sh $(srcdir) tactics.tst
	$(srcdir)/regress.sh $(srcdir) buzco.tst
	$(srcdir)/regress.sh $(srcdir) strategy3.tst

fourth_batch: strategy4.tst capture.tst connect.tst global.tst vie.tst
	$(srcdir)/regress.sh $(srcdir) capture.tst
	$(srcdir)/regress.sh $(srcdir) connect.tst
	$(srcdir)/regress.sh $(srcdir) global.tst
	$(srcdir)/regress.sh $(srcdir) vie.tst
	$(srcdir)/regress.sh $(srcdir) arend.tst
	$(srcdir)/regress.sh $(srcdir) trevora.tst
	$(srcdir)/regress.sh $(srcdir) strategy4.tst
