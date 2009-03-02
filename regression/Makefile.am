TST = 13x13b.tst 13x13.tst arb.tst arend2.tst arend.tst \
      arion.tst atari_atari.tst auto01.tst auto02.tst \
      auto03.tst auto04.tst auto_handtalk.tst blunder.tst \
      break_in.tst buzco.tst capture.tst century2002.tst \
      connection.tst connect.tst \
      dniwog.tst ego.tst \
      endgame.tst endgame1.tst filllib.tst global.tst \
      golife.tst gunnar.tst handtalk.tst heikki.tst \
      joseki.tst lazarus.tst ld_owl.tst \
      manyfaces1.tst manyfaces.tst nando.tst neurogo.tst \
      newscore.tst nicklas1.tst nicklas2.tst nicklas3.tst \
      nicklas4.tst nicklas5.tst niki.tst ninestones.tst \
      nngs1.tst nngs2.tst nngs3.tst nngs4.tst \
      nngs.tst optics.tst owl1.tst \
      owl.tst reading.tst \
      rosebud.tst safety.tst score2.tst \
      score.tst semeai.tst \
      strategy2.tst strategy3.tst strategy4.tst strategy5.tst \
      strategy.tst tactics1.tst tactics.tst \
      thrash.tst trevora.tst trevorb.tst trevorc.tst \
      trevord.tst trevor.tst vie.tst viking.tst gifu03.tst seki.tst \
      9x9.tst unconditional.tst cgf2004.tst kgs.tst olympiad2004.tst \
      tiny.tst gifu05.tst 13x13c.tst STS-RV_0.tst STS-RV_1.tst \
      STS-RV_e.tst STS-RV_Misc.tst

noinst_SCRIPTS = eval.sh regress.sh test.sh eval3.sh

EXTRA_DIST = golois games $(TST) $(noinst_SCRIPTS) regress.awk \
             BREAKAGE regress.pl regress.plx regress.pike breakage2tst.py \
	     view.pike benchmark/*gtp regress.cmd

# Remove these files here... they are created locally
DISTCLEANFILES = *.orig *~

check: first_batch

regression: first_batch

test: all_batches

13x13: 13x13.tst
	env RD=$(srcdir) $(srcdir)/eval.sh $^ $(GG_OPTIONS)

13x13b: 13x13b.tst
	env RD=$(srcdir) $(srcdir)/eval.sh $^ $(GG_OPTIONS)

arb: arb.tst
	env RD=$(srcdir) $(srcdir)/eval.sh $^ $(GG_OPTIONS)

arend: arend.tst
	env RD=$(srcdir) $(srcdir)/eval.sh $^ $(GG_OPTIONS)

arend2: arend2.tst
	env RD=$(srcdir) $(srcdir)/eval.sh $^ $(GG_OPTIONS)

arion: arion.tst
	env RD=$(srcdir) $(srcdir)/eval.sh $^ $(GG_OPTIONS)

atari_atari: atari_atari.tst
	env RD=$(srcdir) $(srcdir)/eval.sh $^ $(GG_OPTIONS)

auto: auto01.tst auto02.tst auto03.tst auto04.tst
	env RD=$(srcdir) $(srcdir)/eval.sh $(srcdir)/auto01.tst $(GG_OPTIONS)
	env RD=$(srcdir) $(srcdir)/eval.sh $(srcdir)/auto02.tst $(GG_OPTIONS)
	env RD=$(srcdir) $(srcdir)/eval.sh $(srcdir)/auto03.tst $(GG_OPTIONS)
	env RD=$(srcdir) $(srcdir)/eval.sh $(srcdir)/auto04.tst $(GG_OPTIONS)

auto_handtalk: auto_handtalk.tst
	env RD=$(srcdir) $(srcdir)/eval.sh $^

blunder: blunder.tst
	env RD=$(srcdir) $(srcdir)/eval.sh $^ $(GG_OPTIONS)

break_in: break_in.tst
	env RD=$(srcdir) $(srcdir)/eval.sh $^ $(GG_OPTIONS)

buzco: buzco.tst
	env RD=$(srcdir) $(srcdir)/eval.sh $^ $(GG_OPTIONS)

capture: capture.tst
	env RD=$(srcdir) $(srcdir)/eval.sh $^  $(GG_OPTIONS)

century2002: century2002.tst
	env RD=$(srcdir) $(srcdir)/eval.sh $^  $(GG_OPTIONS)

connect: connect.tst
	env RD=$(srcdir) $(srcdir)/eval.sh $^ $(GG_OPTIONS)

connection: connection.tst
	env RD=$(srcdir) $(srcdir)/eval.sh $^ $(GG_OPTIONS)

dniwog: dniwog.tst
	env RD=$(srcdir) $(srcdir)/eval.sh $^ $(GG_OPTIONS)

ego: ego.tst
	env RD=$(srcdir) $(srcdir)/eval.sh $^ $(GG_OPTIONS)

endgame: endgame.tst
	env RD=$(srcdir) $(srcdir)/eval.sh $^ $(GG_OPTIONS)

filllib: filllib.tst
	env RD=$(srcdir) $(srcdir)/eval.sh $^ $(GG_OPTIONS)

gifu05: gifu05.tst
	env RD=$(srcdir) $(srcdir)/eval.sh $^ $(GG_OPTIONS)

golife: golife.tst
	env RD=$(srcdir) $(srcdir)/eval.sh $^ $(GG_OPTIONS)

global: global.tst
	env RD=$(srcdir) $(srcdir)/eval.sh $^ $(GG_OPTIONS)

gunnar: gunnar.tst
	env RD=$(srcdir) $(srcdir)/eval.sh $^ $(GG_OPTIONS)

handtalk: handtalk.tst
	env RD=$(srcdir) $(srcdir)/eval.sh $^ $(GG_OPTIONS)

heikki: heikki.tst
	env RD=$(srcdir) $(srcdir)/eval.sh $^ $(GG_OPTIONS)

joseki: joseki.tst
	env RD=$(srcdir) $(srcdir)/eval.sh $^ $(GG_OPTIONS)

lazarus: lazarus.tst
	env RD=$(srcdir) $(srcdir)/eval.sh $^ $(GG_OPTIONS)

ld_owl: ld_owl.tst
	env RD=$(srcdir) $(srcdir)/eval.sh $^ $(GG_OPTIONS)

manyfaces: manyfaces.tst
	env RD=$(srcdir) $(srcdir)/eval.sh $^ $(GG_OPTIONS)

manyfaces1: manyfaces1.tst
	env RD=$(srcdir) $(srcdir)/eval.sh $^ $(GG_OPTIONS)

nando: nando.tst
	env RD=$(srcdir) $(srcdir)/eval.sh $^ $(GG_OPTIONS)

neurogo: neurogo.tst
	env RD=$(srcdir) $(srcdir)/eval.sh $^ $(GG_OPTIONS)

newscore: newscore.tst
	env RD=$(srcdir) $(srcdir)/eval.sh $^ $(GG_OPTIONS)

nicklas: nicklas1 nicklas2 nicklas3 nicklas4 nicklas5

nicklas1: nicklas1.tst
	env RD=$(srcdir) $(srcdir)/eval.sh $^ $(GG_OPTIONS)

nicklas2: nicklas2.tst
	env RD=$(srcdir) $(srcdir)/eval.sh $^ $(GG_OPTIONS)

nicklas3: nicklas3.tst
	env RD=$(srcdir) $(srcdir)/eval.sh $^ $(GG_OPTIONS)

nicklas4: nicklas4.tst
	env RD=$(srcdir) $(srcdir)/eval.sh $^ $(GG_OPTIONS)

nicklas5: nicklas5.tst
	env RD=$(srcdir) $(srcdir)/eval.sh $^ $(GG_OPTIONS)

niki: niki.tst
	env RD=$(srcdir) $(srcdir)/eval.sh $^ $(GG_OPTIONS)

ninestones: ninestones.tst
	env RD=$(srcdir) $(srcdir)/eval.sh $^ $(GG_OPTIONS)

nngs: nngs.tst
	env RD=$(srcdir) $(srcdir)/eval.sh $^ $(GG_OPTIONS)

nngs1: nngs1.tst
	env RD=$(srcdir) $(srcdir)/eval.sh $^ $(GG_OPTIONS)

nngs2: nngs2.tst
	env RD=$(srcdir) $(srcdir)/eval.sh $^ $(GG_OPTIONS)

nngs3: nngs3.tst
	env RD=$(srcdir) $(srcdir)/eval.sh $^ $(GG_OPTIONS)

nngs4: nngs4.tst
	env RD=$(srcdir) $(srcdir)/eval.sh $^ $(GG_OPTIONS)

olympiad2004: olympiad2004.tst
	env RD=$(srcdir) $(srcdir)/eval.sh $^ $(GG_OPTIONS)

optics: optics.tst
	env RD=$(srcdir) $(srcdir)/eval.sh $^ $(GG_OPTIONS)

owl: owl.tst
	env RD=$(srcdir) $(srcdir)/eval.sh $^ $(GG_OPTIONS)

owl1: owl1.tst
	env RD=$(srcdir) $(srcdir)/eval.sh $^ $(GG_OPTIONS)

owl_rotall: owl.tst
	env RD=$(srcdir) $(srcdir)/eval.sh $^ --gtp-initial-orientation 0 $(GG_OPTIONS)
	env RD=$(srcdir) $(srcdir)/eval.sh $^ --gtp-initial-orientation 1 $(GG_OPTIONS)
	env RD=$(srcdir) $(srcdir)/eval.sh $^ --gtp-initial-orientation 2 $(GG_OPTIONS)
	env RD=$(srcdir) $(srcdir)/eval.sh $^ --gtp-initial-orientation 3 $(GG_OPTIONS)
	env RD=$(srcdir) $(srcdir)/eval.sh $^ --gtp-initial-orientation 4 $(GG_OPTIONS)
	env RD=$(srcdir) $(srcdir)/eval.sh $^ --gtp-initial-orientation 5 $(GG_OPTIONS)
	env RD=$(srcdir) $(srcdir)/eval.sh $^ --gtp-initial-orientation 6 $(GG_OPTIONS)
	env RD=$(srcdir) $(srcdir)/eval.sh $^ --gtp-initial-orientation 7 $(GG_OPTIONS)

reading: reading.tst
	env RD=$(srcdir) $(srcdir)/eval.sh $^ $(GG_OPTIONS)

rosebud: rosebud.tst
	env RD=$(srcdir) $(srcdir)/eval.sh $^ $(GG_OPTIONS)

safety: safety.tst
	env RD=$(srcdir) $(srcdir)/eval.sh $^ $(GG_OPTIONS)

score: score.tst
	env RD=$(srcdir) $(srcdir)/eval.sh $^ $(GG_OPTIONS)

score2: score2.tst
	env RD=$(srcdir) $(srcdir)/eval.sh $^ $(GG_OPTIONS)

semeai: semeai.tst
	env RD=$(srcdir) $(srcdir)/eval.sh $^ $(GG_OPTIONS)

strategy: strategy.tst
	env RD=$(srcdir) $(srcdir)/eval.sh $^ $(GG_OPTIONS)

strategy2: strategy2.tst
	env RD=$(srcdir) $(srcdir)/eval.sh $^ $(GG_OPTIONS)

strategy3: strategy3.tst
	env RD=$(srcdir) $(srcdir)/eval.sh $^ $(GG_OPTIONS)

strategy4: strategy4.tst
	env RD=$(srcdir) $(srcdir)/eval.sh $^ $(GG_OPTIONS)

strategy5: strategy5.tst
	env RD=$(srcdir) $(srcdir)/eval.sh $^ $(GG_OPTIONS)

STS-RV_0: STS-RV_0.tst
	env RD=$(srcdir) $(srcdir)/eval.sh $^ $(GG_OPTIONS)

STS-RV_1: STS-RV_1.tst
	env RD=$(srcdir) $(srcdir)/eval.sh $^ $(GG_OPTIONS)

STS-RV_e: STS-RV_e.tst
	env RD=$(srcdir) $(srcdir)/eval.sh $^ $(GG_OPTIONS)

STS-RV_Misc: STS-RV_Misc.tst
	env RD=$(srcdir) $(srcdir)/eval.sh $^ $(GG_OPTIONS)

tactics: tactics.tst
	env RD=$(srcdir) $(srcdir)/eval.sh $^ $(GG_OPTIONS)

tactics1: tactics1.tst
	env RD=$(srcdir) $(srcdir)/eval.sh $^ $(GG_OPTIONS)

thrash: thrash.tst
	env RD=$(srcdir) $(srcdir)/eval.sh $^ $(GG_OPTIONS)

tiny: tiny.tst
	env RD=$(srcdir) $(srcdir)/eval.sh $^ $(GG_OPTIONS)

trevor: trevor.tst
	env RD=$(srcdir) $(srcdir)/eval.sh $^ $(GG_OPTIONS)

trevora: trevora.tst
	env RD=$(srcdir) $(srcdir)/eval.sh $^ $(GG_OPTIONS)

trevorb: trevorb.tst
	env RD=$(srcdir) $(srcdir)/eval.sh $^ $(GG_OPTIONS)

trevorc: trevorc.tst
	env RD=$(srcdir) $(srcdir)/eval.sh $^ $(GG_OPTIONS)

trevord: trevord.tst
	env RD=$(srcdir) $(srcdir)/eval.sh $^ $(GG_OPTIONS)

viking: viking.tst
	env RD=$(srcdir) $(srcdir)/eval.sh $^ $(GG_OPTIONS)

vie: vie.tst
	env RD=$(srcdir) $(srcdir)/eval.sh $^ $(GG_OPTIONS)


all_batches: first_batch second_batch third_batch fourth_batch fifth_batch

first_batch: 
	$(srcdir)/regress.sh $(srcdir) reading.tst $(GG_OPTIONS)
	$(srcdir)/regress.sh $(srcdir) owl.tst $(GG_OPTIONS)
	$(srcdir)/regress.sh $(srcdir) ld_owl.tst $(GG_OPTIONS)
	$(srcdir)/regress.sh $(srcdir) optics.tst $(GG_OPTIONS)
	$(srcdir)/regress.sh $(srcdir) filllib.tst $(GG_OPTIONS)
	$(srcdir)/regress.sh $(srcdir) atari_atari.tst $(GG_OPTIONS)
	$(srcdir)/regress.sh $(srcdir) connection.tst $(GG_OPTIONS)
	$(srcdir)/regress.sh $(srcdir) break_in.tst $(GG_OPTIONS)
	$(srcdir)/regress.sh $(srcdir) blunder.tst $(GG_OPTIONS)
	$(srcdir)/regress.sh $(srcdir) unconditional.tst $(GG_OPTIONS)
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
	$(srcdir)/regress.sh $(srcdir) STS-RV_0.tst $(GG_OPTIONS)
	$(srcdir)/regress.sh $(srcdir) STS-RV_1.tst $(GG_OPTIONS)
	$(srcdir)/regress.sh $(srcdir) STS-RV_e.tst $(GG_OPTIONS)
	$(srcdir)/regress.sh $(srcdir) STS-RV_Misc.tst $(GG_OPTIONS)
	$(srcdir)/regress.sh $(srcdir) trevord.tst $(GG_OPTIONS)
	$(srcdir)/regress.sh $(srcdir) strategy4.tst $(GG_OPTIONS)

fifth_batch:
	$(srcdir)/regress.sh $(srcdir) owl1.tst $(GG_OPTIONS)
	$(srcdir)/regress.sh $(srcdir) handtalk.tst $(GG_OPTIONS)
	$(srcdir)/regress.sh $(srcdir) nngs2.tst $(GG_OPTIONS)
	$(srcdir)/regress.sh $(srcdir) nngs3.tst $(GG_OPTIONS)
	$(srcdir)/regress.sh $(srcdir) nngs4.tst $(GG_OPTIONS)
	$(srcdir)/regress.sh $(srcdir) strategy5.tst $(GG_OPTIONS)
	$(srcdir)/regress.sh $(srcdir) century2002.tst $(GG_OPTIONS)
	$(srcdir)/regress.sh $(srcdir) auto01.tst $(GG_OPTIONS)
	$(srcdir)/regress.sh $(srcdir) auto02.tst $(GG_OPTIONS)
	$(srcdir)/regress.sh $(srcdir) auto03.tst $(GG_OPTIONS)
	$(srcdir)/regress.sh $(srcdir) auto04.tst $(GG_OPTIONS)
	$(srcdir)/regress.sh $(srcdir) auto_handtalk.tst $(GG_OPTIONS)
	$(srcdir)/regress.sh $(srcdir) safety.tst $(GG_OPTIONS)
	$(srcdir)/regress.sh $(srcdir) ninestones.tst $(GG_OPTIONS)
	$(srcdir)/regress.sh $(srcdir) tactics1.tst $(GG_OPTIONS)
	$(srcdir)/regress.sh $(srcdir) manyfaces1.tst $(GG_OPTIONS)
	$(srcdir)/regress.sh $(srcdir) gunnar.tst $(GG_OPTIONS)
	$(srcdir)/regress.sh $(srcdir) arend2.tst $(GG_OPTIONS)
	$(srcdir)/regress.sh $(srcdir) nando.tst $(GG_OPTIONS)
	$(srcdir)/regress.sh $(srcdir) thrash.tst $(GG_OPTIONS)
	$(srcdir)/regress.sh $(srcdir) 13x13b.tst $(GG_OPTIONS)
	$(srcdir)/regress.sh $(srcdir) joseki.tst $(GG_OPTIONS)
	$(srcdir)/regress.sh $(srcdir) gifu03.tst $(GG_OPTIONS)
	$(srcdir)/regress.sh $(srcdir) seki.tst $(GG_OPTIONS)
	$(srcdir)/regress.sh $(srcdir) 9x9.tst $(GG_OPTIONS)
	$(srcdir)/regress.sh $(srcdir) cgf2004.tst $(GG_OPTIONS)
	$(srcdir)/regress.sh $(srcdir) kgs.tst $(GG_OPTIONS)
	$(srcdir)/regress.sh $(srcdir) olympiad2004.tst $(GG_OPTIONS)
	$(srcdir)/regress.sh $(srcdir) tiny.tst $(GG_OPTIONS)
	$(srcdir)/regress.sh $(srcdir) gifu05.tst $(GG_OPTIONS)
	$(srcdir)/regress.sh $(srcdir) 13x13c.tst $(GG_OPTIONS)
