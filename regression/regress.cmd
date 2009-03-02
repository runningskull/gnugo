@echo off
setlocal
rem A simplified .cmd port of regress.sh, using regress.awk.
rem
rem Usage: regress [gnugo.exe]
rem
rem Start in the gnugo/regress subdir, the same one where regress.awk 
rem (and the original regress.sh) resides. You will need a working 
rem awk.exe in your %PATH%.
rem
rem Use the command line to point to the gnugo.exe you want to test.
rem If built with VStudio sln/vcproj files, use one of:
rem   	..\interface\{debug,release,minsizerel,relwithdebinfo}\gnugo.exe
rem If built with NMake/MSYS/MinGW makefiles, use:
rem     ..\interface\gnugo.exe

rem The 5 test batches, based on the main gnugo tests, regress/Makefile*
rem XXX Need to track the main regress/Makefile for changes to these lists!
set b1=reading owl ld_owl optics filllib atari_atari connection break_in blunder unconditional trevora nngs1 strategy 
set b2=endgame heikki neurogo arb rosebud golife arion viking ego dniwog lazarus trevorb strategy2 
set b3=nicklas1 nicklas2 nicklas3 nicklas4 nicklas5 manyfaces niki trevor tactics buzco nngs trevorc strategy3 
set b4=capture connect global vie arend 13x13 semeai STS-RV_0 STS-RV_1 STS-RV_e STS-RV_Misc trevord strategy4 
set b5=owl1 handtalk nngs2 nngs3 nngs4 strategy5 century2002 auto01 auto02 auto03 auto04 auto_handtalk safety ninestones tactics1 manyfaces1 gunnar arend2 nando thrash 13x13b joseki gifu03 seki 9x9 cgf2004 kgs olympiad2004 tiny gifu05 13x13c 

rem Check for regress.awk, fail if not present.
if not exist regress.awk echo ERROR: cannot find regress.awk. aborting...
if not exist regress.awk goto done

rem Optionally get gnugo.exe location from command line; fail if not found.
if "%1"=="" set gnugo=..\interface\gnugo.exe
if not "%1"=="" set gnugo=%1
if not exist %gnugo% echo ERROR: cannot find gnugo.exe executable [%gnugo%]. aborting...
if not exist %gnugo% goto done

rem Finally, do the actual testing.
for %%t in (%b1%) do %gnugo% --quiet --mode gtp < %%t.tst | awk -f regress.awk tst=%%t.tst
for %%t in (%b2%) do %gnugo% --quiet --mode gtp < %%t.tst | awk -f regress.awk tst=%%t.tst
for %%t in (%b3%) do %gnugo% --quiet --mode gtp < %%t.tst | awk -f regress.awk tst=%%t.tst
for %%t in (%b4%) do %gnugo% --quiet --mode gtp < %%t.tst | awk -f regress.awk tst=%%t.tst
for %%t in (%b5%) do %gnugo% --quiet --mode gtp < %%t.tst | awk -f regress.awk tst=%%t.tst

:done

