# Microsoft Developer Studio Project File - Name="patterns" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Static Library" 0x0104

CFG=patterns - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "patterns.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "patterns.mak" CFG="patterns - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "patterns - Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "patterns - Win32 Debug" (based on "Win32 (x86) Static Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "patterns - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_MBCS" /D "_LIB" /YX /FD /c
# ADD CPP /GX /Zi /O2 /I "." /I ".." /I "..\engine" /I "../sgf" /I "../utils" /D "WIN32" /D "NDEBUG" /D "HAVE_CONFIG_H" /D "_MBCS" /D "_LIB" /YX"patterns.h" /Fd"Release/patterns" /FD /c
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo

!ELSEIF  "$(CFG)" == "patterns - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_MBCS" /D "_LIB" /YX /FD /GZ /c
# ADD CPP /W2 /Gm /GX /Zi /Od /I "." /I ".." /I "..\engine" /I "../sgf" /I "../utils" /D "WIN32" /D "_DEBUG" /D "HAVE_CONFIG_H" /D "_MBCS" /D "_LIB" /FR /YX"patterns.h" /Fd"Debug/patterns" /FD /GZ /c
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo

!ENDIF 

# Begin Target

# Name "patterns - Win32 Release"
# Name "patterns - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Source File

SOURCE=.\aa_attackpat.c
# End Source File
# Begin Source File

SOURCE=.\apatterns.c
# End Source File
# Begin Source File

SOURCE=.\barriers.c
# End Source File
# Begin Source File

SOURCE=.\conn.c
# End Source File
# Begin Source File

SOURCE=.\connections.c
# End Source File
# Begin Source File

SOURCE=.\dpatterns.c
# End Source File
# Begin Source File

SOURCE=.\endgame.c
# End Source File
# Begin Source File

SOURCE=.\eyes.c
# SUBTRACT CPP /YX
# End Source File
# Begin Source File

SOURCE=.\fuseki13.c
# End Source File
# Begin Source File

SOURCE=.\fuseki19.c
# End Source File
# Begin Source File

SOURCE=.\fuseki9.c
# End Source File
# Begin Source File

SOURCE=.\fusekipat.c
# End Source File
# Begin Source File

SOURCE=.\handipat.c
# End Source File
# Begin Source File

SOURCE=.\helpers.c
# End Source File
# Begin Source File

SOURCE=.\influence.c
# End Source File
# Begin Source File

SOURCE=.\josekidb.c
# End Source File
# Begin Source File

SOURCE=.\mcpat.c
# End Source File
# Begin Source File

SOURCE=.\owl_attackpat.c
# End Source File
# Begin Source File

SOURCE=.\owl_defendpat.c

!IF  "$(CFG)" == "patterns - Win32 Release"

# ADD CPP /Zi /O2 /YX

!ELSEIF  "$(CFG)" == "patterns - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\owl_vital_apat.c
# End Source File
# Begin Source File

SOURCE=.\patterns.c
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# Begin Source File

SOURCE=.\dfa.h
# End Source File
# Begin Source File

SOURCE=.\eyes.h
# End Source File
# Begin Source File

SOURCE=.\patlib.h
# End Source File
# Begin Source File

SOURCE=.\patterns.h
# End Source File
# End Group
# Begin Group "db files"

# PROP Default_Filter "*.db"
# Begin Group "nobuild"

# PROP Default_Filter ""
# Begin Source File

SOURCE=.\gogo.db
# End Source File
# Begin Source File

SOURCE=.\hoshi_other.db
# End Source File
# Begin Source File

SOURCE=.\komoku.db
# End Source File
# Begin Source File

SOURCE=.\mc_mogo_classic.db
# End Source File
# Begin Source File

SOURCE=.\mc_montegnu_classic.db
# End Source File
# Begin Source File

SOURCE=.\mokuhazushi.db
# End Source File
# Begin Source File

SOURCE=.\patterns2.db
# End Source File
# Begin Source File

SOURCE=.\sansan.db
# End Source File
# Begin Source File

SOURCE=.\takamoku.db
# End Source File
# End Group
# Begin Source File

SOURCE=.\aa_attackpats.db

!IF  "$(CFG)" == "patterns - Win32 Release"

USERDEP__AA_AT="$(IntDir)\mkpat.exe"	
# Begin Custom Build
IntDir=.\Release
InputPath=.\aa_attackpats.db

"aa_attackpat.c" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(IntDir)\mkpat -D -m -b -t ..\patterns\aa_attackpats.dtr aa_attackpat -i ..\patterns\aa_attackpats.db -o aa_attackpat.c

# End Custom Build

!ELSEIF  "$(CFG)" == "patterns - Win32 Debug"

USERDEP__AA_AT="$(IntDir)\mkpat.exe"	
# Begin Custom Build
IntDir=.\Debug
InputPath=.\aa_attackpats.db

"aa_attackpat.c" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(IntDir)\mkpat -D -m -b -t ..\patterns\aa_attackpats.dtr aa_attackpat -i ..\patterns\aa_attackpats.db -o aa_attackpat.c

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\attack.db

!IF  "$(CFG)" == "patterns - Win32 Release"

USERDEP__ATTAC="$(IntDir)\mkpat.exe"	
# Begin Custom Build
IntDir=.\Release
InputPath=.\attack.db

"apatterns.c" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(IntDir)\mkpat -X  attpat -i ..\patterns\attack.db -o apatterns.c

# End Custom Build

!ELSEIF  "$(CFG)" == "patterns - Win32 Debug"

USERDEP__ATTAC="$(IntDir)\mkpat.exe"	
# Begin Custom Build
IntDir=.\Debug
InputPath=.\attack.db

"apatterns.c" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(IntDir)\mkpat -X  attpat -i ..\patterns\attack.db -o apatterns.c

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\barriers.db

!IF  "$(CFG)" == "patterns - Win32 Release"

USERDEP__BARRI="$(IntDir)\mkpat.exe"	
# Begin Custom Build
IntDir=.\Release
InputPath=.\barriers.db

"barriers.c" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(IntDir)\mkpat -c -b barrierspat -i ..\patterns\barriers.db -o barriers.c

# End Custom Build

!ELSEIF  "$(CFG)" == "patterns - Win32 Debug"

USERDEP__BARRI="$(IntDir)\mkpat.exe"	
# Begin Custom Build
IntDir=.\Debug
InputPath=.\barriers.db

"barriers.c" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(IntDir)\mkpat -c -b barrierspat -i ..\patterns\barriers.db -o barriers.c

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\conn.db

!IF  "$(CFG)" == "patterns - Win32 Release"

USERDEP__CONN_="$(IntDir)\mkpat.exe"	
# Begin Custom Build
IntDir=.\Release
InputPath=.\conn.db

"conn.c" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(IntDir)\mkpat -c conn -i ..\patterns\conn.db -o conn.c

# End Custom Build

!ELSEIF  "$(CFG)" == "patterns - Win32 Debug"

USERDEP__CONN_="$(IntDir)\mkpat.exe"	
# Begin Custom Build
IntDir=.\Debug
InputPath=.\conn.db

"conn.c" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(IntDir)\mkpat -c conn -i ..\patterns\conn.db -o conn.c

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\defense.db

!IF  "$(CFG)" == "patterns - Win32 Release"

USERDEP__DEFEN="$(IntDir)\mkpat.exe"	
# Begin Custom Build
IntDir=.\Release
InputPath=.\defense.db

"dpatterns.c" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(IntDir)\mkpat defpat -i ..\patterns\defense.db -o dpatterns.c

# End Custom Build

!ELSEIF  "$(CFG)" == "patterns - Win32 Debug"

USERDEP__DEFEN="$(IntDir)\mkpat.exe"	
# Begin Custom Build
IntDir=.\Debug
InputPath=.\defense.db

"dpatterns.c" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(IntDir)\mkpat defpat -i ..\patterns\defense.db -o dpatterns.c

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\endgame.db

!IF  "$(CFG)" == "patterns - Win32 Release"

USERDEP__ENDGA="$(IntDir)\mkpat.exe"	
# Begin Custom Build
IntDir=.\Release
InputPath=.\endgame.db

"endgame.c" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(IntDir)\mkpat -b endpat -i ..\patterns\endgame.db -o endgame.c

# End Custom Build

!ELSEIF  "$(CFG)" == "patterns - Win32 Debug"

USERDEP__ENDGA="$(IntDir)\mkpat.exe"	
# Begin Custom Build
IntDir=.\Debug
InputPath=.\endgame.db

"endgame.c" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(IntDir)\mkpat -b endpat -i ..\patterns\endgame.db -o endgame.c

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\eyes.db

!IF  "$(CFG)" == "patterns - Win32 Release"

USERDEP__EYES_="$(IntDir)\mkeyes.exe"	
# Begin Custom Build
IntDir=.\Release
InputPath=.\eyes.db

"eyes.c" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(IntDir)\mkeyes <eyes.db >eyes.c

# End Custom Build

!ELSEIF  "$(CFG)" == "patterns - Win32 Debug"

USERDEP__EYES_="$(IntDir)\mkeyes.exe"	
# Begin Custom Build
IntDir=.\Debug
InputPath=.\eyes.db

"eyes.c" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(IntDir)\mkeyes <eyes.db >eyes.c

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\fuseki.db

!IF  "$(CFG)" == "patterns - Win32 Release"

USERDEP__FUSEK="$(IntDir)\mkpat.exe"	
# Begin Custom Build
IntDir=.\Release
InputPath=.\fuseki.db

"fusekipat.c" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(IntDir)\mkpat -b fusekipat -i ..\patterns\fuseki.db -o fusekipat.c

# End Custom Build

!ELSEIF  "$(CFG)" == "patterns - Win32 Debug"

USERDEP__FUSEK="$(IntDir)\mkpat.exe"	
# Begin Custom Build
IntDir=.\Debug
InputPath=.\fuseki.db

"fusekipat.c" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(IntDir)\mkpat -b fusekipat -i ..\patterns\fuseki.db -o fusekipat.c

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\fuseki13.dbz

!IF  "$(CFG)" == "patterns - Win32 Release"

USERDEP__FUSEKI="$(IntDir)\uncompress_fuseki.exe"	
# Begin Custom Build
IntDir=.\Release
InputPath=.\fuseki13.dbz

"fuseki13.c" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(IntDir)\uncompress_fuseki 13 ..\patterns\fuseki13.dbz c >fuseki13.c

# End Custom Build

!ELSEIF  "$(CFG)" == "patterns - Win32 Debug"

USERDEP__FUSEKI="$(IntDir)\uncompress_fuseki.exe"	
# Begin Custom Build
IntDir=.\Debug
InputPath=.\fuseki13.dbz

"fuseki13.c" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(IntDir)\uncompress_fuseki 13 ..\patterns\fuseki13.dbz c >fuseki13.c

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\fuseki19.dbz

!IF  "$(CFG)" == "patterns - Win32 Release"

USERDEP__FUSEKI1="$(IntDir)\uncompress_fuseki.exe"	
# Begin Custom Build
IntDir=.\Release
InputPath=.\fuseki19.dbz

"fuseki19.c" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(IntDir)\uncompress_fuseki 19 ..\patterns\fuseki19.dbz c >fuseki19.c

# End Custom Build

!ELSEIF  "$(CFG)" == "patterns - Win32 Debug"

USERDEP__FUSEKI1="$(IntDir)\uncompress_fuseki.exe"	
# Begin Custom Build
IntDir=.\Debug
InputPath=.\fuseki19.dbz

"fuseki19.c" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(IntDir)\uncompress_fuseki 19 ..\patterns\fuseki19.dbz c >fuseki19.c

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\fuseki9.dbz

!IF  "$(CFG)" == "patterns - Win32 Release"

USERDEP__FUSEKI9="$(IntDir)\uncompress_fuseki.exe"	
# Begin Custom Build
IntDir=.\Release
InputPath=.\fuseki9.dbz

"fuseki9.c" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(IntDir)\uncompress_fuseki 9 ..\patterns\fuseki9.dbz c >fuseki9.c

# End Custom Build

!ELSEIF  "$(CFG)" == "patterns - Win32 Debug"

USERDEP__FUSEKI9="$(IntDir)\uncompress_fuseki.exe"	
# Begin Custom Build
IntDir=.\Debug
InputPath=.\fuseki9.dbz

"fuseki9.c" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(IntDir)\uncompress_fuseki 9 ..\patterns\fuseki9.dbz c >fuseki9.c

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\handicap.db

!IF  "$(CFG)" == "patterns - Win32 Release"

USERDEP__HANDI="$(IntDir)\mkpat.exe"	
# Begin Custom Build
IntDir=.\Release
InputPath=.\handicap.db

"handipat.c" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(IntDir)\mkpat -b handipat -i ..\patterns\handicap.db -o handipat.c

# End Custom Build

!ELSEIF  "$(CFG)" == "patterns - Win32 Debug"

USERDEP__HANDI="$(IntDir)\mkpat.exe"	
# Begin Custom Build
IntDir=.\Debug
InputPath=.\handicap.db

"handipat.c" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(IntDir)\mkpat -b handipat -i ..\patterns\handicap.db -o handipat.c

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\hoshi_keima.db

!IF  "$(CFG)" == "patterns - Win32 Release"

USERDEP__HOSHI="$(IntDir)\mkpat.exe"	"hoshi_other.db"	"komoku.db"	"sansan.db"	"mokuhazushi.db"	"takamoku.db"	"gogo.db"	
# Begin Custom Build
IntDir=.\Release
InputPath=.\hoshi_keima.db

"josekidb.c" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(IntDir)\mkpat -C joseki -i hoshi_keima.db -i hoshi_other.db -i komoku.db -i sansan.db -i mokuhazushi.db -i takamoku.db -i gogo.db -o josekidb.c

# End Custom Build

!ELSEIF  "$(CFG)" == "patterns - Win32 Debug"

USERDEP__HOSHI="$(IntDir)\mkpat.exe"	"hoshi_other.db"	"komoku.db"	"sansan.db"	"mokuhazushi.db"	"takamoku.db"	"gogo.db"	
# Begin Custom Build
IntDir=.\Debug
InputPath=.\hoshi_keima.db

"josekidb.c" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(IntDir)\mkpat -C joseki -i hoshi_keima.db -i hoshi_other.db -i komoku.db -i sansan.db -i mokuhazushi.db -i takamoku.db -i gogo.db -o josekidb.c

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\influence.db

!IF  "$(CFG)" == "patterns - Win32 Release"

USERDEP__INFLU="$(IntDir)\mkpat.exe"	
# Begin Custom Build
IntDir=.\Release
InputPath=.\influence.db

"influence.c" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(IntDir)\mkpat -c influencepat -i ..\patterns\influence.db -o influence.c

# End Custom Build

!ELSEIF  "$(CFG)" == "patterns - Win32 Debug"

USERDEP__INFLU="$(IntDir)\mkpat.exe"	
# Begin Custom Build
IntDir=.\Debug
InputPath=.\influence.db

"influence.c" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(IntDir)\mkpat -c influencepat -i ..\patterns\influence.db -o influence.c

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\mc_uniform.db

!IF  "$(CFG)" == "patterns - Win32 Release"

# Begin Custom Build
IntDir=.\Release
InputPath=.\mc_uniform.db

"mcpat.c" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(IntDir)\mkmcpat mc_mogo_classic.db mc_montegnu_classic.db mc_uniform.db >mcpat.c

# End Custom Build

!ELSEIF  "$(CFG)" == "patterns - Win32 Debug"

# Begin Custom Build
IntDir=.\Debug
InputPath=.\mc_uniform.db

"mcpat.c" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(IntDir)\mkmcpat mc_mogo_classic.db mc_montegnu_classic.db mc_uniform.db >mcpat.c

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\owl_attackpats.db

!IF  "$(CFG)" == "patterns - Win32 Release"

USERDEP__OWL_A="$(IntDir)\mkpat.exe"	
# Begin Custom Build
IntDir=.\Release
InputPath=.\owl_attackpats.db

"owl_attackpat.c" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(IntDir)\mkpat -D -m -b -t ..\patterns\owl_attackpats.dtr owl_attackpat -i ..\patterns\owl_attackpats.db -o owl_attackpat.c

# End Custom Build

!ELSEIF  "$(CFG)" == "patterns - Win32 Debug"

USERDEP__OWL_A="$(IntDir)\mkpat.exe"	
# Begin Custom Build
IntDir=.\Debug
InputPath=.\owl_attackpats.db

"owl_attackpat.c" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(IntDir)\mkpat -D -m -b -t ..\patterns\owl_attackpats.dtr owl_attackpat -i ..\patterns\owl_attackpats.db -o owl_attackpat.c

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\owl_defendpats.db

!IF  "$(CFG)" == "patterns - Win32 Release"

USERDEP__OWL_D="$(IntDir)\mkpat.exe"	
# Begin Custom Build
IntDir=.\Release
InputPath=.\owl_defendpats.db

"owl_defendpat.c" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(IntDir)\mkpat -D -m -b -t ..\patterns\owl_defendpats.dtr owl_defendpat -i ..\patterns\owl_defendpats.db -o owl_defendpat.c

# End Custom Build

!ELSEIF  "$(CFG)" == "patterns - Win32 Debug"

USERDEP__OWL_D="$(IntDir)\mkpat.exe"	
# Begin Custom Build
IntDir=.\Debug
InputPath=.\owl_defendpats.db

"owl_defendpat.c" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(IntDir)\mkpat -D -m -b -t ..\patterns\owl_defendpats.dtr owl_defendpat -i ..\patterns\owl_defendpats.db -o owl_defendpat.c

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\owl_vital_apats.db

!IF  "$(CFG)" == "patterns - Win32 Release"

USERDEP__OWL_V="$(IntDir)\mkpat.exe"	
# Begin Custom Build
IntDir=.\Release
InputPath=.\owl_vital_apats.db

"owl_vital_apat.c" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(IntDir)\mkpat -D -m -b -t ..\patterns\owl_vital_apats.dtr owl_vital_apat -i ..\patterns\owl_vital_apats.db -o owl_vital_apat.c

# End Custom Build

!ELSEIF  "$(CFG)" == "patterns - Win32 Debug"

USERDEP__OWL_V="$(IntDir)\mkpat.exe"	
# Begin Custom Build
IntDir=.\Debug
InputPath=.\owl_vital_apats.db

"owl_vital_apat.c" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(IntDir)\mkpat -D -m -b -t ..\patterns\owl_vital_apats.dtr owl_vital_apat -i ..\patterns\owl_vital_apats.db -o owl_vital_apat.c

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\patterns.db

!IF  "$(CFG)" == "patterns - Win32 Release"

USERDEP__PATTE="$(IntDir)\mkpat.exe"	"patterns2.db"	
# Begin Custom Build
IntDir=.\Release
InputPath=.\patterns.db

"patterns.c" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(IntDir)\mkpat -b pat -i patterns.db -i patterns2.db -o patterns.c

# End Custom Build

!ELSEIF  "$(CFG)" == "patterns - Win32 Debug"

USERDEP__PATTE="$(IntDir)\mkpat.exe"	"patterns2.db"	
# Begin Custom Build
IntDir=.\Debug
InputPath=.\patterns.db

"patterns.c" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(IntDir)\mkpat -b pat -i patterns.db -i patterns2.db -o patterns.c

# End Custom Build

!ENDIF 

# End Source File
# End Group
# Begin Group "sgf files"

# PROP Default_Filter "sgf"
# Begin Source File

SOURCE=.\gogo.sgf

!IF  "$(CFG)" == "patterns - Win32 Release"

USERDEP__GOGO_="$(IntDir)\joseki.exe"	
# Begin Custom Build
IntDir=.\Release
InputPath=.\gogo.sgf

"gogo.db" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(IntDir)\joseki JG gogo.sgf >gogo.db

# End Custom Build

!ELSEIF  "$(CFG)" == "patterns - Win32 Debug"

USERDEP__GOGO_="$(IntDir)\joseki.exe"	
# Begin Custom Build
IntDir=.\Debug
InputPath=.\gogo.sgf

"gogo.db" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(IntDir)\joseki JG gogo.sgf >gogo.db

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\hoshi_keima.sgf

!IF  "$(CFG)" == "patterns - Win32 Release"

# PROP Ignore_Default_Tool 1
USERDEP__HOSHI_="$(IntDir)\joseki.exe"	
# Begin Custom Build
IntDir=.\Release
InputPath=.\hoshi_keima.sgf

"hoshi_keima.db" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(IntDir)\joseki JHK hoshi_keima.sgf >hoshi_keima.db

# End Custom Build

!ELSEIF  "$(CFG)" == "patterns - Win32 Debug"

# PROP Ignore_Default_Tool 1
USERDEP__HOSHI_="$(IntDir)\joseki.exe"	
# Begin Custom Build
IntDir=.\Debug
InputPath=.\hoshi_keima.sgf

"hoshi_keima.db" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(IntDir)\joseki JHK hoshi_keima.sgf >hoshi_keima.db

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\hoshi_other.sgf

!IF  "$(CFG)" == "patterns - Win32 Release"

# PROP Ignore_Default_Tool 1
USERDEP__HOSHI_O="$(IntDir)\joseki.exe"	
# Begin Custom Build
IntDir=.\Release
InputPath=.\hoshi_other.sgf

"hoshi_other.db" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(IntDir)\joseki JHO hoshi_other.sgf >hoshi_other.db

# End Custom Build

!ELSEIF  "$(CFG)" == "patterns - Win32 Debug"

# PROP Ignore_Default_Tool 1
USERDEP__HOSHI_O="$(IntDir)\joseki.exe"	
# Begin Custom Build
IntDir=.\Debug
InputPath=.\hoshi_other.sgf

"hoshi_other.db" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(IntDir)\joseki JHO hoshi_other.sgf >hoshi_other.db

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\komoku.sgf

!IF  "$(CFG)" == "patterns - Win32 Release"

USERDEP__KOMOK="$(IntDir)\joseki.exe"	
# Begin Custom Build
IntDir=.\Release
InputPath=.\komoku.sgf

"komoku.db" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(IntDir)\joseki JK komoku.sgf >komoku.db

# End Custom Build

!ELSEIF  "$(CFG)" == "patterns - Win32 Debug"

USERDEP__KOMOK="$(IntDir)\joseki.exe"	
# Begin Custom Build
IntDir=.\Debug
InputPath=.\komoku.sgf

"komoku.db" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(IntDir)\joseki JK komoku.sgf >komoku.db

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\mokuhazushi.sgf

!IF  "$(CFG)" == "patterns - Win32 Release"

USERDEP__MOKUH="$(IntDir)\joseki.exe"	
# Begin Custom Build
IntDir=.\Release
InputPath=.\mokuhazushi.sgf

"mokuhazushi.db" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(IntDir)\joseki JM mokuhazushi.sgf >mokuhazushi.db

# End Custom Build

!ELSEIF  "$(CFG)" == "patterns - Win32 Debug"

USERDEP__MOKUH="$(IntDir)\joseki.exe"	
# Begin Custom Build
IntDir=.\Debug
InputPath=.\mokuhazushi.sgf

"mokuhazushi.db" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(IntDir)\joseki JM mokuhazushi.sgf >mokuhazushi.db

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\sansan.sgf

!IF  "$(CFG)" == "patterns - Win32 Release"

USERDEP__SANSA="$(IntDir)\joseki.exe"	
# Begin Custom Build
IntDir=.\Release
InputPath=.\sansan.sgf

"sansan.db" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(IntDir)\joseki JS sansan.sgf > sansan.db

# End Custom Build

!ELSEIF  "$(CFG)" == "patterns - Win32 Debug"

USERDEP__SANSA="$(IntDir)\joseki.exe"	
# Begin Custom Build
IntDir=.\Debug
InputPath=.\sansan.sgf

"sansan.db" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(IntDir)\joseki JS sansan.sgf > sansan.db

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\takamoku.sgf

!IF  "$(CFG)" == "patterns - Win32 Release"

USERDEP__TAKAM="$(IntDir)\joseki.exe"	
# Begin Custom Build
IntDir=.\Release
InputPath=.\takamoku.sgf

"takamoku.db" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(IntDir)\joseki  JT takamoku.sgf > takamoku.db

# End Custom Build

!ELSEIF  "$(CFG)" == "patterns - Win32 Debug"

USERDEP__TAKAM="$(IntDir)\joseki.exe"	
# Begin Custom Build
IntDir=.\Debug
InputPath=.\takamoku.sgf

"takamoku.db" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(IntDir)\joseki  JT takamoku.sgf > takamoku.db

# End Custom Build

!ENDIF 

# End Source File
# End Group
# End Target
# End Project
