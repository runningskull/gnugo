# Microsoft Developer Studio Project File - Name="engine" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Static Library" 0x0104

CFG=engine - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "engine.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "engine.mak" CFG="engine - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "engine - Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "engine - Win32 Debug" (based on "Win32 (x86) Static Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "engine - Win32 Release"

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
# ADD CPP /GX /Zi /O2 /I "." /I ".." /I "..\sgf" /I "..\interface" /I "..\patterns" /I "..\utils" /D "WIN32" /D "NDEBUG" /D "_MBCS" /D "_LIB" /D "HAVE_CONFIG_H" /YX"gnugo.h" /Fd"Release/engine" /FD /c
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo

!ELSEIF  "$(CFG)" == "engine - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "engine___Win32_Debug"
# PROP BASE Intermediate_Dir "engine___Win32_Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_MBCS" /D "_LIB" /YX /FD /GZ /c
# ADD CPP /W2 /Gm /GX /ZI /Od /I "." /I ".." /I "..\sgf" /I "..\interface" /I "..\patterns" /I "..\utils" /D "WIN32" /D "_DEBUG" /D "_MBCS" /D "_LIB" /D "HAVE_CONFIG_H" /FR /YX"gnugo.h" /Fd"Debug/engine" /FD /GZ /c
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

# Name "engine - Win32 Release"
# Name "engine - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Source File

SOURCE=.\aftermath.c

!IF  "$(CFG)" == "engine - Win32 Release"

!ELSEIF  "$(CFG)" == "engine - Win32 Debug"

# ADD CPP /YX"gnugo.h"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\board.c

!IF  "$(CFG)" == "engine - Win32 Release"

!ELSEIF  "$(CFG)" == "engine - Win32 Debug"

# ADD CPP /YX"gnugo.h"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\boardlib.c
# End Source File
# Begin Source File

SOURCE=.\breakin.c

!IF  "$(CFG)" == "engine - Win32 Release"

!ELSEIF  "$(CFG)" == "engine - Win32 Debug"

# ADD CPP /YX"gnugo.h"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\cache.c

!IF  "$(CFG)" == "engine - Win32 Release"

!ELSEIF  "$(CFG)" == "engine - Win32 Debug"

# ADD CPP /YX"gnugo.h"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\clock.c

!IF  "$(CFG)" == "engine - Win32 Release"

!ELSEIF  "$(CFG)" == "engine - Win32 Debug"

# ADD CPP /YX"gnugo.h"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\combination.c

!IF  "$(CFG)" == "engine - Win32 Release"

!ELSEIF  "$(CFG)" == "engine - Win32 Debug"

# ADD CPP /YX"gnugo.h"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\dragon.c

!IF  "$(CFG)" == "engine - Win32 Release"

!ELSEIF  "$(CFG)" == "engine - Win32 Debug"

# ADD CPP /YX"gnugo.h"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\endgame.c

!IF  "$(CFG)" == "engine - Win32 Release"

!ELSEIF  "$(CFG)" == "engine - Win32 Debug"

# ADD CPP /YX"gnugo.h"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\filllib.c

!IF  "$(CFG)" == "engine - Win32 Release"

!ELSEIF  "$(CFG)" == "engine - Win32 Debug"

# ADD CPP /YX"gnugo.h"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\fuseki.c

!IF  "$(CFG)" == "engine - Win32 Release"

!ELSEIF  "$(CFG)" == "engine - Win32 Debug"

# ADD CPP /YX"gnugo.h"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\genmove.c

!IF  "$(CFG)" == "engine - Win32 Release"

!ELSEIF  "$(CFG)" == "engine - Win32 Debug"

# ADD CPP /YX"gnugo.h"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\globals.c

!IF  "$(CFG)" == "engine - Win32 Release"

!ELSEIF  "$(CFG)" == "engine - Win32 Debug"

# ADD CPP /YX"gnugo.h"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\handicap.c
# End Source File
# Begin Source File

SOURCE=.\hash.c

!IF  "$(CFG)" == "engine - Win32 Release"

!ELSEIF  "$(CFG)" == "engine - Win32 Debug"

# ADD CPP /YX"gnugo.h"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\influence.c

!IF  "$(CFG)" == "engine - Win32 Release"

!ELSEIF  "$(CFG)" == "engine - Win32 Debug"

# ADD CPP /YX"gnugo.h"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\interface.c

!IF  "$(CFG)" == "engine - Win32 Release"

!ELSEIF  "$(CFG)" == "engine - Win32 Debug"

# ADD CPP /YX"gnugo.h"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\matchpat.c

!IF  "$(CFG)" == "engine - Win32 Release"

!ELSEIF  "$(CFG)" == "engine - Win32 Debug"

# ADD CPP /YX"gnugo.h"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\montecarlo.c
# End Source File
# Begin Source File

SOURCE=.\move_reasons.c

!IF  "$(CFG)" == "engine - Win32 Release"

!ELSEIF  "$(CFG)" == "engine - Win32 Debug"

# ADD CPP /YX"gnugo.h"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\movelist.c

!IF  "$(CFG)" == "engine - Win32 Release"

!ELSEIF  "$(CFG)" == "engine - Win32 Debug"

# ADD CPP /YX"gnugo.h"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\optics.c

!IF  "$(CFG)" == "engine - Win32 Release"

!ELSEIF  "$(CFG)" == "engine - Win32 Debug"

# ADD CPP /YX"gnugo.h"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\owl.c

!IF  "$(CFG)" == "engine - Win32 Release"

!ELSEIF  "$(CFG)" == "engine - Win32 Debug"

# ADD CPP /YX"gnugo.h"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\persistent.c

!IF  "$(CFG)" == "engine - Win32 Release"

!ELSEIF  "$(CFG)" == "engine - Win32 Debug"

# ADD CPP /YX"gnugo.h"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\printutils.c

!IF  "$(CFG)" == "engine - Win32 Release"

!ELSEIF  "$(CFG)" == "engine - Win32 Debug"

# ADD CPP /YX"gnugo.h"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\readconnect.c

!IF  "$(CFG)" == "engine - Win32 Release"

!ELSEIF  "$(CFG)" == "engine - Win32 Debug"

# ADD CPP /YX"gnugo.h"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\reading.c

!IF  "$(CFG)" == "engine - Win32 Release"

!ELSEIF  "$(CFG)" == "engine - Win32 Debug"

# ADD CPP /YX"gnugo.h"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\semeai.c

!IF  "$(CFG)" == "engine - Win32 Release"

!ELSEIF  "$(CFG)" == "engine - Win32 Debug"

# ADD CPP /YX"gnugo.h"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\sgfdecide.c

!IF  "$(CFG)" == "engine - Win32 Release"

!ELSEIF  "$(CFG)" == "engine - Win32 Debug"

# ADD CPP /YX"gnugo.h"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\sgffile.c

!IF  "$(CFG)" == "engine - Win32 Release"

!ELSEIF  "$(CFG)" == "engine - Win32 Debug"

# ADD CPP /YX"gnugo.h"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\shapes.c

!IF  "$(CFG)" == "engine - Win32 Release"

!ELSEIF  "$(CFG)" == "engine - Win32 Debug"

# ADD CPP /YX"gnugo.h"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\showbord.c

!IF  "$(CFG)" == "engine - Win32 Release"

!ELSEIF  "$(CFG)" == "engine - Win32 Debug"

# ADD CPP /YX"gnugo.h"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\surround.c
# End Source File
# Begin Source File

SOURCE=.\unconditional.c

!IF  "$(CFG)" == "engine - Win32 Release"

!ELSEIF  "$(CFG)" == "engine - Win32 Debug"

# ADD CPP /YX"gnugo.h"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\utils.c

!IF  "$(CFG)" == "engine - Win32 Release"

!ELSEIF  "$(CFG)" == "engine - Win32 Debug"

# ADD CPP /YX"gnugo.h"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\value_moves.c

!IF  "$(CFG)" == "engine - Win32 Release"

!ELSEIF  "$(CFG)" == "engine - Win32 Debug"

# ADD CPP /YX"gnugo.h"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\worm.c

!IF  "$(CFG)" == "engine - Win32 Release"

!ELSEIF  "$(CFG)" == "engine - Win32 Debug"

# ADD CPP /YX"gnugo.h"

!ENDIF 

# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter ""
# Begin Source File

SOURCE=.\board.h
# End Source File
# Begin Source File

SOURCE=.\cache.h
# End Source File
# Begin Source File

SOURCE=.\clock.h
# End Source File
# Begin Source File

SOURCE=.\gnugo.h
# End Source File
# Begin Source File

SOURCE=.\hash.h
# End Source File
# Begin Source File

SOURCE=.\influence.h
# End Source File
# Begin Source File

SOURCE=.\liberty.h
# End Source File
# Begin Source File

SOURCE=.\move_reasons.h
# End Source File
# End Group
# End Target
# End Project
