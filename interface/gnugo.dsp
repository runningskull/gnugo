# Microsoft Developer Studio Project File - Name="gnugo" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=gnugo - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "gnugo.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "gnugo.mak" CFG="gnugo - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "gnugo - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "gnugo - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "gnugo - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD CPP /GX /Zi /O2 /I "." /I ".." /I "..\sgf" /I "..\patterns" /I "..\utils" /I "../engine" /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /D "HAVE_CONFIG_H" /Fr"Release/gnugo/" /Fd"Release/gnugo" /FD /c
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 ..\sgf\Release\sgf.lib ..\engine\Release\engine.lib ..\patterns\Release\patterns.lib ..\utils\Release\utils.lib wsock32.lib ..\patterns\Release\dfa.lib /nologo /subsystem:console /profile /debug /machine:I386 /out:"gnugo.exe"
# SUBTRACT LINK32 /nodefaultlib

!ELSEIF  "$(CFG)" == "gnugo - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /GZ /c
# ADD CPP /W2 /Gm /GX /Zi /Od /I "." /I ".." /I "..\sgf" /I "..\patterns" /I "..\utils" /I "../engine" /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /D "HAVE_CONFIG_H" /FR /Fd"Debug/gnugo" /FD /GZ /c
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386 /pdbtype:sept
# ADD LINK32 ..\patterns\Debug\patterns.lib ..\utils\Debug\utils.lib ..\sgf\Debug\sgf.lib ..\engine\Debug\engine.lib wsock32.lib ..\patterns\Debug\dfa.lib /nologo /subsystem:console /profile /debug /machine:I386 /out:"gnugo.exe"
# SUBTRACT LINK32 /nodefaultlib

!ENDIF 

# Begin Target

# Name "gnugo - Win32 Release"
# Name "gnugo - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Source File

SOURCE=..\interface\gmp.c
# End Source File
# Begin Source File

SOURCE=..\interface\gtp.c
# End Source File
# Begin Source File

SOURCE=..\interface\main.c

!IF  "$(CFG)" == "gnugo - Win32 Release"

# ADD CPP /YX"gnugo.h"

!ELSEIF  "$(CFG)" == "gnugo - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\interface\play_ascii.c

!IF  "$(CFG)" == "gnugo - Win32 Release"

# ADD CPP /YX"gnugo.h"

!ELSEIF  "$(CFG)" == "gnugo - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\interface\play_gmp.c

!IF  "$(CFG)" == "gnugo - Win32 Release"

# ADD CPP /YX"gnugo.h"

!ELSEIF  "$(CFG)" == "gnugo - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\interface\play_gtp.c

!IF  "$(CFG)" == "gnugo - Win32 Release"

# ADD CPP /YX"gnugo.h"

!ELSEIF  "$(CFG)" == "gnugo - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\interface\play_solo.c

!IF  "$(CFG)" == "gnugo - Win32 Release"

# ADD CPP /YX"gnugo.h"

!ELSEIF  "$(CFG)" == "gnugo - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\interface\play_test.c

!IF  "$(CFG)" == "gnugo - Win32 Release"

# ADD CPP /YX"gnugo.h"

!ELSEIF  "$(CFG)" == "gnugo - Win32 Debug"

!ENDIF 

# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;rgs;gif;jpg;jpeg;jpe"
# End Group
# End Target
# End Project
