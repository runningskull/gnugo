# Microsoft Developer Studio Project File - Name="sgf" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Static Library" 0x0104

CFG=sgf - Win32 Release
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "sgf.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "sgf.mak" CFG="sgf - Win32 Release"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "sgf - Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "sgf - Win32 Debug" (based on "Win32 (x86) Static Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "sgf - Win32 Release"

# PROP Use_MFC 0
# ADD CPP /GX /Zi /I ".." /I "..\interface" /I "..\engine" /I "..\utils" /D "NDEBUG" /D "WIN32" /D "_MBCS" /D "_LIB" /D "_CONSOLE" /D "HAVE_CONFIG_H" /Fo"Release/" /Fd"Release/sgf" /FD /c
# ADD BASE RSC /l 0x809
# ADD RSC /l 0x809
BSC32=bscmake.exe
LIB32=link.exe -lib
# ADD LIB32 /nologo /out:"Release\sgf.lib"

!ELSEIF  "$(CFG)" == "sgf - Win32 Debug"

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
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "NDEBUG" /D "_MBCS" /D "_LIB" /D "_DEBUG" /YX /FD /GZ /c
# ADD CPP /W2 /Gm /GX /ZI /Od /I ".." /I "..\engine" /I "..\interface" /I "../utils" /D "WIN32" /D "_MBCS" /D "_LIB" /D "_DEBUG" /D "_CONSOLE" /D "HAVE_CONFIG_H" /FR /Fd"Debug/sgf" /FD /GZ /c
# ADD BASE RSC /l 0x809 /d "NDEBUG" /d "_DEBUG"
# ADD RSC /l 0x809 /d "NDEBUG" /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo

!ENDIF 

# Begin Target

# Name "sgf - Win32 Release"
# Name "sgf - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Source File

SOURCE=..\sgf\sgf_utils.c

!IF  "$(CFG)" == "sgf - Win32 Release"

# ADD CPP /W3

!ELSEIF  "$(CFG)" == "sgf - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\sgfnode.c
# End Source File
# Begin Source File

SOURCE=.\sgftree.c
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# Begin Source File

SOURCE=.\sgf_properties.h
# End Source File
# Begin Source File

SOURCE=.\sgf_utils.h
# End Source File
# Begin Source File

SOURCE=.\sgftree.h
# End Source File
# End Group
# End Target
# End Project
