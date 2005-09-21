# Microsoft Developer Studio Project File - Name="uncompress_fuseki" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=uncompress_fuseki - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "uncompress_fuseki.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "uncompress_fuseki.mak" CFG="uncompress_fuseki - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "uncompress_fuseki - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "uncompress_fuseki - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "uncompress_fuseki - Win32 Release"

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
# ADD CPP /GX /Zi /O2 /I ".." /I "..\utils" /I "..\engine" /I "..\sgf" /D "NDEBUG" /D "WIN32" /D "_CONSOLE" /D "_MBCS" /D "HAVE_CONFIG_H" /Fd"Release/uncompress_fuseki" /FD /c
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 ..\utils\Release\utils.lib .\Release\dfa.lib ..\engine\Release\board.lib ..\sgf\Release\sgf.lib /nologo /subsystem:console /debug /machine:I386

!ELSEIF  "$(CFG)" == "uncompress_fuseki - Win32 Debug"

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
# ADD CPP /W2 /Gm /GX /ZI /Od /I ".." /I "..\utils" /I "..\engine" /I "..\sgf" /D "_DEBUG" /D "HAVE_CONFIG_H" /D "WIN32" /D "_CONSOLE" /D "_MBCS" /FR /Fd"Debug/uncompress_fuseki" /FD /GZ /c
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386 /pdbtype:sept
# ADD LINK32 ..\utils\Debug\utils.lib .\Debug\dfa.lib ..\engine\Debug\board.lib ..\sgf\Debug\sgf.lib /nologo /subsystem:console /debug /machine:I386 /pdbtype:sept

!ENDIF 

# Begin Target

# Name "uncompress_fuseki - Win32 Release"
# Name "uncompress_fuseki - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Source File

SOURCE=.\uncompress_fuseki.c
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# End Group
# End Target
# End Project
--X1bOJ3K7DJ5YkBrT
Content-Type: text/plain; charset=us-ascii
Content-Disposition: attachment; filename="gnugo.dsw"

Microsoft Developer Studio Workspace File, Format Version 6.00
# WARNING: DO NOT EDIT OR DELETE THIS WORKSPACE FILE!

###############################################################################

Project: "board"=.\engine\board.dsp - Package Owner=<4>

Package=<5>
{{{
}}}

Package=<4>
{{{
    Begin Project Dependency
    Project_Dep_Name utils
    End Project Dependency
}}}

###############################################################################

Project: "dfa"=.\patterns\dfa.dsp - Package Owner=<4>

Package=<5>
{{{
}}}

Package=<4>
{{{
}}}

###############################################################################

Project: "engine"=.\engine\engine.dsp - Package Owner=<4>

Package=<5>
{{{
}}}

Package=<4>
{{{
    Begin Project Dependency
    Project_Dep_Name sgf
    End Project Dependency
}}}

###############################################################################

Project: "fuseki"=.\patterns\fuseki.dsp - Package Owner=<4>

Package=<5>
{{{
}}}

Package=<4>
{{{
    Begin Project Dependency
    Project_Dep_Name sgf
    End Project Dependency
    Begin Project Dependency
    Project_Dep_Name sgfgen
    End Project Dependency
    Begin Project Dependency
    Project_Dep_Name engine
    End Project Dependency
    Begin Project Dependency
    Project_Dep_Name patterns
    End Project Dependency
    Begin Project Dependency
    Project_Dep_Name dfa
    End Project Dependency
}}}

###############################################################################

Project: "gnugo"=.\interface\gnugo.dsp - Package Owner=<4>

Package=<5>
{{{
}}}

Package=<4>
{{{
    Begin Project Dependency
    Project_Dep_Name board
    End Project Dependency
    Begin Project Dependency
    Project_Dep_Name engine
    End Project Dependency
    Begin Project Dependency
    Project_Dep_Name joseki
    End Project Dependency
    Begin Project Dependency
    Project_Dep_Name mkeyes
    End Project Dependency
    Begin Project Dependency
    Project_Dep_Name mkpat
    End Project Dependency
    Begin Project Dependency
    Project_Dep_Name sgf
    End Project Dependency
    Begin Project Dependency
    Project_Dep_Name sgfgen
    End Project Dependency
    Begin Project Dependency
    Project_Dep_Name utils
    End Project Dependency
    Begin Project Dependency
    Project_Dep_Name fuseki
    End Project Dependency
    Begin Project Dependency
    Project_Dep_Name patterns
    End Project Dependency
    Begin Project Dependency
    Project_Dep_Name dfa
    End Project Dependency
}}}

###############################################################################

Project: "joseki"=.\patterns\joseki.dsp - Package Owner=<4>

Package=<5>
{{{
}}}

Package=<4>
{{{
    Begin Project Dependency
    Project_Dep_Name sgf
    End Project Dependency
    Begin Project Dependency
    Project_Dep_Name board
    End Project Dependency
    Begin Project Dependency
    Project_Dep_Name utils
    End Project Dependency
}}}

###############################################################################

Project: "mkeyes"=.\patterns\mkeyes.dsp - Package Owner=<4>

Package=<5>
{{{
}}}

Package=<4>
{{{
    Begin Project Dependency
    Project_Dep_Name utils
    End Project Dependency
}}}

###############################################################################

Project: "mkpat"=.\patterns\mkpat.dsp - Package Owner=<4>

Package=<5>
{{{
}}}

Package=<4>
{{{
    Begin Project Dependency
    Project_Dep_Name utils
    End Project Dependency
    Begin Project Dependency
    Project_Dep_Name dfa
    End Project Dependency
}}}

###############################################################################

Project: "patterns"=.\patterns\patterns.dsp - Package Owner=<4>

Package=<5>
{{{
}}}

Package=<4>
{{{
    Begin Project Dependency
    Project_Dep_Name joseki
    End Project Dependency
    Begin Project Dependency
    Project_Dep_Name mkeyes
    End Project Dependency
    Begin Project Dependency
    Project_Dep_Name mkpat
    End Project Dependency
    Begin Project Dependency
    Project_Dep_Name uncompress_fuseki
    End Project Dependency
}}}

###############################################################################

Project: "sgf"=.\sgf\sgf.dsp - Package Owner=<4>

Package=<5>
{{{
}}}

Package=<4>
{{{
    Begin Project Dependency
    Project_Dep_Name sgfgen
    End Project Dependency
    Begin Project Dependency
    Project_Dep_Name utils
    End Project Dependency
}}}

###############################################################################

Project: "sgfgen"=.\sgf\sgfgen.dsp - Package Owner=<4>

Package=<5>
{{{
}}}

Package=<4>
{{{
    Begin Project Dependency
    Project_Dep_Name utils
    End Project Dependency
}}}

###############################################################################

Project: "uncompress_fuseki"=.\patterns\uncompress_fuseki.dsp - Package Owner=<4>

Package=<5>
{{{
}}}

Package=<4>
{{{
    Begin Project Dependency
    Project_Dep_Name utils
    End Project Dependency
    Begin Project Dependency
    Project_Dep_Name board
    End Project Dependency
    Begin Project Dependency
    Project_Dep_Name sgf
    End Project Dependency
}}}

###############################################################################

Project: "utils"=.\utils\utils.dsp - Package Owner=<4>

Package=<5>
{{{
}}}

Package=<4>
{{{
}}}

###############################################################################

Global:

Package=<5>
{{{
}}}

Package=<3>
{{{
}}}

###############################################################################

