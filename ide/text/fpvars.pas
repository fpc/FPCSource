{
    $Id$
    This file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 1998 by Berczi Gabor

    Global variables for the IDE

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit FPVars;

interface

uses Objects,Views,App,
     WUtils,
     FPConst,FPDebug,FPUtils,FPViews,FPCalc;

type
    TRecentFileEntry = record
      FileName  : string{$ifdef GABOR}[20]{$endif};
      LastPos   : TPoint;
    end;

    TCompPhase = (cpNothing,cpCompiling,cpLinking,
                  cpAborted,cpFailed,cpDone);

const ClipboardWindow  : PClipboardWindow = nil;
      CalcWindow       : PCalculator = nil;
      RecentFileCount  : integer = 0;
      OpenExts         : string[80] = '*.pas;*.pp;*.inc';
      HighlightExts    : string[80] = '*.pas;*.pp;*.inc';
      TabsPattern      : string{$ifdef GABOR}[30]{$endif} = 'make*;make*.*';
      SourceDirs       : string{$ifdef GABOR}[30]{$endif} = '';
      PrimaryFile      : string{$ifdef GABOR}[80]{$endif} = '';
      PrimaryFileMain  : string{$ifdef GABOR}[80]{$endif} = '';
      PrimaryFileSwitches : string{$ifdef GABOR}[30]{$endif} = '';
      PrimaryFilePara  : string = '';
      GDBOutputFile    : string{$ifdef GABOR}[30]{$endif} = 'gdb$$$.txt';
      IsEXECompiled    : boolean = false;
      LinkAfter        : boolean = true;
      MainFile         : string{$ifdef GABOR}[80]{$endif} = '';
      EXEFile          : string{$ifdef GABOR}[80]{$endif} = '';
      CompilationPhase : TCompPhase = cpNothing;
      ProgramInfoWindow: PProgramInfoWindow = nil;
      GDBWindow        : PGDBWindow = nil;
      BreakpointsWindow : PBreakpointsWindow = nil;
      WatchesWindow    : PWatchesWindow = nil;
      UserScreenWindow : PScreenWindow = nil;
      HeapView         : PFPHeapView = nil;
      HelpFiles        : WUtils.PUnsortedStringCollection = nil;
      ShowStatusOnError: boolean = true;
      StartupDir       : string = '.'+DirSep;
      IDEDir           : string = '.'+DirSep;
      INIPath          : string = ININame;
      SwitchesPath     : string = SwitchesName;
      CtrlMouseAction  : integer = acTopicSearch;
      AltMouseAction   : integer = acBrowseSymbol;
      StartupOptions   : longint = 0;
      LastExitCode     : integer = 0;
      ASCIIChart       : PFPASCIIChart = nil;
      DesktopPath      : string = DesktopName;
      DesktopFileFlags : longint = dfHistoryLists+dfOpenWindows;
      DesktopLocation  : byte    = dlConfigFileDir;
      AutoSaveOptions  : longint = asEnvironment+asDesktop;
      MiscOptions      : longint = moChangeDirOnOpen+moCloseOnGotoSource;

      ActionCommands   : array[acFirstAction..acLastAction] of word =
        (cmHelpTopicSearch,cmGotoCursor,cmToggleBreakpoint,
         cmEvaluate,cmAddWatch,cmBrowseAtCursor);

      AppPalette       : string = CIDEAppColor;

var   RecentFiles      : array[1..MaxRecentFileCount] of TRecentFileEntry;

implementation

END.
{
  $Log$
  Revision 1.19  1999-07-10 01:24:21  pierre
   + First implementation of watches window

  Revision 1.18  1999/06/30 23:58:19  pierre
    + BreakpointsList Window implemented
      with Edit/New/Delete functions
    + Individual breakpoint dialog with support for all types
      ignorecount and conditions
      (commands are not yet implemented, don't know if this wolud be useful)
      awatch and rwatch have problems because GDB does not annotate them
      I fixed v4.16 for this

  Revision 1.17  1999/06/28 19:32:27  peter
    * fixes from gabor

  Revision 1.16  1999/06/21 23:37:58  pierre
   + added LinkAfter var for post linking with -s option

  Revision 1.15  1999/03/23 15:11:36  peter
    * desktop saving things
    * vesa mode
    * preferences dialog

  Revision 1.14  1999/03/19 16:04:32  peter
    * new compiler dialog

  Revision 1.13  1999/03/16 12:38:15  peter
    * tools macro fixes
    + tph writer
    + first things for resource files

  Revision 1.12  1999/03/12 01:14:02  peter
    * flag if trytoopen should look for other extensions
    + browser tab in the tools-compiler

  Revision 1.11  1999/03/08 14:58:15  peter
    + prompt with dialogs for tools

  Revision 1.10  1999/03/01 15:42:07  peter
    + Added dummy entries for functions not yet implemented
    * MenuBar didn't update itself automatically on command-set changes
    * Fixed Debugging/Profiling options dialog
    * TCodeEditor converts spaces to tabs at save only if efUseTabChars is
 set
    * efBackSpaceUnindents works correctly
    + 'Messages' window implemented
    + Added '$CAP MSG()' and '$CAP EDIT' to available tool-macros
    + Added TP message-filter support (for ex. you can call GREP thru
      GREP2MSG and view the result in the messages window - just like in TP)
    * A 'var' was missing from the param-list of THelpFacility.TopicSearch,
      so topic search didn't work...
    * In FPHELP.PAS there were still context-variables defined as word instead
      of THelpCtx
    * StdStatusKeys() was missing from the statusdef for help windows
    + Topic-title for index-table can be specified when adding a HTML-files

  Revision 1.9  1999/02/19 18:43:48  peter
    + open dialog supports mask list

  Revision 1.8  1999/02/11 13:10:04  pierre
   + GDBWindow only with -dGDBWindow for now : still buggy !!

  Revision 1.7  1999/02/05 12:07:55  pierre
    + SourceDirs added

  Revision 1.6  1999/02/04 13:15:40  pierre
   + TabsPattern added

  Revision 1.5  1999/01/21 11:54:26  peter
    + tools menu
    + speedsearch in symbolbrowser
    * working run command

  Revision 1.4  1999/01/12 14:29:41  peter
    + Implemented still missing 'switch' entries in Options menu
    + Pressing Ctrl-B sets ASCII mode in editor, after which keypresses (even
      ones with ASCII < 32 ; entered with Alt+<###>) are interpreted always as
      ASCII chars and inserted directly in the text.
    + Added symbol browser
    * splitted fp.pas to fpide.pas

  Revision 1.3  1999/01/04 11:49:52  peter
   * 'Use tab characters' now works correctly
   + Syntax highlight now acts on File|Save As...
   + Added a new class to syntax highlight: 'hex numbers'.
   * There was something very wrong with the palette managment. Now fixed.
   + Added output directory (-FE<xxx>) support to 'Directories' dialog...
   * Fixed some possible bugs in Running/Compiling, and the compilation/run
     process revised

  Revision 1.1  1998/12/28 15:47:54  peter
    + Added user screen support, display & window
    + Implemented Editor,Mouse Options dialog
    + Added location of .INI and .CFG file
    + Option (INI) file managment implemented (see bottom of Options Menu)
    + Switches updated
    + Run program

  Revision 1.0  1998/12/23 07:34:40  gabor

}
