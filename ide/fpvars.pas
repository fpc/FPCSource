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
{$i globdir.inc}

unit FPVars;

interface

uses Objects,Views,App,
     WUtils,WEditor,
     FPConst,
     FPDebug,
     FPUtils,FPViews,FPCalc;

type
    TRecentFileEntry = record
      FileName  : string{$ifdef GABOR}[60]{$endif};
      LastPos   : TPoint;
    end;

    TCompPhase = (cpNothing,cpCompiling,cpLinking,
                  cpAborted,cpFailed,cpDone);

const ClipboardWindow  : PClipboardWindow = nil;
      CalcWindow       : PCalculator = nil;
      RecentFileCount  : integer = 0;
      LastCompileTime  : cardinal = 0;
      OpenExts         : string{$ifdef GABOR}[40]{$endif} = '*.pas;*.pp;*.inc';
      HighlightExts    : string{$ifdef GABOR}[40]{$endif} = '*.pas;*.pp;*.inc';
      TabsPattern      : string{$ifdef GABOR}[40]{$endif} = 'make*;make*.*';
      SourceDirs       : string{$ifdef GABOR}[40]{$endif} = '';
      PrimaryFile      : string{$ifdef GABOR}[60]{$endif} = '';
      PrimaryFileMain  : string{$ifdef GABOR}[60]{$endif} = '';
      PrimaryFileSwitches : string{$ifdef GABOR}[80]{$endif} = '';
      PrimaryFilePara  : string{$ifdef GABOR}[80]{$endif} = '';
      GDBOutputFile    : string{$ifdef GABOR}[50]{$endif} = GDBOutputFileName;
      IsEXECompiled    : boolean = false;
      { LinkAfter        : boolean = true; changed into a function }
      MainHasDebugInfo : boolean = false;
      UseMouse         : boolean = true;
      MainFile         : string{$ifdef GABOR}[60]{$endif} = '';
      PrevMainFile     : string{$ifdef GABOR}[60]{$endif} = '';
      EXEFile          : string{$ifdef GABOR}[60]{$endif} = '';
      CompilationPhase : TCompPhase = cpNothing;
      ProgramInfoWindow: PProgramInfoWindow = nil;
      GDBWindow        : PGDBWindow = nil;
      DisassemblyWindow : PDisassemblyWindow = nil;
      BreakpointsWindow : PBreakpointsWindow = nil;
      WatchesWindow    : PWatchesWindow = nil;
      UserScreenWindow : PScreenWindow = nil;
      StackWindow      : PStackWindow = nil;
      RegistersWindow  : PRegistersWindow = nil;
      FPUWindow        : PFPUWindow = nil;

      HeapView         : PFPHeapView = nil;
      ClockView        : PFPClockView = nil;
      HelpFiles        : WUtils.PUnsortedStringCollection = nil;
      ShowStatusOnError: boolean = true;
      StartupDir       : string{$ifdef GABOR}[60]{$endif} = '.'+DirSep;
      IDEDir           : string{$ifdef GABOR}[60]{$endif} = '.'+DirSep;
{$ifdef Unix}
      SystemIDEDir     : string{$ifdef GABOR}[60]{$endif} = '';
{$endif Unix}
      INIFileName      : string{$ifdef GABOR}[50]{$endif} = ININame;
      SwitchesPath     : string{$ifdef GABOR}[60]{$endif} = SwitchesName;
      CtrlMouseAction  : integer = acTopicSearch;
      AltMouseAction   : integer = acBrowseSymbol;
      StartupOptions   : longint = 0;
      LastExitCode     : integer = 0;
      ASCIIChart       : PFPASCIIChart = nil;
      BackgroundPath   : string{$ifdef GABOR}[60]{$endif} = BackgroundName;
      DesktopPath      : string{$ifdef GABOR}[60]{$endif} = DesktopName;
      DesktopFileFlags : longint = dfHistoryLists+dfOpenWindows+
                                   dfCodeCompleteWords+dfCodeTemplates;
      DesktopLocation  : byte    = dlConfigFileDir;
      AutoSaveOptions  : longint = asEnvironment+asDesktop;
      MiscOptions      : longint = moChangeDirOnOpen+moCloseOnGotoSource;
      EditorModified   : boolean = false;
      IniCenterDebuggerRow : boolean = true;
      SleepTimeOut     : longint = trunc(10*18.2);
{$ifdef USE_EXTERNAL_COMPILER}
      UseExternalCompiler : boolean = true;
      ExternalCompilerExe : string = 'ppc386'+ExeExt;
{$endif USE_EXTERNAL_COMPILER}
      ShowReadme       : boolean = true;
      AskRecompileIfModifiedFlag : boolean = true;


      ActionCommands   : array[acFirstAction..acLastAction] of word =
        (cmHelpTopicSearch,cmGotoCursor,cmToggleBreakpoint,
         cmEvaluate,cmAddWatch,cmBrowseAtCursor);

      AppPalette       : string = CIDEAppColor;

var   RecentFiles      : array[1..MaxRecentFileCount] of TRecentFileEntry;

implementation

END.
{
  $Log$
  Revision 1.3  2002-05-29 22:33:23  pierre
   Asciitab now in fvision

  Revision 1.2  2001/08/05 12:23:01  peter
    * Automatically support for fvision or old fv

  Revision 1.1  2001/08/04 11:30:24  peter
    * ide works now with both compiler versions

  Revision 1.1.2.5  2001/03/12 17:34:56  pierre
   + Disassembly window started

  Revision 1.1.2.4  2001/02/13 16:04:01  pierre
   * fixes for bugs 1280

  Revision 1.1.2.3  2000/12/30 22:52:27  peter
    * check modified while in debug mode. But placed it between a
      conditional again as it reports also if the file was already modified
      before the first compile.
    * remove unsaved file checks when compiling without primary file so it
      works the same as with a primary file set.

  Revision 1.1.2.2  2000/12/20 14:27:49  pierre
   * fp.ini for unix

  Revision 1.1.2.1  2000/11/14 17:40:44  pierre
   + External linking now optional

  Revision 1.1  2000/07/13 09:48:36  michael
  + Initial import

  Revision 1.33  2000/06/16 08:50:42  pierre
   + new bunch of Gabor's changes

  Revision 1.32  2000/04/25 08:42:33  pierre
   * New Gabor changes : see fixes.txt

  Revision 1.31  2000/04/18 11:42:37  pierre
   lot of Gabor changes : see fixes.txt

  Revision 1.30  2000/03/13 20:35:36  pierre
    * IniPath replaced by IniFileName
    + UseMouse variable

  Revision 1.29  2000/03/08 16:50:27  pierre
   + MainHasDebugInfo boolean var

  Revision 1.28  2000/03/02 22:34:38  pierre
   + external compiler support

  Revision 1.27  2000/02/07 11:53:11  pierre
   Gabor changes

  Revision 1.26  2000/02/04 00:08:35  pierre
   + IniCenterDebuggerRow

  Revision 1.25  1999/09/16 14:34:59  pierre
    + TBreakpoint and TWatch registering
    + WatchesCollection and BreakpointsCollection stored in desk file
    * Syntax highlighting was broken

  Revision 1.24  1999/09/13 16:24:43  peter
    + clock
    * backspace unident like tp7

  Revision 1.23  1999/09/13 11:44:00  peter
    * fixes from gabor, idle event, html fix

  Revision 1.22  1999/08/16 18:25:25  peter
    * Adjusting the selection when the editor didn't contain any line.
    * Reserved word recognition redesigned, but this didn't affect the overall
      syntax highlight speed remarkably (at least not on my Amd-K6/350).
      The syntax scanner loop is a bit slow but the main problem is the
      recognition of special symbols. Switching off symbol processing boosts
      the performance up to ca. 200%...
    * The editor didn't allow copying (for ex to clipboard) of a single character
    * 'File|Save as' caused permanently run-time error 3. Not any more now...
    * Compiler Messages window (actually the whole desktop) did not act on any
      keypress when compilation failed and thus the window remained visible
    + Message windows are now closed upon pressing Esc
    + At 'Run' the IDE checks whether any sources are modified, and recompiles
      only when neccessary
    + BlockRead and BlockWrite (Ctrl+K+R/W) implemented in TCodeEditor
    + LineSelect (Ctrl+K+L) implemented
    * The IDE had problems closing help windows before saving the desktop

  Revision 1.21  1999/08/03 20:22:38  peter
    + TTab acts now on Ctrl+Tab and Ctrl+Shift+Tab...
    + Desktop saving should work now
       - History saved
       - Clipboard content saved
       - Desktop saved
       - Symbol info saved
    * syntax-highlight bug fixed, which compared special keywords case sensitive
      (for ex. 'asm' caused asm-highlighting, while 'ASM' didn't)
    * with 'whole words only' set, the editor didn't found occourences of the
      searched text, if the text appeared previously in the same line, but didn't
      satisfied the 'whole-word' condition
    * ^QB jumped to (SelStart.X,SelEnd.X) instead of (SelStart.X,SelStart.Y)
      (ie. the beginning of the selection)
    * when started typing in a new line, but not at the start (X=0) of it,
      the editor inserted the text one character more to left as it should...
    * TCodeEditor.HideSelection (Ctrl-K+H) didn't update the screen
    * Shift shouldn't cause so much trouble in TCodeEditor now...
    * Syntax highlight had problems recognizing a special symbol if it was
      prefixed by another symbol character in the source text
    * Auto-save also occours at Dos shell, Tool execution, etc. now...

  Revision 1.20  1999/07/28 23:11:25  peter
    * fixes from gabor

  Revision 1.19  1999/07/10 01:24:21  pierre
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
