{
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
     FPDebug,FPRegs,
     FPUtils,FPViews,FPCalc;

type
    TRecentFileEntry = record
      FileName  : string;
      LastPos   : TPoint;
    end;

    TCompPhase = (cpNothing,cpCompiling,cpLinking,
                  cpAborted,cpFailed,cpDone);

    {Use edit keys according to Borland convention (shift+del,ctrl+ins,shift+ins)
     or Microsoft convention (ctrl+x,ctrl+c,ctrl+v).}
    Tedit_key_modes=(ekm_borland,ekm_microsoft);


{$ifdef Unix}
      {Microsoft convention is default on Unix, because the Borland "paste" key, Shift+Ins,
       is not passed on to the program in most X terminal emulators.}
const ekm_default = ekm_microsoft;
{$else}
const ekm_default = ekm_borland;
{$endif}


const ClipboardWindow  : PClipboardWindow = nil;
      CalcWindow       : PCalculator = nil;
      RecentFileCount  : integer = 0;
      LastCompileTime  : cardinal = 0;
      OpenExts         : string = '*.pas;*.pp;*.inc';
      HighlightExts    : string = '*.pas;*.pp;*.inc';
      TabsPattern      : string = 'make*;make*.*;fpcmake.loc';
      SourceDirs       : string = '';
      StandardUnits    : string = '';
      UseStandardUnitsInCodeComplete : boolean = false;
      UseAllUnitsInCodeComplete : boolean = true;
      ShowOnlyUnique   : boolean = true;
      PrimaryFile      : string = '';
      PrimaryFileMain  : string = '';
      PrimaryFileSwitches : string = '';
      PrimaryFilePara  : string = '';
      GDBOutputFile    : string = GDBOutputFileName;
      IsEXECompiled    : boolean = false;
      { LinkAfter        : boolean = true; changed into a function }
      MainHasDebugInfo : boolean = false;
      UseMouse         : boolean = true;
      MainFile         : string = '';
      PrevMainFile     : string = '';
      EXEFile          : ansistring = '';
      CompilationPhase : TCompPhase = cpNothing;
{$ifndef NODEBUG}
      GDBWindow        : PGDBWindow = nil;
      DisassemblyWindow : PDisassemblyWindow = nil;
      BreakpointsWindow : PBreakpointsWindow = nil;
      WatchesWindow    : PWatchesWindow = nil;
      StackWindow      : PStackWindow = nil;
      RegistersWindow  : PRegistersWindow = nil;
      FPUWindow        : PFPUWindow = nil;
      VectorWindow     : PVectorWindow = nil;
{$endif NODEBUG}
      UserScreenWindow : PScreenWindow = nil;

      HeapView         : PFPHeapView = nil;
      ClockView        : PFPClockView = nil;
      HelpFiles        : WUtils.PUnsortedStringCollection = nil;
      ShowStatusOnError: boolean = true;
      StartupDir       : string = '.'+DirSep;
      IDEDir           : string = '.'+DirSep;
{$ifdef Unix}
      SystemIDEDir     : string = '';
{$endif Unix}
      INIFileName      : string = ININame;
      SwitchesPath     : string = SwitchesName;
      CtrlMouseAction  : integer = acTopicSearch;
      AltMouseAction   : integer = acBrowseSymbol;
      StartupOptions   : longint = 0;
      LastExitCode     : integer = 0;
      ASCIIChart       : PFPASCIIChart = nil;
      BackgroundPath   : string = BackgroundName;
      DesktopPath      : string = DesktopName;
      DesktopFileFlags : longint = dfHistoryLists+dfOpenWindows+
                                   dfCodeCompleteWords+dfCodeTemplates;
      DesktopLocation  : byte    = dlConfigFileDir;
      AutoSaveOptions  : longint = asEnvironment+asDesktop;
      MiscOptions      : longint = moChangeDirOnOpen+moCloseOnGotoSource;
      EditorModified   : boolean = false;
      IniCenterDebuggerRow : tcentre = do_centre;
      SleepTimeOut     : longint = trunc(10*18.2);
{$ifdef USE_EXTERNAL_COMPILER}
      UseExternalCompiler : boolean = true;
      ExternalCompilerExe : string = 'ppc386'+ExeExt;
{$endif USE_EXTERNAL_COMPILER}
      ShowReadme       : boolean = true;
      AskRecompileIfModifiedFlag : boolean = true;

      EditKeys:Tedit_key_modes = ekm_default;

{$ifdef SUPPORT_REMOTE}
     RemoteMachine : string = '';
     RemotePort : string = '2345';
     RemoteConfig : string = '';
     RemoteIdent : string = '';
     RemoteDir : string = '';
     RemoteSendCommand : string = 'scp $CONFIG $IDENT $LOCALFILE $REMOTEMACHINE:$REMOTEDIR';
{$endif SUPPORT_REMOTE}

     DebuggeeTTY : string = '';

      ActionCommands   : array[acFirstAction..acLastAction] of word =
        (cmHelpTopicSearch,cmGotoCursor,cmToggleBreakpoint,
         cmEvaluate,cmAddWatch,cmBrowseAtCursor);

      AppPalette       : string = CIDEAppColor;

var   RecentFiles      : array[1..MaxRecentFileCount] of TRecentFileEntry;

implementation

END.
