{
    $Id$
    This file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 1998 by Berczi Gabor

    Main IDEApp object

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpide;
interface

{$i globdir.inc}

uses
  Objects,Drivers,Views,App,Gadgets,MsgBox,
  {$ifdef EDITORS}Editors,{$else}WEditor,{$endif}
  Comphook,Browcol,
  FPViews,FPSymbol;

type
    TExecType = (exNormal,exNoSwap,exDosShell);

    TIDEApp = object(TApplication)
      constructor Init;
      procedure   InitDesktop; virtual;
      procedure   InitMenuBar; virtual;
      procedure   InitStatusLine; virtual;
      procedure   Open(FileName: string);
      function    OpenSearch(FileName: string) : boolean;
      function    SaveAll: boolean;
      function    AutoSave: boolean;
      procedure   Idle; virtual;
      procedure   Update;
      procedure   UpdateTarget;
      procedure   GetEvent(var Event: TEvent); virtual;
      procedure   HandleEvent(var Event: TEvent); virtual;
      procedure   GetTileRect(var R: TRect); virtual;
      function    GetPalette: PPalette; virtual;
      procedure   DosShell; virtual;
      destructor  Done; virtual;
      procedure   ShowUserScreen;
      procedure   ShowIDEScreen;
      function    IsClosing : boolean;
    private
      procedure NewEditor;
      procedure NewFromTemplate;
      procedure OpenRecentFile(RecentIndex: integer);
      procedure ChangeDir;
      procedure ShowClipboard;
      procedure FindProcedure;
      procedure Objects;
      procedure Modules;
      procedure Globals;
      procedure SearchSymbol;
      procedure Parameters;
      procedure DoStepOver;
      procedure DoTraceInto;
      procedure DoRun;
      procedure DoResetDebugger;
      procedure DoContToCursor;
      procedure DoContUntilReturn;
      procedure Target;
      procedure DoCompilerMessages;
      procedure DoPrimaryFile;
      procedure DoClearPrimary;
      procedure DoUserScreenWindow;
      procedure DoCloseUserScreenWindow;
      procedure DoUserScreen;
      procedure DoOpenGDBWindow;
      procedure DoToggleBreak;
      procedure DoShowCallStack;
      procedure DoShowBreakpointList;
      procedure DoShowWatches;
      procedure DoAddWatch;

      procedure DoInformation;
      procedure Messages;
      procedure Calculator;
      procedure DoAsciiTable;
      procedure ExecuteTool(Idx: integer);
      procedure SetSwitchesMode;
      procedure DoCompilerSwitch;
      procedure MemorySizes;
      procedure DoLinkerSwitch;
      procedure DoDebuggerSwitch;
      procedure Directories;
      procedure Tools;
      procedure DoGrep;
      procedure Preferences;
      procedure EditorOptions(Editor: PEditor);
      procedure BrowserOptions(Browser: PBrowserWindow);
      procedure DesktopOptions;
      procedure Mouse;
      procedure StartUp;
      procedure Colors;
      procedure OpenINI;
      procedure SaveINI;
      procedure SaveAsINI;
      procedure CloseAll;
      procedure WindowList;
      procedure HelpContents;
      procedure HelpHelpIndex;
      procedure HelpTopicSearch;
      procedure HelpPrevTopic;
      procedure HelpUsingHelp;
      procedure HelpFiles;
      procedure About;
    private
      SaveCancelled: boolean;
      InsideDone : boolean;
      LastEvent: longint;
      function  DoExecute(ProgramPath, Params, InFile, OutFile: string; ExecType: TExecType): boolean;
      procedure AddRecentFile(AFileName: string; CurX, CurY: integer);
      function  SearchRecentFile(AFileName: string): integer;
      procedure RemoveRecentFile(Index: integer);
      procedure CurDirChanged;
      procedure UpdatePrimaryFile;
      procedure UpdateINIFile;
      procedure UpdateRecentFileList;
      procedure UpdateTools;
    end;


var
  IDEApp: TIDEApp;

implementation

uses
{$ifdef linux}
  linux,
{$endif}
{$ifdef WinClipSupported}
  WinClip,
{$endif WinClipSupported}
  Video,Mouse,Keyboard,
  Dos,Memory,Menus,Dialogs,StdDlg,ColorSel,Commands,HelpCtx,
  AsciiTab,
  Systems,
  WUtils,WHelp,WHlpView,WINI,WViews,
  FPConst,FPVars,FPUtils,FPSwitch,FPIni,FPIntf,FPCompile,FPHelp,
  FPTemplt,FPCalc,FPUsrScr,FPTools,{$ifndef NODEBUG}FPDebug,{$endif}FPRedir,
  FPDesk;


function IDEUseSyntaxHighlight(Editor: PFileEditor): boolean; {$ifndef FPC}far;{$endif}
begin
  IDEUseSyntaxHighlight:=(Editor^.FileName='') or MatchesFileList(NameAndExtOf(Editor^.FileName),HighlightExts);
end;

function IDEUseTabsPattern(Editor: PFileEditor): boolean; {$ifndef FPC}far;{$endif}
begin
  IDEUseTabsPattern:=(Editor^.FileName='') or MatchesFileList(NameAndExtOf(Editor^.FileName),TabsPattern);
end;

constructor TIDEApp.Init;
var R: TRect;
begin
  {$ifndef EDITORS}
{$ifdef TP}
  UseSyntaxHighlight:=IDEUseSyntaxHighlight;
  UseTabsPattern:=IDEUseTabsPattern;
{$else TP}
  UseSyntaxHighlight:=@IDEUseSyntaxHighlight;
  UseTabsPattern:=@IDEUseTabsPattern;
{$endif TP}
  {$endif}
  inherited Init;
  InsideDone:=false;
  MenuBar^.GetBounds(R); R.A.X:=R.B.X-8;
  New(ClockView, Init(R));
  Application^.Insert(ClockView);
  New(ClipboardWindow, Init);
  Desktop^.Insert(ClipboardWindow);
  New(CalcWindow, Init); CalcWindow^.Hide;
  Desktop^.Insert(CalcWindow);
{$ifndef OLDCOMP}
  New(CompilerMessageWindow, Init);
  CompilerMessageWindow^.Hide;
  Desktop^.Insert(CompilerMessageWindow);
{$else}
  New(ProgramInfoWindow, Init);
  ProgramInfoWindow^.Hide;
  Desktop^.Insert(ProgramInfoWindow);
{$endif}
  Message(@Self,evBroadcast,cmUpdate,nil);
  CurDirChanged;
  { heap viewer }
  GetExtent(R); Dec(R.B.X); R.A.X:=R.B.X-9; R.A.Y:=R.B.Y-1;
  New(HeapView, InitKb(R));
  Insert(HeapView);
  Drivers.ShowMouse;
end;

procedure TIDEApp.InitDesktop;
var
  R: TRect;
begin
  GetExtent(R);
  Inc(R.A.Y);
  Dec(R.B.Y);
  Desktop:=New(PFPDesktop, Init(R));
end;

procedure TIDEApp.InitMenuBar;
var R: TRect;
    WinPMI : PMenuItem;
begin
  GetExtent(R); R.B.Y:=R.A.Y+1;
  WinPMI:=nil;
{$ifdef WinClipSupported}
  if WinClipboardSupported then
    WinPMI:=NewLine(
      NewItem('Cop~y~ to Windows','', kbNoKey, cmCopyWin, hcCopyWin,
      NewItem('Paste from ~W~indows','', kbNoKey, cmPasteWin, hcPasteWin,
      nil)));
{$endif WinClipSupported}
  MenuBar:=New(PAdvancedMenuBar, Init(R, NewMenu(
    NewSubMenu('~F~ile',hcFileMenu, NewMenu(
      NewItem('~N~ew','',kbNoKey,cmNew,hcNew,
      NewItem('New from ~t~emplate...','',kbNoKey,cmNewFromTemplate,hcNewFromTemplate,
      NewItem('~O~pen...','F3',kbF3,cmOpen,hcOpen,
      NewItem('~S~ave','F2',kbF2,cmSave,hcSave,
      NewItem('Save ~a~s...','',kbNoKey,cmSaveAs,hcSaveAs,
      NewItem('Save a~l~l','',kbNoKey,cmSaveAll,hcSaveAll,
      NewLine(
      NewItem('~C~hange dir...','',kbNoKey,cmChangeDir,hcChangeDir,
      NewItem('~D~OS shell','',kbNoKey,cmDOSShell,hcDOSShell,
      NewItem('E~x~it','Alt+X',kbNoKey,cmQuit,hcQuit,
      nil))))))))))),
    NewSubMenu('~E~dit',hcEditMenu, NewMenu(
      NewItem('~U~ndo','Alt+BkSp', kbAltBack, cmUndo, hcUndo,
      NewItem('~R~edo','', kbNoKey, cmRedo, hcRedo,
{$ifdef DebugUndo}
      NewItem('~D~ump Undo','', kbNoKey, cmDumpUndo, hcUndo,
      NewItem('U~n~do All','', kbNoKey, cmUndoAll, hcUndo,
      NewItem('R~e~do All','', kbNoKey, cmRedoAll, hcRedo,
{$endif DebugUndo}
      NewLine(
      NewItem('Cu~t~','Shift+Del', kbShiftDel, cmCut, hcCut,
      NewItem('~C~opy','Ctrl+Ins', kbCtrlIns, cmCopy, hcCut,
      NewItem('~P~aste','Shift+Ins', kbShiftIns, cmPaste, hcPaste,
      NewItem('C~l~ear','Ctrl+Del', kbCtrlDel, cmClear, hcClear,
      NewLine(
      NewItem('~S~how clipboard','', kbNoKey, cmShowClipboard, hcShowClipboard,
      WinPMI))))))
{$ifdef DebugUndo}))){$endif DebugUndo}
      )))),
    NewSubMenu('~S~earch',hcSearchMenu, NewMenu(
      NewItem('~F~ind...','', kbNoKey, cmFind, hcFind,
      NewItem('~R~eplace...','', kbNoKey, cmReplace, hcReplace,
      NewItem('~S~earch again','', kbNoKey, cmSearchAgain, hcSearchAgain,
      NewLine(
      NewItem('~G~o to line number...','', kbNoKey, cmJumpLine, hcGotoLine,
      NewItem('Find ~p~rocedure...','', kbNoKey, cmFindProcedure, hcFindProcedure,
      NewLine(
      NewItem('~O~bjects','', kbNoKey, cmObjects, hcObjects,
      NewItem('Mod~u~les','', kbNoKey, cmModules, hcModules,
      NewItem('G~l~obals','', kbNoKey, cmGlobals, hcGlobals,
      NewLine(
      NewItem('S~y~mbol','', kbNoKey, cmSymbol, hcSymbol,
      nil))))))))))))),
    NewSubMenu('~R~un',hcRunMenu, NewMenu(
      NewItem('~R~un','Ctrl+F9', kbCtrlF9, cmRun, hcRun,
      NewItem('~S~tep over','F8', kbF8, cmStepOver, hcRun,
      NewItem('~T~race into','F7', kbF7, cmTraceInto, hcRun,
      NewItem('~G~oto Cursor','F4', kbF4, cmContToCursor, hcContToCursor,
      NewItem('~U~ntil return','', kbNoKey,cmUntilReturn,hcUntilReturn,
      NewItem('P~a~rameters...','', kbNoKey, cmParameters, hcParameters,
      NewItem('~P~rogram reset','Ctrl+F2', kbCtrlF2, cmResetDebugger, hcResetDebugger,
      nil)))))))),
    NewSubMenu('~C~ompile',hcCompileMenu, NewMenu(
      NewItem('~C~ompile','Alt+F9', kbAltF9, cmCompile, hcCompile,
      NewItem('~M~ake','F9', kbF9, cmMake, hcMake,
      NewItem('~B~uild','', kbNoKey, cmBuild, hcBuild,
      NewLine(
      NewItem('~T~arget...','', kbNoKey, cmTarget, hcTarget,
      NewItem('~P~rimary file...','', kbNoKey, cmPrimaryFile, hcPrimaryFile,
      NewItem('C~l~ear primary file','', kbNoKey, cmClearPrimary, hcClearPrimary,
      NewLine(
      NewItem('~I~nformation...','', kbNoKey, cmInformation, hcInformation,
      NewItem('C~o~mpiler messages','F12', kbF12, cmCompilerMessages, hcCompilerMessages,
      nil))))))))))),
    NewSubMenu('~D~ebug', hcDebugMenu, NewMenu(
      NewItem('~O~utput','', kbNoKey, cmUserScreenWindow, hcUserScreenWindow,
      NewItem('~U~ser screen','Alt+F5', kbAltF5, cmUserScreen, hcUserScreen,
      NewItem('~B~reakpoint','Ctrl+F8', kbCtrlF8, cmToggleBreakpoint, hcToggleBreakpoint,
      NewItem('~C~all stack','Ctrl+F3', kbCtrlF3, cmStack, hcStack,
      NewItem('~A~dd Watch','Ctrl+F7', kbCtrlF7, cmAddWatch, hcAddWatch,
      NewItem('~W~atches','', kbNoKey, cmWatches, hcWatches,
      NewItem('Breakpoint ~L~ist','', kbNoKey, cmBreakpointList, hcBreakpointList,
      NewLine(
      NewItem('~G~DB window','', kbNoKey, cmOpenGDBWindow, hcOpenGDBWindow,
      nil)))))))))),
    NewSubMenu('~T~ools', hcToolsMenu, NewMenu(
      NewItem('~M~essages', 'F11', kbF11, cmToolsMessages, hcToolsMessages,
      NewItem('Goto ~n~ext','Alt+F8', kbAltF8, cmToolsMsgNext, hcToolsMsgNext,
      NewItem('Goto ~p~revious','Alt+F7', kbAltF7, cmToolsMsgPrev, hcToolsMsgPrev,
      NewLine(
      NewItem('~G~rep', 'Shift+F2', kbShiftF2, cmGrep, hcGrep,
      NewItem('~C~alculator', '', kbNoKey, cmCalculator, hcCalculator,
      NewItem('Ascii ~t~able', '', kbNoKey, cmAsciiTable, hcAsciiTable,
      nil)))))))),
    NewSubMenu('~O~ptions', hcOptionsMenu, NewMenu(
      NewItem('Mode~.~..','', kbNoKey, cmSwitchesMode, hcSwitchesMode,
      NewItem('~C~ompiler...','', kbNoKey, cmCompiler, hcCompiler,
      NewItem('~M~emory sizes...','', kbNoKey, cmMemorySizes, hcMemorySizes,
      NewItem('~L~inker...','', kbNoKey, cmLinker, hcLinker,
      NewItem('De~b~ugger...','', kbNoKey, cmDebugger, hcDebugger,
      NewItem('~D~irectories...','', kbNoKey, cmDirectories, hcDirectories,
      NewItem('Bro~w~ser...','',kbNoKey, cmBrowser, hcBrowser,
      NewItem('~T~ools...','', kbNoKey, cmTools, hcTools,
      NewLine(
      NewSubMenu('~E~nvironment', hcEnvironmentMenu, NewMenu(
        NewItem('~P~references...','', kbNoKey, cmPreferences, hcPreferences,
        NewItem('~E~ditor...','', kbNoKey, cmEditor, hcEditor,
        NewItem('~D~esktop...','', kbNoKey, cmDesktopOptions, hcDesktopOptions,
        NewItem('~M~ouse...','', kbNoKey, cmMouse, hcMouse,
        NewItem('~S~tartup...','', kbNoKey, cmStartup, hcStartup,
        NewItem('~C~olors...','', kbNoKey, cmColors, hcColors,
        nil))))))),
      NewLine(
      NewItem('~O~pen...','', kbNoKey, cmOpenINI, hcOpenINI,
      NewItem('~S~ave','', kbNoKey, cmSaveINI, hcSaveINI,
      NewItem('Save ~a~s...','', kbNoKey, cmSaveAsINI, hcSaveAsINI,
      nil))))))))))))))),
    NewSubMenu('~W~indow', hcWindowMenu, NewMenu(
      NewItem('~T~ile','', kbNoKey, cmTile, hcTile,
      NewItem('C~a~scade','', kbNoKey, cmCascade, hcCascade,
      NewItem('Cl~o~se all','', kbNoKey, cmCloseAll, hcCloseAll,
      NewLine(
      NewItem('~S~ize/Move','Ctrl+F5', kbCtrlF5, cmResize, hcResize,
      NewItem('~Z~oom','F5', kbF5, cmZoom, hcZoom,
      NewItem('~N~ext','F6', kbF6, cmNext, hcNext,
      NewItem('~P~revious','Shift+F6', kbShiftF6, cmPrev, hcPrev,
      NewItem('~C~lose','Alt+F3', kbAltF3, cmClose, hcClose,
      NewLine(
      NewItem('~L~ist...','Alt+0', kbAlt0, cmWindowList, hcWindowList,
      NewItem('~R~efresh display','', kbNoKey, cmUpdate, hcUpdate,
      nil))))))))))))),
    NewSubMenu('~H~elp', hcHelpMenu, NewMenu(
      NewItem('~C~ontents','', kbNoKey, cmHelpContents, hcHelpContents,
      NewItem('~I~ndex','Shift+F1', kbShiftF1, cmHelpIndex, hcHelpIndex,
      NewItem('~T~opic search','Ctrl+F1', kbCtrlF1, cmHelpTopicSearch, hcHelpTopicSearch,
      NewItem('~P~revious topic','Alt+F1', kbAltF1, cmHelpPrevTopic, hcHelpPrevTopic,
      NewItem('~U~sing help','',kbNoKey, cmHelpUsingHelp, hcHelpUsingHelp,
      NewItem('~F~iles...','',kbNoKey, cmHelpFiles, hcHelpFiles,
      NewLine(
      NewItem('~A~bout...','',kbNoKey, cmAbout, hcAbout,
      nil))))))))),
    nil)))))))))))));
  DisableCommands(EditorCmds+SourceCmds+CompileCmds);
  Update;
end;

procedure TIDEApp.InitStatusLine;
var
  R: TRect;
begin
  GetExtent(R);
  R.A.Y := R.B.Y - 1;
  StatusLine:=New(PIDEStatusLine, Init(R,
    NewStatusDef(hcFirstCommand, hcLastCommand,
      NewStatusKey('~F1~ Help', kbF1, cmHelp,
      StdStatusKeys(
      nil)),
    NewStatusDef(hcHelpWindow, hcHelpWindow,
      NewStatusKey('~F1~ Help on help', kbF1, cmHelpUsingHelp,
      NewStatusKey('~Alt+F1~ Previous topic', kbAltF1, cmHelpPrevTopic,
      NewStatusKey('~Shift+F1~ Help index', kbShiftF1, cmHelpIndex,
      NewStatusKey('~Esc~ Close help', kbEsc, cmClose,
      StdStatusKeys(
      nil))))),
    NewStatusDef(hcSourceWindow, hcSourceWindow,
      NewStatusKey('~F1~ Help', kbF1, cmHelp,
      NewStatusKey('~F2~ Save', kbF2, cmSave,
      NewStatusKey('~F3~ Open', kbF3, cmOpen,
      NewStatusKey('~Alt+F9~ Compile', kbAltF9, cmCompile,
      NewStatusKey('~F9~ Make', kbF9, cmMake,
      NewStatusKey('~Alt+F10~ Local menu', kbAltF10, cmLocalMenu,
      StdStatusKeys(
      nil))))))),
    NewStatusDef(hcASCIITableWindow, hcASCIITableWindow,
      NewStatusKey('~F1~ Help', kbF1, cmHelp,
      NewStatusKey('~Ctrl+Enter~ Transfer char', kbCtrlEnter, cmTransfer,
      StdStatusKeys(
      nil))),
    NewStatusDef(hcMessagesWindow, hcMessagesWindow,
      NewStatusKey('~F1~ Help', kbF1, cmHelp,
      NewStatusKey('~'+EnterSign+'~ Goto source', kbEnter, cmMsgGotoSource,
      NewStatusKey('~Space~ Track source', kbNoKey, cmMsgTrackSource,
      NewStatusKey('~Alt+F10~ Local menu', kbAltF10, cmLocalMenu,
      NewStatusKey('', kbEsc, cmClose,
      StdStatusKeys(
      nil)))))),
    NewStatusDef(hcCalcWindow, hcCalcWindow,
      NewStatusKey('~F1~ Help', kbF1, cmHelp,
      NewStatusKey('~Esc~ Close', kbEsc, cmClose,
      NewStatusKey('~Ctrl+Enter~ Transfer result', kbCtrlEnter, cmCalculatorPaste,
      StdStatusKeys(
      nil)))),
    NewStatusDef(0, $FFFF,
      NewStatusKey('~F1~ Help', kbF1, cmHelp,
      NewStatusKey('~F3~ Open', kbF3, cmOpen,
      NewStatusKey('~Alt+F9~ Compile', kbAltF9, cmCompile,
      NewStatusKey('~F9~ Make', kbF9, cmMake,
      NewStatusKey('~Alt+F10~ Local menu', kbAltF10, cmLocalMenu,
      StdStatusKeys(
      nil)))))),
    nil)))))))));
end;

procedure TIDEApp.Idle;
begin
  inherited Idle;
  Message(Application,evIdle,0,nil);
  GiveUpTimeSlice;
end;

procedure TIDEApp.GetEvent(var Event: TEvent);
begin
  inherited GetEvent(Event);
  if Event.What<>evNothing then
    LastEvent:=GetDosTicks
  else
    if abs(GetDosTicks-LastEvent)>SleepTimeOut then
      GiveUpTimeSlice;
end;

procedure TIDEApp.HandleEvent(var Event: TEvent);
var DontClear: boolean;
begin
  case Event.What of
       evCommand :
         begin
           DontClear:=false;
           case Event.Command of
             cmUpdate        : Message(Application,evBroadcast,cmUpdate,nil);
           { -- File menu -- }
             cmNew           : NewEditor;
             cmNewFromTemplate: NewFromTemplate;
             cmOpen          : begin
                                 if (DirOf(OpenFileName)='') or (Pos(ListSeparator,OpenFileName)<>0) then
                                   OpenFileName:=LocateFile(OpenFileName);
                                 Open(OpenFileName);
                                 OpenFileName:='';
                               end;
             cmSaveAll       : SaveAll;
             cmChangeDir     : ChangeDir;
             cmDOSShell      : DOSShell;
             cmRecentFileBase..
             cmRecentFileBase+10
                             : OpenRecentFile(Event.Command-cmRecentFileBase);
           { -- Edit menu -- }
             cmShowClipboard : ShowClipboard;
           { -- Search menu -- }
             cmFindProcedure : FindProcedure;
             cmObjects       : Objects;
             cmModules       : Modules;
             cmGlobals       : Globals;
             cmSymbol        : SearchSymbol;
           { -- Run menu -- }
             cmParameters    : Parameters;
             cmStepOver      : DoStepOver;
             cmTraceInto     : DoTraceInto;
             cmRun           : DoRun;
             cmResetDebugger : DoResetDebugger;
             cmContToCursor  : DoContToCursor;
             cmUntilReturn   : DoContUntilReturn;
           { -- Compile menu -- }
             cmCompile       : DoCompile(cCompile);
             cmBuild         : DoCompile(cBuild);
             cmMake          : DoCompile(cMake);
             cmTarget        : Target;
             cmPrimaryFile   : DoPrimaryFile;
             cmClearPrimary  : DoClearPrimary;
             cmInformation   : DoInformation;
             cmCompilerMessages : DoCompilerMessages;
           { -- Debug menu -- }
             cmUserScreen    : DoUserScreen;
             cmToggleBreakpoint : DoToggleBreak;
             cmStack         : DoShowCallStack;
             cmBreakpointList : DoShowBreakpointList;
             cmWatches       :  DoShowWatches;
             cmAddWatch      :  DoAddWatch;
             cmOpenGDBWindow : DoOpenGDBWindow;
           { -- Options menu -- }
             cmSwitchesMode  : SetSwitchesMode;
             cmCompiler      : DoCompilerSwitch;
             cmMemorySizes   : MemorySizes;
             cmLinker        : DoLinkerSwitch;
             cmDebugger      : DoDebuggerSwitch;
             cmDirectories   : Directories;
             cmTools         : Tools;
             cmPreferences   : Preferences;
             cmEditor        : EditorOptions(nil);
             cmEditorOptions : EditorOptions(Event.InfoPtr);
             cmBrowser       : BrowserOptions(nil);
             cmBrowserOptions : BrowserOptions(Event.InfoPtr);
             cmMouse         : Mouse;
             cmStartup       : StartUp;
             cmDesktopOptions: DesktopOptions;
             cmColors        : Colors;
             cmOpenINI       : OpenINI;
             cmSaveINI       : SaveINI;
             cmSaveAsINI     : SaveAsINI;
           { -- Tools menu -- }
             cmToolsMessages : Messages;
             cmCalculator    : Calculator;
             cmAsciiTable    : DoAsciiTable;
             cmGrep          : DoGrep;
             cmToolsBase+1..
             cmToolsBase+MaxToolCount
                             : ExecuteTool(Event.Command-cmToolsBase);
           { -- Window menu -- }
             cmCloseAll      : CloseAll;
             cmWindowList    : WindowList;
             cmUserScreenWindow: DoUserScreenWindow;
           { -- Help menu -- }
             cmHelpContents  : HelpContents;
             cmHelpIndex     : HelpHelpIndex;
{             cmHelpTopicSearch: HelpTopicSearch;}
             cmHelpPrevTopic : HelpPrevTopic;
             cmHelpUsingHelp : HelpUsingHelp;
             cmHelpFiles     : HelpFiles;
             cmAbout         : About;
           else DontClear:=true;
           end;
           if DontClear=false then ClearEvent(Event);
         end;
       evBroadcast :
         case Event.Command of
           cmSaveCancelled :
             SaveCancelled:=true;
           cmUpdateTools :
             UpdateTools;
           cmCommandSetChanged :
             UpdateMenu(MenuBar^.Menu);
           cmUpdate              :
             Update;
           cmSourceWndClosing :
             begin
               with PSourceWindow(Event.InfoPtr)^ do
                 if Editor^.FileName<>'' then
                   AddRecentFile(Editor^.FileName,Editor^.CurPos.X,Editor^.CurPos.Y);
               {$ifndef NODEBUG}
               if assigned(Debugger) and (PView(Event.InfoPtr)=Debugger^.LastSource) then
                 Debugger^.LastSource:=nil;
               {$endif}
             end;

         end;
  end;
  inherited HandleEvent(Event);
end;


procedure TIDEApp.GetTileRect(var R: TRect);
begin
  Desktop^.GetExtent(R);
{ Leave the compiler messages window in the bottom }
  if assigned(CompilerMessageWindow) and (CompilerMessageWindow^.GetState(sfVisible)) then
   R.B.Y:=Min(CompilerMessageWindow^.Origin.Y,Desktop^.Size.Y);
{ Leave the messages window in the bottom }
  if assigned(MessagesWindow) and (MessagesWindow^.GetState(sfVisible)) and
     (MessagesWindow^.Origin.Y<R.B.Y) then
   R.B.Y:=MessagesWindow^.Origin.Y;
end;


{****************************************************************************
                                 Switch Screens
****************************************************************************}

procedure TIDEApp.ShowUserScreen;
begin
  DoneSysError;
  DoneEvents;
  DoneMouse;
  DoneScreen; { this is available in FV app.pas (PFV) }
  DoneDosMem;

  if Assigned(UserScreen) then
    UserScreen^.SwitchTo;
end;


procedure TIDEApp.ShowIDEScreen;
begin
  if Assigned(UserScreen) then
    UserScreen^.SwitchBack;

  InitDosMem;
  InitScreen;
  InitMouse;
  InitEvents;
  InitSysError;
  CurDirChanged;
  Message(Application,evBroadcast,cmUpdate,nil);
  UpdateScreen(true);
end;

function TIDEApp.AutoSave: boolean;
var IOK,SOK,DOK: boolean;
begin
  IOK:=true; SOK:=true; DOK:=true;
  if (AutoSaveOptions and asEnvironment)<>0 then
    begin
      IOK:=WriteINIFile;
      if IOK=false then
        ErrorBox('Error saving configuration.',nil);
    end;
  if (AutoSaveOptions and asEditorFiles)=0 then
      SOK:=SaveAll;
  if (AutoSaveOptions and asDesktop)<>0 then
    begin
      { destory all help & browser windows - we don't want to store them }
      { UserScreenWindow is also not registered PM }
      DoCloseUserScreenWindow;
      CloseHelpWindows;
      CloseAllBrowsers;
      DOK:=SaveDesktop;
      if DOK=false then
        ErrorBox('Error saving desktop file.'#13+
                 'Desktop layout could not be stored.',nil);
    end;
  AutoSave:=IOK and SOK and DOK;
end;

function TIDEApp.DoExecute(ProgramPath, Params, InFile,OutFile: string; ExecType: TExecType): boolean;
var CanRun: boolean;
begin
  SaveCancelled:=false;
  CanRun:=AutoSave;
  if (CanRun=false) and (SaveCancelled=false) then
    CanRun:=true; { do not care about .DSK or .INI saving problems - just like TP }
  if CanRun then
  begin
    if UserScreen=nil then
     begin
       ErrorBox('Sorry, user screen not available.',nil);
       Exit;
     end;

    if ExecType<>exNoSwap then
      ShowUserScreen;

    if ExecType=exDosShell then
      WriteShellMsg;

  {$ifdef linux}
    Shell(ProgramPath+' '+Params);
  {$else}
    if (InFile='') and (OutFile='') then
      DosExecute(GetEnv('COMSPEC'),'/C '+ProgramPath+' '+Params)
    else
      ExecuteRedir(GetEnv('COMSPEC'),'/C '+ProgramPath+' '+Params,InFile,OutFile,'stderr');
  {$endif}

    if ExecType<>exNoSwap then
      ShowIDEScreen;
  end;
  DoExecute:=CanRun;
end;


procedure TIDEApp.Update;
begin
  SetCmdState([cmSaveAll],IsThereAnyEditor);
  SetCmdState([cmCloseAll,cmTile,cmCascade,cmWindowList],IsThereAnyWindow);
  SetCmdState([cmFindProcedure,cmObjects,cmModules,cmGlobals,cmSymbol{,cmInformation}],IsSymbolInfoAvailable);
{$ifndef NODEBUG}
  SetCmdState([cmResetDebugger,cmUntilReturn],assigned(debugger) and debugger^.debuggee_started);
{$endif}
  SetCmdState([cmToolsMsgNext,cmToolsMsgPrev],MessagesWindow<>nil);
  UpdateTools;
  UpdateRecentFileList;
  UpdatePrimaryFile;
  UpdateINIFile;
  Message(Application,evBroadcast,cmCommandSetChanged,nil);
end;

procedure TIDEApp.CurDirChanged;
begin
  Message(Application,evBroadcast,cmUpdateTitle,nil);
  UpdatePrimaryFile;
  UpdateINIFile;
  UpdateMenu(MenuBar^.Menu);
end;


procedure TIDEApp.UpdatePrimaryFile;
begin
  SetMenuItemParam(SearchMenuItem(MenuBar^.Menu,cmPrimaryFile),SmartPath(PrimaryFile));
  SetCmdState([cmClearPrimary],PrimaryFile<>'');
  if PrimaryFile<>'' then
     SetCmdState(CompileCmds,true);
  UpdateMenu(MenuBar^.Menu);
  Message(ProgramInfoWindow,evBroadcast,cmUpdate,nil);
end;

procedure TIDEApp.UpdateINIFile;
begin
  SetMenuItemParam(SearchMenuItem(MenuBar^.Menu,cmSaveINI),SmartPath(INIPath));
end;

procedure TIDEApp.UpdateRecentFileList;
var P: PMenuItem;
    ID,I: word;
    FileMenu: PMenuItem;
begin
  ID:=cmRecentFileBase;
  FileMenu:=SearchSubMenu(MenuBar^.Menu,menuFile);
  repeat
{    Inc(ID);
    P:=SearchMenuItem(FileMenu^.SubMenu,ID);
    if FileMenu^.SubMenu^.Default=P then
      FileMenu^.SubMenu^.Default:=FileMenu^.SubMenu^.Items;
    if P<>nil then RemoveMenuItem(FileMenu^.SubMenu,P);}
    P:=GetMenuItemBefore(FileMenu^.SubMenu,nil);
    if (P<>nil) then
    begin
      if (cmRecentFileBase<P^.Command) and (P^.Command<=cmRecentFileBase+MaxRecentFileCount) then
        begin
          RemoveMenuItem(FileMenu^.SubMenu,P);
          if FileMenu^.SubMenu^.Default=P then
            FileMenu^.SubMenu^.Default:=FileMenu^.SubMenu^.Items;
        end
      else
        P:=nil;
    end;
  until P=nil;
  P:=GetMenuItemBefore(FileMenu^.SubMenu,nil);
  if (P<>nil) and IsSeparator(P) then
     RemoveMenuItem(FileMenu^.SubMenu,P);

  if RecentFileCount>0 then
     AppendMenuItem(FileMenu^.SubMenu,NewLine(nil));
  for I:=1 to RecentFileCount do
  begin
    P:=NewItem('~'+IntToStr(I)+'~ '+ShrinkPath(SmartPath(RecentFiles[I].FileName),27),' ',
        kbNoKey,cmRecentFileBase+I,hcRecentFileBase+I,nil);
    AppendMenuItem(FileMenu^.SubMenu,P);
  end;
end;

procedure TIDEApp.UpdateTools;
var P: PMenuItem;
    ID,I: word;
    ToolsMenu: PMenuItem;
    S1,S2,S3: string;
    W: word;
begin
  ID:=cmToolsBase;
  ToolsMenu:=SearchSubMenu(MenuBar^.Menu,menuTools);
  repeat
    P:=GetMenuItemBefore(ToolsMenu^.SubMenu,nil);
    if (P<>nil) then
    begin
      if (cmToolsBase<P^.Command) and (P^.Command<=cmToolsBase+MaxToolCount) then
        begin
          RemoveMenuItem(ToolsMenu^.SubMenu,P);
          if ToolsMenu^.SubMenu^.Default=P then
            ToolsMenu^.SubMenu^.Default:=ToolsMenu^.SubMenu^.Items;
        end
      else
        P:=nil;
    end;
  until P=nil;
  P:=GetMenuItemBefore(ToolsMenu^.SubMenu,nil);
  if (P<>nil) and IsSeparator(P) then
     RemoveMenuItem(ToolsMenu^.SubMenu,P);

  if GetToolCount>0 then
     AppendMenuItem(ToolsMenu^.SubMenu,NewLine(nil));
  for I:=1 to GetToolCount do
  begin
    GetToolParams(I-1,S1,S2,S3,W);
    P:=NewItem(S1,KillTilde(GetHotKeyName(W)),W,cmToolsBase+I,hcToolsBase+I,nil);
    AppendMenuItem(ToolsMenu^.SubMenu,P);
  end;
end;

procedure TIDEApp.DosShell;
begin
  DoExecute(GetEnv('COMSPEC'), '', '', '', exDosShell);
end;

{$I FPMFILE.INC}

{$I FPMEDIT.INC}

{$I FPMSRCH.INC}

{$I FPMRUN.INC}

{$I FPMCOMP.INC}

{$I FPMDEBUG.INC}

{$I FPMTOOLS.INC}

{$I FPMOPTS.INC}

{$I FPMWND.INC}

{$I FPMHELP.INC}

procedure TIDEApp.AddRecentFile(AFileName: string; CurX, CurY: integer);
begin
  if SearchRecentFile(AFileName)<>-1 then Exit;
  if RecentFileCount>0 then
   Move(RecentFiles[1],RecentFiles[2],SizeOf(RecentFiles[1])*Min(RecentFileCount,High(RecentFiles)-1));
  if RecentFileCount<High(RecentFiles) then Inc(RecentFileCount);
  with RecentFiles[1] do
  begin
    FileName:=AFileName;
    LastPos.X:=CurX; LastPos.Y:=CurY;
  end;
  UpdateRecentFileList;
end;

function TIDEApp.SearchRecentFile(AFileName: string): integer;
var Idx,I: integer;
begin
  Idx:=-1;
  for I:=1 to RecentFileCount do
    if UpcaseStr(AFileName)=UpcaseStr(RecentFiles[I].FileName) then
      begin Idx:=I; Break; end;
  SearchRecentFile:=Idx;
end;

procedure TIDEApp.RemoveRecentFile(Index: integer);
begin
  if Index<RecentFileCount then
     Move(RecentFiles[Index+1],RecentFiles[Index],SizeOf(RecentFiles[1])*(RecentFileCount-Index));
  Dec(RecentFileCount);
end;

function TIDEApp.GetPalette: PPalette;
var P: string;
begin
  P:=AppPalette;
  GetPalette:=@P;
end;

function TIDEApp.IsClosing: Boolean;
begin
  IsClosing:=InsideDone;
end;

destructor TIDEApp.Done;
begin
  InsideDone:=true;
  inherited Done;
  RemoveBrowsersCollection;
  DoneHelpSystem;
end;

END.
{
  $Log$
  Revision 1.48  2000-01-03 11:38:33  michael
  Changes from Gabor

  Revision 1.47  1999/12/20 14:23:17  pierre
    * MyApp renamed IDEApp
    * TDebugController.ResetDebuggerRows added to
      get resetting of debugger rows

  Revision 1.46  1999/12/17 15:07:01  florian
    + TIDEApp.Idle does always call GiveUpTimeSlice

  Revision 1.45  1999/12/10 13:02:05  pierre
  + VideoMode save/restore

  Revision 1.44  1999/11/25 00:26:49  pierre
   * RecentFiles missed the last char

  Revision 1.43  1999/11/10 17:19:06  pierre
   * Use DosExecute from Fpredir unit

  Revision 1.42  1999/10/27 12:10:42  pierre
    + With DebugUndo added 3 menu items
      "Dump Undo" "Undo All" and "Redo All"
      for Undo checks

  Revision 1.41  1999/09/22 16:21:41  pierre
   * Use ShrinkPas for RecentFiles

  Revision 1.40  1999/09/22 13:04:31  pierre
   + Close UserScreen to avoid store crash

  Revision 1.39  1999/09/21 17:09:00  pierre
   + Windows clipboard for win32

  Revision 1.38  1999/09/13 16:24:43  peter
    + clock
    * backspace unident like tp7

  Revision 1.37  1999/09/13 11:44:00  peter
    * fixes from gabor, idle event, html fix

  Revision 1.36  1999/09/09 14:15:27  pierre
   + cmCopyWin,cmPasteWin

  Revision 1.35  1999/08/16 18:25:19  peter
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

  Revision 1.34  1999/08/03 20:22:32  peter
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

  Revision 1.33  1999/07/12 13:14:18  pierre
    * LineEnd bug corrected, now goes end of text even if selected
    + Until Return for debugger
    + Code for Quit inside GDB Window

  Revision 1.32  1999/07/10 01:24:17  pierre
   + First implementation of watches window

  Revision 1.31  1999/06/29 22:50:14  peter
    * more fixes from gabor

  Revision 1.30  1999/06/28 19:32:20  peter
    * fixes from gabor

  Revision 1.29  1999/06/28 12:40:19  pierre
   + RemoveBrowsersCollection in TIDEApp.Done

  Revision 1.28  1999/06/25 00:46:33  pierre
     + UpdateTarget to show current target
     + SearchSymbol, not scope aware (this will need a PPU change !)

  Revision 1.27  1999/05/22 13:44:30  peter
    * fixed couple of bugs

  Revision 1.26  1999/04/07 21:55:47  peter
    + object support for browser
    * html help fixes
    * more desktop saving things
    * NODEBUG directive to exclude debugger

  Revision 1.25  1999/03/23 15:11:29  peter
    * desktop saving things
    * vesa mode
    * preferences dialog

  Revision 1.24  1999/03/19 16:04:29  peter
    * new compiler dialog

  Revision 1.23  1999/03/16 12:38:10  peter
    * tools macro fixes
    + tph writer
    + first things for resource files

  Revision 1.22  1999/03/12 01:13:57  peter
    * flag if trytoopen should look for other extensions
    + browser tab in the tools-compiler

  Revision 1.21  1999/03/02 13:48:29  peter
    * fixed far problem is fpdebug
    * tile/cascading with message window
    * grep fixes

  Revision 1.20  1999/03/01 15:41:54  peter
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

  Revision 1.19  1999/02/22 11:51:36  peter
    * browser updates from gabor

  Revision 1.18  1999/02/22 02:15:13  peter
    + default extension for save in the editor
    + Separate Text to Find for the grep dialog
    * fixed redir crash with tp7

  Revision 1.17  1999/02/20 15:18:30  peter
    + ctrl-c capture with confirm dialog
    + ascii table in the tools menu
    + heapviewer
    * empty file fixed
    * fixed callback routines in fpdebug to have far for tp7

  Revision 1.16  1999/02/18 13:44:31  peter
    * search fixed
    + backward search
    * help fixes
    * browser updates

  Revision 1.15  1999/02/16 10:43:55  peter
    * use -dGDB for the compiler
    * only use gdb_file when -dDEBUG is used
    * profiler switch is now a toggle instead of radiobutton

  Revision 1.14  1999/02/11 19:07:22  pierre
    * GDBWindow redesigned :
      normal editor apart from
      that any kbEnter will send the line (for begin to cursor)
      to GDB command !
      GDBWindow opened in Debugger Menu
       still buggy :
       -echo should not be present if at end of text
       -GDBWindow becomes First after each step (I don't know why !)

  Revision 1.13  1999/02/10 09:54:11  pierre
    * cmSourceWindowClosing resets Debugger LastSource field to avoid problems

  Revision 1.12  1999/02/08 17:43:44  pierre
    * RestDebugger or multiple running of debugged program now works
    + added DoContToCursor(F4)
    * Breakpoints are now inserted correctly (was mainlyy a problem
      of directories)

  Revision 1.11  1999/02/08 10:37:44  peter
    + html helpviewer

  Revision 1.7  1999/02/04 13:32:03  pierre
    * Several things added (I cannot commit them independently !)
    + added TBreakpoint and TBreakpointCollection
    + added cmResetDebugger,cmGrep,CmToggleBreakpoint
    + Breakpoint list in INIFile
    * Select items now also depend of SwitchMode
    * Reading of option '-g' was not possible !
    + added search for -Fu args pathes in TryToOpen
    + added code for automatic opening of FileDialog
      if source not found

  Revision 1.6  1999/02/02 16:41:39  peter
    + automatic .pas/.pp adding by opening of file
    * better debuggerscreen changes

  Revision 1.5  1999/01/22 18:13:22  pierre
   * DoneScreen Removed I did not find any such proc ??

  Revision 1.4  1999/01/22 10:24:03  peter
    * first debugger things

  Revision 1.3  1999/01/21 11:54:14  peter
    + tools menu
    + speedsearch in symbolbrowser
    * working run command

  Revision 1.2  1999/01/14 21:42:20  peter
    * source tracking from Gabor

  Revision 1.1  1999/01/12 14:29:34  peter
    + Implemented still missing 'switch' entries in Options menu
    + Pressing Ctrl-B sets ASCII mode in editor, after which keypresses (even
      ones with ASCII < 32 ; entered with Alt+<###>) are interpreted always as
      ASCII chars and inserted directly in the text.
    + Added symbol browser
    * splitted fp.pas to fpide.pas

  Revision 1.4  1999/01/04 11:49:41  peter
   * 'Use tab characters' now works correctly
   + Syntax highlight now acts on File|Save As...
   + Added a new class to syntax highlight: 'hex numbers'.
   * There was something very wrong with the palette managment. Now fixed.
   + Added output directory (-FE<xxx>) support to 'Directories' dialog...
   * Fixed some possible bugs in Running/Compiling, and the compilation/run
     process revised

  Revision 1.2  1998/12/28 15:47:40  peter
    + Added user screen support, display & window
    + Implemented Editor,Mouse Options dialog
    + Added location of .INI and .CFG file
    + Option (INI) file managment implemented (see bottom of Options Menu)
    + Switches updated
    + Run program

  Revision 1.3  1998/12/22 10:39:38  peter
    + options are now written/read
    + find and replace routines

}
