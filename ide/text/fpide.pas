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
  {$ifdef EDITORS}Editors,{$else}WEditor,WCEdit,{$endif}
  Comphook,Browcol,
  WHTMLScn,
  FPViews,FPSymbol,fpstring;

type
    TExecType = (exNormal,exNoSwap,exDosShell);

    TIDEApp = object(TApplication)
      constructor Init;
      procedure   InitDesktop; virtual;
      procedure   InitMenuBar; virtual;
      procedure   InitStatusLine; virtual;
      procedure   Open(FileName: string);
      function    OpenSearch(FileName: string) : boolean;
      function    AskSaveAll: boolean;
      function    SaveAll: boolean;
      function    AutoSave: boolean;
      procedure   Idle; virtual;
      procedure   Update;
      procedure   UpdateMode;
      procedure   UpdateTarget;
      procedure   GetEvent(var Event: TEvent); virtual;
      procedure   HandleEvent(var Event: TEvent); virtual;
      procedure   GetTileRect(var R: TRect); virtual;
      function    GetPalette: PPalette; virtual;
      procedure   DosShell; {virtual;}
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
      procedure DoShowRegisters;

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
      procedure CodeComplete;
      procedure CodeTemplates;
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

procedure PutEvent(TargetView: PView; E: TEvent);
procedure PutCommand(TargetView: PView; What, Command: Word; InfoPtr: Pointer);

var
  IDEApp: TIDEApp;

implementation

uses
{$ifdef linux}
  linux,
{$endif}
{$ifdef HasSignal}
  fpcatch,
{$endif HasSignal}
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
  FPDesk,FPCodCmp,FPCodTmp;

type
   TTargetedEvent = record
     Target: PView;
     Event: TEvent;
   end;

const
     TargetedEventHead   : integer = 0;
     TargetedEventTail   : integer = 0;
var
     TargetedEvents      : array[0..10] of TTargetedEvent;

function IncTargetedEventPtr(I: integer): integer;
begin
  Inc(I);
  if I>High(TargetedEvents) then I:=Low(TargetedEvents);
  IncTargetedEventPtr:=I;
end;

procedure PutEvent(TargetView: PView; E: TEvent);
begin
  if IncTargetedEventPtr(TargetedEventHead)=TargetedEventTail then Exit;
  with TargetedEvents[TargetedEventHead] do
  begin
    Target:=TargetView;
    Event:=E;
  end;
  TargetedEventHead:=IncTargetedEventPtr(TargetedEventHead);
end;

procedure PutCommand(TargetView: PView; What, Command: Word; InfoPtr: Pointer);
var E: TEvent;
begin
  FillChar(E,Sizeof(E),0);
  E.What:=What;
  E.Command:=Command;
  E.InfoPtr:=InfoPtr;
  PutEvent(TargetView,E);
end;

function GetTargetedEvent(var P: PView; var E: TEvent): boolean;
var OK: boolean;
begin
  OK:=TargetedEventHead<>TargetedEventTail;
  if OK then
  begin
    with TargetedEvents[TargetedEventTail] do
    begin
      P:=Target;
      E:=Event;
    end;
    TargetedEventTail:=IncTargetedEventPtr(TargetedEventTail);
  end;
  GetTargetedEvent:=OK;
end;

function IDEUseSyntaxHighlight(Editor: PFileEditor): boolean; {$ifndef FPC}far;{$endif}
begin
  IDEUseSyntaxHighlight:=(Editor^.FileName='') or MatchesFileList(NameAndExtOf(Editor^.FileName),HighlightExts);
end;

function IDEUseTabsPattern(Editor: PFileEditor): boolean; {$ifndef FPC}far;{$endif}
begin
  { the commented code lead all new files
    to become with TAB use enabled which is wrong in my opinion PM }
  IDEUseTabsPattern:={(Editor^.FileName='') or }MatchesFileList(NameAndExtOf(Editor^.FileName),TabsPattern);
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
  ClockView^.GrowMode:=gfGrowLoX+gfGrowHiX;
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
  if (StartupOptions and soHeapMonitor)=0 then HeapView^.Hide;
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
      NewItem(menu_edit_copywin,'', kbNoKey, cmCopyWin, hcCopyWin,
      NewItem(menu_edit_pastewin,'', kbNoKey, cmPasteWin, hcPasteWin,
      nil)));
{$endif WinClipSupported}
  MenuBar:=New(PAdvancedMenuBar, Init(R, NewMenu(
    NewSubMenu(menu_file,hcFileMenu, NewMenu(
      NewItem(menu_file_new,'',kbNoKey,cmNew,hcNew,
      NewItem(menu_file_template,'',kbNoKey,cmNewFromTemplate,hcNewFromTemplate,
      NewItem(menu_file_open,menu_key_file_open,kbF3,cmOpen,hcOpen,
      NewItem(menu_file_save,menu_key_file_save,kbF2,cmSave,hcSave,
      NewItem(menu_file_saveas,'',kbNoKey,cmSaveAs,hcSaveAs,
      NewItem(menu_file_saveall,'',kbNoKey,cmSaveAll,hcSaveAll,
      NewLine(
      NewItem(menu_file_changedir,'',kbNoKey,cmChangeDir,hcChangeDir,
      NewItem(menu_file_dosshell,'',kbNoKey,cmDOSShell,hcDOSShell,
      NewItem(menu_file_exit,menu_key_file_exit,kbNoKey,cmQuit,hcQuit,
      nil))))))))))),
    NewSubMenu(menu_edit,hcEditMenu, NewMenu(
      NewItem(menu_edit_undo,menu_key_edit_undo, kbAltBack, cmUndo, hcUndo,
      NewItem(menu_edit_redo,'', kbNoKey, cmRedo, hcRedo,
{$ifdef DebugUndo}
      NewItem('~D~ump Undo','', kbNoKey, cmDumpUndo, hcUndo,
      NewItem('U~n~do All','', kbNoKey, cmUndoAll, hcUndo,
      NewItem('R~e~do All','', kbNoKey, cmRedoAll, hcRedo,
{$endif DebugUndo}
      NewLine(
      NewItem(menu_edit_cut,menu_key_edit_cut, kbShiftDel, cmCut, hcCut,
      NewItem(menu_edit_copy,menu_key_edit_copy, kbCtrlIns, cmCopy, hcCut,
      NewItem(menu_edit_paste,menu_key_edit_paste, kbShiftIns, cmPaste, hcPaste,
      NewItem(menu_edit_clear,menu_key_edit_clear, kbCtrlDel, cmClear, hcClear,
      NewLine(
      NewItem(menu_edit_showclipboard,'', kbNoKey, cmShowClipboard, hcShowClipboard,
      WinPMI))))))
{$ifdef DebugUndo}))){$endif DebugUndo}
      )))),
    NewSubMenu(menu_search,hcSearchMenu, NewMenu(
      NewItem(menu_search_find,'', kbNoKey, cmFind, hcFind,
      NewItem(menu_search_replace,'', kbNoKey, cmReplace, hcReplace,
      NewItem(menu_search_searchagain,'', kbNoKey, cmSearchAgain, hcSearchAgain,
      NewLine(
      NewItem(menu_search_jumpline,'', kbNoKey, cmJumpLine, hcGotoLine,
      NewItem(menu_search_findproc,'', kbNoKey, cmFindProcedure, hcFindProcedure,
      NewLine(
      NewItem(menu_search_objects,'', kbNoKey, cmObjects, hcObjects,
      NewItem(menu_search_modules,'', kbNoKey, cmModules, hcModules,
      NewItem(menu_search_globals,'', kbNoKey, cmGlobals, hcGlobals,
      NewLine(
      NewItem(menu_search_symbol,'', kbNoKey, cmSymbol, hcSymbol,
      nil))))))))))))),
    NewSubMenu(menu_run,hcRunMenu, NewMenu(
      NewItem(menu_run_run,menu_key_run_run, kbCtrlF9, cmRun, hcRun,
      NewItem(menu_run_stepover,menu_key_run_stepover, kbF8, cmStepOver, hcRun,
      NewItem(menu_run_traceinto,menu_key_run_traceinto, kbF7, cmTraceInto, hcRun,
      NewItem(menu_run_conttocursor,menu_key_run_conttocursor, kbF4, cmContToCursor, hcContToCursor,
      NewItem(menu_run_untilreturn,'', kbNoKey,cmUntilReturn,hcUntilReturn,
      NewItem(menu_run_parameters,'', kbNoKey, cmParameters, hcParameters,
      NewItem(menu_run_resetdebugger,menu_key_run_resetdebugger, kbCtrlF2, cmResetDebugger, hcResetDebugger,
      nil)))))))),
    NewSubMenu(menu_compile,hcCompileMenu, NewMenu(
      NewItem(menu_compile_compile,menu_key_compile_compile, kbAltF9, cmCompile, hcCompile,
      NewItem(menu_compile_make,menu_key_compile_make, kbF9, cmMake, hcMake,
      NewItem(menu_compile_build,'', kbNoKey, cmBuild, hcBuild,
      NewLine(
      NewItem(menu_compile_target,'', kbNoKey, cmTarget, hcTarget,
      NewItem(menu_compile_primaryfile,'', kbNoKey, cmPrimaryFile, hcPrimaryFile,
      NewItem(menu_compile_clearprimaryfile,'', kbNoKey, cmClearPrimary, hcClearPrimary,
      NewLine(
      NewItem(menu_compile_information,'', kbNoKey, cmInformation, hcInformation,
      NewItem(menu_compile_compilermessages,menu_key_compile_compilermessages, kbF12, cmCompilerMessages, hcCompilerMessages,
      nil))))))))))),
    NewSubMenu(menu_debug, hcDebugMenu, NewMenu(
      NewItem(menu_debug_output,'', kbNoKey, cmUserScreenWindow, hcUserScreenWindow,
      NewItem(menu_debug_userscreen,menu_key_debug_userscreen, kbAltF5, cmUserScreen, hcUserScreen,
      NewItem(menu_debug_breakpoint,menu_key_debug_breakpoint, kbCtrlF8, cmToggleBreakpoint, hcToggleBreakpoint,
      NewItem(menu_debug_callstack,menu_key_debug_callstack, kbCtrlF3, cmStack, hcStack,
      NewItem(menu_debug_registers,'', kbNoKey, cmRegisters, hcRegisters,
      NewItem(menu_debug_addwatch,menu_key_debug_addwatch, kbCtrlF7, cmAddWatch, hcAddWatch,
      NewItem(menu_debug_watches,'', kbNoKey, cmWatches, hcWatches,
      NewItem(menu_debug_breakpointlist,'', kbNoKey, cmBreakpointList, hcBreakpointList,
      NewLine(
      NewItem(menu_debug_gdbwindow,'', kbNoKey, cmOpenGDBWindow, hcOpenGDBWindow,
      nil))))))))))),
    NewSubMenu(menu_tools, hcToolsMenu, NewMenu(
      NewItem(menu_tools_messages,menu_key_tools_messages, kbF11, cmToolsMessages, hcToolsMessages,
      NewItem(menu_tools_msgnext,menu_key_tools_msgnext, kbAltF8, cmToolsMsgNext, hcToolsMsgNext,
      NewItem(menu_tools_msgprev,menu_key_tools_msgprev, kbAltF7, cmToolsMsgPrev, hcToolsMsgPrev,
      NewLine(
      NewItem(menu_tools_grep,menu_key_tools_grep, kbShiftF2, cmGrep, hcGrep,
      NewItem(menu_tools_calculator, '', kbNoKey, cmCalculator, hcCalculator,
      NewItem(menu_tools_asciitable, '', kbNoKey, cmAsciiTable, hcAsciiTable,
      nil)))))))),
    NewSubMenu(menu_options, hcOptionsMenu, NewMenu(
      NewItem(menu_options_mode,'', kbNoKey, cmSwitchesMode, hcSwitchesMode,
      NewItem(menu_options_compiler,'', kbNoKey, cmCompiler, hcCompiler,
      NewItem(menu_options_memory,'', kbNoKey, cmMemorySizes, hcMemorySizes,
      NewItem(menu_options_linker,'', kbNoKey, cmLinker, hcLinker,
      NewItem(menu_options_debugger,'', kbNoKey, cmDebugger, hcDebugger,
      NewItem(menu_options_directories,'', kbNoKey, cmDirectories, hcDirectories,
      NewItem(menu_options_browser,'',kbNoKey, cmBrowser, hcBrowser,
      NewItem(menu_options_tools,'', kbNoKey, cmTools, hcTools,
      NewLine(
      NewSubMenu(menu_options_env, hcEnvironmentMenu, NewMenu(
        NewItem(menu_options_env_preferences,'', kbNoKey, cmPreferences, hcPreferences,
        NewItem(menu_options_env_editor,'', kbNoKey, cmEditor, hcEditor,
        NewItem(menu_options_env_codecomplete,'', kbNoKey, cmCodeCompleteOptions, hcCodeCompleteOptions,
        NewItem(menu_options_env_codetemplates,'', kbNoKey, cmCodeTemplateOptions, hcCodeTemplateOptions,
        NewItem(menu_options_env_desktop,'', kbNoKey, cmDesktopOptions, hcDesktopOptions,
        NewItem(menu_options_env_mouse,'', kbNoKey, cmMouse, hcMouse,
        NewItem(menu_options_env_startup,'', kbNoKey, cmStartup, hcStartup,
        NewItem(menu_options_env_colors,'', kbNoKey, cmColors, hcColors,
        nil))))))))),
      NewLine(
      NewItem(menu_options_open,'', kbNoKey, cmOpenINI, hcOpenINI,
      NewItem(menu_options_save,'', kbNoKey, cmSaveINI, hcSaveINI,
      NewItem(menu_options_saveas,'', kbNoKey, cmSaveAsINI, hcSaveAsINI,
      nil))))))))))))))),
    NewSubMenu(menu_window, hcWindowMenu, NewMenu(
      NewItem(menu_window_tile,'', kbNoKey, cmTile, hcTile,
      NewItem(menu_window_cascade,'', kbNoKey, cmCascade, hcCascade,
      NewItem(menu_window_closeall,'', kbNoKey, cmCloseAll, hcCloseAll,
      NewLine(
      NewItem(menu_window_resize,menu_key_window_resize, kbCtrlF5, cmResize, hcResize,
      NewItem(menu_window_zoom,menu_key_window_zoom, kbF5, cmZoom, hcZoom,
      NewItem(menu_window_next,menu_key_window_next, kbF6, cmNext, hcNext,
      NewItem(menu_window_previous,menu_key_window_previous, kbShiftF6, cmPrev, hcPrev,
      NewItem(menu_window_close,menu_key_window_close, kbAltF3, cmClose, hcClose,
      NewLine(
      NewItem(menu_window_list,menu_key_window_list, kbAlt0, cmWindowList, hcWindowList,
      NewItem(menu_window_update,'', kbNoKey, cmUpdate, hcUpdate,
      nil))))))))))))),
    NewSubMenu(menu_help, hcHelpMenu, NewMenu(
      NewItem(menu_help_contents,'', kbNoKey, cmHelpContents, hcHelpContents,
      NewItem(menu_help_index,menu_key_help_helpindex, kbShiftF1, cmHelpIndex, hcHelpIndex,
      NewItem(menu_help_topicsearch,menu_key_help_topicsearch, kbCtrlF1, cmHelpTopicSearch, hcHelpTopicSearch,
      NewItem(menu_help_prevtopic,menu_key_help_prevtopic, kbAltF1, cmHelpPrevTopic, hcHelpPrevTopic,
      NewItem(menu_help_using,'',kbNoKey, cmHelpUsingHelp, hcHelpUsingHelp,
      NewItem(menu_help_files,'',kbNoKey, cmHelpFiles, hcHelpFiles,
      NewLine(
      NewItem(menu_help_about,'',kbNoKey, cmAbout, hcAbout,
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
      NewStatusKey(status_help, kbF1, cmHelp,
      StdStatusKeys(
      nil)),
    NewStatusDef(hcHelpWindow, hcHelpWindow,
      NewStatusKey(status_help_on_help, kbF1, cmHelpUsingHelp,
      NewStatusKey(status_help_previoustopic, kbAltF1, cmHelpPrevTopic,
      NewStatusKey(status_help_index, kbShiftF1, cmHelpIndex,
      NewStatusKey(status_help_close, kbEsc, cmClose,
      StdStatusKeys(
      nil))))),
    NewStatusDef(hcSourceWindow, hcSourceWindow,
      NewStatusKey(status_help, kbF1, cmHelp,
      NewStatusKey(status_save, kbF2, cmSave,
      NewStatusKey(status_open, kbF3, cmOpen,
      NewStatusKey(status_compile, kbAltF9, cmCompile,
      NewStatusKey(status_make, kbF9, cmMake,
      NewStatusKey(status_localmenu, kbAltF10, cmLocalMenu,
      StdStatusKeys(
      nil))))))),
    NewStatusDef(hcASCIITableWindow, hcASCIITableWindow,
      NewStatusKey(status_help, kbF1, cmHelp,
      NewStatusKey(status_transferchar, kbCtrlEnter, cmTransfer,
      StdStatusKeys(
      nil))),
    NewStatusDef(hcMessagesWindow, hcMessagesWindow,
      NewStatusKey(status_help, kbF1, cmHelp,
      NewStatusKey(status_msggotosource, kbEnter, cmMsgGotoSource,
      NewStatusKey(status_msgtracksource, kbNoKey, cmMsgTrackSource,
      NewStatusKey(status_localmenu, kbAltF10, cmLocalMenu,
      NewStatusKey('', kbEsc, cmClose,
      StdStatusKeys(
      nil)))))),
    NewStatusDef(hcCalcWindow, hcCalcWindow,
      NewStatusKey(status_help, kbF1, cmHelp,
      NewStatusKey(status_close, kbEsc, cmClose,
      NewStatusKey(status_calculatorpaste, kbCtrlEnter, cmCalculatorPaste,
      StdStatusKeys(
      nil)))),
    NewStatusDef(0, $FFFF,
      NewStatusKey(status_help, kbF1, cmHelp,
      NewStatusKey(status_open, kbF3, cmOpen,
      NewStatusKey(status_compile, kbAltF9, cmCompile,
      NewStatusKey(status_make, kbF9, cmMake,
      NewStatusKey(status_localmenu, kbAltF10, cmLocalMenu,
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
var P: PView;
begin
  { first of all dispatch queued targeted events }
  while GetTargetedEvent(P,Event) do
    P^.HandleEvent(Event);
  inherited GetEvent(Event);
  if Event.What<>evNothing then
    LastEvent:=GetDosTicks
  else
    if abs(GetDosTicks-LastEvent)>SleepTimeOut then
      GiveUpTimeSlice;
end;

procedure TIDEApp.HandleEvent(var Event: TEvent);
var DontClear: boolean;
{$ifdef HasSignal}
    CtrlCCatched : boolean;
{$endif HasSignal}
begin
{$ifdef HasSignal}
  if (Event.What=evKeyDown) and (Event.keyCode=kbCtrlC) and
     (CtrlCPressed) then
    begin
      CtrlCCatched:=true;
{$ifdef DEBUG}
      Writeln(stderr,'One CtrlC caught');
{$endif DEBUG}
    end
  else
    CtrlCCatched:=false;
{$endif HasSignal}
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
             cmRegisters     : DoShowRegisters;
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
             cmCodeTemplateOptions: CodeTemplates;
             cmCodeCompleteOptions: CodeComplete;
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
{$ifdef HasSignal}
  { Reset flag if CrtlC was handled }
  if CtrlCCatched and (Event.What=evNothing) then
    begin
      CtrlCPressed:=false;
{$ifdef DEBUG}
      Writeln(stderr,'One CtrlC handled');
{$endif DEBUG}
    end;
{$endif HasSignal}
end;


procedure TIDEApp.GetTileRect(var R: TRect);
begin
  Desktop^.GetExtent(R);
{ Leave the compiler messages window in the bottom }
  if assigned(CompilerMessageWindow) and (CompilerMessageWindow^.GetState(sfVisible)) then
   R.B.Y:=Min(CompilerMessageWindow^.Origin.Y,R.B.Y);
{ Leave the messages window in the bottom }
  if assigned(MessagesWindow) and (MessagesWindow^.GetState(sfVisible)) then
   R.B.Y:=Min(MessagesWindow^.Origin.Y,R.B.Y);
{ Leave the watch window in the bottom }
  if assigned(WatchesWindow) and (WatchesWindow^.GetState(sfVisible)) then
   R.B.Y:=Min(WatchesWindow^.Origin.Y,R.B.Y);
end;


{****************************************************************************
                                 Switch Screens
****************************************************************************}

procedure TIDEApp.ShowUserScreen;
begin
  DoneSysError;
  DoneEvents;
  DoneKeyboard;
  If UseMouse then
    DoneMouse
  else
    ButtonCount:=0;
{$ifndef go32v2}
  DoneScreen; { this is available in FV app.pas (PFV) }
{$endif go32v2}
  DoneDosMem;

  if Assigned(UserScreen) then
    UserScreen^.SwitchTo;
end;


procedure TIDEApp.ShowIDEScreen;
begin
  if Assigned(UserScreen) then
    UserScreen^.SwitchBack;

  InitDosMem;
{$ifndef go32v2}
  InitScreen;
{$endif go32v2}
  InitKeyboard;
  If UseMouse then
    InitMouse
  else
    ButtonCount:=0;
  InitEvents;
  InitSysError;
  CurDirChanged;
  Message(Application,evBroadcast,cmUpdate,nil);
{$ifndef go32v2}
  UpdateScreen(true);
{$endif go32v2}
end;

function TIDEApp.AutoSave: boolean;
var IOK,SOK,DOK: boolean;
begin
  IOK:=true; SOK:=true; DOK:=true;
  if (AutoSaveOptions and asEnvironment)<>0 then
    begin
      IOK:=WriteINIFile;
      if IOK=false then
        ErrorBox(error_saving_cfg_file,nil);
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
        ErrorBox(error_saving_dsk_file,nil);
    end;
  AutoSave:=IOK and SOK and DOK;
end;

function TIDEApp.DoExecute(ProgramPath, Params, InFile,OutFile: string; ExecType: TExecType): boolean;
var CanRun: boolean;
    posexe : longint;
begin
  SaveCancelled:=false;
  CanRun:=AutoSave;
  if (CanRun=false) and (SaveCancelled=false) then
    CanRun:=true; { do not care about .DSK or .INI saving problems - just like TP }
  if CanRun then
  begin
    if UserScreen=nil then
     begin
       ErrorBox(error_user_screen_not_avail,nil);
       Exit;
     end;

    if ExecType<>exNoSwap then
      ShowUserScreen;

    if ExecType=exDosShell then
      WriteShellMsg;

  {$ifdef linux}
    Shell(ProgramPath+' '+Params);
  {$else}
    { DO NOT use COMSPEC for exe files as the
      ExitCode is lost in those cases PM }

    posexe:=Pos('.EXE',UpCaseStr(ProgramPath));
    { if programpath was three char long => bug }
    if (posexe>0) and (posexe=Length(ProgramPath)-3) then
      begin
        if (InFile='') and (OutFile='') then
          DosExecute(ProgramPath,Params)
        else
          ExecuteRedir(ProgramPath,Params,InFile,OutFile,'stderr');
      end
    else if (InFile='') and (OutFile='') then
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
  SetMenuItemParam(SearchMenuItem(MenuBar^.Menu,cmSaveINI),SmartPath(IniFileName));
end;

procedure TIDEApp.UpdateRecentFileList;
var P: PMenuItem;
    {ID,}I: word;
    FileMenu: PMenuItem;
begin
{  ID:=cmRecentFileBase;}
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
{    ID,}I: word;
    ToolsMenu: PMenuItem;
    S1,S2,S3: string;
    W: word;
begin
{  ID:=cmToolsBase;}
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
  Revision 1.62  2000-06-11 07:01:33  peter
    * give watches window also a number
    * leave watches window in the bottom when cascading windows

  Revision 1.61  2000/05/17 09:51:11  pierre
   Disable/Enable keyboard on UserScreen

  Revision 1.60  2000/05/02 10:20:40  pierre
   * fix a small problem when deciding to call DosExecute directly

  Revision 1.59  2000/05/02 08:42:27  pierre
   * new set of Gabor changes: see fixes.txt

  Revision 1.58  2000/04/25 08:42:33  pierre
   * New Gabor changes : see fixes.txt

  Revision 1.57  2000/04/18 11:42:37  pierre
   lot of Gabor changes : see fixes.txt

  Revision 1.56  2000/03/21 23:30:49  pierre
   adapted to wcedit addition by Gabor

  Revision 1.55  2000/03/13 20:41:35  pierre
    + option -S to disable the mouse
    * adapted to changes in fpusrscr for DOS

  Revision 1.54  2000/03/07 21:57:59  pierre
    + CtrlC handling
    + UpdateMode method

  Revision 1.53  2000/03/06 11:31:30  pierre
    * Do not use COMSPEC to Run files with .EXE suffix
      because Command.com at least does not return the errorcode
      of the program called

  Revision 1.52  2000/02/07 12:02:32  pierre
   Gabor's changes

  Revision 1.51  2000/01/23 21:25:17  florian
    + start of internationalization support

  Revision 1.50  2000/01/08 18:26:20  florian
    + added a register window, doesn't work yet

  Revision 1.49  2000/01/05 00:31:50  pierre
   * avoid new files to use TABS

  Revision 1.48  2000/01/03 11:38:33  michael
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