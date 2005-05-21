{
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
  Objects,Drivers,Views,App,Gadgets,MsgBox,Tabs,
  WEditor,WCEdit,
  Comphook,Browcol,
  WHTMLScn,
  FPViews,FPSymbol,fpstring;

type
    TExecType = (exNormal,exNoSwap,exDosShell);

    TIDEApp = object(TApplication)
      IsRunning : boolean;
      constructor Init;
      procedure   InitDesktop; virtual;
      procedure   InitMenuBar; virtual;
      procedure   InitStatusLine; virtual;
      procedure   Open(FileName: string;FileDir:string);
      function    OpenSearch(FileName: string) : boolean;
      function    AskSaveAll: boolean;
      function    SaveAll: boolean;
      function    AutoSave: boolean;
      procedure   Idle; virtual;
      procedure   Update;
      procedure   UpdateMode;
      procedure   UpdateRunMenu(DebuggeeRunning : boolean);
      procedure   UpdateTarget;
      procedure   GetEvent(var Event: TEvent); virtual;
      procedure   HandleEvent(var Event: TEvent); virtual;
      procedure   GetTileRect(var R: TRect); virtual;
      function    GetPalette: PPalette; virtual;
      procedure   DosShell; {virtual;}
      procedure   ShowReadme;
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
      procedure RunDir;
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
      procedure DoShowDisassembly;
      procedure DoShowBreakpointList;
      procedure DoShowWatches;
      procedure DoAddWatch;
      procedure DoShowRegisters;
      procedure DoShowFPU;
      procedure DoShowVector;
      function  AskRecompileIfModified:boolean;
      procedure Messages;
      procedure Calculator;
      procedure DoAsciiTable;
      procedure ExecuteTool(Idx: integer);
      procedure SetSwitchesMode;
      procedure DoCompilerSwitch;
      procedure MemorySizes;
      procedure DoLinkerSwitch;
      procedure DoDebuggerSwitch;
{$ifdef SUPPORT_REMOTE}
      procedure DoRemote;
      procedure TransferRemote;
{$endif SUPPORT_REMOTE}
      procedure Directories;
      procedure Tools;
      procedure DoGrep;
      procedure Preferences;
      procedure EditorOptions(Editor: PEditor);
      procedure CodeComplete;
      procedure CodeTemplates;
      procedure BrowserOptions(Browser: PBrowserWindow);
      procedure DesktopOptions;
      procedure ResizeApplication(x, y : longint);
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
      procedure CreateAnsiFile;
    public
      procedure SourceWindowClosed;
      function  DoExecute(ProgramPath, Params, InFile, OutFile, ErrFile: string; ExecType: TExecType): boolean;
    private
      SaveCancelled: boolean;
      InsideDone : boolean;
      LastEvent: longint;
      procedure AddRecentFile(AFileName: string; CurX, CurY: sw_integer);
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
{$ifdef HasSignal}
  fpcatch,
{$endif HasSignal}
{$ifdef WinClipSupported}
  WinClip,
{$endif WinClipSupported}
{$ifdef Unix}
  fpKeys,
{$endif Unix}
  FpDpAnsi,WConsts,
  Video,Mouse,Keyboard,
  Compiler,Version,
  FVConsts,
  Dos,Memory,Menus,Dialogs,StdDlg,timeddlg,
  Systems,
  WUtils,WHlpView,WViews,WHTMLHlp,WHelp,WConsole,
  FPConst,FPVars,FPUtils,FPSwitch,FPIni,FPIntf,FPCompil,FPHelp,
  FPTemplt,FPCalc,FPUsrScr,FPTools,
{$ifndef NODEBUG}
  FPDebug,FPRegs,
{$endif}
  FPRedir,
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
  UseSyntaxHighlight:=@IDEUseSyntaxHighlight;
  UseTabsPattern:=@IDEUseTabsPattern;
  inherited Init;
  InitAdvMsgBox;
  InsideDone:=false;
  IsRunning:=true;
  MenuBar^.GetBounds(R); R.A.X:=R.B.X-8;
  New(ClockView, Init(R));
  ClockView^.GrowMode:=gfGrowLoX+gfGrowHiX;
  Application^.Insert(ClockView);
  New(ClipboardWindow, Init);
  Desktop^.Insert(ClipboardWindow);
  New(CalcWindow, Init); CalcWindow^.Hide;
  Desktop^.Insert(CalcWindow);
  New(CompilerMessageWindow, Init);
  CompilerMessageWindow^.Hide;
  Desktop^.Insert(CompilerMessageWindow);
  Message(@Self,evBroadcast,cmUpdate,nil);
  CurDirChanged;
  { heap viewer }
  GetExtent(R); Dec(R.B.X); R.A.X:=R.B.X-9; R.A.Y:=R.B.Y-1;
  New(HeapView, InitKb(R));
  if (StartupOptions and soHeapMonitor)=0 then HeapView^.Hide;
  Insert(HeapView);
  Drivers.ShowMouse;
{$ifdef win32}
  // Win32ShowMouse;
{$endif win32}
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
      NewItem(menu_file_reload,'',kbNoKey,cmDoReload,hcDoReload,
      NewItem(menu_file_save,menu_key_file_save,kbF2,cmSave,hcSave,
      NewItem(menu_file_saveas,'',kbNoKey,cmSaveAs,hcSaveAs,
      NewItem(menu_file_saveall,'',kbNoKey,cmSaveAll,hcSaveAll,
      NewLine(
      NewItem(menu_file_changedir,'',kbNoKey,cmChangeDir,hcChangeDir,
      NewItem(menu_file_dosshell,'',kbNoKey,cmDOSShell,hcDOSShell,
      NewItem(menu_file_exit,menu_key_file_exit,kbNoKey,cmQuit,hcQuit,
      nil)))))))))))),
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
      NewItem(menu_run_rundir,'', kbNoKey, cmRunDir, hcRunDir,
      NewItem(menu_run_parameters,'', kbNoKey, cmParameters, hcParameters,
      NewItem(menu_run_resetdebugger,menu_key_run_resetdebugger, kbCtrlF2, cmResetDebugger, hcResetDebugger,
      nil))))))))),
    NewSubMenu(menu_compile,hcCompileMenu, NewMenu(
      NewItem(menu_compile_compile,menu_key_compile_compile, kbAltF9, cmCompile, hcCompile,
      NewItem(menu_compile_make,menu_key_compile_make, kbF9, cmMake, hcMake,
      NewItem(menu_compile_build,'', kbNoKey, cmBuild, hcBuild,
      NewLine(
      NewItem(menu_compile_target,'', kbNoKey, cmTarget, hcTarget,
      NewItem(menu_compile_primaryfile,'', kbNoKey, cmPrimaryFile, hcPrimaryFile,
      NewItem(menu_compile_clearprimaryfile,'', kbNoKey, cmClearPrimary, hcClearPrimary,
      NewLine(
      NewItem(menu_compile_compilermessages,menu_key_compile_compilermessages, kbF12, cmCompilerMessages, hcCompilerMessages,
      nil)))))))))),
    NewSubMenu(menu_debug, hcDebugMenu, NewMenu(
      NewItem(menu_debug_output,'', kbNoKey, cmUserScreenWindow, hcUserScreenWindow,
      NewItem(menu_debug_userscreen,menu_key_debug_userscreen, kbAltF5, cmUserScreen, hcUserScreen,
      NewLine(
{$ifdef SUPPORT_REMOTE}
      NewItem(menu_debug_remote,'', kbNoKey, cmTransferRemote, hcTransferRemote,
{$endif SUPPORT_REMOTE}
      NewItem(menu_debug_addwatch,menu_key_debug_addwatch, kbCtrlF7, cmAddWatch, hcAddWatch,
      NewItem(menu_debug_watches,'', kbNoKey, cmWatches, hcWatchesWindow,
      NewItem(menu_debug_breakpoint,menu_key_debug_breakpoint, kbCtrlF8, cmToggleBreakpoint, hcToggleBreakpoint,
      NewItem(menu_debug_breakpointlist,'', kbNoKey, cmBreakpointList, hcBreakpointList,
      NewItem(menu_debug_callstack,menu_key_debug_callstack, kbCtrlF3, cmStack, hcStackWindow,
      NewLine(
      NewItem(menu_debug_disassemble,'', kbNoKey, cmDisassemble, hcStackWindow,
      NewItem(menu_debug_registers,'', kbNoKey, cmRegisters, hcRegistersWindow,
      NewItem(menu_debug_fpu_registers,'', kbNoKey, cmFPURegisters, hcFPURegisters,
      NewItem(menu_debug_vector_registers,'', kbNoKey, cmVectorRegisters, hcVectorRegisters,
      NewLine(
      NewItem(menu_debug_gdbwindow,'', kbNoKey, cmOpenGDBWindow, hcOpenGDBWindow,
      nil
{$ifdef SUPPORT_REMOTE}
      )
{$endif SUPPORT_REMOTE}
      )))))))))))))))),
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
{$ifdef SUPPORT_REMOTE}
      NewItem(menu_options_remote,'', kbNoKey, cmRemoteDialog, hcRemoteDialog,
{$endif SUPPORT_REMOTE}
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
{$ifdef Unix}
        NewItem(menu_options_learn_keys,'', kbNoKey, cmKeys, hcKeys,
{$endif Unix}
        nil
{$ifdef Unix}
        )
{$endif Unix}
        ))))))))),
      NewLine(
      NewItem(menu_options_open,'', kbNoKey, cmOpenINI, hcOpenINI,
      NewItem(menu_options_save,'', kbNoKey, cmSaveINI, hcSaveINI,
      NewItem(menu_options_saveas,'', kbNoKey, cmSaveAsINI, hcSaveAsINI,
      nil
{$ifdef SUPPORT_REMOTE}
      )
{$endif SUPPORT_REMOTE}
      ))))))))))))))),
    NewSubMenu(menu_window, hcWindowMenu, NewMenu(
      NewItem(menu_window_tile,'', kbNoKey, cmTile, hcTile,
      NewItem(menu_window_cascade,'', kbNoKey, cmCascade, hcCascade,
      NewItem(menu_window_closeall,'', kbNoKey, cmCloseAll, hcCloseAll,
      NewLine(
      NewItem(menu_window_resize,menu_key_window_resize, kbCtrlF5, cmResize, hcResize,
      NewItem(menu_window_zoom,menu_key_window_zoom, kbF5, cmZoom, hcZoom,
      NewItem(menu_window_next,menu_key_window_next, kbF6, cmNext, hcNext,
      NewItem(menu_window_previous,menu_key_window_previous, kbShiftF6, cmPrev, hcPrev,
      NewItem(menu_window_hide,menu_key_window_hide, kbCtrlF6, cmHide, hcHide,
      NewItem(menu_window_close,menu_key_window_close, kbAltF3, cmClose, hcClose,
      NewLine(
      NewItem(menu_window_list,menu_key_window_list, kbAlt0, cmWindowList, hcWindowList,
      NewItem(menu_window_update,'', kbNoKey, cmUpdate, hcUpdate,
      nil)))))))))))))),
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
  // Update; Desktop is still nil at that point ...
end;

procedure TIDEApp.InitStatusLine;
var
  R: TRect;
begin
  GetExtent(R);
  R.A.Y := R.B.Y - 1;
  StatusLine:=New(PIDEStatusLine, Init(R,
    NewStatusDef(hcDragging, hcDragging,
      NewStatusKey(status_help, kbF1, cmHelp,
      StdStatusKeys(
      NewStatusKey('~Cursor~ Move', kbNoKey, 65535,
      NewStatusKey('~Shift+Cursor~ Size', kbNoKey, 65535,
      NewStatusKey('~<ды~ Done', kbNoKey, 65535,
      NewStatusKey('~Esc~ Cancel', kbNoKey, 65535,
      nil)))))),
    NewStatusDef(hcStackWindow, hcStackWindow,
      NewStatusKey(status_help, kbF1, cmHelp,
      NewStatusKey(status_disassemble, kbAltI, cmDisassemble,
      StdStatusKeys(
      nil))),
    NewStatusDef(hcFirstCommand, hcLastNormalCommand,
      NewStatusKey(status_help, kbF1, cmHelp,
      StdStatusKeys(
      nil)),
    NewStatusDef(hcFirstNoAltXCommand, hcLastCommand,
      NewStatusKey(status_help, kbF1, cmHelp,
      NewStatusKey('', kbF10, cmMenu,
      NewStatusKey('', kbAltF3, cmClose,
      NewStatusKey('', kbF5, cmZoom,
      NewStatusKey('', kbCtrlF5, cmResize,
      NewStatusKey('', kbF6, cmNext,
      NewStatusKey('', kbShiftF6, cmPrev,
      nil))))))),
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
      StdStatusKeys
      (
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
    nil))))))))))));
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
  { Handle System events directly }
  Drivers.GetSystemEvent(Event);         { Load system event }
  If (Event.What <> evNothing) Then
    HandleEvent(Event);

  inherited GetEvent(Event);
{$ifdef DEBUG}
  if (Event.What=evKeyDown) and (Event.KeyCode=kbAltF11) then
    begin
{$ifdef HasSignal}
      Generate_SIGSEGV;
{$else}
      Halt(1);
{$endif}
    end;
  if (Event.What=evKeyDown) and (Event.KeyCode=kbCtrlF11) then
    begin
      RunError(250);
    end;
{$endif DEBUG}
  if (Event.What=evKeyDown) and (Event.KeyCode=kbAltF12) then
    begin
      CreateAnsiFile;
      ClearEvent(Event);
    end;
  if Event.What<>evNothing then
    LastEvent:=GetDosTicks
  else
    begin
      if abs(GetDosTicks-LastEvent)>SleepTimeOut then
        GiveUpTimeSlice;
    end;
end;

procedure TIDEApp.HandleEvent(var Event: TEvent);
var DontClear: boolean;
    TempS: string;
    ForceDlg: boolean;
    W  : PSourceWindow;
    DS : DirStr;
    NS : NameStr;
    ES : ExtStr;
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
      Writeln(stderr,'One Ctrl-C caught');
{$endif DEBUG}
    end
  else
    CtrlCCatched:=false;
{$endif HasSignal}
  case Event.What of
       evKeyDown :
         begin
           DontClear:=true;
           { just for debugging purposes }
         end;
       evCommand :
         begin
           DontClear:=false;
           case Event.Command of
             cmUpdate        : Message(Application,evBroadcast,cmUpdate,nil);
           { -- File menu -- }
             cmNew           : NewEditor;
             cmNewFromTemplate: NewFromTemplate;
             cmOpen          : begin
                                 ForceDlg:=false;
                                 if (OpenFileName<>'') and
                                    ((DirOf(OpenFileName)='') or (Pos(ListSeparator,OpenFileName)<>0)) then
                                   begin
                                     TempS:=LocateSourceFile(OpenFileName,false);
                                     if TempS='' then
                                       ForceDlg:=true
                                     else
                                       OpenFileName:=TempS;
                                   end;
                                 if ForceDlg then
                                   OpenSearch(OpenFileName)
                                 else
                                   begin
                                     W:=LastSourceEditor;
                                     if assigned(W) then
                                       FSplit(W^.Editor^.FileName,DS,NS,ES)
                                     else
                                       DS:='';
                                     Open(OpenFileName,DS);
                                   end;
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
             cmRunDir        : RunDir;
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
             cmCompilerMessages : DoCompilerMessages;
           { -- Debug menu -- }
             cmUserScreen    : DoUserScreen;
             cmToggleBreakpoint : DoToggleBreak;
             cmStack         : DoShowCallStack;
             cmDisassemble   : DoShowDisassembly;
             cmBreakpointList : DoShowBreakpointList;
             cmWatches       :  DoShowWatches;
             cmAddWatch      :  DoAddWatch;
             cmOpenGDBWindow : DoOpenGDBWindow;
             cmRegisters     : DoShowRegisters;
             cmFPURegisters     : DoShowFPU;
             cmVectorRegisters : DoShowVector;
           { -- Options menu -- }
             cmSwitchesMode  : SetSwitchesMode;
             cmCompiler      : DoCompilerSwitch;
             cmMemorySizes   : MemorySizes;
             cmLinker        : DoLinkerSwitch;
             cmDebugger      : DoDebuggerSwitch;
{$ifdef SUPPORT_REMOTE}
             cmRemoteDialog  : DoRemote;
             cmTransferRemote: TransferRemote;
{$endif SUPPORT_REMOTE}
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
{$ifdef Unix}
             cmKeys          : LearnKeysDialog;
{$endif Unix}
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
             cmHelp,
             cmHelpContents  : HelpContents;
             cmHelpIndex     : HelpHelpIndex;
{             cmHelpTopicSearch: HelpTopicSearch;}
             cmHelpPrevTopic : HelpPrevTopic;
             cmHelpUsingHelp : HelpUsingHelp;
             cmHelpFiles     : HelpFiles;
             cmAbout         : About;
             cmShowReadme    : ShowReadme;
             cmResizeApp     : ResizeApplication(Event.Id, Event.InfoWord);
             cmQuitApp       : Message(@Self, evCommand, cmQuit, nil);
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
{$ifndef NODEBUG}
{ Leave the watch window in the bottom }
  if assigned(WatchesWindow) and (WatchesWindow^.GetState(sfVisible)) then
   R.B.Y:=Min(WatchesWindow^.Origin.Y,R.B.Y);
{$endif NODEBUG}
end;


{****************************************************************************
                                 Switch Screens
****************************************************************************}

procedure TIDEApp.ShowUserScreen;
begin
  if Assigned(UserScreen) then
    UserScreen^.SaveIDEScreen;
  DoneSysError;
  DoneEvents;
  { DoneKeyboard should be called last to
    restore the keyboard correctly PM }
{$ifndef go32v2}
  DoneScreen;
{$endif ndef go32v2}
  DoneKeyboard;
  If UseMouse then
    DoneMouse
  else
    ButtonCount:=0;
  DoneDosMem;

  if Assigned(UserScreen) then
    UserScreen^.SwitchToConsoleScreen;
end;


procedure TIDEApp.ShowIDEScreen;
begin
  if Assigned(UserScreen) then
    UserScreen^.SaveConsoleScreen;
  InitDosMem;
  InitKeyboard;
  If UseMouse then
    InitMouse
  else
    ButtonCount:=0;
{$ifndef go32v2}
  InitScreen;
{$endif ndef go32v2}
{$ifdef win32}
  { write the empty screen to dummy console handle }
  UpdateScreen(true);
{$endif ndef win32}
  InitEvents;
  InitSysError;
  CurDirChanged;
{$ifndef win32}
  Message(Application,evBroadcast,cmUpdate,nil);
{$endif win32}
{$ifdef win32}
  // Win32ShowMouse;
{$endif win32}

  if Assigned(UserScreen) then
    UserScreen^.SwitchBackToIDEScreen;
{$ifdef win32}
  { This message was sent when the VideoBuffer was smaller
    than was the IdeApp thought => writes to random memory and random crashes... PM }
  Message(Application,evBroadcast,cmUpdate,nil);
{$endif win32}
{$ifdef Unix}
  SetKnownKeys;
{$endif Unix}
{$ifndef win32}
{$ifndef go32v2}
  UpdateScreen(true);
{$endif go32v2}
{$endif win32}
end;

function TIDEApp.AutoSave: boolean;
var IOK,SOK,DOK: boolean;
begin
  IOK:=true; SOK:=true; DOK:=true;
  if (AutoSaveOptions and asEnvironment)<>0 then
    begin
      IOK:=WriteINIFile(false);
      if IOK=false then
        ErrorBox(error_saving_cfg_file,nil);
    end;
  if (AutoSaveOptions and asEditorFiles)<>0 then { was a typo here ("=0") - Gabor }
      SOK:=SaveAll;
  if (AutoSaveOptions and asDesktop)<>0 then
    begin
      { destory all help & browser windows - we don't want to store them }
      { UserScreenWindow is also not registered PM }
      DoCloseUserScreenWindow;
      {$IFNDEF NODEBUG}
      DoneDisassemblyWindow;
      {$ENDIF}
      CloseHelpWindows;
      CloseAllBrowsers;
      DOK:=SaveDesktop;
      if DOK=false then
        ErrorBox(error_saving_dsk_file,nil);
    end;
  AutoSave:=IOK and SOK and DOK;
end;

function TIDEApp.DoExecute(ProgramPath, Params, InFile,OutFile,ErrFile: string; ExecType: TExecType): boolean;
var CanRun: boolean;
    ConsoleMode : TConsoleMode;
{$ifndef Unix}
    PosExe: sw_integer;
{$endif Unix}
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
    SaveConsoleMode(ConsoleMode);

    if ExecType=exDosShell then
      WriteShellMsg
    else if ExecType<>exNoSwap then
      Writeln('Running "'+ProgramPath+' '+Params+'"');
     { DO NOT use COMSPEC for exe files as the
      ExitCode is lost in those cases PM }

{$ifndef Unix}
    posexe:=Pos('.EXE',UpCaseStr(ProgramPath));
    { if programpath was three char long => bug }
    if (posexe>0) and (posexe=Length(ProgramPath)-3) then
      begin
{$endif Unix}
        if (InFile='') and (OutFile='') and (ErrFile='') then
          DosExecute(ProgramPath,Params)
        else
          begin
            if ErrFile='' then
              ErrFile:='stderr';
            ExecuteRedir(ProgramPath,Params,InFile,OutFile,ErrFile);
          end;
{$ifndef Unix}
      end
    else if (InFile='') and (OutFile='') and (ErrFile='') then
      DosExecute(GetEnv('COMSPEC'),'/C '+ProgramPath+' '+Params)
    else
      begin
        if ErrFile='' then
          ErrFile:='stderr';
        ExecuteRedir(GetEnv('COMSPEC'),'/C '+ProgramPath+' '+Params,
          InFile,OutFile,ErrFile);
     end;
{$endif Unix}

{$ifdef Unix}
    if (DebuggeeTTY='') and (OutFile='') and (ExecType<>exDosShell) then
      begin
        Write(' Press any key to return to IDE');
        InitKeyBoard;
        Keyboard.GetKeyEvent;
        while (Keyboard.PollKeyEvent<>0) do
         Keyboard.GetKeyEvent;
        DoneKeyboard;
      end;
{$endif}
    RestoreConsoleMode(ConsoleMode);
    if ExecType<>exNoSwap then
      ShowIDEScreen;
  end;
  DoExecute:=CanRun;
end;


procedure TIDEApp.Update;
begin
  SetCmdState([cmSaveAll],IsThereAnyEditor);
  SetCmdState([cmCloseAll,cmWindowList],IsThereAnyWindow);
  SetCmdState([cmTile,cmCascade],IsThereAnyVisibleWindow);
  SetCmdState([cmFindProcedure,cmObjects,cmModules,cmGlobals,cmSymbol],IsSymbolInfoAvailable);
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

procedure TIDEApp.SourceWindowClosed;
begin
  if not IsClosing then
    Update;
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
var
  s : string;
begin
{$ifdef Unix}
  s:=GetEnv('SHELL');
  if s='' then
    if ExistsFile('/bin/sh') then
      s:='/bin/sh';
{$else}
  s:=GetEnv('COMSPEC');
  if s='' then
    if ExistsFile('c:\command.com') then
      s:='c:\command.com'
    else
      begin
        s:='command.com';
        if Not LocateExeFile(s) then
          s:='';
      end;
{$endif}
  if s='' then
    ErrorBox(msg_errorexecutingshell,nil)
  else
    DoExecute(s, '', '', '', '', exDosShell);
  { In case we have something that the compiler touched }
  AskToReloadAllModifiedFiles;
end;

procedure TIDEApp.ShowReadme;
var R,R2: TRect;
    D: PCenterDialog;
    M: PFPMemo;
    VSB: PScrollBar;
    S: PFastBufStream;
begin
  New(S, Init(ReadmeName, stOpenRead, 4096));
  if S^.Status=stOK then
  begin
    R.Assign(0,0,63,18);
    New(D, Init(R, 'Free Pascal IDE'));
    with D^ do
    begin
      GetExtent(R);
      R.Grow(-2,-2); Inc(R.B.Y);
      R2.Copy(R); R2.Move(1,0); R2.A.X:=R2.B.X-1;
      New(VSB, Init(R2)); VSB^.GrowMode:=0; Insert(VSB);
      New(M, Init(R,nil,VSB,nil));
      M^.LoadFromStream(S);
      M^.ReadOnly:=true;
      Insert(M);
    end;
    InsertOK(D);
    ExecuteDialog(D,nil);
  end;
  Dispose(S, Done);
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

{$I fpmansi.inc}

procedure TIDEApp.AddRecentFile(AFileName: string; CurX, CurY: sw_integer);
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
  UpdateRecentFileList;
end;

function TIDEApp.GetPalette: PPalette;
begin
  GetPalette:=@AppPalette;
end;

function TIDEApp.IsClosing: Boolean;
begin
  IsClosing:=InsideDone;
end;

destructor TIDEApp.Done;
begin
  InsideDone:=true;
  IsRunning:=false;
  inherited Done;
  Desktop:=nil;
  RemoveBrowsersCollection;
  DoneHelpSystem;
end;

END.
