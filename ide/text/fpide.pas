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

uses
  Drivers,Views,App,
  {$ifdef EDITORS}Editors,{$else}WEditor,{$endif}
  Comphook,
  FPViews;

type
    TIDEApp = object(TApplication)
      constructor Init;
      procedure   InitMenuBar; virtual;
      procedure   InitStatusLine; virtual;
      procedure   Open(FileName: string);
      function    OpenSearch(FileName: string) : boolean;
      procedure   HandleEvent(var Event: TEvent); virtual;
      function    GetPalette: PPalette; virtual;
      procedure   DosShell; virtual;
      destructor  Done; virtual;
    public
      procedure ShowUserScreen;
      procedure ShowIDEScreen;
    private
      procedure NewEditor;
      procedure NewFromTemplate;
      procedure OpenRecentFile(RecentIndex: integer);
      procedure SaveAll;
      procedure ChangeDir;
      procedure ShowClipboard;
      procedure Objects;
      procedure Modules;
      procedure Globals;
      procedure Parameters;
      procedure DoStepOver;
      procedure DoTraceInto;
      procedure DoRun;
      procedure DoResetDebugger;
      procedure DoContToCursor;
      procedure Target;
      procedure PrimaryFile_;
      procedure ClearPrimary;
      procedure DoUserScreenWindow;
      procedure DoUserScreen;
      procedure DoToggleBreak;
      procedure Information;
      procedure Calculator;
      procedure ExecuteTool(Idx: integer);
      procedure SetSwitchesMode;
      procedure DoCompilerSwitch;
      procedure MemorySizes;
      procedure DoLinkerSwitch;
      procedure DoDebuggerSwitch;
      procedure Directories;
      procedure Tools;
      procedure Grep;
      procedure EditorOptions(Editor: PEditor);
      procedure Mouse;
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
      procedure DoExecute(ProgramPath, Params: string; IsDOSShell: boolean);
    private
      procedure AddRecentFile(AFileName: string; CurX, CurY: integer);
      function  SearchRecentFile(AFileName: string): integer;
      procedure RemoveRecentFile(Index: integer);
    private
      procedure Update;
      procedure CurDirChanged;
      procedure UpdatePrimaryFile;
      procedure UpdateINIFile;
      procedure UpdateRecentFileList;
      procedure UpdateTools;
    end;


var
  MyApp: TIDEApp;

implementation

uses
  Video,Mouse,Keyboard,
  Dos,Objects,Memory,Menus,Dialogs,StdDlg,ColorSel,Commands,HelpCtx,
  Systems,BrowCol,
  WHelp,WHlpView,WINI,
  FPConst,FPVars,FPUtils,FPSwitch,FPIni,FPIntf,FPCompile,FPHelp,
  FPTemplt,FPCalc,FPUsrScr,FPSymbol,FPTools,FPDebug,FPRedir;


function IDEUseSyntaxHighlight(Editor: PFileEditor): boolean; {$ifndef FPC}far;{$endif}
begin
  IDEUseSyntaxHighlight:=(Editor^.FileName='') or MatchesFileList(NameAndExtOf(Editor^.FileName),HighlightExts);
end;

function IDEUseTabsPattern(Editor: PFileEditor): boolean; {$ifndef FPC}far;{$endif}
begin
  IDEUseTabsPattern:=(Editor^.FileName='') or MatchesFileList(NameAndExtOf(Editor^.FileName),TabsPattern);
end;

constructor TIDEApp.Init;
begin
  {$ifndef EDITORS}
  UseSyntaxHighlight:=IDEUseSyntaxHighlight;
  UseTabsPattern:=IDEUseTabsPattern;
  {$endif}
  inherited Init;
  New(ClipboardWindow, Init);
  Desktop^.Insert(ClipboardWindow);
  New(CalcWindow, Init); CalcWindow^.Hide;
  Desktop^.Insert(CalcWindow);
  New(ProgramInfoWindow, Init); ProgramInfoWindow^.Hide; Desktop^.Insert(ProgramInfoWindow);
  Message(@Self,evBroadcast,cmUpdate,nil);
  InitTemplates;
  CurDirChanged;
end;

procedure TIDEApp.InitMenuBar;
var R: TRect;
begin
  GetExtent(R); R.B.Y:=R.A.Y+1;
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
      NewLine(
      NewItem('Cu~t~','Shift+Del', kbShiftDel, cmCut, hcCut,
      NewItem('~C~opy','Ctrl+Ins', kbCtrlIns, cmCopy, hcCut,
      NewItem('~P~aste','Shift+Ins', kbShiftIns, cmPaste, hcPaste,
      NewItem('C~l~ear','Ctrl+Del', kbCtrlDel, cmClear, hcClear,
      NewLine(
      NewItem('~S~how clipboard','', kbNoKey, cmShowClipboard, hcShowClipboard,
      nil)))))))))),
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
      nil))))))))))),
    NewSubMenu('~R~un',hcRunMenu, NewMenu(
      NewItem('~R~un','Ctrl+F9', kbCtrlF9, cmRun, hcRun,
      NewItem('~S~tep over','F8', kbF8, cmStepOver, hcRun,
      NewItem('~T~race into','F7', kbF7, cmTraceInto, hcRun,
      NewItem('~G~oto Cursor','F4', kbF4, cmContToCursor, hcContToCursor,
      NewItem('P~a~rameters...','', kbNoKey, cmParameters, hcParameters,
      NewItem('~P~rogram reset','Ctrl+F2', kbCtrlF2, cmResetDebugger, hcResetDebugger,
      nil))))))),
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
      nil)))))))))),
    NewSubMenu('~D~ebug', hcDebugMenu, NewMenu(
      NewItem('~O~utput','', kbNoKey, cmUserScreenWindow, hcUserScreenWindow,
      NewItem('~U~ser screen','Alt+F5', kbAltF5, cmUserScreen, hcUserScreen,
      NewItem('~B~reakpoint','Ctrl+F8', kbCtrlF8, cmToggleBreakpoint, hcToggleBreakpoint,
      nil)))),
    NewSubMenu('~T~ools', hcToolsMenu, NewMenu(
      NewItem('~M~essages', '', kbNoKey, cmToolsMessages, hcToolsMessages,
      NewLine(
      NewItem('~G~rep', 'Shift+F2', kbShiftF2, cmGrep, hcGrep,
      NewItem('~C~alculator', '', kbNoKey, cmCalculator, hcCalculator,
      nil))))),
    NewSubMenu('~O~ptions', hcOptionsMenu, NewMenu(
      NewItem('Mode~.~..','', kbNoKey, cmSwitchesMode, hcSwitchesMode,
      NewItem('~C~ompiler...','', kbNoKey, cmCompiler, hcCompiler,
      NewItem('~M~emory sizes...','', kbNoKey, cmMemorySizes, hcMemorySizes,
      NewItem('~L~inker...','', kbNoKey, cmLinker, hcLinker,
      NewItem('De~b~ugger...','', kbNoKey, cmDebugger, hcDebugger,
      NewItem('~D~irectories...','', kbNoKey, cmDirectories, hcDirectories,
      NewItem('~T~ools...','', kbNoKey, cmTools, hcTools,
      NewLine(
      NewSubMenu('~E~nvironment', hcEnvironmentMenu, NewMenu(
        NewItem('~P~references...','', kbNoKey, cmPreferences, hcPreferences,
        NewItem('~E~ditor...','', kbNoKey, cmEditor, hcEditor,
        NewItem('~M~ouse...','', kbNoKey, cmMouse, hcMouse,
        NewItem('~S~tartup...','', kbNoKey, cmStartup, hcStartup,
        NewItem('~C~olors...','', kbNoKey, cmColors, hcColors,
        nil)))))),
      NewLine(
      NewItem('~O~pen...','', kbNoKey, cmOpenINI, hcOpenINI,
      NewItem('~S~ave','', kbNoKey, cmSaveINI, hcSaveINI,
      NewItem('Save ~a~s...','', kbNoKey, cmSaveAsINI, hcSaveAsINI,
      nil)))))))))))))),
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
      nil)))),
    NewStatusDef(hcSourceWindow, hcSourceWindow,
      NewStatusKey('~F1~ Help', kbF1, cmHelp,
      NewStatusKey('~F2~ Save', kbF2, cmSave,
      NewStatusKey('~F3~ Open', kbF3, cmOpen,
      NewStatusKey('~Alt+F9~ Compile', kbAltF9, cmCompile,
      NewStatusKey('~F9~ Make', kbF9, cmMake,
      NewStatusKey('~Alt+F10~ Local menu', kbAltF10, cmLocalMenu,
      StdStatusKeys(
      nil))))))),
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
    nil)))))));
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
             cmObjects       : Objects;
             cmModules       : Modules;
             cmGlobals       : Globals;
           { -- Run menu -- }
             cmParameters    : Parameters;
             cmStepOver      : DoStepOver;
             cmTraceInto     : DoTraceInto;
             cmRun           : DoRun;
             cmResetDebugger : DoResetDebugger;
             cmContToCursor  : DoContToCursor;
           { -- Compile menu -- }
             cmCompile       : DoCompile(cCompile);
             cmBuild         : DoCompile(cBuild);
             cmMake          : DoCompile(cMake);
             cmTarget        : Target;
             cmPrimaryFile   : PrimaryFile_;
             cmClearPrimary  : ClearPrimary;
             cmInformation   : Information;
           { -- Debug menu -- }
             cmUserScreen    : DoUserScreen;
             cmToggleBreakpoint   : DoToggleBreak;
           { -- Options menu -- }
             cmSwitchesMode  : SetSwitchesMode;
             cmCompiler      : DoCompilerSwitch;
             cmMemorySizes   : MemorySizes;
             cmLinker        : DoLinkerSwitch;
             cmDebugger      : DoDebuggerSwitch;
             cmDirectories   : Directories;
             cmTools         : Tools;
             cmEditor        : EditorOptions(nil);
             cmEditorOptions : EditorOptions(Event.InfoPtr);
             cmMouse         : Mouse;
             cmColors        : Colors;
             cmOpenINI       : OpenINI;
             cmSaveINI       : SaveINI;
             cmSaveAsINI     : SaveAsINI;
           { -- Tools menu -- }
             cmCalculator    : Calculator;
             cmGrep          : Grep;
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
           cmUpdateTools :
             UpdateTools;
           cmUpdate              :
             Update;
           cmSourceWndClosing :
             with PSourceWindow(Event.InfoPtr)^ do
               if Editor^.FileName<>'' then
                  AddRecentFile(Editor^.FileName,Editor^.CurPos.X,Editor^.CurPos.Y);
         end;
  end;
  inherited HandleEvent(Event);
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
  Redraw;
  CurDirChanged;
  Message(Application,evBroadcast,cmUpdate,nil);
  UpdateScreen(true);
end;


procedure TIDEApp.DoExecute(ProgramPath, Params: string; IsDosShell: boolean);
begin
  if UserScreen=nil then
   begin
     ErrorBox('Sorry, user screen not available.',nil);
     Exit;
   end;

  ShowUserScreen;

  if IsDOSShell then
    WriteShellMsg;

  SwapVectors;
  Dos.Exec(GetEnv('COMSPEC'),'/C '+ProgramPath+' '+Params);
  SwapVectors;

  ShowIDEScreen;
end;


procedure TIDEApp.Update;
begin
  SetCmdState([cmSaveAll],IsThereAnyEditor);
  SetCmdState([cmCloseAll,cmTile,cmCascade,cmWindowList],IsThereAnyWindow);
  SetCmdState([cmFindProcedure,cmObjects,cmModules,cmGlobals{,cmInformation}],IsSymbolInfoAvailable);
  SetCmdState([cmResetDebugger],assigned(debugger) and debugger^.debugger_started);
  UpdatePrimaryFile;
  UpdateINIFile;
  Message(MenuBar,evBroadcast,cmUpdate,nil);
  UpdateRecentFileList;
  UpdateTools;
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
    P:=NewItem('~'+IntToStr(I)+'~. '+SmartPath(RecentFiles[I].FileName),' ',
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
  DoExecute(GetEnv('COMSPEC'), '', true);
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

destructor TIDEApp.Done;
begin
  inherited Done;
  DoneHelpSystem;
  DoneTemplates;
end;

END.
{
  $Log$
  Revision 1.12  1999-02-08 17:43:44  pierre
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
