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
  FPViews;

type
    TIDEApp = object(TApplication)
      constructor Init;
      procedure   InitMenuBar; virtual;
      procedure   InitStatusLine; virtual;
      procedure   Open(FileName: string);
      procedure   HandleEvent(var Event: TEvent); virtual;
      function    GetPalette: PPalette; virtual;
      procedure   DosShell; virtual;
      destructor  Done; virtual;
    private
      function  OpenEditorWindow(FileName: string; CurX,CurY: integer): PSourceWindow;
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
      procedure DoRun;
      procedure Target;
      procedure PrimaryFile_;
      procedure ClearPrimary;
      procedure ShowUserScreen;
      procedure Information;
      procedure Calculator;
      procedure SetSwitchesMode;
      procedure Compiler;
      procedure MemorySizes;
      procedure Linker;
      procedure Debugger;
      procedure Directories;
      procedure EditorOptions(Editor: PEditor);
      procedure Mouse;
      procedure Colors;
      procedure OpenINI;
      procedure SaveINI;
      procedure SaveAsINI;
      procedure CloseAll;
      procedure ShowScreenWindow;
      procedure WindowList;
      procedure HelpContents;
      procedure HelpHelpIndex;
      procedure HelpTopicSearch;
      procedure HelpPrevTopic;
      procedure HelpUsingHelp;
      procedure HelpFiles;
      procedure About;
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
    end;


var
  MyApp: TIDEApp;

implementation

uses
  Video,Mouse,Keyboard,
  Dos,Objects,Memory,Menus,Dialogs,StdDlg,ColorSel,Commands,HelpCtx,
  Systems,BrowCol,
  WHelp,WHlpView,WINI,
  FPConst,FPVars,FPUtils,FPSwitches,FPIni,FPIntf,FPCompile,FPHelp,
  FPTemplt,FPCalc,FPUsrScr,FPSymbol;


function IDEUseSyntaxHighlight(Editor: PFileEditor): boolean; {$ifndef FPC}far;{$endif}
begin
  IDEUseSyntaxHighlight:=MatchesFileList(NameAndExtOf(Editor^.FileName),HighlightExts);
end;

constructor TIDEApp.Init;
begin
  {$ifndef EDITORS}UseSyntaxHighlight:=IDEUseSyntaxHighlight;{$endif}
  inherited Init;
  New(ClipboardWindow, Init);
  Desktop^.Insert(ClipboardWindow);
  New(CalcWindow, Init); CalcWindow^.Hide;
  Desktop^.Insert(CalcWindow);
  New(ProgramInfoWindow, Init); ProgramInfoWindow^.Hide; Desktop^.Insert(ProgramInfoWindow);
  New(UserScreenWindow, Init(UserScreen)); UserScreenWindow^.Hide; Desktop^.Insert(UserScreenWindow);
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
      NewItem('P~a~rameters...','', kbNoKey, cmParameters, hcParameters,
      NewLine(
      NewItem('~U~ser screen','Alt+F5', kbAltF5, cmUserScreen, hcUserScreen,
      nil))))),
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
      NewItem('~U~ser screen','Alt+F5', kbAltF5, cmUserScreen, hcUserScreen,
      nil)),
    NewSubMenu('~T~ools', hcToolsMenu, NewMenu(
      NewItem('~M~essages', '', kbNoKey, cmToolsMessages, hcToolsMessages,
      NewLine(
      NewItem('~C~alculator', '', kbNoKey, cmCalculator, hcCalculator,
      nil)))),
    NewSubMenu('~O~ptions', hcOptionsMenu, NewMenu(
      NewItem('Mode~.~..','', kbNoKey, cmSetSwitchesMode, hcSetSwitchesMode,
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
      NewItem('~U~ser screen window','', kbNoKey, cmUserScreenWindow, hcUserScreenWindow,
      NewItem('~R~efresh display','', kbNoKey, cmUpdate, hcUpdate,
      nil)))))))))))))),
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
             cmRun           : DoRun;
           { -- Compile menu -- }
             cmCompile       : DoCompile(cCompile);
             cmBuild         : DoCompile(cBuild);
             cmMake          : DoCompile(cMake);
             cmTarget        : Target;
             cmPrimaryFile   : PrimaryFile_;
             cmClearPrimary  : ClearPrimary;
             cmInformation   : Information;
           { -- Debug menu -- }
             cmUserScreen    : ShowUserScreen;
           { -- Options menu -- }
             cmSetSwitchesMode : SetSwitchesMode;
             cmCompiler      : Compiler;
             cmMemorySizes   : MemorySizes;
             cmLinker        : Linker;
             cmDebugger      : Debugger;
             cmDirectories   : Directories;
             cmEditor        : EditorOptions(nil);
             cmEditorOptions : EditorOptions(Event.InfoPtr);
             cmMouse         : Mouse;
             cmColors        : Colors;
             cmOpenINI       : OpenINI;
             cmSaveINI       : SaveINI;
             cmSaveAsINI     : SaveAsINI;
           { -- Tools menu -- }
             cmCalculator    : Calculator;
           { -- Window menu -- }
             cmCloseAll      : CloseAll;
             cmWindowList    : WindowList;
             cmUserScreenWindow: ShowScreenWindow;
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
           cmUpdate              :
             Update;
           cmSourceWindowClosing :
             with PSourceWindow(Event.InfoPtr)^ do
               if Editor^.FileName<>'' then
                  AddRecentFile(Editor^.FileName,Editor^.CurPos.X,Editor^.CurPos.Y);
         end;
  end;
  inherited HandleEvent(Event);
end;

procedure TIDEApp.Update;
begin
  SetCmdState([cmSaveAll],IsThereAnyEditor);
  SetCmdState([cmCloseAll,cmTile,cmCascade,cmWindowList],IsThereAnyWindow);
  SetCmdState([cmFindProcedure,cmObjects,cmModules,cmGlobals{,cmInformation}],{SymbolInfoLoaded}true);
  UpdatePrimaryFile;
  UpdateINIFile;
  Message(MenuBar,evBroadcast,cmUpdate,nil);
  UpdateRecentFileList;
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

procedure TIDEApp.DosShell;
begin
  DoneSysError;
  DoneEvents;
  DoneVideo;
  DoneDosMem;
  if UserScreen<>nil then UserScreen^.SwitchTo;
  WriteShellMsg;
  SwapVectors;
  Exec(GetEnv('COMSPEC'), '');
  SwapVectors;
  if UserScreen<>nil then UserScreen^.SwitchBack;
  InitDosMem;
  InitVideo;
  InitEvents;
  InitSysError;
  Redraw;
  CurDirChanged;
  Message(Application,evBroadcast,cmUpdate,nil);
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
  Revision 1.1  1999-01-12 14:29:34  peter
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
