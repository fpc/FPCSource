{
    $Id: fphelp.pas,v 1.12 2005/03/13 12:32:41 florian Exp $
    This file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 1998 by Berczi Gabor

    Help routines for the IDE

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit FPHelp;

interface

uses
  Drivers,
  FVConsts,
  WHelp,WHlpView,WHTML,
  WEditor,WCEdit,
  WViews,WHTMLScn,
  FPViews;

type
    PIDEStatusLine = ^TIDEStatusLine;
    TIDEStatusLine = object(TAdvancedStatusLine)
      function  Hint(AHelpCtx: Word): String; virtual;
      procedure HandleEvent(var Event: TEvent); virtual;
    end;

    PFPHTMLFileLinkScanner = ^TFPHTMLFileLinkScanner;
    TFPHTMLFileLinkScanner = object(THTMLFileLinkScanner)
       function    CheckURL(const URL: string): boolean; virtual;
       function    CheckText(const Text: string): boolean; virtual;
       procedure   ProcessDoc(Doc: PHTMLLinkScanFile); virtual;
    end;

procedure Help(FileID, Context: THelpCtx; Modal: boolean);
procedure HelpIndex(Keyword: string);
procedure HelpTopicSearch(Editor: PEditor);
procedure HelpTopic(const S: string);
procedure CloseHelpWindows;

procedure InitHelpSystem;
procedure DoneHelpSystem;

procedure InitHelpFiles;
procedure DoneHelpFiles;
procedure CheckHelpSystem;

procedure PushStatus(S: string);
procedure SetStatus(S: string);
procedure ClearStatus;
procedure PopStatus;

const
      HelpWindow     : PFPHelpWindow = nil;
      HelpInited     : boolean = false;

implementation

uses Objects,Views,App,MsgBox,
     WUtils,WOAHelp,WHTMLHlp,WNGHelp,WOS2Help,WVPHelp,WWinHelp,
     FPString,FPConst,FPVars,FPUtils;

const
    MaxStatusLevel = {$ifdef FPC}10{$else}1{$endif};

var StatusStack : array[0..MaxStatusLevel] of string[MaxViewWidth];

const
      StatusStackPtr  : integer = 0;

procedure TIDEStatusLine.HandleEvent(var Event: TEvent);
begin
  case Event.What of
    evBroadcast :
      case Event.Command of
        cmUpdate : Update;
      end;
  end;
  inherited HandleEvent(Event);
end;

function TIDEStatusLine.Hint(AHelpCtx: Word): String;
var S: string;
begin
  case AHelpCtx of
    hcNoContext     : S:='';

    hcDragging      : S:='';

    hcSourceWindow  : S:='';
    hcHelpWindow    : S:='';
    hcCalcWindow    : S:='';
    hcInfoWindow    : S:='';
    hcClipboardWindow:S:='';
    hcBrowserWindow : S:='';
    hcMessagesWindow: S:='';
    hcCompilerMessagesWindow: S:='';
    hcASCIITableWindow: S:='';
    hcGDBWindow     : S:=hint_rawgdbwindow;
    hcDisassemblyWindow     : S:=hint_disassemblywindow;
    hcBreakpointListWindow : S:=hint_allbreakpoints;

    hcSystemMenu    : S:=hint_systemmenu;
    hcUpdate        : S:=hint_update;
    hcAbout         : S:=hint_about;

    hcFileMenu      : S:=hint_filemenu;
    hcNew           : S:=hint_filenew;
    hcNewFromTemplate:S:=hint_filenewfromtemplate;
    hcOpen          : S:=hint_fileopen;
    hcSave          : S:=hint_filesave;
    hcSaveAs        : S:=hint_filesaveas;
    hcSaveAll       : S:=hint_filesaveall;
    hcChangeDir     : S:=hint_changedir;
    hcDOSShell      : S:=hint_dosshell;
    hcQuit          : S:=hint_exit;
    hcRecentFileBase..hcRecentFileBase+10
                    : S:=hint_openrecentfile+RecentFiles[AHelpCtx-hcRecentFileBase].FileName;

    hcEditMenu      : S:=hint_editmenu;
    hcUndo          : S:=hint_editundo;
    hcRedo          : S:=hint_editredo;
    hcCut           : S:=hint_editcut;
    hcCopy          : S:=hint_editcopy;
    hcPaste         : S:=hint_editpaste;
    hcCopyWin       : S:=hint_editcopywin;
    hcPasteWin      : S:=hint_editpastewin;
    hcClear         : S:=hint_editclear;
    hcShowClipboard : S:=hint_showclipboard;

    hcSearchMenu    : S:=hint_searchmenu;
    hcFind          : S:=hint_searchfind;
    hcReplace       : S:=hint_searchreplace;
    hcSearchAgain   : S:=hint_searchagain;
    hcGotoLine      : S:=hint_gotoline;
    hcObjects       : S:=hint_objects;
    hcModules       : S:=hint_modules;
    hcGlobals       : S:=hint_globals;
    hcSymbol        : S:=hint_symbol;
    hcRunMenu       : S:=hint_runmenu;
    hcRun           : S:=hint_run;
    hcRunDir        : S:=hint_rundir;
    hcParameters    : S:=hint_runparameters;
    hcResetDebugger : S:=hint_resetprogram;
    hcContToCursor  : S:=hint_rununtilcursor;
    hcUntilReturn   : S:=hint_rununtilreturn;
    hcUserScreen    : S:=hint_userscreen;

    hcCompileMenu   : S:=hint_compilemenu;
    hcCompile       : S:=hint_compile;
    hcMake          : S:=hint_make;
    hcBuild         : S:=hint_build;
    hcTarget        : S:=hint_target;
    hcPrimaryFile   : S:=hint_primaryfile;
    hcClearPrimary  : S:=hint_clearprimaryfile;
    hcCompilerMessages:S:=hint_showmessages;

    hcDebugMenu     : S:=hint_debugmenu;
    hcToggleBreakpoint : S:=hint_togglebreakpoint;
    hcNewBreakpoint    : S:=hint_createnewbreakpoint;
    hcEditBreakpoint   : S:=hint_editbreakpoint;
    hcDeleteBreakpoint : S:=hint_deletebreakpoint;
    hcOpenGDBWindow : S:=hint_opengdbwindow;
    hcAddWatch      : S:=hint_addwatch;
    hcWatchesWindow : S:=hint_watches;
    hcStackWindow   : S:=hint_callstack;
    hcBreakpointList : S:=hint_editbreakpoints;
    hcToolsMenu     : S:=hint_toolsmenu;
    hcCalculator    : S:=hint_calculator;
    hcGrep          : S:=hint_grep;
    hcMsgGotoSource : S:=hint_gotosource;
    hcRegistersWindow : S:=hint_registers;
    hcFPURegisters    : S:=hint_FPURegisters;
    hcVectorRegisters : S:=hint_VectorRegisters;

    hcToolsMessages : S:=hint_messageswindow;
    hcToolsMsgNext  : S:=hint_gotonextmsg;
    hcToolsMsgPrev  : S:=hint_gotoprevmsg;

    hcToolsBase..
    hcToolsBase+MaxToolCount
                    : S:=hint_usertool;
    hcASCIITable    : S:=hint_asciitable;

    hcOptionsMenu   : S:=hint_optionsmenu;
    hcSwitchesMode  : S:=hint_switchesmode;
    hcCompiler,
    hcCompilerNoAltX  : S:=hint_compiler;
    hcMemorySizes   : S:=hint_memorysizes;
    hcLinker        : S:=hint_linkeroptions;
    hcDebugger      : S:=hint_debugoptions;
    hcDirectories   : S:=hint_directories;
    hcBrowser,
    hcBrowserOptions: S:=hint_browser;
    hcTools         : S:=hint_tools;
    hcRemoteDialog  : S:=hint_remotedialog;
    hcTransferRemote: S:=hint_transferremote;
    hcDoReload      : S:=hint_reloadmodifiedfile;

    hcEnvironmentMenu:S:=hint_environmentmenu;
    hcPreferences   : S:=hint_preferences;
    hcEditor        : S:=hint_editoroptions;
    hcCodeCompleteOptions:S:=hint_codecomplete;
    hcCodeTemplateOptions:S:=hint_codetemplates;
    hcMouse         : S:=hint_mouseoptions;
    hcDesktopOptions: S:=hint_desktopoptions;
    hcStartup       : S:=hint_startup;
    hcColors        : S:=hint_colors;
    hcOpenINI       : S:=hint_openini;
    hcSaveINI       : S:=hint_saveini;
    hcSaveAsINI     : S:=hint_saveasini;

    hcWindowMenu    : S:=hint_windowmenu;
    hcTile          : S:=hint_tile;
    hcCascade       : S:=hint_cascade;
    hcCloseAll      : S:=hint_closeall;
    hcResize        : S:=hint_resize;
    hcZoom          : S:=hint_zoom;
    hcNext          : S:=hint_next;
    hcPrev          : S:=hint_prev;
    hcHide          : S:=hint_hide;
    hcClose         : S:=hint_closewindow;
    hcWindowList    : S:=hint_windowlist;
    hcUserScreenWindow:S:=hint_userscreenwindow;

    hcHelpMenu      : S:=hint_helpmenu;
    hcHelpContents  : S:=hint_helpcontents;
    hcHelpIndex     : S:=hint_helpindex;
    hcHelpTopicSearch:S:=hint_helptopicsearch;
    hcHelpPrevTopic : S:=hint_helpprevtopic;
    hcHelpUsingHelp : S:=hint_helphowtouse;
    hcHelpFiles     : S:=hint_helpfiles;

    hcOpenAtCursor  : S:=hint_openatcursor;
    hcBrowseAtCursor: S:=hint_browseatcursor;
    hcEditorOptions : S:=hint_editoroptionscur;
  else S:='???';
  end;
  Hint:=S;
end;

procedure TFPHTMLFileLinkScanner.ProcessDoc(Doc: PHTMLLinkScanFile);
begin
  PushStatus(FormatStrStr(msg_indexingfile,Doc^.GetDocumentURL));
  inherited ProcessDoc(Doc);
  PopStatus;
end;

function TFPHTMLFileLinkScanner.CheckURL(const URL: string): boolean;
var OK: boolean;
const HTTPPrefix = 'http:';
      FTPPrefix  = 'ftp:';
begin
  OK:=inherited CheckURL(URL);
  if OK then OK:=DirAndNameOf(URL)<>'';
  if OK then OK:=CompareText(copy(ExtOf(URL),1,4),'.HTM')=0;
  if OK then OK:=CompareText(copy(URL,1,length(HTTPPrefix)),HTTPPrefix)<>0;
  if OK then OK:=CompareText(copy(URL,1,length(FTPPrefix)),FTPPrefix)<>0;
  CheckURL:=OK;
end;

function TFPHTMLFileLinkScanner.CheckText(const Text: string): boolean;
var OK: boolean;
    i : sw_integer;
    S: string;
begin
  S:=Trim(Text);
  OK:=(S<>'') and (S[1]<>'[') and (S[1]<>',');
  { remove all Indexes }
  if s[1] in ['0'..'9'] then
    begin
      i:=1;
      while (i<length(s)) and (s[i] in ['0'..'9']) do
        inc(i);
      if (i<length(s)) and (s[i] in [' ',#9,'.']) then
        OK:=false;
    end;

  CheckText:=OK;
end;

procedure InitHelpSystem;

  procedure AddHelpFile(HelpFile,Param: string);
  begin
    {$IFDEF DEBUG}SetStatus(msg_LoadingHelpFile+' ('+SmartPath(HelpFile)+')');{$ENDIF}
    if HelpFacility^.AddFile(HelpFile,Param)=nil then
      ErrorBox(FormatStrStr(msg_failedtoloadhelpfile,HelpFile),nil);
    {$IFDEF DEBUG}SetStatus(msg_LoadingHelpFile);{$ENDIF}
  end;

var I,P: sw_integer;
    S: string;
    Param: string;
begin
  New(HelpFacility, Init);

  WOAHelp.RegisterHelpType;
  WNGHelp.RegisterHelpType;
  WOS2Help.RegisterHelpType;
  WWinHelp.RegisterHelpType;
  WVPHelp.RegisterHelpType;
  WHTMLHlp.RegisterHelpType;

  PushStatus(msg_LoadingHelpFiles);
  for I:=0 to HelpFiles^.Count-1 do
    begin
      S:=HelpFiles^.At(I)^; Param:='';
      P:=Pos('|',S);
      if P>0 then
        begin Param:=copy(S,P+1,High(S)); S:=copy(S,1,P-1); end;
      AddHelpFile(S,Param);
    end;
  PopStatus;
end;

procedure CheckHelpSystem;
begin
  if HelpInited then Exit;
  InitHelpSystem;
  HelpInited:=true;
end;

procedure DoneHelpSystem;
begin
  if assigned(HelpFacility) then
    begin
      Dispose(HelpFacility, Done);
      HelpFacility:=nil;
    end;
  HelpInited:=false;
end;

procedure HelpCreateWindow;
var R: TRect;
begin
  CheckHelpSystem;
  if HelpWindow=nil then
  begin
     Desktop^.GetExtent(R); R.Grow(-15,-3); Dec(R.A.Y);
     New(HelpWindow, Init(R, dialog_help, 0, 0, SearchFreeWindowNo));
     if HelpWindow<>nil then
     begin
       HelpWindow^.Hide;
       Desktop^.Insert(HelpWindow);
     end;
  end;
end;

procedure Help(FileID, Context: THelpCtx; Modal: boolean);
begin
  if Modal then
     begin MessageBox(msg_modalhelpnotimplemented,nil,mfInformation+mfInsertInApp+mfOKButton); Exit; end;
  HelpCreateWindow;
  with HelpWindow^ do
  begin
    HelpWindow^.ShowTopic(FileID,Context);
    if GetState(sfVisible)=false then Show;
    MakeFirst;
  end;
  Message(Application,evCommand,cmUpdate,nil);
end;

procedure HelpTopicSearch(Editor: PEditor);
var S: string;
begin
  if Editor=nil then S:='' else
  S:=GetEditorCurWord(Editor,[]);
  HelpTopic(S);
end;

procedure HelpTopic(const S: string);
var FileID: word;
    Ctx   : THelpCtx;
var Found: boolean;
begin
  CheckHelpSystem;
  PushStatus(msg_LocatingTopic);
  Found:=HelpFacility^.TopicSearch(S,FileID,Ctx);
  PopStatus;
  if Found then
     Help(FileID,Ctx,false)
  else
     HelpIndex(S);
end;

procedure HelpIndex(Keyword: string);
begin
  HelpCreateWindow;
  with HelpWindow^ do
  begin
    PushStatus(msg_BuildingHelpIndex);
    HelpWindow^.ShowIndex;
    if Keyword<>'' then
       HelpWindow^.HelpView^.Lookup(Keyword);
    PopStatus;
    if GetState(sfVisible)=false then Show;
    MakeFirst;
  end;
  Message(Application,evCommand,cmUpdate,nil);
end;

procedure PushStatus(S: string);
begin
  if StatusLine=nil then
    Exit;
  If StatusStackPtr<=MaxStatusLevel then
    StatusStack[StatusStackPtr]:=PAdvancedStatusLine(StatusLine)^.GetStatusText
  else
    StatusStack[MaxStatusLevel]:=PAdvancedStatusLine(StatusLine)^.GetStatusText;
  SetStatus(S);
  Inc(StatusStackPtr);
end;

procedure PopStatus;
begin
  if StatusLine=nil then
    Exit;
  Dec(StatusStackPtr);
  If StatusStackPtr<=MaxStatusLevel then
    SetStatus(StatusStack[StatusStackPtr])
  else
    SetStatus(StatusStack[MaxStatusLevel]);
end;

procedure SetStatus(S: string);
begin
  if StatusLine=nil then
    Exit;
  PAdvancedStatusLine(StatusLine)^.SetStatusText(S);
end;

procedure ClearStatus;
begin
  PAdvancedStatusLine(StatusLine)^.ClearStatusText;
end;

function FPHTMLGetSectionColor(Section: THTMLSection; var Color: byte): boolean;
var OK: boolean;
    S: string;
begin
  Color:=0;
  OK:=(ord(Section) in [1..length(CHTMLSectionAttrs)]);
  if OK then
  begin
    S:=#0;
    S:=copy(CHTMLSectionAttrs,ord(Section),1);
    if Assigned(Application)=false then Color:=0 else
    Color:=Application^.GetColor(ord(S[1]));
    if (Color and $0f) = ((Color and $f0) shr 4) then { same color ? }
      OK:=false;
  end;
  FPHTMLGetSectionColor:=OK;
end;

function FPNGGetAttrColor(Attr: char; var Color: byte): boolean;
var OK: boolean;
begin
  OK:=false;
  case Attr of
    'A' : OK:=FPHTMLGetSectionColor(hsHeading1,Color);
    'B' : OK:=FPHTMLGetSectionColor(hsHeading2,Color);
    'b' : OK:=FPHTMLGetSectionColor(hsHeading5,Color);
    'U' : OK:=FPHTMLGetSectionColor(hsHeading3,Color);
    'N' : OK:=FPHTMLGetSectionColor(hsHeading4,Color);
  {$ifdef DEBUGMSG}
  else ErrorBox('Unknown attr encountered : "'+Attr+'"',nil);
  {$endif}
  end;
  FPNGGetAttrColor:=OK;
end;

function FPINFGetAttrColor(TextStyle, TextColor: byte; var Color: byte): boolean;
var OK: boolean;
begin
  OK:=false;
  case TextColor of
    1 : OK:=FPHTMLGetSectionColor(hsHeading1,Color);
    2 : OK:=FPHTMLGetSectionColor(hsHeading2,Color);
    3 : OK:=FPHTMLGetSectionColor(hsHeading3,Color);
  end;
  FPINFGetAttrColor:=OK;
end;

procedure InitHelpFiles;
begin
  HTMLGetSectionColor:={$ifdef FPC}@{$endif}FPHTMLGetSectionColor;
  NGGetAttrColor:={$ifdef FPC}@{$endif}FPNGGetAttrColor;
  INFGetAttrColor:={$ifdef FPC}@{$endif}FPINFGetAttrColor;
  New(HelpFiles, Init(10,10));
end;

procedure DoneHelpFiles;
begin
  if assigned(HelpFiles) then
    Dispose(HelpFiles, Done);
end;

procedure CloseHelpWindows;
procedure CloseIfHelpWindow(P: PView); {$ifndef FPC}far;{$endif}
begin
  if P^.HelpCtx=hcHelpWindow then
    begin
      Message(P,evCommand,cmClose,nil);
      {Dispose(P, Done);  help windows are only hidden on close so we've
                          to destroy them manually
       but this was wrong as it was not correctly
       resetting the corresponding pointer in whelp unit PM }
    end;
end;
begin
  Desktop^.ForEach(@CloseIfHelpWindow);
end;

END.
{
  $Log: fphelp.pas,v $
  Revision 1.12  2005/03/13 12:32:41  florian
    * more status line hints fixed

  Revision 1.11  2005/03/13 12:25:02  florian
    + Recent files write full name now as hint in the status line
    * Rundir hint in status line fixed

  Revision 1.10  2005/02/14 17:13:18  peter
    * truncate log

}
