{
    $Id$
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
{$ifdef FVISION}
  FVConsts,
{$else}
  Commands,HelpCtx,
{$endif}
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
                    : S:=hint_openrecentfile;

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
    hcInformation   : S:=hint_information;
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

    hcToolsMessages : S:=hint_messageswindow;
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
    S: string;
begin
  S:=Trim(Text);
  OK:=(S<>'') and (copy(S,1,1)<>'[');
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
  $Log$
  Revision 1.5  2002-01-24 09:21:41  pierre
   * only disable Alt-X in Options|Compiler dialog

  Revision 1.4  2001/10/01 00:24:09  pierre
   * fix several help problems

  Revision 1.3  2001/09/10 10:52:59  pierre
   * fix web bug 1368

  Revision 1.2  2001/08/05 02:01:47  peter
    * FVISION define to compile with fvision units

  Revision 1.1  2001/08/04 11:30:23  peter
    * ide works now with both compiler versions

  Revision 1.1.2.7  2001/03/12 17:34:55  pierre
   + Disassembly window started

  Revision 1.1.2.6  2000/11/29 11:25:59  pierre
   + TFPDlgWindow that handles cmSearchWindow

  Revision 1.1.2.5  2000/11/29 00:54:44  pierre
   + preserve window number and save special windows

  Revision 1.1.2.4  2000/11/27 12:06:48  pierre
   New bunch of Gabor fixes

  Revision 1.1.2.3  2000/11/23 16:33:30  pierre
   * fix Alt-X problem and set HelpCtx for most dialogs

  Revision 1.1.2.2  2000/09/18 13:20:54  pierre
   New bunch of Gabor changes

  Revision 1.1.2.1  2000/08/15 03:40:53  peter
   [*] no more fatal exits when the IDE can't find the error file (containing
       the redirected assembler/linker output) after compilation
   [*] hidden windows are now added always at the end of the Window List
   [*] TINIFile parsed entries encapsulated in string delimiters incorrectly
   [*] selection was incorrectly adjusted when typing in overwrite mode
   [*] the line wasn't expanded when it's end was reached in overw. mode
   [*] the IDE now tries to locate source files also in the user specified
       unit dirs (for ex. as a response to 'Open at cursor' (Ctrl+Enter) )
   [*] 'Open at cursor' is now aware of the extension (if specified)

  Revision 1.1  2000/07/13 09:48:34  michael
  + Initial import

  Revision 1.35  2000/06/26 07:29:23  pierre
   * new bunch of Gabor's changes

  Revision 1.34  2000/06/22 09:07:12  pierre
   * Gabor changes: see fixes.txt

  Revision 1.33  2000/06/16 08:50:40  pierre
   + new bunch of Gabor's changes

  Revision 1.32  2000/05/30 07:18:33  pierre
   + colors for HTML help by Gabor

  Revision 1.31  2000/05/29 10:44:56  pierre
   + New bunch of Gabor's changes: see fixes.txt

  Revision 1.30  2000/05/02 08:42:27  pierre
   * new set of Gabor changes: see fixes.txt

  Revision 1.29  2000/04/25 08:42:33  pierre
   * New Gabor changes : see fixes.txt

  Revision 1.28  2000/03/21 23:31:14  pierre
   adapted to wcedit addition by Gabor

  Revision 1.27  2000/02/07 11:58:01  pierre
   Gabor's code inserted

  Revision 1.26  2000/01/08 18:26:20  florian
    + added a register window, doesn't work yet

  Revision 1.25  2000/01/05 17:25:26  pierre
   * typo error corrected

  Revision 1.24  2000/01/03 11:38:33  michael
  Changes from Gabor

  Revision 1.23  1999/09/09 16:31:45  pierre
   * some breakpoint related fixes and Help contexts

  Revision 1.22  1999/09/09 14:15:27  pierre
   + cmCopyWin,cmPasteWin

  Revision 1.21  1999/08/16 18:25:17  peter
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

  Revision 1.20  1999/08/03 20:22:31  peter
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

  Revision 1.19  1999/07/12 13:14:17  pierre
    * LineEnd bug corrected, now goes end of text even if selected
    + Until Return for debugger
    + Code for Quit inside GDB Window

  Revision 1.18  1999/07/10 01:24:16  pierre
   + First implementation of watches window

  Revision 1.17  1999/06/30 23:58:14  pierre
    + BreakpointsList Window implemented
      with Edit/New/Delete functions
    + Individual breakpoint dialog with support for all types
      ignorecount and conditions
      (commands are not yet implemented, don't know if this wolud be useful)
      awatch and rwatch have problems because GDB does not annotate them
      I fixed v4.16 for this

  Revision 1.16  1999/06/28 19:32:19  peter
    * fixes from gabor

  Revision 1.15  1999/06/25 00:39:58  pierre
   help for cmSymbol,cmAddWatch,cmStack and cmBreakpoint list

  Revision 1.14  1999/04/07 21:55:46  peter
    + object support for browser
    * html help fixes
    * more desktop saving things
    * NODEBUG directive to exclude debugger

  Revision 1.13  1999/03/23 15:11:28  peter
    * desktop saving things
    * vesa mode
    * preferences dialog

  Revision 1.12  1999/03/16 12:38:09  peter
    * tools macro fixes
    + tph writer
    + first things for resource files

  Revision 1.11  1999/03/01 15:41:53  peter
    + Added dummy entries for functions not yet implemented
    * MenuBar didn't update itself automatically on command-set changes
    * Fixed Debugging/Profiling options dialog
    * TCodeEditor converts spaces to tabs at save only if efUseTabChars is set
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

  Revision 1.10  1999/02/22 11:51:35  peter
    * browser updates from gabor

  Revision 1.9  1999/02/19 18:43:45  peter
    + open dialog supports mask list

  Revision 1.8  1999/02/11 19:07:21  pierre
    * GDBWindow redesigned :
      normal editor apart from
      that any kbEnter will send the line (for begin to cursor)
      to GDB command !
      GDBWindow opened in Debugger Menu
       still buggy :
       -echo should not be present if at end of text
       -GDBWindow becomes First after each step (I don't know why !)

  Revision 1.7  1999/02/08 17:40:01  pierre
   + cmContToCursor added

  Revision 1.6  1999/02/08 10:37:43  peter
    + html helpviewer

  Revision 1.5  1999/02/04 12:23:44  pierre
    + cmResetDebugger and cmGrep
    * Avoid StatusStack overflow

  Revision 1.4  1999/01/21 11:54:13  peter
    + tools menu
    + speedsearch in symbolbrowser
    * working run command

  Revision 1.3  1999/01/04 11:49:44  peter
   * 'Use tab characters' now works correctly
   + Syntax highlight now acts on File|Save As...
   + Added a new class to syntax highlight: 'hex numbers'.
   * There was something very wrong with the palette managment. Now fixed.
   + Added output directory (-FE<xxx>) support to 'Directories' dialog...
   * Fixed some possible bugs in Running/Compiling, and the compilation/run
     process revised

  Revision 1.2  1998/12/28 15:47:44  peter
    + Added user screen support, display & window
    + Implemented Editor,Mouse Options dialog
    + Added location of .INI and .CFG file
    + Option (INI) file managment implemented (see bottom of Options Menu)
    + Switches updated
    + Run program

  Revision 1.3  1998/12/22 10:39:42  peter
    + options are now written/read
    + find and replace routines

}
