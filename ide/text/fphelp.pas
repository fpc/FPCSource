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
  Drivers,HelpCtx,
  WHelp,WHlpView,WHTML,
{$ifdef EDITORS}
  Editors,
{$else}
  WEditor,
{$endif}
  WViews,FPViews;

type
    PIDEStatusLine = ^TIDEStatusLine;
    TIDEStatusLine = object(TAdvancedStatusLine)
      function  Hint(AHelpCtx: Word): String; virtual;
      procedure HandleEvent(var Event: TEvent); virtual;
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

procedure PushStatus(S: string);
procedure SetStatus(S: string);
procedure ClearStatus;
procedure PopStatus;

const
      HelpWindow     : PFPHelpWindow = nil;
      HelpInited     : boolean = false;

implementation

uses Objects,Views,App,MsgBox,Commands,
     WHTMLHlp,
     FPConst,FPVars,FPUtils;

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

    hcSourceWindow  : S:='';
    hcHelpWindow    : S:='';
    hcCalcWindow    : S:='';
    hcInfoWindow    : S:='';
    hcClipboardWindow:S:='';
    hcBrowserWindow : S:='';
    hcMessagesWindow: S:='';
    hcASCIITableWindow: S:='';
    hcGDBWindow     : S:='Raw GDB communication window';
    hcBreakpointListWindow : S:='All current breakpoints';

    hcSystemMenu    : S:='System menu';
    hcUpdate        : S:='Refresh and redraw display';
    hcAbout         : S:='Show version and copyright information';

    hcFileMenu      : S:='File managment commands (Open, New, Save, etc.)';
    hcNew           : S:='Create a new file in a new edit window';
    hcNewFromTemplate:S:='Create a new file using a code template';
    hcOpen          : S:='Locate and open a file in an edit window';
    hcSave          : S:='Save the file in the active edit window';
    hcSaveAs        : S:='Save the current file under a different name, directory or drive';
    hcSaveAll       : S:='Save all modified files';
    hcChangeDir     : S:='Choose a new default directory';
    hcDOSShell      : S:='Temporarily exit to DOS';
    hcQuit          : S:='Exit the IDE';
    hcRecentFileBase..hcRecentFileBase+10
                    : S:='Open indicated file in a new editor window';

    hcEditMenu      : S:='Clipboard editing commands';
    hcUndo          : S:='Undo the previous editor operation';
    hcRedo          : S:='Redo the previously undone editor operation';
    hcCut           : S:='Remove the selected text and put it in the clipboard';
    hcCopy          : S:='Copy the selected text in the clipboard';
    hcPaste         : S:='Insert selected text from the clipboard at the cursor position';
    hcCopyWin       : S:='Copy the selected text in windows clipboard';
    hcPasteWin      : S:='Insert selected text from windows clipboard at the cursor position';
    hcClear         : S:='Delete the selected text';
    hcShowClipboard : S:='Open then clipboard window';

    hcSearchMenu    : S:='Text and symbols search commands';
    hcFind          : S:='Search for text';
    hcReplace       : S:='Search for text and replace it with new text';
    hcSearchAgain   : S:='Repeat the last Search or Replace command';
    hcGotoLine      : S:='Move the cursor to a specified line number';
    hcObjects       : S:='Open a browser displaying all objects in the program';
    hcModules       : S:='Open a browser displaying all modules of the program';
    hcGlobals       : S:='Open a browser displaying all global symbols in the program';
    hcSymbol        : S:='Open a browser a current word (not yet scope sensitive)';
    hcRunMenu       : S:='Execution and parameters';
    hcRun           : S:='Run the current program';
    hcParameters    : S:='Set command-line parameters passed to program at execution';
    hcResetDebugger : S:='Reset Program';
    hcContToCursor  : S:='Go on until Cursor position';
    hcUntilReturn   : S:='Go on until end of current function';
    hcUserScreen    : S:='Switch to the full-screen user output';

    hcCompileMenu   : S:='Compile, build & make';
    hcCompile       : S:='Compile the current source file';
    hcMake          : S:='Rebuild source file and all other files that have been modified';
    hcBuild         : S:='Rebuild program and all available source files';
    hcTarget        : S:='Select target platform to compile for';
    hcPrimaryFile   : S:='Define then file that is the focus of Make and Build';
    hcClearPrimary  : S:='Clear the file previously set to Primary';
    hcInformation   : S:='Show compiler messages and program information';
    hcCompilerMessages:S:='Show compiler messages window';

    hcDebugMenu     : S:='Debug Program';
    hcToggleBreakpoint : S:='Toggles Breakpoint';
    hcNewBreakpoint    : S:='Create a new breakpoint';
    hcEditBreakpoint   : S:='Edit focused breakpoint';
    hcDeleteBreakpoint : S:='Delete focused breakpoint';
    hcOpenGDBWindow : S:='Open direct window to GDB';
    hcAddWatch      : S:='Add a new expression to watch';
    hcWatches       : S:='Open the Watches Window';
    hcStack         : S:='Show call stack';
    hcBreakpointList : S:='Edit breakpoints';
    hcToolsMenu     : S:='User installed tools';
    hcCalculator    : S:='Show calculator';
    hcGrep          : S:='Run grep';
    hcMsgGotoSource : S:='Edit source';
    hcRegisters     : S:='Open the Registers Window';

    hcToolsMessages : S:='Open the message window';
    hcToolsBase..
    hcToolsBase+MaxToolCount
                    : S:='User installed tool';
    hcASCIITable    : S:='Show ASCII table';

    hcOptionsMenu   : S:='Setting for compiler, editor, mouse, etc.';
    hcSwitchesMode  : S:='Select settings for normal, debug or release version';
    hcCompiler      : S:='Set default compiler directives and conditional defines';
    hcMemorySizes   : S:='Set default stack and heap sizes for generated programs';
    hcLinker        : S:='Set linker options';
    hcDebugger      : S:='Set debug information options';
    hcDirectories   : S:='Set paths for units, include, object and generated files';
    hcBrowser       : S:='Specify global browser settings';
    hcTools         : S:='Create or change tools';

    hcEnvironmentMenu:S:='Specify environment settins';
    hcPreferences   : S:='Specify desktop settings';
    hcEditor        : S:='Specify default editor settings';
    hcCodeCompleteOptions:S:='Specify CodeComplete keywords';
    hcCodeTemplateOptions:S:='Specify CodeCompletes';
    hcMouse         : S:='Specify mouse settings';
    hcDesktopOptions: S:='Specify desktop settings';
    hcStartup       : S:='Permanently change default startup options';
    hcColors        : S:='Customize IDE colors for windows, menus, editors, etc.';
    hcOpenINI       : S:='Load a previously saved options file';
    hcSaveINI       : S:='Save all the changes made in the options menu';
    hcSaveAsINI     : S:='Save all the changes made under a different name';

    hcWindowMenu    : S:='Windows managment commands';
    hcTile          : S:='Arrange windows on desktop by tiling';
    hcCascade       : S:='Arrange windows on desktop by cascading';
    hcCloseAll      : S:='Close all windows on the desktop';
    hcResize        : S:='Change the size/postion of the active window';
    hcZoom          : S:='Enlarge or restore the size of the active window';
    hcNext          : S:='Make the next window active';
    hcPrev          : S:='Make the previous window active';
    hcClose         : S:='Close the active window';
    hcWindowList    : S:='Show a list of all open windows';
    hcUserScreenWindow:S:='Show contents of user screen in a window';

    hcHelpMenu      : S:='Get online help';
    hcHelpContents  : S:='Show table of contents for Online Help';
    hcHelpIndex     : S:='Show index for Online Help';
    hcHelpTopicSearch:S:='Display help on the word at cursor';
    hcHelpPrevTopic : S:='Redisplay the last-viewed Online Help screen';
    hcHelpUsingHelp : S:='How to use Online Help';
    hcHelpFiles     : S:='Install or remove installed help files';

    hcOpenAtCursor  : S:='Attempt to open the file indicated by the word at cursor';
    hcBrowseAtCursor: S:='Attempt to browse the symbol at cursor';
    hcEditorOptions : S:='Specify editor settings';
  else S:='???';
  end;
  Hint:=S;
end;

procedure InitHelpSystem;

  procedure AddOAFile(HelpFile: string);
  begin
    {$IFDEF DEBUG}SetStatus(strLoadingHelp+' ('+SmartPath(HelpFile)+')');{$ENDIF}
    HelpFacility^.AddOAHelpFile(HelpFile);
    {$IFDEF DEBUG}SetStatus(strLoadingHelp);{$ENDIF}
  end;

  procedure AddHTMLFile(TOCEntry,HelpFile: string);
  begin
    {$IFDEF DEBUG}SetStatus(strLoadingHelp+' ('+SmartPath(HelpFile)+')');{$ENDIF}
    HelpFacility^.AddHTMLHelpFile(HelpFile, TOCEntry);
    {$IFDEF DEBUG}SetStatus(strLoadingHelp);{$ENDIF}
  end;

var I,P: sw_integer;
    S: string;
    TopicTitle: string;
begin
  New(HelpFacility, Init);
  PushStatus(strLoadingHelp);
{  AddHTMLFile('User''s guide','C:\FP\USER\USER.HTM');}
  for I:=0 to HelpFiles^.Count-1 do
    begin
      S:=HelpFiles^.At(I)^; TopicTitle:='';
      P:=Pos('|',S);
      if P>0 then
        begin TopicTitle:=copy(S,P+1,255); S:=copy(S,1,P-1); end;
      if TopicTitle='' then TopicTitle:=S;
      if copy(UpcaseStr(ExtOf(S)),1,4)='.HTM' then { this recognizes both .htm and .html }
          AddHTMLFile(TopicTitle,S)
      else
        AddOAFile(S);
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
     New(HelpWindow, Init(R, 'Help', 0, 0, SearchFreeWindowNo));
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
     begin MessageBox('Sorry, modal help not yet implemented.',nil,mfInformation+mfInsertInApp+mfOKButton); Exit; end;
  HelpCreateWindow;
  with HelpWindow^ do
  begin
    HelpWindow^.ShowTopic(0,Context);
    if GetState(sfVisible)=false then Show;
    MakeFirst;
  end;
  Message(Application,evCommand,cmUpdate,nil);
end;

procedure HelpTopicSearch(Editor: PEditor);
var S: string;
begin
  if Editor=nil then S:='' else
  S:=GetEditorCurWord(Editor);
  HelpTopic(S);
end;

procedure HelpTopic(const S: string);
var FileID: word;
    Ctx   : THelpCtx;
var Found: boolean;
begin
  CheckHelpSystem;
  PushStatus(strLocatingTopic);
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
    PushStatus(strBuildingHelpIndex);
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

procedure InitHelpFiles;
begin
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
      Dispose(P, Done); { help windows are only hidden on close so we've
                          to destroy them manually }
    end;
end;
begin
  Desktop^.ForEach(@CloseIfHelpWindow);
end;


END.
{
  $Log$
  Revision 1.27  2000-02-07 11:58:01  pierre
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