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
  Drivers,HelpCtx,WHlpView,WHTML,
{$ifdef EDITORS}
  Editors,
{$else}
  WEditor,
{$endif}
  FPViews;

type
    PIDEStatusLine = ^TIDEStatusLine;
    TIDEStatusLine = object(TAdvancedStatusLine)
      function  Hint(AHelpCtx: Word): String; virtual;
      procedure HandleEvent(var Event: TEvent); virtual;
    end;

procedure Help(FileID, Context: word; Modal: boolean);
procedure HelpIndex(Keyword: string);
procedure HelpTopicSearch(Editor: PEditor);
procedure HelpTopic(S: string);

procedure InitHelpSystem;
procedure DoneHelpSystem;

procedure InitHelpFiles;
procedure DoneHelpFiles;

procedure PushStatus(S: string);
procedure SetStatus(S: string);
procedure ClearStatus;
procedure PopStatus;

const
      HelpWindow     : PIDEHelpWindow = nil;
      HelpInited     : boolean = false;

implementation

uses Objects,Views,App,MsgBox,
     WHelp,WHTMLHlp,
     FPConst,FPVars,FPUtils;

const
    MaxStatusLevel = 10;

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

    hcRunMenu       : S:='Execution and parameters';
    hcRun           : S:='Run the current program';
    hcParameters    : S:='Set command-line parameters passed to program at execution';
    hcResetDebugger : S:='Reset Program';
    hcContToCursor  : S:='Go on until Cursor position';
    hcUserScreen    : S:='Switch to the full-screen user output';

    hcCompileMenu   : S:='Compile, build & make';
    hcCompile       : S:='Compile the current source file';
    hcMake          : S:='Rebuild soruce file and all other files that have been modified';
    hcBuild         : S:='Rebuild program and all available source files';
    hcTarget        : S:='Select target platform to compile for';
    hcPrimaryFile   : S:='Define then file that is the focus of Make and Build';
    hcClearPrimary  : S:='Clear the file previously set to Primary';
    hcInformation   : S:='Show compiler messages and program information';

    hcDebugMenu     : S:='';
    hcToggleBreakpoint : S:='Toggles Breakpoint';

    hcToolsMenu     : S:='User installed tools';
    hcCalculator    : S:='Show calculator';
    hcGrep          : S:='Run grep';

    hcToolsBase..
    hcToolsBase+MaxToolCount
                    : S:='User installed tool';

    hcOptionsMenu   : S:='Setting for compiler, editor, mouse, etc.';
    hcSwitchesMode  : S:='Select settings for normal, debug or release version';
    hcCompiler      : S:='Set default compiler directives and conditional defines';
    hcMemorySizes   : S:='Set default stack and heap sizes for generated programs';
    hcLinker        : S:='Set linker options';
    hcDebugger      : S:='Set debug information options';
    hcDirectories   : S:='Set paths for units, include, object and generated files';
    hcTools         : S:='Create or change tools';

    hcEnvironmentMenu:S:='Specify environment settins';
    hcPreferences   : S:='Specify desktop settings';
    hcEditor        : S:='Specify default editor settings';
    hcMouse         : S:='Specify mouse settings';
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
var I: integer;
    S: string;
begin
  New(HelpFacility, Init);
  PushStatus(strLoadingHelp);
{  AddHTMLFile('User''s guide','C:\FP\USER\USER.HTM');}
  for I:=0 to HelpFiles^.Count-1 do
    begin
      S:=HelpFiles^.At(I)^;
      if copy(UpcaseStr(ExtOf(S)),1,4)='.HTM' then
        AddHTMLFile(S,S)
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
  if HelpFacility<>nil then Dispose(HelpFacility, Done); HelpFacility:=nil;
  HelpInited:=false;
end;

procedure HelpCreateWindow;
var R: TRect;
begin
  CheckHelpSystem;
  if HelpWindow=nil then
  begin
     Desktop^.GetExtent(R); R.Grow(-15,-3); Dec(R.A.Y);
     New(HelpWindow, Init(R, 'Help', 0, 0, wnNoNumber));
     if HelpWindow<>nil then
     begin
       HelpWindow^.HelpCtx:=hcHelpWindow;
       HelpWindow^.HideOnClose:=true;
       HelpWindow^.Hide;
       Desktop^.Insert(HelpWindow);
     end;
  end;
end;

procedure Help(FileID, Context: word; Modal: boolean);
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

procedure HelpTopic(S: string);
var FileID, Ctx: word;
var Found: boolean;
begin
  CheckHelpSystem;
  PushStatus(strLocatingTopic);
  Found:=HelpFacility^.TopicSearch(S,FileID,Ctx);
  PopStatus;
  if Found then
     Help(FileID,Ctx,false) else
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
  if StatusLine=nil then Exit;
  If StatusStackPtr<=MaxStatusLevel then
    StatusStack[StatusStackPtr]:=PAdvancedStatusLine(StatusLine)^.GetStatusText
  else
    StatusStack[MaxStatusLevel]:=PAdvancedStatusLine(StatusLine)^.GetStatusText;
  SetStatus(S);
  Inc(StatusStackPtr);
end;

procedure PopStatus;
begin
  if StatusLine=nil then Exit;
  Dec(StatusStackPtr);
  If StatusStackPtr<=MaxStatusLevel then
    SetStatus(StatusStack[StatusStackPtr])
  else
    SetStatus(StatusStack[MaxStatusLevel]);
end;

procedure SetStatus(S: string);
begin
  if StatusLine=nil then Exit;
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
  if HelpFiles<>nil then Dispose(HelpFiles, Done);
end;

END.
{
  $Log$
  Revision 1.7  1999-02-08 17:40:01  pierre
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
