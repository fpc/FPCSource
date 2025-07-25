program DemoEditor;
{$mode fpc}

uses
  {$ifdef UNIX}cwstring,{$endif}Objects,fvconsts,
  //Drivers, Views, Menus, StdDlg, App, Editors,Msgbox{$ifdef unix},fvclip { OSC 52 support unit } {$endif},FVCommon;
  uDrivers, uViews, uMenus, uStdDlg, uApp, uEditors,uMsgbox{$ifdef unix},ufvclip { OSC 52 support unit } {$endif},uFVCommon;


const
  cmShowClip   = 102;
  cmCopyWin    = 240;
  cmPasteWin   = 241;

type
  PEditorApp = ^TEditorApp;
  TEditorApp = object(TApplication)
    constructor Init;
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure InitMenuBar; virtual;
    procedure InitStatusLine; virtual;
  end;

  PMyEditWindow = ^TMyEditWindow;
  TMyEditWindow = object(TEditWindow)
    procedure HandleEvent(var Event: TEvent); virtual;
  end;

var
  EditorApp: TEditorApp;
  ClipWindow: PEditWindow;

function OpenEditor(FileName: FNameStr; Visible: Boolean): PEditWindow;
var
  P: PWindow;
  R: TRect;
begin
  DeskTop^.GetExtent(R);
  P := New(PMyEditWindow, Init(R, FileName, wnNoNumber));
  if not Visible then P^.Hide;
  OpenEditor := PEditWindow(Application^.InsertWindow(P));
end;

procedure TMyEditWindow.HandleEvent(var Event: TEvent);

procedure ClipCopyWin;
var p : pointer;
begin
{$ifdef unix}
  if Editor^.SelStart<>Editor^.SelEnd then { Text selected? }
  begin
    {This is where the magic happens. Parameters are PAnsiChar and Length of data to be copied to global clipboard}
    SetGlobalClipboardData( @Editor^.Buffer^[Editor^.BufPtr(Editor^.SelStart)], Editor^.SelEnd - Editor^.SelStart);
  end;
{$else}
  MessageBox('Not implemented for this platform!', nil, mfInformation + mfOkButton);
{$endif}
end;

procedure ClipPasteWin;
begin
{$ifdef unix}
  GetGlobalClipboardData; {Request data from global Clipboard. That's it}
{$else}
  MessageBox('Not implemented for this platform!', nil, mfInformation + mfOkButton);
{$endif}
end;

begin
  inherited HandleEvent(Event);
  case Event.What of
    evCommand:
      case Event.Command of
        cmCopyWin   : ClipCopyWin;
        cmPasteWin  : ClipPasteWin;
      else
        Exit;
      end;
  else
    Exit;
  end;
  ClearEvent(Event);
end;

constructor TEditorApp.Init;
var
  H: Word;
  R: TRect;
begin
  inherited Init;
  DisableCommands([cmSave, cmSaveAs, cmCut, cmCopy, cmPaste,
    {cmCopyWin, cmPasteWin,}
    cmClear, cmUndo, cmFind, cmReplace, cmSearchAgain]);
  EditorDialog := @StdEditorDialog;
  ClipWindow := OpenEditor('', False);
  if ClipWindow <> nil then
  begin
    Clipboard := ClipWindow^.Editor;
    Clipboard^.CanUndo := False;
  end;
end;

procedure TEditorApp.HandleEvent(var Event: TEvent);

procedure FileOpen;
var
  FileName: FNameStr;
begin
  FileName := '*.*';
  if ExecuteDialog(New(PFileDialog, Init('*.*', 'Open file',
    '~N~ame', fdOpenButton, 100)), @FileName) <> cmCancel then
    OpenEditor(FileName, True);
end;

procedure FileNew;
begin
  OpenEditor('', True);
end;

procedure ChangeDir;
begin
  ExecuteDialog(New(PChDirDialog, Init(cdNormal, 0)), nil);
end;

procedure ShowClip;
begin
  ClipWindow^.Select;
  ClipWindow^.Show;
end;

begin
  inherited HandleEvent(Event);
  case Event.What of
    evCommand:
      case Event.Command of
        cmOpen: FileOpen;
        cmNew: FileNew;
        cmChangeDir : ChangeDir;
        cmShowClip  : ShowClip;
      else
        Exit;
      end;
  else
    Exit;
  end;
  ClearEvent(Event);
end;

procedure TEditorApp.InitMenuBar;
var
  R: TRect;
begin
  GetExtent(R);
  R.B.Y := R.A.Y + 1;
  MenuBar := New(PMenuBar, Init(R, NewMenu(
    NewSubMenu('~F~ile', hcNoContext, NewMenu(
      StdFileMenuItems(
      nil)),
    NewSubMenu('~E~dit', hcNoContext, NewMenu(
      StdEditMenuItems(
      NewLine(
      NewItem('~S~hwo clipboard', '', kbNoKey, cmShowClip, hcNoContext,
      NewLine(
      NewItem('Cop~y~ to Windows', '', kbNoKey, cmCopyWin, hcNoContext,
      NewItem('Paste from ~W~indows', '', kbNoKey, cmPasteWin, hcNoContext,
      nil))))))),
    NewSubMenu('~S~earch', hcNoContext, NewMenu(
      NewItem('~F~ind...', '', kbNoKey, cmFind, hcNoContext,
      NewItem('~R~eplace...', '', kbNoKey, cmReplace, hcNoContext,
      NewItem('~S~earch again', '', kbNoKey, cmSearchAgain, hcNoContext,
      nil)))),
    NewSubMenu('~W~indows', hcNoContext, NewMenu(
      StdWindowMenuItems(
      nil)),
    nil)))))));
end;

procedure TEditorApp.InitStatusLine;
var
  R: TRect;
begin
  GetExtent(R);
  R.A.Y := R.B.Y - 1;
  New(StatusLine, Init(R,
    NewStatusDef(0, $FFFF,
      NewStatusKey('~F2~ Save', kbF2, cmSave,
      NewStatusKey('~F3~ Open', kbF3, cmOpen,
      NewStatusKey('~Alt-F3~ Close', kbAltF3, cmClose,
      NewStatusKey('~F5~ Zoom', kbF5, cmZoom,
      NewStatusKey('~F6~ Next', kbF6, cmNext,
      NewStatusKey('~F10~ Menu', kbF10, cmMenu,
      NewStatusKey('', kbCtrlF5, cmResize,
      nil))))))),
    nil)));
end;

begin
  EditorApp.Init;
  EditorApp.Run;
  EditorApp.Done;
end.
