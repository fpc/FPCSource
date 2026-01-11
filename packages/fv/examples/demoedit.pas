{ This example is part of FPC Free Vision package.

  Program demonstrates Editor use in your Free Vision Application
  and manual clipboard manipulations.

  To have clipboard functionally all you have to do is to add Editor
  (TFileEditor) to Clipboard. By default Free Vision will try to use your OS
  clipboard as long there is one and as long it is supported by Free Vision.
  Otherwise it will fall back to local clipboard. Use of local clipboard can
  be triggered manually if desired. As well OS clipboard can be used directly
  via FVClip unit.

  Note. On Unix/Linux systems Free Vision uses OSC 52 clipboard and bracketed
  paste. It is assumed that OSC 52 clipboard is available. While real
  availability depends on particular terminal in use and particular
  configuration of it.
}
program DemoEditor;
{$mode fpc}

{ Have FV_UNICODE defined to compile and run with Free Vision Unicode version,
  otherwise single byte code page ShortString Free Vision version will be used }
{$define FV_UNICODE}

uses
  {$ifdef UNIX}cwstring,{$endif}Objects,fvconsts,
{$ifdef FV_UNICODE}
  uDrivers, uViews, uMenus, uStdDlg, uApp, uEditors,uMsgbox,ufvclip,uFVCommon;
{$else}
  Drivers, Views, Menus, StdDlg, App, Editors,Msgbox,fvclip,FVCommon;
{$endif}


const
  cmShowClip   = 102;
  cmCopyWin    = 240;
  cmPasteWin   = 241;
  cmLocalClip  = 242;
  cmOSClip     = 243;

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
  if Editor^.SelStart<>Editor^.SelEnd then { Text selected? }
    { This is where the magic happens. Parameters are PAnsiChar and Length of data to be copied to OS clipboard }
    SetTextWinClipboardData( @Editor^.Buffer^[Editor^.BufPtr(Editor^.SelStart)], Editor^.SelEnd - Editor^.SelStart);
end;

procedure ClipPasteWin;
var P : PAnsiChar; Len: Longint;
begin
  { Get OS clipboard data }
  if GetTextWinClipboardData (P, Len) then
    if assigned(P) then
    begin
      Editor^.InsertText (P, Len, False);  { On success insert into Editor window }
      FreeMem(P);  { Our responsibility to free memory }
    end;
end;

begin
  inherited HandleEvent(Event);
  case Event.What of
    evCommand:
      case Event.Command of
        cmCopyWin   : ClipCopyWin;
        cmPasteWin  : ClipPasteWin;
        {  Information. cmPasteText is triggered by Bracketed paste and
           OSC 52 paste. It is necessary handle cmPasteText event only for
           your own Views that are not descendants of TInputLine or TEditro.
           cmPasteText is not part of legacy Turbo Vision v2.0.
           Here Event.Id holds length of data in Event.InfoPtr PAnsiChar.
        cmPasteText : Editor^.InsertText(Event.InfoPtr,Event.Id,false);
        }
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
    cmClear, cmUndo, cmFind, cmReplace, cmSearchAgain]);
  if not WinClipboardSupported then
    DisableCommands([cmCopyWin, cmPasteWin, cmOSClip]);
  EditorDialog := @StdEditorDialog;
  ClipWindow := OpenEditor('', False);
  if ClipWindow <> nil then
  begin
    { Add Editor to Clipboard and by that clipboard functionality is enabled }
    Clipboard := ClipWindow^.Editor;
    Clipboard^.CanUndo := False;  { No Undo for clipboard }
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
        cmLocalClip :
          begin
            SetGlobalClipboard( False );
            MessageBox('Set to use local clipboard!', nil, mfInformation + mfOkButton);
          end;
        cmOSClip    :
          begin
            SetGlobalClipboard( True );
            MessageBox('Set to use OS clipboard!', nil, mfInformation + mfOkButton);
          end;
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
      NewItem('Use ~l~ocal clipboard', '', kbNoKey, cmLocalClip, hcNoContext,
      NewItem('Use ~O~S clipboard', '', kbNoKey, cmOSClip, hcNoContext,
      NewLine(
      NewItem('Cop~y~ to Windows', '', kbNoKey, cmCopyWin, hcNoContext,
      NewItem('Paste from ~W~indows', '', kbNoKey, cmPasteWin, hcNoContext,
      nil)))))))))),
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
