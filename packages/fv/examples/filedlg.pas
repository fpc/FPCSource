program demofiledialog;
{$codepage utf8}
uses
  {$ifdef UNIX}cwstring,{$endif}
  //Objects, Drivers, Views, Menus, Dialogs, App, Stddlg, MsgBox, FVCommon; { for legacy uncomment this line and comment next line }
  Objects, uDrivers, uViews, uMenus, uDialogs, uApp, uStddlg, uMsgBox, uFVCommon; { for unicode support uncomment this line and comment previous line }

const cmOpneFileDlg =14523;
      cmDirChangeDlg=26745;
      cmDirChangeDlg2=3412;

{$if sizeof(sw_string)<=8}
const  cStr1: utf8String = '◀ ◌ ◂ ◃ ◄ ◅ ◆ ◇ ';
       cStr2: utf8String = ' ◈ ◉ ◊ ○ ◌ ◍ ◎ ●';
       cMoStr = '◇ ';
       cMcStr = '◊ ';
       cMeStr = '◌ ';

{$else}
const  cStr1: AnsiString = '';
       cStr2: AnsiString = '';
       cMoStr = '';
       cMcStr = '';
       cMeStr = '';
{$endif}

type
  PFileDlgApp = ^TFileDlgApp;
  TFileDlgApp = object(TApplication)
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure InitMenuBar; virtual;
  end;

procedure TFileDlgApp.InitMenuBar;
var
  R: TRect;
begin
  GetExtent(R);
  R.B.Y := R.A.Y + 1;
  MenuBar := New(PMenubar, Init(R, NewMenu(NewSubMenu('~M~enu',  hcNoContext,
    NewMenu(
    NewItem(cMoStr+'~O~pen File Dialog', 'F2', kbF2, cmOpneFileDlg, hcNoContext,
    NewItem(cMcStr+'~C~hange Directory Dialog', 'F4', kbF4, cmDirChangeDlg, hcNoContext,
    NewItem(cMeStr+'Change ~D~irectory Dialog II', 'F6', kbF6, cmDirChangeDlg2, hcNoContext,

    NewLine(
    NewItem('E~x~it', 'Alt-X', kbNoKey, cmQuit, hcNoContext,
    nil))))))
    ,
    nil))));
end;

procedure TFileDlgApp.HandleEvent(var Event: TEvent);

procedure OpenFileDialog;
var
  R: TRect;
  D: PFileDialog;
  S: Sw_String;
  P : pointer;
begin
  S:='*.pas';
  D := New(PFileDialog, Init(S,cStr1+'File dialog'+cStr2,'Chosen ~f~ile ',fdOkButton,199));
  //D := New(PFileDialog, Init(S,'File dialog','Chosen ~f~ile ',fdOkButton,199));
  { Resize }
  if Desktop^.Size.Y > 26 then
    D^.GrowTo(D^.Size.X,Desktop^.Size.Y-6);
  if Desktop^.Size.X > 60 then
    D^.GrowTo(Min(Desktop^.Size.X-(60-D^.Size.X),102),D^.Size.Y);
  { Number of columns in file open dialog }
  D^.FileList^.NumCols:= Max((D^.FileList^.Size.X-(D^.FileList^.Size.X div 14)) div 14,2);
  { Adjust scrollbar step and page step }
  D^.FileList^.SetRange(D^.FileList^.Range); {set again for scrollbar min max values}

  if ExecuteDialog(D, @S) <> cmCancel then
  begin
    P:=@S;
    MessageBox('The file %s', @P, mfInformation + mfOKButton);
  end;
end;

procedure DirChangeDialog;
var
  R: TRect;
  D: PChDirDialog;
  S: Sw_String;
begin
  GetDir(0,S); { current directory }
  D := New(PEditChDirDialog, Init(cdNormal,213));
  { Resize }
  if Desktop^.Size.Y > 26 then
    D^.GrowTo(D^.Size.X,Desktop^.Size.Y-6);
  if Desktop^.Size.X > 60 then
    D^.GrowTo(Min(Desktop^.Size.X-(60-D^.Size.X),102),D^.Size.Y);

  if ExecuteDialog(D, @S) <> cmCancel then
  begin
    MessageBox('The directory '+S, nil, mfInformation + mfOKButton);
  end;
end;

procedure DirChangeDialogII;
var
  R: TRect;
  D: PChDirDialog;
  S: Sw_String;
begin
  GetDir(0,S); { current directory }
  D := New(PChDirDialog, Init(cdNormal,213));
  { Resize }
  if Desktop^.Size.Y > 26 then
    D^.GrowTo(D^.Size.X,Desktop^.Size.Y-6);
  if Desktop^.Size.X > 60 then
    D^.GrowTo(Min(Desktop^.Size.X-(60-D^.Size.X),102),D^.Size.Y);

  if ExecuteDialog(D, nil) <> cmCancel then
  begin
    GetDir(0,S); { new current directory }
    MessageBox('The directory '+S, nil, mfInformation + mfOKButton);
  end;
end;



begin
  inherited HandleEvent(Event);
  case Event.What of
    evCommand:
      begin
        case Event.Command of
          cmOpneFileDlg : OpenFileDialog;
          cmDirChangeDlg: DirChangeDialog;
          cmDirChangeDlg2:DirChangeDialogII;
        else
          Exit;
        end;
        ClearEvent(Event);
      end;
  end;
end;

var
  FileDlgApp: TFileDlgApp;
begin
  FileDlgApp.Init;
  FileDlgApp.Run;
  FileDlgApp.Done;
end.
