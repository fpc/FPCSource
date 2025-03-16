program onlycolorsel;
uses
  Objects, Drivers, Views, Menus, Dialogs, App, ColorSel;

const cmColors =12828;
      cmCreateSave = 31991;
      cmLoadColors =23467;

type
  PColorApp = ^TColorApp;
  TColorApp = object(TApplication)
    constructor Init;
    function GetPalette: PPalette; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure InitMenuBar; virtual;
  end;

procedure TutorStreamError(var S: TStream);
var ErrorMessage: String;
begin
  case S.Status of
    stError: ErrorMessage := 'Stream access error';
    stInitError: ErrorMessage := 'Cannot initialize stream';
    stReadError: ErrorMessage := 'Read beyond end of stream';
    stWriteError: ErrorMessage := 'Cannot expand stream';
    stGetError: ErrorMessage := 'Unregistered type read from stream';
    stPutError: ErrorMessage :='Unregistered type written to stream';
  end;
  ClearScreen;

  PrintStr('Error: ' + ErrorMessage);
  readln;
  Halt(Abs(S.Status));
end;

constructor TColorApp.Init;
begin
  StreamError := @TutorStreamError;
  RegisterObjects;
  RegisterViews;
  RegisterDialogs;
  RegisterApp;
  RegisterColorsel;
  inherited Init;
end;

function TColorApp.GetPalette: PPalette;
const
  CNewColor = CAppColor ;
  CNewBlackWhite = CAppBlackWhite ;
  CNewMonochrome = CAppMonochrome ;
  P: array[apColor..apMonochrome] of string[Length(CNewColor)] =
    (CNewColor, CNewBlackWhite, CNewMonochrome);
begin
  GetPalette := @P[AppPalette];
end;

procedure TColorApp.InitMenuBar;
var
  R: TRect;
begin
  GetExtent(R);
  R.B.Y := R.A.Y + 1;
  MenuBar := New(PMenubar, Init(R, NewMenu(NewSubMenu('~M~enu',  hcNoContext,
    NewMenu(
    NewItem('Color ~T~est', 'F2', kbF2, cmColors, hcNoContext,
    NewItem('Color ~S~ave', 'F4', kbF4, cmCreateSave, hcNoContext,
    NewItem('Color ~L~oad', 'F6', kbF6, cmLoadColors, hcNoContext,
    nil)))),
    nil))));
end;

procedure TColorApp.HandleEvent(var Event: TEvent);

procedure Colors;
var
  R: TRect;
  D: PDialog;
begin
  D := New(PColorDialog, Init('',
    ColorGroup('Desktop',       DesktopColorItems(nil),
    ColorGroup('Menus',         MenuColorItems(nil),
    ColorGroup('Dialogs',       DialogColorItems(dpGrayDialog, nil),
    ColorGroup('Editor',        WindowColorItems(wpBlueWindow, nil),
    ColorGroup('Ascii table',   WindowColorItems(wpGrayWindow, nil),
    ColorGroup('Calendar',
      WindowColorItems(wpCyanWindow,
      ColorItem('Current day',       22, nil)),
      nil))))))));

  if ExecuteDialog(D, Application^.GetPalette) <> cmCancel then
  begin
    ReDraw;
  end;
end;

procedure ColorsSave;
var
  R: TRect;
  D: PDialog;
var DesktopFile: TBufStream;
begin
  D := New(PColorDialog, Init('',
    ColorGroup('Desktop',       DesktopColorItems(nil),
    ColorGroup('Menus',         MenuColorItems(nil),
    ColorGroup('Dialogs',       DialogColorItems(dpGrayDialog, nil),
    ColorGroup('Editor',        WindowColorItems(wpBlueWindow, nil),
    ColorGroup('Ascii table',   WindowColorItems(wpGrayWindow, nil),
    ColorGroup('Calendar',
      WindowColorItems(wpCyanWindow,
      ColorItem('Current day',       22, nil)),
      nil))))))));

  DesktopFile.Init('DESKTOP.TUT', stCreate, 1024);
  DesktopFile.Put(D);
  DesktopFile.Done;

  Dispose(D,Done);
end;

procedure ColorsLoad;
var
  R: TRect;
  D: PDialog;
  p1, p2, p3 ,p4 : pointer;
var DesktopFile: TBufStream;
begin
  getmem(p1,50); {make some memory offset so load is in different pointers}
  getMem(p2,100);
  getMem(p3,30);
  getMem(p4,150);
  DesktopFile.Init('DESKTOP.TUT', stOpenRead, 1024);
  D:=PDialog(DesktopFile.Get);
  DesktopFile.Done;

  freemem(p1,50);
  freeMem(p2,100);
  freeMem(p3,30);
  freeMem(p4,150);

  if assigned(D) then
  if ExecuteDialog(D, Application^.GetPalette) <> cmCancel then
  begin
    ReDraw;
  end;
end;

begin
  inherited HandleEvent(Event);
  case Event.What of
    evCommand:
      begin
        case Event.Command of
          cmColors: Colors;
          cmCreateSave:ColorsSave;
          cmLoadColors:ColorsLoad;
        else
          Exit;
        end;
        ClearEvent(Event);
      end;
  end;
end;

var
  ColorApp: TColorApp;
begin
  ColorApp.Init;
  ColorApp.Run;
  ColorApp.Done;
end.

