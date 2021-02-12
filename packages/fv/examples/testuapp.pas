program testuapp;

{$codepage UTF8}

uses
  Objects, UDrivers, UViews, UMenus, UApp;

const
  cmOrderNew    = 200;
  cmOrderWin    = 201;
  cmOrderSave   = 202;
  cmOrderCancel = 203;
  cmOrderNext   = 204;
  cmOrderPrev   = 205;
  cmClipShow    = 210;
  cmAbout       = 220;
  cmFindOrderWindow = 1002;
  cmOptionsVideo = 1502;
  cmOptionsSave  = 1503;
  cmOptionsLoad  = 1504;

type

  { TMyUnicodeApp }

  TMyUnicodeApp = object(TApplication)
    procedure InitStatusLine; virtual;
  end;

var
  MyUnicodeApp: TMyUnicodeApp;

{ TMyUnicodeApp }

procedure TMyUnicodeApp.InitStatusLine;
var
  R: TRect;
begin
  GetExtent(R);
  R.A.Y := R.B.Y - 1;
  new(StatusLine, Init(R,
      NewStatusDef(0, $EFFF,
          NewStatusKey('~F1~ 打开', kbF1, cmHelp,
          NewStatusKey('~F3~ Отваряне', kbF3, cmOpen,
          NewStatusKey('~F4~ Νέος',  kbF4, cmNew,
          NewStatusKey('~Alt+F3~ Zavřít', kbAltF3, cmClose,
          NewStatusKey('~Alt-X~ Exit', kbAltX, cmQuit,
          nil))))),
      NewStatusDef($F000, $FFFF,
          NewStatusKey('~F6~ Next', kbF6, cmOrderNext,
          NewStatusKey('~Shift+F6~ Pref', kbShiftF6, cmOrderPrev,
          nil)),nil))));
end;

begin
  MyUnicodeApp.Init;
  MyUnicodeApp.Run;
  MyUnicodeApp.Done;
end.

