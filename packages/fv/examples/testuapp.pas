program testuapp;

{$codepage UTF8}

uses
  Objects, UDrivers, UViews, UMenus, UApp, UMsgBox, SysUtils;

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
    procedure HandleEvent(var Event : TEvent);virtual;
    procedure InitMenuBar; virtual;
    procedure InitStatusLine; virtual;
    procedure ShowAboutBox;
  end;

var
  MyUnicodeApp: TMyUnicodeApp;

{ TMyUnicodeApp }

procedure TMyUnicodeApp.HandleEvent(var Event: TEvent);
begin
  inherited HandleEvent(Event);
  if Event.What = evCommand then
  begin
    case Event.Command of
      cmAbout:
        ShowAboutBox;
      else
        Exit;
    end;
  end;
  ClearEvent(Event);
end;

procedure TMyUnicodeApp.InitMenuBar;
var
  R: TRect;
begin
  GetExtent(R);
  R.B.Y := R.A.Y + 1;
  MenuBar := new (PMenuBar, Init(R, NewMenu(
                 NewSubMenu('打开', hcNoContext, NewMenu(NewItem('~Н~ов打тест по пъ́тя',  'Еф2', kbF2, cmNew, hcNew,
                      NewItem('~O~pen', '💩', kbF3, cmOpen, hcOpen,
                      NewLine(
                      NewItem('E~x~it', 'ъ́ъ́ъ́打', kbAltX, cmQuit, hcNoContext, nil))))),
                 NewSubMenu('~E~dit', hcNoContext, NewMenu({GetEditMenuItems(nil)}nil),
                 NewSubMenu('~O~rders', hcNoContext, {NewMenu(GetOrdersMenuItems(nil))}nil,
                 NewSubMenu('O~p~tions', hcNoContext, {NewMenu(GetOptionsMenuItems(nil))}nil,
                 NewSubMenu('~W~indow', hcNoContext, {NewMenu(GetWindowMenuItems(nil))}nil,
                 NewSubMenu('~H~elp', hcNoContext, NewMenu(NewItem('~A~bout...','',kbNoKey,cmAbout,hcNoContext,
                       nil)), nil)))))))));
end;

procedure TMyUnicodeApp.InitStatusLine;
var
  R: TRect;
begin
  GetExtent(R);
  R.A.Y := R.B.Y - 1;
  new(StatusLine, Init(R,
      NewStatusDef(0, $EFFF,
          NewStatusKey('~F1~ По пъ́тя', kbF1, cmHelp,
          NewStatusKey('~F2~ 打开', kbF2, cmOpen,
          NewStatusKey('~F3~ Отваряне', kbF3, cmOpen,
          NewStatusKey('~F4~ Νέος',  kbF4, cmNew,
          NewStatusKey('~Alt+F3~ Zavřít', kbAltF3, cmClose,
          NewStatusKey('~Alt-X~ Exit', kbAltX, cmQuit,
          nil)))))),
      NewStatusDef($F000, $FFFF,
          NewStatusKey('~F6~ Next', kbF6, cmOrderNext,
          NewStatusKey('~Shift+F6~ Pref', kbShiftF6, cmOrderPrev,
          nil)),nil))));
end;

procedure TMyUnicodeApp.ShowAboutBox;
begin
  MessageBox(#3'Free Vision TUI Framework'#13 +
    #3'Test/Demo Application'#13+
    #3'Мога да ям стъкло, то не ми вреди.'#13+
    #3'我能吞下玻璃而不伤身体。',
    nil, mfInformation or mfOKButton);
end;

begin
  MyUnicodeApp.Init;
  MyUnicodeApp.Run;
  MyUnicodeApp.Done;
end.

