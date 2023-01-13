program testuapp;

{$codepage UTF8}
{$h-}

uses
  Objects, UDrivers, UViews, UMenus, UDialogs, UApp, UMsgBox, UInpLong, UTabs, SysUtils;

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
  cmWindow3     = 1003;
  cmOptionsVideo = 1502;
  cmOptionsSave  = 1503;
  cmOptionsLoad  = 1504;

type

  { TMyUnicodeApp }

  TMyUnicodeApp = object(TApplication)
    P3 : PGroup;
    procedure HandleEvent(var Event : TEvent);virtual;
    procedure InitMenuBar; virtual;
    procedure InitStatusLine; virtual;
    procedure Window3;
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
      cmWindow3:
        Window3;
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
                 NewSubMenu('~E~dit', hcNoContext, NewMenu(
                 NewItem('Window ~3~','',kbNoKey,cmWindow3,hcNoContext,nil)),
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

procedure TMyUnicodeApp.Window3;
VAR R: TRect; P: PGroup; B: PScrollBar;
    List: PUnicodeStringCollection; Lb: PListBox;
begin
  { Create a basic dialog box. In it are buttons,  }
  { list boxes, scrollbars, inputlines, checkboxes }
  R.Assign(32, 2, 77, 18);                           { Assign screen area }
  P := New(PDialog, Init(R, '테스트 대화 상자'));         { Create dialog }
  If (P <> Nil) Then Begin                           { Dialog valid }
    R.Assign(5, 5, 20, 7);                          { Allocate area }
    P^.Insert(New(PCheckBoxes, Init(R,
      NewSItem('Test',
      NewSITem('Item 2', Nil)))));                   { Insert check box }
    R.Assign(5, 2, 20, 3);                           { Assign area }

    B := New(PScrollBar, Init(R));                   { Insert scroll bar }
    If (B <> Nil) Then Begin                         { Scrollbar valid }
      B^.SetRange(0, 100);                           { Set scrollbar range }
      B^.SetValue(50);                               { Set position }
      P^.Insert(B);                                  { Insert scrollbar }
    End;
    R.Assign(5, 10, 20, 11);                         { Assign area }

    P^.Insert(New(PInputLine, Init(R, 60)));         { Create input line }
    R.Assign(5, 13, 20, 14);                         { Assign area }

    P^.Insert(New(PInputLine, Init(R, 60)));         { Create input line }
    R.Assign(40, 8, 41, 14);                         { Assign area }

    B := New(PScrollBar, Init(R));                   { Create scrollbar }
    P^.Insert(B);                                    { Insert scrollbar }
    R.Assign(25, 8, 40, 14);                         { Assign area }

    Lb := New(PListBox, Init(R, 1, B));              { Create listbox }
    P^.Insert(Lb);                                   { Insert listbox }
    List := New(PUnicodeStringCollection, Init(10, 5));        { Create string list }
    List^.AtInsert(0, 'Зебра');              { Insert text }
    List^.AtInsert(1, '林檎');               { Insert text }
    List^.AtInsert(2, 'Third');              { Insert text }
    List^.AtInsert(3, 'Peach');              { Insert text }
    List^.AtInsert(4, 'За́ек');               { Insert text }
    List^.AtInsert(5, 'Item six');           { Insert text }
    List^.AtInsert(6, 'Jaguar');             { Insert text }
    List^.AtInsert(7, 'Melon');              { Insert text }
    List^.AtInsert(8, 'Ninth');              { Insert text }
    List^.AtInsert(9, 'Last item');          { Insert text }
    Lb^.Newlist(List);                               { Give list to listbox }
    R.Assign(30, 2, 40, 4);                          { Assign area }

    P^.Insert(New(PButton, Init(R, '好的', 100, bfGrabFocus)));{ Create okay button }
    R.Assign(30, 15, 40, 17);                        { Assign area }

    Desktop^.Insert(P);                              { Insert dialog }
    P3:=P;
  End;
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

