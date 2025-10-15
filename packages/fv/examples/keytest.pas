PROGRAM KeyTest;

{ Set source file encoding to UTF-8 for correct handling of Unicode strings }
{$codepage UTF8}

USES
  {$ifdef unix}BaseUnix,{$endif}
  Objects,    { Base objects, TObject }
  UDrivers,   { System drivers (keyboard, mouse, video), Unicode version }
  UViews,     { Base classes for views (TView, TWindow), Unicode version }
  UMenus,     { Menu elements (TMenuBar, TMenu), Unicode version }
  UApp,       { Main application class TApplication, Unicode version }
  cwstring,   { Unicode string handling }
  SysUtils;   { For the Format function }

TYPE
  PKeyInfoView = ^TKeyInfoView;
  {
    TKeyInfoView is a special view that will display
    information about the last keyboard event.
  }
  TKeyInfoView = OBJECT(TView)
    LastKeyEvent: TEvent; { Store the last event here }
    TVInputValue: UnicodeString;
    CONSTRUCTOR Init(VAR Bounds: TRect);
    PROCEDURE Draw; VIRTUAL;
    PROCEDURE UpdateInfo(CONST Event: TEvent);
  END;

  PKeyTestApp = ^TKeyTestApp;
  {
    TKeyTestApp is the main class of our application.
  }
  TKeyTestApp = OBJECT(TApplication)
    KeyInfoView: PKeyInfoView; { Pointer to our view for displaying information }
    CONSTRUCTOR Init;
    PROCEDURE HandleEvent(VAR Event: TEvent); VIRTUAL;
    PROCEDURE InitMenuBar; VIRTUAL;
    PROCEDURE InitStatusLine; VIRTUAL;
  END;

{---------------------------------------------------------------------------}
{                        TKeyInfoView OBJECT METHODS                        }
{---------------------------------------------------------------------------}

CONSTRUCTOR TKeyInfoView.Init(VAR Bounds: TRect);
BEGIN
  Inherited Init(Bounds);
  Options := Options OR ofSelectable; { Make the View selectable so it can get focus }
  EventMask := $FFFF;                 { Accept all event types }
  FillChar(LastKeyEvent, SizeOf(TEvent), 0); { Initialize with zeros }
  LastKeyEvent.What := evNothing;     { No events initially }
END;

{ Function to format the modifier key state byte into a readable string }
FUNCTION FormatShiftState(State: Byte): UnicodeString;
VAR S: UnicodeString;
BEGIN
  S := '';
  IF (State AND kbRightShift) <> 0 THEN S := S + 'RightShift ';
  IF (State AND kbLeftShift) <> 0 THEN S := S + 'LeftShift ';
  IF (State AND kbCtrlShift) <> 0 THEN S := S + 'Ctrl ';
  IF (State AND kbAltShift) <> 0 THEN S := S + 'Alt ';
  IF (State AND kbScrollState) <> 0 THEN S := S + 'ScrollLock ';
  IF (State AND kbNumState) <> 0 THEN S := S + 'NumLock ';
  IF (State AND kbCapsState) <> 0 THEN S := S + 'CapsLock ';
  IF (State AND kbInsState) <> 0 THEN S := S + 'Insert ';
  IF S = '' THEN S := '(none)';
  FormatShiftState := S;
END;

PROCEDURE TKeyInfoView.Draw;
VAR
  B: TDrawBuffer;
  Line: UnicodeString;
  Y: Integer;
  Color: Byte;
BEGIN
  Color := GetColor(1);
  { Fill the view's background with spaces using the current color }
  MoveChar(B, ' ', Color, Size.X);
  FOR Y := 0 TO Size.Y - 1 DO
    WriteLine(0, Y, Size.X, 1, B);

  { Set the color for the text }
  Color := GetColor(2);

  { If no key has been pressed yet, display a prompt }
  IF LastKeyEvent.What = evNothing THEN
  BEGIN
    Line := 'Press any key to analyze...';
    MoveStr(B, Line, Color);
    WriteLine(1, 1, StrWidth(Line), 1, B);
    Exit;
  END;

  { Display all information from the TEvent record }
  Line := Format('Event.What: $%4.4x (evKeyDown)', [LastKeyEvent.What]);
  MoveStr(B, Line, Color);
  WriteLine(1, 1, StrWidth(Line), 1, B);

  Line := Format('KeyCode:     $%4.4x', [LastKeyEvent.KeyCode]);
  MoveStr(B, Line, Color);
  WriteLine(1, 2, StrWidth(Line), 1, B);

  Line := Format('CharCode:    ''%s'' ($%2.2x)', [LastKeyEvent.CharCode, Ord(LastKeyEvent.CharCode)]);
  MoveStr(B, Line, Color);
  WriteLine(1, 3, StrWidth(Line), 1, B);

  Line := Format('ScanCode:    $%2.2x', [LastKeyEvent.ScanCode]);
  MoveStr(B, Line, Color);
  WriteLine(1, 4, StrWidth(Line), 1, B);

  Line := Format('UnicodeChar: ''%s'' (U+%4.4x)', [LastKeyEvent.UnicodeChar, Ord(LastKeyEvent.UnicodeChar)]);
  MoveStr(B, Line, Color);
  WriteLine(1, 5, StrWidth(Line), 1, B);

  Line := 'KeyShift:    $' + IntToHex(LastKeyEvent.KeyShift, 2);
  MoveStr(B, Line, Color);
  WriteLine(1, 6, StrWidth(Line), 1, B);

  Line := '             ' + FormatShiftState(LastKeyEvent.KeyShift);
  MoveStr(B, Line, Color);
  WriteLine(1, 7, StrWidth(Line), 1, B);

  Line := 'TV_INPUT Env Var: ';
  if TVInputValue <> '' then
    Line := Line + TVInputValue
  else
    Line := Line + '(not set)';
  MoveStr(B, Line, Color);
  WriteLine(1, 9, StrWidth(Line), 1, B);
END;

PROCEDURE TKeyInfoView.UpdateInfo(CONST Event: TEvent);
BEGIN
  LastKeyEvent := Event;
  DrawView; { Request a redraw }
END;

{---------------------------------------------------------------------------}
{                        TKeyTestApp OBJECT METHODS                         }
{---------------------------------------------------------------------------}

CONSTRUCTOR TKeyTestApp.Init;
VAR
  R, ViewRect: TRect;
  MainWindow: PWindow;
BEGIN
  Inherited Init;

  { Create the main window that will contain our View }
  GetExtent(R);
  R.Grow(-5, -5); { Shrink it a bit to have a margin from the screen edges }
  MainWindow := New(PWindow, Init(R, 'Keyboard Event Inspector', wnNoNumber));

  { Create our View for displaying information. Its coordinates must be relative
    to the parent window. To fill the entire client area, its size should be
    2 chars less in width and height, and its origin should be at (1,1). }
  ViewRect.Assign(1, 1, MainWindow^.Size.X - 1, MainWindow^.Size.Y - 1);
  KeyInfoView := New(PKeyInfoView, Init(ViewRect));
  KeyInfoView^.TVInputValue := {$ifdef unix}fpgetenv('TV_INPUT'){$else unix}''{$endif};
  MainWindow^.Insert(KeyInfoView);

  { Insert the window into the Desktop }
  DeskTop^.Insert(MainWindow);
END;

PROCEDURE TKeyTestApp.HandleEvent(VAR Event: TEvent);
BEGIN
  { First, call the ancestor's handler so standard things like menus work }
  Inherited HandleEvent(Event);

  { If the event is a key press, update the info in our View }
  IF Event.What = evKeyDown THEN
  BEGIN
    IF Assigned(KeyInfoView) THEN
      KeyInfoView^.UpdateInfo(Event);
    { Don't clear the event, so standard handlers (like Alt+X) also get to process it }
  END;
END;

PROCEDURE TKeyTestApp.InitMenuBar;
VAR R: TRect;
BEGIN
  GetExtent(R);
  R.B.Y := R.A.Y + 1;
  MenuBar := New(PMenuBar, Init(R, NewMenu(
    NewSubMenu('~F~ile', hcNoContext, NewMenu(
      NewItem('E~x~it', 'Alt+X', kbAltX, cmQuit, hcNoContext, NIL)
    ), NIL)
  )));
END;

PROCEDURE TKeyTestApp.InitStatusLine;
var
   R: TRect;
begin
  GetExtent(R);
  R.A.Y := R.B.Y - 1;
  New(StatusLine,
    Init(R,
      NewStatusDef(0, $FFFF,
        NewStatusKey('~Alt+X~ Exit', kbAltX, cmQuit, nil),
        nil
      )
    )
  );
end;

{---------------------------------------------------------------------------}
{                           MAIN PROGRAM BLOCK                              }
{---------------------------------------------------------------------------}
VAR
  MyApp: TKeyTestApp;
BEGIN
  MyApp.Init;
  MyApp.Run;
  MyApp.Done;
END.
