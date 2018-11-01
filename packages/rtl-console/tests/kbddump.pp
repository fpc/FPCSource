program kbddump;

{$MODE objfpc}{$H+}

uses
  Video, Keyboard, Mouse, kbdutil;

procedure TextOut(X, Y: Integer; const S: string; TextAttr: Byte);
var
  W, P, I, M: Integer;
begin
  P := ((X-1)+(Y-1)*ScreenWidth);
  M := Length(S);
  if (P+M) > ScreenWidth*ScreenHeight then
    M := ScreenWidth*ScreenHeight-P;
  for I := 1 to M do
    VideoBuf^[P+I-1] := Ord(S[I]) + (TextAttr shl 8);
end;

procedure DrawKey(const Key: TKey; TextAttr: Byte);
var
  Y: Integer;
begin
  for Y := Key.YTop to Key.YBottom do
  begin
    if Y = Key.Y then
      TextOut(Key.X + 1, Y + 1, Key.KeyLabel, TextAttr)
    else
      TextOut(Key.X + 1, Y + 1, StringOfChar(' ', Length(Key.KeyLabel)), TextAttr);
  end;
end;

procedure DrawKeyboard(const Kbd: TKeyboard);
var
  I: Integer;
begin
  for I := Low(kbd.Keys) to High(kbd.Keys) do
    DrawKey(kbd.Keys[I], $70);
end;

procedure SampleAllKeys(const Kbd: TKeyboard; const OutFileName: string);
var
  I: Integer;
  K: TKeyEvent;
  M: TMouseEvent;
  OutF: TextFile;
begin
  AssignFile(OutF, OutFileName);
  Rewrite(OutF);
  for I := Low(kbd.Keys) to High(kbd.Keys) do
  begin
    DrawKey(kbd.Keys[I], $17);
    UpdateScreen(False);

    repeat
      K := PollKeyEvent;
      if PollMouseEvent(M) then
        GetMouseEvent(M);
    until (K <> 0) or ((GetMouseButtons and MouseRightButton) <> 0);
    if K <> 0 then
    begin
      K := GetKeyEvent;
      Write(OutF, K, ' ');
      K:=TranslateKeyEvent(K);
      Writeln(OutF, K);
    end
    else
    begin
      Writeln(OutF, '-1 -1');
      while (GetMouseButtons and MouseRightButton) <> 0 do
      begin
        if PollMouseEvent(M) then
          GetMouseEvent(M);
      end;
    end;

    DrawKey(kbd.Keys[I], $70);
    UpdateScreen(False);
  end;
  CloseFile(OutF);
end;

var
  kbd: TKeyboard;
begin
  if ParamCount <> 2 then
  begin
    Writeln('Usage: ', ParamStr(0), ' <kbd_file> <output_file>');
    Halt(1);
  end;


  InitVideo;
  InitKeyboard;
  InitMouse;

  kbd := ReadKeyboardFromFile(ParamStr(1));
  DrawKeyboard(kbd);
  UpdateScreen(False);

  TextOut(1, 20, 'Press the highlighted key. Use the right mouse button to skip if the key', $07);
  TextOut(1, 21, 'cannot be detected.', $07);
  UpdateScreen(False);
  SampleAllKeys(kbd, ParamStr(2));

  DoneMouse;
  DoneKeyboard;
  DoneVideo;
end.
