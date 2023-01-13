program kbddump;

{$MODE objfpc}{$H+}

uses
  Video, Keyboard, Mouse, kbdutil, vidutil;

procedure SampleAllKeys(const Kbd: TKeyboard; const OutFileName: AnsiString);
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
