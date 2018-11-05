program KbdTest;

{$MODE objfpc}{$H+}

uses
  Video, Keyboard, Mouse, kbdutil, vidutil;

const
  LastPressedAttr = $E0;
  PreviouslyPressedAttr = $0F;
  NotPressedAttr = $6F;
  NotAvailableAttr = $08;
var
  kbd: TKeyboard;
  KbdEventMap: array of array [0..1] of TKeyEvent;
  KeyHasBeenPressed: array of Boolean;
  DumpF: TextFile;
  I: Integer;
  a1, a2: Int64;
  K, TK: TKeyEvent;
  M: TMouseEvent;
  FoundKey: Boolean;
begin
  if ParamCount <> 2 then
  begin
    Writeln('Usage: ', ParamStr(0), ' <kbd_file> <dump_file>');
    Halt(1);
  end;


  InitVideo;
  InitKeyboard;
  InitMouse;

  kbd := ReadKeyboardFromFile(ParamStr(1));
  SetLength(KbdEventMap, Length(kbd.Keys));
  SetLength(KeyHasBeenPressed, Length(kbd.Keys));
  AssignFile(DumpF, ParamStr(2));
  Reset(DumpF);
  for I := Low(kbd.Keys) to High(kbd.Keys) do
  begin
    KeyHasBeenPressed[I] := False;
    Readln(DumpF, a1, a2);
    if (a1 = -1) or (a2 = -1) then
    begin
      KbdEventMap[I][0] := 0;
      KbdEventMap[I][1] := 0;
      DrawKey(kbd.Keys[I], NotAvailableAttr);
    end
    else
    begin
      KbdEventMap[I][0] := a1;
      KbdEventMap[I][1] := a2;
      DrawKey(kbd.Keys[I], NotPressedAttr);
    end;
  end;
  CloseFile(DumpF);

  TextOut(1, 20, 'Press each of the highlighted keys.', $07);
  TextOut(1, 21, 'Click the right mouse button to exit.', $07);
  UpdateScreen(False);

  repeat
    repeat
      K := PollKeyEvent;
      if PollMouseEvent(M) then
        GetMouseEvent(M);
    until (K <> 0) or ((GetMouseButtons and MouseRightButton) <> 0);
    if K <> 0 then
    begin
      K := GetKeyEvent;
      TK := TranslateKeyEvent(K);

      FoundKey := False;
      for I := Low(kbd.Keys) to High(kbd.Keys) do
        if (KbdEventMap[I][0] = K) and (KbdEventMap[I][1] = TK) then
        begin
          FoundKey := True;
          KeyHasBeenPressed[I] := True;
          DrawKey(kbd.Keys[I], LastPressedAttr);
        end
        else if KeyHasBeenPressed[I] then
          DrawKey(kbd.Keys[I], PreviouslyPressedAttr)
        else if KbdEventMap[I][0] <> 0 then
          DrawKey(kbd.Keys[I], NotPressedAttr)
        else
          DrawKey(kbd.Keys[I], NotAvailableAttr);

      if not FoundKey then
      begin
        TextOut(1, 18, 'Unknown key code.', $04);
      end;
      UpdateScreen(False);
    end;
  until (GetMouseButtons and MouseRightButton) <> 0;

  DoneMouse;
  DoneKeyboard;
  DoneVideo;
end.
