{ Keyboard BIOS dump tool. Dumps all keys and shift states using BIOS Int 16h
  function calls. This tool runs in DOS only. }

program kbdbdump;

{$MODE objfpc}{$H+}

uses
  Video, Mouse, kbdutil, vidutil, Dos;

procedure ShowShiftState;

  function BitAttr(var W: Word; bit: Integer): Byte;
  begin
    if (W and (1 shl bit)) <> 0 then
      BitAttr := $70
    else
      BitAttr := $07;
  end;

var
  Regs: Registers;
begin
  Regs.AH := $12;  { get extended shift states }
  Intr($16, Regs);
  TextOut( 1, 16, 'SysReq',      BitAttr(Regs.AX, 15));
  TextOut( 8, 16, 'Caps_Lock',   BitAttr(Regs.AX, 14));
  TextOut(18, 16, 'Num_Lock',    BitAttr(Regs.AX, 13));
  TextOut(27, 16, 'Scroll_Lock', BitAttr(Regs.AX, 12));
  TextOut(39, 16, 'Right_Alt',   BitAttr(Regs.AX, 11));
  TextOut(49, 16, 'Right_Ctrl',  BitAttr(Regs.AX, 10));
  TextOut(60, 16, 'Left_Alt',    BitAttr(Regs.AX,  9));
  TextOut(69, 16, 'Left_Ctrl',   BitAttr(Regs.AX,  8));

  TextOut( 1, 17, 'Insert',      BitAttr(Regs.AX,  7));
  TextOut( 8, 17, 'CapsLock',    BitAttr(Regs.AX,  6));
  TextOut(17, 17, 'NumLock',     BitAttr(Regs.AX,  5));
  TextOut(25, 17, 'ScrollLock',  BitAttr(Regs.AX,  4));
  TextOut(36, 17, 'Alt',         BitAttr(Regs.AX,  3));
  TextOut(40, 17, 'Ctrl',        BitAttr(Regs.AX,  2));
  TextOut(45, 17, 'Left_Shift',  BitAttr(Regs.AX,  1));
  TextOut(56, 17, 'Right_Shift', BitAttr(Regs.AX,  0));
end;

procedure SampleAllKeys(const Kbd: TKeyboard; const OutFileName: string);
var
  I: Integer;
  Regs: Registers;
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
      ShowShiftState;
      UpdateScreen(False);
      Regs.AH := $11;  { check for enhanced keystroke }
      Intr($16, Regs);
      if PollMouseEvent(M) then
        GetMouseEvent(M);
    until ((fZero and Regs.Flags) = 0) or ((GetMouseButtons and MouseRightButton) <> 0);
    if ((fZero and Regs.Flags) = 0) then
    begin
      Regs.AH := $10;  { get enhanced keystroke }
      Intr($16, Regs);
      Write(OutF, Regs.AX, ' ');
      Regs.AH := $12;  { get extended shift states }
      Intr($16, Regs);
      Writeln(OutF, Regs.AX);
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
  InitMouse;

  kbd := ReadKeyboardFromFile(ParamStr(1));
  DrawKeyboard(kbd);
  UpdateScreen(False);

  TextOut(1, 20, 'Press the highlighted key. Use the right mouse button to skip if the key', $07);
  TextOut(1, 21, 'cannot be detected.', $07);
  UpdateScreen(False);
  SampleAllKeys(kbd, ParamStr(2));

  DoneMouse;
  DoneVideo;
end.
