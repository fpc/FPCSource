unit vidcrt;

interface

uses
  Classes, Video, sysutils,
  mouse,
  Types, Math, keyboard;

{$include crth.inc}

implementation

var
  FGPen: Byte = 7;
  BGPen: Byte = 0;
  WinRect: TRect;

  LastKeys: array[0..1] of Char;
  LastKeysIdx: Integer = -1;


function PosToArray(px, py: Integer): Integer; inline;
begin
  PosToArray := px + py * ScreenWidth;
end;

procedure SetChar(p: Integer; c: Char); overload;
begin
  if (p >= 0) and (p < VideoBufSize) then
    VideoBuf^[p] := (BGPen shl 12) or (FGPen shl 8) or Byte(c);
end;

procedure SetChar(x,y: Integer; c: Char); overload;
begin
  SetChar(PosToArray(x,y), c);
end;

procedure ProcessKeyEvent(NKey: TKeyEvent);
var
  c1: Char;
begin
  c1 := GetKeyEventChar(NKey);
  if c1 = #0 then
  begin
    LastKeys[1] := #0;
    LastKeys[0] := Char((NKey shr 8) and $FF);
    LastKeysIdx := 1;
    case LastKeys[0] of
      #28: begin LastKeys[0] := #13; LastKeysIdx := 0; end; // Enter
      #01: begin LastKeys[0] := #27; LastKeysIdx := 0; end; // ESC
    end;
  end
  else
  begin
    LastKeys[0] := c1;
    LastKeysIdx := 0;
  end;
end;

function KeyPressed: Boolean;
var
  NKey: TKeyEvent;
begin
  KeyPressed := False;
  // Try to get a key if not already pressed one
  if LastKeysIdx < 0 then
  begin
    NKey := PollKeyEvent;
    if NKey <> 0 then
    begin
      ProcessKeyEvent(NKey);
    end;
  end;
  // if last key is set, return that we have something
  KeyPressed := LastKeysIdx <> 0;
end;

function ReadKey: Char;
var
  NKey: TKeyEvent;
begin
  ReadKey := #0;
  if LastKeysIdx < 0 then
  begin
    repeat
      NKey := GetKeyEvent;
      if NKey <> 0 then
      begin
        ProcessKeyEvent(NKey);
      end;
    until NKey <> 0;
  end;
  if LastKeysIdx >= 0 then
  begin
    ReadKey := LastKeys[LastKeysIdx];
    Dec(LastKeysIdx);
  end;
end;

procedure TextMode (Mode: word);
begin

end;

procedure Window(X1,Y1,X2,Y2: Byte);
begin
  x1 := EnsureRange(x1,1,256);
  y1 := EnsureRange(y1,1,256);
  x2 := EnsureRange(x2,x1,256);
  y2 := EnsureRange(y2,y1,256);
  WinRect := Rect(x1, y1, x2, y2);
  WindMinX := WinRect.Left - 1;
  WindMaxX := WinRect.Right - 1;
  WindMinY := WinRect.Top - 1;
  WindMaxY := WinRect.Bottom - 1;
  WindMin := WindMinX or (WindMinY shl 8);
  WindMax := WindMaxX or (WindMaxY shl 8);
  GoToXY(1,1);
end;

procedure GotoXY(X,Y: tcrtcoord);
begin
  SetCursorPos(x - 1, y - 1);
end;

function WhereX: tcrtcoord;
begin
  WhereX := CursorX + 1;
end;

function WhereY: tcrtcoord;
begin
  WhereY := CursorY + 1;
end;

procedure ClrScr;
var
  y: Integer;
begin
  for y := WinRect.Top to WinRect.Bottom do
  begin
    FillWord(VideoBuf^[PosToArray(WinRect.Left - 1, y - 1)], WinRect.Width + 1, (BGPen shl 12) or (FGPen shl 8));
  end;
  CursorX := 0;
  CursorY := 0;
  UpdateScreen(False);
end;

procedure ClrEol;
begin
  FillWord(VideoBuf^[PosToArray(WinRect.Left + CursorX - 1, WinRect.Top + CursorY - 1)], WinRect.Width - CursorX, (BGPen shl 12) or (FGPen shl 8));
  UpdateScreen(False);
end;

procedure InsLine;
var
  AFrom, ATo, i: Integer;
begin
  for i := WinRect.Bottom downto WinRect.Top + CursorY + 1 do
  begin
    AFrom := PosToArray(WinRect.Left - 1, i - 2);
    ATo := PosToArray(WinRect.Left - 1, i - 1);
    Move(VideoBuf^[AFrom], VideoBuf^[ATo], (WinRect.Width + 1) * SizeOf(Word));
  end;
  FillWord(VideoBuf^[PosToArray(WinRect.Left - 1, WinRect.Top + CursorY - 1)], WinRect.Width, (BGPen shl 12) or (FGPen shl 8));
  UpdateScreen(False);
end;

procedure DelLine;
var
  AFrom, ATo, i: Integer;
begin
  for i := WinRect.Top + CursorY + 1 to WinRect.Bottom do
  begin
    AFrom := PosToArray(WinRect.Left - 1, i - 1);
    ATo := PosToArray(WinRect.Left - 1, i - 2);
    Move(VideoBuf^[AFrom], VideoBuf^[ATo], (WinRect.Width + 1) * SizeOf(Word));
  end;
  FillWord(VideoBuf^[PosToArray(WinRect.Left - 1, WinRect.Bottom - 1)], WinRect.Width, (BGPen shl 12) or (FGPen shl 8));
  UpdateScreen(False);
end;

procedure TextColor(Color: Byte);
begin
  if InRange(Color, 0, 15) then
    FGPen := Color;
end;

procedure TextBackground(Color: Byte);
begin
  if InRange(Color, 0, 7) then
    BGPen := Color;
end;

procedure LowVideo;
begin

end;

procedure HighVideo;
begin

end;

procedure NormVideo;
begin

end;

procedure Delay(MS: Word);
begin
  Sleep(ms);
end;

procedure Sound(Hz: Word);
begin

end;

procedure NoSound;
begin

end;

procedure cursoron;
begin
  SetCursorType(crUnderline);
end;

procedure cursoroff;
begin
  SetCursorType(crHidden);
end;

procedure cursorbig;
begin
  SetCursorType(crBlock);
end;

procedure NextLine;
var
  i, AFrom, ATo: Integer;
begin
  Inc(CursorY);
  if CursorY > WinRect.Height then
  begin
    for i := WinRect.Top to WinRect.Bottom - 1 do
    begin
      AFrom := PosToArray(WinRect.Left - 1, i);
      ATo := PosToArray(WinRect.Left - 1, i - 1);
      Move(VideoBuf^[AFrom], VideoBuf^[ATo], (WinRect.Width + 1) * SizeOf(Word));
    end;
    CursorY := WinRect.Height;
    FillWord(VideoBuf^[PosToArray(WinRect.Left - 1, WinRect.Top - 1 + CursorY)], WinRect.Width + 1, (BGPen shl 12) or (FGPen shl 8));
  end;
end;

procedure WriteChar(c: Char);
var
  NX,NY: Integer;
begin
  // ignore #13, we only use #10
  case c of
    #13: begin
      //
    end;
    #10: begin
      CursorX := 0;
      NextLine;
    end;
    #7: begin
       Beep;
       Exit;
    end;
    #8: begin
       if CursorX > 0 then
         CursorX := CursorX - 1
    end;
    else
    begin
      // all other Chars
      NX := (WinRect.Left - 1) + CursorX;   // is zero based ... so both - 1
      NY := (WinRect.Top - 1) + CursorY;
      SetChar(NX, NY, c);
      if CursorX >= WinRect.Width then
      begin
        CursorX := 0;
        NextLine;
      end
      else
        Inc(CursorX);
    end;
  end;
end;

procedure CrtWrite(Var F: TextRec);
var
  i: Smallint;
begin
  for i := 0 to f.BufPos - 1 do
    WriteChar(F.Buffer[i]);
  UpdateScreen(False);
  F.BufPos := 0;
end;

Procedure CrtRead(Var F: TextRec);
var
  ch : Char;

  procedure BackSpace;
  begin
    if (f.bufpos>0) and (f.bufpos=f.bufend) then
     begin
       WriteChar(#8);
       WriteChar(' ');
       WriteChar(#8);
       dec(f.bufpos);
       dec(f.bufend);
     end;
  end;

Begin
  f.bufpos:=0;
  f.bufend:=0;
  repeat
    if f.bufpos > f.bufend then
     f.bufend := f.bufpos;
    ch := readkey;
    case ch of
      #0: begin
        readkey;
        Exit;
      end;
      ^S,
      #8: BackSpace;
      ^Y,
      #27: begin
        while f.bufpos < f.bufend do
        begin
          WriteChar(f.bufptr^[f.bufpos]);
          Inc(f.bufpos);
        end;
        while f.bufend>0 do
          BackSpace;
      end;
      #13: begin
        WriteChar(#13);
        WriteChar(#10);
        f.bufptr^[f.bufend] := #13;
        f.bufptr^[f.bufend + 1] := #10;
        Inc(f.bufend, 2);
          break;
      end;
      #26:
        if CheckEOF then
        begin
          f.bufptr^[f.bufend] := #26;
          Inc(f.bufend);
          break;
        end;
      else
      begin
        if f.bufpos < f.bufsize - 2 then
        begin
          f.buffer[f.bufpos] := ch;
          Inc(f.bufpos);
          WriteChar(ch);
        end;
      end;
    end;
    UpdateScreen(False);
  until False;
  f.bufpos := 0;
End;


function CrtReturn (var F: TextRec): integer;
begin
 CrtReturn:=0;
end;

procedure CrtClose(var F: TextRec);
begin
  F.Mode:=fmClosed;
end;

procedure CrtOpen(var F: TextRec);
begin
  if F.Mode = fmOutput then
  begin
    TextRec(F).InOutFunc := @CrtWrite;
    TextRec(F).FlushFunc := @CrtWrite;
  end
  else
  begin
    F.Mode:=fmInput;
    TextRec(F).InOutFunc:=@CrtRead;
    TextRec(F).FlushFunc:=@CrtReturn;
  end;
  TextRec(F).CloseFunc := @CrtClose;
end;

procedure AssignCrt(var F: Text);
begin
  Assign(F,'');
  TextRec(F).OpenFunc:=@CrtOpen;
end;

procedure InitCRT;
begin
  //
  AssignCrt(Output);
  Rewrite(Output);
  TextRec(Output).Handle := StdOutputHandle;
  //
  AssignCrt(Input);
  Reset(Input);
  TextRec(Input).Handle := StdInputHandle;
end;

procedure FreeCRT;
begin

end;

initialization
  InitVideo;
  {$ifdef HASAMIGA}
  SetWindowTitle(ExtractFileName(ParamStr(0)), 'CRT Window');
  {$endif}
  Window(1,1, ScreenWidth, ScreenHeight);
  SetCursorType(crUnderLine);
  //
  InitMouse;
  InitCRT;
  //
  InitKeyboard;
finalization
  DoneKeyboard;
  DoneMouse;
  //
  FreeCRT;
  DoneVideo;
end.

