{ %CPU=i8086 }
program tasm17;

{ This test is TP7 compatible }

{$ifndef FPC}
type
  CodePointer = Pointer;
{$endif}

var
  xx: word;

const
  expect1: array [0..6] of word =
    (Ofs(xx),Ofs(xx),Ofs(xx),Seg(xx),Ofs(xx),0,$aa55);

var
  expect2: array [0..6] of word;
  expect3: array [0..6] of word;
  expect4: array [0..12] of word;

procedure test1; assembler; {$IFDEF FPC_MM_HUGE}nostackframe;{$ENDIF}
asm
  dw xx
  dw offset xx
  dd xx
  dd offset xx
  db 55h, 0aah
end;

procedure test2; assembler; {$IFDEF FPC_MM_HUGE}nostackframe;{$ENDIF}
asm
  dw xx+5
  dw offset xx+6
  dd xx+7
  dd offset xx+8
  db 55h, 0aah
end;

procedure test3; assembler; {$IFDEF FPC_MM_HUGE}nostackframe;{$ENDIF}
asm
  dw 9+xx
  dw 11+offset xx
  dd 12+xx
  dd 14+offset xx
  db 55h, 0aah
end;

procedure test4; assembler; {$IFDEF FPC_MM_HUGE}nostackframe;{$ENDIF}
asm
  nop
  nop
@jumptab:
  nop
  nop
  dw @jumptarget
  nop
  nop
  dw offset @jumptarget
  nop
  nop
  dd @jumptarget
  nop
  nop
  dd offset @jumptarget
  nop
  nop
@jumptarget:
  db 55h, 0aah
end;

procedure Error;
begin
  Writeln('Error!');
  Halt(1);
end;

{ This version works in all i8086 memory models }
function CompareCode(cp: CodePointer; dp: Pointer; sz: Integer): Boolean;
var
  I: Integer;
begin
  for I := 0 to sz - 1 do
    if Mem[Seg(cp^):Ofs(cp^) + I] <> Mem[Seg(dp^):Ofs(dp^) + I] then
    begin
      CompareCode := False;
      exit;
    end;
  CompareCode := True;
end;

begin
  if not CompareCode(CodePointer(@test1), @expect1, SizeOf(expect1)) then
    Error;

  expect2[0] := Ofs(xx)+5;
  expect2[1] := Ofs(xx)+6;
  expect2[2] := Ofs(xx)+7;
  expect2[3] := Seg(xx);
  expect2[4] := Ofs(xx)+8;
  expect2[5] := 0;
  expect2[6] := $aa55;
  if not CompareCode(CodePointer(@test2), @expect2, SizeOf(expect2)) then
    Error;

  expect3[0] := Ofs(xx)+9;
  expect3[1] := Ofs(xx)+11;
  expect3[2] := Ofs(xx)+12;
  expect3[3] := Seg(xx);
  expect3[4] := Ofs(xx)+14;
  expect3[5] := 0;
  expect3[6] := $aa55;
  if not CompareCode(CodePointer(@test3), @expect3, SizeOf(expect3)) then
    Error;

  expect4[0] := $9090;
  expect4[1] := $9090;
  expect4[2] := Ofs(test4) + 24;
  expect4[3] := $9090;
  expect4[4] := Ofs(test4) + 24;
  expect4[5] := $9090;
  expect4[6] := Ofs(test4) + 24;
  expect4[7] := Seg(test4);
  expect4[8] := $9090;
  expect4[9] := Ofs(test4) + 24;
  expect4[10] := 0;
  expect4[11] := $9090;
  expect4[12] := $aa55;
  if not CompareCode(CodePointer(@test4), @expect4, SizeOf(expect4)) then
    Error;

  Writeln('Ok!');
end.
