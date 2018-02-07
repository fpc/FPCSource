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

procedure test1; assembler;
asm
  dw xx
  dw offset xx
  dd xx
  dd offset xx
  db 55h, 0aah
end;

procedure test2; assembler;
asm
  dw xx+5
  dw offset xx+6
  dd xx+7
  dd offset xx+8
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
  Writeln('Ok!');
end.
