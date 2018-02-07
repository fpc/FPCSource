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

procedure test1; assembler;
asm
  dw xx
  dw offset xx
  dd xx
  dd offset xx
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
  Writeln('Ok!');
end.
