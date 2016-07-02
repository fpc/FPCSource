{ %cpu=i8086 }

{ test for local procedure data access from within inline asm }

{ this test is Turbo Pascal 7 compatible }

program tprcdat1;

{$ifdef FPC}
  {$ifdef FPC_MM_HUGE}
    {$warning This test only works if TestProc uses NoStackFrame modifier}
  {$endif}
{$endif}
procedure TestProc; far; assembler; {$ifdef FPC_MM_HUGE} nostackframe;{$endif}
asm
  dw $1234
  dw $4321
end;

var
  a,a_,a__,
  a2,a2_,a2__: Word;
begin
  asm
    mov ax, word [TestProc]
    mov a, ax
    mov ax, word ptr [TestProc]
    mov a_, ax
    mov ax, word ptr TestProc
    mov a__, ax
    mov ax, word ptr [TestProc + 2]
    mov a2, ax
    mov ax, word ptr TestProc + 2
    mov a2_, ax
    mov ax, word ptr [2 + TestProc]
    mov a2__, ax
  end;
  if (a=$1234) and (a_=$1234) and (a__=$1234) and
     (a2=$4321) and (a2_=$4321) and (a2__=$4321) then
    Writeln('Ok!')
  else
  begin
    Writeln('Error');
    Halt(1);
  end;
end.
