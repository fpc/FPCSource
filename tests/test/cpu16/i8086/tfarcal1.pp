{ %cpu=i8086 }

{ This test is Turbo Pascal 7 compatible }

{$IFDEF FPC}
  {$asmmode intel}
  {$goto on}

  { this test only applies to memory models with far code }
  {$ifdef FPC_MM_MEDIUM}
    {$define TEST_ENABLED}
  {$endif}
  {$ifdef FPC_MM_LARGE}
    {$define TEST_ENABLED}
  {$endif}
  {$ifdef FPC_MM_HUGE}
    {$define TEST_ENABLED}
    {$define NEED_POP_DS}
  {$endif}
{$ELSE FPC}
  {$define TEST_ENABLED}
{$ENDIF FPC}

{$IFDEF TP}
  {$define TEST_ENABLED}
{$ENDIF TP}

{$IFDEF TEST_ENABLED}
program tfarcal1;

{$F+}

var
  SavedSP: Word;
  Bug: Boolean;

procedure CheckBug(i : byte);
begin
  if Bug then
  begin
    Writeln('FAIL!!!');
    halt(i);
  end
  else
    Writeln('OK');
end;

procedure farretproc; assembler;
asm
  { Huge mode generates:
    push  ds
    mov   ax,TFARCAL1_DATA
    mov   ds,ax
    sequence }
{$ifdef NEED_POP_DS}
    pop ds
{$endif def NEED_POP_DS}
  { hardcode it with db, because the compiler could generate a near ret, due
    to some bug }
  db $CB  { RETF }
end;

procedure farretproc2; assembler; {$ifdef FPC} nostackframe;{$endif FPC}
asm
  { hardcode it with db, because the compiler could generate a near ret, due
    to some bug }
  { For huge mode, the sequence described above
    is not generated here }
  db $CB  { RETF }
end;

procedure testfarcall;
label
  NoBug1, NoBug2;
begin
  Write('Testing call farretproc...');
  asm
    cli

    { in case of a near call, the retf will pop this word and we'll detect the
      bug without crashing }
    push cs

    mov SavedSP, sp

    { this should emit a far call }
    call farretproc

    xor ax, ax
    cmp SavedSP, sp
    je NoBug1
    mov sp, SavedSP  { restore the broken SP }
    inc ax
NoBug1:
    mov Bug, al
    pop bx  { pop the saved CS }
    sti
  end;
  CheckBug(1);
  Write('Testing call farretproc2 with nostackframe modifier ...');
  asm
    cli

    { in case of a near call, the retf will pop this word and we'll detect the
      bug without crashing }
    push cs

    mov SavedSP, sp

    { this should emit a far call }
    call farretproc2

    xor ax, ax
    cmp SavedSP, sp
    je NoBug2
    mov sp, SavedSP  { restore the broken SP }
    inc ax
NoBug2:
    mov Bug, al
    pop bx  { pop the saved CS }
    sti
  end;
  CheckBug(2);
end;

begin
  testfarcall;
end
{$ELSE TEST_ENABLED}
program tfarcal1;
begin
end
{$ENDIF TEST_ENABLED}
.
