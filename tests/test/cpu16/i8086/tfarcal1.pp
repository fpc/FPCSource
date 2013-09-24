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

procedure CheckBug;
begin
  if Bug then
  begin
    Writeln('FAIL!!!');
    halt(1);
  end
  else
    Writeln('OK');
end;

procedure farretproc; assembler;
asm
  { hardcode it with db, because the compiler could generate a near ret, due
    to some bug }
  db $CB  { RETF }
end;

procedure testfarcall;
label
  NoBug;
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
    je NoBug
    mov sp, SavedSP  { restore the broken SP }
    inc ax
NoBug:
    mov Bug, al
    pop bx  { pop the saved CS }
    sti
  end;
  CheckBug;
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
