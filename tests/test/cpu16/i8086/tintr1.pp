{ %target=msdos }

program tintr1;

{$ifdef FPC}
  {$mode tp}
{$endif FPC}

uses
  Dos;

{$ifndef FPC}
type
  FarPointer = Pointer;
{$endif FPC}

const
  IntNo = $F0;  { some unused interrupt vector }

var
  Handled: Boolean;

procedure TestFail;
begin
  Writeln('Failure!');
  Halt(1);
end;

procedure OurHandler; interrupt;
begin
  Handled := True;
end;

var
  regs: Registers;
  OldHandler: FarPointer;
begin
  regs.DS := $BAAD;
  regs.BP := $BEEF;
  regs.ES := $4A11;
  regs.AX := $1111;
  regs.BX := $2222;
  regs.CX := $3333;
  regs.DX := $4444;
  regs.SI := $5555;
  regs.DI := $6666;
  Handled := False;

  GetIntVec(IntNo, OldHandler);
  SetIntVec(IntNo, @OurHandler);

  Intr(IntNo, regs);

  SetIntVec(IntNo, OldHandler);

  if not Handled then
    TestFail;

  if regs.DS <> $BAAD then
    TestFail;
  if regs.BP <> $BEEF then
    TestFail;
  if regs.ES <> $4A11 then
    TestFail;
  if regs.AX <> $1111 then
    TestFail;
  if regs.BX <> $2222 then
    TestFail;
  if regs.CX <> $3333 then
    TestFail;
  if regs.DX <> $4444 then
    TestFail;
  if regs.SI <> $5555 then
    TestFail;
  if regs.DI <> $6666 then
    TestFail;

  Writeln('Success!')
end.
