{ %target=msdos }

program tintr2;

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
  HandledRegs: Registers;
  HandledCS, HandledIP: Word;

procedure TestFail;
begin
  Writeln('Failure!');
  Halt(1);
end;

procedure OurHandler(Flags, CS, IP, AX, BX, CX, DX, SI, DI, DS, ES, BP: Word); interrupt;
begin
  Handled := True;
  HandledRegs.Flags := Flags;
  HandledCS := CS;
  HandledIP := IP;
  HandledRegs.AX := AX;
  HandledRegs.BX := BX;
  HandledRegs.CX := CX;
  HandledRegs.DX := DX;
  HandledRegs.SI := SI;
  HandledRegs.DI := DI;
  HandledRegs.DS := DS;
  HandledRegs.ES := ES;
  HandledRegs.BP := BP;

  DS := $DAAB;
  BP := $FEEB;
  ES := $11A4;
  AX := $FFFF;
  BX := $EEEE;
  CX := $DDDD;
  DX := $CCCC;
  SI := $BBBB;
  DI := $AAAA;
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

  if HandledRegs.DS <> $BAAD then
    TestFail;
  if HandledRegs.BP <> $BEEF then
    TestFail;
  if HandledRegs.ES <> $4A11 then
    TestFail;
  if HandledRegs.AX <> $1111 then
    TestFail;
  if HandledRegs.BX <> $2222 then
    TestFail;
  if HandledRegs.CX <> $3333 then
    TestFail;
  if HandledRegs.DX <> $4444 then
    TestFail;
  if HandledRegs.SI <> $5555 then
    TestFail;
  if HandledRegs.DI <> $6666 then
    TestFail;

  if regs.DS <> $DAAB then
    TestFail;
  if regs.BP <> $FEEB then
    TestFail;
  if regs.ES <> $11A4 then
    TestFail;
  if regs.AX <> $FFFF then
    TestFail;
  if regs.BX <> $EEEE then
    TestFail;
  if regs.CX <> $DDDD then
    TestFail;
  if regs.DX <> $CCCC then
    TestFail;
  if regs.SI <> $BBBB then
    TestFail;
  if regs.DI <> $AAAA then
    TestFail;

  Writeln('Success!')
end.
