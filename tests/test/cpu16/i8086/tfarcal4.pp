{ %target=msdos }

{ test for i8086 inline assembler near relative and far absolute calls }

{ since testing and detecting near calls miscompiled as far (and vice versa)
  is hard, we don't actually execute the calls, but instead, before each call,
  we issue an int instruction that calls our own interrupt handler that
  manually disassembles the instruction, checks that it is of the correct type
  and then skips the instruction. }

{ this test is Turbo Pascal 7 compatible }

program tfarcal4;

{$F-}

uses
  dos;

{$ifndef FPC}
type
  FarPointer = Pointer;
{$endif ndef FPC}

const
  NearInt = $E7;
  FarInt = $E8;

var
  OldNearIntVec: FarPointer;
  OldFarIntVec: FarPointer;

procedure Error;
begin
  Writeln('Error');
  SetIntVec(NearInt, OldNearIntVec);
  SetIntVec(FarInt, OldFarIntVec);
  halt(1);
end;

procedure IntNearHandler(Flags, CS, IP, AX, BX, CX, DX, SI, DI, DS, ES, BP: Word); interrupt;
begin
  if Mem[CS:IP]<>$E8 then
    Error;
  Inc(IP,3);
end;

procedure IntFarHandler(Flags, CS, IP, AX, BX, CX, DX, SI, DI, DS, ES, BP: Word); interrupt;
begin
  if Mem[CS:IP]<>$9A then
    Error;
  Inc(IP,5);
end;

procedure testproc1; near;
begin
end;

procedure testproc2; far;
begin
end;

procedure testproc3;
begin
end;

{
Note: BP7 treats interrupt procedures as near, as far as inline assembly call
and jmp instructions are concerned. This might be considered a bug, but we
should probably emulate it, or else we might break some BP7 code, that takes
this bug into account, such as:

  pushf
  push cs
  call testproc4  // produces a near call, not far
}
procedure testproc4; interrupt;
begin
end;

procedure testproc5(Flags, CS, IP, AX, BX, CX, DX, SI, DI, DS, ES, BP: Word); interrupt;
begin
end;

begin
  GetIntVec(NearInt, OldNearIntVec);
  SetIntVec(NearInt, @IntNearHandler);
  GetIntVec(FarInt, OldFarIntVec);
  SetIntVec(FarInt, @IntFarHandler);

  asm
    int NearInt
    call testproc1
    int NearInt
    call [testproc1]
    int FarInt
    call testproc2
    int FarInt
    call [testproc2]
    int NearInt
    call testproc3
    int NearInt
    call [testproc3]
    int NearInt
    call testproc4
    int NearInt
    call [testproc4]
    int NearInt
    call testproc5
    int NearInt
    call [testproc5]

    int NearInt
    call near ptr testproc1
    int NearInt
    call near ptr [testproc1]
    int NearInt
    call near ptr testproc2
    int NearInt
    call near ptr [testproc2]
    int NearInt
    call near ptr testproc3
    int NearInt
    call near ptr [testproc3]
    int NearInt
    call near ptr testproc4
    int NearInt
    call near ptr [testproc4]
    int NearInt
    call near ptr testproc5
    int NearInt
    call near ptr [testproc5]

    int FarInt
    call far ptr testproc1
    int FarInt
    call far ptr [testproc1]
    int FarInt
    call far ptr testproc2
    int FarInt
    call far ptr [testproc2]
    int FarInt
    call far ptr testproc3
    int FarInt
    call far ptr [testproc3]
    int FarInt
    call far ptr testproc4
    int FarInt
    call far ptr [testproc4]
    int FarInt
    call far ptr testproc5
    int FarInt
    call far ptr [testproc5]
  end;
  Writeln('Ok');

  SetIntVec(NearInt, OldNearIntVec);
  SetIntVec(FarInt, OldFarIntVec);
end.
