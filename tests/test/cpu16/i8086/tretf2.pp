{ %target=msdos }

{ test for i8086 inline assembler near and far ret instructions }

{ since testing and detecting near rets miscompiled as far (and vice versa)
  is hard, we don't actually execute the rets, but instead, before each ret,
  we issue an int instruction that calls our own interrupt handler that
  manually disassembles the instruction, checks that it is of the correct type
  and then skips the instruction. }

{ this test is Turbo Pascal 7 compatible }

program tretf2;

uses
  dos;

{$ifndef FPC}
type
  FarPointer = Pointer;
{$endif ndef FPC}

{$F+}

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
var
  opcode: Byte;
begin
  opcode:=Mem[CS:IP];
  if (opcode<>$C3) and (opcode<>$C2) then
    Error;
  Inc(IP);
  if opcode=$C2 then
    Inc(IP,2);
end;

procedure IntFarHandler(Flags, CS, IP, AX, BX, CX, DX, SI, DI, DS, ES, BP: Word); interrupt;
var
  opcode: Byte;
begin
  opcode:=Mem[CS:IP];
  if (opcode<>$CB) and (opcode<>$CA) then
    Error;
  Inc(IP);
  if opcode=$CA then
    Inc(IP,2);
end;

procedure TestAsm1; near; assembler;
asm
  int NearInt
  ret

  int NearInt
  ret 17
end;

procedure TestAsm2; far; assembler;
asm
  int FarInt
  ret

  int FarInt
  ret 85
end;

procedure TestAsm3; assembler;
asm
  int FarInt
  ret

  int FarInt
  ret 85
end;

procedure TestAsm4; assembler;
asm
  int NearInt
  retn

  int NearInt
  retn 5

  int FarInt
  retf

  int FarInt
  retf 15
end;

procedure TestPas1; near;
begin
  asm
    int NearInt
    ret

    int NearInt
    ret 17
  end;
end;

procedure TestPas2; far;
begin
  asm
    int FarInt
    ret

    int FarInt
    ret 85
  end;
end;

procedure TestPas3;
begin
  asm
    int FarInt
    ret

    int FarInt
    ret 85
  end;
end;

procedure TestPas4;
begin
  asm
    int NearInt
    retn

    int NearInt
    retn 5

    int FarInt
    retf

    int FarInt
    retf 15
  end;
end;

begin
  GetIntVec(NearInt, OldNearIntVec);
  SetIntVec(NearInt, @IntNearHandler);
  GetIntVec(FarInt, OldFarIntVec);
  SetIntVec(FarInt, @IntFarHandler);

  TestAsm1;
  TestAsm2;
  TestAsm3;
  TestAsm4;

  TestPas1;
  TestPas2;
  TestPas3;
  TestPas4;

  asm
    int NearInt
    retn

    int NearInt
    retn 5

    int FarInt
    retf

    int FarInt
    retf 15
  end;

  Writeln('Ok');

  SetIntVec(NearInt, OldNearIntVec);
  SetIntVec(FarInt, OldFarIntVec);
end.
