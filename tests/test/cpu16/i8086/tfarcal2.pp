{ %target=msdos }

{ test for i8086 inline assembler indirect near and far calls }

{ since testing and detecting near calls miscompiled as far (and vice versa)
  is hard, we don't actually execute the calls, but instead, before each call,
  we issue an int instruction that calls our own interrupt handler that
  manually disassembles the instruction, checks that it is of the correct type
  and then skips the instruction. }

{ this test is Turbo Pascal 7 compatible }

program tfarcal2;

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
var
  modrm: Byte;
begin
  if Mem[CS:IP]<>$FF then
    Error;
  Inc(IP);
  modrm := Mem[CS:IP];
  Inc(IP);
  if ((modrm shr 3) and 7) <> 2 then
    Error;

  { 'call reg'? -> not an indirect call }
  if (modrm shr 6)=3 then
    Error;

  case modrm shr 6 of
    0: if (modrm and 7) = 6 then
         Inc(IP, 2);  { disp16 }
    1: Inc(IP);    { disp8 }
    2: Inc(IP,2);  { disp16 }
  end;
end;

procedure IntFarHandler(Flags, CS, IP, AX, BX, CX, DX, SI, DI, DS, ES, BP: Word); interrupt;
var
  modrm: Byte;
begin
  if Mem[CS:IP]<>$FF then
    Error;
  Inc(IP);
  modrm := Mem[CS:IP];
  Inc(IP);
  if ((modrm shr 3) and 7) <> 3 then
    Error;

  { 'call far reg'??? -> invalid instruction }
  if (modrm shr 6)=3 then
    Error;

  case modrm shr 6 of
    0: if (modrm and 7) = 6 then
         Inc(IP, 2);  { disp16 }
    1: Inc(IP);    { disp8 }
    2: Inc(IP,2);  { disp16 }
  end;
end;

procedure testloc(a: longint; b: integer);
begin
  asm
    int NearInt
    call word [a] { near }
    int NearInt
    call word ptr [a] { near }
    int NearInt
    call word ptr a { near }

    int FarInt
    call [a]      { far }

    int FarInt
    call a        { far }

    int FarInt
    call dword [b] { far }
    int FarInt
    call dword ptr [b] { far }
    int FarInt
    call dword ptr b { far }

    int NearInt
    call [b]       { near }

    int NearInt
    call b         { near }
  end;
end;

var
  g16: integer;
  g32: longint;
begin
  GetIntVec(NearInt, OldNearIntVec);
  SetIntVec(NearInt, @IntNearHandler);
  GetIntVec(FarInt, OldFarIntVec);
  SetIntVec(FarInt, @IntFarHandler);

  asm
    int NearInt
    call word ptr $1234
    int NearInt
    call word ptr [$1234]
    int FarInt
    call dword ptr $1234
    int FarInt
    call dword ptr [$1234]

    int NearInt
    call g16 { near }

    int NearInt
    call [g16] { near }

    int NearInt
    call word [g16] { near }
    int NearInt
    call word ptr [g16] { near }
    int NearInt
    call word ptr g16 { near }

    int FarInt
    call dword [g16] { far }
    int FarInt
    call dword ptr [g16] { far }
    int FarInt
    call dword ptr g16 { far }

    int FarInt
    call g32 { far }

    int FarInt
    call [g32] { far }

    int NearInt
    call word [g32] { near }
    int NearInt
    call word ptr [g32] { near }
    int NearInt
    call word ptr g32 { near }

    int FarInt
    call dword [g32] { far }
    int FarInt
    call dword ptr [g32] { far }
    int FarInt
    call dword ptr g32 { far }

    int NearInt
    call word [bx] { near }
    int NearInt
    call word ptr [bx] { near }
    int FarInt
    call dword [bx] { far }
    int FarInt
    call dword ptr [bx] { far }

{$ifdef FPC}
    { these three are supported by Free Pascal only. They don't work with
      Turbo Pascal 7's inline assembler. }

    { using the 'far' keyword }
    int FarInt
    call far [bx]

    { using the 'near' keyword }
    int NearInt
    call near [bx]

    { ambiguous (that's why it's not supported by TP7), but FPC supports it by
      extension from the 32-bit mode }
    int NearInt
    call [bx]
{$endif FPC}
  end;
  testloc(5, 10);
  Writeln('Ok');

  SetIntVec(NearInt, OldNearIntVec);
  SetIntVec(FarInt, OldFarIntVec);
end.
