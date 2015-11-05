{ %target=msdos }

{ test for i8086 inline assembler indirect near and far jumps }

{ since testing and detecting near jumps miscompiled as far (and vice versa)
  is hard, we don't actually execute the jumps, but instead, before each jump,
  we issue an int instruction that calls our own interrupt handler that
  manually disassembles the instruction, checks that it is of the correct type
  and then skips the instruction. }

{ this test is Turbo Pascal 7 compatible }

program tfarjmp2;

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
  if ((modrm shr 3) and 7) <> 4 then
    Error;

  { 'jmp reg'? -> not an indirect jmp }
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
  if ((modrm shr 3) and 7) <> 5 then
    Error;

  { 'jmp far reg'??? -> invalid instruction }
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
    jmp word [a] { near }
    int NearInt
    jmp word ptr [a] { near }
    int NearInt
    jmp word ptr a { near }

    int FarInt
    jmp [a]      { far }

    int FarInt
    jmp a        { far }

    int FarInt
    jmp dword [b] { far }
    int FarInt
    jmp dword ptr [b] { far }
    int FarInt
    jmp dword ptr b { far }

    int NearInt
    jmp [b]       { near }

    int NearInt
    jmp b         { near }
  end;
end;

var
  g16: integer;
  g32: longint;
begin
  GetIntVec(NearInt, OldNearIntVec);
  SetIntVec(NearInt, Ptr(Seg(IntNearHandler),Ofs(IntNearHandler)));
  GetIntVec(FarInt, OldFarIntVec);
  SetIntVec(FarInt, Ptr(Seg(IntFarHandler),Ofs(IntFarHandler)));

  asm
    int NearInt
    jmp word ptr $1234
    int NearInt
    jmp word ptr [$1234]
    int FarInt
    jmp dword ptr $1234
    int FarInt
    jmp dword ptr [$1234]

    int NearInt
    jmp g16 { near }

    int NearInt
    jmp [g16] { near }

    int NearInt
    jmp word [g16] { near }
    int NearInt
    jmp word ptr [g16] { near }
    int NearInt
    jmp word ptr g16 { near }

    int FarInt
    jmp dword [g16] { far }
    int FarInt
    jmp dword ptr [g16] { far }
    int FarInt
    jmp dword ptr g16 { far }

    int FarInt
    jmp g32 { far }

    int FarInt
    jmp [g32] { far }

    int NearInt
    jmp word [g32] { near }
    int NearInt
    jmp word ptr [g32] { near }
    int NearInt
    jmp word ptr g32 { near }

    int FarInt
    jmp dword [g32] { far }
    int FarInt
    jmp dword ptr [g32] { far }
    int FarInt
    jmp dword ptr g32 { far }

    int NearInt
    jmp word [bx] { near }
    int NearInt
    jmp word ptr [bx] { near }
    int FarInt
    jmp dword [bx] { far }
    int FarInt
    jmp dword ptr [bx] { far }

{$ifdef FPC}
    { these three are supported by Free Pascal only. They don't work with
      Turbo Pascal 7's inline assembler. }

    { using the 'far' keyword }
    int FarInt
    jmp far [bx]

    { using the 'near' keyword }
    int NearInt
    jmp near [bx]

    { ambiguous (that's why it's not supported by TP7), but FPC supports it by
      extension from the 32-bit mode }
    int NearInt
    jmp [bx]
{$endif FPC}
  end;
  testloc(5, 10);
  Writeln('Ok');

  SetIntVec(NearInt, OldNearIntVec);
  SetIntVec(FarInt, OldFarIntVec);
end.
