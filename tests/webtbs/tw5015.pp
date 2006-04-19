{ %cpu=i386 }

{ Source provided for Free Pascal Bug Report 5015 }
{ Submitted by "Zeljan Rikalo" on  2006-04-15 }
{ e-mail: zeljko@holobit.net }
program test;

{$mode delphi}

const

  BitMaskTable: Array[0..31] of LongWord =
    ($00000001, $00000002, $00000004, $00000008,
     $00000010, $00000020, $00000040, $00000080,
     $00000100, $00000200, $00000400, $00000800,
     $00001000, $00002000, $00004000, $00008000,
     $00010000, $00020000, $00040000, $00080000,
     $00100000, $00200000, $00400000, $00800000,
     $01000000, $02000000, $04000000, $08000000,
     $10000000, $20000000, $40000000, $80000000);

  BitsPerByte      = 8;
  BitsPerWord      = 16;
  BitsPerLongWord  = 32;
  BytesPerCardinal = Sizeof(Cardinal);
  BitsPerCardinal  = BytesPerCardinal * 8;


function SetBit(const Value, BitIndex: LongWord): LongWord;
asm
     {$IFOPT R+}
      CMP     BitIndex, BitsPerLongWord
      JAE     @Fin
     {$ENDIF}
      OR      EAX, DWORD PTR [BitIndex * 4 + BitMaskTable]
    @Fin:
end;

begin

end.
