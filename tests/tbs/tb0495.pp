{ %cpu=i386 }
{ %OPT=-Cg- }
{$asmmode intel}
var
  Digits : array[0..63] of byte;
type
  PBcd = ^TBcd;
  TBcd  = packed record
    Precision: Byte;                        { 1..64 }
    SignSpecialPlaces: Byte;                { Sign:1, Special:1, Places:6 }
    Fraction: packed array [0..31] of Byte; { BCD Nibbles, 00..99 per Byte, high Nibble 1st }
  end;

var
  c : currency;
  bcd : TBcd;

begin
  c:=1;
  asm
    lea esi,c
    fild [esi].currency

    lea esi,bcd
    mov [esi].TBcd.SignSpecialPlaces,dl

    mov eax,3
    mov digits.byte[eax],0
    mov digits.word[eax],0
  end
end.
