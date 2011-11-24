{ %cpu=i386 }
{$mode delphi}
type
  TCRCDef = record
    polynomial : longint;
  end;
{$asmmode intel}
function CRCSetup(var CRCDef: TCRCDef; Polynomial, Bits, InitVector,
  FinalVector: Cardinal; Inverse: LongBool): Boolean; register;assembler;
asm // initialize CRCDef according to the parameters, calculate the lookup table
       MOV [EAX].TCRCDef.Polynomial,EDX
end;
begin
end.
