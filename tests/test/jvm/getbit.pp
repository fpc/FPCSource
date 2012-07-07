program getbit;

{$mode delphi}

type
  plint = class
    digits: array of byte;
  end;

function LGetBit(A: PLInt; Bit: Cardinal): Integer;
begin
  Result := (A.Digits[(Bit - 1) shr 5 + 1] shr ((Bit - 1) and $1F{(Bit - 1) mod 32})) and 1;
end;

var
  p: plint;
begin
  p:=plint.create;
  setlength(p.digits,10);
  lgetbit(p,4);
end.

