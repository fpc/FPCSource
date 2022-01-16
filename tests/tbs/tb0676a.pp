{ %opt=-vw -Sew }
{ %norun }

{$mode objfpc}

function SwapEndian(const AValue: Word): Word;inline;
  begin
    Result := Word((AValue shr 8) or (AValue shl 8));
  end;

const
  Value = 8008;

var
  v: Word;
begin
  writeln(sizeof(Value));
  Writeln(HexStr(Value, 4));
  v := swapendian(Value);
  Writeln(HexStr(v, 4));
end.
