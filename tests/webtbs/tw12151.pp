uses
  variants;
var
  Values: OleVariant;
  mTempNames: string;
begin
  Values := VarArrayCreate([0, 0], varVariant);
  //this row will cause runtime error
  Values[0] := VarArrayCreate([0, 1], varByte);
end.
