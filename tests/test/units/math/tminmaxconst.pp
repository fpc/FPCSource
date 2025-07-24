uses
  sysutils,math;
var
  s: Single;
  d: Double;
{$if sizeof(extended) <> sizeof(double)}
  e: Extended;
{$endif}
begin
  s := MaxSingle;
  d := MaxDouble;
  Writeln(IntToHex(PLongInt(@s)^, 8));
  if IntToHex(PLongInt(@s)^, 8)<>'7F7FFFFF' then
     halt(1);
  Writeln(IntToHex(PInt64(@d)^, 16));
  if IntToHex(PInt64(@d)^, 16)<>'7FEFFFFFFFFFFFFF' then
    halt(2);
  s := MinSingle;
  d := MinDouble;
  Writeln(IntToHex(PLongInt(@s)^, 8));
  if IntToHex(PLongInt(@s)^, 8)<>'00800000' then
    halt(3);
  Writeln(IntToHex(PInt64(@d)^, 16));
  if IntToHex(PInt64(@d)^, 16)<>'0010000000000000' then
    halt(4);
{$if sizeof(extended) <> sizeof(double)}
  e := MinExtended;
  Writeln(IntToHex(TExtended80Rec(e)._Exp)+IntToHex(TExtended80Rec(e).Frac));
  if IntToHex(TExtended80Rec(e)._Exp)+IntToHex(TExtended80Rec(e).Frac)<>'00018000000000000000' then
    halt(5);
  e := MaxExtended;
  Writeln(IntToHex(TExtended80Rec(e)._Exp)+IntToHex(TExtended80Rec(e).Frac));
  if IntToHex(TExtended80Rec(e)._Exp)+IntToHex(TExtended80Rec(e).Frac)<>'7FFEFFFFFFFFFFFFFFFF' then
    halt(6);
{$endif}
  writeln('ok');
end.
