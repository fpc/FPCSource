
var
  e: record
    case byte of
      1: (e: extended);
      2: (a: array[0..9] of byte);
  end; 
  i: longint;

  { assume little endian }
const
  a1: array[0..9] of byte = ($f8,$da,$4f,$8d,$97,$6e,$12,$83,$f5,$43);
  a2: array[0..9] of byte = ($05,$dd,$4f,$8d,$97,$6e,$12,$83,$f5,$43);
begin
{$ifdef FPC_HAS_TYPE_EXTENDED}
  writeln('testing');
  e.e := 1.7976931348623157e+305;
  for i:=low(a1) to high(a1) do
    if e.a[i]<>a1[i] then
      halt(1);
  e.e := 1.7976931348623158e+305;
  for i:=low(a1) to high(a1) do
    if e.a[i]<>a2[i] then
      halt(2);
{$endif}
end.
