{ %opt=-Cg }
{ %target=-linux,freebsd,darwin }
{ %norun }
{$mode objfpc}

procedure permute({out} const _out: PByte; {in} const _in: PByte; {in} const p: PByte; {in} const n: Integer);
var
  i: Integer;
begin
  for i := 0 to n-1 do
    _out[i] := _in[p[i]-1];
end;

begin
end.
