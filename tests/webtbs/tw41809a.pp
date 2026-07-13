{ %OPT=-O2 -OoNODFA }
program tw41809a;

{$mode objfpc}

var
  dummy: integer;
  rec: record
    a, b: integer;
  end;
  q: integer;

function kek: boolean; inline;
begin
  result := (dummy <> 0) and (rec.b = 0);
end;

begin
  q := ParamCount;
  if kek and (rec.a <> 0) and (q * q + q = 7) then; // Access violation reading from address $00000000020007D0.
  WriteLn('ok');
end.
