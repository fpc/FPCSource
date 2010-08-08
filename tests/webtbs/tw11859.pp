{$ifdef fpc}
{$mode delphi}
{$endif}

{$z4}
type
  tenum = (ea,eb,ec);

var
  c1, c2: tobject;
  e: tenum;
begin
{$r-}
  c1:=tobject(pointer(12345));
  e:=tenum(c1);
  c2:=tobject(e);
  if (c1<>c2) then
    halt(1);
end.

