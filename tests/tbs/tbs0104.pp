{$ifdef go32v2}
uses
   dpmiexcp;
{$endif}

{ Two cardinal type bugs }
var
  c : cardinal;
begin
  c:=$80000000;
  writeln(c);
  c:=$80001234;
  writeln(c);
  c:=$ffffffff;
  writeln(c);
end.
