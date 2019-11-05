{ %TARGET=win32 }
program tw30205;
{$calling cdecl}
procedure ietest( var f: ansistring );
var
  x: ansistring;
begin
  x :='1234';
  f := x;
end;
begin
end.
