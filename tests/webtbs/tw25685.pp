{ %OPT=-O2 }
program Project1;

procedure Foo(StartPos, EndPos: Cardinal);
var
  s:string;
begin
  if (Cardinal((@s[1])^) >= StartPos) and (Cardinal((@s[1])^) <= EndPos) then
    writeln;
end;

var
  S: string;

begin
  foo(1,2);
  writeln(PByte(@S[1])^ = PByte(@S[1])^ );
end.
