
var
  nc : integer;

procedure array_test(b: integer; parr: array of word; c: integer);cdecl;
begin
  nc:=c;
end;


begin
  nc:=5;
  array_test(0,[1,2,3,4],56);
  if nc<>56 then
    begin
      Writeln('Wrong code generated');
    end;
end.
