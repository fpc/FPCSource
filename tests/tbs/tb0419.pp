
var
  nc : integer;
  test_w : word;

procedure array_test(b: integer; parr: array of word; c: integer);cdecl;
begin
  nc:=c;
  test_w:=parr[2];
end;


begin
  nc:=5;
  test_w:=$abcd;
  array_test(0,[1,2,3,4],56);
  if (nc<>56) or (test_w<>3) then
    begin
      Writeln('Wrong code generated');
    end;
end.
