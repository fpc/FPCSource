{ It tests conversion from "array of char" to "array of PChar" }

function DoTest(params: array of PChar): string;
var
  i: integer;
  res: string;
begin
  res:='';
  for i:=Low(params) to High(params) do
    res:=res + params[i];
  DoTest:=res;
end;

var
  s: string;
begin
  s:=DoTest(['1', '2', '3']);
  if s <> '123' then begin
    writeln('Test failed. S=', s);
    Halt(1);
  end;
end.
