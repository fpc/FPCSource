
function DoTest(params: array of PWideChar): WideString;
var
  i: integer;
  res: WideString;
begin
  res:='';
  for i:=Low(params) to High(params) do
    res:=res + params[i];
  DoTest:=res;
end;

var
  s: WideString;
begin
  s:=DoTest(['аб', 'вг', 'де']);
  if s <> 'абвгде' then begin
    writeln('Test failed. S=', s);
    Halt(1);
  end;
end.
