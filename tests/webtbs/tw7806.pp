{$mode objfpc}
program project1;
{$H+}
uses variants;

function MyFunc(aVar : Variant) : Boolean;
begin
  WriteLn(VarToStr(aVar[0]));
  Result := False;
end;

var
  aVar : Variant;
  I : Integer;
begin
  for I := 0 to 10 do
  begin
    aVar := VarArrayOf(['Test', 0]);
    MyFunc(aVar);
  end;
  writeln('ok');
end.
