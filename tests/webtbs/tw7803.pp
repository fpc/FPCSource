{$mode objfpc}
program project1;
{$H+}
uses variants,varutils;

function MyFunc(aVar : Variant) : Boolean;
begin
  DumpVariant(TVarData(aVar));
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
    DumpVariant(TVarData(VarArrayOf(['Test', 0])));
    DumpVariant(TVarData(aVar));
    MyFunc(aVar);
  end;
end.
