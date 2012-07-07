{ %norun }
{ %opt=-vn -Sen }

{$mode objfpc}{$H+}

uses
  Classes, SysUtils;

function DoSomething(const AName: string): integer;
var
  i, l: Integer;
begin
  Result := 0;;
  l := Length(AName);
  for i:= 1 to Paramcount do begin
    if copy(ParamStr(i),1, l) = AName then
      inc(Result);
  end;
end;

begin
  DoSomething('a');
end.
