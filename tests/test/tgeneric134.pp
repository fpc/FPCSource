{ %FAIL }
{ a member that exists only as a generic, compared with "<" }
program tgeneric134;

{$mode delphi}

type
  TUtil = class
    class function OnlyGen<T>: Integer; static;
  end;

class function TUtil.OnlyGen<T>: Integer;
begin
  Result := 1;
end;

var
  b: Boolean;
begin
  b := TUtil.OnlyGen < 3;
  if b then
    Writeln('compiled');
end.
