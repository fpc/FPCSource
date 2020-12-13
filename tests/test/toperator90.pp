{ %FAIL }

program toperator90;

{$mode objfpc}{$H+}

{ overloading the explicit assignment is NOT allowed }

operator Explicit (aArg: LongInt): Boolean;
begin
  Result := aArg <> 0;
end;

begin

end.
