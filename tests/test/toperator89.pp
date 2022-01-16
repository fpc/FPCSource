{ %NORUN }

program toperator89;

{$mode objfpc}{$H+}

{ overloading the implicit assignment is allowed }

operator := (aArg: LongInt): Boolean;
begin
  Result := aArg <> 0;
end;

begin

end.
