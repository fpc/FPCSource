{ %fail }

program typecasttest;

{$mode objfpc}

procedure Test(const aArgs: array of const);
begin
  TVarRec(aArgs);
end;

begin

end.

