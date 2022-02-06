{ %FAIL }

program tanonfunc14;

{$mode objfpc}{$H+}
{$modeswitch anonymousfunctions}
{$modeswitch functionreferences}

{ verify that "ClassName" is not available even if the anonymous function is
  converted to a class instance method }

type
  tstrfunc = reference to function : string;

function Test: tstrfunc;
begin
  Result := function: string begin result := classname; end;
end;

var
  f: tstrfunc;
begin
  f := Test;
  writeln( f() )
end.

