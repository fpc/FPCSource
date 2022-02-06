program tanonfunc15;

{$mode objfpc}{$H+}
{$modeswitch anonymousfunctions}
{$modeswitch functionreferences}

{ "ClassName" inside an anonymous function inside a method returns the
  "ClassName" of the surrounding class }

type
  tstrfunc = reference to function : string;

  TTest = class
    function Test: tstrfunc;
  end;

function TTest.Test: tstrfunc;
begin
  Result := function: string begin result := classname; end;
end;

var
  f: tstrfunc;
  t: TTest;
begin
  t := TTest.Create;
  f := t.Test;
  if f() <> 'TTest' then
    Halt(1);
  t.Free;
end.

