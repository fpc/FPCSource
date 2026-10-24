{ %FAIL }
{ "type of Instance is aClass" is parsed as "(type of Instance) is aClass",
  so the left side of "is" is a type, not an instance. }
program ttypeinquiry15;

{$mode objfpc}

type
  TA = class end;
  TB = class(TA) end;

var
  Instance: TA;
begin
  Instance:=TB.Create;
  if type of Instance is TB then
    writeln('yes');
  Instance.Free;
end.
