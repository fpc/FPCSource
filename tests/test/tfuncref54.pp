program tfuncref54;

{$mode objfpc}{$H+}
{$modeswitch functionreferences}

function OuterFunc: String;
begin
  Result := 'Foobar';
end;

procedure DoTest;

  function NestedFunc: String;
  begin
    Result := 'Hello World';
  end;

var
  func: reference to function: String;
begin
  func := @NestedFunc;
  if func() <> 'Hello World' then
    Halt(1);
  if NestedFunc() <> 'Hello World' then
    Halt(2);
  func := @OuterFunc;
  if func() <> 'Foobar' then
    Halt(3);
end;

begin
  DoTest;
end.
