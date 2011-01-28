{ a parent class helper's methods are available in a child class helper }
program tchlp37;

{$ifdef fpc}
  {$mode objfpc}
{$endif}
{$apptype console}

type
  TFoo = class
    function Test: Integer;
  end;

  TFooHelper = class helper for TFoo
    function Test: Integer;
  end;

  TFooBarHelper = class helper(TFooHelper) for TFoo
    property AccessTest: Integer read Test;
  end;

function TFoo.Test: Integer;
begin
  Result := 1;
end;

function TFooHelper.Test: Integer;
begin
  Result := 2;
end;

var
  f: TFoo;
  res: Integer;
begin
  f := TFoo.Create;
  res := f.AccessTest;
  Writeln(res);
  if res <> 2 then
    Halt(1);
  Writeln('ok');
end.
