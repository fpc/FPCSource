{ a method defined in a parent helper has higher priority than a method defined
  in the parent of the extended class - test 1}
program tchlp62;

{$ifdef fpc}
  {$mode delphi}
{$endif}
{$apptype console}

type
  TFoo = class
    function Test: Integer;
  end;

  TFooBar = class(TFoo)
  end;

  TFooBarHelper = class helper for TFooBar
    function Test: Integer;
  end;

  TFooBarSubHelper = class helper(TFooBarHelper) for TFooBar
    function AccessTest: Integer;
  end;

function TFoo.Test: Integer;
begin
  Result := 1;
end;

function TFooBarHelper.Test: Integer;
begin
  Result := 2;
end;

function TFooBarSubHelper.AccessTest: Integer;
begin
  Result := Test;
end;

var
  f: TFooBar;
  res: Integer;
begin
  f := TFooBar.Create;
  res := f.AccessTest;
  Writeln('f.AccessTest: ', res);
  if res <> 2 then
    Halt(1);
  Writeln('ok');
end.
