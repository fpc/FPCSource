{ helper methods also influence calls to a parent's method in a derived class }
program tchlp67;

{$ifdef fpc}
  {$mode delphi}
{$endif}
{$apptype console}

type
  TFoo = class
    function Test: Integer;
  end;

  TFooBar = class(TFoo)
    function AccessTest: Integer;
  end;

  TFooHelper = class helper for TFoo
    function Test: Integer;
  end;

function TFoo.Test: Integer;
begin
  Result := 1;
end;

function TFooBar.AccessTest: Integer;
begin
  Result := Test;
end;

function TFooHelper.Test: Integer;
begin
  Result := 2;
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
