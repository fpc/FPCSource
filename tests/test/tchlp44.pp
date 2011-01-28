{ in a parent class helper Self always is of the type of the extended class }
program tchlp44;

{$ifdef fpc}
  {$mode objfpc}
{$endif}
{$apptype console}

type
  TFoo = class
    function Test: Integer;
  end;

  TBar = class(TFoo)
    function Test: Integer;
  end;

  TFooHelper = class helper for TFoo
    function AccessTest: Integer;
  end;

  TBarHelper = class helper(TFooHelper) for TBar
  end;

function TFoo.Test: Integer;
begin
  Result := 1;
end;

function TBar.Test: Integer;
begin
  Result := 2;
end;

function TFooHelper.AccessTest: Integer;
begin
  Result := Test;
end;

var
  b: TBar;
  res: Integer;
begin
  b := TBar.Create;
  res := b.AccessTest;
  Writeln('b.AccessTest: ', res);
  if res <> 1 then
    Halt(1);
  Writeln('ok');
end.
