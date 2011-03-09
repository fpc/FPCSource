{ a helper of a parent class hides methods in the child class if its also a
  parent of the helper for the child class }
program tchlp90;

{$ifdef fpc}
  {$mode delphi}
{$endif}
{$apptype console}

type
  TFoo = class
    function TestFoo: Integer;
  end;

  TBar = class(TFoo)
     function TestFoo: Integer;
  end;

  TFooHelper = class helper for TFoo
    function TestFoo: Integer;
  end;

  TBarHelper = class helper(TFooHelper) for TBar
  end;

function TFoo.TestFoo: Integer;
begin
  Result := 1;
end;

function TBar.TestFoo: Integer;
begin
  Result := 4;
end;

function TFooHelper.TestFoo: Integer;
begin
  Result := 2;
end;

var
  b: TBar;
  res: Integer;
begin
  b := TBar.Create;
  res := b.TestFoo;
  Writeln('b.TestFoo: ', res);
  if res <> 2 then
    Halt(1);
  Writeln('ok');
end.
