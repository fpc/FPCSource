{ a helper of a parent class does not hide methods in the child class }
program tchlp89;

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
  if res <> 4 then
    Halt(1);
  Writeln('ok');
end.
