{ without "inherited" the methods of the helper are called first }
program tchlp65;

{$ifdef fpc}
  {$mode delphi}
{$endif}
{$apptype console}

type
  TFoo = class
    function Test(aRecurse: Boolean): Integer;
  end;

  TFooHelper = class helper for TFoo
    function Test(aRecurse: Boolean): Integer;
  end;

function TFoo.Test(aRecurse: Boolean): Integer;
begin
  Result := 1;
end;

function TFooHelper.Test(aRecurse: Boolean): Integer;
begin
  if aRecurse then
    Result := Test(False)
  else
    Result := 2;
end;

var
  f: TFoo;
  res: Integer;
begin
  f := TFoo.Create;
  res := f.Test(True);
  Writeln('f.Test: ', res);
  if res <> 2 then
    Halt(1);
  Writeln('ok');
end.
