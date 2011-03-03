{ test that helpers can access the methods of the parent helper using
  "inherited" }
program tchlp61;

{$ifdef fpc}
  {$mode delphi}
{$endif}
{$apptype console}

type
  TFoo = class

  end;

  TFooHelper = class helper for TFoo
    function Test(aRecurse: Boolean): Integer;
  end;

  TFooBarHelper = class helper(TFooHelper) for TFoo
    function Test(aRecurse: Boolean): Integer;
  end;

function TFooHelper.Test(aRecurse: Boolean): Integer;
begin
  Result := 1;
end;

function TFooBarHelper.Test(aRecurse: Boolean): Integer;
begin
  if aRecurse then
    Result := inherited Test(False)
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
  if res <> 1 then
    Halt(1);
  Writeln('ok');
end.
