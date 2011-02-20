{ the extended class has higher priority than the parent class when
  searching for symbols }
program tchlp39;

{$ifdef fpc}
  {$mode objfpc}
{$endif}
{$apptype console}

type
  TFoo = class
    function Test(aRecurse: Boolean): Integer;
  end;

  TFooHelper = class helper for TFoo
    function Test(aRecurse: Boolean): Integer;
  end;

  TFooSubHelper = class helper(TFooHelper) for TFoo
    function Test(aRecurse: Boolean): Integer;
  end;

function TFoo.Test(aRecurse: Boolean): Integer;
begin
  Result := 1;
end;

function TFooHelper.Test(aRecurse: Boolean): Integer;
begin
  Result := 2;
end;

function TFooSubHelper.Test(aRecurse: Boolean): Integer;
begin
  if aRecurse then
    Result := inherited Test(False)
  else
    Result := 3;
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
