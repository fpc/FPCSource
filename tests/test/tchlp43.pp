program tchlp43;

{$ifdef fpc}
  {$mode delphi}
{$endif}
{$apptype console}

type
  TFoo = class
    function Test(aRecurse: Boolean): Integer; virtual;
  end;

  TObjectHelper = class helper for TObject
    function Test(aRecurse: Boolean): Integer; virtual;
  end;

  TFooHelper = class helper(TObjectHelper) for TFoo
    function Test(aRecurse: Boolean): Integer; override;
  end;

function TFoo.Test(aRecurse: Boolean): Integer;
begin
  Result := 1;
end;

function TObjectHelper.Test(aRecurse: Boolean): Integer;
begin
  Result := 2;
end;

function TFooHelper.Test(aRecurse: Boolean): Integer;
begin
  if aRecurse then
    Result := inherited Test(False)
  else
    Result := 3;
end;

var
  f: TFoo;
begin
  f := TFoo.Create;
  res := f.Test(True);
  if res <> 2 then
    Halt(1);
  Writeln('ok');
end.
