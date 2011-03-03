{ a class helper can access methods defined in the parent of the extended
  class }
program tchlp64;

{$ifdef fpc}
  {$mode delphi}
{$endif}
{$apptype console}

type
  TFoo = class
    function Test(aRecurse: Boolean): Integer;
  end;

  TFooBar = class(TFoo)
  end;

  TFooBarHelper = class helper for TFooBar
    function Test(aRecurse: Boolean): Integer;
  end;

function TFoo.Test(aRecurse: Boolean): Integer;
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
  f: TFooBar;
  res: Integer;
begin
  f := TFooBar.Create;
  res := f.Test(True);
  Writeln('f.Test: ', res);
  if res <> 1 then
    Halt(1);
  Writeln('ok');
end.

