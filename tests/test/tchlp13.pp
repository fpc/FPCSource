{ class helpers don't hide methods of the subclasses of the extended class }
program tchlp13;

{$ifdef fpc}
  {$mode objfpc}
{$endif}

type
  TFoo = class
    function Test: Integer;
  end;

  TFooHelper = class helper for TFoo
    function Test: Integer;
  end;

  TFooSub = class(TFoo)
    function Test: Integer;
  end;

function TFoo.Test: Integer;
begin
  Result := 1;
end;

function TFooHelper.Test: Integer;
begin
  Result := 2;
end;

function TFooSub.Test: Integer;
begin
  Result := 3;
end;

var
  f: TFooSub;
begin
  f := TFooSub.Create;
  if f.Test <> 3 then
    Halt(1);
end.

