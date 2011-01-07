{ class helpers hide methods of the extended class }
program tchlp12;

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

function TFoo.Test: Integer;
begin
  Result := 1;
end;

function TFooHelper.Test: Integer;
begin
  Result := 2;
end;

var
  f: TFoo;
begin
  f := TFoo.Create;
  if f.Test <> 2 then
    Halt(1);
end.

