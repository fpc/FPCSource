{ %FAIL }

{ class helpers must not override virtual methods of the extended class }
program tchlp14;

{$ifdef fpc}
  {$mode objfpc}
{$endif}

type
  TFoo = class
    function Test: Integer; virtual;
  end;

  TFooHelper = class helper for TFoo
    function Test: Integer; override;
  end;

function TFoo.Test: Integer;
begin
  Result := 1;
end;

function TFooHelper.Test: Integer;
begin
  Result := 2;
end;

begin

end.

