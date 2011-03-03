{ %SKIP }
{ .%NORUN }

{ a helper may contain generic methods }
program tchlp74;

{$ifdef fpc}
  {$mode delphi}
{$endif}
{$apptype console}

type
  TFoo = class

  end;

  TFooHelper = class helper for TFoo
    function Test<T>: T;
  end;

function TFooHelper.Test<T>: T;
begin

end;

begin

end.
