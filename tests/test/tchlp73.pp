{ %FAIL }

{ a helper may not be defined as a generic type }
program tchlp73;

{$ifdef fpc}
  {$mode delphi}
{$endif}
{$apptype console}

type
  TFoo = class

  end;

  TFooHelper<T> = class helper for TFoo
    function Test: T;
  end;

function TFooHelper<T>.Test: T;
begin

end;

begin

end.
