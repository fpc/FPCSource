{ %FAIL }

{ a helper can not extend specialized generics }
program tchlp71;

{$ifdef fpc}
  {$mode delphi}
{$endif}
{$apptype console}

type
  TFoo<T> = class
    Field: T;
  end;

  TFooInteger = TFoo<Integer>;

  TFooHelper = class helper for TFooInteger
  end;

begin
end.
