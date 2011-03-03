{ %NORUN }

{ a helper can extend the subclass of a specialized generic }
program tchlp72;

{$ifdef fpc}
  {$mode delphi}
{$endif}
{$apptype console}

type
  TFoo<T> = class
    Field: T;
  end;

  TFooInteger = TFoo<Integer>;

  TFooBar = class(TFooInteger)

  end;

  TFooHelper = class helper for TFooBar
  end;

begin
end.
