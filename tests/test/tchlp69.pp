{ %FAIL }

{ a helper can not extend inline defined generics }
program tchlp69;

{$ifdef fpc}
  {$mode delphi}
{$endif}
{$apptype console}

type
  TFoo<T> = class
    Field: T;
  end;

  TFooHelper = class helper for TFoo<Integer>
  end;

begin

end.
