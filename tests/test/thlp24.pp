{ %FAIL }

{ a helper can not extend inline defined generics }
program thlp24;

{$ifdef fpc}
  {$mode delphi}
{$endif}

type
  TFoo<T> = class
    Field: T;
  end;

  TFooHelper = class helper for TFoo<Integer>
  end;

begin

end.
