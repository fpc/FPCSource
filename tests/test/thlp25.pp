{ %FAIL }

{ a helper can not extend unspecialized generics }
program thlp25;

{$ifdef fpc}
  {$mode delphi}
{$endif}

type
  TFoo<T> = class
    Field: T;
  end;

  TFooHelper = class helper for TFoo<T>
  end;

begin

end.
