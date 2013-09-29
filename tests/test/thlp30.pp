{ helpers can extend type parameters if they can only be classes }
program thlp30;

{$ifdef fpc}
  {$mode delphi}
{$endif}

type
  TFoo<T: class> = class
  type
    THelper = class helper for T
    end;
  end;

type
  TFooTObject = TFoo<TObject>;

begin

end.
