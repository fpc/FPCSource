{ %FAIL }

{ helpers can not extend type parameters even if they can only be classes }
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

begin

end.
