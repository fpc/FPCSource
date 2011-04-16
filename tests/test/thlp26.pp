{ %NORUN }

{ a helper may extend specialized generics }
{ Note: this does currently not compile in Delphi }
program thlp26;

{$ifdef fpc}
  {$mode delphi}
{$endif}

type
  TFoo<T> = class
    Field: T;
  end;

  TFooInteger = TFoo<Integer>;

  TFooHelper = class helper for TFooInteger
  end;

begin
end.
