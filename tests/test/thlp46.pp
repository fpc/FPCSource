{ helpers can extend type parameters if they can only be records }
program thlp46;

{$ifdef fpc}
  {$mode delphi}
{$endif}

type
  TFoo<T: record> = class
  type
    THelper = record helper for T
    end;
  end;

  TBar = record
    f: LongInt;
  end;

type
  TFooTBar = TFoo<TBar>;

begin

end.
