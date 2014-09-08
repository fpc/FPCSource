{ %NORUN }

program tw24072;

{$mode delphi}

type
  TA<T> = record
  end;

  TB<T> = class
  public
    type
      TC = TA<T>;

      TD = record
        Foo: TC; // ! FATAL !
      end;
  end;

var
  t: TB<LongInt>;
begin
end.
