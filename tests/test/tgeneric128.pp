{ %NORUN }

program tgeneric128;

{$mode delphi}

type
  Foo<T> = record
  public
    class operator Equal(const left: Foo<T>; const right: T): Boolean;
  end;

  Bar<T> = record
  public
    class operator Implicit(const value: Bar<T>): Foo<T>;
  end;

class operator Foo<T>.Equal(const left: Foo<T>; const right: T): Boolean;
begin
end;

class operator Bar<T>.Implicit(const value: Bar<T>): Foo<T>;
begin
end;

begin

end.
