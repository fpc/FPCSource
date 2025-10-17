{ %NORUN }

program tgeneric126;

{$mode delphi}

type
  TTest<S, T> = record
    class operator Implicit(const aArg: TTest<S, T>): T;
    class operator Implicit(const aArg: TTest<S, T>): TArray<T>;
    class operator Implicit(const aArg: TTest<S, T>): S;
  end;

class operator TTest<S, T>.Implicit(const aArg: TTest<S, T>): T;
begin

end;

class operator TTest<S, T>.Implicit(const aArg: TTest<S, T>): TArray<T>;
begin

end;

class operator TTest<S, T>.Implicit(const aArg: TTest<S, T>): S;
begin

end;

begin

end.
