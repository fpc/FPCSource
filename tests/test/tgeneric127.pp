{ %FAIL }

program tgeneric127;

{$mode delphi}

type
  TTest<T> = record
    class operator Implicit(const aArg: TTest<T>): T;
    class operator Implicit(const aArg: TTest<T>): T;
  end;

class operator TTest<T>.Implicit(const aArg: TTest<T>): T;
begin

end;

class operator TTest<T>.Implicit(const aArg: TTest<T>): T;
begin

end;

begin

end.
