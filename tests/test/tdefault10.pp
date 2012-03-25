{ %NORUN }

{ Default also supports inline specializations }
program tdefault10;

{$mode delphi}

type
  TTest<T> = class
    f: T;
  end;

var
  t: TTest<LongInt>;
begin
  t := Default(TTest<LongInt>);
end.

