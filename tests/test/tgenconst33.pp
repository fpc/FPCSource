{ %FAIL }

program tgenconst33;

{$mode delphi}

{var
  Test: LongInt = 42;}

type
  Test<const N: LongInt> = class

  end;

const
  N = 42;

begin
  if Test<N then
    Writeln('Foo');
end.
