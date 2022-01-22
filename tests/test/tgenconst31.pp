program tgenconst31;

{$mode delphi}

uses
  ugenconst31a, ugenconst31b;

const
  N = 42;

begin
  if Test<N then
    Writeln('Foo');
end.
