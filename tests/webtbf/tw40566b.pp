{ %fail }
{$mode objfpc}
{$modeswitch FUNCTIONREFERENCES}
program Project1;
type
{  generic Tbar<_A> = type class
    f:_A;
  end;}

  a = type reference to procedure;

  tabc = specialize TBar<integer>; // Internal error 2012101001

begin
end.
