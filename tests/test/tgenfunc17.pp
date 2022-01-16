{ %FAIL }

{ the type parameters of the implementation need to match those in the interface }
unit tgenfunc17;

{$mode objfpc}{$H+}

interface

generic procedure Test<T>;

implementation

generic procedure Test<S>;
begin

end;

end.

