{ %FAIL }

{ the type parameters of the implementation need to match those of the forward declaration }
unit tgenfunc18;

{$mode objfpc}{$H+}

interface

implementation

generic procedure Test<T>; forward;

generic procedure Test<S>;
begin

end;

end.

