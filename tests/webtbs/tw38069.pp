{ %cpu=i386 }
{$mode objfpc}
{$OPTIMIZATION REGVAR}
{.$FPUTYPE SSE2}  //uncommenting this resolves the problem

uses uw38069;

var z: complex;
    n: integer;
begin
  z := z*n;  //internal error 200604201
end.
