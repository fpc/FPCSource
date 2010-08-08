{$mode macpas}
{$B-}
program test;

type
  IntegerPtr = ^Integer;

var
  gi: Integer;

procedure A( procedure pp( p: univ Pointer));
begin
  pp( @gi)
end;

procedure B( p: IntegerPtr);
begin
  if ( p = nil) or ( p^ <> 12345) then halt( 1)
end;

begin
  gi := 12345;
  A( B)
end.

