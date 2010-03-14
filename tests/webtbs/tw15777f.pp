{$mode macpas}
{$B-}
program procparamvoidpointer2;

type
  IntegerPtr = ^Integer;
  ProcParam = procedure( p: univ Pointer); 

var
  gi: Integer;

procedure A( pp: ProcParam);
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

