{ %opt=-O2 }
// Opt.level: -O2
{$inline on}
program test2;

procedure redirect( p: pointer );
begin
end;

procedure inlined( var R: byte ); inline;
begin
  redirect(@R);
end;

var
  a: byte;
begin
  inlined(a); // ie2006111510
end.
