{ %norun }
{ %opt=-O2 }
{Opt.level: -O2}
{$inline on}
unit tw26534a;
interface

implementation

procedure redirect( p: pointer );
begin
end;

procedure inlined( var R: byte ); inline;
begin
  redirect(@R);
end;

procedure comp_failed;
var
  a: byte;
begin
  inlined(a); // ie2006111510
end;

end.
