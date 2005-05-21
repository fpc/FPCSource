unit ub0461;
{$inline on}
interface
procedure p1;inline;
implementation

procedure p1;inline;
var
  i,k : longint;

  procedure f;
  begin
    i:=20;
    k:=i*10;
    writeln('hello ',k);
  end;

begin
  f;
end;

end.
