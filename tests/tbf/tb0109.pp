{ %fail }

{$mode delphi}

type
  tcl1=class
  end;

  tcl2=class(tcl1)
  end;

procedure p1(var p:tcl1);
begin
end;

var
  t : tcl2;
begin
  t:=tcl2.create;
  { in delphi the passed argument must match exact }
  p1(t);
end.
