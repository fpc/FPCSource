{ %fail }

{$mode objfpc}

type
  ttype=object
    l1,l2 : pointer;
  end;

  tcl=class
  end;

var
  tt : ttype;
  c  : tcl;
begin
  c:=tcl(tt);
end.
