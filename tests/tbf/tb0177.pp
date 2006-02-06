{ %fail }

procedure p;
var
  e1     : byte;
  r : record
    e : (e1,e2);
  end;

begin
  r.e:=e1;
end;

begin
end.

