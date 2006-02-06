var
  e1     : byte;

procedure p;
var
  r : record
    e : (e1,e2);
  end;

begin
  r.e:=e1;
end;

begin
end.

