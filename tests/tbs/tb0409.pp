{ %version=1.1 }
type
   myl = type longint;

var
   i1,i2,i3 : myl;
   l : longint;

procedure p(i : myl);overload;
begin
end;

procedure p(i : longint);overload;
begin
end;

begin
   i1:=i2+i3;
   l:=i1+l;
   inc(i3);
end.
