
var x : byte;
      y : longint;

procedure set_x;
begin
   y:=345;
  {$R-}
    x:=y;
  {$R+}
    Writeln('x = ',x);
  {$R-}
    x:=y
  {$R+}
end;
{ the bug comes from the fact that as there is no
semicolon after x:=y the parser must read up to end; statement
and thus change the range check mode before
the assign node is created !! }

begin
  set_x;
  Writeln('x = ',x);
end.
