{ Old file: tbs0047.pp }
{  compiling with -So crashes the compiler              OK 0.99.1 (CEC) }

procedure test;

  begin
  end;

var
   p1 : procedure;
   p2 : pointer;

begin
   p1:=@test;
   p2:=@test;
end.
