{ %OPT=-Sen }

{ Old file: tbs0339.pp }

type
  rec=record
    x,y : longint;
  end;
var
  r : array[1..10] of rec;
  i : longint;
begin
  i:=1;
  with r[i] do
   begin
     x:=1;
     y:=1;
   end;
  with r[i] do
   begin
     writeln(x,y);
   end;
end.
