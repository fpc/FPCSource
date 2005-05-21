{ %version=1.1 }

type oo2 = pchar;

var a: oo2;

function len(a:pchar):longint;
begin
  len:=-1;
end;

begin
  a:='0123456789';
  if length(a)<>10 then
   begin
     writeln('Error!');
     halt(1);
   end;
end.
