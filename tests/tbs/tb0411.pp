{ %version=1.1}

type
   ta = array of longint;

function f : ta;
  begin
     setlength(result,10);
  end;

begin
   f[1]:=1;
end.

