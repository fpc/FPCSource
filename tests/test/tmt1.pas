{ %version=1.1 }

const
   threadcount = 100;
   finished : longint = 0;

function f(p : pointer) : longint;
  var
     s : ansistring;
     i : longint;
  begin
     for i:=1 to 1000000 div threadcount do
       s:=s+'1';
     inc(finished);
  end;

var
   i : longint;

begin
   for i:=1 to threadcount do
     BeginThread(@f);
   while finished<threadcount do
     ;
   writeln(finished);
end.
