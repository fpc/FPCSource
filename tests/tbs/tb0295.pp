{ %VERSION=1.1 }

{ Old file: tbs0353.pp }
{  }

Var
  I : Int64;
  j : longint;
  K : Int64;
  err : boolean;
begin
  I:=2;
  Writeln(i);
  K:=qword(1) shl 62;
  For j:=1 to 61 do
    begin
    I:=I*2;
    If I/k*100>100 then
      begin
        Writeln('Error');
        err:=true;
      end
    else
      Writeln(j:2,': ',i:20,' ',i div 1024:20,' ',(i/k*100):4:1);
    end;
  if err then
   halt(1);
end.
