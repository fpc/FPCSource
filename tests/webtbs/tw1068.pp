{ %VERSION=1.1 }

PROGRAM bug1068;
VAR i: INT64;
    s : string;
BEGIN
  i:=2147483648;
  str(i,s);
  if s<>'2147483648' then
    begin
       writeln(s);
       halt(1);
    end
  else
    halt(0);
END.
