var j,k : integer;

begin
     k:=4;
     if (k div 1 <> 4) then
       halt(1);
     j := k div 1;
     writeln(k div 1, ' ',j);
     if (k div 1 <> 4) or
        (j <> 4)  then
       halt(1);
     if (k mod 1) <> 0 then
       halt(2);
end.

