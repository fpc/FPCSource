  program bug0186;
   var
    endline:^integer;
    line:array [1..endline^] of ^char;
   begin
    new (endline);
    endline^:=5;
    endline^:=10;
   end.
