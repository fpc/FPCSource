{ %FAIL }
{ Old file: tbf0186.pp }
{ Erroneous array syntax is accepted.                   OK 0.99.9 (PFV) }

  program bug0186;
   var
    endline:^integer;
    line:array [1..endline^] of ^char;
   begin
    new (endline);
    endline^:=5;
    endline^:=10;
   end.
