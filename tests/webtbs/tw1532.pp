{ %version=1.1 }

 uses sysutils;
 Var I : Longint;
     a:char;

 begin
 a:='A';
 a:=upcase(a);   //OK

 Writeln;
 Writeln (Lowercase('ABCDEFGHIJKLMNOPQRSTUVWXYZ')); //OK
 Writeln (Lowercase(A)); // OK
 a:=Lowercase(A);        // ERROR
 Writeln (a);
 end.

