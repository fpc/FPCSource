{ Old file: tbs0006.pp }
{  tests the wrong floating point code generation      OK 0.9.2 }

uses
  erroru;
var
   a,b,c,d,e,f,g,r : double;

begin
   a:=10.0;
   b:=11.0;
   c:=13.0;
   d:=17.0;
   e:=19.0;
   f:=23.0;
   g:=0.0;
   r:=2.0;
   a:= a - 2*b*e - 2*c*f - 2*d*g - Sqr(r);
   writeln(a,' (must be -1010)');
   if a<>-1010.0 then
     Error;
end.
