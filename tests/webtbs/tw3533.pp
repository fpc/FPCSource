{ Source provided for Free Pascal Bug Report 3533 }
{ Submitted by "Vasily Volchenko" on  2005-01-08 }
{ e-mail: Vasily.Volchenko@mstu.edu.ru }
program Bug;

{x$mode objfpc}{x$H+}
//Local function bug
uses
  Classes,Math,SysUtils;
procedure p1;
var tc,t0:double;
s1,s2,s3 : string;
function Theta(x:double):double;
begin
  {Result}Theta:=(x-tc)/(t0-tc);
end;
begin
  tc:=0;t0:=20;
  str(theta(20),s1);
  str(theta(2),s2);
  str(theta(20)-theta(2),s3);
  writeln(s1,' ',s2,' ',s3);
  if (s1<>' 1.000000000000000E+000') or
     (s2<>' 1.000000000000000E-001') or
     (s3<>'-9.000000000000000E-001') then
    halt(1);
end;
begin
p1;
end.

