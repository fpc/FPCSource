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
  str(theta(20):0:6,s1);
  str(theta(2):0:6,s2);
  str((theta(20)-theta(2)):0:6,s3);
  writeln(s1,' ',s2,' ',s3);
  if (s1<>'1.000000') or
     (s2<>'0.100000') or
     (s3<>'0.900000') then
    halt(1);
end;
begin
p1;
end.
