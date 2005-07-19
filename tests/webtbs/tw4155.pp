{ %OPT=-Fcutf8}
{ Source provided for Free Pascal Bug Report 4155 }
{ Submitted by "rimga" on  2005-07-04 }
{ e-mail: rimga@ktl.mii.lt }
{this file encoded in UTF8}
{cmd fpc twchar.pas -FcUTF8}
var
  c: widestring;
  c2: widestring;
begin
  c:= 'ą';      //problem handling char constants
  c2:= 'ąa';
  if c[1]<>c2[1] then
    WriteLn('problem')
  else
    Writeln('OK');
end.
