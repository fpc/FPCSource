{ Source provided for Free Pascal Bug Report 2776 }
{ Submitted by "Vincent Snijders" on  2003-11-09 }
{ e-mail: vslist@zonnet.nl }
{$mode delphi}
var
  a: procedure of object;

begin
  a:=nil;
  if assigned(a)
    then ;
end.
