{ Source provided for Free Pascal Bug Report 3628 }
{ Submitted by "rimga" on  2005-02-03 }
{ e-mail: rimga@ktl.mii.lt }
{$mode delphi}
var
  s: string='12223445';
begin
  s:=string(@s[3]); //internal error 200410231
  WriteLn(ptrint(@s[3]));   //runtime error 203
end.

