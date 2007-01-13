{%NORUN}
{ Source provided for Free Pascal Bug Report 3628 }
{ Submitted by "rimga" on  2005-02-03 }
{ e-mail: rimga@ktl.mii.lt }
{$ifdef fpc}
{$mode delphi}
{$else}
type
  ptrint = longint;
{$endif}
var
  s: string='12223445';
begin
  s:=string(@s[3]);
  WriteLn(ptrint(@s));
  WriteLn(ptrint(@s[3]));
end.
