{ %fail }

{ Source provided for Free Pascal Bug Report 2996 }
{ Submitted by "Michalis Kamburelis" on  2004-02-29 }
{ e-mail: michalis@camelot.homedns.org }
{$mode DELPHI}

type
  TSomeProc = procedure(i:Integer);

var P:TSomeProc;
begin
 if P = nil then ;
end.
