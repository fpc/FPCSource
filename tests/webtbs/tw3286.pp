{ Source provided for Free Pascal Bug Report 3286 }
{ Submitted by "Frank Kintrup" on  2004-08-31 }
{ e-mail: frank.kintrup@gmx.de }
{$mode delphi}
var
  p : Pointer;
  a : array of Integer;
begin
  SetLength(a, 10);
  p := a;
end.
