{ Source provided for Free Pascal Bug Report 3340 }
{ Submitted by "Alexey Barkovoy" on  2004-10-05 }
{ e-mail: clootie@ixbt.com }
program Project1;

{$MODE DELPHI}
{$INLINE ON}

uses
  uw3340;

var
  t: TTT;
begin
  t:= TTT.Create;
  t.zz:= 9;
  WriteLn(t.Yes);
  t.Free;
end.
