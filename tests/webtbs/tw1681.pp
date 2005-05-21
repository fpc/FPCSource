{ Source provided for Free Pascal Bug Report 1681 }
{ Submitted by "Boris Bukh" on  2001-11-13 }
{ e-mail: brbukh@yahoo.com }
program Mode_Bug;
{$MODE Dlphi}
var i:integer;
{$MODE FPC}
j:Integer;
begin
     WriteLn(SizeOf(i));
     WriteLn(SizeOf(j));
end.
