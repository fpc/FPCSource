{ %OPT=-vw -Sew }
{ %FAIL }
{ The above options transform warnings into errors }
{ so that the warning about unsupported Dlphi
  mode becomes an error }

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
