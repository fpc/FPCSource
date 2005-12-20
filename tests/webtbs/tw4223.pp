{ Source provided for Free Pascal Bug Report 4223 }
{ Submitted by "Phil Hess" on  2005-07-25 }
{ e-mail: pjhess@purdue.edu }
program TestSet;

{$Mode delphi}
{$R+}

var
  AnInt : Integer;
begin

  AnInt := -1;
  if AnInt in [1,2,3] then
    WriteLn('In set')
  else
    WriteLn('Not in set');

end.
