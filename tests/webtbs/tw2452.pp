{ Source provided for Free Pascal Bug Report 2452 }
{ Submitted by "Nikolay Nikolov" on  2003-04-06 }
{ e-mail: nickysn1983@netscape.net }
Program Test;

Uses
  Math;

procedure error;
  begin
    writeln('error');
    halt(1);
  end;

var
  s : string;

Begin
  { allow a small error }
  if abs(arctan2(1,1)-7.8539816339744831E-0001)>4.0E-0019 then
    error;
  if arctan2(0, 1)<>0.0 then
    error;
  { allow a small error }
  if abs(arctan2(1, 0)-1.5707963267948966E+0000)>2.0E-0017 then
    begin
      writeln(arctan2(1, 0));
      error;
    end;
  { allow a small error }
  if abs(arctan2(-1, 0)+1.5707963267948966E+0000)>2.0E-0017 then
    error;
  if arctan2(0, 0)<>0.0 then
    error;
End.
