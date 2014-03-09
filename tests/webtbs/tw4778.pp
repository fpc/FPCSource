{ Source provided for Free Pascal Bug Report 4778 }
{ Submitted by "Phil H." on  2006-02-06 }
{ e-mail: pjhess@purdue.edu }
program Test1;

{$mode delphi}

var
  AnInt : Longint;
  
begin

  AnInt := 1;
  
  if single(anint) > 0.9 then
    halt(1);

//  WriteLn(Double(AnInt));
  
end.
