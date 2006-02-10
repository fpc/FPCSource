{ %fail }

{ Source provided for Free Pascal Bug Report 4778 }
{ Submitted by "Phil H." on  2006-02-06 }
{ e-mail: pjhess@purdue.edu }
program Test1;

{$mode delphi}

var
  AnInt : Integer;
  
begin

  AnInt := 1;
  
//  WriteLn(Single(AnInt));

  WriteLn(Double(AnInt));
  
end.
