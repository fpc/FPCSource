{ Source provided for Free Pascal Bug Report 3893 }
{ Submitted by "George Bakhtadze" on  2005-04-14 }
{ e-mail: mirage@avagames.net }
program test;

function GetPropertyValue: Pointer;
begin
  GetPropertyValue:=nil;
end;

var
  Size, Size2: Single;
  d: Integer;
  p: Pointer;

begin
{$ifndef cpu64}
  Size := 9.11;
//  d := Integer(GetPropertyValue());   // All commented code works
//  p := GetPropertyValue();
//  Size2 := single(p);
  Size2 := single(GetPropertyValue());
{$endif cpu64}
end.
