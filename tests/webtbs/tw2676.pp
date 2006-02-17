{ %target=win32, wince }

{ Source provided for Free Pascal Bug Report 2676 }
{ Submitted by "Coenraad Loubser" on  2003-09-12 }
{ e-mail: sprinkaan7@hotmail.com }

var
 x: dword absolute 123;
begin
  writeln(dword(@x));
end.
