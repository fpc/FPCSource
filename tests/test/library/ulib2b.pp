{ %target=win32,win64,wince }
{ %needlibrary }

unit
  ulib2b;

interface
{ Checks that the two functions with the same exported name 'p'
  are each loaded correctly. }
procedure p(var a : dword);external 'tlib1a' name 'p';
procedure p2(var a : dword);external 'tlib1a2' name 'p';

implementation

end.
