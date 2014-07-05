{ %target=win32,win64 }
{ %needlibrary }

unit
  ulib2b;

interface
{ Checks that the two functions with the same exported name 'p'
  are each loaded correctly. }
procedure p(var a : dword);external 'tlib1a' name 'p';
procedure p2(var a : dword);external 'tlib1b' name 'p';

implementation

end.
