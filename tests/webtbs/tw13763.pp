{ %norun }
{ %opt=-vh -Seh }

{$MODE OBJFPC}

var
   f: TextFile;

begin
     AssignFile (f, 'test.dat'); // Hint: Variable not initialized
     Reset (f);
     CloseFile (f);
end.
