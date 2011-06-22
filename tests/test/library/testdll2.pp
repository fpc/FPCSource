{ %target=win32,win64 }
{ %needlibrary }
{ %neededafter }
{
  Copyright (c) 1998 by Pierre Muller

  Windows DLL test.
  Check main executable EXPORT support.
}
library testdll2;


{ This library cannot be called from any other program
  as it loads EXE exported symbols }

procedure TestExeProc; external 'tdllexe.exe' name 'TestExeProc';


var
  teststr : string;

procedure test;export;
begin
  writeln('DLL: Hello, I''m testdll2 DLL');
  teststr:='In test';
  TestExeProc;
  teststr:='After test';
end;

function GetString : string; export;
begin
  GetString := teststr;
end;

exports
 test,
 teststr,
 GetString;

end.
