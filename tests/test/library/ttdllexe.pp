{ %target=win32,win64 }
{ %needlibrary }
{

  Win32 DLL usage example. It needs testdll2.pp
  This test checksq the windows abality to
  export a function in an executable.

  Here procedure TestExeProc is exported
  and is imported by testdll2 DLL.

}

program ttdllexe;

uses
  Windows;

procedure test; external 'ttdllex1' name 'test';
function GetString : string; external 'ttdllex1' name 'GetString';

var
   s : string;external 'ttdllex1' name 'teststr';
const
  called : boolean = false;

procedure TestExeProc;export;
begin
  Writeln('Main: TestExeProc');
  Writeln('Main: S is: "',s,'"');
  called:=true;
end;

exports
  TestExeProc;

begin
  s:='Before test call';
  Writeln('Main: S is: "',GetString,'"');
  if (s<>GetString) then
    begin
      Writeln('Error in DLL variable handling');
      halt(1);
    end;
  test;
  Writeln('Main: S value after call is: "',s,'"');
  if not called then
    begin
      Writeln('Error in DLL variable handling');
      halt(1);
    end;

end.
