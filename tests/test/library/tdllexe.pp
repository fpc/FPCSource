{
  Copyright (c) 1998 by Pierre Muller

  Win32 DLL usage example. It needs testdll.pp
}
program tdllexe;

uses
  Windows;

procedure test; external 'testdll2' name 'test';
function GetString : string; external 'testdll2' name 'GetString';

var
   s : string;external 'testdll2' name 'teststr';
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
