{ %target=win32,win64,wince,os2 }
{ %needlibrary }

uses
  popuperr;

procedure test;external 'tw12987a' name 'test';

function ThreadTest(p : pointer) : PtrInt;
  begin
    test;
  end;

var
  t1,t2,t3 : TThreadID;

begin
  t1:=BeginThread(@ThreadTest);
  t2:=BeginThread(@ThreadTest);
  t3:=BeginThread(@ThreadTest);
  WaitForThreadTerminate(t1,0);
  WaitForThreadTerminate(t2,0);
  WaitForThreadTerminate(t3,0);
  writeln('Finished');
end.
