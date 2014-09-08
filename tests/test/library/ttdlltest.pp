{ %target=win32,win64 }
{ %needlibrary }
{

  Win32 DLL usage example. It needs testdll.pp DLL

  The use of threads creates RTE problems with compilers
  up to 2.4.4 release at least.

}

program ttdlltest;

uses
  Windows;

procedure p1(var S : string);
 external 'ttdlltes1' name 'P1';
procedure proc2(x:longint);
 external 'ttdlltes1' name 'Proc2';
function GetTestStr : string;
 external 'ttdlltes1' name 'GetTestStr';


const
  GlobalThreadIndex : longint = 0;
  ThreadCount = 8;
  StackSize = $100000;

function ThreadMain (Param : pointer) : DWord; stdcall;

var
  ThreadIndex : longint;
begin
  ThreadMain:=0;
  ThreadIndex:=InterlockedIncrement(GlobalThreadIndex);
  Writeln('Main: Starting new thread ',hexstr(PtrUint(Param),2*sizeof(pointer)),' ',ThreadIndex);
  Writeln('Main: Thread Id=',GetCurrentTHreadID);
  Proc2(GlobalThreadIndex);
  Sleep (3000);
  Write('Main: Finishing thread ',ThreadIndex);
  Writeln(' Thread Id=',GetCurrentTHreadID);
  InterlockedDecrement(GlobalThreadIndex);
end;

procedure LaunchThreads;
var
  i : longint;
  ThreadResult : Handle;
  _threadid : DWord;
begin
  for i:=1 to ThreadCount do
    begin
      ThreadResult:=CreateThread(nil,stacksize,@ThreadMain,
                        @GlobalThreadIndex,0,_threadid);
    end;
end;


var
   s : string;external 'ttdlltes1' name 'FPC_string';
   s2 : string;


   procedure MyMainHook(DllParma : PtrInt);
   begin
     Writeln('Main: Thread Detach Hook  called with DLLParam=',DllParam);
   end;

begin
  Dll_Thread_Detach_Hook:=@MyMainHook;
  writeln('Main: Hello!');
  s2:='Test before';
  p1(s2);
  if s2<>'New value' then
    begin
      Writeln('Main: Error while calling P1');
      Halt(1);
    end;
  writeln('Main: ',Hinstance,' ',Hprevinst);
  writeln('Main: testdll s string = ',s);
  s:='Changed by program';
  if GetTestStr<>'Changed by program' then
    begin
      Writeln('Error in DLL variable handling');
      Halt(1);
    end;

  proc2(1234);
  LaunchThreads;
  Sleep(2000);
  While GlobalThreadIndex>0 do
    begin
      Writeln('Main: Waiting for threads to finish');
      Sleep(2000);
    end;
end.
