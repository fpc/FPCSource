{ %target=win32,win64 }
{ %needlibrary }
{ %norun }
{ %neededafter }
{
  Copyright (c) 1998 by Pierre Muller

  Win32 DLL test with threads.
}
library testdll;

function GetModuleFileName(hModule:longint;lpszPath:pchar;cchPath:longint):longint;
  stdcall; external 'kernel32' name 'GetModuleFileNameA';
procedure beep(ID:longint);
  stdcall; external 'user32' name 'MessageBeep';


var
  teststr : string;

var
  global_count : longint;
threadvar
  thread_local_count : longint{ = 6};

procedure P1(var s:string);export;
var
  i : longint;
  p:array[0..255] of char;
begin
  i:=length(s);
  getmodulefilename(Hinstance,@p,255);
  writeln('DLL: Hello, I''m DLL ',pchar(@p));
  writeln('DLL: S before is "',s,'"');
  s:='New value';
end;

procedure P2(x:longint);export;
begin
  writeln('DLL: Argument X=',x);
  writeln('DLL: New teststr="',teststr,'"');
  inc(global_count);
  inc(thread_local_count);
  Writeln('DLL: Thread Id is ',GetCurrentThreadId);
  Writeln('DLL: Global count=',Global_count);
  Writeln('DLL: Thread local count=',thread_local_count);
end;

procedure P3(var t);export;
var
  p : pointer;
begin
  p:=Addr(T);
  p:=p;
end;

procedure P4(x1:pointer);export;
begin
  Inc(x1);
end;

function GetTestStr : string; export;
begin
  GetTestStr:=teststr;
end;


procedure MyDllHook(DllParma : PtrInt);
begin
  Writeln('DLL: Thread Detach Hook  called with DLLParam=',DllParam);
  Writeln('DLL: Thread Id is ',GetCurrentThreadId);
end;

procedure NewExit;
begin
  beep(0);
  writeln('DLL: Exit from testdll');
  Writeln('DLL: Thread Id is ',GetCurrentThreadId);
end;

exports
 P1 index 1,
 P2 name 'Proc2',
 P3,
 GetTestStr,
 P4 resident,
 teststr name 'FPC_string';


begin
  Dll_Thread_Detach_Hook:=@MyDllHook;
  Writeln('DLL: Startup Thread Id is ',GetCurrentThreadId);
  writeln('DLL: HInstance ',Hinstance,'  PrevInst ',Hprevinst,'  DLLReason ',DLLreason,'  DLLParam ',DLLparam);
  teststr:='DLL init done';
  exitproc:=@newExit;
end.
