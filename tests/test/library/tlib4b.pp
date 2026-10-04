{ %target=win32,win64 }
{ %needlibrary }
{ %result=226 }
{ %neededafter }

{ A library's threadvars that cannot be allocated for a new thread end the
  process with run time error 226. The library of tlib4a.pp needs 64 MB of
  threadvars per thread; the program starts a thread suspended, leaves no free
  range of address space that large, and resumes the thread, which the library
  is told of. (A thread started after that could find no free range for its
  stack either.) The error used to call RunError, whose exit code itself uses
  threadvars, so it came back to the allocation over and over: the process
  ended with a stack overflow and no message, or did not end at all. tlib4c.pp
  runs it again and checks where the message is written; with the argument
  stdout the program has the library set its WriteErrorsToStdErr to False
  first (the library's run time library is the one that reports). }
program tlib4b;

{$mode objfpc}

const
  MEM_RESERVE = $2000;
  PAGE_NOACCESS = 1;
  CREATE_SUSPENDED = 4;
  BlockSize = 64*1024*1024;

function VirtualAlloc(lpAddress: pointer; dwSize: ptruint; flAllocationType, flProtect: dword): pointer;
  stdcall; external 'kernel32' name 'VirtualAlloc';

procedure touch; external 'tlib4a' name 'touch';
procedure tostdout; external 'tlib4a' name 'tostdout';

function worker(p: pointer): ptrint;
begin
  touch;
  writeln('the library''s threadvars were allocated for the new thread');
  halt(1);
  result:=0;
end;

var
  size: ptruint;
  id, h: TThreadID;
begin
  if ParamStr(1)='stdout' then
    tostdout;
  h:=BeginThread(nil,256*1024,@worker,nil,CREATE_SUSPENDED,id);
  if h=0 then
    begin
      writeln('the thread could not be started');
      halt(2);
    end;
  size:=ptruint(1) shl (sizeof(pointer)*8-2);
  while size>=BlockSize do
    begin
      while VirtualAlloc(nil,size,MEM_RESERVE,PAGE_NOACCESS)<>nil do
        ;
      size:=size div 2;
    end;
  ResumeThread(h);
  WaitForThreadTerminate(h,0);
  writeln('the thread ended');
  halt(3);
end.
