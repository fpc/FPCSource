{ %TARGET=win32,win64 }
{ %RESULT=226 }

{ A thread whose threadvars cannot be allocated ends the process with run time
  error 226. Here it is a new thread: the main thread starts a thread
  suspended, takes all the address space that could hold a thread's
  threadvars, then resumes it. (A thread started after that could find no free
  range for its stack either.) The error used to call RunError, whose exit
  code itself uses threadvars, so it came back to the allocation over and over:
  the process ended with an access violation and no message, or did not end at
  all. }
program tb0735;

{$mode objfpc}

const
  MEM_RESERVE = $2000;
  PAGE_NOACCESS = 1;
  CREATE_SUSPENDED = 4;
  BlockSize = 64*1024*1024;

function VirtualAlloc(lpAddress: pointer; dwSize: ptruint; flAllocationType, flProtect: dword): pointer;
  stdcall; external 'kernel32' name 'VirtualAlloc';

threadvar
  tv: array[0..BlockSize-1] of byte;

function worker(p: pointer): ptrint;
begin
  tv[0]:=2;
  writeln('the new thread''s threadvars were allocated');
  halt(1);
  result:=0;
end;

var
  size: ptruint;
  id, h: TThreadID;
begin
  { the main thread's threadvars, while there is still room for them }
  tv[0]:=1;
  h:=BeginThread(nil,256*1024,@worker,nil,CREATE_SUSPENDED,id);
  if h=0 then
    begin
      writeln('the thread could not be started');
      halt(2);
    end;
  { leave no free range as large as a thread's threadvars }
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
