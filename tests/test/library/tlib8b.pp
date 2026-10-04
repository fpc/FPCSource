{ %target=win32,win64 }
{ %needlibrary }
{ %result=226 }

{ A library's threadvar block that cannot be stored in a new thread's thread
  local storage slot ends the process with run time error 226, and the block
  is given back from where its allocation starts -- before the address stored,
  which follows the block's entry in the list of a library's blocks. The
  library of tlib8a.pp refuses that store for the thread the program names,
  as tb0757 does for a program's own block; its import of TerminateProcess
  turns a 226 reached after more than one refused store into 5, one with the
  address stored given to HeapFree into 6, and one without the refused block
  given back into 7. }
program tlib8b;

{$mode objfpc}

const
  CREATE_SUSPENDED = 4;

function CreateThread(sa: pointer; stack: ptruint; start: pointer; p: pointer; flags: dword; var id: dword): THandle; stdcall; external 'kernel32' name 'CreateThread';
function ResumeThread(h: THandle): dword; stdcall; external 'kernel32' name 'ResumeThread';
function WaitForSingleObject(h: THandle; ms: dword): dword; stdcall; external 'kernel32' name 'WaitForSingleObject';
function GetCurrentProcess: THandle; stdcall; external 'kernel32' name 'GetCurrentProcess';
function TerminateProcess(h: THandle; code: dword): longbool; stdcall; external 'kernel32' name 'TerminateProcess';

procedure arm(id: dword); external 'tlib8a' name 'arm';

function Body(p: pointer): dword; stdcall;
begin
  Body:=0;
end;

{ not halt: a thread that did not end can still hold the loader lock, which
  ExitProcess needs }
procedure Fail(code: dword; const msg: string);
begin
  writeln(msg);
  flush(output);
  TerminateProcess(GetCurrentProcess,code);
end;

var
  th: THandle;
  tid: dword;
begin
  { the library is told of the thread when it starts, before its body runs;
    arm ends the process with 1 when the library's imports were not found }
  th:=CreateThread(nil,0,@Body,nil,CREATE_SUSPENDED,tid);
  arm(tid);
  ResumeThread(th);
  if WaitForSingleObject(th,20000)<>0 then
    Fail(4,'the thread did not end');
  Fail(2,'the process went on after a refused store');
end.
