{ %TARGET=win32,win64 }
{ %RESULT=226 }

{ A thread whose threadvar block cannot be stored in its thread local storage
  slot ends the process with run time error 226, as when the block cannot be
  allocated. Windows fails that store only when it cannot allocate a thread's
  storage for a slot of 64 or above, so the program makes it fail: it points
  its own import of TlsSetValue -- the run time library's, which is in the
  program -- at a routine that refuses the store of a block for the thread it
  starts. Values on that thread's stack, and nil, still go through. The store's
  result used to be ignored: every threadvar the thread used allocated another
  block, and the thread went on without threadvars. The import of HeapFree is
  pointed at a routine that notes the refused block being freed, and the
  import of TerminateProcess at a routine that turns a 226 reached after more
  than one refused store into 5, and one reached without the refused block
  freed into 7. }
program tb0757;

{$mode objfpc}
{$pointermath on}

type
  TTlsSetValue = function(index: dword; value: pointer): longbool; stdcall;
  TTerminateProcess = function(h: THandle; code: dword): longbool; stdcall;
  THeapFree = function(heap: THandle; flags: dword; mem: pointer): longbool; stdcall;

  TMemoryBasicInformation = record
    BaseAddress,
    AllocationBase: pointer;
    AllocationProtect: dword;
    RegionSize: ptruint;
    State,
    Protect,
    Type_: dword;
  end;

const
  CREATE_SUSPENDED = 4;
  PAGE_READWRITE = 4;
  ERROR_NOT_ENOUGH_MEMORY = 8;

function GetModuleHandle(name: PAnsiChar): ptruint; stdcall; external 'kernel32' name 'GetModuleHandleA';
function GetCurrentThreadId: dword; stdcall; external 'kernel32' name 'GetCurrentThreadId';
function VirtualProtect(addr: pointer; size: ptruint; prot: dword; var old: dword): longbool; stdcall; external 'kernel32' name 'VirtualProtect';
function VirtualQuery(addr: pointer; var info: TMemoryBasicInformation; size: ptruint): ptruint; stdcall; external 'kernel32' name 'VirtualQuery';
procedure SetLastError(code: dword); stdcall; external 'kernel32' name 'SetLastError';
function CreateThread(sa: pointer; stack: ptruint; start: pointer; p: pointer; flags: dword; var id: dword): THandle; stdcall; external 'kernel32' name 'CreateThread';
function ResumeThread(h: THandle): dword; stdcall; external 'kernel32' name 'ResumeThread';
function WaitForSingleObject(h: THandle; ms: dword): dword; stdcall; external 'kernel32' name 'WaitForSingleObject';
function GetCurrentProcess: THandle; stdcall; external 'kernel32' name 'GetCurrentProcess';
function TerminateProcess(h: THandle; code: dword): longbool; stdcall; external 'kernel32' name 'TerminateProcess';
function GetStdHandle(n: dword): THandle; stdcall; external 'kernel32' name 'GetStdHandle';
function WriteFile(h: THandle; buf: pointer; len: dword; var written: dword; ov: pointer): longbool; stdcall; external 'kernel32' name 'WriteFile';

var
  ArmedThread: dword = 0;
  Refused: longint = 0;
  OrigTlsSetValue: TTlsSetValue = nil;
  OrigTerminateProcess: TTerminateProcess = nil;
  OrigHeapFree: THeapFree = nil;
  RefusedBlock: pointer = nil;
  RefusedFreed: boolean = false;

{ the allocation an address belongs to: a thread's whole stack is one }
function AllocationOf(p: pointer): pointer;
var
  info: TMemoryBasicInformation;
begin
  if VirtualQuery(p,info,sizeof(info))=0 then
    AllocationOf:=nil
  else
    AllocationOf:=info.AllocationBase;
end;

{ uses the operating system alone: the thread it runs on has no threadvars }
function FailingTlsSetValue(index: dword; value: pointer): longbool; stdcall;
var
  onstack: pointer;
begin
  if (GetCurrentThreadId=ArmedThread) and assigned(value) and
     (AllocationOf(value)<>AllocationOf(@onstack)) then
    begin
      inc(Refused);
      { a program's block starts at the address stored }
      RefusedBlock:=value;
      SetLastError(ERROR_NOT_ENOUGH_MEMORY);
      FailingTlsSetValue:=false;
    end
  else
    FailingTlsSetValue:=OrigTlsSetValue(index,value);
end;

{ uses the operating system alone, as FailingTlsSetValue }
function NotingHeapFree(heap: THandle; flags: dword; mem: pointer): longbool; stdcall;
begin
  if assigned(mem) and (mem=RefusedBlock) then
    RefusedFreed:=true;
  NotingHeapFree:=OrigHeapFree(heap,flags,mem);
end;

{ the run time library ends the process with this when the threadvars are
  unavailable: writes the number of refused stores first }
function ReportingTerminateProcess(h: THandle; code: dword): longbool; stdcall;
var
  buf: array[0..31] of AnsiChar;
  digits: array[0..15] of AnsiChar;
  n, i, len: longint;
  written: dword;
const
  prefix: array[0..7] of AnsiChar = 'refused=';
begin
  len:=0;
  for i:=0 to 7 do
    begin
      buf[len]:=prefix[i];
      inc(len);
    end;
  n:=Refused;
  i:=0;
  repeat
    digits[i]:=AnsiChar(ord('0')+n mod 10);
    n:=n div 10;
    inc(i);
  until n=0;
  while i>0 do
    begin
      dec(i);
      buf[len]:=digits[i];
      inc(len);
    end;
  buf[len]:=#13;
  buf[len+1]:=#10;
  inc(len,2);
  WriteFile(GetStdHandle(dword(-11)),@buf,len,written,nil);
  { a run time library that ignores the result reaches 226 only by running out
    of room for the blocks it leaked }
  if (code=226) and (Refused<>1) then
    code:=5;
  { the block whose store was refused is given back }
  if (code=226) and not RefusedFreed then
    code:=7;
  ReportingTerminateProcess:=OrigTerminateProcess(h,code);
end;

procedure SetImport(slot: PPointer; value: pointer);
var
  old: dword;
begin
  VirtualProtect(slot,sizeof(pointer),PAGE_READWRITE,old);
  slot^:=value;
end;

{ points the module's imports of kernel32's TlsSetValue, HeapFree and
  TerminateProcess at the routines above; true when TlsSetValue was found }
function PatchImports(base: ptruint): boolean;
var
  opt, importrva, names, thunks, entry: ptruint;
  desc: PDWord;
  dllname, fname: ShortString;
  slot: PPointer;
  i: longint;
begin
  result:=false;
  if base=0 then
    exit;
  opt:=base+ptruint(PLongInt(base+$3C)^)+24;
  { the import directory is the second data directory, which starts 96 (PE32)
    or 112 (PE32+) bytes into the optional header }
  if PWord(opt)^=$20B then
    importrva:=PDWord(opt+112+8)^
  else
    importrva:=PDWord(opt+96+8)^;
  if importrva=0 then
    exit;
  desc:=PDWord(base+importrva);
  while desc[3]<>0 do
    begin
      dllname:=lowercase(ShortString(PAnsiChar(base+desc[3])));
      if (dllname='kernel32.dll') or (dllname='kernel32') then
        begin
          { the names, from the import lookup table: the module is loaded, so
            its import address table holds addresses }
          if desc[0]=0 then
            exit;
          names:=base+desc[0];
          thunks:=base+desc[4];
          i:=0;
          repeat
            entry:=PPtrUInt(names+i*sizeof(pointer))^;
            if entry=0 then
              break;
            { by name: a two byte hint, then the name }
            if (entry and (ptruint(1) shl (sizeof(pointer)*8-1)))=0 then
              begin
                fname:=ShortString(PAnsiChar(base+dword(entry)+2));
                slot:=PPointer(thunks+i*sizeof(pointer));
                if fname='TlsSetValue' then
                  begin
                    OrigTlsSetValue:=TTlsSetValue(slot^);
                    SetImport(slot,@FailingTlsSetValue);
                    result:=true;
                  end
                else if fname='HeapFree' then
                  begin
                    OrigHeapFree:=THeapFree(slot^);
                    SetImport(slot,@NotingHeapFree);
                  end
                else if fname='TerminateProcess' then
                  begin
                    OrigTerminateProcess:=TTerminateProcess(slot^);
                    SetImport(slot,@ReportingTerminateProcess);
                  end;
              end;
            inc(i);
          until false;
        end;
      inc(desc,5);
    end;
end;

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
  h: THandle;
  id: dword;
begin
{$ifndef FPC_USE_TLS_DIRECTORY}
  { without a TLS directory a program that has started no thread through the
    run time library uses the threadvars in its image on every thread, so no
    block is stored to refuse: end as the test expects }
  writeln('skipped: the compiler makes no TLS directory');
  halt(226);
{$endif FPC_USE_TLS_DIRECTORY}
  if not PatchImports(GetModuleHandle(nil)) or
     not assigned(OrigTerminateProcess) or
     not assigned(OrigHeapFree) then
    Fail(1,'the imports were not found');
  { threadvars are allocated when the thread starts, before its body runs }
  h:=CreateThread(nil,0,@Body,nil,CREATE_SUSPENDED,id);
  ArmedThread:=id;
  ResumeThread(h);
  if WaitForSingleObject(h,20000)<>0 then
    Fail(4,'the thread did not end');
  if Refused=0 then
    Fail(3,'no store was refused');
  Fail(2,'the process went on after a refused store');
end.
