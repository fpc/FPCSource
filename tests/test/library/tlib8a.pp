{ %target=win32,win64 }
{ %needlibrary }
{ %norun }
{ %neededafter }

{ a library with its own run time library, used by the program of tlib8b.pp:
  it points its own imports of TlsSetValue, HeapFree and TerminateProcess at
  the routines below, and arm makes it refuse the store of the threadvar block
  of the thread it names }
library tlib8a;

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
  PAGE_READWRITE = 4;
  ERROR_NOT_ENOUGH_MEMORY = 8;

function GetCurrentThreadId: dword; stdcall; external 'kernel32' name 'GetCurrentThreadId';
function VirtualProtect(addr: pointer; size: ptruint; prot: dword; var old: dword): longbool; stdcall; external 'kernel32' name 'VirtualProtect';
function VirtualQuery(addr: pointer; var info: TMemoryBasicInformation; size: ptruint): ptruint; stdcall; external 'kernel32' name 'VirtualQuery';
procedure SetLastError(code: dword); stdcall; external 'kernel32' name 'SetLastError';
function HeapSize(heap: THandle; flags: dword; mem: pointer): ptruint; stdcall; external 'kernel32' name 'HeapSize';
function GetStdHandle(n: dword): THandle; stdcall; external 'kernel32' name 'GetStdHandle';
function GetCurrentProcess: THandle; stdcall; external 'kernel32' name 'GetCurrentProcess';
function TerminateProcess(h: THandle; code: dword): longbool; stdcall; external 'kernel32' name 'TerminateProcess';
function WriteFile(h: THandle; buf: pointer; len: dword; var written: dword; ov: pointer): longbool; stdcall; external 'kernel32' name 'WriteFile';

var
  ArmedThread: dword = 0;
  Refused: longint = 0;
  OrigTlsSetValue: TTlsSetValue = nil;
  OrigTerminateProcess: TTerminateProcess = nil;
  OrigHeapFree: THeapFree = nil;
  RefusedBlock: pointer = nil;
  RefusedFreed: boolean = false;
  FreedAtStored: boolean = false;
  ImportsPatched: boolean = false;

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
      { a library's block starts before the address stored, which follows the
        block's entry in the list of a library's blocks }
      RefusedBlock:=value;
      SetLastError(ERROR_NOT_ENOUGH_MEMORY);
      FailingTlsSetValue:=false;
    end
  else
    FailingTlsSetValue:=OrigTlsSetValue(index,value);
end;

{ uses the operating system alone, as FailingTlsSetValue: the refused block is
  given back when the allocation freed starts before the address stored and
  holds it; the address stored itself is no allocation, so it is not freed }
function NotingHeapFree(heap: THandle; flags: dword; mem: pointer): longbool; stdcall;
begin
  if assigned(RefusedBlock) and (mem=RefusedBlock) then
    begin
      FreedAtStored:=true;
      NotingHeapFree:=true;
      exit;
    end;
  if assigned(RefusedBlock) and assigned(mem) and
     (ptruint(mem)<ptruint(RefusedBlock)) and
     (ptruint(RefusedBlock)<ptruint(mem)+HeapSize(heap,0,mem)) then
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
  { the block whose store was refused is given back, from where it starts }
  if (code=226) and FreedAtStored then
    code:=6;
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
  TerminateProcess at the routines above; true when all three were found }
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
  result:=assigned(OrigTlsSetValue) and assigned(OrigHeapFree) and
    assigned(OrigTerminateProcess);
end;

{ ends the process with 1 when the imports were not all found: this use is
  also what makes the library import TerminateProcess itself, for a run time
  library that does not }
procedure arm(id: dword);
const
  msg: array[0..37] of AnsiChar = 'the library''s imports were not found'#13#10;
var
  written: dword;
begin
  if not ImportsPatched then
    begin
      WriteFile(GetStdHandle(dword(-11)),@msg,sizeof(msg),written,nil);
      TerminateProcess(GetCurrentProcess,1);
    end;
  ArmedThread:=id;
end;

exports arm;

begin
  ImportsPatched:=PatchImports(HInstance);
end.
