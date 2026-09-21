{ %CPU=x86_64 }
{ %TARGET=win64 }

{ The return address of the last call in a routine that never returns must
  not point at its final ret: the Win64 unwinder takes a return address on a
  ret for the routine's epilogue, simulates it, and reads the caller's return
  address from the top of the routine's stack instead of through its unwind
  information. Probe allocates stack, says so in its unwind information and
  ends with a call; the program asks the system's unwinder to unwind Probe
  from the return address of that call, on a stack of its own, and checks
  that the unwinder used the unwind information: the return address planted
  above the allocated space, and the stack pointer above it. Probe itself is
  never run. Exit code 2: Probe's code is not the expected sub/call, so the
  test no longer tests anything and must be adapted }
program tb0738;

{$mode objfpc}
{$asmmode intel}

uses
  windows;

type
  PRuntimeFunction = ^TRuntimeFunction;
  TRuntimeFunction = packed record
    BeginAddress, EndAddress, UnwindData: DWORD;
  end;

function RtlLookupFunctionEntry(pc: PtrUInt; var imagebase: PtrUInt;
  history: pointer): PRuntimeFunction; stdcall;
  external 'kernel32.dll' name 'RtlLookupFunctionEntry';

function RtlVirtualUnwind(handlertype: DWORD; imagebase, pc: PtrUInt;
  entry: PRuntimeFunction; context: pointer; handlerdata: PPointer;
  establisherframe: PPtrUInt; contextpointers: pointer): pointer; stdcall;
  external 'kernel32.dll' name 'RtlVirtualUnwind';

procedure Leaf; assembler; nostackframe;
asm
  nop
end;

procedure Probe; assembler; nostackframe; noreturn;
asm
  sub rsp,40
  .seh_stackalloc 40
  .seh_endprologue
  call Leaf
end;

var
  context: TContext;
  stack: array[0..255] of byte;
  imagebase, pc, sp, frame: PtrUInt;
  entry: PRuntimeFunction;
  handlerdata: pointer;
begin
  pc:=PtrUInt(@Probe);
  imagebase:=0;
  entry:=RtlLookupFunctionEntry(pc,imagebase,nil);
  if entry=nil then
    begin
      writeln('Probe has no unwind information');
      halt(2);
    end;
  { sub rsp,40 (48 83 EC 28) followed by call rel32 (E8) }
  if (PDWord(pc)^<>$28EC8348) or (PByte(pc+4)^<>$E8) then
    begin
      writeln('unexpected code in Probe: ',hexstr(PDWord(pc)^,8),' ',hexstr(PByte(pc+4)^,2));
      halt(2);
    end;
  { the return address of the call }
  inc(pc,9);
  if pc>=imagebase+entry^.EndAddress then
    begin
      writeln('the return address of the call lies past the end of Probe');
      halt(1);
    end;
  fillchar(stack,sizeof(stack),0);
  sp:=(PtrUInt(@stack[64])+15) and not PtrUInt(15);
  PPtrUInt(sp+40)^:=PtrUInt(@Leaf);
  fillchar(context,sizeof(context),0);
  context.ContextFlags:=CONTEXT_CONTROL or CONTEXT_INTEGER;
  context.Rip:=pc;
  context.Rsp:=sp;
  handlerdata:=nil;
  frame:=0;
  RtlVirtualUnwind(0,imagebase,pc,entry,@context,@handlerdata,@frame,nil);
  if (context.Rip<>PtrUInt(@Leaf)) or (context.Rsp<>sp+48) then
    begin
      writeln('unwound to $',hexstr(context.Rip,16),', stack $',hexstr(context.Rsp,16),
        '; expected $',hexstr(PtrUInt(@Leaf),16),', $',hexstr(sp+48,16));
      halt(1);
    end;
  writeln('ok');
end.
