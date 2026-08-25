{ %cpu=aarch64 }
{ %target=win64 }
{ %opt=-O2 }

{ From one page of locals upwards the allocation is emitted through the stack
  probing path, which emitted no ash_stackalloc directive, so the allocation
  was missing from the .xdata altogether.

  For a function whose only unwind relevant prolog operation is that
  allocation, the whole unwind record then stays empty and the function gets no
  .pdata entry at all - which per the ARM64 specification means a leaf that
  does not touch sp and returns through lr. Both are wrong for it: unwinding
  out of such a function leaves sp 8192 bytes too low.

  The test checks both halves: that the .pdata entry exists, and that
  RtlVirtualUnwind recovers the caller's sp from a fabricated context inside
  the function. It does not run the function itself, so it is independent of
  the real stack, of ASLR and of the thread it runs on.

  Exit code 2 means the code generator no longer emits the shape this test
  knows about, so the test needs updating - it does not mean the defect is
  back.

  The CONTEXT offsets are hardcoded because the windows unit has no ARM64
  CONTEXT: wininc/struct.inc defines one for i386, x86_64 and powerpc32 only,
  and TContext is an empty record everywhere else. }

program tw41871;

{$mode objfpc}{$H+}

uses
  Windows;

const
  CTXSIZE  = $3A0;
  O_FLAGS  = $000;
  O_LR     = $0F8;
  O_SP     = $100;
  O_PC     = $108;
  CONTEXT_ARM64_FULL = $00400007;

  LRSENT = QWord($1234123412341234);

  FRAME = 8192;

  { the shape this test knows about }
  I_SUB_SP_8192 = $D1400BFF;   { sub sp,sp,#2,lsl #12 }
  I_ADD_SP_8192 = $91400BFF;   { add sp,sp,#2,lsl #12 }
  I_RET         = $D65F03C0;   { ret                  }

type
  TQP = ^QWord;

function RtlLookupFunctionEntry(ControlPc: PtrUInt; var ImageBase: PtrUInt;
  HistoryTable: pointer): pointer; stdcall;
  external 'kernel32.dll' name 'RtlLookupFunctionEntry';

function RtlVirtualUnwind(HandlerType: DWORD; ImageBase: PtrUInt;
  ControlPc: PtrUInt; FunctionEntry: pointer; ContextRecord: pointer;
  HandlerData: PPointer; EstablisherFrame: TQP;
  ContextPointers: pointer): pointer; stdcall;
  external 'kernel32.dll' name 'RtlVirtualUnwind';

var
  sink: byte = 0;
  ctxbuf: array[0..CTXSIZE + 63] of byte;
  stkbuf: array[0..FRAME * 2 - 1] of byte;   { holds the whole fabricated frame }

{ a leaf whose only stack manipulation is an allocation of one page or more }
procedure LeafOverPage; noinline;
var
  buf: array[0..FRAME - 1] of byte;
begin
  buf[0] := sink;
  sink := buf[high(buf)];
end;

function CtxP: PByte;
begin
  result := PByte((PtrUInt(@ctxbuf) + 15) and not PtrUInt(15));
end;

var
  fn, p, epi: PtrUInt;
  base, estab: PtrUInt;
  fe, hdata: pointer;
  i: integer;
  S: QWord;
  failed: boolean = false;
begin
  fn := PtrUInt(@LeafOverPage);

  if PDWord(fn)^ <> I_SUB_SP_8192 then
    begin
      writeln('prolog shape not recognised (first instruction ',
              hexstr(PDWord(fn)^, 8), ') - this test needs updating');
      halt(2);
    end;
  { find the end of the function, then check that the instruction in front of
    it releases the frame. Bounded by the ret, so the scan cannot wander into
    the next function. }
  epi := 0;
  for i := 0 to 255 do
    begin
      p := fn + PtrUInt(i) * 4;
      if PDWord(p)^ = I_RET then
        begin
          epi := p - 4;
          break;
        end;
    end;
  if (epi = 0) or
     (PDWord(epi)^ <> I_ADD_SP_8192) then
    begin
      writeln('epilog shape not recognised - this test needs updating');
      halt(2);
    end;

  base := 0;
  fe := RtlLookupFunctionEntry(fn, base, nil);
  if fe = nil then
    begin
      writeln('no .pdata entry: Windows takes this function for a leaf that ',
              'does not touch sp');
      halt(1);
    end;

  { fabricated context at the last body instruction before the epilog, where
    the allocation is in effect. A body pc is described by the prolog codes, so
    this half stands on its own and does not depend on the epilog scope index
    of #41870. }
  S := (PtrUInt(@stkbuf) + FRAME + 64) and not PtrUInt(15);   { S - FRAME stays inside stkbuf }
  FillChar(CtxP^, CTXSIZE, 0);
  PDWord(CtxP + O_FLAGS)^ := CONTEXT_ARM64_FULL;
  TQP(CtxP + O_SP)^ := S - FRAME;
  TQP(CtxP + O_PC)^ := epi - 4;
  TQP(CtxP + O_LR)^ := LRSENT;
  estab := 0;
  RtlVirtualUnwind(0, base, epi - 4, fe, CtxP, @hdata, @estab, nil);

  if TQP(CtxP + O_SP)^ <> S then
    begin
      writeln('sp is ', hexstr(TQP(CtxP + O_SP)^, 16), ', expected ',
              hexstr(S, 16), ' (out by ',
              Int64(TQP(CtxP + O_SP)^) - Int64(S), ' bytes)');
      failed := true;
    end;
  if TQP(CtxP + O_PC)^ <> LRSENT then
    begin
      writeln('return address is ', hexstr(TQP(CtxP + O_PC)^, 16),
              ', expected ', hexstr(LRSENT, 16));
      failed := true;
    end;

  if failed then
    halt(1);
  writeln('ok');
end.
