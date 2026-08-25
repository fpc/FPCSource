{ %cpu=aarch64 }
{ %target=win64 }
{ %opt=-O2 }

{ The epilog start index in the .xdata header is a byte index into the unwind
  code array. Writing 1 into it addresses the operand byte of a multi-byte
  first code, and the unwinder then decodes the epilog from the middle of that
  code.

  A 4016 byte frame gives the codes c0 fb e1 81 e4, where c0 fb is alloc_m.
  With index 1 the epilog scope starts at fb, which is not a code.

  The test hands RtlVirtualUnwind a fabricated context at the last body
  instruction and at each instruction of that function's epilog, and checks
  that the caller's sp and return address come back. It does not run the
  function itself, so it is independent of the real stack, of ASLR and of the
  thread it runs on.

  Exit code 2 means the code generator no longer emits the shape this test
  knows about, so the test needs updating - it does not mean the defect is
  back.

  The CONTEXT offsets are hardcoded because the windows unit has no ARM64
  CONTEXT: wininc/struct.inc defines one for i386, x86_64 and powerpc32 only,
  and TContext is an empty record everywhere else. }

program tw41870;

{$mode objfpc}{$H+}

uses
  Windows;

const
  CTXSIZE  = $3A0;
  O_FLAGS  = $000;
  O_FP     = $0F0;
  O_LR     = $0F8;
  O_SP     = $100;
  O_PC     = $108;
  CONTEXT_ARM64_FULL = $00400007;

  FPSENT = QWord($F00DF00DF00DF00D);
  LRSENT = QWord($1234123412341234);
  BADLR  = QWord($BADBADBADBADBAD0);

  FRAME  = 4016;

  { the epilog this test knows about }
  I_ADD_SP_4016 = $913EC3FF;   { add sp,sp,#4016        }
  I_MOV_SP_FP   = $910003BF;   { mov sp,x29             }
  I_LDP_FP_LR   = $A8C17BFD;   { ldp x29,x30,[sp],#16   }
  I_RET         = $D65F03C0;   { ret                    }

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
  failed: boolean = false;
  ctxbuf: array[0..CTXSIZE + 63] of byte;
  stkbuf: array[0..8191] of byte;

procedure Callee; noinline;
begin
  inc(sink);
end;

{ 4016 bytes of locals plus a call: prolog is stp/mov/sub, so the first unwind
  code is the two-byte alloc_m }
procedure NonLeafUnderPage; noinline;
var
  buf: array[0..FRAME - 16] of byte;   { 4001 bytes, rounded up to a 4016 byte frame }
begin
  buf[0] := sink;
  Callee;
  sink := buf[high(buf)];
end;

function CtxP: PByte;
begin
  result := PByte((PtrUInt(@ctxbuf) + 15) and not PtrUInt(15));
end;

function StackTop: QWord;
begin
  { 16 byte aligned, inside the buffer, with room on both sides }
  result := (PtrUInt(@stkbuf) + 6000) and not PtrUInt(15);
end;

procedure Check(const what: string; fnstart, pc, sp, fp, lr, wantsp, wantpc: QWord);
var
  base, estab: PtrUInt;
  fe, hdata: pointer;
begin
  base := 0;
  fe := RtlLookupFunctionEntry(fnstart, base, nil);
  if fe = nil then
    begin
      writeln('no .pdata entry for the test function');
      failed := true;
      exit;
    end;
  FillChar(CtxP^, CTXSIZE, 0);
  PDWord(CtxP + O_FLAGS)^ := CONTEXT_ARM64_FULL;
  TQP(CtxP + O_SP)^ := sp;
  TQP(CtxP + O_PC)^ := pc;
  TQP(CtxP + O_FP)^ := fp;
  TQP(CtxP + O_LR)^ := lr;
  estab := 0;
  RtlVirtualUnwind(0, base, pc, fe, CtxP, @hdata, @estab, nil);
  if TQP(CtxP + O_SP)^ <> wantsp then
    begin
      writeln(what, ': sp is ', hexstr(TQP(CtxP + O_SP)^, 16),
              ', expected ', hexstr(wantsp, 16));
      failed := true;
    end;
  if TQP(CtxP + O_PC)^ <> wantpc then
    begin
      writeln(what, ': return address is ', hexstr(TQP(CtxP + O_PC)^, 16),
              ', expected ', hexstr(wantpc, 16));
      failed := true;
    end;
end;

var
  fn, p, epi: PtrUInt;
  i: integer;
  S: QWord;
begin
  fn := PtrUInt(@NonLeafUnderPage);

  { find the end of the function, then check that the three instructions in
    front of it are the epilog this test knows about. Bounded by the ret, so
    the scan cannot wander into the next function. }
  epi := 0;
  for i := 0 to 255 do
    begin
      p := fn + PtrUInt(i) * 4;
      if PDWord(p)^ = I_RET then
        begin
          epi := p - 12;
          break;
        end;
    end;
  if (epi = 0) or
     (PDWord(epi)^ <> I_ADD_SP_4016) or
     (PDWord(epi + 4)^ <> I_MOV_SP_FP) or
     (PDWord(epi + 8)^ <> I_LDP_FP_LR) then
    begin
      writeln('epilog shape not recognised - this test needs updating');
      halt(2);
    end;

  { a fabricated frame: the caller's sp is S, [S-16] holds x29 and [S-8] x30 }
  S := StackTop;
  TQP(S - 16)^ := FPSENT;
  TQP(S - 8)^ := LRSENT;

  { before the epilog: sp is below the whole frame, x29 points at the saved pair }
  Check('body', fn, epi - 4, S - 16 - QWord(FRAME), S - 16, BADLR, S, LRSENT);
  { at add sp,sp,#4016: same state, the add has not run yet }
  Check('add sp,sp,#4016', fn, epi, S - 16 - QWord(FRAME), S - 16, BADLR, S, LRSENT);
  { at mov sp,x29: the add has run }
  Check('mov sp,x29', fn, epi + 4, S - 16, S - 16, BADLR, S, LRSENT);
  { at ldp x29,x30,[sp],#16: the pair is still on the stack }
  Check('ldp x29,x30,[sp],#16', fn, epi + 8, S - 16, S - 16, BADLR, S, LRSENT);
  { at ret: everything is restored, x30 holds the return address }
  Check('ret', fn, epi + 12, S, FPSENT, LRSENT, S, LRSENT);

  if failed then
    halt(1);
  writeln('ok');
end.
