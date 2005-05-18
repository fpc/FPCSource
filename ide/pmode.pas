{
    This file is part of the Free Sockets Interface
    Copyright (c) 1999 by Berczi Gabor

    Support routines for DPMI programs

    See the file COPYING.FCL, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$ifdef VER70}{$define TP}{$endif}
unit PMode;

interface

uses Dos;

type
    MemPtr = object
      Ofs,Seg: word;
      Size   : word;
    {$ifdef DPMI}
      Sel    : word;
    {$endif}
      function  DosPtr: pointer;
      function  DataPtr: pointer;
      function  DosSeg: word;
      function  DosOfs: word;
      procedure MoveDataTo(var Src; DSize: word);
      procedure MoveDataFrom(DSize: word; var Dest);
      procedure Clear;
    private
      function DataSeg: word;
      function DataOfs: word;
    end;

    PtrRec = packed record
      Ofs,Seg: word;
    end;

    registers32 = packed record     { DPMI call structure }
      EDI     : LongInt;
      ESI     : LongInt;
      EBP     : LongInt;
      Reserved: LongInt;
      EBX     : LongInt;
      EDX     : LongInt;
      ECX     : LongInt;
      EAX     : LongInt;
      Flags   : Word;
      ES      : Word;
      DS      : Word;
      FS      : Word;
      GS      : Word;
      IP      : Word;
      CS      : Word;
      SP      : Word;
      SS      : Word;
    end;

    pregisters = ^registers;

function  GetDosMem(var M: MemPtr; Size: word): boolean;
procedure FreeDosMem(var M: MemPtr);
procedure realintr(IntNo: byte; var r: registers);
{procedure realintr32(IntNo: byte; var r: registers32);}
procedure realcall(Proc: pointer; var r: registers);
function  MoveDosToPM(DosPtr: pointer; PMPtr: pointer; Size: word): boolean;
function  MovePMToDos(PMPtr: pointer; DosPtr: pointer; Size: word): boolean;
procedure realGetIntVec(IntNo: byte; var P: pointer);
function  allocrmcallback(PMAddr: pointer; RealRegs: pregisters): pointer;
procedure freermcallback(RealCallAddr: pointer);

function MakePtr(ASeg,AOfs: word): pointer;

implementation

{$ifdef TP}
{$ifdef DPMI}uses WinAPI;{$endif}

{$IFDEF DPMI}
const
    DPMI_INTR      = $31;

type
    TDPMIRegisters = {$ifdef TP}Registers32{$else}TRegisters32{$endif};

  var
    DPMIRegs: TDPMIRegisters;
{$ENDIF DPMI}

procedure realintr(IntNo: byte; var r: registers);
{$ifdef DPMI}
var Regs: Registers;
begin
  FillChar(DPMIRegs, SizeOf(TDPMIRegisters), 0);
  DPMIRegs.EAX := r.ax;
  DPMIRegs.EBX := r.bx;
  DPMIRegs.ECX := r.cx;
  DPMIRegs.EDX := r.dx;
  DPMIRegs.EDI := r.di;
  DPMIRegs.ESI := r.si;
  DPMIRegs.EBP := r.bp;
  DPMIRegs.DS := r.ds;
  DPMIRegs.ES := r.es;
  DPMIRegs.Flags := r.flags;
  Regs.AX := $0300;
  Regs.BL := IntNo;
  Regs.BH := 0;
  Regs.CX := 0;
  Regs.ES := Seg(DPMIRegs);
  Regs.DI := Ofs(DPMIRegs);
  Intr(DPMI_INTR, Regs);
  r.ax := DPMIRegs.EAX;
  r.bx := DPMIRegs.EBX;
  r.cx := DPMIRegs.ECX;
  r.dx := DPMIRegs.EDX;
  r.di := DPMIRegs.EDI;
  r.si := DPMIRegs.ESI;
  r.bp := DPMIRegs.EBP;
  r.ds := DPMIRegs.DS;
  r.es := DPMIRegs.ES;
  r.Flags := DPMIRegs.Flags;
end;
{$else}
begin
  intr(IntNo,r);
end;
{$endif}

(*procedure realintr32(IntNo: byte; var r: registers32);
{$ifdef DPMI}
var Regs: Registers;
begin
  FillChar(DPMIRegs, SizeOf(TDPMIRegisters), 0);
  DPMIRegs:=r;

  Regs.AX := $0300;
  Regs.BL := IntNo;
  Regs.BH := 0;
  Regs.CX := 0;
  Regs.ES := Seg(DPMIRegs);
  Regs.DI := Ofs(DPMIRegs);
  Intr(DPMI_INTR, Regs);
  r:=DPMIRegs;
end;
{$else}
begin
  { not implemented }
  Halt(99);
end;
{$endif}
*)

{$ifndef DPMI}
const DummyIntRedir: boolean = false;
      CallAddr: pointer = nil;
      DummyInt = $ef;
procedure CallInt; assembler;
asm
  push  ax
  push  ds

  mov   ax, seg CallAddr
  mov   ds, ax
  mov   ax, ds:CallAddr.word[0]
  mov   cs:@JmpAddr.word[0], ax
  mov   ax, ds:CallAddr.word[2]
  mov   cs:@JmpAddr.word[2], ax

  pop   ds
  pop   ax

  sti

  db    $9a
@JmpAddr:
  dw    0,0
  jmp   @over
@regax: dw  0
@over:
  mov  word ptr cs:@regax, ax
  push bx
  pushf
  pop  ax
  mov  bx, sp
  mov  word ptr ss:[bx+6], ax
  pop  bx
  mov  ax, word ptr cs:@regax

  iret
end;
{$endif}

procedure realcall(Proc: pointer; var r: registers);
{$ifdef DPMI}
var Regs: Registers;
begin
  FillChar(DPMIRegs, SizeOf(TDPMIRegisters), 0);
  DPMIRegs.EAX := r.ax;
  DPMIRegs.EBX := r.bx;
  DPMIRegs.ECX := r.cx;
  DPMIRegs.EDX := r.dx;
  DPMIRegs.EDI := r.di;
  DPMIRegs.ESI := r.si;
  DPMIRegs.EBP := r.bp;
  DPMIRegs.DS := r.ds;
  DPMIRegs.ES := r.es;
  DPMIRegs.Flags := r.flags;
  DPMIRegs.CS := PtrRec(Proc).Seg;
  DPMIRegs.IP := PtrRec(Proc).Ofs;
  DPMIRegs.SS :=0; DPMIRegs.SP:=0;
  Regs.AX := $0301;
  Regs.BH := 0;
  Regs.CX := 0;
  Regs.ES := Seg(DPMIRegs);
  Regs.DI := Ofs(DPMIRegs);
  Intr(DPMI_INTR, Regs);
  r.ax := DPMIRegs.EAX and $ffff;
  r.bx := DPMIRegs.EBX and $ffff;
  r.cx := DPMIRegs.ECX and $ffff;
  r.dx := DPMIRegs.EDX and $ffff;
  r.di := DPMIRegs.EDI and $ffff;
  r.si := DPMIRegs.ESI and $ffff;
  r.bp := DPMIRegs.EBP and $ffff;
  r.ds := DPMIRegs.DS;
  r.es := DPMIRegs.ES;
  r.Flags := DPMIRegs.Flags and $ffff;
end;
{$else}
(*begin
  asm
    push ds
    push bp

    mov  ax, Proc.word[2]
    mov  bx, Proc.word[0]
    mov  cs:@Call+1.word, bx
    mov  cs:@Call+3.word, ax

    lds  si, r
    mov  @rptr.word[2], ds
    mov  @rptr.word[0], si

    lodsw
    push ax { -> ax }
    lodsw
    mov  bx, ax
    lodsw
    mov  cx, ax
    lodsw
    mov  dx, ax
    lodsw
    mov  bp, ax
    lodsw
    push ax { -> si }
    lodsw
    mov  di, ax
    lodsw
    push ax { -> ds }
    lodsw
    mov  es, ax
    lodsw
    push ax { -> flags }
    popf

    pop  si
    pop  ds
    pop  ax

@Call:
    db   9ah
    dd   0

    jmp  @skipover
@rptr: dd  0
@skipover:

    pushf
    push es
    push di

    mov  es, @rptr.word[2]
    mov  di, @rptr.word[0]
    stosw
    mov  ax, bx
    stosw
    mov  ax, cx
    stosw
    mov  ax, dx
    stosw
    mov  ax, bp
    stosw
    mov  ax, si
    stosw
    pop  ax { <- di }
    stosw
    mov  ax, ds
    stosw
    pop  ax { <- es }
    stosw
    pop  ax { <- flags }
    stosw

    pop  bp
    pop  ds
  end;
end;
*)
begin
  if DummyIntRedir=false then
    begin
      SetIntVec(DummyInt,@CallInt);
      DummyIntRedir:=true;
    end;
  CallAddr:=Proc;
  dos.intr(DummyInt,r);
end;

{$endif}

(*const ActiveBlocks: word = 0;*)

function GetDosMem(var M: MemPtr; Size: word): boolean;
var P: pointer;
    L: longint;
begin
  M.Size:=Size;
{$ifndef DPMI}
  GetMem(P,Size);
  M.Seg:=PtrRec(P).Seg; M.Ofs:=PtrRec(P).Ofs;
{$else}
  L:=GlobalDosAlloc(Size);
  M.Seg:=(L shr 16); M.Ofs:=0;
  M.Sel:=(L and $ffff);
{$endif}
  if M.Seg<>0 then M.Clear;
  GetDosMem:=M.Seg<>0;
(*  Inc(ActiveBlocks);
  write('|DMC:',ActiveBlocks,'-S:',M.Sel,'-S:',M.Seg);*)
end;

procedure FreeDosMem(var M: MemPtr);
begin
  if M.Size=0 then Exit;
{$ifndef DPMI}
  if M.Seg<>0 then
  FreeMem(Ptr(M.Seg,M.Ofs),M.Size);
{$else}
  if M.Sel<>0 then
   if GlobalDosFree(M.Sel)<>0 then
    writeln('!!!Failed to deallocate Dos block!!!');
{$endif}

  FillChar(M,SizeOf(M),0);
end;

{$ifdef DPMI}
function GetSelectorForSeg(Seg: word): word;
var Sel: word;
    r: registers;
begin
  r.ax:=$0002; r.bx:=Seg;
  intr(DPMI_Intr,r);
  if (r.flags and fCarry)=0 then
    Sel:=r.ax
  else
    Sel:=0;
  GetSelectorForSeg:=Sel;
end;
{$endif}

function MoveDosToPM(DosPtr: pointer; PMPtr: pointer; Size: word): boolean;
{$ifndef DPMI}
begin
  Move(DosPtr^,PMPtr^,Size);
  MoveDosToPM:=true;
end;
{$else}
var Sel: word;
    OK,DisposeSel: boolean;
begin
  Sel:=GetSelectorForSeg(PtrRec(DosPtr).Seg);
  OK:=Sel<>0; DisposeSel:=false;
  if OK=false then
    begin
      Sel:=AllocSelector(0);
      OK:=Sel<>0;
      if OK then
        begin
          SetSelectorLimit(Sel,PtrRec(DosPtr).Ofs+Size);
          OK:=SetSelectorBase(Sel,PtrRec(DosPtr).Seg shl 4)=Sel;
        end;
      if OK then DisposeSel:=true;
    end;
  if OK then
    begin
      Move(ptr(Sel,PtrRec(DosPtr).Ofs)^,PMPtr^,Size);
      if DisposeSel then FreeSelector(Sel);
    end;
  MoveDosToPM:=OK;
end;
{$endif}

function MovePMToDos(PMPtr: pointer; DosPtr: pointer; Size: word): boolean;
{$ifndef DPMI}
begin
  Move(PMPtr^,DosPtr^,Size);
  MovePMToDos:=true;
end;
{$else}
var Sel: word;
    OK,DisposeSel: boolean;
begin
  Sel:=GetSelectorForSeg(PtrRec(DosPtr).Seg);
  OK:=Sel<>0; DisposeSel:=false;
  if OK=false then
    begin
      Sel:=AllocSelector(0);
      OK:=Sel<>0;
      if OK then
        begin
          SetSelectorLimit(Sel,PtrRec(DosPtr).Ofs+Size);
          OK:=SetSelectorBase(Sel,PtrRec(DosPtr).Seg shl 4)=Sel;
        end;
      if OK then DisposeSel:=true;
    end;
  if OK then
    begin
      Move(PMPtr^,ptr(Sel,PtrRec(DosPtr).Ofs)^,Size);
      if DisposeSel then FreeSelector(Sel);
    end;
  MovePMToDos:=OK;
end;
{$endif}

procedure realGetIntVec(IntNo: byte; var P: pointer);
{$ifndef DPMI}
begin
  GetIntVec(IntNo,P);
end;
{$else}
var r: registers;
begin
  r.ax:=$200; r.bl:=IntNo;
  intr(DPMI_Intr,r);
  P:=Ptr(r.cx,r.dx);
end;
{$endif}

procedure MemPtr.MoveDataTo(const Src; DSize: word);
begin
  if DSize>Size then
    RunError(216);
  Move(Src,Ptr(DataSeg,DataOfs)^,DSize);
end;

procedure MemPtr.MoveDataFrom(DSize: word; var Dest);
begin
  if DSize>Size then
    RunError(216);
  Move(Ptr(DataSeg,DataOfs)^,Dest,DSize);
end;

procedure MemPtr.Clear;
begin
  FillChar(Ptr(DataSeg,DataOfs)^,Size,0);
end;

procedure RealAbstract;
begin
  writeln('Abstract call in real mode...');
  RunError(255);
end;

function  allocrmcallback(PMAddr: pointer; RealRegs: pregisters): pointer;
{$ifdef DPMI}
var r: registers;
    P: pointer;
begin
  r.ax:=$0303;
  r.ds:=PtrRec(PMAddr).Seg; r.si:=PtrRec(PMAddr).Ofs;
  r.es:=PtrRec(RealRegs).Seg; r.di:=PtrRec(RealRegs).Ofs;
  intr(DPMI_Intr,r);
  if (r.flags and fCarry)=0 then
    P:=MakePtr(r.cx,r.dx)
  else
    P:=nil;
  allocrmcallback:=P;
end;
{$else}
begin
  RealAbstract;
end;
{$endif}

procedure freermcallback(RealCallAddr: pointer);
{$ifdef DPMI}
var r: registers;
begin
  r.ax:=$0304;
  r.cx:=PtrRec(RealCallAddr).Seg; r.dx:=PtrRec(RealCallAddr).Seg;
  intr(DPMI_Intr,r);
end;
{$else}
begin
  RealAbstract;
end;
{$endif}

{$endif TP}

{$ifdef GO32V2}

{ --------------------- GO32 --------------------- }

uses go32;

function  GetDosMem(var M: MemPtr; Size: word): boolean;
var L: longint;
begin
  M.Size:=Size;
  L:=global_dos_alloc(Size);
  M.Seg:=(L shr 16); M.Ofs:=0;
  M.Sel:=(L and $ffff);
  GetDosMem:=M.Seg<>0;
end;

procedure FreeDosMem(var M: MemPtr);
begin
  if M.Size=0 then Exit;
  if M.Sel<>0 then
  if global_dos_free(M.Sel)=false then
    writeln('!!!Failed to deallocate Dos block!!!');
  FillChar(M,SizeOf(M),0);
end;

procedure realintr(IntNo: byte; var r: registers);
var rr: trealregs;
begin
  rr.realeax:=r.ax;
  rr.realebx:=r.bx;
  rr.realecx:=r.cx;
  rr.realedx:=r.dx;
  rr.realesi:=r.si;
  rr.realedi:=r.di;
  rr.reales:=r.es;
  rr.realds:=r.ds;
  go32.realintr(IntNo,rr);
  r.ax:=rr.realeax and $ffff;
  r.bx:=rr.realebx and $ffff;
  r.cx:=rr.realecx and $ffff;
  r.dx:=rr.realedx and $ffff;
  r.si:=rr.realesi and $ffff;
  r.di:=rr.realedi and $ffff;
  r.es:=rr.reales and $ffff;
  r.ds:=rr.realds and $ffff;
end;

function dorealcall(var regs : trealregs) : boolean;
begin
  regs.realsp:=0;
  regs.realss:=0;
  asm
    movw  $0x0,%bx
    xorl  %ecx,%ecx
    movl  regs,%edi
    { es is always equal ds }
    movl  $0x301,%eax
    int   $0x31
    setnc %al
    movb  %al,__RESULT
  end;
end;


procedure realcall(Proc: pointer; var r: registers);
var rr: trealregs;
begin
  rr.realeax:=r.ax;
  rr.realebx:=r.bx;
  rr.realecx:=r.cx;
  rr.realedx:=r.dx;
  rr.realesi:=r.si;
  rr.realedi:=r.di;
  rr.reales:=r.es;
  rr.realds:=r.ds;
  rr.flags:=r.flags;
  rr.CS:=PtrRec(Proc).Seg;
  rr.IP:=PtrRec(Proc).Ofs;

  rr.realss:=0; rr.realsp:=0;

  dorealcall(rr);

  r.ax:=rr.realeax and $ffff;
  r.bx:=rr.realebx and $ffff;
  r.cx:=rr.realecx and $ffff;
  r.dx:=rr.realedx and $ffff;
  r.si:=rr.realesi and $ffff;
  r.di:=rr.realedi and $ffff;
  r.es:=rr.reales and $ffff;
  r.ds:=rr.realds and $ffff;
  r.flags:=rr.Flags and $ffff;
end;

function MoveDosToPM(DosPtr: pointer; PMPtr: pointer; Size: word): boolean;
begin
  dosmemget(PtrRec(DosPtr).Seg,PtrRec(DosPtr).Ofs,PMPtr^,Size);
  MoveDosToPM:=true;
end;

function MovePMToDos(PMPtr, DosPtr: pointer; Size: word): boolean;
begin
  dosmemput(PtrRec(DosPtr).Seg,PtrRec(DosPtr).Ofs,PMPtr^,Size);
  MovePMToDos:=true;
end;

procedure realGetIntVec(IntNo: byte; var P: pointer);
var si: tseginfo;
begin
  get_rm_interrupt(IntNo,si);
  PtrRec(P).Seg:=si.segment; PtrRec(P).Ofs:=longint(si.offset);
end;

procedure MemPtr.MoveDataTo(var Src; DSize: word);
begin
  dpmi_dosmemput(DosSeg,DosOfs,Src,DSize);
end;

procedure MemPtr.MoveDataFrom(DSize: word; var Dest);
begin
  dpmi_dosmemget(DosSeg,DosOfs,Dest,DSize);
end;

procedure MemPtr.Clear;
begin
  dpmi_dosmemfillchar(DosSeg,DosOfs,Size,#0);
end;


function  allocrmcallback(PMAddr: pointer; RealRegs: pregisters): pointer;
var s: tseginfo;
    P: pointer;
begin
  if get_rm_callback(PMAddr,RealRegs^,s) then
    P:=MakePtr(s.segment,longint(s.offset))
  else
    P:=nil;
  allocrmcallback:=P;
end;

procedure freermcallback(RealCallAddr: pointer);
var s: tseginfo;
begin
  s.segment:=PtrRec(RealCallAddr).seg;
  s.offset:=pointer(longint(PtrRec(RealCallAddr).ofs));
  free_rm_callback(s);
end;

{$endif GO32V2}

{ ---------------------- COMMON ---------------------- }

function MemPtr.DosPtr: pointer;
begin
  DosPtr:=MakePtr(Seg,Ofs);
end;

function MemPtr.DataPtr: pointer;
begin
  DataPtr:=MakePtr(DataSeg,DataOfs);
end;

function MemPtr.DataSeg: word;
begin
{$ifndef DPMI}
  DataSeg:=Seg;
{$else}
  DataSeg:=Sel;
{$endif}
end;

function MemPtr.DataOfs: word;
begin
{$ifndef DPMI}
  DataOfs:=Ofs;
{$else}
  DataOfs:=0;
{$endif}
end;

function MemPtr.DosSeg: word;
begin
  DosSeg:=Seg;
end;

function MemPtr.DosOfs: word;
begin
  DosOfs:=Ofs;
end;

function MakePtr(ASeg, AOfs: word): pointer;
var P: pointer;
begin
  with PtrRec(P) do
  begin
    Seg:=ASeg; Ofs:=AOfs;
  end;
  MakePtr:=P;
end;

END.
