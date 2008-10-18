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
unit PMode;

interface

uses Dos;

type
    MemPtr = object
      Ofs,Seg: word;
      Size   : word;
      Sel    : word;
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
  DataSeg:=Sel;
end;

function MemPtr.DataOfs: word;
begin
  DataOfs:=0;
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
