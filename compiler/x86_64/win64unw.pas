{
    Copyright (c) 2011 by Free Pascal development team

    Support for win64 unwind data

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************
}
unit win64unw;

{$i fpcdefs.inc}

interface

uses
  cclasses,globtype,aasmbase,aasmdata,aasmtai,cgbase,ogbase;

type
  TWin64CFI=class
  private
    FFrameOffs, FFrameReg: Integer;
    FFlags: Integer;
    FCount: Integer;
    FElements:TLinkedList;
    FFrameStartSym:TObjSymbol;
    FPrologueEndPos:aword;
    FName: pshortstring;
    procedure AddElement(objdata:TObjData;aCode,aInfo:Integer;aOffs:dword);
  public
    constructor create;
    destructor destroy;override;
    procedure generate_code(objdata:TObjData);
    procedure start_frame(objdata:TObjData; const name: string);
    procedure end_frame(objdata:TObjData);
    procedure end_prologue(objdata:TObjData);
    procedure push_reg(objdata:TObjData;reg:tregister);
    procedure save_reg(objdata:TObjData;reg:tregister;ofs:dword);
    procedure save_xmm(objdata:TObjData;reg:tregister;ofs:dword);
    procedure set_frame(objdata:TObjData; reg:tregister;ofs:dword);
    procedure stack_alloc(objdata:TObjData;ofs:dword);
  end;


implementation

uses
  cutils,verbose,cpubase;

const
  UWOP_PUSH_NONVOL     = 0;  { info = register number }
  UWOP_ALLOC_LARGE     = 1;  { no info, alloc size in next 2 slots }
  UWOP_ALLOC_SMALL     = 2;  { info = size of allocation / 8 - 1 }
  UWOP_SET_FPREG       = 3;  { no info, FP = RSP + UNWIND_INFO.FPRegOffset*16 }
  UWOP_SAVE_NONVOL     = 4;  { info = register number, offset in next slot }
  UWOP_SAVE_NONVOL_FAR = 5;  { info = register number, offset in next 2 slots }
  UWOP_SAVE_XMM        = 6;
  UWOP_SAVE_XMM_FAR    = 7;
  UWOP_SAVE_XMM128     = 8;  { info = XMM reg number, offset in next slot }
  UWOP_SAVE_XMM128_FAR = 9;  { info = XMM reg number, offset in next 2 slots }
  UWOP_PUSH_MACHFRAME  = 10; { info = 0: no error-code, 1: error-code }

  UNW_FLAG_EHANDLER    = $01; { exceptiion handler }
  UNW_FLAG_UHANDLER    = $02; { termination handler }
  UNW_FLAG_FHANDLER    = UNW_FLAG_EHANDLER or UNW_FLAG_UHANDLER;
  UNW_FLAG_CHAININFO   = $04; { mutually exclusive with the above }


type
  tai_seh_directive_x64=class(tai_seh_directive)
    procedure generate_code(objdata:TObjData);override;
  end;

  TPrologueElement=class(TLinkedListItem)
  public
    opcode: Integer;  { =(info shl 4) or code }
    ofs: dword;
    addr: aword;
  end;

var
  current_unw: TWin64Cfi;

{ This generally duplicates private aasmcpu.regval() }
function EncodeReg(r: TRegister): integer;
begin
  case r of
    NR_RAX: result:=0;
    NR_RCX: result:=1;
    NR_RDX: result:=2;
    NR_RBX: result:=3;
    NR_RSP: result:=4;
    NR_RBP: result:=5;
    NR_RSI: result:=6;
    NR_RDI: result:=7;
    NR_R8:  result:=8;
    NR_R9:  result:=9;
    NR_R10: result:=10;
    NR_R11: result:=11;
    NR_R12: result:=12;
    NR_R13: result:=13;
    NR_R14: result:=14;
    NR_R15: result:=15;
  else
    InternalError(2011072305);
  end;
end;

function EncodeXMM(r: TRegister): integer;
begin
  if getregtype(r)=R_MMREGISTER then
    result:=getsupreg(r)
  else
    InternalError(2011072305);
end;


{ TWin64CFI }

constructor TWin64CFI.create;
begin
  inherited create;
  FElements:=TLinkedList.Create;
end;

destructor TWin64CFI.destroy;
begin
  FElements.Free;
  stringdispose(FName);
  inherited destroy;
end;

procedure TWin64CFI.AddElement(objdata:TObjData;aCode,aInfo:Integer;aOffs:dword);
var
  el:TPrologueElement;
begin
  el:=TPrologueElement.Create;
  FElements.concat(el);
  el.opcode:=(aInfo shl 4) or aCode;
  el.ofs:=aOffs;
  el.addr:=objdata.CurrObjSec.Size;

  { a single element may occupy 1,2 or 3 word-sized slots }
  case aCode of
    UWOP_ALLOC_LARGE:
      Inc(FCount,2+ord(aInfo<>0));

    UWOP_SAVE_NONVOL_FAR,
    UWOP_SAVE_XMM128_FAR:
      Inc(FCount,3);

    UWOP_SAVE_NONVOL,
    UWOP_SAVE_XMM128:
      Inc(FCount,2);

  else
    inc(FCount);
  end;
end;

procedure TWin64CFI.generate_code(objdata:TObjData);
var
  hp: TPrologueElement;
  uwcode: array [0..1] of byte;
  uwdata: array [0..3] of byte;
  cursec: TObjSection;
  xdatasym,pdatasym: TObjSymbol;
  zero: word;
begin
{  if FCount=0 then
     InternalError(2011072308); }
  if FCount>255 then
    InternalError(2011072301);

  cursec:=objdata.CurrObjSec;
  objdata.createsection('.xdata.n_'+lower(FName^),4,[oso_data,oso_load]);
  xdatasym:=objdata.symboldefine('$unwind$'+FName^,AB_GLOBAL,AT_DATA);
  uwdata[0]:=(FFlags shl 3) or 1;
  uwdata[1]:=FPrologueEndPos-FFrameStartSym.offset;
  uwdata[2]:=FCount;
  { Offset is multiple of 16, so it is already shifted into correct position }
  uwdata[3]:=FFrameOffs or FFrameReg;
  objdata.writebytes(uwdata,4);

  { write elements in reverse order (offset descending) }
  hp:=TPrologueElement(FElements.Last);
  while Assigned(hp) do
    begin
      uwcode[0]:=hp.addr-FFrameStartSym.address;
      uwcode[1]:=hp.opcode;
      objdata.writebytes(uwcode,2);
      case hp.opcode and $0F of
    UWOP_PUSH_NONVOL,
    UWOP_ALLOC_SMALL,
    UWOP_SET_FPREG,
    UWOP_PUSH_MACHFRAME: ;  { These have no extra data }

    UWOP_ALLOC_LARGE:
      if (hp.opcode and $F0)<>0 then
            objdata.writebytes(hp.ofs,4)
          else
            objdata.writebytes(hp.ofs,2);

    UWOP_SAVE_NONVOL_FAR,
    UWOP_SAVE_XMM128_FAR:
      objdata.writebytes(hp.ofs,4);

    UWOP_SAVE_NONVOL,
    UWOP_SAVE_XMM128:
          objdata.writebytes(hp.ofs,2);
      else
        InternalError(2011072302);
      end;

      hp:=TPrologueElement(hp.Previous);
    end;
  { pad with zeros to dword boundary }
  zero:=0;
  if odd(FCount) then
    objdata.writebytes(zero,2);
  { TODO: handler rva and data should follow if flags specify a handler }

  FElements.Clear;

  objdata.createsection(sec_pdata,FName^);
  pdatasym:=objdata.symboldefine('$pdata$'+FName^,AB_LOCAL,AT_DATA);
  objdata.writereloc(0,4,FFrameStartSym,RELOC_RVA);
  objdata.writereloc(FFrameStartSym.Size,4,FFrameStartSym,RELOC_RVA);
  objdata.writereloc(0,4,xdatasym,RELOC_RVA);
  { restore previous state }
  objdata.SetSection(cursec);
  { create a dummy relocation, so pdata is not smartlinked away }
  objdata.writereloc(0,0,pdatasym,RELOC_NONE);
end;

procedure TWin64CFI.start_frame(objdata:TObjData;const name:string);
begin
  if assigned(FName) then
    internalerror(2011072306);
  FName:=stringdup(name);
  FFrameStartSym:=objdata.symbolref(name);
  FCount:=0;
  FFrameReg:=0;
  FFrameOffs:=0;
  FPrologueEndPos:=0;
end;

procedure TWin64CFI.end_frame(objdata:TObjData);
begin
  if not assigned(FName) then
    internalerror(2011072307);
  generate_code(objdata);
  FFrameStartSym:=nil;
  stringdispose(FName);
end;

procedure TWin64CFI.end_prologue(objdata:TObjData);
begin
  FPrologueEndPos:=objdata.CurrObjSec.Size;
end;

procedure TWin64CFI.push_reg(objdata:TObjData;reg:tregister);
begin
  AddElement(objdata,UWOP_PUSH_NONVOL,EncodeReg(reg),0);
end;

procedure TWin64CFI.save_reg(objdata:TObjData;reg:tregister;ofs:dword);
var
  info: Integer;
begin
  info:=EncodeReg(reg);
  if ((ofs and 7) = 0) and (ofs<=$ffff*8) then
    AddElement(objdata,UWOP_SAVE_NONVOL,info,ofs shr 3)
  else
    AddElement(objdata,UWOP_SAVE_NONVOL_FAR,info,ofs);
end;

procedure TWin64CFI.save_xmm(objdata:TObjData;reg:tregister;ofs:dword);
var
  info: Integer;
begin
  info:=EncodeXMM(reg);
  if ((ofs and 15)=0) and (ofs<=$ffff*16) then
    AddElement(objdata,UWOP_SAVE_XMM128, info, ofs shr 4)
  else
    AddElement(objdata,UWOP_SAVE_XMM128_FAR, info, ofs);
end;

procedure TWin64CFI.set_frame(objdata:TObjData;reg:tregister;ofs:dword);
var
  info: Integer;
begin
  info:=EncodeReg(reg);
  if FFrameReg<>0 then
    InternalError(2011072303);
  if info=0 then                 { frame register cannot be RAX }
    InternalError(2011072304);
  if (ofs>240) or ((ofs and 15)<>0) then
    InternalError(2011072310);
  FFrameReg:=info;
  FFrameOffs:=ofs;
  { !! looks like docs aren't correct and info should be set to register }
  AddElement(objdata,UWOP_SET_FPREG,0,0);
end;

procedure TWin64CFI.stack_alloc(objdata:TObjData;ofs:dword);
begin
  if ((ofs and 7)=0) and (ofs<=128) then
    AddElement(objdata,UWOP_ALLOC_SMALL,(ofs-8) shr 3,0)
  else if ((ofs and 7) = 0) and (ofs<=$ffff * 8) then
    AddElement(objdata,UWOP_ALLOC_LARGE,0,ofs shr 3)
  else
    AddElement(objdata,UWOP_ALLOC_LARGE,1,ofs);
end;

procedure tai_seh_directive_x64.generate_code(objdata:TObjData);
begin
  case kind of
    ash_proc:
      current_unw.start_frame(objdata,data.name^);
    ash_endproc:
      current_unw.end_frame(objdata);
    ash_endprologue:
      current_unw.end_prologue(objdata);
    ash_handler:     {TBD};
    ash_handlerdata: {TBD};
    ash_eh,ash_32,ash_no32: ; { there are not for x86_64 }
    ash_setframe:
      current_unw.set_frame(objdata,data.reg,data.offset);
    ash_stackalloc:
      current_unw.stack_alloc(objdata,data.offset);
    ash_pushreg:
      current_unw.push_reg(objdata,data.reg);
    ash_savereg:
      current_unw.save_reg(objdata,data.reg,data.offset);
    ash_savexmm:
      current_unw.save_xmm(objdata,data.reg,data.offset);
    ash_pushframe: {TBD};
  end;
end;


initialization
  cai_seh_directive:=tai_seh_directive_x64;
  current_unw:=TWin64CFI.Create;
finalization
  current_unw.Free;
end.

