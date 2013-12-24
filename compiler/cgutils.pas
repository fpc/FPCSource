{
    Copyright (c) 1998-2004 by Florian Klaempfl

    Some basic types and constants for the code generation

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
{ This unit exports some helper routines which are used across the code generator }
unit cgutils;

{$i fpcdefs.inc}

  interface

    uses
      globtype,
      cclasses,
      aasmbase,
      cpubase,cgbase;

    const
      { implementation of max function using only functionality that can be
        evaluated as a constant expression by the compiler -- this is
        basically maxcpureg = max(max(first_int_imreg,first_fpu_imreg),first_mm_imreg)-1 }
      tmpmaxcpufpuintreg = first_int_imreg + ((first_fpu_imreg - first_int_imreg) * ord(first_int_imreg < first_fpu_imreg));
      maxcpuregister = (tmpmaxcpufpuintreg + ((first_mm_imreg - tmpmaxcpufpuintreg) * ord(tmpmaxcpufpuintreg < first_mm_imreg)))-1;

    type
      { Set type definition for cpuregisters }
      tcpuregisterset = set of 0..maxcpuregister;

{$ifdef jvm}
      tarrayreftype = (art_none,art_indexreg,art_indexref,art_indexconst);
{$endif jvm}
      { reference record, reordered for best alignment }
      preference = ^treference;
      treference = record
         offset      : asizeint;
         symbol,
         relsymbol   : tasmsymbol;
{$if defined(x86) or defined(m68k)}
         segment,
{$endif defined(x86) or defined(m68k)}
         base,
         index       : tregister;
         refaddr     : trefaddr;
         scalefactor : byte;
{$ifdef arm}
         symboldata  : tlinkedlistitem;
         signindex   : shortint;
         shiftimm    : byte;
         addressmode : taddressmode;
         shiftmode   : tshiftmode;
{$endif arm}
{$ifdef avr}
         addressmode : taddressmode;
{$endif avr}
{$ifdef m68k}
         { indexed increment and decrement mode }
         { (An)+ and -(An)                      }
         direction : tdirection;
{$endif m68k}
{$ifdef jvm}
         arrayreftype: tarrayreftype;
         indexbase: tregister;
         indexsymbol: tasmsymbol;
         indexoffset: aint;
         checkcast: boolean;
{$endif jvm}
         alignment : byte;
      end;

      tsubsetregister = record
        subsetreg : tregister;
        startbit, bitlen: byte;
        subsetregsize: tcgsize;
      end;

      tsubsetreference = record
        ref: treference;
        bitindexreg: tregister;
        startbit, bitlen: byte;
      end;

      tlocation = record
         loc  : TCGLoc;
         size : TCGSize;
         case TCGLoc of
{$ifdef cpuflags}
            LOC_FLAGS : (resflags : tresflags);
{$endif cpuflags}
            LOC_CONSTANT : (
              case longint of
{$ifdef cpu64bitalu}
                1 : (value : Int64);
{$else cpu64bitalu}
    {$ifdef FPC_BIG_ENDIAN}
                1 : (_valuedummy,value : longint);
    {$else FPC_BIG_ENDIAN}
                1 : (value : longint);
    {$endif FPC_BIG_ENDIAN}
{$endif cpu64bitalu}
                2 : (value64 : Int64);
              );
            LOC_CREFERENCE,
            LOC_REFERENCE : (reference : treference);
            { segment in reference at the same place as in loc_register }
            LOC_REGISTER,
            LOC_CREGISTER : (
              case longint of
                1 : (register : tregister;
                     { some x86_64 targets require two function result registers }
                     registerhi : tregister;
{$ifdef m68k}
                     { some m68k OSes require that the result is returned in d0 and a0
                       the second location must be stored here }
                     registeralias : tregister;
{$endif m68k}
                    );
{$ifdef cpu64bitalu}
                { overlay a 128 Bit register type }
                2 : (register128 : tregister128);
{$else cpu64bitalu}
                { overlay a 64 Bit register type }
                2 : (register64 : tregister64);
{$endif cpu64bitalu}
{$ifdef avr}
                3 : (registers : array[0..3] of tregister);
{$endif avr}
              );
            LOC_SUBSETREG,
            LOC_CSUBSETREG : (
              sreg: tsubsetregister;
            );
            LOC_SUBSETREF : (
              sref: tsubsetreference;
            )
      end;


    { trerefence handling }

    {# Clear to zero a treference }
    procedure reference_reset(var ref : treference; alignment: longint);
    {# Clear to zero a treference, and set is base address
       to base register.
    }
    procedure reference_reset_base(var ref : treference;base : tregister;offset, alignment : longint);
    procedure reference_reset_symbol(var ref : treference;sym : tasmsymbol;offset, alignment : longint);
    { This routine verifies if two references are the same, and
       if so, returns TRUE, otherwise returns false.
    }
    function references_equal(const sref,dref : treference) : boolean;

    { tlocation handling }

    { cannot be used for loc_(c)reference, because that one requires an alignment }
    procedure location_reset(var l : tlocation;lt:TCGNonRefLoc;lsize:TCGSize);
    { for loc_(c)reference }
    procedure location_reset_ref(var l : tlocation;lt:TCGRefLoc;lsize:TCGSize; alignment: longint);
    procedure location_copy(var destloc:tlocation; const sourceloc : tlocation);
    procedure location_swap(var destloc,sourceloc : tlocation);
    function location_reg2string(const locreg: tlocation): string;

    { returns r with the given alignment }
    function setalignment(const r : treference;b : byte) : treference;


implementation

uses
  systems,
  verbose;

{****************************************************************************
                                  TReference
****************************************************************************}

    procedure reference_reset(var ref : treference; alignment: longint);
      begin
        FillChar(ref,sizeof(treference),0);
{$ifdef arm}
        ref.signindex:=1;
{$endif arm}
        ref.alignment:=alignment;
      end;


    procedure reference_reset_base(var ref : treference;base : tregister;offset, alignment : longint);
      begin
        reference_reset(ref,alignment);
        ref.base:=base;
        ref.offset:=offset;
      end;


    procedure reference_reset_symbol(var ref : treference;sym : tasmsymbol;offset, alignment : longint);
      begin
        reference_reset(ref,alignment);
        ref.symbol:=sym;
        ref.offset:=offset;
      end;


    function references_equal(const sref,dref : treference):boolean;
      begin
        references_equal:=CompareByte(sref,dref,sizeof(treference))=0;
      end;


    { returns r with the given alignment }
    function setalignment(const r : treference;b : byte) : treference;
      begin
        result:=r;
        result.alignment:=b;
      end;

{****************************************************************************
                                  TLocation
****************************************************************************}

    procedure location_reset(var l : tlocation;lt:TCGNonRefLoc;lsize:TCGSize);
      begin
        FillChar(l,sizeof(tlocation),0);
        l.loc:=lt;
        l.size:=lsize;
        if l.loc in [LOC_REFERENCE,LOC_CREFERENCE] then
          { call location_reset_ref instead }
          internalerror(2009020705);
      end;

    procedure location_reset_ref(var l: tlocation; lt: tcgrefloc; lsize: tcgsize;
      alignment: longint);
    begin
      FillChar(l,sizeof(tlocation),0);
      l.loc:=lt;
      l.size:=lsize;
{$ifdef arm}
      l.reference.signindex:=1;
{$endif arm}
      l.reference.alignment:=alignment;
    end;


    procedure location_copy(var destloc:tlocation; const sourceloc : tlocation);
      begin
        destloc:=sourceloc;
      end;


    procedure location_swap(var destloc,sourceloc : tlocation);
      var
        swapl : tlocation;
      begin
        swapl := destloc;
        destloc := sourceloc;
        sourceloc := swapl;
      end;


    function location_reg2string(const locreg: tlocation): string;
      begin
        if not (locreg.loc in [LOC_REGISTER,LOC_CREGISTER,
            LOC_MMXREGISTER,LOC_CMMXREGISTER,
            LOC_MMREGISTER,LOC_CMMREGISTER,
            LOC_FPUREGISTER,LOC_CFPUREGISTER]) then
          internalerror(2013122301);

        if locreg.loc in [LOC_REGISTER,LOC_CREGISTER] then
          begin
            case locreg.size of
{$if defined(cpu64bitalu)}
              OS_128,OS_S128:
                result:=std_regname(locreg.registerhi)+':'+std_regname(locreg.register);
{$elseif defined(cpu32bitalu)}
              OS_64,OS_S64:
                result:=std_regname(locreg.registerhi)+':'+std_regname(locreg.register);
{$elseif defined(cpu16bitalu)}
              OS_64,OS_S64:
                if getsupreg(locreg.register)<first_int_imreg then
                  result:='??:'+std_regname(locreg.registerhi)+':??:'+std_regname(locreg.register)
                else
                  result:=std_regname(GetNextReg(locreg.registerhi))+':'+std_regname(locreg.registerhi)+':'+std_regname(GetNextReg(locreg.register))+':'+std_regname(locreg.register);
              OS_32,OS_S32:
                if getsupreg(locreg.register)<first_int_imreg then
                  result:='??:'+std_regname(locreg.register)
                else
                  result:=std_regname(GetNextReg(locreg.register))+':'+std_regname(locreg.register);
{$elseif defined(cpu8bitalu)}
              OS_64,OS_S64:
                if getsupreg(locreg.register)<first_int_imreg then
                  result:='??:??:??:'+std_regname(locreg.registerhi)+':??:??:??:'+std_regname(locreg.register)
                else
                  result:=std_regname(GetNextReg(GetNextReg(GetNextReg(locreg.registerhi))))+':'+std_regname(GetNextReg(GetNextReg(locreg.registerhi)))+':'+std_regname(GetNextReg(locreg.registerhi))+':'+std_regname(locreg.registerhi)+':'+std_regname(GetNextReg(GetNextReg(GetNextReg(locreg.register))))+':'+std_regname(GetNextReg(GetNextReg(locreg.register)))+':'+std_regname(GetNextReg(locreg.register))+':'+std_regname(locreg.register);
              OS_32,OS_S32:
                if getsupreg(locreg.register)<first_int_imreg then
                  result:='??:??:??:'+std_regname(locreg.register)
                else
                  result:=std_regname(GetNextReg(GetNextReg(GetNextReg(locreg.register))))+':'+std_regname(GetNextReg(GetNextReg(locreg.register)))+':'+std_regname(GetNextReg(locreg.register))+':'+std_regname(locreg.register);
              OS_16,OS_S16:
                if getsupreg(locreg.register)<first_int_imreg then
                  result:='??:'+std_regname(locreg.register)
                else
                  result:=std_regname(GetNextReg(locreg.register))+':'+std_regname(locreg.register);
{$endif}
              else
                result:=std_regname(locreg.register);
            end;
          end
        else
          begin
            if locreg.registerhi<>NR_NO then
              result:=std_regname(locreg.registerhi)+':'+std_regname(locreg.register)
            else
              result:=std_regname(locreg.register);
          end;
      end;


end.

