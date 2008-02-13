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

    type
      { reference record, reordered for best alignment }
      preference = ^treference;
      treference = record
         offset      : aint;
         symbol,
         relsymbol   : tasmsymbol;
         segment,
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
{$ifdef SUPPORT_UNALIGNED}
         alignment : byte;
{$endif SUPPORT_UNALIGNED}
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
            LOC_FLAGS : (resflags : tresflags);
            LOC_CONSTANT : (
              case longint of
{$ifdef FPC_BIG_ENDIAN}
                1 : (_valuedummy,value : aint);
{$else FPC_BIG_ENDIAN}
                1 : (value : aint);
{$endif FPC_BIG_ENDIAN}
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
{$ifndef cpu64bitalu}
                { overlay a 64 Bit register type }
                2 : (register64 : tregister64);
{$endif cpu64bitalu}
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
    procedure reference_reset(var ref : treference);
    {# Clear to zero a treference, and set is base address
       to base register.
    }
    procedure reference_reset_base(var ref : treference;base : tregister;offset : longint);
    procedure reference_reset_symbol(var ref : treference;sym : tasmsymbol;offset : longint);
    { This routine verifies if two references are the same, and
       if so, returns TRUE, otherwise returns false.
    }
    function references_equal(const sref,dref : treference) : boolean;

    { tlocation handling }

    procedure location_reset(var l : tlocation;lt:TCGLoc;lsize:TCGSize);
    procedure location_copy(var destloc:tlocation; const sourceloc : tlocation);
    procedure location_swap(var destloc,sourceloc : tlocation);


    { allocate room for parameters on the stack in the entry code? }
    function use_fixed_stack: boolean;

    { returns r with the given alignment }
    function setalignment(const r : treference;b : byte) : treference;


implementation

uses
  systems;

{****************************************************************************
                                  TReference
****************************************************************************}

    procedure reference_reset(var ref : treference);
      begin
        FillChar(ref,sizeof(treference),0);
{$ifdef arm}
        ref.signindex:=1;
{$endif arm}
      end;


    procedure reference_reset_base(var ref : treference;base : tregister;offset : longint);
      begin
        reference_reset(ref);
        ref.base:=base;
        ref.offset:=offset;
      end;


    procedure reference_reset_symbol(var ref : treference;sym : tasmsymbol;offset : longint);
      begin
        reference_reset(ref);
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

    procedure location_reset(var l : tlocation;lt:TCGLoc;lsize:TCGSize);
      begin
        FillChar(l,sizeof(tlocation),0);
        l.loc:=lt;
        l.size:=lsize;
{$ifdef arm}
        if l.loc in [LOC_REFERENCE,LOC_CREFERENCE] then
          l.reference.signindex:=1;
{$endif arm}
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


    function use_fixed_stack: boolean;
      begin
{$ifdef i386}
        result := (target_info.system in [system_i386_darwin,system_x86_64_darwin]);
{$else i386}
{$ifdef cputargethasfixedstack}
        result := true;
{$else cputargethasfixedstack}
        result := false;
{$endif cputargethasfixedstack}
{$endif i386}
      end;

end.

