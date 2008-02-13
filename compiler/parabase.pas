{
    Copyright (c) 2002 by Florian Klaempfl

    Generic calling convention handling

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
unit parabase;

{$i fpcdefs.inc}

  interface

    uses
       cclasses,globtype,
       cpubase,cgbase,cgutils;

    type
       TCGParaReference = record
          index       : tregister;
          offset      : aint;
       end;

       PCGParaLocation = ^TCGParaLocation;
       TCGParaLocation = record
         Next : PCGParaLocation;
         Size : TCGSize; { size of this location }
         Loc  : TCGLoc;
         case TCGLoc of
           LOC_REFERENCE : (reference : TCGParaReference);
           LOC_FPUREGISTER,
           LOC_CFPUREGISTER,
           LOC_MMREGISTER,
           LOC_CMMREGISTER,
           LOC_REGISTER,
           LOC_CREGISTER : (
             { The number of bits the value in the register must be shifted to the left before
             it can be stored to memory in the function prolog.
             This is used for passing OS_NO memory blocks less than register size and of "odd"
             (3, 5, 6, 7) size on big endian machines, so that small memory blocks passed via
             registers are properly aligned.

             E.g. the value $5544433 is passed in bits 40-63 of the register (others are zero),
             but they should actually be stored in the first bits of the stack location reserved
             for this value. So they have to be shifted left by this amount of bits before. }
             {$IFDEF POWERPC64}shiftval : byte;{$ENDIF POWERPC64}
             register : tregister);
       end;

       TCGPara = object
          Location  : PCGParalocation;
          Alignment : ShortInt;
          Size      : TCGSize;  { Size of the parameter included in all locations }
          IntSize: aint; { size of the total location in bytes }
{$ifdef powerpc}
          composite: boolean; { under the AIX abi, how certain parameters are passed depends on whether they are composite or not }
{$endif powerpc}
          constructor init;
          destructor  done;
          procedure   reset;
          function    getcopy:tcgpara;
          procedure   check_simple_location;
          function    add_location:pcgparalocation;
          procedure   get_location(var newloc:tlocation);
       end;

       tvarargsinfo = (
         va_uses_float_reg
       );

       tparalist = class(TFPObjectList)
          procedure SortParas;
       end;

       tvarargsparalist = class(tparalist)
          varargsinfo : set of tvarargsinfo;
{$ifdef x86_64}
          { x86_64 requires %al to contain the no. SSE regs passed }
          mmregsused  : longint;
{$endif x86_64}
       end;



implementation

    uses
      systems,verbose,
      symsym;


{****************************************************************************
                                TCGPara
****************************************************************************}

    constructor tcgpara.init;
      begin
        alignment:=0;
        size:=OS_NO;
        intsize:=0;
        location:=nil;
{$ifdef powerpc}
        composite:=false;
{$endif powerpc}
      end;


    destructor tcgpara.done;
      begin
        reset;
      end;


    procedure tcgpara.reset;
      var
        hlocation : pcgparalocation;
      begin
        while assigned(location) do
          begin
            hlocation:=location^.next;
            dispose(location);
            location:=hlocation;
          end;
        alignment:=0;
        size:=OS_NO;
        intsize:=0;
{$ifdef powerpc}
        composite:=false;
{$endif powerpc}
      end;


    function tcgpara.getcopy:tcgpara;
      var
        hlocation : pcgparalocation;
      begin
        result.init;
        while assigned(location) do
          begin
            hlocation:=result.add_location;
            hlocation^:=location^;
            hlocation^.next:=nil;
            location:=location^.next;
          end;
        result.alignment:=alignment;
        result.size:=size;
        result.intsize:=intsize;
{$ifdef powerpc}
        result.composite:=composite;
{$endif powerpc}
      end;


    function tcgpara.add_location:pcgparalocation;
      var
        prevlocation,
        hlocation : pcgparalocation;
      begin
        prevlocation:=nil;
        hlocation:=location;
        while assigned(hlocation) do
          begin
            prevlocation:=hlocation;
            hlocation:=hlocation^.next;
          end;
        new(hlocation);
        Fillchar(hlocation^,sizeof(tcgparalocation),0);
        if assigned(prevlocation) then
          prevlocation^.next:=hlocation
        else
          location:=hlocation;
        result:=hlocation;
      end;


    procedure tcgpara.check_simple_location;
      begin
        if not assigned(location) then
          internalerror(200408161);
        if assigned(location^.next) then
          internalerror(200408162);
      end;


    procedure tcgpara.get_location(var newloc:tlocation);
      begin
        if not assigned(location) then
          internalerror(200408205);
        fillchar(newloc,sizeof(newloc),0);
        newloc.loc:=location^.loc;
        newloc.size:=size;
        case location^.loc of
          LOC_REGISTER :
            begin
{$ifndef cpu64bitalu}
              if size in [OS_64,OS_S64] then
                begin
                  if not assigned(location^.next) then
                    internalerror(200408206);
                  if (location^.next^.loc<>LOC_REGISTER) then
                    internalerror(200408207);
                  if (target_info.endian = ENDIAN_BIG) then
                    begin
                      newloc.register64.reghi:=location^.register;
                      newloc.register64.reglo:=location^.next^.register;
                    end
                  else
                    begin
                      newloc.register64.reglo:=location^.register;
                      newloc.register64.reghi:=location^.next^.register;
                    end;
                end
              else
{$endif}
                newloc.register:=location^.register;
            end;
          LOC_FPUREGISTER,
          LOC_MMREGISTER :
            newloc.register:=location^.register;
          LOC_REFERENCE :
            begin
              newloc.reference.base:=location^.reference.index;
              newloc.reference.offset:=location^.reference.offset;
            end;
        end;
      end;


{****************************************************************************
                          TParaList
****************************************************************************}

    function ParaNrCompare(Item1, Item2: Pointer): Integer;
      var
        I1 : tparavarsym absolute Item1;
        I2 : tparavarsym absolute Item2;
      begin
        Result:=longint(I1.paranr)-longint(I2.paranr);
      end;


    procedure TParaList.SortParas;
      begin
        Sort(@ParaNrCompare);
      end;


end.
