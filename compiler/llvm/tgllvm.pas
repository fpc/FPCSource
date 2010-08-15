{
    Copyright (c) 1998-2002 by Florian Klaempfl

    This unit implements the LLVM-specific temp. generator

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

unit tgllvm;

{$i fpcdefs.inc}

  interface

    uses
      cclasses,
      globals,globtype,
      symtype,
      cpubase,cpuinfo,cgbase,cgutils,
      aasmbase,aasmtai,aasmdata,
      tgobj;

    type

      { LLVM temp manager: in LLVM, you allocate every temp separately using
        the "alloca" instrinsic. Every such temp is a separate stack slot, but
        can be turned into a regvar (or be decomposed) by LLVM. To avoid
        problems with turning stack slots into regvars, we don't allocate one
        big blob of memory that we manage ourselves using the regular temp
        manager. On the other hand, to avoid using a needlessly large amount of
        stack space we still try to reuse stack slots.

        We basically let the original temp manager handle the temp allocation,
        with the following modifications:
          * don't use partial temp locations, and don't merge temp locations
            (suballoc_and_merging:=false)
          * since there is no frame pointer at the llvm level, convert the
            temp offsets into local symbols
          * we keep track of the offset by making it part of the symbol name
      }

      { ttgllvm }

      ttgllvm = class(ttgobj)
       protected
        function temppostoref(pos, alignment: longint): treference; override;
        function reftotemppos(const ref: treference): longint; override;
        function internalistemp(const ref: treference): boolean; override;
       public
        constructor create;
      end;

implementation

    uses
       cutils,
       systems,verbose,
       procinfo,
       symconst
       ;


    { ttgllvm }

    function ttgllvm.temppostoref(pos, alignment: longint): treference;
      begin
        reference_reset_symbol(result,
          current_asmdata.DefineAsmSymbol('$llvmtemp'+tostr(pos),AB_LOCAL,AT_TEMP),
          0,alignment);
      end;


    function ttgllvm.reftotemppos(const ref: treference): longint;
      var
        error: longint;
      begin
        if assigned(ref.symbol) and
           (ref.symbol.typ=AT_TEMP) and
           (ref.base=NR_NO) and
           (ref.index=NR_NO) and
           (ref.offset=0) then
          begin
            val(copy(ref.symbol.Name,length('$llvmtemp')+1,high(ref.symbol.Name)),result,error);
            if error<>0 then
              internalerror(2010081501);
          end
        else
          result:=high(longint);
      end;


    function ttgllvm.internalistemp(const ref: treference): boolean;
      begin
        Result:=assigned(ref.symbol) and
          (ref.symbol.typ=AT_TEMP) and
          (ref.base=NR_NO) and
          (ref.index=NR_NO) and
          (ref.offset=0);
      end;


    constructor ttgllvm.create;
      begin
        inherited create;
        suballoc_and_merging:=false;
        { always use positive offsets, because symbol names cannot contain "-"
          without being quoted }
        direction:=1;
      end;

end.
