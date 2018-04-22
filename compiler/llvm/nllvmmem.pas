{
    Copyright (c) 2012 by Jonas Maebe

    Generate LLVM byetcode for in memory related nodes

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
unit nllvmmem;

{$i fpcdefs.inc}

interface

    uses
      globtype,
      cgbase,cgutils,
      symtype,
      node,ncgnstmm, ncgmem;

    type
      tllvmloadparentfpnode = class(tcgnestloadparentfpnode)
      end;

      tllvmsubscriptnode = class(tcgsubscriptnode)
       protected
        function handle_platform_subscript: boolean; override;
      end;

      tllvmvecnode= class(tcgvecnode)
       private
        constarrayoffset: aint;
        arraytopointerconverted: boolean;
       public
        procedure pass_generate_code; override;
        procedure update_reference_reg_mul(maybe_const_reg: tregister; regsize: tdef; l: aint); override;
        procedure update_reference_reg_packed(maybe_const_reg: tregister; regsize: tdef; l: aint); override;
        procedure update_reference_offset(var ref: treference; index, mulsize: aint); override;
      end;


implementation

    uses
      verbose,cutils,
      aasmdata,aasmllvm,
      symtable,symconst,symdef,defutil,
      nmem,
      cpubase,llvmbase,hlcgobj,hlcgllvm;

  { tllvmsubscriptnode }

    function tllvmsubscriptnode.handle_platform_subscript: boolean;
      var
        newbase: tregister;
        fielddef: tdef;
      begin
        if not(location.loc in [LOC_REFERENCE,LOC_CREFERENCE]) then
          internalerror(2014011905);
        if is_packed_record_or_object(left.resultdef) then
          begin
            { typecast the result to the expected type, but don't actually index
              (that still has to be done by the generic code, so return false) }
            newbase:=hlcg.getaddressregister(current_asmdata.CurrAsmList,cpointerdef.getreusable(resultdef));
            if is_ordinal(resultdef) then
              fielddef:=
                cgsize_orddef(
                  int_cgsize(
                    packedbitsloadsize(resultdef.packedbitsize)
                  )
                )
            else
              fielddef:=resultdef;
            hlcg.a_loadaddr_ref_reg(current_asmdata.CurrAsmList,
              left.resultdef,
              cpointerdef.getreusable(fielddef),
              location.reference,newbase);
            reference_reset_base(location.reference,newbase,0,location.reference.temppos,location.reference.alignment,location.reference.volatility);
            result:=false;
          end
        else
          begin
            hlcg.g_set_addr_nonbitpacked_field_ref(current_asmdata.CurrAsmList,tabstractrecorddef(left.resultdef),vs,location.reference);
            location.size:=def_cgsize(resultdef);
            result:=true;
          end;
      end;

  { tllvmvecnode }

  procedure tllvmvecnode.pass_generate_code;
    var
      locref: preference;
      hreg: tregister;
      arrptrelementdef: tdef;
      packedloadsize: aint;
      indirect: boolean;

    procedure getarrelementptrdef;
      begin
        if assigned(locref) then
          exit;
        case location.loc of
          LOC_SUBSETREF,LOC_CSUBSETREF:
            locref:=@location.reference;
          LOC_REFERENCE,LOC_CREFERENCE:
            locref:=@location.sref.ref;
          else
            internalerror(2013111001);
        end;
        { special handling for s80real: inside aggregates (such as arrays) it's
          declared as an array of 10 bytes in order to force the allocation of
          the right size (llvm only supports s80real according to the ABI size/
          alignment) -> convert the pointer to this array into a pointer to the
          s80real type (loads from and stores to this type will always only store
          10 bytes) }
        if (resultdef.typ=floatdef) and
           (tfloatdef(resultdef).floattype=s80real) then
          arrptrelementdef:=cpointerdef.getreusable(carraydef.getreusable(u8inttype,10))
        else
          arrptrelementdef:=cpointerdef.getreusable(resultdef);
      end;

    begin
      inherited;
      locref:=nil;
      { avoid uninitialised warning }
      arrptrelementdef:=nil;
      indirect:=
        not is_dynamicstring(left.resultdef) and
        not is_dynamic_array(left.resultdef);
      if (not arraytopointerconverted and
          indirect) or
         (constarrayoffset<>0) then
        begin
          { the result is currently a pointer to left.resultdef (the array type)
             -> convert it into a pointer to an element inside this array }
          getarrelementptrdef;
          hreg:=hlcg.getaddressregister(current_asmdata.CurrAsmList,arrptrelementdef);
          locref^:=thlcgllvm(hlcg).make_simple_ref(current_asmdata.CurrAsmList,location.reference,left.resultdef);
          if indirect then
            current_asmdata.CurrAsmList.Concat(taillvm.getelementptr_reg_size_ref_size_const(hreg,cpointerdef.getreusable(left.resultdef),
              locref^,ptruinttype,constarrayoffset,true))
          else
            current_asmdata.CurrAsmList.Concat(taillvm.getelementptr_reg_size_ref_size_const(hreg,left.resultdef,
              locref^,ptruinttype,constarrayoffset,false));
          reference_reset_base(locref^,hreg,0,locref^.temppos,locref^.alignment,locref^.volatility);
        end;

      { see comment in getarrelementptrdef }
      if (resultdef.typ=floatdef) and
         (tfloatdef(resultdef).floattype=s80real) then
       begin
         if not assigned(locref) then
           getarrelementptrdef;
         hreg:=hlcg.getaddressregister(current_asmdata.CurrAsmList,cpointerdef.getreusable(resultdef));
         hlcg.a_load_reg_reg(current_asmdata.CurrAsmList,arrptrelementdef,cpointerdef.getreusable(resultdef),locref^.base,hreg);
         locref^.base:=hreg;
       end;

      { packed arrays are represented by an array of byte, but when we operate
        on them we treat them as arrays of elements of packedbitsloadsize()
        -> typecast }
      if is_packed_array(left.resultdef) and
         (tarraydef(left.resultdef).elementdef.typ in [enumdef,orddef]) then
        begin
          getarrelementptrdef;
          packedloadsize:=packedbitsloadsize(tarraydef(left.resultdef).elementdef.packedbitsize);
          arrptrelementdef:=cpointerdef.getreusable(cgsize_orddef(int_cgsize(packedloadsize)));
          hlcg.g_ptrtypecast_ref(current_asmdata.CurrAsmList,
            cpointerdef.getreusable(u8inttype),
            arrptrelementdef,
            locref^);
        end;
    end;


  procedure tllvmvecnode.update_reference_reg_mul(maybe_const_reg: tregister; regsize: tdef; l: aint);
    var
      hreg: tregister;
    begin
      if l<>resultdef.size then
        internalerror(2013102602);
      if constarrayoffset<>0 then
        begin
          hreg:=hlcg.getintregister(current_asmdata.CurrAsmList,ptruinttype);
          hlcg.a_op_const_reg_reg(current_asmdata.CurrAsmList,OP_ADD,ptruinttype,constarrayoffset,maybe_const_reg,hreg);
          maybe_const_reg:=hreg;
          constarrayoffset:=0;
        end;
      hreg:=hlcg.getaddressregister(current_asmdata.CurrAsmList,cpointerdef.getreusable(resultdef));
      location.reference:=thlcgllvm(hlcg).make_simple_ref(current_asmdata.CurrAsmList,location.reference,left.resultdef);
      if not is_dynamicstring(left.resultdef) and
         not is_dynamic_array(left.resultdef) then
        begin
          { get address of indexed array element and convert pointer to array into
            pointer to the elementdef in the process }
          current_asmdata.CurrAsmList.Concat(taillvm.getelementptr_reg_size_ref_size_reg(hreg,cpointerdef.getreusable(left.resultdef),
            location.reference,ptruinttype,maybe_const_reg,true));
          arraytopointerconverted:=true;
        end
      else
        { the array is already a pointer -> just index }
        current_asmdata.CurrAsmList.Concat(taillvm.getelementptr_reg_size_ref_size_reg(hreg,left.resultdef,
          location.reference,ptruinttype,maybe_const_reg,false));
      reference_reset_base(location.reference,hreg,0,location.reference.temppos,location.reference.alignment,location.reference.volatility);
      location.reference.alignment:=newalignment(location.reference.alignment,l);
    end;


  procedure tllvmvecnode.update_reference_reg_packed(maybe_const_reg: tregister; regsize: tdef; l: aint);
    var
      sref: tsubsetreference;
      offsetreg, basereg, hreg, hreg2: tregister;
      alignpower: aint;
      temp, intloadsize : longint;
      defloadsize: tdef;
    begin
      { only orddefs are bitpacked. Even then we only need special code in }
      { case the bitpacked *byte size* is not a power of two, otherwise    }
      { everything can be handled using the the regular array code.        }
      if ((l mod 8) = 0) and
         (ispowerof2(l div 8,temp) or
          not is_ordinal(resultdef)
{$ifndef cpu64bitalu}
          or is_64bitint(resultdef)
{$endif not cpu64bitalu}
          ) then
        begin
          update_reference_reg_mul(maybe_const_reg,regsize,l div 8);
          exit;
        end;
      if (l>8*sizeof(aint)) then
        internalerror(200608051);

      { adjust the index by subtracting the lower bound of the array and adding
        any constant adjustments }
      sref.ref:=location.reference;
      hreg:=hlcg.getintregister(current_asmdata.CurrAsmList,ptruinttype);
      hlcg.a_op_const_reg_reg(current_asmdata.CurrAsmList,OP_SUB,ptruinttype,tarraydef(left.resultdef).lowrange-constarrayoffset,maybe_const_reg,hreg);
      constarrayoffset:=0;

      { multiply index with bitsize of every element }
      hreg2:=hlcg.getintregister(current_asmdata.CurrAsmList,ptruinttype);
      hlcg.a_op_const_reg_reg(current_asmdata.CurrAsmList,OP_MUL,ptruinttype,l,hreg,hreg2);
      hreg:=hreg2;

      { keep alignment for index }
      sref.ref.alignment:=left.resultdef.alignment;
      intloadsize:=packedbitsloadsize(l);
      if not ispowerof2(intloadsize,temp) then
        internalerror(2006081201);
      defloadsize:=cgsize_orddef(int_cgsize(intloadsize));
      alignpower:=temp;
      { determine start of the 8/16/32/64 bits chunk that contains the wanted
        value: divide the index by 8 (we're working with a bitpacked array here,
        all quantities are expressed in bits), and then by the size of the
        chunks (alignpower) }
      hreg2:=hlcg.getintregister(current_asmdata.CurrAsmList,ptruinttype);
      hlcg.a_op_const_reg_reg(current_asmdata.CurrAsmList,OP_SHR,ptruinttype,3+alignpower,hreg,hreg2);
      offsetreg:=hlcg.getintregister(current_asmdata.CurrAsmList,ptruinttype);
      hlcg.a_op_const_reg_reg(current_asmdata.CurrAsmList,OP_SHL,ptruinttype,alignpower,hreg2,offsetreg);
      { index the array using this chunk index }
      basereg:=hlcg.getaddressregister(current_asmdata.CurrAsmList,cpointerdef.getreusable(defloadsize));
      current_asmdata.CurrAsmList.Concat(taillvm.getelementptr_reg_size_ref_size_reg(basereg,cpointerdef.getreusable(left.resultdef),
        sref.ref,ptruinttype,offsetreg,true));
      arraytopointerconverted:=true;
      reference_reset_base(sref.ref,basereg,0,sref.ref.temppos,sref.ref.alignment,sref.ref.volatility);
      { calculate the bit index inside that chunk: mask out
        the chunk index part }
      hreg2:=hlcg.getintregister(current_asmdata.CurrAsmList,ptruinttype);
      hlcg.a_op_const_reg_reg(current_asmdata.CurrAsmList,OP_AND,ptruinttype,(1 shl (3+alignpower))-1,hreg,hreg2);
      sref.bitindexreg:=hreg2;
      sref.startbit:=0;
      sref.bitlen:=resultdef.packedbitsize;
      if (left.location.loc=LOC_REFERENCE) then
        location.loc:=LOC_SUBSETREF
      else
        location.loc:=LOC_CSUBSETREF;
      location.sref:=sref;
    end;


  procedure tllvmvecnode.update_reference_offset(var ref: treference; index, mulsize: aint);
    begin
      if not is_packed_array(left.resultdef) or
         not (tarraydef(left.resultdef).elementdef.typ in [enumdef,orddef]) then
        inc(constarrayoffset,index)
      else
        inc(constarrayoffset,index*mulsize)
    end;


begin
  cloadparentfpnode:=tllvmloadparentfpnode;
  csubscriptnode:=tllvmsubscriptnode;
  cvecnode:=tllvmvecnode;
end.

