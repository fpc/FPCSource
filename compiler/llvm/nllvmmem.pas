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
        parentdef,
        subscriptdef,
        currentstructdef,
        llvmfielddef: tdef;
        newbase: tregister;
        implicitpointer: boolean;
      begin
        if not(location.loc in [LOC_REFERENCE,LOC_CREFERENCE]) then
          internalerror(2014011905);
        if is_packed_record_or_object(left.resultdef) then
          begin
            { typecast the result to the expected type, but don't actually index
              (that still has to be done by the generic code, so return false) }
            newbase:=hlcg.getaddressregister(current_asmdata.CurrAsmList,cpointerdef.getreusable(resultdef));
            hlcg.a_loadaddr_ref_reg(current_asmdata.CurrAsmList,left.resultdef,cpointerdef.getreusable(resultdef),location.reference,newbase);
            reference_reset_base(location.reference,newbase,0,location.reference.alignment);
            result:=false;
          end
        else
          begin
            implicitpointer:=is_implicit_pointer_object_type(left.resultdef);
            currentstructdef:=left.resultdef;
            { in case the field is part of a parent of the current object,
              index into the parents until we're at the parent containing the
              field; if it's an implicit pointer type, these embedded parents
              will be of the structure type of the class rather than of the
              class time itself -> one indirection fewer }
            while vs.owner<>tabstractrecorddef(currentstructdef).symtable do
              begin
                { only objectdefs have parents and hence the owner of the
                  fieldvarsym can be different from the current def's owner }
                parentdef:=tobjectdef(currentstructdef).childof;
                if implicitpointer then
                  newbase:=hlcg.getaddressregister(current_asmdata.CurrAsmList,parentdef)
                else
                  newbase:=hlcg.getaddressregister(current_asmdata.CurrAsmList,cpointerdef.getreusable(parentdef));
                location.reference:=thlcgllvm(hlcg).make_simple_ref(current_asmdata.CurrAsmList,location.reference,left.resultdef);
                if implicitpointer then
                  subscriptdef:=currentstructdef
                else
                  subscriptdef:=cpointerdef.getreusable(currentstructdef);
                { recurse into the first field }
                current_asmdata.CurrAsmList.concat(taillvm.getelementptr_reg_size_ref_size_const(newbase,subscriptdef,location.reference,s32inttype,0,true));
                reference_reset_base(location.reference,newbase,vs.offsetfromllvmfield,newalignment(location.reference.alignment,vs.fieldoffset));
                { go to the parent }
                currentstructdef:=parentdef;
              end;
            { get the type of the corresponding field in the llvm shadow
              definition }
            llvmfielddef:=tabstractrecordsymtable(tabstractrecorddef(currentstructdef).symtable).llvmst[vs].def;
            if implicitpointer then
              subscriptdef:=currentstructdef
            else
              subscriptdef:=cpointerdef.getreusable(currentstructdef);
            { load the address of that shadow field }
            newbase:=hlcg.getaddressregister(current_asmdata.CurrAsmList,cpointerdef.getreusable(llvmfielddef));
            location.reference:=thlcgllvm(hlcg).make_simple_ref(current_asmdata.CurrAsmList,location.reference,left.resultdef);
            current_asmdata.CurrAsmList.concat(taillvm.getelementptr_reg_size_ref_size_const(newbase,subscriptdef,location.reference,s32inttype,vs.llvmfieldnr,true));
            reference_reset_base(location.reference,newbase,vs.offsetfromllvmfield,newalignment(location.reference.alignment,vs.fieldoffset));
            { in case of an 80 bits extended type, typecast from an array of 10
              bytes (used because otherwise llvm will allocate the ABI-defined
              size for extended, which is usually larger) into an extended }
            if (llvmfielddef.typ=floatdef) and
               (tfloatdef(llvmfielddef).floattype=s80real) then
              hlcg.g_ptrtypecast_ref(current_asmdata.CurrAsmList,cpointerdef.getreusable(carraydef.getreusable(u8inttype,10)),cpointerdef.getreusable(s80floattype),location.reference);
            { if it doesn't match the requested field exactly (variant record),
              adjust the type of the pointer }
            if (vs.offsetfromllvmfield<>0) or
               (llvmfielddef<>resultdef) then
              hlcg.g_ptrtypecast_ref(current_asmdata.CurrAsmList,cpointerdef.getreusable(llvmfielddef),cpointerdef.getreusable(resultdef),location.reference);
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
      if not arraytopointerconverted and
         not is_dynamicstring(left.resultdef) and
         not is_dynamic_array(left.resultdef) then
        begin
          { the result is currently a pointer to left.resultdef (the array type)
             -> convert it into a pointer to an element inside this array }
          getarrelementptrdef;
          hreg:=hlcg.getaddressregister(current_asmdata.CurrAsmList,arrptrelementdef);
          locref^:=thlcgllvm(hlcg).make_simple_ref(current_asmdata.CurrAsmList,location.reference,left.resultdef);
          current_asmdata.CurrAsmList.Concat(taillvm.getelementptr_reg_size_ref_size_const(hreg,cpointerdef.getreusable(left.resultdef),
            locref^,ptruinttype,constarrayoffset,true));
          reference_reset_base(locref^,hreg,0,locref^.alignment);
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
      reference_reset_base(location.reference,hreg,0,location.reference.alignment);
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
      offsetreg:=hlcg.getintregister(current_asmdata.CurrAsmList,ptruinttype);
      hlcg.a_op_const_reg_reg(current_asmdata.CurrAsmList,OP_SHR,ptruinttype,3+alignpower,hreg,offsetreg);
      { index the array using this chunk index }
      basereg:=hlcg.getaddressregister(current_asmdata.CurrAsmList,cpointerdef.getreusable(defloadsize));
      current_asmdata.CurrAsmList.Concat(taillvm.getelementptr_reg_size_ref_size_reg(basereg,cpointerdef.getreusable(left.resultdef),
        sref.ref,ptruinttype,offsetreg,true));
      arraytopointerconverted:=true;
      reference_reset_base(sref.ref,basereg,0,sref.ref.alignment);
      { calculate the bit index inside that chunk }
      hreg2:=hlcg.getintregister(current_asmdata.CurrAsmList,ptruinttype);
      { multiple index with bitsize of every element }
      hlcg.a_op_const_reg_reg(current_asmdata.CurrAsmList,OP_MUL,ptruinttype,l,hreg,hreg2);
      hreg:=hlcg.getintregister(current_asmdata.CurrAsmList,ptruinttype);
      { mask out the chunk index part }
      hlcg.a_op_const_reg_reg(current_asmdata.CurrAsmList,OP_AND,ptruinttype,(1 shl (3+alignpower))-1,hreg2,hreg);
      sref.bitindexreg:=hreg;
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
      inc(constarrayoffset,index);
    end;


begin
  cloadparentfpnode:=tllvmloadparentfpnode;
  csubscriptnode:=tllvmsubscriptnode;
  cvecnode:=tllvmvecnode;
end.

