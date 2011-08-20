{
    Copyright (c) 2011 by Jonas Maebe

    Generate JVM byetcode for in memory related nodes

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
unit njvmmem;

{$i fpcdefs.inc}

interface

    uses
      globtype,
      cgbase,cpubase,
      node,nmem,ncgmem;

    type
       tjvmloadvmtaddrnode = class(tcgloadvmtaddrnode)
         procedure pass_generate_code; override;
       end;

       tjvmloadparentfpnode = class(tcgloadparentfpnode)
         procedure pass_generate_code;override;
       end;

       tjvmvecnode = class(tcgvecnode)
         function pass_1: tnode; override;
         procedure pass_generate_code;override;
       end;

implementation

    uses
      systems,
      cutils,verbose,constexp,
      symconst,symtype,symtable,symsym,symdef,defutil,
      nadd,ncal,ncnv,ncon,
      aasmdata,aasmcpu,pass_2,
      cgutils,hlcgobj,hlcgcpu;

{*****************************************************************************
                         TJVMLOADVMTADDRNODE
*****************************************************************************}

    procedure tjvmloadvmtaddrnode.pass_generate_code;
      begin
        current_asmdata.CurrAsmList.concat(taicpu.op_sym(a_ldc,current_asmdata.RefAsmSymbol(
          tobjectdef(tclassrefdef(resultdef).pointeddef).jvm_full_typename(true))));
        thlcgjvm(hlcg).incstack(current_asmdata.CurrAsmList,1);
        location_reset(location,LOC_REGISTER,OS_ADDR);
        location.register:=hlcg.getaddressregister(current_asmdata.CurrAsmList,resultdef);
        thlcgjvm(hlcg).a_load_stack_reg(current_asmdata.CurrAsmList,resultdef,location.register);
      end;


    { tjvmloadparentfpnode }

    procedure tjvmloadparentfpnode.pass_generate_code;
      begin
{$ifndef nounsupported}
        location_reset(location,LOC_REGISTER,OS_ADDR);
        location.register:=hlcg.getaddressregister(current_asmdata.CurrAsmList,java_jlobject);
        hlcg.a_load_const_reg(current_asmdata.CurrAsmList,java_jlobject,0,location.register);
{$else}
       internalerror(2011041301);
{$endif}
      end;

{*****************************************************************************
                             TJVMVECNODE
*****************************************************************************}

    function tjvmvecnode.pass_1: tnode;
      var
        psym: tsym;
      begin
        if is_wide_or_unicode_string(left.resultdef) then
          begin
            psym:=search_struct_member(java_jlstring,'CHARAT');
            if not assigned(psym) or
               (psym.typ<>procsym) then
              internalerror(2011031501);
            { Pascal strings are 1-based, Java strings 0-based }
            result:=ccallnode.create(ccallparanode.create(
              caddnode.create(subn,right,genintconstnode(1)),nil),tprocsym(psym),
              psym.owner,ctypeconvnode.create_explicit(left,java_jlstring),[]);
            left:=nil;
            right:=nil;
            exit;
          end
        else
          result:=inherited;
      end;


    procedure tjvmvecnode.pass_generate_code;
      var
        newsize: tcgsize;
      begin
{$ifndef nounsupported}
        if left.resultdef.typ=stringdef then
          begin
            location:=left.location;
            exit;
          end;
{$endif}

        { This routine is not used for Strings, as they are a class type and
          you have to use charAt() there to load a character (and you cannot
          change characters; you have to create a new string in that case)

          As far as arrays are concerned: we have to create a trefererence
          with arrayreftype in [art_indexreg,art_indexref], and ref.base =
          pointer to the array (i.e., left.location.register) }
        secondpass(left);
        newsize:=def_cgsize(resultdef);
        if left.location.loc=LOC_CREFERENCE then
          location_reset_ref(location,LOC_CREFERENCE,newsize,left.location.reference.alignment)
        else
          location_reset_ref(location,LOC_REFERENCE,newsize,left.location.reference.alignment);
        { don't use left.resultdef, because it may be an open or regular array,
          and then asking for the size doesn't make any sense }
        hlcg.location_force_reg(current_asmdata.CurrAsmList,left.location,java_jlobject,java_jlobject,true);
        location.reference.base:=left.location.register;
        secondpass(right);
        { simplify index location if necessary, since array references support
          an index in memory, but not an another array index }
        if (right.location.loc in [LOC_REFERENCE,LOC_CREFERENCE]) and
           (right.location.reference.arrayreftype<>art_none) then
          hlcg.location_force_reg(current_asmdata.CurrAsmList,right.location,right.resultdef,right.resultdef,true);

        { adjust index if necessary }
        if not is_special_array(left.resultdef) and
           (tarraydef(left.resultdef).lowrange<>0) and
           (right.location.loc<>LOC_CONSTANT) then
          begin
            thlcgjvm(hlcg).a_load_loc_stack(current_asmdata.CurrAsmList,right.resultdef,right.location);
            thlcgjvm(hlcg).a_op_const_stack(current_asmdata.CurrAsmList,OP_SUB,right.resultdef,tarraydef(left.resultdef).lowrange);
            if right.location.loc<>LOC_REGISTER then
              begin
                location_reset(right.location,LOC_REGISTER,def_cgsize(right.resultdef));
                right.location.register:=hlcg.getintregister(current_asmdata.CurrAsmList,right.resultdef);
              end;
            thlcgjvm(hlcg).a_load_stack_reg(current_asmdata.CurrAsmList,right.resultdef,right.location.register);
          end;

        { create array reference }
        case right.location.loc of
          LOC_REGISTER,LOC_CREGISTER:
            begin
              location.reference.arrayreftype:=art_indexreg;
              location.reference.index:=right.location.register;
            end;
          LOC_REFERENCE,LOC_CREFERENCE:
            begin
              location.reference.arrayreftype:=art_indexref;
              location.reference.indexbase:=right.location.reference.base;
              location.reference.indexsymbol:=right.location.reference.symbol;
              location.reference.indexoffset:=right.location.reference.offset;
            end;
          LOC_CONSTANT:
            begin
              location.reference.arrayreftype:=art_indexconst;
              location.reference.indexoffset:=right.location.value-tarraydef(left.resultdef).lowrange;
            end
          else
            internalerror(2011012002);
        end;
      end;


begin
   cvecnode:=tjvmvecnode;
   cloadparentfpnode:=tjvmloadparentfpnode;
   cloadvmtaddrnode:=tjvmloadvmtaddrnode;
end.
