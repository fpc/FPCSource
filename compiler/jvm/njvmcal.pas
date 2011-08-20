{
    Copyright (c) 2011 by Jonas Maebe

    JVM-specific code for call nodes

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
unit njvmcal;

{$i fpcdefs.inc}

interface

    uses
      cgbase,
      symdef,
      ncgcal;

    type

       tjvmcallparanode = class(tcgcallparanode)
        protected
         outcopybasereg: tregister;
         procedure push_copyout_para; override;
       end;

       { tjvmcallnode }

       tjvmcallnode = class(tcgcallnode)
        protected
         procedure extra_pre_call_code; override;
         procedure set_result_location(realresdef: tstoreddef); override;
         procedure do_release_unused_return_value;override;
         procedure extra_post_call_code; override;
       end;


implementation

    uses
      verbose,globtype,
      symconst,symtype,defutil,ncal,
      cgutils,tgobj,procinfo,
      cpubase,aasmdata,aasmcpu,
      hlcgobj,hlcgcpu,
      node,
      jvmdef;

{*****************************************************************************
                           TJVMCALLPARANODE
*****************************************************************************}

    procedure tjvmcallparanode.push_copyout_para;
      var
        mangledname: string;
        primitivetype: boolean;
        opc: tasmop;
        arrayloc: tlocation;
        arrayref: treference;
      begin
        { create an array with one element of the parameter type }
        thlcgjvm(hlcg).a_load_const_stack(current_asmdata.CurrAsmList,s32inttype,1,R_INTREGISTER);
        mangledname:=jvmarrtype(left.resultdef,primitivetype);
        if primitivetype then
          opc:=a_newarray
        else
          opc:=a_anewarray;
        { doesn't change stack height: one int replaced by one reference }
        current_asmdata.CurrAsmList.concat(taicpu.op_sym(opc,current_asmdata.RefAsmSymbol(mangledname)));
        { cannot be a regular array or record, because those are passed by
          plain reference (since they are reference types at the Java level,
          but not at the Pascal level) -> no special initialisation necessary }
        outcopybasereg:=hlcg.getaddressregister(current_asmdata.CurrAsmList,java_jlobject);
        thlcgjvm(hlcg).a_load_stack_reg(current_asmdata.CurrAsmList,java_jlobject,outcopybasereg);
        reference_reset_base(arrayref,outcopybasereg,0,4);
        arrayref.arrayreftype:=art_indexconst;
        arrayref.indexoffset:=0;
        { load the current parameter value into the array in case it's not an
          out-parameter; if it's an out-parameter the contents must be nil
          but that's already ok, since the anewarray opcode takes care of that }
        if (parasym.varspez<>vs_out) then
          hlcg.a_load_loc_ref(current_asmdata.CurrAsmList,left.resultdef,left.resultdef,left.location,arrayref);

        { store the array reference into the parameter location (don't change
          left.location, we may need it for copy-back after the call) }
        location_reset(arrayloc,LOC_REGISTER,OS_ADDR);
        arrayloc.register:=outcopybasereg;
        hlcg.gen_load_loc_cgpara(current_asmdata.CurrAsmList,java_jlobject,arrayloc,tempcgpara)
      end;


{*****************************************************************************
                             TJVMCALLNODE
*****************************************************************************}

    procedure tjvmcallnode.extra_pre_call_code;
      begin
        { when calling a constructor, first create a new instance, except
          when calling it from another constructor (because then this has
          already been done before calling the current constructor) }
        if procdefinition.typ<>procdef then
          exit;
        if tabstractprocdef(procdefinition).proctypeoption<>potype_constructor then
          exit;
        if not(methodpointer.resultdef.typ in [classrefdef,recorddef]) then
          exit;
        current_asmdata.CurrAsmList.concat(taicpu.op_sym(a_new,current_asmdata.RefAsmSymbol(tabstractrecorddef(tabstractprocdef(procdefinition).owner.defowner).jvm_full_typename(true))));
        { the constructor doesn't return anything, so put a duplicate of the
          self pointer on the evaluation stack for use as function result
          after the constructor has run }
        current_asmdata.CurrAsmList.concat(taicpu.op_none(a_dup));
        thlcgjvm(hlcg).incstack(current_asmdata.CurrAsmList,2);
      end;


    procedure tjvmcallnode.set_result_location(realresdef: tstoreddef);
      begin
        location_reset_ref(location,LOC_REFERENCE,def_cgsize(realresdef),1);
        { in case of jvmimplicitpointertype(), the function will have allocated
          it already and we don't have to allocate it again here }
        if not jvmimplicitpointertype(realresdef) then
          tg.gethltemp(current_asmdata.CurrAsmList,realresdef,realresdef.size,tt_normal,location.reference)
        else
          tg.gethltemp(current_asmdata.CurrAsmList,java_jlobject,java_jlobject.size,tt_normal,location.reference);
      end;


    procedure tjvmcallnode.do_release_unused_return_value;
      begin
        if (tabstractprocdef(procdefinition).proctypeoption=potype_constructor) and
           (current_procinfo.procdef.proctypeoption=potype_constructor) then
          exit;
        case resultdef.size of
          0:
            ;
          1..4:
            begin
              current_asmdata.CurrAsmList.concat(taicpu.op_none(a_pop));
              thlcgjvm(hlcg).decstack(current_asmdata.CurrAsmList,1);
            end;
          8:
            begin
              current_asmdata.CurrAsmList.concat(taicpu.op_none(a_pop2));
              thlcgjvm(hlcg).decstack(current_asmdata.CurrAsmList,2);
            end
          else
            internalerror(2011010305);
        end;
      end;


    procedure tjvmcallnode.extra_post_call_code;
      var
        totalremovesize: longint;
        realresdef: tdef;
        ppn: tjvmcallparanode;
        pararef: treference;
      begin
        if not assigned(typedef) then
          realresdef:=tstoreddef(resultdef)
        else
          realresdef:=tstoreddef(typedef);
        { a constructor doesn't actually return a value in the jvm }
        if (tabstractprocdef(procdefinition).proctypeoption=potype_constructor) then
          totalremovesize:=pushedparasize
        else
          { even a byte takes up a full stackslot -> align size to multiple of 4 }
          totalremovesize:=pushedparasize-(align(realresdef.size,4) shr 2);
        { remove parameters from internal evaluation stack counter (in case of
          e.g. no parameters and a result, it can also increase) }
        if totalremovesize>0 then
          thlcgjvm(hlcg).decstack(current_asmdata.CurrAsmList,totalremovesize)
        else if totalremovesize<0 then
          thlcgjvm(hlcg).incstack(current_asmdata.CurrAsmList,-totalremovesize);

        { if this was an inherited constructor call, initialise all fields that
          are wrapped types following it }
        if (tabstractprocdef(procdefinition).proctypeoption=potype_constructor) and
           (cnf_inherited in callnodeflags) then
          thlcgjvm(hlcg).gen_initialize_fields_code(current_asmdata.CurrAsmList);

        { copy back the copyout parameter values, if any }
        { Release temps from parameters }
        ppn:=tjvmcallparanode(left);
        while assigned(ppn) do
          begin
            if assigned(ppn.left) then
              begin
                if (ppn.outcopybasereg<>NR_NO) then
                  begin
                    reference_reset_base(pararef,NR_NO,0,4);
                    pararef.arrayreftype:=art_indexconst;
                    pararef.base:=ppn.outcopybasereg;
                    pararef.indexoffset:=0;
                    { the value has to be copied back into persistent storage }
                    case ppn.left.location.loc of
                      LOC_REFERENCE:
                        hlcg.a_load_ref_ref(current_asmdata.CurrAsmList,ppn.left.resultdef,ppn.left.resultdef,pararef,ppn.left.location.reference);
                      LOC_CREGISTER:
                        hlcg.a_load_ref_reg(current_asmdata.CurrAsmList,ppn.left.resultdef,ppn.left.resultdef,pararef,ppn.left.location.register);
                    else
                      internalerror(2011051201);
                    end;
                  end;
              end;
            ppn:=tjvmcallparanode(ppn.right);
          end;
      end;


begin
  ccallnode:=tjvmcallnode;
  ccallparanode:=tjvmcallparanode;
end.
