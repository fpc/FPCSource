{
    Copyright (c) 2014 by Jonas Maebe

    Generate LLVM bytecode for inline nodes

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
unit nllvminl;

{$i fpcdefs.inc}

interface

    uses
      node,
      ncginl;

    type
      tllvminlinenode = class(tcginlinenode)
       protected
        procedure maybe_remove_round_trunc_typeconv;

        function first_get_frame: tnode; override;
        function first_abs_real: tnode; override;
        function first_sqr_real: tnode; override;
        function first_trunc_real: tnode; override;
       public
        procedure second_length; override;
        procedure second_sqr_real; override;
        procedure second_trunc_real; override;
      end;


implementation

     uses
       verbose,globals,globtype,constexp,
       aasmbase, aasmdata,
       symconst,symtype,symdef,defutil,
       nutils,nadd,nbas,ncal,ncnv,ncon,nflw,ninl,nld,nmat,
       pass_2,
       cgbase,cgutils,tgobj,hlcgobj,
       cpubase,
       llvmbase,aasmllvm;

     procedure tllvminlinenode.maybe_remove_round_trunc_typeconv;
       var
         temp: tnode;
       begin
         { the prototype of trunc()/round() in the system unit is declared
           with valreal as parameter type, so the argument will always be
           extended -> remove the typeconversion to extended if any; not done
           in ninl, because there are other code generators that assume that
           the parameter to trunc has been converted to valreal (e.g. PowerPC).

           (copy from code in nx64inl, should be refactored)
         }
         if (left.nodetype=typeconvn) and
            not(nf_explicit in left.flags) and
            (ttypeconvnode(left).left.resultdef.typ=floatdef) then
           begin
             { get rid of the type conversion, so the use_vectorfpu will be
               applied to the original type }
             temp:=ttypeconvnode(left).left;
             ttypeconvnode(left).left:=nil;
             left.free;
             left:=temp;
           end;
       end;


     function tllvminlinenode.first_get_frame: tnode;
       begin
         result:=ccallnode.createintern('llvm_frameaddress',
           ccallparanode.create(genintconstnode(0),nil));
       end;

    { in general, generate regular expression rather than intrinsics: according
      to the "Performance Tips for Frontend Authors", "The optimizer is quite
      good at reasoning about general control flow and arithmetic, it is not
      anywhere near as strong at reasoning about the various intrinsics. If
      profitable for code generation purposes, the optimizer will likely form
      the intrinsics itself late in the optimization pipeline." }

    function tllvminlinenode.first_abs_real: tnode;
      var
        lefttemp,
        resulttemp: ttempcreatenode;
        stat: tstatementnode;
      begin
        result:=internalstatements(stat);
        lefttemp:=ctempcreatenode.create(left.resultdef,left.resultdef.size,tt_persistent,true);
        { assigned twice -> will be spilled if put in register }
        resulttemp:=ctempcreatenode.create(resultdef,resultdef.size,tt_persistent,false);

        addstatement(stat,lefttemp);
        addstatement(stat,resulttemp);

        { lefttemp:=left }
        addstatement(stat,
          cassignmentnode.create(ctemprefnode.create(lefttemp),left)
        );

        { if lefttemp>=0 then
            resulttemp:=lefttemp
          else
            resulttemp:=-lefttemp
        }
        addstatement(stat,
          cifnode.create(
            caddnode.create(
              gten,
              ctemprefnode.create(lefttemp),
              crealconstnode.create(0.0,left.resultdef)
            ),
            cassignmentnode.create(
              ctemprefnode.create(resulttemp),
              ctemprefnode.create(lefttemp)
            ),
            cassignmentnode.create(
              ctemprefnode.create(resulttemp),
              cunaryminusnode.create(ctemprefnode.create(lefttemp))
            )
          )
        );
        addstatement(stat,ctempdeletenode.create(lefttemp));
        addstatement(stat,ctempdeletenode.create_normal_temp(resulttemp));
        { return resulttemp }
        addstatement(stat,ctemprefnode.create(resulttemp));
        { reused }
        left:=nil;
      end;


    function tllvminlinenode.first_sqr_real: tnode;
      begin
        result:=nil;
        if use_vectorfpu(left.resultdef) then
          expectloc:=LOC_MMREGISTER
        else
          expectloc:=LOC_FPUREGISTER;
      end;


    function tllvminlinenode.first_trunc_real: tnode;
      begin
        { fptosi is undefined if the value is out of range -> only generate
          in cast of fastmath }
        if cs_opt_fastmath in current_settings.optimizerswitches then
          begin
            maybe_remove_round_trunc_typeconv;
            expectloc:=LOC_REGISTER;
            result:=nil;
          end
        else
          result:=inherited;
      end;


    procedure tllvminlinenode.second_length;
      var
        lengthlab, nillab: tasmlabel;
        hregister: tregister;
        href, tempref: treference;
        lendef: tdef;
      begin
        secondpass(left);
        if is_shortstring(left.resultdef) then
         begin
            if not(left.location.loc in [LOC_REFERENCE,LOC_CREFERENCE]) then
              internalerror(2014080806);
           { typecast the shortstring reference into a length byte reference }
           location_reset_ref(location,left.location.loc,def_cgsize(resultdef),left.location.reference.alignment,left.location.reference.volatility);
           hregister:=hlcg.getaddressregister(current_asmdata.CurrAsmList,cpointerdef.getreusable(resultdef));
           hlcg.a_loadaddr_ref_reg(current_asmdata.CurrAsmList,left.resultdef,cpointerdef.getreusable(resultdef),left.location.reference,hregister);
           hlcg.reference_reset_base(location.reference,cpointerdef.getreusable(resultdef),hregister,0,left.location.reference.temppos,left.location.reference.alignment,left.location.reference.volatility);
         end
        else
         begin
           tg.gethltemp(current_asmdata.CurrAsmList,resultdef,resultdef.size,tt_normal,tempref);
           { length in ansi/wide strings and high in dynamic arrays is at offset
             -sizeof(sizeint), for widestrings it's at -4 }
           if is_widestring(left.resultdef) then
             lendef:=u32inttype
           else
             lendef:=ossinttype;
           hlcg.location_force_reg(current_asmdata.CurrAsmList,left.location,
             left.resultdef,cpointerdef.getreusable(lendef),true);
           current_asmdata.getjumplabel(nillab);
           current_asmdata.getjumplabel(lengthlab);
           hlcg.a_cmp_const_reg_label(current_asmdata.CurrAsmList,cpointerdef.getreusable(lendef),OC_EQ,0,left.location.register,nillab);
           { volatility of the ansistring/widestring refers to the volatility of the
             string pointer, not of the string data }
           hlcg.reference_reset_base(href,cpointerdef.getreusable(lendef),left.location.register,-lendef.size,ctempposinvalid,lendef.alignment,[]);
           hregister:=hlcg.getintregister(current_asmdata.CurrAsmList,resultdef);
           hlcg.a_load_ref_reg(current_asmdata.CurrAsmList,lendef,resultdef,href,hregister);
           if is_widestring(left.resultdef) then
             hlcg.a_op_const_reg(current_asmdata.CurrAsmList,OP_SHR,resultdef,1,hregister);

           { Dynamic arrays do not have their length attached but their maximum index }
           if is_dynamic_array(left.resultdef) then
             hlcg.a_op_const_reg(current_asmdata.CurrAsmList,OP_ADD,resultdef,1,hregister);
           hlcg.a_load_reg_ref(current_asmdata.CurrAsmList,resultdef,resultdef,hregister,tempref);
           hlcg.a_jmp_always(current_asmdata.CurrAsmList,lengthlab);

           hlcg.a_label(current_asmdata.CurrAsmList,nillab);
           hlcg.a_load_const_ref(current_asmdata.CurrAsmList,resultdef,0,tempref);

           hlcg.a_label(current_asmdata.CurrAsmList,lengthlab);
           hregister:=hlcg.getintregister(current_asmdata.CurrAsmList,resultdef);
           hlcg.a_load_ref_reg(current_asmdata.CurrAsmList,resultdef,resultdef,tempref,hregister);
           tg.ungettemp(current_asmdata.CurrAsmList,tempref);
           location_reset(location,LOC_REGISTER,def_cgsize(resultdef));
           location.register:=hregister;
         end;
      end;


    procedure tllvminlinenode.second_sqr_real;
      begin
        secondpass(left);
        location.loc:=expectloc;
        if expectloc=LOC_MMREGISTER then
          begin
            hlcg.location_force_mmregscalar(current_asmdata.CurrAsmList,left.location,left.resultdef,true);
            location.register:=hlcg.getmmregister(current_asmdata.CurrAsmList,resultdef);
          end
        else
          begin
            hlcg.location_force_fpureg(current_asmdata.CurrAsmList,left.location,left.resultdef,true);
            location.register:=hlcg.getfpuregister(current_asmdata.CurrAsmList,resultdef);
          end;
        current_asmdata.CurrAsmList.concat(
          taillvm.op_reg_size_reg_reg(la_fmul,
            location.register,resultdef,
            left.location.register,left.location.register
          )
        );
      end;


    procedure tllvminlinenode.second_trunc_real;
      begin
        secondpass(left);
        if use_vectorfpu(left.resultdef) then
          hlcg.location_force_mmregscalar(current_asmdata.CurrAsmList,left.location,left.resultdef,true)
        else
          hlcg.location_force_fpureg(current_asmdata.CurrAsmList,left.location,left.resultdef,true);
        location_reset(location,LOC_REGISTER,def_cgsize(resultdef));
        location.register:=hlcg.getregisterfordef(current_asmdata.CurrAsmList,resultdef);
        current_asmdata.CurrAsmList.concat(
          taillvm.op_reg_size_reg_size(la_fptosi,location.register,left.resultdef,left.location.register,resultdef)
        );
      end;

begin
  cinlinenode:=tllvminlinenode;
end.

