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
        function first_get_frame: tnode; override;
       public
        procedure second_length; override;
      end;


implementation

     uses
       verbose,globtype,constexp,
       aasmbase, aasmdata,
       symtype,symdef,defutil,
       ncal,ncon,ninl,
       pass_2,
       cgbase,cgutils,tgobj,hlcgobj,
       cpubase;


     function tllvminlinenode.first_get_frame: tnode;
       begin
         result:=ccallnode.createintern('llvm_frameaddress',
           ccallparanode.create(genintconstnode(0),nil));
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
           location_reset_ref(location,left.location.loc,def_cgsize(resultdef),left.location.reference.alignment);
           hregister:=hlcg.getaddressregister(current_asmdata.CurrAsmList,cpointerdef.getreusable(resultdef));
           hlcg.a_loadaddr_ref_reg(current_asmdata.CurrAsmList,left.resultdef,cpointerdef.getreusable(resultdef),left.location.reference,hregister);
           hlcg.reference_reset_base(location.reference,cpointerdef.getreusable(resultdef),hregister,0,left.location.reference.alignment);
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
           hlcg.reference_reset_base(href,cpointerdef.getreusable(lendef),left.location.register,-lendef.size,lendef.alignment);
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

begin
  cinlinenode:=tllvminlinenode;
end.

