{
    Copyright (c) 1998-2002 by Florian Klaempfl

    Generate i8086 inline nodes

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
unit n8086inl;

{$i fpcdefs.inc}

interface

    uses
       nx86inl,node;

    type

       { ti8086inlinenode }

       ti8086inlinenode = class(tx86inlinenode)
         function typecheck_seg: tnode; override;
         function first_seg: tnode; override;
         procedure second_seg; override;
         procedure second_get_frame;override;
       end;

implementation

  uses
    ninl,
    systems,
    globtype,globals,
    cutils,verbose,
    symconst,
    defutil,
    aasmbase,aasmtai,aasmdata,aasmcpu,
    symtype,symdef,
    cgbase,pass_2,
    cpuinfo,cpubase,paramgr,
    nbas,ncon,ncal,ncnv,nld,ncgutil,
    tgobj,
    cga,cgutils,cgx86,cgobj,hlcgobj,
    htypechk,procinfo;

     function ti8086inlinenode.typecheck_seg: tnode;
       begin
         result := nil;
         resultdef:=u16inttype;
       end;

     function ti8086inlinenode.first_seg: tnode;
       begin
         expectloc:=LOC_REGISTER;
         result:=nil;
       end;

     procedure ti8086inlinenode.second_seg;
       begin
         location_reset(location,LOC_REGISTER,OS_16);
         location.register:=cg.getintregister(current_asmdata.CurrAsmList,OS_16);
         current_asmdata.CurrAsmList.Concat(Taicpu.op_reg_reg(A_MOV,S_W,NR_DS,location.register));
       end;

     procedure ti8086inlinenode.second_get_frame;
       begin
         if current_settings.x86memorymodel in x86_far_data_models then
           begin
             if current_procinfo.framepointer=NR_STACK_POINTER_REG then
               internalerror(2014030201);
             location_reset(location,LOC_REGISTER,OS_32);
             location.register:=cg.getintregister(current_asmdata.CurrAsmList,OS_32);
             emit_reg_reg(A_MOV,S_W,current_procinfo.framepointer,location.register);
             current_asmdata.CurrAsmList.Concat(Taicpu.op_reg_reg(A_MOV,S_W,NR_SS,GetNextReg(location.register)));
           end
         else
           inherited second_get_frame;
       end;

begin
   cinlinenode:=ti8086inlinenode;
end.
