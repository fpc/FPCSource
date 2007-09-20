{
    Copyright (c) 2000-2002 by Florian Klaempfl

    Code generation for add nodes on the i386

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
unit n386add;

{$i fpcdefs.inc}

interface

    uses
       node,nadd,cpubase,nx86add;

    type
       ti386addnode = class(tx86addnode)
         procedure second_add64bit;override;
         procedure second_cmp64bit;override;
         procedure second_mul;override;
       end;

  implementation

    uses
      globtype,systems,
      cutils,verbose,globals,
      symconst,symdef,paramgr,
      aasmbase,aasmtai,aasmdata,aasmcpu,
      cgbase,procinfo,
      ncon,nset,cgutils,tgobj,
      cga,ncgutil,cgobj,cg64f32;

{*****************************************************************************
                                Add64bit
*****************************************************************************}

    procedure ti386addnode.second_add64bit;
      var
        op         : TOpCG;
        op1,op2    : TAsmOp;
        opsize     : TOpSize;
        hregister,
        hregister2 : tregister;
        hl4        : tasmlabel;
        mboverflow,
        unsigned:boolean;
        r:Tregister;
      begin
        firstcomplex(self);
        pass_left_right;

        op1:=A_NONE;
        op2:=A_NONE;
        mboverflow:=false;
        opsize:=S_L;
        unsigned:=((left.resultdef.typ=orddef) and
                   (torddef(left.resultdef).ordtype=u64bit)) or
                  ((right.resultdef.typ=orddef) and
                   (torddef(right.resultdef).ordtype=u64bit));
        case nodetype of
          addn :
            begin
              op:=OP_ADD;
              mboverflow:=true;
            end;
          subn :
            begin
              op:=OP_SUB;
              op1:=A_SUB;
              op2:=A_SBB;
              mboverflow:=true;
            end;
          xorn:
            op:=OP_XOR;
          orn:
            op:=OP_OR;
          andn:
            op:=OP_AND;
          else
            begin
              { everything should be handled in pass_1 (JM) }
              internalerror(200109051);
            end;
        end;

        { left and right no register?  }
        { then one must be demanded    }
        if (left.location.loc<>LOC_REGISTER) then
         begin
           if (right.location.loc<>LOC_REGISTER) then
            begin
              hregister:=cg.getintregister(current_asmdata.CurrAsmList,OS_INT);
              hregister2:=cg.getintregister(current_asmdata.CurrAsmList,OS_INT);
              cg64.a_load64_loc_reg(current_asmdata.CurrAsmList,left.location,joinreg64(hregister,hregister2));
              location_reset(left.location,LOC_REGISTER,OS_64);
              left.location.register64.reglo:=hregister;
              left.location.register64.reghi:=hregister2;
            end
           else
            begin
              location_swap(left.location,right.location);
              toggleflag(nf_swapped);
            end;
         end;

        { at this point, left.location.loc should be LOC_REGISTER }
        if right.location.loc=LOC_REGISTER then
         begin
           { when swapped another result register }
           if (nodetype=subn) and (nf_swapped in flags) then
            begin
              cg64.a_op64_reg_reg(current_asmdata.CurrAsmList,op,location.size,
                left.location.register64,
                right.location.register64);
              location_swap(left.location,right.location);
              toggleflag(nf_swapped);
            end
           else
            begin
              cg64.a_op64_reg_reg(current_asmdata.CurrAsmList,op,location.size,
                right.location.register64,
                left.location.register64);
            end;
         end
        else
         begin
           { right.location<>LOC_REGISTER }
           if (nodetype=subn) and (nf_swapped in flags) then
            begin
              r:=cg.getintregister(current_asmdata.CurrAsmList,OS_INT);
              cg64.a_load64low_loc_reg(current_asmdata.CurrAsmList,right.location,r);
              emit_reg_reg(op1,opsize,left.location.register64.reglo,r);
              emit_reg_reg(A_MOV,opsize,r,left.location.register64.reglo);
              cg64.a_load64high_loc_reg(current_asmdata.CurrAsmList,right.location,r);
              { the carry flag is still ok }
              emit_reg_reg(op2,opsize,left.location.register64.reghi,r);
              emit_reg_reg(A_MOV,opsize,r,left.location.register64.reghi);
            end
           else
            begin
              cg64.a_op64_loc_reg(current_asmdata.CurrAsmList,op,location.size,right.location,
                left.location.register64);
            end;
          location_freetemp(current_asmdata.CurrAsmList,right.location);
         end;

        { only in case of overflow operations }
        { produce overflow code }
        { we must put it here directly, because sign of operation }
        { is in unsigned VAR!!                              }
        if mboverflow then
         begin
           if cs_check_overflow in current_settings.localswitches  then
            begin
              current_asmdata.getjumplabel(hl4);
              if unsigned then
                cg.a_jmp_flags(current_asmdata.CurrAsmList,F_AE,hl4)
              else
                cg.a_jmp_flags(current_asmdata.CurrAsmList,F_NO,hl4);
              cg.a_call_name(current_asmdata.CurrAsmList,'FPC_OVERFLOW');
              cg.a_label(current_asmdata.CurrAsmList,hl4);
            end;
         end;

        location_copy(location,left.location);
      end;


    procedure ti386addnode.second_cmp64bit;
      var
        hregister,
        hregister2 : tregister;
        href       : treference;
        unsigned   : boolean;

      procedure firstjmp64bitcmp;

        var
           oldnodetype : tnodetype;

        begin
{$ifdef OLDREGVARS}
           load_all_regvars(current_asmdata.CurrAsmList);
{$endif OLDREGVARS}
           { the jump the sequence is a little bit hairy }
           case nodetype of
              ltn,gtn:
                begin
                   cg.a_jmp_flags(current_asmdata.CurrAsmList,getresflags(unsigned),current_procinfo.CurrTrueLabel);
                   { cheat a little bit for the negative test }
                   toggleflag(nf_swapped);
                   cg.a_jmp_flags(current_asmdata.CurrAsmList,getresflags(unsigned),current_procinfo.CurrFalseLabel);
                   toggleflag(nf_swapped);
                end;
              lten,gten:
                begin
                   oldnodetype:=nodetype;
                   if nodetype=lten then
                     nodetype:=ltn
                   else
                     nodetype:=gtn;
                   cg.a_jmp_flags(current_asmdata.CurrAsmList,getresflags(unsigned),current_procinfo.CurrTrueLabel);
                   { cheat for the negative test }
                   if nodetype=ltn then
                     nodetype:=gtn
                   else
                     nodetype:=ltn;
                   cg.a_jmp_flags(current_asmdata.CurrAsmList,getresflags(unsigned),current_procinfo.CurrFalseLabel);
                   nodetype:=oldnodetype;
                end;
              equaln:
                cg.a_jmp_flags(current_asmdata.CurrAsmList,F_NE,current_procinfo.CurrFalseLabel);
              unequaln:
                cg.a_jmp_flags(current_asmdata.CurrAsmList,F_NE,current_procinfo.CurrTrueLabel);
           end;
        end;

      procedure secondjmp64bitcmp;

        begin
           { the jump the sequence is a little bit hairy }
           case nodetype of
              ltn,gtn,lten,gten:
                begin
                   { the comparisaion of the low dword have to be }
                   {  always unsigned!                            }
                   cg.a_jmp_flags(current_asmdata.CurrAsmList,getresflags(true),current_procinfo.CurrTrueLabel);
                   cg.a_jmp_always(current_asmdata.CurrAsmList,current_procinfo.CurrFalseLabel);
                end;
              equaln:
                begin
                   cg.a_jmp_flags(current_asmdata.CurrAsmList,F_NE,current_procinfo.CurrFalseLabel);
                   cg.a_jmp_always(current_asmdata.CurrAsmList,current_procinfo.CurrTrueLabel);
                end;
              unequaln:
                begin
                   cg.a_jmp_flags(current_asmdata.CurrAsmList,F_NE,current_procinfo.CurrTrueLabel);
                   cg.a_jmp_always(current_asmdata.CurrAsmList,current_procinfo.CurrFalseLabel);
                end;
           end;
        end;

      begin
        firstcomplex(self);

        pass_left_right;

        unsigned:=((left.resultdef.typ=orddef) and
                   (torddef(left.resultdef).ordtype=u64bit)) or
                  ((right.resultdef.typ=orddef) and
                   (torddef(right.resultdef).ordtype=u64bit));

        { left and right no register?  }
        { then one must be demanded    }
        if (left.location.loc<>LOC_REGISTER) then
         begin
           if (right.location.loc<>LOC_REGISTER) then
            begin
              { we can reuse a CREGISTER for comparison }
              if (left.location.loc<>LOC_CREGISTER) then
               begin
                 hregister:=cg.getintregister(current_asmdata.CurrAsmList,OS_INT);
                 hregister2:=cg.getintregister(current_asmdata.CurrAsmList,OS_INT);
                 cg64.a_load64_loc_reg(current_asmdata.CurrAsmList,left.location,joinreg64(hregister,hregister2));
                 location_freetemp(current_asmdata.CurrAsmList,left.location);
                 location_reset(left.location,LOC_REGISTER,OS_64);
                 left.location.register64.reglo:=hregister;
                 left.location.register64.reghi:=hregister2;
               end;
            end
           else
            begin
              location_swap(left.location,right.location);
              toggleflag(nf_swapped);
            end;
         end;

        { at this point, left.location.loc should be LOC_REGISTER }
        if right.location.loc=LOC_REGISTER then
         begin
           emit_reg_reg(A_CMP,S_L,right.location.register64.reghi,left.location.register64.reghi);
           firstjmp64bitcmp;
           emit_reg_reg(A_CMP,S_L,right.location.register64.reglo,left.location.register64.reglo);
           secondjmp64bitcmp;
         end
        else
         begin
           case right.location.loc of
             LOC_CREGISTER :
               begin
                 emit_reg_reg(A_CMP,S_L,right.location.register64.reghi,left.location.register64.reghi);
                 firstjmp64bitcmp;
                 emit_reg_reg(A_CMP,S_L,right.location.register64.reglo,left.location.register64.reglo);
                 secondjmp64bitcmp;
               end;
             LOC_CREFERENCE,
             LOC_REFERENCE :
               begin
                 href:=right.location.reference;
                 inc(href.offset,4);
                 emit_ref_reg(A_CMP,S_L,href,left.location.register64.reghi);
                 firstjmp64bitcmp;
                 emit_ref_reg(A_CMP,S_L,right.location.reference,left.location.register64.reglo);
                 secondjmp64bitcmp;
                 cg.a_jmp_always(current_asmdata.CurrAsmList,current_procinfo.CurrFalseLabel);
                 location_freetemp(current_asmdata.CurrAsmList,right.location);
               end;
             LOC_CONSTANT :
               begin
                 current_asmdata.CurrAsmList.concat(taicpu.op_const_reg(A_CMP,S_L,aint(hi(right.location.value64)),left.location.register64.reghi));
                 firstjmp64bitcmp;
                 current_asmdata.CurrAsmList.concat(taicpu.op_const_reg(A_CMP,S_L,aint(lo(right.location.value64)),left.location.register64.reglo));
                 secondjmp64bitcmp;
               end;
             else
               internalerror(200203282);
           end;
         end;

        { we have LOC_JUMP as result }
        location_reset(location,LOC_JUMP,OS_NO)
      end;


{*****************************************************************************
                                x86 MUL
*****************************************************************************}

    procedure ti386addnode.second_mul;

    var reg:Tregister;
        ref:Treference;
        use_ref:boolean;
        hl4 : tasmlabel;

    begin
      pass_left_right;

      {The location.register will be filled in later (JM)}
      location_reset(location,LOC_REGISTER,OS_INT);
      { Mul supports registers and references, so if not register/reference,
        load the location into a register}
      use_ref:=false;
      if left.location.loc in [LOC_REGISTER,LOC_CREGISTER] then
        reg:=left.location.register
      else if left.location.loc in [LOC_REFERENCE,LOC_CREFERENCE] then
        begin
          ref:=left.location.reference;
          use_ref:=true;
        end
      else
        begin
          {LOC_CONSTANT for example.}
          reg:=cg.getintregister(current_asmdata.CurrAsmList,OS_INT);
          cg.a_load_loc_reg(current_asmdata.CurrAsmList,OS_INT,left.location,reg);
        end;
      {Allocate EAX.}
      cg.getcpuregister(current_asmdata.CurrAsmList,NR_EAX);
      {Load the right value.}
      cg.a_load_loc_reg(current_asmdata.CurrAsmList,OS_INT,right.location,NR_EAX);
      {Also allocate EDX, since it is also modified by a mul (JM).}
      cg.getcpuregister(current_asmdata.CurrAsmList,NR_EDX);
      if use_ref then
        emit_ref(A_MUL,S_L,ref)
      else
        emit_reg(A_MUL,S_L,reg);
      if cs_check_overflow in current_settings.localswitches  then
       begin
         current_asmdata.getjumplabel(hl4);
         cg.a_jmp_flags(current_asmdata.CurrAsmList,F_AE,hl4);
         cg.a_call_name(current_asmdata.CurrAsmList,'FPC_OVERFLOW');
         cg.a_label(current_asmdata.CurrAsmList,hl4);
       end;
      {Free EAX,EDX}
      cg.ungetcpuregister(current_asmdata.CurrAsmList,NR_EDX);
      cg.ungetcpuregister(current_asmdata.CurrAsmList,NR_EAX);
      {Allocate a new register and store the result in EAX in it.}
      location.register:=cg.getintregister(current_asmdata.CurrAsmList,OS_INT);
      cg.a_load_reg_reg(current_asmdata.CurrAsmList,OS_INT,OS_INT,NR_EAX,location.register);
      location_freetemp(current_asmdata.CurrAsmList,left.location);
      location_freetemp(current_asmdata.CurrAsmList,right.location);
    end;


begin
   caddnode:=ti386addnode;
end.
