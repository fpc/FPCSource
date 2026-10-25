{
    Copyright (c) 2000-2002 by Florian Klaempfl

    Code generation for add nodes on the x86-64

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
unit nx64add;

{$i fpcdefs.inc}

interface

    uses
       node,nadd,cpubase,nx86add;

    type
       tx8664addnode = class(tx86addnode)
         function use_generic_mul64bit: boolean; override;
         procedure second_addordinal; override;
         procedure second_mul;
         procedure second_cmp128bit;override;
       end;

  implementation

    uses
      globtype,globals,verbose,
      aasmbase,aasmdata,aasmcpu,
      defutil,
      symconst,symdef,
      cgbase,cgutils,cga,cgobj,hlcgobj,cgx86,
      tgobj;

    function tx8664addnode.use_generic_mul64bit: boolean;
    begin
      result:=false;
    end;

{*****************************************************************************
                                Addordinal
*****************************************************************************}

    procedure tx8664addnode.second_addordinal;
    begin
      { filter unsigned MUL opcode, which requires special handling.
        Note that when overflow checking is off, we can use IMUL instead. }
      if (nodetype=muln) and
        needoverflowcheck and
        (not(is_signed(left.resultdef)) or
         not(is_signed(right.resultdef))) then
      begin
        second_mul;
        exit;
      end;

      inherited second_addordinal;
    end;

{*****************************************************************************
                                MUL
*****************************************************************************}

    procedure tx8664addnode.second_mul;
      var
        reg,rega,regd:Tregister;
        ref:Treference;
        use_ref:boolean;
        hl4 : tasmlabel;
        cgsize:TCgSize;
        opsize:topsize;
      begin
        reference_reset(ref,0,[]);
        reg:=NR_NO;

        cgsize:=def_cgsize(resultdef);
        opsize:=TCGSize2OpSize[cgsize];
        case cgsize of
          OS_S64,OS_64:
            begin
              rega:=NR_RAX;
              regd:=NR_RDX;
            end;
          OS_S32,OS_32:
            begin
              rega:=NR_EAX;
              regd:=NR_EDX;
            end;
          else
            internalerror(2013102703);
        end;

        pass_left_right;

        { The location.register will be filled in later (JM) }
        location_reset(location,LOC_REGISTER,def_cgsize(resultdef));
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
            reg:=cg.getintregister(current_asmdata.CurrAsmList,cgsize);
            hlcg.a_load_loc_reg(current_asmdata.CurrAsmList,left.resultdef,resultdef,left.location,reg);
          end;
        { Allocate RAX. }
        cg.getcpuregister(current_asmdata.CurrAsmList,rega);
        { Load the right value. }
        hlcg.a_load_loc_reg(current_asmdata.CurrAsmList,right.resultdef,resultdef,right.location,rega);
        { Also allocate RDX, since it is also modified by a mul (JM). }
        cg.getcpuregister(current_asmdata.CurrAsmList,regd);

        if needoverflowcheck then
          cg.a_reg_alloc(current_asmdata.CurrAsmList,NR_DEFAULTFLAGS);

        if use_ref then
          emit_ref(A_MUL,opsize,ref)
        else
          emit_reg(A_MUL,opsize,reg);
        if needoverflowcheck then
         begin
           current_asmdata.getjumplabel(hl4);
           cg.a_jmp_flags(current_asmdata.CurrAsmList,F_AE,hl4);
           cg.a_reg_dealloc(current_asmdata.CurrAsmList,NR_DEFAULTFLAGS);
           cg.a_call_name(current_asmdata.CurrAsmList,'FPC_OVERFLOW',false);
           cg.a_label(current_asmdata.CurrAsmList,hl4);
         end;
        { Free RDX,RAX }
        cg.ungetcpuregister(current_asmdata.CurrAsmList,regd);
        cg.ungetcpuregister(current_asmdata.CurrAsmList,rega);
        { Allocate a new register and store the result in RAX in it. }
        location.register:=cg.getintregister(current_asmdata.CurrAsmList,cgsize);
        emit_reg_reg(A_MOV,opsize,rega,location.register);
        location_freetemp(current_asmdata.CurrAsmList,left.location);
        location_freetemp(current_asmdata.CurrAsmList,right.location);
      end;

{*****************************************************************************
                                cmp128bit
*****************************************************************************}

    procedure tx8664addnode.second_cmp128bit;
      var
        truelabel,
        falselabel,
        hlab       : tasmlabel;
        href       : treference;
        unsigned   : boolean;

      procedure firstjmp128bitcmp;

        var
           oldnodetype : tnodetype;

        begin
           { the jump the sequence is a little bit hairy }
           case nodetype of
              ltn,gtn:
                begin
                   if (hlab<>location.truelabel) then
                     cg.a_jmp_flags(current_asmdata.CurrAsmList,getresflags(unsigned),location.truelabel);
                   { cheat a little bit for the negative test }
                   toggleflag(nf_swapped);
                   if (hlab<>location.falselabel) then
                     cg.a_jmp_flags(current_asmdata.CurrAsmList,getresflags(unsigned),location.falselabel);
                   toggleflag(nf_swapped);
                end;
              lten,gten:
                begin
                   oldnodetype:=nodetype;
                   if nodetype=lten then
                     nodetype:=ltn
                   else
                     nodetype:=gtn;
                   if (hlab<>location.truelabel) then
                     cg.a_jmp_flags(current_asmdata.CurrAsmList,getresflags(unsigned),location.truelabel);
                   { cheat for the negative test }
                   if nodetype=ltn then
                     nodetype:=gtn
                   else
                     nodetype:=ltn;
                   if (hlab<>location.falselabel) then
                     cg.a_jmp_flags(current_asmdata.CurrAsmList,getresflags(unsigned),location.falselabel);
                   nodetype:=oldnodetype;
                end;
              equaln:
                cg.a_jmp_flags(current_asmdata.CurrAsmList,F_NE,location.falselabel);
              unequaln:
                cg.a_jmp_flags(current_asmdata.CurrAsmList,F_NE,location.truelabel);
              else
                internalerror(2019050905);
           end;
        end;

      procedure secondjmp128bitcmp;

        begin
           { the jump the sequence is a little bit hairy }
           case nodetype of
              ltn,gtn,lten,gten:
                begin
                   { the comparison of the low dword have to be   }
                   {  always unsigned!                            }
                   cg.a_jmp_flags(current_asmdata.CurrAsmList,getresflags(true),location.truelabel);
                   cg.a_jmp_always(current_asmdata.CurrAsmList,location.falselabel);
                end;
              equaln:
                begin
                   cg.a_jmp_flags(current_asmdata.CurrAsmList,F_NE,location.falselabel);
                   cg.a_jmp_always(current_asmdata.CurrAsmList,location.truelabel);
                end;
              unequaln:
                begin
                   cg.a_jmp_flags(current_asmdata.CurrAsmList,F_NE,location.truelabel);
                   cg.a_jmp_always(current_asmdata.CurrAsmList,location.falselabel);
                end;
              else
                internalerror(2019050904);
           end;
        end;

      procedure gen_cmp64_const_ref(c : aint;ref : treference);
        var
          tmpreg: TRegister;
        begin
          if (c<low(longint)) or (c>high(longint)) then
            begin
              tmpreg:=cg.getintregister(current_asmdata.CurrAsmList,OS_64);
              cg.a_load_const_reg(current_asmdata.CurrAsmList,OS_64,c,tmpreg);
              emit_reg_ref(A_CMP,S_Q,tmpreg,ref);
            end
          else
            emit_const_ref(A_CMP,S_Q,c,ref);
        end;

      begin
        truelabel:=nil;
        falselabel:=nil;
        pass_left_right;

        unsigned:=((left.resultdef.typ=orddef) and
                   (torddef(left.resultdef).ordtype=u128bit)) or
                  ((right.resultdef.typ=orddef) and
                   (torddef(right.resultdef).ordtype=u128bit));

        { we have LOC_JUMP as result }
        current_asmdata.getjumplabel(truelabel);
        current_asmdata.getjumplabel(falselabel);
        location_reset_jump(location,truelabel,falselabel);

        { Relational compares against constants having low dword=0 can omit the
          second compare based on the fact that any unsigned value is >=0 }
        hlab:=nil;
        if (right.location.loc=LOC_CONSTANT) and
           (hlcgint_lo(right.location.value128)=0) then
          begin
            case getresflags(true) of
              F_AE: hlab:=location.truelabel ;
              F_B:  hlab:=location.falselabel;
              else
                ;
            end;
          end;

        if (right.location.loc=LOC_CONSTANT) and
           (left.location.loc in [LOC_REFERENCE,LOC_CREFERENCE]) then
          begin
            tcgx86(cg).make_simple_ref(current_asmdata.CurrAsmList,left.location.reference);
            href:=left.location.reference;
            inc(href.offset,8);
            cg.a_reg_alloc(current_asmdata.CurrAsmList,NR_DEFAULTFLAGS);
            gen_cmp64_const_ref(aint(hlcgint_hi(right.location.value128)),href);
            firstjmp128bitcmp;
            cg.a_reg_dealloc(current_asmdata.CurrAsmList,NR_DEFAULTFLAGS);
            if assigned(hlab) then
              cg.a_jmp_always(current_asmdata.CurrAsmList,hlab)
            else
              begin
                cg.a_reg_alloc(current_asmdata.CurrAsmList,NR_DEFAULTFLAGS);
                gen_cmp64_const_ref(aint(hlcgint_lo(right.location.value128)),left.location.reference);
                secondjmp128bitcmp;
                cg.a_reg_dealloc(current_asmdata.CurrAsmList,NR_DEFAULTFLAGS);
              end;
            location_freetemp(current_asmdata.CurrAsmList,left.location);
            exit;
          end;

        { left and right no register?  }
        { then one must be demanded    }
        if not (left.location.loc in [LOC_REGISTER,LOC_CREGISTER]) then
         begin
           if not (right.location.loc in [LOC_REGISTER,LOC_CREGISTER]) then
             hlcg.location_force_reg(current_asmdata.CurrAsmList,left.location,left.resultdef,left.resultdef,true)
           else
            begin
              location_swap(left.location,right.location);
              toggleflag(nf_swapped);
            end;
         end;

        { at this point, left.location.loc should be LOC_[C]REGISTER }
        case right.location.loc of
          LOC_REGISTER,
          LOC_CREGISTER :
            begin
              cg.a_reg_alloc(current_asmdata.CurrAsmList,NR_DEFAULTFLAGS);
              emit_reg_reg(A_CMP,S_Q,right.location.register128.reghi,left.location.register128.reghi);
              firstjmp128bitcmp;
              emit_reg_reg(A_CMP,S_Q,right.location.register128.reglo,left.location.register128.reglo);
              secondjmp128bitcmp;
              cg.a_reg_dealloc(current_asmdata.CurrAsmList,NR_DEFAULTFLAGS);
            end;
          LOC_CREFERENCE,
          LOC_REFERENCE :
            begin
              tcgx86(cg).make_simple_ref(current_asmdata.CurrAsmList,right.location.reference);
              href:=right.location.reference;
              inc(href.offset,8);
              cg.a_reg_alloc(current_asmdata.CurrAsmList,NR_DEFAULTFLAGS);
              emit_ref_reg(A_CMP,S_Q,href,left.location.register128.reghi);
              firstjmp128bitcmp;
              emit_ref_reg(A_CMP,S_Q,right.location.reference,left.location.register128.reglo);
              secondjmp128bitcmp;
              cg.a_reg_dealloc(current_asmdata.CurrAsmList,NR_DEFAULTFLAGS);
              location_freetemp(current_asmdata.CurrAsmList,right.location);
            end;
          LOC_CONSTANT :
            begin
              cg.a_reg_alloc(current_asmdata.CurrAsmList,NR_DEFAULTFLAGS);
              current_asmdata.CurrAsmList.concat(taicpu.op_const_reg(A_CMP,S_Q,aint(hlcgint_hi(right.location.value128)),left.location.register128.reghi));
              firstjmp128bitcmp;
              cg.a_reg_dealloc(current_asmdata.CurrAsmList,NR_DEFAULTFLAGS);
              if assigned(hlab) then
                cg.a_jmp_always(current_asmdata.CurrAsmList,hlab)
              else
                begin
                  cg.a_reg_alloc(current_asmdata.CurrAsmList,NR_DEFAULTFLAGS);
                  current_asmdata.CurrAsmList.concat(taicpu.op_const_reg(A_CMP,S_Q,aint(hlcgint_lo(right.location.value128)),left.location.register128.reglo));
                  secondjmp128bitcmp;
                  cg.a_reg_dealloc(current_asmdata.CurrAsmList,NR_DEFAULTFLAGS);
                end;
            end;
        else
          internalerror(2002032803);
        end;

      end;


begin
   caddnode:=tx8664addnode;
end.
