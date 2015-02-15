{
    Copyright (c) 2000-2002 by Florian Klaempfl and Jonas Maebe

    Code generation for add nodes on the Motorola 680x0 family

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
unit n68kadd;

{$i fpcdefs.inc}

interface

    uses
       node,nadd,ncgadd,cpubase,cgbase;


    type
       t68kaddnode = class(tcgaddnode)
       private
          function getresflags(unsigned: boolean) : tresflags;
       protected
          procedure second_addfloat;override;
          procedure second_cmpfloat;override;
          procedure second_cmpordinal;override;
          procedure second_cmpsmallset;override;
          procedure second_cmp64bit;override;
       end;


implementation

    uses
      globtype,systems,
      cutils,verbose,globals,
      symconst,symdef,paramgr,symtype,
      aasmbase,aasmtai,aasmdata,aasmcpu,defutil,htypechk,
      cpuinfo,pass_1,pass_2,regvars,
      cpupara,cgutils,procinfo,
      ncon,nset,
      ncgutil,tgobj,rgobj,rgcpu,cgobj,cgcpu,hlcgobj,cg64f32;

{*****************************************************************************
                                  Helpers
*****************************************************************************}

    function t68kaddnode.getresflags(unsigned : boolean) : tresflags;
      begin
         case nodetype of
           equaln : getresflags:=F_E;
           unequaln : getresflags:=F_NE;
          else
           if not(unsigned) then
             begin
                if nf_swapped in flags then
                  case nodetype of
                     ltn : getresflags:=F_G;
                     lten : getresflags:=F_GE;
                     gtn : getresflags:=F_L;
                     gten : getresflags:=F_LE;
                     else
                       internalerror(2014082030);
                  end
                else
                  case nodetype of
                     ltn : getresflags:=F_L;
                     lten : getresflags:=F_LE;
                     gtn : getresflags:=F_G;
                     gten : getresflags:=F_GE;
                     else
                       internalerror(2014082031);
                  end;
             end
           else
             begin
                if nf_swapped in flags then
                  case nodetype of
                     ltn : getresflags:=F_A;
                     lten : getresflags:=F_AE;
                     gtn : getresflags:=F_B;
                     gten : getresflags:=F_BE;
                     else
                       internalerror(2014082032);
                  end
                else
                  case nodetype of
                     ltn : getresflags:=F_B;
                     lten : getresflags:=F_BE;
                     gtn : getresflags:=F_A;
                     gten : getresflags:=F_AE;
                     else
                       internalerror(2014082033);
                  end;
             end;
         end;
      end;


{*****************************************************************************
                                AddFloat
*****************************************************************************}

    procedure t68kaddnode.second_addfloat;
      var
        op    : TAsmOp;
        href  : TReference;
      begin
        pass_left_right;

        case nodetype of
          addn :
            op:=A_FADD;
          muln :
            op:=A_FMUL;
          subn :
            op:=A_FSUB;
          slashn :
            op:=A_FDIV;
          else
            internalerror(200403182);
        end;

        // get the operands in the correct order, there are no special cases
        // here, everything is register-based
        if nf_swapped in flags then
          swapleftright;

        case current_settings.fputype of
          fpu_68881:
            begin
              { have left in the register, right can be a memory location }
              hlcg.location_force_fpureg(current_asmdata.CurrAsmList,left.location,left.resultdef,true);

              { initialize the result }
              location_reset(location,LOC_FPUREGISTER,def_cgsize(resultdef));
              location.register := cg.getfpuregister(current_asmdata.CurrAsmList,location.size);

              { emit the actual operation }
              cg.a_loadfpu_reg_reg(current_asmdata.CurrAsmlist,OS_NO,OS_NO,left.location.register,location.register);
              case right.location.loc of
                LOC_FPUREGISTER,LOC_CFPUREGISTER:
                    current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(op,S_FX,right.location.register,location.register));
                LOC_REFERENCE,LOC_CREFERENCE:
                    begin
                      href:=right.location.reference;
                      tcg68k(cg).fixref(current_asmdata.CurrAsmList,href);
                      current_asmdata.CurrAsmList.concat(taicpu.op_ref_reg(op,tcgsize2opsize[right.location.size],href,location.register));
                    end
                else
                  internalerror(2015021501);
              end;
            end;
          else
            // softfpu should be handled in pass1, others are not yet supported...
            internalerror(2015010201);
        end;
      end;


    procedure t68kaddnode.second_cmpfloat;
      var
        tmpreg : tregister;
        ai: taicpu;
        href  : TReference;
      begin
        pass_left_right;
        if (nf_swapped in flags) then
          swapleftright;

        case current_settings.fputype of
          fpu_68881:
            begin
              { force left fpureg as register, right can be reference }
              hlcg.location_force_fpureg(current_asmdata.CurrAsmList,left.location,left.resultdef,true);

              { emit compare }
              case right.location.loc of
                LOC_FPUREGISTER,LOC_CFPUREGISTER:
                    current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_FCMP,S_FX,right.location.register,left.location.register));
                LOC_REFERENCE,LOC_CREFERENCE:
                    begin
                      href:=right.location.reference;
                      tcg68k(cg).fixref(current_asmdata.CurrAsmList,href);
                      current_asmdata.CurrAsmList.concat(taicpu.op_ref_reg(A_FCMP,tcgsize2opsize[right.location.size],href,left.location.register));
                    end
                else
                  internalerror(2015021502);
              end;

              // temporary(?) hack, move condition result back to the CPU from the FPU.
              // 6888x has its own FBcc branch instructions and FScc flags->reg instruction,
              // which we don't support yet in the rest of the cg. (KB)
              tmpreg:=cg.getintregister(current_asmdata.CurrAsmList,OS_8);
              ai:=taicpu.op_reg(A_FSxx,S_B,tmpreg);
              ai.SetCondition(flags_to_cond(getresflags(false)));
              current_asmdata.CurrAsmList.concat(ai);
              current_asmdata.CurrAsmList.concat(taicpu.op_reg(A_NEG,S_B,tmpreg));

              location_reset(location,LOC_REGISTER,OS_8);
              location.register:=tmpreg;
            end;
          else
            // softfpu should be handled in pass1, others are not yet supported...
            internalerror(2015010201);
        end;
      end;




{*****************************************************************************
                                Smallsets
*****************************************************************************}

    procedure t68kaddnode.second_cmpsmallset;
     var
       tmpreg : tregister;
     begin
       pass_left_right;

       location_reset(location,LOC_FLAGS,OS_NO);

       if (not(nf_swapped in flags) and
           (nodetype = lten)) or
          ((nf_swapped in flags) and
           (nodetype = gten)) then
         swapleftright;

       { Try to keep right as a constant }
       if right.location.loc<>LOC_CONSTANT then
         hlcg.location_force_reg(current_asmdata.CurrAsmList,right.location,right.resultdef,right.resultdef,true);
       hlcg.location_force_reg(current_asmdata.CurrAsmList,left.location,left.resultdef,left.resultdef,true);

       case nodetype of
         equaln,
         unequaln:
           begin
             if right.location.loc=LOC_CONSTANT then
               current_asmdata.CurrAsmList.concat(taicpu.op_const_reg(A_CMP,S_L,right.location.value,left.location.register))
             else
               current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_CMP,S_L,right.location.register,left.location.register));
             if nodetype=equaln then
               location.resflags:=F_E
             else
               location.resflags:=F_NE;
           end;
         lten,
         gten:
           begin
             tmpreg:=cg.getintregister(current_asmdata.CurrAsmList,left.location.size);
             if right.location.loc=LOC_CONSTANT then
               hlcg.location_force_reg(current_asmdata.CurrAsmList,right.location,right.resultdef,right.resultdef,false);
             cg.a_op_reg_reg_reg(current_asmdata.CurrAsmList,OP_AND,OS_32,left.location.register,right.location.register,tmpreg);
             current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_CMP,S_L,tmpreg,right.location.register));
             location.resflags:=F_E;
           end;
         else
           internalerror(2013092701);
       end;
     end;


{*****************************************************************************
                                Ordinals
*****************************************************************************}

    procedure t68kaddnode.second_cmpordinal;
     var
      unsigned : boolean;
      tmpreg : tregister;
      opsize : topsize;
      cmpsize : tcgsize;
      href: treference;
     begin
       { determine if the comparison will be unsigned }
       unsigned:=not(is_signed(left.resultdef)) or
                   not(is_signed(right.resultdef));
       { this puts constant operand (if any) to the right }
       pass_left_right;
       { tentatively assume left size (correct for possible TST, will fix later) }
       cmpsize:=def_cgsize(left.resultdef);
       opsize:=tcgsize2opsize[cmpsize];

       { set result location }
       location_reset(location,LOC_FLAGS,OS_NO);

       { see if we can optimize into TST }
       if (right.location.loc=LOC_CONSTANT) and (right.location.value=0) then
         begin
           { Unsigned <0 or >=0 should not reach pass2, most likely }
           case left.location.loc of
             LOC_REFERENCE,
             LOC_CREFERENCE:
               begin
                 href:=left.location.reference;
                 tcg68k(cg).fixref(current_asmdata.CurrAsmList,href);
                 current_asmdata.CurrAsmList.concat(taicpu.op_ref(A_TST,opsize,href));
                 location_freetemp(current_asmdata.CurrAsmList,left.location);
               end;
           else
             hlcg.location_force_reg(current_asmdata.CurrAsmList,left.location,left.resultdef,left.resultdef,true);
             current_asmdata.CurrAsmList.concat(taicpu.op_reg(A_TST,opsize,left.location.register));
           end;
           location.resflags := getresflags(unsigned);
           exit;
         end;

       { Coldfire supports byte/word compares only starting with ISA_B,
         !!see remark about Qemu weirdness in tcg68k.a_cmp_const_reg_label }
       if (opsize<>S_L) and (current_settings.cputype in cpu_coldfire{-[cpu_isa_b,cpu_isa_c]}) then
         begin
           { 1) Extension is needed for LOC_REFERENCE, but what about LOC_REGISTER ? Perhaps after fixing cg we can assume
                that high bits of registers are correct.
             2) Assuming that extension depends only on source signedness --> destination OS_32 is acceptable. }
           hlcg.location_force_reg(current_asmdata.CurrAsmList,left.location,left.resultdef,cgsize_orddef(OS_32),false);
           if (right.location.loc<>LOC_CONSTANT) then
             hlcg.location_force_reg(current_asmdata.CurrAsmList,right.location,right.resultdef,cgsize_orddef(OS_32),false);
           opsize:=S_L;
         end
       else if not (left.location.loc in [LOC_REGISTER,LOC_CREGISTER]) then
         begin
           if not (right.location.loc in [LOC_REGISTER,LOC_CREGISTER]) then
             hlcg.location_force_reg(current_asmdata.CurrAsmList,left.location,left.resultdef,left.resultdef,true)
           else
             begin
               location_swap(left.location,right.location);
               toggleflag(nf_swapped);
             end;
         end;
       { left is now in register }
       case right.location.loc of
         LOC_CONSTANT:
           current_asmdata.CurrAsmList.concat(taicpu.op_const_reg(A_CMP,opsize,
             longint(right.location.value),left.location.register));
         LOC_REFERENCE,
         LOC_CREFERENCE:
           begin
             href:=right.location.reference;
             tcg68k(cg).fixref(current_asmdata.CurrAsmList,href);
             current_asmdata.CurrAsmList.concat(taicpu.op_ref_reg(A_CMP,opsize,href,
               left.location.register));
           end;
         LOC_REGISTER,
         LOC_CREGISTER:
           current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_CMP,opsize,
             right.location.register,left.location.register));
       else
         hlcg.location_force_reg(current_asmdata.CurrAsmList,right.location,right.resultdef,right.resultdef,true);
         current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_CMP,opsize,
           right.location.register,left.location.register));
       end;

       { update location because sides could have been swapped }
       location.resflags:=getresflags(unsigned);
     end;


{*****************************************************************************
                                64-bit
*****************************************************************************}

    procedure t68kaddnode.second_cmp64bit;
      var
        hlab: tasmlabel;
        unsigned : boolean;
        href: treference;

      procedure firstjmp64bitcmp;
        var
          oldnodetype : tnodetype;
        begin
          case nodetype of
            ltn,gtn:
              begin
                if (hlab<>current_procinfo.CurrTrueLabel) then
                  cg.a_jmp_flags(current_asmdata.CurrAsmList,getresflags(unsigned),current_procinfo.CurrTrueLabel);
                { cheat a little bit for the negative test }
                toggleflag(nf_swapped);
                if (hlab<>current_procinfo.CurrFalseLabel) then
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
                if (hlab<>current_procinfo.CurrTrueLabel) then
                  cg.a_jmp_flags(current_asmdata.CurrAsmList,getresflags(unsigned),current_procinfo.CurrTrueLabel);
                { cheat for the negative test }
                if nodetype=ltn then
                  nodetype:=gtn
                else
                  nodetype:=ltn;
                if (hlab<>current_procinfo.CurrFalseLabel) then
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
          case nodetype of
            ltn,gtn,lten,gten:
              begin
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
        { This puts constant operand (if any) to the right }
        pass_left_right;

        unsigned:=not(is_signed(left.resultdef)) or
                  not(is_signed(right.resultdef));

        location_reset(location,LOC_JUMP,OS_NO);

        { Relational compares against constants having low dword=0 can omit the
          second compare based on the fact that any unsigned value is >=0 }
        hlab:=nil;
        if (right.location.loc=LOC_CONSTANT) and
           (lo(right.location.value64)=0) then
          begin
            case getresflags(true) of
              F_AE: hlab:=current_procinfo.CurrTrueLabel;
              F_B:  hlab:=current_procinfo.CurrFalseLabel;
            end;
          end;

        if (right.location.loc=LOC_CONSTANT) and (right.location.value64=0) and
          (nodetype in [equaln,unequaln]) then
          begin
            case left.location.loc of
              LOC_REFERENCE,
              LOC_CREFERENCE:
                begin
                  href:=left.location.reference;
                  tcg68k(cg).fixref(current_asmdata.CurrAsmList,href);
                  current_asmdata.CurrAsmList.concat(taicpu.op_ref(A_TST,S_L,href));
                  firstjmp64bitcmp;
                  inc(href.offset,4);
                  current_asmdata.CurrAsmList.concat(taicpu.op_ref(A_TST,S_L,href));
                  secondjmp64bitcmp;
                  location_freetemp(current_asmdata.CurrAsmList,left.location);
                end;
            else
              hlcg.location_force_reg(current_asmdata.CurrAsmList,left.location,left.resultdef,left.resultdef,true);
              current_asmdata.CurrAsmList.concat(taicpu.op_reg(A_TST,S_L,left.location.register64.reglo));
              firstjmp64bitcmp;
              current_asmdata.CurrAsmList.concat(taicpu.op_reg(A_TST,S_L,left.location.register64.reghi));
              secondjmp64bitcmp;
            end;
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

        { left is now in register }
        case right.location.loc of
          LOC_REGISTER,LOC_CREGISTER:
            begin
              current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_CMP,S_L,right.location.register64.reghi,left.location.register64.reghi));
              firstjmp64bitcmp;
              current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_CMP,S_L,right.location.register64.reglo,left.location.register64.reglo));
              secondjmp64bitcmp;
            end;
          LOC_REFERENCE,LOC_CREFERENCE:
            begin
              href:=right.location.reference;
              tcg68k(cg).fixref(current_asmdata.CurrAsmList,href);
              current_asmdata.CurrAsmList.concat(taicpu.op_ref_reg(A_CMP,S_L,href,left.location.register64.reghi));
              firstjmp64bitcmp;
              inc(href.offset,4);
              current_asmdata.CurrAsmList.concat(taicpu.op_ref_reg(A_CMP,S_L,href,left.location.register64.reglo));
              secondjmp64bitcmp;
              location_freetemp(current_asmdata.CurrAsmList,right.location);
            end;
          LOC_CONSTANT:
            begin
              current_asmdata.CurrAsmList.concat(taicpu.op_const_reg(A_CMP,S_L,aint(hi(right.location.value64)),left.location.register64.reghi));
              firstjmp64bitcmp;
              if assigned(hlab) then
                cg.a_jmp_always(current_asmdata.CurrAsmList,hlab)
              else
                begin
                  current_asmdata.CurrAsmList.concat(taicpu.op_const_reg(A_CMP,S_L,aint(lo(right.location.value64)),left.location.register64.reglo));
                  secondjmp64bitcmp;
                end;
            end;
        else
          InternalError(2014072501);
        end;
      end;


begin
   caddnode:=t68kaddnode;
end.
