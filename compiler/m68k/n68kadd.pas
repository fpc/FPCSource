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
       node,nadd,ncgadd,cpubase;


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
          procedure second_cmpboolean;override;
       end;


implementation

    uses
      globtype,systems,
      cutils,verbose,globals,
      symconst,symdef,paramgr,
      aasmbase,aasmtai,aasmdata,aasmcpu,defutil,htypechk,
      cgbase,cpuinfo,pass_1,pass_2,regvars,
      cpupara,cgutils,procinfo,
      ncon,nset,
      ncgutil,tgobj,rgobj,rgcpu,cgobj,cg64f32;

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
                  end
                else
                  case nodetype of
                     ltn : getresflags:=F_L;
                     lten : getresflags:=F_LE;
                     gtn : getresflags:=F_G;
                     gten : getresflags:=F_GE;
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
                  end
                else
                  case nodetype of
                     ltn : getresflags:=F_B;
                     lten : getresflags:=F_BE;
                     gtn : getresflags:=F_A;
                     gten : getresflags:=F_AE;
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
        cmpop : boolean;
      begin
        pass_left_right;

        cmpop:=false;
        case nodetype of
          addn :
            op:=A_FADD;
          muln :
            op:=A_FMUL;
          subn :
            op:=A_FSUB;
          slashn :
            op:=A_FDIV;
          ltn,lten,gtn,gten,
          equaln,unequaln :
            begin
//              op:=A_FCMPO;
              cmpop:=true;
            end;
          else
            internalerror(200403182);
        end;

        // get the operands in the correct order, there are no special cases
        // here, everything is register-based
        if nf_swapped in flags then
          swapleftright;

        // put both operands in a register
        location_force_fpureg(current_asmdata.CurrAsmList,right.location,true);
        location_force_fpureg(current_asmdata.CurrAsmList,left.location,true);

        // initialize de result
        if not cmpop then
          begin
            location_reset(location,LOC_FPUREGISTER,def_cgsize(resultdef));
            if left.location.loc = LOC_FPUREGISTER then
              location.register := left.location.register
            else if right.location.loc = LOC_FPUREGISTER then
              location.register := right.location.register
            else
              location.register := cg.getfpuregister(current_asmdata.CurrAsmList,location.size);
          end
        else
         begin
           location_reset(location,LOC_FLAGS,OS_NO);
           // FIX ME!
//           location.resflags := getresflags;
         end;

        // emit the actual operation
        if not cmpop then
          begin
          {
            current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(op,
              location.register,left.location.register,
              right.location.register))
             }
          end
        else
          begin
{            current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(op,
              newreg(R_SPECIALREGISTER,location.resflags.cr,R_SUBNONE),left.location.register,right.location.register))}
          end;
      end;


    procedure t68kaddnode.second_cmpfloat;
      begin
        pass_left_right;

{
        if (nf_swapped in flags) then
          swapleftright;
}
        { force fpureg as location, left right doesn't matter
          as both will be in a fpureg }
        location_force_fpureg(current_asmdata.CurrAsmList,left.location,true);
        location_force_fpureg(current_asmdata.CurrAsmList,right.location,true);

        location_reset(location,LOC_FLAGS,OS_NO);
        location.resflags:=getresflags(true);
{
        if nodetype in [equaln,unequaln] then
          current_asmdata.CurrAsmList.concat(setoppostfix(taicpu.op_reg_reg(A_CMF,
             left.location.register,right.location.register),
             cgsize2fpuoppostfix[def_cgsize(resultdef)]))
        else
          current_asmdata.CurrAsmList.concat(setoppostfix(taicpu.op_reg_reg(A_CMFE,
             left.location.register,right.location.register),
             cgsize2fpuoppostfix[def_cgsize(resultdef)]));

        location_reset(location,LOC_FLAGS,OS_NO);
        location.resflags:=getresflags(false);
}
      end;




{*****************************************************************************
                                Smallsets
*****************************************************************************}

    procedure t68kaddnode.second_cmpsmallset;
     var
      tmpreg : tregister;
     begin
       location_reset(location,LOC_FLAGS,OS_NO);

       case nodetype of
          equaln,
          unequaln :
            begin
              {emit_compare(true);}
            end;
          lten,gten:
            begin
              If (not(nf_swapped in flags) and
                  (nodetype = lten)) or
                 ((nf_swapped in flags) and
                  (nodetype = gten)) then
                swapleftright;
              // now we have to check whether left >= right
              tmpreg := cg.getintregister(current_asmdata.CurrAsmList,OS_INT);
              if left.location.loc = LOC_CONSTANT then
                begin
                  cg.a_op_const_reg_reg(current_asmdata.CurrAsmList,OP_AND,OS_INT,
                    not(left.location.value),right.location.register,tmpreg);
                  current_asmdata.CurrAsmList.concat(taicpu.op_reg(A_TST,S_L,tmpreg));
                  // the two instructions above should be folded together by
                  // the peepholeoptimizer
                end
              else
                begin
                  if right.location.loc = LOC_CONSTANT then
                    begin
                      cg.a_load_const_reg(current_asmdata.CurrAsmList,OS_INT,
                        aword(right.location.value),tmpreg);
                      current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_AND,S_L,
                        tmpreg,left.location.register));
                    end
                  else
                    current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_AND,S_L,
                      right.location.register,left.location.register));
                end;
//              cg.ungetcpuregister(current_asmdata.CurrAsmList,tmpreg);
              location.resflags := getresflags(true);
            end;
          else
            internalerror(2002072701);
        end;


     end;


{*****************************************************************************
                                Ordinals
*****************************************************************************}

    procedure t68kaddnode.second_cmpordinal;
     var
      unsigned : boolean;
      useconst : boolean;
      tmpreg : tregister;
      op : tasmop;
     begin
//       writeln('second_cmpordinal');
       pass_left_right;
       { set result location }
       location_reset(location,LOC_JUMP,OS_NO);

       { load values into registers (except constants) }
       force_reg_left_right(true, false);

       { determine if the comparison will be unsigned }
       unsigned:=not(is_signed(left.resultdef)) or
                   not(is_signed(right.resultdef));

        // get the constant on the right if there is one
        if (left.location.loc = LOC_CONSTANT) then
          swapleftright;
        // can we use an immediate, or do we have to load the
        // constant in a register first?
        if (right.location.loc = LOC_CONSTANT) then
          begin
{$ifdef extdebug}
            if (right.location.size in [OS_64,OS_S64]) and (hi(right.location.value64)<>0) and ((hi(right.location.value64)<>-1) or unsigned) then
              internalerror(2002080301);
{$endif extdebug}
            if (nodetype in [equaln,unequaln]) then
              if (unsigned and
                  (right.location.value > high(word))) or
                 (not unsigned and
                  (longint(right.location.value) < low(smallint)) or
                   (longint(right.location.value) > high(smallint))) then
                { we can then maybe use a constant in the 'othersigned' case
                 (the sign doesn't matter for // equal/unequal)}
                unsigned := not unsigned;

            if (unsigned and
                ((right.location.value) <= high(word))) or
               (not(unsigned) and
                (longint(right.location.value) >= low(smallint)) and
                (longint(right.location.value) <= high(smallint))) then
               useconst := true
            else
              begin
                useconst := false;
                tmpreg := cg.getintregister(current_asmdata.CurrAsmList,OS_INT);
                cg.a_load_const_reg(current_asmdata.CurrAsmList,OS_INT,
                  aword(right.location.value),tmpreg);
               end
          end
        else
          useconst := false;
        location.loc := LOC_FLAGS;
        location.resflags := getresflags(unsigned);
        op := A_CMP;
        if (right.location.loc = LOC_CONSTANT) then
          if useconst then
            current_asmdata.CurrAsmList.concat(taicpu.op_reg_const(op,S_L,
              left.location.register,longint(right.location.value)))
          else
            begin
              current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(op,S_L,
                left.location.register,tmpreg));
//              cg.ungetcpuregister(current_asmdata.CurrAsmList,tmpreg);
            end
        else
          current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(op,S_L,
            left.location.register,right.location.register));
     end;

{*****************************************************************************
                                Boolean
*****************************************************************************}

    procedure t68kaddnode.second_cmpboolean;
      var
        cgop      : TOpCg;
        cgsize  : TCgSize;
        isjump  : boolean;
        otl,ofl : tasmlabel;
      begin
//        writeln('second_cmpboolean');
        if (torddef(left.resultdef).ordtype=bool8bit) or
           (torddef(right.resultdef).ordtype=bool8bit) then
         cgsize:=OS_8
        else
          if (torddef(left.resultdef).ordtype=bool16bit) or
             (torddef(right.resultdef).ordtype=bool16bit) then
           cgsize:=OS_16
        else
           cgsize:=OS_32;

        if (cs_full_boolean_eval in current_settings.localswitches) or
           (nodetype in [unequaln,ltn,lten,gtn,gten,equaln,xorn]) then
          begin
            if left.nodetype in [ordconstn,realconstn] then
             swapleftright;

            isjump:=(left.location.loc=LOC_JUMP);
            if isjump then
              begin
                 otl:=current_procinfo.CurrTrueLabel;
                 current_asmdata.getjumplabel(current_procinfo.CurrTrueLabel);
                 ofl:=current_procinfo.CurrFalseLabel;
                 current_asmdata.getjumplabel(current_procinfo.CurrFalseLabel);
              end;
            secondpass(left);
            if left.location.loc in [LOC_FLAGS,LOC_JUMP] then begin
//             writeln('ajjaj');
             location_force_reg(current_asmdata.CurrAsmList,left.location,cgsize,false);
//             writeln('reccs?');
            end;
            if isjump then
             begin
               current_procinfo.CurrTrueLabel:=otl;
               current_procinfo.CurrFalseLabel:=ofl;
             end;

            isjump:=(right.location.loc=LOC_JUMP);
            if isjump then
              begin
                 otl:=current_procinfo.CurrTrueLabel;
                 current_asmdata.getjumplabel(current_procinfo.CurrTrueLabel);
                 ofl:=current_procinfo.CurrFalseLabel;
                 current_asmdata.getjumplabel(current_procinfo.CurrFalseLabel);
              end;
            secondpass(right);
            if right.location.loc in [LOC_FLAGS,LOC_JUMP] then
             location_force_reg(current_asmdata.CurrAsmList,right.location,cgsize,false);
            if isjump then
             begin
               current_procinfo.CurrTrueLabel:=otl;
               current_procinfo.CurrFalseLabel:=ofl;
             end;

         location_reset(location,LOC_FLAGS,OS_NO);

         force_reg_left_right(true,false);

            if (left.location.loc = LOC_CONSTANT) then
              swapleftright;

         if (right.location.loc <> LOC_CONSTANT) then
           current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_CMP,S_L,
             left.location.register,right.location.register))
         else
           current_asmdata.CurrAsmList.concat(taicpu.op_const_reg(A_CMP,S_L,
             longint(right.location.value),left.location.register));
         location.resflags := getresflags(true);
        end;

        //release_reg_left_right;
      end;


{*****************************************************************************
                                64-bit
*****************************************************************************}

    procedure t68kaddnode.second_cmp64bit;
     begin
      // writeln('second_cmp64bit');
      pass_left_right;


//     load_left_right(true,false);
(*
        case nodetype of
          ltn,lten,
          gtn,gten:
           begin
             emit_cmp64_hi;
             firstjmp64bitcmp;
             emit_cmp64_lo;
             secondjmp64bitcmp;
           end;
          equaln,unequaln:
           begin
             // instead of doing a complicated compare, do
             // (left.hi xor right.hi) or (left.lo xor right.lo)
             // (somewhate optimized so that no superfluous 'mr's are
             //  generated)
                  if (left.location.loc = LOC_CONSTANT) then
                    swapleftright;
                  if (right.location.loc = LOC_CONSTANT) then
                    begin
                      if left.location.loc = LOC_REGISTER then
                        begin
                          tempreg64.reglo := left.location.register64.reglo;
                          tempreg64.reghi := left.location.register64.reghi;
                        end
                      else
                        begin
                          if (aword(right.location.valueqword) <> 0) then
                            tempreg64.reglo := cg.getintregister(current_asmdata.CurrAsmList)
                          else
                            tempreg64.reglo := left.location.register64.reglo;
                          if ((right.location.valueqword shr 32) <> 0) then
                            tempreg64.reghi := cg.getintregister(current_asmdata.CurrAsmList)
                          else
                            tempreg64.reghi := left.location.register64.reghi;
                        end;

                      if (aword(right.location.valueqword) <> 0) then
                        { negative values can be handled using SUB, }
                        { positive values < 65535 using XOR.        }
                        if (longint(right.location.valueqword) >= -32767) and
                           (longint(right.location.valueqword) < 0) then
                          cg.a_op_const_reg_reg(current_asmdata.CurrAsmList,OP_SUB,OS_INT,
                            aword(right.location.valueqword),
                            left.location.register64.reglo,tempreg64.reglo)
                        else
                          cg.a_op_const_reg_reg(current_asmdata.CurrAsmList,OP_XOR,OS_INT,
                            aword(right.location.valueqword),
                            left.location.register64.reglo,tempreg64.reglo);

                      if ((right.location.valueqword shr 32) <> 0) then
                        if (longint(right.location.valueqword shr 32) >= -32767) and
                           (longint(right.location.valueqword shr 32) < 0) then
                          cg.a_op_const_reg_reg(current_asmdata.CurrAsmList,OP_SUB,OS_INT,
                            aword(right.location.valueqword shr 32),
                            left.location.register64.reghi,tempreg64.reghi)
                        else
                          cg.a_op_const_reg_reg(current_asmdata.CurrAsmList,OP_XOR,OS_INT,
                            aword(right.location.valueqword shr 32),
                            left.location.register64.reghi,tempreg64.reghi);
                    end
                  else
                    begin
                       tempreg64.reglo := cg.getintregister(current_asmdata.CurrAsmList);
                       tempreg64.reghi := cg.getintregister(current_asmdata.CurrAsmList);
                       cg64.a_op64_reg_reg_reg(current_asmdata.CurrAsmList,OP_XOR,
                         left.location.register64,right.location.register64,
                         tempreg64);
                    end;

                  cg.a_reg_alloc(current_asmdata.CurrAsmList,R_0);
                  current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(A_OR_,R_0,
                    tempreg64.reglo,tempreg64.reghi));
                  cg.a_reg_dealloc(current_asmdata.CurrAsmList,R_0);
                  if (tempreg64.reglo <> left.location.register64.reglo) then
                    cg.ungetregister(current_asmdata.CurrAsmList,tempreg64.reglo);
                  if (tempreg64.reghi <> left.location.register64.reghi) then
                    cg.ungetregister(current_asmdata.CurrAsmList,tempreg64.reghi);

                  location_reset(location,LOC_FLAGS,OS_NO);
                  location.resflags := getresflags;
                end;
              else
                internalerror(2002072803);
            end;


        { set result location }
        { (emit_compare sets it to LOC_FLAGS for compares, so set the }
        {  real location only now) (JM)                               }
        if cmpop and
           not(nodetype in [equaln,unequaln]) then
          location_reset(location,LOC_JUMP,OS_NO);
*)
       location_reset(location,LOC_JUMP,OS_NO);
       // writeln('second_cmp64_exit');
     end;


begin
   caddnode:=t68kaddnode;
end.
