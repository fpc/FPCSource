{
    Copyright (c) 1998-2002 by Florian Klaempfl

    Generate x86 assembler for in/case nodes

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
unit nx86set;

{$i fpcdefs.inc}

interface

    uses
       node,nset,pass_1,ncgset;

    type

       tx86innode = class(tinnode)
          procedure pass_generate_code;override;
          function pass_1 : tnode;override;
       end;


implementation

    uses
      globtype,systems,
      verbose,globals,
      symconst,symdef,defutil,
      aasmbase,aasmtai,aasmdata,aasmcpu,
      cgbase,pass_2,tgobj,
      ncon,
      cpubase,
      cga,cgobj,cgutils,ncgutil,
      cgx86;

{*****************************************************************************
                              TX86INNODE
*****************************************************************************}

    function tx86innode.pass_1 : tnode;
      begin
         result:=nil;
         { this is the only difference from the generic version }
         expectloc:=LOC_FLAGS;

         firstpass(right);
         firstpass(left);
         if codegenerror then
           exit;

         left_right_max;
         { a smallset needs maybe an misc. register }
         if (left.nodetype<>ordconstn) and
            not(right.expectloc in [LOC_CREGISTER,LOC_REGISTER]) and
            (right.registersint<1) then
           inc(registersint);
      end;



    procedure tx86innode.pass_generate_code;
       type
         Tsetpart=record
           range : boolean;      {Part is a range.}
           start,stop : byte;    {Start/stop when range; Stop=element when an element.}
         end;
       var
         genjumps,
         use_small,
         ranges     : boolean;
         hreg,hreg2,
         pleftreg   : tregister;
         opsize     : tcgsize;
         setparts   : array[1..8] of Tsetpart;
         i,numparts : byte;
         adjustment : longint;
         l,l2       : tasmlabel;
{$ifdef CORRECT_SET_IN_FPC}
         AM         : tasmop;
{$endif CORRECT_SET_IN_FPC}

         function analizeset(Aset:pconstset;is_small:boolean):boolean;
           var
             compares,maxcompares:word;
             i:byte;
           begin
             if tnormalset(Aset^)=[] then
                {The expression...
                    if expr in []
                 ...is allways false. It should be optimized away in the
                 resultdef pass, and thus never occur here. Since we
                 do generate wrong code for it, do internalerror.}
                internalerror(2002072301);
             analizeset:=false;
             ranges:=false;
             numparts:=0;
             compares:=0;
             { Lots of comparisions take a lot of time, so do not allow
               too much comparisions. 8 comparisions are, however, still
               smalller than emitting the set }
             if cs_opt_size in current_settings.optimizerswitches then
               maxcompares:=8
             else
               maxcompares:=5;
             { when smallset is possible allow only 3 compares the smallset
               code is for littlesize also smaller when more compares are used }
             if is_small then
               maxcompares:=3;
             for i:=0 to 255 do
              if i in tnormalset(Aset^) then
               begin
                 if (numparts=0) or (i<>setparts[numparts].stop+1) then
                  begin
                  {Set element is a separate element.}
                    inc(compares);
                    if compares>maxcompares then
                         exit;
                    inc(numparts);
                    setparts[numparts].range:=false;
                    setparts[numparts].stop:=i;
                  end
                 else
                  {Set element is part of a range.}
                  if not setparts[numparts].range then
                   begin
                     {Transform an element into a range.}
                     setparts[numparts].range:=true;
                     setparts[numparts].start:=setparts[numparts].stop;
                     setparts[numparts].stop:=i;
                     ranges := true;
                     { there's only one compare per range anymore. Only a }
                     { sub is added, but that's much faster than a        }
                     { cmp/jcc combo so neglect its effect                }
{                     inc(compares);
                     if compares>maxcompares then
                      exit; }
                   end
                  else
                   begin
                    {Extend a range.}
                    setparts[numparts].stop:=i;
                   end;
              end;
             analizeset:=true;
           end;

       begin
         { We check first if we can generate jumps, this can be done
           because the resultdef is already set in firstpass }

         { check if we can use smallset operation using btl which is limited
           to 32 bits, the left side may also not contain higher values !! }
         use_small:=(tsetdef(right.resultdef).settype=smallset) and
                    ((left.resultdef.typ=orddef) and (torddef(left.resultdef).high<=32) or
                     (left.resultdef.typ=enumdef) and (tenumdef(left.resultdef).max<=32));

         { Can we generate jumps? Possible for all types of sets }
         genjumps:=(right.nodetype=setconstn) and
                   analizeset(tsetconstnode(right).value_set,use_small);
         { calculate both operators }
         { the complex one first }
         firstcomplex(self);
         secondpass(left);
         { Only process the right if we are not generating jumps }
         if not genjumps then
          begin
            secondpass(right);
          end;
         if codegenerror then
          exit;

         { ofcourse not commutative }
         if nf_swapped in flags then
          swapleftright;

         if not(left.location.loc in [LOC_REGISTER,LOC_CREGISTER,LOC_REFERENCE,LOC_CREFERENCE]) then
           location_force_reg(current_asmdata.CurrAsmList,left.location,OS_INT,true);
         if genjumps then
          begin
            { It gives us advantage to check for the set elements
              separately instead of using the SET_IN_BYTE procedure.
              To do: Build in support for LOC_JUMP }

            opsize := def_cgsize(left.resultdef);
            { If register is used, use only lower 8 bits }
            if left.location.loc in [LOC_REGISTER,LOC_CREGISTER] then
             begin
               { for ranges we always need a 32bit register, because then we }
               { use the register as base in a reference (JM)                }
               if ranges then
                 begin
                   pleftreg:=cg.makeregsize(current_asmdata.CurrAsmList,left.location.register,OS_INT);
                   cg.a_load_reg_reg(current_asmdata.CurrAsmList,left.location.size,OS_INT,left.location.register,pleftreg);
                   if opsize<>OS_INT then
                     cg.a_op_const_reg(current_asmdata.CurrAsmList,OP_AND,OS_INT,255,pleftreg);
                   opsize:=OS_INT;
                 end
               else
                 { otherwise simply use the lower 8 bits (no "and" }
                 { necessary this way) (JM)                        }
                 begin
                   pleftreg:=cg.makeregsize(current_asmdata.CurrAsmList,left.location.register,OS_8);
                   opsize := OS_8;
                 end;
             end
            else
             begin
               { load the value in a register }
               pleftreg:=cg.getintregister(current_asmdata.CurrAsmList,OS_32);
               opsize:=OS_32;
               cg.a_load_ref_reg(current_asmdata.CurrAsmList,OS_8,OS_32,left.location.reference,pleftreg);
             end;

            { Get a label to jump to the end }
            location_reset(location,LOC_FLAGS,OS_NO);

            { It's better to use the zero flag when there are
              no ranges }
            if ranges then
              location.resflags:=F_C
            else
              location.resflags:=F_E;

            current_asmdata.getjumplabel(l);

            { how much have we already substracted from the x in the }
            { "x in [y..z]" expression                               }
            adjustment := 0;

            for i:=1 to numparts do
             if setparts[i].range then
              { use fact that a <= x <= b <=> cardinal(x-a) <= cardinal(b-a) }
              begin
                { is the range different from all legal values? }
                if (setparts[i].stop-setparts[i].start <> 255) then
                  begin
                    { yes, is the lower bound <> 0? }
                    if (setparts[i].start <> 0) then
                      begin
                        if (left.location.loc = LOC_CREGISTER) then
                          begin
                            hreg:=cg.getintregister(current_asmdata.CurrAsmList,OS_INT);
                            cg.a_load_reg_reg(current_asmdata.CurrAsmList,opsize,OS_INT,pleftreg,hreg);
                            pleftreg:=hreg;
                            opsize:=OS_INT;
                          end;
                        cg.a_op_const_reg(current_asmdata.CurrAsmList,OP_SUB,opsize,setparts[i].start-adjustment,pleftreg);
                      end;

                    { new total value substracted from x:           }
                    { adjustment + (setparts[i].start - adjustment) }
                    adjustment := setparts[i].start;

                    { check if result < b-a+1 (not "result <= b-a", since }
                    { we need a carry in case the element is in the range }
                    { (this will never overflow since we check at the     }
                    { beginning whether stop-start <> 255)                }
                    cg.a_cmp_const_reg_label(current_asmdata.CurrAsmList,opsize,OC_B,setparts[i].stop-setparts[i].start+1,pleftreg,l);
                  end
                else
                  { if setparts[i].start = 0 and setparts[i].stop = 255,  }
                  { it's always true since "in" is only allowed for bytes }
                  begin
                    current_asmdata.CurrAsmList.concat(taicpu.op_none(A_STC,S_NO));
                    cg.a_jmp_always(current_asmdata.CurrAsmList,l);
                  end;
              end
             else
              begin
                { Emit code to check if left is an element }
                current_asmdata.CurrAsmList.concat(taicpu.op_const_reg(A_CMP,TCGSize2OpSize[opsize],setparts[i].stop-adjustment,
                  pleftreg));
                { Result should be in carry flag when ranges are used }
                if ranges then
                  current_asmdata.CurrAsmList.concat(taicpu.op_none(A_STC,S_NO));
                { If found, jump to end }
                cg.a_jmp_flags(current_asmdata.CurrAsmList,F_E,l);
              end;
             if ranges and
                { if the last one was a range, the carry flag is already }
                { set appropriately                                      }
                not(setparts[numparts].range) then
               current_asmdata.CurrAsmList.concat(taicpu.op_none(A_CLC,S_NO));
             { To compensate for not doing a second pass }
             right.location.reference.symbol:=nil;
             { Now place the end label }
             cg.a_label(current_asmdata.CurrAsmList,l);
          end
         else
          begin
            location_reset(location,LOC_FLAGS,OS_NO);

            { We will now generated code to check the set itself, no jmps,
              handle smallsets separate, because it allows faster checks }
            if use_small then
             begin
               if left.nodetype=ordconstn then
                begin
                  location.resflags:=F_NE;
                  case right.location.loc of
                    LOC_REGISTER,
                    LOC_CREGISTER:
                      begin
                         emit_const_reg(A_TEST,S_L,
                           1 shl (tordconstnode(left).value and 31),right.location.register);
                      end;
                    LOC_REFERENCE,
                    LOC_CREFERENCE :
                      begin
                        emit_const_ref(A_TEST,S_L,1 shl (tordconstnode(left).value and 31),
                           right.location.reference);
                      end;
                    else
                      internalerror(200203312);
                  end;
                end
               else
                begin
                  case left.location.loc of
                     LOC_REGISTER,
                     LOC_CREGISTER:
                       begin
                          hreg:=cg.makeregsize(current_asmdata.CurrAsmList,left.location.register,OS_32);
                          cg.a_load_reg_reg(current_asmdata.CurrAsmList,left.location.size,OS_32,left.location.register,hreg);
                       end;
                  else
                    begin
                      { the set element isn't never samller than a byte
                        and because it's a small set we need only 5 bits
                        but 8 bits are easier to load                    }
                      hreg:=cg.getintregister(current_asmdata.CurrAsmList,OS_32);
                      cg.a_load_ref_reg(current_asmdata.CurrAsmList,OS_8,OS_32,left.location.reference,hreg);
                    end;
                  end;

                  case right.location.loc of
                    LOC_REGISTER,
                    LOC_CREGISTER :
                      begin
                        emit_reg_reg(A_BT,S_L,hreg,right.location.register);
                      end;
                     LOC_CONSTANT :
                       begin
                         { We have to load the value into a register because
                            btl does not accept values only refs or regs (PFV) }
                         hreg2:=cg.getintregister(current_asmdata.CurrAsmList,OS_32);
                         cg.a_load_const_reg(current_asmdata.CurrAsmList,OS_32,right.location.value,hreg2);
                         emit_reg_reg(A_BT,S_L,hreg,hreg2);
                       end;
                     LOC_CREFERENCE,
                     LOC_REFERENCE :
                       begin
                         emit_reg_ref(A_BT,S_L,hreg,right.location.reference);
                       end;
                     else
                       internalerror(2002032210);
                  end;
                  location.resflags:=F_C;
                end;
             end
            else
             begin
               if right.location.loc=LOC_CONSTANT then
                begin
                  location.resflags:=F_C;
                  current_asmdata.getjumplabel(l);
                  current_asmdata.getjumplabel(l2);

                  { load constants to a register }
                  if left.nodetype=ordconstn then
                    location_force_reg(current_asmdata.CurrAsmList,left.location,OS_INT,true);

                  case left.location.loc of
                     LOC_REGISTER,
                     LOC_CREGISTER:
                       begin
                          hreg:=cg.makeregsize(current_asmdata.CurrAsmList,left.location.register,OS_32);
                          cg.a_load_reg_reg(current_asmdata.CurrAsmList,left.location.size,OS_32,left.location.register,hreg);
                          cg.a_cmp_const_reg_label(current_asmdata.CurrAsmList,OS_32,OC_BE,31,hreg,l);
                          { reset carry flag }
                          current_asmdata.CurrAsmList.concat(taicpu.op_none(A_CLC,S_NO));
                          cg.a_jmp_always(current_asmdata.CurrAsmList,l2);
                          cg.a_label(current_asmdata.CurrAsmList,l);
                          { We have to load the value into a register because
                            btl does not accept values only refs or regs (PFV) }
                          hreg2:=cg.getintregister(current_asmdata.CurrAsmList,OS_32);
                          cg.a_load_const_reg(current_asmdata.CurrAsmList,OS_32,right.location.value,hreg2);
                          emit_reg_reg(A_BT,S_L,hreg,hreg2);
                       end;
                  else
                    begin
{$ifdef CORRECT_SET_IN_FPC}
                          if m_tp in current_settings.modeswitches then
                            begin
                              {***WARNING only correct if
                                reference is 32 bits (PM) *****}
                               emit_const_ref(A_CMP,S_L,31,reference_copy(left.location.reference));
                            end
                          else
{$endif CORRECT_SET_IN_FPC}
                            begin
                               emit_const_ref(A_CMP,S_B,31,left.location.reference);
                            end;
                       cg.a_jmp_flags(current_asmdata.CurrAsmList,F_BE,l);
                       { reset carry flag }
                       current_asmdata.CurrAsmList.concat(taicpu.op_none(A_CLC,S_NO));
                       cg.a_jmp_always(current_asmdata.CurrAsmList,l2);
                       cg.a_label(current_asmdata.CurrAsmList,l);
                       hreg:=cg.getintregister(current_asmdata.CurrAsmList,OS_32);
                       cg.a_load_ref_reg(current_asmdata.CurrAsmList,OS_32,OS_32,left.location.reference,hreg);
                       { We have to load the value into a register because
                         btl does not accept values only refs or regs (PFV) }
                       hreg2:=cg.getintregister(current_asmdata.CurrAsmList,OS_32);
                       cg.a_load_const_reg(current_asmdata.CurrAsmList,OS_32,right.location.value,hreg2);
                       emit_reg_reg(A_BT,S_L,hreg,hreg2);
                    end;
                  end;
                  cg.a_label(current_asmdata.CurrAsmList,l2);
                end { of right.location.loc=LOC_CONSTANT }
               { do search in a normal set which could have >32 elementsm
                 but also used if the left side contains higher values > 32 }
               else if left.nodetype=ordconstn then
                begin
                  location.resflags:=F_NE;
                  inc(right.location.reference.offset,tordconstnode(left).value shr 3);
                  emit_const_ref(A_TEST,S_B,1 shl (tordconstnode(left).value and 7),right.location.reference);
                end
               else
                begin
                  if (left.location.loc=LOC_REGISTER) then
                    pleftreg:=cg.makeregsize(current_asmdata.CurrAsmList,left.location.register,OS_32)
                  else
                    pleftreg:=cg.getintregister(current_asmdata.CurrAsmList,OS_32);
                  cg.a_load_loc_reg(current_asmdata.CurrAsmList,OS_32,left.location,pleftreg);
                  location_freetemp(current_asmdata.CurrAsmList,left.location);
                  emit_reg_ref(A_BT,S_L,pleftreg,right.location.reference);
                  { tg.ungetiftemp(current_asmdata.CurrAsmList,right.location.reference) happens below }
                  location.resflags:=F_C;
                end;
             end;
          end;
          if not genjumps then
            location_freetemp(current_asmdata.CurrAsmList,right.location);
       end;

begin
   cinnode:=tx86innode;
end.
