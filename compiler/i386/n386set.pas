{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

    Generate i386 assembler for in set/case nodes

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
unit n386set;

{$i fpcdefs.inc}

interface

    uses
       node,nset,pass_1,ncgset;

    type

       ti386innode = class(tinnode)
          procedure pass_2;override;
          function pass_1 : tnode;override;
       end;

       ti386casenode = class(tcgcasenode)
          procedure optimizevalues(var max_linear_list:longint;var max_dist:cardinal);override;
          function  has_jumptable : boolean;override;
          procedure genjumptable(hp : pcaserecord;min_,max_ : longint);override;
          procedure genlinearlist(hp : pcaserecord);override;
       end;


implementation

    uses
      globtype,systems,
      verbose,globals,
      symconst,symdef,defutil,
      aasmbase,aasmtai,aasmcpu,
      cginfo,cgbase,pass_2,
      ncon,
      cpubase,cpuinfo,
      cga,cgx86,cgobj,tgobj,ncgutil,rgobj;


{*****************************************************************************
                              TI386INNODE
*****************************************************************************}

    function ti386innode.pass_1 : tnode;
      begin
         result:=nil;
         { this is the only difference from the generic version }
         expectloc:=LOC_FLAGS;

         firstpass(right);
         firstpass(left);
         if codegenerror then
           exit;

         left_right_max;
         { this is not allways true due to optimization }
         { but if we don't set this we get problems with optimizing self code }
         if tsetdef(right.resulttype.def).settype<>smallset then
           procinfo.flags:=procinfo.flags or pi_do_call
         else
           begin
              { a smallset needs maybe an misc. register }
              if (left.nodetype<>ordconstn) and
                not(right.location.loc in [LOC_CREGISTER,LOC_REGISTER]) and
                (right.registers32<1) then
                inc(registers32);
           end;
      end;



    procedure ti386innode.pass_2;
       type
         Tsetpart=record
           range : boolean;      {Part is a range.}
           start,stop : byte;    {Start/stop when range; Stop=element when an element.}
         end;
       var
         genjumps,
         use_small,
         ranges     : boolean;
         hr,hr2,
         pleftreg   : tregister;
         href       : treference;
         opsize     : topsize;
         setparts   : array[1..8] of Tsetpart;
         i,numparts : byte;
         adjustment : longint;
         pushedregs : tmaybesave;
         l,l2       : tasmlabel;
         r          : Tregister;
{$ifdef CORRECT_SET_IN_FPC}
         AM         : tasmop;
{$endif CORRECT_SET_IN_FPC}

         function analizeset(Aset:pconstset;is_small:boolean):boolean;
           type
             byteset=set of byte;
           var
             compares,maxcompares:word;
             i:byte;
           begin
             if byteset(Aset^)=[] then
                {The expression...
                    if expr in []
                 ...is allways false. It should be optimized away in the
                 resulttype pass, and thus never occur here. Since we
                 do generate wrong code for it, do internalerror.}
                internalerror(2002072301);
             analizeset:=false;
             ranges:=false;
             numparts:=0;
             compares:=0;
             { Lots of comparisions take a lot of time, so do not allow
               too much comparisions. 8 comparisions are, however, still
               smalller than emitting the set }
             if cs_littlesize in aktglobalswitches then
              maxcompares:=8
             else
              maxcompares:=5;
             { when smallset is possible allow only 3 compares the smallset
               code is for littlesize also smaller when more compares are used }
             if is_small then
              maxcompares:=3;
             for i:=0 to 255 do
              if i in byteset(Aset^) then
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
           because the resulttype.def is already set in firstpass }

         { check if we can use smallset operation using btl which is limited
           to 32 bits, the left side may also not contain higher values !! }
         use_small:=(tsetdef(right.resulttype.def).settype=smallset) and
                    ((left.resulttype.def.deftype=orddef) and (torddef(left.resulttype.def).high<=32) or
                     (left.resulttype.def.deftype=enumdef) and (tenumdef(left.resulttype.def).max<=32));

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
          {$ifndef newra}
            maybe_save(exprasmlist,right.registers32,left.location,pushedregs);
          {$endif}
            secondpass(right);
          {$ifndef newra}
            maybe_restore(exprasmlist,left.location,pushedregs);
          {$endif newra}
          end;
         if codegenerror then
          exit;

         { ofcourse not commutative }
         if nf_swaped in flags then
          swapleftright;

         if genjumps then
          begin
            { It gives us advantage to check for the set elements
              separately instead of using the SET_IN_BYTE procedure.
              To do: Build in support for LOC_JUMP }

            opsize := def_opsize(left.resulttype.def);
            { If register is used, use only lower 8 bits }
            if left.location.loc in [LOC_REGISTER,LOC_CREGISTER] then
             begin
               { for ranges we always need a 32bit register, because then we }
               { use the register as base in a reference (JM)                }
               pleftreg.enum:=R_INTREGISTER;
               if ranges then
                 begin
                   pleftreg.number:=(left.location.register.number and not $ff) or R_SUBWHOLE;
                   cg.a_load_reg_reg(exprasmlist,left.location.size,OS_INT,left.location.register,pleftreg);
                   if opsize <> S_L then
                     emit_const_reg(A_AND,S_L,255,pleftreg);
                   opsize := S_L;
                 end
               else
                 { otherwise simply use the lower 8 bits (no "and" }
                 { necessary this way) (JM)                        }
                 begin
                   pleftreg.number:=(left.location.register.number and not $ff) or R_SUBL;
                   opsize := S_B;
                 end;
             end
            else
             begin
               { load the value in a register }
             {$ifdef newra}
               pleftreg:=rg.getregisterint(exprasmlist,OS_INT);
             {$else}
               pleftreg := rg.getexplicitregisterint(exprasmlist,NR_EDI);
             {$endif}
               opsize := S_L;
               emit_ref_reg(A_MOVZX,S_BL,left.location.reference,pleftreg);
               location_release(exprasmlist,left.location);
             end;

            { Get a label to jump to the end }
            location_reset(location,LOC_FLAGS,OS_NO);

            { It's better to use the zero flag when there are
              no ranges }
            if ranges then
              location.resflags:=F_C
            else
              location.resflags:=F_E;

            objectlibrary.getlabel(l);

            { how much have we already substracted from the x in the }
            { "x in [y..z]" expression                               }
            adjustment := 0;

            r.enum:=R_NO;
            for i:=1 to numparts do
             if setparts[i].range then
              { use fact that a <= x <= b <=> cardinal(x-a) <= cardinal(b-a) }
              begin
                { is the range different from all legal values? }
                if (setparts[i].stop-setparts[i].start <> 255) then
                  begin
                    { yes, is the lower bound <> 0? }
                    if (setparts[i].start <> 0) then
                      { we're going to substract from the left register,   }
                      { so in case of a LOC_CREGISTER first move the value }
                      { to edi (not done before because now we can do the  }
                      { move and substract in one instruction with LEA)    }
                      if {$ifndef newra}(pleftreg.number <> NR_EDI) and{$endif}
                         (left.location.loc = LOC_CREGISTER) then
                        begin
                          rg.ungetregister(exprasmlist,pleftreg);
                        {$ifdef newra}
                          r:=rg.getregisterint(exprasmlist,OS_INT);
                        {$else}
                          r.enum:=R_INTREGISTER;
                          r.number:=NR_EDI;
                          rg.getexplicitregisterint(exprasmlist,NR_EDI);
                        {$endif}
                          reference_reset_base(href,pleftreg,-setparts[i].start);
                          emit_ref_reg(A_LEA,S_L,href,r);
                          { only now change pleftreg since previous value is }
                          { still used in previous instruction               }
                          pleftreg := r;
                          opsize := S_L;
                        end
                      else
                        begin
                          { otherwise, the value is already in a register   }
                          { that can be modified                            }
                          if setparts[i].start-adjustment <> 1 then
                            emit_const_reg(A_SUB,opsize,
                              setparts[i].start-adjustment,pleftreg)
                          else emit_reg(A_DEC,opsize,pleftreg);
                        end;
                    { new total value substracted from x:           }
                    { adjustment + (setparts[i].start - adjustment) }
                    adjustment := setparts[i].start;

                    { check if result < b-a+1 (not "result <= b-a", since }
                    { we need a carry in case the element is in the range }
                    { (this will never overflow since we check at the     }
                    { beginning whether stop-start <> 255)                }
                    emit_const_reg(A_CMP,opsize,
                      setparts[i].stop-setparts[i].start+1,pleftreg);
                    { use C_C instead of C_B: the meaning is the same, but }
                    { then the optimizer can easier trace the jump to its  }
                    { final destination since the resultflag of this node  }
                    { is set to the carryflag                              }
                    emitjmp(C_C,l);
                  end
                else
                  { if setparts[i].start = 0 and setparts[i].stop = 255,  }
                  { it's always true since "in" is only allowed for bytes }
                  begin
                    emit_none(A_STC,S_NO);
                    cg.a_jmp_always(exprasmlist,l);
                  end;
              end
             else
              begin
                { Emit code to check if left is an element }
                emit_const_reg(A_CMP,opsize,setparts[i].stop-adjustment,
                  pleftreg);
                { Result should be in carry flag when ranges are used }
                if ranges then
                 emit_none(A_STC,S_NO);
                { If found, jump to end }
                emitjmp(C_E,l);
              end;
             if ranges and
                { if the last one was a range, the carry flag is already }
                { set appropriately                                      }
                not(setparts[numparts].range) then
              emit_none(A_CLC,S_NO);
             { To compensate for not doing a second pass }
             right.location.reference.symbol:=nil;
             { Now place the end label }
             cg.a_label(exprasmlist,l);
          {$ifdef newra}
             rg.ungetregisterint(exprasmlist,pleftreg);
             if r.enum=R_INTREGISTER then
              rg.ungetregisterint(exprasmlist,r);
          {$else}
             case left.location.loc of
               LOC_REGISTER,
               LOC_CREGISTER :
                 rg.ungetregisterint(exprasmlist,pleftreg);
               else
                 begin
                   reference_release(exprasmlist,left.location.reference);
                   r.enum:=R_INTREGISTER;
                   r.number:=NR_EDI;
                   rg.ungetregisterint(exprasmlist,r);
                 end;
             end;
          {$endif}
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
                  location_release(exprasmlist,right.location);
                end
               else
                begin
                  case left.location.loc of
                     LOC_REGISTER,
                     LOC_CREGISTER:
                       begin
                          hr.enum:=R_INTREGISTER;
                          hr.number:=(left.location.register.number and not $ff) or R_SUBWHOLE;
                          cg.a_load_reg_reg(exprasmlist,left.location.size,OS_INT,left.location.register,hr);
                       end;
                  else
                    begin
                      { the set element isn't never samller than a byte  }
                      { and because it's a small set we need only 5 bits }
                      { but 8 bits are easier to load               }
                      r.enum:=R_INTREGISTER;
                      r.number:=NR_EDI;
                      rg.getexplicitregisterint(exprasmlist,NR_EDI);
                      emit_ref_reg(A_MOVZX,S_BL,left.location.reference,r);
                      hr:=r;
                      location_release(exprasmlist,left.location);
                    end;
                  end;

                  case right.location.loc of
                 LOC_REGISTER,
                LOC_CREGISTER :
                          begin
                            emit_reg_reg(A_BT,S_L,hr,
                              right.location.register);
                            rg.ungetregisterint(exprasmlist,right.location.register);
                          end;
                   LOC_CONSTANT :
                       begin
                       { We have to load the value into a register because
                         btl does not accept values only refs or regs (PFV) }
                         hr2:=rg.getregisterint(exprasmlist,OS_INT);
                         emit_const_reg(A_MOV,S_L,
                           right.location.value,hr2);
                         emit_reg_reg(A_BT,S_L,hr,hr2);
                         rg.ungetregisterint(exprasmlist,hr2);
                       end;
                   LOC_CREFERENCE,
                   LOC_REFERENCE :
                       begin
                         location_release(exprasmlist,right.location);
                         emit_reg_ref(A_BT,S_L,hr,right.location.reference);
                       end;
                     else
                       internalerror(2002032210);
                  end;
                  { simply to indicate EDI is deallocated here too (JM) }
                  rg.ungetregisterint(exprasmlist,hr);
                  location.resflags:=F_C;
                end;
             end
            else
             begin
               if right.location.loc=LOC_CONSTANT then
                begin
                  location.resflags:=F_C;
                  objectlibrary.getlabel(l);
                  objectlibrary.getlabel(l2);

                  { load constants to a register }
                  if left.nodetype=ordconstn then
                    location_force_reg(exprasmlist,left.location,OS_INT,true);

                  case left.location.loc of
                     LOC_REGISTER,
                     LOC_CREGISTER:
                       begin
                          hr:=rg.makeregsize(left.location.register,OS_INT);
                          cg.a_load_reg_reg(exprasmlist,left.location.size,OS_INT,left.location.register,hr);
                          emit_const_reg(A_CMP,S_L,31,hr);
                          emitjmp(C_NA,l);
                        { reset carry flag }
                          emit_none(A_CLC,S_NO);
                          cg.a_jmp_always(exprasmlist,l2);
                          cg.a_label(exprasmlist,l);
                        { We have to load the value into a register because
                          btl does not accept values only refs or regs (PFV) }
                          hr2:=rg.getregisterint(exprasmlist,OS_INT);
                          emit_const_reg(A_MOV,S_L,right.location.value,hr2);
                          emit_reg_reg(A_BT,S_L,hr,hr2);
                          rg.ungetregisterint(exprasmlist,hr2);
                       end;
                  else
                    begin
{$ifdef CORRECT_SET_IN_FPC}
                          if m_tp in aktmodeswitches then
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
                       emitjmp(C_NA,l);
                     { reset carry flag }
                       emit_none(A_CLC,S_NO);
                       cg.a_jmp_always(exprasmlist,l2);
                       cg.a_label(exprasmlist,l);
                       location_release(exprasmlist,left.location);
                       hr:=rg.getregisterint(exprasmlist,OS_INT);
                       emit_ref_reg(A_MOV,S_L,left.location.reference,hr);
                     { We have to load the value into a register because
                       btl does not accept values only refs or regs (PFV) }
                       hr2:=rg.getregisterint(exprasmlist,OS_INT);
                       emit_const_reg(A_MOV,S_L,
                         right.location.value,hr2);
                       emit_reg_reg(A_BT,S_L,hr,hr2);
                       rg.ungetregisterint(exprasmlist,hr2);
                    end;
                  end;
                  cg.a_label(exprasmlist,l2);
                end { of right.location.loc=LOC_CONSTANT }
               { do search in a normal set which could have >32 elementsm
                 but also used if the left side contains higher values > 32 }
               else if left.nodetype=ordconstn then
                begin
                  location.resflags:=F_NE;
                  inc(right.location.reference.offset,tordconstnode(left).value shr 3);
                  emit_const_ref(A_TEST,S_B,1 shl (tordconstnode(left).value and 7),right.location.reference);
                  location_release(exprasmlist,right.location);
                end
               else
                begin
                  if (left.location.loc in [LOC_REGISTER,LOC_CREGISTER]) then
                    begin
                      pleftreg.enum:=R_INTREGISTER;
                      pleftreg.number:=(left.location.register.number and not $ff) or R_SUBWHOLE;
                    end
                  else
                  {$ifdef newra}
                    pleftreg:=rg.getregisterint(exprasmlist,OS_INT);
                  {$else}
                    pleftreg:=rg.getexplicitregisterint(exprasmlist,NR_EDI);
                  {$endif}
                  cg.a_load_loc_reg(exprasmlist,left.location,pleftreg);
                  location_freetemp(exprasmlist,left.location);
                  location_release(exprasmlist,left.location);
                  emit_reg_ref(A_BT,S_L,pleftreg,right.location.reference);
                  rg.ungetregisterint(exprasmlist,pleftreg);
                  location_release(exprasmlist,right.location);
                  { tg.ungetiftemp(exprasmlist,right.location.reference) happens below }
                  location.resflags:=F_C;
                end;
             end;
          end;
          if not genjumps then
            location_freetemp(exprasmlist,right.location);
       end;


{*****************************************************************************
                            TI386CASENODE
*****************************************************************************}

    procedure ti386casenode.optimizevalues(var max_linear_list:longint;var max_dist:cardinal);
      begin
        { a jump table crashes the pipeline! }
        if aktoptprocessor=Class386 then
          inc(max_linear_list,3)
        else if aktoptprocessor=ClassP5 then
          inc(max_linear_list,6)
        else if aktoptprocessor>=ClassP6 then
          inc(max_linear_list,9);
      end;


    function ti386casenode.has_jumptable : boolean;
      begin
        has_jumptable:=true;
      end;


    procedure ti386casenode.genjumptable(hp : pcaserecord;min_,max_ : longint);
      var
        table : tasmlabel;
        last : TConstExprInt;
        indexreg : tregister;
        href : treference;
        jumpsegment : TAAsmOutput;

        procedure genitem(t : pcaserecord);
          var
            i : longint;
          begin
            if assigned(t^.less) then
              genitem(t^.less);
            { fill possible hole }
            for i:=last+1 to t^._low-1 do
              jumpSegment.concat(Tai_const_symbol.Create(elselabel));
            for i:=t^._low to t^._high do
              jumpSegment.concat(Tai_const_symbol.Create(t^.statement));
            last:=t^._high;
            if assigned(t^.greater) then
              genitem(t^.greater);
          end;

      begin
        if (cs_create_smart in aktmoduleswitches) then
          jumpsegment:=procinfo.aktlocaldata
        else
          jumpsegment:=datasegment;
        if not(jumptable_no_range) then
          begin
             { case expr less than min_ => goto elselabel }
             cg.a_cmp_const_reg_label(exprasmlist,OS_INT,jmp_lt,aword(min_),hregister,elselabel);
             { case expr greater than max_ => goto elselabel }
             cg.a_cmp_const_reg_label(exprasmlist,OS_INT,jmp_gt,aword(max_),hregister,elselabel);
          end;
        objectlibrary.getlabel(table);
        { make it a 32bit register }
        indexreg.enum:=R_INTREGISTER;
        indexreg.number:=(hregister.number and not $ff) or R_SUBWHOLE;
        cg.a_load_reg_reg(exprasmlist,opsize,OS_INT,hregister,indexreg);
        { create reference }
        reference_reset_symbol(href,table,0);
        href.offset:=(-longint(min_))*4;
        href.index:=indexreg;
        href.scalefactor:=4;
        emit_ref(A_JMP,S_NO,href);
        { generate jump table }
        if not(cs_littlesize in aktglobalswitches) then
          jumpSegment.concat(Tai_Align.Create_Op(4,0));
        jumpSegment.concat(Tai_label.Create(table));
        last:=min_;
        genitem(hp);
      end;


    procedure ti386casenode.genlinearlist(hp : pcaserecord);
      var
        first : boolean;
        lastrange : boolean;
        last : TConstExprInt;
        cond_lt,cond_le : tasmcond;

        procedure genitem(t : pcaserecord);
          begin
             if assigned(t^.less) then
               genitem(t^.less);
             { need we to test the first value }
             if first and (t^._low>get_min_value(left.resulttype.def)) then
               begin
                 cg.a_cmp_const_reg_label(exprasmlist,OS_INT,jmp_lt,aword(t^._low),hregister,elselabel);
               end;
             if t^._low=t^._high then
               begin
                  if t^._low-last=0 then
                    cg.a_cmp_const_reg_label(exprasmlist, OS_INT, OC_EQ,0,hregister,t^.statement)
                  else
                    begin
                      cg.a_op_const_reg(exprasmlist, OP_SUB, aword(t^._low-last), hregister);
                      emitjmp(C_Z,t^.statement);
                    end;
                  last:=t^._low;
                  lastrange:=false;
               end
             else
               begin
                  { it begins with the smallest label, if the value }
                  { is even smaller then jump immediately to the    }
                  { ELSE-label                                }
                  if first then
                    begin
                       { have we to ajust the first value ? }
                       if (t^._low>get_min_value(left.resulttype.def)) then
                         cg.a_op_const_reg(exprasmlist, OP_SUB, longint(t^._low), hregister);
                    end
                  else
                    begin
                      { if there is no unused label between the last and the }
                      { present label then the lower limit can be checked    }
                      { immediately. else check the range in between:       }

                      cg.a_op_const_reg(exprasmlist, OP_SUB, longint(t^._low-last), hregister);
                      { no jump necessary here if the new range starts at }
                      { at the value following the previous one           }
                      if ((t^._low-last) <> 1) or
                         (not lastrange) then
                        emitjmp(cond_lt,elselabel);
                    end;
                  {we need to use A_SUB, because A_DEC does not set the correct flags, therefor
                   using a_op_const_reg(OP_SUB) is not possible }
                  emit_const_reg(A_SUB,TCGSize2OpSize[opsize],longint(t^._high-t^._low),hregister);
                  emitjmp(cond_le,t^.statement);
                  last:=t^._high;
                  lastrange:=true;
               end;
             first:=false;
             if assigned(t^.greater) then
               genitem(t^.greater);
          end;

        begin
           if with_sign then
             begin
                cond_lt:=C_L;
                cond_le:=C_LE;
             end
           else
              begin
                cond_lt:=C_B;
                cond_le:=C_BE;
             end;
           { do we need to generate cmps? }
           if (with_sign and (min_label<0)) then
             genlinearcmplist(hp)
           else
             begin
                last:=0;
                lastrange:=false;
                first:=true;
                genitem(hp);
                cg.a_jmp_always(exprasmlist,elselabel);
             end;
        end;

begin
{$ifndef TEST_GENERIC}
   cinnode:=ti386innode;
{$endif}
   ccasenode:=ti386casenode;
end.
{
  $Log$
  Revision 1.56  2003-04-25 08:25:26  daniel
    * Ifdefs around a lot of calls to cleartempgen
    * Fixed registers that are allocated but not freed in several nodes
    * Tweak to register allocator to cause less spills
    * 8-bit registers now interfere with esi,edi and ebp
      Compiler can now compile rtl successfully when using new register
      allocator

  Revision 1.55  2003/04/23 09:51:16  daniel
    * Removed usage of edi in a lot of places when new register allocator used
    + Added newra versions of g_concatcopy and secondadd_float

  Revision 1.54  2003/04/22 23:50:23  peter
    * firstpass uses expectloc
    * checks if there are differences between the expectloc and
      location.loc from secondpass in EXTDEBUG

  Revision 1.53  2003/04/22 14:33:38  peter
    * removed some notes/hints

  Revision 1.52  2003/04/22 10:09:35  daniel
    + Implemented the actual register allocator
    + Scratch registers unavailable when new register allocator used
    + maybe_save/maybe_restore unavailable when new register allocator used

  Revision 1.51  2003/03/13 19:52:23  jonas
    * and more new register allocator fixes (in the i386 code generator this
      time). At least now the ppc cross compiler can compile the linux
      system unit again, but I haven't tested it.

  Revision 1.50  2003/02/26 23:06:13  daniel
    * Fixed an illegal use of makeregsize

  Revision 1.49  2003/02/19 22:39:56  daniel
    * Fixed a few issues

  Revision 1.48  2003/02/19 22:00:15  daniel
    * Code generator converted to new register notation
    - Horribily outdated todo.txt removed

  Revision 1.47  2003/01/13 14:54:34  daniel
    * Further work to convert codegenerator register convention;
      internalerror bug fixed.

  Revision 1.46  2003/01/08 18:43:57  daniel
   * Tregister changed into a record

  Revision 1.45  2002/11/25 17:43:27  peter
    * splitted defbase in defutil,symutil,defcmp
    * merged isconvertable and is_equal into compare_defs(_ext)
    * made operator search faster by walking the list only once

  Revision 1.44  2002/10/03 21:34:45  carl
    * range check error fixes

  Revision 1.43  2002/09/17 18:54:05  jonas
    * a_load_reg_reg() now has two size parameters: source and dest. This
      allows some optimizations on architectures that don't encode the
      register size in the register name.

  Revision 1.42  2002/09/16 18:08:26  peter
    * fix last optimization in genlinearlist, detected by bug tw1066
    * use generic casenode.pass2 routine and override genlinearlist
    * add jumptable support to generic casenode, by default there is
      no jumptable support

  Revision 1.41  2002/09/09 13:57:45  jonas
    * small optimization to case genlist() case statements

  Revision 1.40  2002/08/17 09:23:46  florian
    * first part of procinfo rewrite

  Revision 1.39  2002/08/12 15:08:42  carl
    + stab register indexes for powerpc (moved from gdb to cpubase)
    + tprocessor enumeration moved to cpuinfo
    + linker in target_info is now a class
    * many many updates for m68k (will soon start to compile)
    - removed some ifdef or correct them for correct cpu

  Revision 1.38  2002/08/11 14:32:30  peter
    * renamed current_library to objectlibrary

  Revision 1.37  2002/08/11 13:24:17  peter
    * saving of asmsymbols in ppu supported
    * asmsymbollist global is removed and moved into a new class
      tasmlibrarydata that will hold the info of a .a file which
      corresponds with a single module. Added librarydata to tmodule
      to keep the library info stored for the module. In the future the
      objectfiles will also be stored to the tasmlibrarydata class
    * all getlabel/newasmsymbol and friends are moved to the new class

  Revision 1.36  2002/07/23 14:31:00  daniel
  * Added internal error when asked to generate code for 'if expr in []'

  Revision 1.35  2002/07/20 11:58:04  florian
    * types.pas renamed to defbase.pas because D6 contains a types
      unit so this would conflicts if D6 programms are compiled
    + Willamette/SSE2 instructions to assembler added

  Revision 1.34  2002/07/11 14:41:34  florian
    * start of the new generic parameter handling

  Revision 1.33  2002/07/06 20:27:26  carl
  + generic set handling

  Revision 1.32  2002/07/01 18:46:33  peter
    * internal linker
    * reorganized aasm layer

  Revision 1.31  2002/05/18 13:34:25  peter
    * readded missing revisions

  Revision 1.30  2002/05/16 19:46:52  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.28  2002/05/13 19:54:38  peter
    * removed n386ld and n386util units
    * maybe_save/maybe_restore added instead of the old maybe_push

  Revision 1.27  2002/05/12 16:53:17  peter
    * moved entry and exitcode to ncgutil and cgobj
    * foreach gets extra argument for passing local data to the
      iterator function
    * -CR checks also class typecasts at runtime by changing them
      into as
    * fixed compiler to cycle with the -CR option
    * fixed stabs with elf writer, finally the global variables can
      be watched
    * removed a lot of routines from cga unit and replaced them by
      calls to cgobj
    * u32bit-s32bit updates for and,or,xor nodes. When one element is
      u32bit then the other is typecasted also to u32bit without giving
      a rangecheck warning/error.
    * fixed pascal calling method with reversing also the high tree in
      the parast, detected by tcalcst3 test

  Revision 1.26  2002/04/25 20:16:40  peter
    * moved more routines from cga/n386util

  Revision 1.25  2002/04/21 19:02:07  peter
    * removed newn and disposen nodes, the code is now directly
      inlined from pexpr
    * -an option that will write the secondpass nodes to the .s file, this
      requires EXTDEBUG define to actually write the info
    * fixed various internal errors and crashes due recent code changes

  Revision 1.24  2002/04/21 15:37:26  carl
  * changeregsize -> rg.makeregsize

  Revision 1.23  2002/04/19 15:39:35  peter
    * removed some more routines from cga
    * moved location_force_reg/mem to ncgutil
    * moved arrayconstructnode secondpass to ncgld

  Revision 1.22  2002/04/15 19:44:21  peter
    * fixed stackcheck that would be called recursively when a stack
      error was found
    * generic changeregsize(reg,size) for i386 register resizing
    * removed some more routines from cga unit
    * fixed returnvalue handling
    * fixed default stacksize of linux and go32v2, 8kb was a bit small :-)

  Revision 1.21  2002/04/02 17:11:36  peter
    * tlocation,treference update
    * LOC_CONSTANT added for better constant handling
    * secondadd splitted in multiple routines
    * location_force_reg added for loading a location to a register
      of a specified size
    * secondassignment parses now first the right and then the left node
      (this is compatible with Kylix). This saves a lot of push/pop especially
      with string operations
    * adapted some routines to use the new cg methods

  Revision 1.20  2002/03/31 20:26:39  jonas
    + a_loadfpu_* and a_loadmm_* methods in tcg
    * register allocation is now handled by a class and is mostly processor
      independent (+rgobj.pas and i386/rgcpu.pas)
    * temp allocation is now handled by a class (+tgobj.pas, -i386\tgcpu.pas)
    * some small improvements and fixes to the optimizer
    * some register allocation fixes
    * some fpuvaroffset fixes in the unary minus node
    * push/popusedregisters is now called rg.save/restoreusedregisters and
      (for i386) uses temps instead of push/pop's when using -Op3 (that code is
      also better optimizable)
    * fixed and optimized register saving/restoring for new/dispose nodes
    * LOC_FPU locations now also require their "register" field to be set to
      R_ST, not R_ST0 (the latter is used for LOC_CFPUREGISTER locations only)
    - list field removed of the tnode class because it's not used currently
      and can cause hard-to-find bugs

}
