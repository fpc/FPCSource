{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

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

{$i defines.inc}

interface

    uses
       node,nset;

    type
       ti386setelementnode = class(tsetelementnode)
          procedure pass_2;override;
       end;

       ti386innode = class(tinnode)
          procedure pass_2;override;
       end;
       ti386casenode = class(tcasenode)
          procedure pass_2;override;
       end;

implementation

    uses
      globtype,systems,cpuinfo,
      cobjects,verbose,globals,
      symconst,symtable,aasm,types,
      hcodegen,temp_gen,pass_2,
      ncon,
      cpubase,cpuasm,
      cgai386,tgeni386,n386util;

     const
       bytes2Sxx:array[1..8] of Topsize=(S_B,S_W,S_NO,S_L,S_NO,S_NO,S_NO,S_Q);

{*****************************************************************************
                          TI386SETELEMENTNODE
*****************************************************************************}

    procedure ti386setelementnode.pass_2;
       begin
       { load first value in 32bit register }
         secondpass(left);
         if left.location.loc in [LOC_REGISTER,LOC_CREGISTER] then
           emit_to_reg32(left.location.register);

       { also a second value ? }
         if assigned(right) then
           begin
             secondpass(right);
             if right.location.loc in [LOC_REGISTER,LOC_CREGISTER] then
              emit_to_reg32(right.location.register);
           end;

         { we doesn't modify the left side, we check only the type }
         set_location(location,left.location);
       end;


{*****************************************************************************
                              TI386INNODE
*****************************************************************************}

    procedure ti386innode.pass_2;
       type
         Tsetpart=record
           range : boolean;      {Part is a range.}
           start,stop : byte;    {Start/stop when range; Stop=element when an element.}
         end;
       var
         genjumps,
         use_small,
         pushed,
         ranges     : boolean;
         hr,hr2,
         pleftreg   : tregister;
         opsize     : topsize;
         setparts   : array[1..8] of Tsetpart;
         i,numparts : byte;
         {href,href2 : Treference;}
         l,l2       : pasmlabel;
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
                     inc(compares);
                     if compares>maxcompares then
                      exit;
                   end
                 else
                  begin
                    {Extend a range.}
                    setparts[numparts].stop:=i;
                    {A range of two elements can better
                     be checked as two separate ones.
                     When extending a range, our range
                     becomes larger than two elements.}
                    ranges:=true;
                  end;
              end;
             analizeset:=true;
           end;

       begin
         { We check first if we can generate jumps, this can be done
           because the resulttype is already set in firstpass }

         { check if we can use smallset operation using btl which is limited
           to 32 bits, the left side may also not contain higher values !! }
         use_small:=(psetdef(right.resulttype)^.settype=smallset) and
                    ((left.resulttype^.deftype=orddef) and (porddef(left.resulttype)^.high<=32) or
                     (left.resulttype^.deftype=enumdef) and (penumdef(left.resulttype)^.max<=32));

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
            pushed:=maybe_push(right.registers32,left,false);
            secondpass(right);
            if pushed then
             restore(left,false);
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

            { If register is used, use only lower 8 bits }
            if left.location.loc in [LOC_REGISTER,LOC_CREGISTER] then
             begin
               pleftreg:=left.location.register;
               if pleftreg in [R_AX..R_DX] then
                begin
                  emit_const_reg(A_AND,S_W,255,pleftreg);
                  opsize:=S_W;
                end
               else
                if pleftreg in [R_EAX..R_EDI] then
                 begin
                   emit_const_reg(A_AND,S_L,255,pleftreg);
                   opsize:=S_L;
                 end
               else
                opsize:=S_B;
             end;

            { Get a label to jump to the end }
            location.loc:=LOC_FLAGS;

            { It's better to use the zero flag when there are
              no ranges }
            if ranges then
              location.resflags:=F_C
            else
              location.resflags:=F_E;

            getlabel(l);

            for i:=1 to numparts do
             if setparts[i].range then
              begin
                { Check if left is in a range }
                { Get a label to jump over the check }
                getlabel(l2);
                if setparts[i].start=setparts[i].stop-1 then
                 begin
                   case left.location.loc of
                  LOC_REGISTER,
                 LOC_CREGISTER : emit_const_reg(A_CMP,opsize,
                                   setparts[i].start,pleftreg);
                   else
                     emit_const_ref(A_CMP,S_B,
                       setparts[i].start,newreference(left.location.reference));
                   end;
                   { Result should be in carry flag when ranges are used }
                   if ranges then
                     emit_none(A_STC,S_NO);
                   { If found, jump to end }
                   emitjmp(C_E,l);
                   case left.location.loc of
                  LOC_REGISTER,
                 LOC_CREGISTER : emit_const_reg(A_CMP,opsize,
                                   setparts[i].stop,pleftreg);
                   else
                     emit_const_ref(A_CMP,S_B,
                       setparts[i].stop,newreference(left.location.reference));
                   end;
                   { Result should be in carry flag when ranges are used }
                   if ranges then
                     emit_none(A_STC,S_NO);
                   { If found, jump to end }
                   emitjmp(C_E,l);
                 end
                else
                 begin
                   if setparts[i].start<>0 then
                    begin
                      { We only check for the lower bound if it is > 0, because
                        set elements lower than 0 dont exist }
                      case left.location.loc of
                     LOC_REGISTER,
                    LOC_CREGISTER :
                    emit_const_reg(A_CMP,opsize,
                                      setparts[i].start,pleftreg);
                      else
                        emit_const_ref(A_CMP,S_B,
                          setparts[i].start,newreference(left.location.reference));
                      end;
                      { If lower, jump to next check }
                      emitjmp(C_B,l2);
                    end;
                   { We only check for the high bound if it is < 255, because
                     set elements higher than 255 do nt exist, the its always true,
                     so only a JMP is generated }
                   if setparts[i].stop<>255 then
                    begin
                      case left.location.loc of
                     LOC_REGISTER,
                    LOC_CREGISTER : emit_const_reg(A_CMP,opsize,
                                      setparts[i].stop+1,pleftreg);
                      else
                        emit_const_ref(A_CMP,S_B,
                          setparts[i].stop+1,newreference(left.location.reference));
                      end;
                      { If higher, element is in set }
                      emitjmp(C_B,l);
                    end
                   else
                    begin
                      emit_none(A_STC,S_NO);
                      emitjmp(C_None,l);
                    end;
                 end;
                { Emit the jump over label }
                emitlab(l2);
              end
             else
              begin
                { Emit code to check if left is an element }
                case left.location.loc of
               LOC_REGISTER,
              LOC_CREGISTER : emit_const_reg(A_CMP,opsize,
                                setparts[i].stop,pleftreg);
                else
                  emit_const_ref(A_CMP,S_B,
                    setparts[i].stop,newreference(left.location.reference));
                end;
                { Result should be in carry flag when ranges are used }
                if ranges then
                 emit_none(A_STC,S_NO);
                { If found, jump to end }
                emitjmp(C_E,l);
              end;
             if ranges then
              emit_none(A_CLC,S_NO);
             { To compensate for not doing a second pass }
             right.location.reference.symbol:=nil;
             { Now place the end label }
             emitlab(l);
             case left.location.loc of
            LOC_REGISTER,
           LOC_CREGISTER : ungetregister32(pleftreg);
             else
               del_reference(left.location.reference);
             end;
          end
         else
          begin
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
                         ungetregister32(right.location.register);
                       end
                  else
                   begin
                     emit_const_ref(A_TEST,S_L,1 shl (tordconstnode(left).value and 31),
                       newreference(right.location.reference));
                     del_reference(right.location.reference);
                   end;
                  end;
                end
               else
                begin
                  case left.location.loc of
                     LOC_REGISTER,
                     LOC_CREGISTER:
                       begin
                          hr:=left.location.register;
                          emit_to_reg32(hr);
                       end;
                  else
                    begin
                      { the set element isn't never samller than a byte  }
                      { and because it's a small set we need only 5 bits }
                      { but 8 bits are easier to load               }
                      getexplicitregister32(R_EDI);
                      emit_ref_reg(A_MOVZX,S_BL,
                        newreference(left.location.reference),R_EDI);
                      hr:=R_EDI;
                      del_reference(left.location.reference);
                    end;
                  end;

                  case right.location.loc of
                 LOC_REGISTER,
                LOC_CREGISTER :
                          begin
                            emit_reg_reg(A_BT,S_L,hr,
                              right.location.register);
                            ungetregister32(right.location.register);
                          end
                  else
                    begin
                      del_reference(right.location.reference);
                      if right.location.reference.is_immediate then
                       begin
                       { We have to load the value into a register because
                         btl does not accept values only refs or regs (PFV) }
                         hr2:=getregister32;
                         emit_const_reg(A_MOV,S_L,
                           right.location.reference.offset,hr2);
                         emit_reg_reg(A_BT,S_L,hr,hr2);
                         ungetregister32(hr2);
                       end
                      else
                        emit_reg_ref(A_BT,S_L,hr,
                          newreference(right.location.reference));
                    end;
                  end;
                  { simply to indicate EDI is deallocated here too (JM) }
                  ungetregister32(hr);
                  location.loc:=LOC_FLAGS;
                  location.resflags:=F_C;
                end;
             end
            else
             begin
               if right.location.reference.is_immediate then
                begin
                  location.resflags:=F_C;
                  getlabel(l);
                  getlabel(l2);

                  { Is this treated in firstpass ?? }
                  if left.nodetype=ordconstn then
                    begin
                      hr:=getregister32;
                      left.location.loc:=LOC_REGISTER;
                      left.location.register:=hr;
                      emit_const_reg(A_MOV,S_L,
                            tordconstnode(left).value,hr);
                    end;
                  case left.location.loc of
                     LOC_REGISTER,
                     LOC_CREGISTER:
                       begin
                          hr:=left.location.register;
                          emit_to_reg32(hr);
                          emit_const_reg(A_CMP,S_L,31,hr);
                          emitjmp(C_NA,l);
                        { reset carry flag }
                          emit_none(A_CLC,S_NO);
                          emitjmp(C_NONE,l2);
                          emitlab(l);
                        { We have to load the value into a register because
                          btl does not accept values only refs or regs (PFV) }
                          hr2:=getregister32;
                          emit_const_reg(A_MOV,S_L,right.location.reference.offset,hr2);
                          emit_reg_reg(A_BT,S_L,hr,hr2);
                          ungetregister32(hr2);
                       end;
                  else
                    begin
{$ifdef CORRECT_SET_IN_FPC}
                          if m_tp in aktmodeswitches then
                            begin
                            {***WARNING only correct if
                              reference is 32 bits (PM) *****}
                               emit_const_ref(A_CMP,S_L,
                                 31,newreference(left.location.reference));
                            end
                          else
{$endif CORRECT_SET_IN_FPC}
                            begin
                               emit_const_ref(A_CMP,S_B,
                                 31,newreference(left.location.reference));
                            end;
                       emitjmp(C_NA,l);
                     { reset carry flag }
                       emit_none(A_CLC,S_NO);
                       emitjmp(C_NONE,l2);
                       emitlab(l);
                       del_reference(left.location.reference);
                       hr:=getregister32;
                       emit_ref_reg(A_MOV,S_L,
                         newreference(left.location.reference),hr);
                     { We have to load the value into a register because
                       btl does not accept values only refs or regs (PFV) }
                       hr2:=getregister32;
                       emit_const_reg(A_MOV,S_L,
                         right.location.reference.offset,hr2);
                       emit_reg_reg(A_BT,S_L,hr,hr2);
                       ungetregister32(hr2);
                    end;
                  end;
                  emitlab(l2);
                end { of right.location.reference.is_immediate }
               { do search in a normal set which could have >32 elementsm
                 but also used if the left side contains higher values > 32 }
               else if left.nodetype=ordconstn then
                begin
                  location.resflags:=F_NE;
                  inc(right.location.reference.offset,tordconstnode(left).value shr 3);
                  emit_const_ref(A_TEST,S_B,1 shl (tordconstnode(left).value and 7),
                    newreference(right.location.reference));
                  del_reference(right.location.reference);
                end
               else
                begin
                  pushsetelement(left);
                  emitpushreferenceaddr(right.location.reference);
                  del_reference(right.location.reference);
                  { registers need not be save. that happens in SET_IN_BYTE }
                  { (EDI is changed) }
                  emitcall('FPC_SET_IN_BYTE');
                  { ungetiftemp(right.location.reference); }
                  location.loc:=LOC_FLAGS;
                  location.resflags:=F_C;
                end;
             end;
          end;
          if (right.location.loc in [LOC_MEM,LOC_REFERENCE]) then
            ungetiftemp(right.location.reference);
       end;


{*****************************************************************************
                            TI386CASENODE
*****************************************************************************}

    procedure ti386casenode.pass_2;
      var
         with_sign : boolean;
         opsize : topsize;
         jmp_gt,jmp_le,jmp_lee : tasmcond;
         hp : tnode;
         { register with case expression }
         hregister,hregister2 : tregister;
         endlabel,elselabel : pasmlabel;

         { true, if we can omit the range check of the jump table }
         jumptable_no_range : boolean;
         { where to put the jump table }
         jumpsegment : paasmoutput;
         min_label : TConstExprInt;

      procedure gentreejmp(p : pcaserecord);

        var
           lesslabel,greaterlabel : pasmlabel;

       begin
         emitlab(p^._at);
         { calculate labels for left and right }
         if (p^.less=nil) then
           lesslabel:=elselabel
         else
           lesslabel:=p^.less^._at;
         if (p^.greater=nil) then
           greaterlabel:=elselabel
         else
           greaterlabel:=p^.greater^._at;
           { calculate labels for left and right }
         { no range label: }
         if p^._low=p^._high then
           begin
              emit_const_reg(A_CMP,opsize,p^._low,hregister);
              if greaterlabel=lesslabel then
                emitjmp(C_NE,lesslabel)
              else
                begin
                   emitjmp(jmp_le,lesslabel);
                   emitjmp(jmp_gt,greaterlabel);
                end;
              emitjmp(C_None,p^.statement);
           end
         else
           begin
              emit_const_reg(A_CMP,opsize,p^._low,hregister);
              emitjmp(jmp_le,lesslabel);
              emit_const_reg(A_CMP,opsize,p^._high,hregister);
              emitjmp(jmp_gt,greaterlabel);
              emitjmp(C_None,p^.statement);
           end;
          if assigned(p^.less) then
           gentreejmp(p^.less);
          if assigned(p^.greater) then
           gentreejmp(p^.greater);
      end;

      procedure genlinearcmplist(hp : pcaserecord);

        var
           first : boolean;
           last : TConstExprInt;

        procedure genitem(t : pcaserecord);

          var
             l1 : pasmlabel;

          begin
             if assigned(t^.less) then
               genitem(t^.less);
             if t^._low=t^._high then
               begin
                  if opsize=S_Q then
                    begin
                       getlabel(l1);
                       emit_const_reg(A_CMP,S_L,hi(int64(t^._low)),hregister2);
                       emitjmp(C_NZ,l1);
                       emit_const_reg(A_CMP,S_L,lo(int64(t^._low)),hregister);
                       emitjmp(C_Z,t^.statement);
                       emitlab(l1);
                    end
                  else
                    begin
                       emit_const_reg(A_CMP,opsize,t^._low,hregister);
                       emitjmp(C_Z,t^.statement);
                       last:=t^._low;
                    end;
               end
             else
               begin
                  { if there is no unused label between the last and the }
                  { present label then the lower limit can be checked    }
                  { immediately. else check the range in between:        }
                  if first or (t^._low-last>1) then
                    begin
                       if opsize=S_Q then
                         begin
                            getlabel(l1);
                            emit_const_reg(A_CMP,S_L,hi(int64(t^._low)),hregister2);
                            emitjmp(jmp_le,elselabel);
                            emitjmp(jmp_gt,l1);
                            emit_const_reg(A_CMP,S_L,lo(int64(t^._low)),hregister);
                            { the comparisation of the low dword must be always unsigned! }
                            emitjmp(C_B,elselabel);
                            emitlab(l1);
                         end
                       else
                         begin
                            emit_const_reg(A_CMP,opsize,t^._low,hregister);
                            emitjmp(jmp_le,elselabel);
                         end;
                    end;

                  if opsize=S_Q then
                    begin
                       getlabel(l1);
                       emit_const_reg(A_CMP,S_L,hi(int64(t^._high)),hregister2);
                       emitjmp(jmp_le,t^.statement);
                       emitjmp(jmp_gt,l1);
                       emit_const_reg(A_CMP,S_L,lo(int64(t^._high)),hregister);
                       { the comparisation of the low dword must be always unsigned! }
                       emitjmp(C_BE,t^.statement);
                       emitlab(l1);
                    end
                  else
                    begin
                       emit_const_reg(A_CMP,opsize,t^._high,hregister);
                       emitjmp(jmp_lee,t^.statement);
                    end;

                  last:=t^._high;
               end;
             first:=false;
             if assigned(t^.greater) then
               genitem(t^.greater);
          end;

        begin
           last:=0;
           first:=true;
           genitem(hp);
           emitjmp(C_None,elselabel);
        end;

      procedure genlinearlist(hp : pcaserecord);

        var
           first : boolean;
           last : TConstExprInt;
           {helplabel : longint;}

        procedure genitem(t : pcaserecord);

            procedure gensub(value:longint);
            begin
              if value=1 then
                emit_reg(A_DEC,opsize,hregister)
              else
                emit_const_reg(A_SUB,opsize,value,hregister);
            end;

          begin
             if assigned(t^.less) then
               genitem(t^.less);
             { need we to test the first value }
             if first and (t^._low>get_min_value(left.resulttype)) then
               begin
                  emit_const_reg(A_CMP,opsize,t^._low,hregister);
                  emitjmp(jmp_le,elselabel);
               end;
             if t^._low=t^._high then
               begin
                  if t^._low-last=0 then
                    emit_reg_reg(A_OR,opsize,hregister,hregister)
                  else
                    gensub(t^._low-last);
                  last:=t^._low;
                  emitjmp(C_Z,t^.statement);
               end
             else
               begin
                  { it begins with the smallest label, if the value }
                  { is even smaller then jump immediately to the    }
                  { ELSE-label                                }
                  if first then
                    begin
                       { have we to ajust the first value ? }
                       if t^._low>get_min_value(left.resulttype) then
                         gensub(t^._low);
                    end
                  else
                    begin
                      { if there is no unused label between the last and the }
                      { present label then the lower limit can be checked    }
                      { immediately. else check the range in between:       }
                      emit_const_reg(A_SUB,opsize,t^._low-last,hregister);
                      emitjmp(jmp_le,elselabel);
                    end;
                  emit_const_reg(A_SUB,opsize,t^._high-t^._low,hregister);
                  emitjmp(jmp_lee,t^.statement);
                  last:=t^._high;
               end;
             first:=false;
             if assigned(t^.greater) then
               genitem(t^.greater);
          end;

        begin
           { do we need to generate cmps? }
           if (with_sign and (min_label<0)) then
             genlinearcmplist(hp)
           else
             begin
                last:=0;
                first:=true;
                genitem(hp);
                emitjmp(C_None,elselabel);
             end;
        end;

      procedure genjumptable(hp : pcaserecord;min_,max_ : longint);

        var
           table : pasmlabel;
           last : TConstExprInt;
           hr : preference;

        procedure genitem(t : pcaserecord);

          var
             i : longint;

          begin
             if assigned(t^.less) then
               genitem(t^.less);
             { fill possible hole }
             for i:=last+1 to t^._low-1 do
               jumpsegment^.concat(new(pai_const_symbol,init(elselabel)));
             for i:=t^._low to t^._high do
               jumpsegment^.concat(new(pai_const_symbol,init(t^.statement)));
              last:=t^._high;
             if assigned(t^.greater) then
               genitem(t^.greater);
            end;

          begin
           if not(jumptable_no_range) then
             begin
                emit_const_reg(A_CMP,opsize,min_,hregister);
                { case expr less than min_ => goto elselabel }
                emitjmp(jmp_le,elselabel);
                emit_const_reg(A_CMP,opsize,max_,hregister);
                emitjmp(jmp_gt,elselabel);
             end;
           getlabel(table);
           { extend with sign }
           if opsize=S_W then
             begin
                if with_sign then
                  emit_reg_reg(A_MOVSX,S_WL,hregister,
                    reg16toreg32(hregister))
                else
                  emit_reg_reg(A_MOVZX,S_WL,hregister,
                    reg16toreg32(hregister));
                hregister:=reg16toreg32(hregister);
             end
           else if opsize=S_B then
             begin
                if with_sign then
                  emit_reg_reg(A_MOVSX,S_BL,hregister,
                    reg8toreg32(hregister))
                else
                  emit_reg_reg(A_MOVZX,S_BL,hregister,
                    reg8toreg32(hregister));
                hregister:=reg8toreg32(hregister);
             end;
           new(hr);
           reset_reference(hr^);
           hr^.symbol:=table;
           hr^.offset:=(-min_)*4;
           hr^.index:=hregister;
           hr^.scalefactor:=4;
           emit_ref(A_JMP,S_NO,hr);
           { !!!!! generate tables
             if not(cs_littlesize in aktlocalswitches) then
             jumpsegment^.concat(new(paicpu,op_const(A_ALIGN,S_NO,4)));
           }
           jumpsegment^.concat(new(pai_label,init(table)));
             last:=min_;
           genitem(hp);
             { !!!!!!!
           if not(cs_littlesize in aktlocalswitches) then
             emit_const(A_ALIGN,S_NO,4);
           }
        end;

      var
         lv,hv,max_label,labels : longint;
         max_linear_list : longint;
         otl, ofl: pasmlabel;
{$ifdef Delphi}
         dist : cardinal;
{$else Delphi}
         dist : dword;
{$endif Delphi}
         hr : preference;

      begin
         getlabel(endlabel);
         getlabel(elselabel);
         if (cs_create_smart in aktmoduleswitches) then
           jumpsegment:=procinfo^.aktlocaldata
         else
           jumpsegment:=datasegment;
         with_sign:=is_signed(left.resulttype);
         if with_sign then
           begin
              jmp_gt:=C_G;
              jmp_le:=C_L;
              jmp_lee:=C_LE;
           end
         else
            begin
              jmp_gt:=C_A;
              jmp_le:=C_B;
              jmp_lee:=C_BE;
           end;
         cleartempgen;
         { save current truelabel and falselabel (they are restored in }
         { locjump2reg) (JM)                                           }
         if left.location.loc=LOC_JUMP then
           begin
            otl:=truelabel;
            getlabel(truelabel);
            ofl:=falselabel;
            getlabel(falselabel);
           end;
         secondpass(left);
         { determines the size of the operand }
         opsize:=bytes2Sxx[left.resulttype^.size];
         { copy the case expression to a register }
         case left.location.loc of
            LOC_REGISTER:
              begin
                 if opsize=S_Q then
                   begin
                      hregister:=left.location.registerlow;
                      hregister2:=left.location.registerhigh;
                   end
                 else
                   hregister:=left.location.register;
              end;
            LOC_FLAGS :
              begin
                locflags2reg(left.location,opsize);
                hregister := left.location.register;
              end;
            LOC_JUMP:
              begin
                locjump2reg(left.location,opsize,otl,ofl);
                hregister := left.location.register;
              end;
            LOC_CREGISTER:
              begin
                 hregister:=getregister32;
                 case opsize of
                    S_B:
                      hregister:=reg32toreg8(hregister);
                    S_W:
                      hregister:=reg32toreg16(hregister);
                    S_Q:
                      hregister2:=R_EDI;
                 end;
                 if opsize=S_Q then
                   begin
                      emit_reg_reg(A_MOV,S_L,left.location.registerlow,hregister);
                      hr:=newreference(left.location.reference);
                      inc(hr^.offset,4);
                      emit_reg_reg(A_MOV,S_L,left.location.registerhigh,hregister2);
                   end
                 else
                   emit_reg_reg(A_MOV,opsize,
                     left.location.register,hregister);
              end;
            LOC_MEM,LOC_REFERENCE:
              begin
                 del_reference(left.location.reference);
                 hregister:=getregister32;
                 case opsize of
                    S_B:
                      hregister:=reg32toreg8(hregister);
                    S_W:
                      hregister:=reg32toreg16(hregister);
                    S_Q:
                      hregister2:=R_EDI;
                 end;
                 if opsize=S_Q then
                   begin
                      emit_ref_reg(A_MOV,S_L,newreference(
                        left.location.reference),hregister);
                      hr:=newreference(left.location.reference);
                      inc(hr^.offset,4);
                      emit_ref_reg(A_MOV,S_L,hr,hregister2);
                   end
                 else
                   emit_ref_reg(A_MOV,opsize,newreference(
                     left.location.reference),hregister);
              end;
            else internalerror(2002);
         end;
         { we need the min_label always to choose between }
         { cmps and subs/decs                             }
         min_label:=case_get_min(nodes);
         { now generate the jumps }
         if opsize=S_Q then
           genlinearcmplist(nodes)
         else
           begin
              if cs_optimize in aktglobalswitches then
                begin
                   { procedures are empirically passed on }
                   { consumption can also be calculated   }
                   { but does it pay on the different     }
                   { processors?                       }
                   { moreover can the size only be appro- }
                   { ximated as it is not known if rel8,  }
                   { rel16 or rel32 jumps are used   }
                   max_label:=case_get_max(nodes);
                   labels:=case_count_labels(nodes);
                   { can we omit the range check of the jump table ? }
                   getrange(left.resulttype,lv,hv);
                   jumptable_no_range:=(lv=min_label) and (hv=max_label);
                   { hack a little bit, because the range can be greater }
                   { than the positive range of a longint            }

                   if (min_label<0) and (max_label>0) then
                     begin
{$ifdef Delphi}
                        if min_label=longint($80000000) then
                          dist:=Cardinal(max_label)+Cardinal($80000000)
                        else
                          dist:=Cardinal(max_label)+Cardinal(-min_label)
{$else Delphi}
                        if min_label=$80000000 then
                          dist:=dword(max_label)+dword($80000000)
                        else
                          dist:=dword(max_label)+dword(-min_label)
{$endif Delphi}
                     end
                   else
                     dist:=max_label-min_label;

                   { optimize for size ? }
                   if cs_littlesize in aktglobalswitches  then
                     begin
                        if (labels<=2) or
                           ((max_label-min_label)<0) or
                           ((max_label-min_label)>3*labels) then
                       { a linear list is always smaller than a jump tree }
                          genlinearlist(nodes)
                        else
                       { if the labels less or more a continuum then }
                          genjumptable(nodes,min_label,max_label);
                     end
                   else
                     begin
                        if jumptable_no_range then
                          max_linear_list:=4
                        else
                          max_linear_list:=2;
                        { a jump table crashes the pipeline! }
                        if aktoptprocessor=Class386 then
                          inc(max_linear_list,3);
                            if aktoptprocessor=ClassP5 then
                          inc(max_linear_list,6);
                        if aktoptprocessor>=ClassP6 then
                          inc(max_linear_list,9);

                        if (labels<=max_linear_list) then
                          genlinearlist(nodes)
                        else
                          begin
                             if (dist>4*labels) then
                               begin
                                  if labels>16 then
                                    gentreejmp(nodes)
                                  else
                                    genlinearlist(nodes);
                               end
                             else
                               genjumptable(nodes,min_label,max_label);
                          end;
                     end;
                end
              else
                { it's always not bad }
                genlinearlist(nodes);
           end;

         ungetregister(hregister);

         { now generate the instructions }
         hp:=right;
         while assigned(hp) do
           begin
              cleartempgen;
              secondpass(tbinarynode(hp).right);
              { don't come back to case line }
              aktfilepos:=exprasmlist^.getlasttaifilepos^;
              emitjmp(C_None,endlabel);
              hp:=tbinarynode(hp).left;
           end;
         emitlab(elselabel);
         { ...and the else block }
         if assigned(elseblock) then
           begin
              cleartempgen;
              secondpass(elseblock);
           end;
         emitlab(endlabel);
      end;


begin
   csetelementnode:=ti386setelementnode;
   cinnode:=ti386innode;
   ccasenode:=ti386casenode;
end.
{
  $Log$
  Revision 1.1  2000-10-15 09:33:32  peter
    * moved n386*.pas to i386/ cpu_target dir

  Revision 1.4  2000/10/14 10:14:49  peter
    * moehrendorf oct 2000 rewrite

  Revision 1.3  2000/09/30 16:08:45  peter
    * more cg11 updates

  Revision 1.2  2000/09/24 20:17:44  florian
    * more conversion work done

  Revision 1.1  2000/09/24 19:38:39  florian
    * initial implementation

}