{
    $Id$
    Copyright (c) 1993-98 by Florian Klaempfl

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
unit cg386set;
interface

    uses
      tree;

    procedure secondsetelement(var p : ptree);
    procedure secondin(var p : ptree);
    procedure secondcase(var p : ptree);


implementation

    uses
      cobjects,verbose,globals,systems,
      symtable,aasm,i386,types,
      cgi386,cgai386,tgeni386,temp_gen,hcodegen;

     const
       bytes2Sxx:array[1..4] of Topsize=(S_B,S_W,S_NO,S_L);

{*****************************************************************************
                              SecondSetElement
*****************************************************************************}

    procedure secondsetelement(var p : ptree);
       begin
       { load first value in 32bit register }
         secondpass(p^.left);
         if p^.left^.location.loc in [LOC_REGISTER,LOC_CREGISTER] then
           emit_to_reg32(p^.left^.location.register);

       { also a second value ? }
         if assigned(p^.right) then
           begin
             secondpass(p^.right);
             if p^.right^.location.loc in [LOC_REGISTER,LOC_CREGISTER] then
              emit_to_reg32(p^.right^.location.register);
           end;

         { we doesn't modify the left side, we check only the type }
         set_location(p^.location,p^.left^.location);
       end;


{*****************************************************************************
                              SecondIn
*****************************************************************************}

    procedure secondin(var p : ptree);
       type
         Tsetpart=record
           range : boolean;      {Part is a range.}
           start,stop : byte;    {Start/stop when range; Stop=element when an element.}
         end;
       var
         use_small,
         pushed,
         ranges     : boolean;
         hr,hr2,
         pleftreg   : tregister;
         opsize     : topsize;
         setparts   : array[1..8] of Tsetpart;
         i,numparts : byte;
         href,href2 : Treference;
         l,l2       : plabel;

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
         { calculate both operators }
         { the complex one first }
         firstcomplex(p);
         secondpass(p^.left);
         { are too few registers free? }
         pushed:=maybe_push(p^.right^.registers32,p^.left);
         secondpass(p^.right);
         if pushed then
          restore(p^.left);
         if codegenerror then
          exit;

         { ofcourse not commutative }
         if p^.swaped then
          swaptree(p);

         { check if we can use smallset operation using btl which is limited
           to 32 bits, the left side may also not contain higher values !! }
         use_small:=(psetdef(p^.right^.resulttype)^.settype=smallset) and
                    ((p^.left^.resulttype^.deftype=orddef) and (porddef(p^.left^.resulttype)^.high<=32) or
                     (p^.left^.resulttype^.deftype=enumdef) and (penumdef(p^.left^.resulttype)^.max<=32));

         { Can we generate jumps? Possible for all types of sets }
         if (p^.right^.treetype=setconstn) and
            analizeset(p^.right^.value_set,use_small) then
          begin
            { It gives us advantage to check for the set elements
              separately instead of using the SET_IN_BYTE procedure.
              To do: Build in support for LOC_JUMP }

            { If register is used, use only lower 8 bits }
            if p^.left^.location.loc in [LOC_REGISTER,LOC_CREGISTER] then
             begin
               pleftreg:=p^.left^.location.register;
               if pleftreg in [R_AX..R_DX] then
                begin
                  exprasmlist^.concat(new(pai386,op_const_reg(A_AND,S_W,255,pleftreg)));
                  opsize:=S_W;
                end
               else
                if pleftreg in [R_EAX..R_EDI] then
                 begin
                   exprasmlist^.concat(new(pai386,op_const_reg(A_AND,S_L,255,pleftreg)));
                   opsize:=S_L;
                 end
               else
                opsize:=S_B;
             end;

            { Get a label to jump to the end }
            p^.location.loc:=LOC_FLAGS;

            { It's better to use the zero flag when there are
              no ranges }
            if ranges then
              p^.location.resflags:=F_C
            else
              p^.location.resflags:=F_E;

            reset_reference(href);
            getlabel(l);
            href.symbol:=stringdup(lab2str(l));

            for i:=1 to numparts do
             if setparts[i].range then
              begin
                { Check if left is in a range }
                { Get a label to jump over the check }
                reset_reference(href2);
                getlabel(l2);
                href.symbol:=stringdup(lab2str(l2));
                if setparts[i].start=setparts[i].stop-1 then
                 begin
                   case p^.left^.location.loc of
                  LOC_REGISTER,
                 LOC_CREGISTER : exprasmlist^.concat(new(pai386,op_const_reg(A_CMP,opsize,
                                   setparts[i].start,pleftreg)));
                   else
                     exprasmlist^.concat(new(pai386,op_const_ref(A_CMP,S_B,
                       setparts[i].start,newreference(p^.left^.location.reference))));
                   end;
                   { Result should be in carry flag when ranges are used }
                   if ranges then
                     exprasmlist^.concat(new(pai386,op_none(A_STC,S_NO)));
                   { If found, jump to end }
                   emitl(A_JE,l);
                   case p^.left^.location.loc of
                  LOC_REGISTER,
                 LOC_CREGISTER : exprasmlist^.concat(new(pai386,op_const_reg(A_CMP,opsize,
                                   setparts[i].stop,pleftreg)));
                   else
                     exprasmlist^.concat(new(pai386,op_const_ref(A_CMP,S_B,
                       setparts[i].stop,newreference(p^.left^.location.reference))));
                   end;
                   { Result should be in carry flag when ranges are used }
                   if ranges then
                     exprasmlist^.concat(new(pai386,op_none(A_STC,S_NO)));
                   { If found, jump to end }
                   emitl(A_JE,l);
                 end
                else
                 begin
                   if setparts[i].start<>0 then
                    begin
                      { We only check for the lower bound if it is > 0, because
                        set elements lower than 0 dont exist }
                      case p^.left^.location.loc of
                     LOC_REGISTER,
                    LOC_CREGISTER : exprasmlist^.concat(new(pai386,op_const_reg(A_CMP,opsize,
                                      setparts[i].start,pleftreg)));
                      else
                        exprasmlist^.concat(new(pai386,op_const_ref(A_CMP,S_B,
                          setparts[i].start,newreference(p^.left^.location.reference))));
                      end;
                      { If lower, jump to next check }
                      emitl(A_JB,l2);
                    end;
                   { We only check for the high bound if it is < 255, because
                     set elements higher than 255 do nt exist, the its always true,
                     so only a JMP is generated }
                   if setparts[i].stop<>255 then
                    begin
                      case p^.left^.location.loc of
                     LOC_REGISTER,
                    LOC_CREGISTER : exprasmlist^.concat(new(pai386,op_const_reg(A_CMP,opsize,
                                      setparts[i].stop+1,pleftreg)));
                      else
                        exprasmlist^.concat(new(pai386,op_const_ref(A_CMP,S_B,
                          setparts[i].stop+1,newreference(p^.left^.location.reference))));
                      end;
                      { If higher, element is in set }
                      emitl(A_JB,l);
                    end
                   else
                    begin
                      exprasmlist^.concat(new(pai386,op_none(A_STC,S_NO)));
                      emitl(A_JMP,l);
                    end;
                 end;
                { Emit the jump over label }
                exprasmlist^.concat(new(pai_label,init(l2)));
              end
             else
              begin
                { Emit code to check if left is an element }
                case p^.left^.location.loc of
               LOC_REGISTER,
              LOC_CREGISTER : exprasmlist^.concat(new(pai386,op_const_reg(A_CMP,opsize,
                                setparts[i].stop,pleftreg)));
                else
                  exprasmlist^.concat(new(pai386,op_const_ref(A_CMP,S_B,
                    setparts[i].stop,newreference(p^.left^.location.reference))));
                end;
                { Result should be in carry flag when ranges are used }
                if ranges then
                 exprasmlist^.concat(new(pai386,op_none(A_STC,S_NO)));
                { If found, jump to end }
                emitl(A_JE,l);
              end;
             if ranges then
              exprasmlist^.concat(new(pai386,op_none(A_CLC,S_NO)));
             { To compensate for not doing a second pass }
             stringdispose(p^.right^.location.reference.symbol);
             { Now place the end label }
             exprasmlist^.concat(new(pai_label,init(l)));
             case p^.left^.location.loc of
            LOC_REGISTER,
           LOC_CREGISTER : ungetregister32(pleftreg);
             else
               del_reference(p^.left^.location.reference);
             end;
          end
         else
          begin
          { We will now generated code to check the set itself, no jmps,
            handle smallsets separate, because it allows faster checks }
            if use_small then
             begin
               if p^.left^.treetype=ordconstn then
                begin
                  p^.location.resflags:=F_NE;
                  case p^.right^.location.loc of
                     LOC_REGISTER,
                     LOC_CREGISTER:
                      begin
                         exprasmlist^.concat(new(pai386,op_const_reg(A_TEST,S_L,
                           1 shl (p^.left^.value and 31),p^.right^.location.register)));
                         ungetregister32(p^.right^.location.register);
                       end
                  else
                   begin
                     exprasmlist^.concat(new(pai386,op_const_ref(A_TEST,S_L,1 shl (p^.left^.value and 31),
                       newreference(p^.right^.location.reference))));
                     del_reference(p^.right^.location.reference);
                   end;
                  end;
                end
               else
                begin
                  case p^.left^.location.loc of
                     LOC_REGISTER,
                     LOC_CREGISTER:
                       begin
                          hr:=p^.left^.location.register;
                          emit_to_reg32(hr);
                       end;
                  else
                    begin
                      { the set element isn't never samller than a byte  }
                      { and because it's a small set we need only 5 bits }
                      { but 8 bits are easier to load                    }
                      exprasmlist^.concat(new(pai386,op_ref_reg(A_MOVZX,S_BL,
                        newreference(p^.left^.location.reference),R_EDI)));
                      hr:=R_EDI;
                      del_reference(p^.left^.location.reference);
                    end;
                  end;

                  case p^.right^.location.loc of
                 LOC_REGISTER,
                LOC_CREGISTER : exprasmlist^.concat(new(pai386,op_reg_reg(A_BT,S_L,hr,
                                  p^.right^.location.register)));
                  else
                    begin
                      del_reference(p^.right^.location.reference);
                      if p^.right^.location.reference.isintvalue then
                       begin
                       { We have to load the value into a register because
                         btl does not accept values only refs or regs (PFV) }
                         hr2:=getregister32;
                         exprasmlist^.concat(new(pai386,op_const_reg(A_MOV,S_L,
                           p^.right^.location.reference.offset,hr2)));
                         exprasmlist^.concat(new(pai386,op_reg_reg(A_BT,S_L,hr,hr2)));
                         ungetregister32(hr2);
                       end
                      else
                        exprasmlist^.concat(new(pai386,op_reg_ref(A_BT,S_L,hr,
                          newreference(p^.right^.location.reference))));
                    end;
                  end;
                  ungetregister32(hr);
                  p^.location.loc:=LOC_FLAGS;
                  p^.location.resflags:=F_C;
                end;
             end
            else
             begin
               { do search in a normal set which could have >32 elementsm
                 but also used if the left side contains higher values > 32 }
               if p^.left^.treetype=ordconstn then
                begin
                  p^.location.resflags:=F_NE;
                  inc(p^.right^.location.reference.offset,p^.left^.value shr 3);
                  exprasmlist^.concat(new(pai386,op_const_ref(A_TEST,S_B,1 shl (p^.left^.value and 7),
                    newreference(p^.right^.location.reference))));
                  del_reference(p^.right^.location.reference);
                end
               else
                begin
                  pushsetelement(p^.left);
                  emitpushreferenceaddr(exprasmlist,p^.right^.location.reference);
                  del_reference(p^.right^.location.reference);
                  { registers need not be save. that happens in SET_IN_BYTE }
                  { (EDI is changed) }
                  emitcall('SET_IN_BYTE',true);
                  { ungetiftemp(p^.right^.location.reference); }
                  p^.location.loc:=LOC_FLAGS;
                  p^.location.resflags:=F_C;
                end;
             end;
          end;
       end;


{*****************************************************************************
                              SecondCase
*****************************************************************************}

    procedure secondcase(var p : ptree);
      var
         with_sign : boolean;
         opsize : topsize;
         jmp_gt,jmp_le,jmp_lee : tasmop;
         hp : ptree;
         { register with case expression }
         hregister : tregister;
         endlabel,elselabel : plabel;

         { true, if we can omit the range check of the jump table }
         jumptable_no_range : boolean;
         { where to put the jump table }
         jumpsegment : paasmoutput;

      procedure gentreejmp(p : pcaserecord);

        var
           lesslabel,greaterlabel : plabel;

       begin
         emitl(A_LABEL,p^._at);
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
              exprasmlist^.concat(new(pai386,op_const_reg(A_CMP,opsize,p^._low,hregister)));
              if greaterlabel=lesslabel then
                begin
                   emitl(A_JNE,lesslabel);
                end
              else
                begin
                   emitl(jmp_le,lesslabel);
                   emitl(jmp_gt,greaterlabel);
                end;
              emitl(A_JMP,p^.statement);
           end
         else
           begin
              exprasmlist^.concat(new(pai386,op_const_reg(A_CMP,opsize,p^._low,hregister)));
              emitl(jmp_le,lesslabel);
                exprasmlist^.concat(new(pai386,op_const_reg(A_CMP,opsize,p^._high,hregister)));
              emitl(jmp_gt,greaterlabel);
              emitl(A_JMP,p^.statement);
           end;
          if assigned(p^.less) then
           gentreejmp(p^.less);
          if assigned(p^.greater) then
           gentreejmp(p^.greater);
      end;

      procedure genlinearlist(hp : pcaserecord);

        var
           first : boolean;
           last : longint;
           {helplabel : longint;}

        procedure genitem(t : pcaserecord);

          begin
             if assigned(t^.less) then
               genitem(t^.less);
             { need we to test the first value }
             if first and (t^._low>get_min_value(p^.left^.resulttype)) then
               begin
                  exprasmlist^.concat(new(pai386,op_const_reg(A_CMP,opsize,t^._low,hregister)));
                  emitl(jmp_le,elselabel);
               end;
             if t^._low=t^._high then
               begin
                  if t^._low-last=1 then
                    exprasmlist^.concat(new(pai386,op_reg(A_DEC,opsize,hregister)))
                  else if t^._low-last=0 then
                    exprasmlist^.concat(new(pai386,op_reg_reg(A_OR,opsize,hregister,hregister)))
                  else
                    exprasmlist^.concat(new(pai386,op_const_reg(A_SUB,opsize,t^._low-last,hregister)));
                  last:=t^._low;

                  emitl(A_JZ,t^.statement);
               end
             else
               begin
                  { it begins with the smallest label, if the value }
                  { is even smaller then jump immediately to the    }
                  { ELSE-label                                      }
                  if first then
                    begin
                       { have we to ajust the first value ?}
                       if t^._low>get_min_value(p^.left^.resulttype) then
                         begin
                            if t^._low=1 then
                              exprasmlist^.concat(new(pai386,op_reg(A_DEC,opsize,
                                hregister)))
                            else
                              exprasmlist^.concat(new(pai386,op_const_reg(A_SUB,opsize,
                                t^._low,hregister)));
                         end;
                       { work around: if the lower range=0 and we
                         do the subtraction we have to take care
                         of the sign!
                       this isn't necessary, this is tested before now (FK)
                       if t^._low=0 then
                         emitl(A_JBE,elselabel)
                       else
                       emitl(jmp_lee,elselabel);
                       }
                    end
                  else
                  { if there is no unused label between the last and the }
                  { present label then the lower limit can be checked    }
                  { immediately. else check the range in between:        }
                  if (t^._low-last>1) then
                    begin
                       if t^._low-last-1=1 then
                         exprasmlist^.concat(new(pai386,op_reg(A_DEC,opsize,hregister)))
                       else
                         exprasmlist^.concat(new(pai386,op_const_reg(A_SUB,opsize,t^._low-last-1,hregister)));
                       emitl(jmp_le,elselabel);
                    end;
                  exprasmlist^.concat(new(pai386,op_const_reg(A_SUB,opsize,t^._high-t^._low+1,hregister)));
                  emitl(jmp_lee,t^.statement);

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
           emitl(A_JMP,elselabel);
        end;

      procedure genjumptable(hp : pcaserecord;min_,max_ : longint);

        var
           table : plabel;
           last : longint;
           hr : preference;

        procedure genitem(t : pcaserecord);

          var
             i : longint;

          begin
             if assigned(t^.less) then
               genitem(t^.less);
             { fill possible hole }
             for i:=last+1 to t^._low-1 do
               jumpsegment^.concat(new(pai_const,init_symbol(strpnew(lab2str
                 (elselabel)))));
             for i:=t^._low to t^._high do
               jumpsegment^.concat(new(pai_const,init_symbol(strpnew(lab2str
                    (t^.statement)))));
              last:=t^._high;
             if assigned(t^.greater) then
               genitem(t^.greater);
            end;

          begin
           if not(jumptable_no_range) then
             begin
                exprasmlist^.concat(new(pai386,op_const_reg(A_CMP,opsize,min_,hregister)));
                { case expr less than min_ => goto elselabel }
                emitl(jmp_le,elselabel);
                exprasmlist^.concat(new(pai386,op_const_reg(A_CMP,opsize,max_,hregister)));
                emitl(jmp_gt,elselabel);
             end;
           getlabel(table);
           { extend with sign }
           if opsize=S_W then
             begin
                exprasmlist^.concat(new(pai386,op_reg_reg(A_MOVZX,S_WL,hregister,
                  reg16toreg32(hregister))));
                hregister:=reg16toreg32(hregister);
             end
           else if opsize=S_B then
             begin
                exprasmlist^.concat(new(pai386,op_reg_reg(A_MOVZX,S_BL,hregister,
                  reg8toreg32(hregister))));
                hregister:=reg8toreg32(hregister);
             end;
           new(hr);
           reset_reference(hr^);
           hr^.symbol:=stringdup(lab2str(table));
           hr^.offset:=(-min_)*4;
           hr^.index:=hregister;
           hr^.scalefactor:=4;
           exprasmlist^.concat(new(pai386,op_ref(A_JMP,S_NO,hr)));
           { !!!!! generate tables
             if not(cs_littlesize in aktlocalswitches) then
             jumpsegment^.concat(new(pai386,op_const(A_ALIGN,S_NO,4)));
           }
           jumpsegment^.concat(new(pai_label,init(table)));
             last:=min_;
           genitem(hp);
             { !!!!!!!
           if not(cs_littlesize in aktlocalswitches) then
             exprasmlist^.concat(new(pai386,op_const(A_ALIGN,S_NO,4)));
           }
        end;

      var
         lv,hv,min_label,max_label,labels : longint;
         max_linear_list : longint;

      begin
         getlabel(endlabel);
         getlabel(elselabel);
         if (cs_smartlink in aktmoduleswitches) then
           jumpsegment:=procinfo.aktlocaldata
         else
           jumpsegment:=datasegment;
         with_sign:=is_signed(p^.left^.resulttype);
         if with_sign then
           begin
              jmp_gt:=A_JG;
              jmp_le:=A_JL;
              jmp_lee:=A_JLE;
           end
         else
            begin
              jmp_gt:=A_JA;
              jmp_le:=A_JB;
              jmp_lee:=A_JBE;
           end;
         cleartempgen;
         secondpass(p^.left);
         { determines the size of the operand }
         opsize:=bytes2Sxx[p^.left^.resulttype^.size];
         { copy the case expression to a register }
         case p^.left^.location.loc of
            LOC_REGISTER:
              hregister:=p^.left^.location.register;
            LOC_CREGISTER:
              begin
                 hregister:=getregister32;
                 case opsize of
                    S_B : hregister:=reg32toreg8(hregister);
                    S_W : hregister:=reg32toreg16(hregister);
                 end;
                 exprasmlist^.concat(new(pai386,op_reg_reg(A_MOV,opsize,
                   p^.left^.location.register,hregister)));
              end;
            LOC_MEM,LOC_REFERENCE : begin
                                       del_reference(p^.left^.location.reference);
                                       hregister:=getregister32;
                                       case opsize of
                                          S_B : hregister:=reg32toreg8(hregister);
                                          S_W : hregister:=reg32toreg16(hregister);
                                       end;
                                       exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,opsize,newreference(
                                         p^.left^.location.reference),hregister)));
                                    end;
            else internalerror(2002);
         end;
         { now generate the jumps }
           if cs_optimize in aktglobalswitches then
           begin
              { procedures are empirically passed on }
              { consumption can also be calculated   }
              { but does it pay on the different     }
              { processors?                          }
              { moreover can the size only be appro- }
              { ximated as it is not known if rel8,  }
              { rel16 or rel32 jumps are used        }
              min_label:=case_get_min(p^.nodes);
              max_label:=case_get_max(p^.nodes);
              labels:=case_count_labels(p^.nodes);
              { can we omit the range check of the jump table }
              getrange(p^.left^.resulttype,lv,hv);
              jumptable_no_range:=(lv=min_label) and (hv=max_label);

              { optimize for size ? }
              if cs_littlesize in aktglobalswitches  then
                begin
                   if (labels<=2) or ((max_label-min_label)>3*labels) then
                  { a linear list is always smaller than a jump tree }
                     genlinearlist(p^.nodes)
                   else
                  { if the labels less or more a continuum then }
                     genjumptable(p^.nodes,min_label,max_label);
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
                     genlinearlist(p^.nodes)
                   else
                     begin
                        if ((max_label-min_label)>4*labels) then
                          begin
                             if labels>16 then
                               gentreejmp(p^.nodes)
                             else
                               genlinearlist(p^.nodes);
                          end
                        else
                          genjumptable(p^.nodes,min_label,max_label);
                     end;
                end;
             end
           else
           { it's always not bad }
           genlinearlist(p^.nodes);

         { now generate the instructions }
           hp:=p^.right;
         while assigned(hp) do
           begin
              cleartempgen;
              secondpass(hp^.right);
              { don't come back to case line }
              aktfilepos:=exprasmlist^.getlasttaifilepos^;
              emitl(A_JMP,endlabel);
              hp:=hp^.left;
           end;
         emitl(A_LABEL,elselabel);
         { ...and the else block }
         if assigned(p^.elseblock) then
             begin
              cleartempgen;
              secondpass(p^.elseblock);
           end;
         emitl(A_LABEL,endlabel);
      end;


end.
{
  $Log$
  Revision 1.14  1998-09-09 16:44:21  florian
    * I hope, the case bug is fixed now

  Revision 1.13  1998/09/07 18:45:54  peter
    * update smartlinking, uses getdatalabel
    * renamed ptree.value vars to value_str,value_real,value_set

  Revision 1.12  1998/09/05 23:51:05  florian
    * possible bug with too few registers in first/secondin fixed

  Revision 1.11  1998/09/04 08:41:41  peter
    * updated some error messages

  Revision 1.10  1998/09/03 17:08:40  pierre
    * better lines for stabs
      (no scroll back to if before else part
      no return to case line at jump outside case)
    + source lines also if not in order

  Revision 1.9  1998/08/28 10:54:19  peter
    * fixed smallset generation from elements, it has never worked before!

  Revision 1.8  1998/08/25 11:51:46  peter
    * fixed -15 seen as byte in case

  Revision 1.7  1998/08/19 16:07:38  jonas
    * changed optimizer switches + cleanup of DestroyRefs in daopt386.pas

  Revision 1.6  1998/08/18 09:24:39  pierre
    * small warning position bug fixed
    * support_mmx switches splitting was missing
    * rhide error and warning output corrected

  Revision 1.5  1998/08/14 18:18:40  peter
    + dynamic set contruction
    * smallsets are now working (always longint size)

  Revision 1.4  1998/08/10 14:49:51  peter
    + localswitches, moduleswitches, globalswitches splitting

  Revision 1.3  1998/06/25 08:48:10  florian
    * first version of rtti support

  Revision 1.2  1998/06/16 08:56:18  peter
    + targetcpu
    * cleaner pmodules for newppu

  Revision 1.1  1998/06/05 17:44:13  peter
    * splitted cgi386

}

