{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

    Generate m68k assembler for in set/case nodes

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
unit cg68kset;
interface

    uses
      tree;

    procedure secondsetelement(var p : ptree);
    procedure secondin(var p : ptree);
    procedure secondcase(var p : ptree);


implementation

    uses
      globtype,systems,symconst,
      cobjects,verbose,globals,
      symtable,aasm,types,
      hcodegen,temp_gen,pass_2,
      cpubase,cga68k,tgen68k;

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

    { could be built into secondadd but it }
    { should be easy to read }
    procedure secondin(var p : ptree);


      type  Tsetpart=record
                range:boolean;      {Part is a range.}
                start,stop:byte;    {Start/stop when range; Stop=element
                                     when an element.}
            end;

      var
         pushed,ranges : boolean;
         hr : tregister;
         setparts:array[1..8] of Tsetpart;
         i,numparts:byte;
         {href,href2:Treference;}
         l,l2 : pasmlabel;
         hl,hl1 : pasmlabel;
         hl2, hl3: pasmlabel;
         opsize : topsize;


               function swaplongint(l : longint): longint;
               var
                 w1: word;
                 w2: word;
               begin
                 w1:=l and $ffff;
                 w2:=l shr 16;
                 l:=swap(w2)+(longint(swap(w1)) shl 16);
                 swaplongint:=l;
               end;

            function analizeset(Aset:Pconstset):boolean;

            type    byteset=set of byte;
                    tlongset  = array[0..7] of longint;
            var compares,maxcompares:word;
                someset : tlongset;
                i:byte;

            begin
                analizeset:=false;
                ranges:=false;
                numparts:=0;
                compares:=0;
                {Lots of comparisions take a lot of time, so do not allow
                 too much comparisions. 8 comparisions are, however, still
                 smalller than emitting the set.}
                maxcompares:=5;
                if cs_littlesize in aktglobalswitches then
                    maxcompares:=8;
                move(ASet^,someset,32);
                { On Big endian machines sets are stored   }
                { as INTEL Little-endian format, therefore }
                { we must convert it to the correct format }
{$IFDEF BIG_ENDIAN}
                for I:=0 to 7 do
                  someset[i]:=swaplongint(someset[i]);
{$ENDIF}
                for i:=0 to 255 do
                    if i in byteset(someset) then
                        begin
                            if (numparts=0) or
                             (i<>setparts[numparts].stop+1) then
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
                                        setparts[numparts].start:=
                                         setparts[numparts].stop;
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
            end;  { end analizeset }

      begin
         if psetdef(p^.right^.resulttype)^.settype=smallset then
           begin
              if p^.left^.treetype=ordconstn then
                begin
                   { only compulsory }
                   secondpass(p^.left);
                   secondpass(p^.right);
                   if codegenerror then
                     exit;
                   p^.location.resflags:=F_NE;
                   { Because of the Endian of the m68k, we have to consider this as a  }
                   { normal set and load it byte per byte, otherwise we will never get }
                   { the correct result.                                               }
                   case p^.right^.location.loc of
                     LOC_REGISTER,LOC_CREGISTER :
                       begin
                         emit_reg_reg(A_MOVE,S_L,p^.right^.location.register,R_D1);
                         exprasmlist^.concat(new(paicpu,
                           op_const_reg(A_AND,S_L, 1 shl (p^.left^.value and 31),R_D1)));
                       end;
                   else
                       begin
                         exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOVE,S_L,newreference(
                           p^.right^.location.reference),R_D1)));
                         exprasmlist^.concat(new(paicpu,op_const_reg(
                           A_AND,S_L,1 shl (p^.left^.value and 31) ,R_D1)));
                       end;
                   end;
                   del_reference(p^.right^.location.reference);
                end
              else
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
                   { of course not commutative }
                   if p^.swaped then
                        swaptree(p);
                   { load index into register }
                   case p^.left^.location.loc of
                      LOC_REGISTER,
                      LOC_CREGISTER :
                          hr:=p^.left^.location.register;
                      else
                         begin
                            { Small sets are always 32 bit values, there is no  }
                            { way they can be anything else, so no problems here}
                            exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOVE,S_L,
                              newreference(p^.left^.location.reference),R_D1)));
                            hr:=R_D1;
                            del_reference(p^.left^.location.reference);
                         end;
                   end;
                   case p^.right^.location.loc of
                      LOC_REGISTER,
                      LOC_CREGISTER : exprasmlist^.concat(new(paicpu, op_reg_reg(A_BTST,S_L,hr,p^.right^.location.register)));
                      else
                         begin
                            exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOVE,S_L,newreference(p^.right^.location.reference),
                              R_D0)));
                            exprasmlist^.concat(new(paicpu,op_reg_reg(A_BTST,S_L,hr,R_D0)));
                            del_reference(p^.right^.location.reference);
                         end;
                   end;
                   { support carry routines }
                   { sets the carry flags according to the result of BTST }
                   { i.e the Z flag.                                      }
                   getlabel(hl);
                   emitl(A_BNE,hl);
                   { leave all bits unchanged except Carry  = 0 }
                   exprasmlist^.concat(new(paicpu, op_const_reg(A_AND, S_B, $FE, R_CCR)));
                   getlabel(hl1);
                   emitl(A_BRA,hl1);
                   emitl(A_LABEL, hl);
                   { set carry to 1 }
                   exprasmlist^.concat(new(paicpu, op_const_reg(A_OR, S_B, $01, R_CCR)));
                   emitl(A_LABEL, hl1);
                   { end support carry routines }
                   p^.location.loc:=LOC_FLAGS;
                   p^.location.resflags:=F_C;
                end;
           end
         else { //// NOT a small set  //// }
           begin
              if p^.left^.treetype=ordconstn then
                begin
                   { only compulsory }
                   secondpass(p^.left);
                   secondpass(p^.right);
                   if codegenerror then
                     exit;
                   p^.location.resflags:=F_NE;
                   inc(p^.right^.location.reference.offset,(p^.left^.value div 32)*4);
                   exprasmlist^.concat(new(paicpu, op_ref_reg(A_MOVE, S_L,
                       newreference(p^.right^.location.reference), R_D1)));
                   exprasmlist^.concat(new(paicpu, op_const_reg(A_AND, S_L,
                       1 shl (p^.left^.value mod 32),R_D1)));
                   del_reference(p^.right^.location.reference);
                end
             else
                begin
                  if (p^.right^.treetype=setconstn) and
                     analizeset(p^.right^.value_set) then
                    begin
                      {It gives us advantage to check for the set elements
                        separately instead of using the SET_IN_BYTE procedure.
                       To do: Build in support for LOC_JUMP.}
                      secondpass(p^.left);
                      {We won't do a second pass on p^.right, because
                      this will emit the constant set.}
                      case p^.left^.location.loc of
                        LOC_REGISTER,
                        LOC_CREGISTER :
                           exprasmlist^.concat(new(paicpu,op_const_reg(A_AND,S_L,
                             255,p^.left^.location.register)));
                        else
                         Begin
                           { Because of the m68k endian, then we must LOAD normally the    }
                           { value into a register first, all depending on the source      }
                           { size!                                                         }
                           opsize:=S_NO;
                           case integer(p^.left^.resulttype^.size) of
                             1 : opsize:=S_B;
                             2 : opsize:=S_W;
                             4 : opsize:=S_L;
                           else
                             internalerror(19);
                           end;
                           exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOVE,opsize,
                             newreference(p^.left^.location.reference),R_D0)));
                           exprasmlist^.concat(new(paicpu,op_const_reg(A_AND,S_L,
                             255,R_D0)));
                         end;
                      end;
                      {Get a label to jump to the end.}
                      p^.location.loc:=LOC_FLAGS;
                      {It's better to use the zero flag when there are no ranges.}
                      if ranges then
                        p^.location.resflags:=F_C
                      else
                        p^.location.resflags:=F_E;
                      {href.symbol := nil;
                      clear_reference(href);}
                      getlabel(l);
                      {href.symbol:=stringdup(lab2str(l));}
                      for i:=1 to numparts do
                          if setparts[i].range then
                             begin
                                  {Check if left is in a range.}
                                  {Get a label to jump over the check.}
                                  {href2.symbol := nil;
                                  clear_reference(href2);}
                                  getlabel(l2);
                                  {href.symbol:=stringdup(lab2str(l2));}
                                  if setparts[i].start=setparts[i].stop-1 then
                                  begin
                                    case p^.left^.location.loc of
                                      LOC_REGISTER,
                                      LOC_CREGISTER :
                                         exprasmlist^.concat(new(paicpu,op_const_reg(A_CMP,S_W,
                                           setparts[i].start,p^.left^.location.register)));
                                    else
                                         exprasmlist^.concat(new(paicpu,op_const_reg(A_CMP,S_W,
                                           setparts[i].start,R_D0)));
{                                         exprasmlist^.concat(new(paicpu,op_const_ref(A_CMP,S_B,
                                           setparts[i].start,newreference(p^.left^.location.reference))));}
                                    end;
                                  {Result should be in carry flag when ranges are used.}
                                  { Here the m68k does not affect any flag except the  }
                                  { flag which is OR'ed                                }
                                  if ranges then
                                     exprasmlist^.concat(new(paicpu,op_const_reg(A_OR,S_B,$01,R_CCR)));
                                  {If found, jump to end.}
                                  emitl(A_BEQ,l);
                                  case p^.left^.location.loc of
                                    LOC_REGISTER,
                                    LOC_CREGISTER :
                                      exprasmlist^.concat(new(paicpu,op_const_reg(A_CMP,S_W,
                                        setparts[i].stop,p^.left^.location.register)));
                                    else
                                      exprasmlist^.concat(new(paicpu,op_const_reg(A_CMP,S_W,
                                        setparts[i].stop,R_D0)));
{                                      exprasmlist^.concat(new(paicpu,op_const_ref(A_CMP,S_B,
                                      setparts[i].stop,newreference(p^.left^.location.reference))));}
                                  end;
                                  {Result should be in carry flag when ranges are used.}
                                  { Here the m68k does not affect any flag except the  }
                                  { flag which is OR'ed                                }
                                  if ranges then
                                     exprasmlist^.concat(new(paicpu,op_const_reg(A_OR,S_B,$01,R_CCR)));
                                  {If found, jump to end.}
                                  emitl(A_BEQ,l);
                             end
                          else
                             begin
                               if setparts[i].start<>0 then
                                  begin
                                  {We only check for the lower bound if it is > 0, because
                                   set elements lower than 0 do nt exist.}
                                    case p^.left^.location.loc of
                                      LOC_REGISTER,
                                      LOC_CREGISTER :
                                        exprasmlist^.concat(new(paicpu,op_const_reg(A_CMP,S_W,
                                        setparts[i].start,p^.left^.location.register)));
                                    else
                                        exprasmlist^.concat(new(paicpu,op_const_reg(A_CMP,S_W,
                                        setparts[i].start,R_D0)));
{                                        exprasmlist^.concat(new(paicpu,op_const_ref(A_CMP,S_B,
                                        setparts[i].start,newreference(p^.left^.location.reference)))); }
                                    end;
                                    {If lower, jump to next check.}
                                    emitl(A_BCS,l2);
                                    end;
                                    if setparts[i].stop<>255 then
                                       begin
                                       {We only check for the high bound if it is < 255, because
                                          set elements higher than 255 do nt exist.}
                                          case p^.left^.location.loc of
                                            LOC_REGISTER,
                                            LOC_CREGISTER :
                                              exprasmlist^.concat(new(paicpu,op_const_reg(A_CMP,S_W,
                                                setparts[i].stop+1,p^.left^.location.register)));
                                          else
                                              exprasmlist^.concat(new(paicpu,op_const_reg(A_CMP,S_W,
                                                setparts[i].stop+1,R_D0)));
{                                              exprasmlist^.concat(new(paicpu,op_const_ref(A_CMP,S_B,
                                                setparts[i].stop+1,newreference(p^.left^.location.reference))));}
                                          end; { end case }
                                          {If higher, element is in set.}
                                          emitl(A_BCS,l);
                                       end
                                     else
                                       begin
                                         exprasmlist^.concat(new(paicpu,op_const_reg(A_OR,S_B,$01,R_CCR)));
                                         emitl(A_JMP,l);
                                       end;
                                  end;
                               {Emit the jump over label.}
                               exprasmlist^.concat(new(pai_label,init(l2)));
                             end
                            else
                               begin
                               {Emit code to check if left is an element.}
                                 case p^.left^.location.loc of
                                   LOC_REGISTER,
                                   LOC_CREGISTER :
                                     exprasmlist^.concat(new(paicpu,op_const_reg(A_CMP,S_W,
                                      setparts[i].stop,p^.left^.location.register)));
                                   else
{                                     exprasmlist^.concat(new(paicpu,op_const_ref(A_CMP,S_B,
                                     setparts[i].stop,newreference(p^.left^.location.reference))));}
                                     exprasmlist^.concat(new(paicpu,op_const_reg(A_CMP,S_W,
                                      setparts[i].stop,R_D0)));
                                   end;
                                 {Result should be in carry flag when ranges are used.}
                                 if ranges then
                                   exprasmlist^.concat(new(paicpu, op_const_reg(A_OR,S_B,$01,R_CCR)));
                                   {If found, jump to end.}
                                 emitl(A_BEQ,l);
                               end;
                            if ranges then
                            { clear carry flag }
                                exprasmlist^.concat(new(paicpu,op_const_reg(A_AND,S_B,$FE,R_CCR)));
                            {To compensate for not doing a second pass.}
                            stringdispose(p^.right^.location.reference.symbol);
                            {Now place the end label.}
                            exprasmlist^.concat(new(pai_label,init(l)));
                        end
                   else
                        begin
                           { calculate both operators }
                           { the complex one first }
                           firstcomplex(p);
                           secondpass(p^.left);
                           {
                           unnecessary !! PM
                           set_location(p^.location,p^.left^.location);}
                           { are too few registers free? }
                           pushed:=maybe_push(p^.right^.registers32,p);
                           secondpass(p^.right);
                           if pushed then restore(p);
                           { of course not commutative }
                           if p^.swaped then
                             swaptree(p);
                            { SET_IN_BYTE is an inline assembler procedure instead  }
                            { of a normal procedure, which is *MUCH* faster         }
                            { Parameters are passed by registers, and FLAGS are set }
                            { according to the result.                              }
                            { a0   = address of set                                 }
                            { d0.b = value to compare with                          }
                            { CARRY SET IF FOUND ON EXIT                            }
                            loadsetelement(p^.left);
                            exprasmlist^.concat(new(paicpu,op_ref_reg(A_LEA,S_L,
                              newreference(p^.right^.location.reference),R_A0)));;
{                            emitpushreferenceaddr(p^.right^.location.reference);}
                            del_reference(p^.right^.location.reference);
                            emitcall('FPC_SET_IN_BYTE',true);
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
         endlabel,elselabel : pasmlabel;

         { true, if we can omit the range check of the jump table }
         jumptable_no_range : boolean;

      procedure gentreejmp(p : pcaserecord);

        var
           lesslabel,greaterlabel : pasmlabel;

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
              exprasmlist^.concat(new(paicpu,op_const_reg(A_CMP,opsize,p^._low,hregister)));
              if greaterlabel=lesslabel then
                begin
                   emitl(A_BNE,lesslabel);
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
              exprasmlist^.concat(new(paicpu,op_const_reg(A_CMP,opsize,p^._low,hregister)));
              emitl(jmp_le,lesslabel);
              exprasmlist^.concat(new(paicpu,op_const_reg(A_CMP,opsize,p^._high,hregister)));
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

        procedure genitem(t : pcaserecord);

          begin
             if assigned(t^.less) then
               genitem(t^.less);
             if t^._low=t^._high then
               begin
                  if (t^._low-last > 0) and (t^._low-last < 9) then
                     exprasmlist^.concat(new(paicpu,op_const_reg(A_SUBQ,opsize,t^._low-last,hregister)))
                  else
                  if (t^._low-last = 0) then
                     exprasmlist^.concat(new(paicpu,op_reg(A_TST,opsize,hregister)))
                  else
                     exprasmlist^.concat(new(paicpu,op_const_reg(A_SUB,opsize,t^._low-last,hregister)));
                  last:=t^._low;

                  emitl(A_BEQ,t^.statement);
               end
             else
               begin
                  { it begins with the smallest label, if the value }
                  { is even smaller then jump immediately to the    }
                  { ELSE-label                                      }
                  if first then
                    begin
                       if (t^._low-1 > 0) and (t^._low < 9) then
                          exprasmlist^.concat(new(paicpu,op_const_reg(A_SUBQ,opsize,t^._low-1,hregister)))
                       else
                       if t^._low-1=0 then
                         exprasmlist^.concat(new(paicpu,op_reg(A_TST,opsize,hregister)))
                       else
                         exprasmlist^.concat(new(paicpu,op_const_reg(A_SUB,opsize,t^._low-1,hregister)));
                       if t^._low = 0 then
                          emitl(A_BLE,elselabel)
                       else
                          emitl(jmp_lee,elselabel);
                    end
                  { if there is no unused label between the last and the }
                  { present label then the lower limit can be checked    }
                  { immediately. else check the range in between:        }
                  else if (t^._low-last>1)then

                    begin
                       if ((t^._low-last-1) > 0) and ((t^._low-last-1) < 9) then
                         exprasmlist^.concat(new(paicpu,op_const_reg(A_SUBQ,opsize,t^._low-last-1,hregister)))
                       else
                         exprasmlist^.concat(new(paicpu,op_const_reg(A_SUB,opsize,t^._low-last-1,hregister)));
                       emitl(jmp_lee,elselabel);
                    end;
                  exprasmlist^.concat(new(paicpu,op_const_reg(A_SUB,opsize,t^._high-t^._low+1,hregister)));
                  emitl(jmp_lee,t^.statement);

                  last:=t^._high;
               end;
             first:=false;
             if assigned(t^.greater) then
               genitem(t^.greater);
          end;

        var
           hr : tregister;

        begin
           { case register is modified by the list evalution }
           if (p^.left^.location.loc=LOC_CREGISTER) then
             begin
                hr:=getregister32;
             end;
           last:=0;
           first:=true;
           genitem(hp);
           emitl(A_JMP,elselabel);
        end;

      procedure genjumptable(hp : pcaserecord;min_,max_ : longint);

        var
           table : pasmlabel;
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
               datasegment^.concat(new(pai_const_symbol,init(elselabel)));
             for i:=t^._low to t^._high do
               datasegment^.concat(new(pai_const_symbol,init(t^.statement)));
              last:=t^._high;
             if assigned(t^.greater) then
               genitem(t^.greater);
          end;

        begin
           if not(jumptable_no_range) then
             begin
                exprasmlist^.concat(new(paicpu,op_const_reg(A_CMP,opsize,min_,hregister)));
                { case expr less than min_ => goto elselabel }
                emitl(jmp_le,elselabel);
                exprasmlist^.concat(new(paicpu,op_const_reg(A_CMP,opsize,max_,hregister)));
                emitl(jmp_gt,elselabel);
             end;
           getlabel(table);
           { extend with sign }
           if opsize=S_W then
             begin
                { word to long - unsigned }
                exprasmlist^.concat(new(paicpu,op_const_reg(A_AND,S_L,$ffff,hregister)));
             end
           else if opsize=S_B then
             begin
                { byte to long - unsigned }
                exprasmlist^.concat(new(paicpu,op_const_reg(A_AND,S_L,$ff,hregister)));
             end;
           new(hr);
           reset_reference(hr^);
           hr^.symbol:=stringdup(table^.name);
           hr^.offset:=(-min_)*4;

           { add scalefactor *4 to index }
           exprasmlist^.concat(new(paicpu,op_const_reg(A_LSL,S_L,2,hregister)));
{           hr^.scalefactor:=4; }
           hr^.base:=getaddressreg;
           emit_reg_reg(A_MOVE,S_L,hregister,hr^.base);
           exprasmlist^.concat(new(paicpu,op_ref(A_JMP,S_NO,hr)));
{          if not(cs_littlesize in aktglobalswitches^ ) then
             datasegment^.concat(new(paicpu,op_const(A_ALIGN,S_NO,4))); }
           datasegment^.concat(new(pai_label,init(table)));
             last:=min_;
           genitem(hp);
           if hr^.base <> R_NO then ungetregister(hr^.base);
           { !!!!!!!
           if not(cs_littlesize in aktglobalswitches^ ) then
             exprasmlist^.concat(new(paicpu,op_const(A_ALIGN,S_NO,4)));
           }
        end;

      var
         lv,hv,min_label,max_label,labels : longint;
         max_linear_list : longint;

      begin
         getlabel(endlabel);
         getlabel(elselabel);
         with_sign:=is_signed(p^.left^.resulttype);
         if with_sign then
           begin
              jmp_gt:=A_BGT;
              jmp_le:=A_BLT;
              jmp_lee:=A_BLE;
           end
         else
           begin
              jmp_gt:=A_BHI;
              jmp_le:=A_BCS;
              jmp_lee:=A_BLS;
           end;
         cleartempgen;
         secondpass(p^.left);
         { determines the size of the operand }
         { determines the size of the operand }
         opsize:=bytes2Sxx[p^.left^.resulttype^.size];
         { copy the case expression to a register }
         { copy the case expression to a register }
         case p^.left^.location.loc of
            LOC_REGISTER,
            LOC_CREGISTER : hregister:=p^.left^.location.register;
            LOC_MEM,LOC_REFERENCE : begin
                                       del_reference(p^.left^.location.reference);
                                           hregister:=getregister32;
                                       exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOVE,opsize,newreference(
                                         p^.left^.location.reference),hregister)));
                                    end;
            else internalerror(2002);
         end;
         { now generate the jumps }
         if cs_optimize in aktglobalswitches  then
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
              emitl(A_JMP,endlabel);
              hp:=hp^.left;
           end;
         emitl(A_LABEL,elselabel);
         { ... and the else block }
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
  Revision 1.1  2000-07-13 06:29:46  michael
  + Initial import

  Revision 1.11  2000/02/09 13:22:49  peter
    * log truncated

  Revision 1.10  2000/01/07 01:14:22  peter
    * updated copyright to 2000

  Revision 1.9  1999/09/16 23:05:51  florian
    * m68k compiler is again compilable (only gas writer, no assembler reader)

}

