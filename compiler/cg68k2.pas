{
    $Id$
    Copyright (c) 1993-98 by Florian Klaempfl, Carl Eric Codere

    This unit generates 68000 (or better) assembler from the parse tree

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

{$ifdef tp}
  {$E+,F+,N+,D+,L+,Y+}
{$endif}
Unit Cg68k2;

Interface

    uses
       objects,verbose,cobjects,systems,globals,tree,
       symtable,types,strings,pass_1,hcodegen,temp_gen,
       aasm,m68k,tgen68k,files,cga68k;

      const

       { process condition codes bit definitions }
       CARRY_FLAG    = $01;
       OVFL_FLAG     = $02;
       ZERO_FLAG     = $04;
       NEG_FLAG      = $08;
       { to set OR with flags     }
       { to clear AND (NOT flag)  }


    procedure secondadd(var p : ptree);
    procedure processcc(p: ptree);
    procedure secondfor(var p : ptree);
    procedure secondas(var p : ptree);
    procedure secondraise(var p : ptree);
    procedure secondin(var p : ptree);
    procedure secondexpr(var p : ptree);
    procedure secondblockn(var p : ptree);
    procedure second_while_repeatn(var p : ptree);
    procedure secondifn(var p : ptree);
    procedure secondbreakn(var p : ptree);
    { copies p a set element into the d0.b register }
    procedure loadsetelement(var p : ptree);

Implementation

    uses cg68k;


    procedure secondadd(var p : ptree);

    { is also being used for xor, and "mul", "sub, or and comparative }
    { operators                                                       }

      label do_normal;
      var
         swapp : ptree;
         hregister : tregister;
         pushed,mboverflow,cmpop : boolean;
         op : tasmop;
         pushedregs : tpushed;
         flags : tresflags;
         otl,ofl : plabel;
         power : longint;
         href : treference;
         opsize : topsize;
         swapl : tlocation;
         tmpref: treference;
         { true, if unsigned types are compared }
         unsigned : boolean;

          { is_in_dest if the result is put directly into }
          { the resulting refernce or varregister }
           { true, if a small set is handled with the longint code }
          is_set : boolean;
          is_in_dest : boolean;
           { true, if for sets subtractions the extra not should generated }
           extra_not : boolean;

      begin
         unsigned:=false;
         is_in_dest := false;
         extra_not:=false;

         opsize:=S_L;

         { calculate the operator which is more difficult }
         firstcomplex(p);
         { handling boolean expressions extra: }
         if ((p^.left^.resulttype^.deftype=orddef) and
            (porddef(p^.left^.resulttype)^.typ=bool8bit)) then
{            ((p^.right^.resulttype^.deftype=orddef) and
            (porddef(p^.right^.resulttype)^.typ=bool8bit)) then }
           begin
              if (p^.treetype=andn) or (p^.treetype=orn) then
                begin
                   p^.location.loc:=LOC_JUMP;
                   cmpop:=false;
                   case p^.treetype of
                     andn : begin
                               otl:=truelabel;
                               getlabel(truelabel);
                               secondpass(p^.left);
                               maketojumpbool(p^.left);
                               emitl(A_LABEL,truelabel);
                               truelabel:=otl;
                            end;
                     orn : begin
                              ofl:=falselabel;
                              getlabel(falselabel);
                              secondpass(p^.left);
                              maketojumpbool(p^.left);
                              emitl(A_LABEL,falselabel);
                              falselabel:=ofl;
                           end;
                     else Message(sym_e_type_mismatch);
                   end; { end case }
                  secondpass(p^.right);
                  maketojumpbool(p^.right);
                end { endif }
              else if p^.treetype in [unequaln,equaln,xorn] then
                begin
                   opsize:=S_B;
                   if p^.left^.treetype=ordconstn then
                     begin
                        swapp:=p^.right;
                        p^.right:=p^.left;
                        p^.left:=swapp;
                        p^.swaped:=not(p^.swaped);
                     end;
                   secondpass(p^.left);
                   p^.location:=p^.left^.location;
                   (* register needed *)
                   pushed:=maybe_push(p^.right^.registers32,p);
                   secondpass(p^.right);
                   if pushed then restore(p);
                   goto do_normal;
                end { endif }
              else Message(sym_e_type_mismatch);
           end { endif }
         { also handle string operations seperately }
         else if (p^.left^.resulttype^.deftype=stringdef) then
           begin
              { string operations are not commutative }
              if p^.swaped then
                begin
                   swapp:=p^.left;
                   p^.left:=p^.right;
                   p^.right:=swapp;
                   { because of jump being produced at comparison below: }
                   p^.swaped:=not(p^.swaped);
                end;
              case p^.treetype of
                 addn : begin
                           cmpop:=false;
                           secondpass(p^.left);
                           if (p^.left^.treetype<>addn) then
                             begin
                                { can only reference be }
                                { string in register would be funny    }
                                { therefore produce a temporary string }

                                { release the registers }
                                del_reference(p^.left^.location.reference);
                                gettempofsizereference(256,href);
                                copystring(href,p^.left^.location.reference,255);
                                ungetiftemp(p^.left^.location.reference);

                                { does not hurt: }
                                p^.left^.location.loc:=LOC_MEM;
                                p^.left^.location.reference:=href;
                             end;

                           secondpass(p^.right);

                           { on the right we do not need the register anymore too }
                           del_reference(p^.right^.location.reference);
                           pushusedregisters(pushedregs,$ffff);
                           { WE INVERSE THE PARAMETERS!!! }
                           { Because parameters are inversed in the rtl }
                           emitpushreferenceaddr(p^.right^.location.reference);
                           emitpushreferenceaddr(p^.left^.location.reference);
                           emitcall('STRCONCAT',true);
                           maybe_loadA5;
                           popusedregisters(pushedregs);
                           set_location(p^.location,p^.left^.location);
                           ungetiftemp(p^.right^.location.reference);
                        end; { this case }
              ltn,lten,gtn,gten,
                equaln,unequaln :
                        begin
                           secondpass(p^.left);
                           { are too few registers free? }
                           pushed:=maybe_push(p^.right^.registers32,p);
                           secondpass(p^.right);
                           if pushed then restore(p);
                           cmpop:=true;
                           del_reference(p^.right^.location.reference);
                           del_reference(p^.left^.location.reference);
                           { generates better code }
                           { s='' and s<>''        }
                           if (p^.treetype in [equaln,unequaln]) and
                             (
                               ((p^.left^.treetype=stringconstn) and
                                (p^.left^.values^='')) or
                               ((p^.right^.treetype=stringconstn) and
                                (p^.right^.values^=''))
                             ) then
                             begin
                                { only one node can be stringconstn }
                                { else pass 1 would have evaluted   }
                                { this node                         }
                                if p^.left^.treetype=stringconstn then
                                  exprasmlist^.concat(new(pai68k,op_ref(
                                    A_TST,S_B,newreference(p^.right^.location.reference))))
                                else
                                  exprasmlist^.concat(new(pai68k,op_ref(
                                    A_TST,S_B,newreference(p^.left^.location.reference))));
                             end
                           else
                             begin
                               pushusedregisters(pushedregs,$ffff);

                               { parameters are directly passed via registers       }
                               { this has several advantages, no loss of the flags  }
                               { on exit ,and MUCH faster on m68k machines          }
                               {  speed difference (68000)                          }
                               {   normal routine: entry, exit code + push  = 124   }
                               {   (best case)                                      }
                               {   assembler routine: param setup (worst case) = 48 }

                               exprasmlist^.concat(new(pai68k,op_ref_reg(
                                    A_LEA,S_L,newreference(p^.left^.location.reference),R_A0)));
                               exprasmlist^.concat(new(pai68k,op_ref_reg(
                                    A_LEA,S_L,newreference(p^.right^.location.reference),R_A1)));
{
                               emitpushreferenceaddr(p^.left^.location.reference);
                               emitpushreferenceaddr(p^.right^.location.reference); }
                               emitcall('STRCMP',true);
                               maybe_loada5;
                               popusedregisters(pushedregs);
                          end;
                           ungetiftemp(p^.left^.location.reference);
                           ungetiftemp(p^.right^.location.reference);
                        end; { end this case }
                else Message(sym_e_type_mismatch);
              end; { end case }
           end { end else if }
         else
           begin
              { in case of constant put it to the left }
              if p^.left^.treetype=ordconstn then
                begin
                   swapp:=p^.right;
                   p^.right:=p^.left;
                   p^.left:=swapp;
                   p^.swaped:=not(p^.swaped);
                end;
              secondpass(p^.left);
              set_location(p^.location,p^.left^.location);
              { are to few registers free? }
              pushed:=maybe_push(p^.right^.registers32,p);
              secondpass(p^.right);
              if pushed then restore(p);
              if (p^.left^.resulttype^.deftype=pointerdef) or

                 (p^.right^.resulttype^.deftype=pointerdef) or

                 ((p^.right^.resulttype^.deftype=objectdef) and
                  pobjectdef(p^.right^.resulttype)^.isclass and
                 (p^.left^.resulttype^.deftype=objectdef) and
                  pobjectdef(p^.left^.resulttype)^.isclass
                 ) or

                 (p^.left^.resulttype^.deftype=classrefdef) or

                 (p^.left^.resulttype^.deftype=procvardef) or

                 (p^.left^.resulttype^.deftype=enumdef) or

                 ((p^.left^.resulttype^.deftype=orddef) and
                 (porddef(p^.left^.resulttype)^.typ=s32bit)) or
                 ((p^.right^.resulttype^.deftype=orddef) and
                 (porddef(p^.right^.resulttype)^.typ=s32bit)) or

                ((p^.left^.resulttype^.deftype=orddef) and
                 (porddef(p^.left^.resulttype)^.typ=u32bit)) or
                 ((p^.right^.resulttype^.deftype=orddef) and
                 (porddef(p^.right^.resulttype)^.typ=u32bit))

                 { SMALL SETS DO NOT WORK BECAUSE OF ENDIAN! }
              or  { as well as small sets }
                ((p^.left^.resulttype^.deftype=setdef) and
                 (psetdef(p^.left^.resulttype)^.settype=smallset))
                 then
                begin
           do_normal:
                   mboverflow:=false;
                   cmpop:=false;
                   if (p^.left^.resulttype^.deftype=pointerdef) or
                      (p^.right^.resulttype^.deftype=pointerdef) or
                      ((p^.left^.resulttype^.deftype=orddef) and
                      (porddef(p^.left^.resulttype)^.typ=u32bit)) or
                      ((p^.right^.resulttype^.deftype=orddef) and
                      (porddef(p^.right^.resulttype)^.typ=u32bit)) then
                     unsigned:=true;
                   is_set:=p^.resulttype^.deftype=setdef;

                   case p^.treetype of
                      addn : begin
                                if is_set then
                                  begin
                                     op:=A_OR;
                                     mboverflow:=false;
                                     unsigned:=false;
                                  end
                                else
                                  begin
                                     op:=A_ADD;
                                     mboverflow:=true;
                                  end;
                             end; { end this case }
                        symdifn : begin
                                  { the symetric diff is only for sets }
                                  if is_set then
                                    begin
                                       op:=A_EOR;
                                       mboverflow:=false;
                                       unsigned:=false;
                                    end
                                  else
                                    begin
                                       Message(sym_e_type_mismatch);
                                    end;
                               end;
                      muln : begin
                                if is_set then
                                  begin
                                     op:=A_AND;
                                     mboverflow:=false;
                                     unsigned:=false;
                                  end
                                else
                                  begin
                                     if unsigned then
                                       op:=A_MULU
                                     else
                                       op:=A_MULS;
                                     mboverflow:=true;
                                  end;
                             end; { end this case }
                      subn : begin
                                if is_set then
                                  begin
                                     op:=A_AND;
                                     mboverflow:=false;
                                     unsigned:=false;
                                     extra_not:=true;
                                  end
                                else
                                  begin
                                     op:=A_SUB;
                                     mboverflow:=true;
                                  end;
                             end; {end this case }
                      ltn,lten,gtn,gten,
                      equaln,unequaln :
                             begin
                                op:=A_CMP;
                                cmpop:=true;
                             end;
                      xorn : op:=A_EOR;
                      orn : op:=A_OR;
                      andn : op:=A_AND;
                      else Message(sym_e_type_mismatch);
                   end; {end case }
                   { left and right no register?  }
                   { then one must be demanded    }
                   if (p^.left^.location.loc<>LOC_REGISTER) and
                     (p^.right^.location.loc<>LOC_REGISTER) then
                     begin
                        { register variable ? }
                        if (p^.left^.location.loc=LOC_CREGISTER) then
                          begin
                               { it is OK if this is the destination }
                               if is_in_dest then
                                 begin
                                    hregister:=p^.location.register;
                                    emit_reg_reg(A_MOVE,opsize,p^.left^.location.register,
                                      hregister);
                                 end
                               else
                             if cmpop then
                               begin
                                  { do not disturb the register }
                                  hregister:=p^.location.register;
                               end
                             else
                               begin
                                  hregister := getregister32;
                                  emit_reg_reg(A_MOVE,opsize,p^.left^.location.register,
                                    hregister);
                               end
                          end
                        else
                          begin
                             del_reference(p^.left^.location.reference);
                               if is_in_dest then
                                 begin
                                    hregister:=p^.location.register;
                                    exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,opsize,
                                    newreference(p^.left^.location.reference),hregister)));
                                 end
                               else
                                 begin

                                 { first give free, then demand new register }
                                 hregister := getregister32;
                                 exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,opsize,
                                   newreference(p^.left^.location.reference),
                                    hregister)));
                                 end;{ endif p^... }
                          end;

                        p^.location.loc:=LOC_REGISTER;
                        p^.location.register:=hregister;

                     end
                   else
                     { if on the right the register then swap }
                     if (p^.right^.location.loc=LOC_REGISTER) then
                       begin
                          swap_location(p^.location,p^.right^.location);

                          { newly swapped also set swapped flag }
                          p^.swaped:=not(p^.swaped);
                       end;
                   { endif p^...<> LOC_REGISTER }
                   { at this point, p^.location.loc should be LOC_REGISTER }
                   { and p^.location.register should be a valid register   }
                   { containing the left result                    }
                   if p^.right^.location.loc<>LOC_REGISTER then
                     begin
                        if (p^.treetype=subn) and p^.swaped then
                          begin
                             if p^.right^.location.loc=LOC_CREGISTER then
                               begin
                                  if extra_not then
                                    exprasmlist^.concat(new(pai68k,op_reg(A_NOT,opsize,p^.location.register)));


                                  emit_reg_reg(A_MOVE,opsize,p^.right^.location.register,R_D6);
                                  emit_reg_reg(op,opsize,p^.location.register,R_D6);
                                  emit_reg_reg(A_MOVE,opsize,R_D6,p^.location.register);
                               end
                             else
                               begin
                                  if extra_not then
                                    exprasmlist^.concat(new(pai68k,op_reg(A_NOT,opsize,p^.location.register)));

                                  exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,opsize,
                                    newreference(p^.right^.location.reference),R_D6)));
                                  exprasmlist^.concat(new(pai68k,op_reg_reg(op,opsize,p^.location.register,R_D6)));
                                  exprasmlist^.concat(new(pai68k,op_reg_reg(A_MOVE,opsize,R_D6,p^.location.register)));
                                  del_reference(p^.right^.location.reference);
                               end;
                          end
                          { end subn ... }
                        else
                          begin
                             if (p^.right^.treetype=ordconstn) and (op=A_CMP) and
                                (p^.right^.value=0) then
                                  exprasmlist^.concat(new(pai68k,op_reg(A_TST,opsize,p^.location.register)))
                             else
                                if (p^.right^.treetype=ordconstn) and (op=A_MULS) and
                                   (ispowerof2(p^.right^.value,power)) then
                                  begin
                                    if (power <= 8) then
                                        exprasmlist^.concat(new(pai68k,op_const_reg(A_ASL,opsize,power,
                                         p^.location.register)))
                                    else
                                      begin
                                        exprasmlist^.concat(new(pai68k,op_const_reg(A_MOVE,S_L,power,
                                         R_D6)));
                                        exprasmlist^.concat(new(pai68k,op_reg_reg(A_ASL,opsize,R_D6,
                                          p^.location.register)))
                                      end;
                                  end
                             else
                               begin
                                  if (p^.right^.location.loc=LOC_CREGISTER) then
                                    begin
                                       if extra_not then
                                         begin
                                            emit_reg_reg(A_MOVE,S_L,p^.right^.location.register,R_D6);
                                            exprasmlist^.concat(new(pai68k,op_reg(A_NOT,S_L,R_D6)));
                                            emit_reg_reg(A_AND,S_L,R_D6,
                                              p^.location.register);
                                         end
                                       else
                                         begin
                                            if (op=A_MULS) and (opsize = S_L) and (aktoptprocessor=MC68000) then
                                            { Emulation for MC68000 }
                                            begin
                                              emit_reg_reg(A_MOVE,opsize,p^.right^.location.register,
                                                 R_D0);
                                              emit_reg_reg(A_MOVE,opsize,p^.location.register,R_D1);
                                              emitcall('LONGMUL',true);
                                              emit_reg_reg(A_MOVE,opsize,R_D0,p^.location.register);
                                            end
                                            else
                                            if (op=A_MULU) and (opsize = S_L) and (aktoptprocessor=MC68000) then
                                             Message(cg_f_32bit_not_supported_in_68000)
                                            else
                                              emit_reg_reg(op,opsize,p^.right^.location.register,
                                                p^.location.register);
                                         end;
                                    end
                                  else
                                    begin
                                       if extra_not then
                                         begin
                                            exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,S_L,newreference(
                                              p^.right^.location.reference),R_D6)));
                                            exprasmlist^.concat(new(pai68k,op_reg(A_NOT,S_L,R_D6)));
                                            emit_reg_reg(A_AND,S_L,R_D6,
                                              p^.location.register);
                                         end
                                       else
                                         begin
                                            if (op=A_MULS) and (opsize = S_L) and (aktoptprocessor=MC68000) then
                                            { Emulation for MC68000 }
                                            begin
                                              exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE, opsize,
                                                 newreference(p^.right^.location.reference),R_D1)));
                                              emit_reg_reg(A_MOVE,opsize,p^.location.register,R_D0);
                                              emitcall('LONGMUL',true);
                                              emit_reg_reg(A_MOVE,opsize,R_D0,p^.location.register);
                                            end
                                            else
                                            if (op=A_MULU) and (opsize = S_L) and (aktoptprocessor=MC68000) then
                                             Message(cg_f_32bit_not_supported_in_68000)
                                            else
                                            { When one of the source/destination is a memory reference  }
                                            { and the operator is EOR, the we must load it into the     }
                                            { value into a register first since only EOR reg,reg exists }
                                            { on the m68k                                               }
                                            if (op=A_EOR) then
                                              begin
                                                exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,opsize,newreference(
                                                    p^.right^.location.reference),R_D0)));
                                                exprasmlist^.concat(new(pai68k,op_reg_reg(op,opsize,R_D0,
                                                    p^.location.register)));
                                              end
                                            else
                                              exprasmlist^.concat(new(pai68k,op_ref_reg(op,opsize,newreference(
                                                p^.right^.location.reference),p^.location.register)));
                                         end;
                                       del_reference(p^.right^.location.reference);
                                    end;
                               end;
                          end;
                     end
                   else
                     begin
                        { when swapped another result register }
                        if (p^.treetype=subn) and p^.swaped then
                          begin
                             if extra_not then
                               exprasmlist^.concat(new(pai68k,op_reg(A_NOT,S_L,p^.location.register)));

                             exprasmlist^.concat(new(pai68k,op_reg_reg(op,opsize,
                               p^.location.register,p^.right^.location.register)));
                               swap_location(p^.location,p^.right^.location);
                               { newly swapped also set swapped flag }
                               { just to maintain ordering           }
                               p^.swaped:=not(p^.swaped);
                          end
                        else
                          begin
                             if extra_not then
                                   exprasmlist^.concat(new(pai68k,op_reg(A_NOT,S_L,p^.right^.location.register)));

                             if (op=A_MULS) and (opsize = S_L) and (aktoptprocessor=MC68000) then
                             { Emulation for MC68000 }
                             begin
                               emit_reg_reg(A_MOVE,opsize,p^.right^.location.register,
                               R_D0);
                               emit_reg_reg(A_MOVE,opsize,p^.location.register,R_D1);
                               emitcall('LONGMUL',true);
                               emit_reg_reg(A_MOVE,opsize,R_D0,p^.location.register);
                             end
                             else
                             if (op=A_MULU) and (opsize = S_L) and (aktoptprocessor=MC68000) then
                              Message(cg_f_32bit_not_supported_in_68000)
                             else

                               exprasmlist^.concat(new(pai68k,op_reg_reg(op,opsize,
                               p^.right^.location.register,
                               p^.location.register)));
                          end;
                           ungetregister32(p^.right^.location.register);
                     end;

                   if cmpop then
                        ungetregister32(p^.location.register);
                   { only in case of overflow operations }
                   { produce overflow code }
                   if mboverflow then
                     emitoverflowcheck(p);
               end
{*********************************************************************}
              else if ((p^.left^.resulttype^.deftype=orddef) and
                 (porddef(p^.left^.resulttype)^.typ=uchar)) then
                begin
                   case p^.treetype of
                      ltn,lten,gtn,gten,
                      equaln,unequaln :
                                cmpop:=true;
                      else Message(sym_e_type_mismatch);
                   end;
                   unsigned:=true;
                   { left and right no register? }
                   { the one must be demanded    }
                   if (p^.location.loc<>LOC_REGISTER) and
                     (p^.right^.location.loc<>LOC_REGISTER) then
                     begin
                        if p^.location.loc=LOC_CREGISTER then
                          begin
                             if cmpop then
                               { do not disturb register }
                               hregister:=p^.location.register
                             else
                               begin
                                  hregister:=getregister32;
                                  emit_reg_reg(A_MOVE,S_B,p^.location.register,
                                    hregister);
                               end;
                          end
                        else
                          begin
                             del_reference(p^.location.reference);

                             { first give free then demand new register }
                             hregister:=getregister32;
                             exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,S_B,newreference(p^.location.reference),
                               hregister)));
                          end;
                        p^.location.loc:=LOC_REGISTER;
                        p^.location.register:=hregister;
                     end;

                   { now p always a register }

                   if (p^.right^.location.loc=LOC_REGISTER) and
                      (p^.location.loc<>LOC_REGISTER) then
                     begin
                       swap_location(p^.location,p^.right^.location);

                        { newly swapped also set swapped flag }
                        p^.swaped:=not(p^.swaped);
                     end;
                   if p^.right^.location.loc<>LOC_REGISTER then
                     begin
                        if p^.right^.location.loc=LOC_CREGISTER then
                          begin
                             emit_reg_reg(A_CMP,S_B,
                                p^.right^.location.register,p^.location.register);
                          end
                        else
                          begin
                             exprasmlist^.concat(new(pai68k,op_ref_reg(A_CMP,S_B,newreference(
                                p^.right^.location.reference),p^.location.register)));
                             del_reference(p^.right^.location.reference);
                          end;
                     end
                   else
                     begin
                        emit_reg_reg(A_CMP,S_B,p^.right^.location.register,
                          p^.location.register);
                        ungetregister32(p^.right^.location.register);
                     end;
                   ungetregister32(p^.location.register);
                end


{*********************************************************************}

              else if (p^.left^.resulttype^.deftype=floatdef) and
                  (pfloatdef(p^.left^.resulttype)^.typ<>f32bit) then
                 begin
                    { real constants to the left }
                    if p^.left^.treetype=realconstn then
                      begin
                         swapp:=p^.right;
                         p^.right:=p^.left;
                         p^.left:=swapp;
                         p^.swaped:=not(p^.swaped);
                      end;
                    cmpop:=false;
                    case p^.treetype of
                       addn : op:=A_FADD;
                       muln : op:=A_FMUL;
                       subn : op:=A_FSUB;
                       slashn : op:=A_FDIV;
                       ltn,lten,gtn,gten,
                       equaln,unequaln : begin
                                            op:=A_FCMP;
                                            cmpop:=true;
                                         end;
                       else Message(sym_e_type_mismatch);
                    end;

                    if (p^.left^.location.loc <> LOC_FPU) and
                       (p^.right^.location.loc <> LOC_FPU) then
                      begin
                         { we suppose left in reference }
                         del_reference(p^.left^.location.reference);
                         { get a copy, since we don't want to modify the same }
                         { node at the same time.                             }
                         tmpref:=p^.left^.location.reference;
                         if assigned(p^.left^.location.reference.symbol) then
                           tmpref.symbol:=stringdup(p^.left^.location.reference.symbol^);

                         floatload(pfloatdef(p^.left^.resulttype)^.typ, tmpref,
                           p^.left^.location);
                         clear_reference(tmpref);
                      end
                    else
                      begin
                        if (p^.right^.location.loc = LOC_FPU)
                        and(p^.left^.location.loc <> LOC_FPU) then
                           begin
                             swap_location(p^.left^.location, p^.right^.location);
                             p^.swaped := not(p^.swaped);
                           end
                      end;

                   { ---------------- LEFT = LOC_FPUREG -------------------- }
                       if ((p^.treetype =subn) or (p^.treetype = slashn)) and (p^.swaped) then
                          {  fpu_reg =  right(FP1) / fpu_reg }
                          {  fpu_reg = right(FP1) -  fpu_reg  }
                          begin
                             if (cs_fp_emulation in aktmoduleswitches) then
                              begin
                               { fpu_reg = right / D1 }
                               { fpu_reg = right - D1 }
                                  exprasmlist^.concat(new(pai68k,op_reg_reg(A_MOVE,S_L,p^.left^.location.fpureg,R_D0)));


                                  { load value into D1 }
                                  if p^.right^.location.loc <> LOC_FPU then
                                     exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,S_L,
                                       newreference(p^.right^.location.reference),R_D1)))
                                  else
                                     emit_reg_reg(A_MOVE,S_L,p^.right^.location.fpureg,R_D1);

                                  { probably a faster way to do this but... }
                                  case op of
                                   A_FADD: emitcall('SINGLE_ADD',true);
                                   A_FMUL: emitcall('SINGLE_MUL',true);
                                   A_FSUB: emitcall('SINGLE_SUB',true);
                                   A_FDIV: emitcall('SINGLE_DIV',true);
                                   A_FCMP: emitcall('SINGLE_CMP',true);
                                  end;
                                  if not cmpop then { only flags are affected with cmpop }
                                     exprasmlist^.concat(new(pai68k,op_reg_reg(A_MOVE,S_L,R_D0,
                                       p^.left^.location.fpureg)));

                                  { if this was a reference, then delete as it }
                                  { it no longer required.                     }
                                  if p^.right^.location.loc <> LOC_FPU then
                                     del_reference(p^.right^.location.reference);
                              end
                             else
                              begin

                                  if p^.right^.location.loc <> LOC_FPU then
                                    exprasmlist^.concat(new(pai68k,op_ref_reg(A_FMOVE,
                                       getfloatsize(pfloatdef(p^.left^.resulttype)^.typ),
                                      newreference(p^.right^.location.reference),
                                      R_FP1)))
                                  else
                                    { FPm --> FPn must use extended precision }
                                    emit_reg_reg(A_FMOVE,S_FX,p^.right^.location.fpureg,R_FP1);

                                  { arithmetic expression performed in extended mode }
                                  exprasmlist^.concat(new(pai68k,op_reg_reg(op,S_FX,
                                      p^.left^.location.fpureg,R_FP1)));

                                  { cmpop does not change any floating point register!! }
                                  if not cmpop then
                                       emit_reg_reg(A_FMOVE,S_FX,R_FP1,p^.left^.location.fpureg)
{                                       exprasmlist^.concat(new(pai68k,op_reg_reg(A_FMOVE,
                                       getfloatsize(pfloatdef(p^.left^.resulttype)^.typ),
                                       R_FP1,p^.left^.location.fpureg)))}
                                  else
                                  { process comparison, to make it compatible with the rest of the code }
                                      processcc(p);

                                  { if this was a reference, then delete as it }
                                  { it no longer required.                     }
                                  if p^.right^.location.loc <> LOC_FPU then
                                     del_reference(p^.right^.location.reference);
                              end;
                          end
                       else { everything is in the right order }
                         begin
                           {  fpu_reg = fpu_reg / right }
                           {  fpu_reg = fpu_reg - right }
                           { + commutative ops }
                           if cs_fp_emulation in aktmoduleswitches then
                           begin

                             { load value into D7 }
                             if p^.right^.location.loc <> LOC_FPU then
                               exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,S_L,
                                 newreference(p^.right^.location.reference),R_D0)))
                             else
                               emit_reg_reg(A_MOVE,S_L,p^.right^.location.fpureg,R_D0);

                             emit_reg_reg(A_MOVE,S_L,p^.left^.location.fpureg,R_D1);
                             { probably a faster way to do this but... }
                             case op of
                               A_FADD: emitcall('SINGLE_ADD',true);
                               A_FMUL: emitcall('SINGLE_MUL',true);
                               A_FSUB: emitcall('SINGLE_SUB',true);
                               A_FDIV: emitcall('SINGLE_DIV',true);
                               A_FCMP: emitcall('SINGLE_CMP',true);
                             end;
                             if not cmpop then { only flags are affected with cmpop }
                               exprasmlist^.concat(new(pai68k,op_reg_reg(A_MOVE,S_L,R_D0,
                                 p^.left^.location.fpureg)));
                             { if this was a reference, then delete as it }
                             { it no longer required.                     }
                             if p^.right^.location.loc <> LOC_FPU then
                               del_reference(p^.right^.location.reference);
                           end
                           else
                           begin
                             if p^.right^.location.loc <> LOC_FPU then
                               exprasmlist^.concat(new(pai68k,op_ref_reg(A_FMOVE,
                                 getfloatsize(pfloatdef(p^.left^.resulttype)^.typ),
                                 newreference(p^.right^.location.reference),R_FP1)))
                             else
                               emit_reg_reg(A_FMOVE,getfloatsize(pfloatdef(p^.left^.resulttype)^.typ),
                                 p^.right^.location.fpureg,R_FP1);

                               emit_reg_reg(op,S_FX,R_FP1,p^.left^.location.fpureg);

                               if cmpop then
                                 processcc(p);

                             { if this was a reference, then delete as it }
                             { it no longer required.                     }
                             if p^.right^.location.loc <> LOC_FPU then
                               del_reference(p^.right^.location.reference);

                           end
                         end; { endif treetype = .. }


                         if cmpop then
                          begin
                             if p^.swaped then
                                 case p^.treetype of
                                     equaln: flags := F_E;
                                     unequaln: flags := F_NE;
                                     ltn : flags := F_G;
                                     lten : flags := F_GE;
                                     gtn : flags := F_L;
                                     gten: flags := F_LE;
                                 end
                             else
                                 case p^.treetype of
                                     equaln: flags := F_E;
                                     unequaln : flags := F_NE;
                                     ltn: flags := F_L;
                                     lten : flags := F_LE;
                                     gtn : flags := F_G;
                                     gten: flags := F_GE;
                                 end;
                             p^.location.loc := LOC_FLAGS;
                             p^.location.resflags := flags;
                             cmpop := false;
                          end
                         else
                         begin
                             p^.location.loc := LOC_FPU;
                             if p^.left^.location.loc = LOC_FPU then
                             { copy fpu register result . }
                             { HERE ON EXIT FPU REGISTER IS IN EXTENDED MODE! }
                                p^.location.fpureg := p^.left^.location.fpureg
                             else
                             begin
                               InternalError(34);
                             end;
                         end;

                end
{*********************************************************************}
              else if (p^.left^.resulttype^.deftype=setdef) then
                begin
                   { not commutative }
                   if p^.swaped then
                     begin
                        swapp:=p^.left;
                        p^.left:=p^.right;
                        p^.right:=swapp;
                        { because of jump being produced by comparison }
                        p^.swaped:=not(p^.swaped);
                     end;
                   case p^.treetype of
                      equaln,unequaln : begin
                                     cmpop:=true;
                                     del_reference(p^.left^.location.reference);
                                     del_reference(p^.right^.location.reference);
                                     pushusedregisters(pushedregs,$ffff);

{                                     emitpushreferenceaddr(p^.right^.location.reference);
                                     emitpushreferenceaddr(p^.left^.location.reference);}
                                     exprasmlist^.concat(new(pai68k,op_ref_reg(A_LEA,S_L,
                                       newreference(p^.left^.location.reference),R_A0)));
                                     exprasmlist^.concat(new(pai68k,op_ref_reg(A_LEA,S_L,
                                       newreference(p^.right^.location.reference),R_A1)));
                                     emitcall('SET_COMP_SETS',true);
                                     maybe_loada5;
                                     popusedregisters(pushedregs);
                                     ungetiftemp(p^.left^.location.reference);
                                     ungetiftemp(p^.right^.location.reference);
                                  end;

                      addn,subn,muln,symdifn : begin
                                     cmpop:=false;
                                     del_reference(p^.left^.location.reference);
                                     del_reference(p^.right^.location.reference);
                                     href.symbol:=nil;
                                     pushusedregisters(pushedregs,$ffff);
                                     gettempofsizereference(32,href);
                                     emitpushreferenceaddr(href);
                                     { wrong place !! was hard to find out
                                     pushusedregisters(pushedregs,$ff);}
                                     emitpushreferenceaddr(p^.right^.location.reference);
                                     emitpushreferenceaddr(p^.left^.location.reference);
                                     case p^.treetype of
                                       subn : exprasmlist^.concat(new(pai68k,op_csymbol(A_JSR,S_NO,
                                         newcsymbol('SET_SUB_SETS',0))));
                                       addn : exprasmlist^.concat(new(pai68k,op_csymbol(A_JSR,S_NO,
                                         newcsymbol('SET_ADD_SETS',0))));
                                       muln : exprasmlist^.concat(new(pai68k,op_csymbol(A_JSR,S_NO,
                                         newcsymbol('SET_MUL_SETS',0))));
                                       symdifn:
                                              emitcall('SET_SYMDIF_SETS',true);
                                     end;
                                     maybe_loada5;
                                     popusedregisters(pushedregs);
                                     ungetiftemp(p^.left^.location.reference);
                                     ungetiftemp(p^.right^.location.reference);
                                     p^.location.loc:=LOC_MEM;
                                     stringdispose(p^.location.reference.symbol);
                                     p^.location.reference:=href;
                                  end;
                      else Message(sym_e_type_mismatch);
                   end; { end case }
                end {else if begin }
              else Message(sym_e_type_mismatch);
           end; { endif }
          if (p^.left^.resulttype^.deftype<>stringdef) and
             not ((p^.left^.resulttype^.deftype=setdef) and
                (psetdef(p^.left^.resulttype)^.settype<>smallset)) then
            begin
               { this can be useful if for instance length(string) is called }
               if (p^.left^.location.loc=LOC_REFERENCE) or
                  (p^.left^.location.loc=LOC_MEM) then
                 ungetiftemp(p^.left^.location.reference);
               if (p^.right^.location.loc=LOC_REFERENCE) or
                  (p^.right^.location.loc=LOC_MEM) then
                 ungetiftemp(p^.right^.location.reference);
            end;

         { in case of comparison operation the put result in the flags }
         if cmpop then
           begin
              if not(unsigned) then
                begin
                   if p^.swaped then
                     case p^.treetype of
                        equaln : flags:=F_E;
                        unequaln : flags:=F_NE;
                        ltn : flags:=F_G;
                        lten : flags:=F_GE;
                        gtn : flags:=F_L;
                        gten : flags:=F_LE;
                     end
                   else
                     case p^.treetype of
                        equaln : flags:=F_E;
                        unequaln : flags:=F_NE;
                        ltn : flags:=F_L;
                        lten : flags:=F_LE;
                        gtn : flags:=F_G;
                        gten : flags:=F_GE;
                     end;
                end
              else
                begin
                   if p^.swaped then
                     case p^.treetype of
                        equaln : flags:=F_E;
                        unequaln : flags:=F_NE;
                        ltn : flags:=F_A;
                        lten : flags:=F_AE;
                        gtn : flags:=F_B;
                        gten : flags:=F_BE;
                     end
                   else
                     case p^.treetype of
                        equaln : flags:=F_E;
                        unequaln : flags:=F_NE;
                        ltn : flags:=F_B;
                        lten : flags:=F_BE;
                        gtn : flags:=F_A;
                        gten : flags:=F_AE;
                     end;
                end; { end begin }
              p^.location.loc:=LOC_FLAGS;
              p^.location.resflags:=flags;
           end; { endif cmpop }
      end;

 procedure processcc(p: ptree);
 var
   label1,label2: plabel;
 (*************************************************************************)
 (*  Description: This routine handles the conversion of Floating point   *)
 (*  condition codes to normal cpu condition codes.                       *)
 (*************************************************************************)
 begin
      getlabel(label1);
      getlabel(label2);
      case p^.treetype of
        equaln,unequaln: begin
                           { not equal clear zero flag }
                           emitl(A_FBEQ,label1);
                           exprasmlist^.concat(new(pai68k, op_const_reg(
                             A_AND, S_B, NOT ZERO_FLAG, R_CCR)));
                           emitl(A_BRA,label2);
                           emitl(A_LABEL,label1);
                           { equal - set zero flag }
                           exprasmlist^.concat(new(pai68k, op_const_reg(
                             A_OR,S_B, ZERO_FLAG, R_CCR)));
                           emitl(A_LABEL,label2);
                        end;
         ltn:           begin
                           emitl(A_FBLT,label1);
                           { not less than       }
                           { clear N and V flags }
                           exprasmlist^.concat(new(pai68k, op_const_reg(
                             A_AND, S_B, NOT (NEG_FLAG OR OVFL_FLAG), R_CCR)));
                           emitl(A_BRA,label2);
                           emitl(A_LABEL,label1);
                           { less than }
                           exprasmlist^.concat(new(pai68k, op_const_reg(
                             A_OR,S_B, NEG_FLAG, R_CCR)));
                           exprasmlist^.concat(new(pai68k, op_const_reg(
                             A_AND,S_B, NOT OVFL_FLAG, R_CCR)));
                           emitl(A_LABEL,label2);
                        end;
         gtn:           begin
                           emitl(A_FBGT,label1);
                           { not greater than }
                           { set Z flag       }
                           exprasmlist^.concat(new(pai68k, op_const_reg(
                             A_OR, S_B, ZERO_FLAG, R_CCR)));
                           emitl(A_BRA,label2);
                           emitl(A_LABEL,label1);
                           { greater than      }
                           { set N and V flags }
                           exprasmlist^.concat(new(pai68k, op_const_reg(
                             A_OR,S_B, NEG_FLAG OR OVFL_FLAG , R_CCR)));
                           emitl(A_LABEL,label2);
                        end;
         gten:           begin
                           emitl(A_FBGE,label1);
                           { not greater or equal }
                           { set N and clear V    }
                           exprasmlist^.concat(new(pai68k, op_const_reg(
                             A_AND, S_B, NOT OVFL_FLAG, R_CCR)));
                           exprasmlist^.concat(new(pai68k, op_const_reg(
                             A_OR,S_B, NEG_FLAG, R_CCR)));
                           emitl(A_BRA,label2);
                           emitl(A_LABEL,label1);
                           { greater or equal    }
                           { clear V and N flags }
                           exprasmlist^.concat(new(pai68k, op_const_reg(
                             A_AND, S_B, NOT (OVFL_FLAG OR NEG_FLAG), R_CCR)));
                           emitl(A_LABEL,label2);
                        end;
         lten:           begin
                           emitl(A_FBLE,label1);
                           { not less or equal }
                           { clear Z, N and V  }
                           exprasmlist^.concat(new(pai68k, op_const_reg(
                             A_AND, S_B, NOT (ZERO_FLAG OR NEG_FLAG OR OVFL_FLAG), R_CCR)));
                           emitl(A_BRA,label2);
                           emitl(A_LABEL,label1);
                           { less or equal     }
                           { set Z and N       }
                           { and clear V       }
                           exprasmlist^.concat(new(pai68k, op_const_reg(
                             A_OR,S_B, ZERO_FLAG OR NEG_FLAG, R_CCR)));
                           exprasmlist^.concat(new(pai68k, op_const_reg(
                             A_AND,S_B, NOT OVFL_FLAG, R_CCR)));
                           emitl(A_LABEL,label2);
                        end;
           else
             begin
               InternalError(34);
             end;
      end; { end case }
 end;

    procedure secondfor(var p : ptree);

      var
         l1,l3,oldclabel,oldblabel : plabel;
         omitfirstcomp,temptovalue : boolean;
         hs : byte;
         temp1 : treference;
         hop : tasmop;
         cmpreg,cmp32 : tregister;
         opsize : topsize;
         count_var_is_signed : boolean;

      begin
         oldclabel:=aktcontinuelabel;
         oldblabel:=aktbreaklabel;
         getlabel(aktcontinuelabel);
         getlabel(aktbreaklabel);
         getlabel(l3);

         { could we spare the first comparison ? }
         omitfirstcomp:=false;
         if p^.right^.treetype=ordconstn then
           if p^.left^.right^.treetype=ordconstn then
             omitfirstcomp:=(p^.backward and (p^.left^.right^.value>=p^.right^.value))
               or (not(p^.backward) and (p^.left^.right^.value<=p^.right^.value));

         { only calculate reference }
         cleartempgen;
         secondpass(p^.t2);
         if not(simple_loadn) then
          Message(cg_e_illegal_count_var);

         { produce start assignment }
         cleartempgen;
         secondpass(p^.left);
         count_var_is_signed:=is_signed(porddef(p^.t2^.resulttype));
         hs:=p^.t2^.resulttype^.size;
         cmp32:=getregister32;
         cmpreg:=cmp32;
         case hs of
            1 : begin
                   opsize:=S_B;
                end;
            2 : begin
                   opsize:=S_W;
                end;
            4 : begin
                   opsize:=S_L;
                end;
         end;
         cleartempgen;
         secondpass(p^.right);
         { calculate pointer value and check if changeable and if so }
         { load into temporary variable                              }
         if p^.right^.treetype<>ordconstn then
           begin
              temp1.symbol:=nil;
              gettempofsizereference(hs,temp1);
              temptovalue:=true;
              if (p^.right^.location.loc=LOC_REGISTER) or
                 (p^.right^.location.loc=LOC_CREGISTER) then
                begin
                   exprasmlist^.concat(new(pai68k,op_reg_ref(A_MOVE,opsize,p^.right^.location.register,
                      newreference(temp1))));
                 end
              else
                 concatcopy(p^.right^.location.reference,temp1,hs,false);
           end
         else temptovalue:=false;

         if temptovalue then
           begin
              if p^.t2^.location.loc=LOC_CREGISTER then
               begin
                   exprasmlist^.concat(new(pai68k,op_ref_reg(A_CMP,opsize,newreference(temp1),
                     p^.t2^.location.register)));
                end
              else
                begin
                   exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,opsize,newreference(p^.t2^.location.reference),
                     cmpreg)));
                   exprasmlist^.concat(new(pai68k,op_ref_reg(A_CMP,opsize,newreference(temp1),
                     cmpreg)));
                end;
           end
         else
           begin
              if not(omitfirstcomp) then
                begin
                   if p^.t2^.location.loc=LOC_CREGISTER then
                     exprasmlist^.concat(new(pai68k,op_const_reg(A_CMP,opsize,p^.right^.value,
                       p^.t2^.location.register)))
                   else
                     exprasmlist^.concat(new(pai68k,op_const_ref(A_CMP,opsize,p^.right^.value,
               newreference(p^.t2^.location.reference))));
                end;
           end;
         if p^.backward then
          begin
           if count_var_is_signed then
              hop:=A_BLT
           else
              hop:=A_BCS;
          end
         else
           if count_var_is_signed then
             hop:=A_BGT
           else hop:=A_BHI;

         if not(omitfirstcomp) or temptovalue then
          emitl(hop,aktbreaklabel);

         emitl(A_LABEL,l3);

         { help register must not be in instruction block }
         cleartempgen;
         if assigned(p^.t1) then
           secondpass(p^.t1);

         emitl(A_LABEL,aktcontinuelabel);

         { makes no problems there }
         cleartempgen;

         { demand help register again }
         cmp32:=getregister32;
         case hs of
            1 : begin
                   opsize:=S_B;
                end;
            2 : begin
                   opsize:=S_W;
                end;
            4 : opsize:=S_L;
         end;

     { produce comparison and the corresponding }
     { jump                                     }
         if temptovalue then
           begin
              if p^.t2^.location.loc=LOC_CREGISTER then
                begin
                   exprasmlist^.concat(new(pai68k,op_ref_reg(A_CMP,opsize,newreference(temp1),
                     p^.t2^.location.register)));
                end
              else
                begin
                   exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,opsize,newreference(p^.t2^.location.reference),
                     cmpreg)));
                   exprasmlist^.concat(new(pai68k,op_ref_reg(A_CMP,opsize,newreference(temp1),
                     cmpreg)));
                end;
           end
         else
           begin
              if p^.t2^.location.loc=LOC_CREGISTER then
                exprasmlist^.concat(new(pai68k,op_const_reg(A_CMP,opsize,p^.right^.value,
                  p^.t2^.location.register)))
              else
                exprasmlist^.concat(new(pai68k,op_const_ref(A_CMP,opsize,p^.right^.value,
                  newreference(p^.t2^.location.reference))));
           end;
         if p^.backward then
           if count_var_is_signed then
             hop:=A_BLE
           else
             hop :=A_BLS
          else
            if count_var_is_signed then
              hop:=A_BGE
            else
               hop:=A_BCC;
         emitl(hop,aktbreaklabel);
         { according to count direction DEC or INC... }
         { must be after the test because of 0to 255 for bytes !! }
         if p^.backward then
           hop:=A_SUB
         else hop:=A_ADD;

         if p^.t2^.location.loc=LOC_CREGISTER then
           exprasmlist^.concat(new(pai68k,op_const_reg(hop,opsize,1,p^.t2^.location.register)))
         else
            exprasmlist^.concat(new(pai68k,op_const_ref(hop,opsize,1,newreference(p^.t2^.location.reference))));
         emitl(A_JMP,l3);

     { this is the break label: }
         emitl(A_LABEL,aktbreaklabel);
         ungetregister32(cmp32);

         if temptovalue then
           ungetiftemp(temp1);

         aktcontinuelabel:=oldclabel;
         aktbreaklabel:=oldblabel;
      end;


    procedure secondas(var p : ptree);

      var
         pushed : tpushed;

      begin
         secondpass(p^.left);
         { save all used registers }
         pushusedregisters(pushed,$ffff);

         { push instance to check: }
         case p^.left^.location.loc of
            LOC_REGISTER,LOC_CREGISTER:
              exprasmlist^.concat(new(pai68k,op_reg_reg(A_MOVE,
                S_L,p^.left^.location.register,R_SPPUSH)));
            LOC_MEM,LOC_REFERENCE:
              exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,
                S_L,newreference(p^.left^.location.reference),R_SPPUSH)));
            else internalerror(100);
         end;

         { we doesn't modifiy the left side, we check only the type }
         set_location(p^.location,p^.left^.location);

         { generate type checking }
         secondpass(p^.right);
         case p^.right^.location.loc of
            LOC_REGISTER,LOC_CREGISTER:
              begin
                 exprasmlist^.concat(new(pai68k,op_reg_reg(A_MOVE,
                   S_L,p^.right^.location.register,R_SPPUSH)));
                 ungetregister32(p^.right^.location.register);
              end;
            LOC_MEM,LOC_REFERENCE:
              begin
                 exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,
                   S_L,newreference(p^.right^.location.reference),R_SPPUSH)));
                 del_reference(p^.right^.location.reference);
              end;
            else internalerror(100);
         end;
         emitcall('DO_AS',true);
         { restore register, this restores automatically the }
         { result                                            }
         popusedregisters(pushed);
      end;


    { generates the code for a raise statement }
    procedure secondraise(var p : ptree);

      var
         a : plabel;

      begin
         if assigned(p^.left) then
           begin
              { generate the address }
              if assigned(p^.right) then
                begin
                   secondpass(p^.right);
                   if codegenerror then
                     exit;
                end
              else
                begin
                   getlabel(a);
                   emitl(A_LABEL,a);
                   exprasmlist^.concat(new(pai68k,
                     op_csymbol_reg(A_MOVE,S_L,newcsymbol(lab2str(a),0),R_SPPUSH)));
                end;
              secondpass(p^.left);
              if codegenerror then
                exit;

              case p^.left^.location.loc of
                 LOC_MEM,LOC_REFERENCE : emitpushreferenceaddr(p^.left^.location.reference);
                 LOC_CREGISTER,LOC_REGISTER : exprasmlist^.concat(new(pai68k,op_reg_reg(A_MOVE,S_L,
                   p^.left^.location.register,R_SPPUSH)));
                 else Message(sym_e_type_mismatch);
              end;
              emitcall('DO_RAISE',true);
           end
         else
           emitcall('DO_RERAISE',true);
      end;



    { This routine needs to be further checked to see if it works correctly  }
    { because contrary to the intel version, all large set elements are read }
    { as 32-bit values, and then decomposed to find the correct byte.        }

    { CHECKED : Depending on the result size, if reference, a load may be    }
    { required on word, long or byte.                                        }
    procedure loadsetelement(var p : ptree);

      var
         hr : tregister;
         opsize : topsize;

      begin
         { copy the element in the d0.b register, slightly complicated }
         case p^.location.loc of
            LOC_REGISTER,
            LOC_CREGISTER : begin
                              hr:=p^.location.register;
                              emit_reg_reg(A_MOVE,S_L,hr,R_D0);
                              ungetregister32(hr);
                           end;
            else
               begin
                 { This is quite complicated, because of the endian on }
                 { the m68k!                                           }
                 opsize:=S_NO;
                 case integer(p^.resulttype^.savesize) of
                   1 : opsize:=S_B;
                   2 : opsize:=S_W;
                   4 : opsize:=S_L;
                 else
                   internalerror(19);
                 end;
                 exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,opsize,
                    newreference(p^.location.reference),R_D0)));
                 exprasmlist^.concat(new(pai68k,op_const_reg(A_AND,S_L,
                    255,R_D0)));
{
                  exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,S_L,
                    newreference(p^.location.reference),R_D0)));        }
{                  exprasmlist^.concat(new(pai68k,op_const_reg(A_AND,S_L,
                    $ff,R_D0))); }
                  del_reference(p^.location.reference);
               end;
         end;
      end;

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
         href,href2:Treference;
         l,l2 : plabel;
         hl,hl1 : plabel;
         hl2, hl3: plabel;
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
                         exprasmlist^.concat(new(pai68k,
                           op_const_reg(A_AND,S_L, 1 shl (p^.left^.value and 31),R_D1)));
                       end;
                   else
                       begin
                         exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,S_L,newreference(
                           p^.right^.location.reference),R_D1)));
                         exprasmlist^.concat(new(pai68k,op_const_reg(
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
                            exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,S_L,
                              newreference(p^.left^.location.reference),R_D1)));
                            hr:=R_D1;
                            del_reference(p^.left^.location.reference);
                         end;
                   end;
                   case p^.right^.location.loc of
                      LOC_REGISTER,
                      LOC_CREGISTER : exprasmlist^.concat(new(pai68k, op_reg_reg(A_BTST,S_L,hr,p^.right^.location.register)));
                      else
                         begin
                            exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,S_L,newreference(p^.right^.location.reference),
                              R_D0)));
                            exprasmlist^.concat(new(pai68k,op_reg_reg(A_BTST,S_L,hr,R_D0)));
                            del_reference(p^.right^.location.reference);
                         end;
                   end;
                   { support carry routines }
                   { sets the carry flags according to the result of BTST }
                   { i.e the Z flag.                                      }
                   getlabel(hl);
                   emitl(A_BNE,hl);
                   { leave all bits unchanged except Carry  = 0 }
                   exprasmlist^.concat(new(pai68k, op_const_reg(A_AND, S_B, $FE, R_CCR)));
                   getlabel(hl1);
                   emitl(A_BRA,hl1);
                   emitl(A_LABEL, hl);
                   { set carry to 1 }
                   exprasmlist^.concat(new(pai68k, op_const_reg(A_OR, S_B, $01, R_CCR)));
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
                   exprasmlist^.concat(new(pai68k, op_ref_reg(A_MOVE, S_L,
                       newreference(p^.right^.location.reference), R_D1)));
                   exprasmlist^.concat(new(pai68k, op_const_reg(A_AND, S_L,
                       1 shl (p^.left^.value mod 32),R_D1)));
                   del_reference(p^.right^.location.reference);
                end
             else
                begin
                  if (p^.right^.treetype=setconstrn) and
                     analizeset(p^.right^.constset) then
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
                           exprasmlist^.concat(new(pai68k,op_const_reg(A_AND,S_L,
                             255,p^.left^.location.register)));
                        else
                         Begin
                           { Because of the m68k endian, then we must LOAD normally the    }
                           { value into a register first, all depending on the source      }
                           { size!                                                         }
                           opsize:=S_NO;
                           case integer(p^.left^.resulttype^.savesize) of
                             1 : opsize:=S_B;
                             2 : opsize:=S_W;
                             4 : opsize:=S_L;
                           else
                             internalerror(19);
                           end;
                           exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,opsize,
                             newreference(p^.left^.location.reference),R_D0)));
                           exprasmlist^.concat(new(pai68k,op_const_reg(A_AND,S_L,
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
                      href.symbol := nil;
                      clear_reference(href);
                      getlabel(l);
                      href.symbol:=stringdup(lab2str(l));
                      for i:=1 to numparts do
                          if setparts[i].range then
                             begin
                                  {Check if left is in a range.}
                                  {Get a label to jump over the check.}
                                  href2.symbol := nil;
                                  clear_reference(href2);
                                  getlabel(l2);
                                  href.symbol:=stringdup(lab2str(l2));
                                  if setparts[i].start=setparts[i].stop-1 then
                                  begin
                                    case p^.left^.location.loc of
                                      LOC_REGISTER,
                                      LOC_CREGISTER :
                                         exprasmlist^.concat(new(pai68k,op_const_reg(A_CMP,S_W,
                                           setparts[i].start,p^.left^.location.register)));
                                    else
                                         exprasmlist^.concat(new(pai68k,op_const_reg(A_CMP,S_W,
                                           setparts[i].start,R_D0)));
{                                         exprasmlist^.concat(new(pai68k,op_const_ref(A_CMP,S_B,
                                           setparts[i].start,newreference(p^.left^.location.reference))));}
                                    end;
                                  {Result should be in carry flag when ranges are used.}
                                  { Here the m68k does not affect any flag except the  }
                                  { flag which is OR'ed                                }
                                  if ranges then
                                     exprasmlist^.concat(new(pai68k,op_const_reg(A_OR,S_B,$01,R_CCR)));
                                  {If found, jump to end.}
                                  emitl(A_BEQ,l);
                                  case p^.left^.location.loc of
                                    LOC_REGISTER,
                                    LOC_CREGISTER :
                                      exprasmlist^.concat(new(pai68k,op_const_reg(A_CMP,S_W,
                                        setparts[i].stop,p^.left^.location.register)));
                                    else
                                      exprasmlist^.concat(new(pai68k,op_const_reg(A_CMP,S_W,
                                        setparts[i].stop,R_D0)));
{                                      exprasmlist^.concat(new(pai68k,op_const_ref(A_CMP,S_B,
                                      setparts[i].stop,newreference(p^.left^.location.reference))));}
                                  end;
                                  {Result should be in carry flag when ranges are used.}
                                  { Here the m68k does not affect any flag except the  }
                                  { flag which is OR'ed                                }
                                  if ranges then
                                     exprasmlist^.concat(new(pai68k,op_const_reg(A_OR,S_B,$01,R_CCR)));
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
                                        exprasmlist^.concat(new(pai68k,op_const_reg(A_CMP,S_W,
                                        setparts[i].start,p^.left^.location.register)));
                                    else
                                        exprasmlist^.concat(new(pai68k,op_const_reg(A_CMP,S_W,
                                        setparts[i].start,R_D0)));
{                                        exprasmlist^.concat(new(pai68k,op_const_ref(A_CMP,S_B,
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
                                              exprasmlist^.concat(new(pai68k,op_const_reg(A_CMP,S_W,
                                                setparts[i].stop+1,p^.left^.location.register)));
                                          else
                                              exprasmlist^.concat(new(pai68k,op_const_reg(A_CMP,S_W,
                                                setparts[i].stop+1,R_D0)));
{                                              exprasmlist^.concat(new(pai68k,op_const_ref(A_CMP,S_B,
                                                setparts[i].stop+1,newreference(p^.left^.location.reference))));}
                                          end; { end case }
                                          {If higher, element is in set.}
                                          emitl(A_BCS,l);
                                       end
                                     else
                                       begin
                                         exprasmlist^.concat(new(pai68k,op_const_reg(A_OR,S_B,$01,R_CCR)));
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
                                     exprasmlist^.concat(new(pai68k,op_const_reg(A_CMP,S_W,
                                      setparts[i].stop,p^.left^.location.register)));
                                   else
{                                     exprasmlist^.concat(new(pai68k,op_const_ref(A_CMP,S_B,
                                     setparts[i].stop,newreference(p^.left^.location.reference))));}
                                     exprasmlist^.concat(new(pai68k,op_const_reg(A_CMP,S_W,
                                      setparts[i].stop,R_D0)));
                                   end;
                                 {Result should be in carry flag when ranges are used.}
                                 if ranges then
                                   exprasmlist^.concat(new(pai68k, op_const_reg(A_OR,S_B,$01,R_CCR)));
                                   {If found, jump to end.}
                                 emitl(A_BEQ,l);
                               end;
                            if ranges then
                            { clear carry flag }
                                exprasmlist^.concat(new(pai68k,op_const_reg(A_AND,S_B,$FE,R_CCR)));
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
                           set_location(p^.location,p^.left^.location);
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
                            exprasmlist^.concat(new(pai68k,op_ref_reg(A_LEA,S_L,
                              newreference(p^.right^.location.reference),R_A0)));;
{                            emitpushreferenceaddr(p^.right^.location.reference);}
                            del_reference(p^.right^.location.reference);
                            emitcall('SET_IN_BYTE',true);
                            { ungetiftemp(p^.right^.location.reference); }
                            p^.location.loc:=LOC_FLAGS;
                            p^.location.resflags:=F_C;
                        end;
                end;
             end;
      end;


    procedure secondexpr(var p : ptree);

      begin
         secondpass(p^.left);
      end;

    procedure secondblockn(var p : ptree);

      var
         hp : ptree;

      begin
         hp:=p^.left;
         while assigned(hp) do
           begin
              { assignments could be distance optimized }
              if assigned(hp^.right) then
                begin
                   cleartempgen;
                   secondpass(hp^.right);
                end;
              hp:=hp^.left;
           end;
      end;

    procedure second_while_repeatn(var p : ptree);

      var
         l1,l2,l3,oldclabel,oldblabel : plabel;
         otlabel,oflabel : plabel;
      begin
         getlabel(l1);
         getlabel(l2);
         { arrange continue and breaklabels: }
         oldclabel:=aktcontinuelabel;
         oldblabel:=aktbreaklabel;
         if p^.treetype=repeatn then
           begin
              emitl(A_LABEL,l1);
              aktcontinuelabel:=l1;
              aktbreaklabel:=l2;
              cleartempgen;
              if assigned(p^.right) then
               secondpass(p^.right);

              otlabel:=truelabel;
              oflabel:=falselabel;
              truelabel:=l2;
              falselabel:=l1;
              cleartempgen;
              secondpass(p^.left);
              maketojumpbool(p^.left);
              emitl(A_LABEL,l2);
              truelabel:=otlabel;
              falselabel:=oflabel;
           end
         else { //// NOT a small set  //// }
           begin
              { handling code at the end as it is much more efficient }
              emitl(A_JMP,l2);

              emitl(A_LABEL,l1);
              cleartempgen;

              getlabel(l3);
              aktcontinuelabel:=l2;
              aktbreaklabel:=l3;

              if assigned(p^.right) then
               secondpass(p^.right);

              emitl(A_LABEL,l2);
              otlabel:=truelabel;
              oflabel:=falselabel;
              truelabel:=l1;
              falselabel:=l3;
              cleartempgen;
              secondpass(p^.left);
              maketojumpbool(p^.left);

              emitl(A_LABEL,l3);
              truelabel:=otlabel;
              falselabel:=oflabel;
           end;
         aktcontinuelabel:=oldclabel;
         aktbreaklabel:=oldblabel;
      end;

    procedure secondifn(var p : ptree);

      var
         hl,otlabel,oflabel : plabel;

      begin
         otlabel:=truelabel;
         oflabel:=falselabel;
         getlabel(truelabel);
         getlabel(falselabel);
         cleartempgen;
         secondpass(p^.left);
         maketojumpbool(p^.left);
         if assigned(p^.right) then
           begin
              emitl(A_LABEL,truelabel);
              cleartempgen;
              secondpass(p^.right);
           end;
         if assigned(p^.t1) then
           begin
              if assigned(p^.right) then
                begin
                   getlabel(hl);
                   emitl(A_JMP,hl);
                end;
              emitl(A_LABEL,falselabel);
              cleartempgen;
              secondpass(p^.t1);
              if assigned(p^.right) then
                emitl(A_LABEL,hl);
           end
         else
           emitl(A_LABEL,falselabel);
         if not(assigned(p^.right)) then
           emitl(A_LABEL,truelabel);
         truelabel:=otlabel;
         falselabel:=oflabel;
      end;

    procedure secondbreakn(var p : ptree);

      begin
         if aktbreaklabel<>nil then
           emitl(A_JMP,aktbreaklabel)
         else
           Message(cg_e_break_not_allowed);
      end;


end.
{
  $Log$
  Revision 1.8  1998-08-31 12:26:23  peter
    * m68k and palmos updates from surebugfixes

  Revision 1.7  1998/08/10 14:43:17  peter
    * string type st_ fixed

  Revision 1.6  1998/07/10 10:51:00  peter
    * m68k updates

  Revision 1.5  1998/06/08 13:13:37  pierre
    + temporary variables now in temp_gen.pas unit
      because it is processor independent
    * mppc68k.bat modified to undefine i386 and support_mmx
      (which are defaults for i386)

  Revision 1.4  1998/06/04 23:51:35  peter
    * m68k compiles
    + .def file creation moved to gendef.pas so it could also be used
      for win32

  Revision 1.3  1998/04/29 10:33:45  pierre
    + added some code for ansistring (not complete nor working yet)
    * corrected operator overloading
    * corrected nasm output
    + started inline procedures
    + added starstarn : use ** for exponentiation (^ gave problems)
    + started UseTokenInfo cond to get accurate positions
}
