{
    $Id$
    Copyright (c) 1993-98 by Florian Klaempfl

    Generate i386 assembler for in add node

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
unit cg386add;
interface

    uses
      tree;

    procedure secondadd(var p : ptree);

implementation

    uses
      cobjects,verbose,globals,
      symtable,aasm,i386,types,
      cgi386,cgai386,temp_gen,tgeni386,hcodegen;

{*****************************************************************************
                                Helpers
*****************************************************************************}

    procedure SetResultLocation(cmpop,unsigned:boolean;var p :ptree);
      var
         flags : tresflags;
      begin
         { remove temporary location if not a set or string }
         if (p^.left^.resulttype^.deftype<>stringdef) and
            ((p^.left^.resulttype^.deftype<>setdef) or (psetdef(p^.left^.resulttype)^.settype=smallset)) and
            (p^.left^.location.loc in [LOC_MEM,LOC_REFERENCE]) then
           ungetiftemp(p^.left^.location.reference);
         if (p^.right^.resulttype^.deftype<>stringdef) and
            ((p^.right^.resulttype^.deftype<>setdef) or (psetdef(p^.right^.resulttype)^.settype=smallset)) and
            (p^.right^.location.loc in [LOC_MEM,LOC_REFERENCE]) then
           ungetiftemp(p^.right^.location.reference);
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
                end;
              p^.location.loc:=LOC_FLAGS;
              p^.location.resflags:=flags;
           end;
      end;


{*****************************************************************************
                                Addstring
*****************************************************************************}

    procedure addstring(var p : ptree);
      var
        pushedregs : tpushed;
        href       : treference;
        pushed,
        cmpop      : boolean;
      begin
        { string operations are not commutative }
        if p^.swaped then
         swaptree(p);

{$ifdef UseAnsiString}
              if is_ansistring(p^.left^.resulttype) then
                begin
                  case p^.treetype of
                  addn :
                    begin
                       { we do not need destination anymore }
                       del_reference(p^.left^.location.reference);
                       del_reference(p^.right^.location.reference);
                       { concatansistring(p); }
                    end;
                  ltn,lten,gtn,gten,
                  equaln,unequaln :
                    begin
                       pushusedregisters(pushedregs,$ff);
                       secondpass(p^.left);
                       del_reference(p^.left^.location.reference);
                       emitpushreferenceaddr(exprasmlist,p^.left^.location.reference);
                       secondpass(p^.right);
                       del_reference(p^.right^.location.reference);
                       emitpushreferenceaddr(exprasmlist,p^.right^.location.reference);
                       emitcall('ANSISTRCMP',true);
                       maybe_loadesi;
                       popusedregisters(pushedregs);
                    end;
                  end;
                end
              else
{$endif UseAnsiString}
       case p^.treetype of
          addn :
            begin
               cmpop:=false;
               secondpass(p^.left);
               { if str_concat is set in expr
                 s:=s+ ... no need to create a temp string (PM) }

               if (p^.left^.treetype<>addn) and not (p^.use_strconcat) then
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
{               if p^.right^.resulttype^.deftype=orddef then
                begin
                  pushusedregisters(pushedregs,$ff);
                  exprasmlist^.concat(new(pai386,op_ref_reg(
                     A_LEA,S_L,newreference(p^.left^.location.reference),R_EDI)));
                  exprasmlist^.concat(new(pai386,op_reg_reg(
                     A_XOR,S_L,R_EBX,R_EBX)));
                  reset_reference(href);
                  href.base:=R_EDI;
                  exprasmlist^.concat(new(pai386,op_ref_reg(
                     A_MOV,S_B,newreference(href),R_BL)));
                  exprasmlist^.concat(new(pai386,op_reg(
                     A_INC,S_L,R_EBX)));
                  exprasmlist^.concat(new(pai386,op_reg_ref(
                     A_MOV,S_B,R_BL,newreference(href))));
                  href.index:=R_EBX;
                  if p^.right^.treetype=ordconstn then
                    exprasmlist^.concat(new(pai386,op_const_ref(
                       A_MOV,S_L,p^.right^.value,newreference(href))))
                  else
                   begin
                     if p^.right^.location.loc in [LOC_CREGISTER,LOC_REGISTER] then
                      exprasmlist^.concat(new(pai386,op_reg_ref(
                        A_MOV,S_B,p^.right^.location.register,newreference(href))))
                     else
                      begin
                        exprasmlist^.concat(new(pai386,op_ref_reg(
                          A_MOV,S_L,newreference(p^.right^.location.reference),R_EAX)));
                        exprasmlist^.concat(new(pai386,op_reg_ref(
                          A_MOV,S_B,R_AL,newreference(href))));
                      end;
                   end;
                  popusedregisters(pushedregs);
                end
               else }
                begin
                  if p^.use_strconcat then
                    pushusedregisters(pushedregs,pstringdef(p^.left^.resulttype)^.len)
                  else
                    pushusedregisters(pushedregs,$ff);
                  emitpushreferenceaddr(exprasmlist,p^.left^.location.reference);
                  emitpushreferenceaddr(exprasmlist,p^.right^.location.reference);
                  emitcall('STRCONCAT',true);
                  maybe_loadesi;
                  popusedregisters(pushedregs);
                end;

               set_location(p^.location,p^.left^.location);
               ungetiftemp(p^.right^.location.reference);
            end;
          ltn,lten,gtn,gten,
          equaln,unequaln :
            begin
               cmpop:=true;
             { generate better code for s='' and s<>'' }
               if (p^.treetype in [equaln,unequaln]) and
                  (((p^.left^.treetype=stringconstn) and (p^.left^.values^='')) or
                   ((p^.right^.treetype=stringconstn) and (p^.right^.values^=''))) then
                 begin
                    secondpass(p^.left);
                    { are too few registers free? }
                    pushed:=maybe_push(p^.right^.registers32,p);
                    secondpass(p^.right);
                    if pushed then restore(p);
                    del_reference(p^.right^.location.reference);
                    del_reference(p^.left^.location.reference);
                    { only one node can be stringconstn }
                    { else pass 1 would have evaluted   }
                    { this node                         }
                    if p^.left^.treetype=stringconstn then
                      exprasmlist^.concat(new(pai386,op_const_ref(
                        A_CMP,S_B,0,newreference(p^.right^.location.reference))))
                    else
                      exprasmlist^.concat(new(pai386,op_const_ref(
                        A_CMP,S_B,0,newreference(p^.left^.location.reference))));
                 end
               else
                 begin
                    pushusedregisters(pushedregs,$ff);
                    secondpass(p^.left);
                    del_reference(p^.left^.location.reference);
                    emitpushreferenceaddr(exprasmlist,p^.left^.location.reference);
                    secondpass(p^.right);
                    del_reference(p^.right^.location.reference);
                    emitpushreferenceaddr(exprasmlist,p^.right^.location.reference);
                    emitcall('STRCMP',true);
                    maybe_loadesi;
                    popusedregisters(pushedregs);
                 end;
               ungetiftemp(p^.left^.location.reference);
               ungetiftemp(p^.right^.location.reference);
            end;
            else Message(sym_e_type_mismatch);
          end;
        SetResultLocation(cmpop,true,p);
      end;


{*****************************************************************************
                                Addset
*****************************************************************************}

    procedure addset(var p : ptree);
      var
        right_small,
        cmpop,
        pushed : boolean;
        href2,
        href   : treference;
        swapp  : ptree;
        pushedregs : tpushed;
      begin
        cmpop:=false;

        { not commutative }
        if p^.swaped then
         swaptree(p);

        secondpass(p^.left);
        { are too few registers free? }
        pushed:=maybe_push(p^.right^.registers32,p);
        secondpass(p^.right);
        if codegenerror then
          exit;
        if pushed then
          restore(p);

        set_location(p^.location,p^.left^.location);
        right_small:=(p^.right^.resulttype^.deftype=setdef) and (psetdef(p^.right^.resulttype)^.settype=smallset);

        { handle operations }
        reset_reference(href2);
        case p^.treetype of
          equaln,
        unequaln : begin
                     cmpop:=true;
                     del_reference(p^.left^.location.reference);
                     del_reference(p^.right^.location.reference);
                     pushusedregisters(pushedregs,$ff);
                     emitpushreferenceaddr(exprasmlist,href2);
                     emitpushreferenceaddr(exprasmlist,p^.left^.location.reference);
                     emitcall('SET_COMP_SETS',true);
                     maybe_loadesi;
                     popusedregisters(pushedregs);
                     ungetiftemp(p^.left^.location.reference);
                     ungetiftemp(p^.right^.location.reference);
                   end;
            addn : begin
                   { add can be an other SET or Range or Element ! }
                     del_reference(p^.left^.location.reference);
                     del_reference(p^.right^.location.reference);
                     pushusedregisters(pushedregs,$ff);
                     href.symbol:=nil;
                     gettempofsizereference(32,href);
                     case p^.right^.treetype of
                    setelen : begin
                                concatcopy(p^.left^.location.reference,href,32,false);
                                pushsetelement(p^.right^.left);
                                emitpushreferenceaddr(exprasmlist,href);
                                emitcall('SET_SET_BYTE',true);
                              end;
                     rangen : begin
                                concatcopy(p^.left^.location.reference,href,32,false);
                                pushsetelement(p^.right^.right);
                                pushsetelement(p^.right^.left);
                                emitpushreferenceaddr(exprasmlist,href);
                                emitcall('SET_SET_RANGE',true);
                              end;
                     else
                      begin
                      { must be an other set }
                        emitpushreferenceaddr(exprasmlist,href);
                        emitpushreferenceaddr(exprasmlist,p^.right^.location.reference);
                        emitpushreferenceaddr(exprasmlist,p^.left^.location.reference);
                        emitcall('SET_ADD_SETS',true);
                      end;
                     end;
                     maybe_loadesi;
                     popusedregisters(pushedregs);
                     ungetiftemp(p^.left^.location.reference);
                     ungetiftemp(p^.right^.location.reference);
                     p^.location.loc:=LOC_MEM;
                     stringdispose(p^.location.reference.symbol);
                     p^.location.reference:=href;
                   end;
            subn,
         symdifn,
            muln : begin
                     if p^.right^.treetype in [rangen,setelen] then
                      internalerror(45362);
                     del_reference(p^.left^.location.reference);
                     del_reference(p^.right^.location.reference);
                     href.symbol:=nil;
                     pushusedregisters(pushedregs,$ff);
                     gettempofsizereference(32,href);
                     emitpushreferenceaddr(exprasmlist,href);
                     emitpushreferenceaddr(exprasmlist,p^.right^.location.reference);
                     emitpushreferenceaddr(exprasmlist,p^.left^.location.reference);
                     case p^.treetype of
                      subn : emitcall('SET_SUB_SETS',true);
                   symdifn : emitcall('SET_SYMDIF_SETS',true);
                      muln : emitcall('SET_MUL_SETS',true);
                     end;
                     maybe_loadesi;
                     popusedregisters(pushedregs);
                     ungetiftemp(p^.left^.location.reference);
                     ungetiftemp(p^.right^.location.reference);
                     p^.location.loc:=LOC_MEM;
                     stringdispose(p^.location.reference.symbol);
                     p^.location.reference:=href;
                   end;
        else
          Message(sym_e_type_mismatch);
        end;
        SetResultLocation(cmpop,true,p);
      end;


{*****************************************************************************
                                SecondAdd
*****************************************************************************}

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
         hl4: plabel;

         { true, if unsigned types are compared }
         unsigned : boolean;

         { is_in_dest if the result is put directly into }
         { the resulting refernce or varregister }
         { true, if a small set is handled with the longint code }
         is_set : boolean;
         is_in_dest : boolean;
         { true, if for sets subtractions the extra not should generated }
         extra_not : boolean;

{$ifdef SUPPORT_MMX}
         mmxbase : tmmxtype;
{$endif SUPPORT_MMX}

      begin
      { to make it more readable, string and set (not smallset!) have their
        own procedures }
        case p^.left^.resulttype^.deftype of
         stringdef : begin
                       addstring(p);
                       exit;
                     end;
            setdef : begin
                     { not for smallsets }
                       if not(psetdef(p^.left^.resulttype)^.settype=smallset) then
                        begin
                          addset(p);
                          exit;
                        end;
                     end;
        end;

         unsigned:=false;
         is_in_dest:=false;
         extra_not:=false;

         opsize:=S_L;

         { calculate the operator which is more difficult }
         firstcomplex(p);
         { handling boolean expressions extra: }
         if ((p^.left^.resulttype^.deftype=orddef) and
            (porddef(p^.left^.resulttype)^.typ in [bool8bit,bool16bit,bool32bit])) or
            ((p^.right^.resulttype^.deftype=orddef) and
            (porddef(p^.right^.resulttype)^.typ in [bool8bit,bool16bit,bool32bit])) then
           begin
             if (porddef(p^.left^.resulttype)^.typ=bool8bit) or
                (porddef(p^.right^.resulttype)^.typ=bool8bit) then
               opsize:=S_B
             else
               if (porddef(p^.left^.resulttype)^.typ=bool16bit) or
                  (porddef(p^.right^.resulttype)^.typ=bool16bit) then
                 opsize:=S_W
             else
               opsize:=S_L;
             case p^.treetype of
              andn,
               orn : begin
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
                       else
                         Message(sym_e_type_mismatch);
                       end;
                       secondpass(p^.right);
                       maketojumpbool(p^.right);
                     end;
          unequaln,
       equaln,xorn : begin
                       if p^.left^.treetype=ordconstn then
                        swaptree(p);
                       secondpass(p^.left);
                       p^.location:=p^.left^.location;
                       { are enough registers free ? }
                       pushed:=maybe_push(p^.right^.registers32,p);
                       secondpass(p^.right);
                       if pushed then restore(p);
                       goto do_normal;
                    end
             else
               Message(sym_e_type_mismatch);
             end
           end
         else
           begin
              { in case of constant put it to the left }
              if p^.left^.treetype=ordconstn then
               swaptree(p);
              secondpass(p^.left);
              { this will be complicated as
               a lot of code below assumes that
               p^.location and p^.left^.location are the same }

{$ifdef test_dest_loc}
              if dest_loc_known and (dest_loc_tree=p) and
                 ((dest_loc.loc=LOC_REGISTER) or (dest_loc.loc=LOC_CREGISTER)) then
                begin
                   set_location(p^.location,dest_loc);
                   in_dest_loc:=true;
                   is_in_dest:=true;
                end
              else
{$endif test_dest_loc}
                set_location(p^.location,p^.left^.location);

              { are too few registers free? }
              pushed:=maybe_push(p^.right^.registers32,p);
              secondpass(p^.right);
              if pushed then
                restore(p);

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
                 (porddef(p^.right^.resulttype)^.typ=u32bit)) or

                { as well as small sets }
                ((p^.left^.resulttype^.deftype=setdef) and
                 (psetdef(p^.left^.resulttype)^.settype=smallset)
                ) then
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
                             end;
                      symdifn : begin
                                { the symetric diff is only for sets }
                                if is_set then
                                  begin
                                     op:=A_XOR;
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
                                       op:=A_MUL
                                     else
                                       op:=A_IMUL;
                                     mboverflow:=true;
                                  end;
                             end;
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
                             end;
                      ltn,lten,gtn,gten,
                      equaln,unequaln :
                             begin
                                op:=A_CMP;
                                cmpop:=true;
                             end;
                      xorn : op:=A_XOR;
                      orn : op:=A_OR;
                      andn : op:=A_AND;
                      else Message(sym_e_type_mismatch);
                   end;
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
                                  emit_reg_reg(A_MOV,opsize,p^.left^.location.register,
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
                                  case opsize of
                                     S_L : hregister:=getregister32;
                                     S_B : hregister:=reg32toreg8(getregister32);
                                  end;
                                  emit_reg_reg(A_MOV,opsize,p^.left^.location.register,
                                    hregister);
                               end
                          end
                        else
                          begin
                             del_reference(p^.left^.location.reference);
                             if is_in_dest then
                               begin
                                  hregister:=p^.location.register;
                                  exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,opsize,
                                  newreference(p^.left^.location.reference),hregister)));
                               end
                             else
                               begin
                                  { first give free, then demand new register }
                                  case opsize of
                                     S_L : hregister:=getregister32;
                                     S_W : hregister:=reg32toreg16(getregister32);
                                     S_B : hregister:=reg32toreg8(getregister32);
                                  end;
                                  exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,opsize,
                                    newreference(p^.left^.location.reference),hregister)));
                               end;
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
                   { at this point, p^.location.loc should be LOC_REGISTER }
                   { and p^.location.register should be a valid register   }
                   { containing the left result                            }
                   if p^.right^.location.loc<>LOC_REGISTER then
                     begin
                        if (p^.treetype=subn) and p^.swaped then
                          begin
                             if p^.right^.location.loc=LOC_CREGISTER then
                               begin
                                  if extra_not then
                                    exprasmlist^.concat(new(pai386,op_reg(A_NOT,opsize,p^.location.register)));

                                  emit_reg_reg(A_MOV,opsize,p^.right^.location.register,R_EDI);
                                  emit_reg_reg(op,opsize,p^.location.register,R_EDI);
                                  emit_reg_reg(A_MOV,opsize,R_EDI,p^.location.register);
                               end
                             else
                               begin
                                  if extra_not then
                                    exprasmlist^.concat(new(pai386,op_reg(A_NOT,opsize,p^.location.register)));

                                  exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,opsize,
                                    newreference(p^.right^.location.reference),R_EDI)));
                                  exprasmlist^.concat(new(pai386,op_reg_reg(op,opsize,p^.location.register,R_EDI)));
                                  exprasmlist^.concat(new(pai386,op_reg_reg(A_MOV,opsize,R_EDI,p^.location.register)));
                                  del_reference(p^.right^.location.reference);
                               end;
                          end
                        else
                          begin
                             if (p^.right^.treetype=ordconstn) and
                                (op=A_CMP) and
                                (p^.right^.value=0) then
                               begin
                                  exprasmlist^.concat(new(pai386,op_reg_reg(A_TEST,opsize,p^.location.register,
                                    p^.location.register)));
                               end
                             else if (p^.right^.treetype=ordconstn) and
                                (op=A_ADD) and
                                (p^.right^.value=1) then
                               begin
                                  exprasmlist^.concat(new(pai386,op_reg(A_INC,opsize,
                                    p^.location.register)));
                               end
                             else if (p^.right^.treetype=ordconstn) and
                                (op=A_SUB) and
                                (p^.right^.value=1) then
                               begin
                                  exprasmlist^.concat(new(pai386,op_reg(A_DEC,opsize,
                                    p^.location.register)));
                               end
                             else if (p^.right^.treetype=ordconstn) and
                                (op=A_IMUL) and
                                (ispowerof2(p^.right^.value,power)) then
                               begin
                                  exprasmlist^.concat(new(pai386,op_const_reg(A_SHL,opsize,power,
                                    p^.location.register)));
                               end
                             else
                               begin
                                  if (p^.right^.location.loc=LOC_CREGISTER) then
                                    begin
                                       if extra_not then
                                         begin
                                            emit_reg_reg(A_MOV,S_L,p^.right^.location.register,R_EDI);
                                            exprasmlist^.concat(new(pai386,op_reg(A_NOT,S_L,R_EDI)));
                                            emit_reg_reg(A_AND,S_L,R_EDI,
                                              p^.location.register);
                                         end
                                       else
                                         begin
                                            emit_reg_reg(op,opsize,p^.right^.location.register,
                                              p^.location.register);
                                         end;
                                    end
                                  else
                                    begin
                                       if extra_not then
                                         begin
                                            exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,newreference(
                                              p^.right^.location.reference),R_EDI)));
                                            exprasmlist^.concat(new(pai386,op_reg(A_NOT,S_L,R_EDI)));
                                            emit_reg_reg(A_AND,S_L,R_EDI,
                                              p^.location.register);
                                         end
                                       else
                                         begin
                                            exprasmlist^.concat(new(pai386,op_ref_reg(op,opsize,newreference(
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
                               exprasmlist^.concat(new(pai386,op_reg(A_NOT,S_L,p^.location.register)));

                             exprasmlist^.concat(new(pai386,op_reg_reg(op,opsize,
                               p^.location.register,p^.right^.location.register)));
                               swap_location(p^.location,p^.right^.location);
                               { newly swapped also set swapped flag }
                               { just to maintain ordering           }
                               p^.swaped:=not(p^.swaped);
                          end
                        else
                          begin
                             if extra_not then
                               exprasmlist^.concat(new(pai386,op_reg(A_NOT,S_L,p^.right^.location.register)));
                             exprasmlist^.concat(new(pai386,op_reg_reg(op,opsize,
                               p^.right^.location.register,
                               p^.location.register)));
                          end;
                        case opsize of
                           S_L : ungetregister32(p^.right^.location.register);
                           S_B : ungetregister32(reg8toreg32(p^.right^.location.register));
                        end;
                     end;

                   if cmpop then
                     case opsize of
                        S_L : ungetregister32(p^.location.register);
                        S_B : ungetregister32(reg8toreg32(p^.location.register));
                     end;

                   { only in case of overflow operations }
                   { produce overflow code }
                   if mboverflow then
                   { we must put it here directly, because sign of operation }
                   { is in unsigned VAR!!                                    }
                   begin
                     if cs_check_overflow in aktlocalswitches  then
                     begin
                       getlabel(hl4);
                       if unsigned then
                        emitl(A_JNB,hl4)
                       else
                        emitl(A_JNO,hl4);
                       emitcall('RE_OVERFLOW',true);
                       emitl(A_LABEL,hl4);
                     end;
                   end;
                end
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
                                  hregister:=reg32toreg8(getregister32);
                                  emit_reg_reg(A_MOV,S_B,p^.location.register,
                                    hregister);
                               end;
                          end
                        else
                          begin
                             del_reference(p^.location.reference);

                             { first give free then demand new register }
                             hregister:=reg32toreg8(getregister32);
                             exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_B,newreference(p^.location.reference),
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
                             exprasmlist^.concat(new(pai386,op_ref_reg(A_CMP,S_B,newreference(
                                p^.right^.location.reference),p^.location.register)));
                             del_reference(p^.right^.location.reference);
                          end;
                     end
                   else
                     begin
                        emit_reg_reg(A_CMP,S_B,p^.right^.location.register,
                          p^.location.register);
                        ungetregister32(reg8toreg32(p^.right^.location.register));
                     end;
                   ungetregister32(reg8toreg32(p^.location.register));
                end
              else if (p^.left^.resulttype^.deftype=floatdef) and
                  (pfloatdef(p^.left^.resulttype)^.typ<>f32bit) then
                 begin
                    { real constants to the left }
                    if p^.left^.treetype=realconstn then
                      swaptree(p);
                    cmpop:=false;
                    case p^.treetype of
                       addn : op:=A_FADDP;
                       muln : op:=A_FMULP;
                       subn : op:=A_FSUBP;
                       slashn : op:=A_FDIVP;
                       ltn,lten,gtn,gten,
                       equaln,unequaln : begin
                                            op:=A_FCOMPP;
                                            cmpop:=true;
                                         end;
                       else Message(sym_e_type_mismatch);
                    end;

                    if (p^.right^.location.loc<>LOC_FPU) then
                      begin
                         floatload(pfloatdef(p^.right^.resulttype)^.typ,p^.right^.location.reference);
                         if (p^.left^.location.loc<>LOC_FPU) then
                           floatload(pfloatdef(p^.left^.resulttype)^.typ,p^.left^.location.reference)
                         { left was on the stack => swap }
                         else
                           p^.swaped:=not(p^.swaped);

                         { releases the right reference }
                         del_reference(p^.right^.location.reference);
                      end
                    { the nominator in st0 }
                    else if (p^.left^.location.loc<>LOC_FPU) then
                      floatload(pfloatdef(p^.left^.resulttype)^.typ,p^.left^.location.reference)
                    { fpu operands are always in the wrong order on the stack }
                    else
                      p^.swaped:=not(p^.swaped);

                    { releases the left reference }
                    if (p^.left^.location.loc<>LOC_FPU) then
                      del_reference(p^.left^.location.reference);

                    { if we swaped the tree nodes, then use the reverse operator }
                    if p^.swaped then
                      begin
                         if (p^.treetype=slashn) then
                           op:=A_FDIVRP
                         else if (p^.treetype=subn) then
                           op:=A_FSUBRP;
                      end;
                    { to avoid the pentium bug
                    if (op=FDIVP) and (opt_processors=pentium) then
                      exprasmlist^.concat(new(pai386,op_CALL,S_NO,'EMUL_FDIVP')
                    else
                    }
                    { the Intel assemblers want operands }
                    if op<>A_FCOMPP then
                       exprasmlist^.concat(new(pai386,op_reg_reg(op,S_NO,R_ST,R_ST1)))
                    else
                      exprasmlist^.concat(new(pai386,op_none(op,S_NO)));
                    { on comparison load flags }
                    if cmpop then
                      begin
                         if not(R_EAX in unused) then
                           emit_reg_reg(A_MOV,S_L,R_EAX,R_EDI);
                         exprasmlist^.concat(new(pai386,op_reg(A_FNSTSW,S_NO,R_AX)));
                         exprasmlist^.concat(new(pai386,op_none(A_SAHF,S_NO)));
                         if not(R_EAX in unused) then
                           emit_reg_reg(A_MOV,S_L,R_EDI,R_EAX);
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
                         p^.location.loc:=LOC_FLAGS;
                         p^.location.resflags:=flags;
                         cmpop:=false;
                      end
                    else
                      p^.location.loc:=LOC_FPU;
                 end
{$ifdef SUPPORT_MMX}
               else if is_mmx_able_array(p^.left^.resulttype) then
                 begin
                   cmpop:=false;
                   mmxbase:=mmx_type(p^.left^.resulttype);
                   case p^.treetype of
                      addn : begin
                                if (cs_mmx_saturation in aktlocalswitches) then
                                  begin
                                     case mmxbase of
                                        mmxs8bit:
                                          op:=A_PADDSB;
                                        mmxu8bit:
                                          op:=A_PADDUSB;
                                        mmxs16bit,mmxfixed16:
                                          op:=A_PADDSB;
                                        mmxu16bit:
                                          op:=A_PADDUSW;
                                     end;
                                  end
                                else
                                  begin
                                     case mmxbase of
                                        mmxs8bit,mmxu8bit:
                                          op:=A_PADDB;
                                        mmxs16bit,mmxu16bit,mmxfixed16:
                                          op:=A_PADDW;
                                        mmxs32bit,mmxu32bit:
                                          op:=A_PADDD;
                                     end;
                                  end;
                             end;
                      muln : begin
                                case mmxbase of
                                   mmxs16bit,mmxu16bit:
                                     op:=A_PMULLW;
                                   mmxfixed16:
                                     op:=A_PMULHW;
                                end;
                             end;
                      subn : begin
                                if (cs_mmx_saturation in aktlocalswitches) then
                                  begin
                                     case mmxbase of
                                        mmxs8bit:
                                          op:=A_PSUBSB;
                                        mmxu8bit:
                                          op:=A_PSUBUSB;
                                        mmxs16bit,mmxfixed16:
                                          op:=A_PSUBSB;
                                        mmxu16bit:
                                          op:=A_PSUBUSW;
                                     end;
                                  end
                                else
                                  begin
                                     case mmxbase of
                                        mmxs8bit,mmxu8bit:
                                          op:=A_PSUBB;
                                        mmxs16bit,mmxu16bit,mmxfixed16:
                                          op:=A_PSUBW;
                                        mmxs32bit,mmxu32bit:
                                          op:=A_PSUBD;
                                     end;
                                  end;
                             end;
                      {
                      ltn,lten,gtn,gten,
                      equaln,unequaln :
                             begin
                                op:=A_CMP;
                                cmpop:=true;
                             end;
                      }
                      xorn:
                        op:=A_PXOR;
                      orn:
                        op:=A_POR;
                      andn:
                        op:=A_PAND;
                      else Message(sym_e_type_mismatch);
                   end;
                   { left and right no register?  }
                   { then one must be demanded    }
                   if (p^.left^.location.loc<>LOC_MMXREGISTER) and
                     (p^.right^.location.loc<>LOC_MMXREGISTER) then
                     begin
                        { register variable ? }
                        if (p^.left^.location.loc=LOC_CMMXREGISTER) then
                          begin
                             { it is OK if this is the destination }
                             if is_in_dest then
                               begin
                                  hregister:=p^.location.register;
                                  emit_reg_reg(A_MOVQ,S_NO,p^.left^.location.register,
                                    hregister);
                               end
                             else
                               begin
                                  hregister:=getregistermmx;
                                  emit_reg_reg(A_MOVQ,S_NO,p^.left^.location.register,
                                    hregister);
                               end
                          end
                        else
                          begin
                             del_reference(p^.left^.location.reference);

                             if is_in_dest then
                               begin
                                  hregister:=p^.location.register;
                                  exprasmlist^.concat(new(pai386,op_ref_reg(A_MOVQ,S_NO,
                                  newreference(p^.left^.location.reference),hregister)));
                               end
                             else
                               begin
                                  hregister:=getregistermmx;
                                  exprasmlist^.concat(new(pai386,op_ref_reg(A_MOVQ,S_NO,
                                    newreference(p^.left^.location.reference),hregister)));
                               end;
                          end;
                        p^.location.loc:=LOC_MMXREGISTER;
                        p^.location.register:=hregister;
                     end
                   else
                     { if on the right the register then swap }
                     if (p^.right^.location.loc=LOC_MMXREGISTER) then
                       begin
                          swap_location(p^.location,p^.right^.location);
                          { newly swapped also set swapped flag }
                          p^.swaped:=not(p^.swaped);
                       end;
                   { at this point, p^.location.loc should be LOC_MMXREGISTER }
                   { and p^.location.register should be a valid register      }
                   { containing the left result                               }
                   if p^.right^.location.loc<>LOC_MMXREGISTER then
                     begin
                        if (p^.treetype=subn) and p^.swaped then
                          begin
                             if p^.right^.location.loc=LOC_CMMXREGISTER then
                               begin
                                  emit_reg_reg(A_MOVQ,S_NO,p^.right^.location.register,R_MM7);
                                  emit_reg_reg(op,S_NO,p^.location.register,R_EDI);
                                  emit_reg_reg(A_MOVQ,S_NO,R_MM7,p^.location.register);
                               end
                             else
                               begin
                                  exprasmlist^.concat(new(pai386,op_ref_reg(A_MOVQ,S_NO,
                                    newreference(p^.right^.location.reference),R_MM7)));
                                  exprasmlist^.concat(new(pai386,op_reg_reg(op,S_NO,p^.location.register,
                                    R_MM7)));
                                  exprasmlist^.concat(new(pai386,op_reg_reg(A_MOVQ,S_NO,
                                    R_MM7,p^.location.register)));
                                  del_reference(p^.right^.location.reference);
                               end;
                          end
                        else
                          begin
                             if (p^.right^.location.loc=LOC_CREGISTER) then
                               begin
                                  emit_reg_reg(op,S_NO,p^.right^.location.register,
                                    p^.location.register);
                               end
                             else
                               begin
                                  exprasmlist^.concat(new(pai386,op_ref_reg(op,S_NO,newreference(
                                    p^.right^.location.reference),p^.location.register)));
                                  del_reference(p^.right^.location.reference);
                               end;
                          end;
                     end
                   else
                     begin
                        { when swapped another result register }
                        if (p^.treetype=subn) and p^.swaped then
                          begin
                             exprasmlist^.concat(new(pai386,op_reg_reg(op,S_NO,
                               p^.location.register,p^.right^.location.register)));
                               swap_location(p^.location,p^.right^.location);
                               { newly swapped also set swapped flag }
                               { just to maintain ordering           }
                               p^.swaped:=not(p^.swaped);
                          end
                        else
                          begin
                             exprasmlist^.concat(new(pai386,op_reg_reg(op,S_NO,
                               p^.right^.location.register,
                               p^.location.register)));
                          end;
                        ungetregistermmx(p^.right^.location.register);
                     end;
                end
{$endif SUPPORT_MMX}
              else Message(sym_e_type_mismatch);
           end;
       SetResultLocation(cmpop,unsigned,p);
    end;


end.
{
  $Log$
  Revision 1.6  1998-08-18 09:24:35  pierre
    * small warning position bug fixed
    * support_mmx switches splitting was missing
    * rhide error and warning output corrected

  Revision 1.5  1998/08/14 18:18:37  peter
    + dynamic set contruction
    * smallsets are now working (always longint size)

  Revision 1.4  1998/08/10 14:49:42  peter
    + localswitches, moduleswitches, globalswitches splitting

  Revision 1.3  1998/06/25 08:48:04  florian
    * first version of rtti support

  Revision 1.2  1998/06/08 13:13:28  pierre
    + temporary variables now in temp_gen.pas unit
      because it is processor independent
    * mppc68k.bat modified to undefine i386 and support_mmx
      (which are defaults for i386)

  Revision 1.1  1998/06/05 17:44:10  peter
    * splitted cgi386

}
