{
    $Id$
    Copyright (c) 2000 by Florian Klaempfl

    Code generation for add nodes on the i386

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
unit n386add;

{$i defines.inc}

interface

    uses
       node,nadd,cpubase;

    type
       ti386addnode = class(taddnode)
          procedure pass_2;override;
          function getresflags(unsigned : boolean) : tresflags;
          procedure SetResultLocation(cmpop,unsigned : boolean);
         protected
          function first_addstring : tnode; override;
         private
          procedure second_addstring;
       end;

  implementation

    uses
      globtype,systems,
      cutils,verbose,globals,widestr,
      symconst,symdef,aasm,types,htypechk,
      cgbase,temp_gen,pass_2,regvars,
      cpuasm,
      ncon,nset,
      cga,n386util,tgcpu;

    function ti386addnode.getresflags(unsigned : boolean) : tresflags;

      begin
         case nodetype of
           equaln : getresflags:=F_E;
           unequaln : getresflags:=F_NE;
          else
           if not(unsigned) then
             begin
                if nf_swaped in flags then
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
                if nf_swaped in flags then
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


    procedure ti386addnode.SetResultLocation(cmpop,unsigned : boolean);

      begin
         { remove temporary location if not a set or string }
         { that's a bad hack (FK) who did this ?            }
         if (left.resulttype.def.deftype<>stringdef) and
            ((left.resulttype.def.deftype<>setdef) or (tsetdef(left.resulttype.def).settype=smallset)) and
            (left.location.loc in [LOC_MEM,LOC_REFERENCE]) then
           ungetiftemp(left.location.reference);
         if (right.resulttype.def.deftype<>stringdef) and
            ((right.resulttype.def.deftype<>setdef) or (tsetdef(right.resulttype.def).settype=smallset)) and
            (right.location.loc in [LOC_MEM,LOC_REFERENCE]) then
           ungetiftemp(right.location.reference);
         { in case of comparison operation the put result in the flags }
         if cmpop then
           begin
              clear_location(location);
              location.loc:=LOC_FLAGS;
              location.resflags:=getresflags(unsigned);
           end;
      end;


{*****************************************************************************
                                Addstring
*****************************************************************************}

    { note: if you implemented an fpc_shortstr_concat similar to the    }
    { one in i386.inc, you have to override first_addstring like in     }
    { ti386addnode.first_string and implement the shortstring concat    }
    { manually! The generic routine is different from the i386 one (JM) }
    function ti386addnode.first_addstring : tnode;

      begin
        { special cases for shortstrings, handled in pass_2 (JM) }
        { can't handle fpc_shortstr_compare with compilerproc either because it }
        { returns its results in the flags instead of in eax                    }
        if (((nodetype = addn) and
             is_shortstring(resulttype.def)) or
            ((nodetype in [ltn,lten,gtn,gten,equaln,unequaln]) and
             is_shortstring(left.resulttype.def))) then
          begin
            if nodetype = addn then
              location.loc := LOC_MEM
            else
              location.loc := LOC_FLAGS;
            calcregisters(self,0,0,0);
            result := nil;
            exit;
          end;
        { otherwise, use the generic code }
        result := inherited first_addstring;
      end;


    procedure ti386addnode.second_addstring;

      var
        pushedregs : tpushed;
        href       : treference;
        pushed,
        cmpop      : boolean;
        regstopush : byte;
      begin
        { string operations are not commutative }
        if nf_swaped in flags then
          swapleftright;
        case tstringdef(left.resulttype.def).string_typ of
           st_shortstring:
             begin
                case nodetype of
                   addn:
                     begin
                        cmpop:=false;
                        secondpass(left);
                        { if str_concat is set in expr
                          s:=s+ ... no need to create a temp string (PM) }
                        { the tempstring can also come from a typeconversion }
                        { or a function result, so simply check for a        }
                        { temp of 256 bytes(JM)                                          }

                        if not(istemp(left.location.reference) and
                               (getsizeoftemp(left.location.reference) = 256)) and
                           not(nf_use_strconcat in flags) then
                          begin

                             { can only reference be }
                             { string in register would be funny    }
                             { therefore produce a temporary string }

                             gettempofsizereference(256,href);
                             copyshortstring(href,left.location.reference,255,false,true);
                             { release the registers }
{                             done by copyshortstring now (JM)           }
{                             del_reference(left.location.reference); }
                             ungetiftemp(left.location.reference);

                             { does not hurt: }
                             clear_location(left.location);
                             left.location.loc:=LOC_MEM;
                             left.location.reference:=href;

                          end;

                        secondpass(right);

                        { on the right we do not need the register anymore too }
                        { Instead of releasing them already, simply do not }
                        { push them (so the release is in the right place, }
                        { because emitpushreferenceaddr doesn't need extra }
                        { registers) (JM)                                  }
                        regstopush := $ff;
                        remove_non_regvars_from_loc(right.location,
                          regstopush);
                        pushusedregisters(pushedregs,regstopush);
                       { push the maximum possible length of the result }
                        emitpushreferenceaddr(left.location.reference);
                       { the optimizer can more easily put the          }
                       { deallocations in the right place if it happens }
                       { too early than when it happens too late (if    }
                       { the pushref needs a "lea (..),edi; push edi")  }
                        del_reference(right.location.reference);
                        emitpushreferenceaddr(right.location.reference);
                        saveregvars(regstopush);
                        emitcall('FPC_SHORTSTR_CONCAT');
                        ungetiftemp(right.location.reference);
                        maybe_loadself;
                        popusedregisters(pushedregs);
                        set_location(location,left.location);
                     end;
                   ltn,lten,gtn,gten,equaln,unequaln :
                     begin
                       cmpop := true;
                       pushusedregisters(pushedregs,$ff);
                       secondpass(left);
                       emitpushreferenceaddr(left.location.reference);
                       del_reference(left.location.reference);
                       secondpass(right);
                       emitpushreferenceaddr(right.location.reference);
                       del_reference(right.location.reference);
                       saveregvars($ff);
                       emitcall('FPC_SHORTSTR_COMPARE');
                       maybe_loadself;
                       popusedregisters(pushedregs);
                       ungetiftemp(left.location.reference);
                       ungetiftemp(right.location.reference);
                     end;
                end;
                SetResultLocation(cmpop,true);
             end;
           else
             { rest should be handled in first pass (JM) }
             internalerror(200108303);
       end;
     end;


{*****************************************************************************
                                pass_2
*****************************************************************************}

    procedure ti386addnode.pass_2;
    { is also being used for xor, and "mul", "sub, or and comparative }
    { operators                                                }

      label do_normal;

      var
         unusedregisters : tregisterset;
         usablecount : byte;
         hregister,hregister2 : tregister;
         noswap,popeax,popedx,
         pushed,pushedfpu,
         mboverflow,cmpop : boolean;
         op,op2 : tasmop;
         resflags : tresflags;
         otl,ofl : tasmlabel;
         power : longint;
         opsize : topsize;
         hl4: tasmlabel;
         hr : preference;

         { true, if unsigned types are compared }
         unsigned : boolean;
         { true, if a small set is handled with the longint code }
         is_set : boolean;
         { is_in_dest if the result is put directly into }
         { the resulting refernce or varregister }
         is_in_dest : boolean;
         { true, if for sets subtractions the extra not should generated }
         extra_not : boolean;

{$ifdef SUPPORT_MMX}
         mmxbase : tmmxtype;
{$endif SUPPORT_MMX}
         pushedreg : tpushed;
         regstopush: byte;

      procedure firstjmp64bitcmp;

        var
           oldnodetype : tnodetype;

        begin
           load_all_regvars(exprasmlist);
           { the jump the sequence is a little bit hairy }
           case nodetype of
              ltn,gtn:
                begin
                   emitjmp(flag_2_cond[getresflags(unsigned)],truelabel);
                   { cheat a little bit for the negative test }
                   toggleflag(nf_swaped);
                   emitjmp(flag_2_cond[getresflags(unsigned)],falselabel);
                   toggleflag(nf_swaped);
                end;
              lten,gten:
                begin
                   oldnodetype:=nodetype;
                   if nodetype=lten then
                     nodetype:=ltn
                   else
                     nodetype:=gtn;
                   emitjmp(flag_2_cond[getresflags(unsigned)],truelabel);
                   { cheat for the negative test }
                   if nodetype=ltn then
                     nodetype:=gtn
                   else
                     nodetype:=ltn;
                   emitjmp(flag_2_cond[getresflags(unsigned)],falselabel);
                   nodetype:=oldnodetype;
                end;
              equaln:
                emitjmp(C_NE,falselabel);
              unequaln:
                emitjmp(C_NE,truelabel);
           end;
        end;

      procedure secondjmp64bitcmp;

        begin
           { the jump the sequence is a little bit hairy }
           case nodetype of
              ltn,gtn,lten,gten:
                begin
                   { the comparisaion of the low dword have to be }
                   {  always unsigned!                            }
                   emitjmp(flag_2_cond[getresflags(true)],truelabel);
                   emitjmp(C_None,falselabel);
                end;
              equaln:
                begin
                   emitjmp(C_NE,falselabel);
                   emitjmp(C_None,truelabel);
                end;
              unequaln:
                begin
                   emitjmp(C_NE,truelabel);
                   emitjmp(C_None,falselabel);
                end;
           end;
        end;

      begin
      { to make it more readable, string and set (not smallset!) have their
        own procedures }
         case left.resulttype.def.deftype of
         stringdef : begin
                       second_addstring;
                       exit;
                     end;
            setdef : begin
                     { normalsets are handled separate }
                       if not(tsetdef(left.resulttype.def).settype=smallset) then
                        begin
                          { should be handled in pass 1 (JM) }
                          internalerror(200109041);
                        end;
                     end;
         end;

         { defaults }
         unsigned:=false;
         is_in_dest:=false;
         extra_not:=false;
         noswap:=false;
         opsize:=S_L;

         { are we a (small)set, must be set here because the side can be
           swapped ! (PFV) }
         is_set:=(left.resulttype.def.deftype=setdef);

         { calculate the operator which is more difficult }
         firstcomplex(self);

         { handling boolean expressions extra: }
         if is_boolean(left.resulttype.def) and
            is_boolean(right.resulttype.def) then
           begin
             if (torddef(left.resulttype.def).typ=bool8bit) or
                (torddef(right.resulttype.def).typ=bool8bit) then
               opsize:=S_B
             else
               if (torddef(left.resulttype.def).typ=bool16bit) or
                  (torddef(right.resulttype.def).typ=bool16bit) then
                 opsize:=S_W
             else
               opsize:=S_L;
             if (cs_full_boolean_eval in aktlocalswitches) or
                (nodetype in
                  [unequaln,ltn,lten,gtn,gten,equaln,xorn]) then
               begin
                 if left.nodetype in [ordconstn,realconstn] then
                  swapleftright;
                 if left.location.loc=LOC_JUMP then
                   begin
                      otl:=truelabel;
                      getlabel(truelabel);
                      ofl:=falselabel;
                      getlabel(falselabel);
                   end;

                 secondpass(left);
                 { if in flags then copy first to register, because the
                   flags can be destroyed }
                 case left.location.loc of
                    LOC_FLAGS:
                      locflags2reg(left.location,opsize);
                    LOC_JUMP:
                      locjump2reg(left.location,opsize, otl, ofl);
                 end;
                 set_location(location,left.location);
                 pushed:=maybe_push(right.registers32,self,false);
                 if right.location.loc=LOC_JUMP then
                   begin
                      otl:=truelabel;
                      getlabel(truelabel);
                      ofl:=falselabel;
                      getlabel(falselabel);
                   end;
                 secondpass(right);
                 if pushed then
                   begin
                      restore(self,false);
                      set_location(left.location,location);
                   end;
                 case right.location.loc of
                    LOC_FLAGS:
                      locflags2reg(right.location,opsize);
                    LOC_JUMP:
                      locjump2reg(right.location,opsize,otl,ofl);
                 end;
                 goto do_normal;
              end;

             case nodetype of
              andn,
               orn : begin
                       clear_location(location);
                       location.loc:=LOC_JUMP;
                       cmpop:=false;
                       case nodetype of
                        andn : begin
                                  otl:=truelabel;
                                  getlabel(truelabel);
                                  secondpass(left);
                                  maketojumpbool(left,lr_load_regvars);
                                  emitlab(truelabel);
                                  truelabel:=otl;
                               end;
                        orn : begin
                                 ofl:=falselabel;
                                 getlabel(falselabel);
                                 secondpass(left);
                                 maketojumpbool(left,lr_load_regvars);
                                 emitlab(falselabel);
                                 falselabel:=ofl;
                              end;
                       else
                         CGMessage(type_e_mismatch);
                       end;
                       secondpass(right);
                       maketojumpbool(right,lr_load_regvars);
                     end;
             else
               CGMessage(type_e_mismatch);
             end
           end
         else
           begin
              { in case of constant put it to the left }
              if (left.nodetype=ordconstn) then
               swapleftright;
              secondpass(left);
              { this will be complicated as
               a lot of code below assumes that
               location and left.location are the same }

{$ifdef test_dest_loc}
              if dest_loc_known and (dest_loc_tree=p) and
                 ((dest_loc.loc=LOC_REGISTER) or (dest_loc.loc=LOC_CREGISTER)) then
                begin
                   set_location(location,dest_loc);
                   in_dest_loc:=true;
                   is_in_dest:=true;
                end
              else
{$endif test_dest_loc}
                set_location(location,left.location);

              { are too few registers free? }
              pushed:=maybe_push(right.registers32,self,is_64bitint(left.resulttype.def));
              if location.loc=LOC_FPU then
                pushedfpu:=maybe_pushfpu(right.registersfpu,self)
              else
                pushedfpu:=false;
              secondpass(right);
              if pushed then
                begin
                  restore(self,is_64bitint(left.resulttype.def));
                  set_location(left.location,location);
                end;

              if (left.resulttype.def.deftype=pointerdef) or

                 (right.resulttype.def.deftype=pointerdef) or

                 (is_class_or_interface(right.resulttype.def) and is_class_or_interface(left.resulttype.def)) or

                 (left.resulttype.def.deftype=classrefdef) or

                 (left.resulttype.def.deftype=procvardef) or

                 ((left.resulttype.def.deftype=enumdef) and
                  (left.resulttype.def.size=4)) or

                 ((left.resulttype.def.deftype=orddef) and
                 (torddef(left.resulttype.def).typ=s32bit)) or
                 ((right.resulttype.def.deftype=orddef) and
                 (torddef(right.resulttype.def).typ=s32bit)) or

                ((left.resulttype.def.deftype=orddef) and
                 (torddef(left.resulttype.def).typ=u32bit)) or
                 ((right.resulttype.def.deftype=orddef) and
                 (torddef(right.resulttype.def).typ=u32bit)) or

                { as well as small sets }
                 is_set then
                begin
          do_normal:
                   mboverflow:=false;
                   cmpop:=false;
                   unsigned := not(is_signed(left.resulttype.def)) or
                               not(is_signed(right.resulttype.def));
                   case nodetype of
                      addn : begin
                               { this is a really ugly hack!!!!!!!!!! }
                               { this could be done later using EDI   }
                               { as it is done for subn               }
                               { instead of two registers!!!!         }
                               if is_set then
                                begin
                                { adding elements is not commutative }
                                  if (nf_swaped in flags) and (left.nodetype=setelementn) then
                                   swapleftright;
                                { are we adding set elements ? }
                                  if right.nodetype=setelementn then
                                   begin
                                   { no range support for smallsets! }
                                     if assigned(tsetelementnode(right).right) then
                                      internalerror(43244);
                                   { bts requires both elements to be registers }
                                     if left.location.loc in [LOC_MEM,LOC_REFERENCE] then
                                      begin
                                        ungetiftemp(left.location.reference);
                                        del_location(left.location);
{!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!}
                                        hregister:=getregister32;
                                        emit_ref_reg(A_MOV,opsize,
                                          newreference(left.location.reference),hregister);
                                        clear_location(left.location);
                                        left.location.loc:=LOC_REGISTER;
                                        left.location.register:=hregister;
                                        set_location(location,left.location);
                                      end;
                                     if right.location.loc in [LOC_MEM,LOC_REFERENCE] then
                                      begin
                                        ungetiftemp(right.location.reference);
                                        del_location(right.location);
                                        hregister:=getregister32;
                                        emit_ref_reg(A_MOV,opsize,
                                          newreference(right.location.reference),hregister);
                                        clear_location(right.location);
                                        right.location.loc:=LOC_REGISTER;
                                        right.location.register:=hregister;
                                      end;
                                     op:=A_BTS;
                                     noswap:=true;
                                   end
                                  else
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
                                CGMessage(type_e_mismatch);
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
                                  If (not (nf_swaped in flags)) and
                                     (right.nodetype = setconstn) then
                                    right.location.reference.offset := not(right.location.reference.offset)
                                  Else If (nf_swaped in flags) and
                                     (left.nodetype = setconstn) then
                                    left.location.reference.offset := not(left.location.reference.offset)
                                  Else
                                    extra_not:=true;
                                end
                               else
                                begin
                                  op:=A_SUB;
                                  mboverflow:=true;
                                end;
                             end;
                  ltn,lten,
                  gtn,gten,
           equaln,unequaln : begin
                               If is_set Then
                                 Case nodetype of
                                   lten,gten:
                                     Begin
                                      If nodetype = lten then
                                        swapleftright;
                                      if left.location.loc in [LOC_MEM,LOC_REFERENCE] then
                                        begin
                                         ungetiftemp(left.location.reference);
                                         del_reference(left.location.reference);
                                         hregister:=getregister32;
                                         emit_ref_reg(A_MOV,opsize,
                                           newreference(left.location.reference),hregister);
                                         clear_location(left.location);
                                         left.location.loc:=LOC_REGISTER;
                                         left.location.register:=hregister;
                                         set_location(location,left.location);
                                       end
                                      else
                                       if left.location.loc = LOC_CREGISTER Then
                                        {save the register var in a temp register, because
                                          its value is going to be modified}
                                          begin
                                            hregister := getregister32;
                                            emit_reg_reg(A_MOV,opsize,
                                              left.location.register,hregister);
                                             clear_location(left.location);
                                             left.location.loc:=LOC_REGISTER;
                                             left.location.register:=hregister;
                                             set_location(location,left.location);
                                           end;
                                     {here, left.location should be LOC_REGISTER}
                                      If right.location.loc in [LOC_MEM,LOC_REFERENCE] Then
                                         emit_ref_reg(A_AND,opsize,
                                           newreference(right.location.reference),left.location.register)
                                      Else
                                        emit_reg_reg(A_AND,opsize,
                                          right.location.register,left.location.register);
                {warning: ugly hack ahead: we need a "jne" after the cmp, so
                 change the nodetype from lten/gten to equaln}
                                      nodetype := equaln
                                     End;
                           {no < or > support for sets}
                                   ltn,gtn: CGMessage(type_e_mismatch);
                                 End;
                               op:=A_CMP;
                               cmpop:=true;
                             end;
                      xorn : op:=A_XOR;
                       orn : op:=A_OR;
                      andn : op:=A_AND;
                   else
                     CGMessage(type_e_mismatch);
                   end;

                   { filter MUL, which requires special handling }
                   if op=A_MUL then
                     begin
                       popeax:=false;
                       popedx:=false;
                       { here you need to free the symbol first }
                       { left.location and right.location must }
                       { only be freed when they are really released,  }
                       { because the optimizer NEEDS correct regalloc  }
                       { info!!! (JM)                                  }
                       clear_location(location);

                 { the location.register will be filled in later (JM) }
                       location.loc:=LOC_REGISTER;
{$IfNDef NoShlMul}
                       if right.nodetype=ordconstn then
                        swapleftright;
                       If (left.nodetype = ordconstn) and
                          ispowerof2(tordconstnode(left).value, power) and
                          not(cs_check_overflow in aktlocalswitches) then
                         Begin
                           { This release will be moved after the next }
                           { instruction by the optimizer. No need to  }
                           { release left.location, since it's a   }
                           { constant (JM)                             }
                           release_loc(right.location);
                           location.register := getregister32;
                           emitloadord2reg(right.location,torddef(u32bittype.def),location.register,false);
                           emit_const_reg(A_SHL,S_L,power,location.register)
                         End
                       Else
                        Begin
{$EndIf NoShlMul}
                         regstopush := $ff;
                         remove_non_regvars_from_loc(right.location,regstopush);
                         remove_non_regvars_from_loc(left.location,regstopush);
                         { now, regstopush does NOT contain EAX and/or EDX if they are }
                         { used in either the left or the right location, excepts if   }
                         {they are regvars. It DOES contain them if they are used in   }
                         { another location (JM)                                       }
                         if not(R_EAX in unused) and ((regstopush and ($80 shr byte(R_EAX))) <> 0) then
                          begin
                           emit_reg(A_PUSH,S_L,R_EAX);
                           popeax:=true;
                          end;
                         if not(R_EDX in unused) and ((regstopush and ($80 shr byte(R_EDX))) <> 0) then
                          begin
                           emit_reg(A_PUSH,S_L,R_EDX);
                           popedx:=true;
                          end;
                         { left.location can be R_EAX !!! }
                         getexplicitregister32(R_EDI);
                         { load the left value }
                         emitloadord2reg(left.location,torddef(u32bittype.def),R_EDI,true);
                         release_loc(left.location);
                         { allocate EAX }
                         if R_EAX in unused then
                           exprasmList.concat(Tairegalloc.Alloc(R_EAX));
                         { load he right value }
                         emitloadord2reg(right.location,torddef(u32bittype.def),R_EAX,true);
                         release_loc(right.location);
                         { allocate EAX if it isn't yet allocated (JM) }
                         if (R_EAX in unused) then
                           exprasmList.concat(Tairegalloc.Alloc(R_EAX));
                         { also allocate EDX, since it is also modified by }
                         { a mul (JM)                                      }
                         if R_EDX in unused then
                           exprasmList.concat(Tairegalloc.Alloc(R_EDX));
                         emit_reg(A_MUL,S_L,R_EDI);
                         ungetregister32(R_EDI);
                         if R_EDX in unused then
                           exprasmList.concat(Tairegalloc.DeAlloc(R_EDX));
                         if R_EAX in unused then
                           exprasmList.concat(Tairegalloc.DeAlloc(R_EAX));
                         location.register := getregister32;
                         emit_reg_reg(A_MOV,S_L,R_EAX,location.register);
                         if popedx then
                          emit_reg(A_POP,S_L,R_EDX);
                         if popeax then
                          emit_reg(A_POP,S_L,R_EAX);
{$IfNDef NoShlMul}
                        End;
{$endif NoShlMul}
                       SetResultLocation(false,true);
                       exit;
                     end;

                   { Convert flags to register first }
                   if (left.location.loc=LOC_FLAGS) then
                    locflags2reg(left.location,opsize);
                   if (right.location.loc=LOC_FLAGS) then
                    locflags2reg(right.location,opsize);

                   { left and right no register?  }
                   { then one must be demanded    }
                   if (left.location.loc<>LOC_REGISTER) and
                      (right.location.loc<>LOC_REGISTER) then
                     begin
                        { register variable ? }
                        if (left.location.loc=LOC_CREGISTER) then
                          begin
                             { it is OK if this is the destination }
                             if is_in_dest then
                               begin
                                  hregister:=location.register;
                                  emit_reg_reg(A_MOV,opsize,left.location.register,
                                    hregister);
                               end
                             else
                             if cmpop then
                               begin
                                  { do not disturb the register }
                                  hregister:=location.register;
                               end
                             else
                               begin
                                  case opsize of
                                     S_L : hregister:=getregister32;
                                     S_B : hregister:=reg32toreg8(getregister32);
                                  end;
                                  emit_reg_reg(A_MOV,opsize,left.location.register,
                                    hregister);
                               end
                          end
                        else
                          begin
                             ungetiftemp(left.location.reference);
                             del_reference(left.location.reference);
                             if is_in_dest then
                               begin
                                  hregister:=location.register;
                                  emit_ref_reg(A_MOV,opsize,
                                    newreference(left.location.reference),hregister);
                               end
                             else
                               begin
                                  { first give free, then demand new register }
                                  case opsize of
                                     S_L : hregister:=getregister32;
                                     S_W : hregister:=reg32toreg16(getregister32);
                                     S_B : hregister:=reg32toreg8(getregister32);
                                  end;
                                  emit_ref_reg(A_MOV,opsize,
                                    newreference(left.location.reference),hregister);
                               end;
                          end;
                        clear_location(location);
                        location.loc:=LOC_REGISTER;
                        location.register:=hregister;
                     end
                   else
                     { if on the right the register then swap }
                     if not(noswap) and (right.location.loc=LOC_REGISTER) then
                       begin
                          swap_location(location,right.location);

                          { newly swapped also set swapped flag }
                          toggleflag(nf_swaped);
                       end;
                   { at this point, location.loc should be LOC_REGISTER }
                   { and location.register should be a valid register   }
                   { containing the left result                     }

                    if right.location.loc<>LOC_REGISTER then
                     begin
                        if (nodetype=subn) and (nf_swaped in flags) then
                          begin
                             if right.location.loc=LOC_CREGISTER then
                               begin
                                  if extra_not then
                                    emit_reg(A_NOT,opsize,location.register);
                                  getexplicitregister32(R_EDI);
                                  emit_reg_reg(A_MOV,opsize,right.location.register,R_EDI);
                                  emit_reg_reg(op,opsize,location.register,R_EDI);
                                  emit_reg_reg(A_MOV,opsize,R_EDI,location.register);
                                  ungetregister32(R_EDI);
                               end
                             else
                               begin
                                  if extra_not then
                                    emit_reg(A_NOT,opsize,location.register);
                                  getexplicitregister32(R_EDI);
                                  emit_ref_reg(A_MOV,opsize,
                                    newreference(right.location.reference),R_EDI);
                                  emit_reg_reg(op,opsize,location.register,R_EDI);
                                  emit_reg_reg(A_MOV,opsize,R_EDI,location.register);
                                  ungetregister32(R_EDI);
                                  ungetiftemp(right.location.reference);
                                  del_reference(right.location.reference);
                               end;
                          end
                        else
                          begin
                             if (right.nodetype=ordconstn) and
                                (op=A_CMP) and
                                (tordconstnode(right).value=0) then
                               begin
                                  emit_reg_reg(A_TEST,opsize,location.register,
                                    location.register);
                               end
                             else if (right.nodetype=ordconstn) and
                                (op=A_ADD) and
                                (tordconstnode(right).value=1) and
                                not(cs_check_overflow in aktlocalswitches) then
                               begin
                                  emit_reg(A_INC,opsize,
                                    location.register);
                               end
                             else if (right.nodetype=ordconstn) and
                                (op=A_SUB) and
                                (tordconstnode(right).value=1) and
                                not(cs_check_overflow in aktlocalswitches) then
                               begin
                                  emit_reg(A_DEC,opsize,
                                    location.register);
                               end
                             else if (right.nodetype=ordconstn) and
                                (op=A_IMUL) and
                                (ispowerof2(tordconstnode(right).value,power)) and
                                not(cs_check_overflow in aktlocalswitches) then
                               begin
                                  emit_const_reg(A_SHL,opsize,power,
                                    location.register);
                               end
                             else
                               begin
                                  if (right.location.loc=LOC_CREGISTER) then
                                    begin
                                       if extra_not then
                                         begin
                                            getexplicitregister32(R_EDI);
                                            emit_reg_reg(A_MOV,S_L,right.location.register,R_EDI);
                                            emit_reg(A_NOT,S_L,R_EDI);
                                            emit_reg_reg(A_AND,S_L,R_EDI,
                                              location.register);
                                            ungetregister32(R_EDI);
                                         end
                                       else
                                         begin
                                            emit_reg_reg(op,opsize,right.location.register,
                                              location.register);
                                         end;
                                    end
                                  else
                                    begin
                                       if extra_not then
                                         begin
                                            getexplicitregister32(R_EDI);
                                            emit_ref_reg(A_MOV,S_L,newreference(
                                              right.location.reference),R_EDI);
                                            emit_reg(A_NOT,S_L,R_EDI);
                                            emit_reg_reg(A_AND,S_L,R_EDI,
                                              location.register);
                                            ungetregister32(R_EDI);
                                         end
                                       else
                                         begin
                                            emit_ref_reg(op,opsize,newreference(
                                              right.location.reference),location.register);
                                         end;
                                       ungetiftemp(right.location.reference);
                                       del_reference(right.location.reference);
                                    end;
                               end;
                          end;
                     end
                   else
                     begin
                        { when swapped another result register }
                        if (nodetype=subn) and (nf_swaped in flags) then
                          begin
                             if extra_not then
                               emit_reg(A_NOT,S_L,location.register);

                             emit_reg_reg(op,opsize,
                               location.register,right.location.register);
                               swap_location(location,right.location);
                               { newly swapped also set swapped flag }
                               { just to maintain ordering         }
                               toggleflag(nf_swaped);
                          end
                        else
                          begin
                             if extra_not then
                               emit_reg(A_NOT,S_L,right.location.register);
                             emit_reg_reg(op,opsize,
                               right.location.register,
                               location.register);
                          end;
                        case opsize of
                           S_L : ungetregister32(right.location.register);
                           S_B : ungetregister32(reg8toreg32(right.location.register));
                        end;
                     end;

                   if cmpop then
                     case opsize of
                        S_L : ungetregister32(location.register);
                        S_B : ungetregister32(reg8toreg32(location.register));
                     end;

                   { only in case of overflow operations }
                   { produce overflow code }
                   { we must put it here directly, because sign of operation }
                   { is in unsigned VAR!!                                   }
                   if mboverflow then
                    begin
                      if cs_check_overflow in aktlocalswitches  then
                       begin
                         getlabel(hl4);
                         if unsigned then
                          emitjmp(C_NB,hl4)
                         else
                          emitjmp(C_NO,hl4);
                         emitcall('FPC_OVERFLOW');
                         emitlab(hl4);
                       end;
                    end;
                end
              else

              { Char type }
                if ((left.resulttype.def.deftype=orddef) and
                    (torddef(left.resulttype.def).typ=uchar)) or
              { enumeration type 16 bit }
                   ((left.resulttype.def.deftype=enumdef) and
                    (left.resulttype.def.size=1)) then
                 begin
                   case nodetype of
                      ltn,lten,gtn,gten,
                      equaln,unequaln :
                                cmpop:=true;
                      else CGMessage(type_e_mismatch);
                   end;
                   unsigned:=true;
                   { left and right no register? }
                   { the one must be demanded    }
                   if (location.loc<>LOC_REGISTER) and
                     (right.location.loc<>LOC_REGISTER) then
                     begin
                        if location.loc=LOC_CREGISTER then
                          begin
                             if cmpop then
                               { do not disturb register }
                               hregister:=location.register
                             else
                               begin
                                  hregister:=reg32toreg8(getregister32);
                                  emit_reg_reg(A_MOV,S_B,location.register,
                                    hregister);
                               end;
                          end
                        else
                          begin
                             del_reference(location.reference);

                             { first give free then demand new register }
                             hregister:=reg32toreg8(getregister32);
                             emit_ref_reg(A_MOV,S_B,newreference(location.reference),
                               hregister);
                          end;
                        clear_location(location);
                        location.loc:=LOC_REGISTER;
                        location.register:=hregister;
                     end;

                   { now p always a register }

                   if (right.location.loc=LOC_REGISTER) and
                      (location.loc<>LOC_REGISTER) then
                     begin
                       swap_location(location,right.location);
                       { newly swapped also set swapped flag }
                       toggleflag(nf_swaped);
                     end;

                   if right.location.loc<>LOC_REGISTER then
                     begin
                        if right.location.loc=LOC_CREGISTER then
                          begin
                             emit_reg_reg(A_CMP,S_B,
                                right.location.register,location.register);
                          end
                        else
                          begin
                             emit_ref_reg(A_CMP,S_B,newreference(
                                right.location.reference),location.register);
                             del_reference(right.location.reference);
                          end;
                     end
                   else
                     begin
                        emit_reg_reg(A_CMP,S_B,right.location.register,
                          location.register);
                        ungetregister32(reg8toreg32(right.location.register));
                     end;
                   ungetregister32(reg8toreg32(location.register));
                end
              else
              { 16 bit enumeration type }
                if ((left.resulttype.def.deftype=enumdef) and
                    (left.resulttype.def.size=2)) then
                 begin
                   case nodetype of
                      ltn,lten,gtn,gten,
                      equaln,unequaln :
                                cmpop:=true;
                      else CGMessage(type_e_mismatch);
                   end;
                   unsigned:=true;
                   { left and right no register? }
                   { the one must be demanded    }
                   if (location.loc<>LOC_REGISTER) and
                     (right.location.loc<>LOC_REGISTER) then
                     begin
                        if location.loc=LOC_CREGISTER then
                          begin
                             if cmpop then
                               { do not disturb register }
                               hregister:=location.register
                             else
                               begin
                                  hregister:=reg32toreg16(getregister32);
                                  emit_reg_reg(A_MOV,S_W,location.register,
                                    hregister);
                               end;
                          end
                        else
                          begin
                             del_reference(location.reference);

                             { first give free then demand new register }
                             hregister:=reg32toreg16(getregister32);
                             emit_ref_reg(A_MOV,S_W,newreference(location.reference),
                               hregister);
                          end;
                        clear_location(location);
                        location.loc:=LOC_REGISTER;
                        location.register:=hregister;
                     end;

                   { now p always a register }

                   if (right.location.loc=LOC_REGISTER) and
                      (location.loc<>LOC_REGISTER) then
                     begin
                       swap_location(location,right.location);
                       { newly swapped also set swapped flag }
                       toggleflag(nf_swaped);
                     end;

                   if right.location.loc<>LOC_REGISTER then
                     begin
                        if right.location.loc=LOC_CREGISTER then
                          begin
                             emit_reg_reg(A_CMP,S_W,
                                right.location.register,location.register);
                          end
                        else
                          begin
                             emit_ref_reg(A_CMP,S_W,newreference(
                                right.location.reference),location.register);
                             del_reference(right.location.reference);
                          end;
                     end
                   else
                     begin
                        emit_reg_reg(A_CMP,S_W,right.location.register,
                          location.register);
                        ungetregister32(reg16toreg32(right.location.register));
                     end;
                   ungetregister32(reg16toreg32(location.register));
                end
              else
              { 64 bit types }
              if is_64bitint(left.resulttype.def) then
                begin
                   mboverflow:=false;
                   cmpop:=false;
                   unsigned:=((left.resulttype.def.deftype=orddef) and
                       (torddef(left.resulttype.def).typ=u64bit)) or
                      ((right.resulttype.def.deftype=orddef) and
                       (torddef(right.resulttype.def).typ=u64bit));
                   case nodetype of
                      addn : begin
                                begin
                                  op:=A_ADD;
                                  op2:=A_ADC;
                                  mboverflow:=true;
                                end;
                             end;
                      subn : begin
                                op:=A_SUB;
                                op2:=A_SBB;
                                mboverflow:=true;
                             end;
                      ltn,lten,
                      gtn,gten,
                      equaln,unequaln:
                             begin
                               op:=A_CMP;
                               op2:=A_CMP;
                               cmpop:=true;
                             end;

                      xorn:
                        begin
                           op:=A_XOR;
                           op2:=A_XOR;
                        end;

                      orn:
                        begin
                           op:=A_OR;
                           op2:=A_OR;
                        end;

                      andn:
                        begin
                           op:=A_AND;
                           op2:=A_AND;
                        end;
                      muln:
                        ;
                   else
                     CGMessage(type_e_mismatch);
                   end;

                   if nodetype=muln then
                     begin
                       { should be handled in pass_1 (JM) }
                       internalerror(200109051);
                     end
                   else
                     begin
                        { left and right no register?  }
                        { then one must be demanded    }
                        if (left.location.loc<>LOC_REGISTER) and
                           (right.location.loc<>LOC_REGISTER) then
                          begin
                             { register variable ? }
                             if (left.location.loc=LOC_CREGISTER) then
                               begin
                                  { it is OK if this is the destination }
                                  if is_in_dest then
                                    begin
                                       hregister:=location.registerlow;
                                       hregister2:=location.registerhigh;
                                       emit_reg_reg(A_MOV,S_L,left.location.registerlow,
                                         hregister);
                                       emit_reg_reg(A_MOV,S_L,left.location.registerlow,
                                         hregister2);
                                    end
                                  else
                                  if cmpop then
                                    begin
                                       { do not disturb the register }
                                       hregister:=location.registerlow;
                                       hregister2:=location.registerhigh;
                                    end
                                  else
                                    begin
                                       hregister:=getregister32;
                                       hregister2:=getregister32;
                                       emit_reg_reg(A_MOV,S_L,left.location.registerlow,
                                         hregister);
                                       emit_reg_reg(A_MOV,S_L,left.location.registerhigh,
                                         hregister2);
                                    end
                               end
                             else
                               begin
                                  ungetiftemp(left.location.reference);
                                  del_reference(left.location.reference);
                                  if is_in_dest then
                                    begin
                                       hregister:=location.registerlow;
                                       hregister2:=location.registerhigh;
                                       emit_mov_ref_reg64(left.location.reference,hregister,hregister2);
                                    end
                                  else
                                    begin
                                       hregister:=getregister32;
                                       hregister2:=getregister32;
                                       emit_mov_ref_reg64(left.location.reference,hregister,hregister2);
                                    end;
                               end;
                             clear_location(location);
                             location.loc:=LOC_REGISTER;
                             location.registerlow:=hregister;
                             location.registerhigh:=hregister2;
                          end
                        else
                          { if on the right the register then swap }
                          if not(noswap) and (right.location.loc=LOC_REGISTER) then
                            begin
                               swap_location(location,right.location);

                               { newly swapped also set swapped flag }
                               toggleflag(nf_swaped);
                            end;
                        { at this point, location.loc should be LOC_REGISTER }
                        { and location.register should be a valid register   }
                        { containing the left result                        }

                        if right.location.loc<>LOC_REGISTER then
                          begin
                             if (nodetype=subn) and (nf_swaped in flags) then
                               begin
                                  if right.location.loc=LOC_CREGISTER then
                                    begin
                                       getexplicitregister32(R_EDI);
                                       emit_reg_reg(A_MOV,opsize,right.location.register,R_EDI);
                                       emit_reg_reg(op,opsize,location.register,R_EDI);
                                       emit_reg_reg(A_MOV,opsize,R_EDI,location.register);
                                       ungetregister32(R_EDI);
                                       getexplicitregister32(R_EDI);
                                       emit_reg_reg(A_MOV,opsize,right.location.registerhigh,R_EDI);
                                       { the carry flag is still ok }
                                       emit_reg_reg(op2,opsize,location.registerhigh,R_EDI);
                                       emit_reg_reg(A_MOV,opsize,R_EDI,location.registerhigh);
                                       ungetregister32(R_EDI);
                                    end
                                  else
                                    begin
                                       getexplicitregister32(R_EDI);
                                       emit_ref_reg(A_MOV,opsize,
                                         newreference(right.location.reference),R_EDI);
                                       emit_reg_reg(op,opsize,location.registerlow,R_EDI);
                                       emit_reg_reg(A_MOV,opsize,R_EDI,location.registerlow);
                                       ungetregister32(R_EDI);
                                       getexplicitregister32(R_EDI);
                                       hr:=newreference(right.location.reference);
                                       inc(hr^.offset,4);
                                       emit_ref_reg(A_MOV,opsize,
                                         hr,R_EDI);
                                       { here the carry flag is still preserved }
                                       emit_reg_reg(op2,opsize,location.registerhigh,R_EDI);
                                       emit_reg_reg(A_MOV,opsize,R_EDI,
                                         location.registerhigh);
                                       ungetregister32(R_EDI);
                                       ungetiftemp(right.location.reference);
                                       del_reference(right.location.reference);
                                    end;
                               end
                             else if cmpop then
                               begin
                                  if (right.location.loc=LOC_CREGISTER) then
                                    begin
                                       emit_reg_reg(A_CMP,S_L,right.location.registerhigh,
                                          location.registerhigh);
                                       firstjmp64bitcmp;
                                       emit_reg_reg(A_CMP,S_L,right.location.registerlow,
                                          location.registerlow);
                                       secondjmp64bitcmp;
                                    end
                                  else
                                    begin
                                       hr:=newreference(right.location.reference);
                                       inc(hr^.offset,4);

                                       emit_ref_reg(A_CMP,S_L,
                                         hr,location.registerhigh);
                                       firstjmp64bitcmp;

                                       emit_ref_reg(A_CMP,S_L,newreference(
                                         right.location.reference),location.registerlow);
                                       secondjmp64bitcmp;

                                       emitjmp(C_None,falselabel);

                                       ungetiftemp(right.location.reference);
                                       del_reference(right.location.reference);
                                    end;
                               end
                             else
                               begin
                                  {
                                  if (right.nodetype=ordconstn) and
                                     (op=A_CMP) and
                                     (right.value=0) then
                                    begin
                                       emit_reg_reg(A_TEST,opsize,location.register,
                                         location.register);
                                    end
                                  else if (right.nodetype=ordconstn) and
                                     (op=A_IMUL) and
                                     (ispowerof2(right.value,power)) then
                                    begin
                                       emit_const_reg(A_SHL,opsize,power,
                                         location.register);
                                    end
                                  else
                                  }
                                    begin
                                       if (right.location.loc=LOC_CREGISTER) then
                                         begin
                                            emit_reg_reg(op,S_L,right.location.registerlow,
                                               location.registerlow);
                                            emit_reg_reg(op2,S_L,right.location.registerhigh,
                                               location.registerhigh);
                                         end
                                       else
                                         begin
                                            emit_ref_reg(op,S_L,newreference(
                                              right.location.reference),location.registerlow);
                                            hr:=newreference(right.location.reference);
                                            inc(hr^.offset,4);
                                            emit_ref_reg(op2,S_L,
                                              hr,location.registerhigh);
                                            ungetiftemp(right.location.reference);
                                            del_reference(right.location.reference);
                                         end;
                                    end;
                               end;
                          end
                        else
                          begin
                             { when swapped another result register }
                             if (nodetype=subn) and (nf_swaped in flags) then
                               begin
                                 emit_reg_reg(op,S_L,
                                    location.registerlow,
                                    right.location.registerlow);
                                 emit_reg_reg(op2,S_L,
                                    location.registerhigh,
                                    right.location.registerhigh);
                                  swap_location(location,right.location);
                                  { newly swapped also set swapped flag }
                                  { just to maintain ordering           }
                                  toggleflag(nf_swaped);
                               end
                             else if cmpop then
                               begin
                                  emit_reg_reg(A_CMP,S_L,
                                    right.location.registerhigh,
                                    location.registerhigh);
                                  firstjmp64bitcmp;
                                  emit_reg_reg(A_CMP,S_L,
                                    right.location.registerlow,
                                    location.registerlow);
                                  secondjmp64bitcmp;
                               end
                             else
                               begin
                                  emit_reg_reg(op,S_L,
                                    right.location.registerlow,
                                    location.registerlow);
                                  emit_reg_reg(op2,S_L,
                                    right.location.registerhigh,
                                    location.registerhigh);
                               end;
                             ungetregister32(right.location.registerlow);
                             ungetregister32(right.location.registerhigh);
                          end;

                        if cmpop then
                          begin
                             ungetregister32(location.registerlow);
                             ungetregister32(location.registerhigh);
                          end;

                        { only in case of overflow operations }
                        { produce overflow code }
                        { we must put it here directly, because sign of operation }
                        { is in unsigned VAR!!                              }
                        if mboverflow then
                         begin
                           if cs_check_overflow in aktlocalswitches  then
                            begin
                              getlabel(hl4);
                              if unsigned then
                               emitjmp(C_NB,hl4)
                              else
                               emitjmp(C_NO,hl4);
                              emitcall('FPC_OVERFLOW');
                              emitlab(hl4);
                            end;
                         end;
                        { we have LOC_JUMP as result }
                        if cmpop then
                          begin
                             clear_location(location);
                             location.loc:=LOC_JUMP;
                             cmpop:=false;
                          end;
                     end;
                end
              else
              { Floating point }
               if (left.resulttype.def.deftype=floatdef) then
                 begin
                    { real constants to the right, but only if it
                      isn't on the FPU stack, i.e. 1.0 or 0.0!
                    if (left.nodetype=realconstn) and
                      (left.location.loc<>LOC_FPU) then
                      swapleftright; obsolete, already swaped above PM }
                    cmpop:=false;
                    case nodetype of
                       addn : op:=A_FADDP;
                       muln : op:=A_FMULP;
                       subn : op:=A_FSUBP;
                       slashn : op:=A_FDIVP;
                       ltn,lten,gtn,gten,
                       equaln,unequaln : begin
                                            op:=A_FCOMPP;
                                            cmpop:=true;
                                         end;
                       else CGMessage(type_e_mismatch);
                    end;

                    if (right.location.loc<>LOC_FPU) then
                      begin
                         if right.location.loc=LOC_CFPUREGISTER then
                           begin
                              emit_reg( A_FLD,S_NO,
                                correct_fpuregister(right.location.register,fpuvaroffset));
                              inc(fpuvaroffset);
                            end
                         else
                            begin
                              floatload(tfloatdef(right.resulttype.def).typ,right.location.reference);
                              if pushedfpu then
                                ungetiftemp(right.location.reference);
                            end;
                         if (left.location.loc<>LOC_FPU) then
                           begin
                              if left.location.loc=LOC_CFPUREGISTER then
                                begin
                                   emit_reg( A_FLD,S_NO,
                                     correct_fpuregister(left.location.register,fpuvaroffset));
                                   inc(fpuvaroffset);
                                end
                              else
                                begin
                                  floatload(tfloatdef(left.resulttype.def).typ,left.location.reference);
                                  if pushedfpu then
                                     ungetiftemp(left.location.reference);
                                end;
                           end
                         { left was on the stack => swap }
                         else
                           toggleflag(nf_swaped);

                         { releases the right reference }
                         del_reference(right.location.reference);
                      end
                    { the nominator in st0 }
                    else if (left.location.loc<>LOC_FPU) then
                      begin
                         if left.location.loc=LOC_CFPUREGISTER then
                           begin
                              emit_reg( A_FLD,S_NO,
                                correct_fpuregister(left.location.register,fpuvaroffset));
                              inc(fpuvaroffset);
                           end
                         else
                           begin
                             floatload(tfloatdef(left.resulttype.def).typ,left.location.reference);
                             if pushedfpu then
                              ungetiftemp(left.location.reference);
                           end;
                      end
                    { fpu operands are always in the wrong order on the stack }
                    else
                      toggleflag(nf_swaped);

                    { releases the left reference }
                    if (left.location.loc in [LOC_MEM,LOC_REFERENCE]) then
                      del_reference(left.location.reference);

                    { if we swaped the tree nodes, then use the reverse operator }
                    if nf_swaped in flags then
                      begin
                         if (nodetype=slashn) then
                           op:=A_FDIVRP
                         else if (nodetype=subn) then
                           op:=A_FSUBRP;
                      end;
                    { to avoid the pentium bug
                    if (op=FDIVP) and (opt_processors=pentium) then
                      emitcall('EMUL_FDIVP')
                    else
                    }
                    { the Intel assemblers want operands }
                    if op<>A_FCOMPP then
                      begin
                         emit_reg_reg(op,S_NO,R_ST,R_ST1);
                         dec(fpuvaroffset);
                      end
                    else
                      begin
                         emit_none(op,S_NO);
                         dec(fpuvaroffset,2);
                      end;

                    { on comparison load flags }
                    if cmpop then
                     begin
                       if not(R_EAX in unused) then
                         begin
                           getexplicitregister32(R_EDI);
                           emit_reg_reg(A_MOV,S_L,R_EAX,R_EDI);
                         end;
                       emit_reg(A_FNSTSW,S_NO,R_AX);
                       emit_none(A_SAHF,S_NO);
                       if not(R_EAX in unused) then
                         begin
                           emit_reg_reg(A_MOV,S_L,R_EDI,R_EAX);
                           ungetregister32(R_EDI);
                         end;
                       if nf_swaped in flags then
                        begin
                          case nodetype of
                              equaln : resflags:=F_E;
                            unequaln : resflags:=F_NE;
                                 ltn : resflags:=F_A;
                                lten : resflags:=F_AE;
                                 gtn : resflags:=F_B;
                                gten : resflags:=F_BE;
                          end;
                        end
                       else
                        begin
                          case nodetype of
                              equaln : resflags:=F_E;
                            unequaln : resflags:=F_NE;
                                 ltn : resflags:=F_B;
                                lten : resflags:=F_BE;
                                 gtn : resflags:=F_A;
                                gten : resflags:=F_AE;
                          end;
                        end;
                       clear_location(location);
                       location.loc:=LOC_FLAGS;
                       location.resflags:=resflags;
                       cmpop:=false;
                     end
                    else
                     begin
                        clear_location(location);
                        location.loc:=LOC_FPU;
                     end;
                 end
{$ifdef SUPPORT_MMX}
               else

               { MMX Arrays }
                if is_mmx_able_array(left.resulttype.def) then
                 begin
                   cmpop:=false;
                   mmxbase:=mmx_type(left.resulttype.def);
                   case nodetype of
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
                      else CGMessage(type_e_mismatch);
                   end;
                   { left and right no register?  }
                   { then one must be demanded    }
                   if (left.location.loc<>LOC_MMXREGISTER) and
                     (right.location.loc<>LOC_MMXREGISTER) then
                     begin
                        { register variable ? }
                        if (left.location.loc=LOC_CMMXREGISTER) then
                          begin
                             { it is OK if this is the destination }
                             if is_in_dest then
                               begin
                                  hregister:=location.register;
                                  emit_reg_reg(A_MOVQ,S_NO,left.location.register,
                                    hregister);
                               end
                             else
                               begin
                                  hregister:=getregistermmx;
                                  emit_reg_reg(A_MOVQ,S_NO,left.location.register,
                                    hregister);
                               end
                          end
                        else
                          begin
                             del_reference(left.location.reference);

                             if is_in_dest then
                               begin
                                  hregister:=location.register;
                                  emit_ref_reg(A_MOVQ,S_NO,
                                    newreference(left.location.reference),hregister);
                               end
                             else
                               begin
                                  hregister:=getregistermmx;
                                  emit_ref_reg(A_MOVQ,S_NO,
                                    newreference(left.location.reference),hregister);
                               end;
                          end;
                        clear_location(location);
                        location.loc:=LOC_MMXREGISTER;
                        location.register:=hregister;
                     end
                   else
                     { if on the right the register then swap }
                     if (right.location.loc=LOC_MMXREGISTER) then
                       begin
                          swap_location(location,right.location);
                          { newly swapped also set swapped flag }
                          toggleflag(nf_swaped);
                       end;
                   { at this point, location.loc should be LOC_MMXREGISTER }
                   { and location.register should be a valid register      }
                   { containing the left result                        }
                   if right.location.loc<>LOC_MMXREGISTER then
                     begin
                        if (nodetype=subn) and (nf_swaped in flags) then
                          begin
                             if right.location.loc=LOC_CMMXREGISTER then
                               begin
                                  emit_reg_reg(A_MOVQ,S_NO,right.location.register,R_MM7);
                                  emit_reg_reg(op,S_NO,location.register,R_MM0);
                                  emit_reg_reg(A_MOVQ,S_NO,R_MM7,location.register);
                               end
                             else
                               begin
                                  emit_ref_reg(A_MOVQ,S_NO,
                                    newreference(right.location.reference),R_MM7);
                                  emit_reg_reg(op,S_NO,location.register,
                                    R_MM7);
                                  emit_reg_reg(A_MOVQ,S_NO,
                                    R_MM7,location.register);
                                  del_reference(right.location.reference);
                               end;
                          end
                        else
                          begin
                             if (right.location.loc=LOC_CREGISTER) then
                               begin
                                  emit_reg_reg(op,S_NO,right.location.register,
                                    location.register);
                               end
                             else
                               begin
                                  emit_ref_reg(op,S_NO,newreference(
                                    right.location.reference),location.register);
                                  del_reference(right.location.reference);
                               end;
                          end;
                     end
                   else
                     begin
                        { when swapped another result register }
                        if (nodetype=subn) and (nf_swaped in flags) then
                          begin
                             emit_reg_reg(op,S_NO,
                               location.register,right.location.register);
                             swap_location(location,right.location);
                             { newly swapped also set swapped flag }
                             { just to maintain ordering         }
                             toggleflag(nf_swaped);
                          end
                        else
                          begin
                             emit_reg_reg(op,S_NO,
                               right.location.register,
                               location.register);
                          end;
                        ungetregistermmx(right.location.register);
                     end;
                end
{$endif SUPPORT_MMX}
              else CGMessage(type_e_mismatch);
           end;
       SetResultLocation(cmpop,unsigned);
    end;

begin
   caddnode:=ti386addnode;
end.
{
  $Log$
  Revision 1.26  2001-12-02 16:19:17  jonas
    * less unnecessary regvar loading with if-statements

  Revision 1.25  2001/10/12 13:51:51  jonas
    * fixed internalerror(10) due to previous fpu overflow fixes ("merged")
    * fixed bug in n386add (introduced after compilerproc changes for string
      operations) where calcregisters wasn't called for shortstring addnodes
    * NOTE: from now on, the location of a binary node must now always be set
       before you call calcregisters() for it

  Revision 1.24  2001/09/17 21:29:13  peter
    * merged netbsd, fpu-overflow from fixes branch

  Revision 1.23  2001/09/05 15:22:09  jonas
    * made multiplying, dividing and mod'ing of int64 and qword processor
      independent with compilerprocs (+ small optimizations by using shift/and
      where possible)

  Revision 1.22  2001/09/04 11:38:55  jonas
    + searchsystype() and searchsystype() functions in symtable
    * changed ninl and nadd to use these functions
    * i386 set comparison functions now return their results in al instead
      of in the flags so that they can be sued as compilerprocs
    - removed all processor specific code from n386add.pas that has to do
      with set handling, it's now all done in nadd.pas
    * fixed fpc_set_contains_sets in genset.inc
    * fpc_set_in_byte is now coded inline in n386set.pas and doesn't use a
      helper anymore
    * some small fixes in compproc.inc/set.inc regarding the declaration of
      internal helper types (fpc_small_set and fpc_normal_set)

  Revision 1.21  2001/09/03 13:27:42  jonas
    * compilerproc implementation of set addition/substraction/...
    * changed the declaration of some set helpers somewhat to accomodate the
      above change
    * i386 still uses the old code for comparisons of sets, because its
      helpers return the results in the flags
    * dummy tc_normal_2_small_set type conversion because I need the original
      resulttype of the set add nodes
    NOTE: you have to start a cycle with 1.0.5!

  Revision 1.20  2001/08/30 15:43:14  jonas
    * converted adding/comparing of strings to compileproc. Note that due
      to the way the shortstring helpers for i386 are written, they are
      still handled by the old code (reason: fpc_shortstr_compare returns
      results in the flags instead of in eax and fpc_shortstr_concat
      has wierd parameter conventions). The compilerproc stuff should work
      fine with the generic implementations though.
    * removed some nested comments warnings

  Revision 1.19  2001/08/29 17:50:45  jonas
    * removed unused var

  Revision 1.18  2001/08/29 12:03:23  jonas
    * fixed wrong regalloc info around FPC_MUL/DIV/MOD_INT64/QWORD calls
    * fixed partial result overwriting with the above calls too

  Revision 1.17  2001/08/26 13:36:55  florian
    * some cg reorganisation
    * some PPC updates

  Revision 1.16  2001/07/08 21:00:16  peter
    * various widestring updates, it works now mostly without charset
      mapping supported

  Revision 1.15  2001/06/25 14:11:37  jonas
    * fixed set bug discovered by Carl (merged)

  Revision 1.14  2001/06/18 20:36:25  peter
    * -Ur switch (merged)
    * masm fixes (merged)
    * quoted filenames for go32v2 and win32

  Revision 1.13  2001/05/27 14:30:56  florian
    + some widestring stuff added

  Revision 1.12  2001/05/06 17:12:14  jonas
    * fixed an IE10 and another bug with [var1..var2] construct

  Revision 1.11  2001/04/13 01:22:18  peter
    * symtable change to classes
    * range check generation and errors fixed, make cycle DEBUG=1 works
    * memory leaks fixed

  Revision 1.10  2001/04/02 21:20:36  peter
    * resulttype rewrite

  Revision 1.9  2000/12/31 11:14:11  jonas
    + implemented/fixed docompare() mathods for all nodes (not tested)
    + nopt.pas, nadd.pas, i386/n386opt.pas: optimized nodes for adding strings
      and constant strings/chars together
    * n386add.pas: don't copy temp strings (of size 256) to another temp string
      when adding

  Revision 1.8  2000/12/25 00:07:32  peter
    + new tlinkedlist class (merge of old tstringqueue,tcontainer and
      tlinkedlist objects)

  Revision 1.7  2000/12/16 15:56:18  jonas
    - removed all ifdef cardinalmulfix code

  Revision 1.6  2000/12/05 11:44:32  jonas
    + new integer regvar handling, should be much more efficient

  Revision 1.5  2000/11/29 00:30:45  florian
    * unused units removed from uses clause
    * some changes for widestrings

  Revision 1.4  2000/11/13 11:30:56  florian
    * some bugs with interfaces and NIL fixed

  Revision 1.3  2000/11/04 14:25:23  florian
    + merged Attila's changes for interfaces, not tested yet

  Revision 1.2  2000/10/31 22:02:56  peter
    * symtable splitted, no real code changes

  Revision 1.1  2000/10/15 09:33:31  peter
    * moved n386*.pas to i386/ cpu_target dir

  Revision 1.6  2000/10/14 10:14:47  peter
    * moehrendorf oct 2000 rewrite

  Revision 1.5  2000/09/30 16:08:45  peter
    * more cg11 updates

  Revision 1.4  2000/09/24 15:06:18  peter
    * use defines.inc

  Revision 1.3  2000/09/22 22:42:52  florian
    * more fixes

  Revision 1.2  2000/09/21 12:24:22  jonas
    * small fix to my changes for full boolean evaluation support (moved
      opsize determination for boolean operations back in boolean
      processing block)
    + full boolean evaluation support (from cg386add)

  Revision 1.1  2000/09/20 21:23:32  florian
    * initial revision
}
