{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

    Generate m68k assembler for add node

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
{$ifdef FPC}
  {$goto on}
{$endif FPC}
unit cg68kadd;
interface

    uses
      tree;

    procedure secondadd(var p : ptree);

implementation

    uses
      globtype,systems,symconst,
      cobjects,verbose,globals,
      symtable,aasm,types,
      temp_gen,hcodegen,pass_2,cpubase,
      cga68k,tgen68k;

{*****************************************************************************
                                Helpers
*****************************************************************************}

 procedure processcc(p: ptree);
 const
       { process condition codes bit definitions }
       CARRY_FLAG    = $01;
       OVFL_FLAG     = $02;
       ZERO_FLAG     = $04;
       NEG_FLAG      = $08;
 var
   label1,label2: pasmlabel;
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
                           exprasmlist^.concat(new(paicpu, op_const_reg(
                             A_AND, S_B, NOT ZERO_FLAG, R_CCR)));
                           emitl(A_BRA,label2);
                           emitl(A_LABEL,label1);
                           { equal - set zero flag }
                           exprasmlist^.concat(new(paicpu, op_const_reg(
                             A_OR,S_B, ZERO_FLAG, R_CCR)));
                           emitl(A_LABEL,label2);
                        end;
         ltn:           begin
                           emitl(A_FBLT,label1);
                           { not less than       }
                           { clear N and V flags }
                           exprasmlist^.concat(new(paicpu, op_const_reg(
                             A_AND, S_B, NOT (NEG_FLAG OR OVFL_FLAG), R_CCR)));
                           emitl(A_BRA,label2);
                           emitl(A_LABEL,label1);
                           { less than }
                           exprasmlist^.concat(new(paicpu, op_const_reg(
                             A_OR,S_B, NEG_FLAG, R_CCR)));
                           exprasmlist^.concat(new(paicpu, op_const_reg(
                             A_AND,S_B, NOT OVFL_FLAG, R_CCR)));
                           emitl(A_LABEL,label2);
                        end;
         gtn:           begin
                           emitl(A_FBGT,label1);
                           { not greater than }
                           { set Z flag       }
                           exprasmlist^.concat(new(paicpu, op_const_reg(
                             A_OR, S_B, ZERO_FLAG, R_CCR)));
                           emitl(A_BRA,label2);
                           emitl(A_LABEL,label1);
                           { greater than      }
                           { set N and V flags }
                           exprasmlist^.concat(new(paicpu, op_const_reg(
                             A_OR,S_B, NEG_FLAG OR OVFL_FLAG , R_CCR)));
                           emitl(A_LABEL,label2);
                        end;
         gten:           begin
                           emitl(A_FBGE,label1);
                           { not greater or equal }
                           { set N and clear V    }
                           exprasmlist^.concat(new(paicpu, op_const_reg(
                             A_AND, S_B, NOT OVFL_FLAG, R_CCR)));
                           exprasmlist^.concat(new(paicpu, op_const_reg(
                             A_OR,S_B, NEG_FLAG, R_CCR)));
                           emitl(A_BRA,label2);
                           emitl(A_LABEL,label1);
                           { greater or equal    }
                           { clear V and N flags }
                           exprasmlist^.concat(new(paicpu, op_const_reg(
                             A_AND, S_B, NOT (OVFL_FLAG OR NEG_FLAG), R_CCR)));
                           emitl(A_LABEL,label2);
                        end;
         lten:           begin
                           emitl(A_FBLE,label1);
                           { not less or equal }
                           { clear Z, N and V  }
                           exprasmlist^.concat(new(paicpu, op_const_reg(
                             A_AND, S_B, NOT (ZERO_FLAG OR NEG_FLAG OR OVFL_FLAG), R_CCR)));
                           emitl(A_BRA,label2);
                           emitl(A_LABEL,label1);
                           { less or equal     }
                           { set Z and N       }
                           { and clear V       }
                           exprasmlist^.concat(new(paicpu, op_const_reg(
                             A_OR,S_B, ZERO_FLAG OR NEG_FLAG, R_CCR)));
                           exprasmlist^.concat(new(paicpu, op_const_reg(
                             A_AND,S_B, NOT OVFL_FLAG, R_CCR)));
                           emitl(A_LABEL,label2);
                        end;
           else
             begin
               InternalError(34);
             end;
      end; { end case }
 end;


    procedure SetResultLocation(cmpop,unsigned:boolean;var p :ptree);
      var
         flags : tresflags;
      begin
         { remove temporary location if not a set or string }
         { that's a hack (FK)                               }
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
              clear_location(p^.location);
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
        case pstringdef(p^.left^.resulttype)^.string_typ of
           st_ansistring:
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
                     emitcall('FPC_ANSISTRCMP',true);
                     maybe_loada5;
                     popusedregisters(pushedregs);
                  end;
                end;
             end;
           st_shortstring:
             begin
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
                                  clear_location(p^.left^.location);
                                  p^.left^.location.loc:=LOC_MEM;
                                  p^.left^.location.reference:=href;
                               end;

                             secondpass(p^.right);

                             { on the right we do not need the register anymore too }
                             del_reference(p^.right^.location.reference);
                             pushusedregisters(pushedregs,$ffff);
                             { WE INVERSE THE PARAMETERS!!! }
                             { Because parameters are inversed in the rtl }
                             emitpushreferenceaddr(exprasmlist,p^.right^.location.reference);
                             emitpushreferenceaddr(exprasmlist,p^.left^.location.reference);
                             emitcall('FPC_STRCONCAT',true);
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
                                  (str_length(p^.left)=0)) or
                                 ((p^.right^.treetype=stringconstn) and
                                  (str_length(p^.right)=0))
                               ) then
                               begin
                                  { only one node can be stringconstn }
                                  { else pass 1 would have evaluted   }
                                  { this node                         }
                                  if p^.left^.treetype=stringconstn then
                                    exprasmlist^.concat(new(paicpu,op_ref(
                                      A_TST,S_B,newreference(p^.right^.location.reference))))
                                  else
                                    exprasmlist^.concat(new(paicpu,op_ref(
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

                                 exprasmlist^.concat(new(paicpu,op_ref_reg(
                                      A_LEA,S_L,newreference(p^.left^.location.reference),R_A0)));
                                 exprasmlist^.concat(new(paicpu,op_ref_reg(
                                      A_LEA,S_L,newreference(p^.right^.location.reference),R_A1)));
                                 {
                                 emitpushreferenceaddr(p^.left^.location.reference);
                                 emitpushreferenceaddr(p^.right^.location.reference); }
                                 emitcall('FPC_STRCMP',true);
                                 maybe_loada5;
                                 popusedregisters(pushedregs);
                            end;
                             ungetiftemp(p^.left^.location.reference);
                             ungetiftemp(p^.right^.location.reference);
                          end; { end this case }

                   else CGMessage(type_e_mismatch);
                end;
             end; { end case }
          end;
        SetResultLocation(cmpop,true,p);
      end;


{*****************************************************************************
                                Addset
*****************************************************************************}

    procedure addset(var p : ptree);
      var
        cmpop,
        pushed : boolean;
        href   : treference;
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

        { handle operations }
        case p^.treetype of
          equaln,
        unequaln : begin
                     cmpop:=true;
                     del_reference(p^.left^.location.reference);
                     del_reference(p^.right^.location.reference);
                     pushusedregisters(pushedregs,$ff);
                     emitpushreferenceaddr(exprasmlist,p^.right^.location.reference);
                     emitpushreferenceaddr(exprasmlist,p^.left^.location.reference);
                     emitcall('FPC_SET_COMP_SETS',true);
                     maybe_loada5;
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
                   { add a range or a single element? }
                     if p^.right^.treetype=setelementn then
                      begin
                        concatcopy(p^.left^.location.reference,href,32,false);
                        if assigned(p^.right^.right) then
                         begin
                           loadsetelement(p^.right^.right);
                           loadsetelement(p^.right^.left);
                           emitpushreferenceaddr(exprasmlist,href);
                           emitcall('FPC_SET_SET_RANGE',true);
                         end
                        else
                         begin
                           loadsetelement(p^.right^.left);
                           emitpushreferenceaddr(exprasmlist,href);
                           emitcall('FPC_SET_SET_BYTE',true);
                         end;
                      end
                     else
                      begin
                      { must be an other set }
                        emitpushreferenceaddr(exprasmlist,href);
                        emitpushreferenceaddr(exprasmlist,p^.right^.location.reference);
                        emitpushreferenceaddr(exprasmlist,p^.left^.location.reference);
                        emitcall('FPC_SET_ADD_SETS',true);
                      end;
                     maybe_loada5;
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
                     del_reference(p^.left^.location.reference);
                     del_reference(p^.right^.location.reference);
                     href.symbol:=nil;
                     pushusedregisters(pushedregs,$ff);
                     gettempofsizereference(32,href);
                     emitpushreferenceaddr(exprasmlist,href);
                     emitpushreferenceaddr(exprasmlist,p^.right^.location.reference);
                     emitpushreferenceaddr(exprasmlist,p^.left^.location.reference);
                     case p^.treetype of
                      subn : emitcall('FPC_SET_SUB_SETS',true);
                   symdifn : emitcall('FPC_SET_SYMDIF_SETS',true);
                      muln : emitcall('FPC_SET_MUL_SETS',true);
                     end;
                     maybe_loada5;
                     popusedregisters(pushedregs);
                     ungetiftemp(p^.left^.location.reference);
                     ungetiftemp(p^.right^.location.reference);
                     p^.location.loc:=LOC_MEM;
                     stringdispose(p^.location.reference.symbol);
                     p^.location.reference:=href;
                   end;
        else
          CGMessage(type_e_mismatch);
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
         hregister : tregister;
         noswap,
         pushed,mboverflow,cmpop : boolean;
         op : tasmop;
         flags : tresflags;
         otl,ofl : pasmlabel;
         power : longint;
         opsize : topsize;
         hl4: pasmlabel;
         tmpref : treference;


         { true, if unsigned types are compared }
         unsigned : boolean;
         { true, if a small set is handled with the longint code }
         is_set : boolean;
         { is_in_dest if the result is put directly into }
         { the resulting refernce or varregister }
         is_in_dest : boolean;
         { true, if for sets subtractions the extra not should generated }
         extra_not : boolean;

      begin
      { to make it more readable, string and set (not smallset!) have their
        own procedures }
         case p^.left^.resulttype^.deftype of
         stringdef : begin
                       addstring(p);
                       exit;
                     end;
            setdef : begin
                     { normalsets are handled separate }
                       if not(psetdef(p^.left^.resulttype)^.settype=smallset) then
                        begin
                          addset(p);
                          exit;
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
         is_set:=(p^.left^.resulttype^.deftype=setdef);

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
                       clear_location(p^.location);
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
                         CGMessage(type_e_mismatch);
                       end;
                       secondpass(p^.right);
                       maketojumpbool(p^.right);
                     end;
          unequaln,
       equaln,xorn : begin
                       if p^.left^.treetype=ordconstn then
                        swaptree(p);
                       secondpass(p^.left);
                       set_location(p^.location,p^.left^.location);
                       { are enough registers free ? }
                       pushed:=maybe_push(p^.right^.registers32,p);
                       secondpass(p^.right);
                       if pushed then restore(p);
                       goto do_normal;
                    end
             else
               CGMessage(type_e_mismatch);
             end
           end
         else
           begin
              { in case of constant put it to the left }
              if (p^.left^.treetype=ordconstn) then
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
                  pobjectdef(p^.right^.resulttype)^.is_class and
                 (p^.left^.resulttype^.deftype=objectdef) and
                  pobjectdef(p^.left^.resulttype)^.is_class
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
                 is_set then
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
                   case p^.treetype of
                      addn : begin
                               if is_set then
                                begin
                                { adding elements is not commutative }
                                  if p^.swaped and (p^.left^.treetype=setelementn) then
                                   swaptree(p);
                                { are we adding set elements ? }
                                  if p^.right^.treetype=setelementn then
                                   begin
                                   { no range support for smallsets! }
                                     if assigned(p^.right^.right) then
                                      internalerror(43244);
                                   { Not supported for m68k}
                                     Comment(V_Fatal,'No smallsets for m68k');
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
                                  op:=A_EOR;
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
                                   op:=A_MULU
                                  else
                                   op:=A_MULS;
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
                  ltn,lten,
                  gtn,gten,
           equaln,unequaln : begin
                               op:=A_CMP;
                               cmpop:=true;
                             end;
                      xorn : op:=A_EOR;
                       orn : op:=A_OR;
                      andn : op:=A_AND;
                   else
                     CGMessage(type_e_mismatch);
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
                                  hregister:=getregister32;
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
                                  exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOVE,opsize,
                                    newreference(p^.left^.location.reference),hregister)));
                               end
                             else
                               begin
                                  hregister:=getregister32;
                                  { first give free, then demand new register }
                                 exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOVE,opsize,
                                   newreference(p^.left^.location.reference),hregister)));
                               end;
                          end;
                        clear_location(p^.location);
                        p^.location.loc:=LOC_REGISTER;
                        p^.location.register:=hregister;
                     end
                   else
                     { if on the right the register then swap }
                     if not(noswap) and (p^.right^.location.loc=LOC_REGISTER) then
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
                                    exprasmlist^.concat(new(paicpu,op_reg(A_NOT,opsize,p^.location.register)));

                                  emit_reg_reg(A_MOVE,opsize,p^.right^.location.register,R_D6);
                                  emit_reg_reg(op,opsize,p^.location.register,R_D6);
                                  emit_reg_reg(A_MOVE,opsize,R_D6,p^.location.register);
                               end
                             else
                               begin
                                  if extra_not then
                                    exprasmlist^.concat(new(paicpu,op_reg(A_NOT,opsize,p^.location.register)));

                                  exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOVE,opsize,
                                    newreference(p^.right^.location.reference),R_D6)));
                                  exprasmlist^.concat(new(paicpu,op_reg_reg(op,opsize,p^.location.register,R_D6)));
                                  exprasmlist^.concat(new(paicpu,op_reg_reg(A_MOVE,opsize,R_D6,p^.location.register)));
                                  del_reference(p^.right^.location.reference);
                               end;
                          end
                        else
                          begin
                             if (p^.right^.treetype=ordconstn) and (op=A_CMP) and
                                (p^.right^.value=0) then
                                  exprasmlist^.concat(new(paicpu,op_reg(A_TST,opsize,p^.location.register)))
                             else
                                if (p^.right^.treetype=ordconstn) and (op=A_MULS) and
                                   (ispowerof2(p^.right^.value,power)) then
                                  begin
                                    if (power <= 8) then
                                        exprasmlist^.concat(new(paicpu,op_const_reg(A_ASL,opsize,power,
                                         p^.location.register)))
                                    else
                                      begin
                                        exprasmlist^.concat(new(paicpu,op_const_reg(A_MOVE,S_L,power,
                                         R_D6)));
                                        exprasmlist^.concat(new(paicpu,op_reg_reg(A_ASL,opsize,R_D6,
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
                                            exprasmlist^.concat(new(paicpu,op_reg(A_NOT,S_L,R_D6)));
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
                                              emitcall('FPC_LONGMUL',true);
                                              emit_reg_reg(A_MOVE,opsize,R_D0,p^.location.register);
                                            end
                                            else
                                            if (op=A_MULU) and (opsize = S_L) and (aktoptprocessor=MC68000) then
                                             CGMessage(cg_f_32bit_not_supported_in_68000)
                                            else
                                              emit_reg_reg(op,opsize,p^.right^.location.register,
                                                p^.location.register);
                                         end;
                                    end
                                  else
                                    begin
                                       if extra_not then
                                         begin
                                            exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOVE,S_L,newreference(
                                              p^.right^.location.reference),R_D6)));
                                            exprasmlist^.concat(new(paicpu,op_reg(A_NOT,S_L,R_D6)));
                                            emit_reg_reg(A_AND,S_L,R_D6,
                                              p^.location.register);
                                         end
                                       else
                                         begin
                                            if (op=A_MULS) and (opsize = S_L) and (aktoptprocessor=MC68000) then
                                            { Emulation for MC68000 }
                                            begin
                                              exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOVE, opsize,
                                                 newreference(p^.right^.location.reference),R_D1)));
                                              emit_reg_reg(A_MOVE,opsize,p^.location.register,R_D0);
                                              emitcall('FPC_LONGMUL',true);
                                              emit_reg_reg(A_MOVE,opsize,R_D0,p^.location.register);
                                            end
                                            else
                                            if (op=A_MULU) and (opsize = S_L) and (aktoptprocessor=MC68000) then
                                             CGMessage(cg_f_32bit_not_supported_in_68000)
                                            else
                                            { When one of the source/destination is a memory reference  }
                                            { and the operator is EOR, the we must load it into the     }
                                            { value into a register first since only EOR reg,reg exists }
                                            { on the m68k                                               }
                                            if (op=A_EOR) then
                                              begin
                                                exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOVE,opsize,newreference(
                                                    p^.right^.location.reference),R_D0)));
                                                exprasmlist^.concat(new(paicpu,op_reg_reg(op,opsize,R_D0,
                                                    p^.location.register)));
                                              end
                                            else
                                              exprasmlist^.concat(new(paicpu,op_ref_reg(op,opsize,newreference(
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
                               exprasmlist^.concat(new(paicpu,op_reg(A_NOT,S_L,p^.location.register)));

                             exprasmlist^.concat(new(paicpu,op_reg_reg(op,opsize,
                               p^.location.register,p^.right^.location.register)));
                               swap_location(p^.location,p^.right^.location);

                               { newly swapped also set swapped flag }
                               { just to maintain ordering           }
                               p^.swaped:=not(p^.swaped);
                          end
                        else
                          begin
                             if extra_not then
                                   exprasmlist^.concat(new(paicpu,op_reg(A_NOT,S_L,p^.right^.location.register)));

                             if (op=A_MULS) and (opsize = S_L) and (aktoptprocessor=MC68000) then
                             { Emulation for MC68000 }
                             begin
                               emit_reg_reg(A_MOVE,opsize,p^.right^.location.register,
                               R_D0);
                               emit_reg_reg(A_MOVE,opsize,p^.location.register,R_D1);
                               emitcall('FPC_LONGMUL',true);
                               emit_reg_reg(A_MOVE,opsize,R_D0,p^.location.register);
                             end
                             else
                             if (op=A_MULU) and (opsize = S_L) and (aktoptprocessor=MC68000) then
                              CGMessage(cg_f_32bit_not_supported_in_68000)
                             else

                               exprasmlist^.concat(new(paicpu,op_reg_reg(op,opsize,
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
                   { only in case of overflow operations }
                   { produce overflow code }
                   { we must put it here directly, because sign of operation }
                   { is in unsigned VAR!!                                    }
                end
              else

              { Char type }
                if ((p^.left^.resulttype^.deftype=orddef) and
                    (porddef(p^.left^.resulttype)^.typ=uchar)) then
                 begin
                   case p^.treetype of
                      ltn,lten,gtn,gten,
                      equaln,unequaln :
                                cmpop:=true;
                      else CGMessage(type_e_mismatch);
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
                             exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOVE,S_B,newreference(p^.location.reference),
                               hregister)));
                          end;
                        clear_location(p^.location);
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
                             exprasmlist^.concat(new(paicpu,op_ref_reg(A_CMP,S_B,newreference(
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
              else

              { Floating point }
               if (p^.left^.resulttype^.deftype=floatdef) and
                  (pfloatdef(p^.left^.resulttype)^.typ<>f32bit) then
                 begin
                    { real constants to the left }
                    if p^.left^.treetype=realconstn then
                     swaptree(p);
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
                       else CGMessage(type_e_mismatch);
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
                                  exprasmlist^.concat(new(paicpu,op_reg_reg(A_MOVE,S_L,p^.left^.location.fpureg,R_D0)));


                                  { load value into D1 }
                                  if p^.right^.location.loc <> LOC_FPU then
                                     exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOVE,S_L,
                                       newreference(p^.right^.location.reference),R_D1)))
                                  else
                                     emit_reg_reg(A_MOVE,S_L,p^.right^.location.fpureg,R_D1);

                                  { probably a faster way to do this but... }
                                  case op of
                                   A_FADD: emitcall('FPC_SINGLE_ADD',true);
                                   A_FMUL: emitcall('FPC_SINGLE_MUL',true);
                                   A_FSUB: emitcall('FPC_SINGLE_SUB',true);
                                   A_FDIV: emitcall('FPC_SINGLE_DIV',true);
                                   A_FCMP: emitcall('FPC_SINGLE_CMP',true);
                                  end;
                                  if not cmpop then { only flags are affected with cmpop }
                                     exprasmlist^.concat(new(paicpu,op_reg_reg(A_MOVE,S_L,R_D0,
                                       p^.left^.location.fpureg)));

                                  { if this was a reference, then delete as it }
                                  { it no longer required.                     }
                                  if p^.right^.location.loc <> LOC_FPU then
                                     del_reference(p^.right^.location.reference);
                              end
                             else
                              begin

                                  if p^.right^.location.loc <> LOC_FPU then
                                    exprasmlist^.concat(new(paicpu,op_ref_reg(A_FMOVE,
                                       getfloatsize(pfloatdef(p^.left^.resulttype)^.typ),
                                      newreference(p^.right^.location.reference),
                                      R_FP1)))
                                  else
                                    { FPm --> FPn must use extended precision }
                                    emit_reg_reg(A_FMOVE,S_FX,p^.right^.location.fpureg,R_FP1);

                                  { arithmetic expression performed in extended mode }
                                  exprasmlist^.concat(new(paicpu,op_reg_reg(op,S_FX,
                                      p^.left^.location.fpureg,R_FP1)));

                                  { cmpop does not change any floating point register!! }
                                  if not cmpop then
                                       emit_reg_reg(A_FMOVE,S_FX,R_FP1,p^.left^.location.fpureg)
{                                       exprasmlist^.concat(new(paicpu,op_reg_reg(A_FMOVE,
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
                               exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOVE,S_L,
                                 newreference(p^.right^.location.reference),R_D0)))
                             else
                               emit_reg_reg(A_MOVE,S_L,p^.right^.location.fpureg,R_D0);

                             emit_reg_reg(A_MOVE,S_L,p^.left^.location.fpureg,R_D1);
                             { probably a faster way to do this but... }
                             case op of
                               A_FADD: emitcall('FPC_SINGLE_ADD',true);
                               A_FMUL: emitcall('FPC_SINGLE_MUL',true);
                               A_FSUB: emitcall('FPC_SINGLE_SUB',true);
                               A_FDIV: emitcall('FPC_SINGLE_DIV',true);
                               A_FCMP: emitcall('FPC_SINGLE_CMP',true);
                             end;
                             if not cmpop then { only flags are affected with cmpop }
                               exprasmlist^.concat(new(paicpu,op_reg_reg(A_MOVE,S_L,R_D0,
                                 p^.left^.location.fpureg)));
                             { if this was a reference, then delete as it }
                             { it no longer required.                     }
                             if p^.right^.location.loc <> LOC_FPU then
                               del_reference(p^.right^.location.reference);
                           end
                           else
                           begin
                             if p^.right^.location.loc <> LOC_FPU then
                               exprasmlist^.concat(new(paicpu,op_ref_reg(A_FMOVE,
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
                             { the register is now longer required }
                             if p^.left^.location.loc = LOC_FPU then
                              begin
                                ungetregister(p^.left^.location.fpureg);
                              end;


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
                             clear_location(p^.location);
                             p^.location.loc := LOC_FLAGS;
                             p^.location.resflags := flags;
                             cmpop := false;
                          end
                         else
                         begin
                             clear_location(p^.location);
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


              else CGMessage(type_e_mismatch);
           end;
       SetResultLocation(cmpop,unsigned,p);
    end;


end.
{
  $Log$
  Revision 1.20  2000-03-01 00:04:31  pierre
   Use $GOTO ON

  Revision 1.19  2000/02/09 13:22:48  peter
    * log truncated

  Revision 1.18  2000/01/07 01:14:21  peter
    * updated copyright to 2000

  Revision 1.17  1999/09/16 23:05:51  florian
    * m68k compiler is again compilable (only gas writer, no assembler reader)

  Revision 1.16  1999/09/16 11:34:52  pierre
   * typo correction

}