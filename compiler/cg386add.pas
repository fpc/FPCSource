 {
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

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
{$ifdef FPC}
  {$goto on}
{$endif FPC}
unit cg386add;
interface

{$define usecreateset}

    uses
      tree;

    procedure secondadd(var p : ptree);

implementation

    uses
      globtype,systems,
      cutils,cobjects,verbose,globals,
      symconst,symtable,aasm,types,
      hcodegen,temp_gen,pass_2,
      cpubase,cpuasm,
      cgai386,tgeni386;

{*****************************************************************************
                                Helpers
*****************************************************************************}

    function getresflags(p : ptree;unsigned : boolean) : tresflags;

      begin
         if not(unsigned) then
           begin
              if p^.swaped then
                case p^.treetype of
                   equaln : getresflags:=F_E;
                   unequaln : getresflags:=F_NE;
                   ltn : getresflags:=F_G;
                   lten : getresflags:=F_GE;
                   gtn : getresflags:=F_L;
                   gten : getresflags:=F_LE;
                end
              else
                case p^.treetype of
                   equaln : getresflags:=F_E;
                   unequaln : getresflags:=F_NE;
                   ltn : getresflags:=F_L;
                   lten : getresflags:=F_LE;
                   gtn : getresflags:=F_G;
                   gten : getresflags:=F_GE;
                end;
           end
         else
           begin
              if p^.swaped then
                case p^.treetype of
                   equaln : getresflags:=F_E;
                   unequaln : getresflags:=F_NE;
                   ltn : getresflags:=F_A;
                   lten : getresflags:=F_AE;
                   gtn : getresflags:=F_B;
                   gten : getresflags:=F_BE;
                end
              else
                case p^.treetype of
                   equaln : getresflags:=F_E;
                   unequaln : getresflags:=F_NE;
                   ltn : getresflags:=F_B;
                   lten : getresflags:=F_BE;
                   gtn : getresflags:=F_A;
                   gten : getresflags:=F_AE;
                end;
           end;
      end;


    procedure SetResultLocation(cmpop,unsigned:boolean;var p :ptree);

      begin
         { remove temporary location if not a set or string }
         { that's a bad hack (FK) who did this ?            }
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
              clear_location(p^.location);
              p^.location.loc:=LOC_FLAGS;
              p^.location.resflags:=getresflags(p,unsigned);
           end;
      end;


{*****************************************************************************
                                Addstring
*****************************************************************************}

    procedure addstring(var p : ptree);
      var
{$ifdef newoptimizations2}
        l: pasmlabel;
        hreg: tregister;
        href2: preference;
        oldregisterdef: boolean;
{$endif newoptimizations2}
        pushedregs : tpushed;
        href       : treference;
        pushed,
        cmpop      : boolean;
        regstopush : byte;
      begin
        { string operations are not commutative }
        if p^.swaped then
          swaptree(p);
        case pstringdef(p^.left^.resulttype)^.string_typ of
           st_ansistring:
             begin
                case p^.treetype of
                   addn:
                     begin
                        cmpop:=false;
                        secondpass(p^.left);
                        { to avoid problem with maybe_push and restore }
                        set_location(p^.location,p^.left^.location);
                        pushed:=maybe_push(p^.right^.registers32,p,false);
                        secondpass(p^.right);
                        if pushed then
                          begin
                             restore(p,false);
                             set_location(p^.left^.location,p^.location);
                          end;
                        { get the temp location, must be done before regs are
                          released/pushed because after the release the regs are
                          still used for the push (PFV) }
                        clear_location(p^.location);
                        p^.location.loc:=LOC_MEM;
                        gettempansistringreference(p^.location.reference);
                        decrstringref(cansistringdef,p^.location.reference);
                        { release used registers }
                        del_location(p^.right^.location);
                        del_location(p^.left^.location);
                        { push the still used registers }
                        pushusedregisters(pushedregs,$ff);
                        { push data }
                        emitpushreferenceaddr(p^.location.reference);
                        emit_push_loc(p^.right^.location);
                        emit_push_loc(p^.left^.location);
                        emitcall('FPC_ANSISTR_CONCAT');
                        popusedregisters(pushedregs);
                        maybe_loadesi;
                        ungetiftempansi(p^.left^.location.reference);
                        ungetiftempansi(p^.right^.location.reference);
                     end;
                   ltn,lten,gtn,gten,
                   equaln,unequaln:
                     begin
                        cmpop:=true;
                        if (p^.treetype in [equaln,unequaln]) and
                           (p^.left^.treetype=stringconstn) and
                           (p^.left^.length=0) then
                          begin
                             secondpass(p^.right);
                             { release used registers }
                             del_location(p^.right^.location);
                             del_location(p^.left^.location);
                             case p^.right^.location.loc of
                               LOC_REFERENCE,LOC_MEM:
                                 emit_const_ref(A_CMP,S_L,0,newreference(p^.right^.location.reference));
                               LOC_REGISTER,LOC_CREGISTER:
                                 emit_const_reg(A_CMP,S_L,0,p^.right^.location.register);
                             end;
                             ungetiftempansi(p^.left^.location.reference);
                             ungetiftempansi(p^.right^.location.reference);
                          end
                        else if (p^.treetype in [equaln,unequaln]) and
                          (p^.right^.treetype=stringconstn) and
                          (p^.right^.length=0) then
                          begin
                             secondpass(p^.left);
                             { release used registers }
                             del_location(p^.right^.location);
                             del_location(p^.left^.location);
                             case p^.right^.location.loc of
                               LOC_REFERENCE,LOC_MEM:
                                 emit_const_ref(A_CMP,S_L,0,newreference(p^.left^.location.reference));
                               LOC_REGISTER,LOC_CREGISTER:
                                 emit_const_reg(A_CMP,S_L,0,p^.left^.location.register);
                             end;
                             ungetiftempansi(p^.left^.location.reference);
                             ungetiftempansi(p^.right^.location.reference);
                          end
                        else
                          begin
                             secondpass(p^.left);
                             pushed:=maybe_push(p^.right^.registers32,p^.left,false);
                             secondpass(p^.right);
                             if pushed then
                               restore(p^.left,false);
                             { release used registers }
                             del_location(p^.right^.location);
                             del_location(p^.left^.location);
                             { push the still used registers }
                             pushusedregisters(pushedregs,$ff);
                             { push data }
                             case p^.right^.location.loc of
                               LOC_REFERENCE,LOC_MEM:
                                 emit_push_mem(p^.right^.location.reference);
                               LOC_REGISTER,LOC_CREGISTER:
                                 emit_reg(A_PUSH,S_L,p^.right^.location.register);
                             end;
                             case p^.left^.location.loc of
                               LOC_REFERENCE,LOC_MEM:
                                 emit_push_mem(p^.left^.location.reference);
                               LOC_REGISTER,LOC_CREGISTER:
                                 emit_reg(A_PUSH,S_L,p^.left^.location.register);
                             end;
                             emitcall('FPC_ANSISTR_COMPARE');
                             emit_reg_reg(A_OR,S_L,R_EAX,R_EAX);
                             popusedregisters(pushedregs);
                             maybe_loadesi;
                             ungetiftempansi(p^.left^.location.reference);
                             ungetiftempansi(p^.right^.location.reference);
                          end;
                     end;
                end;
               { the result of ansicompare is signed }
               SetResultLocation(cmpop,false,p);
             end;
           st_shortstring:
             begin
                case p^.treetype of
                   addn:
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

                             gettempofsizereference(256,href);
                             copyshortstring(href,p^.left^.location.reference,255,false,true);
                             { release the registers }
{                             done by copyshortstring now (JM)           }
{                             del_reference(p^.left^.location.reference); }
                             ungetiftemp(p^.left^.location.reference);

                             { does not hurt: }
                             clear_location(p^.left^.location);
                             p^.left^.location.loc:=LOC_MEM;
                             p^.left^.location.reference:=href;

{$ifdef newoptimizations2}
                             { length of temp string = 255 (JM) }
                             { *** redefining a type is not allowed!! (thanks, Pierre) }
                             { also problem with constant string!                      }
                             pstringdef(p^.left^.resulttype)^.len := 255;

{$endif newoptimizations2}
                          end;

                        secondpass(p^.right);

{$ifdef newoptimizations2}
                        { special case for string := string + char (JM) }
                        { needs string length stuff from above!         }
                        hreg := R_NO;
                        if is_shortstring(p^.left^.resulttype) and
                           is_char(p^.right^.resulttype) then
                          begin
                            getlabel(l);
                            getexplicitregister32(R_EDI);
                            { load the current string length }
                            emit_ref_reg(A_MOVZX,S_BL,
                              newreference(p^.left^.location.reference),R_EDI);
                            { is it already maximal? }
                            emit_const_reg(A_CMP,S_L,
                              pstringdef(p^.left^.resulttype)^.len,R_EDI);
                            emitjmp(C_E,l);
                            { no, so add the new character }
                            { is it a constant char? }
                            if (p^.right^.treetype <> ordconstn) then
                              { no, make sure it is in a register }
                              if p^.right^.location.loc in [LOC_REFERENCE,LOC_MEM] then
                                begin
                                  { free the registers of p^.right }
                                  del_reference(p^.right^.location.reference);
                                  { get register for the char }
                                  hreg := reg32toreg8(getregister32);
                                  emit_ref_reg(A_MOV,S_B,
                                    newreference(p^.right^.location.reference),
                                    hreg);
                                 { I don't think a temp char exists, but it won't hurt (JM)Ê}
                                 ungetiftemp(p^.right^.location.reference);
                                end
                              else hreg := p^.right^.location.register;
                            href2 := newreference(p^.left^.location.reference);
                            { we need a new reference to store the character }
                            { at the end of the string. Check if the base or }
                            { index register is still free                   }
                            if (p^.left^.location.reference.base <> R_NO) and
                               (p^.left^.location.reference.index <> R_NO) then
                              begin
                                { they're not free, so add the base reg to }
                                { the string length (since the index can   }
                                { have a scalefactor) and use EDI as base  }
                                emit_reg_reg(A_ADD,S_L,
                                  p^.left^.location.reference.base,R_EDI);
                                href2^.base := R_EDI;
                              end
                            else
                              { at least one is still free, so put EDI there }
                              if href2^.base = R_NO then
                                href2^.base := R_EDI
                              else
                                begin
                                  href2^.index := R_EDI;
                                  href2^.scalefactor := 1;
                                end;
                            { we need to be one position after the last char }
                            inc(href2^.offset);
                            { increase the string length }
                            emit_ref(A_INC,S_B,newreference(p^.left^.location.reference));
                            { and store the character at the end of the string }
                            if (p^.right^.treetype <> ordconstn) then
                              begin
                                { no new_reference(href2) because it's only }
                                { used once (JM)                            }
                                emit_reg_ref(A_MOV,S_B,hreg,href2);
                                ungetregister(hreg);
                              end
                            else
                              emit_const_ref(A_MOV,S_B,p^.right^.value,href2);
                            emitlab(l);
                            ungetregister32(R_EDI);
                          end
                        else
                          begin
{$endif  newoptimizations2}
                        { on the right we do not need the register anymore too }
                        { Instead of releasing them already, simply do not }
                        { push them (so the release is in the right place, }
                        { because emitpushreferenceaddr doesn't need extra }
                        { registers) (JM)                                  }
                            regstopush := $ff;
                            remove_non_regvars_from_loc(p^.right^.location,
                              regstopush);
                           pushusedregisters(pushedregs,regstopush);
                           { push the maximum possible length of the result }
{$ifdef newoptimizations2}
                           { string (could be < 255 chars now) (JM)         }
                            emit_const(A_PUSH,S_L,
                              pstringdef(p^.left^.resulttype)^.len);
{$endif newoptimizations2}
                            emitpushreferenceaddr(p^.left^.location.reference);
                           { the optimizer can more easily put the          }
                           { deallocations in the right place if it happens }
                           { too early than when it happens too late (if    }
                           { the pushref needs a "lea (..),edi; push edi")  }
                            del_reference(p^.right^.location.reference);
                            emitpushreferenceaddr(p^.right^.location.reference);
{$ifdef newoptimizations2}
                            emitcall('FPC_SHORTSTR_CONCAT_LEN');
{$else newoptimizations2}
                            emitcall('FPC_SHORTSTR_CONCAT');
{$endif newoptimizations2}
                            ungetiftemp(p^.right^.location.reference);
                            maybe_loadesi;
                            popusedregisters(pushedregs);
{$ifdef newoptimizations2}
                        end;
{$endif newoptimizations2}
                        set_location(p^.location,p^.left^.location);
                     end;
                   ltn,lten,gtn,gten,
                   equaln,unequaln :
                     begin
                        cmpop:=true;
                        { generate better code for s='' and s<>'' }
                        if (p^.treetype in [equaln,unequaln]) and
                           (((p^.left^.treetype=stringconstn) and (str_length(p^.left)=0)) or
                            ((p^.right^.treetype=stringconstn) and (str_length(p^.right)=0))) then
                          begin
                             secondpass(p^.left);
                             { are too few registers free? }
                             pushed:=maybe_push(p^.right^.registers32,p^.left,false);
                             secondpass(p^.right);
                             if pushed then
                               restore(p^.left,false);
                             { only one node can be stringconstn }
                             { else pass 1 would have evaluted   }
                             { this node                         }
                             if p^.left^.treetype=stringconstn then
                               emit_const_ref(
                                 A_CMP,S_B,0,newreference(p^.right^.location.reference))
                             else
                               emit_const_ref(
                                 A_CMP,S_B,0,newreference(p^.left^.location.reference));
                             del_reference(p^.right^.location.reference);
                             del_reference(p^.left^.location.reference);
                          end
                        else
                          begin
                             pushusedregisters(pushedregs,$ff);
                             secondpass(p^.left);
                             emitpushreferenceaddr(p^.left^.location.reference);
                             del_reference(p^.left^.location.reference);
                             secondpass(p^.right);
                             emitpushreferenceaddr(p^.right^.location.reference);
                             del_reference(p^.right^.location.reference);
                             emitcall('FPC_SHORTSTR_COMPARE');
                             maybe_loadesi;
                             popusedregisters(pushedregs);
                          end;
                        ungetiftemp(p^.left^.location.reference);
                        ungetiftemp(p^.right^.location.reference);
                     end;
                   else CGMessage(type_e_mismatch);
                end;
               SetResultLocation(cmpop,true,p);
             end;
          end;
      end;


{*****************************************************************************
                                Addset
*****************************************************************************}

    procedure addset(var p : ptree);
      var
        createset,
        cmpop,
        pushed : boolean;
        href   : treference;
        pushedregs : tpushed;
        regstopush: byte;
      begin
        cmpop:=false;

        { not commutative }
        if p^.swaped then
         swaptree(p);

        { optimize first loading of a set }
{$ifdef usecreateset}
        if (p^.right^.treetype=setelementn) and
           not(assigned(p^.right^.right)) and
           is_emptyset(p^.left) then
         createset:=true
        else
{$endif}
         begin
           createset:=false;
           secondpass(p^.left);
         end;

        { are too few registers free? }
        pushed:=maybe_push(p^.right^.registers32,p^.left,false);
        secondpass(p^.right);
        if codegenerror then
          exit;
        if pushed then
          restore(p^.left,false);

        set_location(p^.location,p^.left^.location);

        { handle operations }

        case p^.treetype of
          equaln,
        unequaln
{$IfNDef NoSetInclusion}
        ,lten, gten
{$EndIf NoSetInclusion}
                  : begin
                     cmpop:=true;
                     del_location(p^.left^.location);
                     del_location(p^.right^.location);
                     pushusedregisters(pushedregs,$ff);
{$IfNDef NoSetInclusion}
                     If (p^.treetype in [equaln, unequaln, lten]) Then
                       Begin
{$EndIf NoSetInclusion}
                         emitpushreferenceaddr(p^.right^.location.reference);
                         emitpushreferenceaddr(p^.left^.location.reference);
{$IfNDef NoSetInclusion}
                       End
                     Else  {gten = lten, if the arguments are reversed}
                       Begin
                         emitpushreferenceaddr(p^.left^.location.reference);
                         emitpushreferenceaddr(p^.right^.location.reference);
                       End;
                     Case p^.treetype of
                       equaln, unequaln:
{$EndIf NoSetInclusion}
                         emitcall('FPC_SET_COMP_SETS');
{$IfNDef NoSetInclusion}
                       lten, gten:
                         Begin
                           emitcall('FPC_SET_CONTAINS_SETS');
                           { we need a jne afterwards, not a jnbe/jnae }
                           p^.treetype := equaln;
                        End;
                     End;
{$EndIf NoSetInclusion}
                     maybe_loadesi;
                     popusedregisters(pushedregs);
                     ungetiftemp(p^.left^.location.reference);
                     ungetiftemp(p^.right^.location.reference);
                   end;
            addn : begin
                   { add can be an other SET or Range or Element ! }
                     { del_location(p^.right^.location);
                       done in pushsetelement below PM

                     And someone added it again because those registers must
                     not be pushed by the pushusedregisters, however this
                     breaks the optimizer (JM)

                     del_location(p^.right^.location);
                     pushusedregisters(pushedregs,$ff);}

                     regstopush := $ff;
                     remove_non_regvars_from_loc(p^.right^.location,regstopush);
                     remove_non_regvars_from_loc(p^.left^.location,regstopush);
                     pushusedregisters(pushedregs,regstopush);
                     { this is still right before the instruction that uses }
                     { p^.left^.location, but that can be fixed by the      }
                     { optimizer. There must never be an additional         }
                     { between the release and the use, because that is not }
                     { detected/fixed. As Pierre said above, p^.right^.loc  }
                     { will be released in pushsetelement (JM)              }
                     del_location(p^.left^.location);
                     href.symbol:=nil;
                     gettempofsizereference(32,href);
                     if createset then
                      begin
                        pushsetelement(p^.right^.left);
                        emitpushreferenceaddr(href);
                        emitcall('FPC_SET_CREATE_ELEMENT');
                      end
                     else
                      begin
                      { add a range or a single element? }
                        if p^.right^.treetype=setelementn then
                         begin
{$IfNDef regallocfix}
                           concatcopy(p^.left^.location.reference,href,32,false,false);
{$Else regallocfix}
                           concatcopy(p^.left^.location.reference,href,32,true,false);
{$EndIf regallocfix}
                           if assigned(p^.right^.right) then
                            begin
                              pushsetelement(p^.right^.right);
                              pushsetelement(p^.right^.left);
                              emitpushreferenceaddr(href);
                              emitcall('FPC_SET_SET_RANGE');
                            end
                           else
                            begin
                              pushsetelement(p^.right^.left);
                              emitpushreferenceaddr(href);
                              emitcall('FPC_SET_SET_BYTE');
                            end;
                         end
                        else
                         begin
                         { must be an other set }
                           emitpushreferenceaddr(href);
                           emitpushreferenceaddr(p^.right^.location.reference);
{$IfDef regallocfix}
                           del_location(p^.right^.location);
{$EndIf regallocfix}
                           emitpushreferenceaddr(p^.left^.location.reference);
{$IfDef regallocfix}
                           del_location(p^.left^.location);
{$EndIf regallocfix}
                           emitcall('FPC_SET_ADD_SETS');
                         end;
                      end;
                     maybe_loadesi;
                     popusedregisters(pushedregs);
                     ungetiftemp(p^.left^.location.reference);
                     ungetiftemp(p^.right^.location.reference);
                     p^.location.loc:=LOC_MEM;
                     p^.location.reference:=href;
                   end;
            subn,
         symdifn,
            muln : begin
                     { Find out which registers have to pushed (JM) }
                     regstopush := $ff;
                     remove_non_regvars_from_loc(p^.left^.location,regstopush);
                     remove_non_regvars_from_loc(p^.right^.location,regstopush);
                     { Push them (JM) }
                     pushusedregisters(pushedregs,regstopush);
                     href.symbol:=nil;
                     gettempofsizereference(32,href);
                     emitpushreferenceaddr(href);
                     { Release the registers right before they're used,  }
                     { see explanation in cgai386.pas:loadansistring for }
                     { info why this is done right before the push (JM)  }
                     del_location(p^.right^.location);
                     emitpushreferenceaddr(p^.right^.location.reference);
                     { The same here }
                     del_location(p^.left^.location);
                     emitpushreferenceaddr(p^.left^.location.reference);
                     case p^.treetype of
                      subn : emitcall('FPC_SET_SUB_SETS');
                   symdifn : emitcall('FPC_SET_SYMDIF_SETS');
                      muln : emitcall('FPC_SET_MUL_SETS');
                     end;
                     maybe_loadesi;
                     popusedregisters(pushedregs);
                     ungetiftemp(p^.left^.location.reference);
                     ungetiftemp(p^.right^.location.reference);
                     p^.location.loc:=LOC_MEM;
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
    { operators                                                }

      label do_normal;

      var
         hregister,hregister2 : tregister;
         noswap,popeax,popedx,
         pushed,mboverflow,cmpop : boolean;
         op,op2 : tasmop;
         flags : tresflags;
         otl,ofl : pasmlabel;
         power : longint;
         opsize : topsize;
         hl4: pasmlabel;
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
         hloc : tlocation;
         regstopush: byte;

      procedure firstjmp64bitcmp;

        var
           oldtreetype : ttreetyp;

        begin
           { the jump the sequence is a little bit hairy }
           case p^.treetype of
              ltn,gtn:
                begin
                   emitjmp(flag_2_cond[getresflags(p,unsigned)],truelabel);
                   { cheat a little bit for the negative test }
                   p^.swaped:=not(p^.swaped);
                   emitjmp(flag_2_cond[getresflags(p,unsigned)],falselabel);
                   p^.swaped:=not(p^.swaped);
                end;
              lten,gten:
                begin
                   oldtreetype:=p^.treetype;
                   if p^.treetype=lten then
                     p^.treetype:=ltn
                   else
                     p^.treetype:=gtn;
                   emitjmp(flag_2_cond[getresflags(p,unsigned)],truelabel);
                   { cheat for the negative test }
                   if p^.treetype=ltn then
                     p^.treetype:=gtn
                   else
                     p^.treetype:=ltn;
                   emitjmp(flag_2_cond[getresflags(p,unsigned)],falselabel);
                   p^.treetype:=oldtreetype;
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
           case p^.treetype of
              ltn,gtn,lten,gten:
                begin
                   { the comparisaion of the low dword have to be }
                   {  always unsigned!                            }
                   emitjmp(flag_2_cond[getresflags(p,true)],truelabel);
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


    procedure handle_bool_as_int;

      begin
        if p^.left^.treetype=ordconstn then
        swaptree(p);
        if p^.left^.location.loc=LOC_JUMP then
          begin
            otl:=truelabel;
            getlabel(truelabel);
            ofl:=falselabel;
            getlabel(falselabel);
          end;

        secondpass(p^.left);
        { if in flags then copy first to register, because the
          flags can be destroyed }
        case p^.left^.location.loc of
          LOC_FLAGS:
            locflags2reg(p^.left^.location,opsize);
          LOC_JUMP:
            locjump2reg(p^.left^.location,opsize, otl, ofl);
        end;
        set_location(p^.location,p^.left^.location);
        pushed:=maybe_push(p^.right^.registers32,p,false);
        if p^.right^.location.loc=LOC_JUMP then
          begin
            otl:=truelabel;
            getlabel(truelabel);
            ofl:=falselabel;
            getlabel(falselabel);
          end;
        secondpass(p^.right);
        if pushed then
          begin
            restore(p,false);
            set_location(p^.left^.location,p^.location);
          end;
        case p^.right^.location.loc of
          LOC_FLAGS:
            locflags2reg(p^.right^.location,opsize);
          LOC_JUMP:
            locjump2reg(p^.right^.location,opsize,otl,ofl);
        end;
      end;


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
         if is_boolean(p^.left^.resulttype) and
            is_boolean(p^.right^.resulttype) then
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
             if (cs_full_boolean_eval in aktlocalswitches) or
                (p^.treetype in
                  [unequaln,ltn,lten,gtn,gten,equaln,xorn]) then
               begin
                 if p^.left^.treetype=ordconstn then
                 swaptree(p);
                 if p^.left^.location.loc=LOC_JUMP then
                   begin
                     otl:=truelabel;
                     getlabel(truelabel);
                     ofl:=falselabel;
                     getlabel(falselabel);
                   end;

                 secondpass(p^.left);
                 { if in flags then copy first to register, because the
                   flags can be destroyed }
                 case p^.left^.location.loc of
                   LOC_FLAGS:
                     locflags2reg(p^.left^.location,opsize);
                   LOC_JUMP:
                     locjump2reg(p^.left^.location,opsize, otl, ofl);
                 end;
                 set_location(p^.location,p^.left^.location);
                 pushed:=maybe_push(p^.right^.registers32,p,false);
                 if p^.right^.location.loc=LOC_JUMP then
                   begin
                     otl:=truelabel;
                     getlabel(truelabel);
                     ofl:=falselabel;
                     getlabel(falselabel);
                   end;
                 secondpass(p^.right);
                 if pushed then
                   begin
                     restore(p,false);
                     set_location(p^.left^.location,p^.location);
                   end;
                 case p^.right^.location.loc of
                   LOC_FLAGS:
                     locflags2reg(p^.right^.location,opsize);
                   LOC_JUMP:
                     locjump2reg(p^.right^.location,opsize,otl,ofl);
                 end;
                 goto do_normal;
               end;
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
                                  emitlab(truelabel);
                                  truelabel:=otl;
                               end;
                        orn : begin
                                 ofl:=falselabel;
                                 getlabel(falselabel);
                                 secondpass(p^.left);
                                 maketojumpbool(p^.left);
                                 emitlab(falselabel);
                                 falselabel:=ofl;
                              end;
                       else
                         CGMessage(type_e_mismatch);
                       end;
                       secondpass(p^.right);
                       maketojumpbool(p^.right);
                     end;
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
              pushed:=maybe_push(p^.right^.registers32,p,is_64bitint(p^.left^.resulttype));
              secondpass(p^.right);
              if pushed then
                begin
                  restore(p,is_64bitint(p^.left^.resulttype));
                  set_location(p^.left^.location,p^.location);
                end;

              if (p^.left^.resulttype^.deftype=pointerdef) or

                 (p^.right^.resulttype^.deftype=pointerdef) or

                 ((p^.right^.resulttype^.deftype=objectdef) and
                  pobjectdef(p^.right^.resulttype)^.is_class and
                 (p^.left^.resulttype^.deftype=objectdef) and
                  pobjectdef(p^.left^.resulttype)^.is_class
                 ) or

                 (p^.left^.resulttype^.deftype=classrefdef) or

                 (p^.left^.resulttype^.deftype=procvardef) or

                 ((p^.left^.resulttype^.deftype=enumdef) and
                  (p^.left^.resulttype^.size=4)) or

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
{$ifndef cardinalmulfix}
                   unsigned :=
                     (p^.left^.resulttype^.deftype=pointerdef) or
                     (p^.right^.resulttype^.deftype=pointerdef) or
                     ((p^.left^.resulttype^.deftype=orddef) and
                      (porddef(p^.left^.resulttype)^.typ=u32bit)) or
                     ((p^.right^.resulttype^.deftype=orddef) and
                      (porddef(p^.right^.resulttype)^.typ=u32bit));
{$else cardinalmulfix}
                   unsigned := not(is_signed(p^.left^.resulttype)) or
                               not(is_signed(p^.right^.resulttype));
{$endif cardinalmulfix}
                   case p^.treetype of
                      addn : begin
                               { this is a really ugly hack!!!!!!!!!! }
                               { this could be done later using EDI   }
                               { as it is done for subn               }
                               { instead of two registers!!!!         }
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
                                   { bts requires both elements to be registers }
                                     if p^.left^.location.loc in [LOC_MEM,LOC_REFERENCE] then
                                      begin
                                        ungetiftemp(p^.left^.location.reference);
                                        del_location(p^.left^.location);
{!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!}
                                        hregister:=getregister32;
                                        emit_ref_reg(A_MOV,opsize,
                                          newreference(p^.left^.location.reference),hregister);
                                        clear_location(p^.left^.location);
                                        p^.left^.location.loc:=LOC_REGISTER;
                                        p^.left^.location.register:=hregister;
                                        set_location(p^.location,p^.left^.location);
                                      end;
                                     if p^.right^.location.loc in [LOC_MEM,LOC_REFERENCE] then
                                      begin
                                        ungetiftemp(p^.right^.location.reference);
                                        del_location(p^.right^.location);
                                        hregister:=getregister32;
                                        emit_ref_reg(A_MOV,opsize,
                                          newreference(p^.right^.location.reference),hregister);
                                        clear_location(p^.right^.location);
                                        p^.right^.location.loc:=LOC_REGISTER;
                                        p^.right^.location.register:=hregister;
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
{$IfNDef NoSetConstNot}
                                  If (p^.right^.treetype = setconstn) then
                                    p^.right^.location.reference.offset := not(p^.right^.location.reference.offset)
                                  Else
{$EndIf NoNosetConstNot}
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
{$IfNDef NoSetInclusion}
                               If is_set Then
                                 Case p^.treetype of
                                   lten,gten:
                                     Begin
                                      If p^.treetype = lten then
                                        swaptree(p);
                                      if p^.left^.location.loc in [LOC_MEM,LOC_REFERENCE] then
                                        begin
                                         ungetiftemp(p^.left^.location.reference);
                                         del_reference(p^.left^.location.reference);
                                         hregister:=getregister32;
                                         emit_ref_reg(A_MOV,opsize,
                                           newreference(p^.left^.location.reference),hregister);
                                         clear_location(p^.left^.location);
                                         p^.left^.location.loc:=LOC_REGISTER;
                                         p^.left^.location.register:=hregister;
                                         set_location(p^.location,p^.left^.location);
                                       end
                                      else
                                       if p^.left^.location.loc = LOC_CREGISTER Then
                                        {save the register var in a temp register, because
                                          its value is going to be modified}
                                          begin
                                            hregister := getregister32;
                                            emit_reg_reg(A_MOV,opsize,
                                              p^.left^.location.register,hregister);
                                             clear_location(p^.left^.location);
                                             p^.left^.location.loc:=LOC_REGISTER;
                                             p^.left^.location.register:=hregister;
                                             set_location(p^.location,p^.left^.location);
                                           end;
                                     {here, p^.left^.location should be LOC_REGISTER}
                                      If p^.right^.location.loc in [LOC_MEM,LOC_REFERENCE] Then
                                         emit_ref_reg(A_AND,opsize,
                                           newreference(p^.right^.location.reference),p^.left^.location.register)
                                      Else
                                        emit_reg_reg(A_AND,opsize,
                                          p^.right^.location.register,p^.left^.location.register);
                {warning: ugly hack ahead: we need a "jne" after the cmp, so
                 change the treetype from lten/gten to equaln}
                                      p^.treetype := equaln
                                     End;
                           {no < or > support for sets}
                                   ltn,gtn: CGMessage(type_e_mismatch);
                                 End;
{$EndIf NoSetInclusion}
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
                       { p^.left^.location and p^.right^.location must }
                       { only be freed when they are really released,  }
                       { because the optimizer NEEDS correct regalloc  }
                       { info!!! (JM)                                  }
                       clear_location(p^.location);

                 { the p^.location.register will be filled in later (JM) }
                       p^.location.loc:=LOC_REGISTER;
{$IfNDef NoShlMul}
                       if p^.right^.treetype=ordconstn then
                        swaptree(p);
                       If (p^.left^.treetype = ordconstn) and
                          ispowerof2(p^.left^.value, power) and
                          not(cs_check_overflow in aktlocalswitches) then
                         Begin
                           { This release will be moved after the next }
                           { instruction by the optimizer. No need to  }
                           { release p^.left^.location, since it's a   }
                           { constant (JM)                             }
                           release_loc(p^.right^.location);
                           p^.location.register := getregister32;
                           emitloadord2reg(p^.right^.location,u32bitdef,p^.location.register,false);
                           emit_const_reg(A_SHL,S_L,power,p^.location.register)
                         End
                       Else
                        Begin
{$EndIf NoShlMul}
                         regstopush := $ff;
                         remove_non_regvars_from_loc(p^.right^.location,regstopush);
                         remove_non_regvars_from_loc(p^.left^.location,regstopush);
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
                         { p^.left^.location can be R_EAX !!! }
{$ifndef noAllocEdi}
                         getexplicitregister32(R_EDI);
{$endif noAllocEdi}
                         { load the left value }
                         emitloadord2reg(p^.left^.location,u32bitdef,R_EDI,true);
                         release_loc(p^.left^.location);
                         { allocate EAX }
                         if R_EAX in unused then
                           exprasmlist^.concat(new(pairegalloc,alloc(R_EAX)));
                         { load he right value }
                         emitloadord2reg(p^.right^.location,u32bitdef,R_EAX,true);
                         release_loc(p^.right^.location);
                         { allocate EAX if it isn't yet allocated (JM) }
                         if (R_EAX in unused) then
                           exprasmlist^.concat(new(pairegalloc,alloc(R_EAX)));
{$ifndef noAllocEdi}
                         { also allocate EDX, since it is also modified by }
                         { a mul (JM)                                      }
                         if R_EDX in unused then
                           exprasmlist^.concat(new(pairegalloc,alloc(R_EDX)));
{$endif noAllocEdi}
                         emit_reg(A_MUL,S_L,R_EDI);
{$ifndef noAllocEdi}
                         ungetregister32(R_EDI);
                         if R_EDX in unused then
                           exprasmlist^.concat(new(pairegalloc,dealloc(R_EDX)));
{$endif noAllocEdi}
                         if R_EAX in unused then
                           exprasmlist^.concat(new(pairegalloc,dealloc(R_EAX)));
                         p^.location.register := getregister32;
                         emit_reg_reg(A_MOV,S_L,R_EAX,p^.location.register);
                         if popedx then
                          emit_reg(A_POP,S_L,R_EDX);
                         if popeax then
                          emit_reg(A_POP,S_L,R_EAX);
{$IfNDef NoShlMul}
                        End;
{$endif NoShlMul}
                       SetResultLocation(false,true,p);
                       exit;
                     end;

                   { Convert flags to register first }
                   if (p^.left^.location.loc=LOC_FLAGS) then
                    locflags2reg(p^.left^.location,opsize);
                   if (p^.right^.location.loc=LOC_FLAGS) then
                    locflags2reg(p^.right^.location,opsize);

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
                             ungetiftemp(p^.left^.location.reference);
                             del_reference(p^.left^.location.reference);
                             if is_in_dest then
                               begin
                                  hregister:=p^.location.register;
                                  emit_ref_reg(A_MOV,opsize,
                                    newreference(p^.left^.location.reference),hregister);
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
                                    newreference(p^.left^.location.reference),hregister);
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
                   { containing the left result                     }

                    if p^.right^.location.loc<>LOC_REGISTER then
                     begin
                        if (p^.treetype=subn) and p^.swaped then
                          begin
                             if p^.right^.location.loc=LOC_CREGISTER then
                               begin
                                  if extra_not then
                                    emit_reg(A_NOT,opsize,p^.location.register);
{$ifndef noAllocEdi}
                                  getexplicitregister32(R_EDI);
{$endif noAllocEdi}
                                  emit_reg_reg(A_MOV,opsize,p^.right^.location.register,R_EDI);
                                  emit_reg_reg(op,opsize,p^.location.register,R_EDI);
                                  emit_reg_reg(A_MOV,opsize,R_EDI,p^.location.register);
{$ifndef noAllocEdi}
                                  ungetregister32(R_EDI);
{$endif noAllocEdi}
                               end
                             else
                               begin
                                  if extra_not then
                                    emit_reg(A_NOT,opsize,p^.location.register);

{$ifndef noAllocEdi}
                                  getexplicitregister32(R_EDI);
{$endif noAllocEdi}
                                  emit_ref_reg(A_MOV,opsize,
                                    newreference(p^.right^.location.reference),R_EDI);
                                  emit_reg_reg(op,opsize,p^.location.register,R_EDI);
                                  emit_reg_reg(A_MOV,opsize,R_EDI,p^.location.register);
{$ifndef noAllocEdi}
                                  ungetregister32(R_EDI);
{$endif noAllocEdi}
                                  ungetiftemp(p^.right^.location.reference);
                                  del_reference(p^.right^.location.reference);
                               end;
                          end
                        else
                          begin
                             if (p^.right^.treetype=ordconstn) and
                                (op=A_CMP) and
                                (p^.right^.value=0) then
                               begin
                                  emit_reg_reg(A_TEST,opsize,p^.location.register,
                                    p^.location.register);
                               end
                             else if (p^.right^.treetype=ordconstn) and
                                (op=A_ADD) and
                                (p^.right^.value=1) and
                                not(cs_check_overflow in aktlocalswitches) then
                               begin
                                  emit_reg(A_INC,opsize,
                                    p^.location.register);
                               end
                             else if (p^.right^.treetype=ordconstn) and
                                (op=A_SUB) and
                                (p^.right^.value=1) and
                                not(cs_check_overflow in aktlocalswitches) then
                               begin
                                  emit_reg(A_DEC,opsize,
                                    p^.location.register);
                               end
                             else if (p^.right^.treetype=ordconstn) and
                                (op=A_IMUL) and
                                (ispowerof2(p^.right^.value,power)) and
                                not(cs_check_overflow in aktlocalswitches) then
                               begin
                                  emit_const_reg(A_SHL,opsize,power,
                                    p^.location.register);
                               end
                             else
                               begin
                                  if (p^.right^.location.loc=LOC_CREGISTER) then
                                    begin
                                       if extra_not then
                                         begin
{$ifndef noAllocEdi}
                                            getexplicitregister32(R_EDI);
{$endif noAllocEdi}
                                            emit_reg_reg(A_MOV,S_L,p^.right^.location.register,R_EDI);
                                            emit_reg(A_NOT,S_L,R_EDI);
                                            emit_reg_reg(A_AND,S_L,R_EDI,
                                              p^.location.register);
{$ifndef noAllocEdi}
                                            ungetregister32(R_EDI);
{$endif noAllocEdi}
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
{$ifndef noAllocEdi}
                                            getexplicitregister32(R_EDI);
{$endif noAllocEdi}
                                            emit_ref_reg(A_MOV,S_L,newreference(
                                              p^.right^.location.reference),R_EDI);
                                            emit_reg(A_NOT,S_L,R_EDI);
                                            emit_reg_reg(A_AND,S_L,R_EDI,
                                              p^.location.register);
{$ifndef noAllocEdi}
                                            ungetregister32(R_EDI);
{$endif noAllocEdi}
                                         end
                                       else
                                         begin
                                            emit_ref_reg(op,opsize,newreference(
                                              p^.right^.location.reference),p^.location.register);
                                         end;
                                       ungetiftemp(p^.right^.location.reference);
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
                               emit_reg(A_NOT,S_L,p^.location.register);

                             emit_reg_reg(op,opsize,
                               p^.location.register,p^.right^.location.register);
                               swap_location(p^.location,p^.right^.location);
                               { newly swapped also set swapped flag }
                               { just to maintain ordering         }
                               p^.swaped:=not(p^.swaped);
                          end
                        else
                          begin
                             if extra_not then
                               emit_reg(A_NOT,S_L,p^.right^.location.register);
                             emit_reg_reg(op,opsize,
                               p^.right^.location.register,
                               p^.location.register);
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
                if ((p^.left^.resulttype^.deftype=orddef) and
                    (porddef(p^.left^.resulttype)^.typ=uchar)) or
              { enumeration type 16 bit }
                   ((p^.left^.resulttype^.deftype=enumdef) and
                    (p^.left^.resulttype^.size=1)) then
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
                             emit_ref_reg(A_MOV,S_B,newreference(p^.location.reference),
                               hregister);
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
                             emit_ref_reg(A_CMP,S_B,newreference(
                                p^.right^.location.reference),p^.location.register);
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
              else
              { 16 bit enumeration type }
                if ((p^.left^.resulttype^.deftype=enumdef) and
                    (p^.left^.resulttype^.size=2)) then
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
                                  hregister:=reg32toreg16(getregister32);
                                  emit_reg_reg(A_MOV,S_W,p^.location.register,
                                    hregister);
                               end;
                          end
                        else
                          begin
                             del_reference(p^.location.reference);

                             { first give free then demand new register }
                             hregister:=reg32toreg16(getregister32);
                             emit_ref_reg(A_MOV,S_W,newreference(p^.location.reference),
                               hregister);
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
                             emit_reg_reg(A_CMP,S_W,
                                p^.right^.location.register,p^.location.register);
                          end
                        else
                          begin
                             emit_ref_reg(A_CMP,S_W,newreference(
                                p^.right^.location.reference),p^.location.register);
                             del_reference(p^.right^.location.reference);
                          end;
                     end
                   else
                     begin
                        emit_reg_reg(A_CMP,S_W,p^.right^.location.register,
                          p^.location.register);
                        ungetregister32(reg16toreg32(p^.right^.location.register));
                     end;
                   ungetregister32(reg16toreg32(p^.location.register));
                end
              else
              { 64 bit types }
              if is_64bitint(p^.left^.resulttype) then
                begin
                   mboverflow:=false;
                   cmpop:=false;
                   unsigned:=((p^.left^.resulttype^.deftype=orddef) and
                       (porddef(p^.left^.resulttype)^.typ=u64bit)) or
                      ((p^.right^.resulttype^.deftype=orddef) and
                       (porddef(p^.right^.resulttype)^.typ=u64bit));
                   case p^.treetype of
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

                   if p^.treetype=muln then
                     begin
                        { save p^.lcoation, because we change it now }
                        set_location(hloc,p^.location);
                        release_qword_loc(p^.location);
                        release_qword_loc(p^.right^.location);
                        p^.location.registerlow:=getexplicitregister32(R_EAX);
                        p^.location.registerhigh:=getexplicitregister32(R_EDX);
                        pushusedregisters(pushedreg,$ff
                          and not($80 shr byte(p^.location.registerlow))
                          and not($80 shr byte(p^.location.registerhigh)));
                        if cs_check_overflow in aktlocalswitches then
                          push_int(1)
                        else
                          push_int(0);
                        { the left operand is in hloc, because the
                          location of left is p^.location but p^.location
                          is already destroyed
                        }
                        emit_pushq_loc(hloc);
                        clear_location(hloc);
                        emit_pushq_loc(p^.right^.location);
                        if porddef(p^.resulttype)^.typ=u64bit then
                          emitcall('FPC_MUL_QWORD')
                        else
                          emitcall('FPC_MUL_INT64');
                        emit_reg_reg(A_MOV,S_L,R_EAX,p^.location.registerlow);
                        emit_reg_reg(A_MOV,S_L,R_EDX,p^.location.registerhigh);
                        popusedregisters(pushedreg);
                        p^.location.loc:=LOC_REGISTER;
                     end
                   else
                     begin
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
                                       hregister:=p^.location.registerlow;
                                       hregister2:=p^.location.registerhigh;
                                       emit_reg_reg(A_MOV,S_L,p^.left^.location.registerlow,
                                         hregister);
                                       emit_reg_reg(A_MOV,S_L,p^.left^.location.registerlow,
                                         hregister2);
                                    end
                                  else
                                  if cmpop then
                                    begin
                                       { do not disturb the register }
                                       hregister:=p^.location.registerlow;
                                       hregister2:=p^.location.registerhigh;
                                    end
                                  else
                                    begin
                                       hregister:=getregister32;
                                       hregister2:=getregister32;
                                       emit_reg_reg(A_MOV,S_L,p^.left^.location.registerlow,
                                         hregister);
                                       emit_reg_reg(A_MOV,S_L,p^.left^.location.registerhigh,
                                         hregister2);
                                    end
                               end
                             else
                               begin
                                  ungetiftemp(p^.left^.location.reference);
                                  del_reference(p^.left^.location.reference);
                                  if is_in_dest then
                                    begin
                                       hregister:=p^.location.registerlow;
                                       hregister2:=p^.location.registerhigh;
                                       emit_mov_ref_reg64(p^.left^.location.reference,hregister,hregister2);
                                    end
                                  else
                                    begin
                                       hregister:=getregister32;
                                       hregister2:=getregister32;
                                       emit_mov_ref_reg64(p^.left^.location.reference,hregister,hregister2);
                                    end;
                               end;
                             clear_location(p^.location);
                             p^.location.loc:=LOC_REGISTER;
                             p^.location.registerlow:=hregister;
                             p^.location.registerhigh:=hregister2;
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
                        { containing the left result                        }

                        if p^.right^.location.loc<>LOC_REGISTER then
                          begin
                             if (p^.treetype=subn) and p^.swaped then
                               begin
                                  if p^.right^.location.loc=LOC_CREGISTER then
                                    begin
{$ifndef noAllocEdi}
                                       getexplicitregister32(R_EDI);
{$endif noAllocEdi}
                                       emit_reg_reg(A_MOV,opsize,p^.right^.location.register,R_EDI);
                                       emit_reg_reg(op,opsize,p^.location.register,R_EDI);
                                       emit_reg_reg(A_MOV,opsize,R_EDI,p^.location.register);
{$ifndef noAllocEdi}
                                       ungetregister32(R_EDI);
                                       getexplicitregister32(R_EDI);
{$endif noAllocEdi}
                                       emit_reg_reg(A_MOV,opsize,p^.right^.location.registerhigh,R_EDI);
                                       { the carry flag is still ok }
                                       emit_reg_reg(op2,opsize,p^.location.registerhigh,R_EDI);
                                       emit_reg_reg(A_MOV,opsize,R_EDI,p^.location.registerhigh);
{$ifndef noAllocEdi}
                                       ungetregister32(R_EDI);
{$endif noAllocEdi}
                                    end
                                  else
                                    begin
{$ifndef noAllocEdi}
                                       getexplicitregister32(R_EDI);
{$endif noAllocEdi}
                                       emit_ref_reg(A_MOV,opsize,
                                         newreference(p^.right^.location.reference),R_EDI);
                                       emit_reg_reg(op,opsize,p^.location.registerlow,R_EDI);
                                       emit_reg_reg(A_MOV,opsize,R_EDI,p^.location.registerlow);
{$ifndef noAllocEdi}
                                       ungetregister32(R_EDI);
                                       getexplicitregister32(R_EDI);
{$endif noAllocEdi}
                                       hr:=newreference(p^.right^.location.reference);
                                       inc(hr^.offset,4);
                                       emit_ref_reg(A_MOV,opsize,
                                         hr,R_EDI);
                                       { here the carry flag is still preserved }
                                       emit_reg_reg(op2,opsize,p^.location.registerhigh,R_EDI);
                                       emit_reg_reg(A_MOV,opsize,R_EDI,
                                         p^.location.registerhigh);
{$ifndef noAllocEdi}
                                       ungetregister32(R_EDI);
{$endif noAllocEdi}
                                       ungetiftemp(p^.right^.location.reference);
                                       del_reference(p^.right^.location.reference);
                                    end;
                               end
                             else if cmpop then
                               begin
                                  if (p^.right^.location.loc=LOC_CREGISTER) then
                                    begin
                                       emit_reg_reg(A_CMP,S_L,p^.right^.location.registerhigh,
                                          p^.location.registerhigh);
                                       firstjmp64bitcmp;
                                       emit_reg_reg(A_CMP,S_L,p^.right^.location.registerlow,
                                          p^.location.registerlow);
                                       secondjmp64bitcmp;
                                    end
                                  else
                                    begin
                                       hr:=newreference(p^.right^.location.reference);
                                       inc(hr^.offset,4);

                                       emit_ref_reg(A_CMP,S_L,
                                         hr,p^.location.registerhigh);
                                       firstjmp64bitcmp;

                                       emit_ref_reg(A_CMP,S_L,newreference(
                                         p^.right^.location.reference),p^.location.registerlow);
                                       secondjmp64bitcmp;

                                       emitjmp(C_None,falselabel);

                                       ungetiftemp(p^.right^.location.reference);
                                       del_reference(p^.right^.location.reference);
                                    end;
                               end
                             else
                               begin
                                  {
                                  if (p^.right^.treetype=ordconstn) and
                                     (op=A_CMP) and
                                     (p^.right^.value=0) then
                                    begin
                                       emit_reg_reg(A_TEST,opsize,p^.location.register,
                                         p^.location.register);
                                    end
                                  else if (p^.right^.treetype=ordconstn) and
                                     (op=A_IMUL) and
                                     (ispowerof2(p^.right^.value,power)) then
                                    begin
                                       emit_const_reg(A_SHL,opsize,power,
                                         p^.location.register);
                                    end
                                  else
                                  }
                                    begin
                                       if (p^.right^.location.loc=LOC_CREGISTER) then
                                         begin
                                            emit_reg_reg(op,S_L,p^.right^.location.registerlow,
                                               p^.location.registerlow);
                                            emit_reg_reg(op2,S_L,p^.right^.location.registerhigh,
                                               p^.location.registerhigh);
                                         end
                                       else
                                         begin
                                            emit_ref_reg(op,S_L,newreference(
                                              p^.right^.location.reference),p^.location.registerlow);
                                            hr:=newreference(p^.right^.location.reference);
                                            inc(hr^.offset,4);
                                            emit_ref_reg(op2,S_L,
                                              hr,p^.location.registerhigh);
                                            ungetiftemp(p^.right^.location.reference);
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
                                 emit_reg_reg(op,S_L,
                                    p^.location.registerlow,
                                    p^.right^.location.registerlow);
                                 emit_reg_reg(op2,S_L,
                                    p^.location.registerhigh,
                                    p^.right^.location.registerhigh);
                                  swap_location(p^.location,p^.right^.location);
                                  { newly swapped also set swapped flag }
                                  { just to maintain ordering           }
                                  p^.swaped:=not(p^.swaped);
                               end
                             else if cmpop then
                               begin
                                  emit_reg_reg(A_CMP,S_L,
                                    p^.right^.location.registerhigh,
                                    p^.location.registerhigh);
                                  firstjmp64bitcmp;
                                  emit_reg_reg(A_CMP,S_L,
                                    p^.right^.location.registerlow,
                                    p^.location.registerlow);
                                  secondjmp64bitcmp;
                               end
                             else
                               begin
                                  emit_reg_reg(op,S_L,
                                    p^.right^.location.registerlow,
                                    p^.location.registerlow);
                                  emit_reg_reg(op2,S_L,
                                    p^.right^.location.registerhigh,
                                    p^.location.registerhigh);
                               end;
                             ungetregister32(p^.right^.location.registerlow);
                             ungetregister32(p^.right^.location.registerhigh);
                          end;

                        if cmpop then
                          begin
                             ungetregister32(p^.location.registerlow);
                             ungetregister32(p^.location.registerhigh);
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
                             clear_location(p^.location);
                             p^.location.loc:=LOC_JUMP;
                             cmpop:=false;
                          end;
                     end;
                end
              else
              { Floating point }
               if (p^.left^.resulttype^.deftype=floatdef) and
                  (pfloatdef(p^.left^.resulttype)^.typ<>f32bit) then
                 begin
                    { real constants to the right, but only if it
                      isn't on the FPU stack, i.e. 1.0 or 0.0! }
                    if (p^.left^.treetype=realconstn) and
                      (p^.left^.location.loc<>LOC_FPU) then
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
                       else CGMessage(type_e_mismatch);
                    end;

                    if (p^.right^.location.loc<>LOC_FPU) then
                      begin
                         if p^.right^.location.loc=LOC_CFPUREGISTER then
                           begin
                              emit_reg( A_FLD,S_NO,
                                correct_fpuregister(p^.right^.location.register,fpuvaroffset));
                              inc(fpuvaroffset);
                            end
                         else
                           floatload(pfloatdef(p^.right^.resulttype)^.typ,p^.right^.location.reference);
                         if (p^.left^.location.loc<>LOC_FPU) then
                           begin
                              if p^.left^.location.loc=LOC_CFPUREGISTER then
                                begin
                                   emit_reg( A_FLD,S_NO,
                                     correct_fpuregister(p^.left^.location.register,fpuvaroffset));
                                   inc(fpuvaroffset);
                                end
                              else
                                floatload(pfloatdef(p^.left^.resulttype)^.typ,p^.left^.location.reference)
                           end
                         { left was on the stack => swap }
                         else
                           p^.swaped:=not(p^.swaped);

                         { releases the right reference }
                         del_reference(p^.right^.location.reference);
                      end
                    { the nominator in st0 }
                    else if (p^.left^.location.loc<>LOC_FPU) then
                      begin
                         if p^.left^.location.loc=LOC_CFPUREGISTER then
                           begin
                              emit_reg( A_FLD,S_NO,
                                correct_fpuregister(p^.left^.location.register,fpuvaroffset));
                              inc(fpuvaroffset);
                           end
                         else
                           floatload(pfloatdef(p^.left^.resulttype)^.typ,p^.left^.location.reference)
                      end
                    { fpu operands are always in the wrong order on the stack }
                    else
                      p^.swaped:=not(p^.swaped);

                    { releases the left reference }
                    if (p^.left^.location.loc in [LOC_MEM,LOC_REFERENCE]) then
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
{$ifndef noAllocEdi}
                           getexplicitregister32(R_EDI);
{$endif noAllocEdi}
                           emit_reg_reg(A_MOV,S_L,R_EAX,R_EDI);
                         end;
                       emit_reg(A_FNSTSW,S_NO,R_AX);
                       emit_none(A_SAHF,S_NO);
                       if not(R_EAX in unused) then
                         begin
                           emit_reg_reg(A_MOV,S_L,R_EDI,R_EAX);
{$ifndef noAllocEdi}
                           ungetregister32(R_EDI);
{$endif noAllocEdi}
                         end;
                       if p^.swaped then
                        begin
                          case p^.treetype of
                              equaln : flags:=F_E;
                            unequaln : flags:=F_NE;
                                 ltn : flags:=F_A;
                                lten : flags:=F_AE;
                                 gtn : flags:=F_B;
                                gten : flags:=F_BE;
                          end;
                        end
                       else
                        begin
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
                       cmpop:=false;
                     end
                    else
                     begin
                        clear_location(p^.location);
                        p^.location.loc:=LOC_FPU;
                     end;
                 end
{$ifdef SUPPORT_MMX}
               else

               { MMX Arrays }
                if is_mmx_able_array(p^.left^.resulttype) then
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
                      else CGMessage(type_e_mismatch);
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
                                  emit_ref_reg(A_MOVQ,S_NO,
                                    newreference(p^.left^.location.reference),hregister);
                               end
                             else
                               begin
                                  hregister:=getregistermmx;
                                  emit_ref_reg(A_MOVQ,S_NO,
                                    newreference(p^.left^.location.reference),hregister);
                               end;
                          end;
                        clear_location(p^.location);
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
                   { containing the left result                        }
                   if p^.right^.location.loc<>LOC_MMXREGISTER then
                     begin
                        if (p^.treetype=subn) and p^.swaped then
                          begin
                             if p^.right^.location.loc=LOC_CMMXREGISTER then
                               begin
                                  emit_reg_reg(A_MOVQ,S_NO,p^.right^.location.register,R_MM7);
                                  emit_reg_reg(op,S_NO,p^.location.register,R_MM0);
                                  emit_reg_reg(A_MOVQ,S_NO,R_MM7,p^.location.register);
                               end
                             else
                               begin
                                  emit_ref_reg(A_MOVQ,S_NO,
                                    newreference(p^.right^.location.reference),R_MM7);
                                  emit_reg_reg(op,S_NO,p^.location.register,
                                    R_MM7);
                                  emit_reg_reg(A_MOVQ,S_NO,
                                    R_MM7,p^.location.register);
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
                                  emit_ref_reg(op,S_NO,newreference(
                                    p^.right^.location.reference),p^.location.register);
                                  del_reference(p^.right^.location.reference);
                               end;
                          end;
                     end
                   else
                     begin
                        { when swapped another result register }
                        if (p^.treetype=subn) and p^.swaped then
                          begin
                             emit_reg_reg(op,S_NO,
                               p^.location.register,p^.right^.location.register);
                             swap_location(p^.location,p^.right^.location);
                             { newly swapped also set swapped flag }
                             { just to maintain ordering         }
                             p^.swaped:=not(p^.swaped);
                          end
                        else
                          begin
                             emit_reg_reg(op,S_NO,
                               p^.right^.location.register,
                               p^.location.register);
                          end;
                        ungetregistermmx(p^.right^.location.register);
                     end;
                end
{$endif SUPPORT_MMX}
              else CGMessage(type_e_mismatch);
           end;
       SetResultLocation(cmpop,unsigned,p);
    end;


end.
{
  $Log$
  Revision 1.7  2000-09-21 12:23:49  jonas
    * small fix to my changes for full boolean evaluation support (moved
      opsize determination for boolean operations back in boolean
      processing block)

  Revision 1.6  2000/09/21 11:30:49  jonas
    + support for full boolean evaluation (b+/b-), default remains short
      circuit boolean evaluation

  Revision 1.5  2000/08/27 16:11:49  peter
    * moved some util functions from globals,cobjects to cutils
    * splitted files into finput,fmodule

  Revision 1.4  2000/08/04 22:00:50  peter
    * merges from fixes

  Revision 1.3  2000/07/27 09:25:05  jonas
    * moved locflags2reg() procedure from cg386add to cgai386
    + added locjump2reg() procedure to cgai386
    * fixed internalerror(2002) when the result of a case expression has
      LOC_JUMP
    (all merged from fixes branch)

  Revision 1.2  2000/07/13 11:32:32  michael
  + removed logs

}
