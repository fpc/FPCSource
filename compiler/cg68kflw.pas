{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

    Generate m68k assembler for nodes that influence the flow

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
unit cg68kflw;
interface

    uses
      tree;

    procedure second_while_repeatn(var p : ptree);
    procedure secondifn(var p : ptree);
    procedure secondfor(var p : ptree);
    procedure secondexitn(var p : ptree);
    procedure secondbreakn(var p : ptree);
    procedure secondcontinuen(var p : ptree);
    procedure secondgoto(var p : ptree);
    procedure secondlabel(var p : ptree);
    procedure secondraise(var p : ptree);
    procedure secondtryexcept(var p : ptree);
    procedure secondtryfinally(var p : ptree);
    procedure secondon(var p : ptree);
    procedure secondfail(var p : ptree);


implementation

    uses
      globtype,systems,symconst,
      cobjects,verbose,globals,
      symtable,aasm,types,
      hcodegen,temp_gen,pass_2,
      cpubase,cga68k,tgen68k;

{*****************************************************************************
                         Second_While_RepeatN
*****************************************************************************}

    procedure second_while_repeatn(var p : ptree);

      var
         l1,l2,l3,oldclabel,oldblabel : pasmlabel;
         otlabel,oflabel : pasmlabel;
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


{*****************************************************************************
                               SecondIfN
*****************************************************************************}

    procedure secondifn(var p : ptree);

      var
         hl,otlabel,oflabel : pasmlabel;

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

{*****************************************************************************
                              SecondFor
*****************************************************************************}

    procedure secondfor(var p : ptree);

      var
         l1,l3,oldclabel,oldblabel : pasmlabel;
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
          CGMessage(cg_e_illegal_count_var);

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
                   exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOVE,opsize,p^.right^.location.register,
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
                   exprasmlist^.concat(new(paicpu,op_ref_reg(A_CMP,opsize,newreference(temp1),
                     p^.t2^.location.register)));
                end
              else
                begin
                   exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOVE,opsize,newreference(p^.t2^.location.reference),
                     cmpreg)));
                   exprasmlist^.concat(new(paicpu,op_ref_reg(A_CMP,opsize,newreference(temp1),
                     cmpreg)));
                end;
           end
         else
           begin
              if not(omitfirstcomp) then
                begin
                   if p^.t2^.location.loc=LOC_CREGISTER then
                     exprasmlist^.concat(new(paicpu,op_const_reg(A_CMP,opsize,p^.right^.value,
                       p^.t2^.location.register)))
                   else
                     exprasmlist^.concat(new(paicpu,op_const_ref(A_CMP,opsize,p^.right^.value,
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
                   exprasmlist^.concat(new(paicpu,op_ref_reg(A_CMP,opsize,newreference(temp1),
                     p^.t2^.location.register)));
                end
              else
                begin
                   exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOVE,opsize,newreference(p^.t2^.location.reference),
                     cmpreg)));
                   exprasmlist^.concat(new(paicpu,op_ref_reg(A_CMP,opsize,newreference(temp1),
                     cmpreg)));
                end;
           end
         else
           begin
              if p^.t2^.location.loc=LOC_CREGISTER then
                exprasmlist^.concat(new(paicpu,op_const_reg(A_CMP,opsize,p^.right^.value,
                  p^.t2^.location.register)))
              else
                exprasmlist^.concat(new(paicpu,op_const_ref(A_CMP,opsize,p^.right^.value,
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
           exprasmlist^.concat(new(paicpu,op_const_reg(hop,opsize,1,p^.t2^.location.register)))
         else
            exprasmlist^.concat(new(paicpu,op_const_ref(hop,opsize,1,newreference(p^.t2^.location.reference))));
         emitl(A_JMP,l3);

     { this is the break label: }
         emitl(A_LABEL,aktbreaklabel);
         ungetregister32(cmp32);

         if temptovalue then
           ungetiftemp(temp1);

         aktcontinuelabel:=oldclabel;
         aktbreaklabel:=oldblabel;
      end;


{*****************************************************************************
                              SecondExitN
*****************************************************************************}

    procedure secondexitn(var p : ptree);

      var
         is_mem : boolean;
         {op : tasmop;
         s : topsize;}
         otlabel,oflabel : pasmlabel;

      label
         do_jmp;

      begin
         if assigned(p^.left) then
           begin
              otlabel:=truelabel;
              oflabel:=falselabel;
              getlabel(truelabel);
              getlabel(falselabel);
              secondpass(p^.left);
              case p^.left^.location.loc of
                 LOC_FPU : goto do_jmp;
                 LOC_MEM,LOC_REFERENCE : is_mem:=true;
                 LOC_CREGISTER,
                 LOC_REGISTER : is_mem:=false;
                 LOC_FLAGS : begin
                                exprasmlist^.concat(new(paicpu,op_reg(flag_2_set[p^.right^.location.resflags],S_B,R_D0)));
                                exprasmlist^.concat(new(paicpu,op_reg(A_NEG, S_B, R_D0)));
                                goto do_jmp;
                             end;
                 LOC_JUMP : begin
                               emitl(A_LABEL,truelabel);
                               exprasmlist^.concat(new(paicpu,op_const_reg(A_MOVE,S_B,1,R_D0)));
                               emitl(A_JMP,aktexit2label);
                               exprasmlist^.concat(new(paicpu,op_reg(A_CLR,S_B,R_D0)));
                               goto do_jmp;
                            end;
                 else internalerror(2001);
              end;
              case procinfo^.retdef^.deftype of
               orddef,
              enumdef : begin
                          case procinfo^.retdef^.size of
                           4 : if is_mem then
                                 exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOVE,S_L,
                                   newreference(p^.left^.location.reference),R_D0)))
                               else
                                 emit_reg_reg(A_MOVE,S_L,p^.left^.location.register,R_D0);
                           2 : if is_mem then
                                 exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOVE,S_W,
                                   newreference(p^.left^.location.reference),R_D0)))
                               else
                                 emit_reg_reg(A_MOVE,S_W,p^.left^.location.register,R_D0);
                           1 : if is_mem then
                                 exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOVE,S_B,
                                   newreference(p^.left^.location.reference),R_D0)))
                               else
                                 emit_reg_reg(A_MOVE,S_B,p^.left^.location.register,R_D0);
                          end;
                        end;
           pointerdef,
           procvardef : begin
                          if is_mem then
                            exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOVE,S_L,
                              newreference(p^.left^.location.reference),R_D0)))
                          else
                            exprasmlist^.concat(new(paicpu,op_reg_reg(A_MOVE,S_L,p^.left^.location.register,R_D0)));
                        end;
             floatdef : begin
                          { floating point return values .... }
                          { single are returned in d0         }
                          if (pfloatdef(procinfo^.retdef)^.typ=f32bit) or
                             (pfloatdef(procinfo^.retdef)^.typ=s32real) then
                           begin
                             if is_mem then
                               exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOVE,S_L,
                                 newreference(p^.left^.location.reference),R_D0)))
                             else
                               begin
                                 if pfloatdef(procinfo^.retdef)^.typ=f32bit then
                                   emit_reg_reg(A_MOVE,S_L,p^.left^.location.register,R_D0)
                                 else
                                   begin
                                      { single values are in the floating point registers }
                                      if cs_fp_emulation in aktmoduleswitches then
                                         emit_reg_reg(A_MOVE,S_L,p^.left^.location.fpureg,R_D0)
                                      else
                                         exprasmlist^.concat(new(paicpu,op_reg_reg(A_FMOVE,S_FS,
                                            p^.left^.location.fpureg,R_D0)));
                                   end;
                               end;
                           end
                          else
                           Begin
                             { this is only possible in real non emulation mode }
                             { LOC_MEM,LOC_REFERENCE }
                             if is_mem then
                              begin
                                exprasmlist^.concat(new(paicpu,op_ref_reg(A_FMOVE,
                                  getfloatsize(pfloatdef(procinfo^.retdef)^.typ),
                                    newreference(p^.left^.location.reference),R_FP0)));
                              end
                             else
                             { LOC_FPU }
                              begin
                                { convert from extended to correct type }
                                { when storing                          }
                                exprasmlist^.concat(new(paicpu,op_reg_reg(A_FMOVE,
                                  getfloatsize(pfloatdef(procinfo^.retdef)^.typ),p^.left^.location.fpureg,R_FP0)));
                              end;
                           end;
                        end;
              end;
do_jmp:
              truelabel:=otlabel;
              falselabel:=oflabel;
              emitl(A_JMP,aktexit2label);
           end
         else
           begin
              emitl(A_JMP,aktexitlabel);
           end;
      end;


{*****************************************************************************
                              SecondBreakN
*****************************************************************************}

    procedure secondbreakn(var p : ptree);
      begin
         if aktbreaklabel<>nil then
           emitl(A_JMP,aktbreaklabel)
         else
           CGMessage(cg_e_break_not_allowed);
      end;


{*****************************************************************************
                              SecondContinueN
*****************************************************************************}

    procedure secondcontinuen(var p : ptree);
      begin
         if aktcontinuelabel<>nil then
           emitl(A_JMP,aktcontinuelabel)
         else
           CGMessage(cg_e_continue_not_allowed);
      end;


{*****************************************************************************
                             SecondGoto
*****************************************************************************}

    procedure secondgoto(var p : ptree);

       begin
         emitl(A_JMP,p^.labelnr);
       end;


{*****************************************************************************
                             SecondLabel
*****************************************************************************}

    procedure secondlabel(var p : ptree);
      begin
         emitl(A_LABEL,p^.labelnr);
         cleartempgen;
         secondpass(p^.left);
      end;


{*****************************************************************************
                             SecondRaise
*****************************************************************************}

    { generates the code for a raise statement }
    procedure secondraise(var p : ptree);

      var
         a : pasmlabel;

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
                   exprasmlist^.concat(new(paicpu,
                     op_csymbol_reg(A_MOVE,S_L,newcsymbol(a^.name,0),R_SPPUSH)));
                end;
              secondpass(p^.left);
              if codegenerror then
                exit;

              case p^.left^.location.loc of
                 LOC_MEM,LOC_REFERENCE : emitpushreferenceaddr(exprasmlist,p^.left^.location.reference);
                 LOC_CREGISTER,LOC_REGISTER : exprasmlist^.concat(new(paicpu,op_reg_reg(A_MOVE,S_L,
                   p^.left^.location.register,R_SPPUSH)));
                 else CGMessage(type_e_mismatch);
              end;
              emitcall('FPC_RAISEEXCEPTION',true);
             end
           else
            emitcall('FPC_RERAISE',true);
      end;


{*****************************************************************************
                             SecondTryExcept
*****************************************************************************}

    var
       endexceptlabel : pasmlabel;

    procedure secondtryexcept(var p : ptree);

      var
         exceptlabel,doexceptlabel,oldendexceptlabel,
         lastonlabel : pasmlabel;

      begin
        InternalError(3431243);
(*
         { this can be called recursivly }
         oldendexceptlabel:=endexceptlabel;
         { we modify EAX }
         usedinproc:=usedinproc or ($80 shr byte(R_EAX));

         getlabel(exceptlabel);
         getlabel(doexceptlabel);
         getlabel(endexceptlabel);
         getlabel(lastonlabel);
         push_int (1); { push type of exceptionframe }
         emitcall('FPC_PUSHEXCEPTADDR',true);
         exprasmlist^.concat(new(paicpu,
           op_reg(A_PUSH,S_L,R_EAX)));
         emitcall('FPC_SETJMP',true);
         exprasmlist^.concat(new(paicpu,
           op_reg(A_PUSH,S_L,R_EAX)));
         exprasmlist^.concat(new(paicpu,
           op_reg_reg(A_TEST,S_L,R_EAX,R_EAX)));
         emitl(A_JNE,exceptlabel);

         { try code }
         secondpass(p^.left);
         if codegenerror then
           exit;

         emitl(A_LABEL,exceptlabel);
         exprasmlist^.concat(new(paicpu,
           op_reg(A_POP,S_L,R_EAX)));
         exprasmlist^.concat(new(paicpu,
           op_reg_reg(A_TEST,S_L,R_EAX,R_EAX)));
         emitl(A_JNE,doexceptlabel);
         emitcall('FPC_POPADDRSTACK',true);
         emitl(A_JMP,endexceptlabel);
         emitl(A_LABEL,doexceptlabel);

         if assigned(p^.right) then
           secondpass(p^.right);

         emitl(A_LABEL,lastonlabel);
         { default handling }
         if assigned(p^.t1) then
           begin
              { FPC_CATCHES must be called with
                'default handler' flag (=-1)
              }
              push_int (-1);
              emitcall('FPC_CATCHES',true);
              secondpass(p^.t1);
           end
         else
           emitcall('FPC_RERAISE',true);
         emitl(A_LABEL,endexceptlabel);
         endexceptlabel:=oldendexceptlabel; *)
      end;


{*****************************************************************************
                             SecondOn
*****************************************************************************}

    procedure secondon(var p : ptree);
      var
         nextonlabel,myendexceptlabel : pasmlabel;
         ref : treference;

      begin
{ !!!!!!!!!!!!!!! }
(*         getlabel(nextonlabel);
         { push the vmt }
         exprasmlist^.concat(new(paicpu,op_csymbol(A_PUSH,S_L,
           newcsymbol(p^.excepttype^.vmt_mangledname,0))));
         maybe_concat_external(p^.excepttype^.owner,
           p^.excepttype^.vmt_mangledname);

         emitcall('FPC_CATCHES',true);
         exprasmlist^.concat(new(paicpu,
           op_reg_reg(A_TEST,S_L,R_EAX,R_EAX)));
         emitl(A_JE,nextonlabel);
         ref.symbol:=nil;
         gettempofsizereference(4,ref);

         { what a hack ! }
         if assigned(p^.exceptsymtable) then
           pvarsym(p^.exceptsymtable^.root)^.address:=ref.offset;

         exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,S_L,
           R_EAX,newreference(ref))));

         if assigned(p^.right) then
           secondpass(p^.right);
         { clear some stuff }
         ungetiftemp(ref);
         emitl(A_JMP,endexceptlabel);
         emitl(A_LABEL,nextonlabel);
         { next on node }
         if assigned(p^.left) then
           secondpass(p^.left); *)
      end;

{*****************************************************************************
                             SecondTryFinally
*****************************************************************************}

    procedure secondtryfinally(var p : ptree);

      var
         finallylabel,noreraiselabel,endfinallylabel : pasmlabel;

      begin
(*         { we modify EAX }
         usedinproc:=usedinproc or ($80 shr byte(R_EAX));

         getlabel(finallylabel);
         getlabel(noreraiselabel);
         getlabel(endfinallylabel);
         push_int(1); { Type of stack-frame must be pushed}
         emitcall('FPC_PUSHEXCEPTADDR',true);
         exprasmlist^.concat(new(paicpu,
           op_reg(A_PUSH,S_L,R_EAX)));
         emitcall('FPC_SETJMP',true);
         exprasmlist^.concat(new(paicpu,
           op_reg(A_PUSH,S_L,R_EAX)));
         exprasmlist^.concat(new(paicpu,
           op_reg_reg(A_TEST,S_L,R_EAX,R_EAX)));
         emitl(A_JNE,finallylabel);

         { try code }
         secondpass(p^.left);
         if codegenerror then
           exit;

         emitl(A_LABEL,finallylabel);

         { finally code }
         secondpass(p^.right);
         if codegenerror then
           exit;
         exprasmlist^.concat(new(paicpu,
           op_reg(A_POP,S_L,R_EAX)));
         exprasmlist^.concat(new(paicpu,
           op_reg_reg(A_TEST,S_L,R_EAX,R_EAX)));
         emitl(A_JE,noreraiselabel);
         emitcall('FPC_RERAISE',true);
         emitl(A_LABEL,noreraiselabel);
         emitcall('FPC_POPADDRSTACK',true);
         emitl(A_LABEL,endfinallylabel); *)
      end;


{*****************************************************************************
                             SecondFail
*****************************************************************************}

    procedure secondfail(var p : ptree);
      var
        hp : preference;
      begin
         exprasmlist^.concat(new(paicpu,op_reg(A_CLR,S_L,R_A5)));
         { also reset to zero in the stack }
         new(hp);
         reset_reference(hp^);
         hp^.offset:=procinfo^.selfpointer_offset;
         hp^.base:=procinfo^.framepointer;
         exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOVE,S_L,R_A5,hp)));
         exprasmlist^.concat(new(pai_labeled,init(A_JMP,quickexitlabel)));
      end;

end.
{
  $Log$
  Revision 1.15  2000-02-09 13:22:49  peter
    * log truncated

  Revision 1.14  2000/01/07 01:14:22  peter
    * updated copyright to 2000

  Revision 1.13  1999/12/22 01:01:47  peter
    - removed freelabel()
    * added undefined label detection in internal assembler, this prevents
      a lot of ld crashes and wrong .o files
    * .o files aren't written anymore if errors have occured
    * inlining of assembler labels is now correct

  Revision 1.12  1999/11/09 23:06:44  peter
    * esi_offset -> selfpointer_offset to be newcg compatible
    * hcogegen -> cgbase fixes for newcg

  Revision 1.11  1999/09/27 23:44:48  peter
    * procinfo is now a pointer
    * support for result setting in sub procedure

  Revision 1.10  1999/09/16 23:05:51  florian
    * m68k compiler is again compilable (only gas writer, no assembler reader)

  Revision 1.9  1999/08/25 11:59:49  jonas
    * changed pai386, paippc and paiapha (same for tai*) to paicpu (taicpu)

}

