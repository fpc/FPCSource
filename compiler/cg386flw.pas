{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

    Generate i386 assembler for nodes that influence the flow

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
unit cg386flw;
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

    type
       tenumflowcontrol = (fc_exit,fc_break,fc_continue);
       tflowcontrol = set of tenumflowcontrol;

    var
       flowcontrol : tflowcontrol;

implementation

    uses
      cobjects,verbose,globtype,globals,systems,
      symconst,symtable,aasm,types,
      hcodegen,temp_gen,pass_2,
      cpubase,cpuasm,
      cgai386,tgeni386,tcflw;

{*****************************************************************************
                         Second_While_RepeatN
*****************************************************************************}

    procedure second_while_repeatn(var p : ptree);
      var
         lcont,lbreak,lloop,
         oldclabel,oldblabel : pasmlabel;
         otlabel,oflabel : pasmlabel;

      begin
         getlabel(lloop);
         getlabel(lcont);
         getlabel(lbreak);
         { arrange continue and breaklabels: }
         oldclabel:=aktcontinuelabel;
         oldblabel:=aktbreaklabel;

         { handling code at the end as it is much more efficient, and makes
           while equal to repeat loop, only the end true/false is swapped (PFV) }
         if p^.treetype=whilen then
          emitjmp(C_None,lcont);

         emitlab(lloop);

         aktcontinuelabel:=lcont;
         aktbreaklabel:=lbreak;
         cleartempgen;
         if assigned(p^.right) then
           secondpass(p^.right);
         emitlab(lcont);
         otlabel:=truelabel;
         oflabel:=falselabel;
         if p^.treetype=whilen then
          begin
            truelabel:=lloop;
            falselabel:=lbreak;
          end
         { repeatn }
         else
          begin
            truelabel:=lbreak;
            falselabel:=lloop;
          end;
         cleartempgen;
         secondpass(p^.left);
         maketojumpbool(p^.left);
         emitlab(lbreak);
         truelabel:=otlabel;
         falselabel:=oflabel;

         aktcontinuelabel:=oldclabel;
         aktbreaklabel:=oldblabel;
         { a break/continue in a while/repeat block can't be seen outside }
         flowcontrol:=flowcontrol-[fc_break,fc_continue];
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
              emitlab(truelabel);
              cleartempgen;
              secondpass(p^.right);
           end;
         if assigned(p^.t1) then
           begin
              if assigned(p^.right) then
                begin
                   getlabel(hl);
                   { do go back to if line !! }
                   aktfilepos:=exprasmlist^.getlasttaifilepos^;
                   emitjmp(C_None,hl);
                end;
              emitlab(falselabel);
              cleartempgen;
              secondpass(p^.t1);
              if assigned(p^.right) then
                emitlab(hl);
           end
         else
           begin
              emitlab(falselabel);
           end;
         if not(assigned(p^.right)) then
           begin
              emitlab(truelabel);
           end;
         truelabel:=otlabel;
         falselabel:=oflabel;
      end;


{*****************************************************************************
                              SecondFor
*****************************************************************************}

    procedure secondfor(var p : ptree);
      var
         l3,oldclabel,oldblabel : pasmlabel;
         omitfirstcomp,temptovalue : boolean;
         hs : byte;
         temp1 : treference;
         hop : tasmop;
         hcond : tasmcond;
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
         hs:=p^.t2^.resulttype^.size;
         cmp32:=getregister32;
         case hs of
            1 : begin
                   opsize:=S_B;
                   cmpreg:=reg32toreg8(cmp32);
                end;
            2 : begin
                   opsize:=S_W;
                   cmpreg:=reg32toreg16(cmp32);
                end;
            4 : begin
                   opsize:=S_L;
                   cmpreg:=cmp32;
                end;
         end;

         { first set the to value
           because the count var can be in the expression !! }
         cleartempgen;
         secondpass(p^.right);
         { calculate pointer value and check if changeable and if so }
         { load into temporary variable                       }
         if p^.right^.treetype<>ordconstn then
           begin
              temp1.symbol:=nil;
              gettempofsizereference(hs,temp1);
              temptovalue:=true;
              if (p^.right^.location.loc=LOC_REGISTER) or
                 (p^.right^.location.loc=LOC_CREGISTER) then
                begin
                   emit_reg_ref(A_MOV,opsize,p^.right^.location.register,
                      newreference(temp1));
                 end
              else
                 concatcopy(p^.right^.location.reference,temp1,hs,false,false);
           end
         else
           temptovalue:=false;

         { produce start assignment }
         cleartempgen;
         secondpass(p^.left);
         count_var_is_signed:=is_signed(porddef(p^.t2^.resulttype));
         if temptovalue then
             begin
              if p^.t2^.location.loc=LOC_CREGISTER then
                begin
                   emit_ref_reg(A_CMP,opsize,newreference(temp1),
                     p^.t2^.location.register);
                end
              else
                begin
                   emit_ref_reg(A_MOV,opsize,newreference(p^.t2^.location.reference),
                     cmpreg);
                   emit_ref_reg(A_CMP,opsize,newreference(temp1),
                     cmpreg);
                end;
           end
         else
             begin
              if not(omitfirstcomp) then
                begin
                   if p^.t2^.location.loc=LOC_CREGISTER then
                     emit_const_reg(A_CMP,opsize,p^.right^.value,
                       p^.t2^.location.register)
                   else
                     emit_const_ref(A_CMP,opsize,p^.right^.value,
                       newreference(p^.t2^.location.reference));
                end;
           end;
         if p^.backward then
           if count_var_is_signed then
             hcond:=C_L
           else
             hcond:=C_B
         else
           if count_var_is_signed then
             hcond:=C_G
           else
             hcond:=C_A;

         if not(omitfirstcomp) or temptovalue then
           emitjmp(hcond,aktbreaklabel);

         { align loop target }
         if not(cs_littlesize in aktglobalswitches) then
           exprasmlist^.concat(new(pai_align,init_op(4,$90)));

         emitlab(l3);

         { help register must not be in instruction block }
         cleartempgen;
         if assigned(p^.t1) then
           secondpass(p^.t1);

         emitlab(aktcontinuelabel);

         { makes no problems there }
         cleartempgen;

         { demand help register again }
         cmp32:=getregister32;
         case hs of
            1 : begin
                   opsize:=S_B;
                   cmpreg:=reg32toreg8(cmp32);
                end;
            2 : begin
                   opsize:=S_W;
                   cmpreg:=reg32toreg16(cmp32);
                end;
            4 : opsize:=S_L;
         end;

         { produce comparison and the corresponding }
         { jump                              }
         if temptovalue then
           begin
              if p^.t2^.location.loc=LOC_CREGISTER then
                begin
                   emit_ref_reg(A_CMP,opsize,newreference(temp1),
                     p^.t2^.location.register);
                end
              else
                begin
                   emit_ref_reg(A_MOV,opsize,newreference(p^.t2^.location.reference),
                     cmpreg);
                   emit_ref_reg(A_CMP,opsize,newreference(temp1),
                     cmpreg);
                    end;
           end
         else
           begin
              if p^.t2^.location.loc=LOC_CREGISTER then
                emit_const_reg(A_CMP,opsize,p^.right^.value,
                  p^.t2^.location.register)
              else
                 emit_const_ref(A_CMP,opsize,p^.right^.value,
                   newreference(p^.t2^.location.reference));
           end;
         if p^.backward then
           if count_var_is_signed then
             hcond:=C_LE
           else
             hcond:=C_BE
          else
            if count_var_is_signed then
              hcond:=C_GE
            else
              hcond:=C_AE;
         emitjmp(hcond,aktbreaklabel);
         { according to count direction DEC or INC... }
         { must be after the test because of 0to 255 for bytes !! }
         if p^.backward then
           hop:=A_DEC
         else
           hop:=A_INC;

         if p^.t2^.location.loc=LOC_CREGISTER then
           emit_reg(hop,opsize,p^.t2^.location.register)
         else
           emit_ref(hop,opsize,newreference(p^.t2^.location.reference));
         emitjmp(C_None,l3);

         { this is the break label: }
         ungetregister32(cmp32);
         if temptovalue then
           ungetiftemp(temp1);

         emitlab(aktbreaklabel);

         aktcontinuelabel:=oldclabel;
         aktbreaklabel:=oldblabel;
         { a break/continue in a for block can't be seen outside }
         flowcontrol:=flowcontrol-[fc_break,fc_continue];
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
         include(flowcontrol,fc_exit);
         if assigned(p^.left) then
         if p^.left^.treetype=assignn then
           begin
              { just do a normal assignment followed by exit }
              secondpass(p^.left);
              emitjmp(C_None,aktexitlabel);
           end
         else
           begin
              otlabel:=truelabel;
              oflabel:=falselabel;
              getlabel(truelabel);
              getlabel(falselabel);
              secondpass(p^.left);
              case p^.left^.location.loc of
                 LOC_FPU : goto do_jmp;
                 LOC_MEM,
           LOC_REFERENCE : is_mem:=true;
           LOC_CREGISTER,
            LOC_REGISTER : is_mem:=false;
               LOC_FLAGS : begin
                             emit_flag2reg(p^.left^.location.resflags,R_AL);
                             goto do_jmp;
                           end;
                LOC_JUMP : begin
                             emitlab(truelabel);
                             emit_const_reg(A_MOV,S_B,1,R_AL);
                             emitjmp(C_None,aktexit2label);
                             emitlab(falselabel);
                             emit_reg_reg(A_XOR,S_B,R_AL,R_AL);
                             goto do_jmp;
                           end;
              else
                internalerror(2001);
              end;
              case procinfo^.returntype.def^.deftype of
           pointerdef,
           procvardef : begin
                          if is_mem then
                            emit_ref_reg(A_MOV,S_L,
                              newreference(p^.left^.location.reference),R_EAX)
                          else
                            emit_reg_reg(A_MOV,S_L,
                              p^.left^.location.register,R_EAX);
                        end;
             floatdef : begin
                          if pfloatdef(procinfo^.returntype.def)^.typ=f32bit then
                           begin
                             if is_mem then
                               emit_ref_reg(A_MOV,S_L,
                                 newreference(p^.left^.location.reference),R_EAX)
                             else
                               emit_reg_reg(A_MOV,S_L,p^.left^.location.register,R_EAX);
                           end
                          else
                           if is_mem then
                            floatload(pfloatdef(procinfo^.returntype.def)^.typ,p^.left^.location.reference);
                        end;
              { orddef,
              enumdef : }
              else
              { it can be anything shorter than 4 bytes PM
              this caused form bug 711 }
                       begin
                          case procinfo^.returntype.def^.size of
                          { if its 3 bytes only we can still
                            copy one of garbage ! PM }
                           4,3 : if is_mem then
                                 emit_ref_reg(A_MOV,S_L,
                                   newreference(p^.left^.location.reference),R_EAX)
                               else
                                 emit_reg_reg(A_MOV,S_L,p^.left^.location.register,R_EAX);
                           2 : if is_mem then
                                 emit_ref_reg(A_MOV,S_W,
                                   newreference(p^.left^.location.reference),R_AX)
                               else
                                 emit_reg_reg(A_MOV,S_W,makereg16(p^.left^.location.register),R_AX);
                           1 : if is_mem then
                                 emit_ref_reg(A_MOV,S_B,
                                   newreference(p^.left^.location.reference),R_AL)
                               else
                                 emit_reg_reg(A_MOV,S_B,makereg8(p^.left^.location.register),R_AL);
                          end;
                        end;
              end;
do_jmp:
              truelabel:=otlabel;
              falselabel:=oflabel;
              emitjmp(C_None,aktexit2label);
           end
         else
           begin
              emitjmp(C_None,aktexitlabel);
           end;
       end;


{*****************************************************************************
                              SecondBreakN
*****************************************************************************}

    procedure secondbreakn(var p : ptree);
      begin
         include(flowcontrol,fc_break);
         if aktbreaklabel<>nil then
           emitjmp(C_None,aktbreaklabel)
         else
           CGMessage(cg_e_break_not_allowed);
      end;


{*****************************************************************************
                              SecondContinueN
*****************************************************************************}

    procedure secondcontinuen(var p : ptree);
      begin
         include(flowcontrol,fc_continue);
         if aktcontinuelabel<>nil then
           emitjmp(C_None,aktcontinuelabel)
         else
           CGMessage(cg_e_continue_not_allowed);
      end;


{*****************************************************************************
                             SecondGoto
*****************************************************************************}

    procedure secondgoto(var p : ptree);

       begin
         emitjmp(C_None,p^.labelnr);
         { the assigned avoids only crashes if the label isn't defined }
         if assigned(p^.labsym) and
           assigned(p^.labsym^.code) and
            (aktexceptblock<>ptree(p^.labsym^.code)^.exceptionblock) then
           CGMessage(cg_e_goto_inout_of_exception_block);
       end;


{*****************************************************************************
                             SecondLabel
*****************************************************************************}

    procedure secondlabel(var p : ptree);
      begin
         emitlab(p^.labelnr);
         cleartempgen;
         secondpass(p^.left);
      end;


{*****************************************************************************
                             SecondRaise
*****************************************************************************}

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
                   emit_push_loc(p^.right^.location);
                end
              else
                begin
                   getlabel(a);
                   emitlab(a);
                   emit_sym(A_PUSH,S_L,a);
                end;
              secondpass(p^.left);
              if codegenerror then
                exit;

              case p^.left^.location.loc of
                 LOC_MEM,LOC_REFERENCE:
                   emit_ref(A_PUSH,S_L,
                       newreference(p^.left^.location.reference));
                 LOC_CREGISTER,LOC_REGISTER : emit_reg(A_PUSH,S_L,
                       p^.left^.location.register);
                 else CGMessage(type_e_mismatch);
              end;
              emitcall('FPC_RAISEEXCEPTION');
             end
           else
             begin
                emitcall('FPC_POPADDRSTACK');
                emitcall('FPC_RERAISE');
             end;
       end;


{*****************************************************************************
                             SecondTryExcept
*****************************************************************************}

    var
       endexceptlabel : pasmlabel;

    { does the necessary things to clean up the object stack }
    { in the except block                                    }
    procedure cleanupobjectstack;

      begin
         emitcall('FPC_POPOBJECTSTACK');
         exprasmlist^.concat(new(pairegalloc,alloc(R_EAX)));
         emit_reg(A_PUSH,S_L,R_EAX);
         emitcall('FPC_DESTROYEXCEPTION');
         exprasmlist^.concat(new(pairegalloc,dealloc(R_EAX)));
         maybe_loadesi;
      end;

    { pops one element from the exception address stack }
    { and removes the flag                              }
    procedure cleanupaddrstack;

      begin
         emitcall('FPC_POPADDRSTACK');
         { allocate eax }
         exprasmlist^.concat(new(pairegalloc,alloc(R_EAX)));
         emit_reg(A_POP,S_L,R_EAX);
         { deallocate eax }
         exprasmlist^.concat(new(pairegalloc,dealloc(R_EAX)));
      end;

    procedure secondtryexcept(var p : ptree);

      var
         exceptlabel,doexceptlabel,oldendexceptlabel,
         lastonlabel,
         exitexceptlabel,
         continueexceptlabel,
         breakexceptlabel,
         exittrylabel,
         continuetrylabel,
         breaktrylabel,
         doobjectdestroy,
         doobjectdestroyandreraise,
         oldaktexitlabel,
         oldaktexit2label,
         oldaktcontinuelabel,
         oldaktbreaklabel : pasmlabel;
         oldexceptblock : ptree;


         oldflowcontrol,tryflowcontrol,
         exceptflowcontrol : tflowcontrol;

      begin
         oldflowcontrol:=flowcontrol;
         flowcontrol:=[];
         { this can be called recursivly }
         oldendexceptlabel:=endexceptlabel;

         { we modify EAX }
         usedinproc:=usedinproc or ($80 shr byte(R_EAX));

         { save the old labels for control flow statements }
         oldaktexitlabel:=aktexitlabel;
         oldaktexit2label:=aktexit2label;
         if assigned(aktbreaklabel) then
           begin
              oldaktcontinuelabel:=aktcontinuelabel;
              oldaktbreaklabel:=aktbreaklabel;
           end;

         { get new labels for the control flow statements }
         getlabel(exittrylabel);
         getlabel(exitexceptlabel);
         if assigned(aktbreaklabel) then
           begin
              getlabel(breaktrylabel);
              getlabel(continuetrylabel);
              getlabel(breakexceptlabel);
              getlabel(continueexceptlabel);
           end;

         getlabel(exceptlabel);
         getlabel(doexceptlabel);
         getlabel(endexceptlabel);
         getlabel(lastonlabel);
         push_int (1); { push type of exceptionframe }
         emitcall('FPC_PUSHEXCEPTADDR');
         { allocate eax }
         exprasmlist^.concat(new(pairegalloc,alloc(R_EAX)));
         emit_reg(A_PUSH,S_L,R_EAX);
         emitcall('FPC_SETJMP');
         emit_reg(A_PUSH,S_L,R_EAX);
         emit_reg_reg(A_TEST,S_L,R_EAX,R_EAX);
         { deallocate eax }
         exprasmlist^.concat(new(pairegalloc,dealloc(R_EAX)));
         emitjmp(C_NE,exceptlabel);

         { try block }
         { set control flow labels for the try block }
         aktexitlabel:=exittrylabel;
         aktexit2label:=exittrylabel;
         if assigned(oldaktbreaklabel) then
          begin
            aktcontinuelabel:=continuetrylabel;
            aktbreaklabel:=breaktrylabel;
          end;

         oldexceptblock:=aktexceptblock;
         aktexceptblock:=p^.left;
         flowcontrol:=[];
         secondpass(p^.left);
         tryflowcontrol:=flowcontrol;
         aktexceptblock:=oldexceptblock;
         if codegenerror then
           exit;

         emitlab(exceptlabel);
         emitcall('FPC_POPADDRSTACK');

         exprasmlist^.concat(new(pairegalloc,alloc(R_EAX)));
         emit_reg(A_POP,S_L,R_EAX);
         emit_reg_reg(A_TEST,S_L,R_EAX,R_EAX);
         exprasmlist^.concat(new(pairegalloc,dealloc(R_EAX)));

         emitjmp(C_E,endexceptlabel);
         emitlab(doexceptlabel);

         { set control flow labels for the except block }
         { and the on statements                        }
         aktexitlabel:=exitexceptlabel;
         aktexit2label:=exitexceptlabel;
         if assigned(oldaktbreaklabel) then
          begin
            aktcontinuelabel:=continueexceptlabel;
            aktbreaklabel:=breakexceptlabel;
          end;

         flowcontrol:=[];
         { on statements }
         if assigned(p^.right) then
           begin
              oldexceptblock:=aktexceptblock;
              aktexceptblock:=p^.right;
              secondpass(p^.right);
              aktexceptblock:=oldexceptblock;
           end;

         emitlab(lastonlabel);
         { default handling except handling }
         if assigned(p^.t1) then
           begin
              { FPC_CATCHES must be called with
                'default handler' flag (=-1)
              }
              push_int (-1);
              emitcall('FPC_CATCHES');
              maybe_loadesi;

              { the destruction of the exception object must be also }
              { guarded by an exception frame                        }
              getlabel(doobjectdestroy);
              getlabel(doobjectdestroyandreraise);
              exprasmlist^.concat(new(paicpu,op_const(A_PUSH,S_L,1)));
              emitcall('FPC_PUSHEXCEPTADDR');
              exprasmlist^.concat(new(pairegalloc,alloc(R_EAX)));
              exprasmlist^.concat(new(paicpu,
                op_reg(A_PUSH,S_L,R_EAX)));
              exprasmlist^.concat(new(pairegalloc,dealloc(R_EAX)));
              emitcall('FPC_SETJMP');
              exprasmlist^.concat(new(pairegalloc,alloc(R_EAX)));
              exprasmlist^.concat(new(paicpu,
                op_reg(A_PUSH,S_L,R_EAX)));
              exprasmlist^.concat(new(paicpu,
                op_reg_reg(A_TEST,S_L,R_EAX,R_EAX)));
              exprasmlist^.concat(new(pairegalloc,dealloc(R_EAX)));
              emitjmp(C_NE,doobjectdestroyandreraise);

              oldexceptblock:=aktexceptblock;
              aktexceptblock:=p^.t1;
              { here we don't have to reset flowcontrol           }
              { the default and on flowcontrols are handled equal }
              secondpass(p^.t1);
              exceptflowcontrol:=flowcontrol;
              aktexceptblock:=oldexceptblock;

              emitlab(doobjectdestroyandreraise);
              emitcall('FPC_POPADDRSTACK');
              exprasmlist^.concat(new(pairegalloc,alloc(R_EAX)));
              exprasmlist^.concat(new(paicpu,
                op_reg(A_POP,S_L,R_EAX)));
              exprasmlist^.concat(new(paicpu,
                op_reg_reg(A_TEST,S_L,R_EAX,R_EAX)));
              exprasmlist^.concat(new(pairegalloc,dealloc(R_EAX)));
              emitjmp(C_E,doobjectdestroy);
              emitcall('FPC_POPSECONDOBJECTSTACK');
              exprasmlist^.concat(new(pairegalloc,alloc(R_EAX)));
              emit_reg(A_PUSH,S_L,R_EAX);
              emitcall('FPC_DESTROYEXCEPTION');
              exprasmlist^.concat(new(pairegalloc,dealloc(R_EAX)));
              { we don't need to restore esi here because reraise never }
              { returns                                                 }
              emitcall('FPC_RERAISE');

              emitlab(doobjectdestroy);
              cleanupobjectstack;
              emitjmp(C_None,endexceptlabel);
           end
         else
           begin
              emitcall('FPC_RERAISE');
              exceptflowcontrol:=flowcontrol;
           end;

         if fc_exit in exceptflowcontrol then
           begin
              { do some magic for exit in the try block }
              emitlab(exitexceptlabel);
              { we must also destroy the address frame which guards }
              { exception object                                    }
              cleanupaddrstack;
              cleanupobjectstack;
              emitjmp(C_None,oldaktexitlabel);
           end;

         if fc_break in exceptflowcontrol then
           begin
              emitlab(breakexceptlabel);
              { we must also destroy the address frame which guards }
              { exception object                                    }
              cleanupaddrstack;
              cleanupobjectstack;
              emitjmp(C_None,oldaktbreaklabel);
           end;

         if fc_continue in exceptflowcontrol then
           begin
              emitlab(continueexceptlabel);
              { we must also destroy the address frame which guards }
              { exception object                                    }
              cleanupaddrstack;
              cleanupobjectstack;
              emitjmp(C_None,oldaktcontinuelabel);
           end;

         if fc_exit in tryflowcontrol then
           begin
              { do some magic for exit in the try block }
              emitlab(exittrylabel);
              cleanupaddrstack;
              emitjmp(C_None,oldaktexitlabel);
           end;

         if fc_break in tryflowcontrol then
           begin
              emitlab(breaktrylabel);
              cleanupaddrstack;
              emitjmp(C_None,oldaktbreaklabel);
           end;

         if fc_continue in tryflowcontrol then
           begin
              emitlab(continuetrylabel);
              cleanupaddrstack;
              emitjmp(C_None,oldaktcontinuelabel);
           end;

         emitlab(endexceptlabel);

         endexceptlabel:=oldendexceptlabel;

         { restore the control flow labels }
         aktexitlabel:=oldaktexitlabel;
         aktexit2label:=oldaktexit2label;
         if assigned(oldaktbreaklabel) then
          begin
            aktcontinuelabel:=oldaktcontinuelabel;
            aktbreaklabel:=oldaktbreaklabel;
          end;

         { return all used control flow statements }
         flowcontrol:=oldflowcontrol+exceptflowcontrol+
           tryflowcontrol;
      end;

    procedure secondon(var p : ptree);

      var
         nextonlabel,
         exitonlabel,
         continueonlabel,
         breakonlabel,
         oldaktexitlabel,
         oldaktexit2label,
         oldaktcontinuelabel,
         doobjectdestroyandreraise,
         doobjectdestroy,
         oldaktbreaklabel : pasmlabel;
         ref : treference;
         oldexceptblock : ptree;
         oldflowcontrol : tflowcontrol;

      begin
         oldflowcontrol:=flowcontrol;
         flowcontrol:=[];
         getlabel(nextonlabel);

         { push the vmt }
         emit_sym(A_PUSH,S_L,
           newasmsymbol(p^.excepttype^.vmt_mangledname));
         emitcall('FPC_CATCHES');
         { allocate eax }
         exprasmlist^.concat(new(pairegalloc,alloc(R_EAX)));
         emit_reg_reg(A_TEST,S_L,R_EAX,R_EAX);
         emitjmp(C_E,nextonlabel);
         ref.symbol:=nil;
         gettempofsizereference(4,ref);

         { what a hack ! }
         if assigned(p^.exceptsymtable) then
           pvarsym(p^.exceptsymtable^.symindex^.first)^.address:=ref.offset;

         emit_reg_ref(A_MOV,S_L,
           R_EAX,newreference(ref));
         { deallocate eax }
         exprasmlist^.concat(new(pairegalloc,dealloc(R_EAX)));

         { in the case that another exception is risen }
         { we've to destroy the old one                }
         getlabel(doobjectdestroyandreraise);
         exprasmlist^.concat(new(paicpu,op_const(A_PUSH,S_L,1)));
         emitcall('FPC_PUSHEXCEPTADDR');
         exprasmlist^.concat(new(pairegalloc,alloc(R_EAX)));
         exprasmlist^.concat(new(paicpu,
           op_reg(A_PUSH,S_L,R_EAX)));
         exprasmlist^.concat(new(pairegalloc,dealloc(R_EAX)));
         emitcall('FPC_SETJMP');
         exprasmlist^.concat(new(pairegalloc,alloc(R_EAX)));
         exprasmlist^.concat(new(paicpu,
           op_reg(A_PUSH,S_L,R_EAX)));
         exprasmlist^.concat(new(paicpu,
           op_reg_reg(A_TEST,S_L,R_EAX,R_EAX)));
         exprasmlist^.concat(new(pairegalloc,dealloc(R_EAX)));
         emitjmp(C_NE,doobjectdestroyandreraise);

         if assigned(p^.right) then
           begin
              oldaktexitlabel:=aktexitlabel;
              oldaktexit2label:=aktexit2label;
              getlabel(exitonlabel);
              aktexitlabel:=exitonlabel;
              aktexit2label:=exitonlabel;
              if assigned(aktbreaklabel) then
               begin
                 oldaktcontinuelabel:=aktcontinuelabel;
                 oldaktbreaklabel:=aktbreaklabel;
                 getlabel(breakonlabel);
                 getlabel(continueonlabel);
                 aktcontinuelabel:=continueonlabel;
                 aktbreaklabel:=breakonlabel;
               end;

              { esi is destroyed by FPC_CATCHES }
              maybe_loadesi;
              oldexceptblock:=aktexceptblock;
              aktexceptblock:=p^.right;
              secondpass(p^.right);
              aktexceptblock:=oldexceptblock;
           end;
         getlabel(doobjectdestroy);
         emitlab(doobjectdestroyandreraise);
         emitcall('FPC_POPADDRSTACK');
         exprasmlist^.concat(new(pairegalloc,alloc(R_EAX)));
         exprasmlist^.concat(new(paicpu,
           op_reg(A_POP,S_L,R_EAX)));
         exprasmlist^.concat(new(paicpu,
           op_reg_reg(A_TEST,S_L,R_EAX,R_EAX)));
         exprasmlist^.concat(new(pairegalloc,dealloc(R_EAX)));
         emitjmp(C_E,doobjectdestroy);
         emitcall('FPC_POPSECONDOBJECTSTACK');
         exprasmlist^.concat(new(pairegalloc,alloc(R_EAX)));
         emit_reg(A_PUSH,S_L,R_EAX);
         emitcall('FPC_DESTROYEXCEPTION');
         exprasmlist^.concat(new(pairegalloc,dealloc(R_EAX)));
         { we don't need to restore esi here because reraise never }
         { returns                                                 }
         emitcall('FPC_RERAISE');

         emitlab(doobjectdestroy);
         cleanupobjectstack;
         { clear some stuff }
         ungetiftemp(ref);
         emitjmp(C_None,endexceptlabel);

         if assigned(p^.right) then
           begin
              { special handling for control flow instructions }
              if fc_exit in flowcontrol then
                begin
                   { the address and object pop does secondtryexcept }
                   emitlab(exitonlabel);
                   emitjmp(C_None,oldaktexitlabel);
                end;

              if fc_break in flowcontrol then
                begin
                   { the address and object pop does secondtryexcept }
                   emitlab(breakonlabel);
                   emitjmp(C_None,oldaktbreaklabel);
                end;

              if fc_continue in flowcontrol then
                begin
                   { the address and object pop does secondtryexcept }
                   emitlab(continueonlabel);
                   emitjmp(C_None,oldaktcontinuelabel);
                end;

              aktexitlabel:=oldaktexitlabel;
              aktexit2label:=oldaktexit2label;
              if assigned(oldaktbreaklabel) then
               begin
                 aktcontinuelabel:=oldaktcontinuelabel;
                 aktbreaklabel:=oldaktbreaklabel;
               end;
           end;

         emitlab(nextonlabel);
         flowcontrol:=oldflowcontrol+flowcontrol;
         { next on node }
         if assigned(p^.left) then
           secondpass(p^.left);
      end;

{*****************************************************************************
                             SecondTryFinally
*****************************************************************************}

    procedure secondtryfinally(var p : ptree);

      var
         reraiselabel,
         finallylabel,
         endfinallylabel,
         exitfinallylabel,
         continuefinallylabel,
         breakfinallylabel,
         oldaktexitlabel,
         oldaktexit2label,
         oldaktcontinuelabel,
         oldaktbreaklabel : pasmlabel;
         oldexceptblock : ptree;
         oldflowcontrol,tryflowcontrol : tflowcontrol;
         decconst : longint;

      begin
         { check if child nodes do a break/continue/exit }
         oldflowcontrol:=flowcontrol;
         flowcontrol:=[];
         { we modify EAX }
         usedinproc:=usedinproc or ($80 shr byte(R_EAX));
         getlabel(finallylabel);
         getlabel(endfinallylabel);
         getlabel(reraiselabel);

         { the finally block must catch break, continue and exit }
         { statements                                            }
         oldaktexitlabel:=aktexitlabel;
         oldaktexit2label:=aktexit2label;
         getlabel(exitfinallylabel);
         aktexitlabel:=exitfinallylabel;
         aktexit2label:=exitfinallylabel;
         if assigned(aktbreaklabel) then
          begin
            oldaktcontinuelabel:=aktcontinuelabel;
            oldaktbreaklabel:=aktbreaklabel;
            getlabel(breakfinallylabel);
            getlabel(continuefinallylabel);
            aktcontinuelabel:=continuefinallylabel;
            aktbreaklabel:=breakfinallylabel;
          end;

         push_int(1); { Type of stack-frame must be pushed}
         emitcall('FPC_PUSHEXCEPTADDR');
         { allocate eax }
         exprasmlist^.concat(new(pairegalloc,alloc(R_EAX)));
         emit_reg(A_PUSH,S_L,R_EAX);
         emitcall('FPC_SETJMP');
         emit_reg(A_PUSH,S_L,R_EAX);
         emit_reg_reg(A_TEST,S_L,R_EAX,R_EAX);
         { deallocate eax }
         exprasmlist^.concat(new(pairegalloc,dealloc(R_EAX)));
         emitjmp(C_NE,finallylabel);

         { try code }
         if assigned(p^.left) then
           begin
              oldexceptblock:=aktexceptblock;
              aktexceptblock:=p^.left;
              secondpass(p^.left);
              tryflowcontrol:=flowcontrol;
              if codegenerror then
                exit;
              aktexceptblock:=oldexceptblock;
           end;

         emitlab(finallylabel);
         emitcall('FPC_POPADDRSTACK');
         { finally code }
         oldexceptblock:=aktexceptblock;
         aktexceptblock:=p^.right;
         flowcontrol:=[];
         secondpass(p^.right);
         if flowcontrol<>[] then
           CGMessage(cg_e_control_flow_outside_finally);
         aktexceptblock:=oldexceptblock;
         if codegenerror then
           exit;
         { allocate eax }
         exprasmlist^.concat(new(pairegalloc,alloc(R_EAX)));
         emit_reg(A_POP,S_L,R_EAX);
         emit_reg_reg(A_TEST,S_L,R_EAX,R_EAX);
         emitjmp(C_E,endfinallylabel);
         emit_reg(A_DEC,S_L,R_EAX);
         emitjmp(C_Z,reraiselabel);
         if fc_exit in tryflowcontrol then
           begin
              emit_reg(A_DEC,S_L,R_EAX);
              emitjmp(C_Z,oldaktexitlabel);
              decconst:=1;
           end
         else
           decconst:=2;
         if fc_break in tryflowcontrol then
           begin
              emit_const_reg(A_SUB,S_L,decconst,R_EAX);
              emitjmp(C_Z,oldaktbreaklabel);
              decconst:=1;
           end
         else
           inc(decconst);
         if fc_continue in tryflowcontrol then
           begin
              emit_const_reg(A_SUB,S_L,decconst,R_EAX);
              emitjmp(C_Z,oldaktcontinuelabel);
           end;
         { deallocate eax }
         exprasmlist^.concat(new(pairegalloc,dealloc(R_EAX)));
         emitlab(reraiselabel);
         emitcall('FPC_RERAISE');
         { do some magic for exit,break,continue in the try block }
         if fc_exit in tryflowcontrol then
           begin
              emitlab(exitfinallylabel);
              { allocate eax }
              exprasmlist^.concat(new(pairegalloc,alloc(R_EAX)));
              emit_reg(A_POP,S_L,R_EAX);
              exprasmlist^.concat(new(pairegalloc,alloc(R_EAX)));
              emit_const(A_PUSH,S_L,2);
              emitjmp(C_NONE,finallylabel);
           end;
         if fc_break in tryflowcontrol then
          begin
             emitlab(breakfinallylabel);
             { allocate eax }
             exprasmlist^.concat(new(pairegalloc,alloc(R_EAX)));
             emit_reg(A_POP,S_L,R_EAX);
             { deallocate eax }
             exprasmlist^.concat(new(pairegalloc,dealloc(R_EAX)));
             emit_const(A_PUSH,S_L,3);
             emitjmp(C_NONE,finallylabel);
           end;
         if fc_continue in tryflowcontrol then
           begin
              emitlab(continuefinallylabel);
              exprasmlist^.concat(new(pairegalloc,alloc(R_EAX)));
              emit_reg(A_POP,S_L,R_EAX);
              exprasmlist^.concat(new(pairegalloc,alloc(R_EAX)));
              emit_const(A_PUSH,S_L,4);
              emitjmp(C_NONE,finallylabel);
           end;

         emitlab(endfinallylabel);

         aktexitlabel:=oldaktexitlabel;
         aktexit2label:=oldaktexit2label;
         if assigned(aktbreaklabel) then
          begin
            aktcontinuelabel:=oldaktcontinuelabel;
            aktbreaklabel:=oldaktbreaklabel;
          end;
         flowcontrol:=oldflowcontrol+tryflowcontrol;
      end;


{*****************************************************************************
                             SecondFail
*****************************************************************************}

    procedure secondfail(var p : ptree);
      begin
        emitjmp(C_None,faillabel);
      end;


end.
{
  $Log$
  Revision 1.71  2000-04-16 08:08:44  jonas
    * release register used in for-loop before end label (for better
      optimizations)

  Revision 1.70  2000/02/29 23:58:19  pierre
    Use $GOTO ON

  Revision 1.69  2000/02/10 23:44:42  florian
    * big update for exception handling code generation: possible mem holes
      fixed, break/continue/exit should work always now as expected

  Revision 1.68  2000/02/09 13:22:47  peter
    * log truncated

  Revision 1.67  2000/01/21 12:17:42  jonas
    * regallocation fixes

  Revision 1.66  2000/01/07 01:14:20  peter
    * updated copyright to 2000

  Revision 1.65  1999/12/22 23:30:06  peter
    * nested try blocks work again

  Revision 1.64  1999/12/22 01:01:46  peter
    - removed freelabel()
    * added undefined label detection in internal assembler, this prevents
      a lot of ld crashes and wrong .o files
    * .o files aren't written anymore if errors have occured
    * inlining of assembler labels is now correct

  Revision 1.63  1999/12/19 17:02:45  peter
    * support exit,break,continue in try...except
    * support break,continue in try...finally

  Revision 1.62  1999/12/17 11:20:06  florian
    * made the goto checking for excpetions more fool proof against errors

  Revision 1.61  1999/12/14 09:58:41  florian
    + compiler checks now if a goto leaves an exception block

  Revision 1.60  1999/12/01 12:36:23  peter
    * fixed selfpointer after destroyexception

  Revision 1.59  1999/11/30 10:40:42  peter
    + ttype, tsymlist

  Revision 1.58  1999/11/28 23:15:23  pierre
   * fix for form bug 721

  Revision 1.57  1999/11/15 21:49:09  peter
    * push address also for raise at

  Revision 1.56  1999/11/06 14:34:17  peter
    * truncated log to 20 revs

  Revision 1.55  1999/10/30 17:35:26  peter
    * fpc_freemem fpc_getmem new callings updated

  Revision 1.54  1999/10/21 16:41:37  florian
    * problems with readln fixed: esi wasn't restored correctly when
      reading ordinal fields of objects futher the register allocation
      didn't take care of the extra register when reading ordinal values
    * enumerations can now be used in constant indexes of properties

  Revision 1.53  1999/10/05 22:01:52  pierre
   * bug exit('test') + fail for classes

  Revision 1.52  1999/09/27 23:44:46  peter
    * procinfo is now a pointer
    * support for result setting in sub procedure

  Revision 1.51  1999/09/26 13:26:05  florian
    * exception patch of Romio nevertheless the excpetion handling
      needs some corections regarding register saving
    * gettempansistring is again a procedure

  Revision 1.50  1999/09/20 16:35:43  peter
    * restored old alignment, saves 40k on ppc386

  Revision 1.49  1999/09/15 20:35:37  florian
    * small fix to operator overloading when in MMX mode
    + the compiler uses now fldz and fld1 if possible
    + some fixes to floating point registers
    + some math. functions (arctan, ln, sin, cos, sqrt, sqr, pi) are now inlined
    * .... ???

  Revision 1.48  1999/09/07 07:56:37  peter
    * reload esi in except block to allow virtual methods

  Revision 1.47  1999/08/25 11:59:42  jonas
    * changed pai386, paippc and paiapha (same for tai*) to paicpu (taicpu)
}