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
unit n386flw;

{$i defines.inc}

interface

    uses
      node,nflw;

    type
       ti386whilerepeatnode = class(twhilerepeatnode)
          procedure pass_2;override;
       end;

       ti386ifnode = class(tifnode)
          procedure pass_2;override;
       end;

       ti386fornode = class(tfornode)
          procedure pass_2;override;
       end;

       ti386exitnode = class(texitnode)
          procedure pass_2;override;
       end;

       ti386breaknode = class(tbreaknode)
          procedure pass_2;override;
       end;

       ti386continuenode = class(tcontinuenode)
          procedure pass_2;override;
       end;

       ti386gotonode = class(tgotonode)
          procedure pass_2;override;
       end;

       ti386labelnode = class(tlabelnode)
          procedure pass_2;override;
       end;

       ti386raisenode = class(traisenode)
          procedure pass_2;override;
       end;

       ti386tryexceptnode = class(ttryexceptnode)
          procedure pass_2;override;
       end;

       ti386tryfinallynode = class(ttryfinallynode)
          procedure pass_2;override;
       end;

       ti386onnode = class(tonnode)
          procedure pass_2;override;
       end;

       ti386failnode = class(tfailnode)
          procedure pass_2;override;
       end;

implementation

    uses
      verbose,globtype,globals,systems,
      symconst,symdef,symsym,aasm,types,
      hcodegen,temp_gen,pass_2,
      cpubase,cpuasm,
      pass_1,nld,ncon,
      cgai386,tgcpu,n386util,regvars;

{*****************************************************************************
                         Second_While_RepeatN
*****************************************************************************}

    procedure ti386whilerepeatnode.pass_2;
      var
         lcont,lbreak,lloop,
         oldclabel,oldblabel : pasmlabel;
         otlabel,oflabel : pasmlabel;

         //start_regvars_loaded,
         //then_regvars_loaded: regvar_booleanarray;

      begin
         getlabel(lloop);
         getlabel(lcont);
         getlabel(lbreak);
         { arrange continue and breaklabels: }
         oldclabel:=aktcontinuelabel;
         oldblabel:=aktbreaklabel;

         load_all_regvars(exprasmlist);
         { handling code at the end as it is much more efficient, and makes
           while equal to repeat loop, only the end true/false is swapped (PFV) }
         if nodetype=whilen then
           emitjmp(C_None,lcont);

         emitlab(lloop);

         aktcontinuelabel:=lcont;
         aktbreaklabel:=lbreak;
         cleartempgen;
         if assigned(right) then
           secondpass(right);

         load_all_regvars(exprasmlist);

         emitlab(lcont);
         otlabel:=truelabel;
         oflabel:=falselabel;
         if nodetype=whilen then
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
         secondpass(left);

         load_all_regvars(exprasmlist);

         maketojumpbool(left);
         emitlab(lbreak);
         truelabel:=otlabel;
         falselabel:=oflabel;


         aktcontinuelabel:=oldclabel;
         aktbreaklabel:=oldblabel;
         { a break/continue in a while/repeat block can't be seen outside }
         flowcontrol:=flowcontrol-[fc_break,fc_continue];
      end;


{*****************************************************************************
                               TI386IFNODE
*****************************************************************************}

    procedure ti386ifnode.pass_2;

      var
         hl,otlabel,oflabel : pasmlabel;

      begin
         otlabel:=truelabel;
         oflabel:=falselabel;
         getlabel(truelabel);
         getlabel(falselabel);
         cleartempgen;
         secondpass(left);
         load_all_regvars(exprasmlist);
         maketojumpbool(left);
         if assigned(right) then
           begin
              emitlab(truelabel);
              cleartempgen;
              secondpass(right);
              { automatically done for blocks, but not for statements (JM) }
              load_all_regvars(exprasmlist);
           end;
         if assigned(t1) then
           begin
              if assigned(right) then
                begin
                   getlabel(hl);
                   { do go back to if line !! }
                   aktfilepos:=exprasmList.getlasttaifilepos^;
                   emitjmp(C_None,hl);
                end;
              emitlab(falselabel);
              cleartempgen;
              secondpass(t1);
              load_all_regvars(exprasmlist);
              if assigned(right) then
                emitlab(hl);
           end
         else
           begin
              emitlab(falselabel);
           end;
         if not(assigned(right)) then
           begin
              emitlab(truelabel);
           end;
         truelabel:=otlabel;
         falselabel:=oflabel;
      end;


{*****************************************************************************
                              SecondFor
*****************************************************************************}

    procedure ti386fornode.pass_2;
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
         if right.nodetype=ordconstn then
           if tassignmentnode(left).right.nodetype=ordconstn then
             omitfirstcomp:=((nf_backward in flags) and
               (tordconstnode(tassignmentnode(left).right).value>=tordconstnode(right).value))
               or (not(nf_backward in flags) and
                  (tordconstnode(tassignmentnode(left).right).value<=tordconstnode(right).value));

         { only calculate reference }
         cleartempgen;
         secondpass(t2);
         hs:=t2.resulttype^.size;
         if t2.location.loc <> LOC_CREGISTER then
           cmp32:=getregister32;
         case hs of
            1 : begin
                   opsize:=S_B;
                   if t2.location.loc <> LOC_CREGISTER then
                     cmpreg:=reg32toreg8(cmp32);
                end;
            2 : begin
                   opsize:=S_W;
                   if t2.location.loc <> LOC_CREGISTER then
                     cmpreg:=reg32toreg16(cmp32);
                end;
            4 : begin
                   opsize:=S_L;
                   if t2.location.loc <> LOC_CREGISTER then
                     cmpreg:=cmp32;
                end;
         end;

         { first set the to value
           because the count var can be in the expression !! }
         cleartempgen;
         secondpass(right);
         { calculate pointer value and check if changeable and if so }
         { load into temporary variable                       }
         if right.nodetype<>ordconstn then
           begin
              temp1.symbol:=nil;
              gettempofsizereference(hs,temp1);
              temptovalue:=true;
              if (right.location.loc=LOC_REGISTER) or
                 (right.location.loc=LOC_CREGISTER) then
                begin
                   emit_reg_ref(A_MOV,opsize,right.location.register,
                      newreference(temp1));
                 end
              else
                 concatcopy(right.location.reference,temp1,hs,false,false);
           end
         else
           temptovalue:=false;

         { produce start assignment }
         cleartempgen;
         secondpass(left);
         count_var_is_signed:=is_signed(porddef(t2.resulttype));
         if temptovalue then
             begin
              if t2.location.loc=LOC_CREGISTER then
                begin
                   emit_ref_reg(A_CMP,opsize,newreference(temp1),
                     t2.location.register);
                end
              else
                begin
                   emit_ref_reg(A_MOV,opsize,newreference(t2.location.reference),
                     cmpreg);
                   emit_ref_reg(A_CMP,opsize,newreference(temp1),
                     cmpreg);
                   { temp register not necessary anymore currently (JM) }
                   ungetregister32(cmp32);
                end;
           end
         else
             begin
              if not(omitfirstcomp) then
                begin
                   if t2.location.loc=LOC_CREGISTER then
                     emit_const_reg(A_CMP,opsize,tordconstnode(right).value,
                       t2.location.register)
                   else
                     emit_const_ref(A_CMP,opsize,tordconstnode(right).value,
                       newreference(t2.location.reference));
                end;
           end;
         if nf_backward in flags then
           if count_var_is_signed then
             hcond:=C_L
           else
             hcond:=C_B
         else
           if count_var_is_signed then
             hcond:=C_G
           else
             hcond:=C_A;

         load_all_regvars(exprasmlist);

         if not(omitfirstcomp) or temptovalue then
           emitjmp(hcond,aktbreaklabel);

         { align loop target }
         if not(cs_littlesize in aktglobalswitches) then
           exprasmList.concat(Tai_align.Create_op(4,$90));

         emitlab(l3);

         { help register must not be in instruction block }
         cleartempgen;
         if assigned(t1) then
           begin
             secondpass(t1);
             load_all_regvars(exprasmlist);
           end;

         emitlab(aktcontinuelabel);

         { makes no problems there }
         cleartempgen;

         if (t2.location.loc <> LOC_CREGISTER) then
           begin
             { demand help register again }
             cmp32:=getregister32;
             case hs of
                1 : cmpreg:=reg32toreg8(cmp32);
                2 : cmpreg:=reg32toreg16(cmp32);
                4 : cmpreg:=cmp32;
             end;
           end;

         { produce comparison and the corresponding }
         { jump                              }
         if temptovalue then
           begin
              if t2.location.loc=LOC_CREGISTER then
                begin
                   emit_ref_reg(A_CMP,opsize,newreference(temp1),
                     t2.location.register);
                end
              else
                begin
                   emit_ref_reg(A_MOV,opsize,newreference(t2.location.reference),
                     cmpreg);
                   emit_ref_reg(A_CMP,opsize,newreference(temp1),
                     cmpreg);
                    end;
           end
         else
           begin
              if t2.location.loc=LOC_CREGISTER then
                emit_const_reg(A_CMP,opsize,tordconstnode(right).value,
                  t2.location.register)
              else
                 emit_const_ref(A_CMP,opsize,tordconstnode(right).value,
                   newreference(t2.location.reference));
           end;
         if nf_backward in flags then
           if count_var_is_signed then
             hcond:=C_LE
           else
             hcond:=C_BE
          else
            if count_var_is_signed then
              hcond:=C_GE
            else
              hcond:=C_AE;
         load_all_regvars(exprasmlist);
         emitjmp(hcond,aktbreaklabel);
         { according to count direction DEC or INC... }
         { must be after the test because of 0 to 255 for bytes !! }
         if nf_backward in flags then
           hop:=A_DEC
         else
           hop:=A_INC;

         if t2.location.loc=LOC_CREGISTER then
           emit_reg(hop,opsize,t2.location.register)
         else
           emit_ref(hop,opsize,newreference(t2.location.reference));
         emitjmp(C_None,l3);

         if (t2.location.loc <> LOC_CREGISTER) then
           ungetregister32(cmp32);
         if temptovalue then
           ungetiftemp(temp1);

         { this is the break label: }
         emitlab(aktbreaklabel);

         aktcontinuelabel:=oldclabel;
         aktbreaklabel:=oldblabel;
         { a break/continue in a for block can't be seen outside }
         flowcontrol:=flowcontrol-[fc_break,fc_continue];
      end;


{*****************************************************************************
                              SecondExitN
*****************************************************************************}

    procedure ti386exitnode.pass_2;

      var
         {op : tasmop;
         s : topsize;}
         otlabel,oflabel : pasmlabel;
         r : preference;
         is_mem,
         allocated_eax,
         allocated_edx: boolean;

      procedure cleanleft;
        begin
          if is_mem then
            begin
              del_reference(left.location.reference);
              ungetiftemp(left.location.reference);
            end
          else
            begin
              ungetregister(left.location.register);
              if left.location.registerhigh <> R_NO then
                ungetregister(left.location.registerhigh);
            end;
        end;

      label
         do_jmp;
      begin
         load_all_regvars(exprasmlist);
         include(flowcontrol,fc_exit);
         if assigned(left) then
         if left.nodetype=assignn then
           begin
              { just do a normal assignment followed by exit }
              secondpass(left);
              emitjmp(C_None,aktexitlabel);
           end
         else
           begin
              allocated_eax := false;
              allocated_edx := false;
              otlabel:=truelabel;
              oflabel:=falselabel;
              getlabel(truelabel);
              getlabel(falselabel);
              secondpass(left);
              case left.location.loc of
                 LOC_FPU : goto do_jmp;
                 LOC_MEM,
           LOC_REFERENCE : is_mem:=true;
           LOC_CREGISTER,
            LOC_REGISTER : is_mem:=false;
               LOC_FLAGS : begin
                             exprasmlist.concat(tairegalloc.alloc(R_EAX));
                             allocated_eax := true;
                             emit_flag2reg(left.location.resflags,R_AL);
                             goto do_jmp;
                           end;
                LOC_JUMP : begin
                             exprasmlist.concat(tairegalloc.alloc(R_EAX));
                             allocated_eax := true;
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
                          cleanleft;
                          exprasmlist.concat(tairegalloc.alloc(R_EAX));
                          allocated_eax := true;
                          if is_mem then
                            emit_ref_reg(A_MOV,S_L,
                              newreference(left.location.reference),R_EAX)
                          else
                            emit_reg_reg(A_MOV,S_L,
                              left.location.register,R_EAX);
                        end;
             floatdef : begin
                          cleanleft;
                          if pfloatdef(procinfo^.returntype.def)^.typ=f32bit then
                           begin
                             exprasmlist.concat(tairegalloc.alloc(R_EAX));
                             allocated_eax := true;
                             if is_mem then
                               emit_ref_reg(A_MOV,S_L,
                                 newreference(left.location.reference),R_EAX)
                             else
                               emit_reg_reg(A_MOV,S_L,left.location.register,R_EAX);
                           end
                          else
                           if is_mem then
                            floatload(pfloatdef(procinfo^.returntype.def)^.typ,left.location.reference);
                        end;
              { orddef,
              enumdef : }
              else
              { it can be anything shorter than 4 bytes PM
              this caused form bug 711 }
                       begin
                          cleanleft;
                          exprasmlist.concat(tairegalloc.alloc(R_EAX));
                          allocated_eax := true;
                          case procinfo^.returntype.def^.size of
                           { it can be a qword/int64 too ... }
                           8 : if is_mem then
                                 begin
                                    emit_ref_reg(A_MOV,S_L,
                                      newreference(left.location.reference),R_EAX);
                                    r:=newreference(left.location.reference);
                                    inc(r^.offset,4);
                                    exprasmlist.concat(tairegalloc.alloc(R_EDX));
                                    allocated_edx := true;
                                    emit_ref_reg(A_MOV,S_L,r,R_EDX);
                                 end
                               else
                                 begin
                                    emit_reg_reg(A_MOV,S_L,left.location.registerlow,R_EAX);
                                    exprasmlist.concat(tairegalloc.alloc(R_EDX));
                                    allocated_edx := true;
                                    emit_reg_reg(A_MOV,S_L,left.location.registerhigh,R_EDX);
                                 end;
                          { if its 3 bytes only we can still
                            copy one of garbage ! PM }
                           4,3 : if is_mem then
                                 emit_ref_reg(A_MOV,S_L,
                                   newreference(left.location.reference),R_EAX)
                               else
                                 emit_reg_reg(A_MOV,S_L,left.location.register,R_EAX);
                           2 : if is_mem then
                                 emit_ref_reg(A_MOV,S_W,
                                   newreference(left.location.reference),R_AX)
                               else
                                 emit_reg_reg(A_MOV,S_W,makereg16(left.location.register),R_AX);
                           1 : if is_mem then
                                 emit_ref_reg(A_MOV,S_B,
                                   newreference(left.location.reference),R_AL)
                               else
                                 emit_reg_reg(A_MOV,S_B,makereg8(left.location.register),R_AL);
                           else internalerror(605001);
                          end;
                        end;
              end;
do_jmp:
              truelabel:=otlabel;
              falselabel:=oflabel;
              emitjmp(C_None,aktexit2label);
              if allocated_eax then
                exprasmlist.concat(tairegalloc.dealloc(R_EAX));
              if allocated_edx then
                exprasmlist.concat(tairegalloc.dealloc(R_EDX));
           end
         else
            emitjmp(C_None,aktexitlabel);
       end;


{*****************************************************************************
                              SecondBreakN
*****************************************************************************}

    procedure ti386breaknode.pass_2;
      begin
         include(flowcontrol,fc_break);
         if aktbreaklabel<>nil then
           begin
             load_all_regvars(exprasmlist);
             emitjmp(C_None,aktbreaklabel)
           end
         else
           CGMessage(cg_e_break_not_allowed);
      end;


{*****************************************************************************
                              SecondContinueN
*****************************************************************************}

    procedure ti386continuenode.pass_2;
      begin
         include(flowcontrol,fc_continue);
         if aktcontinuelabel<>nil then
           begin
             load_all_regvars(exprasmlist);
             emitjmp(C_None,aktcontinuelabel)
           end
         else
           CGMessage(cg_e_continue_not_allowed);
      end;


{*****************************************************************************
                             SecondGoto
*****************************************************************************}

    procedure ti386gotonode.pass_2;

       begin
         load_all_regvars(exprasmlist);
         emitjmp(C_None,labelnr);
         { the assigned avoids only crashes if the label isn't defined }
         if assigned(labsym) and
           assigned(labsym^.code) and
            (aktexceptblock<>tlabelnode(labsym^.code).exceptionblock) then
           CGMessage(cg_e_goto_inout_of_exception_block);
       end;


{*****************************************************************************
                             SecondLabel
*****************************************************************************}

    procedure ti386labelnode.pass_2;
      begin
         load_all_regvars(exprasmlist);
         emitlab(labelnr);
         cleartempgen;
         secondpass(left);
      end;


{*****************************************************************************
                             SecondRaise
*****************************************************************************}

    procedure ti386raisenode.pass_2;

      var
         a : pasmlabel;
      begin
         if assigned(left) then
           begin
              { multiple parameters? }
              if assigned(right) then
                begin
                  { push frame }
                  if assigned(frametree) then
                    begin
                      secondpass(frametree);
                      if codegenerror then
                       exit;
                      emit_push_loc(frametree.location);
                    end
                  else
                    emit_const(A_PUSH,S_L,0);
                  { push address }
                  secondpass(right);
                  if codegenerror then
                   exit;
                  emit_push_loc(right.location);
                end
              else
                begin
                   getaddrlabel(a);
                   emitlab(a);
                   emit_reg(A_PUSH,S_L,R_EBP);
                   emit_sym(A_PUSH,S_L,a);
                end;
              { push object }
              secondpass(left);
              if codegenerror then
                exit;
              emit_push_loc(left.location);
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
         exprasmList.concat(Tairegalloc.Alloc(R_EAX));
         emit_reg(A_PUSH,S_L,R_EAX);
         emitcall('FPC_DESTROYEXCEPTION');
         exprasmList.concat(Tairegalloc.DeAlloc(R_EAX));
         maybe_loadesi;
      end;

    { pops one element from the exception address stack }
    { and removes the flag                              }
    procedure cleanupaddrstack;

      begin
         emitcall('FPC_POPADDRSTACK');
         { allocate eax }
         exprasmList.concat(Tairegalloc.Alloc(R_EAX));
         emit_reg(A_POP,S_L,R_EAX);
         { deallocate eax }
         exprasmList.concat(Tairegalloc.DeAlloc(R_EAX));
      end;

    procedure ti386tryexceptnode.pass_2;

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
         oldexceptblock : tnode;


         oldflowcontrol,tryflowcontrol,
         exceptflowcontrol : tflowcontrol;
         tempbuf,tempaddr : treference;

      label
         errorexit;
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

         gettempofsizereferencepersistant(24,tempbuf);
         gettempofsizereferencepersistant(12,tempaddr);
         emitpushreferenceaddr(tempaddr);
         emitpushreferenceaddr(tempbuf);
         push_int (1); { push type of exceptionframe }
         emitcall('FPC_PUSHEXCEPTADDR');

         { allocate eax }
         exprasmList.concat(Tairegalloc.Alloc(R_EAX));
         emit_reg(A_PUSH,S_L,R_EAX);
         emitcall('FPC_SETJMP');
         emit_reg(A_PUSH,S_L,R_EAX);
         emit_reg_reg(A_TEST,S_L,R_EAX,R_EAX);
         { deallocate eax }
         exprasmList.concat(Tairegalloc.DeAlloc(R_EAX));
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
         aktexceptblock:=left;
         flowcontrol:=[];
         secondpass(left);
         tryflowcontrol:=flowcontrol;
         aktexceptblock:=oldexceptblock;
         if codegenerror then
           goto errorexit;

         emitlab(exceptlabel);
         emitcall('FPC_POPADDRSTACK');
         ungetpersistanttempreference(tempaddr);
         ungetpersistanttempreference(tempbuf);

         exprasmList.concat(Tairegalloc.Alloc(R_EAX));
         emit_reg(A_POP,S_L,R_EAX);
         emit_reg_reg(A_TEST,S_L,R_EAX,R_EAX);
         exprasmList.concat(Tairegalloc.DeAlloc(R_EAX));

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
         if assigned(right) then
           begin
              oldexceptblock:=aktexceptblock;
              aktexceptblock:=right;
              secondpass(right);
              aktexceptblock:=oldexceptblock;
           end;

         emitlab(lastonlabel);
         { default handling except handling }
         if assigned(t1) then
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

              gettempofsizereferencepersistant(12,tempaddr);
              gettempofsizereferencepersistant(24,tempbuf);
              emitpushreferenceaddr(tempaddr);
              emitpushreferenceaddr(tempbuf);
              exprasmList.concat(Taicpu.Op_const(A_PUSH,S_L,1));
              emitcall('FPC_PUSHEXCEPTADDR');

              exprasmList.concat(Tairegalloc.Alloc(R_EAX));
              exprasmList.concat(Taicpu.op_reg(A_PUSH,S_L,R_EAX));
              exprasmList.concat(Tairegalloc.DeAlloc(R_EAX));
              emitcall('FPC_SETJMP');
              exprasmList.concat(Tairegalloc.Alloc(R_EAX));
              exprasmList.concat(Taicpu.op_reg(A_PUSH,S_L,R_EAX));
              exprasmList.concat(Taicpu.op_reg_reg(A_TEST,S_L,R_EAX,R_EAX));
              exprasmList.concat(Tairegalloc.DeAlloc(R_EAX));
              emitjmp(C_NE,doobjectdestroyandreraise);

              oldexceptblock:=aktexceptblock;
              aktexceptblock:=t1;
              { here we don't have to reset flowcontrol           }
              { the default and on flowcontrols are handled equal }
              secondpass(t1);
              exceptflowcontrol:=flowcontrol;
              aktexceptblock:=oldexceptblock;

              emitlab(doobjectdestroyandreraise);
              emitcall('FPC_POPADDRSTACK');
              ungetpersistanttempreference(tempaddr);
              ungetpersistanttempreference(tempbuf);

              exprasmList.concat(Tairegalloc.Alloc(R_EAX));
              exprasmList.concat(Taicpu.op_reg(A_POP,S_L,R_EAX));
              exprasmList.concat(Taicpu.op_reg_reg(A_TEST,S_L,R_EAX,R_EAX));
              exprasmList.concat(Tairegalloc.DeAlloc(R_EAX));
              emitjmp(C_E,doobjectdestroy);
              emitcall('FPC_POPSECONDOBJECTSTACK');
              exprasmList.concat(Tairegalloc.Alloc(R_EAX));
              emit_reg(A_PUSH,S_L,R_EAX);
              emitcall('FPC_DESTROYEXCEPTION');
              exprasmList.concat(Tairegalloc.DeAlloc(R_EAX));
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

       errorexit:
         { restore all saved labels }
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

    procedure ti386onnode.pass_2;
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
         oldexceptblock : tnode;
         oldflowcontrol : tflowcontrol;
         tempbuf,tempaddr : treference;

      begin
         oldflowcontrol:=flowcontrol;
         flowcontrol:=[];
         getlabel(nextonlabel);

         { push the vmt }
         emit_sym(A_PUSH,S_L,
           newasmsymbol(excepttype^.vmt_mangledname));
         emitcall('FPC_CATCHES');
         { allocate eax }
         exprasmList.concat(Tairegalloc.Alloc(R_EAX));
         emit_reg_reg(A_TEST,S_L,R_EAX,R_EAX);
         emitjmp(C_E,nextonlabel);
         ref.symbol:=nil;
         gettempofsizereference(4,ref);

         { what a hack ! }
         if assigned(exceptsymtable) then
           pvarsym(exceptsymtable^.symindex^.first)^.address:=ref.offset;

         emit_reg_ref(A_MOV,S_L,
           R_EAX,newreference(ref));
         { deallocate eax }
         exprasmList.concat(Tairegalloc.DeAlloc(R_EAX));

         { in the case that another exception is risen }
         { we've to destroy the old one                }
         getlabel(doobjectdestroyandreraise);

         gettempofsizereferencepersistant(12,tempaddr);
         gettempofsizereferencepersistant(24,tempbuf);
         emitpushreferenceaddr(tempaddr);
         emitpushreferenceaddr(tempbuf);
         exprasmList.concat(Taicpu.Op_const(A_PUSH,S_L,1));
         emitcall('FPC_PUSHEXCEPTADDR');

         exprasmList.concat(Tairegalloc.Alloc(R_EAX));
         exprasmList.concat(Taicpu.op_reg(A_PUSH,S_L,R_EAX));
         exprasmList.concat(Tairegalloc.DeAlloc(R_EAX));
         emitcall('FPC_SETJMP');
         exprasmList.concat(Tairegalloc.Alloc(R_EAX));
         exprasmList.concat(Taicpu.op_reg(A_PUSH,S_L,R_EAX));
         exprasmList.concat(Taicpu.op_reg_reg(A_TEST,S_L,R_EAX,R_EAX));
         exprasmList.concat(Tairegalloc.DeAlloc(R_EAX));
         emitjmp(C_NE,doobjectdestroyandreraise);

         if assigned(right) then
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
              aktexceptblock:=right;
              secondpass(right);
              aktexceptblock:=oldexceptblock;
           end;
         getlabel(doobjectdestroy);
         emitlab(doobjectdestroyandreraise);
         emitcall('FPC_POPADDRSTACK');
         ungetpersistanttempreference(tempaddr);
         ungetpersistanttempreference(tempbuf);

         exprasmList.concat(Tairegalloc.Alloc(R_EAX));
         exprasmList.concat(Taicpu.op_reg(A_POP,S_L,R_EAX));
         exprasmList.concat(Taicpu.op_reg_reg(A_TEST,S_L,R_EAX,R_EAX));
         exprasmList.concat(Tairegalloc.DeAlloc(R_EAX));
         emitjmp(C_E,doobjectdestroy);
         emitcall('FPC_POPSECONDOBJECTSTACK');
         exprasmList.concat(Tairegalloc.Alloc(R_EAX));
         emit_reg(A_PUSH,S_L,R_EAX);
         emitcall('FPC_DESTROYEXCEPTION');
         exprasmList.concat(Tairegalloc.DeAlloc(R_EAX));
         { we don't need to restore esi here because reraise never }
         { returns                                                 }
         emitcall('FPC_RERAISE');

         emitlab(doobjectdestroy);
         cleanupobjectstack;
         { clear some stuff }
         ungetiftemp(ref);
         emitjmp(C_None,endexceptlabel);

         if assigned(right) then
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
         if assigned(left) then
           begin
              cleartempgen;
              secondpass(left);
           end;
      end;

{*****************************************************************************
                             SecondTryFinally
*****************************************************************************}

    procedure ti386tryfinallynode.pass_2;
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
         oldexceptblock : tnode;
         oldflowcontrol,tryflowcontrol : tflowcontrol;
         decconst : longint;
         tempbuf,tempaddr : treference;

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

         gettempofsizereferencepersistant(12,tempaddr);
         gettempofsizereferencepersistant(24,tempbuf);
         emitpushreferenceaddr(tempaddr);
         emitpushreferenceaddr(tempbuf);
         push_int(1); { Type of stack-frame must be pushed}
         emitcall('FPC_PUSHEXCEPTADDR');

         { allocate eax }
         exprasmList.concat(Tairegalloc.Alloc(R_EAX));
         emit_reg(A_PUSH,S_L,R_EAX);
         emitcall('FPC_SETJMP');
         emit_reg(A_PUSH,S_L,R_EAX);
         emit_reg_reg(A_TEST,S_L,R_EAX,R_EAX);
         { deallocate eax }
         exprasmList.concat(Tairegalloc.DeAlloc(R_EAX));
         emitjmp(C_NE,finallylabel);

         { try code }
         if assigned(left) then
           begin
              oldexceptblock:=aktexceptblock;
              aktexceptblock:=left;
              secondpass(left);
              tryflowcontrol:=flowcontrol;
              if codegenerror then
                exit;
              aktexceptblock:=oldexceptblock;
           end;

         emitlab(finallylabel);
         emitcall('FPC_POPADDRSTACK');
         ungetpersistanttempreference(tempaddr);
         ungetpersistanttempreference(tempbuf);

         { finally code }
         oldexceptblock:=aktexceptblock;
         aktexceptblock:=right;
         flowcontrol:=[];
         secondpass(right);
         if flowcontrol<>[] then
           CGMessage(cg_e_control_flow_outside_finally);
         aktexceptblock:=oldexceptblock;
         if codegenerror then
           exit;
         { allocate eax }
         exprasmList.concat(Tairegalloc.Alloc(R_EAX));
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
         exprasmList.concat(Tairegalloc.DeAlloc(R_EAX));
         emitlab(reraiselabel);
         emitcall('FPC_RERAISE');
         { do some magic for exit,break,continue in the try block }
         if fc_exit in tryflowcontrol then
           begin
              emitlab(exitfinallylabel);
              { allocate eax }
              exprasmList.concat(Tairegalloc.Alloc(R_EAX));
              emit_reg(A_POP,S_L,R_EAX);
              exprasmList.concat(Tairegalloc.Alloc(R_EAX));
              emit_const(A_PUSH,S_L,2);
              emitjmp(C_NONE,finallylabel);
           end;
         if fc_break in tryflowcontrol then
          begin
             emitlab(breakfinallylabel);
             { allocate eax }
             exprasmList.concat(Tairegalloc.Alloc(R_EAX));
             emit_reg(A_POP,S_L,R_EAX);
             { deallocate eax }
             exprasmList.concat(Tairegalloc.DeAlloc(R_EAX));
             emit_const(A_PUSH,S_L,3);
             emitjmp(C_NONE,finallylabel);
           end;
         if fc_continue in tryflowcontrol then
           begin
              emitlab(continuefinallylabel);
              exprasmList.concat(Tairegalloc.Alloc(R_EAX));
              emit_reg(A_POP,S_L,R_EAX);
              exprasmList.concat(Tairegalloc.Alloc(R_EAX));
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

    procedure ti386failnode.pass_2;
      begin
        emitjmp(C_None,faillabel);
      end;


begin
   cwhilerepeatnode:=ti386whilerepeatnode;
   cifnode:=ti386ifnode;
   cfornode:=ti386fornode;
   cexitnode:=ti386exitnode;
   cbreaknode:=ti386breaknode;
   ccontinuenode:=ti386continuenode;
   cgotonode:=ti386gotonode;
   clabelnode:=ti386labelnode;
   craisenode:=ti386raisenode;
   ctryexceptnode:=ti386tryexceptnode;
   ctryfinallynode:=ti386tryfinallynode;
   connode:=ti386onnode;
   cfailnode:=ti386failnode;
end.
{
  $Log$
  Revision 1.7  2001-01-06 23:35:05  jonas
    * fixed webbug 1323

  Revision 1.6  2001/01/05 17:36:58  florian
  * the info about exception frames is stored now on the stack
  instead on the heap

  Revision 1.5  2000/12/25 00:07:32  peter
    + new tlinkedlist class (merge of old tstringqueue,tcontainer and
      tlinkedlist objects)

  Revision 1.4  2000/12/05 11:44:33  jonas
    + new integer regvar handling, should be much more efficient

  Revision 1.3  2000/11/29 00:30:47  florian
    * unused units removed from uses clause
    * some changes for widestrings

  Revision 1.2  2000/10/31 22:02:56  peter
    * symtable splitted, no real code changes

  Revision 1.1  2000/10/15 09:33:31  peter
    * moved n386*.pas to i386/ cpu_target dir

  Revision 1.1  2000/10/14 10:14:48  peter
    * moehrendorf oct 2000 rewrite

}
