{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

    Generate assembler for nodes that influence the flow

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


{ in sync with cg386flw rev 1.55 of the main branch }

unit cg386flw;
interface

    uses
      tree;

    procedure second_while_repeatn(list: paasmoutput; var p : ptree);
    procedure secondifn(list: paasmoutput; var p : ptree);
    procedure secondfor(list: paasmoutput; var p : ptree);
    procedure secondexitn(list: paasmoutput; var p : ptree);
    procedure secondbreakn(list: paasmoutput; var p : ptree);
    procedure secondcontinuen( list: paasmoutput; var p : ptree);
    procedure secondgoto(list: paasmoutput; var p : ptree);
    procedure secondlabel(list: paasmoutput; var p : ptree);
    procedure secondraise(list: paasmoutput; var p : ptree);
    procedure secondtryexcept(list: paasmoutput; var p : ptree);
    procedure secondtryfinally(list: paasmoutput; var p : ptree);
    procedure secondon(list: paasmoutput; var p : ptree);
    procedure secondfail(list: paasmoutput; var p : ptree);


implementation

    uses
      cobjects,verbose,globtype,globals,systems,
      symconst,symtable,aasm,types,
      hcodegen,temp_gen,pass_2,
      cpubase,cpuasm{,
      cgai386,tgeni386}, cgcpu;

{*****************************************************************************
                         Second_While_RepeatN
*****************************************************************************}

    procedure second_while_repeatn(list: paasmoutput; var p : ptree);
    { converted, problems left: }
    {   * maketojumpbool        }
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
          a_jmp_cond(list,C_None,lcont);

         a_label(list,lloop);

         aktcontinuelabel:=lcont;
         aktbreaklabel:=lbreak;
         cleartempgen;
         if assigned(p^.right) then
           secondpass(list,p^.right);
         a_label(list,lcont);
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
         secondpass(list,p^.left);
         { has to be implemented processor dependently !!!! }
         maketojumpbool(list, p^.left);
         a_label(ist,lbreak);
         freelabel(lloop);
         freelabel(lcont);
         freelabel(lbreak);
         truelabel:=otlabel;
         falselabel:=oflabel;

         aktcontinuelabel:=oldclabel;
         aktbreaklabel:=oldblabel;
      end;


{*****************************************************************************
                               SecondIfN
*****************************************************************************}

    procedure secondifn(list: paasmoutput; var p : ptree);
    { converted, problems left: }
    {   * maketojumpbool        }
      var
         hl,otlabel,oflabel : pasmlabel;

      begin
         otlabel:=truelabel;
         oflabel:=falselabel;
         getlabel(truelabel);
         getlabel(falselabel);
         cleartempgen;
         secondpass(list,p^.left);
         maketojumpbool(list,p^.left);
         if assigned(p^.right) then
           begin
              a_label(list, truelabel);
              cleartempgen;
              secondpass(list,p^.right);
           end;
         if assigned(p^.t1) then
           begin
              if assigned(p^.right) then
                begin
                   getlabel(hl);
                   { do go back to if line !! }
                   aktfilepos:=list^.getlasttaifilepos^;
                   a_jmp_cond(list,C_None,hl);
                end;
              a_label(list,falselabel);
              cleartempgen;
              secondpass(list,p^.t1);
              if assigned(p^.right) then
                a_label(list,hl);
           end
         else
           begin
              a_label(list,falselabel);
           end;
         if not(assigned(p^.right)) then
           begin
              a_label(list,truelabel);
           end;
         freelabel(truelabel);
         freelabel(falselabel);
         truelabel:=otlabel;
         falselabel:=oflabel;
      end;


{*****************************************************************************
                              SecondFor
*****************************************************************************}

    procedure secondfor(list: paasmoutput; var p : ptree);
    { still being converted, problems left:         }
    {   * getregister32                             }
    {   * size issues (" p^.t2^.resulttype^.size")  }
     
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
         secondpass(list,p^.t2);
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
         secondpass(list,p^.right);
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
         emitlab(aktbreaklabel);
         ungetregister32(cmp32);

         if temptovalue then
           ungetiftemp(temp1);

         freelabel(aktcontinuelabel);
         freelabel(aktbreaklabel);
         freelabel(l3);
         aktcontinuelabel:=oldclabel;
         aktbreaklabel:=oldblabel;
      end;


{*****************************************************************************
                              SecondExitN
*****************************************************************************}

    procedure secondexitn(list: paasmoutput; var p : ptree);
      var
         is_mem : boolean;
         {op : tasmop;
         s : topsize;}
         otlabel,oflabel : pasmlabel;
      label
         do_jmp;
      begin
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
              case procinfo^.retdef^.deftype of
               orddef,
              enumdef : begin
                          case procinfo^.retdef^.size of
                           4 : if is_mem then
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
                          if pfloatdef(procinfo^.retdef)^.typ=f32bit then
                           begin
                             if is_mem then
                               emit_ref_reg(A_MOV,S_L,
                                 newreference(p^.left^.location.reference),R_EAX)
                             else
                               emit_reg_reg(A_MOV,S_L,p^.left^.location.register,R_EAX);
                           end
                          else
                           if is_mem then
                            floatload(pfloatdef(procinfo^.retdef)^.typ,p^.left^.location.reference);
                        end;
              end;
do_jmp:
              freelabel(truelabel);
              freelabel(falselabel);
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

    procedure secondbreakn(list: paasmoutput; var p : ptree);
      begin
         if aktbreaklabel<>nil then
           emitjmp(C_None,aktbreaklabel)
         else
           CGMessage(cg_e_break_not_allowed);
      end;


{*****************************************************************************
                              SecondContinueN
*****************************************************************************}

    procedure secondcontinuen(list: paasmoutput; var p : ptree);
      begin
         if aktcontinuelabel<>nil then
           emitjmp(C_None,aktcontinuelabel)
         else
           CGMessage(cg_e_continue_not_allowed);
      end;


{*****************************************************************************
                             SecondGoto
*****************************************************************************}

    procedure secondgoto(list: paasmoutput; var p : ptree);

       begin
         emitjmp(C_None,p^.labelnr);
       end;


{*****************************************************************************
                             SecondLabel
*****************************************************************************}

    procedure secondlabel(list: paasmoutput; var p : ptree);
      begin
         emitlab(p^.labelnr);
         cleartempgen;
         secondpass(p^.left);
      end;


{*****************************************************************************
                             SecondRaise
*****************************************************************************}

    procedure secondraise(list: paasmoutput; var p : ptree);

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

    procedure secondtryexcept(list: paasmoutput; var p : ptree);

      var
         exceptlabel,doexceptlabel,oldendexceptlabel,
         lastonlabel : pasmlabel;

      begin
         { this can be called recursivly }
         oldendexceptlabel:=endexceptlabel;
         { we modify EAX }
         usedinproc:=usedinproc or ($80 shr byte(R_EAX));

         getlabel(exceptlabel);
         getlabel(doexceptlabel);
         getlabel(endexceptlabel);
         getlabel(lastonlabel);
         push_int (1); { push type of exceptionframe }
         emitcall('FPC_PUSHEXCEPTADDR');
         emit_reg(A_PUSH,S_L,R_EAX);
         emitcall('FPC_SETJMP');
         emit_reg(A_PUSH,S_L,R_EAX);
         emit_reg_reg(A_TEST,S_L,R_EAX,R_EAX);
         emitjmp(C_NE,exceptlabel);

         { try code }
         secondpass(p^.left);
         if codegenerror then
           exit;

         emitlab(exceptlabel);
         emitcall('FPC_POPADDRSTACK');
         emit_reg(A_POP,S_L,R_EAX);
         emit_reg_reg(A_TEST,S_L,R_EAX,R_EAX);
         emitjmp(C_E,endexceptlabel);
         emitlab(doexceptlabel);

         if assigned(p^.right) then
           secondpass(p^.right);

         emitlab(lastonlabel);
         { default handling }
         if assigned(p^.t1) then
           begin
              { FPC_CATCHES must be called with
                'default handler' flag (=-1)
              }
              push_int (-1);
              emitcall('FPC_CATCHES');
              maybe_loadesi;
              secondpass(p^.t1);
              emitcall('FPC_POPOBJECTSTACK');
              maybe_loadesi;
           end
         else
           emitcall('FPC_RERAISE');
         { reraise doesn't need a maybe_loadesi because it never }
         { returns (FK)                                          }
         emitlab(endexceptlabel);
         freelabel(exceptlabel);
         freelabel(doexceptlabel);
         freelabel(endexceptlabel);
         freelabel(lastonlabel);
         endexceptlabel:=oldendexceptlabel;
      end;

    procedure secondon(list: paasmoutput; var p : ptree);

      var
         nextonlabel : pasmlabel;
         ref : treference;

      begin
         getlabel(nextonlabel);

         { push the vmt }
         emit_sym(A_PUSH,S_L,
           newasmsymbol(p^.excepttype^.vmt_mangledname));
         emitcall('FPC_CATCHES');
         emit_reg_reg(A_TEST,S_L,R_EAX,R_EAX);
         emitjmp(C_E,nextonlabel);
         ref.symbol:=nil;
         gettempofsizereference(4,ref);

         { what a hack ! }
         if assigned(p^.exceptsymtable) then
           pvarsym(p^.exceptsymtable^.symindex^.first)^.address:=ref.offset;

         emit_reg_ref(A_MOV,S_L,
           R_EAX,newreference(ref));

         if assigned(p^.right) then
           begin
             { esi is destroyed by FPC_CATCHES }
             maybe_loadesi;
             secondpass(p^.right);
           end;


         emit_ref(A_PUSH,S_L,
           newreference(ref));
         emitcall('FPC_DESTROYEXCEPTION');
         emitcall('FPC_POPOBJECTSTACK');

         { clear some stuff }
         ungetiftemp(ref);
         emitjmp(C_None,endexceptlabel);
         emitlab(nextonlabel);
         { next on node }
         if assigned(p^.left) then
           secondpass(p^.left);
      end;

{*****************************************************************************
                             SecondTryFinally
*****************************************************************************}

    procedure secondtryfinally(list: paasmoutput; var p : ptree);

      var
         finallylabel,noreraiselabel : pasmlabel;
         oldaktexitlabel,exitfinallylabel : pasmlabel;
         oldaktexit2label : pasmlabel;

      begin
         { we modify EAX }
         usedinproc:=usedinproc or ($80 shr byte(R_EAX));
         getlabel(finallylabel);
         getlabel(noreraiselabel);
         oldaktexitlabel:=aktexitlabel;
         oldaktexit2label:=aktexit2label;
         getlabel(exitfinallylabel);
         aktexitlabel:=exitfinallylabel;
         aktexit2label:=exitfinallylabel;

         push_int(1); { Type of stack-frame must be pushed}
         emitcall('FPC_PUSHEXCEPTADDR');
         emit_reg(A_PUSH,S_L,R_EAX);
         emitcall('FPC_SETJMP');
         emit_reg(A_PUSH,S_L,R_EAX);
         emit_reg_reg(A_TEST,S_L,R_EAX,R_EAX);
         emitjmp(C_NE,finallylabel);

         { try code }
         secondpass(p^.left);
         if codegenerror then
           exit;

         emitlab(finallylabel);
         emitcall('FPC_POPADDRSTACK');
         { finally code }
         secondpass(p^.right);
         if codegenerror then
           exit;
         emit_reg(A_POP,S_L,R_EAX);
         emit_reg_reg(A_TEST,S_L,R_EAX,R_EAX);
         emitjmp(C_E,noreraiselabel);
         emit_reg(A_DEC,S_L,R_EAX);
         emitjmp(C_NE,oldaktexitlabel);
         emitcall('FPC_RERAISE');
         { reraise never returns ! }
         emitlab(exitfinallylabel);

         { do some magic for exit in the try block }
         emit_reg(A_POP,S_L,R_EAX);
         emit_const(A_PUSH,S_L,2);
         emitjmp(C_NONE,finallylabel);
         emitlab(noreraiselabel);
         aktexitlabel:=oldaktexitlabel;
         aktexit2label:=oldaktexit2label;
      end;


{*****************************************************************************
                             SecondFail
*****************************************************************************}

    procedure secondfail(list: paasmoutput; var p : ptree);
      begin
        emitjmp(C_None,faillabel);
      end;


end.
{
  $Log$
  Revision 1.1  2000-07-13 06:30:07  michael
  + Initial import

  Revision 1.2  2000/01/07 01:14:52  peter
    * updated copyright to 2000

  Revision 1.1  1999/11/03 14:13:59  jonas
    + initial implementation


}
