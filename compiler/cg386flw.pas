{
    $Id$
    Copyright (c) 1993-98 by Florian Klaempfl

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


implementation

    uses
      cobjects,verbose,globtype,globals,systems,
      symconst,symtable,aasm,types,
      hcodegen,temp_gen,pass_2,
      cpubase,cpuasm,
      cgai386,tgeni386;

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
         freelabel(truelabel);
         freelabel(falselabel);
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
           exprasmlist^.concat(new(pai_align,init(4)));

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
              case procinfo.retdef^.deftype of
               orddef,
              enumdef : begin
                          case procinfo.retdef^.size of
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
                          if pfloatdef(procinfo.retdef)^.typ=f32bit then
                           begin
                             if is_mem then
                               emit_ref_reg(A_MOV,S_L,
                                 newreference(p^.left^.location.reference),R_EAX)
                             else
                               emit_reg_reg(A_MOV,S_L,p^.left^.location.register,R_EAX);
                           end
                          else
                           if is_mem then
                            floatload(pfloatdef(procinfo.retdef)^.typ,p^.left^.location.reference);
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

    procedure secondbreakn(var p : ptree);
      begin
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

    procedure secondtryexcept(var p : ptree);

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

    procedure secondon(var p : ptree);

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

    procedure secondtryfinally(var p : ptree);

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

    procedure secondfail(var p : ptree);
      {var
        hp : preference;
        nofreememcall, afterfreememcall : pasmlabel; }
      begin
         (* { check if getmem was called :
           VMT at 8(%ebp) is set to -1 after call to getmem PM }
         { also reset to zero in the stack }
         getlabel(nofreememcall);
         getlabel(afterfreememcall);
         new(hp);
         reset_reference(hp^);
         hp^.offset:=8;
         hp^.base:=procinfo.framepointer;
         emit_const_ref(A_CMP,S_L,-1,hp);
         emitjmp(C_NE,nofreememcall);
         new(hp);
         reset_reference(hp^);
         hp^.offset:=procinfo._class^.vmt_offset;
         hp^.base:=R_ESI;
         emit_ref_reg(A_MOV,S_L,hp,R_EDI);
         new(hp);
         reset_reference(hp^);
         hp^.base:=R_EDI;
         {hp^.offset:=0; done in reset_reference }
         emit_ref(A_PUSH,S_L,hp);
         new(hp);
         reset_reference(hp^);
         hp^.offset:=procinfo.ESI_offset;
         hp^.base:=procinfo.framepointer;
         emit_ref_reg(A_LEA,S_L,hp,R_EDI);
         emit_reg(A_PUSH,S_L,R_EDI);
         emitcall('FPC_FREEMEM');
         emitjmp(C_None,afterfreememcall);


         emitlab(nofreememcall);
         { reset VMT field for static object }
         new(hp);
         reset_reference(hp^);
         hp^.offset:=procinfo._class^.vmt_offset;
         hp^.base:=R_ESI;
         emit_const_ref(A_MOV,S_L,0,hp);
         emitlab(afterfreememcall);
         emit_reg_reg(A_XOR,S_L,R_ESI,R_ESI);
         { also reset to zero in the stack }
         new(hp);
         reset_reference(hp^);
         hp^.offset:=procinfo.ESI_offset;
         hp^.base:=procinfo.framepointer;
         emit_reg_ref(A_MOV,S_L,R_ESI,hp); *)
         emitjmp(C_None,faillabel);
      end;


end.
{
  $Log$
  Revision 1.51  1999-09-26 13:26:05  florian
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

  Revision 1.46  1999/08/19 13:06:47  pierre
    * _FAIL will now free memory correctly
      and reset VMT field on static instances
      (not yet tested for classes, is it allowed ?)
    + emit_.... all variant defined here to avoid tons of
      exprasmlist^.concat(new(paicpu,...) in cg386***

  Revision 1.45  1999/08/04 00:22:46  florian
    * renamed i386asm and i386base to cpuasm and cpubase

  Revision 1.44  1999/08/03 22:02:39  peter
    * moved bitmask constants to sets
    * some other type/const renamings

  Revision 1.43  1999/07/26 12:13:45  florian
    * exit in try..finally blocks needed a second fix
    * a raise in a try..finally lead into a endless loop, fixed

  Revision 1.42  1999/07/26 09:41:59  florian
    * bugs 494-496 fixed

  Revision 1.41  1999/07/05 20:13:09  peter
    * removed temp defines

  Revision 1.40  1999/06/14 00:43:35  peter
    * merged

  Revision 1.39.2.1  1999/06/14 00:39:29  peter
    * don't pop object stack in catches, because it's needed for reraise

  Revision 1.39  1999/05/27 19:44:12  peter
    * removed oldasm
    * plabel -> pasmlabel
    * -a switches to source writing automaticly
    * assembler readers OOPed
    * asmsymbol automaticly external
    * jumptables and other label fixes for asm readers

  Revision 1.38  1999/05/21 13:54:48  peter
    * NEWLAB for label as symbol

  Revision 1.37  1999/05/17 21:57:01  florian
    * new temporary ansistring handling

  Revision 1.36  1999/05/13 21:59:21  peter
    * removed oldppu code
    * warning if objpas is loaded from uses
    * first things for new deref writing

  Revision 1.35  1999/05/01 13:24:07  peter
    * merged nasm compiler
    * old asm moved to oldasm/

  Revision 1.34  1999/04/26 13:31:25  peter
    * release storenumber,double_checksum

  Revision 1.33  1999/04/21 09:43:29  peter
    * storenumber works
    * fixed some typos in double_checksum
    + incompatible types type1 and type2 message (with storenumber)

  Revision 1.32  1999/04/17 13:10:58  peter
    * fixed exit()

  Revision 1.31  1999/04/14 09:14:46  peter
    * first things to store the symbol/def number in the ppu

  Revision 1.30  1999/03/05 16:14:59  peter
    * fixed exit() with word/byte return

  Revision 1.29  1999/02/25 21:02:26  peter
    * ag386bin updates
    + coff writer

  Revision 1.28  1999/02/22 02:15:09  peter
    * updates for ag386bin

  Revision 1.27  1999/01/26 11:26:21  pierre
   * bug0152 for i:=1 to i-5 do (i-5) evaluated first

  Revision 1.26  1998/12/19 00:23:44  florian
    * ansistring memory leaks fixed

  Revision 1.25  1998/11/30 09:43:03  pierre
    * some range check bugs fixed (still not working !)
    + added DLL writing support for win32 (also accepts variables)
    + TempAnsi for code that could be used for Temporary ansi strings
      handling

  Revision 1.24  1998/11/18 15:44:09  peter
    * VALUEPARA for tp7 compatible value parameters

  Revision 1.23  1998/11/12 16:43:32  florian
    * functions with ansi strings as result didn't work, solved

  Revision 1.22  1998/10/29 15:42:44  florian
    + partial disposing of temp. ansistrings

  Revision 1.21  1998/10/26 22:58:16  florian
    * new introduded problem with classes fix, the parent class wasn't set
      correct, if the class was defined forward before

  Revision 1.20  1998/10/06 17:16:42  pierre
    * some memory leaks fixed (thanks to Peter for heaptrc !)

  Revision 1.19  1998/09/28 12:13:53  peter
    * fixed repeat continue until true;

  Revision 1.18  1998/09/26 15:03:04  florian
    * small problems with DOM and excpetions fixed (code generation
      of raise was wrong and self was sometimes destroyed :()

  Revision 1.17  1998/09/17 09:42:14  peter
    + pass_2 for cg386
    * Message() -> CGMessage() for pass_1/pass_2

  Revision 1.16  1998/09/14 10:43:48  peter
    * all internal RTL functions start with FPC_

  Revision 1.15  1998/09/04 08:41:39  peter
    * updated some error CGMessages

  Revision 1.14  1998/09/03 17:08:39  pierre
    * better lines for stabs
      (no scroll back to if before else part
      no return to case line at jump outside case)
    + source lines also if not in order

  Revision 1.13  1998/09/01 12:47:58  peter
    * use pdef^.size instead of orddef^.typ

  Revision 1.12  1998/08/28 10:56:58  peter
    * removed warnings

  Revision 1.11  1998/08/05 16:00:10  florian
    * some fixes for ansi strings

  Revision 1.10  1998/08/04 16:26:26  jonas
    * converted // comment to TP comment

  Revision 1.9  1998/07/31 11:36:34  michael
  Default exception handler also needs to call FPC_CATCHES

  Revision 1.8  1998/07/30 13:30:32  florian
    * final implemenation of exception support, maybe it needs
      some fixes :)

  Revision 1.7  1998/07/30 11:18:13  florian
    + first implementation of try ... except on .. do end;
    * limitiation of 65535 bytes parameters for cdecl removed

  Revision 1.6  1998/07/29 13:29:11  michael
  + Corrected try.. code. Type of exception fram is pushed

  Revision 1.5  1998/07/28 21:52:49  florian
    + implementation of raise and try..finally
    + some misc. exception stuff

  Revision 1.4  1998/07/24 22:16:53  florian
    * internal error 10 together with array access fixed. I hope
      that's the final fix.

  Revision 1.3  1998/06/25 08:48:08  florian
    * first version of rtti support

  Revision 1.2  1998/06/08 13:13:33  pierre
    + temporary variables now in temp_gen.pas unit
      because it is processor independent
    * mppc68k.bat modified to undefine i386 and support_mmx
      (which are defaults for i386)

  Revision 1.1  1998/06/05 17:44:12  peter
    * splitted cgi386

}

