{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

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

{$i fpcdefs.inc}

interface

    uses
      node,nflw;

    type

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
      verbose,systems,
      symsym,aasm,
      cgbase,pass_2,
      cpuinfo,cpubase,cpuasm,
      nld,ncon,
      tainst,cga,cgobj,tgobj,rgobj;

{*****************************************************************************
                             SecondRaise
*****************************************************************************}

    procedure ti386raisenode.pass_2;

      var
         a : tasmlabel;
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
                      cg.a_param_loc(exprasmlist,frametree.location,2);
                    end
                  else
                    cg.a_param_const(exprasmlist,OS_INT,0,2);
                  { push address }
                  secondpass(right);
                  if codegenerror then
                   exit;
                  cg.a_param_loc(exprasmlist,right.location,1);
                end
              else
                begin
                   getaddrlabel(a);
                   cg.a_label(exprasmlist,a);
                   cg.a_param_reg(exprasmlist,OS_INT,R_EBP,2);
                   emit_sym(A_PUSH,S_L,a);
                end;
              { push object }
              secondpass(left);
              if codegenerror then
                exit;
              cg.a_param_loc(exprasmlist,left.location,1);
              cg.a_call_name(exprasmlist,'FPC_RAISEEXCEPTION');
           end
         else
           begin
              cg.a_call_name(exprasmlist,'FPC_POPADDRSTACK');
              cg.a_call_name(exprasmlist,'FPC_RERAISE');
           end;
       end;


{*****************************************************************************
                             SecondTryExcept
*****************************************************************************}

    var
       endexceptlabel : tasmlabel;

    { does the necessary things to clean up the object stack }
    { in the except block                                    }
    procedure cleanupobjectstack;

      begin
         cg.a_call_name(exprasmlist,'FPC_POPOBJECTSTACK');
         exprasmList.concat(Tairegalloc.Alloc(R_EAX));
         emit_reg(A_PUSH,S_L,R_EAX);
         cg.a_call_name(exprasmlist,'FPC_DESTROYEXCEPTION');
         exprasmList.concat(Tairegalloc.DeAlloc(R_EAX));
         cg.g_maybe_loadself(exprasmlist);
      end;

    { pops one element from the exception address stack }
    { and removes the flag                              }
    procedure cleanupaddrstack;

      begin
         cg.a_call_name(exprasmlist,'FPC_POPADDRSTACK');
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
         oldaktbreaklabel : tasmlabel;
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
         include(rg.usedinproc,R_EAX);

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

         tg.gettempofsizereferencepersistant(exprasmlist,24,tempbuf);
         tg.gettempofsizereferencepersistant(exprasmlist,12,tempaddr);
         cg.a_paramaddr_ref(exprasmlist,tempaddr,3);
         cg.a_paramaddr_ref(exprasmlist,tempbuf,2);
         { push type of exceptionframe }
         cg.a_param_const(exprasmlist,OS_INT,1,1);
         cg.a_call_name(exprasmlist,'FPC_PUSHEXCEPTADDR');

         { allocate eax }
         exprasmList.concat(Tairegalloc.Alloc(R_EAX));
         emit_reg(A_PUSH,S_L,R_EAX);
         cg.a_call_name(exprasmlist,'FPC_SETJMP');
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

         flowcontrol:=[];
         secondpass(left);
         tryflowcontrol:=flowcontrol;
         if codegenerror then
           goto errorexit;

         cg.a_label(exprasmlist,exceptlabel);
         cg.a_call_name(exprasmlist,'FPC_POPADDRSTACK');
         tg.ungetpersistanttempreference(exprasmlist,tempaddr);
         tg.ungetpersistanttempreference(exprasmlist,tempbuf);

         exprasmList.concat(Tairegalloc.Alloc(R_EAX));
         emit_reg(A_POP,S_L,R_EAX);
         emit_reg_reg(A_TEST,S_L,R_EAX,R_EAX);
         exprasmList.concat(Tairegalloc.DeAlloc(R_EAX));

         emitjmp(C_E,endexceptlabel);
         cg.a_label(exprasmlist,doexceptlabel);

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
           secondpass(right);

         cg.a_label(exprasmlist,lastonlabel);
         { default handling except handling }
         if assigned(t1) then
           begin
              { FPC_CATCHES must be called with
                'default handler' flag (=-1)
              }
              cg.a_param_const(exprasmlist,OS_INT,aword(-1),1);
              cg.a_call_name(exprasmlist,'FPC_CATCHES');
              cg.g_maybe_loadself(exprasmlist);

              { the destruction of the exception object must be also }
              { guarded by an exception frame                        }
              getlabel(doobjectdestroy);
              getlabel(doobjectdestroyandreraise);

              tg.gettempofsizereferencepersistant(exprasmlist,24,tempbuf);
              tg.gettempofsizereferencepersistant(exprasmlist,12,tempaddr);
              cg.a_paramaddr_ref(exprasmlist,tempaddr,3);
              cg.a_paramaddr_ref(exprasmlist,tempbuf,2);
              { push type of exceptionframe }
              cg.a_param_const(exprasmlist,OS_INT,1,1);
              cg.a_call_name(exprasmlist,'FPC_PUSHEXCEPTADDR');

              { allocate eax }
              exprasmList.concat(Tairegalloc.Alloc(R_EAX));
              emit_reg(A_PUSH,S_L,R_EAX);
              cg.a_call_name(exprasmlist,'FPC_SETJMP');
              emit_reg(A_PUSH,S_L,R_EAX);
              emit_reg_reg(A_TEST,S_L,R_EAX,R_EAX);
              { deallocate eax }
              exprasmList.concat(Tairegalloc.DeAlloc(R_EAX));
              emitjmp(C_NE,exceptlabel);

              { here we don't have to reset flowcontrol           }
              { the default and on flowcontrols are handled equal }
              secondpass(t1);
              exceptflowcontrol:=flowcontrol;

              cg.a_label(exprasmlist,doobjectdestroyandreraise);
              cg.a_call_name(exprasmlist,'FPC_POPADDRSTACK');
              tg.ungetpersistanttempreference(exprasmlist,tempaddr);
              tg.ungetpersistanttempreference(exprasmlist,tempbuf);

              exprasmList.concat(Tairegalloc.Alloc(R_EAX));
              exprasmList.concat(Taicpu.op_reg(A_POP,S_L,R_EAX));
              exprasmList.concat(Taicpu.op_reg_reg(A_TEST,S_L,R_EAX,R_EAX));
              exprasmList.concat(Tairegalloc.DeAlloc(R_EAX));
              emitjmp(C_E,doobjectdestroy);
              cg.a_call_name(exprasmlist,'FPC_POPSECONDOBJECTSTACK');
              exprasmList.concat(Tairegalloc.Alloc(R_EAX));
              emit_reg(A_PUSH,S_L,R_EAX);
              cg.a_call_name(exprasmlist,'FPC_DESTROYEXCEPTION');
              exprasmList.concat(Tairegalloc.DeAlloc(R_EAX));
              { we don't need to restore esi here because reraise never }
              { returns                                                 }
              cg.a_call_name(exprasmlist,'FPC_RERAISE');

              cg.a_label(exprasmlist,doobjectdestroy);
              cleanupobjectstack;
              cg.a_jmp_always(exprasmlist,endexceptlabel);
           end
         else
           begin
              cg.a_call_name(exprasmlist,'FPC_RERAISE');
              exceptflowcontrol:=flowcontrol;
           end;

         if fc_exit in exceptflowcontrol then
           begin
              { do some magic for exit in the try block }
              cg.a_label(exprasmlist,exitexceptlabel);
              { we must also destroy the address frame which guards }
              { exception object                                    }
              cleanupaddrstack;
              cleanupobjectstack;
              cg.a_jmp_always(exprasmlist,oldaktexitlabel);
           end;

         if fc_break in exceptflowcontrol then
           begin
              cg.a_label(exprasmlist,breakexceptlabel);
              { we must also destroy the address frame which guards }
              { exception object                                    }
              cleanupaddrstack;
              cleanupobjectstack;
              cg.a_jmp_always(exprasmlist,oldaktbreaklabel);
           end;

         if fc_continue in exceptflowcontrol then
           begin
              cg.a_label(exprasmlist,continueexceptlabel);
              { we must also destroy the address frame which guards }
              { exception object                                    }
              cleanupaddrstack;
              cleanupobjectstack;
              cg.a_jmp_always(exprasmlist,oldaktcontinuelabel);
           end;

         if fc_exit in tryflowcontrol then
           begin
              { do some magic for exit in the try block }
              cg.a_label(exprasmlist,exittrylabel);
              cleanupaddrstack;
              cg.a_jmp_always(exprasmlist,oldaktexitlabel);
           end;

         if fc_break in tryflowcontrol then
           begin
              cg.a_label(exprasmlist,breaktrylabel);
              cleanupaddrstack;
              cg.a_jmp_always(exprasmlist,oldaktbreaklabel);
           end;

         if fc_continue in tryflowcontrol then
           begin
              cg.a_label(exprasmlist,continuetrylabel);
              cleanupaddrstack;
              cg.a_jmp_always(exprasmlist,oldaktcontinuelabel);
           end;

         cg.a_label(exprasmlist,endexceptlabel);

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
         oldaktbreaklabel : tasmlabel;
         ref : treference;
         oldflowcontrol : tflowcontrol;
         tempbuf,tempaddr : treference;

      begin
         oldflowcontrol:=flowcontrol;
         flowcontrol:=[];
         getlabel(nextonlabel);

         { push the vmt }
         emit_sym(A_PUSH,S_L,
           newasmsymbol(excepttype.vmt_mangledname));
         cg.a_call_name(exprasmlist,'FPC_CATCHES');
         { allocate eax }
         exprasmList.concat(Tairegalloc.Alloc(R_EAX));
         emit_reg_reg(A_TEST,S_L,R_EAX,R_EAX);
         emitjmp(C_E,nextonlabel);
         ref.symbol:=nil;
         tg.gettempofsizereference(exprasmlist,4,ref);

         { what a hack ! }
         if assigned(exceptsymtable) then
           tvarsym(exceptsymtable.symindex.first).address:=ref.offset;

         emit_reg_ref(A_MOV,S_L,R_EAX,ref);
         { deallocate eax }
         exprasmList.concat(Tairegalloc.DeAlloc(R_EAX));

         { in the case that another exception is risen }
         { we've to destroy the old one                }
         getlabel(doobjectdestroyandreraise);

         tg.gettempofsizereferencepersistant(exprasmlist,12,tempaddr);
         tg.gettempofsizereferencepersistant(exprasmlist,24,tempbuf);
         cg.a_paramaddr_ref(exprasmlist,tempaddr,3);
         cg.a_paramaddr_ref(exprasmlist,tempbuf,2);
         cg.a_param_const(exprasmlist,OS_INT,1,1);
         cg.a_call_name(exprasmlist,'FPC_PUSHEXCEPTADDR');

         exprasmList.concat(Tairegalloc.Alloc(R_EAX));
         exprasmList.concat(Taicpu.op_reg(A_PUSH,S_L,R_EAX));
         exprasmList.concat(Tairegalloc.DeAlloc(R_EAX));
         cg.a_call_name(exprasmlist,'FPC_SETJMP');
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
              cg.g_maybe_loadself(exprasmlist);
              secondpass(right);
           end;
         getlabel(doobjectdestroy);
         cg.a_label(exprasmlist,doobjectdestroyandreraise);
         cg.a_call_name(exprasmlist,'FPC_POPADDRSTACK');
         tg.ungetpersistanttempreference(exprasmlist,tempaddr);
         tg.ungetpersistanttempreference(exprasmlist,tempbuf);

         exprasmList.concat(Tairegalloc.Alloc(R_EAX));
         exprasmList.concat(Taicpu.op_reg(A_POP,S_L,R_EAX));
         exprasmList.concat(Taicpu.op_reg_reg(A_TEST,S_L,R_EAX,R_EAX));
         exprasmList.concat(Tairegalloc.DeAlloc(R_EAX));
         emitjmp(C_E,doobjectdestroy);
         cg.a_call_name(exprasmlist,'FPC_POPSECONDOBJECTSTACK');
         exprasmList.concat(Tairegalloc.Alloc(R_EAX));
         emit_reg(A_PUSH,S_L,R_EAX);
         cg.a_call_name(exprasmlist,'FPC_DESTROYEXCEPTION');
         exprasmList.concat(Tairegalloc.DeAlloc(R_EAX));
         { we don't need to restore esi here because reraise never }
         { returns                                                 }
         cg.a_call_name(exprasmlist,'FPC_RERAISE');

         cg.a_label(exprasmlist,doobjectdestroy);
         cleanupobjectstack;
         { clear some stuff }
         tg.ungetiftemp(exprasmlist,ref);
         cg.a_jmp_always(exprasmlist,endexceptlabel);

         if assigned(right) then
           begin
              { special handling for control flow instructions }
              if fc_exit in flowcontrol then
                begin
                   { the address and object pop does secondtryexcept }
                   cg.a_label(exprasmlist,exitonlabel);
                   cg.a_jmp_always(exprasmlist,oldaktexitlabel);
                end;

              if fc_break in flowcontrol then
                begin
                   { the address and object pop does secondtryexcept }
                   cg.a_label(exprasmlist,breakonlabel);
                   cg.a_jmp_always(exprasmlist,oldaktbreaklabel);
                end;

              if fc_continue in flowcontrol then
                begin
                   { the address and object pop does secondtryexcept }
                   cg.a_label(exprasmlist,continueonlabel);
                   cg.a_jmp_always(exprasmlist,oldaktcontinuelabel);
                end;

              aktexitlabel:=oldaktexitlabel;
              aktexit2label:=oldaktexit2label;
              if assigned(oldaktbreaklabel) then
               begin
                 aktcontinuelabel:=oldaktcontinuelabel;
                 aktbreaklabel:=oldaktbreaklabel;
               end;
           end;

         cg.a_label(exprasmlist,nextonlabel);
         flowcontrol:=oldflowcontrol+flowcontrol;
         { next on node }
         if assigned(left) then
           begin
              rg.cleartempgen;
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
         oldaktbreaklabel : tasmlabel;
         oldflowcontrol,tryflowcontrol : tflowcontrol;
         decconst : longint;
         tempbuf,tempaddr : treference;

      begin
         { check if child nodes do a break/continue/exit }
         oldflowcontrol:=flowcontrol;
         flowcontrol:=[];
         { we modify EAX }
         include(rg.usedinproc,R_EAX);
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

         tg.gettempofsizereferencepersistant(exprasmlist,12,tempaddr);
         tg.gettempofsizereferencepersistant(exprasmlist,24,tempbuf);
         cg.a_paramaddr_ref(exprasmlist,tempaddr,3);
         cg.a_paramaddr_ref(exprasmlist,tempbuf,2);
         { Type of stack-frame must be pushed}
         cg.a_param_const(exprasmlist,OS_INT,1,1);
         cg.a_call_name(exprasmlist,'FPC_PUSHEXCEPTADDR');

         { allocate eax }
         exprasmList.concat(Tairegalloc.Alloc(R_EAX));
         emit_reg(A_PUSH,S_L,R_EAX);
         cg.a_call_name(exprasmlist,'FPC_SETJMP');
         emit_reg(A_PUSH,S_L,R_EAX);
         emit_reg_reg(A_TEST,S_L,R_EAX,R_EAX);
         { deallocate eax }
         exprasmList.concat(Tairegalloc.DeAlloc(R_EAX));
         emitjmp(C_NE,finallylabel);

         { try code }
         if assigned(left) then
           begin
              secondpass(left);
              tryflowcontrol:=flowcontrol;
              if codegenerror then
                exit;
           end;

         cg.a_label(exprasmlist,finallylabel);
         cg.a_call_name(exprasmlist,'FPC_POPADDRSTACK');
         tg.ungetpersistanttempreference(exprasmlist,tempaddr);
         tg.ungetpersistanttempreference(exprasmlist,tempbuf);

         { finally code }
         flowcontrol:=[];
         secondpass(right);
         if flowcontrol<>[] then
           CGMessage(cg_e_control_flow_outside_finally);
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
         cg.a_label(exprasmlist,reraiselabel);
         cg.a_call_name(exprasmlist,'FPC_RERAISE');
         { do some magic for exit,break,continue in the try block }
         if fc_exit in tryflowcontrol then
           begin
              cg.a_label(exprasmlist,exitfinallylabel);
              { allocate eax }
              exprasmList.concat(Tairegalloc.Alloc(R_EAX));
              emit_reg(A_POP,S_L,R_EAX);
              exprasmList.concat(Tairegalloc.Alloc(R_EAX));
              emit_const(A_PUSH,S_L,2);
              cg.a_jmp_always(exprasmlist,finallylabel);
           end;
         if fc_break in tryflowcontrol then
          begin
             cg.a_label(exprasmlist,breakfinallylabel);
             { allocate eax }
             exprasmList.concat(Tairegalloc.Alloc(R_EAX));
             emit_reg(A_POP,S_L,R_EAX);
             { deallocate eax }
             exprasmList.concat(Tairegalloc.DeAlloc(R_EAX));
             emit_const(A_PUSH,S_L,3);
             cg.a_jmp_always(exprasmlist,finallylabel);
           end;
         if fc_continue in tryflowcontrol then
           begin
              cg.a_label(exprasmlist,continuefinallylabel);
              exprasmList.concat(Tairegalloc.Alloc(R_EAX));
              emit_reg(A_POP,S_L,R_EAX);
              exprasmList.concat(Tairegalloc.Alloc(R_EAX));
              emit_const(A_PUSH,S_L,4);
              cg.a_jmp_always(exprasmlist,finallylabel);
           end;

         cg.a_label(exprasmlist,endfinallylabel);

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
        cg.a_jmp_always(exprasmlist,faillabel);
      end;


begin
   craisenode:=ti386raisenode;
   ctryexceptnode:=ti386tryexceptnode;
   ctryfinallynode:=ti386tryfinallynode;
   connode:=ti386onnode;
   cfailnode:=ti386failnode;
end.
{
  $Log$
  Revision 1.26  2002-05-18 13:34:25  peter
    * readded missing revisions

  Revision 1.25  2002/05/16 19:46:51  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.23  2002/05/12 16:53:17  peter
    * moved entry and exitcode to ncgutil and cgobj
    * foreach gets extra argument for passing local data to the
      iterator function
    * -CR checks also class typecasts at runtime by changing them
      into as
    * fixed compiler to cycle with the -CR option
    * fixed stabs with elf writer, finally the global variables can
      be watched
    * removed a lot of routines from cga unit and replaced them by
      calls to cgobj
    * u32bit-s32bit updates for and,or,xor nodes. When one element is
      u32bit then the other is typecasted also to u32bit without giving
      a rangecheck warning/error.
    * fixed pascal calling method with reversing also the high tree in
      the parast, detected by tcalcst3 test

  Revision 1.22  2002/04/04 19:06:11  peter
    * removed unused units
    * use tlocation.size in cg.a_*loc*() routines

  Revision 1.21  2002/04/02 17:11:36  peter
    * tlocation,treference update
    * LOC_CONSTANT added for better constant handling
    * secondadd splitted in multiple routines
    * location_force_reg added for loading a location to a register
      of a specified size
    * secondassignment parses now first the right and then the left node
      (this is compatible with Kylix). This saves a lot of push/pop especially
      with string operations
    * adapted some routines to use the new cg methods

  Revision 1.20  2002/03/31 20:26:38  jonas
    + a_loadfpu_* and a_loadmm_* methods in tcg
    * register allocation is now handled by a class and is mostly processor
      independent (+rgobj.pas and i386/rgcpu.pas)
    * temp allocation is now handled by a class (+tgobj.pas, -i386\tgcpu.pas)
    * some small improvements and fixes to the optimizer
    * some register allocation fixes
    * some fpuvaroffset fixes in the unary minus node
    * push/popusedregisters is now called rg.save/restoreusedregisters and
      (for i386) uses temps instead of push/pop's when using -Op3 (that code is
      also better optimizable)
    * fixed and optimized register saving/restoring for new/dispose nodes
    * LOC_FPU locations now also require their "register" field to be set to
      R_ST, not R_ST0 (the latter is used for LOC_CFPUREGISTER locations only)
    - list field removed of the tnode class because it's not used currently
      and can cause hard-to-find bugs

}
