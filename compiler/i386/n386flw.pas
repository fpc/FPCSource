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
      cpubase,cpuasm,
      nld,ncon,
      tainst,cga,tgobj,rgobj;

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
       endexceptlabel : tasmlabel;

    { does the necessary things to clean up the object stack }
    { in the except block                                    }
    procedure cleanupobjectstack;

      begin
         emitcall('FPC_POPOBJECTSTACK');
         exprasmList.concat(Tairegalloc.Alloc(R_EAX));
         emit_reg(A_PUSH,S_L,R_EAX);
         emitcall('FPC_DESTROYEXCEPTION');
         exprasmList.concat(Tairegalloc.DeAlloc(R_EAX));
         maybe_loadself;
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

         flowcontrol:=[];
         secondpass(left);
         tryflowcontrol:=flowcontrol;
         if codegenerror then
           goto errorexit;

         emitlab(exceptlabel);
         emitcall('FPC_POPADDRSTACK');
         tg.ungetpersistanttempreference(exprasmlist,tempaddr);
         tg.ungetpersistanttempreference(exprasmlist,tempbuf);

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
           secondpass(right);

         emitlab(lastonlabel);
         { default handling except handling }
         if assigned(t1) then
           begin
              { FPC_CATCHES must be called with
                'default handler' flag (=-1)
              }
              push_int (-1);
              emitcall('FPC_CATCHES');
              maybe_loadself;

              { the destruction of the exception object must be also }
              { guarded by an exception frame                        }
              getlabel(doobjectdestroy);
              getlabel(doobjectdestroyandreraise);

              tg.gettempofsizereferencepersistant(exprasmlist,12,tempaddr);
              tg.gettempofsizereferencepersistant(exprasmlist,24,tempbuf);
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

              { here we don't have to reset flowcontrol           }
              { the default and on flowcontrols are handled equal }
              secondpass(t1);
              exceptflowcontrol:=flowcontrol;

              emitlab(doobjectdestroyandreraise);
              emitcall('FPC_POPADDRSTACK');
              tg.ungetpersistanttempreference(exprasmlist,tempaddr);
              tg.ungetpersistanttempreference(exprasmlist,tempbuf);

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
         emitcall('FPC_CATCHES');
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
              maybe_loadself;
              secondpass(right);
           end;
         getlabel(doobjectdestroy);
         emitlab(doobjectdestroyandreraise);
         emitcall('FPC_POPADDRSTACK');
         tg.ungetpersistanttempreference(exprasmlist,tempaddr);
         tg.ungetpersistanttempreference(exprasmlist,tempbuf);

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
         tg.ungetiftemp(exprasmlist,ref);
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
              secondpass(left);
              tryflowcontrol:=flowcontrol;
              if codegenerror then
                exit;
           end;

         emitlab(finallylabel);
         emitcall('FPC_POPADDRSTACK');
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
   craisenode:=ti386raisenode;
   ctryexceptnode:=ti386tryexceptnode;
   ctryfinallynode:=ti386tryfinallynode;
   connode:=ti386onnode;
   cfailnode:=ti386failnode;
end.
{
  $Log$
  Revision 1.21  2002-04-02 17:11:36  peter
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

  Revision 1.19  2001/12/29 15:29:58  jonas
    * powerpc/cgcpu.pas compiles :)
    * several powerpc-related fixes
    * cpuasm unit is now based on common tainst unit
    + nppcmat unit for powerpc (almost complete)

  Revision 1.17  2001/09/29 21:34:04  jonas
    - removed unused code (replaced by processor independent code)

  Revision 1.16  2001/09/28 20:39:33  jonas
    * changed all flow control structures (except for exception handling
      related things) to processor independent code (in new ncgflw unit)
    + generic cgobj unit which contains lots of code generator helpers with
      global "cg" class instance variable
    + cgcpu unit for i386 (implements processor specific routines of the above
      unit)
    * updated cgbase and cpubase for the new code generator units
    * include ncgflw unit in cpunode unit

  Revision 1.15  2001/08/26 13:36:58  florian
    * some cg reorganisation
    * some PPC updates

  Revision 1.14  2001/08/06 21:40:50  peter
    * funcret moved from tprocinfo to tprocdef

  Revision 1.13  2001/07/01 20:16:20  peter
    * alignmentinfo record added
    * -Oa argument supports more alignment settings that can be specified
      per type: PROC,LOOP,VARMIN,VARMAX,CONSTMIN,CONSTMAX,RECORDMIN
      RECORDMAX,LOCALMIN,LOCALMAX. It is possible to set the mimimum
      required alignment and the maximum usefull alignment. The final
      alignment will be choosen per variable size dependent on these
      settings

  Revision 1.12  2001/04/15 09:48:31  peter
    * fixed crash in labelnode
    * easier detection of goto and label in try blocks

  Revision 1.11  2001/04/14 14:07:11  peter
    * moved more code from pass_1 to det_resulttype

  Revision 1.10  2001/04/13 01:22:19  peter
    * symtable change to classes
    * range check generation and errors fixed, make cycle DEBUG=1 works
    * memory leaks fixed

  Revision 1.9  2001/04/02 21:20:37  peter
    * resulttype rewrite

  Revision 1.8  2001/01/27 21:29:35  florian
     * behavior -Oa optimized

  Revision 1.7  2001/01/06 23:35:05  jonas
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
