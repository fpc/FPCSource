{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

    This unit handles the codegeneration pass

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
{$ifdef TP}
  {$E+,F+,N+}
{$endif}
unit pass_2;
interface

uses
  tree;

{ produces assembler for the expression in variable p }
{ and produces an assembler node at the end           }
procedure generatecode(var p : pnode);

{ produces the actual code }
function do_secondpass(p : pnode) : boolean;
procedure secondpass(p : pnode);


implementation

   uses
     globtype,systems,
     cobjects,verbose,comphook,globals,files,
     symconst,symtable,types,aasm,scanner,
     pass_1,tgobj,cgbase,cgobj,tgcpu,cpuasm,cpubase
{$ifdef GDB}
     ,gdb
{$endif}
     ;
   type
       perrornode = ^terrornode;

       terrornode = object(tnode)
          constructor init;
          procedure secondpass;virtual;
       end;

       tstatementnode = object(tbinarynode)
          procedure secondpass;virtual;
       end;

       tblocknode = object(tunarynode)
          procedure secondpass;virtual;
       end;

       tasmnode = object(tnode)
          p_asm : paasmoutput;
          object_preserved : boolean;
          procedure secondpass;virtual;
       end;

{****************************************************************************
                                 TERRORNODE
 ****************************************************************************}

    constructor terrornode.init;

      begin
         inherited init;
         treetype:=errorn;
      end;

    procedure terrornode.secondpass;

      begin
         error:=true;
         codegenerror:=true;
      end;

{****************************************************************************
                               TSTATEMENTNODE
 ****************************************************************************}

    procedure tstatementnode.secondpass;

      var
         hp : pbinarynode;
         oldrl : plinkedlist;

      begin
         hp:=@self;
         while assigned(hp) do
          begin
            if assigned(hp^.right) then
             begin
               tg.cleartempgen;
               oldrl:=temptoremove;
               temptoremove:=new(plinkedlist,init);
               hp^.right^.secondpass;
               { release temp. ansi strings }
               cg^.g_removetemps(exprasmlist,temptoremove);
               dispose(temptoremove,done);
               temptoremove:=oldrl;
             end;
            hp:=pbinarynode(hp^.left);
          end;
      end;

    procedure tblocknode.secondpass;
      begin
         { do second pass on left node }
         if assigned(left) then
           left^.secondpass;
      end;

    procedure tasmnode.secondpass;
      begin
         exprasmlist^.concatlist(p_asm);
         if not object_preserved then
           cg^.g_maybe_loadself(exprasmlist);
       end;

     function generateexprlist(p : pnode) : plinkedlist;

       var
          l : plinkedlist;

       begin
          l:=new(plinkedlist,init);
          p^.concattolist(l);
          generateexprlist:=l;
       end;

     procedure secondpass(p : pnode);

      var
         oldcodegenerror  : boolean;
         oldlocalswitches : tlocalswitches;
         oldpos           : tfileposinfo;
         l                : plinkedlist;
         hp : pnode;

      begin
         if not(p^.error) then
          begin
            oldcodegenerror:=codegenerror;
            oldlocalswitches:=aktlocalswitches;
            oldpos:=aktfilepos;

            aktfilepos:=p^.fileinfo;
            aktlocalswitches:=p^.localswitches;
            codegenerror:=false;

            { do we have a list of statements? }
            if p^.treetype=statementn then
              begin
                 l:=generateexprlist(p);
                 { here we should do CSE and node reordering }
                 hp:=pnode(l^.first);
                 while assigned(hp) do
                   begin
                      if assigned(hp^.parent) then
                        begin
                           if nf_needs_truefalselabel in hp^.parent^.flags then
                             begin
                                if not(assigned(punarynode(hp^.parent)^.truelabel)) then
                                  getlabel(punarynode(hp^.parent)^.truelabel);
                                if not(assigned(punarynode(hp^.parent)^.falselabel)) then
                                  getlabel(punarynode(hp^.parent)^.falselabel);
                                truelabel:=punarynode(hp^.parent)^.truelabel;
                                falselabel:=punarynode(hp^.parent)^.falselabel;
                             end;
                        end;
                      hp^.secondpass;
                      hp:=pnode(hp^.next);
                   end;
              end
            else
              p^.secondpass;

            p^.error:=codegenerror;
            codegenerror:=codegenerror or oldcodegenerror;
            aktlocalswitches:=oldlocalswitches;
            aktfilepos:=oldpos;
          end
         else
           codegenerror:=true;
      end;


    function do_secondpass(p : pnode) : boolean;

      begin
         codegenerror:=false;
         if not(p^.error) then
           secondpass(p);
         do_secondpass:=codegenerror;
      end;

    var
       regvars : array[1..maxvarregs] of pvarsym;
       regvars_para : array[1..maxvarregs] of boolean;
       regvars_refs : array[1..maxvarregs] of longint;
       parasym : boolean;

    procedure searchregvars(p : pnamedindexobject);
      var
         i,j,k : longint;
      begin
         if (pvarsym(p)^.typ=varsym) and (vo_regable in pvarsym(p)^.varoptions) then
           begin
              { walk through all momentary register variables }
              for i:=1 to maxvarregs do
                begin
                   { free register ? }
                   if regvars[i]=nil then
                     begin
                        regvars[i]:=pvarsym(p);
                        regvars_para[i]:=parasym;
                        break;
                     end;
                   { else throw out a variable ? }
                       j:=pvarsym(p)^.refs;
                   { parameter get a less value }
                   if parasym then
                     begin
                        if cs_littlesize in aktglobalswitches  then
                          dec(j,1)
                        else
                          dec(j,100);
                     end;
                   if (j>regvars_refs[i]) and (j>0) then
                     begin
                        for k:=maxvarregs-1 downto i do
                          begin
                             regvars[k+1]:=regvars[k];
                             regvars_para[k+1]:=regvars_para[k];
                          end;
                        { calc the new refs
                        pvarsym(p)^.refs:=j; }
                        regvars[i]:=pvarsym(p);
                        regvars_para[i]:=parasym;
                        regvars_refs[i]:=j;
                        break;
                     end;
                end;
           end;
      end;

    procedure generatecode(var p : pnode);
      var
         i       : longint;
         hr      : preference;
{$ifdef i386}
         regsize : topsize;
{$endif i386}

      label
         nextreg;
      begin
         temptoremove:=nil;
         tg.cleartempgen;
         { when size optimization only count occurrence }
         if cs_littlesize in aktglobalswitches then
           t_times:=1
         else
           { reference for repetition is 100 }
           t_times:=100;
         { clear register count }
         tg.clearregistercount;
         use_esp_stackframe:=false;

         if not(do_firstpassnode(p)) then
           begin
              { max. optimizations     }
              { only if no asm is used }
              { and no try statement   }
              if (cs_regalloc in aktglobalswitches) and
                ((procinfo^.flags and (pi_uses_asm or pi_uses_exceptions))=0) then
                begin
                   { can we omit the stack frame ? }
                   { conditions:
                     1. procedure (not main block)
                     2. no constructor or destructor
                     3. no call to other procedures
                     4. no interrupt handler
                   }
                   if assigned(aktprocsym) then
                     begin
                       if not(aktprocsym^.definition^.proctypeoption in [potype_constructor,potype_destructor]) and
                          not(po_interrupt in aktprocsym^.definition^.procoptions) and
                          ((procinfo^.flags and pi_do_call)=0) and
                          (lexlevel>=normal_function_level) then
                       begin
                         { use ESP as frame pointer }
                         procinfo^.framepointer:=stack_pointer;
                         use_esp_stackframe:=true;

                         { calc parameter distance new }
                         dec(procinfo^.framepointer_offset,pointersize);
                         dec(procinfo^.selfpointer_offset,pointersize);

                         { is this correct ???}
                         { retoffset can be negativ for results in eax !! }
                         { the value should be decreased only if positive }
                         if procinfo^.return_offset>=0 then
                           dec(procinfo^.return_offset,4);

                         dec(procinfo^.call_offset,4);
                         aktprocsym^.definition^.parast^.address_fixup:=procinfo^.call_offset;
                       end;
                     end;
                   if (p^.registersint<maxvarregs) then
                       begin
                        for i:=1 to maxvarregs do
                          regvars[i]:=nil;
                        parasym:=false;
                      {$ifdef tp}
                        symtablestack^.foreach(searchregvars);
                      {$else}
                        symtablestack^.foreach(@searchregvars);
                      {$endif}
                        { copy parameter into a register ? }
                        parasym:=true;
                      {$ifdef tp}
                        symtablestack^.next^.foreach(searchregvars);
                      {$else}
                        symtablestack^.next^.foreach(@searchregvars);
                      {$endif}
                        { hold needed registers free }
                        for i:=maxvarregs downto maxvarregs-p^.registersint+1 do
                          regvars[i]:=nil;
                        { now assign register }
                        for i:=1 to maxvarregs-p^.registersint do
                          begin
                             if assigned(regvars[i]) then
                               begin
                                  { it is nonsens, to copy the variable to }
                                  { a register because we need then much   }
                                  { pushes ?                               }
                                  if tg.reg_pushes[varregs[i]]>=regvars[i]^.refs then
                                    begin
                                       regvars[i]:=nil;
                                       goto nextreg;
                                    end;

                                  { register is no longer available for }
                                  { expressions                         }
                                  { search the register which is the most }
                                  { unused                                }
                                  exclude(tg.availabletempregsint,varregs[i]);
                                  tg.is_reg_var[varregs[i]]:=true;
                                  dec(tg.c_countusableregsint);

                                  { possibly no 32 bit register are needed }
                                  { call by reference/const ? }
                                  {!!!!!!!!!!!!!!
                                  if (regvars[i]^.varspez=vs_var) or
                                     ((regvars[i]^.varspez=vs_const) and
                                       dont_copy_const_param(regvars[i]^.definition)) then
                                    begin
                                       regvars[i]^.reg:=varregs[i];
                                       regsize:=sizepostfix_pointer;
                                    end
                                  else
                                   if (regvars[i]^.definition^.deftype=orddef) and
                                      (porddef(regvars[i]^.definition)^.size=1) then
                                    begin
                                       regvars[i]^.reg:=regtoreg8(varregs[i]);
                                       regsize:=S_B;
                                    end
                                  else
                                   if (regvars[i]^.definition^.deftype=orddef) and
                                      (porddef(regvars[i]^.definition)^.size=2) then
                                    begin
                                       regvars[i]^.reg:=regtoreg16(varregs[i]);
                                       regsize:=S_W;
                                    end
                                  else
                                   if (regvars[i]^.definition^.deftype=orddef) and
                                      (porddef(regvars[i]^.definition)^.size=4) then
                                    begin
                                       regvars[i]^.reg:=regtoreg32(varregs[i]);
                                       regsize:=S_L;
                                    end
                                  else
                                   if (cf_registers64 in cpuflags) and
                                      (regvars[i]^.definition^.deftype=orddef) and
                                      (porddef(regvars[i]^.definition)^.size=8) then
                                    begin
                                       regvars[i]^.reg:=regtoreg64(varregs[i]);
                                       regsize:=S_Q;
                                    end;
                                  }
                                  { parameter must be load }
                                  if regvars_para[i] then
                                    begin
                                       { procinfo is there actual,      }
                                       { because we can't never be in a }
                                       { nested procedure               }
                                       { when loading parameter to reg  }
                                       new(hr);
                                       reset_reference(hr^);
                                       hr^.offset:=pvarsym(regvars[i])^.address+procinfo^.call_offset;
                                       hr^.base:=procinfo^.framepointer;
{$ifdef i386}
                                       procinfo^.aktentrycode^.concat(new(paicpu,op_ref_reg(A_MOV,regsize,
                                         hr,regvars[i]^.reg)));
{$endif i386}
{$ifdef m68k}
                                       procinfo.aktentrycode^.concat(new(pai68k,op_ref_reg(A_MOVE,regsize,
                                         hr,regvars[i]^.reg)));
{$endif m68k}
                                       tg.unusedregsint:=tg.unusedregsint - [regvars[i]^.reg];
                                    end;
                                  { procedure uses this register }
                                  include(tg.usedinproc,varregs[i]);
                               end;
                             nextreg:
{$ifdef i386}
                               { dummy }
                               regsize:=S_W;
{$endif i386}
                          end;
                        if (status.verbosity and v_debug)=v_debug then
                          begin
                             for i:=1 to maxvarregs do
                               begin
                                  if assigned(regvars[i]) then
                                    Message3(cg_d_register_weight,reg2str(regvars[i]^.reg),
                                           tostr(regvars[i]^.refs),regvars[i]^.name);
                               end;
                          end;
                     end;
                end;
              if assigned(aktprocsym) and
                 (pocall_inline in aktprocsym^.definition^.proccalloptions) then
                make_const_global:=true;

              do_secondpass(p);

              if assigned(procinfo^.def) then
                procinfo^.def^.fpu_used:=p^.registersfpu;

              { all registers can be used again }
              tg.resetusableregisters;
           end;
         procinfo^.aktproccode^.concatlist(exprasmlist);
         make_const_global:=false;
      end;

end.
{
  $Log$
  Revision 1.10  2000-01-07 01:14:54  peter
    * updated copyright to 2000

  Revision 1.9  1999/12/06 18:17:10  peter
    * newcg compiler compiles again

  Revision 1.8  1999/10/12 21:20:47  florian
    * new codegenerator compiles again

  Revision 1.7  1999/08/25 12:00:13  jonas
    * changed pai386, paippc and paiapha (same for tai*) to paicpu (taicpu)

  Revision 1.6  1999/08/05 14:58:15  florian
    * some fixes for the floating point registers
    * more things for the new code generator

  Revision 1.5  1999/08/04 00:23:58  florian
    * renamed i386asm and i386base to cpuasm and cpubase

  Revision 1.4  1999/08/03 17:09:46  florian
    * the alpha compiler can be compiled now

  Revision 1.3  1999/08/03 00:30:36  florian
    * again a fix for the alpha

  Revision 1.2  1999/08/03 00:28:03  florian
    * some updates to compile for the alpha

  Revision 1.1  1999/08/03 00:07:16  florian
    * initial revision

}
