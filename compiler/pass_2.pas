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
unit pass_2;

{$i defines.inc}

interface

uses
   node;

    type
       tenumflowcontrol = (fc_exit,fc_break,fc_continue);
       tflowcontrol = set of tenumflowcontrol;

    var
       flowcontrol : tflowcontrol;
{ produces assembler for the expression in variable p }
{ and produces an assembler node at the end        }
procedure generatecode(var p : tnode);

{ produces the actual code }
function do_secondpass(var p : tnode) : boolean;
procedure secondpass(var p : tnode);


implementation

   uses
     globtype,systems,
     cobjects,globals,
     symconst,symbase,symtype,symsym,aasm,
     pass_1,hcodegen,temp_gen,regvars,nflw,tgcpu;

{*****************************************************************************
                              SecondPass
*****************************************************************************}

{$ifdef logsecondpass}
     procedure logsecond(const s: string; entry: boolean);
     var p: pchar;
     begin
       if entry then
         p := strpnew(s+' (entry)')
       else p := strpnew(s+' (exit)');
       exprasmlist^.concat(new(pai_asm_comment,init(p)));
     end;
{$endif logsecondpass}

     procedure secondpass(var p : tnode);
{$ifdef logsecondpass}
      secondnames: array[ttreetyp] of string[13] =
            ('add-addn',  {addn}
             'add-muln)',  {muln}
             'add-subn',  {subn}
             'moddiv-divn',      {divn}
             'add-symdifn',      {symdifn}
             'moddiv-modn',      {modn}
             'assignment',  {assignn}
             'load',        {loadn}
             'nothing-range',     {range}
             'add-ltn',  {ltn}
             'add-lten',  {lten}
             'add-gtn',  {gtn}
             'add-gten',  {gten}
             'add-equaln',  {equaln}
             'add-unequaln',  {unequaln}
             'in',    {inn}
             'add-orn',  {orn}
             'add-xorn',  {xorn}
             'shlshr-shrn',      {shrn}
             'shlshr-shln',      {shln}
             'add-slashn',  {slashn}
             'add-andn',  {andn}
             'subscriptn',  {subscriptn}
             'dderef',       {derefn}
             'addr',        {addrn}
             'doubleaddr',  {doubleaddrn}
             'ordconst',    {ordconstn}
             'typeconv',    {typeconvn}
             'calln',       {calln}
             'nothing-callp',     {callparan}
             'realconst',   {realconstn}
             'fixconst',    {fixconstn}
             'unaryminus',  {unaryminusn}
             'asm',         {asmn}
             'vecn',        {vecn}
             'pointerconst', {pointerconstn}
             'stringconst', {stringconstn}
             'funcret',     {funcretn}
             'selfn',       {selfn}
             'not',  {notn}
             'inline',      {inlinen}
             'niln',        {niln}
             'error',       {errorn}
             'nothing-typen',     {typen}
             'hnewn',       {hnewn}
             'hdisposen',   {hdisposen}
             'newn',        {newn}
             'simplenewDISP', {simpledisposen}
             'setelement',  {setelementn}
             'setconst',    {setconstn}
             'blockn',      {blockn}
             'statement',   {statementn}
             'nothing-loopn',     {loopn}
             'ifn',  {ifn}
             'breakn',      {breakn}
             'continuen',   {continuen}
             '_while_REPEAT', {repeatn}
             '_WHILE_repeat', {whilen}
             'for',  {forn}
             'exitn',       {exitn}
             'with',        {withn}
             'case',        {casen}
             'label',       {labeln}
             'goto',        {goton}
             'simpleNEWdisp', {simplenewn}
             'tryexcept',   {tryexceptn}
             'raise',       {raisen}
             'nothing-swtch',     {switchesn}
             'tryfinally',  {tryfinallyn}
             'on',    {onn}
             'is',    {isn}
             'as',    {asn}
             'error-caret',       {caretn}
             'fail',        {failn}
             'add-startstar',  {starstarn}
             'procinline',  {procinlinen}
             'arrayconstruc', {arrayconstructn}
             'noth-arrcnstr',     {arrayconstructrangen}
             'nothing-nothg',     {nothingn}
             'loadvmt'      {loadvmtn}
             );

{$endif logsecondpass}
      var
         oldcodegenerror  : boolean;
         oldlocalswitches : tlocalswitches;
         oldpos    : tfileposinfo;
{$ifdef TEMPREGDEBUG}
         prevp : pptree;
{$endif TEMPREGDEBUG}
      begin
         if not(nf_error in p.flags) then
          begin
            oldcodegenerror:=codegenerror;
            oldlocalswitches:=aktlocalswitches;
            oldpos:=aktfilepos;
{$ifdef TEMPREGDEBUG}
            testregisters32;
            prevp:=curptree;
            curptree:=@p;
            p^.usableregs:=usablereg32;
{$endif TEMPREGDEBUG}
            aktfilepos:=p.fileinfo;
            aktlocalswitches:=p.localswitches;
            codegenerror:=false;
{$ifdef logsecondpass}
            logsecond('second'+secondnames[p.nodetype],true);
{$endif logsecondpass}
            p.pass_2;
{$ifdef logsecondpass}
            logsecond('second'+secondnames[p.nodetype],false);
{$endif logsecondpass}
            if codegenerror then
              include(p.flags,nf_error);

            codegenerror:=codegenerror or oldcodegenerror;
            aktlocalswitches:=oldlocalswitches;
            aktfilepos:=oldpos;
{$ifdef TEMPREGDEBUG}
            curptree:=prevp;
{$endif TEMPREGDEBUG}
{$ifdef EXTTEMPREGDEBUG}
            if p.usableregs-usablereg32>p.reallyusedregs then
              p.reallyusedregs:=p.usableregs-usablereg32;
            if p.reallyusedregs<p.registers32 then
              Comment(V_Debug,'registers32 overestimated '+tostr(p^.registers32)+
                '>'+tostr(p^.reallyusedregs));
{$endif EXTTEMPREGDEBUG}
          end
         else
           codegenerror:=true;
      end;


    function do_secondpass(var p : tnode) : boolean;
      begin
         codegenerror:=false;
         if not(nf_error in p.flags) then
           secondpass(p);
         do_secondpass:=codegenerror;
      end;

    procedure clearrefs(p : pnamedindexobject);

      begin
         if (psym(p)^.typ=varsym) then
           if pvarsym(p)^.refs>1 then
             pvarsym(p)^.refs:=1;
      end;

    procedure generatecode(var p : tnode);
      begin
         cleartempgen;
         flowcontrol:=[];
         { when size optimization only count occurrence }
         if cs_littlesize in aktglobalswitches then
           t_times:=1
         else
           { reference for repetition is 100 }
           t_times:=100;
         { clear register count }
         clearregistercount;
         use_esp_stackframe:=false;
         aktexceptblock:=nil;
         symtablestack^.foreach(@clearrefs);
         symtablestack^.next^.foreach(@clearrefs);
         if not(do_firstpass(p)) then
           begin
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
                                   {!!!!!! this doesn work yet, because of problems with
                                      with linux and windows
                                   }
                                   (*
                                   if assigned(aktprocsym) then
                                     begin
                                       if not(assigned(procinfo^._class)) and
                                          not(aktprocsym^.definition^.proctypeoption in [potype_constructor,potype_destructor]) and
                                          not(po_interrupt in aktprocsym^.definition^.procoptions) and
                                          ((procinfo^.flags and pi_do_call)=0) and
                                          (lexlevel>=normal_function_level) then
                                         begin
                                          { use ESP as frame pointer }
                                           procinfo^.framepointer:=stack_pointer;
                                           use_esp_stackframe:=true;

                                          { calc parameter distance new }
                                           dec(procinfo^.framepointer_offset,4);
                                           dec(procinfo^.selfpointer_offset,4);

                                          { is this correct ???}
                                          { retoffset can be negativ for results in eax !! }
                                          { the value should be decreased only if positive }
                                           if procinfo^.retoffset>=0 then
                                             dec(procinfo^.retoffset,4);

                                           dec(procinfo^.para_offset,4);
                                           aktprocsym^.definition^.parast^.address_fixup:=procinfo^.para_offset;
                                         end;
                                     end;
                                    *)
                                  end;
              { process register variable stuff (JM) }
              assign_regvars(p);
              load_regvars(procinfo^.aktentrycode,p);
              cleanup_regvars(procinfo^.aktexitcode);

              if assigned(aktprocsym) and
                 (pocall_inline in aktprocsym^.definition^.proccalloptions) then
                make_const_global:=true;
              do_secondpass(p);

              if assigned(procinfo^.def) then
                procinfo^.def^.fpu_used:=p.registersfpu;

           end;
         procinfo^.aktproccode.concatlist(exprasmlist);
         make_const_global:=false;
      end;

end.
{
  $Log$
  Revision 1.12  2000-12-25 00:07:27  peter
    + new tlinkedlist class (merge of old tstringqueue,tcontainer and
      tlinkedlist objects)

  Revision 1.11  2000/11/29 00:30:35  florian
    * unused units removed from uses clause
    * some changes for widestrings

  Revision 1.10  2000/10/31 22:02:49  peter
    * symtable splitted, no real code changes

  Revision 1.9  2000/10/14 10:14:51  peter
    * moehrendorf oct 2000 rewrite

  Revision 1.8  2000/09/24 15:06:21  peter
    * use defines.inc

  Revision 1.7  2000/08/27 16:11:51  peter
    * moved some util functions from globals,cobjects to cutils
    * splitted files into finput,fmodule

  Revision 1.6  2000/08/12 15:34:22  peter
    + usedasmsymbollist to check and reset only the used symbols (merged)

  Revision 1.5  2000/08/03 13:17:25  jonas
    + allow regvars to be used inside inlined procs, which required  the
      following changes:
        + load regvars in genentrycode/free them in genexitcode (cgai386)
        * moved all regvar related code to new regvars unit
        + added pregvarinfo type to hcodegen
        + added regvarinfo field to tprocinfo (symdef/symdefh)
        * deallocate the regvars of the caller in secondprocinline before
          inlining the called procedure and reallocate them afterwards

  Revision 1.4  2000/08/03 11:15:42  jonas
    - disable regvars for inlined procedures (merged from fixes branch)

  Revision 1.3  2000/07/21 15:14:02  jonas
    + added is_addr field for labels, if they are only used for getting the address
       (e.g. for io checks) and corresponding getaddrlabel() procedure

  Revision 1.2  2000/07/13 11:32:44  michael
  + removed logs

}
