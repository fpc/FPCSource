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
{$ifdef FPC}
  {$goto on}
{$endif FPC}
{$ifdef TP}
  {$E+,F+,N+}
{$endif}
unit pass_2;
interface

uses
  tree;

{ produces assembler for the expression in variable p }
{ and produces an assembler node at the end        }
procedure generatecode(var p : ptree);

{ produces the actual code }
function do_secondpass(var p : ptree) : boolean;
procedure secondpass(var p : ptree);


implementation

   uses
     globtype,systems,
     cobjects,comphook,verbose,globals,files,
     symconst,symtable,types,aasm,scanner,
     pass_1,hcodegen,temp_gen,cpubase,cpuasm,regvars
{$ifndef newcg}
     ,tcflw
{$endif newcg}
{$ifdef GDB}
     ,gdb
{$endif}
{$ifdef i386}
     ,tgeni386,cgai386
     ,cg386con,cg386mat,cg386cnv,cg386set,cg386add
     ,cg386mem,cg386cal,cg386ld,cg386flw,cg386inl
{$endif}
{$ifdef m68k}
     ,tgen68k,cga68k
     ,cg68kcon,cg68kmat,cg68kcnv,cg68kset,cg68kadd
     ,cg68kmem,cg68kcal,cg68kld,cg68kflw,cg68kinl
{$endif}
     ;

{*****************************************************************************
                              SecondPass
*****************************************************************************}

    type
       secondpassproc = procedure(var p : ptree);

    procedure secondnothing(var p : ptree);

      begin
      end;

    procedure seconderror(var p : ptree);

      begin
         p^.error:=true;
         codegenerror:=true;
      end;


    procedure secondstatement(var p : ptree);

      var
         hp : ptree;
      begin
         hp:=p;
         while assigned(hp) do
          begin
            if assigned(hp^.right) then
             begin
               cleartempgen;
               {!!!!!!
               oldrl:=temptoremove;
               temptoremove:=new(plinkedlist,init);
               }
               secondpass(hp^.right);
               { !!!!!!!
                 some temporary data which can't be released elsewhere
               removetemps(exprasmlist,temptoremove);
               dispose(temptoremove,done);
               temptoremove:=oldrl;
               }
             end;
            hp:=hp^.left;
          end;
      end;


    procedure secondblockn(var p : ptree);
      begin
      { do second pass on left node }
        if assigned(p^.left) then
         secondpass(p^.left);
      end;


    procedure secondasm(var p : ptree);

        procedure ReLabel(var p:pasmsymbol);
        begin
          if p^.proclocal then
           begin
             if not assigned(p^.altsymbol) then
              p^.GenerateAltSymbol;
             p:=p^.altsymbol;
           end;
        end;

      var
        hp,hp2 : pai;
        localfixup,parafixup,
        i : longint;
        r : preference;
        skipnode : boolean;
      begin
         if inlining_procedure then
           begin
             localfixup:=aktprocsym^.definition^.localst^.address_fixup;
             parafixup:=aktprocsym^.definition^.parast^.address_fixup;
             ResetAsmSymbolListAltSymbol;
             hp:=pai(p^.p_asm^.first);
             while assigned(hp) do
              begin
                hp2:=pai(hp^.getcopy);
                skipnode:=false;
                case hp2^.typ of
                  ait_label :
                     begin
                       { regenerate the labels by setting altsymbol }
                       ReLabel(pasmsymbol(pai_label(hp2)^.l));
                     end;
                  ait_const_rva,
                  ait_const_symbol :
                     begin
                       ReLabel(pai_const_symbol(hp2)^.sym);
                     end;
                  ait_instruction :
                     begin
{$ifdef i386}
                       { fixup the references }
                       for i:=1 to paicpu(hp2)^.ops do
                        case paicpu(hp2)^.oper[i-1].typ of
                          top_ref :
                            begin
                              r:=paicpu(hp2)^.oper[i-1].ref;
                              case r^.options of
                                ref_parafixup :
                                  r^.offsetfixup:=parafixup;
                                ref_localfixup :
                                  r^.offsetfixup:=localfixup;
                              end;
                              if assigned(r^.symbol) then
                               ReLabel(r^.symbol);
                            end;
                          top_symbol :
                            begin
                              ReLabel(paicpu(hp2)^.oper[i-1].sym);
                            end;
                         end;
{$endif i386}
                     end;
                   ait_marker :
                     begin
                     { it's not an assembler block anymore }
                       if (pai_marker(hp2)^.kind in [AsmBlockStart, AsmBlockEnd]) then
                        skipnode:=true;
                     end;
                   else
                end;
                if not skipnode then
                 exprasmlist^.concat(hp2)
                else
                 dispose(hp2,done);
                hp:=pai(hp^.next);
              end
           end
         else
           begin
             { if the routine is an inline routine, then we must hold a copy
               becuase it can be necessary for inlining later }
             if (pocall_inline in aktprocsym^.definition^.proccalloptions) then
               exprasmlist^.concatlistcopy(p^.p_asm)
             else
               exprasmlist^.concatlist(p^.p_asm);
           end;
         if not p^.object_preserved then
          begin
{$ifdef i386}
            maybe_loadesi;
{$endif}
{$ifdef m68k}
            maybe_loada5;
{$endif}
          end;
       end;

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

     procedure secondpass(var p : ptree);
       const
         procedures : array[ttreetyp] of secondpassproc =
            (secondadd,  {addn}
             secondadd,  {muln}
             secondadd,  {subn}
             secondmoddiv,      {divn}
             secondadd,  {symdifn}
             secondmoddiv,      {modn}
             secondassignment,  {assignn}
             secondload,        {loadn}
             secondnothing,     {range}
             secondadd,  {ltn}
             secondadd,  {lten}
             secondadd,  {gtn}
             secondadd,  {gten}
             secondadd,  {equaln}
             secondadd,  {unequaln}
             secondin,    {inn}
             secondadd,  {orn}
             secondadd,  {xorn}
             secondshlshr,      {shrn}
             secondshlshr,      {shln}
             secondadd,  {slashn}
             secondadd,  {andn}
             secondsubscriptn,  {subscriptn}
             secondderef,       {derefn}
             secondaddr,        {addrn}
             seconddoubleaddr,  {doubleaddrn}
             secondordconst,    {ordconstn}
             secondtypeconv,    {typeconvn}
             secondcalln,       {calln}
             secondnothing,     {callparan}
             secondrealconst,   {realconstn}
             secondfixconst,    {fixconstn}
             secondunaryminus,  {unaryminusn}
             secondasm,         {asmn}
             secondvecn,        {vecn}
             secondpointerconst, {pointerconstn}
             secondstringconst, {stringconstn}
             secondfuncret,     {funcretn}
             secondselfn,       {selfn}
             secondnot,  {notn}
             secondinline,      {inlinen}
             secondniln,        {niln}
             seconderror,       {errorn}
             secondnothing,     {typen}
             secondhnewn,       {hnewn}
             secondhdisposen,   {hdisposen}
             secondnewn,        {newn}
             secondsimplenewdispose, {simpledisposen}
             secondsetelement,  {setelementn}
             secondsetconst,    {setconstn}
             secondblockn,      {blockn}
             secondstatement,   {statementn}
             secondnothing,     {loopn}
             secondifn,  {ifn}
             secondbreakn,      {breakn}
             secondcontinuen,   {continuen}
             second_while_repeatn, {repeatn}
             second_while_repeatn, {whilen}
             secondfor,  {forn}
             secondexitn,       {exitn}
             secondwith,        {withn}
             secondcase,        {casen}
             secondlabel,       {labeln}
             secondgoto,        {goton}
             secondsimplenewdispose, {simplenewn}
             secondtryexcept,   {tryexceptn}
             secondraise,       {raisen}
             secondnothing,     {switchesn}
             secondtryfinally,  {tryfinallyn}
             secondon,    {onn}
             secondis,    {isn}
             secondas,    {asn}
             seconderror,       {caretn}
             secondfail,        {failn}
             secondadd,  {starstarn}
             secondprocinline,  {procinlinen}
             secondarrayconstruct, {arrayconstructn}
             secondnothing,     {arrayconstructrangen}
             secondnothing,     {nothingn}
             secondloadvmt      {loadvmtn}
             );
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
         if not(p^.error) then
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
            aktfilepos:=p^.fileinfo;
            aktlocalswitches:=p^.localswitches;
            codegenerror:=false;
{$ifdef logsecondpass}
            logsecond('second'+secondnames[p^.treetype],true);
{$endif logsecondpass}
            procedures[p^.treetype](p);
{$ifdef logsecondpass}
            logsecond('second'+secondnames[p^.treetype],false);
{$endif logsecondpass}
            p^.error:=codegenerror;

            codegenerror:=codegenerror or oldcodegenerror;
            aktlocalswitches:=oldlocalswitches;
            aktfilepos:=oldpos;
{$ifdef TEMPREGDEBUG}
            curptree:=prevp;
{$endif TEMPREGDEBUG}
{$ifdef EXTTEMPREGDEBUG}
            if p^.usableregs-usablereg32>p^.reallyusedregs then
              p^.reallyusedregs:=p^.usableregs-usablereg32;
            if p^.reallyusedregs<p^.registers32 then
              Comment(V_Debug,'registers32 overestimated '+tostr(p^.registers32)+
                '>'+tostr(p^.reallyusedregs));
{$endif EXTTEMPREGDEBUG}
          end
         else
           codegenerror:=true;
      end;


    function do_secondpass(var p : ptree) : boolean;
      begin
         codegenerror:=false;
         if not(p^.error) then
           secondpass(p);
         do_secondpass:=codegenerror;
      end;

    procedure clearrefs(p : pnamedindexobject);

      begin
         if (psym(p)^.typ=varsym) then
           if pvarsym(p)^.refs>1 then
             pvarsym(p)^.refs:=1;
      end;

    procedure generatecode(var p : ptree);
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
                procinfo^.def^.fpu_used:=p^.registersfpu;

           end;
         procinfo^.aktproccode^.concatlist(exprasmlist);
         make_const_global:=false;
      end;

end.
{
  $Log$
  Revision 1.5  2000-08-03 13:17:25  jonas
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
