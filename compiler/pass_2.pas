{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

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

{$i fpcdefs.inc}

interface

uses
   node;

    type
       tenumflowcontrol = (fc_exit,fc_break,fc_continue);
       tflowcontrol = set of tenumflowcontrol;

    var
       allow_multi_pass2 : boolean;
       flowcontrol : tflowcontrol;

{ produces assembler for the expression in variable p }
{ and produces an assembler node at the end        }
procedure generatecode(var p : tnode);

{ produces the actual code }
function do_secondpass(var p : tnode) : boolean;
procedure secondpass(var p : tnode);


implementation

   uses
{$ifdef EXTDEBUG}
     cutils,
{$endif}
     globtype,systems,verbose,
     cclasses,globals,
     symconst,symbase,symtype,symsym,paramgr,
     aasmbase,aasmtai,
     pass_1,cpubase,cginfo,cgbase,
{$ifdef EXTDEBUG}
     cgobj,
{$endif EXTDEBUG}
     regvars,nflw,rgobj;

{*****************************************************************************
                              SecondPass
*****************************************************************************}

{$ifdef EXTDEBUG}
     procedure logsecond(ht:tnodetype; entry: boolean);
       const
         secondnames: array[tnodetype] of string[13] =
            ('<emptynode>',
             'add-addn',  {addn}
             'add-muln',  {muln}
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
             'noth-callpar',{callparan}
             'realconst',   {realconstn}
             'unaryminus',  {unaryminusn}
             'asm',         {asmn}
             'vecn',        {vecn}
             'pointerconst',{pointerconstn}
             'stringconst', {stringconstn}
             'not',         {notn}
             'inline',      {inlinen}
             'niln',        {niln}
             'error',       {errorn}
             'nothing-typen',     {typen}
             'setelement',  {setelementn}
             'setconst',    {setconstn}
             'blockn',      {blockn}
             'statement',   {statementn}
             'ifn',         {ifn}
             'breakn',      {breakn}
             'continuen',   {continuen}
             'while_repeat', {whilerepeatn}
             'for',         {forn}
             'exitn',       {exitn}
             'with',        {withn}
             'case',        {casen}
             'label',       {labeln}
             'goto',        {goton}
             'tryexcept',   {tryexceptn}
             'raise',       {raisen}
             'tryfinally',  {tryfinallyn}
             'on',    {onn}
             'is',    {isn}
             'as',    {asn}
             'error-caret',       {caretn}
             'add-starstar',  {starstarn}
             'arrayconstruc', {arrayconstructn}
             'noth-arrcnstr',     {arrayconstructrangen}
             'tempcreaten',
             'temprefn',
             'tempdeleten',
             'addoptn',
             'nothing-nothg',     {nothingn}
             'loadvmt',      {loadvmtn}
             'guidconstn',
             'rttin'
             );
      var
        p: pchar;
      begin
        if entry then
          p := strpnew('second '+secondnames[ht]+' (entry)')
        else
          p := strpnew('second '+secondnames[ht]+' (exit)');
        exprasmlist.concat(tai_comment.create(p));
      end;
{$endif EXTDEBUG}

     procedure secondpass(var p : tnode);
      var
         oldcodegenerror  : boolean;
         oldlocalswitches : tlocalswitches;
         oldpos    : tfileposinfo;
{$ifdef TEMPREGDEBUG}
         prevp : pptree;
{$endif TEMPREGDEBUG}
{$ifdef EXTDEBUG}
         i : longint;
{$endif EXTDEBUG}
      begin
         if not assigned(p) then
          internalerror(200208221);
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
{$ifdef EXTDEBUG}
            if (p.expectloc=LOC_INVALID) then
              Comment(V_Warning,'ExpectLoc is not set before secondpass: '+nodetype2str[p.nodetype]);
            if (not allow_multi_pass2) and
               (p.location.loc<>LOC_INVALID) then
              Comment(V_Warning,'Location.Loc is already set before secondpass: '+nodetype2str[p.nodetype]);
            if (cs_asm_nodes in aktglobalswitches) then
              logsecond(p.nodetype,true);
{$endif EXTDEBUG}
            p.pass_2;
{$ifdef EXTDEBUG}
            if (cs_asm_nodes in aktglobalswitches) then
              logsecond(p.nodetype,false);
            if (not codegenerror) then
             begin
               if (p.location.loc=LOC_INVALID) then
                 Comment(V_Warning,'Location not set in secondpass: '+nodetype2str[p.nodetype])
               else if (p.location.loc<>p.expectloc) then
                 Comment(V_Warning,'Location is different in secondpass: '+nodetype2str[p.nodetype]);
             end;

{$ifdef newra}
            if rg.unusedregsint*([first_supreg..last_supreg] - [RS_FRAME_POINTER_REG,RS_STACK_POINTER_REG])<>
                                ([first_supreg..last_supreg] - [RS_FRAME_POINTER_REG,RS_STACK_POINTER_REG]) then
              internalerror(200306171);
{$else}
            { check if all scratch registers are freed }
            for i:=1 to max_scratch_regs do
              if not(scratch_regs[i] in cg.unusedscratchregisters) then
                begin
                   printnode(stdout,p);
                   internalerror(2003042201);
                end;
{$endif newra}
{$endif EXTDEBUG}
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

    procedure clearrefs(p : tnamedindexitem;arg:pointer);

      begin
         if (tsym(p).typ=varsym) then
           if tvarsym(p).refs>1 then
             tvarsym(p).refs:=1;
      end;

    procedure generatecode(var p : tnode);
      begin
       {$ifndef newra}
         rg.cleartempgen;
       {$endif}
         flowcontrol:=[];
         { when size optimization only count occurrence }
         if cs_littlesize in aktglobalswitches then
           rg.t_times:=1
         else
           { reference for repetition is 100 }
           rg.t_times:=100;
         { clear register count }
         rg.clearregistercount;
         symtablestack.foreach_static({$ifdef FPCPROCVAR}@{$endif}clearrefs,nil);
         symtablestack.next.foreach_static({$ifdef FPCPROCVAR}@{$endif}clearrefs,nil);
         { firstpass everything }
         do_firstpass(p);
         { only do secondpass if there are no errors }
         if ErrorCount=0 then
           begin
              { assign parameter locations }
              current_procinfo.after_pass1;

              { callee paraloc register info is necessary for regvars }
              paramanager.create_paraloc_info(current_procinfo.procdef,calleeside);
              { caller paraloc info is also necessary in the stackframe_entry }
              { code of the ppc (and possibly other processors)               }
              if not current_procinfo.procdef.has_paraloc_info then
                begin
                  paramanager.create_paraloc_info(current_procinfo.procdef,callerside);
                  current_procinfo.procdef.has_paraloc_info:=true;
                end;

              { process register variable stuff (JM) }
              assign_regvars(p);
//              load_regvars(current_procinfo.aktentrycode,p);

              { for the i386 it must be done in genexitcode because it has  }
              { to add 'fstp' instructions when using fpu regvars and those }
              { must come after the "exitlabel" (JM)                        }
{$ifndef i386}
//              cleanup_regvars(current_procinfo.aktexitcode);
{$endif i386}

              current_procinfo.allocate_framepointer;

              do_secondpass(p);

              if assigned(current_procinfo.procdef) then
                current_procinfo.procdef.fpu_used:=p.registersfpu;

           end;
         current_procinfo.aktproccode.concatlist(exprasmlist);
      end;

end.
{
  $Log$
  Revision 1.60  2003-07-06 15:31:20  daniel
    * Fixed register allocator. *Lots* of fixes.

  Revision 1.59  2003/07/06 10:18:47  jonas
    * also generate the caller paraloc info of a procedure if it doesn't exist
      yet at the start of pass_2

  Revision 1.58  2003/07/05 20:13:03  jonas
     * create_paraloc_info() is now called separately for the caller and
       callee info
     * fixed ppc cycle

  Revision 1.57  2003/06/13 21:19:30  peter
    * current_procdef removed, use current_procinfo.procdef instead

  Revision 1.56  2003/06/12 16:43:07  peter
    * newra compiles for sparc

  Revision 1.55  2003/06/09 12:23:30  peter
    * init/final of procedure data splitted from genentrycode
    * use asmnode getposition to insert final at the correct position
      als for the implicit try...finally

  Revision 1.54  2003/06/03 13:01:59  daniel
    * Register allocator finished

  Revision 1.53  2003/05/26 21:17:17  peter
    * procinlinenode removed
    * aktexit2label removed, fast exit removed
    + tcallnode.inlined_pass_2 added

  Revision 1.52  2003/05/15 18:58:53  peter
    * removed selfpointer_offset, vmtpointer_offset
    * tvarsym.adjusted_address
    * address in localsymtable is now in the real direction
    * removed some obsolete globals

  Revision 1.51  2003/05/13 19:14:41  peter
    * failn removed
    * inherited result code check moven to pexpr

  Revision 1.50  2003/05/09 17:47:02  peter
    * self moved to hidden parameter
    * removed hdisposen,hnewn,selfn

  Revision 1.49  2003/04/27 11:21:33  peter
    * aktprocdef renamed to current_procinfo.procdef
    * procinfo renamed to current_procinfo
    * procinfo will now be stored in current_module so it can be
      cleaned up properly
    * gen_main_procsym changed to create_main_proc and release_main_proc
      to also generate a tprocinfo structure
    * fixed unit implicit initfinal

  Revision 1.48  2003/04/27 07:29:50  peter
    * current_procinfo.procdef cleanup, current_procdef is now always nil when parsing
      a new procdef declaration
    * aktprocsym removed
    * lexlevel removed, use symtable.symtablelevel instead
    * implicit init/final code uses the normal genentry/genexit
    * funcret state checking updated for new funcret handling

  Revision 1.47  2003/04/25 20:59:33  peter
    * removed funcretn,funcretsym, function result is now in varsym
      and aliases for result and function name are added using absolutesym
    * vs_hidden parameter for funcret passed in parameter
    * vs_hidden fixes
    * writenode changed to printnode and released from extdebug
    * -vp option added to generate a tree.log with the nodetree
    * nicer printnode for statements, callnode

  Revision 1.46  2003/04/23 10:12:14  peter
    * allow multi pass2 changed to global boolean instead of node flag

  Revision 1.45  2003/04/22 23:50:23  peter
    * firstpass uses expectloc
    * checks if there are differences between the expectloc and
      location.loc from secondpass in EXTDEBUG

  Revision 1.44  2003/04/22 12:45:58  florian
    * fixed generic in operator code
    + added debug code to check if all scratch registers are released

  Revision 1.43  2003/01/09 20:40:59  daniel
    * Converted some code in cgx86.pas to new register numbering

  Revision 1.42  2003/01/09 15:49:56  daniel
    * Added register conversion

  Revision 1.41  2002/12/22 14:35:39  peter
    * removed Writeln

  Revision 1.40  2002/12/21 23:21:47  mazen
  + added support for the shift nodes
  + added debug output on screen with -an command line option

  Revision 1.39  2002/08/23 16:14:49  peter
    * tempgen cleanup
    * tt_noreuse temp type added that will be used in genentrycode

  Revision 1.38  2002/08/20 16:55:38  peter
    * don't write (stabs)line info when inlining a procedure

  Revision 1.37  2002/08/19 19:36:44  peter
    * More fixes for cross unit inlining, all tnodes are now implemented
    * Moved pocall_internconst to po_internconst because it is not a
      calling type at all and it conflicted when inlining of these small
      functions was requested

  Revision 1.36  2002/08/18 20:06:24  peter
    * inlining is now also allowed in interface
    * renamed write/load to ppuwrite/ppuload
    * tnode storing in ppu
    * nld,ncon,nbas are already updated for storing in ppu

  Revision 1.35  2002/08/17 09:23:38  florian
    * first part of procinfo rewrite

  Revision 1.34  2002/08/15 19:10:35  peter
    * first things tai,tnode storing in ppu

  Revision 1.33  2002/07/30 20:50:44  florian
    * the code generator knows now if parameters are in registers

  Revision 1.32  2002/07/19 11:41:36  daniel
  * State tracker work
  * The whilen and repeatn are now completely unified into whilerepeatn. This
    allows the state tracker to change while nodes automatically into
    repeat nodes.
  * Resulttypepass improvements to the notn. 'not not a' is optimized away and
    'not(a>b)' is optimized into 'a<=b'.
  * Resulttypepass improvements to the whilerepeatn. 'while not a' is optimized
    by removing the notn and later switchting the true and falselabels. The
    same is done with 'repeat until not a'.

  Revision 1.31  2002/07/01 18:46:25  peter
    * internal linker
    * reorganized aasm layer

  Revision 1.30  2002/05/18 13:34:11  peter
    * readded missing revisions

  Revision 1.29  2002/05/16 19:46:42  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.27  2002/05/12 16:53:08  peter
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

  Revision 1.26  2002/04/21 19:02:04  peter
    * removed newn and disposen nodes, the code is now directly
      inlined from pexpr
    * -an option that will write the secondpass nodes to the .s file, this
      requires EXTDEBUG define to actually write the info
    * fixed various internal errors and crashes due recent code changes

  Revision 1.25  2002/04/20 21:32:24  carl
  + generic FPC_CHECKPOINTER
  + first parameter offset in stack now portable
  * rename some constants
  + move some cpu stuff to other units
  - remove unused constents
  * fix stacksize for some targets
  * fix generic size problems which depend now on EXTEND_SIZE constant

  Revision 1.24  2002/04/07 13:30:13  carl
  - removed unused variable

  Revision 1.23  2002/04/02 17:11:29  peter
    * tlocation,treference update
    * LOC_CONSTANT added for better constant handling
    * secondadd splitted in multiple routines
    * location_force_reg added for loading a location to a register
      of a specified size
    * secondassignment parses now first the right and then the left node
      (this is compatible with Kylix). This saves a lot of push/pop especially
      with string operations
    * adapted some routines to use the new cg methods

  Revision 1.22  2002/03/31 20:26:35  jonas
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
