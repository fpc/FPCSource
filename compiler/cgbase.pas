{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

    This unit exports some help routines for the code generation

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
{# Some helpers for the code generator.
}
unit cgbase;

{$i fpcdefs.inc}

  interface

    uses
      { common }
      cclasses,
      { global }
      globtype,globals,verbose,
      { symtable }
      symconst,symtype,symdef,symsym,
      { aasm }
      cpubase,cpuinfo,cginfo,aasmbase,aasmtai
      ;


    type
      tprocinfoflag=(
        {# procedure uses asm }
        pi_uses_asm,
        {# procedure does a call }
        pi_do_call,
        {# procedure has a try statement = no register optimization }
        pi_uses_exceptions,
        {# procedure is declared as @var(assembler), don't optimize}
        pi_is_assembler,
        {# procedure contains data which needs to be finalized }
        pi_needs_implicit_finally
      );
      tprocinfoflags=set of tprocinfoflag;

    type
       {# This object gives information on the current routine being
          compiled.
       }
       tprocinfo = class(tlinkedlistitem)
          { pointer to parent in nested procedures }
          parent : tprocinfo;
          {# the definition of the routine itself }
          procdef : tprocdef;
          { file location of begin of procedure }
          entrypos  : tfileposinfo;
          { file location of end of procedure }
          exitpos   : tfileposinfo;
          { local switches at begin of procedure }
          entryswitches : tlocalswitches;
          { local switches at end of procedure }
          exitswitches  : tlocalswitches;
          {# offset from frame pointer to get parent frame pointer reference
             (used in nested routines only)
             On the PowerPC, this is used to store the offset where the
             frame pointer from the outer procedure is stored.
          }
          parent_framepointer_offset : longint;
          {# firsttemp position }
          firsttemp_offset : longint;

          { Size of the parameters on the stack }
          para_stack_size : longint;

          {# some collected informations about the procedure
             see pi_xxxx constants above
          }
          flags : tprocinfoflags;

          { register used as frame pointer }
          framepointer : tregister;

          { Holds the reference used to store alll saved registers. }
          save_regs_ref : treference;

          { label to leave the sub routine }
          aktexitlabel : tasmlabel;

          {# The code for the routine itself, excluding entry and
             exit code. This is a linked list of tai classes.
          }
          aktproccode : taasmoutput;
          { Data (like jump tables) that belongs to this routine }
          aktlocaldata : taasmoutput;

          constructor create(aparent:tprocinfo);virtual;
          destructor destroy;override;

          procedure allocate_parent_framepointer_parameter;virtual;

          procedure allocate_interrupt_parameter;virtual;

          { Allocate framepointer so it can not be used by the
            register allocator }
          procedure allocate_framepointer_reg;virtual;

          procedure allocate_push_parasize(size:longint);virtual;

          function calc_stackframe_size:longint;virtual;

          { Does the necessary stuff before a procedure body is compiled }
          procedure handle_body_start;virtual;

          { This procedure is called after the pass 1 of the subroutine body is done.
            Here the address fix ups to generate code for the body must be done.
          }
          procedure after_pass1;virtual;
       end;

       pregvarinfo = ^tregvarinfo;
       tregvarinfo = record
          regvars : array[1..maxvarregs] of tvarsym;
          regvars_para : array[1..maxvarregs] of boolean;
          regvars_refs : array[1..maxvarregs] of longint;

          fpuregvars : array[1..maxfpuvarregs] of tvarsym;
          fpuregvars_para : array[1..maxfpuvarregs] of boolean;
          fpuregvars_refs : array[1..maxfpuvarregs] of longint;
       end;

       tcprocinfo = class of tprocinfo;

    var
       cprocinfo : tcprocinfo;
       {# information about the current sub routine being parsed (@var(pprocinfo))}
       current_procinfo : tprocinfo;

       { labels for BREAK and CONTINUE }
       aktbreaklabel,aktcontinuelabel : tasmlabel;

       { label when the result is true or false }
       truelabel,falselabel : tasmlabel;

       {# true, if there was an error while code generation occurs }
       codegenerror : boolean;

       { save the size of pushed parameter, needed for aligning }
       pushedparasize : longint;

    { message calls with codegenerror support }
    procedure cgmessage(t : longint);
    procedure cgmessage1(t : longint;const s : string);
    procedure cgmessage2(t : longint;const s1,s2 : string);
    procedure cgmessage3(t : longint;const s1,s2,s3 : string);
    procedure CGMessagePos(const pos:tfileposinfo;t:longint);
    procedure CGMessagePos1(const pos:tfileposinfo;t:longint;const s1:string);
    procedure CGMessagePos2(const pos:tfileposinfo;t:longint;const s1,s2:string);
    procedure CGMessagePos3(const pos:tfileposinfo;t:longint;const s1,s2,s3:string);

    { initialize respectively terminates the code generator }
    { for a new module or procedure                      }
    procedure codegen_newmodule;
    procedure codegen_donemodule;

    {# From a definition return the abstract code generator size enum. It is
       to note that the value returned can be @var(OS_NO) }
    function def_cgsize(def: tdef): tcgsize;
    {# From a constant numeric value, return the abstract code generator
       size.
    }
    function int_cgsize(const a: aword): tcgsize;

    {# return the inverse condition of opcmp }
    function inverse_opcmp(opcmp: topcmp): topcmp;

    {# return whether op is commutative }
    function commutativeop(op: topcg): boolean;


implementation

     uses
        cutils,systems,
        cresstr,
        tgobj,rgobj,
        defutil,
        fmodule
        ,symbase,paramgr
        ;


{*****************************************************************************
            override the message calls to set codegenerror
*****************************************************************************}

    procedure cgmessage(t : longint);
      var
         olderrorcount : longint;
      begin
         if not(codegenerror) then
           begin
              olderrorcount:=Errorcount;
              verbose.Message(t);
              codegenerror:=olderrorcount<>Errorcount;
           end;
      end;

    procedure cgmessage1(t : longint;const s : string);
      var
         olderrorcount : longint;
      begin
         if not(codegenerror) then
           begin
              olderrorcount:=Errorcount;
              verbose.Message1(t,s);
              codegenerror:=olderrorcount<>Errorcount;
           end;
      end;

    procedure cgmessage2(t : longint;const s1,s2 : string);
      var
         olderrorcount : longint;
      begin
         if not(codegenerror) then
           begin
              olderrorcount:=Errorcount;
              verbose.Message2(t,s1,s2);
              codegenerror:=olderrorcount<>Errorcount;
           end;
      end;

    procedure cgmessage3(t : longint;const s1,s2,s3 : string);
      var
         olderrorcount : longint;
      begin
         if not(codegenerror) then
           begin
              olderrorcount:=Errorcount;
              verbose.Message3(t,s1,s2,s3);
              codegenerror:=olderrorcount<>Errorcount;
           end;
      end;


    procedure cgmessagepos(const pos:tfileposinfo;t : longint);
      var
         olderrorcount : longint;
      begin
         if not(codegenerror) then
           begin
              olderrorcount:=Errorcount;
              verbose.MessagePos(pos,t);
              codegenerror:=olderrorcount<>Errorcount;
           end;
      end;

    procedure cgmessagepos1(const pos:tfileposinfo;t : longint;const s1 : string);
      var
         olderrorcount : longint;
      begin
         if not(codegenerror) then
           begin
              olderrorcount:=Errorcount;
              verbose.MessagePos1(pos,t,s1);
              codegenerror:=olderrorcount<>Errorcount;
           end;
      end;

    procedure cgmessagepos2(const pos:tfileposinfo;t : longint;const s1,s2 : string);
      var
         olderrorcount : longint;
      begin
         if not(codegenerror) then
           begin
              olderrorcount:=Errorcount;
              verbose.MessagePos2(pos,t,s1,s2);
              codegenerror:=olderrorcount<>Errorcount;
           end;
      end;

    procedure cgmessagepos3(const pos:tfileposinfo;t : longint;const s1,s2,s3 : string);
      var
         olderrorcount : longint;
      begin
         if not(codegenerror) then
           begin
              olderrorcount:=Errorcount;
              verbose.MessagePos3(pos,t,s1,s2,s3);
              codegenerror:=olderrorcount<>Errorcount;
           end;
      end;


{****************************************************************************
                                 TProcInfo
****************************************************************************}

    constructor tprocinfo.create(aparent:tprocinfo);
      begin
        parent:=aparent;
        procdef:=nil;
        para_stack_size:=0;
{$warning TODO maybe remove parent_framepointer_offset for i386}
        parent_framepointer_offset:=0;
        firsttemp_offset:=0;
        flags:=[];
        framepointer:=NR_FRAME_POINTER_REG;
        { asmlists }
        aktproccode:=Taasmoutput.Create;
        aktlocaldata:=Taasmoutput.Create;
        reference_reset(save_regs_ref);
        { labels }
        objectlibrary.getlabel(aktexitlabel);
      end;


    destructor tprocinfo.destroy;
      begin
         aktproccode.free;
         aktlocaldata.free;
      end;


    procedure tprocinfo.allocate_parent_framepointer_parameter;
      begin
        parent_framepointer_offset:=target_info.first_parm_offset;
      end;


    procedure tprocinfo.allocate_interrupt_parameter;
      begin
      end;


    procedure tprocinfo.allocate_framepointer_reg;
      begin
      end;


    procedure tprocinfo.allocate_push_parasize(size:longint);
      begin
      end;


    function tprocinfo.calc_stackframe_size:longint;
      var
        _align : longint;
      begin
        { align to 4 bytes at least
          otherwise all those subl $2,%esp are meaningless PM }
        _align:=target_info.alignment.localalignmin;
        if _align<4 then
          _align:=4;
        result:=Align(tg.direction*tg.lasttemp,_align);
      end;


    procedure tprocinfo.handle_body_start;
      begin
(*
         { temporary space is set, while the BEGIN of the procedure }
         if (symtablestack.symtabletype=localsymtable) then
           current_procinfo.firsttemp_offset := tg.direction*symtablestack.datasize
         else
           current_procinfo.firsttemp_offset := 0;
*)
      end;


    procedure tprocinfo.after_pass1;
      begin
      end;


{*****************************************************************************
         initialize/terminate the codegen for procedure and modules
*****************************************************************************}

    procedure codegen_newmodule;
      begin
         exprasmlist:=taasmoutput.create;
         datasegment:=taasmoutput.create;
         codesegment:=taasmoutput.create;
         bsssegment:=taasmoutput.create;
         debuglist:=taasmoutput.create;
         withdebuglist:=taasmoutput.create;
         consts:=taasmoutput.create;
         rttilist:=taasmoutput.create;
         ResourceStringList:=Nil;
         importssection:=nil;
         exportssection:=nil;
         resourcesection:=nil;
         { resourcestrings }
         ResourceStrings:=TResourceStrings.Create;
         { use the librarydata from current_module }
         objectlibrary:=current_module.librarydata;
      end;


    procedure codegen_donemodule;
{$ifdef MEMDEBUG}
      var
        d : tmemdebug;
{$endif}
      begin
{$ifdef MEMDEBUG}
         d:=tmemdebug.create(current_module.modulename^+' - asmlists');
{$endif}
         exprasmlist.free;
         codesegment.free;
         bsssegment.free;
         datasegment.free;
         debuglist.free;
         withdebuglist.free;
         consts.free;
         rttilist.free;
         if assigned(ResourceStringList) then
          ResourceStringList.free;
         if assigned(importssection) then
          importssection.free;
         if assigned(exportssection) then
          exportssection.free;
         if assigned(resourcesection) then
          resourcesection.free;
{$ifdef MEMDEBUG}
         d.free;
{$endif}
         { resource strings }
         ResourceStrings.free;
         objectlibrary:=nil;
      end;


    function def_cgsize(def: tdef): tcgsize;
      begin
        case def.deftype of
          orddef,
          enumdef,
          setdef:
            begin
              result := int_cgsize(def.size);
              if is_signed(def) then
                result := tcgsize(ord(result)+(ord(OS_S8)-ord(OS_8)));
            end;
          classrefdef,
          pointerdef:
            result := OS_ADDR;
          procvardef:
            begin
              if tprocvardef(def).is_methodpointer and
                 (not tprocvardef(def).is_addressonly) then
                result := OS_64
              else
                result := OS_ADDR;
            end;
          stringdef :
            begin
              if is_ansistring(def) or is_widestring(def) then
                result := OS_ADDR
              else
                result := OS_NO;
            end;
          objectdef :
            begin
              if is_class_or_interface(def) then
                result := OS_ADDR
              else
                result := OS_NO;
            end;
          floatdef:
            result := tfloat2tcgsize[tfloatdef(def).typ];
          recorddef :
            result:=int_cgsize(def.size);
          arraydef :
            begin
              if not is_special_array(def) then
                result := int_cgsize(def.size)
              else
                begin
                  if is_dynamic_array(def) then
                    result := OS_ADDR
                  else
                    result := OS_NO;
                end;
            end;
          else
            begin
              { undefined size }
              result:=OS_NO;
            end;
        end;
      end;


    function int_cgsize(const a: aword): tcgsize;
      begin
        if a > 8 then
          begin
            int_cgsize := OS_NO;
            exit;
          end;
        case byte(a) of
          1 :
            result := OS_8;
          2 :
            result := OS_16;
          3,4 :
            result := OS_32;
          5..8 :
            result := OS_64;
        end;
      end;


    function inverse_opcmp(opcmp: topcmp): topcmp;
      const
        list: array[TOpCmp] of TOpCmp =
          (OC_NONE,OC_NE,OC_LTE,OC_GTE,OC_LT,OC_GT,OC_EQ,OC_A,OC_AE,
           OC_B,OC_BE);
      begin
        inverse_opcmp := list[opcmp];
      end;


    function commutativeop(op: topcg): boolean;
      const
        list: array[topcg] of boolean =
          (true,true,true,false,false,true,true,false,false,
           true,false,false,false,false,true);
      begin
        commutativeop := list[op];
      end;

end.
{
  $Log$
  Revision 1.64  2003-09-23 17:56:05  peter
    * locals and paras are allocated in the code generation
    * tvarsym.localloc contains the location of para/local when
      generating code for the current procedure

  Revision 1.63  2003/09/14 19:18:10  peter
    * remove obsolete code already in comments

  Revision 1.62  2003/09/07 22:09:34  peter
    * preparations for different default calling conventions
    * various RA fixes

  Revision 1.61  2003/09/03 15:55:00  peter
    * NEWRA branch merged

  Revision 1.60.2.1  2003/08/29 17:28:59  peter
    * next batch of updates

  Revision 1.60  2003/08/26 12:43:02  peter
    * methodpointer fixes

  Revision 1.59  2003/08/20 17:48:49  peter
    * fixed stackalloc to not allocate localst.datasize twice
    * order of stackalloc code fixed for implicit init/final

  Revision 1.58  2003/08/11 21:18:20  peter
    * start of sparc support for newra

  Revision 1.57  2003/07/06 17:58:22  peter
    * framepointer fixes for sparc
    * parent framepointer code more generic

  Revision 1.56  2003/06/13 21:19:30  peter
    * current_procdef removed, use current_procinfo.procdef instead

  Revision 1.55  2003/06/12 16:43:07  peter
    * newra compiles for sparc

  Revision 1.54  2003/06/09 12:23:29  peter
    * init/final of procedure data splitted from genentrycode
    * use asmnode getposition to insert final at the correct position
      als for the implicit try...finally

  Revision 1.53  2003/06/02 21:42:05  jonas
    * function results can now also be regvars
    - removed tprocinfo.return_offset, never use it again since it's invalid
      if the result is a regvar

  Revision 1.52  2003/05/26 21:17:17  peter
    * procinlinenode removed
    * aktexit2label removed, fast exit removed
    + tcallnode.inlined_pass_2 added

  Revision 1.51  2003/05/23 14:27:35  peter
    * remove some unit dependencies
    * current_procinfo changes to store more info

  Revision 1.50  2003/05/16 20:54:12  jonas
    - undid previous commit, it wasn't necessary

  Revision 1.49  2003/05/16 20:00:39  jonas
    * powerpc nested procedure fixes, should work completely now if all
      local variables of the parent procedure are declared before the
      nested procedures are declared

  Revision 1.48  2003/05/15 18:58:53  peter
    * removed selfpointer_offset, vmtpointer_offset
    * tvarsym.adjusted_address
    * address in localsymtable is now in the real direction
    * removed some obsolete globals

  Revision 1.47  2003/05/13 19:14:41  peter
    * failn removed
    * inherited result code check moven to pexpr

  Revision 1.46  2003/05/09 17:47:02  peter
    * self moved to hidden parameter
    * removed hdisposen,hnewn,selfn

  Revision 1.45  2003/04/27 11:21:32  peter
    * aktprocdef renamed to current_procinfo.procdef
    * procinfo renamed to current_procinfo
    * procinfo will now be stored in current_module so it can be
      cleaned up properly
    * gen_main_procsym changed to create_main_proc and release_main_proc
      to also generate a tprocinfo structure
    * fixed unit implicit initfinal

  Revision 1.44  2003/04/27 07:29:50  peter
    * current_procinfo.procdef cleanup, current_procdef is now always nil when parsing
      a new procdef declaration
    * aktprocsym removed
    * lexlevel removed, use symtable.symtablelevel instead
    * implicit init/final code uses the normal genentry/genexit
    * funcret state checking updated for new funcret handling

  Revision 1.43  2003/04/26 00:31:42  peter
    * set return_offset moved to after_header

  Revision 1.42  2003/04/25 20:59:33  peter
    * removed funcretn,funcretsym, function result is now in varsym
      and aliases for result and function name are added using absolutesym
    * vs_hidden parameter for funcret passed in parameter
    * vs_hidden fixes
    * writenode changed to printnode and released from extdebug
    * -vp option added to generate a tree.log with the nodetree
    * nicer printnode for statements, callnode

  Revision 1.41  2003/04/23 12:35:34  florian
    * fixed several issues with powerpc
    + applied a patch from Jonas for nested function calls (PowerPC only)
    * ...

  Revision 1.40  2003/04/22 13:47:08  peter
    * fixed C style array of const
    * fixed C array passing
    * fixed left to right with high parameters

  Revision 1.39  2003/04/05 21:09:31  jonas
    * several ppc/generic result offset related fixes. The "normal" result
      offset seems now to be calculated correctly and a lot of duplicate
      calculations have been removed. Nested functions accessing the parent's
      function result don't work at all though :(

  Revision 1.38  2003/03/28 19:16:56  peter
    * generic constructor working for i386
    * remove fixed self register
    * esi added as address register for i386

  Revision 1.37  2003/03/20 17:51:45  peter
    * dynamic arrays have size OS_ADDR

  Revision 1.36  2003/01/08 18:43:56  daniel
   * Tregister changed into a record

  Revision 1.35  2003/01/01 21:04:48  peter
    * removed unused method

  Revision 1.34  2002/11/25 17:43:16  peter
    * splitted defbase in defutil,symutil,defcmp
    * merged isconvertable and is_equal into compare_defs(_ext)
    * made operator search faster by walking the list only once

  Revision 1.33  2002/11/18 17:31:54  peter
    * pass proccalloption to ret_in_xxx and push_xxx functions

  Revision 1.32  2002/10/05 12:43:23  carl
    * fixes for Delphi 6 compilation
     (warning : Some features do not work under Delphi)

  Revision 1.31  2002/10/03 21:20:19  carl
    * range check error fix

  Revision 1.30  2002/09/30 07:00:44  florian
    * fixes to common code to get the alpha compiler compiled applied

  Revision 1.29  2002/09/07 19:35:45  florian
    + tcg.direction is used now

  Revision 1.28  2002/09/07 15:25:01  peter
    * old logs removed and tabs fixed

  Revision 1.27  2002/09/05 19:29:42  peter
    * memdebug enhancements

  Revision 1.26  2002/08/18 20:06:23  peter
    * inlining is now also allowed in interface
    * renamed write/load to ppuwrite/ppuload
    * tnode storing in ppu
    * nld,ncon,nbas are already updated for storing in ppu

  Revision 1.25  2002/08/17 09:23:33  florian
    * first part of procinfo rewrite

  Revision 1.24  2002/08/11 14:32:26  peter
    * renamed current_library to objectlibrary

  Revision 1.23  2002/08/11 13:24:11  peter
    * saving of asmsymbols in ppu supported
    * asmsymbollist global is removed and moved into a new class
      tasmlibrarydata that will hold the info of a .a file which
      corresponds with a single module. Added librarydata to tmodule
      to keep the library info stored for the module. In the future the
      objectfiles will also be stored to the tasmlibrarydata class
    * all getlabel/newasmsymbol and friends are moved to the new class

  Revision 1.22  2002/08/06 20:55:20  florian
    * first part of ppc calling conventions fix

  Revision 1.21  2002/08/05 18:27:48  carl
    + more more more documentation
    + first version include/exclude (can't test though, not enough scratch for i386 :()...

  Revision 1.20  2002/08/04 19:06:41  carl
    + added generic exception support (still does not work!)
    + more documentation

  Revision 1.19  2002/07/20 11:57:53  florian
    * types.pas renamed to defbase.pas because D6 contains a types
      unit so this would conflicts if D6 programms are compiled
    + Willamette/SSE2 instructions to assembler added

  Revision 1.18  2002/07/01 18:46:22  peter
    * internal linker
    * reorganized aasm layer

  Revision 1.17  2002/05/20 13:30:40  carl
  * bugfix of hdisponen (base must be set, not index)
  * more portability fixes

  Revision 1.16  2002/05/18 13:34:05  peter
    * readded missing revisions

  Revision 1.15  2002/05/16 19:46:35  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.13  2002/04/25 20:16:38  peter
    * moved more routines from cga/n386util

  Revision 1.12  2002/04/21 15:28:06  carl
  - remove duplicate constants
  - move some constants to cginfo

  Revision 1.11  2002/04/20 21:32:23  carl
  + generic FPC_CHECKPOINTER
  + first parameter offset in stack now portable
  * rename some constants
  + move some cpu stuff to other units
  - remove unused constents
  * fix stacksize for some targets
  * fix generic size problems which depend now on EXTEND_SIZE constant

  Revision 1.10  2002/04/07 09:13:39  carl
  + documentation
  - remove unused variables

  Revision 1.9  2002/04/04 19:05:54  peter
    * removed unused units
    * use tlocation.size in cg.a_*loc*() routines

  Revision 1.8  2002/04/02 17:11:27  peter
    * tlocation,treference update
    * LOC_CONSTANT added for better constant handling
    * secondadd splitted in multiple routines
    * location_force_reg added for loading a location to a register
      of a specified size
    * secondassignment parses now first the right and then the left node
      (this is compatible with Kylix). This saves a lot of push/pop especially
      with string operations
    * adapted some routines to use the new cg methods

  Revision 1.7  2002/03/31 20:26:33  jonas
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
