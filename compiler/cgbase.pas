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
      globals,verbose,
      { symtable }
      symconst,symtype,symdef,symsym,
      { aasm }
      cpubase,cpuinfo,cginfo,aasmbase,aasmtai
      ;



    const
       {# bitmask indicating if the procedure uses asm }
       pi_uses_asm  = $1;
       {# bitmask indicating if the procedure is exported by an unit }
       pi_is_global = $2;
       {# bitmask indicating if the procedure does a call }
       pi_do_call   = $4;
       {# bitmask indicating if the procedure is an operator   }
       pi_operator  = $8;
       {# bitmask indicating if the procedure is an external C function }
       pi_c_import  = $10;
       {# bitmask indicating if the procedure has a try statement = no register optimization }
       pi_uses_exceptions = $20;
       {# bitmask indicating if the procedure is declared as @var(assembler), don't optimize}
       pi_is_assembler = $40;
       {# bitmask indicating if the procedure contains data which needs to be finalized }
       pi_needs_implicit_finally = $80;

    type
       {# This object gives information on the current routine being
          compiled.
       }
       tprocinfo = class
          { pointer to parent in nested procedures }
          parent : tprocinfo;
          {# current class, if we are in a method }
          _class : tobjectdef;
          {# the definition of the routine itself }
          procdef : tprocdef;
          {# offset from frame pointer to get parent frame pointer reference
             (used in nested routines only)
             On the PowerPC, this is used to store the offset where the
             frame pointer from the outer procedure is stored.
          }
          framepointer_offset : longint;
          {# offset from frame pointer to get self reference }
          selfpointer_offset : longint;
          {# offset from frame pointer to get vmt reference (constructors only) }
          inheritedflag_offset,
          vmtpointer_offset  : longint;
          {# result value offset in stack (functions only) }
          return_offset : longint;
          {# firsttemp position }
          firsttemp_offset : longint;
          {# offset from frame pointer to parameters }
          para_offset : longint;

          {# some collected informations about the procedure
             see pi_xxxx constants above
          }
          flags : longint;

          {# register used as frame pointer }
          framepointer : tregister;

          {# true, if the procedure is exported by a unit }
          globalsymbol : boolean;

          {# true, if the procedure should be exported (only OS/2) }
          exported : boolean;

          {# true, if we can not use fast exit code }
          no_fast_exit : boolean;

          {# Holds the environment reference for default exceptions

             The exception reference is created when ansistrings
             or classes are used. It holds buffer for exception
             frames. It is allocted by g_new_exception.
          }
          exception_env_ref : treference;
          {# Holds the environment reference for default exceptions

             The exception reference is created when ansistrings
             or classes are used. It holds buffer for setjmp
             It is allocted by g_new_exception.
          }
          exception_jmp_ref :treference;
          {# Holds the environment reference for default exceptions

             The exception reference is created when ansistrings
             or classes are used. It holds the location where
             temporary storage of the setjmp result is stored.

             This reference can be unused, if the result is instead
             saved on the stack.
          }
          exception_result_ref :treference;

          {# Holds the reference used to store the original stackpointer
             after all registers are saved
          }
          save_stackptr_ref :treference;
          {# Holds the reference used to store alll saved registers.

             This is used on systems which do not have direct stack
             operations (such as the PowerPC), it is unused on other
             systems
          }
          save_regs_ref : treference;
          {# The code for the routine itself, excluding entry and
             exit code. This is a linked list of tai classes.
          }
          aktproccode : taasmoutput;
          {# The code for the routine entry code.
          }
          aktentrycode: taasmoutput;
          {# The code for the routine exit code.
          }
          aktexitcode: taasmoutput;
          aktlocaldata : taasmoutput;

          constructor create;virtual;
          destructor destroy;override;

          procedure allocate_interrupt_stackframe;virtual;

          { Does the necessary stuff before a procedure body is compiled }
          procedure handle_body_start;virtual;

          { This is called by parser, after the header of a subroutine is parsed.
            If the local symtable offset depends on the para symtable size, the
            necessary stuff must be done here.
          }
          procedure after_header;virtual;

          { This procedure is called after the pass 1 of the subroutine body is done.
            Here the address fix ups to generate code for the body must be done.
          }
          procedure after_pass1;virtual;

(*        done by symtablestack.insertvardata() (JM)
          { sets the offset for a temp used by the result }
          procedure set_result_offset;virtual;
*)
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
       {# information about the current sub routine being parsed (@var(pprocinfo))}
       procinfo : tprocinfo;

       cprocinfo : tcprocinfo;

       { labels for BREAK and CONTINUE }
       aktbreaklabel,aktcontinuelabel : tasmlabel;

       { label when the result is true or false }
       truelabel,falselabel : tasmlabel;

       { label to leave the sub routine }
       aktexitlabel : tasmlabel;

       { also an exit label, only used we need to clear only the stack }
       aktexit2label : tasmlabel;

       {# only used in constructor for fail keyword or if getmem fails }
       faillabel      : tasmlabel;
       quickexitlabel : tasmlabel;

       {# true, if there was an error while code generation occurs }
       codegenerror : boolean;

       { save the size of pushed parameter, needed for aligning }
       pushedparasize : longint;

       { procinfo instance which is used in procedures created automatically by the compiler }
       voidprocpi : tprocinfo;

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
    procedure codegen_doneprocedure;
    procedure codegen_donemodule;
    procedure codegen_newmodule;
    procedure codegen_newprocedure;

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
        systems,
        cresstr,
        tgobj,rgobj,
        defutil,
        fmodule
{$ifdef fixLeaksOnError}
        ,comphook
{$endif fixLeaksOnError}
        ,symbase,paramgr
        ;

{$ifdef fixLeaksOnError}
     var procinfoStack: TStack;
         hcodegen_old_do_stop: tstopprocedure;
{$endif fixLeaksOnError}

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

    constructor tprocinfo.create;
      begin
        parent:=nil;
        _class:=nil;
        procdef:=nil;
        framepointer_offset:=0;
        selfpointer_offset:=0;
        vmtpointer_offset:=0;
        inheritedflag_offset:=0;
        return_offset:=0;
        firsttemp_offset:=0;
        para_offset:=0;
        flags:=0;
        framepointer.enum:=R_NO;
        framepointer.number:=NR_NO;
        globalsymbol:=false;
        exported:=false;
        no_fast_exit:=false;

        aktentrycode:=Taasmoutput.Create;
        aktexitcode:=Taasmoutput.Create;
        aktproccode:=Taasmoutput.Create;
        aktlocaldata:=Taasmoutput.Create;
        reference_reset(exception_env_ref);
        reference_reset(exception_jmp_ref);
        reference_reset(exception_result_ref);
        reference_reset(save_stackptr_ref);
      end;


    destructor tprocinfo.destroy;
      begin
         aktentrycode.free;
         aktexitcode.free;
         aktproccode.free;
         aktlocaldata.free;
      end;

    procedure tprocinfo.allocate_interrupt_stackframe;
      begin
      end;


    procedure tprocinfo.handle_body_start;
      begin
         { temporary space is set, while the BEGIN of the procedure }
         if (symtablestack.symtabletype=localsymtable) then
           procinfo.firsttemp_offset := tg.direction*symtablestack.datasize
         else
           procinfo.firsttemp_offset := 0;
         { space for the return value }
         { !!!!!   this means that we can not set the return value
         in a subfunction !!!!! }
         { because we don't know yet where the address is }
         if not is_void(procdef.rettype.def) then
           begin
              if paramanager.ret_in_reg(procdef.rettype.def,procdef.proccalloption) then
                begin
(* already done in symtable.pas:tlocalsymtable.insertvardata() (JM)
                   { the space has been set in the local symtable }
                   procinfo.return_offset:=tg.direction*tfuncretsym(procdef.funcretsym).address;
*)
                   if ((procinfo.flags and pi_operator)<>0) and
                      assigned(otsym) then
                     otsym.address:=tfuncretsym(procdef.funcretsym).address;

                   rg.usedinproc := rg.usedinproc +
                      getfuncretusedregisters(procdef.rettype.def,procdef.proccalloption);
                end;
           end;
      end;


(* already done in symtable.pas:tlocalsymtable.insertvardata() (JM)
    procedure tprocinfo.set_result_offset;
      begin
         if paramanager.ret_in_reg(procdef.rettype.def,procdef.proccalloption) then
           procinfo.return_offset:=tg.direction*tfuncretsym(procdef.funcretsym).address;
      end;
*)


    procedure tprocinfo.after_header;
      begin
      end;

    procedure tprocinfo.after_pass1;
      begin
      end;


{*****************************************************************************
         initialize/terminate the codegen for procedure and modules
*****************************************************************************}

    procedure codegen_newprocedure;
      begin
         aktbreaklabel:=nil;
         aktcontinuelabel:=nil;
         { aktexitlabel:=0; is store in oldaktexitlabel
           so it must not be reset to zero before this storage !}
         { new procinfo }
         procinfo:=cprocinfo.create;
{$ifdef fixLeaksOnError}
         procinfoStack.push(procinfo);
{$endif fixLeaksOnError}
      end;



    procedure codegen_doneprocedure;
      begin
{$ifdef fixLeaksOnError}
         if procinfo <> procinfoStack.pop then
           writeln('problem with procinfoStack!');
{$endif fixLeaksOnError}
         procinfo.free;
         procinfo:=nil;
      end;



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
         { for the implicitly generated init/final. procedures for global init. variables,
           a dummy procinfo is necessary }
         voidprocpi:=cprocinfo.create;
         with voidprocpi do
           begin
              framepointer.enum:=R_INTREGISTER;
              framepointer.number:=NR_FRAME_POINTER_REG;
           end;
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
         // voidprocpi.free;
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
          pointerdef,
          procvardef:
            result := OS_ADDR;
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

{$ifdef fixLeaksOnError}
procedure hcodegen_do_stop;
var p: pprocinfo;
begin
  p := pprocinfo(procinfoStack.pop);
  while p <> nil Do
    begin
      if p<>voidprocpi then
        p.free;
      p := pprocinfo(procinfoStack.pop);
    end;
  procinfoStack.done;
  do_stop := hcodegen_old_do_stop;
  do_stop{$ifdef FPCPROCVAR}(){$endif};
end;

begin
  hcodegen_old_do_stop := do_stop;
  do_stop := {$ifdef FPCPROCVAR}@{$endif}hcodegen_do_stop;
  procinfoStack.init;
{$endif fixLeaksOnError}
end.
{
  $Log$
  Revision 1.41  2003-04-23 12:35:34  florian
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
