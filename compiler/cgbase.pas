{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

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
unit cgbase;

{$i defines.inc}

  interface

    uses
      { common }
      cclasses,
      { global }
      globals,verbose,
      { symtable }
      symconst,symtype,symdef,symsym,
      { aasm }
      aasm,cpubase, cpuinfo
      ;

    type
       TOpCg = (OP_ADD,OP_AND,OP_DIV,OP_IDIV,OP_IMUL,OP_MUL,OP_NEG,OP_NOT,
                   OP_OR,OP_SAR,OP_SHL,OP_SHR,OP_SUB,OP_XOR);

       TOpCmp = (OC_NONE,OC_EQ,OC_GT,OC_LT,OC_GTE,OC_LTE,OC_NE,OC_BE,OC_B,
                 OC_AE,OC_A);

       TCgSize = (OS_NO,OS_8,OS_16,OS_32,OS_64,OS_S8,OS_S16,OS_S32,OS_S64);

    const
       pi_uses_asm  = $1;       { set, if the procedure uses asm }
       pi_is_global = $2;       { set, if the procedure is exported by an unit }
       pi_do_call   = $4;       { set, if the procedure does a call }
       pi_operator  = $8;       { set, if the procedure is an operator   }
       pi_C_import  = $10;      { set, if the procedure is an external C function }
       pi_uses_exceptions = $20;{ set, if the procedure has a try statement => }
                                { no register variables                 }
       pi_is_assembler = $40;   { set if the procedure is declared as ASSEMBLER
                                  => don't optimize}
       pi_needs_implicit_finally = $80; { set, if the procedure contains data which }
                                        { needs to be finalized              }

       { defines the default address size for a processor }
       { and defines the natural int size for a processor }
{$ifdef i386}
       OS_ADDR = OS_32;
       OS_INT = OS_32;
{$endif i386}
{$ifdef alpha}
       OS_ADDR = OS_64;
       OS_INT = OS_64;
{$endif alpha}
{$ifdef powerpc}
       OS_ADDR = OS_32;
       OS_INT = OS_32;
{$endif powercc}
{$ifdef ia64}
       OS_ADDR = OS_64;
       OS_INT = OS_64;
{$endif ia64}

    type
       pprocinfo = ^tprocinfo;
       tprocinfo = object
          { pointer to parent in nested procedures }
          parent : pprocinfo;
          { current class, if we are in a method }
          _class : tobjectdef;
          { the definition of the proc itself }
          procdef : tprocdef;

          { frame pointer offset }
          framepointer_offset : longint;
          { self pointer offset }
          selfpointer_offset : longint;
          { result value offset }
          return_offset : longint;
          { firsttemp position }
          firsttemp_offset : longint;
          { parameter offset }
          para_offset : longint;

          { some collected informations about the procedure }
          { see pi_xxxx above                          }
          flags : longint;

          { register used as frame pointer }
          framepointer : tregister;

          { true, if the procedure is exported by an unit }
          globalsymbol : boolean;

          { true, if the procedure should be exported (only OS/2) }
          exported : boolean;

          { true, if we can not use fast exit code }
          no_fast_exit : boolean;

          { code for the current procedure }
          aktproccode,aktentrycode,
          aktexitcode,aktlocaldata : taasmoutput;
          { local data is used for smartlink }

          constructor init;
          destructor done;
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


    var
       { info about the current sub routine }
       procinfo : pprocinfo;

       { labels for BREAK and CONTINUE }
       aktbreaklabel,aktcontinuelabel : tasmlabel;

       { label when the result is true or false }
       truelabel,falselabel : tasmlabel;

       { label to leave the sub routine }
       aktexitlabel : tasmlabel;

       { also an exit label, only used we need to clear only the stack }
       aktexit2label : tasmlabel;

       { only used in constructor for fail or if getmem fails }
       faillabel,quickexitlabel : tasmlabel;

       { Boolean, wenn eine loadn kein Assembler erzeugt hat }
       simple_loadn : boolean;

       { true, if an error while code generation occurs }
       codegenerror : boolean;

       { save the size of pushed parameter, needed for aligning }
       pushedparasize : longint;

       make_const_global : boolean;

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


    function def_cgsize(const p1: tdef): tcgsize;
    function int_cgsize(const l: aword): tcgsize;

    { return the inverse condition of opcmp }
    function inverse_opcmp(opcmp: topcmp): topcmp;

    { return whether op is commutative }
    function commutativeop(op: topcg): boolean;


implementation

     uses
        systems,
        cresstr,
        types
{$ifdef fixLeaksOnError}
        ,comphook
{$endif fixLeaksOnError}

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

    constructor tprocinfo.init;
      begin
        parent:=nil;
        _class:=nil;
        procdef:=nil;
        framepointer_offset:=0;
        selfpointer_offset:=0;
        return_offset:=0;
        firsttemp_offset:=0;
        para_offset:=0;
        flags:=0;
        framepointer:=R_NO;
        globalsymbol:=false;
        exported:=false;
        no_fast_exit:=false;

        aktentrycode:=Taasmoutput.Create;
        aktexitcode:=Taasmoutput.Create;
        aktproccode:=Taasmoutput.Create;
        aktlocaldata:=Taasmoutput.Create;
      end;


    destructor tprocinfo.done;
      begin
         aktentrycode.free;
         aktexitcode.free;
         aktproccode.free;
         aktlocaldata.free;
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
         new(procinfo,init);
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
         dispose(procinfo,done);
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
         { assembler symbols }
         asmsymbollist:=tdictionary.create;
         asmsymbollist.usehash;
         { resourcestrings }
         ResourceStrings:=TResourceStrings.Create;
      end;



    procedure codegen_donemodule;
{$ifdef MEMDEBUG}
      var
        d : tmemdebug;
{$endif}
      begin
{$ifdef MEMDEBUG}
         d:=tmemdebug.create('asmlist');
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
         { assembler symbols }
{$ifdef MEMDEBUG}
         d:=tmemdebug.create('asmsymbol');
{$endif}
         asmsymbollist.free;
{$ifdef MEMDEBUG}
         d.free;
{$endif}
         { resource strings }
         ResourceStrings.free;
      end;


    function def_cgsize(const p1: tdef): tcgsize;
      begin
        result := int_cgsize(p1.size);
        if is_signed(p1) then
          result := tcgsize(ord(result)+(ord(OS_S8)-ord(OS_8)));
      end;

    function int_cgsize(const l: aword): tcgsize;
      begin
        case l of
          1: result := OS_8;
          2: result := OS_16;
          4: result := OS_32;
          8: result := OS_64;
          else
            internalerror(2001092311);
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
          (true,true,false,false,true,true,false,false,
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
      dispose(p,done);
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
  Revision 1.5  2001-12-30 17:24:48  jonas
    * range checking is now processor independent (part in cgobj, part in    cg64f32) and should work correctly again (it needed some changes after    the changes of the low and high of tordef's to int64)  * maketojumpbool() is now processor independent (in ncgutil)  * getregister32 is now called getregisterint

  Revision 1.4  2001/11/06 14:53:48  jonas
    * compiles again with -dmemdebug

  Revision 1.3  2001/09/29 21:33:47  jonas
    * support 64bit operands in def_cgsize()

  Revision 1.2  2001/09/28 20:39:33  jonas
    * changed all flow control structures (except for exception handling
      related things) to processor independent code (in new ncgflw unit)
    + generic cgobj unit which contains lots of code generator helpers with
      global "cg" class instance variable
    + cgcpu unit for i386 (implements processor specific routines of the above
      unit)
    * updated cgbase and cpubase for the new code generator units
    * include ncgflw unit in cpunode unit

  Revision 1.1  2001/08/26 13:36:36  florian
    * some cg reorganisation
    * some PPC updates

  Revision 1.11  2001/08/06 21:40:46  peter
    * funcret moved from tprocinfo to tprocdef

  Revision 1.10  2001/04/13 01:22:07  peter
    * symtable change to classes
    * range check generation and errors fixed, make cycle DEBUG=1 works
    * memory leaks fixed

  Revision 1.9  2000/12/25 00:07:26  peter
    + new tlinkedlist class (merge of old tstringqueue,tcontainer and
      tlinkedlist objects)

  Revision 1.8  2000/11/30 22:16:49  florian
    * moved to i386

  Revision 1.7  2000/10/31 22:02:47  peter
    * symtable splitted, no real code changes

  Revision 1.6  2000/09/24 15:06:17  peter
    * use defines.inc

  Revision 1.5  2000/08/27 16:11:51  peter
    * moved some util functions from globals,cobjects to cutils
    * splitted files into finput,fmodule

  Revision 1.4  2000/08/12 15:34:22  peter
    + usedasmsymbollist to check and reset only the used symbols (merged)

  Revision 1.3  2000/08/03 13:17:26  jonas
    + allow regvars to be used inside inlined procs, which required  the
      following changes:
        + load regvars in genentrycode/free them in genexitcode (cgai386)
        * moved all regvar related code to new regvars unit
        + added pregvarinfo type to hcodegen
        + added regvarinfo field to tprocinfo (symdef/symdefh)
        * deallocate the regvars of the caller in secondprocinline before
          inlining the called procedure and reallocate them afterwards

  Revision 1.2  2000/07/13 11:32:41  michael
  + removed logs

}
