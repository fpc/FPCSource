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
unit hcodegen;

{$i defines.inc}

{$ifdef newcg}
interface

implementation
{$else newcg}

  interface

    uses
      cobjects,
      { global }
      verbose,
      { symtable }
      symconst,symtype,symdef,symsym,
      { aasm }
      aasm,cpubase
      ;

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
    type
       pprocinfo = ^tprocinfo;
       tprocinfo = object
          { pointer to parent in nested procedures }
          parent : pprocinfo;
          { current class, if we are in a method }
          _class : pobjectdef;
          { return type }
          returntype : ttype;
          { symbol of the function, and the sym for result variable }
          resultfuncretsym,
          funcretsym : pfuncretsym;
          funcret_state : tvarstate;
          { the definition of the proc itself }
          def : pprocdef;
          sym : pprocsym;

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
          aktexitcode,aktlocaldata : paasmoutput;
          { local data is used for smartlink }

          constructor init;
          destructor done;
       end;

       { some kind of temp. types needs to be destructed }
       { for example ansistring, this is done using this }
       { list                                       }
       ptemptodestroy = ^ttemptodestroy;
       ttemptodestroy = object(tlinkedlist_item)
          typ : pdef;
          address : treference;
          constructor init(const a : treference;p : pdef);
       end;

       pregvarinfo = ^tregvarinfo;
       tregvarinfo = record
          regvars : array[1..maxvarregs] of pvarsym;
          regvars_para : array[1..maxvarregs] of boolean;
          regvars_refs : array[1..maxvarregs] of longint;

          fpuregvars : array[1..maxfpuvarregs] of pvarsym;
          fpuregvars_para : array[1..maxfpuvarregs] of boolean;
          fpuregvars_refs : array[1..maxfpuvarregs] of longint;
       end;


    var
       { info about the current sub routine }
       procinfo : pprocinfo;

       { labels for BREAK and CONTINUE }
       aktbreaklabel,aktcontinuelabel : pasmlabel;

       { label when the result is true or false }
       truelabel,falselabel : pasmlabel;

       { label to leave the sub routine }
       aktexitlabel : pasmlabel;

       { also an exit label, only used we need to clear only the stack }
       aktexit2label : pasmlabel;

       { only used in constructor for fail or if getmem fails }
       faillabel,quickexitlabel : pasmlabel;

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

implementation

     uses
        systems,globals,cresstr
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
        returntype.reset;
        resultfuncretsym:=nil;
        funcretsym:=nil;
        funcret_state:=vs_none;
        def:=nil;
        sym:=nil;
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

        aktentrycode:=new(paasmoutput,init);
        aktexitcode:=new(paasmoutput,init);
        aktproccode:=new(paasmoutput,init);
        aktlocaldata:=new(paasmoutput,init);
      end;


    destructor tprocinfo.done;
      begin
         dispose(aktentrycode,done);
         dispose(aktexitcode,done);
         dispose(aktproccode,done);
         dispose(aktlocaldata,done);
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
         exprasmlist:=new(paasmoutput,init);
         datasegment:=new(paasmoutput,init);
         codesegment:=new(paasmoutput,init);
         bsssegment:=new(paasmoutput,init);
         debuglist:=new(paasmoutput,init);
         withdebuglist:=new(paasmoutput,init);
         consts:=new(paasmoutput,init);
         rttilist:=new(paasmoutput,init);
         ResourceStringList:=Nil;
         importssection:=nil;
         exportssection:=nil;
         resourcesection:=nil;
         { assembler symbols }
         asmsymbollist:=new(pdictionary,init);
         asmsymbollist^.usehash;
         { resourcestrings }
         new(ResourceStrings,Init);
      end;



    procedure codegen_donemodule;
{$ifdef MEMDEBUG}
      var
        d : tmemdebug;
{$endif}
      begin
{$ifdef MEMDEBUG}
         d.init('asmlist');
{$endif}
         dispose(exprasmlist,done);
         dispose(codesegment,done);
         dispose(bsssegment,done);
         dispose(datasegment,done);
         dispose(debuglist,done);
         dispose(withdebuglist,done);
         dispose(consts,done);
         dispose(rttilist,done);
         if assigned(ResourceStringList) then
          dispose(ResourceStringList,done);
         if assigned(importssection) then
          dispose(importssection,done);
         if assigned(exportssection) then
          dispose(exportssection,done);
         if assigned(resourcesection) then
          dispose(resourcesection,done);
{$ifdef MEMDEBUG}
         d.done;
{$endif}
         { assembler symbols }
{$ifdef MEMDEBUG}
         d.init('asmsymbol');
{$endif}
         dispose(asmsymbollist,done);
{$ifdef MEMDEBUG}
         d.done;
{$endif}
         { resource strings }
         dispose(ResourceStrings,done);
      end;


{*****************************************************************************
                              TTempToDestroy
*****************************************************************************}

    constructor ttemptodestroy.init(const a : treference;p : pdef);
      begin
         inherited init;
         address:=a;
         typ:=p;
      end;
{$endif newcg}

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
  Revision 1.7  2000-10-31 22:02:47  peter
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