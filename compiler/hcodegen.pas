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

{$ifdef newcg}
interface

implementation
{$else newcg}

  interface

    uses
      cobjects,
      tokens,verbose,
      aasm,symconst,symtable,cpubase;

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

       { tries to hold the amount of times which the current tree is processed  }
       t_times : longint;

       { true, if an error while code generation occurs }
       codegenerror : boolean;

       { save the size of pushed parameter, needed for aligning }
       pushedparasize : longint;

       make_const_global : boolean;

    { message calls with codegenerror support }
    procedure cgmessage(t : tmsgconst);
    procedure cgmessage1(t : tmsgconst;const s : string);
    procedure cgmessage2(t : tmsgconst;const s1,s2 : string);
    procedure cgmessage3(t : tmsgconst;const s1,s2,s3 : string);
    procedure CGMessagePos(const pos:tfileposinfo;t:tmsgconst);
    procedure CGMessagePos1(const pos:tfileposinfo;t:tmsgconst;const s1:string);
    procedure CGMessagePos2(const pos:tfileposinfo;t:tmsgconst;const s1,s2:string);
    procedure CGMessagePos3(const pos:tfileposinfo;t:tmsgconst;const s1,s2,s3:string);

    { initialize respectively terminates the code generator }
    { for a new module or procedure                      }
    procedure codegen_doneprocedure;
    procedure codegen_donemodule;
    procedure codegen_newmodule;
    procedure codegen_newprocedure;


implementation

     uses
        systems,globals,files,strings,cresstr
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

    procedure cgmessage(t : tmsgconst);
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

    procedure cgmessage1(t : tmsgconst;const s : string);
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

    procedure cgmessage2(t : tmsgconst;const s1,s2 : string);
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

    procedure cgmessage3(t : tmsgconst;const s1,s2,s3 : string);
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


    procedure cgmessagepos(const pos:tfileposinfo;t : tmsgconst);
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

    procedure cgmessagepos1(const pos:tfileposinfo;t : tmsgconst;const s1 : string);
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

    procedure cgmessagepos2(const pos:tfileposinfo;t : tmsgconst;const s1,s2 : string);
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

    procedure cgmessagepos3(const pos:tfileposinfo;t : tmsgconst;const s1,s2,s3 : string);
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
         consts:=new(paasmoutput,init);
         rttilist:=new(paasmoutput,init);
         importssection:=nil;
         exportssection:=nil;
         resourcesection:=nil;
         { assembler symbols }
         asmsymbollist:=new(pasmsymbollist,init);
         asmsymbollist^.usehash;
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
         dispose(consts,done);
         dispose(rttilist,done);
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
         { resourcestrings }
         ResetResourceStrings;
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
procedure hcodegen_do_stop; {$ifdef tp} far; {$endif tp}
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
{$ifdef tp}
  do_stop;
{$else tp}
  do_stop();
{$endif tp}
end;

begin
  hcodegen_old_do_stop := do_stop;
  do_stop := {$ifdef tp}@{$endif}hcodegen_do_stop;
  procinfoStack.init;
{$endif fixLeaksOnError}
end.

{
  $Log$
  Revision 1.56  2000-02-09 13:22:53  peter
    * log truncated

  Revision 1.55  2000/01/16 22:17:11  peter
    * renamed call_offset to para_offset

  Revision 1.54  2000/01/11 17:16:04  jonas
    * removed a lot of memory leaks when an error is encountered (caused by
      procinfo and pstringcontainers). There are still plenty left though :)

  Revision 1.53  2000/01/07 01:14:27  peter
    * updated copyright to 2000

  Revision 1.52  1999/12/09 23:18:04  pierre
   * no_fast_exit if procedure contains implicit termination code

  Revision 1.51  1999/12/01 12:42:32  peter
    * fixed bug 698
    * removed some notes about unused vars

  Revision 1.50  1999/11/30 10:40:43  peter
    + ttype, tsymlist

  Revision 1.49  1999/11/17 17:04:59  pierre
   * Notes/hints changes

  Revision 1.48  1999/11/09 23:06:45  peter
    * esi_offset -> selfpointer_offset to be newcg compatible
    * hcogegen -> cgbase fixes for newcg

  Revision 1.47  1999/11/06 14:34:21  peter
    * truncated log to 20 revs

  Revision 1.46  1999/10/21 14:18:54  peter
    * tp7 fix

  Revision 1.45  1999/10/14 14:57:52  florian
    - removed the hcodegen use in the new cg, use cgbase instead

  Revision 1.44  1999/10/13 10:42:15  peter
    * cgmessagepos functions

  Revision 1.43  1999/09/27 23:44:51  peter
    * procinfo is now a pointer
    * support for result setting in sub procedure

  Revision 1.42  1999/08/26 20:24:40  michael
  + Hopefuly last fixes for resourcestrings

  Revision 1.41  1999/08/24 13:14:03  peter
    * MEMDEBUG to see the sizes of asmlist,asmsymbols,symtables

  Revision 1.40  1999/08/24 12:01:32  michael
  + changes for resourcestrings

  Revision 1.39  1999/08/19 13:10:18  pierre
   + faillabel for _FAIL

  Revision 1.38  1999/08/16 18:23:56  peter
    * reset resourcestringlist in newmodule.

  Revision 1.37  1999/08/04 00:23:02  florian
    * renamed i386asm and i386base to cpuasm and cpubase

  Revision 1.36  1999/08/01 23:09:26  michael
  * procbase -> cpubase

}
