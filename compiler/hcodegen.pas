{
    $Id$
    Copyright (c) 1996-98 by Florian Klaempfl

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

  interface

    uses
      cobjects,
      tokens,verbose,
      aasm,symtable,cpubase;

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
       tprocinfo = record
          { pointer to parent in nested procedures }
          parent : pprocinfo;
          { current class, if we are in a method }
          _class : pobjectdef;
          { return type }
          retdef : pdef;
          { return type }
          sym : pprocsym;
          { symbol of the function }
          funcretsym : pfuncretsym;
          { the definition of the proc itself }
          { why was this a pdef only ?? PM    }
          def : pprocdef;
          { frame pointer offset }
          framepointer_offset : longint;
          { self pointer offset }
          ESI_offset : longint;
          { result value offset }
          retoffset : longint;

          { firsttemp position }
          firsttemp : longint;

          funcret_is_valid : boolean;

          { parameter offset }
          call_offset : longint;

          { some collected informations about the procedure }
          { see pi_xxxx above                          }
          flags : longint;

          { register used as frame pointer }
          framepointer : tregister;

          { true, if the procedure is exported by an unit }
          globalsymbol : boolean;

          { true, if the procedure should be exported (only OS/2) }
          exported : boolean;

          { code for the current procedure }
          aktproccode,aktentrycode,
          aktexitcode,aktlocaldata : paasmoutput;
          { local data is used for smartlink }
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
       procinfo : tprocinfo;

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
    procedure cgmessage(const t : tmsgconst);
    procedure cgmessage1(const t : tmsgconst;const s : string);
    procedure cgmessage2(const t : tmsgconst;const s1,s2 : string);
    procedure cgmessage3(const t : tmsgconst;const s1,s2,s3 : string);

    { initialize respectively terminates the code generator }
    { for a new module or procedure                      }
    procedure codegen_doneprocedure;
    procedure codegen_donemodule;
    procedure codegen_newmodule;
    procedure codegen_newprocedure;


implementation

     uses
        systems,globals,files,strings,cresstr;

{*****************************************************************************
            override the message calls to set codegenerror
*****************************************************************************}

    procedure cgmessage(const t : tmsgconst);
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

    procedure cgmessage1(const t : tmsgconst;const s : string);
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

    procedure cgmessage2(const t : tmsgconst;const s1,s2 : string);
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

    procedure cgmessage3(const t : tmsgconst;const s1,s2,s3 : string);
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


{*****************************************************************************
         initialize/terminate the codegen for procedure and modules
*****************************************************************************}

    procedure codegen_newprocedure;
      begin
         aktbreaklabel:=nil;
         aktcontinuelabel:=nil;
         { aktexitlabel:=0; is store in oldaktexitlabel
           so it must not be reset to zero before this storage !}
         { the type of this lists isn't important }
         { because the code of this lists is      }
         { copied to the code segment        }
         procinfo.aktentrycode:=new(paasmoutput,init);
         procinfo.aktexitcode:=new(paasmoutput,init);
         procinfo.aktproccode:=new(paasmoutput,init);
         procinfo.aktlocaldata:=new(paasmoutput,init);
      end;



    procedure codegen_doneprocedure;
      begin
         dispose(procinfo.aktentrycode,done);
         dispose(procinfo.aktexitcode,done);
         dispose(procinfo.aktproccode,done);
         dispose(procinfo.aktlocaldata,done);
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
         { resourcestrings }
         ResetResourceStrings;
      end;



    procedure codegen_donemodule;
      begin
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
         { assembler symbols }
         dispose(asmsymbollist,done);
         { resource strings }
         { if assigned(resourcestringlist) then
          dispose(resourcestringlist,done); }
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

end.

{
  $Log$
  Revision 1.40  1999-08-24 12:01:32  michael
  + changes for resourcestrings

  Revision 1.39  1999/08/19 13:10:18  pierre
   + faillabel for _FAIL

  Revision 1.38  1999/08/16 18:23:56  peter
    * reset resourcestringlist in newmodule.

  Revision 1.37  1999/08/04 00:23:02  florian
    * renamed i386asm and i386base to cpuasm and cpubase

  Revision 1.36  1999/08/01 23:09:26  michael
  * procbase -> cpubase

  Revision 1.35  1999/08/01 23:04:49  michael
  + Changes for Alpha

  Revision 1.34  1999/07/22 09:37:42  florian
    + resourcestring implemented
    + start of longstring support

  Revision 1.33  1999/05/27 19:44:31  peter
    * removed oldasm
    * plabel -> pasmlabel
    * -a switches to source writing automaticly
    * assembler readers OOPed
    * asmsymbol automaticly external
    * jumptables and other label fixes for asm readers

  Revision 1.32  1999/05/21 13:55:01  peter
    * NEWLAB for label as symbol

  Revision 1.31  1999/05/17 21:57:08  florian
    * new temporary ansistring handling

  Revision 1.30  1999/05/01 13:24:22  peter
    * merged nasm compiler
    * old asm moved to oldasm/

  Revision 1.29  1999/04/21 09:43:38  peter
    * storenumber works
    * fixed some typos in double_checksum
    + incompatible types type1 and type2 message (with storenumber)

  Revision 1.28  1999/03/24 23:17:00  peter
    * fixed bugs 212,222,225,227,229,231,233

  Revision 1.27  1999/02/25 21:02:37  peter
    * ag386bin updates
    + coff writer

  Revision 1.26  1999/02/22 02:15:21  peter
    * updates for ag386bin

  Revision 1.25  1999/01/21 22:10:45  peter
    * fixed array of const
    * generic platform independent high() support

  Revision 1.24  1998/12/29 18:48:18  jonas
    + optimize pascal code surrounding assembler blocks

  Revision 1.23  1998/11/27 14:50:38  peter
    + open strings, $P switch support

  Revision 1.22  1998/11/16 12:12:21  peter
    - generate_pascii which is obsolete

  Revision 1.21  1998/11/04 10:11:38  peter
    * ansistring fixes

  Revision 1.20  1998/10/29 15:42:48  florian
    + partial disposing of temp. ansistrings

  Revision 1.19  1998/10/26 22:58:18  florian
    * new introduded problem with classes fix, the parent class wasn't set
      correct, if the class was defined forward before

  Revision 1.18  1998/10/06 17:16:50  pierre
    * some memory leaks fixed (thanks to Peter for heaptrc !)

  Revision 1.17  1998/09/17 09:42:37  peter
    + pass_2 for cg386
    * Message() -> CGMessage() for pass_1/pass_2

  Revision 1.16  1998/09/07 18:46:04  peter
    * update smartlinking, uses getdatalabel
    * renamed ptree.value vars to value_str,value_real,value_set

  Revision 1.15  1998/09/01 09:02:51  peter
    * moved message() to hcodegen, so pass_2 also uses them

  Revision 1.14  1998/08/21 14:08:43  pierre
    + TEST_FUNCRET now default (old code removed)
      works also for m68k (at least compiles)

  Revision 1.13  1998/08/20 09:26:38  pierre
    + funcret setting in underproc testing
      compile with _dTEST_FUNCRET

  Revision 1.12  1998/08/10 14:50:01  peter
    + localswitches, moduleswitches, globalswitches splitting

  Revision 1.11  1998/07/28 21:52:51  florian
    + implementation of raise and try..finally
    + some misc. exception stuff

  Revision 1.10  1998/07/20 18:40:13  florian
    * handling of ansi string constants should now work

  Revision 1.9  1998/06/05 16:13:34  pierre
    * fix for real and string consts inside inlined procs

  Revision 1.8  1998/06/04 23:51:40  peter
    * m68k compiles
    + .def file creation moved to gendef.pas so it could also be used
      for win32

  Revision 1.7  1998/06/04 09:55:38  pierre
    * demangled name of procsym reworked to become independant of the mangling scheme

  Revision 1.6  1998/05/23 01:21:08  peter
    + aktasmmode, aktoptprocessor, aktoutputformat
    + smartlink per module $SMARTLINK-/+ (like MMX) and moved to aktswitches
    + $LIBNAME to set the library name where the unit will be put in
    * splitted cgi386 a bit (codeseg to large for bp7)
    * nasm, tasm works again. nasm moved to ag386nsm.pas

  Revision 1.5  1998/05/20 09:42:34  pierre
    + UseTokenInfo now default
    * unit in interface uses and implementation uses gives error now
    * only one error for unknown symbol (uses lastsymknown boolean)
      the problem came from the label code !
    + first inlined procedures and function work
      (warning there might be allowed cases were the result is still wrong !!)
    * UseBrower updated gives a global list of all position of all used symbols
      with switch -gb

  Revision 1.4  1998/05/07 00:17:01  peter
    * smartlinking for sets
    + consts labels are now concated/generated in hcodegen
    * moved some cpu code to cga and some none cpu depended code from cga
      to tree and hcodegen and cleanup of hcodegen
    * assembling .. output reduced for smartlinking ;)

  Revision 1.3  1998/05/06 08:38:40  pierre
    * better position info with UseTokenInfo
      UseTokenInfo greatly simplified
    + added check for changed tree after first time firstpass
      (if we could remove all the cases were it happen
      we could skip all firstpass if firstpasscount > 1)
      Only with ExtDebug

  Revision 1.2  1998/04/29 10:33:53  pierre
    + added some code for ansistring (not complete nor working yet)
    * corrected operator overloading
    * corrected nasm output
    + started inline procedures
    + added starstarn : use ** for exponentiation (^ gave problems)
    + started UseTokenInfo cond to get accurate positions
}
