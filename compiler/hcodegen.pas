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
      verbose,aasm,tree,symtable
{$ifdef i386}
      ,i386
{$endif}
{$ifdef m68k}
      ,m68k
{$endif}
      ;

    const
       pi_uses_asm  = $1;       { set, if the procedure uses asm }
       pi_is_global = $2;       { set, if the procedure is exported by an unit }
       pi_do_call   = $4;       { set, if the procedure does a call }
       pi_operator  = $8;       { set, if the procedure is an operator   }
       pi_C_import  = $10;      { set, if the procedure is an external C function }
       pi_uses_exceptions = $20;{ set, if the procedure has a try statement => }
                                { no register variables                        }

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
          { see pi_xxxx above                               }
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

    var
       { info about the current sub routine }
       procinfo : tprocinfo;

       { labels for BREAK and CONTINUE }
       aktbreaklabel,aktcontinuelabel : plabel;

       { label when the result is true or false }
       truelabel,falselabel : plabel;

       { label to leave the sub routine }
       aktexitlabel : plabel;

       { also an exit label, only used we need to clear only the stack }
       aktexit2label : plabel;

       { only used in constructor for fail or if getmem fails }
       quickexitlabel : plabel;

       { Boolean, wenn eine loadn kein Assembler erzeugt hat }
       simple_loadn : boolean;

       { tries to hold the amount of times which the current tree is processed  }
       t_times : longint;

       { true, if an error while code generation occurs }
       codegenerror : boolean;

    { message calls with codegenerror support }
    procedure cgmessage(const t : tmsgconst);
    procedure cgmessage1(const t : tmsgconst;const s : string);
    procedure cgmessage2(const t : tmsgconst;const s1,s2 : string);
    procedure cgmessage3(const t : tmsgconst;const s1,s2,s3 : string);


    { initialize respectively terminates the code generator }
    { for a new module or procedure                         }
    procedure codegen_doneprocedure;
    procedure codegen_donemodule;
    procedure codegen_newmodule;
    procedure codegen_newprocedure;

    { counts the labels }
    function case_count_labels(root : pcaserecord) : longint;
    { searches the highest label }
    function case_get_max(root : pcaserecord) : longint;
    { searches the lowest label }
    function case_get_min(root : pcaserecord) : longint;

    { concates/inserts the ASCII string to the data segment }
    procedure generate_ascii(a : paasmoutput;const hs : string);
    { concates/inserts the ASCII string from pchar to the data  segment }
    { WARNING : if hs has no #0 and strlen(hs)=length           }
    { the terminal zero is not written                          }
    procedure generate_pascii(a : paasmoutput;hs : pchar;length : longint);

    { convert/concats a label for constants in the consts section }
{    function constlabel2str(l : plabel;ctype:tconsttype):string;
    function constlabelnb2str(pnb : longint;ctype:tconsttype):string;
    procedure concat_constlabel(p:plabel;ctype:tconsttype); }

    { to be able to force to have a global label for const }
    const
       make_const_global : boolean = false;

implementation

     uses
        systems,comphook,cobjects,globals,files,strings;

{*****************************************************************************
            override the message calls to set codegenerror
*****************************************************************************}

    procedure cgmessage(const t : tmsgconst);
      var
         olderrorcount : longint;
      begin
         if not(codegenerror) then
           begin
              olderrorcount:=status.errorcount;
              verbose.Message(t);
              codegenerror:=olderrorcount<>status.errorcount;
           end;
      end;

    procedure cgmessage1(const t : tmsgconst;const s : string);
      var
         olderrorcount : longint;
      begin
         if not(codegenerror) then
           begin
              olderrorcount:=status.errorcount;
              verbose.Message1(t,s);
              codegenerror:=olderrorcount<>status.errorcount;
           end;
      end;

    procedure cgmessage2(const t : tmsgconst;const s1,s2 : string);
      var
         olderrorcount : longint;
      begin
         if not(codegenerror) then
           begin
              olderrorcount:=status.errorcount;
              verbose.Message2(t,s1,s2);
              codegenerror:=olderrorcount<>status.errorcount;
           end;
      end;

    procedure cgmessage3(const t : tmsgconst;const s1,s2,s3 : string);
      var
         olderrorcount : longint;
      begin
         if not(codegenerror) then
           begin
              olderrorcount:=status.errorcount;
              verbose.Message3(t,s1,s2,s3);
              codegenerror:=olderrorcount<>status.errorcount;
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
         { copied to the code segment             }
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
         externals:=new(paasmoutput,init);
         internals:=new(paasmoutput,init);
         consts:=new(paasmoutput,init);
         rttilist:=new(paasmoutput,init);
         importssection:=nil;
         exportssection:=nil;
         resourcesection:=nil;
      end;



    procedure codegen_donemodule;
      begin
         dispose(exprasmlist,done);
         dispose(codesegment,done);
         dispose(bsssegment,done);
         dispose(datasegment,done);
         dispose(debuglist,done);
         dispose(externals,done);
         dispose(internals,done);
         dispose(consts,done);
         dispose(rttilist,done);
         if assigned(importssection) then
          dispose(importssection,done);
         if assigned(exportssection) then
          dispose(exportssection,done);
         if assigned(resourcesection) then
          dispose(resourcesection,done);
      end;


{*****************************************************************************
                              Case Helpers
*****************************************************************************}

    function case_count_labels(root : pcaserecord) : longint;
      var
         _l : longint;

      procedure count(p : pcaserecord);
        begin
           inc(_l);
           if assigned(p^.less) then
             count(p^.less);
           if assigned(p^.greater) then
             count(p^.greater);
        end;

      begin
         _l:=0;
         count(root);
         case_count_labels:=_l;
      end;


    function case_get_max(root : pcaserecord) : longint;
      var
         hp : pcaserecord;
      begin
         hp:=root;
         while assigned(hp^.greater) do
           hp:=hp^.greater;
         case_get_max:=hp^._high;
      end;


    function case_get_min(root : pcaserecord) : longint;
      var
         hp : pcaserecord;
      begin
         hp:=root;
         while assigned(hp^.less) do
           hp:=hp^.less;
         case_get_min:=hp^._low;
      end;


{*****************************************************************************
                              String Helpers
*****************************************************************************}

    procedure generate_ascii(a : paasmoutput;const hs : string);
      begin
         a^.concat(new(pai_string,init(hs)))
      end;


    function strnew(p : pchar;length : longint) : pchar;
      var
         pc : pchar;
      begin
         getmem(pc,length);
         move(p^,pc^,length);
         strnew:=pc;
      end;


    { concates the ASCII string from pchar to the asmslist a }
    procedure generate_pascii(a : paasmoutput;hs : pchar;length : longint);
      var
         real_end,current_begin,current_end : pchar;
         c :char;
      begin
         if assigned(hs) then
           begin
              current_begin:=hs;
              real_end:=strend(hs);
              c:=hs[0];
              while length>32 do
                begin
                   { restore the char displaced }
                   current_begin[0]:=c;
                   current_end:=current_begin+32;
                   { store the char for next loop }
                   c:=current_end[0];
                   current_end[0]:=#0;
                   a^.concat(new(pai_string,init_length_pchar(strnew(current_begin,32),32)));
                   current_begin:=current_end;
                   length:=length-32;
                end;
              current_begin[0]:=c;
              a^.concat(new(pai_string,init_length_pchar(strnew(current_begin,length),length)));
           end;
      end;


end.

{
  $Log$
  Revision 1.18  1998-10-06 17:16:50  pierre
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
