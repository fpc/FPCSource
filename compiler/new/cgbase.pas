{
    $Id$
    Copyright (c) 1993-98 by Florian Klaempfl

    This units implements some code generator helper routines

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

  interface

    uses
       globtype,cobjects,aasm,symconst,symtable,verbose,tree,cpuasm,cpubase;

    const
       pi_uses_asm  = $1;       { set, if the procedure uses asm }
       pi_is_global = $2;       { set, if the procedure is exported by an unit }
       pi_do_call   = $4;       { set, if the procedure does a call }
       pi_operator  = $8;       { set, if the procedure is an operator   }
       pi_C_import  = $10;      { set, if the procedure is an external C function }
       pi_uses_exceptions = $20;{ set, if the procedure has a try statement => }
                                { no register variables                        }
       pi_is_assembler = $40;   { set if the procedure is declared as ASSEMBLER
                                  => don't optimize}
       pi_needs_implicit_finally = $80; { set, if the procedure contains data which }
                                        { needs to be finalized              }

    type
       TOpCg = (OP_ADD,OP_AND,OP_DIV,OP_IDIV,OP_IMUL,OP_MUL,OP_NEG,OP_NOT,
                   OP_OR,OP_SAR,OP_SHL,OP_SHR,OP_SUB,OP_XOR);

       TOpCmp = (OC_NONE,OC_EQ,OC_GT,OC_LT,OC_GTE,OC_LTE,OC_NE,OC_BE,OC_B,
                 OC_AE,OC_A);

       TCgSize = (OS_NO,OS_8,OS_16,OS_32,OS_64);

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
          call_offset : longint;

          { every register which must be saved by the entry code }
          { (and restored by the exit code) must be in that set  }
          registerstosave : tregisterset;

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

          constructor init;
          destructor done;
       end;

       { some kind of temp. types needs to be destructed }
       { for example ansistring, this is done using this }
       { list                                            }
       ptemptodestroy = ^ttemptodestroy;
       ttemptodestroy = object(tlinkedlist_item)
          typ : pdef;
          address : treference;
          constructor init(const a : treference;p : pdef);
       end;

    const
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

       { this is for open arrays and strings        }
       { but be careful, this data is in the        }
       { generated code destroyed quick, and also   }
       { the next call of secondload destroys this  }
       { data                                       }
       { So be careful using the informations       }
       { provided by this variables                 }
       highframepointer : tregister;
       highoffset : longint;

       make_const_global : boolean;
       temptoremove : plinkedlist;

    { message calls with codegenerror support }
    procedure cgmessage(const t : tmsgconst);
    procedure cgmessage1(const t : tmsgconst;const s : string);
    procedure cgmessage2(const t : tmsgconst;const s1,s2 : string);
    procedure cgmessage3(const t : tmsgconst;const s1,s2,s3 : string);

    procedure CGMessagePos(const pos:tfileposinfo;t:tmsgconst);
    procedure CGMessagePos1(const pos:tfileposinfo;t:tmsgconst;const s1:string);
    procedure CGMessagePos2(const pos:tfileposinfo;t:tmsgconst;const s1,s2:string);
    procedure CGMessagePos3(const pos:tfileposinfo;t:tmsgconst;const s1,s2,s3:string);

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

    { clears a location record }
    procedure clear_location(var loc : tlocation);
    { copies a location, takes care of the symbol }
    procedure set_location(var destloc,sourceloc : tlocation);
    { swaps two locations }
    procedure swap_location(var destloc,sourceloc : tlocation);

  implementation

     uses
        comphook;

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
        call_offset:=0;
        registerstosave:=[];
        flags:=0;
        framepointer:=R_NO;
        globalsymbol:=false;
        exported:=false;
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
         new(procinfo,init);
         { aktexitlabel:=0; is store in oldaktexitlabel
           so it must not be reset to zero before this storage !}
      end;



    procedure codegen_doneprocedure;
      begin
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
         asmsymbollist:=new(pasmsymbollist,init);
         asmsymbollist^.usehash;
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
         if assigned(resourcestringlist) then
          dispose(resourcestringlist,done);
         dispose(asmsymbollist,done);
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
                              TTempToDestroy
*****************************************************************************}

    constructor ttemptodestroy.init(const a : treference;p : pdef);
      begin
         inherited init;
         address:=a;
         typ:=p;
      end;

{*****************************************************************************
            some helper routines to handle locations
*****************************************************************************}

    procedure clear_location(var loc : tlocation);

      begin
        if ((loc.loc=LOC_MEM) or (loc.loc=LOC_REFERENCE)) and
           assigned(loc.reference.symbol) then
          dispose(loc.reference.symbol,done);
        loc.loc:=LOC_INVALID;
      end;

    procedure set_location(var destloc,sourceloc : tlocation);

      begin
         { this is needed if you want to be able to delete }
         { the string with the nodes                       }
         if assigned(destloc.reference.symbol) then
           dispose(destloc.reference.symbol,done);
         destloc:= sourceloc;
         if sourceloc.loc in [LOC_MEM,LOC_REFERENCE] then
           begin
              if assigned(sourceloc.reference.symbol) then
                destloc.reference.symbol:=
                  sourceloc.reference.symbol;
           end
         else
           destloc.reference.symbol:=nil;
      end;

    procedure swap_location(var destloc,sourceloc : tlocation);

      var
         swapl : tlocation;

      begin
         swapl:=destloc;
         destloc:=sourceloc;
         sourceloc:=swapl;
      end;

end.
{
  $Log$
  Revision 1.14  1999-12-24 22:47:42  jonas
    * added OC_NONE to the compare forms (to allow unconditional jumps)

  Revision 1.13  1999/12/01 12:42:33  peter
    * fixed bug 698
    * removed some notes about unused vars

  Revision 1.12  1999/11/05 13:15:00  florian
    * some fixes to get the new cg compiling again

  Revision 1.11  1999/10/14 14:57:54  florian
    - removed the hcodegen use in the new cg, use cgbase instead

  Revision 1.10  1999/10/12 21:20:46  florian
    * new codegenerator compiles again

  Revision 1.9  1999/09/10 18:48:11  florian
    * some bug fixes (e.g. must_be_valid and procinfo.funcret_is_valid)
    * most things for stored properties fixed

  Revision 1.8  1999/08/06 13:26:49  florian
    * more changes ...

  Revision 1.7  1999/08/05 14:58:10  florian
    * some fixes for the floating point registers
    * more things for the new code generator

  Revision 1.6  1999/08/04 00:23:51  florian
    * renamed i386asm and i386base to cpuasm and cpubase

  Revision 1.5  1999/08/01 18:22:32  florian
   * made it again compilable

  Revision 1.4  1999/01/23 23:29:45  florian
    * first running version of the new code generator
    * when compiling exceptions under Linux fixed

  Revision 1.3  1999/01/06 22:58:48  florian
    + some stuff for the new code generator

  Revision 1.2  1998/12/26 15:20:28  florian
    + more changes for the new version

  Revision 1.1  1998/12/15 22:18:55  florian
    * some code added
}