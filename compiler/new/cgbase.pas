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
       globtype,cobjects,aasm,symtable,verbose,tree
{$I cpuunit.inc}
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
          selfpointer_offset : longint;
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

       { some kind of temp. types needs to be destructed }
       { for example ansistring, this is done using this }
       { list                                            }
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
          stringdispose(loc.reference.symbol);
        loc.loc:=LOC_INVALID;
      end;

    procedure set_location(var destloc,sourceloc : tlocation);

      begin
         { this is needed if you want to be able to delete }
         { the string with the nodes                       }
         if assigned(destloc.reference.symbol) then
           stringdispose(destloc.reference.symbol);
         destloc:= sourceloc;
         if sourceloc.loc in [LOC_MEM,LOC_REFERENCE] then
           begin
              if assigned(sourceloc.reference.symbol) then
                destloc.reference.symbol:=
                  stringdup(sourceloc.reference.symbol^);
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
  Revision 1.4  1999-01-23 23:29:45  florian
    * first running version of the new code generator
    * when compiling exceptions under Linux fixed

  Revision 1.3  1999/01/06 22:58:48  florian
    + some stuff for the new code generator

  Revision 1.2  1998/12/26 15:20:28  florian
    + more changes for the new version

  Revision 1.1  1998/12/15 22:18:55  florian
    * some code added
}