{
    $Id$
    Copyright (c) 1993-98 by Florian Klaempfl, Pierre Muller

    Interface for the symbols types of the symtable

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

{************************************************
                   TSym
************************************************}

       { possible types for symtable entries }
       tsymtyp = (abstractsym,varsym,typesym,procsym,unitsym,programsym,
                  constsym,enumsym,typedconstsym,errorsym,syssym,
                  labelsym,absolutesym,propertysym,funcretsym,
                  macrosym);

       { this object is the base for all symbol objects }
       psym = ^tsym;
       tsym = object(tsymtableentry)
          typ        : tsymtyp;
          symoptions : tsymoptions;
          fileinfo   : tfileposinfo;
{$ifdef GDB}
          isstabwritten : boolean;
{$endif GDB}
          lastref,
          defref,
          lastwritten : pref;
          refcount    : longint;
          constructor init(const n : string);
          constructor load;
          destructor done;virtual;
          procedure write;virtual;
          procedure deref;virtual;
          function mangledname : string;virtual;
          procedure insert_in_data;virtual;
{$ifdef GDB}
          function stabstring : pchar;virtual;
          procedure concatstabto(asmlist : paasmoutput);virtual;
{$endif GDB}
          procedure load_references;virtual;
          function  write_references : boolean;virtual;
{$ifdef BrowserLog}
          procedure add_to_browserlog;virtual;
{$endif BrowserLog}
       end;

       plabelsym = ^tlabelsym;
       tlabelsym = object(tsym)
          lab     : pasmlabel;
          defined : boolean;
          constructor init(const n : string; l : pasmlabel);
          destructor done;virtual;
          constructor load;
          function mangledname : string;virtual;
          procedure write;virtual;
       end;

       punitsym = ^tunitsym;
       tunitsym = object(tsym)
          unitsymtable : punitsymtable;
          prevsym      : punitsym;
          refs         : longint;
          constructor init(const n : string;ref : punitsymtable);
          constructor load;
          destructor done;virtual;
          procedure write;virtual;
{$ifdef GDB}
          procedure concatstabto(asmlist : paasmoutput);virtual;
{$endif GDB}
       end;

       pmacrosym = ^tmacrosym;
       tmacrosym = object(tsym)
          defined : boolean;
          buftext : pchar;
          buflen  : longint;
          { macros aren't written to PPU files ! }
          constructor init(const n : string);
          destructor done;virtual;
       end;

       perrorsym = ^terrorsym;
       terrorsym = object(tsym)
          constructor init;
       end;

       pprocsym = ^tprocsym;
       tprocsym = object(tsym)
          definition  : pprocdef;
{$ifdef CHAINPROCSYMS}
          nextprocsym : pprocsym;
{$endif CHAINPROCSYMS}
{$ifdef GDB}
          is_global   : boolean; { necessary for stab }
{$endif GDB}
          constructor init(const n : string);
          constructor load;
          destructor done;virtual;
          function mangledname : string;virtual;
          function demangledname:string;
          { writes all declarations }
          procedure write_parameter_lists;
          { tests, if all procedures definitions are defined and not }
          { only forward                                             }
          procedure check_forward;
          procedure write;virtual;
          procedure deref;virtual;
          procedure load_references;virtual;
          function  write_references : boolean;virtual;
{$ifdef BrowserLog}
          procedure add_to_browserlog;virtual;
{$endif BrowserLog}
{$ifdef GDB}
          function stabstring : pchar;virtual;
          procedure concatstabto(asmlist : paasmoutput);virtual;
{$endif GDB}
       end;

       pforwardpointer=^tforwardpointer;
       tforwardpointer=record
         next : pforwardpointer;
         def  : ppointerdef;
       end;

       ttypesym = object(tsym)
          definition : pdef;
          synonym    : ptypesym;
{$ifdef GDB}
          isusedinstab : boolean;
{$endif GDB}
          constructor init(const n : string;d : pdef);
          constructor load;
          destructor done;virtual;
          procedure write;virtual;
          procedure deref;virtual;
          procedure addforwardpointer(p:ppointerdef);
          procedure updateforwarddef(p:pdef);
          procedure load_references;virtual;
          function  write_references : boolean;virtual;
{$ifdef BrowserLog}
          procedure add_to_browserlog;virtual;
{$endif BrowserLog}
{$ifdef GDB}
          function stabstring : pchar;virtual;
          procedure concatstabto(asmlist : paasmoutput);virtual;
{$endif GDB}
       private
          forwardpointer : pforwardpointer;
       end;

       pvarsym = ^tvarsym;
       tvarsym = object(tsym)
          address       : longint;
          localvarsym   : pvarsym;
          islocalcopy   : boolean;
          definition    : pdef;
          definitionsym : ptypesym;
          refs          : longint;
          varoptions    : tvaroptions;
          reg           : tregister; { if reg<>R_NO, then the variable is an register variable }
          varspez       : tvarspez;  { sets the type of access }
          varstate      : tvarstate;
          constructor init(const n : string;p : pdef);
          constructor init_dll(const n : string;p : pdef);
          constructor init_C(const n,mangled : string;p : pdef);
          constructor initsym(const n : string;p : ptypesym);
          constructor initsym_dll(const n : string;p : ptypesym);
          constructor initsym_C(const n,mangled : string;p : ptypesym);
          constructor load;
          destructor  done;virtual;
          procedure write;virtual;
          procedure deref;virtual;
          procedure setmangledname(const s : string);
          function  mangledname : string;virtual;
          procedure insert_in_data;virtual;
          function  getsize : longint;
          function  getpushsize : longint;
{$ifdef GDB}
          function  stabstring : pchar;virtual;
          procedure concatstabto(asmlist : paasmoutput);virtual;
{$endif GDB}
       private
          _mangledname  : pchar;
       end;

       ppropertysym = ^tpropertysym;
       tpropertysym = object(tsym)
          propoptions : tpropertyoptions;
          proptype    : pdef;
          readaccesssym,writeaccesssym,storedsym : psym;
          readaccessdef,writeaccessdef,storeddef : pdef;
          index,default : longint;
          constructor init(const n : string);
          destructor  done;virtual;
          constructor load;
          function  getsize : longint;virtual;
          procedure write;virtual;
          procedure deref;virtual;
{$ifdef GDB}
          function  stabstring : pchar;virtual;
          procedure concatstabto(asmlist : paasmoutput);virtual;
{$endif GDB}
       end;

       pfuncretsym = ^tfuncretsym;
       tfuncretsym = object(tsym)
          funcretprocinfo : pointer{ should be pprocinfo};
          funcretdef : pdef;
          address    : longint;
          constructor init(const n : string;approcinfo : pointer{pprocinfo});
          constructor load;
          procedure write;virtual;
          procedure deref;virtual;
          procedure insert_in_data;virtual;
{$ifdef GDB}
          procedure concatstabto(asmlist : paasmoutput);virtual;
{$endif GDB}
       end;

       absolutetyp = (tovar,toasm,toaddr);

       pabsolutesym = ^tabsolutesym;
       tabsolutesym = object(tvarsym)
          abstyp  : absolutetyp;
          absseg  : boolean;
          ref     : psym;
          asmname : pstring;
          constructor init(const n : string;p : pdef);
          constructor load;
          procedure deref;virtual;
          function  mangledname : string;virtual;
          procedure write;virtual;
          procedure insert_in_data;virtual;
{$ifdef GDB}
          procedure concatstabto(asmlist : paasmoutput);virtual;
{$endif GDB}
       end;

       ptypedconstsym = ^ttypedconstsym;
       ttypedconstsym = object(tsym)
          prefix          : pstring;
          definition      : pdef;
          definitionsym   : ptypesym;
          is_really_const : boolean;
          constructor init(const n : string;p : pdef;really_const : boolean);
          constructor initsym(const n : string;p : ptypesym;really_const : boolean);
          constructor load;
          destructor done;virtual;
          function  mangledname : string;virtual;
          procedure write;virtual;
          procedure deref;virtual;
          function  getsize:longint;
          procedure insert_in_data;virtual;
{$ifdef GDB}
          function  stabstring : pchar;virtual;
{$endif GDB}
       end;

       tconsttype = (constord,conststring,constreal,constbool,
                     constint,constchar,constset,constnil,
                     constresourcestring);

       pconstsym = ^tconstsym;
       tconstsym = object(tsym)
          definition : pdef;
          consttype  : tconsttype;
          reshash,    { needed for resource strings }
          value,
          len        : longint; { len is needed for string length }
          constructor init(const n : string;t : tconsttype;v : longint);
          constructor init_def(const n : string;t : tconsttype;v : longint;def : pdef);
          constructor init_string(const n : string;t : tconsttype;str:pchar;l:longint);
          constructor load;
          destructor  done;virtual;
          function  mangledname : string;virtual;
          procedure deref;virtual;
          procedure write;virtual;
{$ifdef GDB}
          function  stabstring : pchar;virtual;
          procedure concatstabto(asmlist : paasmoutput);virtual;
{$endif GDB}
       end;

       tenumsym = object(tsym)
          value      : longint;
          definition : penumdef;
          nextenum   : penumsym;
          constructor init(const n : string;def : penumdef;v : longint);
          constructor load;
          procedure write;virtual;
          procedure deref;virtual;
          procedure order;
{$ifdef GDB}
          procedure concatstabto(asmlist : paasmoutput);virtual;
{$endif GDB}
       end;

       pprogramsym = ^tprogramsym;
       tprogramsym = object(tsym)
          constructor init(const n : string);
       end;

       psyssym = ^tsyssym;
       tsyssym = object(tsym)
          number : longint;
          constructor init(const n : string;l : longint);
          constructor load;
          destructor  done;virtual;
          procedure write;virtual;
{$ifdef GDB}
          procedure concatstabto(asmlist : paasmoutput);virtual;
{$endif GDB}
       end;

{
  $Log$
  Revision 1.30  1999-08-03 22:03:21  peter
    * moved bitmask constants to sets
    * some other type/const renamings

  Revision 1.29  1999/07/27 23:42:23  peter
    * indirect type referencing is now allowed

  Revision 1.28  1999/07/24 15:13:01  michael
  changes for resourcestrings

  Revision 1.27  1999/07/22 09:37:57  florian
    + resourcestring implemented
    + start of longstring support

  Revision 1.26  1999/05/27 19:45:07  peter
    * removed oldasm
    * plabel -> pasmlabel
    * -a switches to source writing automaticly
    * assembler readers OOPed
    * asmsymbol automaticly external
    * jumptables and other label fixes for asm readers

  Revision 1.25  1999/05/21 13:55:23  peter
    * NEWLAB for label as symbol

  Revision 1.24  1999/05/20 22:22:45  pierre
    + added synonym filed for ttypesym
      allows a clean disposal of tdefs and related ttypesyms

  Revision 1.23  1999/05/13 21:59:47  peter
    * removed oldppu code
    * warning if objpas is loaded from uses
    * first things for new deref writing

  Revision 1.22  1999/04/26 13:31:53  peter
    * release storenumber,double_checksum

  Revision 1.21  1999/04/25 22:38:40  pierre
    + added is_really_const booleanfield for typedconstsym
      for Delphi in $J- mode (not yet implemented !)

  Revision 1.20  1999/04/21 09:43:56  peter
    * storenumber works
    * fixed some typos in double_checksum
    + incompatible types type1 and type2 message (with storenumber)

  Revision 1.19  1999/04/17 13:16:23  peter
    * fixes for storenumber

  Revision 1.18  1999/04/14 09:15:03  peter
    * first things to store the symbol/def number in the ppu

  Revision 1.17  1999/03/31 13:55:23  peter
    * assembler inlining working for ag386bin

  Revision 1.16  1999/03/24 23:17:29  peter
    * fixed bugs 212,222,225,227,229,231,233

  Revision 1.15  1999/02/22 13:07:11  pierre
    + -b and -bl options work !
    + cs_local_browser ($L+) is disabled if cs_browser ($Y+)
      is not enabled when quitting global section
    * local vars and procedures are not yet stored into PPU

  Revision 1.14  1999/01/20 10:20:22  peter
    * don't make localvar copies for assembler procedures

  Revision 1.13  1999/01/14 21:49:59  peter
    * fixed forwardpointer problem with multiple forwards for the same
      typesym. It now uses a linkedlist instead of a single pointer

  Revision 1.12  1999/01/12 14:25:37  peter
    + BrowserLog for browser.log generation
    + BrowserCol for browser info in TCollections
    * released all other UseBrowser

  Revision 1.11  1998/12/30 22:15:55  peter
    + farpointer type
    * absolutesym now also stores if its far

  Revision 1.10  1998/12/30 13:41:15  peter
    * released valuepara

  Revision 1.9  1998/11/28 16:20:57  peter
    + support for dll variables

  Revision 1.8  1998/11/18 15:44:19  peter
    * VALUEPARA for tp7 compatible value parameters

  Revision 1.7  1998/11/16 10:13:50  peter
    * label defines are checked at the end of the proc

  Revision 1.6  1998/11/13 10:18:12  peter
    + nil constants

  Revision 1.5  1998/11/05 23:39:32  peter
    + typedconst.getsize

  Revision 1.4  1998/10/20 08:07:02  pierre
    * several memory corruptions due to double freemem solved
      => never use p^.loc.location:=p^.left^.loc.location;
    + finally I added now by default
      that ra386dir translates global and unit symbols
    + added a first field in tsymtable and
      a nextsym field in tsym
      (this allows to obtain ordered type info for
      records and objects in gdb !)

  Revision 1.3  1998/10/08 17:17:34  pierre
    * current_module old scanner tagged as invalid if unit is recompiled
    + added ppheap for better info on tracegetmem of heaptrc
      (adds line column and file index)
    * several memory leaks removed ith help of heaptrc !!

  Revision 1.2  1998/09/24 15:11:18  peter
    * fixed enum for not GDB

  Revision 1.1  1998/09/23 12:03:57  peter
    * overloading fix for array of const

}
