{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl, Pierre Muller

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

       { this object is the base for all symbol objects }
       tsym = object(tsymtableentry)
          typ        : tsymtyp;
          symoptions : tsymoptions;
          fileinfo   : tfileposinfo;
{$ifdef GDB}
          isstabwritten : boolean;
{$endif GDB}
          refs          : longint;
          lastref,
          defref,
          lastwritten : pref;
          refcount    : longint;
          constructor init(const n : string);
          constructor load;
          destructor done;virtual;
          procedure write;virtual;
          procedure prederef;virtual; { needed for ttypesym to be deref'd first }
          procedure deref;virtual;
          function  mangledname : string;virtual;
          procedure insert_in_data;virtual;
{$ifdef GDB}
          function  stabstring : pchar;virtual;
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
          used,
          defined : boolean;
          code : pointer; { should be ptree! }
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
          defined,
          defined_at_startup,
          is_used : boolean;
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

       tprocsym = object(tsym)
          definition  : pprocdef;
{$ifdef CHAINPROCSYMS}
          nextprocsym : pprocsym;
{$endif CHAINPROCSYMS}
          is_global   : boolean;
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
          procedure order_overloaded;
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

       ttypesym = object(tsym)
          restype    : ttype;
{$ifdef SYNONYM}
          synonym    : ptypesym;
{$endif}
{$ifdef GDB}
          isusedinstab : boolean;
{$endif GDB}
          constructor init(const n : string;const tt : ttype);
          constructor initdef(const n : string;d : pdef);
          constructor load;
{$ifdef SYNONYM}
          destructor done;virtual;
{$endif}
          procedure write;virtual;
          procedure prederef;virtual;
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

       pvarsym = ^tvarsym;
       tvarsym = object(tsym)
          address       : longint;
          localvarsym   : pvarsym;
          vartype       : ttype;
          varoptions    : tvaroptions;
          reg           : tregister; { if reg<>R_NO, then the variable is an register variable }
          varspez       : tvarspez;  { sets the type of access }
          varstate      : tvarstate;
          constructor init(const n : string;const tt : ttype);
          constructor init_dll(const n : string;const tt : ttype);
          constructor init_C(const n,mangled : string;const tt : ttype);
          constructor initdef(const n : string;p : pdef);
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
          propoptions   : tpropertyoptions;
          proptype      : ttype;
          propoverriden : ppropertysym;
          indextype     : ttype;
          index,
          default       : longint;
          readaccess,
          writeaccess,
          storedaccess  : psymlist;
          constructor init(const n : string);
          destructor  done;virtual;
          constructor load;
          function  getsize : longint;virtual;
          procedure write;virtual;
          procedure deref;virtual;
          procedure dooverride(overriden:ppropertysym);
{$ifdef GDB}
          function  stabstring : pchar;virtual;
          procedure concatstabto(asmlist : paasmoutput);virtual;
{$endif GDB}
       end;

       pfuncretsym = ^tfuncretsym;
       tfuncretsym = object(tsym)
          funcretprocinfo : pointer{ should be pprocinfo};
          rettype  : ttype;
          address  : longint;
          constructor init(const n : string;approcinfo : pointer{pprocinfo});
          constructor load;
          destructor  done;virtual;
          procedure write;virtual;
          procedure deref;virtual;
          procedure insert_in_data;virtual;
{$ifdef GDB}
          procedure concatstabto(asmlist : paasmoutput);virtual;
{$endif GDB}
       end;

       pabsolutesym = ^tabsolutesym;
       tabsolutesym = object(tvarsym)
          abstyp  : absolutetyp;
          absseg  : boolean;
          ref     : psym;
          asmname : pstring;
          constructor init(const n : string;const tt : ttype);
          constructor initdef(const n : string;p : pdef);
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
          typedconsttype  : ttype;
          is_really_const : boolean;
          constructor init(const n : string;p : pdef;really_const : boolean);
          constructor inittype(const n : string;const tt : ttype;really_const : boolean);
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

       pconstsym = ^tconstsym;
       tconstsym = object(tsym)
          consttype  : ttype;
          consttyp : tconsttyp;
          resstrindex,    { needed for resource strings }
          value,
          len        : longint; { len is needed for string length }
          constructor init(const n : string;t : tconsttyp;v : longint);
          constructor init_def(const n : string;t : tconsttyp;v : longint;def : pdef);
          constructor init_string(const n : string;t : tconsttyp;str:pchar;l:longint);
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
  Revision 1.48  2000-04-27 10:06:04  pierre
    * fix for snapshot failue
    * order_overloaded reintrocduced and adapted to operators

  Revision 1.47  2000/04/26 08:54:19  pierre
    * More changes for operator bug
      Order_overloaded method removed because it conflicted with
      new implementation where the defs are ordered
      according to the unit loading order !

  Revision 1.46  2000/02/09 13:23:05  peter
    * log truncated

  Revision 1.45  2000/01/07 01:14:41  peter
    * updated copyright to 2000

  Revision 1.44  2000/01/03 19:26:04  peter
    * fixed resolving of ttypesym which are reference from object/record
      fields.

  Revision 1.43  1999/12/14 09:58:42  florian
    + compiler checks now if a goto leaves an exception block

  Revision 1.42  1999/11/30 10:40:56  peter
    + ttype, tsymlist

  Revision 1.41  1999/11/26 00:19:12  peter
    * property overriding dereference fix, but it need a bigger redesign
      which i'll do tomorrow. This quick hack is for the lazarus ppl so
      they can hack on mwcustomedit.

  Revision 1.40  1999/11/17 17:05:06  pierre
   * Notes/hints changes

  Revision 1.39  1999/11/15 22:00:48  peter
    * labels used but not defined give error instead of warning, the warning
      is now only with declared but not defined and not used.

  Revision 1.38  1999/11/08 14:02:17  florian
    * problem with "index X"-properties solved
    * typed constants of class references are now allowed

  Revision 1.37  1999/11/06 14:34:28  peter
    * truncated log to 20 revs

  Revision 1.36  1999/10/01 08:02:48  peter
    * forward type declaration rewritten

  Revision 1.35  1999/09/26 21:30:22  peter
    + constant pointer support which can happend with typecasting like
      const p=pointer(1)
    * better procvar parsing in typed consts

  Revision 1.34  1999/08/31 15:42:26  pierre
   + tmacrosym is_used and defined_at_startup boolean fields added

  Revision 1.33  1999/08/23 11:45:45  michael
  * Hopefully final attempt at resourcestrings

  Revision 1.32  1999/08/14 00:39:01  peter
    * hack to support property with record fields

  Revision 1.31  1999/08/10 12:33:38  pierre
   * pprocsym defined earlier for use in tprocdef

  Revision 1.30  1999/08/03 22:03:21  peter
    * moved bitmask constants to sets
    * some other type/const renamings

  Revision 1.29  1999/07/27 23:42:23  peter
    * indirect type referencing is now allowed

  Revision 1.28  1999/07/24 15:13:01  michael
  changes for resourcestrings

  Revision 1.27  1999/07/22 09:37:57  florian
    + resourcestring implemented
    + start of longstring support

}