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
          procedure restoreunitsym;
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
          procedure write_parameter_lists(skipdef:pprocdef);
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
          function  getvaluesize : longint;
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
  Revision 1.3  2000-08-13 12:54:56  peter
    * class member decl wrong then no other error after it
    * -vb has now also line numbering
    * -vb is also used for interface/implementation different decls and
      doesn't list the current function (merged)

  Revision 1.2  2000/07/13 11:32:50  michael
  + removed logs
 
}
