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

       symprop = byte;

       { possible types for symtable entries }
       tsymtyp = (abstractsym,varsym,typesym,procsym,unitsym,programsym,
                  constsym,enumsym,typedconstsym,errorsym,syssym,
                  labelsym,absolutesym,propertysym,funcretsym,
                  macrosym);
                  { varsym_C,typedconstsym_C); }

       { this object is the base for all symbol objects }
       psym = ^tsym;
       tsym = object
          typ        : tsymtyp;
          _name      : pchar;
          left,right : psym;
          speedvalue : longint;
          properties : symprop;
          owner      : psymtable;
          indexnb    : longint;
          fileinfo   : tfileposinfo;
{$ifdef GDB}
          isstabwritten : boolean;
{$endif GDB}
{$ifdef UseBrowser}
          lastref,
          defref,
          lastwritten : pref;
          refcount    : longint;
{$endif UseBrowser}
          constructor init(const n : string);
          constructor load;
          destructor done;virtual;
          procedure write;virtual;
          procedure deref;virtual;
          function name : string;
          function mangledname : string;virtual;
          procedure setname(const s : string);
          procedure insert_in_data;virtual;
{$ifdef GDB}
          function stabstring : pchar;virtual;
          procedure concatstabto(asmlist : paasmoutput);virtual;
{$endif GDB}
{$ifdef UseBrowser}
          procedure load_references;virtual;
          function  write_references : boolean;virtual;
          procedure add_to_browserlog;virtual;
{$endif UseBrowser}
       end;

       plabelsym = ^tlabelsym;
       tlabelsym = object(tsym)
          number : plabel;
          defined : boolean;
          constructor init(const n : string; l : plabel);
          constructor load;
          destructor done;virtual;
          function mangledname : string;virtual;
          procedure write;virtual;
       end;

       punitsym = ^tunitsym;
       tunitsym = object(tsym)
          unitsymtable : punitsymtable;
          prevsym : punitsym;
          refs : longint;
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
          buflen : longint;
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
          definition : pprocdef;
{$ifdef CHAINPROCSYMS}
          nextprocsym : pprocsym;
{$endif CHAINPROCSYMS}
{$ifdef GDB}
          is_global : boolean;{necessary for stab}
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
{$ifdef UseBrowser}
          procedure load_references;virtual;
          function  write_references : boolean;virtual;
          procedure add_to_browserlog;virtual;
{$endif UseBrowser}
{$ifdef GDB}
          function stabstring : pchar;virtual;
          procedure concatstabto(asmlist : paasmoutput);virtual;
{$endif GDB}
       end;

       ttypesym = object(tsym)
          definition : pdef;
          forwardpointer : ppointerdef;
{$ifdef GDB}
          isusedinstab : boolean;
{$endif GDB}
          constructor init(const n : string;d : pdef);
          constructor load;
          destructor done;virtual;
          procedure write;virtual;
          procedure deref;virtual;
{$ifdef UseBrowser}
          procedure load_references;virtual;
          function  write_references : boolean;virtual;
          procedure add_to_browserlog;virtual;
{$endif UseBrowser}
{$ifdef GDB}
          function stabstring : pchar;virtual;
          procedure concatstabto(asmlist : paasmoutput);virtual;
{$endif GDB}
       end;

       pvarsym = ^tvarsym;
       tvarsym = object(tsym)
          address      : longint;
          definition   : pdef;
          refs         : longint;
          var_options  : byte;
          _mangledname : pchar;
          reg          : tregister; { if reg<>R_NO, then the variable is an register variable }
          varspez      : tvarspez;  { sets the type of access }
          is_valid     : byte;
          constructor init(const n : string;p : pdef);
          constructor load;
          constructor init_C(const n,mangled : string;p : pdef);
          constructor load_C;
          destructor done;virtual;
          function mangledname : string;virtual;
          procedure insert_in_data;virtual;
          function getsize : longint;
          procedure write;virtual;
          procedure deref;virtual;
{$ifdef GDB}
          function stabstring : pchar;virtual;
          procedure concatstabto(asmlist : paasmoutput);virtual;
{$endif GDB}
       end;

       ppropertysym = ^tpropertysym;
       tpropertysym = object(tsym)
          options : longint;
          proptype : pdef;
          { proppara : pdefcoll; }
          readaccesssym,writeaccesssym,storedsym : psym;
          readaccessdef,writeaccessdef,storeddef : pdef;
          index,default : longint;
          constructor init(const n : string);
          destructor done;virtual;
          constructor load;
          function getsize : longint;virtual;
          procedure write;virtual;
          procedure deref;virtual;
{$ifdef GDB}
          { I don't know how (FK) }
          function stabstring : pchar;virtual;
          procedure concatstabto(asmlist : paasmoutput);virtual;
{$endif GDB}
       end;

       pfuncretsym = ^tfuncretsym;
       tfuncretsym = object(tsym)
          funcretprocinfo : pointer{ should be pprocinfo};
          funcretdef : pdef;
          address : longint;
          constructor init(const n : string;approcinfo : pointer{pprocinfo});
{$ifdef GDB}
          procedure concatstabto(asmlist : paasmoutput);virtual;
{$endif GDB}
       end;

       absolutetyp = (tovar,toasm,toaddr);

       pabsolutesym = ^tabsolutesym;
       tabsolutesym = object(tvarsym)
          abstyp : absolutetyp;
          absseg : boolean;
          ref : psym;
          asmname : pstring;
          constructor load;
          procedure deref;virtual;
          function mangledname : string;virtual;
          procedure write;virtual;
          procedure insert_in_data;virtual;
          { this creates a problem in gen_vmt !!!!!
          because the pdef is not resolved yet !!
          we should fix this
          constructor init(const s : string;p : pdef;newref : psym);}
{$ifdef GDB}
          procedure concatstabto(asmlist : paasmoutput);virtual;
{$endif GDB}
       end;

       ptypedconstsym = ^ttypedconstsym;
       ttypedconstsym = object(tsym)
          prefix : pstring;
          definition : pdef;
          constructor init(const n : string;p : pdef);
          constructor load;
          destructor done;virtual;
          function  mangledname : string;virtual;
          procedure write;virtual;
          procedure deref;virtual;
          procedure insert_in_data;virtual;
          procedure really_insert_in_data;
{$ifdef GDB}
          function stabstring : pchar;virtual;
{$endif GDB}
       end;

       tconsttype = (constord,conststring,constreal,constbool,
                     constint,constchar,constset);

       pconstsym = ^tconstsym;
       tconstsym = object(tsym)
          definition : pdef;
          consttype  : tconsttype;
          value      : longint;
          constructor init(const n : string;t : tconsttype;v : longint;def : pdef);
          constructor load;
          function  mangledname : string;virtual;
          destructor done;virtual;
          procedure deref;virtual;
          procedure write;virtual;
{$ifdef GDB}
          function stabstring : pchar;virtual;
          procedure concatstabto(asmlist : paasmoutput);virtual;
{$endif GDB}
       end;

       tenumsym = object(tsym)
          value : longint;
          definition : penumdef;
          next : penumsym;
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
          procedure write;virtual;
{$ifdef GDB}
          procedure concatstabto(asmlist : paasmoutput);virtual;
{$endif GDB}
       end;

{
  $Log$
  Revision 1.3  1998-10-08 17:17:34  pierre
    * current_module old scanner tagged as invalid if unit is recompiled
    + added ppheap for better info on tracegetmem of heaptrc
      (adds line column and file index)
    * several memory leaks removed ith help of heaptrc !!

  Revision 1.2  1998/09/24 15:11:18  peter
    * fixed enum for not GDB

  Revision 1.1  1998/09/23 12:03:57  peter
    * overloading fix for array of const

}
