{
    Copyright (c) 1998-2002 by Florian Klaempfl, Pierre Muller

    Symbol table implementation for the definitions

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
unit symdef;

{$i fpcdefs.inc}

interface

    uses
       { common }
       cutils,cclasses,
       { global }
       globtype,globals,tokens,
       { symtable }
       symconst,symbase,symtype,
       { ppu }
       ppu,
       { node }
       node,
       { aasm }
       aasmbase,aasmtai,
       cpubase,cpuinfo,
       cgbase,cgutils,
       parabase
       ;


    type
{************************************************
                    TDef
************************************************}

       tstoreddef = class(tdef)
       protected
          typesymderef  : tderef;
       public
          { persistent (available across units) rtti and init tables }
          rttitablesym,
          inittablesym  : tsym; {trttisym}
          rttitablesymderef,
          inittablesymderef : tderef;
          { local (per module) rtti and init tables }
          localrttilab  : array[trttitype] of tasmlabel;
          { linked list of global definitions }
{$ifdef EXTDEBUG}
          fileinfo   : tfileposinfo;
{$endif}
{$ifdef GDB}
          globalnb   : word;
          stab_state : tdefstabstatus;
{$endif GDB}
          constructor create;
          constructor ppuloaddef(ppufile:tcompilerppufile);
          procedure reset;
          function getcopy : tstoreddef;virtual;
          procedure ppuwritedef(ppufile:tcompilerppufile);
          procedure ppuwrite(ppufile:tcompilerppufile);virtual;abstract;
          procedure buildderef;override;
          procedure buildderefimpl;override;
          procedure deref;override;
          procedure derefimpl;override;
          function  size:aint;override;
          function  alignment:longint;override;
          function  is_publishable : boolean;override;
          function  needs_inittable : boolean;override;
          { debug }
{$ifdef GDB}
          function get_var_value(const s:string):string;
          function stabstr_evaluate(const s:string;const vars:array of string):Pchar;
          function  stabstring : pchar;virtual;
          procedure concatstabto(asmlist : taasmoutput);virtual;
          function  numberstring:string;virtual;
          procedure set_globalnb;virtual;
          function  allstabstring : pchar;virtual;
{$endif GDB}
          { rtti generation }
          procedure write_rtti_name;
          procedure write_rtti_data(rt:trttitype);virtual;
          procedure write_child_rtti_data(rt:trttitype);virtual;
          function  get_rtti_label(rt:trttitype):tasmsymbol;
          { regvars }
          function is_intregable : boolean;
          function is_fpuregable : boolean;
       private
          savesize  : aint;
       end;

       tfiletyp = (ft_text,ft_typed,ft_untyped);

       tfiledef = class(tstoreddef)
          filetyp : tfiletyp;
          typedfiletype : ttype;
          constructor createtext;
          constructor createuntyped;
          constructor createtyped(const tt : ttype);
          constructor ppuload(ppufile:tcompilerppufile);
          function getcopy : tstoreddef;override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          procedure buildderef;override;
          procedure deref;override;
          function  gettypename:string;override;
          function  getmangledparaname:string;override;
          procedure setsize;
          { debug }
{$ifdef GDB}
          function  stabstring : pchar;override;
          procedure concatstabto(asmlist : taasmoutput);override;
{$endif GDB}
       end;

       tvariantdef = class(tstoreddef)
          varianttype : tvarianttype;
          constructor create(v : tvarianttype);
          constructor ppuload(ppufile:tcompilerppufile);
          function getcopy : tstoreddef;override;
          function gettypename:string;override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          procedure setsize;
          function is_publishable : boolean;override;
          function needs_inittable : boolean;override;
          procedure write_rtti_data(rt:trttitype);override;
{$ifdef GDB}
          function  numberstring:string;override;
          function  stabstring : pchar;override;
          procedure concatstabto(asmlist : taasmoutput);override;
{$endif GDB}
       end;

       tformaldef = class(tstoreddef)
          constructor create;
          constructor ppuload(ppufile:tcompilerppufile);
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          function  gettypename:string;override;
{$ifdef GDB}
          function  numberstring:string;override;
          function  stabstring : pchar;override;
          procedure concatstabto(asmlist : taasmoutput);override;
{$endif GDB}
       end;

       tforwarddef = class(tstoreddef)
          tosymname : pstring;
          forwardpos : tfileposinfo;
          constructor create(const s:string;const pos : tfileposinfo);
          destructor destroy;override;
          function  gettypename:string;override;
       end;

       terrordef = class(tstoreddef)
          constructor create;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          function  gettypename:string;override;
          function  getmangledparaname : string;override;
          { debug }
{$ifdef GDB}
          function  stabstring : pchar;override;
          procedure concatstabto(asmlist : taasmoutput);override;
{$endif GDB}
       end;

       { tpointerdef and tclassrefdef should get a common
         base class, but I derived tclassrefdef from tpointerdef
         to avoid problems with bugs (FK)
       }

       tpointerdef = class(tstoreddef)
          pointertype : ttype;
          is_far : boolean;
          constructor create(const tt : ttype);
          constructor createfar(const tt : ttype);
          function getcopy : tstoreddef;override;
          constructor ppuload(ppufile:tcompilerppufile);
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          procedure buildderef;override;
          procedure deref;override;
          function  gettypename:string;override;
          { debug }
{$ifdef GDB}
          function  stabstring : pchar;override;
          procedure concatstabto(asmlist : taasmoutput);override;
{$endif GDB}
       end;

       Trecord_stabgen_state=record
          stabstring:Pchar;
          stabsize,staballoc,recoffset:integer;
       end;

       tabstractrecorddef= class(tstoreddef)
       private
          Count         : integer;
          FRTTIType     : trttitype;
{$ifdef GDB}
          procedure field_addname(p:Tnamedindexitem;arg:pointer);
          procedure field_concatstabto(p:Tnamedindexitem;arg:pointer);
{$endif}
          procedure count_field_rtti(sym : tnamedindexitem;arg:pointer);
          procedure write_field_rtti(sym : tnamedindexitem;arg:pointer);
          procedure generate_field_rtti(sym : tnamedindexitem;arg:pointer);
       public
          symtable : tsymtable;
          function  getsymtable(t:tgetsymtable):tsymtable;override;
       end;

       trecorddef = class(tabstractrecorddef)
       public
          isunion       : boolean;
          constructor create(p : tsymtable);
          constructor ppuload(ppufile:tcompilerppufile);
          destructor destroy;override;
          function getcopy : tstoreddef;override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          procedure buildderef;override;
          procedure deref;override;
          function  size:aint;override;
          function  alignment : longint;override;
          function  padalignment: longint;
          function  gettypename:string;override;
          { debug }
{$ifdef GDB}
          function  stabstring : pchar;override;
          procedure concatstabto(asmlist:taasmoutput);override;
{$endif GDB}
          function  needs_inittable : boolean;override;
          { rtti }
          procedure write_child_rtti_data(rt:trttitype);override;
          procedure write_rtti_data(rt:trttitype);override;
       end;

       tprocdef = class;
       tobjectdef = class;
       timplementedinterfaces = class;

       timplintfentry = class(TNamedIndexItem)
         intf         : tobjectdef;
         intfderef    : tderef;
         ioffset      : longint;
         implindex    : longint;
         namemappings : tdictionary;
         procdefs     : TIndexArray;
         constructor create(aintf: tobjectdef);
         constructor create_deref(const d:tderef);
         destructor  destroy; override;
       end;

       tobjectdef = class(tabstractrecorddef)
       private
{$ifdef GDB}
          procedure proc_addname(p :tnamedindexitem;arg:pointer);
          procedure proc_concatstabto(p :tnamedindexitem;arg:pointer);
{$endif GDB}
          procedure count_published_properties(sym:tnamedindexitem;arg:pointer);
          procedure write_property_info(sym : tnamedindexitem;arg:pointer);
          procedure generate_published_child_rtti(sym : tnamedindexitem;arg:pointer);
          procedure count_published_fields(sym:tnamedindexitem;arg:pointer);
          procedure writefields(sym:tnamedindexitem;arg:pointer);
       public
          childof  : tobjectdef;
          childofderef  : tderef;
          objname,
          objrealname   : pstring;
          objectoptions : tobjectoptions;
          { to be able to have a variable vmt position }
          { and no vmt field for objects without virtuals }
          vmt_offset : longint;
{$ifdef GDB}
          writing_class_record_stab : boolean;
{$endif GDB}
          objecttype : tobjectdeftype;
          iidguid: pguid;
          iidstr: pstring;
          lastvtableindex: longint;
          { store implemented interfaces defs and name mappings }
          implementedinterfaces: timplementedinterfaces;
          constructor create(ot : tobjectdeftype;const n : string;c : tobjectdef);
          constructor ppuload(ppufile:tcompilerppufile);
          destructor  destroy;override;
          function getcopy : tstoreddef;override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          function gettypename:string;override;
          procedure buildderef;override;
          procedure deref;override;
          function  getparentdef:tdef;override;
          function  size : aint;override;
          function  alignment:longint;override;
          function  vmtmethodoffset(index:longint):longint;
          function  members_need_inittable : boolean;
          { this should be called when this class implements an interface }
          procedure prepareguid;
          function  is_publishable : boolean;override;
          function  needs_inittable : boolean;override;
          function  vmt_mangledname : string;
          function  rtti_name : string;
          procedure check_forwards;
          function  is_related(d : tdef) : boolean;override;
          function  next_free_name_index : longint;
          procedure insertvmt;
          procedure set_parent(c : tobjectdef);
          function searchdestructor : tprocdef;
          { debug }
{$ifdef GDB}
          function  stabstring : pchar;override;
          procedure set_globalnb;override;
          function  classnumberstring : string;
          procedure concatstabto(asmlist : taasmoutput);override;
          function  allstabstring : pchar;override;
{$endif GDB}
          { rtti }
          procedure write_child_rtti_data(rt:trttitype);override;
          procedure write_rtti_data(rt:trttitype);override;
          function generate_field_table : tasmlabel;
       end;

       timplementedinterfaces = class
          constructor create;
          destructor  destroy; override;

          function  count: longint;
          function  interfaces(intfindex: longint): tobjectdef;
          function  interfacesderef(intfindex: longint): tderef;
          function  ioffsets(intfindex: longint): longint;
          procedure setioffsets(intfindex,iofs:longint);
          function  implindex(intfindex:longint):longint;
          procedure setimplindex(intfindex,implidx:longint);
          function  searchintf(def: tdef): longint;
          procedure addintf(def: tdef);

          procedure buildderef;
          procedure deref;
          { add interface reference loaded from ppu }
          procedure addintf_deref(const d:tderef;iofs:longint);

          procedure clearmappings;
          procedure addmappings(intfindex: longint; const name, newname: string);
          function  getmappings(intfindex: longint; const name: string; var nextexist: pointer): string;

          procedure addimplproc(intfindex: longint; procdef: tprocdef);
          function  implproccount(intfindex: longint): longint;
          function  implprocs(intfindex: longint; procindex: longint): tprocdef;
          function  isimplmergepossible(intfindex, remainindex: longint; var weight: longint): boolean;

       private
          finterfaces: tindexarray;
          procedure checkindex(intfindex: longint);
       end;


       tclassrefdef = class(tpointerdef)
          constructor create(const t:ttype);
          constructor ppuload(ppufile:tcompilerppufile);
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          function gettypename:string;override;
          function  is_publishable : boolean;override;
          { debug }
{$ifdef GDB}
          function stabstring : pchar;override;
{$endif GDB}
       end;

       tarraydef = class(tstoreddef)
          lowrange,
          highrange  : aint;
          rangetype  : ttype;
          IsConvertedPointer,
          IsDynamicArray,
          IsVariant,
          IsConstructor,
          IsArrayOfConst : boolean;
       protected
          _elementtype : ttype;
       public
          function elesize : aint;
          function elecount : aint;
          constructor create_from_pointer(const elemt : ttype);
          constructor create(l,h : aint;const t : ttype);
          constructor ppuload(ppufile:tcompilerppufile);
          function getcopy : tstoreddef;override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          function  gettypename:string;override;
          function  getmangledparaname : string;override;
          procedure setelementtype(t: ttype);
{$ifdef GDB}
          function  stabstring : pchar;override;
          procedure concatstabto(asmlist : taasmoutput);override;
{$endif GDB}
          procedure buildderef;override;
          procedure deref;override;
          function size : aint;override;
          function alignment : longint;override;
          { returns the label of the range check string }
          function needs_inittable : boolean;override;
          procedure write_child_rtti_data(rt:trttitype);override;
          procedure write_rtti_data(rt:trttitype);override;
          property elementtype : ttype Read _ElementType;
       end;

       torddef = class(tstoreddef)
          low,high : TConstExprInt;
          typ      : tbasetype;
          constructor create(t : tbasetype;v,b : TConstExprInt);
          constructor ppuload(ppufile:tcompilerppufile);
          function getcopy : tstoreddef;override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          function  is_publishable : boolean;override;
          function  gettypename:string;override;
          procedure setsize;
          { debug }
{$ifdef GDB}
          function  stabstring : pchar;override;
{$endif GDB}
          { rtti }
          procedure write_rtti_data(rt:trttitype);override;
       end;

       tfloatdef = class(tstoreddef)
          typ : tfloattype;
          constructor create(t : tfloattype);
          constructor ppuload(ppufile:tcompilerppufile);
          function getcopy : tstoreddef;override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          function  gettypename:string;override;
          function  is_publishable : boolean;override;
          procedure setsize;
          { debug }
{$ifdef GDB}
          function stabstring : pchar;override;
          procedure concatstabto(asmlist:taasmoutput);override;
{$endif GDB}
          { rtti }
          procedure write_rtti_data(rt:trttitype);override;
       end;

       tabstractprocdef = class(tstoreddef)
          { saves a definition to the return type }
          rettype         : ttype;
          parast          : tsymtable;
          paras           : tparalist;
          proctypeoption  : tproctypeoption;
          proccalloption  : tproccalloption;
          procoptions     : tprocoptions;
          requiredargarea : aint;
          { number of user visibile parameters }
          maxparacount,
          minparacount    : byte;
{$ifdef i386}
          fpu_used        : longint;    { how many stack fpu must be empty }
{$endif i386}
          funcretloc : array[tcallercallee] of TLocation;
          has_paraloc_info : boolean; { paraloc info is available }
          constructor create(level:byte);
          constructor ppuload(ppufile:tcompilerppufile);
          destructor destroy;override;
          procedure  ppuwrite(ppufile:tcompilerppufile);override;
          procedure buildderef;override;
          procedure deref;override;
          procedure releasemem;
          procedure calcparas;
          function  typename_paras(showhidden:boolean): string;
          procedure test_if_fpu_result;
          function  is_methodpointer:boolean;virtual;
          function  is_addressonly:boolean;virtual;
          { debug }
{$ifdef GDB}
          function  stabstring : pchar;override;
{$endif GDB}
       private
          procedure count_para(p:tnamedindexitem;arg:pointer);
          procedure insert_para(p:tnamedindexitem;arg:pointer);
       end;

       tprocvardef = class(tabstractprocdef)
          constructor create(level:byte);
          constructor ppuload(ppufile:tcompilerppufile);
          function getcopy : tstoreddef;override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          procedure buildderef;override;
          procedure deref;override;
          function  getsymtable(t:tgetsymtable):tsymtable;override;
          function  size : aint;override;
          function  gettypename:string;override;
          function  is_publishable : boolean;override;
          function  is_methodpointer:boolean;override;
          function  is_addressonly:boolean;override;
          function  getmangledparaname:string;override;
          { debug }
{$ifdef GDB}
          function stabstring : pchar;override;
          procedure concatstabto(asmlist:taasmoutput);override;
{$endif GDB}
          { rtti }
          procedure write_rtti_data(rt:trttitype);override;
       end;

       tmessageinf = record
         case integer of
           0 : (str : pchar);
           1 : (i : longint);
       end;

       tinlininginfo = record
          { node tree }
          code  : tnode;
          flags : tprocinfoflags;
       end;
       pinlininginfo = ^tinlininginfo;


{$ifdef oldregvars}
       { register variables }
       pregvarinfo = ^tregvarinfo;
       tregvarinfo = record
          regvars : array[1..maxvarregs] of tsym;
          regvars_para : array[1..maxvarregs] of boolean;
          regvars_refs : array[1..maxvarregs] of longint;

          fpuregvars : array[1..maxfpuvarregs] of tsym;
          fpuregvars_para : array[1..maxfpuvarregs] of boolean;
          fpuregvars_refs : array[1..maxfpuvarregs] of longint;
       end;
{$endif oldregvars}

       tprocdef = class(tabstractprocdef)
       private
          _mangledname : pstring;
{$ifdef GDB}
          isstabwritten : boolean;
{$endif GDB}
       public
          extnumber      : word;
          messageinf : tmessageinf;
{$ifndef EXTDEBUG}
          { where is this function defined and what were the symbol
            flags, needed here because there
            is only one symbol for all overloaded functions
            EXTDEBUG has fileinfo in tdef (PFV) }
          fileinfo : tfileposinfo;
{$endif}
          symoptions : tsymoptions;
          { symbol owning this definition }
          procsym : tsym;
          procsymderef : tderef;
          { alias names }
          aliasnames : tstringlist;
          { symtables }
          localst : tsymtable;
          funcretsym : tsym;
          funcretsymderef : tderef;
          { browser info }
          lastref,
          defref,
          lastwritten : tref;
          refcount : longint;
          _class : tobjectdef;
          _classderef : tderef;
{$ifdef powerpc}
          { library symbol for AmigaOS/MorphOS }
          libsym : tsym;
          libsymderef : tderef;
{$endif powerpc}
          { name of the result variable to insert in the localsymtable }
          resultname : stringid;
          { true, if the procedure is only declared
            (forward procedure) }
          forwarddef,
          { true if the procedure is declared in the interface }
          interfacedef : boolean;
          { true if the procedure has a forward declaration }
          hasforward : boolean;
          { import info }
          import_dll,
          import_name : pstring;
          import_nr   : word;
          { info for inlining the subroutine, if this pointer is nil,
            the procedure can't be inlined }
          inlininginfo : pinlininginfo;
{$ifdef oldregvars}
          regvarinfo: pregvarinfo;
{$endif oldregvars}
          constructor create(level:byte);
          constructor ppuload(ppufile:tcompilerppufile);
          destructor  destroy;override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          procedure buildderef;override;
          procedure buildderefimpl;override;
          procedure deref;override;
          procedure derefimpl;override;
          function  getsymtable(t:tgetsymtable):tsymtable;override;
          function gettypename : string;override;
          function  mangledname : string;
          procedure setmangledname(const s : string);
          procedure load_references(ppufile:tcompilerppufile;locals:boolean);
          function  write_references(ppufile:tcompilerppufile;locals:boolean):boolean;
          { inserts the local symbol table, if this is not
            no local symbol table is built. Should be called only
            when we are sure that a local symbol table will be required.
          }
          procedure insert_localst;
          function  fullprocname(showhidden:boolean):string;
          function  cplusplusmangledname : string;
          function  is_methodpointer:boolean;override;
          function  is_addressonly:boolean;override;
          function  is_visible_for_object(currobjdef:tobjectdef):boolean;
          { debug }
{$ifdef GDB}
          function  numberstring:string;override;
          function  stabstring : pchar;override;
          procedure concatstabto(asmlist : taasmoutput);override;
{$endif GDB}
       end;

       { single linked list of overloaded procs }
       pprocdeflist = ^tprocdeflist;
       tprocdeflist = record
         def  : tprocdef;
         defderef : tderef;
         own  : boolean;
         next : pprocdeflist;
       end;

       tstringdef = class(tstoreddef)
          string_typ : tstringtype;
          len        : aint;
          constructor createshort(l : byte);
          constructor loadshort(ppufile:tcompilerppufile);
          constructor createlong(l : aint);
          constructor loadlong(ppufile:tcompilerppufile);
       {$ifdef ansistring_bits}
          constructor createansi(l:aint;bits:Tstringbits);
          constructor loadansi(ppufile:tcompilerppufile;bits:Tstringbits);
       {$else}
          constructor createansi(l : aint);
          constructor loadansi(ppufile:tcompilerppufile);
       {$endif}
          constructor createwide(l : aint);
          constructor loadwide(ppufile:tcompilerppufile);
          function getcopy : tstoreddef;override;
          function  stringtypname:string;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          function  gettypename:string;override;
          function  getmangledparaname:string;override;
          function  is_publishable : boolean;override;
          { debug }
{$ifdef GDB}
          function  stabstring : pchar;override;
          procedure concatstabto(asmlist : taasmoutput);override;
{$endif GDB}
          function alignment : longint;override;
          { init/final }
          function  needs_inittable : boolean;override;
          { rtti }
          procedure write_rtti_data(rt:trttitype);override;
       end;

       tenumdef = class(tstoreddef)
          minval,
          maxval    : aint;
          has_jumps : boolean;
          firstenum : tsym;  {tenumsym}
          basedef   : tenumdef;
          basedefderef : tderef;
          constructor create;
          constructor create_subrange(_basedef:tenumdef;_min,_max:aint);
          constructor ppuload(ppufile:tcompilerppufile);
          destructor destroy;override;
          function getcopy : tstoreddef;override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          procedure buildderef;override;
          procedure deref;override;
          function  gettypename:string;override;
          function  is_publishable : boolean;override;
          procedure calcsavesize;
          procedure setmax(_max:aint);
          procedure setmin(_min:aint);
          function  min:aint;
          function  max:aint;
          { debug }
{$ifdef GDB}
          function stabstring : pchar;override;
{$endif GDB}
          { rtti }
          procedure write_rtti_data(rt:trttitype);override;
          procedure write_child_rtti_data(rt:trttitype);override;
       private
          procedure correct_owner_symtable;
       end;

       tsetdef = class(tstoreddef)
          elementtype : ttype;
          settype : tsettype;
          setbase,
          setmax : aint;
          constructor create(const t:ttype;low,high : aint);
          constructor ppuload(ppufile:tcompilerppufile);
          destructor  destroy;override;
          function getcopy : tstoreddef;override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          procedure buildderef;override;
          procedure deref;override;
          function  gettypename:string;override;
          function  is_publishable : boolean;override;
          { debug }
{$ifdef GDB}
          function  stabstring : pchar;override;
          procedure concatstabto(asmlist : taasmoutput);override;
{$endif GDB}
          { rtti }
          procedure write_rtti_data(rt:trttitype);override;
          procedure write_child_rtti_data(rt:trttitype);override;
       end;

       Tdefmatch=(dm_exact,dm_equal,dm_convertl1);

    var
       aktobjectdef : tobjectdef;  { used for private functions check !! }
{$ifdef GDB}
       writing_def_stabs : boolean;
       { for STAB debugging }
       globaltypecount  : word;
       pglobaltypecount : pword;
{$endif GDB}

    { default types }
       generrortype,              { error in definition }
       voidpointertype,           { pointer for Void-Pointerdef }
       charpointertype,           { pointer for Char-Pointerdef }
       widecharpointertype,       { pointer for WideChar-Pointerdef }
       voidfarpointertype,
       cformaltype,               { unique formal definition }
       voidtype,                  { Void (procedure) }
       cchartype,                 { Char }
       cwidechartype,             { WideChar }
       booltype,                  { boolean type }
       u8inttype,                 { 8-Bit unsigned integer }
       s8inttype,                 { 8-Bit signed integer }
       u16inttype,                { 16-Bit unsigned integer }
       s16inttype,                { 16-Bit signed integer }
       u32inttype,                { 32-Bit unsigned integer }
       s32inttype,                { 32-Bit signed integer }
       u64inttype,                { 64-bit unsigned integer }
       s64inttype,                { 64-bit signed integer }
       s32floattype,              { pointer for realconstn }
       s64floattype,              { pointer for realconstn }
       s80floattype,              { pointer to type of temp. floats }
       s64currencytype,           { pointer to a currency type }
       cshortstringtype,          { pointer to type of short string const   }
       clongstringtype,           { pointer to type of long string const   }
{$ifdef ansistring_bits}
       cansistringtype16,         { pointer to type of ansi string const  }
       cansistringtype32,         { pointer to type of ansi string const  }
       cansistringtype64,         { pointer to type of ansi string const  }
{$else}
       cansistringtype,           { pointer to type of ansi string const  }
{$endif}
       cwidestringtype,           { pointer to type of wide string const  }
       openshortstringtype,       { pointer to type of an open shortstring,
                                    needed for readln() }
       openchararraytype,         { pointer to type of an open array of char,
                                    needed for readln() }
       cfiletype,                 { get the same definition for all file }
                                  { used for stabs }
       methodpointertype,         { typecasting of methodpointers to extract self }
       { we use only one variant def for every variant class }
       cvarianttype,
       colevarianttype,
       { default integer type s32inttype on 32 bit systems, s64bittype on 64 bit systems }
       sinttype,
       uinttype,
       { unsigned ord type with the same size as a pointer }
       ptrinttype,
       { several types to simulate more or less C++ objects for GDB }
       vmttype,
       vmtarraytype,
       pvmttype      : ttype;     { type of classrefs, used for stabs }

       { pointer to the anchestor of all classes }
       class_tobject : tobjectdef;
       { pointer to the ancestor of all COM interfaces }
       interface_iunknown : tobjectdef;
       { pointer to the TGUID type
         of all interfaces         }
       rec_tguid : trecorddef;

    const
{$ifdef i386}
       pbestrealtype : ^ttype = @s80floattype;
{$endif}
{$ifdef x86_64}
       pbestrealtype : ^ttype = @s80floattype;
{$endif}
{$ifdef m68k}
       pbestrealtype : ^ttype = @s64floattype;
{$endif}
{$ifdef alpha}
       pbestrealtype : ^ttype = @s64floattype;
{$endif}
{$ifdef powerpc}
       pbestrealtype : ^ttype = @s64floattype;
{$endif}
{$ifdef ia64}
       pbestrealtype : ^ttype = @s64floattype;
{$endif}
{$ifdef SPARC}
       pbestrealtype : ^ttype = @s64floattype;
{$endif SPARC}
{$ifdef vis}
       pbestrealtype : ^ttype = @s64floattype;
{$endif vis}
{$ifdef ARM}
       pbestrealtype : ^ttype = @s64floattype;
{$endif ARM}
{$ifdef MIPS}
       pbestrealtype : ^ttype = @s64floattype;
{$endif MIPS}

    function make_mangledname(const typeprefix:string;st:tsymtable;const suffix:string):string;

    { should be in the types unit, but the types unit uses the node stuff :( }
    function is_interfacecom(def: tdef): boolean;
    function is_interfacecorba(def: tdef): boolean;
    function is_interface(def: tdef): boolean;
    function is_object(def: tdef): boolean;
    function is_class(def: tdef): boolean;
    function is_cppclass(def: tdef): boolean;
    function is_class_or_interface(def: tdef): boolean;


{$ifdef x86}
    function use_sse(def : tdef) : boolean;
{$endif x86}

implementation

    uses
       strings,
       { global }
       verbose,
       { target }
       systems,aasmcpu,paramgr,
       { symtable }
       symsym,symtable,symutil,defutil,
       { module }
{$ifdef GDB}
       gdb,
{$endif GDB}
       fmodule,
       { other }
       gendef,
       crc
       ;


{****************************************************************************
                                  Helpers
****************************************************************************}

    function make_mangledname(const typeprefix:string;st:tsymtable;const suffix:string):string;
      var
        s,hs,
        prefix : string;
        oldlen,
        newlen,
        i   : longint;
        crc : dword;
        hp  : tparavarsym;
      begin
        prefix:='';
        if not assigned(st) then
         internalerror(200204212);
        { sub procedures }
        while (st.symtabletype=localsymtable) do
         begin
           if st.defowner.deftype<>procdef then
            internalerror(200204173);
           { Add the full mangledname of procedure to prevent
             conflicts with 2 overloads having both a nested procedure
             with the same name, see tb0314 (PFV) }
           s:=tprocdef(st.defowner).procsym.name;
           oldlen:=length(s);
           for i:=0 to tprocdef(st.defowner).paras.count-1 do
            begin
              hp:=tparavarsym(tprocdef(st.defowner).paras[i]);
              if not(vo_is_hidden_para in hp.varoptions) then
                s:=s+'$'+hp.vartype.def.mangledparaname;
            end;
           if not is_void(tprocdef(st.defowner).rettype.def) then
             s:=s+'$$'+tprocdef(st.defowner).rettype.def.mangledparaname;
           newlen:=length(s);
           { Replace with CRC if the parameter line is very long }
           if (newlen-oldlen>12) and
              ((newlen>128) or (newlen-oldlen>64)) then
             begin
               crc:=$ffffffff;
               for i:=0 to tprocdef(st.defowner).paras.count-1 do
                 begin
                   hp:=tparavarsym(tprocdef(st.defowner).paras[i]);
                   if not(vo_is_hidden_para in hp.varoptions) then
                     begin
                       hs:=hp.vartype.def.mangledparaname;
                       crc:=UpdateCrc32(crc,hs[1],length(hs));
                     end;
                 end;
               hs:=hp.vartype.def.mangledparaname;
               crc:=UpdateCrc32(crc,hs[1],length(hs));
               s:=Copy(s,1,oldlen)+'$crc'+hexstr(crc,8);
             end;
           if prefix<>'' then
             prefix:=s+'_'+prefix
           else
             prefix:=s;
           st:=st.defowner.owner;
         end;
        { object/classes symtable }
        if (st.symtabletype=objectsymtable) then
         begin
           if st.defowner.deftype<>objectdef then
            internalerror(200204174);
           prefix:=tobjectdef(st.defowner).objname^+'_$_'+prefix;
           st:=st.defowner.owner;
         end;
        { symtable must now be static or global }
        if not(st.symtabletype in [staticsymtable,globalsymtable]) then
         internalerror(200204175);
        result:='';
        if typeprefix<>'' then
          result:=result+typeprefix+'_';
        { Add P$ for program, which can have the same name as
          a unit }
        if (tsymtable(main_module.localsymtable)=st) and
           (not main_module.is_unit) then
          result:=result+'P$'+st.name^
        else
          result:=result+st.name^;
        if prefix<>'' then
          result:=result+'_'+prefix;
        if suffix<>'' then
          result:=result+'_'+suffix;
        { the Darwin assembler assumes that all symbols starting with 'L' are local }
        if (target_info.system = system_powerpc_darwin) and
           (result[1] = 'L') then
          result := '_' + result;
      end;


{****************************************************************************
                     TDEF (base class for definitions)
****************************************************************************}

    constructor tstoreddef.create;
      begin
         inherited create;
         savesize := 0;
{$ifdef EXTDEBUG}
         fileinfo := aktfilepos;
{$endif}
         if registerdef then
           symtablestack.registerdef(self);
{$ifdef GDB}
         stab_state:=stab_state_unused;
         globalnb := 0;
{$endif GDB}
         fillchar(localrttilab,sizeof(localrttilab),0);
      end;


    constructor tstoreddef.ppuloaddef(ppufile:tcompilerppufile);
      begin
         inherited create;
{$ifdef EXTDEBUG}
         fillchar(fileinfo,sizeof(fileinfo),0);
{$endif}
{$ifdef GDB}
         stab_state:=stab_state_unused;
         globalnb := 0;
{$endif GDB}
         fillchar(localrttilab,sizeof(localrttilab),0);
      { load }
         indexnr:=ppufile.getword;
         ppufile.getderef(typesymderef);
         ppufile.getsmallset(defoptions);
         if df_has_rttitable in defoptions then
          ppufile.getderef(rttitablesymderef);
         if df_has_inittable in defoptions then
          ppufile.getderef(inittablesymderef);
      end;


    procedure Tstoreddef.reset;
      begin
{$ifdef GDB}
        stab_state:=stab_state_unused;
{$endif GDB}
        if assigned(rttitablesym) then
          trttisym(rttitablesym).lab := nil;
        if assigned(inittablesym) then
          trttisym(inittablesym).lab := nil;
        localrttilab[initrtti]:=nil;
        localrttilab[fullrtti]:=nil;
      end;


    function tstoreddef.getcopy : tstoreddef;
      begin
        Message(sym_e_cant_create_unique_type);
        getcopy:=terrordef.create;
      end;


    procedure tstoreddef.ppuwritedef(ppufile:tcompilerppufile);
      begin
        ppufile.putword(indexnr);
        ppufile.putderef(typesymderef);
        ppufile.putsmallset(defoptions);
        if df_has_rttitable in defoptions then
         ppufile.putderef(rttitablesymderef);
        if df_has_inittable in defoptions then
         ppufile.putderef(inittablesymderef);
{$ifdef GDB}
        if globalnb=0 then
          begin
            if (cs_gdb_dbx in aktglobalswitches) and
               assigned(owner) then
              globalnb := owner.getnewtypecount
            else
              set_globalnb;
          end;
{$endif GDB}
      end;


    procedure tstoreddef.buildderef;
      begin
        typesymderef.build(typesym);
        rttitablesymderef.build(rttitablesym);
        inittablesymderef.build(inittablesym);
      end;


    procedure tstoreddef.buildderefimpl;
      begin
      end;


    procedure tstoreddef.deref;
      begin
        typesym:=ttypesym(typesymderef.resolve);
        if df_has_rttitable in defoptions then
          rttitablesym:=trttisym(rttitablesymderef.resolve);
        if df_has_inittable in defoptions then
          inittablesym:=trttisym(inittablesymderef.resolve);
      end;


    procedure tstoreddef.derefimpl;
      begin
      end;


    function tstoreddef.size : aint;
      begin
         size:=savesize;
      end;


    function tstoreddef.alignment : longint;
      begin
         { natural alignment by default }
         alignment:=size_2_align(savesize);
      end;


{$ifdef GDB}
    procedure tstoreddef.set_globalnb;
      begin
        globalnb:=PGlobalTypeCount^;
        inc(PglobalTypeCount^);
      end;


    function Tstoreddef.get_var_value(const s:string):string;
      begin
        if s='numberstring' then
          get_var_value:=numberstring
        else if s='sym_name' then
          if assigned(typesym) then
             get_var_value:=Ttypesym(typesym).name
          else
             get_var_value:=' '
        else if s='N_LSYM' then
          get_var_value:=tostr(N_LSYM)
        else if s='savesize' then
          get_var_value:=tostr(savesize);
      end;


    function Tstoreddef.stabstr_evaluate(const s:string;const vars:array of string):Pchar;
      begin
        stabstr_evaluate:=string_evaluate(s,@get_var_value,vars);
      end;


    function tstoreddef.stabstring : pchar;
      begin
        stabstring:=stabstr_evaluate('t${numberstring};',[]);
      end;


    function tstoreddef.numberstring : string;
      begin
        { Stab must already be written, or we must be busy writing it }
        if writing_def_stabs and
           not(stab_state in [stab_state_writing,stab_state_written]) then
          internalerror(200403091);
        { Keep track of used stabs, this info is only usefull for stabs
          referenced by the symbols. Definitions will always include all
          required stabs }
        if stab_state=stab_state_unused then
          stab_state:=stab_state_used;
        { Need a new number? }
        if globalnb=0 then
          begin
            if (cs_gdb_dbx in aktglobalswitches) and
               assigned(owner) then
              globalnb := owner.getnewtypecount
            else
              set_globalnb;
          end;
        if (cs_gdb_dbx in aktglobalswitches) and
           assigned(typesym) and
           (ttypesym(typesym).owner.symtabletype in [staticsymtable,globalsymtable]) and
           (ttypesym(typesym).owner.iscurrentunit) then
          result:='('+tostr(tabstractunitsymtable(ttypesym(typesym).owner).moduleid)+','+tostr(tstoreddef(ttypesym(typesym).restype.def).globalnb)+')'
        else
          result:=tostr(globalnb);
      end;


    function tstoreddef.allstabstring : pchar;
      var
        stabchar : string[2];
        ss,st,su : pchar;
      begin
        ss := stabstring;
        stabchar := 't';
        if deftype in tagtypes then
          stabchar := 'Tt';
        { Here we maybe generate a type, so we have to use numberstring }
        st:=stabstr_evaluate('"${sym_name}:$1$2=',[stabchar,numberstring]);
        reallocmem(st,strlen(ss)+512);
        { line info is set to 0 for all defs, because the def can be in an other
          unit and then the linenumber is invalid in the current sourcefile }
        su:=stabstr_evaluate('",${N_LSYM},0,0,0',[]);
        strcopy(strecopy(strend(st),ss),su);
        reallocmem(st,strlen(st)+1);
        allstabstring:=st;
        strdispose(ss);
        strdispose(su);
      end;


    procedure tstoreddef.concatstabto(asmlist : taasmoutput);
      var
        stab_str : pchar;
      begin
        if (stab_state in [stab_state_writing,stab_state_written]) then
          exit;
        If cs_gdb_dbx in aktglobalswitches then
          begin
            { otherwise you get two of each def }
            If assigned(typesym) then
              begin
                if (ttypesym(typesym).owner = nil) or
                   ((ttypesym(typesym).owner.symtabletype = globalsymtable) and
                    tglobalsymtable(ttypesym(typesym).owner).dbx_count_ok)  then
                  begin
                    {with DBX we get the definition from the other objects }
                    stab_state := stab_state_written;
                    exit;
                  end;
              end;
          end;
        { to avoid infinite loops }
        stab_state := stab_state_writing;
        stab_str := allstabstring;
        asmList.concat(Tai_stabs.Create(stab_str));
        stab_state := stab_state_written;
      end;
{$endif GDB}


    procedure tstoreddef.write_rtti_name;
      var
         str : string;
      begin
         { name }
         if assigned(typesym) then
           begin
              str:=ttypesym(typesym).realname;
              rttiList.concat(Tai_string.Create(chr(length(str))+str));
           end
         else
           rttiList.concat(Tai_string.Create(#0))
      end;


    procedure tstoreddef.write_rtti_data(rt:trttitype);
      begin
        rttilist.concat(tai_const.create_8bit(tkUnknown));
        write_rtti_name;
      end;


    procedure tstoreddef.write_child_rtti_data(rt:trttitype);
      begin
      end;


    function tstoreddef.get_rtti_label(rt:trttitype) : tasmsymbol;
      begin
         { try to reuse persistent rtti data }
         if (rt=fullrtti) and (df_has_rttitable in defoptions) then
          get_rtti_label:=trttisym(rttitablesym).get_label
         else
          if (rt=initrtti) and (df_has_inittable in defoptions) then
           get_rtti_label:=trttisym(inittablesym).get_label
         else
          begin
            if not assigned(localrttilab[rt]) then
             begin
               objectlibrary.getdatalabel(localrttilab[rt]);
               write_child_rtti_data(rt);
               maybe_new_object_file(rttiList);
               new_section(rttiList,sec_rodata,localrttilab[rt].name,const_align(sizeof(aint)));
               rttiList.concat(Tai_symbol.Create_global(localrttilab[rt],0));
               write_rtti_data(rt);
               rttiList.concat(Tai_symbol_end.Create(localrttilab[rt]));
             end;
            get_rtti_label:=localrttilab[rt];
          end;
      end;


    { returns true, if the definition can be published }
    function tstoreddef.is_publishable : boolean;
      begin
         is_publishable:=false;
      end;


    { needs an init table }
    function tstoreddef.needs_inittable : boolean;
      begin
         needs_inittable:=false;
      end;


   function tstoreddef.is_intregable : boolean;
     begin
        is_intregable:=false;
        case deftype of
          orddef,
          pointerdef,
          enumdef:
            is_intregable:=true;
          procvardef :
            is_intregable:=not(po_methodpointer in tprocvardef(self).procoptions);
          objectdef:
            is_intregable:=is_class(self) or is_interface(self);
          setdef:
            is_intregable:=(tsetdef(self).settype=smallset);
        end;
     end;


   function tstoreddef.is_fpuregable : boolean;
     begin
{$ifdef x86}
       result:=use_sse(self);
{$else x86}
       result:=(deftype=floatdef);
{$endif x86}
     end;



{****************************************************************************
                               Tstringdef
****************************************************************************}

    constructor tstringdef.createshort(l : byte);
      begin
         inherited create;
         string_typ:=st_shortstring;
         deftype:=stringdef;
         len:=l;
         savesize:=len+1;
      end;


    constructor tstringdef.loadshort(ppufile:tcompilerppufile);
      begin
         inherited ppuloaddef(ppufile);
         string_typ:=st_shortstring;
         deftype:=stringdef;
         len:=ppufile.getbyte;
         savesize:=len+1;
      end;


    constructor tstringdef.createlong(l : aint);
      begin
         inherited create;
         string_typ:=st_longstring;
         deftype:=stringdef;
         len:=l;
         savesize:=sizeof(aint);
      end;


    constructor tstringdef.loadlong(ppufile:tcompilerppufile);
      begin
         inherited ppuloaddef(ppufile);
         deftype:=stringdef;
         string_typ:=st_longstring;
         len:=ppufile.getaint;
         savesize:=sizeof(aint);
      end;

{$ifdef ansistring_bits}
    constructor tstringdef.createansi(l:aint;bits:Tstringbits);
      begin
         inherited create;
         case bits of
           sb_16:
             string_typ:=st_ansistring16;
           sb_32:
             string_typ:=st_ansistring32;
           sb_64:
             string_typ:=st_ansistring64;
         end;
         deftype:=stringdef;
         len:=l;
         savesize:=POINTER_SIZE;
      end;


    constructor tstringdef.loadansi(ppufile:tcompilerppufile;bits:Tstringbits);
      begin
         inherited ppuloaddef(ppufile);
         deftype:=stringdef;
         case bits of
           sb_16:
             string_typ:=st_ansistring16;
           sb_32:
             string_typ:=st_ansistring32;
           sb_64:
             string_typ:=st_ansistring64;
         end;
         len:=ppufile.getaint;
         savesize:=POINTER_SIZE;
      end;
{$else}
    constructor tstringdef.createansi(l:aint);
      begin
         inherited create;
         string_typ:=st_ansistring;
         deftype:=stringdef;
         len:=l;
         savesize:=sizeof(aint);
      end;


    constructor tstringdef.loadansi(ppufile:tcompilerppufile);

      begin
         inherited ppuloaddef(ppufile);
         deftype:=stringdef;
         string_typ:=st_ansistring;
         len:=ppufile.getaint;
         savesize:=sizeof(aint);
      end;
{$endif}

    constructor tstringdef.createwide(l : aint);
      begin
         inherited create;
         string_typ:=st_widestring;
         deftype:=stringdef;
         len:=l;
         savesize:=sizeof(aint);
      end;


    constructor tstringdef.loadwide(ppufile:tcompilerppufile);
      begin
         inherited ppuloaddef(ppufile);
         deftype:=stringdef;
         string_typ:=st_widestring;
         len:=ppufile.getaint;
         savesize:=sizeof(aint);
      end;


    function tstringdef.getcopy : tstoreddef;
      begin
        result:=tstringdef.create;
        result.deftype:=stringdef;
        tstringdef(result).string_typ:=string_typ;
        tstringdef(result).len:=len;
        tstringdef(result).savesize:=savesize;
      end;


    function tstringdef.stringtypname:string;
{$ifdef ansistring_bits}
      const
        typname:array[tstringtype] of string[9]=('',
          'shortstr','longstr','ansistr16','ansistr32','ansistr64','widestr'
        );
{$else}
      const
        typname:array[tstringtype] of string[8]=('',
          'shortstr','longstr','ansistr','widestr'
        );
{$endif}
      begin
        stringtypname:=typname[string_typ];
      end;


    procedure tstringdef.ppuwrite(ppufile:tcompilerppufile);
      begin
         inherited ppuwritedef(ppufile);
         if string_typ=st_shortstring then
           begin
{$ifdef extdebug}
            if len > 255 then internalerror(12122002);
{$endif}
            ppufile.putbyte(byte(len))
           end
         else
           ppufile.putaint(len);
         case string_typ of
            st_shortstring : ppufile.writeentry(ibshortstringdef);
            st_longstring : ppufile.writeentry(iblongstringdef);
         {$ifdef ansistring_bits}
            st_ansistring16 : ppufile.writeentry(ibansistring16def);
            st_ansistring32 : ppufile.writeentry(ibansistring32def);
            st_ansistring64 : ppufile.writeentry(ibansistring64def);
         {$else}
            st_ansistring : ppufile.writeentry(ibansistringdef);
         {$endif}
            st_widestring : ppufile.writeentry(ibwidestringdef);
         end;
      end;


{$ifdef GDB}
    function tstringdef.stabstring : pchar;
      var
        bytest,charst,longst : string;
        slen : aint;
      begin
        case string_typ of
           st_shortstring:
             begin
               charst:=tstoreddef(cchartype.def).numberstring;
               { this is what I found in stabs.texinfo but
                 gdb 4.12 for go32 doesn't understand that !! }
             {$IfDef GDBknowsstrings}
                stabstring:=stabstr_evaluate('n$1;$2',[charst,tostr(len)]);
             {$else}
               { fix length of openshortstring }
               slen:=len;
               if slen=0 then
                 slen:=255;
               bytest:=tstoreddef(u8inttype.def).numberstring;
               stabstring:=stabstr_evaluate('s$1length:$2,0,8;st:ar$2;1;$3;$4,8,$5;;',
                           [tostr(slen+1),bytest,tostr(slen),charst,tostr(slen*8)]);
             {$EndIf}
             end;
           st_longstring:
             begin
               charst:=tstoreddef(cchartype.def).numberstring;
               { this is what I found in stabs.texinfo but
                 gdb 4.12 for go32 doesn't understand that !! }
             {$IfDef GDBknowsstrings}
               stabstring:=stabstr_evaluate('n$1;$2',[charst,tostr(len)]);
             {$else}
               bytest:=tstoreddef(u8inttype.def).numberstring;
               longst:=tstoreddef(u32inttype.def).numberstring;
               stabstring:=stabstr_evaluate('s$1length:$2,0,32;dummy:$6,32,8;st:ar$2;1;$3;$4,40,$5;;',
                            [tostr(len+5),longst,tostr(len),charst,tostr(len*8),bytest]);
              {$EndIf}
             end;
         {$ifdef ansistring_bits}
           st_ansistring16,st_ansistring32,st_ansistring64:
         {$else}
           st_ansistring:
         {$endif}
             begin
               { an ansi string looks like a pchar easy !! }
               charst:=tstoreddef(cchartype.def).numberstring;
               stabstring:=strpnew('*'+charst);
             end;
           st_widestring:
             begin
               { an ansi string looks like a pwidechar easy !! }
               charst:=tstoreddef(cwidechartype.def).numberstring;
               stabstring:=strpnew('*'+charst);
             end;
        end;
      end;


    procedure tstringdef.concatstabto(asmlist:taasmoutput);
      begin
        if (stab_state in [stab_state_writing,stab_state_written]) then
          exit;
        case string_typ of
           st_shortstring:
             begin
               tstoreddef(cchartype.def).concatstabto(asmlist);
             {$IfNDef GDBknowsstrings}
               tstoreddef(u8inttype.def).concatstabto(asmlist);
             {$EndIf}
             end;
           st_longstring:
             begin
               tstoreddef(cchartype.def).concatstabto(asmlist);
             {$IfNDef GDBknowsstrings}
               tstoreddef(u8inttype.def).concatstabto(asmlist);
               tstoreddef(u32inttype.def).concatstabto(asmlist);
             {$EndIf}
             end;
         {$ifdef ansistring_bits}
           st_ansistring16,st_ansistring32,st_ansistring64:
         {$else}
           st_ansistring:
         {$endif}
             tstoreddef(cchartype.def).concatstabto(asmlist);
           st_widestring:
             tstoreddef(cwidechartype.def).concatstabto(asmlist);
        end;
        inherited concatstabto(asmlist);
      end;
{$endif GDB}


    function tstringdef.needs_inittable : boolean;
      begin
      {$ifdef ansistring_bits}
         needs_inittable:=string_typ in [st_ansistring16,st_ansistring32,st_ansistring64,st_widestring];
      {$else}
         needs_inittable:=string_typ in [st_ansistring,st_widestring];
      {$endif}
      end;


    function tstringdef.gettypename : string;
{$ifdef ansistring_bits}
      const
         names : array[tstringtype] of string[20] = ('',
           'shortstring','longstring','ansistring16','ansistring32','ansistring64','widestring');
{$else}
      const
         names : array[tstringtype] of string[20] = ('',
           'ShortString','LongString','AnsiString','WideString');
{$endif}
      begin
         gettypename:=names[string_typ];
      end;


    function tstringdef.alignment : longint;
      begin
        case string_typ of
          st_widestring,
          st_ansistring:
            alignment:=size_2_align(savesize);
          st_longstring,
          st_shortstring:
            alignment:=size_2_align(1);
          else
            internalerror(200412301);
        end;
      end;


    procedure tstringdef.write_rtti_data(rt:trttitype);
      begin
         case string_typ of
          {$ifdef ansistring_bits}
            st_ansistring16:
              begin
                 rttiList.concat(Tai_const.Create_8bit(tkA16String));
                 write_rtti_name;
              end;
            st_ansistring32:
              begin
                 rttiList.concat(Tai_const.Create_8bit(tkA32String));
                 write_rtti_name;
              end;
            st_ansistring64:
              begin
                 rttiList.concat(Tai_const.Create_8bit(tkA64String));
                 write_rtti_name;
              end;
          {$else}
            st_ansistring:
              begin
                 rttiList.concat(Tai_const.Create_8bit(tkAString));
                 write_rtti_name;
              end;
          {$endif}
            st_widestring:
              begin
                 rttiList.concat(Tai_const.Create_8bit(tkWString));
                 write_rtti_name;
              end;
            st_longstring:
              begin
                 rttiList.concat(Tai_const.Create_8bit(tkLString));
                 write_rtti_name;
              end;
            st_shortstring:
              begin
                 rttiList.concat(Tai_const.Create_8bit(tkSString));
                 write_rtti_name;
                 rttiList.concat(Tai_const.Create_8bit(len));
{$ifdef cpurequiresproperalignment}
                 rttilist.concat(Tai_align.Create(sizeof(TConstPtrUInt)));
{$endif cpurequiresproperalignment}
              end;
         end;
      end;


    function tstringdef.getmangledparaname : string;
      begin
        getmangledparaname:='STRING';
      end;


    function tstringdef.is_publishable : boolean;
      begin
         is_publishable:=true;
      end;


{****************************************************************************
                                 TENUMDEF
****************************************************************************}

    constructor tenumdef.create;
      begin
         inherited create;
         deftype:=enumdef;
         minval:=0;
         maxval:=0;
         calcsavesize;
         has_jumps:=false;
         basedef:=nil;
         firstenum:=nil;
         correct_owner_symtable;
      end;


    constructor tenumdef.create_subrange(_basedef:tenumdef;_min,_max:aint);
      begin
         inherited create;
         deftype:=enumdef;
         minval:=_min;
         maxval:=_max;
         basedef:=_basedef;
         calcsavesize;
         has_jumps:=false;
         firstenum:=basedef.firstenum;
         while assigned(firstenum) and (tenumsym(firstenum).value<>minval) do
           firstenum:=tenumsym(firstenum).nextenum;
         correct_owner_symtable;
      end;


    constructor tenumdef.ppuload(ppufile:tcompilerppufile);
      begin
         inherited ppuloaddef(ppufile);
         deftype:=enumdef;
         ppufile.getderef(basedefderef);
         minval:=ppufile.getaint;
         maxval:=ppufile.getaint;
         savesize:=ppufile.getaint;
         has_jumps:=false;
         firstenum:=Nil;
      end;


    function tenumdef.getcopy : tstoreddef;
      begin
        if assigned(basedef) then
          result:=tenumdef.create_subrange(basedef,minval,maxval)
        else
          begin
            result:=tenumdef.create;
            tenumdef(result).minval:=minval;
            tenumdef(result).maxval:=maxval;
          end;
        tenumdef(result).has_jumps:=has_jumps;
        tenumdef(result).firstenum:=firstenum;
        tenumdef(result).basedefderef:=basedefderef;
      end;


    procedure tenumdef.calcsavesize;
      begin
        if (aktpackenum=8) or (min<low(longint)) or (int64(max)>high(cardinal)) then
         savesize:=8
        else
         if (aktpackenum=4) or (min<low(smallint)) or (max>high(word)) then
          savesize:=4
        else
         if (aktpackenum=2) or (min<low(shortint)) or (max>high(byte)) then
          savesize:=2
        else
         savesize:=1;
      end;


    procedure tenumdef.setmax(_max:aint);
      begin
        maxval:=_max;
        calcsavesize;
      end;


    procedure tenumdef.setmin(_min:aint);
      begin
        minval:=_min;
        calcsavesize;
      end;


    function tenumdef.min:aint;
      begin
        min:=minval;
      end;


    function tenumdef.max:aint;
      begin
        max:=maxval;
      end;


    procedure tenumdef.buildderef;
      begin
        inherited buildderef;
        basedefderef.build(basedef);
      end;


    procedure tenumdef.deref;
      begin
        inherited deref;
        basedef:=tenumdef(basedefderef.resolve);
        { restart ordering }
        firstenum:=nil;
      end;


    destructor tenumdef.destroy;
      begin
        inherited destroy;
      end;


    procedure tenumdef.ppuwrite(ppufile:tcompilerppufile);
      begin
         inherited ppuwritedef(ppufile);
         ppufile.putderef(basedefderef);
         ppufile.putaint(min);
         ppufile.putaint(max);
         ppufile.putaint(savesize);
         ppufile.writeentry(ibenumdef);
      end;


    { used for enumdef because the symbols are
      inserted in the owner symtable }
    procedure tenumdef.correct_owner_symtable;
      var
         st : tsymtable;
      begin
         if assigned(owner) and
            (owner.symtabletype in [recordsymtable,objectsymtable]) then
           begin
              owner.defindex.deleteindex(self);
              st:=owner;
              while (st.symtabletype in [recordsymtable,objectsymtable]) do
                st:=st.next;
              st.registerdef(self);
           end;
      end;



{$ifdef GDB}
    function tenumdef.stabstring : pchar;

    var st:Pchar;
        p:Tenumsym;
        s:string;
        memsize,stl:cardinal;

    begin
      memsize:=memsizeinc;
      getmem(st,memsize);
      { we can specify the size with @s<size>; prefix PM }
      if savesize <> std_param_align then
        strpcopy(st,'@s'+tostr(savesize*8)+';e')
      else
        strpcopy(st,'e');
      p := tenumsym(firstenum);
      stl:=strlen(st);
      while assigned(p) do
        begin
          s :=p.name+':'+tostr(p.value)+',';
          { place for the ending ';' also }
          if (stl+length(s)+1>=memsize) then
            begin
              inc(memsize,memsizeinc);
              reallocmem(st,memsize);
            end;
          strpcopy(st+stl,s);
          inc(stl,length(s));
          p:=p.nextenum;
        end;
      st[stl]:=';';
      st[stl+1]:=#0;
      reallocmem(st,stl+2);
      stabstring:=st;
    end;
{$endif GDB}


    procedure tenumdef.write_child_rtti_data(rt:trttitype);
      begin
         if assigned(basedef) then
           basedef.get_rtti_label(rt);
      end;


    procedure tenumdef.write_rtti_data(rt:trttitype);
      var
         hp : tenumsym;
      begin
         rttiList.concat(Tai_const.Create_8bit(tkEnumeration));
         write_rtti_name;
{$ifdef cpurequiresproperalignment}
         rttilist.concat(Tai_align.Create(sizeof(TConstPtrUInt)));
{$endif cpurequiresproperalignment}
         case longint(savesize) of
            1:
              rttiList.concat(Tai_const.Create_8bit(otUByte));
            2:
              rttiList.concat(Tai_const.Create_8bit(otUWord));
            4:
              rttiList.concat(Tai_const.Create_8bit(otULong));
         end;
{$ifdef cpurequiresproperalignment}
         rttilist.concat(Tai_align.Create(4));
{$endif cpurequiresproperalignment}
         rttiList.concat(Tai_const.Create_32bit(min));
         rttiList.concat(Tai_const.Create_32bit(max));
         if assigned(basedef) then
           rttiList.concat(Tai_const.Create_sym(basedef.get_rtti_label(rt)))
         else
           rttiList.concat(Tai_const.create_sym(nil));
         hp:=tenumsym(firstenum);
         while assigned(hp) do
           begin
              rttiList.concat(Tai_const.Create_8bit(length(hp.realname)));
              rttiList.concat(Tai_string.Create(hp.realname));
              hp:=hp.nextenum;
           end;
         rttiList.concat(Tai_const.Create_8bit(0));
      end;


    function tenumdef.is_publishable : boolean;
      begin
         is_publishable:=true;
      end;

    function tenumdef.gettypename : string;

      begin
         gettypename:='<enumeration type>';
      end;

{****************************************************************************
                                 TORDDEF
****************************************************************************}

    constructor torddef.create(t : tbasetype;v,b : TConstExprInt);
      begin
         inherited create;
         deftype:=orddef;
         low:=v;
         high:=b;
         typ:=t;
         setsize;
      end;


    constructor torddef.ppuload(ppufile:tcompilerppufile);
      begin
         inherited ppuloaddef(ppufile);
         deftype:=orddef;
         typ:=tbasetype(ppufile.getbyte);
         if sizeof(TConstExprInt)=8 then
           begin
             low:=ppufile.getint64;
             high:=ppufile.getint64;
           end
         else
           begin
             low:=ppufile.getlongint;
             high:=ppufile.getlongint;
           end;
         setsize;
      end;


    function torddef.getcopy : tstoreddef;
      begin
         result:=torddef.create(typ,low,high);
         result.deftype:=orddef;
         torddef(result).low:=low;
         torddef(result).high:=high;
         torddef(result).typ:=typ;
         torddef(result).savesize:=savesize;
      end;


    procedure torddef.setsize;
      const
        sizetbl : array[tbasetype] of longint = (
          0,
          1,2,4,8,
          1,2,4,8,
          1,2,4,
          1,2,8
        );
      begin
        savesize:=sizetbl[typ];
      end;


    procedure torddef.ppuwrite(ppufile:tcompilerppufile);
      begin
         inherited ppuwritedef(ppufile);
         ppufile.putbyte(byte(typ));
         if sizeof(TConstExprInt)=8 then
          begin
            ppufile.putint64(low);
            ppufile.putint64(high);
          end
         else
          begin
            ppufile.putlongint(low);
            ppufile.putlongint(high);
          end;
         ppufile.writeentry(iborddef);
      end;


{$ifdef GDB}
    function torddef.stabstring : pchar;
      begin
        if cs_gdb_valgrind in aktglobalswitches then
          begin
            case typ of
              uvoid :
                stabstring := strpnew(numberstring);
              bool8bit,
              bool16bit,
              bool32bit :
                stabstring := stabstr_evaluate('r${numberstring};0;255;',[]);
              u32bit,
              s64bit,
              u64bit :
                stabstring:=stabstr_evaluate('r${numberstring};0;-1;',[]);
              else
                stabstring:=stabstr_evaluate('r${numberstring};$1;$2;',[tostr(longint(low)),tostr(longint(high))]);
            end;
          end
        else
          begin
            case typ of
              uvoid :
                stabstring := strpnew(numberstring);
              uchar :
                stabstring := strpnew('-20;');
              uwidechar :
                stabstring := strpnew('-30;');
              bool8bit :
                stabstring := strpnew('-21;');
              bool16bit :
                stabstring := strpnew('-22;');
              bool32bit :
                stabstring := strpnew('-23;');
              u64bit :
                stabstring := strpnew('-32;');
              s64bit :
                stabstring := strpnew('-31;');
              {u32bit : stabstring := tstoreddef(s32inttype.def).numberstring+';0;-1;'); }
              else
                stabstring:=stabstr_evaluate('r${numberstring};$1;$2;',[tostr(longint(low)),tostr(longint(high))]);
            end;
         end;
      end;
{$endif GDB}


    procedure torddef.write_rtti_data(rt:trttitype);

        procedure dointeger;
        const
          trans : array[tbasetype] of byte =
            (otUByte{otNone},
             otUByte,otUWord,otULong,otUByte{otNone},
             otSByte,otSWord,otSLong,otUByte{otNone},
             otUByte,otUWord,otULong,
             otUByte,otUWord,otUByte);
        begin
          write_rtti_name;
{$ifdef cpurequiresproperalignment}
          rttilist.concat(Tai_align.Create(sizeof(TConstPtrUInt)));
{$endif cpurequiresproperalignment}
          rttiList.concat(Tai_const.Create_8bit(byte(trans[typ])));
{$ifdef cpurequiresproperalignment}
         rttilist.concat(Tai_align.Create(4));
{$endif cpurequiresproperalignment}
          rttiList.concat(Tai_const.Create_32bit(longint(low)));
          rttiList.concat(Tai_const.Create_32bit(longint(high)));
        end;

      begin
        case typ of
          s64bit :
            begin
              rttiList.concat(Tai_const.Create_8bit(tkInt64));
              write_rtti_name;
{$ifdef cpurequiresproperalignment}
              rttilist.concat(Tai_align.Create(sizeof(TConstPtrUInt)));
{$endif cpurequiresproperalignment}
              { low }
              rttiList.concat(Tai_const.Create_64bit(int64($80000000) shl 32));
              { high }
              rttiList.concat(Tai_const.Create_64bit((int64($7fffffff) shl 32) or int64($ffffffff)));
            end;
          u64bit :
            begin
              rttiList.concat(Tai_const.Create_8bit(tkQWord));
              write_rtti_name;
{$ifdef cpurequiresproperalignment}
              rttilist.concat(Tai_align.Create(sizeof(TConstPtrUInt)));
{$endif cpurequiresproperalignment}
              { low }
              rttiList.concat(Tai_const.Create_64bit(0));
              { high }
              rttiList.concat(Tai_const.Create_64bit(int64((int64($ffffffff) shl 32) or int64($ffffffff))));
            end;
          bool8bit:
            begin
              rttiList.concat(Tai_const.Create_8bit(tkBool));
              dointeger;
            end;
          uchar:
            begin
              rttiList.concat(Tai_const.Create_8bit(tkChar));
              dointeger;
            end;
          uwidechar:
            begin
              rttiList.concat(Tai_const.Create_8bit(tkWChar));
              dointeger;
            end;
          else
            begin
              rttiList.concat(Tai_const.Create_8bit(tkInteger));
              dointeger;
            end;
        end;
      end;


    function torddef.is_publishable : boolean;
      begin
         is_publishable:=(typ<>uvoid);
      end;


    function torddef.gettypename : string;

      const
        names : array[tbasetype] of string[20] = (
          'untyped',
          'Byte','Word','DWord','QWord',
          'ShortInt','SmallInt','LongInt','Int64',
          'Boolean','WordBool','LongBool',
          'Char','WideChar','Currency');

      begin
         gettypename:=names[typ];
      end;

{****************************************************************************
                                TFLOATDEF
****************************************************************************}

    constructor tfloatdef.create(t : tfloattype);
      begin
         inherited create;
         deftype:=floatdef;
         typ:=t;
         setsize;
      end;


    constructor tfloatdef.ppuload(ppufile:tcompilerppufile);
      begin
         inherited ppuloaddef(ppufile);
         deftype:=floatdef;
         typ:=tfloattype(ppufile.getbyte);
         setsize;
      end;


    function tfloatdef.getcopy : tstoreddef;
      begin
         result:=tfloatdef.create(typ);
         result.deftype:=floatdef;
         tfloatdef(result).savesize:=savesize;
      end;


    procedure tfloatdef.setsize;
      begin
         case typ of
           s32real : savesize:=4;
           s80real : savesize:=10;
           s64real,
           s64currency,
           s64comp : savesize:=8;
         else
           savesize:=0;
         end;
      end;


    procedure tfloatdef.ppuwrite(ppufile:tcompilerppufile);
      begin
         inherited ppuwritedef(ppufile);
         ppufile.putbyte(byte(typ));
         ppufile.writeentry(ibfloatdef);
      end;


{$ifdef GDB}
    function Tfloatdef.stabstring:Pchar;
      begin
        case typ of
          s32real,s64real,s80real:
            stabstring:=stabstr_evaluate('r$1;${savesize};0;',[tstoreddef(s32inttype.def).numberstring]);
          s64currency,s64comp:
            stabstring:=stabstr_evaluate('r$1;-${savesize};0;',[tstoreddef(s32inttype.def).numberstring]);
          else
            internalerror(10005);
        end;
      end;


    procedure tfloatdef.concatstabto(asmlist:taasmoutput);
      begin
        if (stab_state in [stab_state_writing,stab_state_written]) then
          exit;
        tstoreddef(s32inttype.def).concatstabto(asmlist);
        inherited concatstabto(asmlist);
      end;
{$endif GDB}


    procedure tfloatdef.write_rtti_data(rt:trttitype);
      const
         {tfloattype = (s32real,s64real,s80real,s64bit,s128bit);}
         translate : array[tfloattype] of byte =
           (ftSingle,ftDouble,ftExtended,ftComp,ftCurr,ftFloat128);
      begin
         rttiList.concat(Tai_const.Create_8bit(tkFloat));
         write_rtti_name;
{$ifdef cpurequiresproperalignment}
         rttilist.concat(Tai_align.Create(sizeof(TConstPtrUInt)));
{$endif cpurequiresproperalignment}
         rttiList.concat(Tai_const.Create_8bit(translate[typ]));
      end;


    function tfloatdef.is_publishable : boolean;
      begin
         is_publishable:=true;
      end;

    function tfloatdef.gettypename : string;

      const
        names : array[tfloattype] of string[20] = (
          'Single','Double','Extended','Comp','Currency','Float128');

      begin
         gettypename:=names[typ];
      end;

{****************************************************************************
                                TFILEDEF
****************************************************************************}

    constructor tfiledef.createtext;
      begin
         inherited create;
         deftype:=filedef;
         filetyp:=ft_text;
         typedfiletype.reset;
         setsize;
      end;


    constructor tfiledef.createuntyped;
      begin
         inherited create;
         deftype:=filedef;
         filetyp:=ft_untyped;
         typedfiletype.reset;
         setsize;
      end;


    constructor tfiledef.createtyped(const tt : ttype);
      begin
         inherited create;
         deftype:=filedef;
         filetyp:=ft_typed;
         typedfiletype:=tt;
         setsize;
      end;


    constructor tfiledef.ppuload(ppufile:tcompilerppufile);
      begin
         inherited ppuloaddef(ppufile);
         deftype:=filedef;
         filetyp:=tfiletyp(ppufile.getbyte);
         if filetyp=ft_typed then
           ppufile.gettype(typedfiletype)
         else
           typedfiletype.reset;
         setsize;
      end;


    function tfiledef.getcopy : tstoreddef;
      begin
        case filetyp of
          ft_typed:
            result:=tfiledef.createtyped(typedfiletype);
          ft_untyped:
            result:=tfiledef.createuntyped;
          ft_text:
            result:=tfiledef.createtext;
          else
            internalerror(2004121201);
        end;
      end;


    procedure tfiledef.buildderef;
      begin
        inherited buildderef;
        if filetyp=ft_typed then
          typedfiletype.buildderef;
      end;


    procedure tfiledef.deref;
      begin
        inherited deref;
        if filetyp=ft_typed then
          typedfiletype.resolve;
      end;


    procedure tfiledef.setsize;
      begin
{$ifdef cpu64bit}
        case filetyp of
          ft_text :
            if target_info.system in [system_x86_64_win64,system_ia64_win64] then
              savesize:=632
            else
              savesize:=628;
          ft_typed,
          ft_untyped :
            if target_info.system in [system_x86_64_win64,system_ia64_win64] then
              savesize:=372
            else
              savesize:=368;
        end;
{$else cpu64bit}
        case filetyp of
          ft_text :
            savesize:=592;
          ft_typed,
          ft_untyped :
            savesize:=332;
        end;
{$endif cpu64bit}
      end;


    procedure tfiledef.ppuwrite(ppufile:tcompilerppufile);
      begin
         inherited ppuwritedef(ppufile);
         ppufile.putbyte(byte(filetyp));
         if filetyp=ft_typed then
           ppufile.puttype(typedfiletype);
         ppufile.writeentry(ibfiledef);
      end;


{$ifdef GDB}
    function tfiledef.stabstring : pchar;
      begin
   {$IfDef GDBknowsfiles}
      case filetyp of
        ft_typed :
          stabstring := strpnew('d'+typedfiletype.def.numberstring{+';'});
        ft_untyped :
          stabstring := strpnew('d'+voiddef.numberstring{+';'});
        ft_text :
          stabstring := strpnew('d'+cchartype^.numberstring{+';'});
      end;
   {$Else}
{$ifdef cpu64bit}
      stabstring:=stabstr_evaluate('s${savesize}HANDLE:$1,0,32;MODE:$1,32,32;RECSIZE:$2,64,64;'+
                                   '_PRIVATE:ar$1;1;64;$3,128,256;USERDATA:ar$1;1;16;$3,384,128;'+
                                   'NAME:ar$1;0;255;$4,512,2048;;',[tstoreddef(s32inttype.def).numberstring,
                                   tstoreddef(s64inttype.def).numberstring,
                                   tstoreddef(u8inttype.def).numberstring,
                                   tstoreddef(cchartype.def).numberstring]);
{$else cpu64bit}
      stabstring:=stabstr_evaluate('s${savesize}HANDLE:$1,0,32;MODE:$1,32,32;RECSIZE:$1,64,32;'+
                                   '_PRIVATE:ar$1;1;32;$3,96,256;USERDATA:ar$1;1;16;$2,352,128;'+
                                   'NAME:ar$1;0;255;$3,480,2048;;',[tstoreddef(s32inttype.def).numberstring,
                                   tstoreddef(u8inttype.def).numberstring,
                                   tstoreddef(cchartype.def).numberstring]);
{$endif cpu64bit}
   {$EndIf}
      end;


    procedure tfiledef.concatstabto(asmlist:taasmoutput);
      begin
        if (stab_state in [stab_state_writing,stab_state_written]) then
          exit;
  {$IfDef GDBknowsfiles}
        case filetyp of
          ft_typed :
            tstoreddef(typedfiletype.def).concatstabto(asmlist);
          ft_untyped :
            tstoreddef(voidtype.def).concatstabto(asmlist);
          ft_text :
            tstoreddef(cchartype.def).concatstabto(asmlist);
        end;
  {$Else}
        tstoreddef(s32inttype.def).concatstabto(asmlist);
{$ifdef cpu64bit}
        tstoreddef(s64inttype.def).concatstabto(asmlist);
{$endif cpu64bit}
        tstoreddef(u8inttype.def).concatstabto(asmlist);
        tstoreddef(cchartype.def).concatstabto(asmlist);
  {$EndIf}
        inherited concatstabto(asmlist);
      end;
{$endif GDB}


    function tfiledef.gettypename : string;
      begin
         case filetyp of
           ft_untyped:
             gettypename:='File';
           ft_typed:
             gettypename:='File Of '+typedfiletype.def.typename;
           ft_text:
             gettypename:='Text'
         end;
      end;


    function tfiledef.getmangledparaname : string;
      begin
         case filetyp of
           ft_untyped:
             getmangledparaname:='FILE';
           ft_typed:
             getmangledparaname:='FILE$OF$'+typedfiletype.def.mangledparaname;
           ft_text:
             getmangledparaname:='TEXT'
         end;
      end;


{****************************************************************************
                               TVARIANTDEF
****************************************************************************}

    constructor tvariantdef.create(v : tvarianttype);
      begin
         inherited create;
         varianttype:=v;
         deftype:=variantdef;
         setsize;
      end;


    constructor tvariantdef.ppuload(ppufile:tcompilerppufile);
      begin
         inherited ppuloaddef(ppufile);
         varianttype:=tvarianttype(ppufile.getbyte);
         deftype:=variantdef;
         setsize;
      end;


    function tvariantdef.getcopy : tstoreddef;
      begin
        result:=tvariantdef.create(varianttype);
      end;


    procedure tvariantdef.ppuwrite(ppufile:tcompilerppufile);
      begin
         inherited ppuwritedef(ppufile);
         ppufile.putbyte(byte(varianttype));
         ppufile.writeentry(ibvariantdef);
      end;


    procedure tvariantdef.setsize;
      begin
         savesize:=16;
      end;


    function tvariantdef.gettypename : string;
      begin
         case varianttype of
           vt_normalvariant:
             gettypename:='Variant';
           vt_olevariant:
             gettypename:='OleVariant';
         end;
      end;


    procedure tvariantdef.write_rtti_data(rt:trttitype);
      begin
         rttiList.concat(Tai_const.Create_8bit(tkVariant));
      end;


    function tvariantdef.needs_inittable : boolean;
      begin
         needs_inittable:=true;
      end;

{$ifdef GDB}
    function tvariantdef.stabstring : pchar;
      begin
        stabstring:=stabstr_evaluate('formal${numberstring};',[]);
      end;


    function tvariantdef.numberstring:string;
      begin
        result:=tstoreddef(voidtype.def).numberstring;
      end;


    procedure tvariantdef.concatstabto(asmlist : taasmoutput);
      begin
        { don't know how to handle this }
      end;
{$endif GDB}


    function tvariantdef.is_publishable : boolean;
      begin
         is_publishable:=true;
      end;


{****************************************************************************
                               TPOINTERDEF
****************************************************************************}

    constructor tpointerdef.create(const tt : ttype);
      begin
        inherited create;
        deftype:=pointerdef;
        pointertype:=tt;
        is_far:=false;
        savesize:=sizeof(aint);
      end;


    constructor tpointerdef.createfar(const tt : ttype);
      begin
        inherited create;
        deftype:=pointerdef;
        pointertype:=tt;
        is_far:=true;
        savesize:=sizeof(aint);
      end;


    constructor tpointerdef.ppuload(ppufile:tcompilerppufile);
      begin
         inherited ppuloaddef(ppufile);
         deftype:=pointerdef;
         ppufile.gettype(pointertype);
         is_far:=(ppufile.getbyte<>0);
         savesize:=sizeof(aint);
      end;


    function tpointerdef.getcopy : tstoreddef;
      begin
        result:=tpointerdef.create(pointertype);
        tpointerdef(result).is_far:=is_far;
        tpointerdef(result).savesize:=savesize;
      end;


    procedure tpointerdef.buildderef;
      begin
        inherited buildderef;
        pointertype.buildderef;
      end;


    procedure tpointerdef.deref;
      begin
        inherited deref;
        pointertype.resolve;
      end;


    procedure tpointerdef.ppuwrite(ppufile:tcompilerppufile);
      begin
         inherited ppuwritedef(ppufile);
         ppufile.puttype(pointertype);
         ppufile.putbyte(byte(is_far));
         ppufile.writeentry(ibpointerdef);
      end;


{$ifdef GDB}
    function tpointerdef.stabstring : pchar;
      begin
        stabstring := strpnew('*'+tstoreddef(pointertype.def).numberstring);
      end;


    procedure tpointerdef.concatstabto(asmlist : taasmoutput);
      var st,nb : string;

      begin
        if (stab_state in [stab_state_writing,stab_state_written]) then
          exit;
        stab_state:=stab_state_writing;

        tstoreddef(pointertype.def).concatstabto(asmlist);

        if (pointertype.def.deftype in [recorddef,objectdef]) then
          begin
            if pointertype.def.deftype=objectdef then
              nb:=tobjectdef(pointertype.def).classnumberstring
            else
              nb:=tstoreddef(pointertype.def).numberstring;
            {to avoid infinite recursion in record with next-like fields }
            if tstoreddef(pointertype.def).stab_state=stab_state_writing then
              begin
                if assigned(pointertype.def.typesym) then
                  begin
                    if assigned(typesym) then
                      st := ttypesym(typesym).name
                    else
                      st := ' ';
                    asmlist.concat(Tai_stabs.create(stabstr_evaluate(
                            '"$1:t${numberstring}=*$2=xs$3:",${N_LSYM},0,0,0',
                            [st,nb,pointertype.def.typesym.name])));
                  end;
                stab_state:=stab_state_written;
              end
            else
              begin
                stab_state:=stab_state_used;
                inherited concatstabto(asmlist);
              end;
          end
        else
          begin
            stab_state:=stab_state_used;
            inherited concatstabto(asmlist);
          end;
      end;
{$endif GDB}


    function tpointerdef.gettypename : string;
      begin
         if is_far then
          gettypename:='^'+pointertype.def.typename+';far'
         else
          gettypename:='^'+pointertype.def.typename;
      end;


{****************************************************************************
                              TCLASSREFDEF
****************************************************************************}

    constructor tclassrefdef.create(const t:ttype);
      begin
         inherited create(t);
         deftype:=classrefdef;
      end;


    constructor tclassrefdef.ppuload(ppufile:tcompilerppufile);
      begin
         { be careful, tclassdefref inherits from tpointerdef }
         inherited ppuloaddef(ppufile);
         deftype:=classrefdef;
         ppufile.gettype(pointertype);
         is_far:=false;
         savesize:=sizeof(aint);
      end;


    procedure tclassrefdef.ppuwrite(ppufile:tcompilerppufile);
      begin
         { be careful, tclassdefref inherits from tpointerdef }
         inherited ppuwritedef(ppufile);
         ppufile.puttype(pointertype);
         ppufile.writeentry(ibclassrefdef);
      end;


{$ifdef GDB}
    function tclassrefdef.stabstring : pchar;
      begin
         stabstring:=strpnew(tstoreddef(pvmttype.def).numberstring);
      end;
{$endif GDB}


    function tclassrefdef.gettypename : string;
      begin
         gettypename:='Class Of '+pointertype.def.typename;
      end;


    function tclassrefdef.is_publishable : boolean;
      begin
         is_publishable:=true;
      end;



{***************************************************************************
                                   TSETDEF
***************************************************************************}

    constructor tsetdef.create(const t:ttype;high : longint);
      begin
         inherited create;
         deftype:=setdef;
         elementtype:=t;
         setbase:=low;
         setmax:=high;
         if (high-low)+1<=32 then
           begin
            settype:=smallset;
           {$ifdef testvarsets}
            if aktsetalloc=0 THEN      { $PACKSET Fixed?}
           {$endif}
            savesize:=Sizeof(longint)
           {$ifdef testvarsets}
           else                       {No, use $PACKSET VALUE for rounding}
            savesize:=aktsetalloc*((high+aktsetalloc*8-1) DIV (aktsetalloc*8))
           {$endif}
              ;
          end
         else
          if high<256 then
           begin
              settype:=normset;
              savesize:=32;
           end
         else
{$ifdef testvarsets}
         if high<$10000 then
           begin
              settype:=varset;
              savesize:=4*((high+31) div 32);
           end
         else
{$endif testvarsets}
          Message(sym_e_ill_type_decl_set);
      end;


    constructor tsetdef.ppuload(ppufile:tcompilerppufile);
      begin
         inherited ppuloaddef(ppufile);
         deftype:=setdef;
         ppufile.gettype(elementtype);
         settype:=tsettype(ppufile.getbyte);
         case settype of
            normset : savesize:=32;
            varset : savesize:=ppufile.getlongint;
            smallset : savesize:=Sizeof(longint);
         end;
          normset : savesize:=ppufile.getaint;
          smallset : savesize:=Sizeof(longint);
        end;
      end;


    destructor tsetdef.destroy;
      begin
        inherited destroy;
      end;


    function tsetdef.getcopy : tstoreddef;
      begin
        case settype of
          smallset:
            result:=tsetdef.create(elementtype,31);
          normset:
            result:=tsetdef.create(elementtype,255);
          else
            internalerror(2004121202);
        end;
      end;


    procedure tsetdef.ppuwrite(ppufile:tcompilerppufile);
      begin
         inherited ppuwritedef(ppufile);
         ppufile.puttype(elementtype);
         ppufile.putbyte(byte(settype));
         if settype=varset then
           ppufile.putlongint(savesize);
         if settype=normset then
           ppufile.putaint(savesize);
         ppufile.writeentry(ibsetdef);
      end;


{$ifdef GDB}
    function tsetdef.stabstring : pchar;
      begin
        stabstring:=stabstr_evaluate('@s$1;S$2',[tostr(savesize*8),tstoreddef(elementtype.def).numberstring]);
      end;


    procedure tsetdef.concatstabto(asmlist:taasmoutput);
      begin
        if (stab_state in [stab_state_writing,stab_state_written]) then
          exit;
        tstoreddef(elementtype.def).concatstabto(asmlist);
        inherited concatstabto(asmlist);
      end;
{$endif GDB}


    procedure tsetdef.buildderef;
      begin
        inherited buildderef;
        elementtype.buildderef;
      end;


    procedure tsetdef.deref;
      begin
        inherited deref;
        elementtype.resolve;
      end;


    procedure tsetdef.write_child_rtti_data(rt:trttitype);
      begin
        tstoreddef(elementtype.def).get_rtti_label(rt);
      end;


    procedure tsetdef.write_rtti_data(rt:trttitype);
      begin
         rttiList.concat(Tai_const.Create_8bit(tkSet));
         write_rtti_name;
{$ifdef cpurequiresproperalignment}
         rttilist.concat(Tai_align.Create(sizeof(TConstPtrUInt)));
{$endif cpurequiresproperalignment}
         rttiList.concat(Tai_const.Create_8bit(otULong));
{$ifdef cpurequiresproperalignment}
         rttilist.concat(Tai_align.Create(sizeof(TConstPtrUInt)));
{$endif cpurequiresproperalignment}
         rttiList.concat(Tai_const.Create_sym(tstoreddef(elementtype.def).get_rtti_label(rt)));
      end;


    function tsetdef.is_publishable : boolean;
      begin
         is_publishable:=(settype=smallset);
      end;


    function tsetdef.gettypename : string;
      begin
         if assigned(elementtype.def) then
          gettypename:='Set Of '+elementtype.def.typename
         else
          gettypename:='Empty Set';
      end;


{***************************************************************************
                                 TFORMALDEF
***************************************************************************}

    constructor tformaldef.create;
      var
         stregdef : boolean;
      begin
         stregdef:=registerdef;
         registerdef:=false;
         inherited create;
         deftype:=formaldef;
         registerdef:=stregdef;
         { formaldef must be registered at unit level !! }
         if registerdef and assigned(current_module) then
            if assigned(current_module.localsymtable) then
              tsymtable(current_module.localsymtable).registerdef(self)
            else if assigned(current_module.globalsymtable) then
              tsymtable(current_module.globalsymtable).registerdef(self);
         savesize:=0;
      end;


    constructor tformaldef.ppuload(ppufile:tcompilerppufile);
      begin
         inherited ppuloaddef(ppufile);
         deftype:=formaldef;
         savesize:=0;
      end;


    procedure tformaldef.ppuwrite(ppufile:tcompilerppufile);
      begin
         inherited ppuwritedef(ppufile);
         ppufile.writeentry(ibformaldef);
      end;


{$ifdef GDB}
    function tformaldef.stabstring : pchar;
      begin
        stabstring:=stabstr_evaluate('formal${numberstring};',[]);
      end;


    function tformaldef.numberstring:string;
      begin
        result:=tstoreddef(voidtype.def).numberstring;
      end;


    procedure tformaldef.concatstabto(asmlist : taasmoutput);
      begin
        { formaldef can't be stab'ed !}
      end;
{$endif GDB}


    function tformaldef.gettypename : string;
      begin
         gettypename:='<Formal type>';
      end;


{***************************************************************************
                           TARRAYDEF
***************************************************************************}

    constructor tarraydef.create(l,h : aint;const t : ttype);
      begin
         inherited create;
         deftype:=arraydef;
         lowrange:=l;
         highrange:=h;
         rangetype:=t;
         elementtype.reset;
         IsVariant:=false;
         IsConstructor:=false;
         IsArrayOfConst:=false;
         IsDynamicArray:=false;
         IsConvertedPointer:=false;
      end;


    constructor tarraydef.create_from_pointer(const elemt : ttype);
      begin
         self.create(0,$7fffffff,s32inttype);
         IsConvertedPointer:=true;
         setelementtype(elemt);
      end;


    constructor tarraydef.ppuload(ppufile:tcompilerppufile);
      begin
         inherited ppuloaddef(ppufile);
         deftype:=arraydef;
         { the addresses are calculated later }
         ppufile.gettype(_elementtype);
         ppufile.gettype(rangetype);
         lowrange:=ppufile.getaint;
         highrange:=ppufile.getaint;
         IsArrayOfConst:=boolean(ppufile.getbyte);
         IsDynamicArray:=boolean(ppufile.getbyte);
         IsVariant:=false;
         IsConstructor:=false;
      end;


    function tarraydef.getcopy : tstoreddef;
      begin
        result:=tarraydef.create(lowrange,highrange,rangetype);
        tarraydef(result).IsConvertedPointer:=IsConvertedPointer;
        tarraydef(result).IsDynamicArray:=IsDynamicArray;
        tarraydef(result).IsVariant:=IsVariant;
        tarraydef(result).IsConstructor:=IsConstructor;
        tarraydef(result).IsArrayOfConst:=IsArrayOfConst;
        tarraydef(result)._elementtype:=_elementtype;
      end;


    procedure tarraydef.buildderef;
      begin
        inherited buildderef;
        _elementtype.buildderef;
        rangetype.buildderef;
      end;


    procedure tarraydef.deref;
      begin
        inherited deref;
        _elementtype.resolve;
        rangetype.resolve;
      end;


    procedure tarraydef.ppuwrite(ppufile:tcompilerppufile);
      begin
         inherited ppuwritedef(ppufile);
         ppufile.puttype(_elementtype);
         ppufile.puttype(rangetype);
         ppufile.putaint(lowrange);
         ppufile.putaint(highrange);
         ppufile.putbyte(byte(IsArrayOfConst));
         ppufile.putbyte(byte(IsDynamicArray));
         ppufile.writeentry(ibarraydef);
      end;


{$ifdef GDB}
    function tarraydef.stabstring : pchar;
      begin
        stabstring:=stabstr_evaluate('ar$1;$2;$3;$4',[Tstoreddef(rangetype.def).numberstring,
                    tostr(lowrange),tostr(highrange),Tstoreddef(_elementtype.def).numberstring]);
      end;


    procedure tarraydef.concatstabto(asmlist:taasmoutput);
      begin
        if (stab_state in [stab_state_writing,stab_state_written]) then
          exit;
        tstoreddef(rangetype.def).concatstabto(asmlist);
        tstoreddef(_elementtype.def).concatstabto(asmlist);
        inherited concatstabto(asmlist);
      end;
{$endif GDB}


    function tarraydef.elesize : aint;
      begin
        elesize:=_elementtype.def.size;
      end;


    function tarraydef.elecount : aint;
      var
        qhigh,qlow : qword;
      begin
        if IsDynamicArray then
          begin
            result:=0;
            exit;
          end;
        if (highrange>0) and (lowrange<0) then
          begin
            qhigh:=highrange;
            qlow:=qword(-lowrange);
            { prevent overflow, return -1 to indicate overflow }
            if qhigh+qlow>qword(high(aint)-1) then
              result:=-1
            else
              result:=qhigh+qlow+1;
          end
        else
          result:=int64(highrange)-lowrange+1;
      end;


    function tarraydef.size : aint;
      var
        cachedelecount,
        cachedelesize : aint;
      begin
        if IsDynamicArray then
          begin
            size:=sizeof(aint);
            exit;
          end;
        { Tarraydef.size may never be called for an open array! }
        if highrange<lowrange then
          internalerror(99080501);
        cachedelesize:=elesize;
        cachedelecount:=elecount;
        { prevent overflow, return -1 to indicate overflow }
        if (cachedelesize <> 0) and
           (
            (cachedelecount < 0) or
            ((high(aint) div cachedelesize) < cachedelecount) or
            { also lowrange*elesize must be < high(aint) to prevent overflow when
              accessing the array, see ncgmem (PFV) }
            ((high(aint) div cachedelesize) < abs(lowrange))
           ) then
          result:=-1
        else
          result:=cachedelesize*cachedelecount;
      end;


    procedure tarraydef.setelementtype(t: ttype);
      begin
        _elementtype:=t;
       if not(IsDynamicArray or
              IsConvertedPointer or
              (highrange<lowrange)) then
         begin
           if (size=-1) then
             Message(sym_e_segment_too_large);
         end;
      end;


    function tarraydef.alignment : longint;
      begin
         { alignment is the size of the elements }
         if (elementtype.def.deftype in [arraydef,recorddef]) or
           ((elementtype.def.deftype=objectdef) and
             is_object(elementtype.def)) then
           alignment:=elementtype.def.alignment
         else
           alignment:=elesize;
      end;


    function tarraydef.needs_inittable : boolean;
      begin
         needs_inittable:=IsDynamicArray or elementtype.def.needs_inittable;
      end;


    procedure tarraydef.write_child_rtti_data(rt:trttitype);
      begin
        tstoreddef(elementtype.def).get_rtti_label(rt);
      end;


    procedure tarraydef.write_rtti_data(rt:trttitype);
      begin
         if IsDynamicArray then
           rttiList.concat(Tai_const.Create_8bit(tkdynarray))
         else
           rttiList.concat(Tai_const.Create_8bit(tkarray));
         write_rtti_name;
{$ifdef cpurequiresproperalignment}
         rttilist.concat(Tai_align.Create(sizeof(TConstPtrUInt)));
{$endif cpurequiresproperalignment}
         { size of elements }
         rttiList.concat(Tai_const.Create_aint(elesize));
         if not(IsDynamicArray) then
           rttiList.concat(Tai_const.Create_aint(elecount));
         { element type }
         rttiList.concat(Tai_const.Create_sym(tstoreddef(elementtype.def).get_rtti_label(rt)));
         { variant type }
         // !!!!!!!!!!!!!!!!
      end;


    function tarraydef.gettypename : string;
      begin
         if isarrayofconst or isConstructor then
           begin
             if isvariant or ((highrange=-1) and (lowrange=0)) then
               gettypename:='Array Of Const'
             else
               gettypename:='Array Of '+elementtype.def.typename;
           end
         else if ((highrange=-1) and (lowrange=0)) or IsDynamicArray then
           gettypename:='Array Of '+elementtype.def.typename
         else
           begin
              if rangetype.def.deftype=enumdef then
                gettypename:='Array['+rangetype.def.typename+'] Of '+elementtype.def.typename
              else
                gettypename:='Array['+tostr(lowrange)+'..'+
                  tostr(highrange)+'] Of '+elementtype.def.typename
           end;
      end;


    function tarraydef.getmangledparaname : string;
      begin
         if isarrayofconst then
          getmangledparaname:='array_of_const'
         else
          if ((highrange=-1) and (lowrange=0)) then
           getmangledparaname:='array_of_'+elementtype.def.mangledparaname
         else
          internalerror(200204176);
      end;


{***************************************************************************
                              tabstractrecorddef
***************************************************************************}

    function tabstractrecorddef.getsymtable(t:tgetsymtable):tsymtable;
      begin
         if t=gs_record then
         getsymtable:=symtable
        else
         getsymtable:=nil;
      end;


{$ifdef GDB}
    procedure tabstractrecorddef.field_addname(p:Tnamedindexitem;arg:pointer);
      var
        newrec:Pchar;
        spec:string[3];
        varsize : aint;
        state   : ^Trecord_stabgen_state;
      begin
        state:=arg;
        { static variables from objects are like global objects }
        if (Tsym(p).typ=fieldvarsym) and not (sp_static in Tsym(p).symoptions) then
          begin
            if ([sp_protected,sp_strictprotected]*tsym(p).symoptions)<>[] then
              spec:='/1'
            else if ([sp_private,sp_strictprivate]*tsym(p).symoptions)<>[] then
              spec:='/0'
            else
              spec:='';
            varsize:=tfieldvarsym(p).vartype.def.size;
            { open arrays made overflows !! }
            if varsize>$fffffff then
              varsize:=$fffffff;
            newrec:=stabstr_evaluate('$1:$2,$3,$4;',[p.name,
                                     spec+tstoreddef(tfieldvarsym(p).vartype.def).numberstring,
                                     tostr(tfieldvarsym(p).fieldoffset*8),tostr(varsize*8)]);
            if state^.stabsize+strlen(newrec)>=state^.staballoc-256 then
              begin
                inc(state^.staballoc,memsizeinc);
                reallocmem(state^.stabstring,state^.staballoc);
              end;
            strcopy(state^.stabstring+state^.stabsize,newrec);
            inc(state^.stabsize,strlen(newrec));
            strdispose(newrec);
            {This should be used for case !!}
            inc(state^.recoffset,Tfieldvarsym(p).vartype.def.size);
          end;
      end;


    procedure tabstractrecorddef.field_concatstabto(p:Tnamedindexitem;arg:pointer);
      begin
        if (Tsym(p).typ=fieldvarsym) and not (sp_static in Tsym(p).symoptions) then
          tstoreddef(tfieldvarsym(p).vartype.def).concatstabto(taasmoutput(arg));
      end;


{$endif GDB}


    procedure tabstractrecorddef.count_field_rtti(sym : tnamedindexitem;arg:pointer);
      begin
         if (FRTTIType=fullrtti) or
            ((tsym(sym).typ=fieldvarsym) and
             tfieldvarsym(sym).vartype.def.needs_inittable) then
           inc(Count);
      end;


    procedure tabstractrecorddef.generate_field_rtti(sym:tnamedindexitem;arg:pointer);
      begin
         if (FRTTIType=fullrtti) or
            ((tsym(sym).typ=fieldvarsym) and
             tfieldvarsym(sym).vartype.def.needs_inittable) then
           tstoreddef(tfieldvarsym(sym).vartype.def).get_rtti_label(FRTTIType);
      end;


    procedure tabstractrecorddef.write_field_rtti(sym : tnamedindexitem;arg:pointer);
      begin
         if (FRTTIType=fullrtti) or
            ((tsym(sym).typ=fieldvarsym) and
             tfieldvarsym(sym).vartype.def.needs_inittable) then
          begin
            rttiList.concat(Tai_const.Create_sym(tstoreddef(tfieldvarsym(sym).vartype.def).get_rtti_label(FRTTIType)));
            rttiList.concat(Tai_const.Create_32bit(tfieldvarsym(sym).fieldoffset));
          end;
      end;



{***************************************************************************
                                  trecorddef
***************************************************************************}

    constructor trecorddef.create(p : tsymtable);
      begin
         inherited create;
         deftype:=recorddef;
         symtable:=p;
         symtable.defowner:=self;
         isunion:=false;
      end;


    constructor trecorddef.ppuload(ppufile:tcompilerppufile);
      begin
         inherited ppuloaddef(ppufile);
         deftype:=recorddef;
         symtable:=trecordsymtable.create(0);
         trecordsymtable(symtable).datasize:=ppufile.getaint;
         trecordsymtable(symtable).fieldalignment:=shortint(ppufile.getbyte);
         trecordsymtable(symtable).recordalignment:=shortint(ppufile.getbyte);
         trecordsymtable(symtable).padalignment:=shortint(ppufile.getbyte);
         trecordsymtable(symtable).ppuload(ppufile);
         symtable.defowner:=self;
         isunion:=false;
      end;


    destructor trecorddef.destroy;
      begin
         if assigned(symtable) then
           symtable.free;
         inherited destroy;
      end;


    function trecorddef.getcopy : tstoreddef;
      begin
        result:=trecorddef.create(symtable.getcopy);
        trecorddef(result).isunion:=isunion;
      end;


    function trecorddef.needs_inittable : boolean;
      begin
        needs_inittable:=trecordsymtable(symtable).needs_init_final
      end;


    procedure trecorddef.buildderef;
      var
         oldrecsyms : tsymtable;
      begin
         inherited buildderef;
         oldrecsyms:=aktrecordsymtable;
         aktrecordsymtable:=symtable;
         { now build the definitions }
         tstoredsymtable(symtable).buildderef;
         aktrecordsymtable:=oldrecsyms;
      end;


    procedure trecorddef.deref;
      var
         oldrecsyms : tsymtable;
      begin
         inherited deref;
         oldrecsyms:=aktrecordsymtable;
         aktrecordsymtable:=symtable;
         { now dereference the definitions }
         tstoredsymtable(symtable).deref;
         aktrecordsymtable:=oldrecsyms;
         { assign TGUID? load only from system unit }
         if not(assigned(rec_tguid)) and
            (upper(typename)='TGUID') and
            assigned(owner) and
            assigned(owner.name) and
            (owner.name^='SYSTEM') then
           rec_tguid:=self;
      end;


    procedure trecorddef.ppuwrite(ppufile:tcompilerppufile);
      begin
         inherited ppuwritedef(ppufile);
         ppufile.putaint(trecordsymtable(symtable).datasize);
         ppufile.putbyte(byte(trecordsymtable(symtable).fieldalignment));
         ppufile.putbyte(byte(trecordsymtable(symtable).recordalignment));
         ppufile.putbyte(byte(trecordsymtable(symtable).padalignment));
         ppufile.writeentry(ibrecorddef);
         trecordsymtable(symtable).ppuwrite(ppufile);
      end;


    function trecorddef.size:aint;
      begin
        result:=trecordsymtable(symtable).datasize;
      end;


    function trecorddef.alignment:longint;
      begin
        alignment:=trecordsymtable(symtable).recordalignment;
      end;


    function trecorddef.padalignment:longint;
      begin
        padalignment := trecordsymtable(symtable).padalignment;
      end;

{$ifdef GDB}
    function trecorddef.stabstring : pchar;
      var
        state:Trecord_stabgen_state;
      begin
        getmem(state.stabstring,memsizeinc);
        state.staballoc:=memsizeinc;
        strpcopy(state.stabstring,'s'+tostr(size));
        state.recoffset:=0;
        state.stabsize:=strlen(state.stabstring);
        symtable.foreach(@field_addname,@state);
        state.stabstring[state.stabsize]:=';';
        state.stabstring[state.stabsize+1]:=#0;
        reallocmem(state.stabstring,state.stabsize+2);
        stabstring:=state.stabstring;
      end;


    procedure trecorddef.concatstabto(asmlist:taasmoutput);
      begin
        if (stab_state in [stab_state_writing,stab_state_written]) then
          exit;
        symtable.foreach(@field_concatstabto,asmlist);
        inherited concatstabto(asmlist);
      end;
{$endif GDB}


    procedure trecorddef.write_child_rtti_data(rt:trttitype);
      begin
         FRTTIType:=rt;
         symtable.foreach(@generate_field_rtti,nil);
      end;


    procedure trecorddef.write_rtti_data(rt:trttitype);
      begin
         rttiList.concat(Tai_const.Create_8bit(tkrecord));
         write_rtti_name;
{$ifdef cpurequiresproperalignment}
         rttilist.concat(Tai_align.Create(sizeof(TConstPtrUInt)));
{$endif cpurequiresproperalignment}
         rttiList.concat(Tai_const.Create_32bit(size));
         Count:=0;
         FRTTIType:=rt;
         symtable.foreach(@count_field_rtti,nil);
         rttiList.concat(Tai_const.Create_32bit(Count));
         symtable.foreach(@write_field_rtti,nil);
      end;


    function trecorddef.gettypename : string;
      begin
         gettypename:='<record type>'
      end;


{***************************************************************************
                       TABSTRACTPROCDEF
***************************************************************************}

    constructor tabstractprocdef.create(level:byte);
      begin
         inherited create;
         parast:=tparasymtable.create(level);
         parast.defowner:=self;
         parast.next:=owner;
         paras:=nil;
         minparacount:=0;
         maxparacount:=0;
         proctypeoption:=potype_none;
         proccalloption:=pocall_none;
         procoptions:=[];
         rettype:=voidtype;
{$ifdef i386}
         fpu_used:=0;
{$endif i386}
         savesize:=sizeof(aint);
         requiredargarea:=0;
         has_paraloc_info:=false;

         location_reset(funcretloc[callerside],LOC_INVALID,OS_NO);
         location_reset(funcretloc[calleeside],LOC_INVALID,OS_NO);
      end;


    destructor tabstractprocdef.destroy;
      begin
         if assigned(paras) then
           begin
{$ifdef MEMDEBUG}
             memprocpara.start;
{$endif MEMDEBUG}
             paras.free;
{$ifdef MEMDEBUG}
             memprocpara.stop;
{$endif MEMDEBUG}
          end;
         if assigned(parast) then
          begin
{$ifdef MEMDEBUG}
            memprocparast.start;
{$endif MEMDEBUG}
            parast.free;
{$ifdef MEMDEBUG}
            memprocparast.stop;
{$endif MEMDEBUG}
          end;
         inherited destroy;
      end;


    procedure tabstractprocdef.releasemem;
      begin
        if assigned(paras) then
          begin
            paras.free;
            paras:=nil;
          end;
        parast.free;
        parast:=nil;
      end;


    procedure tabstractprocdef.count_para(p:tnamedindexitem;arg:pointer);
      begin
        if (tsym(p).typ<>paravarsym) then
          exit;
        inc(plongint(arg)^);
        if not(vo_is_hidden_para in tparavarsym(p).varoptions) then
         begin
           if not assigned(tparavarsym(p).defaultconstsym) then
             inc(minparacount);
           inc(maxparacount);
         end;
      end;


    procedure tabstractprocdef.insert_para(p:tnamedindexitem;arg:pointer);
      begin
        if (tsym(p).typ<>paravarsym) then
          exit;
        paras.add(p);
      end;


    procedure tabstractprocdef.calcparas;
      var
        paracount : longint;
      begin
        { This can already be assigned when
          we need to reresolve this unit (PFV) }
        if assigned(paras) then
          paras.free;
        paras:=tparalist.create;
        paracount:=0;
        minparacount:=0;
        maxparacount:=0;
        parast.foreach(@count_para,@paracount);
        paras.capacity:=paracount;
        { Insert parameters in table }
        parast.foreach(@insert_para,nil);
        { Order parameters }
        paras.sortparas;
      end;


    { all functions returning in FPU are
      assume to use 2 FPU registers
      until the function implementation
      is processed   PM }
    procedure tabstractprocdef.test_if_fpu_result;
      begin
{$ifdef i386}
         if assigned(rettype.def) and
            (rettype.def.deftype=floatdef) then
           fpu_used:=maxfpuregs;
{$endif i386}
      end;


    procedure tabstractprocdef.buildderef;
      begin
         { released procdef? }
         if not assigned(parast) then
           exit;
         inherited buildderef;
         rettype.buildderef;
         { parast }
         tparasymtable(parast).buildderef;
      end;


    procedure tabstractprocdef.deref;
      begin
         inherited deref;
         rettype.resolve;
         { parast }
         tparasymtable(parast).deref;
         { recalculated parameters }
         calcparas;
      end;


    constructor tabstractprocdef.ppuload(ppufile:tcompilerppufile);
      var
        b : byte;
      begin
         inherited ppuloaddef(ppufile);
         parast:=nil;
         Paras:=nil;
         minparacount:=0;
         maxparacount:=0;
         ppufile.gettype(rettype);
{$ifdef i386}
         fpu_used:=ppufile.getbyte;
{$else}
         ppufile.getbyte;
{$endif i386}
         proctypeoption:=tproctypeoption(ppufile.getbyte);
         proccalloption:=tproccalloption(ppufile.getbyte);
         ppufile.getsmallset(procoptions);

         location_reset(funcretloc[callerside],LOC_INVALID,OS_NO);
         location_reset(funcretloc[calleeside],LOC_INVALID,OS_NO);
         if po_explicitparaloc in procoptions then
           begin
             b:=ppufile.getbyte;
             if b<>sizeof(funcretloc[callerside]) then
               internalerror(200411154);
             ppufile.getdata(funcretloc[callerside],sizeof(funcretloc[callerside]));
           end;

         savesize:=sizeof(aint);
         has_paraloc_info:=(po_explicitparaloc in procoptions);
      end;


    procedure tabstractprocdef.ppuwrite(ppufile:tcompilerppufile);
      var
        oldintfcrc : boolean;
      begin
         { released procdef? }
         if not assigned(parast) then
           exit;
         inherited ppuwritedef(ppufile);
         ppufile.puttype(rettype);
         oldintfcrc:=ppufile.do_interface_crc;
         ppufile.do_interface_crc:=false;
{$ifdef i386}
         if simplify_ppu then
          fpu_used:=0;
         ppufile.putbyte(fpu_used);
{$else}
         ppufile.putbyte(0);
{$endif}
         ppufile.putbyte(ord(proctypeoption));
         ppufile.putbyte(ord(proccalloption));
         ppufile.putsmallset(procoptions);
         ppufile.do_interface_crc:=oldintfcrc;

         if (po_explicitparaloc in procoptions) then
           begin
             { Make a 'valid' funcretloc for procedures }
             ppufile.putbyte(sizeof(funcretloc[callerside]));
             ppufile.putdata(funcretloc[callerside],sizeof(funcretloc[callerside]));
           end;
      end;


    function tabstractprocdef.typename_paras(showhidden:boolean) : string;
      var
        hs,s  : string;
        hp    : TParavarsym;
        hpc   : tconstsym;
        first : boolean;
        i     : integer;
      begin
        s:='';
        first:=true;
        for i:=0 to paras.count-1 do
         begin
           hp:=tparavarsym(paras[i]);
           if not(vo_is_hidden_para in hp.varoptions) or
              (showhidden) then
            begin
               if first then
                begin
                  s:=s+'(';
                  first:=false;
                end
               else
                s:=s+',';
               case hp.varspez of
                 vs_var :
                   s:=s+'var';
                 vs_const :
                   s:=s+'const';
                 vs_out :
                   s:=s+'out';
               end;
               if assigned(hp.vartype.def.typesym) then
                 begin
                   if s<>'(' then
                    s:=s+' ';
                   hs:=hp.vartype.def.typesym.realname;
                   if hs[1]<>'$' then
                     s:=s+hp.vartype.def.typesym.realname
                   else
                     s:=s+hp.vartype.def.gettypename;
                 end
               else
                 s:=s+hp.vartype.def.gettypename;
               { default value }
               if assigned(hp.defaultconstsym) then
                begin
                  hpc:=tconstsym(hp.defaultconstsym);
                  hs:='';
                  case hpc.consttyp of
                    conststring,
                    constresourcestring :
                      hs:=strpas(pchar(hpc.value.valueptr));
                    constreal :
                      str(pbestreal(hpc.value.valueptr)^,hs);
                    constpointer :
                      hs:=tostr(hpc.value.valueordptr);
                    constord :
                      begin
                        if is_boolean(hpc.consttype.def) then
                          begin
                            if hpc.value.valueord<>0 then
                             hs:='TRUE'
                            else
                             hs:='FALSE';
                          end
                        else
                          hs:=tostr(hpc.value.valueord);
                      end;
                    constnil :
                      hs:='nil';
                    constset :
                      hs:='<set>';
                  end;
                  if hs<>'' then
                   s:=s+'="'+hs+'"';
                end;
             end;
         end;
        if not first then
         s:=s+')';
        if (po_varargs in procoptions) then
         s:=s+';VarArgs';
        typename_paras:=s;
      end;


    function tabstractprocdef.is_methodpointer:boolean;
      begin
        result:=false;
      end;


    function tabstractprocdef.is_addressonly:boolean;
      begin
        result:=true;
      end;


{$ifdef GDB}
    function tabstractprocdef.stabstring : pchar;
      begin
        stabstring := strpnew('abstractproc'+numberstring+';');
      end;
{$endif GDB}


{***************************************************************************
                                  TPROCDEF
***************************************************************************}

    constructor tprocdef.create(level:byte);
      begin
         inherited create(level);
         deftype:=procdef;
         _mangledname:=nil;
         fileinfo:=aktfilepos;
         extnumber:=$ffff;
         aliasnames:=tstringlist.create;
         funcretsym:=nil;
         localst := nil;
         defref:=nil;
         lastwritten:=nil;
         refcount:=0;
         if (cs_browser in aktmoduleswitches) and make_ref then
          begin
            defref:=tref.create(defref,@akttokenpos);
            inc(refcount);
          end;
         lastref:=defref;
         forwarddef:=true;
         interfacedef:=false;
         hasforward:=false;
         _class := nil;
         import_dll:=nil;
         import_name:=nil;
         import_nr:=0;
         inlininginfo:=nil;
{$ifdef GDB}
         isstabwritten := false;
{$endif GDB}
      end;


    constructor tprocdef.ppuload(ppufile:tcompilerppufile);
      var
        level : byte;
      begin
         inherited ppuload(ppufile);
         deftype:=procdef;

         if po_has_mangledname in procoptions then
          _mangledname:=stringdup(ppufile.getstring)
         else
          _mangledname:=nil;
         extnumber:=ppufile.getword;
         level:=ppufile.getbyte;
         ppufile.getderef(_classderef);
         ppufile.getderef(procsymderef);
         ppufile.getposinfo(fileinfo);
         ppufile.getsmallset(symoptions);
{$ifdef powerpc}
         { library symbol for AmigaOS/MorphOS }
         ppufile.getderef(libsymderef);
{$endif powerpc}
         { import stuff }
         import_dll:=nil;
         import_name:=nil;
         import_nr:=0;
         { inline stuff }
         if (po_has_inlininginfo in procoptions) then
           begin
             ppufile.getderef(funcretsymderef);
             new(inlininginfo);
             ppufile.getsmallset(inlininginfo^.flags);
           end
         else
           begin
             inlininginfo:=nil;
             funcretsym:=nil;
           end;
         { load para symtable }
         parast:=tparasymtable.create(level);
         tparasymtable(parast).ppuload(ppufile);
         parast.defowner:=self;
         { load local symtable }
         if (po_has_inlininginfo in procoptions) or
            ((current_module.flags and uf_local_browser)<>0) then
          begin
            localst:=tlocalsymtable.create(level);
            tlocalsymtable(localst).ppuload(ppufile);
            localst.defowner:=self;
          end
         else
          localst:=nil;
         { inline stuff }
         if (po_has_inlininginfo in procoptions) then
           inlininginfo^.code:=ppuloadnodetree(ppufile);
         { default values for no persistent data }
         if (cs_link_deffile in aktglobalswitches) and
            (tf_need_export in target_info.flags) and
            (po_exports in procoptions) then
           deffile.AddExport(mangledname);
         aliasnames:=tstringlist.create;
         forwarddef:=false;
         interfacedef:=false;
         hasforward:=false;
         lastref:=nil;
         lastwritten:=nil;
         defref:=nil;
         refcount:=0;
{$ifdef GDB}
         isstabwritten := false;
{$endif GDB}
         { Disable po_has_inlining until the derefimpl is done }
         exclude(procoptions,po_has_inlininginfo);
      end;


    destructor tprocdef.destroy;
      begin
         if assigned(defref) then
           begin
             defref.freechain;
             defref.free;
           end;
         aliasnames.free;
         if assigned(localst) and (localst.symtabletype<>staticsymtable) then
          begin
{$ifdef MEMDEBUG}
            memproclocalst.start;
{$endif MEMDEBUG}
            localst.free;
{$ifdef MEMDEBUG}
            memproclocalst.start;
{$endif MEMDEBUG}
          end;
         if assigned(inlininginfo) then
          begin
{$ifdef MEMDEBUG}
            memprocnodetree.start;
{$endif MEMDEBUG}
            tnode(inlininginfo^.code).free;
{$ifdef MEMDEBUG}
            memprocnodetree.start;
{$endif MEMDEBUG}
            dispose(inlininginfo);
          end;
         stringdispose(import_dll);
         stringdispose(import_name);
         if (po_msgstr in procoptions) then
           strdispose(messageinf.str);
         if assigned(_mangledname) then
          begin
{$ifdef MEMDEBUG}
            memmanglednames.start;
{$endif MEMDEBUG}
            stringdispose(_mangledname);
{$ifdef MEMDEBUG}
            memmanglednames.stop;
{$endif MEMDEBUG}
          end;
         inherited destroy;
      end;


    procedure tprocdef.ppuwrite(ppufile:tcompilerppufile);
      var
        oldintfcrc : boolean;
        oldparasymtable,
        oldlocalsymtable : tsymtable;
      begin
         { released procdef? }
         if not assigned(parast) then
           exit;

         oldparasymtable:=aktparasymtable;
         oldlocalsymtable:=aktlocalsymtable;
         aktparasymtable:=parast;
         aktlocalsymtable:=localst;

         inherited ppuwrite(ppufile);
         oldintfcrc:=ppufile.do_interface_crc;
         ppufile.do_interface_crc:=false;
         ppufile.do_interface_crc:=oldintfcrc;
         if po_has_mangledname in procoptions then
          ppufile.putstring(_mangledname^);
         ppufile.putword(extnumber);
         ppufile.putbyte(parast.symtablelevel);
         ppufile.putderef(_classderef);
         ppufile.putderef(procsymderef);
         ppufile.putposinfo(fileinfo);
         ppufile.putsmallset(symoptions);
{$ifdef powerpc}
         { library symbol for AmigaOS/MorphOS }
         ppufile.putderef(libsymderef);
{$endif powerpc}
         { inline stuff }
         oldintfcrc:=ppufile.do_crc;
         ppufile.do_crc:=false;
         if (po_has_inlininginfo in procoptions) then
           begin
             ppufile.putderef(funcretsymderef);
             ppufile.putsmallset(inlininginfo^.flags);
           end;
         ppufile.do_crc:=oldintfcrc;

         { write this entry }
         ppufile.writeentry(ibprocdef);

         { Save the para symtable, this is taken from the interface }
         tparasymtable(parast).ppuwrite(ppufile);

         { save localsymtable for inline procedures or when local
           browser info is requested, this has no influence on the crc }
         if (po_has_inlininginfo in procoptions) or
            ((current_module.flags and uf_local_browser)<>0) then
          begin
            { we must write a localsymtable }
            if not assigned(localst) then
              insert_localst;
            oldintfcrc:=ppufile.do_crc;
            ppufile.do_crc:=false;
            tlocalsymtable(localst).ppuwrite(ppufile);
            ppufile.do_crc:=oldintfcrc;
          end;

         { node tree for inlining }
         oldintfcrc:=ppufile.do_crc;
         ppufile.do_crc:=false;
         if (po_has_inlininginfo in procoptions) then
           ppuwritenodetree(ppufile,inlininginfo^.code);
         ppufile.do_crc:=oldintfcrc;

         aktparasymtable:=oldparasymtable;
         aktlocalsymtable:=oldlocalsymtable;
      end;


    procedure tprocdef.insert_localst;
      begin
         localst:=tlocalsymtable.create(parast.symtablelevel);
         localst.defowner:=self;
         { this is used by insert
           to check same names in parast and localst }
         localst.next:=parast;
      end;


    function tprocdef.fullprocname(showhidden:boolean):string;
      var
        s : string;
        t : ttoken;
      begin
{$ifdef EXTDEBUG}
        showhidden:=true;
{$endif EXTDEBUG}
        s:='';
        if owner.symtabletype=localsymtable then
         s:=s+'local ';
        if assigned(_class) then
         begin
           if po_classmethod in procoptions then
            s:=s+'class ';
           s:=s+_class.objrealname^+'.';
         end;
        if proctypeoption=potype_operator then
          begin
            for t:=NOTOKEN to last_overloaded do
              if procsym.realname='$'+overloaded_names[t] then
                begin
                  s:='operator '+arraytokeninfo[t].str+typename_paras(showhidden);
                  break;
                end;
          end
        else
          s:=s+procsym.realname+typename_paras(showhidden);
        case proctypeoption of
          potype_constructor:
            s:='constructor '+s;
          potype_destructor:
            s:='destructor '+s;
          else
            if assigned(rettype.def) and
              not(is_void(rettype.def)) then
              s:=s+':'+rettype.def.gettypename;
        end;
        { forced calling convention? }
        if (po_hascallingconvention in procoptions) then
          s:=s+';'+ProcCallOptionStr[proccalloption];
        fullprocname:=s;
      end;


    function tprocdef.is_methodpointer:boolean;
      begin
        result:=assigned(_class);
      end;


    function tprocdef.is_addressonly:boolean;
      begin
        result:=assigned(owner) and
                (owner.symtabletype<>objectsymtable);
      end;


    function tprocdef.is_visible_for_object(currobjdef:tobjectdef):boolean;
      begin
        is_visible_for_object:=false;

        { private symbols are allowed when we are in the same
          module as they are defined }
        if (sp_private in symoptions) and
           (owner.defowner.owner.symtabletype in [globalsymtable,staticsymtable]) and
           not(owner.defowner.owner.iscurrentunit) then
          exit;

        if (sp_strictprivate in symoptions) then
          begin
            result:=currobjdef=tobjectdef(owner.defowner);
            exit;
          end;

        if (sp_strictprotected in symoptions) then
          begin
             result:=assigned(currobjdef) and
               currobjdef.is_related(tobjectdef(owner.defowner));
             exit;
          end;

        { protected symbols are visible in the module that defines them and
          also visible to related objects. The related object must be defined
          in the current module }
        if (sp_protected in symoptions) and
           (
            (
             (owner.defowner.owner.symtabletype in [globalsymtable,staticsymtable]) and
             not(owner.defowner.owner.iscurrentunit)
            ) and
            not(
                assigned(currobjdef) and
                (currobjdef.owner.symtabletype in [globalsymtable,staticsymtable]) and
                (currobjdef.owner.iscurrentunit) and
                currobjdef.is_related(tobjectdef(owner.defowner))
               )
           ) then
          exit;

        is_visible_for_object:=true;
      end;


    function tprocdef.getsymtable(t:tgetsymtable):tsymtable;
      begin
        case t of
          gs_local :
            getsymtable:=localst;
          gs_para :
            getsymtable:=parast;
          else
            getsymtable:=nil;
        end;
      end;


    procedure tprocdef.load_references(ppufile:tcompilerppufile;locals:boolean);
      var
        pos : tfileposinfo;
        move_last : boolean;
        oldparasymtable,
        oldlocalsymtable : tsymtable;
      begin
        oldparasymtable:=aktparasymtable;
        oldlocalsymtable:=aktlocalsymtable;
        aktparasymtable:=parast;
        aktlocalsymtable:=localst;

        move_last:=lastwritten=lastref;
        while (not ppufile.endofentry) do
         begin
           ppufile.getposinfo(pos);
           inc(refcount);
           lastref:=tref.create(lastref,@pos);
           lastref.is_written:=true;
           if refcount=1 then
            defref:=lastref;
         end;
        if move_last then
          lastwritten:=lastref;
        if ((current_module.flags and uf_local_browser)<>0) and
           assigned(localst) and
           locals then
          begin
             tparasymtable(parast).load_references(ppufile,locals);
             tlocalsymtable(localst).load_references(ppufile,locals);
          end;

        aktparasymtable:=oldparasymtable;
        aktlocalsymtable:=oldlocalsymtable;
      end;


    Const
      local_symtable_index : word = $8001;

    function tprocdef.write_references(ppufile:tcompilerppufile;locals:boolean):boolean;
      var
        ref : tref;
{$ifdef supportbrowser}
        pdo : tobjectdef;
{$endif supportbrowser}
        move_last : boolean;
        d : tderef;
        oldparasymtable,
        oldlocalsymtable : tsymtable;
      begin
        d.reset;
        move_last:=lastwritten=lastref;
        if move_last and
           (((current_module.flags and uf_local_browser)=0) or
            not locals) then
          exit;
        oldparasymtable:=aktparasymtable;
        oldlocalsymtable:=aktlocalsymtable;
        aktparasymtable:=parast;
        aktlocalsymtable:=localst;
        { write address of this symbol }
        d.build(self);
        ppufile.putderef(d);
        { write refs }
        if assigned(lastwritten) then
          ref:=lastwritten
        else
          ref:=defref;
        while assigned(ref) do
         begin
           if ref.moduleindex=current_module.unit_index then
             begin
                ppufile.putposinfo(ref.posinfo);
                ref.is_written:=true;
                if move_last then
                  lastwritten:=ref;
             end
           else if not ref.is_written then
             move_last:=false
           else if move_last then
             lastwritten:=ref;
           ref:=ref.nextref;
         end;
        ppufile.writeentry(ibdefref);
        write_references:=true;
{$ifdef supportbrowser}
        if ((current_module.flags and uf_local_browser)<>0) and
           assigned(localst) and
           locals then
          begin
             pdo:=_class;
             if (owner.symtabletype<>localsymtable) then
               while assigned(pdo) do
                 begin
                    if pdo.symtable<>aktrecordsymtable then
                      begin
                         pdo.symtable.moduleid:=local_symtable_index;
                         inc(local_symtable_index);
                      end;
                    pdo:=pdo.childof;
                 end;
             parast.moduleid:=local_symtable_index;
             inc(local_symtable_index);
             localst.moduleid:=local_symtable_index;
             inc(local_symtable_index);
             tstoredsymtable(parast).write_references(ppufile,locals);
             tstoredsymtable(localst).write_references(ppufile,locals);
             { decrement for }
             local_symtable_index:=local_symtable_index-2;
             pdo:=_class;
             if (owner.symtabletype<>localsymtable) then
               while assigned(pdo) do
                 begin
                    if pdo.symtable<>aktrecordsymtable then
                      dec(local_symtable_index);
                    pdo:=pdo.childof;
                 end;
          end;
{$endif supportbrowser}
        aktparasymtable:=oldparasymtable;
        aktlocalsymtable:=oldlocalsymtable;
      end;

{$ifdef GDB}
    function tprocdef.numberstring : string;
      begin
        { procdefs are always available }
        stab_state:=stab_state_written;
        result:=inherited numberstring;
      end;


    function tprocdef.stabstring: pchar;
      Var
        RType : Char;
        Obj,Info : String;
        stabsstr : string;
        p : pchar;
      begin
        obj := procsym.name;
        info := '';
        if tprocsym(procsym).is_global then
          RType := 'F'
        else
          RType := 'f';
        if assigned(owner) then
         begin
           if (owner.symtabletype = objectsymtable) then
             obj := owner.name^+'__'+procsym.name;
           if not(cs_gdb_valgrind in aktglobalswitches) and
              (owner.symtabletype=localsymtable) and
              assigned(owner.defowner) and
              assigned(tprocdef(owner.defowner).procsym) then
             info := ','+procsym.name+','+tprocdef(owner.defowner).procsym.name;
         end;
        stabsstr:=mangledname;
        getmem(p,length(stabsstr)+255);
        strpcopy(p,'"'+obj+':'+RType
              +tstoreddef(rettype.def).numberstring+info+'",'+tostr(n_function)
              +',0,'+
              tostr(fileinfo.line)
              +',');
        strpcopy(strend(p),stabsstr);
        stabstring:=strnew(p);
        freemem(p,length(stabsstr)+255);
      end;


    procedure tprocdef.concatstabto(asmlist : taasmoutput);
      begin
        { released procdef? }
        if not assigned(parast) then
          exit;
        if (proccalloption=pocall_internproc) then
          exit;
        { be sure to have a number assigned for this def }
        numberstring;
        { write stabs }
        stab_state:=stab_state_writing;
        asmList.concat(Tai_stabs.Create(stabstring));
        if not(po_external in procoptions) then
          begin
            tparasymtable(parast).concatstabto(asmlist);
            { local type defs and vars should not be written
              inside the main proc stab }
            if assigned(localst) and
               (localst.symtabletype=localsymtable) then
              tlocalsymtable(localst).concatstabto(asmlist);
          end;
        stab_state:=stab_state_written;
      end;
{$endif GDB}


    procedure tprocdef.buildderef;
      var
        oldparasymtable,
        oldlocalsymtable : tsymtable;
      begin
         oldparasymtable:=aktparasymtable;
         oldlocalsymtable:=aktlocalsymtable;
         aktparasymtable:=parast;
         aktlocalsymtable:=localst;

         inherited buildderef;
         _classderef.build(_class);
         { procsym that originaly defined this definition, should be in the
           same symtable }
         procsymderef.build(procsym);
{$ifdef powerpc}
         { library symbol for AmigaOS/MorphOS }
         libsymderef.build(libsym);
{$endif powerpc}

         aktparasymtable:=oldparasymtable;
         aktlocalsymtable:=oldlocalsymtable;
      end;


    procedure tprocdef.buildderefimpl;
      var
        oldparasymtable,
        oldlocalsymtable : tsymtable;
      begin
         { released procdef? }
         if not assigned(parast) then
           exit;

         oldparasymtable:=aktparasymtable;
         oldlocalsymtable:=aktlocalsymtable;
         aktparasymtable:=parast;
         aktlocalsymtable:=localst;

         inherited buildderefimpl;

         { Locals }
         if assigned(localst) and
            ((po_has_inlininginfo in procoptions) or
             ((current_module.flags and uf_local_browser)<>0)) then
           begin
             tlocalsymtable(localst).buildderef;
             tlocalsymtable(localst).buildderefimpl;
           end;

         { inline tree }
         if (po_has_inlininginfo in procoptions) then
           begin
             funcretsymderef.build(funcretsym);
             inlininginfo^.code.buildderefimpl;
           end;

         aktparasymtable:=oldparasymtable;
         aktlocalsymtable:=oldlocalsymtable;
      end;


    procedure tprocdef.deref;
      var
        oldparasymtable,
        oldlocalsymtable : tsymtable;
      begin
         { released procdef? }
         if not assigned(parast) then
           exit;

         oldparasymtable:=aktparasymtable;
         oldlocalsymtable:=aktlocalsymtable;
         aktparasymtable:=parast;
         aktlocalsymtable:=localst;

         inherited deref;
         _class:=tobjectdef(_classderef.resolve);
         { procsym that originaly defined this definition, should be in the
           same symtable }
         procsym:=tprocsym(procsymderef.resolve);
{$ifdef powerpc}
         { library symbol for AmigaOS/MorphOS }
         libsym:=tsym(libsymderef.resolve);
{$endif powerpc}

         aktparasymtable:=oldparasymtable;
         aktlocalsymtable:=oldlocalsymtable;
      end;


    procedure tprocdef.derefimpl;
      var
        oldparasymtable,
        oldlocalsymtable : tsymtable;
      begin
         oldparasymtable:=aktparasymtable;
         oldlocalsymtable:=aktlocalsymtable;
         aktparasymtable:=parast;
         aktlocalsymtable:=localst;

         { Enable has_inlininginfo when the inlininginfo
           structure is available. The has_inlininginfo was disabled
           after the load, since the data was invalid }
         if assigned(inlininginfo) then
           include(procoptions,po_has_inlininginfo);

         { Locals }
         if assigned(localst) then
          begin
            tlocalsymtable(localst).deref;
            tlocalsymtable(localst).derefimpl;
          end;

        { Inline }
        if (po_has_inlininginfo in procoptions) then
          begin
            inlininginfo^.code.derefimpl;
            { funcretsym, this is always located in the localst }
            funcretsym:=tsym(funcretsymderef.resolve);
          end
        else
          begin
            { safety }
            funcretsym:=nil;
          end;

        aktparasymtable:=oldparasymtable;
        aktlocalsymtable:=oldlocalsymtable;
      end;


    function tprocdef.gettypename : string;
      begin
         gettypename := FullProcName(false);
      end;


    function tprocdef.mangledname : string;
      var
        hp   : TParavarsym;
        hs   : string;
        crc  : dword;
        newlen,
        oldlen,
        i    : integer;
      begin
        if assigned(_mangledname) then
         begin
         {$ifdef compress}
           mangledname:=minilzw_decode(_mangledname^);
         {$else}
           mangledname:=_mangledname^;
         {$endif}
           exit;
         end;
        { we need to use the symtable where the procsym is inserted,
          because that is visible to the world }
        mangledname:=make_mangledname('',procsym.owner,procsym.name);
        oldlen:=length(mangledname);
        { add parameter types }
        for i:=0 to paras.count-1 do
         begin
           hp:=tparavarsym(paras[i]);
           if not(vo_is_hidden_para in hp.varoptions) then
             mangledname:=mangledname+'$'+hp.vartype.def.mangledparaname;
         end;
        { add resulttype, add $$ as separator to make it unique from a
          parameter separator }
        if not is_void(rettype.def) then
          mangledname:=mangledname+'$$'+rettype.def.mangledparaname;
        newlen:=length(mangledname);
        { Replace with CRC if the parameter line is very long }
        if (newlen-oldlen>12) and
           ((newlen>128) or (newlen-oldlen>64)) then
          begin
            crc:=$ffffffff;
            for i:=0 to paras.count-1 do
              begin
                hp:=tparavarsym(paras[i]);
                if not(vo_is_hidden_para in hp.varoptions) then
                  begin
                    hs:=hp.vartype.def.mangledparaname;
                    crc:=UpdateCrc32(crc,hs[1],length(hs));
                  end;
              end;
            hs:=hp.vartype.def.mangledparaname;
            crc:=UpdateCrc32(crc,hs[1],length(hs));
            mangledname:=Copy(mangledname,1,oldlen)+'$crc'+hexstr(crc,8);
          end;
       {$ifdef compress}
        _mangledname:=stringdup(minilzw_encode(mangledname));
       {$else}
        _mangledname:=stringdup(mangledname);
       {$endif}
      end;


    function tprocdef.cplusplusmangledname : string;

      function getcppparaname(p : tdef) : string;

        const
           ordtype2str : array[tbasetype] of string[2] = (
             '',
             'Uc','Us','Ui','Us',
             'Sc','s','i','x',
             'b','b','b',
             'c','w','x');

        var
           s : string;

        begin
           case p.deftype of
              orddef:
                s:=ordtype2str[torddef(p).typ];
              pointerdef:
                s:='P'+getcppparaname(tpointerdef(p).pointertype.def);
              else
                internalerror(2103001);
           end;
           getcppparaname:=s;
        end;

      var
         s,s2 : string;
         hp   : TParavarsym;
         i    : integer;

      begin
         s := procsym.realname;
         if procsym.owner.symtabletype=objectsymtable then
           begin
              s2:=upper(tobjectdef(procsym.owner.defowner).typesym.realname);
              case proctypeoption of
                 potype_destructor:
                   s:='_$_'+tostr(length(s2))+s2;
                 potype_constructor:
                   s:='___'+tostr(length(s2))+s2;
                 else
                   s:='_'+s+'__'+tostr(length(s2))+s2;
              end;

           end
         else s:=s+'__';

         s:=s+'F';

         { concat modifiers }
         { !!!!! }

         { now we handle the parameters }
         if maxparacount>0 then
           begin
             for i:=0 to paras.count-1 do
               begin
                 hp:=tparavarsym(paras[i]);
                 s2:=getcppparaname(hp.vartype.def);
                 if hp.varspez in [vs_var,vs_out] then
                   s2:='R'+s2;
                 s:=s+s2;
               end;
           end
         else
           s:=s+'v';
         cplusplusmangledname:=s;
      end;


    procedure tprocdef.setmangledname(const s : string);
      begin
        { This is not allowed anymore, the forward declaration
          already needs to create the correct mangledname, no changes
          afterwards are allowed (PFV) }
        if assigned(_mangledname) then
          internalerror(200411171);
      {$ifdef compress}
        _mangledname:=stringdup(minilzw_encode(s));
      {$else}
        _mangledname:=stringdup(s);
      {$endif}
        include(procoptions,po_has_mangledname);
      end;


{***************************************************************************
                                 TPROCVARDEF
***************************************************************************}

    constructor tprocvardef.create(level:byte);
      begin
         inherited create(level);
         deftype:=procvardef;
      end;


    constructor tprocvardef.ppuload(ppufile:tcompilerppufile);
      begin
         inherited ppuload(ppufile);
         deftype:=procvardef;
         { load para symtable }
         parast:=tparasymtable.create(unknown_level);
         tparasymtable(parast).ppuload(ppufile);
         parast.defowner:=self;
      end;


    function tprocvardef.getcopy : tstoreddef;
      begin
        result:=self;
      (*
          { saves a definition to the return type }
          rettype         : ttype;
          parast          : tsymtable;
          paras           : tparalist;
          proctypeoption  : tproctypeoption;
          proccalloption  : tproccalloption;
          procoptions     : tprocoptions;
          requiredargarea : aint;
          { number of user visibile parameters }
          maxparacount,
          minparacount    : byte;
{$ifdef i386}
          fpu_used        : longint;    { how many stack fpu must be empty }
{$endif i386}
          funcretloc : array[tcallercallee] of TLocation;
          has_paraloc_info : boolean; { paraloc info is available }

       tprocvardef = class(tabstractprocdef)
          constructor create(level:byte);
          constructor ppuload(ppufile:tcompilerppufile);
          function getcopy : tstoreddef;override;
       *)
      end;


    procedure tprocvardef.ppuwrite(ppufile:tcompilerppufile);
      var
        oldparasymtable,
        oldlocalsymtable : tsymtable;
      begin
        oldparasymtable:=aktparasymtable;
        oldlocalsymtable:=aktlocalsymtable;
        aktparasymtable:=parast;
        aktlocalsymtable:=nil;

        { here we cannot get a real good value so just give something }
        { plausible (PM) }
        { a more secure way would be
          to allways store in a temp }
{$ifdef i386}
        if is_fpu(rettype.def) then
          fpu_used:={2}maxfpuregs
        else
          fpu_used:=0;
{$endif i386}
        inherited ppuwrite(ppufile);

        { Write this entry }
        ppufile.writeentry(ibprocvardef);

        { Save the para symtable, this is taken from the interface }
        tparasymtable(parast).ppuwrite(ppufile);

        aktparasymtable:=oldparasymtable;
        aktlocalsymtable:=oldlocalsymtable;
      end;


    procedure tprocvardef.buildderef;
      var
        oldparasymtable,
        oldlocalsymtable : tsymtable;
      begin
        oldparasymtable:=aktparasymtable;
        oldlocalsymtable:=aktlocalsymtable;
        aktparasymtable:=parast;
        aktlocalsymtable:=nil;

        inherited buildderef;

        aktparasymtable:=oldparasymtable;
        aktlocalsymtable:=oldlocalsymtable;
      end;


    procedure tprocvardef.deref;
      var
        oldparasymtable,
        oldlocalsymtable : tsymtable;
      begin
        oldparasymtable:=aktparasymtable;
        oldlocalsymtable:=aktlocalsymtable;
        aktparasymtable:=parast;
        aktlocalsymtable:=nil;

        inherited deref;

        aktparasymtable:=oldparasymtable;
        aktlocalsymtable:=oldlocalsymtable;
      end;


    function tprocvardef.getsymtable(t:tgetsymtable):tsymtable;
      begin
        case t of
          gs_para :
            getsymtable:=parast;
          else
            getsymtable:=nil;
        end;
      end;


    function tprocvardef.size : aint;
      begin
         if (po_methodpointer in procoptions) and
            not(po_addressonly in procoptions) then
           size:=2*sizeof(aint)
         else
           size:=sizeof(aint);
      end;


    function tprocvardef.is_methodpointer:boolean;
      begin
        result:=(po_methodpointer in procoptions);
      end;


    function tprocvardef.is_addressonly:boolean;
      begin
        result:=not(po_methodpointer in procoptions) or
                (po_addressonly in procoptions);
      end;


    function tprocvardef.getmangledparaname:string;
      begin
        result:='procvar';
      end;


{$ifdef GDB}
    function tprocvardef.stabstring : pchar;
      var
         nss : pchar;
        { i   : longint; }
      begin
        { i := maxparacount; }
        getmem(nss,1024);
        { it is not a function but a function pointer !! (PM) }

        strpcopy(nss,'*f'+tstoreddef(rettype.def).numberstring{+','+tostr(i)});
        { this confuses gdb !!
          we should use 'F' instead of 'f' but
          as we use c++ language mode
          it does not like that either
          Please do not remove this part
          might be used once
          gdb for pascal is ready PM }
      {$ifdef disabled}
        param := para1;
        i := 0;
        while assigned(param) do
          begin
            inc(i);
            if param^.paratyp = vs_value then vartyp := '1' else vartyp := '0';
            {Here we have lost the parameter names !!}
            pst := strpnew('p'+tostr(i)+':'+param^.vartype.def.numberstring+','+vartyp+';');
            strcat(nss,pst);
            strdispose(pst);
            param := param^.next;
          end;
      {$endif}
        {strpcopy(strend(nss),';');}
        stabstring := strnew(nss);
        freemem(nss,1024);
      end;


    procedure tprocvardef.concatstabto(asmlist : taasmoutput);
      begin
        if (stab_state in [stab_state_writing,stab_state_written]) then
          exit;
        tstoreddef(rettype.def).concatstabto(asmlist);
        inherited concatstabto(asmlist);
      end;
{$endif GDB}


    procedure tprocvardef.write_rtti_data(rt:trttitype);

         procedure write_para(parasym:tparavarsym);
         var
           paraspec : byte;
         begin
           { only store user visible parameters }
           if not(vo_is_hidden_para in parasym.varoptions) then
             begin
               case parasym.varspez of
                 vs_value: paraspec := 0;
                 vs_const: paraspec := pfConst;
                 vs_var  : paraspec := pfVar;
                 vs_out  : paraspec := pfOut;
               end;
               { write flags for current parameter }
               rttiList.concat(Tai_const.Create_8bit(paraspec));
               { write name of current parameter }
               rttiList.concat(Tai_const.Create_8bit(length(parasym.realname)));
               rttiList.concat(Tai_string.Create(parasym.realname));

               { write name of type of current parameter }
               tstoreddef(parasym.vartype.def).write_rtti_name;
             end;
         end;

       var
         methodkind : byte;
         i : integer;
      begin
        if po_methodpointer in procoptions then
          begin
             { write method id and name }
             rttiList.concat(Tai_const.Create_8bit(tkmethod));
             write_rtti_name;

{$ifdef cpurequiresproperalignment}
             rttilist.concat(Tai_align.Create(sizeof(TConstPtrUInt)));
{$endif cpurequiresproperalignment}
             { write kind of method (can only be function or procedure)}
             if rettype.def = voidtype.def then
               methodkind := mkProcedure
             else
               methodkind := mkFunction;
             rttiList.concat(Tai_const.Create_8bit(methodkind));

             { get # of parameters }
             rttiList.concat(Tai_const.Create_8bit(maxparacount));

             { write parameter info. The parameters must be written in reverse order
               if this method uses right to left parameter pushing! }
             if proccalloption in pushleftright_pocalls then
               begin
                 for i:=0 to paras.count-1 do
                   write_para(tparavarsym(paras[i]));
               end
             else
               begin
                 for i:=paras.count-1 downto 0 do
                   write_para(tparavarsym(paras[i]));
               end;

             { write name of result type }
             tstoreddef(rettype.def).write_rtti_name;
          end;
      end;


    function tprocvardef.is_publishable : boolean;
      begin
         is_publishable:=(po_methodpointer in procoptions);
      end;


    function tprocvardef.gettypename : string;
      var
        s: string;
        showhidden : boolean;
      begin
{$ifdef EXTDEBUG}
         showhidden:=true;
{$else EXTDEBUG}
         showhidden:=false;
{$endif EXTDEBUG}
         s:='<';
         if po_classmethod in procoptions then
           s := s+'class method type of'
         else
           if po_addressonly in procoptions then
             s := s+'address of'
           else
             s := s+'procedure variable type of';
         if po_local in procoptions then
           s := s+' local';
         if assigned(rettype.def) and
            (rettype.def<>voidtype.def) then
           s:=s+' function'+typename_paras(showhidden)+':'+rettype.def.gettypename
         else
           s:=s+' procedure'+typename_paras(showhidden);
         if po_methodpointer in procoptions then
           s := s+' of object';
         gettypename := s+';'+ProcCallOptionStr[proccalloption]+'>';
      end;


{***************************************************************************
                              TOBJECTDEF
***************************************************************************}


   constructor tobjectdef.create(ot : tobjectdeftype;const n : string;c : tobjectdef);
     begin
        inherited create;
        objecttype:=ot;
        deftype:=objectdef;
        objectoptions:=[];
        childof:=nil;
        symtable:=tobjectsymtable.create(n,aktpackrecords);
        { create space for vmt !! }
        vmt_offset:=0;
        symtable.defowner:=self;
        lastvtableindex:=0;
        set_parent(c);
        objname:=stringdup(upper(n));
        objrealname:=stringdup(n);
        if objecttype in [odt_interfacecorba,odt_interfacecom] then
          prepareguid;
        { setup implemented interfaces }
        if objecttype in [odt_class,odt_interfacecorba] then
          implementedinterfaces:=timplementedinterfaces.create
        else
          implementedinterfaces:=nil;

{$ifdef GDB}
        writing_class_record_stab:=false;
{$endif GDB}
     end;


    constructor tobjectdef.ppuload(ppufile:tcompilerppufile);
      var
         i,implintfcount: longint;
         d : tderef;
      begin
         inherited ppuloaddef(ppufile);
         deftype:=objectdef;
         objecttype:=tobjectdeftype(ppufile.getbyte);
         objrealname:=stringdup(ppufile.getstring);
         objname:=stringdup(upper(objrealname^));
         symtable:=tobjectsymtable.create(objrealname^,0);
         tobjectsymtable(symtable).datasize:=ppufile.getaint;
         tobjectsymtable(symtable).fieldalignment:=ppufile.getbyte;
         tobjectsymtable(symtable).recordalignment:=ppufile.getbyte;
         vmt_offset:=ppufile.getlongint;
         ppufile.getderef(childofderef);
         ppufile.getsmallset(objectoptions);

         { load guid }
         iidstr:=nil;
         if objecttype in [odt_interfacecom,odt_interfacecorba] then
           begin
              new(iidguid);
              ppufile.getguid(iidguid^);
              iidstr:=stringdup(ppufile.getstring);
              lastvtableindex:=ppufile.getlongint;
           end;

         { load implemented interfaces }
         if objecttype in [odt_class,odt_interfacecorba] then
           begin
             implementedinterfaces:=timplementedinterfaces.create;
             implintfcount:=ppufile.getlongint;
             for i:=1 to implintfcount do
               begin
                  ppufile.getderef(d);
                  implementedinterfaces.addintf_deref(d,ppufile.getlongint);
               end;
           end
         else
           implementedinterfaces:=nil;

         tobjectsymtable(symtable).ppuload(ppufile);

         symtable.defowner:=self;

         { handles the predefined class tobject  }
         { the last TOBJECT which is loaded gets }
         { it !                                  }
         if (childof=nil) and
            (objecttype=odt_class) and
            (objname^='TOBJECT') then
           class_tobject:=self;
         if (childof=nil) and
            (objecttype=odt_interfacecom) and
            (objname^='IUNKNOWN') then
           interface_iunknown:=self;
{$ifdef GDB}
         writing_class_record_stab:=false;
{$endif GDB}
       end;

    destructor tobjectdef.destroy;
      begin
         if assigned(symtable) then
           symtable.free;
         stringdispose(objname);
         stringdispose(objrealname);
         if assigned(iidstr) then
           stringdispose(iidstr);
         if assigned(implementedinterfaces) then
           implementedinterfaces.free;
         if assigned(iidguid) then
           dispose(iidguid);
         inherited destroy;
      end;


    function tobjectdef.getcopy : tstoreddef;
      begin
        result:=inherited getcopy;
      (*
        result:=tobjectdef.create(objecttype,objname^,childof);
          childofderef  : tderef;
          objname,
          objrealname   : pstring;
          objectoptions : tobjectoptions;
          { to be able to have a variable vmt position }
          { and no vmt field for objects without virtuals }
          vmt_offset : longint;
{$ifdef GDB}
          writing_class_record_stab : boolean;
{$endif GDB}
          objecttype : tobjectdeftype;
          iidguid: pguid;
          iidstr: pstring;
          lastvtableindex: longint;
          { store implemented interfaces defs and name mappings }
          implementedinterfaces: timplementedinterfaces;
      *)
      end;


    procedure tobjectdef.ppuwrite(ppufile:tcompilerppufile);
      var
         implintfcount : longint;
         i : longint;
      begin
         inherited ppuwritedef(ppufile);
         ppufile.putbyte(byte(objecttype));
         ppufile.putstring(objrealname^);
         ppufile.putaint(tobjectsymtable(symtable).datasize);
         ppufile.putbyte(tobjectsymtable(symtable).fieldalignment);
         ppufile.putbyte(tobjectsymtable(symtable).recordalignment);
         ppufile.putlongint(vmt_offset);
         ppufile.putderef(childofderef);
         ppufile.putsmallset(objectoptions);
         if objecttype in [odt_interfacecom,odt_interfacecorba] then
           begin
              ppufile.putguid(iidguid^);
              ppufile.putstring(iidstr^);
              ppufile.putlongint(lastvtableindex);
           end;

         if objecttype in [odt_class,odt_interfacecorba] then
           begin
              implintfcount:=implementedinterfaces.count;
              ppufile.putlongint(implintfcount);
              for i:=1 to implintfcount do
                begin
                   ppufile.putderef(implementedinterfaces.interfacesderef(i));
                   ppufile.putlongint(implementedinterfaces.ioffsets(i));
                end;
           end;

         ppufile.writeentry(ibobjectdef);

         tobjectsymtable(symtable).ppuwrite(ppufile);
      end;


    function tobjectdef.gettypename:string;
      begin
        gettypename:=typename;
      end;


    procedure tobjectdef.buildderef;
      var
         oldrecsyms : tsymtable;
      begin
         inherited buildderef;
         childofderef.build(childof);
         oldrecsyms:=aktrecordsymtable;
         aktrecordsymtable:=symtable;
         tstoredsymtable(symtable).buildderef;
         aktrecordsymtable:=oldrecsyms;
         if objecttype in [odt_class,odt_interfacecorba] then
           implementedinterfaces.buildderef;
      end;


    procedure tobjectdef.deref;
      var
         oldrecsyms : tsymtable;
      begin
         inherited deref;
         childof:=tobjectdef(childofderef.resolve);
         oldrecsyms:=aktrecordsymtable;
         aktrecordsymtable:=symtable;
         tstoredsymtable(symtable).deref;
         aktrecordsymtable:=oldrecsyms;
         if objecttype in [odt_class,odt_interfacecorba] then
           implementedinterfaces.deref;
      end;


    function tobjectdef.getparentdef:tdef;
      begin
        result:=childof;
      end;


    procedure tobjectdef.prepareguid;
      begin
        { set up guid }
        if not assigned(iidguid) then
         begin
            new(iidguid);
            fillchar(iidguid^,sizeof(iidguid^),0); { default null guid }
         end;
        { setup iidstring }
        if not assigned(iidstr) then
          iidstr:=stringdup(''); { default is empty string }
      end;


    procedure tobjectdef.set_parent( c : tobjectdef);
      begin
        { nothing to do if the parent was not forward !}
        if assigned(childof) then
          exit;
        childof:=c;
        { some options are inherited !! }
        if assigned(c) then
          begin
             { only important for classes }
             lastvtableindex:=c.lastvtableindex;
             objectoptions:=objectoptions+(c.objectoptions*
               inherited_objectoptions);
             if not (objecttype in [odt_interfacecom,odt_interfacecorba]) then
               begin
                  { add the data of the anchestor class }
                  inc(tobjectsymtable(symtable).datasize,tobjectsymtable(c.symtable).datasize);
                  if (oo_has_vmt in objectoptions) and
                     (oo_has_vmt in c.objectoptions) then
                    dec(tobjectsymtable(symtable).datasize,sizeof(aint));
                  { if parent has a vmt field then
                    the offset is the same for the child PM }
                  if (oo_has_vmt in c.objectoptions) or is_class(self) then
                    begin
                       vmt_offset:=c.vmt_offset;
                       include(objectoptions,oo_has_vmt);
                    end;
               end;
          end;
      end;


   procedure tobjectdef.insertvmt;
     begin
        if objecttype in [odt_interfacecom,odt_interfacecorba] then
          exit;
        if (oo_has_vmt in objectoptions) then
          internalerror(12345)
        else
          begin
             tobjectsymtable(symtable).datasize:=align(tobjectsymtable(symtable).datasize,
                 tobjectsymtable(symtable).fieldalignment);

{$ifdef cpurequiresproperalignment}
             tobjectsymtable(symtable).datasize:=align(tobjectsymtable(symtable).datasize,sizeof(aint));
{$endif cpurequiresproperalignment}

             vmt_offset:=tobjectsymtable(symtable).datasize;
             inc(tobjectsymtable(symtable).datasize,sizeof(aint));
             include(objectoptions,oo_has_vmt);
          end;
     end;



   procedure tobjectdef.check_forwards;
     begin
        if not(objecttype in [odt_interfacecom,odt_interfacecorba]) then
          tstoredsymtable(symtable).check_forwards;
        if (oo_is_forward in objectoptions) then
          begin
             { ok, in future, the forward can be resolved }
             Message1(sym_e_class_forward_not_resolved,objrealname^);
             exclude(objectoptions,oo_is_forward);
          end;
     end;


   { true, if self inherits from d (or if they are equal) }
   function tobjectdef.is_related(d : tdef) : boolean;
     var
        hp : tobjectdef;
     begin
        hp:=self;
        while assigned(hp) do
          begin
             if hp=d then
               begin
                  is_related:=true;
                  exit;
               end;
             hp:=hp.childof;
          end;
        is_related:=false;
     end;


(*   procedure tobjectdef._searchdestructor(sym : tnamedindexitem;arg:pointer);

     var
        p : pprocdeflist;

     begin
        { if we found already a destructor, then we exit }
        if assigned(sd) then
          exit;
        if tsym(sym).typ=procsym then
          begin
             p:=tprocsym(sym).defs;
             while assigned(p) do
               begin
                  if p^.def.proctypeoption=potype_destructor then
                    begin
                       sd:=p^.def;
                       exit;
                    end;
                  p:=p^.next;
               end;
          end;
     end;*)

    procedure _searchdestructor(sym:Tnamedindexitem;sd:pointer);

    begin
        { if we found already a destructor, then we exit }
        if (ppointer(sd)^=nil) and
           (Tsym(sym).typ=procsym) then
          ppointer(sd)^:=Tprocsym(sym).search_procdef_bytype(potype_destructor);
    end;

   function tobjectdef.searchdestructor : tprocdef;

     var
        o : tobjectdef;
        sd : tprocdef;
     begin
        searchdestructor:=nil;
        o:=self;
        sd:=nil;
        while assigned(o) do
          begin
             o.symtable.foreach_static(@_searchdestructor,@sd);
             if assigned(sd) then
               begin
                  searchdestructor:=sd;
                  exit;
               end;
             o:=o.childof;
          end;
     end;


    function tobjectdef.size : aint;
      begin
        if objecttype in [odt_class,odt_interfacecom,odt_interfacecorba] then
          result:=sizeof(aint)
        else
          result:=tobjectsymtable(symtable).datasize;
      end;


    function tobjectdef.alignment:longint;
      begin
        if objecttype in [odt_class,odt_interfacecom,odt_interfacecorba] then
          alignment:=sizeof(aint)
        else
          alignment:=tobjectsymtable(symtable).recordalignment;
      end;


    function tobjectdef.vmtmethodoffset(index:longint):longint;
      begin
        { for offset of methods for classes, see rtl/inc/objpash.inc }
        case objecttype of
        odt_class:
          { the +2*sizeof(Aint) is size and -size }
          vmtmethodoffset:=(index+10)*sizeof(aint)+2*sizeof(AInt);
        odt_interfacecom,odt_interfacecorba:
          vmtmethodoffset:=index*sizeof(aint);
        else
{$ifdef WITHDMT}
          vmtmethodoffset:=(index+4)*sizeof(aint);
{$else WITHDMT}
          vmtmethodoffset:=(index+3)*sizeof(aint);
{$endif WITHDMT}
        end;
      end;


    function tobjectdef.vmt_mangledname : string;
      begin
        if not(oo_has_vmt in objectoptions) then
          Message1(parser_n_object_has_no_vmt,objrealname^);
        vmt_mangledname:=make_mangledname('VMT',owner,objname^);
      end;


    function tobjectdef.rtti_name : string;
      begin
        rtti_name:=make_mangledname('RTTI',owner,objname^);
      end;


{$ifdef GDB}
    procedure tobjectdef.proc_addname(p :tnamedindexitem;arg:pointer);
      var virtualind,argnames : string;
          newrec : pchar;
          pd     : tprocdef;
          lindex : longint;
          arglength : byte;
          sp : char;
          state:^Trecord_stabgen_state;
          olds:integer;
          i : integer;
          parasym : tparavarsym;
      begin
        state:=arg;
        if tsym(p).typ = procsym then
         begin
           pd := tprocsym(p).first_procdef;
           if (po_virtualmethod in pd.procoptions) then
             begin
               lindex := pd.extnumber;
               {doesnt seem to be necessary
               lindex := lindex or $80000000;}
               virtualind := '*'+tostr(lindex)+';'+pd._class.classnumberstring+';'
             end
            else
             virtualind := '.';

            { used by gdbpas to recognize constructor and destructors }
            if (pd.proctypeoption=potype_constructor) then
              argnames:='__ct__'
            else if (pd.proctypeoption=potype_destructor) then
              argnames:='__dt__'
            else
              argnames := '';

           { arguments are not listed here }
           {we don't need another definition}
            for i:=0 to pd.paras.count-1 do
              begin
                parasym:=tparavarsym(pd.paras[i]);
                if Parasym.vartype.def.deftype = formaldef then
                  begin
                    case Parasym.varspez of
                      vs_var :
                        argnames := argnames+'3var';
                      vs_const :
                        argnames:=argnames+'5const';
                      vs_out :
                        argnames:=argnames+'3out';
                    end;
                  end
                else
                  begin
                    { if the arg definition is like (v: ^byte;..
                    there is no sym attached to data !!! }
                    if assigned(Parasym.vartype.def.typesym) then
                      begin
                        arglength := length(Parasym.vartype.def.typesym.name);
                        argnames := argnames + tostr(arglength)+Parasym.vartype.def.typesym.name;
                      end
                    else
                      argnames:=argnames+'11unnamedtype';
                  end;
              end;
           { here 2A must be changed for private and protected }
           { 0 is private 1 protected and 2 public }
           if ([sp_private,sp_strictprivate]*tsym(p).symoptions)<>[] then
             sp:='0'
           else if ([sp_protected,sp_strictprocted*tsym(p).symoptions)<>[] then
             sp:='1'
           else
             sp:='2';
           newrec:=stabstr_evaluate('$1::$2=##$3;:$4;$5A$6;',[p.name,pd.numberstring,
                                    Tstoreddef(pd.rettype.def).numberstring,argnames,sp,
                                    virtualind]);
           { get spare place for a string at the end }
           olds:=state^.stabsize;
           inc(state^.stabsize,strlen(newrec));
           if state^.stabsize>=state^.staballoc-256 then
             begin
                inc(state^.staballoc,memsizeinc);
                reallocmem(state^.stabstring,state^.staballoc);
             end;
           strcopy(state^.stabstring+olds,newrec);
           strdispose(newrec);
           {This should be used for case !!
           RecOffset := RecOffset + pd.size;}
         end;
      end;


    procedure tobjectdef.proc_concatstabto(p :tnamedindexitem;arg:pointer);
      var
        pd : tprocdef;
      begin
        if tsym(p).typ = procsym then
          begin
            pd := tprocsym(p).first_procdef;
            tstoreddef(pd.rettype.def).concatstabto(taasmoutput(arg));
          end;
      end;


    function tobjectdef.stabstring : pchar;
      var anc : tobjectdef;
          state:Trecord_stabgen_state;
          ts : string;
      begin
        if not (objecttype=odt_class) or writing_class_record_stab then
          begin
            state.staballoc:=memsizeinc;
            getmem(state.stabstring,state.staballoc);
            strpcopy(state.stabstring,'s'+tostr(tobjectsymtable(symtable).datasize));
            if assigned(childof) then
              begin
                {only one ancestor not virtual, public, at base offset 0 }
                {       !1           ,    0       2         0    ,       }
                strpcopy(strend(state.stabstring),'!1,020,'+childof.classnumberstring+';');
              end;
            {virtual table to implement yet}
            state.recoffset:=0;
            state.stabsize:=strlen(state.stabstring);
            symtable.foreach(@field_addname,@state);
            if (oo_has_vmt in objectoptions) then
              if not assigned(childof) or not(oo_has_vmt in childof.objectoptions) then
                 begin
                    ts:='$vf'+classnumberstring+':'+tstoreddef(vmtarraytype.def).numberstring+','+tostr(vmt_offset*8)+';';
                    strpcopy(state.stabstring+state.stabsize,ts);
                    inc(state.stabsize,length(ts));
                 end;
            symtable.foreach(@proc_addname,@state);
            if (oo_has_vmt in objectoptions) then
              begin
                 anc := self;
                 while assigned(anc.childof) and (oo_has_vmt in anc.childof.objectoptions) do
                   anc := anc.childof;
                 { just in case anc = self }
                 ts:=';~%'+anc.classnumberstring+';';
              end
            else
              ts:=';';
            strpcopy(state.stabstring+state.stabsize,ts);
            inc(state.stabsize,length(ts));
            reallocmem(state.stabstring,state.stabsize+1);
            stabstring:=state.stabstring;
          end
        else
          begin
            stabstring:=strpnew('*'+classnumberstring);
          end;
      end;

   procedure tobjectdef.set_globalnb;
     begin
         globalnb:=PglobalTypeCount^;
         inc(PglobalTypeCount^);
         { classes need two type numbers, the globalnb is set to the ptr }
         if objecttype=odt_class then
           begin
             globalnb:=PGlobalTypeCount^;
             inc(PglobalTypeCount^);
           end;
     end;


   function tobjectdef.classnumberstring : string;
     begin
       if objecttype=odt_class then
         begin
           if globalnb=0 then
             numberstring;
           dec(globalnb);
           classnumberstring:=numberstring;
           inc(globalnb);
         end
       else
         classnumberstring:=numberstring;
     end;


    function tobjectdef.allstabstring : pchar;
      var
        stabchar : string[2];
        ss,st : pchar;
        sname : string;
      begin
        ss := stabstring;
        getmem(st,strlen(ss)+512);
        stabchar := 't';
        if deftype in tagtypes then
          stabchar := 'Tt';
        if assigned(typesym) then
          sname := typesym.name
        else
          sname := ' ';
        if writing_class_record_stab then
          strpcopy(st,'"'+sname+':'+stabchar+classnumberstring+'=')
        else
          strpcopy(st,'"'+sname+':'+stabchar+numberstring+'=');
        strpcopy(strecopy(strend(st),ss),'",'+tostr(N_LSYM)+',0,0,0');
        allstabstring := strnew(st);
        freemem(st,strlen(ss)+512);
        strdispose(ss);
      end;


    procedure tobjectdef.concatstabto(asmlist : taasmoutput);
      var
        oldtypesym : tsym;
        stab_str   : pchar;
        anc        : tobjectdef;
      begin
        if (stab_state in [stab_state_writing,stab_state_written]) then
          exit;
        stab_state:=stab_state_writing;
        tstoreddef(vmtarraytype.def).concatstabto(asmlist);
        { first the parents }
        anc:=self;
        while assigned(anc.childof) do
          begin
            anc:=anc.childof;
            anc.concatstabto(asmlist);
          end;
        symtable.foreach(@field_concatstabto,asmlist);
        symtable.foreach(@proc_concatstabto,asmlist);
        stab_state:=stab_state_used;
        if objecttype=odt_class then
          begin
            { Write the record class itself }
            writing_class_record_stab:=true;
            inherited concatstabto(asmlist);
            writing_class_record_stab:=false;
            { Write the invisible pointer class }
            oldtypesym:=typesym;
            typesym:=nil;
            stab_str := allstabstring;
            asmList.concat(Tai_stabs.Create(stab_str));
            typesym:=oldtypesym;
          end
        else
          inherited concatstabto(asmlist);
      end;
{$endif GDB}


    function tobjectdef.needs_inittable : boolean;
      begin
         case objecttype of
            odt_class :
              needs_inittable:=false;
            odt_interfacecom:
              needs_inittable:=true;
            odt_interfacecorba:
              needs_inittable:=is_related(interface_iunknown);
            odt_object:
              needs_inittable:=tobjectsymtable(symtable).needs_init_final;
            else
              internalerror(200108267);
         end;
      end;


    function tobjectdef.members_need_inittable : boolean;
      begin
        members_need_inittable:=tobjectsymtable(symtable).needs_init_final;
      end;


    procedure tobjectdef.count_published_properties(sym:tnamedindexitem;arg:pointer);
      begin
         if needs_prop_entry(tsym(sym)) and
            (tsym(sym).typ<>fieldvarsym) then
           inc(count);
      end;


    procedure tobjectdef.write_property_info(sym : tnamedindexitem;arg:pointer);
      var
         proctypesinfo : byte;

      procedure writeproc(proc : tsymlist; shiftvalue : byte);

        var
           typvalue : byte;
           hp : psymlistitem;
           address : longint;
           def : tdef;
        begin
           if not(assigned(proc) and assigned(proc.firstsym))  then
             begin
                rttiList.concat(Tai_const.create(ait_const_ptr,1));
                typvalue:=3;
             end
           else if proc.firstsym^.sym.typ=fieldvarsym then
             begin
                address:=0;
                hp:=proc.firstsym;
                def:=nil;
                while assigned(hp) do
                  begin
                     case hp^.sltype of
                       sl_load :
                         begin
                           def:=tfieldvarsym(hp^.sym).vartype.def;
                           inc(address,tfieldvarsym(hp^.sym).fieldoffset);
                         end;
                       sl_subscript :
                         begin
                           if not(assigned(def) and (def.deftype=recorddef)) then
                             internalerror(200402171);
                           inc(address,tfieldvarsym(hp^.sym).fieldoffset);
                           def:=tfieldvarsym(hp^.sym).vartype.def;
                         end;
                       sl_vec :
                         begin
                           if not(assigned(def) and (def.deftype=arraydef)) then
                             internalerror(200402172);
                           def:=tarraydef(def).elementtype.def;
                           inc(address,def.size*hp^.value);
                         end;
                     end;
                     hp:=hp^.next;
                  end;
                rttiList.concat(Tai_const.create(ait_const_ptr,address));
                typvalue:=0;
             end
           else
             begin
                { When there was an error then procdef is not assigned }
                if not assigned(proc.procdef) then
                  exit;
                if not(po_virtualmethod in tprocdef(proc.procdef).procoptions) then
                  begin
                     rttiList.concat(Tai_const.createname(tprocdef(proc.procdef).mangledname,AT_FUNCTION,0));
                     typvalue:=1;
                  end
                else
                  begin
                     { virtual method, write vmt offset }
                     rttiList.concat(Tai_const.create(ait_const_ptr,
                       tprocdef(proc.procdef)._class.vmtmethodoffset(tprocdef(proc.procdef).extnumber)));
                     typvalue:=2;
                  end;
             end;
           proctypesinfo:=proctypesinfo or (typvalue shl shiftvalue);
        end;

      begin
         if needs_prop_entry(tsym(sym)) then
           case tsym(sym).typ of
              fieldvarsym:
                begin
{$ifdef dummy}
                   if not(tvarsym(sym).vartype.def.deftype=objectdef) or
                     not(tobjectdef(tvarsym(sym).vartype.def).is_class) then
                     internalerror(1509992);
                   { access to implicit class property as field }
                   proctypesinfo:=(0 shl 0) or (0 shl 2) or (0 shl 4);
                   rttiList.concat(Tai_const_symbol.Createname(tvarsym(sym.vartype.def.get_rtti_label),AT_DATA,0));
                   rttiList.concat(Tai_const.create(ait_const_ptr,tvarsym(sym.address)));
                   rttiList.concat(Tai_const.create(ait_const_ptr,tvarsym(sym.address)));
                   { by default stored }
                   rttiList.concat(Tai_const.Create_32bit(1));
                   { index as well as ... }
                   rttiList.concat(Tai_const.Create_32bit(0));
                   { default value are zero }
                   rttiList.concat(Tai_const.Create_32bit(0));
                   rttiList.concat(Tai_const.Create_16bit(count));
                   inc(count);
                   rttiList.concat(Tai_const.Create_8bit(proctypesinfo));
                   rttiList.concat(Tai_const.Create_8bit(length(tvarsym(sym.realname))));
                   rttiList.concat(Tai_string.Create(tvarsym(sym.realname)));
{$endif dummy}
                end;
              propertysym:
                begin
                   if ppo_indexed in tpropertysym(sym).propoptions then
                     proctypesinfo:=$40
                   else
                     proctypesinfo:=0;
                   rttiList.concat(Tai_const.Create_sym(tstoreddef(tpropertysym(sym).proptype.def).get_rtti_label(fullrtti)));
                   writeproc(tpropertysym(sym).readaccess,0);
                   writeproc(tpropertysym(sym).writeaccess,2);
                   { isn't it stored ? }
                   if not(ppo_stored in tpropertysym(sym).propoptions) then
                     begin
                        rttiList.concat(Tai_const.create_sym(nil));
                        proctypesinfo:=proctypesinfo or (3 shl 4);
                     end
                   else
                     writeproc(tpropertysym(sym).storedaccess,4);
                   rttiList.concat(Tai_const.Create_32bit(tpropertysym(sym).index));
                   rttiList.concat(Tai_const.Create_32bit(tpropertysym(sym).default));
                   rttiList.concat(Tai_const.Create_16bit(count));
                   inc(count);
                   rttiList.concat(Tai_const.Create_8bit(proctypesinfo));
                   rttiList.concat(Tai_const.Create_8bit(length(tpropertysym(sym).realname)));
                   rttiList.concat(Tai_string.Create(tpropertysym(sym).realname));
{$ifdef cpurequiresproperalignment}
                   rttilist.concat(Tai_align.Create(sizeof(TConstPtrUInt)));
{$endif cpurequiresproperalignment}
                end;
              else internalerror(1509992);
           end;
      end;


    procedure tobjectdef.generate_published_child_rtti(sym : tnamedindexitem;arg:pointer);
      begin
         if needs_prop_entry(tsym(sym)) then
          begin
            case tsym(sym).typ of
              propertysym:
                tstoreddef(tpropertysym(sym).proptype.def).get_rtti_label(fullrtti);
              fieldvarsym:
                tstoreddef(tfieldvarsym(sym).vartype.def).get_rtti_label(fullrtti);
              else
                internalerror(1509991);
            end;
          end;
      end;


    procedure tobjectdef.write_child_rtti_data(rt:trttitype);
      begin
         FRTTIType:=rt;
         case rt of
           initrtti :
             symtable.foreach(@generate_field_rtti,nil);
           fullrtti :
             symtable.foreach(@generate_published_child_rtti,nil);
           else
             internalerror(200108301);
         end;
      end;


    type
       tclasslistitem = class(TLinkedListItem)
          index : longint;
          p : tobjectdef;
       end;

    var
       classtablelist : tlinkedlist;
       tablecount : longint;

    function searchclasstablelist(p : tobjectdef) : tclasslistitem;

      var
         hp : tclasslistitem;

      begin
         hp:=tclasslistitem(classtablelist.first);
         while assigned(hp) do
           if hp.p=p then
             begin
                searchclasstablelist:=hp;
                exit;
             end
           else
             hp:=tclasslistitem(hp.next);
         searchclasstablelist:=nil;
      end;


    procedure tobjectdef.count_published_fields(sym:tnamedindexitem;arg:pointer);
      var
         hp : tclasslistitem;
      begin
         if needs_prop_entry(tsym(sym)) and
          (tsym(sym).typ=fieldvarsym) then
          begin
             if tfieldvarsym(sym).vartype.def.deftype<>objectdef then
               internalerror(0206001);
             hp:=searchclasstablelist(tobjectdef(tfieldvarsym(sym).vartype.def));
             if not(assigned(hp)) then
               begin
                  hp:=tclasslistitem.create;
                  hp.p:=tobjectdef(tfieldvarsym(sym).vartype.def);
                  hp.index:=tablecount;
                  classtablelist.concat(hp);
                  inc(tablecount);
               end;
             inc(count);
          end;
      end;


    procedure tobjectdef.writefields(sym:tnamedindexitem;arg:pointer);
      var
         hp : tclasslistitem;
      begin
         if needs_prop_entry(tsym(sym)) and
          (tsym(sym).typ=fieldvarsym) then
          begin
{$ifdef cpurequiresproperalignment}
             rttilist.concat(Tai_align.Create(sizeof(AInt)));
{$endif cpurequiresproperalignment}
             rttiList.concat(Tai_const.Create_aint(tfieldvarsym(sym).fieldoffset));
             hp:=searchclasstablelist(tobjectdef(tfieldvarsym(sym).vartype.def));
             if not(assigned(hp)) then
               internalerror(0206002);
             rttiList.concat(Tai_const.Create_16bit(hp.index));
             rttiList.concat(Tai_const.Create_8bit(length(tfieldvarsym(sym).realname)));
             rttiList.concat(Tai_string.Create(tfieldvarsym(sym).realname));
          end;
      end;


    function tobjectdef.generate_field_table : tasmlabel;
      var
         fieldtable,
         classtable : tasmlabel;
         hp : tclasslistitem;

      begin
         classtablelist:=TLinkedList.Create;
         objectlibrary.getdatalabel(fieldtable);
         objectlibrary.getdatalabel(classtable);
         count:=0;
         tablecount:=0;
         maybe_new_object_file(rttiList);
         new_section(rttiList,sec_rodata,classtable.name,const_align(sizeof(aint)));
         { fields }
         symtable.foreach({$ifdef FPC}@{$endif}count_published_fields,nil);
         rttiList.concat(Tai_label.Create(fieldtable));
         rttiList.concat(Tai_const.Create_16bit(count));
{$ifdef cpurequiresproperalignment}
         rttilist.concat(Tai_align.Create(sizeof(TConstPtrUInt)));
{$endif cpurequiresproperalignment}
         rttiList.concat(Tai_const.Create_sym(classtable));
         symtable.foreach({$ifdef FPC}@{$endif}writefields,nil);

         { generate the class table }
         rttilist.concat(tai_align.create(const_align(sizeof(aint))));
         rttiList.concat(Tai_label.Create(classtable));
         rttiList.concat(Tai_const.Create_16bit(tablecount));
{$ifdef cpurequiresproperalignment}
         rttilist.concat(Tai_align.Create(sizeof(TConstPtrUInt)));
{$endif cpurequiresproperalignment}
         hp:=tclasslistitem(classtablelist.first);
         while assigned(hp) do
           begin
              rttiList.concat(Tai_const.Createname(tobjectdef(hp.p).vmt_mangledname,AT_DATA,0));
              hp:=tclasslistitem(hp.next);
           end;

         generate_field_table:=fieldtable;
         classtablelist.free;
      end;


    function tobjectdef.next_free_name_index : longint;
      var
         i : longint;
      begin
         if assigned(childof) and (oo_can_have_published in childof.objectoptions) then
           i:=childof.next_free_name_index
         else
           i:=0;
         count:=0;
         symtable.foreach(@count_published_properties,nil);
         next_free_name_index:=i+count;
      end;


    procedure tobjectdef.write_rtti_data(rt:trttitype);
      var
        i : longint;
      begin
         case objecttype of
            odt_class:
              rttiList.concat(Tai_const.Create_8bit(tkclass));
            odt_object:
              rttiList.concat(Tai_const.Create_8bit(tkobject));
            odt_interfacecom:
              rttiList.concat(Tai_const.Create_8bit(tkinterface));
            odt_interfacecorba:
              rttiList.concat(Tai_const.Create_8bit(tkinterfaceCorba));
          else
            exit;
          end;

         { generate the name }
         rttiList.concat(Tai_const.Create_8bit(length(objrealname^)));
         rttiList.concat(Tai_string.Create(objrealname^));
{$ifdef cpurequiresproperalignment}
         rttilist.concat(Tai_align.Create(sizeof(TConstPtrUInt)));
{$endif cpurequiresproperalignment}
         case rt of
           initrtti :
             begin
               rttiList.concat(Tai_const.Create_32bit(size));
               if objecttype in [odt_class,odt_object] then
                begin
                  count:=0;
                  FRTTIType:=rt;
                  symtable.foreach(@count_field_rtti,nil);
                  rttiList.concat(Tai_const.Create_32bit(count));
                  symtable.foreach(@write_field_rtti,nil);
                end;
             end;
           fullrtti :
             begin
               if not(objecttype in [odt_interfacecom,odt_interfacecorba]) then
                 begin
                   if (oo_has_vmt in objectoptions) then
                     rttiList.concat(Tai_const.Createname(vmt_mangledname,AT_DATA,0))
                   else
                     rttiList.concat(Tai_const.create_sym(nil));
                 end;

               { write parent typeinfo }
               if assigned(childof) and ((oo_can_have_published in childof.objectoptions) or
                 (objecttype in [odt_interfacecom,odt_interfacecorba])) then
                 rttiList.concat(Tai_const.Create_sym(childof.get_rtti_label(fullrtti)))
               else
                 rttiList.concat(Tai_const.create_sym(nil));

               if objecttype in [odt_object,odt_class] then
                 begin
                   { count total number of properties }
                   if assigned(childof) and (oo_can_have_published in childof.objectoptions) then
                     count:=childof.next_free_name_index
                   else
                     count:=0;

                   { write it }
                   symtable.foreach(@count_published_properties,nil);
                   rttiList.concat(Tai_const.Create_16bit(count));
                 end
               else
                 { interface: write flags, iid and iidstr }
                 begin
                   rttiList.concat(Tai_const.Create_32bit(
                     { ugly, but working }
                     longint([
                       TCompilerIntfFlag(ord(ifHasGuid)*ord(assigned(iidguid))),
                       TCompilerIntfFlag(ord(ifHasStrGUID)*ord(assigned(iidstr)))
                     ])
                     {
                     ifDispInterface,
                     ifDispatch, }
                     ));
{$ifdef cpurequiresproperalignment}
                   rttilist.concat(Tai_align.Create(sizeof(TConstPtrUInt)));
{$endif cpurequiresproperalignment}
                   rttilist.concat(Tai_const.Create_32bit(longint(iidguid^.D1)));
                   rttilist.concat(Tai_const.Create_16bit(iidguid^.D2));
                   rttilist.concat(Tai_const.Create_16bit(iidguid^.D3));
                   for i:=Low(iidguid^.D4) to High(iidguid^.D4) do
                     rttilist.concat(Tai_const.Create_8bit(iidguid^.D4[i]));
                 end;

               { write unit name }
               rttiList.concat(Tai_const.Create_8bit(length(current_module.realmodulename^)));
               rttiList.concat(Tai_string.Create(current_module.realmodulename^));

{$ifdef cpurequiresproperalignment}
               rttilist.concat(Tai_align.Create(sizeof(TConstPtrUInt)));
{$endif cpurequiresproperalignment}

               { write iidstr }
               if objecttype in [odt_interfacecom,odt_interfacecorba] then
                 begin
                   if assigned(iidstr) then
                     begin
                       rttiList.concat(Tai_const.Create_8bit(length(iidstr^)));
                       rttiList.concat(Tai_string.Create(iidstr^));
                     end
                   else
                     rttiList.concat(Tai_const.Create_8bit(0));
{$ifdef cpurequiresproperalignment}
                   rttilist.concat(Tai_align.Create(sizeof(TConstPtrUInt)));
{$endif cpurequiresproperalignment}
                 end;

               if objecttype in [odt_object,odt_class] then
                 begin
                   { write published properties count }
                   count:=0;
                   symtable.foreach(@count_published_properties,nil);
                   rttiList.concat(Tai_const.Create_16bit(count));

{$ifdef cpurequiresproperalignment}
                   rttilist.concat(Tai_align.Create(sizeof(TConstPtrUInt)));
{$endif cpurequiresproperalignment}
                 end;

               { count is used to write nameindex   }

               { but we need an offset of the owner }
               { to give each property an own slot  }
               if assigned(childof) and (oo_can_have_published in childof.objectoptions) then
                 count:=childof.next_free_name_index
               else
                 count:=0;

               symtable.foreach(@write_property_info,nil);
             end;
         end;
      end;


    function tobjectdef.is_publishable : boolean;
      begin
         is_publishable:=objecttype in [odt_class,odt_interfacecom,odt_interfacecorba];
      end;


{****************************************************************************
                             TIMPLEMENTEDINTERFACES
****************************************************************************}
    type
      tnamemap = class(TNamedIndexItem)
        newname: pstring;
        constructor create(const aname, anewname: string);
        destructor  destroy; override;
      end;

    constructor tnamemap.create(const aname, anewname: string);
      begin
        inherited createname(name);
        newname:=stringdup(anewname);
      end;

    destructor  tnamemap.destroy;
      begin
        stringdispose(newname);
        inherited destroy;
      end;


    type
      tprocdefstore = class(TNamedIndexItem)
        procdef: tprocdef;
        constructor create(aprocdef: tprocdef);
      end;

    constructor tprocdefstore.create(aprocdef: tprocdef);
      begin
        inherited create;
        procdef:=aprocdef;
      end;


    constructor timplintfentry.create(aintf: tobjectdef);
      begin
        inherited create;
        intf:=aintf;
        ioffset:=-1;
        namemappings:=nil;
        procdefs:=nil;
      end;


    constructor timplintfentry.create_deref(const d:tderef);
      begin
        inherited create;
        intf:=nil;
        intfderef:=d;
        ioffset:=-1;
        namemappings:=nil;
        procdefs:=nil;
      end;


    destructor  timplintfentry.destroy;
      begin
        if assigned(namemappings) then
          namemappings.free;
        if assigned(procdefs) then
          procdefs.free;
        inherited destroy;
      end;


    constructor timplementedinterfaces.create;
      begin
        finterfaces:=tindexarray.create(1);
      end;

    destructor  timplementedinterfaces.destroy;
      begin
        finterfaces.destroy;
      end;

    function  timplementedinterfaces.count: longint;
      begin
        count:=finterfaces.count;
      end;

    procedure timplementedinterfaces.checkindex(intfindex: longint);
      begin
        if (intfindex<1) or (intfindex>count) then
          InternalError(200006123);
      end;

    function  timplementedinterfaces.interfaces(intfindex: longint): tobjectdef;
      begin
        checkindex(intfindex);
        interfaces:=timplintfentry(finterfaces.search(intfindex)).intf;
      end;

    function  timplementedinterfaces.interfacesderef(intfindex: longint): tderef;
      begin
        checkindex(intfindex);
        interfacesderef:=timplintfentry(finterfaces.search(intfindex)).intfderef;
      end;

    function  timplementedinterfaces.ioffsets(intfindex: longint): longint;
      begin
        checkindex(intfindex);
        ioffsets:=timplintfentry(finterfaces.search(intfindex)).ioffset;
      end;

    procedure timplementedinterfaces.setioffsets(intfindex,iofs:longint);
      begin
        checkindex(intfindex);
        timplintfentry(finterfaces.search(intfindex)).ioffset:=iofs;
      end;

    function timplementedinterfaces.implindex(intfindex:longint):longint;
      begin
        checkindex(intfindex);
        result:=timplintfentry(finterfaces.search(intfindex)).implindex;
      end;

    procedure timplementedinterfaces.setimplindex(intfindex,implidx:longint);
      begin
        checkindex(intfindex);
        timplintfentry(finterfaces.search(intfindex)).implindex:=implidx;
      end;

    function  timplementedinterfaces.searchintf(def: tdef): longint;
      var
        i: longint;
      begin
        i:=1;
        while (i<=count) and (tdef(interfaces(i))<>def) do inc(i);
        if i<=count then
          searchintf:=i
        else
          searchintf:=-1;
      end;


    procedure timplementedinterfaces.buildderef;
      var
        i: longint;
      begin
        for i:=1 to count do
          with timplintfentry(finterfaces.search(i)) do
            intfderef.build(intf);
      end;


    procedure timplementedinterfaces.deref;
      var
        i: longint;
      begin
        for i:=1 to count do
          with timplintfentry(finterfaces.search(i)) do
            intf:=tobjectdef(intfderef.resolve);
      end;

    procedure timplementedinterfaces.addintf_deref(const d:tderef;iofs:longint);
      var
        hintf : timplintfentry;
      begin
        hintf:=timplintfentry.create_deref(d);
        hintf.ioffset:=iofs;
        finterfaces.insert(hintf);
      end;

    procedure timplementedinterfaces.addintf(def: tdef);
      begin
        if not assigned(def) or (searchintf(def)<>-1) or (def.deftype<>objectdef) or
           not (tobjectdef(def).objecttype in [odt_interfacecom,odt_interfacecorba]) then
          internalerror(200006124);
        finterfaces.insert(timplintfentry.create(tobjectdef(def)));
      end;

    procedure timplementedinterfaces.clearmappings;
      var
        i: longint;
      begin
        for i:=1 to count do
          with timplintfentry(finterfaces.search(i)) do
            begin
              if assigned(namemappings) then
                namemappings.free;
              namemappings:=nil;
            end;
      end;

    procedure timplementedinterfaces.addmappings(intfindex: longint; const name, newname: string);
      begin
        checkindex(intfindex);
        with timplintfentry(finterfaces.search(intfindex)) do
          begin
            if not assigned(namemappings) then
              namemappings:=tdictionary.create;
            namemappings.insert(tnamemap.create(name,newname));
          end;
      end;

    function  timplementedinterfaces.getmappings(intfindex: longint; const name: string; var nextexist: pointer): string;
      begin
        checkindex(intfindex);
        if not assigned(nextexist) then
          with timplintfentry(finterfaces.search(intfindex)) do
            begin
              if assigned(namemappings) then
                nextexist:=namemappings.search(name)
              else
                nextexist:=nil;
            end;
        if assigned(nextexist) then
          begin
            getmappings:=tnamemap(nextexist).newname^;
            nextexist:=tnamemap(nextexist).listnext;
          end
        else
          getmappings:='';
      end;

    procedure timplementedinterfaces.addimplproc(intfindex: longint; procdef: tprocdef);
      var
        found : boolean;
        i     : longint;
      begin
        checkindex(intfindex);
        with timplintfentry(finterfaces.search(intfindex)) do
          begin
            if not assigned(procdefs) then
              procdefs:=tindexarray.create(4);
            { No duplicate entries of the same procdef }
            found:=false;
            for i:=1 to procdefs.count do
              if tprocdefstore(procdefs.search(i)).procdef=procdef then
                begin
                  found:=true;
                  break;
                end;
            if not found then
              procdefs.insert(tprocdefstore.create(procdef));
          end;
      end;

    function  timplementedinterfaces.implproccount(intfindex: longint): longint;
      begin
        checkindex(intfindex);
        with timplintfentry(finterfaces.search(intfindex)) do
          if assigned(procdefs) then
            implproccount:=procdefs.count
          else
            implproccount:=0;
      end;

    function  timplementedinterfaces.implprocs(intfindex: longint; procindex: longint): tprocdef;
      begin
        checkindex(intfindex);
        with timplintfentry(finterfaces.search(intfindex)) do
          if assigned(procdefs) then
            implprocs:=tprocdefstore(procdefs.search(procindex)).procdef
          else
            internalerror(200006131);
      end;

    function  timplementedinterfaces.isimplmergepossible(intfindex, remainindex: longint; var weight: longint): boolean;
      var
        possible: boolean;
        i: longint;
        iiep1: TIndexArray;
        iiep2: TIndexArray;
      begin
        checkindex(intfindex);
        checkindex(remainindex);
        iiep1:=timplintfentry(finterfaces.search(intfindex)).procdefs;
        iiep2:=timplintfentry(finterfaces.search(remainindex)).procdefs;
        if not assigned(iiep1) then { empty interface is mergeable :-) }
          begin
            possible:=true;
            weight:=0;
          end
        else
          begin
            possible:=assigned(iiep2) and (iiep1.count<=iiep2.count);
            i:=1;
            while (possible) and (i<=iiep1.count) do
              begin
                possible:=
                  (tprocdefstore(iiep1.search(i)).procdef=tprocdefstore(iiep2.search(i)).procdef);
                inc(i);
              end;
            if possible then
              weight:=iiep1.count;
          end;
        isimplmergepossible:=possible;
      end;


{****************************************************************************
                                TFORWARDDEF
****************************************************************************}

   constructor tforwarddef.create(const s:string;const pos : tfileposinfo);
     var
       oldregisterdef : boolean;
     begin
        { never register the forwarddefs, they are disposed at the
          end of the type declaration block }
        oldregisterdef:=registerdef;
        registerdef:=false;
        inherited create;
        registerdef:=oldregisterdef;
        deftype:=forwarddef;
        tosymname:=stringdup(s);
        forwardpos:=pos;
     end;


    function tforwarddef.gettypename:string;
      begin
        gettypename:='unresolved forward to '+tosymname^;
      end;

     destructor tforwarddef.destroy;
      begin
        if assigned(tosymname) then
          stringdispose(tosymname);
        inherited destroy;
      end;


{****************************************************************************
                                  TERRORDEF
****************************************************************************}

   constructor terrordef.create;
     begin
        inherited create;
        deftype:=errordef;
     end;


    procedure terrordef.ppuwrite(ppufile:tcompilerppufile);
      begin
        { Can't write errordefs to ppu }
        internalerror(200411063);
      end;


{$ifdef GDB}
    function terrordef.stabstring : pchar;
      begin
         stabstring:=strpnew('error'+numberstring);
      end;

    procedure terrordef.concatstabto(asmlist : taasmoutput);
      begin
        { No internal error needed, an normal error is already
          thrown }
      end;
{$endif GDB}

    function terrordef.gettypename:string;

      begin
         gettypename:='<erroneous type>';
      end;

    function terrordef.getmangledparaname:string;

      begin
         getmangledparaname:='error';
      end;


{****************************************************************************
                           Definition Helpers
****************************************************************************}

    function is_interfacecom(def: tdef): boolean;
      begin
        is_interfacecom:=
          assigned(def) and
          (def.deftype=objectdef) and
          (tobjectdef(def).objecttype=odt_interfacecom);
      end;

    function is_interfacecorba(def: tdef): boolean;
      begin
        is_interfacecorba:=
          assigned(def) and
          (def.deftype=objectdef) and
          (tobjectdef(def).objecttype=odt_interfacecorba);
      end;

    function is_interface(def: tdef): boolean;
      begin
        is_interface:=
          assigned(def) and
          (def.deftype=objectdef) and
          (tobjectdef(def).objecttype in [odt_interfacecom,odt_interfacecorba]);
      end;


    function is_class(def: tdef): boolean;
      begin
        is_class:=
          assigned(def) and
          (def.deftype=objectdef) and
          (tobjectdef(def).objecttype=odt_class);
      end;

    function is_object(def: tdef): boolean;
      begin
        is_object:=
          assigned(def) and
          (def.deftype=objectdef) and
          (tobjectdef(def).objecttype=odt_object);
      end;

    function is_cppclass(def: tdef): boolean;
      begin
        is_cppclass:=
          assigned(def) and
          (def.deftype=objectdef) and
          (tobjectdef(def).objecttype=odt_cppclass);
      end;

    function is_class_or_interface(def: tdef): boolean;
      begin
        is_class_or_interface:=
          assigned(def) and
          (def.deftype=objectdef) and
          (tobjectdef(def).objecttype in [odt_class,odt_interfacecom,odt_interfacecorba]);
      end;


{$ifdef x86}
    function use_sse(def : tdef) : boolean;
      begin
        use_sse:=(is_single(def) and (aktfputype in sse_singlescalar)) or
          (is_double(def) and (aktfputype in sse_doublescalar));
      end;
{$endif x86}

end.
