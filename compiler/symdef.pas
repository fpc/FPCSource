{
    $Id$
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
       symppu,ppu,
       { node }
       node,
       { aasm }
       aasmbase,aasmtai,cpubase,cpuinfo
{$ifdef Delphi}
       ,dmisc
{$endif}
       ;


    type
{************************************************
                    TDef
************************************************}

       tstoreddef = class(tdef)
          { persistent (available across units) rtti and init tables }
          rttitablesym,
          inittablesym  : tsym; {trttisym}
          { local (per module) rtti and init tables }
          localrttilab  : array[trttitype] of tasmlabel;
          { linked list of global definitions }
          nextglobal,
          previousglobal : tstoreddef;
{$ifdef EXTDEBUG}
          fileinfo   : tfileposinfo;
{$endif}
{$ifdef GDB}
          globalnb       : word;
          is_def_stab_written : tdefstabstatus;
{$endif GDB}
          constructor create;
          constructor ppuloaddef(ppufile:tcompilerppufile);
          destructor  destroy;override;
          function getcopy : tstoreddef;virtual;
          procedure ppuwritedef(ppufile:tcompilerppufile);
          procedure ppuwrite(ppufile:tcompilerppufile);virtual;abstract;
          procedure deref;override;
          procedure derefimpl;override;
          function  size:longint;override;
          function  alignment:longint;override;
          function  is_publishable : boolean;override;
          function  needs_inittable : boolean;override;
          { debug }
{$ifdef GDB}
          function  stabstring : pchar;virtual;
          procedure concatstabto(asmlist : taasmoutput);virtual;
          function  NumberString:string;
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
          savesize  : longint;
       end;

       tparaitem = class(TLinkedListItem)
          paratype     : ttype; { required for procvar }
          parasym      : tsym;
          defaultvalue : tsym; { tconstsym }
          paratyp      : tvarspez; { required for procvar }
          paraloc      : tparalocation;
{$ifdef EXTDEBUG}
          eqval        : tequaltype;
{$endif EXTDEBUG}
       end;

       tfiletyp = (ft_text,ft_typed,ft_untyped);

       tfiledef = class(tstoreddef)
          filetyp : tfiletyp;
          typedfiletype : ttype;
          constructor createtext;
          constructor createuntyped;
          constructor createtyped(const tt : ttype);
          constructor ppuload(ppufile:tcompilerppufile);
          procedure ppuwrite(ppufile:tcompilerppufile);override;
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
          constructor create;
          constructor ppuload(ppufile:tcompilerppufile);
          function gettypename:string;override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          procedure setsize;
          function needs_inittable : boolean;override;
          procedure write_rtti_data(rt:trttitype);override;
{$ifdef GDB}
          procedure concatstabto(asmlist : taasmoutput);override;
{$endif GDB}
       end;

       tformaldef = class(tstoreddef)
          constructor create;
          constructor ppuload(ppufile:tcompilerppufile);
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          function  gettypename:string;override;
{$ifdef GDB}
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
          constructor ppuload(ppufile:tcompilerppufile);
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          procedure deref;override;
          function  gettypename:string;override;
          { debug }
{$ifdef GDB}
          function  stabstring : pchar;override;
          procedure concatstabto(asmlist : taasmoutput);override;
{$endif GDB}
       end;

       tabstractrecorddef = class(tstoreddef)
       private
          Count         : integer;
          FRTTIType     : trttitype;
{$ifdef GDB}
          StabRecString : pchar;
          StabRecSize   : Integer;
          RecOffset     : Integer;
          procedure addname(p : tnamedindexitem;arg:pointer);
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
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          procedure deref;override;
          function  size:longint;override;
          function  alignment : longint;override;
          function  gettypename:string;override;
          { debug }
{$ifdef GDB}
          function  stabstring : pchar;override;
          procedure concatstabto(asmlist : taasmoutput);override;
{$endif GDB}
          function  needs_inittable : boolean;override;
          { rtti }
          procedure write_child_rtti_data(rt:trttitype);override;
          procedure write_rtti_data(rt:trttitype);override;
       end;

       tprocdef = class;

       timplementedinterfaces = class;

       tobjectdef = class(tabstractrecorddef)
       private
          sd : tprocdef;
          procedure _searchdestructor(sym : tnamedindexitem;arg:pointer);
{$ifdef GDB}
          procedure addprocname(p :tnamedindexitem;arg:pointer);
{$endif GDB}
          procedure count_published_properties(sym:tnamedindexitem;arg:pointer);
          procedure write_property_info(sym : tnamedindexitem;arg:pointer);
          procedure generate_published_child_rtti(sym : tnamedindexitem;arg:pointer);
          procedure count_published_fields(sym:tnamedindexitem;arg:pointer);
          procedure writefields(sym:tnamedindexitem;arg:pointer);
       public
          childof  : tobjectdef;
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
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          procedure deref;override;
          function  size : longint;override;
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
          function  is_related(d : tobjectdef) : boolean;
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
          function  ioffsets(intfindex: longint): plongint;
          function  searchintf(def: tdef): longint;
          procedure addintf(def: tdef);

          procedure deref;
          { add interface reference loaded from ppu }
          procedure addintfref(def: pointer);

          procedure clearmappings;
          procedure addmappings(intfindex: longint; const name, newname: string);
          function  getmappings(intfindex: longint; const name: string; var nextexist: pointer): string;

          procedure clearimplprocs;
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
          { debug }
{$ifdef GDB}
          function stabstring : pchar;override;
          procedure concatstabto(asmlist : taasmoutput);override;
{$endif GDB}
       end;

       tarraydef = class(tstoreddef)
          lowrange,
          highrange  : longint;
          rangetype  : ttype;
          IsDynamicArray,
          IsVariant,
          IsConstructor,
          IsArrayOfConst : boolean;
       protected
          _elementtype : ttype;
       public
          function elesize : longint;
          constructor create(l,h : longint;const t : ttype);
          constructor ppuload(ppufile:tcompilerppufile);
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          function  gettypename:string;override;
          function  getmangledparaname : string;override;
          procedure setelementtype(t: ttype);
{$ifdef GDB}
          function stabstring : pchar;override;
          procedure concatstabto(asmlist : taasmoutput);override;
{$endif GDB}
          procedure deref;override;
          function size : longint;override;
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
{$endif GDB}
          { rtti }
          procedure write_rtti_data(rt:trttitype);override;
       end;

       tabstractprocdef = class(tstoreddef)
          { saves a definition to the return type }
          rettype         : ttype;
          parast          : tsymtable;
          para            : tlinkedlist;
          selfpara        : tparaitem;
          proctypeoption  : tproctypeoption;
          proccalloption  : tproccalloption;
          procoptions     : tprocoptions;
          maxparacount,
          minparacount    : byte;
          fpu_used        : byte;    { how many stack fpu must be empty }
          constructor create(level:byte);
          constructor ppuload(ppufile:tcompilerppufile);
          destructor destroy;override;
          procedure  ppuwrite(ppufile:tcompilerppufile);override;
          procedure deref;override;
          procedure releasemem;
          function  concatpara(afterpara:tparaitem;const tt:ttype;sym : tsym;vsp : tvarspez;defval:tsym):tparaitem;
          function  insertpara(const tt:ttype;sym : tsym;vsp : tvarspez;defval:tsym):tparaitem;
          procedure removepara(currpara:tparaitem);
          function  para_size(alignsize:longint) : longint;
          function  typename_paras(showhidden:boolean): string;
          procedure test_if_fpu_result;
          function  is_methodpointer:boolean;virtual;
          function  is_addressonly:boolean;virtual;
          { debug }
{$ifdef GDB}
          function  stabstring : pchar;override;
          procedure concatstabto(asmlist : taasmoutput);override;
{$endif GDB}
       end;

       tprocvardef = class(tabstractprocdef)
          constructor create(level:byte);
          constructor ppuload(ppufile:tcompilerppufile);
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          function  getsymtable(t:tgetsymtable):tsymtable;override;
          function  size : longint;override;
          function  gettypename:string;override;
          function  is_publishable : boolean;override;
          function  is_methodpointer:boolean;override;
          function  is_addressonly:boolean;override;
          { debug }
{$ifdef GDB}
          function stabstring : pchar;override;
          procedure concatstabto(asmlist : taasmoutput); override;
{$endif GDB}
          { rtti }
          procedure write_rtti_data(rt:trttitype);override;
       end;

       tmessageinf = record
         case integer of
           0 : (str : pchar);
           1 : (i : longint);
       end;

       tprocdef = class(tabstractprocdef)
       private
          _mangledname : pstring;
{$ifdef GDB}
          isstabwritten : boolean;
{$endif GDB}
       public
          extnumber      : word;
          overloadnumber : word;
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
          { alias names }
          aliasnames : tstringlist;
          { symtables }
          localst : tsymtable;
          funcretsym : tsym;
          { browser info }
          lastref,
          defref,
          lastwritten : tref;
          refcount : longint;
          _class : tobjectdef;
          { it's a tree, but this not easy to handle }
          { used for inlined procs                   }
          code : tnode;
          { info about register variables (JM) }
          regvarinfo: pointer;
          { name of the result variable to insert in the localsymtable }
          resultname : stringid;
          { true, if the procedure is only declared }
          { (forward procedure) }
          forwarddef,
          { true if the procedure is declared in the interface }
          interfacedef : boolean;
          { true if the procedure has a forward declaration }
          hasforward : boolean;
          { check the problems of manglednames }
          has_mangledname : boolean;
          { small set which contains the modified registers }
          usedintregisters:Tsupregset;
          usedotherregisters:Tregisterset;
          constructor create(level:byte);
          constructor ppuload(ppufile:tcompilerppufile);
          destructor  destroy;override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
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
          function  is_visible_for_proc(currprocdef:tprocdef):boolean;
          function  is_visible_for_object(currobjdef:tobjectdef):boolean;
          { debug }
{$ifdef GDB}
          function  stabstring : pchar;override;
          procedure concatstabto(asmlist : taasmoutput);override;
{$endif GDB}
       end;

       { single linked list of overloaded procs }
       pprocdeflist = ^tprocdeflist;
       tprocdeflist = record
         def  : tprocdef;
         next : pprocdeflist;
       end;

       tstringdef = class(tstoreddef)
          string_typ : tstringtype;
          len        : longint;
          constructor createshort(l : byte);
          constructor loadshort(ppufile:tcompilerppufile);
          constructor createlong(l : longint);
          constructor loadlong(ppufile:tcompilerppufile);
          constructor createansi(l : longint);
          constructor loadansi(ppufile:tcompilerppufile);
          constructor createwide(l : longint);
          constructor loadwide(ppufile:tcompilerppufile);
          function getcopy : tstoreddef;override;
          function  stringtypname:string;
          function  size : longint;override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          function  gettypename:string;override;
          function  getmangledparaname:string;override;
          function  is_publishable : boolean;override;
          { debug }
{$ifdef GDB}
          function  stabstring : pchar;override;
          procedure concatstabto(asmlist : taasmoutput);override;
{$endif GDB}
          { init/final }
          function  needs_inittable : boolean;override;
          { rtti }
          procedure write_rtti_data(rt:trttitype);override;
       end;

       tenumdef = class(tstoreddef)
          minval,
          maxval    : longint;
          has_jumps : boolean;
          firstenum : tsym;  {tenumsym}
          basedef   : tenumdef;
          constructor create;
          constructor create_subrange(_basedef:tenumdef;_min,_max:longint);
          constructor ppuload(ppufile:tcompilerppufile);
          destructor destroy;override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          procedure deref;override;
          function  gettypename:string;override;
          function  is_publishable : boolean;override;
          procedure calcsavesize;
          procedure setmax(_max:longint);
          procedure setmin(_min:longint);
          function  min:longint;
          function  max:longint;
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
          constructor create(const t:ttype;high : longint);
          constructor ppuload(ppufile:tcompilerppufile);
          destructor  destroy;override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          procedure deref;override;
          function  gettypename:string;override;
          function  is_publishable : boolean;override;
          procedure changesettype(s:tsettype);
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

       firstglobaldef,               { linked list of all globals defs }
       lastglobaldef : tstoreddef;   { used to reset stabs/ranges }

{$ifdef GDB}
       { for STAB debugging }
       globaltypecount  : word;
       pglobaltypecount : pword;
{$endif GDB}

    { default types }
       generrortype,              { error in definition }
       voidpointertype,           { pointer for Void-Pointerdef }
       charpointertype,           { pointer for Char-Pointerdef }
       voidfarpointertype,
       cformaltype,               { unique formal definition }
       voidtype,                  { Pointer to Void (procedure) }
       cchartype,                 { Pointer to Char }
       cwidechartype,             { Pointer to WideChar }
       booltype,                  { pointer to boolean type }
       u8bittype,                 { Pointer to 8-Bit unsigned }
       u16bittype,                { Pointer to 16-Bit unsigned }
       u32bittype,                { Pointer to 32-Bit unsigned }
       s32bittype,                { Pointer to 32-Bit signed }
       cu64bittype,               { pointer to 64 bit unsigned def }
       cs64bittype,               { pointer to 64 bit signed def, }
       s32floattype,              { pointer for realconstn }
       s64floattype,              { pointer for realconstn }
       s80floattype,              { pointer to type of temp. floats }
       s64currencytype,           { pointer to a currency type }
       s32fixedtype,              { pointer to type of temp. fixed }
       cshortstringtype,          { pointer to type of short string const   }
       clongstringtype,           { pointer to type of long string const   }
       cansistringtype,           { pointer to type of ansi string const  }
       cwidestringtype,           { pointer to type of wide string const  }
       openshortstringtype,       { pointer to type of an open shortstring,
                                    needed for readln() }
       openchararraytype,         { pointer to type of an open array of char,
                                    needed for readln() }
       cfiletype,                 { get the same definition for all file }
                                  { used for stabs }
       { we use only one variant def }
       cvarianttype,
       { unsigned ord type with the same size as a pointer }
       ordpointertype,
       defaultordconsttype,       { pointer to type of ordinal constants }
       pvmttype      : ttype;     { type of classrefs, used for stabs }

       { pointer to the anchestor of all classes }
       class_tobject : tobjectdef;
       { pointer to the ancestor of all COM interfaces }
       interface_iunknown : tobjectdef;
       { pointer to the TGUID type
         of all interfaces         }
       rec_tguid : trecorddef;

       { Pointer to a procdef with no parameters and no return value.
         This is used for procedures which are generated automatically
         by the compiler.
       }
       voidprocdef : tprocdef;

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

    function mangledname_prefix(typeprefix:string;st:tsymtable):string;

{$ifdef GDB}
    { GDB Helpers }
    function typeglobalnumber(const s : string) : string;
{$endif GDB}

    { should be in the types unit, but the types unit uses the node stuff :( }
    function is_interfacecom(def: tdef): boolean;
    function is_interfacecorba(def: tdef): boolean;
    function is_interface(def: tdef): boolean;
    function is_object(def: tdef): boolean;
    function is_class(def: tdef): boolean;
    function is_cppclass(def: tdef): boolean;
    function is_class_or_interface(def: tdef): boolean;

    procedure reset_global_defs;


implementation

    uses
{$ifdef Delphi}
       sysutils,
{$else Delphi}
       strings,
{$endif Delphi}
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
       gendef
       ;


{****************************************************************************
                                  Helpers
****************************************************************************}

    function mangledname_prefix(typeprefix:string;st:tsymtable):string;
      var
        s,
        prefix : string;
      begin
        prefix:='';
        if not assigned(st) then
         internalerror(200204212);
        { sub procedures }
        while (st.symtabletype=localsymtable) do
         begin
           if st.defowner.deftype<>procdef then
            internalerror(200204173);
           s:=tprocdef(st.defowner).procsym.name;
           if tprocdef(st.defowner).overloadnumber>0 then
            s:=s+'$'+tostr(tprocdef(st.defowner).overloadnumber);
           prefix:=s+'$'+prefix;
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
        mangledname_prefix:=typeprefix+'_'+st.name^+'_'+prefix;
      end;


{$ifdef GDB}
    procedure forcestabto(asmlist : taasmoutput; pd : tdef);
      begin
        if tstoreddef(pd).is_def_stab_written = not_written then
         begin
           if assigned(pd.typesym) then
            ttypesym(pd.typesym).isusedinstab := true;
           tstoreddef(pd).concatstabto(asmlist);
         end;
      end;
{$endif GDB}


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
         is_def_stab_written := not_written;
         globalnb := 0;
{$endif GDB}
         if assigned(lastglobaldef) then
           begin
              lastglobaldef.nextglobal := self;
              previousglobal:=lastglobaldef;
           end
         else
           begin
              firstglobaldef := self;
              previousglobal := nil;
           end;
         lastglobaldef := self;
         nextglobal := nil;
         fillchar(localrttilab,sizeof(localrttilab),0);
      end;


    constructor tstoreddef.ppuloaddef(ppufile:tcompilerppufile);
      begin
         inherited create;
{$ifdef EXTDEBUG}
         fillchar(fileinfo,sizeof(fileinfo),0);
{$endif}
{$ifdef GDB}
         is_def_stab_written := not_written;
         globalnb := 0;
{$endif GDB}
         if assigned(lastglobaldef) then
           begin
              lastglobaldef.nextglobal := self;
              previousglobal:=lastglobaldef;
           end
         else
           begin
              firstglobaldef := self;
              previousglobal:=nil;
           end;
         lastglobaldef := self;
         nextglobal := nil;
         fillchar(localrttilab,sizeof(localrttilab),0);
      { load }
         indexnr:=ppufile.getword;
         typesym:=ttypesym(pointer(ppufile.getderef));
         ppufile.getsmallset(defoptions);
         if df_has_rttitable in defoptions then
          rttitablesym:=tsym(ppufile.getderef);
         if df_has_inittable in defoptions then
          inittablesym:=tsym(ppufile.getderef);
      end;


    destructor tstoreddef.destroy;
      begin
         { first element  ? }
         if not(assigned(previousglobal)) then
           begin
              firstglobaldef := nextglobal;
              if assigned(firstglobaldef) then
                firstglobaldef.previousglobal:=nil;
           end
         else
           begin
              { remove reference in the element before }
              previousglobal.nextglobal:=nextglobal;
           end;
         { last element ? }
         if not(assigned(nextglobal)) then
           begin
              lastglobaldef := previousglobal;
              if assigned(lastglobaldef) then
                lastglobaldef.nextglobal:=nil;
           end
         else
           nextglobal.previousglobal:=previousglobal;
         previousglobal:=nil;
         nextglobal:=nil;
      end;

    function tstoreddef.getcopy : tstoreddef;
      begin
         Message(sym_e_cant_create_unique_type);
         getcopy:=nil;
      end;

    procedure tstoreddef.ppuwritedef(ppufile:tcompilerppufile);
      begin
        ppufile.putword(indexnr);
        ppufile.putderef(typesym);
        ppufile.putsmallset(defoptions);
        if df_has_rttitable in defoptions then
         ppufile.putderef(rttitablesym);
        if df_has_inittable in defoptions then
         ppufile.putderef(inittablesym);
{$ifdef GDB}
        if globalnb = 0 then
          begin
            if assigned(owner) then
              globalnb := owner.getnewtypecount
            else
              begin
                globalnb := PGlobalTypeCount^;
                Inc(PGlobalTypeCount^);
              end;
           end;
{$endif GDB}
      end;


    procedure tstoreddef.deref;
      begin
        resolvesym(pointer(typesym));
        resolvesym(pointer(rttitablesym));
        resolvesym(pointer(inittablesym));
      end;


    procedure tstoreddef.derefimpl;
      begin
      end;


    function tstoreddef.size : longint;
      begin
         size:=savesize;
      end;


    function tstoreddef.alignment : longint;
      begin
         { normal alignment by default }
         alignment:=0;
      end;


{$ifdef GDB}
   procedure tstoreddef.set_globalnb;
     begin
         globalnb :=PGlobalTypeCount^;
         inc(PglobalTypeCount^);
     end;

    function tstoreddef.stabstring : pchar;
      begin
      stabstring := strpnew('t'+numberstring+';');
      end;


    function tstoreddef.numberstring : string;
      var table : tsymtable;
      begin
      {formal def have no type !}
      if deftype = formaldef then
        begin
        numberstring := tstoreddef(voidtype.def).numberstring;
        exit;
        end;
      if (not assigned(typesym)) or (not ttypesym(typesym).isusedinstab) then
        begin
           {set even if debuglist is not defined}
           if assigned(typesym) then
             ttypesym(typesym).isusedinstab := true;
           if assigned(debuglist) and (is_def_stab_written = not_written) then
             concatstabto(debuglist);
        end;
      if not (cs_gdb_dbx in aktglobalswitches) then
        begin
           if globalnb = 0 then
             set_globalnb;
           numberstring := tostr(globalnb);
        end
      else
        begin
           if globalnb = 0 then
             begin
                if assigned(owner) then
                  globalnb := owner.getnewtypecount
                else
                  begin
                     globalnb := PGlobalTypeCount^;
                     Inc(PGlobalTypeCount^);
                  end;
             end;
           if assigned(typesym) then
             begin
                table := ttypesym(typesym).owner;
                if table.unitid > 0 then
                  numberstring := '('+tostr(table.unitid)+','+tostr(tstoreddef(ttypesym(typesym).restype.def).globalnb)+')'
                else
                  numberstring := tostr(globalnb);
                exit;
             end;
           numberstring := tostr(globalnb);
        end;
      end;


    function tstoreddef.allstabstring : pchar;
    var stabchar : string[2];
        ss,st : pchar;
        sname : string;
        sym_line_no : longint;
      begin
      ss := stabstring;
      getmem(st,strlen(ss)+512);
      stabchar := 't';
      if deftype in tagtypes then
        stabchar := 'Tt';
      if assigned(typesym) then
        begin
           sname := ttypesym(typesym).name;
           sym_line_no:=ttypesym(typesym).fileinfo.line;
        end
      else
        begin
           sname := ' ';
           sym_line_no:=0;
        end;
      strpcopy(st,'"'+sname+':'+stabchar+numberstring+'=');
      strpcopy(strecopy(strend(st),ss),'",'+tostr(N_LSYM)+',0,'+tostr(sym_line_no)+',0');
      allstabstring := strnew(st);
      freemem(st,strlen(ss)+512);
      strdispose(ss);
      end;


    procedure tstoreddef.concatstabto(asmlist : taasmoutput);
     var stab_str : pchar;
    begin
    if ((typesym = nil) or ttypesym(typesym).isusedinstab or (cs_gdb_dbx in aktglobalswitches))
      and (is_def_stab_written = not_written) then
      begin
      If cs_gdb_dbx in aktglobalswitches then
        begin
           { otherwise you get two of each def }
           If assigned(typesym) then
             begin
                if ttypesym(typesym).typ=symconst.typesym then
                  ttypesym(typesym).isusedinstab:=true;
                if (ttypesym(typesym).owner = nil) or
                   ((ttypesym(typesym).owner.symtabletype = globalsymtable) and
                    tglobalsymtable(ttypesym(typesym).owner).dbx_count_ok)  then
                begin
                   {with DBX we get the definition from the other objects }
                   is_def_stab_written := written;
                   exit;
                end;
             end;
        end;
      { to avoid infinite loops }
      is_def_stab_written := being_written;
      stab_str := allstabstring;
      asmList.concat(Tai_stabs.Create(stab_str));
      is_def_stab_written := written;
      end;
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
               if (cs_create_smart in aktmoduleswitches) then
                rttiList.concat(Tai_cut.Create);
               rttiList.concat(Tai_align.create(const_align(pointer_size)));
               rttiList.concat(Tai_symbol.Create(localrttilab[rt],0));
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
          pointerdef,
          enumdef:
            is_intregable:=true;
          procvardef :
            is_intregable:=not(po_methodpointer in tprocvardef(self).procoptions);
          orddef :
            case torddef(self).typ of
              bool8bit,bool16bit,bool32bit,
              u8bit,u16bit,u32bit,
              s8bit,s16bit,s32bit:
                is_intregable:=true;
            end;
          setdef:
            is_intregable:=(tsetdef(self).settype=smallset);
        end;
     end;


   function tstoreddef.is_fpuregable : boolean;
     begin
        is_fpuregable:=(deftype=floatdef);
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


    constructor tstringdef.createlong(l : longint);
      begin
         inherited create;
         string_typ:=st_longstring;
         deftype:=stringdef;
         len:=l;
         savesize:=POINTER_SIZE;
      end;


    constructor tstringdef.loadlong(ppufile:tcompilerppufile);
      begin
         inherited ppuloaddef(ppufile);
         deftype:=stringdef;
         string_typ:=st_longstring;
         len:=ppufile.getlongint;
         savesize:=POINTER_SIZE;
      end;


    constructor tstringdef.createansi(l : longint);
      begin
         inherited create;
         string_typ:=st_ansistring;
         deftype:=stringdef;
         len:=l;
         savesize:=POINTER_SIZE;
      end;


    constructor tstringdef.loadansi(ppufile:tcompilerppufile);
      begin
         inherited ppuloaddef(ppufile);
         deftype:=stringdef;
         string_typ:=st_ansistring;
         len:=ppufile.getlongint;
         savesize:=POINTER_SIZE;
      end;


    constructor tstringdef.createwide(l : longint);
      begin
         inherited create;
         string_typ:=st_widestring;
         deftype:=stringdef;
         len:=l;
         savesize:=POINTER_SIZE;
      end;


    constructor tstringdef.loadwide(ppufile:tcompilerppufile);
      begin
         inherited ppuloaddef(ppufile);
         deftype:=stringdef;
         string_typ:=st_widestring;
         len:=ppufile.getlongint;
         savesize:=POINTER_SIZE;
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
      const
        typname:array[tstringtype] of string[8]=('',
          'shortstr','longstr','ansistr','widestr'
        );
      begin
        stringtypname:=typname[string_typ];
      end;


    function tstringdef.size : longint;
      begin
        size:=savesize;
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
           ppufile.putlongint(len);
         case string_typ of
            st_shortstring : ppufile.writeentry(ibshortstringdef);
            st_longstring : ppufile.writeentry(iblongstringdef);
            st_ansistring : ppufile.writeentry(ibansistringdef);
            st_widestring : ppufile.writeentry(ibwidestringdef);
         end;
      end;


{$ifdef GDB}
    function tstringdef.stabstring : pchar;
      var
        bytest,charst,longst : string;
      begin
        case string_typ of
           st_shortstring:
             begin
               charst := typeglobalnumber('char');
               { this is what I found in stabs.texinfo but
                 gdb 4.12 for go32 doesn't understand that !! }
             {$IfDef GDBknowsstrings}
               stabstring := strpnew('n'+charst+';'+tostr(len));
             {$else}
               bytest := typeglobalnumber('byte');
               stabstring := strpnew('s'+tostr(len+1)+'length:'+bytest
                  +',0,8;st:ar'+bytest
                  +';1;'+tostr(len)+';'+charst+',8,'+tostr(len*8)+';;');
             {$EndIf}
             end;
           st_longstring:
             begin
               charst := typeglobalnumber('char');
               { this is what I found in stabs.texinfo but
                 gdb 4.12 for go32 doesn't understand that !! }
             {$IfDef GDBknowsstrings}
               stabstring := strpnew('n'+charst+';'+tostr(len));
             {$else}
               bytest := typeglobalnumber('byte');
               longst := typeglobalnumber('longint');
               stabstring := strpnew('s'+tostr(len+5)+'length:'+longst
                  +',0,32;dummy:'+bytest+',32,8;st:ar'+bytest
                  +';1;'+tostr(len)+';'+charst+',40,'+tostr(len*8)+';;');
             {$EndIf}
             end;
           st_ansistring:
             begin
               { an ansi string looks like a pchar easy !! }
               stabstring:=strpnew('*'+typeglobalnumber('char'));
             end;
           st_widestring:
             begin
               { an ansi string looks like a pwidechar easy !! }
               stabstring:=strpnew('*'+typeglobalnumber('widechar'));
             end;
      end;
    end;


    procedure tstringdef.concatstabto(asmlist : taasmoutput);
      begin
        inherited concatstabto(asmlist);
      end;
{$endif GDB}


    function tstringdef.needs_inittable : boolean;
      begin
         needs_inittable:=string_typ in [st_ansistring,st_widestring];
      end;

    function tstringdef.gettypename : string;

      const
         names : array[tstringtype] of string[20] = ('',
           'ShortString','LongString','AnsiString','WideString');

      begin
         gettypename:=names[string_typ];
      end;

    procedure tstringdef.write_rtti_data(rt:trttitype);
      begin
         case string_typ of
            st_ansistring:
              begin
                 rttiList.concat(Tai_const.Create_8bit(tkAString));
                 write_rtti_name;
              end;
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

    constructor tenumdef.create_subrange(_basedef:tenumdef;_min,_max:longint);
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
         basedef:=tenumdef(ppufile.getderef);
         minval:=ppufile.getlongint;
         maxval:=ppufile.getlongint;
         savesize:=ppufile.getlongint;
         has_jumps:=false;
         firstenum:=Nil;
      end;


    procedure tenumdef.calcsavesize;
      begin
        if (aktpackenum=4) or (min<0) or (max>65535) then
         savesize:=4
        else
         if (aktpackenum=2) or (min<0) or (max>255) then
          savesize:=2
        else
         savesize:=1;
      end;


    procedure tenumdef.setmax(_max:longint);
      begin
        maxval:=_max;
        calcsavesize;
      end;


    procedure tenumdef.setmin(_min:longint);
      begin
        minval:=_min;
        calcsavesize;
      end;


    function tenumdef.min:longint;
      begin
        min:=minval;
      end;


    function tenumdef.max:longint;
      begin
        max:=maxval;
      end;


    procedure tenumdef.deref;
      begin
        inherited deref;
        resolvedef(pointer(basedef));
      end;


    destructor tenumdef.destroy;
      begin
        inherited destroy;
      end;


    procedure tenumdef.ppuwrite(ppufile:tcompilerppufile);
      begin
         inherited ppuwritedef(ppufile);
         ppufile.putderef(basedef);
         ppufile.putlongint(min);
         ppufile.putlongint(max);
         ppufile.putlongint(savesize);
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
      var st,st2 : pchar;
          p : tenumsym;
          s : string;
          memsize : word;
      begin
        memsize := memsizeinc;
        getmem(st,memsize);
        { we can specify the size with @s<size>; prefix PM }
        if savesize <> std_param_align then
          strpcopy(st,'@s'+tostr(savesize*8)+';e')
        else
          strpcopy(st,'e');
        p := tenumsym(firstenum);
        while assigned(p) do
          begin
            s :=p.name+':'+tostr(p.value)+',';
            { place for the ending ';' also }
            if (strlen(st)+length(s)+1<memsize) then
              strpcopy(strend(st),s)
            else
              begin
                getmem(st2,memsize+memsizeinc);
                strcopy(st2,st);
                freemem(st,memsize);
                st := st2;
                memsize := memsize+memsizeinc;
                strpcopy(strend(st),s);
              end;
            p := p.nextenum;
          end;
        strpcopy(strend(st),';');
        stabstring := strnew(st);
        freemem(st,memsize);
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
         case savesize of
            1:
              rttiList.concat(Tai_const.Create_8bit(otUByte));
            2:
              rttiList.concat(Tai_const.Create_8bit(otUWord));
            4:
              rttiList.concat(Tai_const.Create_8bit(otULong));
         end;
         rttiList.concat(Tai_const.Create_32bit(min));
         rttiList.concat(Tai_const.Create_32bit(max));
         if assigned(basedef) then
           rttiList.concat(Tai_const_symbol.Create(basedef.get_rtti_label(rt)))
         else
           rttiList.concat(Tai_const.Create_32bit(0));
         hp:=tenumsym(firstenum);
         while assigned(hp) do
           begin
              rttiList.concat(Tai_const.Create_8bit(length(hp.name)));
              rttiList.concat(Tai_string.Create(lower(hp.name)));
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
      var
        l1,l2 : longint;
      begin
         inherited ppuloaddef(ppufile);
         deftype:=orddef;
         typ:=tbasetype(ppufile.getbyte);
         if sizeof(TConstExprInt)=8 then
          begin
            l1:=ppufile.getlongint;
            l2:=ppufile.getlongint;
{$ifopt R+}
  {$define Range_check_on}
{$endif opt R+}
{$R- needed here }
            low:=qword(l1)+(int64(l2) shl 32);
{$ifdef Range_check_on}
  {$R+}
  {$undef Range_check_on}
{$endif Range_check_on}
          end
         else
          low:=ppufile.getlongint;
         if sizeof(TConstExprInt)=8 then
          begin
            l1:=ppufile.getlongint;
            l2:=ppufile.getlongint;
{$ifopt R+}
  {$define Range_check_on}
{$endif opt R+}
{$R- needed here }
            high:=qword(l1)+(int64(l2) shl 32);
{$ifdef Range_check_on}
  {$R+}
  {$undef Range_check_on}
{$endif Range_check_on}
          end
         else
          high:=ppufile.getlongint;
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
            ppufile.putlongint(longint(lo(low)));
            ppufile.putlongint(longint(hi(low)));
          end
         else
          ppufile.putlongint(low);
         if sizeof(TConstExprInt)=8 then
          begin
            ppufile.putlongint(longint(lo(high)));
            ppufile.putlongint(longint(hi(high)));
          end
         else
          ppufile.putlongint(high);
         ppufile.writeentry(iborddef);
      end;


{$ifdef GDB}
    function torddef.stabstring : pchar;
      begin
        case typ of
            uvoid : stabstring := strpnew(numberstring+';');
         {GDB 4.12 for go32 doesn't like boolean as range for 0 to 1 !!!}
{$ifdef Use_integer_types_for_boolean}
         bool8bit,
        bool16bit,
        bool32bit : stabstring := strpnew('r'+numberstring+';0;255;');
{$else : not Use_integer_types_for_boolean}
           uchar  : stabstring := strpnew('-20;');
       uwidechar  : stabstring := strpnew('-30;');
         bool8bit : stabstring := strpnew('-21;');
        bool16bit : stabstring := strpnew('-22;');
        bool32bit : stabstring := strpnew('-23;');
        u64bit    : stabstring := strpnew('-32;');
        s64bit    : stabstring := strpnew('-31;');
{$endif not Use_integer_types_for_boolean}
         {u32bit : stabstring := tstoreddef(s32bittype.def).numberstring+';0;-1;'); }
        else
          stabstring := strpnew('r'+tstoreddef(s32bittype.def).numberstring+';'+tostr(longint(low))+';'+tostr(longint(high))+';');
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
          rttiList.concat(Tai_const.Create_8bit(byte(trans[typ])));
          rttiList.concat(Tai_const.Create_32bit(longint(low)));
          rttiList.concat(Tai_const.Create_32bit(longint(high)));
        end;

      begin
        case typ of
          s64bit :
            begin
              rttiList.concat(Tai_const.Create_8bit(tkInt64));
              write_rtti_name;
              if target_info.endian=endian_little then
                begin
                  { low }
                  rttiList.concat(Tai_const.Create_32bit($0));
                  rttiList.concat(Tai_const.Create_32bit(longint($80000000)));
                  { high }
                  rttiList.concat(Tai_const.Create_32bit(longint($ffffffff)));
                  rttiList.concat(Tai_const.Create_32bit($7fffffff));
                end
              else
                begin
                  { low }
                  rttiList.concat(Tai_const.Create_32bit(longint($80000000)));
                  rttiList.concat(Tai_const.Create_32bit($0));
                  { high }
                  rttiList.concat(Tai_const.Create_32bit($7fffffff));
                  rttiList.concat(Tai_const.Create_32bit(longint($ffffffff)));
                end;
            end;
          u64bit :
            begin
              rttiList.concat(Tai_const.Create_8bit(tkQWord));
              write_rtti_name;
              { low }
              rttiList.concat(Tai_const.Create_32bit($0));
              rttiList.concat(Tai_const.Create_32bit($0));
              { high }
              rttiList.concat(Tai_const.Create_32bit(longint($ffffffff)));
              rttiList.concat(Tai_const.Create_32bit(longint($ffffffff)));
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
           s80real : savesize:=extended_size;
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
    function tfloatdef.stabstring : pchar;
      begin
         case typ of
            s32real,
            s64real : stabstring := strpnew('r'+
               tstoreddef(s32bittype.def).numberstring+';'+tostr(savesize)+';0;');
            { found this solution in stabsread.c from GDB v4.16 }
            s64currency,
            s64comp : stabstring := strpnew('r'+
               tstoreddef(s32bittype.def).numberstring+';-'+tostr(savesize)+';0;');
            { under dos at least you must give a size of twelve instead of 10 !! }
            { this is probably do to the fact that in gcc all is pushed in 4 bytes size }
            s80real : stabstring := strpnew('r'+tstoreddef(s32bittype.def).numberstring+';12;0;');
            else
              internalerror(10005);
         end;
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
            savesize:=592;
          ft_typed,
          ft_untyped :
            savesize:=316;
        end;
{$else cpu64bit}
        case filetyp of
          ft_text :
            savesize:=572;
          ft_typed,
          ft_untyped :
            savesize:=316;
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
      {based on
        FileRec = Packed Record
          Handle,
          Mode,
          RecSize   : longint;
          _private  : array[1..32] of byte;
          UserData  : array[1..16] of byte;
          name      : array[0..255] of char;
        End; }
      { the buffer part is still missing !! (PM) }
      { but the string could become too long !! }
      stabstring := strpnew('s'+tostr(savesize)+
                     'HANDLE:'+typeglobalnumber('longint')+',0,32;'+
                     'MODE:'+typeglobalnumber('longint')+',32,32;'+
                     'RECSIZE:'+typeglobalnumber('longint')+',64,32;'+
                     '_PRIVATE:ar'+typeglobalnumber('word')+';1;32;'+typeglobalnumber('byte')
                        +',96,256;'+
                     'USERDATA:ar'+typeglobalnumber('word')+';1;16;'+typeglobalnumber('byte')
                        +',352,128;'+
                     'NAME:ar'+typeglobalnumber('word')+';0;255;'+typeglobalnumber('char')
                        +',480,2048;;');
   {$EndIf}
      end;


    procedure tfiledef.concatstabto(asmlist : taasmoutput);
      begin
      { most file defs are unnamed !!! }
      if ((typesym = nil) or ttypesym(typesym).isusedinstab or (cs_gdb_dbx in aktglobalswitches)) and
         (is_def_stab_written  = not_written) then
        begin
        if assigned(typedfiletype.def) then
          forcestabto(asmlist,typedfiletype.def);
        inherited concatstabto(asmlist);
        end;
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

    constructor tvariantdef.create;
      begin
         inherited create;
         deftype:=variantdef;
         setsize;
      end;


    constructor tvariantdef.ppuload(ppufile:tcompilerppufile);
      begin
         inherited ppuloaddef(ppufile);
         deftype:=variantdef;
         setsize;
      end;


    procedure tvariantdef.ppuwrite(ppufile:tcompilerppufile);
      begin
         inherited ppuwritedef(ppufile);
         ppufile.writeentry(ibvariantdef);
      end;


    procedure tvariantdef.setsize;
      begin
         savesize:=16;
      end;


    function tvariantdef.gettypename : string;
      begin
         gettypename:='Variant';
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
   procedure tvariantdef.concatstabto(asmlist : taasmoutput);
      begin
        { don't know how to handle this }
      end;
{$endif GDB}

{****************************************************************************
                               TPOINTERDEF
****************************************************************************}

    constructor tpointerdef.create(const tt : ttype);
      begin
        inherited create;
        deftype:=pointerdef;
        pointertype:=tt;
        is_far:=false;
        savesize:=POINTER_SIZE;
      end;


    constructor tpointerdef.createfar(const tt : ttype);
      begin
        inherited create;
        deftype:=pointerdef;
        pointertype:=tt;
        is_far:=true;
        savesize:=POINTER_SIZE;
      end;


    constructor tpointerdef.ppuload(ppufile:tcompilerppufile);
      begin
         inherited ppuloaddef(ppufile);
         deftype:=pointerdef;
         ppufile.gettype(pointertype);
         is_far:=(ppufile.getbyte<>0);
         savesize:=POINTER_SIZE;
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
          sym_line_no : longint;
      begin
      if assigned(pointertype.def) and
         (pointertype.def.deftype=forwarddef) then
        exit;

      if ( (typesym=nil) or ttypesym(typesym).isusedinstab or (cs_gdb_dbx in aktglobalswitches)) and
         (is_def_stab_written = not_written) then
        begin
          is_def_stab_written := being_written;
        if assigned(pointertype.def) and
           (pointertype.def.deftype in [recorddef,objectdef]) then
          begin
            if pointertype.def.deftype=objectdef then
              nb:=tobjectdef(pointertype.def).classnumberstring
            else
              nb:=tstoreddef(pointertype.def).numberstring;
            {to avoid infinite recursion in record with next-like fields }
            if tstoreddef(pointertype.def).is_def_stab_written = being_written then
              begin
                if assigned(pointertype.def.typesym) then
                  begin
                    if assigned(typesym) then
                      begin
                         st := ttypesym(typesym).name;
                         sym_line_no:=ttypesym(typesym).fileinfo.line;
                      end
                    else
                      begin
                         st := ' ';
                         sym_line_no:=0;
                      end;
                    st := '"'+st+':t'+numberstring+'=*'+nb
                          +'=xs'+pointertype.def.typesym.name+':",'+tostr(N_LSYM)+',0,'+tostr(sym_line_no)+',0';
                    asmList.concat(Tai_stabs.Create(strpnew(st)));
                    end;
              end
            else
              begin
                is_def_stab_written := not_written;
                inherited concatstabto(asmlist);
              end;
            is_def_stab_written := written;
          end
        else
          begin
            if assigned(pointertype.def) then
              forcestabto(asmlist,pointertype.def);
            is_def_stab_written := not_written;
            inherited concatstabto(asmlist);
          end;
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
         savesize:=POINTER_SIZE;
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
         stabstring:=strpnew(tstoreddef(pvmttype.def).numberstring+';');
      end;


    procedure tclassrefdef.concatstabto(asmlist : taasmoutput);
      begin
        inherited concatstabto(asmlist);
      end;
{$endif GDB}

    function tclassrefdef.gettypename : string;

      begin
         gettypename:='Class Of '+pointertype.def.typename;
      end;


{***************************************************************************
                                   TSETDEF
***************************************************************************}

    constructor tsetdef.create(const t:ttype;high : longint);
      begin
         inherited create;
         deftype:=setdef;
         elementtype:=t;
         if high<32 then
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
      end;


    destructor tsetdef.destroy;
      begin
        inherited destroy;
      end;


    procedure tsetdef.ppuwrite(ppufile:tcompilerppufile);
      begin
         inherited ppuwritedef(ppufile);
         ppufile.puttype(elementtype);
         ppufile.putbyte(byte(settype));
         if settype=varset then
           ppufile.putlongint(savesize);
         ppufile.writeentry(ibsetdef);
      end;


    procedure tsetdef.changesettype(s:tsettype);
      begin
        case s of
          smallset :
            savesize:=sizeof(longint);
          normset :
            savesize:=32;
          varset :
            internalerror(200110201);
        end;
        settype:=s;
      end;


{$ifdef GDB}
    function tsetdef.stabstring : pchar;
      begin
         { For small sets write a longint, which can at least be seen
           in the current GDB's (PFV)
           this is obsolete with GDBPAS !!
           and anyhow creates problems with version 4.18!! PM
         if settype=smallset then
           stabstring := strpnew('r'+s32bittype^.numberstring+';0;0xffffffff;')
         else }
           stabstring := strpnew('@s'+tostr(savesize*8)+';S'+tstoreddef(elementtype.def).numberstring);
      end;


    procedure tsetdef.concatstabto(asmlist : taasmoutput);
      begin
      if ( not assigned(typesym) or ttypesym(typesym).isusedinstab or (cs_gdb_dbx in aktglobalswitches)) and
          (is_def_stab_written = not_written) then
        begin
          if assigned(elementtype.def) then
            forcestabto(asmlist,elementtype.def);
          inherited concatstabto(asmlist);
        end;
      end;
{$endif GDB}


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
         rttiList.concat(Tai_const.Create_8bit(otULong));
         rttiList.concat(Tai_const_symbol.Create(tstoreddef(elementtype.def).get_rtti_label(rt)));
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
         savesize:=POINTER_SIZE;
      end;


    constructor tformaldef.ppuload(ppufile:tcompilerppufile);
      begin
         inherited ppuloaddef(ppufile);
         deftype:=formaldef;
         savesize:=POINTER_SIZE;
      end;


    procedure tformaldef.ppuwrite(ppufile:tcompilerppufile);
      begin
         inherited ppuwritedef(ppufile);
         ppufile.writeentry(ibformaldef);
      end;


{$ifdef GDB}
    function tformaldef.stabstring : pchar;
      begin
      stabstring := strpnew('formal'+numberstring+';');
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

    constructor tarraydef.create(l,h : longint;const t : ttype);
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
      end;


    constructor tarraydef.ppuload(ppufile:tcompilerppufile);
      begin
         inherited ppuloaddef(ppufile);
         deftype:=arraydef;
         { the addresses are calculated later }
         ppufile.gettype(_elementtype);
         ppufile.gettype(rangetype);
         lowrange:=ppufile.getlongint;
         highrange:=ppufile.getlongint;
         IsArrayOfConst:=boolean(ppufile.getbyte);
         IsDynamicArray:=boolean(ppufile.getbyte);
         IsVariant:=false;
         IsConstructor:=false;
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
         ppufile.putlongint(lowrange);
         ppufile.putlongint(highrange);
         ppufile.putbyte(byte(IsArrayOfConst));
         ppufile.putbyte(byte(IsDynamicArray));
         ppufile.writeentry(ibarraydef);
      end;


{$ifdef GDB}
    function tarraydef.stabstring : pchar;
      begin
      stabstring := strpnew('ar'+tstoreddef(rangetype.def).numberstring+';'
                    +tostr(lowrange)+';'+tostr(highrange)+';'+tstoreddef(_elementtype.def).numberstring);
      end;


    procedure tarraydef.concatstabto(asmlist : taasmoutput);
      begin
      if (not assigned(typesym) or ttypesym(typesym).isusedinstab or (cs_gdb_dbx in aktglobalswitches))
        and (is_def_stab_written = not_written) then
        begin
        {when array are inserted they have no definition yet !!}
        if assigned(_elementtype.def) then
          inherited concatstabto(asmlist);
        end;
      end;
{$endif GDB}


    function tarraydef.elesize : longint;
      begin
        elesize:=_elementtype.def.size;
      end;


    function tarraydef.size : longint;
      var
        _resultsize,
        newsize,
        cachedsize: TConstExprInt;
      begin
        if IsDynamicArray then
          begin
            size:=POINTER_SIZE;
            exit;
          end;
        cachedsize := elesize;
        {Tarraydef.size may never be called for an open array!}
        if highrange<lowrange then
            internalerror(99080501);
        newsize:=(int64(highrange)-int64(lowrange)+1)*cachedsize;
        if (cachedsize>0) and
            (
             (TConstExprInt(highrange)-TConstExprInt(lowrange) > $7fffffff) or
             { () are needed around elesize-1 to avoid a possible
               integer overflow for elesize=1 !! PM }
             (($7fffffff div cachedsize + (cachedsize -1)) < (int64(highrange) - int64(lowrange)))
            ) Then
          begin
             Message(sym_e_segment_too_large);
             _resultsize := 4
          end
        else
          begin
            { prevent an overflow }
            if newsize>high(longint) then
              _resultsize:=high(longint)
            else
              _resultsize:=newsize;
          end;

        size := _resultsize;
      end;


     procedure tarraydef.setelementtype(t: ttype);
       begin
         _elementtype:=t;
         If IsDynamicArray then
            exit;
         if (TConstExprInt(highrange)-TConstExprInt(lowrange) > $7fffffff) then
             Message(sym_e_segment_too_large);
       end;



    function tarraydef.alignment : longint;
      begin
         { alignment is the size of the elements }
         if elementtype.def.deftype=recorddef then
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
         { size of elements }
         rttiList.concat(Tai_const.Create_32bit(elesize));
         { count of elements }
         if not(IsDynamicArray) then
           rttiList.concat(Tai_const.Create_32bit(highrange-lowrange+1));
         { element type }
         rttiList.concat(Tai_const_symbol.Create(tstoreddef(elementtype.def).get_rtti_label(rt)));
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
    procedure tabstractrecorddef.addname(p : tnamedindexitem;arg:pointer);
      var
        news, newrec : pchar;
        spec : string[3];
        varsize : longint;
      begin
        { static variables from objects are like global objects }
        if (sp_static in tsym(p).symoptions) then
          exit;
        If tsym(p).typ = varsym then
         begin
           if (sp_protected in tsym(p).symoptions) then
             spec:='/1'
           else if (sp_private in tsym(p).symoptions) then
             spec:='/0'
           else
             spec:='';
           if not assigned(tvarsym(p).vartype.def) then
            writeln(tvarsym(p).name);
           { class fields are pointers PM, obsolete now PM }
           {if (tvarsym(p).vartype.def.deftype=objectdef) and
              tobjectdef(tvarsym(p).vartype.def).is_class then
              spec:=spec+'*'; }
           varsize:=tvarsym(p).vartype.def.size;
           { open arrays made overflows !! }
           if varsize>$fffffff then
             varsize:=$fffffff;
           newrec := strpnew(p.name+':'+spec+tstoreddef(tvarsym(p).vartype.def).numberstring
                         +','+tostr(tvarsym(p).address*8)+','
                         +tostr(varsize*8)+';');
           if strlen(StabRecString) + strlen(newrec) >= StabRecSize-256 then
             begin
                getmem(news,stabrecsize+memsizeinc);
                strcopy(news,stabrecstring);
                freemem(stabrecstring,stabrecsize);
                stabrecsize:=stabrecsize+memsizeinc;
                stabrecstring:=news;
             end;
           strcat(StabRecstring,newrec);
           strdispose(newrec);
           {This should be used for case !!}
           inc(RecOffset,tvarsym(p).vartype.def.size);
         end;
      end;
{$endif GDB}


    procedure tabstractrecorddef.count_field_rtti(sym : tnamedindexitem;arg:pointer);
      begin
         if (FRTTIType=fullrtti) or
            ((tsym(sym).typ=varsym) and
             tvarsym(sym).vartype.def.needs_inittable) then
           inc(Count);
      end;


    procedure tabstractrecorddef.generate_field_rtti(sym:tnamedindexitem;arg:pointer);
      begin
         if (FRTTIType=fullrtti) or
            ((tsym(sym).typ=varsym) and
             tvarsym(sym).vartype.def.needs_inittable) then
           tstoreddef(tvarsym(sym).vartype.def).get_rtti_label(FRTTIType);
      end;


    procedure tabstractrecorddef.write_field_rtti(sym : tnamedindexitem;arg:pointer);
      begin
         if (FRTTIType=fullrtti) or
            ((tsym(sym).typ=varsym) and
             tvarsym(sym).vartype.def.needs_inittable) then
          begin
            rttiList.concat(Tai_const_symbol.Create(tstoreddef(tvarsym(sym).vartype.def).get_rtti_label(FRTTIType)));
            rttiList.concat(Tai_const.Create_32bit(tvarsym(sym).address));
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
         { recordalign -1 means C record packing, that starts
           with an alignment of 1 }
         if aktalignment.recordalignmax=-1 then
          symtable.dataalignment:=1
         else
          symtable.dataalignment:=aktalignment.recordalignmax;
         isunion:=false;
      end;


    constructor trecorddef.ppuload(ppufile:tcompilerppufile);
      begin
         inherited ppuloaddef(ppufile);
         deftype:=recorddef;
         savesize:=ppufile.getlongint;
         symtable:=trecordsymtable.create;
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


    function trecorddef.needs_inittable : boolean;
      begin
        needs_inittable:=trecordsymtable(symtable).needs_init_final
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
         { assign TGUID? load only from system unit (unitid=1) }
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
         ppufile.putlongint(savesize);
         ppufile.writeentry(ibrecorddef);
         trecordsymtable(symtable).ppuwrite(ppufile);
      end;


    function trecorddef.size:longint;
      var
        _resultsize : longint;
      begin
        _resultsize:=symtable.datasize;
        size:=_resultsize;
      end;


    function trecorddef.alignment:longint;
      var
        l  : longint;
        hp : tvarsym;
      begin
        { also check the first symbol for it's size, because a
          packed record has dataalignment of 1, but the first
          sym could be a longint which should be aligned on 4 bytes,
          this is compatible with C record packing (PFV) }
        hp:=tvarsym(symtable.symindex.first);
        if assigned(hp) then
         begin
           if hp.vartype.def.deftype in [recorddef,arraydef] then
            l:=hp.vartype.def.alignment
           else
            l:=hp.vartype.def.size;
           if l>symtable.dataalignment then
            begin
              if l>=4 then
               alignment:=4
              else
               if l>=2 then
                alignment:=2
              else
               alignment:=1;
            end
           else
            alignment:=symtable.dataalignment;
         end
        else
         alignment:=symtable.dataalignment;
      end;


{$ifdef GDB}
    function trecorddef.stabstring : pchar;
      begin
        GetMem(stabrecstring,memsizeinc);
        stabrecsize:=memsizeinc;
        strpcopy(stabRecString,'s'+tostr(size));
        RecOffset := 0;
        symtable.foreach({$ifdef FPCPROCVAR}@{$endif}addname,nil);
        strpcopy(strend(StabRecString),';');
        stabstring := strnew(StabRecString);
        Freemem(stabrecstring,stabrecsize);
      end;


    procedure trecorddef.concatstabto(asmlist : taasmoutput);
      begin
        if (not assigned(typesym) or ttypesym(typesym).isusedinstab or (cs_gdb_dbx in aktglobalswitches)) and
           (is_def_stab_written = not_written)  then
          inherited concatstabto(asmlist);
      end;

{$endif GDB}

    procedure trecorddef.write_child_rtti_data(rt:trttitype);
      begin
         FRTTIType:=rt;
         symtable.foreach({$ifdef FPCPROCVAR}@{$endif}generate_field_rtti,nil);
      end;


    procedure trecorddef.write_rtti_data(rt:trttitype);
      begin
         rttiList.concat(Tai_const.Create_8bit(tkrecord));
         write_rtti_name;
         rttiList.concat(Tai_const.Create_32bit(size));
         Count:=0;
         FRTTIType:=rt;
         symtable.foreach({$ifdef FPCPROCVAR}@{$endif}count_field_rtti,nil);
         rttiList.concat(Tai_const.Create_32bit(Count));
         symtable.foreach({$ifdef FPCPROCVAR}@{$endif}write_field_rtti,nil);
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
         para:=TLinkedList.Create;
         selfpara:=nil;
         minparacount:=0;
         maxparacount:=0;
         proctypeoption:=potype_none;
         proccalloption:=pocall_none;
         procoptions:=[];
         rettype:=voidtype;
         fpu_used:=0;
         savesize:=POINTER_SIZE;
      end;


    destructor tabstractprocdef.destroy;
      begin
         if assigned(para) then
          para.free;
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
        para.free;
        para:=nil;
        parast.free;
        parast:=nil;
      end;


    function tabstractprocdef.concatpara(afterpara:tparaitem;const tt:ttype;sym : tsym;vsp : tvarspez;defval:tsym):tparaitem;
      var
        hp : TParaItem;
      begin
        hp:=TParaItem.Create;
        hp.paratyp:=vsp;
        hp.parasym:=sym;
        hp.paratype:=tt;
        hp.defaultvalue:=defval;
        { Parameters are stored from left to right }
        if assigned(afterpara) then
          Para.insertafter(hp,afterpara)
        else
          Para.concat(hp);
        { Don't count hidden parameters }
        if (vsp<>vs_hidden) then
         begin
           if not assigned(defval) then
            inc(minparacount);
           inc(maxparacount);
         end;
        concatpara:=hp;
      end;


    function tabstractprocdef.insertpara(const tt:ttype;sym : tsym;vsp : tvarspez;defval:tsym):tparaitem;
      var
        hp : TParaItem;
      begin
        hp:=TParaItem.Create;
        hp.paratyp:=vsp;
        hp.parasym:=sym;
        hp.paratype:=tt;
        hp.defaultvalue:=defval;
        { Parameters are stored from left to right }
        Para.insert(hp);
        { Don't count hidden parameters }
        if (vsp<>vs_hidden) then
         begin
           if not assigned(defval) then
            inc(minparacount);
           inc(maxparacount);
         end;
        insertpara:=hp;
      end;


    procedure tabstractprocdef.removepara(currpara:tparaitem);
      begin
        { Don't count hidden parameters }
        if (currpara.paratyp<>vs_hidden) then
         begin
           if not assigned(currpara.defaultvalue) then
            dec(minparacount);
           dec(maxparacount);
         end;
        Para.Remove(currpara);
        currpara.free;
      end;


    { all functions returning in FPU are
      assume to use 2 FPU registers
      until the function implementation
      is processed   PM }
    procedure tabstractprocdef.test_if_fpu_result;
      begin
         if assigned(rettype.def) and
            (rettype.def.deftype=floatdef) then
           fpu_used:=maxfpuregs;
      end;


    procedure tabstractprocdef.deref;
      var
         hp : TParaItem;
         oldlocalsymtable : tsymtable;
      begin
         inherited deref;
         rettype.resolve;
         { parast }
         oldlocalsymtable:=aktlocalsymtable;
         aktlocalsymtable:=parast;
         tparasymtable(parast).deref;
         aktlocalsymtable:=oldlocalsymtable;
         { paraitems }
         hp:=TParaItem(Para.first);
         while assigned(hp) do
          begin
            hp.paratype.resolve;
            resolvesym(pointer(hp.defaultvalue));
            resolvesym(pointer(hp.parasym));
            hp:=TParaItem(hp.next);
          end;
      end;


    constructor tabstractprocdef.ppuload(ppufile:tcompilerppufile);
      var
         hp : TParaItem;
         count,i : word;
         paraloclen : byte;
      begin
         inherited ppuloaddef(ppufile);
         parast:=nil;
         Para:=TLinkedList.Create;
         selfpara:=nil;
         minparacount:=0;
         maxparacount:=0;
         ppufile.gettype(rettype);
         fpu_used:=ppufile.getbyte;
         proctypeoption:=tproctypeoption(ppufile.getbyte);
         proccalloption:=tproccalloption(ppufile.getbyte);
         ppufile.getsmallset(procoptions);
         { get the number of parameters }
         count:=ppufile.getbyte;
         savesize:=POINTER_SIZE;
         for i:=1 to count do
          begin
            hp:=TParaItem.Create;
            hp.paratyp:=tvarspez(ppufile.getbyte);
            ppufile.gettype(hp.paratype);
            hp.defaultvalue:=tsym(ppufile.getderef);
            hp.parasym:=tsym(ppufile.getderef);
            { later, we'll gerate this on the fly (FK) }
            paraloclen:=ppufile.getbyte;
            if paraloclen<>sizeof(tparalocation) then
              internalerror(200304261);
            ppufile.getdata(hp.paraloc,sizeof(tparalocation));
            { Don't count hidden parameters }
            if (hp.paratyp<>vs_hidden) then
             begin
               if not assigned(hp.defaultvalue) then
                inc(minparacount);
               inc(maxparacount);
             end;
            { Parameters are stored left to right in both ppu and memory }
            Para.concat(hp);
          end;
      end;


    procedure tabstractprocdef.ppuwrite(ppufile:tcompilerppufile);
      var
        hp : TParaItem;
        oldintfcrc : boolean;
      begin
         inherited ppuwritedef(ppufile);
         ppufile.puttype(rettype);
         oldintfcrc:=ppufile.do_interface_crc;
         ppufile.do_interface_crc:=false;
         if simplify_ppu then
          fpu_used:=0;
         ppufile.putbyte(fpu_used);
         ppufile.putbyte(ord(proctypeoption));
         ppufile.putbyte(ord(proccalloption));
         ppufile.putsmallset(procoptions);
         ppufile.do_interface_crc:=oldintfcrc;
         { we need to store the count including vs_hidden }
         ppufile.putbyte(para.count);
         hp:=TParaItem(Para.first);
         while assigned(hp) do
          begin
            ppufile.putbyte(byte(hp.paratyp));
            ppufile.puttype(hp.paratype);
            ppufile.putderef(hp.defaultvalue);
            ppufile.putderef(hp.parasym);
            { write the length of tparalocation so ppudump can
              parse the .ppu without knowing the tparalocation size }
            ppufile.putbyte(sizeof(tparalocation));
            ppufile.putdata(hp.paraloc,sizeof(tparalocation));
            hp:=TParaItem(hp.next);
          end;
      end;



    function tabstractprocdef.para_size(alignsize:longint) : longint;
      var
         pdc : TParaItem;
         l : longint;
      begin
         l:=0;
         pdc:=TParaItem(Para.first);
         while assigned(pdc) do
          begin
            inc(l,paramanager.push_size(pdc.paratyp,pdc.paratype.def,proccalloption));
            l:=align(l,alignsize);
            if assigned(pdc.paratype.def) and
               is_special_array(pdc.paratype.def) then
              begin
                inc(l,POINTER_SIZE);
                l:=align(l,alignsize);
              end;
            pdc:=TParaItem(pdc.next);
          end;
         para_size:=l;
      end;


    function tabstractprocdef.typename_paras(showhidden:boolean) : string;
      var
        hs,s : string;
        hp : TParaItem;
        hpc : tconstsym;
        first : boolean;
      begin
        hp:=TParaItem(Para.first);
        s:='';
        first:=true;
        while assigned(hp) do
         begin
           if (hp.paratyp<>vs_hidden) or
              (showhidden) then
            begin
               if first then
                begin
                  s:=s+'(';
                  first:=false;
                end
               else
                s:=s+',';
               case hp.paratyp of
                 vs_var :
                   s:=s+'var';
                 vs_const :
                   s:=s+'const';
                 vs_out :
                   s:=s+'out';
                 vs_hidden :
                   s:=s+'hidden';
               end;
               if assigned(hp.paratype.def.typesym) then
                 begin
                   s:=s+' ';
                   hs:=hp.paratype.def.typesym.realname;
                   if hs[1]<>'$' then
                     s:=s+hp.paratype.def.typesym.realname
                   else
                     s:=s+hp.paratype.def.gettypename;
                 end;
               { default value }
               if assigned(hp.defaultvalue) then
                begin
                  hpc:=tconstsym(hp.defaultvalue);
                  hs:='';
                  case hpc.consttyp of
                    conststring,
                    constresourcestring :
                      hs:=strpas(pchar(hpc.value.valueptr));
                    constreal :
                      str(pbestreal(hpc.value.valueptr)^,hs);
                    constord :
                      hs:=tostr(hpc.value.valueord);
                    constpointer :
                      hs:=tostr(hpc.value.valueordptr);
                    constbool :
                      begin
                        if hpc.value.valueord<>0 then
                         hs:='TRUE'
                        else
                         hs:='FALSE';
                      end;
                    constnil :
                      hs:='nil';
                    constchar :
                      hs:=chr(hpc.value.valueord);
                    constset :
                      hs:='<set>';
                  end;
                  if hs<>'' then
                   s:=s+'="'+hs+'"';
                end;
             end;
           hp:=TParaItem(hp.next);
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


    procedure tabstractprocdef.concatstabto(asmlist : taasmoutput);
      begin
         if (not assigned(typesym) or ttypesym(typesym).isusedinstab or (cs_gdb_dbx in aktglobalswitches))
            and (is_def_stab_written = not_written)  then
           begin
              if assigned(rettype.def) then forcestabto(asmlist,rettype.def);
              inherited concatstabto(asmlist);
           end;
      end;
{$endif GDB}


{***************************************************************************
                                  TPROCDEF
***************************************************************************}

    constructor tprocdef.create(level:byte);
      begin
         inherited create(level);
         deftype:=procdef;
         has_mangledname:=false;
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
       { first, we assume that all registers are used }
         usedintregisters:=ALL_INTREGISTERS;
         usedotherregisters:=ALL_REGISTERS;
         forwarddef:=true;
         interfacedef:=false;
         hasforward:=false;
         _class := nil;
         code:=nil;
         regvarinfo := nil;
         overloadnumber:=0;
{$ifdef GDB}
         isstabwritten := false;
{$endif GDB}
      end;


    constructor tprocdef.ppuload(ppufile:tcompilerppufile);
      begin
         inherited ppuload(ppufile);
         deftype:=procdef;

         ppufile.getnormalset(usedintregisters);
         ppufile.getnormalset(usedotherregisters);
         has_mangledname:=boolean(ppufile.getbyte);
         if has_mangledname then
          _mangledname:=stringdup(ppufile.getstring)
         else
          _mangledname:=nil;
         overloadnumber:=ppufile.getword;
         extnumber:=ppufile.getword;
         _class := tobjectdef(ppufile.getderef);
         procsym := tsym(ppufile.getderef);
         ppufile.getposinfo(fileinfo);
         ppufile.getsmallset(symoptions);
         { inline stuff }
         if proccalloption=pocall_inline then
          begin
            funcretsym:=tsym(ppufile.getderef);
            code:=ppuloadnode(ppufile);
          end
         else
          begin
            code := nil;
            funcretsym:=nil;
          end;
         { load para symtable }
         parast:=tparasymtable.create(unknown_level);
         tparasymtable(parast).ppuload(ppufile);
         parast.defowner:=self;
         { load local symtable }
         if (proccalloption=pocall_inline) or
            ((current_module.flags and uf_local_browser)<>0) then
          begin
            localst:=tlocalsymtable.create(unknown_level);
            tlocalsymtable(localst).ppuload(ppufile);
            localst.defowner:=self;
          end
         else
          localst:=nil;

         { default values for no persistent data }
         if (cs_link_deffile in aktglobalswitches) and
            (tf_need_export in target_info.flags) and
            (po_exports in procoptions) then
           deffile.AddExport(mangledname);
         aliasnames:=tstringlist.create;
         forwarddef:=false;
         interfacedef:=false;
         hasforward:=false;
         regvarinfo := nil;
         lastref:=nil;
         lastwritten:=nil;
         defref:=nil;
         refcount:=0;
{$ifdef GDB}
         isstabwritten := false;
{$endif GDB}
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
         if (proccalloption=pocall_inline) and assigned(code) then
          begin
{$ifdef MEMDEBUG}
            memprocnodetree.start;
{$endif MEMDEBUG}
            tnode(code).free;
{$ifdef MEMDEBUG}
            memprocnodetree.start;
{$endif MEMDEBUG}
          end;
         if assigned(regvarinfo) then
           dispose(pregvarinfo(regvarinfo));
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
      begin
         inherited ppuwrite(ppufile);
         oldintfcrc:=ppufile.do_interface_crc;
         ppufile.do_interface_crc:=false;
         { set all registers to used for simplified compilation PM }
         if simplify_ppu then
           begin
             usedintregisters:=ALL_INTREGISTERS;
             usedotherregisters:=ALL_REGISTERS;
           end;

         ppufile.putnormalset(usedintregisters);
         ppufile.putnormalset(usedotherregisters);
         ppufile.do_interface_crc:=oldintfcrc;
         ppufile.putbyte(byte(has_mangledname));
         if has_mangledname then
          ppufile.putstring(mangledname);
         ppufile.putword(overloadnumber);
         ppufile.putword(extnumber);
         ppufile.putderef(_class);
         ppufile.putderef(procsym);
         ppufile.putposinfo(fileinfo);
         ppufile.putsmallset(symoptions);

         { inline stuff references to localsymtable, no influence
           on the crc }
         oldintfcrc:=ppufile.do_crc;
         ppufile.do_crc:=false;
         { inline stuff }
         if proccalloption=pocall_inline then
          begin
            ppufile.putderef(funcretsym);
            ppuwritenode(ppufile,code);
          end;
         ppufile.do_crc:=oldintfcrc;

         { write this entry }
         ppufile.writeentry(ibprocdef);

         { Save the para symtable, this is taken from the interface }
         tparasymtable(parast).ppuwrite(ppufile);

         { save localsymtable for inline procedures or when local
           browser info is requested, this has no influence on the crc }
         if (proccalloption=pocall_inline) or
            ((current_module.flags and uf_local_browser)<>0) then
          begin
            oldintfcrc:=ppufile.do_crc;
            ppufile.do_crc:=false;
            if not assigned(localst) then
             begin
               localst:=tlocalsymtable.create(unknown_level);
               localst.defowner:=self;
             end;
            tlocalsymtable(localst).ppuwrite(ppufile);
            ppufile.do_crc:=oldintfcrc;
          end;
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
      begin
        s:='';
        if assigned(_class) then
         begin
           if po_classmethod in procoptions then
            s:=s+'class ';
           s:=s+_class.objrealname^+'.';
         end;
        s:=s+procsym.realname+typename_paras(showhidden);
        if assigned(rettype.def) and
          not(is_void(rettype.def)) then
               s:=s+':'+rettype.def.gettypename;
        fullprocname:=s;
      end;


    function tprocdef.is_methodpointer:boolean;
      begin
        result:=assigned(owner) and
                (owner.symtabletype=objectsymtable);
      end;


    function tprocdef.is_addressonly:boolean;
      begin
        result:=assigned(owner) and
                (owner.symtabletype<>objectsymtable);
      end;


    function tprocdef.is_visible_for_proc(currprocdef:tprocdef):boolean;
      begin
        is_visible_for_proc:=false;

        { private symbols are allowed when we are in the same
          module as they are defined }
        if (sp_private in symoptions) and
           assigned(owner.defowner) and
           (owner.defowner.owner.symtabletype in [globalsymtable,staticsymtable]) and
           (owner.defowner.owner.unitid<>0) then
          exit;

        { protected symbols are vissible in the module that defines them and
          also visible to related objects }
        if (sp_protected in symoptions) and
           (
            (
             (owner.defowner.owner.symtabletype in [globalsymtable,staticsymtable]) and
             (owner.defowner.owner.unitid<>0)
            ) and
            not(
                assigned(currprocdef) and
                assigned(currprocdef._class) and
                currprocdef._class.is_related(tobjectdef(owner.defowner))
               )
           ) then
          exit;

        is_visible_for_proc:=true;
      end;


    function tprocdef.is_visible_for_object(currobjdef:tobjectdef):boolean;
      begin
        is_visible_for_object:=false;

        { private symbols are allowed when we are in the same
          module as they are defined }
        if (sp_private in symoptions) and
           (owner.defowner.owner.symtabletype in [globalsymtable,staticsymtable]) and
           (owner.defowner.owner.unitid<>0) then
          exit;

        { protected symbols are vissible in the module that defines them and
          also visible to related objects }
        if (sp_protected in symoptions) and
           (
            (
             (owner.defowner.owner.symtabletype in [globalsymtable,staticsymtable]) and
             (owner.defowner.owner.unitid<>0)
            ) and
            not(
                assigned(currobjdef) and
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
      begin
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
           locals then
          begin
             tparasymtable(parast).load_references(ppufile,locals);
             tlocalsymtable(localst).load_references(ppufile,locals);
          end;
      end;


    Const
      local_symtable_index : word = $8001;

    function tprocdef.write_references(ppufile:tcompilerppufile;locals:boolean):boolean;
      var
        ref : tref;
        pdo : tobjectdef;
        move_last : boolean;
      begin
        move_last:=lastwritten=lastref;
        if move_last and
           (((current_module.flags and uf_local_browser)=0) or
            not locals) then
          exit;
      { write address of this symbol }
        ppufile.putderef(self);
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
        if ((current_module.flags and uf_local_browser)<>0) and
           locals then
          begin
             pdo:=_class;
             if (owner.symtabletype<>localsymtable) then
               while assigned(pdo) do
                 begin
                    if pdo.symtable<>aktrecordsymtable then
                      begin
                         pdo.symtable.unitid:=local_symtable_index;
                         inc(local_symtable_index);
                      end;
                    pdo:=pdo.childof;
                 end;
             parast.unitid:=local_symtable_index;
             inc(local_symtable_index);
             localst.unitid:=local_symtable_index;
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
      end;

{$ifdef GDB}

{$ifdef unused}
{    procedure addparaname(p : tsym);
      var vs : char;
      begin
      if tvarsym(p).varspez = vs_value then vs := '1'
        else vs := '0';
      strpcopy(strend(StabRecString),p^.name+':'+tstoreddef(tvarsym(p).vartype.def).numberstring+','+vs+';');
      end; }


    function tprocdef.stabstring : pchar;
      var
          i : longint;
          stabrecstring : pchar;
      begin
      getmem(StabRecString,1024);
      strpcopy(StabRecString,'f'+tstoreddef(rettype.def).numberstring);
      i:=maxparacount;
      if i>0 then
        begin
        strpcopy(strend(StabRecString),','+tostr(i)+';');
        (* confuse gdb !! PM
        if assigned(parast) then
          parast.foreach({$ifdef FPCPROCVAR}@{$endif}addparaname)
          else
          begin
          param := para1;
          i := 0;
          while assigned(param) do
            begin
            inc(i);
            if param^.paratyp = vs_value then vartyp := '1' else vartyp := '0';
            {Here we have lost the parameter names !!}
            {using lower case parameters }
            strpcopy(strend(stabrecstring),'p'+tostr(i)
               +':'+param^.paratype.def.numberstring+','+vartyp+';');
            param := param^.next;
            end;
          end;   *)
        {strpcopy(strend(StabRecString),';');}
        end;
      stabstring := strnew(stabrecstring);
      freemem(stabrecstring,1024);
      end;
{$endif unused}

    function tprocdef.stabstring: pchar;
     Var RType : Char;
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
        { this code was correct only as long as the local symboltable
          of the parent had the same name as the function
          but this is no true anymore !! PM
        if (owner.symtabletype=localsymtable) and assigned(owner.name) then
         info := ','+name+','+owner.name^;  }
        if (owner.symtabletype=localsymtable) and
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
      if (proccalloption=pocall_internproc) then
        exit;
      if not isstabwritten then
        asmList.concat(Tai_stabs.Create(stabstring));
      isstabwritten := true;
      if assigned(parast) then
        tstoredsymtable(parast).concatstabto(asmlist);
      { local type defs and vars should not be written
        inside the main proc stab }
      if assigned(localst) and
         (localst.symtablelevel>main_program_level) then
        tstoredsymtable(localst).concatstabto(asmlist);
      is_def_stab_written := written;
    end;
{$endif GDB}


    procedure tprocdef.deref;
      begin
         inherited deref;
         resolvedef(pointer(_class));
         { procsym that originaly defined this definition, should be in the
           same symtable }
         resolvesym(pointer(procsym));
      end;


    procedure tprocdef.derefimpl;
      var
        oldlocalsymtable : tsymtable;
      begin
         { locals }
         if assigned(localst) then
          begin
            { localst }
            oldlocalsymtable:=aktlocalsymtable;
            aktlocalsymtable:=localst;
            { we can deref both interface and implementation parts }
            tlocalsymtable(localst).deref;
            tlocalsymtable(localst).derefimpl;
            aktlocalsymtable:=oldlocalsymtable;
            { funcretsym, this is always located in the localst }
            resolvesym(pointer(funcretsym));
          end
         else
          begin
            { safety }
            funcretsym:=nil;
          end;

        { inline tree }
        if (proccalloption=pocall_inline) then
          code.derefimpl;
      end;


    function tprocdef.gettypename : string;
      begin
         gettypename := FullProcName(false)+';'+ProcCallOptionStr[proccalloption];
      end;


    function tprocdef.mangledname : string;
      var
        s  : string;
        hp : TParaItem;
      begin
        if assigned(_mangledname) then
         begin
           mangledname:=_mangledname^;
           exit;
         end;
        { we need to use the symtable where the procsym is inserted,
          because that is visible to the world }
        s:=mangledname_prefix('',procsym.owner)+procsym.name;
        if overloadnumber>0 then
         s:=s+'$'+tostr(overloadnumber);
        { add parameter types }
        hp:=TParaItem(Para.first);
        while assigned(hp) do
         begin
           if hp.paratyp<>vs_hidden then
             s:=s+'$'+hp.paratype.def.mangledparaname;
           hp:=TParaItem(hp.next);
         end;
        _mangledname:=stringdup(s);
        mangledname:=_mangledname^;
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
         param : TParaItem;

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
         param := TParaItem(Para.first);
         if assigned(param) then
           while assigned(param) do
             begin
                s2:=getcppparaname(param.paratype.def);
                if param.paratyp in [vs_var,vs_out] then
                  s2:='R'+s2;
                s:=s+s2;
                param:=TParaItem(param.next);
             end
         else
           s:=s+'v';
         cplusplusmangledname:=s;
      end;


    procedure tprocdef.setmangledname(const s : string);
      begin
        stringdispose(_mangledname);
        _mangledname:=stringdup(s);
        has_mangledname:=true;
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


    procedure tprocvardef.ppuwrite(ppufile:tcompilerppufile);
      begin
         { here we cannot get a real good value so just give something }
         { plausible (PM) }
         { a more secure way would be
           to allways store in a temp }
         if is_fpu(rettype.def) then
           fpu_used:={2}maxfpuregs
         else
           fpu_used:=0;
         inherited ppuwrite(ppufile);

         { Write this entry }
         ppufile.writeentry(ibprocvardef);

         { Save the para symtable, this is taken from the interface }
         tparasymtable(parast).ppuwrite(ppufile);
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


    function tprocvardef.size : longint;
      begin
         if (po_methodpointer in procoptions) and
            not(po_addressonly in procoptions) then
           size:=2*POINTER_SIZE
         else
           size:=POINTER_SIZE;
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


{$ifdef GDB}
    function tprocvardef.stabstring : pchar;
      var
         nss : pchar;
        { i   : longint; }
      begin
        { i := maxparacount; }
        getmem(nss,1024);
        { it is not a function but a function pointer !! (PM) }

        strpcopy(nss,'*f'+tstoreddef(rettype.def).numberstring{+','+tostr(i)}+';');
        { this confuses gdb !!
          we should use 'F' instead of 'f' but
          as we use c++ language mode
          it does not like that either
          Please do not remove this part
          might be used once
          gdb for pascal is ready PM }
        (*
        param := para1;
        i := 0;
        while assigned(param) do
          begin
          inc(i);
          if param^.paratyp = vs_value then vartyp := '1' else vartyp := '0';
          {Here we have lost the parameter names !!}
          pst := strpnew('p'+tostr(i)+':'+param^.paratype.def.numberstring+','+vartyp+';');
          strcat(nss,pst);
          strdispose(pst);
          param := param^.next;
          end; *)
        {strpcopy(strend(nss),';');}
        stabstring := strnew(nss);
        freemem(nss,1024);
      end;


    procedure tprocvardef.concatstabto(asmlist : taasmoutput);
      begin
         if ( not assigned(typesym) or ttypesym(typesym).isusedinstab or (cs_gdb_dbx in aktglobalswitches))
           and (is_def_stab_written = not_written)  then
           inherited concatstabto(asmlist);
         is_def_stab_written:=written;
      end;
{$endif GDB}


    procedure tprocvardef.write_rtti_data(rt:trttitype);
      var
         pdc : TParaItem;
         methodkind, paraspec : byte;
      begin
        if po_methodpointer in procoptions then
          begin
             { write method id and name }
             rttiList.concat(Tai_const.Create_8bit(tkmethod));
             write_rtti_name;

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
             if (po_leftright in procoptions) then
              pdc:=TParaItem(Para.first)
             else
              pdc:=TParaItem(Para.last);
             while assigned(pdc) do
               begin
                 case pdc.paratyp of
                   vs_value: paraspec := 0;
                   vs_const: paraspec := pfConst;
                   vs_var  : paraspec := pfVar;
                   vs_out  : paraspec := pfOut;
                 end;
                 { write flags for current parameter }
                 rttiList.concat(Tai_const.Create_8bit(paraspec));
                 { write name of current parameter ### how can I get this??? (sg)}
                 rttiList.concat(Tai_const.Create_8bit(0));

                 { write name of type of current parameter }
                 tstoreddef(pdc.paratype.def).write_rtti_name;

                 if (po_leftright in procoptions) then
                  pdc:=TParaItem(pdc.next)
                 else
                  pdc:=TParaItem(pdc.previous);
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
      begin
         s:='<';
         if po_classmethod in procoptions then
           s := s+'class method type of'
         else
           if po_addressonly in procoptions then
             s := s+'address of'
           else
             s := s+'procedure variable type of';
         if assigned(rettype.def) and
            (rettype.def<>voidtype.def) then
           s:=s+' function'+typename_paras(false)+':'+rettype.def.gettypename
         else
           s:=s+' procedure'+typename_paras(false);
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
        symtable:=tobjectsymtable.create(n);
        { create space for vmt !! }
        vmt_offset:=0;
        symtable.datasize:=0;
        symtable.defowner:=self;
        { recordalign -1 means C record packing, that starts
          with an alignment of 1 }
        if aktalignment.recordalignmax=-1 then
         symtable.dataalignment:=1
        else
         symtable.dataalignment:=aktalignment.recordalignmax;
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
      begin
         inherited ppuloaddef(ppufile);
         deftype:=objectdef;
         objecttype:=tobjectdeftype(ppufile.getbyte);
         savesize:=ppufile.getlongint;
         vmt_offset:=ppufile.getlongint;
         objrealname:=stringdup(ppufile.getstring);
         objname:=stringdup(upper(objrealname^));
         childof:=tobjectdef(ppufile.getderef);
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
                  implementedinterfaces.addintfref(ppufile.getderef);
                  implementedinterfaces.ioffsets(i)^:=ppufile.getlongint;
               end;
           end
         else
           implementedinterfaces:=nil;

         symtable:=tobjectsymtable.create(objrealname^);
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


    procedure tobjectdef.ppuwrite(ppufile:tcompilerppufile);
      var
         implintfcount : longint;
         i : longint;
      begin
         inherited ppuwritedef(ppufile);
         ppufile.putbyte(byte(objecttype));
         ppufile.putlongint(size);
         ppufile.putlongint(vmt_offset);
         ppufile.putstring(objrealname^);
         ppufile.putderef(childof);
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
                   ppufile.putderef(implementedinterfaces.interfaces(i));
                   ppufile.putlongint(implementedinterfaces.ioffsets(i)^);
                end;
           end;

         ppufile.writeentry(ibobjectdef);

         tobjectsymtable(symtable).ppuwrite(ppufile);
      end;


    procedure tobjectdef.deref;
      var
         oldrecsyms : tsymtable;
      begin
         inherited deref;
         resolvedef(pointer(childof));
         oldrecsyms:=aktrecordsymtable;
         aktrecordsymtable:=symtable;
         tstoredsymtable(symtable).deref;
         aktrecordsymtable:=oldrecsyms;
         if objecttype in [odt_class,odt_interfacecorba] then
           implementedinterfaces.deref;
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
               [oo_has_virtual,oo_has_private,oo_has_protected,
                oo_has_constructor,oo_has_destructor]);
             if not (objecttype in [odt_interfacecom,odt_interfacecorba]) then
               begin
                  { add the data of the anchestor class }
                  inc(symtable.datasize,c.symtable.datasize);
                  if (oo_has_vmt in objectoptions) and
                     (oo_has_vmt in c.objectoptions) then
                    dec(symtable.datasize,POINTER_SIZE);
                  { if parent has a vmt field then
                    the offset is the same for the child PM }
                  if (oo_has_vmt in c.objectoptions) or is_class(self) then
                    begin
                       vmt_offset:=c.vmt_offset;
                       include(objectoptions,oo_has_vmt);
                    end;
               end;
          end;
        savesize := symtable.datasize;
      end;


   procedure tobjectdef.insertvmt;
     begin
        if objecttype in [odt_interfacecom,odt_interfacecorba] then
          exit;
        if (oo_has_vmt in objectoptions) then
          internalerror(12345)
        else
          begin
             symtable.datasize:=align(symtable.datasize,symtable.dataalignment);
             vmt_offset:=symtable.datasize;
             inc(symtable.datasize,POINTER_SIZE);
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
   function tobjectdef.is_related(d : tobjectdef) : boolean;
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

    procedure Tobjectdef._searchdestructor(sym:Tnamedindexitem;arg:pointer);

    begin
        { if we found already a destructor, then we exit }
        if (sd=nil) and (Tsym(sym).typ=procsym) then
            sd:=Tprocsym(sym).search_procdef_bytype(potype_destructor);
    end;

   function tobjectdef.searchdestructor : tprocdef;

     var
        o : tobjectdef;

     begin
        searchdestructor:=nil;
        o:=self;
        sd:=nil;
        while assigned(o) do
          begin
             symtable.foreach({$ifdef FPCPROCVAR}@{$endif}_searchdestructor,nil);
             if assigned(sd) then
               begin
                  searchdestructor:=sd;
                  exit;
               end;
             o:=o.childof;
          end;
     end;


    function tobjectdef.size : longint;
      var
        _resultsize : longint;
      begin
        if objecttype in [odt_class,odt_interfacecom,odt_interfacecorba] then
          _resultsize:=POINTER_SIZE
        else
          _resultsize:=symtable.datasize;
        size := _resultsize;
      end;


    function tobjectdef.alignment:longint;
      begin
        alignment:=symtable.dataalignment;
      end;


    function tobjectdef.vmtmethodoffset(index:longint):longint;
      begin
        { for offset of methods for classes, see rtl/inc/objpash.inc }
        case objecttype of
        odt_class:
          vmtmethodoffset:=(index+12)*POINTER_SIZE;
        odt_interfacecom,odt_interfacecorba:
          vmtmethodoffset:=index*POINTER_SIZE;
        else
{$ifdef WITHDMT}
          vmtmethodoffset:=(index+4)*POINTER_SIZE;
{$else WITHDMT}
          vmtmethodoffset:=(index+3)*POINTER_SIZE;
{$endif WITHDMT}
        end;
      end;


    function tobjectdef.vmt_mangledname : string;
    begin
      if not(oo_has_vmt in objectoptions) then
        Message1(parser_n_object_has_no_vmt,objrealname^);
      vmt_mangledname:=mangledname_prefix('VMT',owner)+objname^;
    end;


    function tobjectdef.rtti_name : string;
    begin
      rtti_name:=mangledname_prefix('RTTI',owner)+objname^;
    end;


{$ifdef GDB}
    procedure tobjectdef.addprocname(p :tnamedindexitem;arg:pointer);
      var virtualind,argnames : string;
          news, newrec : pchar;
          pd,ipd : tprocdef;
          lindex : longint;
          para : TParaItem;
          arglength : byte;
          sp : char;
      begin
        If tsym(p).typ = procsym then
         begin
           pd := tprocsym(p).first_procdef;
           { this will be used for full implementation of object stabs
           not yet done }
           ipd := Tprocsym(p).last_procdef;
           if (po_virtualmethod in pd.procoptions) then
             begin
               lindex := pd.extnumber;
               {doesnt seem to be necessary
               lindex := lindex or $80000000;}
               virtualind := '*'+tostr(lindex)+';'+ipd._class.classnumberstring+';'
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
            para := TParaItem(pd.Para.first);
            while assigned(para) do
              begin
              if Para.paratype.def.deftype = formaldef then
                begin
                   if Para.paratyp=vs_var then
                     argnames := argnames+'3var'
                   else if Para.paratyp=vs_const then
                     argnames:=argnames+'5const'
                   else if Para.paratyp=vs_out then
                     argnames:=argnames+'3out';
                end
              else
                begin
                { if the arg definition is like (v: ^byte;..
                there is no sym attached to data !!! }
                if assigned(Para.paratype.def.typesym) then
                  begin
                     arglength := length(Para.paratype.def.typesym.name);
                     argnames := argnames + tostr(arglength)+Para.paratype.def.typesym.name;
                  end
                else
                  begin
                     argnames:=argnames+'11unnamedtype';
                  end;
                end;
              para := TParaItem(Para.next);
              end;
           ipd.is_def_stab_written := written;
           { here 2A must be changed for private and protected }
           { 0 is private 1 protected and 2 public }
           if (sp_private in tsym(p).symoptions) then sp:='0'
           else if (sp_protected in tsym(p).symoptions) then sp:='1'
           else sp:='2';
           newrec := strpnew(p.name+'::'+ipd.numberstring
                +'=##'+tstoreddef(pd.rettype.def).numberstring+';:'+argnames+';'+sp+'A'
                +virtualind+';');
          { get spare place for a string at the end }
          if strlen(StabRecString) + strlen(newrec) >= StabRecSize-256 then
            begin
               getmem(news,stabrecsize+memsizeinc);
               strcopy(news,stabrecstring);
               freemem(stabrecstring,stabrecsize);
               stabrecsize:=stabrecsize+memsizeinc;
               stabrecstring:=news;
            end;
          strcat(StabRecstring,newrec);
          {freemem(newrec,memsizeinc);    }
          strdispose(newrec);
          {This should be used for case !!
          RecOffset := RecOffset + pd.size;}
        end;
      end;


    function tobjectdef.stabstring : pchar;
      var anc : tobjectdef;
          oldrec : pchar;
          oldrecsize,oldrecoffset : longint;
          str_end : string;
      begin
        if not (objecttype=odt_class) or writing_class_record_stab then
          begin
            oldrec := stabrecstring;
            oldrecsize:=stabrecsize;
            stabrecsize:=memsizeinc;
            GetMem(stabrecstring,stabrecsize);
            strpcopy(stabRecString,'s'+tostr(symtable.datasize));
            if assigned(childof) then
              begin
                {only one ancestor not virtual, public, at base offset 0 }
                {       !1           ,    0       2         0    ,       }
                strpcopy(strend(stabrecstring),'!1,020,'+childof.classnumberstring+';');
              end;
            {virtual table to implement yet}
            OldRecOffset:=RecOffset;
            RecOffset := 0;
            symtable.foreach({$ifdef FPCPROCVAR}@{$endif}addname,nil);
            RecOffset:=OldRecOffset;
            if (oo_has_vmt in objectoptions) then
              if not assigned(childof) or not(oo_has_vmt in childof.objectoptions) then
                 begin
                    strpcopy(strend(stabrecstring),'$vf'+classnumberstring+':'+typeglobalnumber('vtblarray')
                      +','+tostr(vmt_offset*8)+';');
                 end;
            symtable.foreach({$ifdef FPCPROCVAR}@{$endif}addprocname,nil);
            if (oo_has_vmt in objectoptions) then
              begin
                 anc := self;
                 while assigned(anc.childof) and (oo_has_vmt in anc.childof.objectoptions) do
                   anc := anc.childof;
                 { just in case anc = self }
                 str_end:=';~%'+anc.classnumberstring+';';
              end
            else
              str_end:=';';
            strpcopy(strend(stabrecstring),str_end);
            stabstring := strnew(StabRecString);
            freemem(stabrecstring,stabrecsize);
            stabrecstring := oldrec;
            stabrecsize:=oldrecsize;
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
       { write stabs again if needed }
       numberstring;
       if objecttype=odt_class then
         begin
           dec(globalnb);
           classnumberstring:=numberstring;
           inc(globalnb);
         end
       else
         classnumberstring:=numberstring;
     end;


    function tobjectdef.allstabstring : pchar;
    var stabchar : string[2];
        ss,st : pchar;
        sname : string;
        sym_line_no : longint;
      begin
      ss := stabstring;
      getmem(st,strlen(ss)+512);
      stabchar := 't';
      if deftype in tagtypes then
        stabchar := 'Tt';
      if assigned(typesym) then
        begin
           sname := typesym.name;
           sym_line_no:=typesym.fileinfo.line;
        end
      else
        begin
           sname := ' ';
           sym_line_no:=0;
        end;
      if writing_class_record_stab then
        strpcopy(st,'"'+sname+':'+stabchar+classnumberstring+'=')
      else
        strpcopy(st,'"'+sname+':'+stabchar+numberstring+'=');
      strpcopy(strecopy(strend(st),ss),'",'+tostr(N_LSYM)+',0,'+tostr(sym_line_no)+',0');
      allstabstring := strnew(st);
      freemem(st,strlen(ss)+512);
      strdispose(ss);
      end;

    procedure tobjectdef.concatstabto(asmlist : taasmoutput);
      var st : pstring;
      begin
        if objecttype<>odt_class then
          begin
            inherited concatstabto(asmlist);
            exit;
          end;

      if ((typesym=nil) or ttypesym(typesym).isusedinstab or (cs_gdb_dbx in aktglobalswitches)) and
         (is_def_stab_written = not_written) then
        begin
          if globalnb=0 then
            set_globalnb;
          { Write the record class itself }
          writing_class_record_stab:=true;
          inherited concatstabto(asmlist);
          writing_class_record_stab:=false;
          { Write the invisible pointer class }
          is_def_stab_written:=not_written;
          if assigned(typesym) then
            begin
              st:=typesym.FName;
              typesym.FName:=stringdup(' ');
            end;
          inherited concatstabto(asmlist);
          if assigned(typesym) then
            begin
              stringdispose(typesym.FName);
              typesym.FName:=st;
            end;
        end;
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
          (tsym(sym).typ<>varsym) then
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

        begin
           if not(assigned(proc) and assigned(proc.firstsym))  then
             begin
                rttiList.concat(Tai_const.Create_32bit(1));
                typvalue:=3;
             end
           else if proc.firstsym^.sym.typ=varsym then
             begin
                address:=0;
                hp:=proc.firstsym;
                while assigned(hp) do
                  begin
                     inc(address,tvarsym(hp^.sym).address);
                     hp:=hp^.next;
                  end;
                rttiList.concat(Tai_const.Create_32bit(address));
                typvalue:=0;
             end
           else
             begin
                if not(po_virtualmethod in tprocdef(proc.def).procoptions) then
                  begin
                     rttiList.concat(Tai_const_symbol.Createname(tprocdef(proc.def).mangledname));
                     typvalue:=1;
                  end
                else
                  begin
                     { virtual method, write vmt offset }
                     rttiList.concat(Tai_const.Create_32bit(
                       tprocdef(proc.def)._class.vmtmethodoffset(tprocdef(proc.def).extnumber)));
                     typvalue:=2;
                  end;
             end;
           proctypesinfo:=proctypesinfo or (typvalue shl shiftvalue);
        end;

      begin
         if needs_prop_entry(tsym(sym)) then
           case tsym(sym).typ of
              varsym:
                begin
{$ifdef dummy}
                   if not(tvarsym(sym).vartype.def.deftype=objectdef) or
                     not(tobjectdef(tvarsym(sym).vartype.def).is_class) then
                     internalerror(1509992);
                   { access to implicit class property as field }
                   proctypesinfo:=(0 shl 0) or (0 shl 2) or (0 shl 4);
                   rttiList.concat(Tai_const_symbol.Createname(tvarsym(sym.vartype.def.get_rtti_label)));
                   rttiList.concat(Tai_const.Create_32bit(tvarsym(sym.address)));
                   rttiList.concat(Tai_const.Create_32bit(tvarsym(sym.address)));
                   { per default stored }
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
                   rttiList.concat(Tai_const_symbol.Create(tstoreddef(tpropertysym(sym).proptype.def).get_rtti_label(fullrtti)));
                   writeproc(tpropertysym(sym).readaccess,0);
                   writeproc(tpropertysym(sym).writeaccess,2);
                   { isn't it stored ? }
                   if not(ppo_stored in tpropertysym(sym).propoptions) then
                     begin
                        rttiList.concat(Tai_const.Create_32bit(0));
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
              varsym:
                tstoreddef(tvarsym(sym).vartype.def).get_rtti_label(fullrtti);
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
             symtable.foreach({$ifdef FPCPROCVAR}@{$endif}generate_field_rtti,nil);
           fullrtti :
             symtable.foreach({$ifdef FPCPROCVAR}@{$endif}generate_published_child_rtti,nil);
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
          (tsym(sym).typ=varsym) then
          begin
             if tvarsym(sym).vartype.def.deftype<>objectdef then
               internalerror(0206001);
             hp:=searchclasstablelist(tobjectdef(tvarsym(sym).vartype.def));
             if not(assigned(hp)) then
               begin
                  hp:=tclasslistitem.create;
                  hp.p:=tobjectdef(tvarsym(sym).vartype.def);
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
          (tsym(sym).typ=varsym) then
          begin
             rttiList.concat(Tai_const.Create_32bit(tvarsym(sym).address));
             hp:=searchclasstablelist(tobjectdef(tvarsym(sym).vartype.def));
             if not(assigned(hp)) then
               internalerror(0206002);
             rttiList.concat(Tai_const.Create_16bit(hp.index));
             rttiList.concat(Tai_const.Create_8bit(length(tvarsym(sym).realname)));
             rttiList.concat(Tai_string.Create(tvarsym(sym).realname));
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
         symtable.foreach({$ifdef FPC}@{$endif}count_published_fields,nil);
         if (cs_create_smart in aktmoduleswitches) then
          rttiList.concat(Tai_cut.Create);
         rttilist.concat(tai_align.create(const_align(pointer_size)));
         rttiList.concat(Tai_label.Create(fieldtable));
         rttiList.concat(Tai_const.Create_16bit(count));
         rttiList.concat(Tai_const_symbol.Create(classtable));
         symtable.foreach({$ifdef FPC}@{$endif}writefields,nil);

         { generate the class table }
         rttilist.concat(tai_align.create(const_align(pointer_size)));
         rttiList.concat(Tai_label.Create(classtable));
         rttiList.concat(Tai_const.Create_16bit(tablecount));
         hp:=tclasslistitem(classtablelist.first);
         while assigned(hp) do
           begin
              rttiList.concat(Tai_const_symbol.Createname(tobjectdef(hp.p).vmt_mangledname));
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
         symtable.foreach({$ifdef FPCPROCVAR}@{$endif}count_published_properties,nil);
         next_free_name_index:=i+count;
      end;


    procedure tobjectdef.write_rtti_data(rt:trttitype);
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

         case rt of
           initrtti :
             begin
               rttiList.concat(Tai_const.Create_32bit(size));
               if objecttype in [odt_class,odt_object] then
                begin
                  count:=0;
                  FRTTIType:=rt;
                  symtable.foreach({$ifdef FPCPROCVAR}@{$endif}count_field_rtti,nil);
                  rttiList.concat(Tai_const.Create_32bit(count));
                  symtable.foreach({$ifdef FPCPROCVAR}@{$endif}write_field_rtti,nil);
                end;
             end;
           fullrtti :
             begin
               if objecttype in [odt_interfacecom,odt_interfacecorba] then
                 rttiList.concat(Tai_const.Create_32bit(0))
               else
                 rttiList.concat(Tai_const_symbol.Createname(vmt_mangledname));

               { write owner typeinfo }
               if assigned(childof) and (oo_can_have_published in childof.objectoptions) then
                 rttiList.concat(Tai_const_symbol.Create(childof.get_rtti_label(fullrtti)))
               else
                 rttiList.concat(Tai_const.Create_32bit(0));

               { count total number of properties }
               if assigned(childof) and (oo_can_have_published in childof.objectoptions) then
                 count:=childof.next_free_name_index
               else
                 count:=0;

               { write it }
               symtable.foreach({$ifdef FPCPROCVAR}@{$endif}count_published_properties,nil);
               rttiList.concat(Tai_const.Create_16bit(count));

               { write unit name }
               rttiList.concat(Tai_const.Create_8bit(length(current_module.realmodulename^)));
               rttiList.concat(Tai_string.Create(current_module.realmodulename^));

               { write published properties count }
               count:=0;
               symtable.foreach({$ifdef FPCPROCVAR}@{$endif}count_published_properties,nil);
               rttiList.concat(Tai_const.Create_16bit(count));

               { count is used to write nameindex   }

               { but we need an offset of the owner }
               { to give each property an own slot  }
               if assigned(childof) and (oo_can_have_published in childof.objectoptions) then
                 count:=childof.next_free_name_index
               else
                 count:=0;

               symtable.foreach({$ifdef FPCPROCVAR}@{$endif}write_property_info,nil);
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


    type
      timplintfentry = class(TNamedIndexItem)
        intf: tobjectdef;
        ioffs: longint;
        namemappings: tdictionary;
        procdefs: TIndexArray;
        constructor create(aintf: tobjectdef);
        destructor  destroy; override;
      end;

    constructor timplintfentry.create(aintf: tobjectdef);
      begin
        inherited create;
        intf:=aintf;
        ioffs:=-1;
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

    function  timplementedinterfaces.ioffsets(intfindex: longint): plongint;
      begin
        checkindex(intfindex);
        ioffsets:=@timplintfentry(finterfaces.search(intfindex)).ioffs;
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

    procedure timplementedinterfaces.deref;
      var
        i: longint;
      begin
        for i:=1 to count do
          with timplintfentry(finterfaces.search(i)) do
            resolvedef(pointer(intf));
      end;

    procedure timplementedinterfaces.addintfref(def: pointer);
      begin
        finterfaces.insert(timplintfentry.create(tobjectdef(def)));
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

    procedure timplementedinterfaces.clearimplprocs;
      var
        i: longint;
      begin
        for i:=1 to count do
          with timplintfentry(finterfaces.search(i)) do
            begin
              if assigned(procdefs) then
                procdefs.free;
              procdefs:=nil;
            end;
      end;

    procedure timplementedinterfaces.addimplproc(intfindex: longint; procdef: tprocdef);
      begin
        checkindex(intfindex);
        with timplintfentry(finterfaces.search(intfindex)) do
          begin
            if not assigned(procdefs) then
              procdefs:=tindexarray.create(4);
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
                               GDB Helpers
****************************************************************************}

{$ifdef GDB}
    function typeglobalnumber(const s : string) : string;

      var st : string;
          symt : tsymtable;
          srsym : tsym;
          srsymtable : tsymtable;
          old_make_ref : boolean;
      begin
         old_make_ref:=make_ref;
         make_ref:=false;
         typeglobalnumber := '0';
         srsym := nil;
         if pos('.',s) > 0 then
           begin
           st := copy(s,1,pos('.',s)-1);
           searchsym(st,srsym,srsymtable);
           st := copy(s,pos('.',s)+1,255);
           if assigned(srsym) then
             begin
             if srsym.typ = unitsym then
               begin
               symt := tunitsym(srsym).unitsymtable;
               srsym := tsym(symt.search(st));
               end else srsym := nil;
             end;
           end else st := s;
         if srsym = nil then
          searchsym(st,srsym,srsymtable);
         if (srsym=nil) or
            (srsym.typ<>typesym) then
           begin
             Message(type_e_type_id_expected);
             exit;
           end;
         typeglobalnumber := tstoreddef(ttypesym(srsym).restype.def).numberstring;
         make_ref:=old_make_ref;
      end;
{$endif GDB}


{****************************************************************************
                           Definition Helpers
****************************************************************************}

   procedure reset_global_defs;
     var
       def     : tstoreddef;
{$ifdef debug}
       prevdef : tstoreddef;
{$endif debug}
     begin
{$ifdef debug}
        prevdef:=nil;
{$endif debug}
{$ifdef GDB}
        pglobaltypecount:=@globaltypecount;
{$endif GDB}
        def:=firstglobaldef;
        while assigned(def) do
          begin
{$ifdef GDB}
            if assigned(def.typesym) then
              ttypesym(def.typesym).isusedinstab:=false;
            def.is_def_stab_written:=not_written;
{$endif GDB}
            if assigned(def.rttitablesym) then
              trttisym(def.rttitablesym).lab := nil;
            if assigned(def.inittablesym) then
              trttisym(def.inittablesym).lab := nil;
            def.localrttilab[initrtti]:=nil;
            def.localrttilab[fullrtti]:=nil;
{$ifdef debug}
            prevdef:=def;
{$endif debug}
            def:=def.nextglobal;
          end;
     end;

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

end.
{
  $Log$
  Revision 1.139  2003-05-01 07:59:43  florian
    * introduced defaultordconsttype to decribe the default size of ordinal constants
      on 64 bit CPUs it's equal to cs64bitdef while on 32 bit CPUs it's equal to s32bitdef
    + added defines CPU32 and CPU64 for 32 bit and 64 bit CPUs
    * int64s/qwords are allowed as for loop counter on 64 bit CPUs

  Revision 1.138  2003/04/27 11:21:34  peter
    * aktprocdef renamed to current_procdef
    * procinfo renamed to current_procinfo
    * procinfo will now be stored in current_module so it can be
      cleaned up properly
    * gen_main_procsym changed to create_main_proc and release_main_proc
      to also generate a tprocinfo structure
    * fixed unit implicit initfinal

  Revision 1.137  2003/04/27 07:29:51  peter
    * current_procdef cleanup, current_procdef is now always nil when parsing
      a new procdef declaration
    * aktprocsym removed
    * lexlevel removed, use symtable.symtablelevel instead
    * implicit init/final code uses the normal genentry/genexit
    * funcret state checking updated for new funcret handling

  Revision 1.136  2003/04/25 20:59:35  peter
    * removed funcretn,funcretsym, function result is now in varsym
      and aliases for result and function name are added using absolutesym
    * vs_hidden parameter for funcret passed in parameter
    * vs_hidden fixes
    * writenode changed to printnode and released from extdebug
    * -vp option added to generate a tree.log with the nodetree
    * nicer printnode for statements, callnode

  Revision 1.135  2003/04/23 20:16:04  peter
    + added currency support based on int64
    + is_64bit for use in cg units instead of is_64bitint
    * removed cgmessage from n386add, replace with internalerrors

  Revision 1.134  2003/04/23 12:35:34  florian
    * fixed several issues with powerpc
    + applied a patch from Jonas for nested function calls (PowerPC only)
    * ...

  Revision 1.133  2003/04/10 17:57:53  peter
    * vs_hidden released

  Revision 1.132  2003/03/18 16:25:50  peter
    * no itnernalerror for errordef.concatstabto()

  Revision 1.131  2003/03/17 16:54:41  peter
    * support DefaultHandler and anonymous inheritance fixed
      for message methods

  Revision 1.130  2003/03/17 15:54:22  peter
    * store symoptions also for procdef
    * check symoptions (private,public) when calculating possible
      overload candidates

  Revision 1.129  2003/02/19 22:00:14  daniel
    * Code generator converted to new register notation
    - Horribily outdated todo.txt removed

  Revision 1.128  2003/02/02 19:25:54  carl
    * Several bugfixes for m68k target (register alloc., opcode emission)
    + VIS target
    + Generic add more complete (still not verified)

  Revision 1.127  2003/01/21 14:36:44  pierre
   * set sizes needs to be passes in bits not bytes to stabs info

  Revision 1.126  2003/01/16 22:11:33  peter
    * fixed tprocdef.is_addressonly

  Revision 1.125  2003/01/15 01:44:33  peter
    * merged methodpointer fixes from 1.0.x

  Revision 1.124  2003/01/09 21:52:37  peter
    * merged some verbosity options.
    * V_LineInfo is a verbosity flag to include line info

  Revision 1.123  2003/01/06 21:16:52  peter
    * po_addressonly added to retrieve the address of a methodpointer
      only, this is used for @tclass.method which has no self pointer

  Revision 1.122  2003/01/05 15:54:15  florian
    + added proper support of type = type <type>; for simple types

  Revision 1.121  2003/01/05 13:36:53  florian
    * x86-64 compiles
    + very basic support for float128 type (x86-64 only)

  Revision 1.120  2003/01/02 19:49:00  peter
    * update self parameter only for methodpointer and methods

  Revision 1.119  2002/12/29 18:25:59  peter
    * tprocdef.gettypename implemented

  Revision 1.118  2002/12/27 15:23:09  peter
    * write class methods in fullname

  Revision 1.117  2002/12/15 19:34:31  florian
    + some front end stuff for vs_hidden added

  Revision 1.116  2002/12/15 11:26:02  peter
    * ignore vs_hidden parameters when choosing overloaded proc

  Revision 1.115  2002/12/07 14:27:09  carl
    * 3% memory optimization
    * changed some types
    + added type checking with different size for call node and for
       parameters

  Revision 1.114  2002/12/01 22:05:27  carl
    * no more warnings for structures over 32K since this is
      handled correctly in this version of the compiler.

  Revision 1.113  2002/11/27 20:04:09  peter
    * tvarsym.get_push_size replaced by paramanager.push_size

  Revision 1.112  2002/11/25 21:05:53  carl
   * several mistakes fixed in message files

  Revision 1.111  2002/11/25 18:43:33  carl
   - removed the invalid if <> checking (Delphi is strange on this)
   + implemented abstract warning on instance creation of class with
      abstract methods.
   * some error message cleanups

  Revision 1.110  2002/11/25 17:43:24  peter
    * splitted defbase in defutil,symutil,defcmp
    * merged isconvertable and is_equal into compare_defs(_ext)
    * made operator search faster by walking the list only once

  Revision 1.109  2002/11/23 22:50:06  carl
    * some small speed optimizations
    + added several new warnings/hints

  Revision 1.108  2002/11/22 22:48:10  carl
  * memory optimization with tconstsym (1.5%)

  Revision 1.107  2002/11/19 16:21:29  pierre
   * correct several stabs generation problems

  Revision 1.106  2002/11/18 17:31:59  peter
    * pass proccalloption to ret_in_xxx and push_xxx functions

  Revision 1.105  2002/11/17 16:31:57  carl
    * memory optimization (3-4%) : cleanup of tai fields,
       cleanup of tdef and tsym fields.
    * make it work for m68k

  Revision 1.104  2002/11/16 19:53:18  carl
    * avoid Range check errors

  Revision 1.103  2002/11/15 16:29:09  peter
    * fixed rtti for int64 (merged)

  Revision 1.102  2002/11/15 01:58:54  peter
    * merged changes from 1.0.7 up to 04-11
      - -V option for generating bug report tracing
      - more tracing for option parsing
      - errors for cdecl and high()
      - win32 import stabs
      - win32 records<=8 are returned in eax:edx (turned off by default)
      - heaptrc update
      - more info for temp management in .s file with EXTDEBUG

  Revision 1.101  2002/11/09 15:31:02  carl
    + align RTTI tables

  Revision 1.100  2002/10/19 15:09:25  peter
    + tobjectdef.members_need_inittable that is used to generate only the
      inittable when it is really used. This saves a lot of useless calls
      to fpc_finalize when destroying classes

  Revision 1.99  2002/10/07 21:30:27  peter
    * removed obsolete rangecheck stuff

  Revision 1.98  2002/10/05 15:14:26  peter
    * getparamangeldname for errordef

  Revision 1.97  2002/10/05 12:43:28  carl
    * fixes for Delphi 6 compilation
     (warning : Some features do not work under Delphi)

  Revision 1.96  2002/09/27 21:13:29  carl
    * low-highval always checked if limit ober 2GB is reached (to avoid overflow)

  Revision 1.95  2002/09/16 09:31:10  florian
    * fixed  currency size

  Revision 1.94  2002/09/09 17:34:15  peter
    * tdicationary.replace added to replace and item in a dictionary. This
      is only allowed for the same name
    * varsyms are inserted in symtable before the types are parsed. This
      fixes the long standing "var longint : longint" bug
    - consume_idlist and idstringlist removed. The loops are inserted
      at the callers place and uses the symtable for duplicate id checking

  Revision 1.93  2002/09/07 15:25:07  peter
    * old logs removed and tabs fixed

  Revision 1.92  2002/09/05 19:29:42  peter
    * memdebug enhancements

  Revision 1.91  2002/08/25 19:25:20  peter
    * sym.insert_in_data removed
    * symtable.insertvardata/insertconstdata added
    * removed insert_in_data call from symtable.insert, it needs to be
      called separatly. This allows to deref the address calculation
    * procedures now calculate the parast addresses after the procedure
      directives are parsed. This fixes the cdecl parast problem
    * push_addr_param has an extra argument that specifies if cdecl is used
      or not

  Revision 1.90  2002/08/18 20:06:25  peter
    * inlining is now also allowed in interface
    * renamed write/load to ppuwrite/ppuload
    * tnode storing in ppu
    * nld,ncon,nbas are already updated for storing in ppu

  Revision 1.89  2002/08/11 15:28:00  florian
    + support of explicit type case <any ordinal type>->pointer
      (delphi mode only)

  Revision 1.88  2002/08/11 14:32:28  peter
    * renamed current_library to objectlibrary

  Revision 1.87  2002/08/11 13:24:13  peter
    * saving of asmsymbols in ppu supported
    * asmsymbollist global is removed and moved into a new class
      tasmlibrarydata that will hold the info of a .a file which
      corresponds with a single module. Added librarydata to tmodule
      to keep the library info stored for the module. In the future the
      objectfiles will also be stored to the tasmlibrarydata class
    * all getlabel/newasmsymbol and friends are moved to the new class

  Revision 1.86  2002/08/09 07:33:03  florian
    * a couple of interface related fixes

  Revision 1.85  2002/07/23 09:51:24  daniel
  * Tried to make Tprocsym.defs protected. I didn't succeed but the cleanups
    are worth comitting.

  Revision 1.84  2002/07/20 11:57:57  florian
    * types.pas renamed to defbase.pas because D6 contains a types
      unit so this would conflicts if D6 programms are compiled
    + Willamette/SSE2 instructions to assembler added

  Revision 1.83  2002/07/11 14:41:30  florian
    * start of the new generic parameter handling

  Revision 1.82  2002/07/07 09:52:32  florian
    * powerpc target fixed, very simple units can be compiled
    * some basic stuff for better callparanode handling, far from being finished

  Revision 1.81  2002/07/01 18:46:26  peter
    * internal linker
    * reorganized aasm layer

  Revision 1.80  2002/07/01 16:23:54  peter
    * cg64 patch
    * basics for currency
    * asnode updates for class and interface (not finished)

  Revision 1.79  2002/05/18 13:34:18  peter
    * readded missing revisions

  Revision 1.78  2002/05/16 19:46:44  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.76  2002/05/12 16:53:10  peter
    * moved entry and exitcode to ncgutil and cgobj
    * foreach gets extra argument for passing local data to the
      iterator function
    * -CR checks also class typecasts at runtime by changing them
      into as
    * fixed compiler to cycle with the -CR option
    * fixed stabs with elf writer, finally the global variables can
      be watched
    * removed a lot of routines from cga unit and replaced them by
      calls to cgobj
    * u32bit-s32bit updates for and,or,xor nodes. When one element is
      u32bit then the other is typecasted also to u32bit without giving
      a rangecheck warning/error.
    * fixed pascal calling method with reversing also the high tree in
      the parast, detected by tcalcst3 test

  Revision 1.75  2002/04/25 20:16:39  peter
    * moved more routines from cga/n386util

  Revision 1.74  2002/04/23 19:16:35  peter
    * add pinline unit that inserts compiler supported functions using
      one or more statements
    * moved finalize and setlength from ninl to pinline

  Revision 1.73  2002/04/21 19:02:05  peter
    * removed newn and disposen nodes, the code is now directly
      inlined from pexpr
    * -an option that will write the secondpass nodes to the .s file, this
      requires EXTDEBUG define to actually write the info
    * fixed various internal errors and crashes due recent code changes

  Revision 1.72  2002/04/20 21:32:25  carl
  + generic FPC_CHECKPOINTER
  + first parameter offset in stack now portable
  * rename some constants
  + move some cpu stuff to other units
  - remove unused constents
  * fix stacksize for some targets
  * fix generic size problems which depend now on EXTEND_SIZE constant

}
