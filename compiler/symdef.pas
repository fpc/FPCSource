{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl, Pierre Muller

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

{$i defines.inc}

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
       aasm,cpubase,cpuinfo
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
          constructor loaddef(ppufile:tcompilerppufile);
          destructor  destroy;override;
          procedure writedef(ppufile:tcompilerppufile);
          procedure write(ppufile:tcompilerppufile);virtual;abstract;
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

       targconvtyp = (act_convertable,act_equal,act_exact);

       tvarspez = (vs_value,vs_const,vs_var,vs_out);

       tparaitem = class(tlinkedlistitem)
          paratype     : ttype;
          parasym      : tsym;
          paratyp      : tvarspez;
          argconvtyp   : targconvtyp;
          convertlevel : byte;
          defaultvalue : tsym; { tconstsym }
       end;

       { this is only here to override the count method,
         which can't be used }
       tparalinkedlist = class(tlinkedlist)
          function count:longint;
       end;

       tfiletyp = (ft_text,ft_typed,ft_untyped);

       tfiledef = class(tstoreddef)
          filetyp : tfiletyp;
          typedfiletype : ttype;
          constructor createtext;
          constructor createuntyped;
          constructor createtyped(const tt : ttype);
          constructor load(ppufile:tcompilerppufile);
          procedure write(ppufile:tcompilerppufile);override;
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
          constructor load(ppufile:tcompilerppufile);
          function gettypename:string;override;
          procedure write(ppufile:tcompilerppufile);override;
          procedure setsize;
          function needs_inittable : boolean;override;
          procedure write_rtti_data(rt:trttitype);override;
       end;

       tformaldef = class(tstoreddef)
          constructor create;
          constructor load(ppufile:tcompilerppufile);
          procedure write(ppufile:tcompilerppufile);override;
          function  gettypename:string;override;
{$ifdef GDB}
          function  stabstring : pchar;override;
          procedure concatstabto(asmlist : taasmoutput);override;
{$endif GDB}
       end;

       tforwarddef = class(tstoreddef)
          tosymname : string;
          forwardpos : tfileposinfo;
          constructor create(const s:string;const pos : tfileposinfo);
          function  gettypename:string;override;
       end;

       terrordef = class(tstoreddef)
          constructor create;
          function  gettypename:string;override;
          { debug }
{$ifdef GDB}
          function  stabstring : pchar;override;
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
          constructor load(ppufile:tcompilerppufile);
          procedure write(ppufile:tcompilerppufile);override;
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
          procedure addname(p : tnamedindexitem);
{$endif}
          procedure count_field_rtti(sym : tnamedindexitem);
          procedure write_field_rtti(sym : tnamedindexitem);
          procedure generate_field_rtti(sym : tnamedindexitem);
       public
          symtable : tsymtable;
          function  getsymtable(t:tgetsymtable):tsymtable;override;
       end;

       trecorddef = class(tabstractrecorddef)
       public
          constructor create(p : tsymtable);
          constructor load(ppufile:tcompilerppufile);
          destructor destroy;override;
          procedure write(ppufile:tcompilerppufile);override;
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
          procedure _searchdestructor(sym : tnamedindexitem);
{$ifdef GDB}
          procedure addprocname(p :tnamedindexitem);
{$endif GDB}
          procedure count_published_properties(sym:tnamedindexitem);
          procedure write_property_info(sym : tnamedindexitem);
          procedure generate_published_child_rtti(sym : tnamedindexitem);
          procedure count_published_fields(sym:tnamedindexitem);
          procedure writefields(sym:tnamedindexitem);
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
          isiidguidvalid: boolean;
          iidguid: TGUID;
          iidstr: pstring;
          lastvtableindex: longint;
          { store implemented interfaces defs and name mappings }
          implementedinterfaces: timplementedinterfaces;
          constructor create(ot : tobjectdeftype;const n : string;c : tobjectdef);
          constructor load(ppufile:tcompilerppufile);
          destructor  destroy;override;
          procedure write(ppufile:tcompilerppufile);override;
          procedure deref;override;
          function  size : longint;override;
          function  alignment:longint;override;
          function  vmtmethodoffset(index:longint):longint;
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
          procedure addintfref(def: tdef);

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
          constructor load(ppufile:tcompilerppufile);
          procedure write(ppufile:tcompilerppufile);override;
          function gettypename:string;override;
          { debug }
{$ifdef GDB}
          function stabstring : pchar;override;
          procedure concatstabto(asmlist : taasmoutput);override;
{$endif GDB}
       end;

       tarraydef = class(tstoreddef)
          rangenr    : longint;
          lowrange,
          highrange  : longint;
          elementtype,
          rangetype  : ttype;
          IsDynamicArray,
          IsVariant,
          IsConstructor,
          IsArrayOfConst : boolean;
          function elesize : longint;
          constructor create(l,h : longint;const t : ttype);
          constructor load(ppufile:tcompilerppufile);
          procedure write(ppufile:tcompilerppufile);override;
          function  gettypename:string;override;
          function  getmangledparaname : string;override;
{$ifdef GDB}
          function stabstring : pchar;override;
          procedure concatstabto(asmlist : taasmoutput);override;
{$endif GDB}
          procedure deref;override;
          function size : longint;override;
          function alignment : longint;override;
          { generates the ranges needed by the asm instruction BOUND (i386)
            or CMP2 (Motorola) }
          procedure genrangecheck;

          { returns the label of the range check string }
          function getrangecheckstring : string;
          function needs_inittable : boolean;override;
          procedure write_child_rtti_data(rt:trttitype);override;
          procedure write_rtti_data(rt:trttitype);override;
       end;

       torddef = class(tstoreddef)
          rangenr  : longint;
          low,high : TConstExprInt;
          typ      : tbasetype;
          constructor create(t : tbasetype;v,b : TConstExprInt);
          constructor load(ppufile:tcompilerppufile);
          procedure write(ppufile:tcompilerppufile);override;
          function  is_publishable : boolean;override;
          function  gettypename:string;override;
          procedure setsize;
          { generates the ranges needed by the asm instruction BOUND }
          { or CMP2 (Motorola)                                       }
          procedure genrangecheck;
          function  getrangecheckstring : string;
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
          constructor load(ppufile:tcompilerppufile);
          procedure write(ppufile:tcompilerppufile);override;
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
          proctypeoption  : tproctypeoption;
          proccalloption  : tproccalloption;
          procoptions     : tprocoptions;
          para            : tparalinkedlist;
          maxparacount,
          minparacount    : longint;
          symtablelevel   : byte;
          fpu_used        : byte;    { how many stack fpu must be empty }
          constructor create;
          constructor load(ppufile:tcompilerppufile);
          destructor destroy;override;
          procedure  write(ppufile:tcompilerppufile);override;
          procedure deref;override;
          procedure concatpara(const tt:ttype;sym : tsym;vsp : tvarspez;defval:tsym);
          function  para_size(alignsize:longint) : longint;
          function  typename_paras : string;
          procedure test_if_fpu_result;
          { debug }
{$ifdef GDB}
          function  stabstring : pchar;override;
          procedure concatstabto(asmlist : taasmoutput);override;
{$endif GDB}
       end;

       tprocvardef = class(tabstractprocdef)
          constructor create;
          constructor load(ppufile:tcompilerppufile);
          procedure write(ppufile:tcompilerppufile);override;
          function  size : longint;override;
          function gettypename:string;override;
          function is_publishable : boolean;override;
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
          { where is this function defined, needed here because there
            is only one symbol for all overloaded functions
            EXTDEBUG has fileinfo in tdef (PFV) }
          fileinfo : tfileposinfo;
{$endif}
          { symbol owning this definition }
          procsym : tsym;
          { alias names }
          aliasnames : tstringlist;
          { symtables }
          parast,
          localst : tsymtable;
          funcretsym : tsym;
          { next is only used to check if RESULT is accessed,
            not stored in a tnode }
          resultfuncretsym : tsym;
          { browser info }
          lastref,
          defref,
          crossref,
          lastwritten : tref;
          refcount : longint;
          _class : tobjectdef;
          { it's a tree, but this not easy to handle }
          { used for inlined procs                   }
          code : tnode;
          { info about register variables (JM) }
          regvarinfo: pointer;
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
          usedregisters : tregisterset;
          constructor create;
          constructor load(ppufile:tcompilerppufile);
          destructor  destroy;override;
          procedure write(ppufile:tcompilerppufile);override;
          procedure deref;override;
          procedure derefimpl;override;
          function  getsymtable(t:tgetsymtable):tsymtable;override;
          function  haspara:boolean;
          function  mangledname : string;
          procedure setmangledname(const s : string);
          procedure load_references(ppufile:tcompilerppufile;locals:boolean);
          function  write_references(ppufile:tcompilerppufile;locals:boolean):boolean;
          function  fullprocname:string;
          function fullprocnamewithret:string;
          function  cplusplusmangledname : string;
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
          function  stringtypname:string;
          function  size : longint;override;
          procedure write(ppufile:tcompilerppufile);override;
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
          rangenr,
          minval,
          maxval    : longint;
          has_jumps : boolean;
          firstenum : tsym;  {tenumsym}
          basedef   : tenumdef;
          constructor create;
          constructor create_subrange(_basedef:tenumdef;_min,_max:longint);
          constructor load(ppufile:tcompilerppufile);
          destructor destroy;override;
          procedure write(ppufile:tcompilerppufile);override;
          procedure deref;override;
          function  gettypename:string;override;
          function  is_publishable : boolean;override;
          procedure calcsavesize;
          procedure setmax(_max:longint);
          procedure setmin(_min:longint);
          function  min:longint;
          function  max:longint;
          function  getrangecheckstring:string;
          procedure genrangecheck;
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
          constructor load(ppufile:tcompilerppufile);
          destructor  destroy;override;
          procedure write(ppufile:tcompilerppufile);override;
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
       cvarianttype,              { we use only one variant def }
       pvmttype      : ttype;     { type of classrefs, used for stabs }


       class_tobject : tobjectdef;      { pointer to the anchestor of all classes }
       interface_iunknown : tobjectdef; { KAZ: pointer to the ancestor }
       rec_tguid : trecorddef;          { KAZ: pointer to the TGUID type }
                                        { of all interfaces            }

    const
{$ifdef i386}
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
       systems,
       { symtable }
       symsym,symtable,
       types,
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

{$ifdef MEMDEBUG}
   var
       manglenamesize : longint;
{$endif}

    constructor tstoreddef.loaddef(ppufile:tcompilerppufile);
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
         typesym:=ttypesym(ppufile.getderef);
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


    procedure tstoreddef.writedef(ppufile:tcompilerppufile);
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
        resolvesym(typesym);
        resolvesym(rttitablesym);
        resolvesym(inittablesym);
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
               getdatalabel(localrttilab[rt]);
               write_child_rtti_data(rt);
               if (cs_create_smart in aktmoduleswitches) then
                rttiList.concat(Tai_cut.Create);
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
                                TPARALINKEDLIST
****************************************************************************}

    function tparalinkedlist.count:longint;
      begin
        { You must use tabstractprocdef.minparacount and .maxparacount instead }
        internalerror(432432978);
        count:=0;
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
         inherited loaddef(ppufile);
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
         inherited loaddef(ppufile);
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
         inherited loaddef(ppufile);
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
         inherited loaddef(ppufile);
         deftype:=stringdef;
         string_typ:=st_widestring;
         len:=ppufile.getlongint;
         savesize:=POINTER_SIZE;
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


    procedure tstringdef.write(ppufile:tcompilerppufile);
      begin
         inherited writedef(ppufile);
         if string_typ=st_shortstring then
           ppufile.putbyte(len)
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
               { an ansi string looks like a pchar easy !! }
               stabstring:=strpnew('*'+typeglobalnumber('char'));
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
         rangenr:=0;
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
         rangenr:=0;
         firstenum:=basedef.firstenum;
         while assigned(firstenum) and (tenumsym(firstenum).value<>minval) do
          firstenum:=tenumsym(firstenum).nextenum;
         correct_owner_symtable;
      end;


    constructor tenumdef.load(ppufile:tcompilerppufile);
      begin
         inherited loaddef(ppufile);
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
        resolvedef(tdef(basedef));
      end;


    destructor tenumdef.destroy;
      begin
        inherited destroy;
      end;


    procedure tenumdef.write(ppufile:tcompilerppufile);
      begin
         inherited writedef(ppufile);
         ppufile.putderef(basedef);
         ppufile.putlongint(min);
         ppufile.putlongint(max);
         ppufile.putlongint(savesize);
         ppufile.writeentry(ibenumdef);
      end;


    function tenumdef.getrangecheckstring : string;
      begin
         if (cs_create_smart in aktmoduleswitches) then
           getrangecheckstring:='R_'+current_module.modulename^+tostr(rangenr)
         else
           getrangecheckstring:='R_'+tostr(rangenr);
      end;


    procedure tenumdef.genrangecheck;
      begin
         if rangenr=0 then
           begin
              { generate two constant for bounds }
              getlabelnr(rangenr);
              if (cs_create_smart in aktmoduleswitches) then
                dataSegment.concat(Tai_symbol.Createname_global(getrangecheckstring,8))
              else
                dataSegment.concat(Tai_symbol.Createname(getrangecheckstring,8));
              dataSegment.concat(Tai_const.Create_32bit(min));
              dataSegment.concat(Tai_const.Create_32bit(max));
           end;
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
         rangenr:=0;
         setsize;
      end;


    constructor torddef.load(ppufile:tcompilerppufile);
      var
        l1,l2 : longint;
      begin
         inherited loaddef(ppufile);
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
         rangenr:=0;
         setsize;
      end;


    procedure torddef.setsize;
      begin
         case typ of
            u8bit,s8bit,
            uchar,bool8bit:
              savesize:=1;
            u16bit,s16bit,
            bool16bit,uwidechar:
              savesize:=2;
            s32bit,u32bit,
            bool32bit:
              savesize:=4;
            u64bit,s64bit:
              savesize:=8;
            else
              savesize:=0;
         end;
       { there are no entrys for range checking }
         rangenr:=0;
      end;


    function torddef.getrangecheckstring : string;

      begin
         if (cs_create_smart in aktmoduleswitches) then
           getrangecheckstring:='R_'+current_module.modulename^+tostr(rangenr)
         else
           getrangecheckstring:='R_'+tostr(rangenr);
      end;

    procedure torddef.genrangecheck;
      var
        rangechecksize : longint;
      begin
         if rangenr=0 then
           begin
              if low<=high then
               rangechecksize:=8
              else
               rangechecksize:=16;
              { generate two constant for bounds }
              getlabelnr(rangenr);
              if (cs_create_smart in aktmoduleswitches) then
                dataSegment.concat(Tai_symbol.Createname_global(getrangecheckstring,rangechecksize))
              else
                dataSegment.concat(Tai_symbol.Createname(getrangecheckstring,rangechecksize));
              if low<=high then
                begin
                   dataSegment.concat(Tai_const.Create_32bit(low));
                   dataSegment.concat(Tai_const.Create_32bit(high));
                end
              { for u32bit we need two bounds }
              else
                begin
                   dataSegment.concat(Tai_const.Create_32bit(low));
                   dataSegment.concat(Tai_const.Create_32bit($7fffffff));
                   dataSegment.concat(Tai_const.Create_32bit(longint($80000000)));
                   dataSegment.concat(Tai_const.Create_32bit(high));
                end;
           end;
      end;


    procedure torddef.write(ppufile:tcompilerppufile);
      begin
         inherited writedef(ppufile);
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
             otUByte,otUWord);
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
              { low }
              rttiList.concat(Tai_const.Create_32bit($0));
              rttiList.concat(Tai_const.Create_32bit($8000));
              { high }
              rttiList.concat(Tai_const.Create_32bit($ffff));
              rttiList.concat(Tai_const.Create_32bit($7fff));
            end;
          u64bit :
            begin
              rttiList.concat(Tai_const.Create_8bit(tkQWord));
              write_rtti_name;
              { low }
              rttiList.concat(Tai_const.Create_32bit($0));
              rttiList.concat(Tai_const.Create_32bit($0));
              { high }
              rttiList.concat(Tai_const.Create_32bit($0));
              rttiList.concat(Tai_const.Create_32bit($8000));
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
          'Char','WideChar');

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


    constructor tfloatdef.load(ppufile:tcompilerppufile);
      begin
         inherited loaddef(ppufile);
         deftype:=floatdef;
         typ:=tfloattype(ppufile.getbyte);
         setsize;
      end;


    procedure tfloatdef.setsize;
      begin
         case typ of
           s32real : savesize:=4;
           s64real : savesize:=8;
           s80real : savesize:=extended_size;
           s64comp : savesize:=8;
         else
           savesize:=0;
         end;
      end;


    procedure tfloatdef.write(ppufile:tcompilerppufile);
      begin
         inherited writedef(ppufile);
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
         {tfloattype = (s32real,s64real,s80real,s64bit);}
         translate : array[tfloattype] of byte =
           (ftSingle,ftDouble,ftExtended,ftComp);
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
          'Single','Double','Extended','Comp');

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


    constructor tfiledef.load(ppufile:tcompilerppufile);
      begin
         inherited loaddef(ppufile);
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
        case filetyp of
          ft_text :
            savesize:=572;
          ft_typed,
          ft_untyped :
            savesize:=316;
        end;
      end;


    procedure tfiledef.write(ppufile:tcompilerppufile);
      begin
         inherited writedef(ppufile);
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


    constructor tvariantdef.load(ppufile:tcompilerppufile);
      begin
         inherited loaddef(ppufile);
         deftype:=variantdef;
         setsize;
      end;


    procedure tvariantdef.write(ppufile:tcompilerppufile);
      begin
         inherited writedef(ppufile);
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


    constructor tpointerdef.load(ppufile:tcompilerppufile);
      begin
         inherited loaddef(ppufile);
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


    procedure tpointerdef.write(ppufile:tcompilerppufile);
      begin
         inherited writedef(ppufile);
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


    constructor tclassrefdef.load(ppufile:tcompilerppufile);
      begin
         { be careful, tclassdefref inherits from tpointerdef }
         inherited loaddef(ppufile);
         deftype:=classrefdef;
         ppufile.gettype(pointertype);
         is_far:=false;
         savesize:=POINTER_SIZE;
      end;


    procedure tclassrefdef.write(ppufile:tcompilerppufile);
      begin
         { be careful, tclassdefref inherits from tpointerdef }
         inherited writedef(ppufile);
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


    constructor tsetdef.load(ppufile:tcompilerppufile);
      begin
         inherited loaddef(ppufile);
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


    procedure tsetdef.write(ppufile:tcompilerppufile);
      begin
         inherited writedef(ppufile);
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
           stabstring := strpnew('S'+tstoreddef(elementtype.def).numberstring);
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


    constructor tformaldef.load(ppufile:tcompilerppufile);
      begin
         inherited loaddef(ppufile);
         deftype:=formaldef;
         savesize:=POINTER_SIZE;
      end;


    procedure tformaldef.write(ppufile:tcompilerppufile);
      begin
         inherited writedef(ppufile);
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
         gettypename:='Var';
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
         rangenr:=0;
      end;


    constructor tarraydef.load(ppufile:tcompilerppufile);
      begin
         inherited loaddef(ppufile);
         deftype:=arraydef;
         { the addresses are calculated later }
         ppufile.gettype(elementtype);
         ppufile.gettype(rangetype);
         lowrange:=ppufile.getlongint;
         highrange:=ppufile.getlongint;
         IsArrayOfConst:=boolean(ppufile.getbyte);
         IsDynamicArray:=boolean(ppufile.getbyte);
         IsVariant:=false;
         IsConstructor:=false;
         rangenr:=0;
      end;


    function tarraydef.getrangecheckstring : string;
      begin
         if (cs_create_smart in aktmoduleswitches) then
           getrangecheckstring:='R_'+current_module.modulename^+tostr(rangenr)
         else
           getrangecheckstring:='R_'+tostr(rangenr);
      end;


    procedure tarraydef.genrangecheck;
      begin
         if rangenr=0 then
           begin
              { generates the data for range checking }
              getlabelnr(rangenr);
              if (cs_create_smart in aktmoduleswitches) then
                dataSegment.concat(Tai_symbol.Createname_global(getrangecheckstring,8))
              else
                dataSegment.concat(Tai_symbol.Createname(getrangecheckstring,8));
              if lowrange<=highrange then
                begin
                  dataSegment.concat(Tai_const.Create_32bit(lowrange));
                  dataSegment.concat(Tai_const.Create_32bit(highrange));
                end
              { for big arrays we need two bounds }
              else
                begin
                  dataSegment.concat(Tai_const.Create_32bit(lowrange));
                  dataSegment.concat(Tai_const.Create_32bit($7fffffff));
                  dataSegment.concat(Tai_const.Create_32bit(longint($80000000)));
                  dataSegment.concat(Tai_const.Create_32bit(highrange));
                end;
           end;
      end;


    procedure tarraydef.deref;
      begin
        inherited deref;
        elementtype.resolve;
        rangetype.resolve;
      end;


    procedure tarraydef.write(ppufile:tcompilerppufile);
      begin
         inherited writedef(ppufile);
         ppufile.puttype(elementtype);
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
                    +tostr(lowrange)+';'+tostr(highrange)+';'+tstoreddef(elementtype.def).numberstring);
      end;


    procedure tarraydef.concatstabto(asmlist : taasmoutput);
      begin
      if (not assigned(typesym) or ttypesym(typesym).isusedinstab or (cs_gdb_dbx in aktglobalswitches))
        and (is_def_stab_written = not_written) then
        begin
        {when array are inserted they have no definition yet !!}
        if assigned(elementtype.def) then
          inherited concatstabto(asmlist);
        end;
      end;
{$endif GDB}


    function tarraydef.elesize : longint;
      begin
        elesize:=elementtype.def.size;
      end;


    function tarraydef.size : longint;
      var
        newsize,
        cachedsize: TConstExprInt;
      begin
        if IsDynamicArray then
          begin
            size:=POINTER_SIZE;
            exit;
          end;
        {Tarraydef.size may never be called for an open array!}
        if highrange<lowrange then
            internalerror(99080501);
        cachedsize := elesize;
        If (cachedsize>0) and
           (
            (TConstExprInt(highrange)-TConstExprInt(lowrange) > $7fffffff) or
            { () are needed around elesize-1 to avoid a possible
              integer overflow for elesize=1 !! PM }
            (($7fffffff div cachedsize + (cachedsize -1)) < (int64(highrange) - int64(lowrange)))
           ) Then
          Begin
            Message(sym_e_segment_too_large);
            size := 4
          End
        Else
          begin
            newsize:=(int64(highrange)-int64(lowrange)+1)*cachedsize;
            { prevent an overflow }
            if newsize>high(longint) then
             size:=high(longint)
            else
             size:=newsize;
          end
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
    procedure tabstractrecorddef.addname(p : tnamedindexitem);
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


    procedure tabstractrecorddef.count_field_rtti(sym : tnamedindexitem);
      begin
         if (FRTTIType=fullrtti) or
            ((tsym(sym).typ=varsym) and
             tvarsym(sym).vartype.def.needs_inittable) then
           inc(Count);
      end;


    procedure tabstractrecorddef.generate_field_rtti(sym:tnamedindexitem);
      begin
         if (FRTTIType=fullrtti) or
            ((tsym(sym).typ=varsym) and
             tvarsym(sym).vartype.def.needs_inittable) then
           tstoreddef(tvarsym(sym).vartype.def).get_rtti_label(FRTTIType);
      end;


    procedure tabstractrecorddef.write_field_rtti(sym : tnamedindexitem);
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
      end;


    constructor trecorddef.load(ppufile:tcompilerppufile);
      var
         oldread_member : boolean;
      begin
         inherited loaddef(ppufile);
         deftype:=recorddef;
         savesize:=ppufile.getlongint;
         oldread_member:=read_member;
         read_member:=true;
         symtable:=trecordsymtable.create;
         trecordsymtable(symtable).load(ppufile);
         read_member:=oldread_member;
         symtable.defowner:=self;
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


    procedure trecorddef.write(ppufile:tcompilerppufile);
      var
         oldread_member : boolean;
      begin
         oldread_member:=read_member;
         read_member:=true;
         inherited writedef(ppufile);
         ppufile.putlongint(savesize);
         ppufile.writeentry(ibrecorddef);
         trecordsymtable(symtable).write(ppufile);
         read_member:=oldread_member;
      end;


    function trecorddef.size:longint;
      begin
        size:=symtable.datasize;
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
        symtable.foreach({$ifdef FPCPROCVAR}@{$endif}addname);
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
         symtable.foreach({$ifdef FPCPROCVAR}@{$endif}generate_field_rtti);
      end;


    procedure trecorddef.write_rtti_data(rt:trttitype);
      begin
         rttiList.concat(Tai_const.Create_8bit(tkrecord));
         write_rtti_name;
         rttiList.concat(Tai_const.Create_32bit(size));
         Count:=0;
         FRTTIType:=rt;
         symtable.foreach({$ifdef FPCPROCVAR}@{$endif}count_field_rtti);
         rttiList.concat(Tai_const.Create_32bit(Count));
         symtable.foreach({$ifdef FPCPROCVAR}@{$endif}write_field_rtti);
      end;


    function trecorddef.gettypename : string;
      begin
         gettypename:='<record type>'
      end;


{***************************************************************************
                       TABSTRACTPROCDEF
***************************************************************************}

    constructor tabstractprocdef.create;
      begin
         inherited create;
         para:=TParaLinkedList.Create;
         minparacount:=0;
         maxparacount:=0;
         proctypeoption:=potype_none;
         proccalloption:=pocall_none;
         procoptions:=[];
         rettype:=voidtype;
         symtablelevel:=0;
         fpu_used:=0;
         savesize:=POINTER_SIZE;
      end;


    destructor tabstractprocdef.destroy;
      begin
         Para.Free;
         inherited destroy;
      end;


    procedure tabstractprocdef.concatpara(const tt:ttype;sym : tsym;vsp : tvarspez;defval:tsym);
      var
        hp : TParaItem;
      begin
        hp:=TParaItem.Create;
        hp.paratyp:=vsp;
        hp.parasym:=sym;
        hp.paratype:=tt;
        hp.defaultvalue:=defval;
        Para.insert(hp);
        if not assigned(defval) then
         inc(minparacount);
        inc(maxparacount);
      end;


    { all functions returning in FPU are
      assume to use 2 FPU registers
      until the function implementation
      is processed   PM }
    procedure tabstractprocdef.test_if_fpu_result;
      begin
         if assigned(rettype.def) and
            (rettype.def.deftype=floatdef) then
{$ifdef FAST_FPU}
           fpu_used:=3;
{$else : not FAST_FPU, i.e. SAFE_FPU}
           fpu_used:={2}maxfpuregs;
{$endif FAST_FPU}
      end;


    procedure tabstractprocdef.deref;
      var
         hp : TParaItem;
      begin
         inherited deref;
         rettype.resolve;
         hp:=TParaItem(Para.first);
         while assigned(hp) do
          begin
            hp.paratype.resolve;
            resolvesym(tsym(hp.defaultvalue));
            resolvesym(tsym(hp.parasym));
            hp:=TParaItem(hp.next);
          end;
      end;


    constructor tabstractprocdef.load(ppufile:tcompilerppufile);
      var
         hp : TParaItem;
         count,i : word;
      begin
         inherited loaddef(ppufile);
         Para:=TParaLinkedList.Create;
         minparacount:=0;
         maxparacount:=0;
         ppufile.gettype(rettype);
         fpu_used:=ppufile.getbyte;
         proctypeoption:=tproctypeoption(ppufile.getbyte);
         proccalloption:=tproccalloption(ppufile.getbyte);
         ppufile.getsmallset(procoptions);
         count:=ppufile.getword;
         savesize:=POINTER_SIZE;
         for i:=1 to count do
          begin
            hp:=TParaItem.Create;
            hp.paratyp:=tvarspez(ppufile.getbyte);
            { hp.register:=tregister(ppufile.getbyte); }
            ppufile.gettype(hp.paratype);
            hp.defaultvalue:=tsym(ppufile.getderef);
            hp.parasym:=tsym(ppufile.getderef);
            if not assigned(hp.defaultvalue) then
             inc(minparacount);
            inc(maxparacount);
            Para.concat(hp);
          end;
      end;


    procedure tabstractprocdef.write(ppufile:tcompilerppufile);
      var
        hp : TParaItem;
        oldintfcrc : boolean;
      begin
         inherited writedef(ppufile);
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
         ppufile.putword(maxparacount);
         hp:=TParaItem(Para.first);
         while assigned(hp) do
          begin
            ppufile.putbyte(byte(hp.paratyp));
            { ppufile.putbyte(byte(hp.register)); }
            ppufile.puttype(hp.paratype);
            ppufile.putderef(hp.defaultvalue);
            ppufile.putderef(hp.parasym);
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
            case pdc.paratyp of
              vs_out,
              vs_var   : inc(l,POINTER_SIZE);
              vs_value,
              vs_const : if push_addr_param(pdc.paratype.def) then
                          inc(l,POINTER_SIZE)
                         else
                          inc(l,pdc.paratype.def.size);
            end;
            l:=align(l,alignsize);
            pdc:=TParaItem(pdc.next);
          end;
         para_size:=l;
      end;


    function tabstractprocdef.typename_paras : string;
      var
        hs,s : string;
        hp : TParaItem;
        hpc : tconstsym;
      begin
        hp:=TParaItem(Para.last);
        if not(assigned(hp)) then
          begin
             typename_paras:='';
             exit;
          end;
        s:='(';
        while assigned(hp) do
         begin
           if hp.paratyp=vs_var then
             s:=s+'var'
           else if hp.paratyp=vs_const then
             s:=s+'const'
           else if hp.paratyp=vs_out then
             s:=s+'out';
           if assigned(hp.paratype.def.typesym) then
             begin
               if hp.paratyp in [vs_var,vs_const,vs_out] then
                 s := s + ' ';
               s:=s+hp.paratype.def.typesym.realname;
             end;
           { default value }
           if assigned(hp.defaultvalue) then
            begin
              hpc:=tconstsym(hp.defaultvalue);
              hs:='';
              case hpc.consttyp of
                conststring,
                constresourcestring :
                  hs:=strpas(pchar(hpc.valueptr));
                constreal :
                  str(pbestreal(hpc.valueptr)^,hs);
                constord :
                  hs:=tostr(hpc.valueord);
                constpointer :
                  hs:=tostr(hpc.valueordptr);
                constbool :
                  begin
                    if hpc.valueord<>0 then
                     hs:='TRUE'
                    else
                     hs:='FALSE';
                  end;
                constnil :
                  hs:='nil';
                constchar :
                  hs:=chr(hpc.valueord);
                constset :
                  hs:='<set>';
              end;
              if hs<>'' then
               s:=s+'="'+hs+'"';
            end;
           hp:=TParaItem(hp.previous);
           if assigned(hp) then
            s:=s+',';
         end;
        s:=s+')';
        if (po_varargs in procoptions) then
         s:=s+';VarArgs';
        typename_paras:=s;
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

    constructor tprocdef.create;
      begin
         inherited create;
         deftype:=procdef;
         has_mangledname:=false;
         _mangledname:=nil;
         fileinfo:=aktfilepos;
         extnumber:=$ffff;
         aliasnames:=tstringlist.create;
         localst:=tlocalsymtable.create;
         parast:=tparasymtable.create;
         funcretsym:=nil;
         localst.defowner:=self;
         parast.defowner:=self;
         { this is used by insert
           to check same names in parast and localst }
         localst.next:=parast;
         defref:=nil;
         crossref:=nil;
         lastwritten:=nil;
         refcount:=0;
         if (cs_browser in aktmoduleswitches) and make_ref then
          begin
            defref:=tref.create(defref,@akttokenpos);
            inc(refcount);
          end;
         lastref:=defref;
       { first, we assume that all registers are used }
         usedregisters:=ALL_REGISTERS;
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


    constructor tprocdef.load(ppufile:tcompilerppufile);
      begin
         inherited load(ppufile);
         deftype:=procdef;

         ppufile.getnormalset(usedregisters);
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
         { inline stuff }
         if proccalloption=pocall_inline then
           funcretsym:=tsym(ppufile.getderef)
         else
           funcretsym:=nil;
         { load para and local symtables }
         parast:=tparasymtable.create;
         tparasymtable(parast).load(ppufile);
         parast.defowner:=self;
         if (proccalloption=pocall_inline) or
            ((current_module.flags and uf_local_browser)<>0) then
          begin
            localst:=tlocalsymtable.create;
            tlocalsymtable(localst).load(ppufile);
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
         code := nil;
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
         if assigned(parast) then
           parast.free;
         if assigned(localst) and (localst.symtabletype<>staticsymtable) then
           localst.free;
         if (proccalloption=pocall_inline) and assigned(code) then
           tnode(code).free;
         if assigned(regvarinfo) then
           dispose(pregvarinfo(regvarinfo));
         if (po_msgstr in procoptions) then
           strdispose(messageinf.str);
         if assigned(_mangledname) then
           stringdispose(_mangledname);
         inherited destroy;
      end;


    procedure tprocdef.write(ppufile:tcompilerppufile);
      var
        oldintfcrc : boolean;
      begin
         inherited write(ppufile);
         oldintfcrc:=ppufile.do_interface_crc;
         ppufile.do_interface_crc:=false;
         { set all registers to used for simplified compilation PM }
         if simplify_ppu then
           begin
             usedregisters:=ALL_REGISTERS;
           end;

         ppufile.putnormalset(usedregisters);
         ppufile.do_interface_crc:=oldintfcrc;
         ppufile.putbyte(byte(has_mangledname));
         if has_mangledname then
          ppufile.putstring(mangledname);
         ppufile.putword(overloadnumber);
         ppufile.putword(extnumber);
         ppufile.putderef(_class);
         ppufile.putderef(procsym);
         ppufile.putposinfo(fileinfo);

         { inline stuff references to localsymtable, no influence
           on the crc }
         oldintfcrc:=ppufile.do_crc;
         ppufile.do_crc:=false;
         if (proccalloption=pocall_inline) then
           ppufile.putderef(funcretsym);
         ppufile.do_crc:=oldintfcrc;

         { write this entry }
         ppufile.writeentry(ibprocdef);

         { Save the para symtable, this is taken from the interface }
         if not assigned(parast) then
          begin
            parast:=tparasymtable.create;
            parast.defowner:=self;
          end;
         tparasymtable(parast).write(ppufile);

         { save localsymtable for inline procedures or when local
           browser info is requested, this has no influence on the crc }
         if (proccalloption=pocall_inline) or
            ((current_module.flags and uf_local_browser)<>0) then
          begin
            oldintfcrc:=ppufile.do_crc;
            ppufile.do_crc:=false;
            if not assigned(localst) then
             begin
               localst:=tlocalsymtable.create;
               localst.defowner:=self;
             end;
            tlocalsymtable(localst).write(ppufile);
            ppufile.do_crc:=oldintfcrc;
          end;
      end;


    function tprocdef.fullprocname:string;
      var
        s : string;
      begin
        s:='';
        if assigned(_class) then
         s:=_class.objrealname^+'.';
        s:=s+procsym.realname+typename_paras;
        fullprocname:=s;
      end;


    function tprocdef.fullprocnamewithret:string;
      var
        s : string;
      begin
        s:=fullprocname;
        if assigned(rettype.def) and
          not(is_equal(rettype.def,voidtype.def)) then
               s:=s+' : '+rettype.def.gettypename;
        fullprocnamewithret:=s;
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
      local_symtable_index : longint = $8001;

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


    function tprocdef.haspara:boolean;
      begin
        haspara:=assigned(parast.symindex.first);
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
         obj := upper(owner.name^)+'__'+procsym.name;
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
         (lexlevel>main_program_level) then
        tstoredsymtable(localst).concatstabto(asmlist);
      is_def_stab_written := written;
    end;
{$endif GDB}


    procedure tprocdef.deref;
      var
        oldlocalsymtable : tsymtable;
      begin
         inherited deref;
         resolvedef(tdef(_class));
         { parast }
         oldlocalsymtable:=aktlocalsymtable;
         aktlocalsymtable:=parast;
         tparasymtable(parast).deref;
         aktlocalsymtable:=oldlocalsymtable;
         { procsym that originaly defined this definition, should be in the
           same symtable }
         resolvesym(procsym);
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
            resolvesym(funcretsym);
          end
         else
          begin
            { safety }
            funcretsym:=nil;
          end;
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
        s:=mangledname_prefix('',procsym.owner)+procsym.name+'$';
        if overloadnumber>0 then
         s:=s+tostr(overloadnumber)+'$';
        { add parameter types }
        hp:=TParaItem(Para.last);
        while assigned(hp) do
         begin
           s:=s+hp.paratype.def.mangledparaname;
           hp:=TParaItem(hp.previous);
           if assigned(hp) then
            s:=s+'$';
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
             'c','w');

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

    constructor tprocvardef.create;
      begin
         inherited create;
         deftype:=procvardef;
      end;


    constructor tprocvardef.load(ppufile:tcompilerppufile);
      begin
         inherited load(ppufile);
         deftype:=procvardef;
      end;


    procedure tprocvardef.write(ppufile:tcompilerppufile);
      begin
         { here we cannot get a real good value so just give something }
         { plausible (PM) }
         { a more secure way would be
           to allways store in a temp }
         if is_fpu(rettype.def) then
{$ifdef FAST_FPU}
           fpu_used:=3
{$else : not FAST_FPU, i.e. SAFE_FPU}
           fpu_used:={2}maxfpuregs
{$endif FAST_FPU}
         else
           fpu_used:=0;
         inherited write(ppufile);
         ppufile.writeentry(ibprocvardef);
      end;


    function tprocvardef.size : longint;
      begin
         if (po_methodpointer in procoptions) then
           size:=2*POINTER_SIZE
         else
           size:=POINTER_SIZE;
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
              pdc:=TParaItem(Para.last)
             else
              pdc:=TParaItem(Para.first);
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
                  pdc:=TParaItem(pdc.previous)
                 else
                  pdc:=TParaItem(pdc.next);
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
         if assigned(rettype.def) and
            (rettype.def<>voidtype.def) then
           s:='<procedure variable type of function'+typename_paras+
             ':'+rettype.def.gettypename
         else
           s:='<procedure variable type of procedure'+typename_paras;
         if po_methodpointer in procoptions then
           s := s+' of object';
         gettypename := s+';'+ProcCallOptionStr[proccalloption]+'>';
      end;


{***************************************************************************
                              TOBJECTDEF
***************************************************************************}

{$ifdef GDB}
    const
       vtabletype : word = 0;
       vtableassigned : boolean = false;
{$endif GDB}

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

        { set up guid }
        isiidguidvalid:=true; { default null guid }
        fillchar(iidguid,sizeof(iidguid),0); { default null guid }
        iidstr:=stringdup(''); { default is empty string }

        { setup implemented interfaces }
        if objecttype in [odt_class,odt_interfacecorba] then
          implementedinterfaces:=timplementedinterfaces.create
        else
          implementedinterfaces:=nil;

{$ifdef GDB}
        writing_class_record_stab:=false;
{$endif GDB}
     end;


    constructor tobjectdef.load(ppufile:tcompilerppufile);
      var
         oldread_member : boolean;
         i,implintfcount: longint;
      begin
         inherited loaddef(ppufile);
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
              isiidguidvalid:=boolean(ppufile.getbyte);
              ppufile.putguid(iidguid);
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
                  implementedinterfaces.addintfref(tdef(ppufile.getderef));
                  implementedinterfaces.ioffsets(i)^:=ppufile.getlongint;
               end;
           end
         else
           implementedinterfaces:=nil;

         oldread_member:=read_member;
         read_member:=true;
         symtable:=tobjectsymtable.create(objrealname^);
         tobjectsymtable(symtable).load(ppufile);
         read_member:=oldread_member;

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
        stringdispose(iidstr);
        if assigned(implementedinterfaces) then
          implementedinterfaces.free;
        inherited destroy;
     end;


    procedure tobjectdef.write(ppufile:tcompilerppufile);
      var
         oldread_member : boolean;
         implintfcount : longint;
         i : longint;
      begin
         inherited writedef(ppufile);
         ppufile.putbyte(byte(objecttype));
         ppufile.putlongint(size);
         ppufile.putlongint(vmt_offset);
         ppufile.putstring(objrealname^);
         ppufile.putderef(childof);
         ppufile.putsmallset(objectoptions);
         if objecttype in [odt_interfacecom,odt_interfacecorba] then
           begin
              ppufile.putbyte(byte(isiidguidvalid));
              ppufile.putguid(iidguid);
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

         oldread_member:=read_member;
         read_member:=true;
         tobjectsymtable(symtable).write(ppufile);
         read_member:=oldread_member;
      end;


    procedure tobjectdef.deref;
      var
         oldrecsyms : tsymtable;
      begin
         inherited deref;
         resolvedef(tdef(childof));
         oldrecsyms:=aktrecordsymtable;
         aktrecordsymtable:=symtable;
         tstoredsymtable(symtable).deref;
         aktrecordsymtable:=oldrecsyms;
         if objecttype in [odt_class,odt_interfacecorba] then
           implementedinterfaces.deref;
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


   procedure tobjectdef._searchdestructor(sym : tnamedindexitem);

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
             symtable.foreach({$ifdef FPCPROCVAR}@{$endif}_searchdestructor);
             if assigned(sd) then
               begin
                  searchdestructor:=sd;
                  exit;
               end;
             o:=o.childof;
          end;
     end;

    function tobjectdef.size : longint;
      begin
        if objecttype in [odt_class,odt_interfacecom,odt_interfacecorba] then
          size:=POINTER_SIZE
        else
          size:=symtable.datasize;
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
        Message1(parser_object_has_no_vmt,objrealname^);
      vmt_mangledname:=mangledname_prefix('VMT',owner)+objname^;
    end;


    function tobjectdef.rtti_name : string;
    begin
      rtti_name:=mangledname_prefix('RTTI',owner)+objname^;
    end;


{$ifdef GDB}
    procedure tobjectdef.addprocname(p :tnamedindexitem);
      var virtualind,argnames : string;
          news, newrec : pchar;
          pd,ipd : tprocdef;
          lindex : longint;
          para : TParaItem;
          arglength : byte;
          sp : char;
          pdl : pprocdeflist;
      begin
        If tsym(p).typ = procsym then
         begin
           pd := tprocsym(p).defs^.def;
           { this will be used for full implementation of object stabs
           not yet done }
           pdl:=tprocsym(p).defs;
           while assigned(pdl) do
            begin
              ipd:=pdl^.def;
              pdl:=pdl^.next;
            end;
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
            symtable.foreach({$ifdef FPCPROCVAR}@{$endif}addname);
            RecOffset:=OldRecOffset;
            if (oo_has_vmt in objectoptions) then
              if not assigned(childof) or not(oo_has_vmt in childof.objectoptions) then
                 begin
                    strpcopy(strend(stabrecstring),'$vf'+classnumberstring+':'+typeglobalnumber('vtblarray')
                      +','+tostr(vmt_offset*8)+';');
                 end;
            symtable.foreach({$ifdef FPCPROCVAR}@{$endif}addprocname);
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


    procedure tobjectdef.count_published_properties(sym:tnamedindexitem);
      begin
         if needs_prop_entry(tsym(sym)) and
          (tsym(sym).typ<>varsym) then
           inc(count);
      end;


    procedure tobjectdef.write_property_info(sym : tnamedindexitem);
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


    procedure tobjectdef.generate_published_child_rtti(sym : tnamedindexitem);
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
             symtable.foreach({$ifdef FPCPROCVAR}@{$endif}generate_field_rtti);
           fullrtti :
             symtable.foreach({$ifdef FPCPROCVAR}@{$endif}generate_published_child_rtti);
           else
             internalerror(200108301);
         end;
      end;


    type
       tclasslistitem = class(tlinkedlistitem)
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


    procedure tobjectdef.count_published_fields(sym:tnamedindexitem);
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


    procedure tobjectdef.writefields(sym:tnamedindexitem);
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
         getdatalabel(fieldtable);
         getdatalabel(classtable);
         count:=0;
         tablecount:=0;
         symtable.foreach({$ifdef FPC}@{$endif}count_published_fields);
         if (cs_create_smart in aktmoduleswitches) then
          rttiList.concat(Tai_cut.Create);
         rttiList.concat(Tai_label.Create(fieldtable));
         rttiList.concat(Tai_const.Create_16bit(count));
         rttiList.concat(Tai_const_symbol.Create(classtable));
         symtable.foreach({$ifdef FPC}@{$endif}writefields);

         { generate the class table }
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
         symtable.foreach({$ifdef FPCPROCVAR}@{$endif}count_published_properties);
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
                  symtable.foreach({$ifdef FPCPROCVAR}@{$endif}count_field_rtti);
                  rttiList.concat(Tai_const.Create_32bit(count));
                  symtable.foreach({$ifdef FPCPROCVAR}@{$endif}write_field_rtti);
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
               symtable.foreach({$ifdef FPCPROCVAR}@{$endif}count_published_properties);
               rttiList.concat(Tai_const.Create_16bit(count));

               { write unit name }
               rttiList.concat(Tai_const.Create_8bit(length(current_module.realmodulename^)));
               rttiList.concat(Tai_string.Create(current_module.realmodulename^));

               { write published properties count }
               count:=0;
               symtable.foreach({$ifdef FPCPROCVAR}@{$endif}count_published_properties);
               rttiList.concat(Tai_const.Create_16bit(count));

               { count is used to write nameindex   }

               { but we need an offset of the owner }
               { to give each property an own slot  }
               if assigned(childof) and (oo_can_have_published in childof.objectoptions) then
                 count:=childof.next_free_name_index
               else
                 count:=0;

               symtable.foreach({$ifdef FPCPROCVAR}@{$endif}write_property_info);
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
            resolvedef(tdef(intf));
      end;

    procedure timplementedinterfaces.addintfref(def: tdef);
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
        tosymname:=s;
        forwardpos:=pos;
     end;


    function tforwarddef.gettypename:string;
      begin
        gettypename:='unresolved forward to '+tosymname;
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
{$endif GDB}

    function terrordef.gettypename:string;

      begin
         gettypename:='<erroneous type>';
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
            { reset rangenr's }
            case def.deftype of
              orddef   : torddef(def).rangenr:=0;
              enumdef  : tenumdef(def).rangenr:=0;
              arraydef : tarraydef(def).rangenr:=0;
            end;
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
  Revision 1.75  2002-04-25 20:16:39  peter
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

  Revision 1.71  2002/04/19 15:46:03  peter
    * mangledname rewrite, tprocdef.mangledname is now created dynamicly
      in most cases and not written to the ppu
    * add mangeledname_prefix() routine to generate the prefix of
      manglednames depending on the current procedure, object and module
    * removed static procprefix since the mangledname is now build only
      on demand from tprocdef.mangledname

  Revision 1.70  2002/04/15 19:06:34  carl
  + target_info.size_of_pointer -> pointer_Size

  Revision 1.69  2002/04/14 16:55:43  carl
  + replace some stuff by ALL_REGISTERS

  Revision 1.68  2002/04/02 17:11:29  peter
    * tlocation,treference update
    * LOC_CONSTANT added for better constant handling
    * secondadd splitted in multiple routines
    * location_force_reg added for loading a location to a register
      of a specified size
    * secondassignment parses now first the right and then the left node
      (this is compatible with Kylix). This saves a lot of push/pop especially
      with string operations
    * adapted some routines to use the new cg methods

  Revision 1.67  2002/03/31 20:26:36  jonas
    + a_loadfpu_* and a_loadmm_* methods in tcg
    * register allocation is now handled by a class and is mostly processor
      independent (+rgobj.pas and i386/rgcpu.pas)
    * temp allocation is now handled by a class (+tgobj.pas, -i386\tgcpu.pas)
    * some small improvements and fixes to the optimizer
    * some register allocation fixes
    * some fpuvaroffset fixes in the unary minus node
    * push/popusedregisters is now called rg.save/restoreusedregisters and
      (for i386) uses temps instead of push/pop's when using -Op3 (that code is
      also better optimizable)
    * fixed and optimized register saving/restoring for new/dispose nodes
    * LOC_FPU locations now also require their "register" field to be set to
      R_ST, not R_ST0 (the latter is used for LOC_CFPUREGISTER locations only)
    - list field removed of the tnode class because it's not used currently
      and can cause hard-to-find bugs

  Revision 1.66  2002/03/24 19:10:14  carl
  + patch for SPARC from Mazen NEIFER

  Revision 1.65  2002/02/04 08:16:07  jonas
    * fixed severe slowdown when compiling a program with arrays that have
      a lot (15+) dimensions ("merged")

  Revision 1.64  2002/01/19 15:12:34  peter
    * check for unresolved forward classes in the interface

  Revision 1.63  2002/01/06 21:52:30  peter

    * fixed previous commit

  Revision 1.62  2002/01/06 12:08:15  peter
    * removed uauto from orddef, use new range_to_basetype generating
      the correct ordinal type for a range

  Revision 1.61  2001/12/19 09:34:51  florian
    * publishing of publishable classes fixed

  Revision 1.60  2001/12/06 17:57:39  florian
    + parasym to tparaitem added

  Revision 1.59  2001/12/03 21:48:42  peter
    * freemem change to value parameter
    * torddef low/high range changed to int64

  Revision 1.58  2001/11/30 15:01:51  jonas
    * tarraydef.size returns POINTER_SIZE instead of 4 for
      dynamic arrays

  Revision 1.57  2001/11/18 18:43:14  peter
    * overloading supported in child classes
    * fixed parsing of classes with private and virtual and overloaded
      so it is compatible with delphi

  Revision 1.56  2001/11/18 18:27:57  florian
    * publishing of qword, int64 and widechar properties is now possible

  Revision 1.55  2001/11/02 22:58:06  peter
    * procsym definition rewrite

  Revision 1.54  2001/10/25 21:22:37  peter
    * calling convention rewrite

  Revision 1.53  2001/10/20 17:21:54  peter
    * fixed size of constset when change from small to normalset

  Revision 1.52  2001/10/15 13:16:26  jonas
    * better size checking of data (now an error is returned for
      "array[longint] of longint") ("merged")

  Revision 1.51  2001/09/19 11:04:42  michael
  * Smartlinking with interfaces fixed
  * Better smartlinking for rtti and init tables

  Revision 1.50  2001/09/17 21:29:12  peter
    * merged netbsd, fpu-overflow from fixes branch

  Revision 1.49  2001/09/10 10:26:27  jonas
    * fixed web bug 1593
    * writing of procvar headers is more complete (mention var/const/out for
      paras, add "of object" if applicable)
    + error if declaring explicit self para as var/const
    * fixed mangled name of procedures which contain an explicit self para
    * parsing para's should be slightly faster because mangled name of
      procedure is only updated once instead of after parsing each para
      (all merged from fixes)

  Revision 1.48  2001/09/03 15:18:38  jonas
    * aded missing reset of labels of rttitablesym and inittablesym labels

  Revision 1.47  2001/09/02 21:18:28  peter
    * split constsym.value in valueord,valueordptr,valueptr. The valueordptr
      is used for holding target platform pointer values. As those can be
      bigger than the source platform.

  Revision 1.46  2001/08/30 20:13:54  peter
    * rtti/init table updates
    * rttisym for reusable global rtti/init info
    * support published for interfaces

  Revision 1.45  2001/08/26 13:36:49  florian
    * some cg reorganisation
    * some PPC updates

  Revision 1.44  2001/08/22 21:16:22  florian
    * some interfaces related problems regarding
      mapping of interface implementions fixed

  Revision 1.43  2001/08/19 09:39:27  peter
    * local browser support fixed

  Revision 1.41  2001/08/12 20:04:33  peter
    * fpu_used=0 when simplify_ppu is used
    * small crc updating fixes for tprocdef

  Revision 1.40  2001/08/06 21:40:48  peter
    * funcret moved from tprocinfo to tprocdef

  Revision 1.39  2001/08/01 21:47:48  peter
    * fixed passing of array of record or shortstring to open array

  Revision 1.38  2001/07/30 20:59:27  peter
    * m68k updates from v10 merged

  Revision 1.37  2001/07/30 11:52:57  jonas
    * fixed web bugs 1563/1564: procvars of object can't be regvars (merged)

  Revision 1.36  2001/07/01 20:16:16  peter
    * alignmentinfo record added
    * -Oa argument supports more alignment settings that can be specified
      per type: PROC,LOOP,VARMIN,VARMAX,CONSTMIN,CONSTMAX,RECORDMIN
      RECORDMAX,LOCALMIN,LOCALMAX. It is possible to set the mimimum
      required alignment and the maximum usefull alignment. The final
      alignment will be choosen per variable size dependent on these
      settings

  Revision 1.35  2001/06/27 21:37:36  peter
    * v10 merges

  Revision 1.34  2001/06/04 18:05:39  peter
    * procdef demangling fixed

  Revision 1.33  2001/06/04 11:53:13  peter
    + varargs directive

  Revision 1.32  2001/05/09 19:58:45  peter
    * m68k doesn't support double (merged)

  Revision 1.31  2001/05/06 14:49:17  peter
    * ppu object to class rewrite
    * move ppu read and write stuff to fppu

  Revision 1.30  2001/04/22 22:46:49  florian
    * more variant support

  Revision 1.29  2001/04/21 12:03:12  peter
    * m68k updates merged from fixes branch

  Revision 1.28  2001/04/18 22:01:58  peter
    * registration of targets and assemblers

  Revision 1.27  2001/04/13 01:22:15  peter
    * symtable change to classes
    * range check generation and errors fixed, make cycle DEBUG=1 works
    * memory leaks fixed

  Revision 1.26  2001/04/05 21:32:22  peter
    * enum stabs fix (merged)

  Revision 1.25  2001/04/04 21:30:45  florian
    * applied several fixes to get the DD8 Delphi Unit compiled
     e.g. "forward"-interfaces are working now

  Revision 1.24  2001/04/02 21:20:34  peter
    * resulttype rewrite

  Revision 1.23  2001/03/22 23:28:39  florian
    * correct initialisation of rec_tguid when loading the system unit

  Revision 1.22  2001/03/22 00:10:58  florian
    + basic variant type support in the compiler

  Revision 1.21  2001/03/11 22:58:50  peter
    * getsym redesign, removed the globals srsym,srsymtable

  Revision 1.20  2001/01/06 20:11:29  peter
    * merged c packrecords fix

  Revision 1.19  2000/12/25 00:07:29  peter
    + new tlinkedlist class (merge of old tstringqueue,tcontainer and
      tlinkedlist objects)

  Revision 1.18  2000/12/24 12:20:45  peter
    * classes, enum stabs fixes merged from 1.0.x

  Revision 1.17  2000/12/07 17:19:43  jonas
    * new constant handling: from now on, hex constants >$7fffffff are
      parsed as unsigned constants (otherwise, $80000000 got sign extended
      and became $ffffffff80000000), all constants in the longint range
      become longints, all constants >$7fffffff and <=cardinal($ffffffff)
      are cardinals and the rest are int64's.
    * added lots of longint typecast to prevent range check errors in the
      compiler and rtl
    * type casts of symbolic ordinal constants are now preserved
    * fixed bug where the original resulttype.def wasn't restored correctly
      after doing a 64bit rangecheck

  Revision 1.16  2000/11/30 23:12:57  florian
  * if raw interfaces inherit from IUnknown they are ref. counted too

  Revision 1.15  2000/11/29 00:30:40  florian
    * unused units removed from uses clause
    * some changes for widestrings

  Revision 1.14  2000/11/28 00:28:06  pierre
   * stabs fixing

  Revision 1.13  2000/11/26 18:09:40  florian
    * fixed rtti for chars

  Revision 1.12  2000/11/19 16:23:35  florian
  *** empty log message ***

  Revision 1.11  2000/11/12 23:24:12  florian
    * interfaces are basically running

  Revision 1.10  2000/11/11 16:12:38  peter
    * add far; to typename for far pointer

  Revision 1.9  2000/11/07 20:01:57  peter
    * fix vmt index for classes

  Revision 1.8  2000/11/06 23:13:53  peter
    * uppercase manglednames

  Revision 1.7  2000/11/06 23:11:38  florian
    * writeln debugger uninstalled ;)

  Revision 1.6  2000/11/06 23:05:52  florian
    * more fixes

  Revision 1.5  2000/11/06 20:30:55  peter
    * more fixes to get make cycle working

  Revision 1.4  2000/11/04 14:25:22  florian
    + merged Attila's changes for interfaces, not tested yet

  Revision 1.3  2000/11/02 12:04:10  pierre
  * remove RecOffset code, that created problems

  Revision 1.2  2000/11/01 23:04:38  peter
    * tprocdef.fullprocname added for better casesensitve writing of
      procedures

  Revision 1.1  2000/10/31 22:02:52  peter
    * symtable splitted, no real code changes

}
