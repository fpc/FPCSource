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
       cutils,cobjects,cclasses,
       { global }
       globtype,globals,tokens,
       { symtable }
       symconst,symbase,symtype,
       { node }
       node,
       { aasm }
       aasm,cpubase
       ;


    type
{************************************************
                    TDef
************************************************}

       pstoreddef = ^tstoreddef;
       tstoreddef = object(tdef)
          has_inittable : boolean;
          { adress of init informations }
          inittable_label : pasmlabel;

          has_rtti   : boolean;
          { address of rtti }
          rtti_label : pasmlabel;

          nextglobal,
          previousglobal : pstoreddef;
{$ifdef GDB}
          globalnb       : word;
          is_def_stab_written : tdefstabstatus;
{$endif GDB}
          constructor init;
          constructor load;
          destructor  done;virtual;
          procedure write;virtual;
          function  size:longint;virtual;
          function  alignment:longint;virtual;
          function  is_publishable : boolean;virtual;
          function  is_in_current : boolean;
          { debug }
{$ifdef GDB}
          function  stabstring : pchar;virtual;
          procedure concatstabto(asmlist : taasmoutput);virtual;
          function  NumberString:string;
          procedure set_globalnb;virtual;
          function  allstabstring : pchar;virtual;
{$endif GDB}
          { init. tables }
          function  needs_inittable : boolean;virtual;
          procedure generate_inittable;
          function  get_inittable_label : pasmlabel;
          { the default implemenation calls write_rtti_data     }
          { if init and rtti data is different these procedures }
          { must be overloaded                                  }
          procedure write_init_data;virtual;
          procedure write_child_init_data;virtual;
          { rtti }
          procedure write_rtti_name;
          function  get_rtti_label : string;virtual;
          procedure generate_rtti;virtual;
          procedure write_rtti_data;virtual;
          procedure write_child_rtti_data;virtual;
          function is_intregable : boolean;
          function is_fpuregable : boolean;
       private
          savesize  : longint;
       end;

       targconvtyp = (act_convertable,act_equal,act_exact);

       tvarspez = (vs_value,vs_const,vs_var,vs_out);

       tparaitem = class(tlinkedlistitem)
          paratype     : ttype;
          paratyp      : tvarspez;
          argconvtyp   : targconvtyp;
          convertlevel : byte;
          register     : tregister;
          defaultvalue : psym; { pconstsym }
       end;

       { this is only here to override the count method,
         which can't be used }
       tparalinkedlist = class(tlinkedlist)
          function count:longint;
       end;

       tfiletyp = (ft_text,ft_typed,ft_untyped);

       pfiledef = ^tfiledef;
       tfiledef = object(tstoreddef)
          filetyp : tfiletyp;
          typedfiletype : ttype;
          constructor inittext;
          constructor inituntyped;
          constructor inittyped(const tt : ttype);
          constructor inittypeddef(p : pdef);
          constructor load;
          procedure write;virtual;
          procedure deref;virtual;
          function  gettypename:string;virtual;
          procedure setsize;
          { debug }
{$ifdef GDB}
          function  stabstring : pchar;virtual;
          procedure concatstabto(asmlist : taasmoutput);virtual;
{$endif GDB}
       end;

       pformaldef = ^tformaldef;
       tformaldef = object(tstoreddef)
          constructor init;
          constructor load;
          procedure write;virtual;
          function  gettypename:string;virtual;
{$ifdef GDB}
          function  stabstring : pchar;virtual;
          procedure concatstabto(asmlist : taasmoutput);virtual;
{$endif GDB}
       end;

       pforwarddef = ^tforwarddef;
       tforwarddef = object(tstoreddef)
          tosymname : string;
          forwardpos : tfileposinfo;
          constructor init(const s:string;const pos : tfileposinfo);
          function  gettypename:string;virtual;
       end;

       perrordef = ^terrordef;
       terrordef = object(tstoreddef)
          constructor init;
          function  gettypename:string;virtual;
          { debug }
{$ifdef GDB}
          function  stabstring : pchar;virtual;
{$endif GDB}
       end;

       { tpointerdef and tclassrefdef should get a common
         base class, but I derived tclassrefdef from tpointerdef
         to avoid problems with bugs (FK)
       }

       ppointerdef = ^tpointerdef;
       tpointerdef = object(tstoreddef)
          pointertype : ttype;
          is_far : boolean;
          constructor init(const tt : ttype);
          constructor initfar(const tt : ttype);
          constructor initdef(p : pdef);
          constructor initfardef(p : pdef);
          constructor load;
          destructor  done;virtual;
          procedure write;virtual;
          procedure deref;virtual;
          function  gettypename:string;virtual;
          { debug }
{$ifdef GDB}
          function  stabstring : pchar;virtual;
          procedure concatstabto(asmlist : taasmoutput);virtual;
{$endif GDB}
       end;

       pprocdef = ^tprocdef;

       pimplementedinterfaces = ^timplementedinterfaces;

       pobjectdef = ^tobjectdef;
       tobjectdef = object(tstoreddef)
          childof  : pobjectdef;
          objname  : pstring;
          symtable : psymtable;
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
          implementedinterfaces: pimplementedinterfaces;
          constructor init(ot : tobjectdeftype;const n : string;c : pobjectdef);
          constructor load;
          destructor  done;virtual;
          procedure write;virtual;
          procedure deref;virtual;
          function  size : longint;virtual;
          function  alignment:longint;virtual;
          function  getsymtable(t:tgetsymtable):psymtable;virtual;
          function  vmtmethodoffset(index:longint):longint;
          function  is_publishable : boolean;virtual;
          function  vmt_mangledname : string;
          function  rtti_name : string;
          procedure check_forwards;
          function  is_related(d : pobjectdef) : boolean;
          function  next_free_name_index : longint;
          procedure insertvmt;
          procedure set_parent(c : pobjectdef);
          function searchdestructor : pprocdef;
          { debug }
{$ifdef GDB}
          function stabstring : pchar;virtual;
          procedure set_globalnb;virtual;
          function  classnumberstring : string;
          procedure concatstabto(asmlist : taasmoutput);virtual;
          function  allstabstring : pchar;virtual;
{$endif GDB}
          { init/final }
          function  needs_inittable : boolean;virtual;
          procedure write_init_data;virtual;
          procedure write_child_init_data;virtual;
          { rtti }
          function  get_rtti_label : string;virtual;
          procedure generate_rtti;virtual;
          procedure write_rtti_data;virtual;
          procedure write_child_rtti_data;virtual;
          function generate_field_table : pasmlabel;
       end;

       timplementedinterfaces = object
          constructor init;
          destructor  done; virtual;

          function  count: longint;
          function  interfaces(intfindex: longint): pobjectdef;
          function  ioffsets(intfindex: longint): plongint;
          function  searchintf(def: pdef): longint;
          procedure addintf(def: pdef);

          procedure deref;
          procedure addintfref(def: pdef);

          procedure clearmappings;
          procedure addmappings(intfindex: longint; const name, newname: string);
          function  getmappings(intfindex: longint; const name: string; var nextexist: pointer): string;

          procedure clearimplprocs;
          procedure addimplproc(intfindex: longint; procdef: pprocdef);
          function  implproccount(intfindex: longint): longint;
          function  implprocs(intfindex: longint; procindex: longint): pprocdef;
          function  isimplmergepossible(intfindex, remainindex: longint; var weight: longint): boolean;

       private
          finterfaces: tindexarray;
          procedure checkindex(intfindex: longint);
       end;


       pclassrefdef = ^tclassrefdef;
       tclassrefdef = object(tpointerdef)
          constructor init(def : pdef);
          constructor load;
          procedure write;virtual;
          function gettypename:string;virtual;
          { debug }
{$ifdef GDB}
          function stabstring : pchar;virtual;
          procedure concatstabto(asmlist : taasmoutput);virtual;
{$endif GDB}
       end;

       parraydef = ^tarraydef;
       tarraydef = object(tstoreddef)
          rangenr    : longint;
          lowrange,
          highrange  : longint;
          elementtype,
          rangetype  : ttype;
          IsDynamicArray,
          IsVariant,
          IsConstructor,
          IsArrayOfConst : boolean;
          function gettypename:string;virtual;
          function elesize : longint;
          constructor init(l,h : longint;rd : pdef);
          constructor load;
          procedure write;virtual;
{$ifdef GDB}
          function stabstring : pchar;virtual;
          procedure concatstabto(asmlist : taasmoutput);virtual;
{$endif GDB}
          procedure deref;virtual;
          function size : longint;virtual;
          function alignment : longint;virtual;
          { generates the ranges needed by the asm instruction BOUND (i386)
            or CMP2 (Motorola) }
          procedure genrangecheck;

          { returns the label of the range check string }
          function getrangecheckstring : string;
          function needs_inittable : boolean;virtual;
          procedure write_rtti_data;virtual;
          procedure write_child_rtti_data;virtual;
       end;

       precorddef = ^trecorddef;
       trecorddef = object(tstoreddef)
          symtable : psymtable;
          constructor init(p : psymtable);
          constructor load;
          destructor done;virtual;
          procedure write;virtual;
          procedure deref;virtual;
          function  size:longint;virtual;
          function  alignment : longint;virtual;
          function  gettypename:string;virtual;
          function  getsymtable(t:tgetsymtable):psymtable;virtual;
          { debug }
{$ifdef GDB}
          function  stabstring : pchar;virtual;
          procedure concatstabto(asmlist : taasmoutput);virtual;
{$endif GDB}
          { init/final }
          procedure write_init_data;virtual;
          procedure write_child_init_data;virtual;
          function  needs_inittable : boolean;virtual;
          { rtti }
          procedure write_rtti_data;virtual;
          procedure write_child_rtti_data;virtual;
       end;

       porddef = ^torddef;
       torddef = object(tstoreddef)
          rangenr  : longint;
          low,high : longint;
          typ      : tbasetype;
          constructor init(t : tbasetype;v,b : longint);
          constructor load;
          procedure write;virtual;
          function  is_publishable : boolean;virtual;
          function  gettypename:string;virtual;
          procedure setsize;
          { generates the ranges needed by the asm instruction BOUND }
          { or CMP2 (Motorola)                                       }
          procedure genrangecheck;
          function  getrangecheckstring : string;
          { debug }
{$ifdef GDB}
          function  stabstring : pchar;virtual;
{$endif GDB}
          { rtti }
          procedure write_rtti_data;virtual;
       end;

       pfloatdef = ^tfloatdef;
       tfloatdef = object(tstoreddef)
          typ : tfloattype;
          constructor init(t : tfloattype);
          constructor load;
          procedure write;virtual;
          function  gettypename:string;virtual;
          function  is_publishable : boolean;virtual;
          procedure setsize;
          { debug }
{$ifdef GDB}
          function stabstring : pchar;virtual;
{$endif GDB}
          { rtti }
          procedure write_rtti_data;virtual;
       end;

       pabstractprocdef = ^tabstractprocdef;
       tabstractprocdef = object(tstoreddef)
          { saves a definition to the return type }
          rettype         : ttype;
          proctypeoption  : tproctypeoption;
          proccalloptions : tproccalloptions;
          procoptions     : tprocoptions;
          para            : tparalinkedlist;
          maxparacount,
          minparacount    : longint;
          symtablelevel   : byte;
          fpu_used        : byte;    { how many stack fpu must be empty }
          constructor init;
          constructor load;
          destructor done;virtual;
          procedure  write;virtual;
          procedure deref;virtual;
          procedure concatpara(tt:ttype;vsp : tvarspez;defval:psym);
          function  para_size(alignsize:longint) : longint;
          function  demangled_paras : string;
          function  proccalloption2str : string;
          procedure test_if_fpu_result;
          { debug }
{$ifdef GDB}
          function  stabstring : pchar;virtual;
          procedure concatstabto(asmlist : taasmoutput);virtual;
{$endif GDB}
       end;

       pprocvardef = ^tprocvardef;
       tprocvardef = object(tabstractprocdef)
          constructor init;
          constructor load;
          procedure write;virtual;
          function  size : longint;virtual;
          function gettypename:string;virtual;
          function is_publishable : boolean;virtual;
          { debug }
{$ifdef GDB}
          function stabstring : pchar;virtual;
          procedure concatstabto(asmlist : taasmoutput); virtual;
{$endif GDB}
          { rtti }
          procedure write_child_rtti_data;virtual;
          procedure write_rtti_data;virtual;
       end;

       tmessageinf = record
         case integer of
           0 : (str : pchar);
           1 : (i : longint);
       end;

       tprocdef = object(tabstractprocdef)
       private
          _mangledname : pstring;
       public
          extnumber  : longint;
          messageinf : tmessageinf;
          nextoverloaded : pprocdef;
          { where is this function defined, needed here because there
            is only one symbol for all overloaded functions }
          fileinfo : tfileposinfo;
          { symbol owning this definition }
          procsym : psym;
          { alias names }
          aliasnames : tstringlist;
          { symtables }
          parast,
          localst : psymtable;
          { browser info }
          lastref,
          defref,
          crossref,
          lastwritten : pref;
          refcount : longint;
          _class : pobjectdef;
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
          count      : boolean;
          is_used    : boolean;
          { small set which contains the modified registers }
{$ifdef newcg}
          usedregisters : tregisterset;
{$else newcg}
          usedregisters : longint;
{$endif newcg}
          constructor init;
          constructor load;
          destructor  done;virtual;
          procedure write;virtual;
          procedure deref;virtual;
          function  getsymtable(t:tgetsymtable):psymtable;virtual;
          function  haspara:boolean;
          function  mangledname : string;
          procedure setmangledname(const s : string);
          procedure load_references;
          function  write_references : boolean;
          function  fullprocname:string;
          function fullprocnamewithret:string;
          function  cplusplusmangledname : string;
          { debug }
{$ifdef GDB}
          function  stabstring : pchar;virtual;
          procedure concatstabto(asmlist : taasmoutput);virtual;
{$endif GDB}
       end;

       pstringdef = ^tstringdef;
       tstringdef = object(tstoreddef)
          string_typ : tstringtype;
          len        : longint;
          constructor shortinit(l : byte);
          constructor shortload;
          constructor longinit(l : longint);
          constructor longload;
          constructor ansiinit(l : longint);
          constructor ansiload;
          constructor wideinit(l : longint);
          constructor wideload;
          function  stringtypname:string;
          function  size : longint;virtual;
          procedure write;virtual;
          function  gettypename:string;virtual;
          function  is_publishable : boolean;virtual;
          { debug }
{$ifdef GDB}
          function  stabstring : pchar;virtual;
          procedure concatstabto(asmlist : taasmoutput);virtual;
{$endif GDB}
          { init/final }
          function  needs_inittable : boolean;virtual;
          { rtti }
          procedure write_rtti_data;virtual;
       end;

       penumdef = ^tenumdef;
       tenumdef = object(tstoreddef)
          rangenr,
          minval,
          maxval    : longint;
          has_jumps : boolean;
          firstenum : psym;  {penumsym}
          basedef   : penumdef;
          constructor init;
          constructor init_subrange(_basedef:penumdef;_min,_max:longint);
          constructor load;
          destructor done;virtual;
          procedure write;virtual;
          procedure deref;virtual;
          function  gettypename:string;virtual;
          function  is_publishable : boolean;virtual;
          procedure calcsavesize;
          procedure setmax(_max:longint);
          procedure setmin(_min:longint);
          function  min:longint;
          function  max:longint;
          function  getrangecheckstring:string;
          procedure genrangecheck;
          { debug }
{$ifdef GDB}
          function stabstring : pchar;virtual;
{$endif GDB}
          { rtti }
          procedure write_child_rtti_data;virtual;
          procedure write_rtti_data;virtual;
       private
          procedure correct_owner_symtable;
       end;

       psetdef = ^tsetdef;
       tsetdef = object(tstoreddef)
          elementtype : ttype;
          settype : tsettype;
          constructor init(s : pdef;high : longint);
          constructor load;
          destructor  done;virtual;
          procedure write;virtual;
          procedure deref;virtual;
          function  gettypename:string;virtual;
          function  is_publishable : boolean;virtual;
          { debug }
{$ifdef GDB}
          function  stabstring : pchar;virtual;
          procedure concatstabto(asmlist : taasmoutput);virtual;
{$endif GDB}
          { rtti }
          procedure write_rtti_data;virtual;
          procedure write_child_rtti_data;virtual;
       end;


    var
       aktobjectdef : pobjectdef;  { used for private functions check !! }

       firstglobaldef,               { linked list of all globals defs }
       lastglobaldef : pstoreddef;   { used to reset stabs/ranges }

{$ifdef GDB}
       { for STAB debugging }
       globaltypecount  : word;
       pglobaltypecount : pword;
{$endif GDB}

    { default types }
       generrordef : pdef;       { error in definition }

       voidpointerdef : ppointerdef; { pointer for Void-Pointerdef }
       charpointerdef : ppointerdef; { pointer for Char-Pointerdef }
       voidfarpointerdef : ppointerdef;

       cformaldef : pformaldef;    { unique formal definition }
       voiddef   : porddef;        { Pointer to Void (procedure) }
       cchardef  : porddef;        { Pointer to Char }
       cwidechardef : porddef;     { Pointer to WideChar }
       booldef   : porddef;        { pointer to boolean type }
       u8bitdef  : porddef;        { Pointer to 8-Bit unsigned }
       u16bitdef : porddef;        { Pointer to 16-Bit unsigned }
       u32bitdef : porddef;        { Pointer to 32-Bit unsigned }
       s32bitdef : porddef;        { Pointer to 32-Bit signed }

       cu64bitdef : porddef;       { pointer to 64 bit unsigned def }
       cs64bitdef : porddef;       { pointer to 64 bit signed def, }
                                   { calculated by the int unit on i386 }

       s32floatdef : pfloatdef;    { pointer for realconstn }
       s64floatdef : pfloatdef;    { pointer for realconstn }
       s80floatdef : pfloatdef;    { pointer to type of temp. floats }
       s32fixeddef : pfloatdef;    { pointer to type of temp. fixed }

       cshortstringdef : pstringdef;     { pointer to type of short string const   }
       clongstringdef  : pstringdef;     { pointer to type of long string const   }
       cansistringdef  : pstringdef;     { pointer to type of ansi string const  }
       cwidestringdef  : pstringdef;     { pointer to type of wide string const  }
       openshortstringdef : pstringdef;  { pointer to type of an open shortstring,
                                           needed for readln() }
       openchararraydef : parraydef;     { pointer to type of an open array of char,
                                            needed for readln() }

       cfiledef : pfiledef;       { get the same definition for all file }
                                  { used for stabs }

       class_tobject : pobjectdef;   { pointer to the anchestor of all classes }
       interface_iunknown : pobjectdef; { KAZ: pointer to the ancestor }
       rec_tguid : precorddef;          { KAZ: pointer to the TGUID type }
                                        { of all interfaces            }
       pvmtdef       : ppointerdef;  { type of classrefs }

    const
{$ifdef i386}
       bestrealdef : ^pfloatdef = @s80floatdef;
{$endif}
{$ifdef m68k}
       bestrealdef : ^pfloatdef = @s64floatdef;
{$endif}
{$ifdef alpha}
       bestrealdef : ^pfloatdef = @s64floatdef;
{$endif}
{$ifdef powerpc}
       bestrealdef : ^pfloatdef = @s64floatdef;
{$endif}
{$ifdef ia64}
       bestrealdef : ^pfloatdef = @s64floatdef;
{$endif}

{$ifdef GDB}
    { GDB Helpers }
    function typeglobalnumber(const s : string) : string;
{$endif GDB}

    { should be in the types unit, but the types unit uses the node stuff :( }
    function is_interfacecom(def: pdef): boolean;
    function is_interfacecorba(def: pdef): boolean;
    function is_interface(def: pdef): boolean;
    function is_class(def: pdef): boolean;
    function is_object(def: pdef): boolean;
    function is_cppclass(def: pdef): boolean;
    function is_class_or_interface(def: pdef): boolean;

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
       systems,cpuinfo,
       { symtable }
       symsym,symtable,
       types,
       { ppu }
       ppu,symppu,
       { module }
{$ifdef GDB}
       gdb,
{$endif GDB}
       fmodule,
       { other }
       gendef
       ;

    const
       memsizeinc = 2048; { for long stabstrings }

{****************************************************************************
                                  Helpers
****************************************************************************}

{$ifdef GDB}
    procedure forcestabto(asmlist : taasmoutput; pd : pdef);
      begin
        if pstoreddef(pd)^.is_def_stab_written = not_written then
         begin
           if assigned(pd^.typesym) then
            ptypesym(pd^.typesym)^.isusedinstab := true;
           pstoreddef(pd)^.concatstabto(asmlist);
         end;
      end;
{$endif GDB}


{****************************************************************************
                     TDEF (base class for definitions)
****************************************************************************}

    constructor tstoreddef.init;
      begin
         inherited init;
         savesize := 0;
         has_rtti:=false;
         has_inittable:=false;
         if registerdef then
           symtablestack^.registerdef(@self);
{$ifdef GDB}
         is_def_stab_written := not_written;
         globalnb := 0;
{$endif GDB}
         if assigned(lastglobaldef) then
           begin
              lastglobaldef^.nextglobal := @self;
              previousglobal:=lastglobaldef;
           end
         else
           begin
              firstglobaldef := @self;
              previousglobal := nil;
           end;
         lastglobaldef := @self;
         nextglobal := nil;
      end;

{$ifdef MEMDEBUG}
   var
       manglenamesize : longint;
{$endif}

    constructor tstoreddef.load;
      begin
         inherited init;
         has_rtti:=false;
         has_inittable:=false;
{$ifdef GDB}
         is_def_stab_written := not_written;
         globalnb := 0;
{$endif GDB}
         if assigned(lastglobaldef) then
           begin
              lastglobaldef^.nextglobal := @self;
              previousglobal:=lastglobaldef;
           end
         else
           begin
              firstglobaldef := @self;
              previousglobal:=nil;
           end;
         lastglobaldef := @self;
         nextglobal := nil;
      { load }
         indexnr:=readword;
         typesym:=ptypesym(readderef);
      end;


    destructor tstoreddef.done;
      begin
         { first element  ? }
         if not(assigned(previousglobal)) then
           begin
              firstglobaldef := nextglobal;
              if assigned(firstglobaldef) then
                firstglobaldef^.previousglobal:=nil;
           end
         else
           begin
              { remove reference in the element before }
              previousglobal^.nextglobal:=nextglobal;
           end;
         { last element ? }
         if not(assigned(nextglobal)) then
           begin
              lastglobaldef := previousglobal;
              if assigned(lastglobaldef) then
                lastglobaldef^.nextglobal:=nil;
           end
         else
           nextglobal^.previousglobal:=previousglobal;
         previousglobal:=nil;
         nextglobal:=nil;
{$ifdef SYNONYM}
         while assigned(typesym) do
           begin
              ptypesym(typesym)^.restype.setdef(nil);
              typesym:=ptypesym(typesym)^.synonym;
           end;
{$endif}
      end;


    function tstoreddef.is_in_current : boolean;
      var
        p : psymtable;
      begin
         p:=owner;
         is_in_current:=false;
         while assigned(p) do
           begin
              if (p=current_module.globalsymtable) or (p=current_module.localsymtable)
                 or (p^.symtabletype in [globalsymtable,staticsymtable]) then
                begin
                   is_in_current:=true;
                   exit;
                end
              else if p^.symtabletype in [localsymtable,parasymtable,objectsymtable] then
                begin
                  if assigned(p^.defowner) then
                    p:=pobjectdef(p^.defowner)^.owner
                  else
                    exit;
                end
              else
                exit;
           end;

      end;

    procedure tstoreddef.write;
      begin
        writeword(indexnr);
        writederef(typesym);
{$ifdef GDB}
        if globalnb = 0 then
          begin
            if assigned(owner) then
              globalnb := owner^.getnewtypecount
            else
              begin
                globalnb := PGlobalTypeCount^;
                Inc(PGlobalTypeCount^);
              end;
           end;
{$endif GDB}
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
      var table : psymtable;
      begin
      {formal def have no type !}
      if deftype = formaldef then
        begin
        numberstring := voiddef^.numberstring;
        exit;
        end;
      if (not assigned(typesym)) or (not ptypesym(typesym)^.isusedinstab) then
        begin
           {set even if debuglist is not defined}
           if assigned(typesym) then
             ptypesym(typesym)^.isusedinstab := true;
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
                  globalnb := owner^.getnewtypecount
                else
                  begin
                     globalnb := PGlobalTypeCount^;
                     Inc(PGlobalTypeCount^);
                  end;
             end;
           if assigned(typesym) then
             begin
                table := ptypesym(typesym)^.owner;
                if table^.unitid > 0 then
                  numberstring := '('+tostr(table^.unitid)+','+tostr(pstoreddef(ptypesym(typesym)^.restype.def)^.globalnb)+')'
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
           sname := ptypesym(typesym)^.name;
           sym_line_no:=ptypesym(typesym)^.fileinfo.line;
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
    if ((typesym = nil) or ptypesym(typesym)^.isusedinstab or (cs_gdb_dbx in aktglobalswitches))
      and (is_def_stab_written = not_written) then
      begin
      If cs_gdb_dbx in aktglobalswitches then
        begin
           { otherwise you get two of each def }
           If assigned(typesym) then
             begin
                if ptypesym(typesym)^.typ=symconst.typesym then
                  ptypesym(typesym)^.isusedinstab:=true;
                if (ptypesym(typesym)^.owner = nil) or
                  ((ptypesym(typesym)^.owner^.symtabletype = unitsymtable) and
                 punitsymtable(ptypesym(typesym)^.owner)^.dbx_count_ok)  then
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


    { rtti generation }
    procedure tstoreddef.generate_rtti;
      begin
         if not has_rtti then
          begin
            has_rtti:=true;
            getdatalabel(rtti_label);
            write_child_rtti_data;
            rttiList.concat(Tai_symbol.Create(rtti_label,0));
            write_rtti_data;
            rttiList.concat(Tai_symbol_end.Create(rtti_label));
          end;
      end;


    function tstoreddef.get_rtti_label : string;
      begin
         generate_rtti;
         get_rtti_label:=rtti_label^.name;
      end;


    { init table handling }
    function tstoreddef.needs_inittable : boolean;
      begin
         needs_inittable:=false;
      end;


    procedure tstoreddef.generate_inittable;
      begin
         has_inittable:=true;
         getdatalabel(inittable_label);
         write_child_init_data;
         rttiList.concat(Tai_label.Create(inittable_label));
         write_init_data;
      end;


    procedure tstoreddef.write_init_data;
      begin
         write_rtti_data;
      end;


    procedure tstoreddef.write_child_init_data;
      begin
         write_child_rtti_data;
      end;


    function tstoreddef.get_inittable_label : pasmlabel;
      begin
         if not(has_inittable) then
           generate_inittable;
         get_inittable_label:=inittable_label;
      end;


    procedure tstoreddef.write_rtti_name;
      var
         str : string;
      begin
         { name }
         if assigned(typesym) then
           begin
              str:=ptypesym(typesym)^.realname;
              rttiList.concat(Tai_string.Create(chr(length(str))+str));
           end
         else
           rttiList.concat(Tai_string.Create(#0))
      end;


    { returns true, if the definition can be published }
    function tstoreddef.is_publishable : boolean;
      begin
         is_publishable:=false;
      end;


    procedure tstoreddef.write_rtti_data;
      begin
      end;


    procedure tstoreddef.write_child_rtti_data;
      begin
      end;


   function tstoreddef.is_intregable : boolean;

     begin
        is_intregable:=false;
        case deftype of
          pointerdef,
          enumdef,
          procvardef :
            is_intregable:=true;
          orddef :
            case porddef(@self)^.typ of
              bool8bit,bool16bit,bool32bit,
              u8bit,u16bit,u32bit,
              s8bit,s16bit,s32bit:
                is_intregable:=true;
            end;
          setdef:
            is_intregable:=(psetdef(@self)^.settype=smallset);
        end;
     end;

   function tstoreddef.is_fpuregable : boolean;

     begin
        is_fpuregable:=(deftype=floatdef) and not(pfloatdef(@self)^.typ in [f32bit,f16bit]);
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
                               TSTRINGDEF
****************************************************************************}

    constructor tstringdef.shortinit(l : byte);
      begin
         inherited init;
         string_typ:=st_shortstring;
         deftype:=stringdef;
         len:=l;
         savesize:=len+1;
      end;


    constructor tstringdef.shortload;
      begin
         inherited load;
         string_typ:=st_shortstring;
         deftype:=stringdef;
         len:=readbyte;
         savesize:=len+1;
      end;


    constructor tstringdef.longinit(l : longint);
      begin
         inherited init;
         string_typ:=st_longstring;
         deftype:=stringdef;
         len:=l;
         savesize:=target_os.size_of_pointer;
      end;


    constructor tstringdef.longload;
      begin
         inherited load;
         deftype:=stringdef;
         string_typ:=st_longstring;
         len:=readlong;
         savesize:=target_os.size_of_pointer;
      end;


    constructor tstringdef.ansiinit(l : longint);
      begin
         inherited init;
         string_typ:=st_ansistring;
         deftype:=stringdef;
         len:=l;
         savesize:=target_os.size_of_pointer;
      end;


    constructor tstringdef.ansiload;
      begin
         inherited load;
         deftype:=stringdef;
         string_typ:=st_ansistring;
         len:=readlong;
         savesize:=target_os.size_of_pointer;
      end;


    constructor tstringdef.wideinit(l : longint);
      begin
         inherited init;
         string_typ:=st_widestring;
         deftype:=stringdef;
         len:=l;
         savesize:=target_os.size_of_pointer;
      end;


    constructor tstringdef.wideload;
      begin
         inherited load;
         deftype:=stringdef;
         string_typ:=st_widestring;
         len:=readlong;
         savesize:=target_os.size_of_pointer;
      end;


    function tstringdef.stringtypname:string;
      const
        typname:array[tstringtype] of string[8]=('',
          'SHORTSTR','LONGSTR','ANSISTR','WIDESTR'
        );
      begin
        stringtypname:=typname[string_typ];
      end;


    function tstringdef.size : longint;
      begin
        size:=savesize;
      end;


    procedure tstringdef.write;
      begin
         inherited write;
         if string_typ=st_shortstring then
           writebyte(len)
         else
           writelong(len);
         case string_typ of
           st_shortstring : current_ppu^.writeentry(ibshortstringdef);
            st_longstring : current_ppu^.writeentry(iblongstringdef);
            st_ansistring : current_ppu^.writeentry(ibansistringdef);
            st_widestring : current_ppu^.writeentry(ibwidestringdef);
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

    procedure tstringdef.write_rtti_data;
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


    function tstringdef.is_publishable : boolean;
      begin
         is_publishable:=true;
      end;


{****************************************************************************
                                 TENUMDEF
****************************************************************************}

    constructor tenumdef.init;
      begin
         inherited init;
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

    constructor tenumdef.init_subrange(_basedef:penumdef;_min,_max:longint);
      begin
         inherited init;
         deftype:=enumdef;
         minval:=_min;
         maxval:=_max;
         basedef:=_basedef;
         calcsavesize;
         has_jumps:=false;
         rangenr:=0;
         firstenum:=basedef^.firstenum;
         while assigned(firstenum) and (penumsym(firstenum)^.value<>minval) do
          firstenum:=penumsym(firstenum)^.nextenum;
         correct_owner_symtable;
      end;


    constructor tenumdef.load;
      begin
         inherited load;
         deftype:=enumdef;
         basedef:=penumdef(readderef);
         minval:=readlong;
         maxval:=readlong;
         savesize:=readlong;
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
        resolvedef(pdef(basedef));
      end;


    destructor tenumdef.done;
      begin
        inherited done;
      end;


    procedure tenumdef.write;
      begin
         inherited write;
         writederef(basedef);
         writelong(min);
         writelong(max);
         writelong(savesize);
         current_ppu^.writeentry(ibenumdef);
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
         st : psymtable;
      begin
         if assigned(owner) and
            (owner^.symtabletype in [recordsymtable,objectsymtable]) then
           begin
              owner^.defindex^.deleteindex(@self);
              st:=owner;
              while (st^.symtabletype in [recordsymtable,objectsymtable]) do
                st:=st^.next;
              st^.registerdef(@self);
           end;
      end;



{$ifdef GDB}
    function tenumdef.stabstring : pchar;
      var st,st2 : pchar;
          p : penumsym;
          s : string;
          memsize : word;
      begin
        memsize := memsizeinc;
        getmem(st,memsize);
        { we can specify the size with @s<size>; prefix PM }
        if savesize <> target_os.size_of_longint then
          strpcopy(st,'@s'+tostr(savesize)+';e')
        else
          strpcopy(st,'e');
        p := penumsym(firstenum);
        while assigned(p) do
          begin
            s :=p^.name+':'+tostr(p^.value)+',';
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
            p := p^.nextenum;
          end;
        strpcopy(strend(st),';');
        stabstring := strnew(st);
        freemem(st,memsize);
      end;
{$endif GDB}


    procedure tenumdef.write_child_rtti_data;
      begin
         if assigned(basedef) then
           basedef^.get_rtti_label;
      end;


    procedure tenumdef.write_rtti_data;

      var
         hp : penumsym;

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
           rttiList.concat(Tai_const_symbol.Createname(basedef^.get_rtti_label))
         else
           rttiList.concat(Tai_const.Create_32bit(0));
         hp:=penumsym(firstenum);
         while assigned(hp) do
           begin
              rttiList.concat(Tai_const.Create_8bit(length(hp^.name)));
              rttiList.concat(Tai_string.Create(lower(hp^.name)));
              hp:=hp^.nextenum;
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

    constructor torddef.init(t : tbasetype;v,b : longint);
      begin
         inherited init;
         deftype:=orddef;
         low:=v;
         high:=b;
         typ:=t;
         rangenr:=0;
         setsize;
      end;


    constructor torddef.load;
      begin
         inherited load;
         deftype:=orddef;
         typ:=tbasetype(readbyte);
         low:=readlong;
         high:=readlong;
         rangenr:=0;
         setsize;
      end;


    procedure torddef.setsize;
      begin
         if typ=uauto then
           begin
              { generate a unsigned range if high<0 and low>=0 }
              if (low>=0) and (high<0) then
                begin
                   savesize:=4;
                   typ:=u32bit;
                end
              else if (low>=0) and (high<=255) then
                begin
                   savesize:=1;
                   typ:=u8bit;
                end
              else if (low>=-128) and (high<=127) then
                begin
                   savesize:=1;
                   typ:=s8bit;
                end
              else if (low>=0) and (high<=65536) then
                begin
                   savesize:=2;
                   typ:=u16bit;
                end
              else if (low>=-32768) and (high<=32767) then
                begin
                   savesize:=2;
                   typ:=s16bit;
                end
              else
                begin
                   savesize:=4;
                   typ:=s32bit;
                end;
           end
         else
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


    procedure torddef.write;
      begin
         inherited write;
         writebyte(byte(typ));
         writelong(low);
         writelong(high);
         current_ppu^.writeentry(iborddef);
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
         { u32bit : stabstring := strpnew('r'+
              s32bitdef^.numberstring+';0;-1;'); }
        else
          stabstring := strpnew('r'+s32bitdef^.numberstring+';'+tostr(low)+';'+tostr(high)+';');
        end;
      end;
{$endif GDB}


    procedure torddef.write_rtti_data;

        procedure dointeger;
        const
          trans : array[uchar..bool8bit] of byte =
            (otUByte,otUByte,otUWord,otULong,otSByte,otSWord,otSLong,otUByte);
        begin
          write_rtti_name;
          rttiList.concat(Tai_const.Create_8bit(byte(trans[typ])));
          rttiList.concat(Tai_const.Create_32bit(low));
          rttiList.concat(Tai_const.Create_32bit(high));
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
         is_publishable:=typ in [uchar..bool8bit];
      end;

    function torddef.gettypename : string;

      const
        names : array[tbasetype] of string[20] = ('<unknown type>',
          'untyped','Char','Byte','Word','DWord','ShortInt',
          'SmallInt','LongInt','Boolean','WordBool',
          'LongBool','QWord','Int64','WideChar');

      begin
         gettypename:=names[typ];
      end;

{****************************************************************************
                                TFLOATDEF
****************************************************************************}

    constructor tfloatdef.init(t : tfloattype);
      begin
         inherited init;
         deftype:=floatdef;
         typ:=t;
         setsize;
      end;


    constructor tfloatdef.load;
      begin
         inherited load;
         deftype:=floatdef;
         typ:=tfloattype(readbyte);
         setsize;
      end;


    procedure tfloatdef.setsize;
      begin
         case typ of
            f16bit : savesize:=2;
            f32bit,
           s32real : savesize:=4;
           s64real : savesize:=8;
           s80real : savesize:=extended_size;
           s64comp : savesize:=8;
         else
           savesize:=0;
         end;
      end;


    procedure tfloatdef.write;
      begin
         inherited write;
         writebyte(byte(typ));
         current_ppu^.writeentry(ibfloatdef);
      end;


{$ifdef GDB}
    function tfloatdef.stabstring : pchar;
      begin
         case typ of
            s32real,
            s64real : stabstring := strpnew('r'+
               s32bitdef^.numberstring+';'+tostr(savesize)+';0;');
            { for fixed real use longint instead to be able to }
            { debug something at least                         }
            f32bit:
              stabstring := s32bitdef^.stabstring;
            f16bit:
              stabstring := strpnew('r'+s32bitdef^.numberstring+';0;'+
                tostr($ffff)+';');
            { found this solution in stabsread.c from GDB v4.16 }
            s64comp : stabstring := strpnew('r'+
               s32bitdef^.numberstring+';-'+tostr(savesize)+';0;');
{$ifdef i386}
            { under dos at least you must give a size of twelve instead of 10 !! }
            { this is probably do to the fact that in gcc all is pushed in 4 bytes size }
            s80real : stabstring := strpnew('r'+s32bitdef^.numberstring+';12;0;');
{$endif i386}
            else
              internalerror(10005);
         end;
      end;
{$endif GDB}


    procedure tfloatdef.write_rtti_data;
      const
         {tfloattype = (s32real,s64real,s80real,s64bit,f16bit,f32bit);}
         translate : array[tfloattype] of byte =
           (ftSingle,ftDouble,ftExtended,ftComp,ftFixed16,ftFixed32);
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
          'Single','Double','Extended','Comp','Fixed','Fixed16');

      begin
         gettypename:=names[typ];
      end;

{****************************************************************************
                                TFILEDEF
****************************************************************************}

    constructor tfiledef.inittext;
      begin
         inherited init;
         deftype:=filedef;
         filetyp:=ft_text;
         typedfiletype.reset;
         setsize;
      end;


    constructor tfiledef.inituntyped;
      begin
         inherited init;
         deftype:=filedef;
         filetyp:=ft_untyped;
         typedfiletype.reset;
         setsize;
      end;


    constructor tfiledef.inittyped(const tt : ttype);
      begin
         inherited init;
         deftype:=filedef;
         filetyp:=ft_typed;
         typedfiletype:=tt;
         setsize;
      end;


    constructor tfiledef.inittypeddef(p : pdef);
      begin
         inherited init;
         deftype:=filedef;
         filetyp:=ft_typed;
         typedfiletype.setdef(p);
         setsize;
      end;


    constructor tfiledef.load;
      begin
         inherited load;
         deftype:=filedef;
         filetyp:=tfiletyp(readbyte);
         if filetyp=ft_typed then
           typedfiletype.load
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


    procedure tfiledef.write;
      begin
         inherited write;
         writebyte(byte(filetyp));
         if filetyp=ft_typed then
           typedfiletype.write;
         current_ppu^.writeentry(ibfiledef);
      end;


{$ifdef GDB}
    function tfiledef.stabstring : pchar;
      begin
   {$IfDef GDBknowsfiles}
      case filetyp of
        ft_typed :
          stabstring := strpnew('d'+typedfiletype.def^.numberstring{+';'});
        ft_untyped :
          stabstring := strpnew('d'+voiddef^.numberstring{+';'});
        ft_text :
          stabstring := strpnew('d'+cchardef^.numberstring{+';'});
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
      if ((typesym = nil) or ptypesym(typesym)^.isusedinstab or (cs_gdb_dbx in aktglobalswitches)) and
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
             gettypename:='File Of '+typedfiletype.def^.typename;
           ft_text:
             gettypename:='Text'
         end;
      end;



{****************************************************************************
                               TPOINTERDEF
****************************************************************************}

    constructor tpointerdef.init(const tt : ttype);
      begin
        inherited init;
        deftype:=pointerdef;
        pointertype:=tt;
        is_far:=false;
        savesize:=target_os.size_of_pointer;
      end;


    constructor tpointerdef.initfar(const tt : ttype);
      begin
        inherited init;
        deftype:=pointerdef;
        pointertype:=tt;
        is_far:=true;
        savesize:=target_os.size_of_pointer;
      end;


    constructor tpointerdef.initdef(p : pdef);
      var
        t : ttype;
      begin
        t.setdef(p);
        tpointerdef.init(t);
      end;


    constructor tpointerdef.initfardef(p : pdef);
      var
        t : ttype;
      begin
        t.setdef(p);
        tpointerdef.initfar(t);
      end;



    constructor tpointerdef.load;
      begin
         inherited load;
         deftype:=pointerdef;
         pointertype.load;
         is_far:=(readbyte<>0);
         savesize:=target_os.size_of_pointer;
      end;


    destructor tpointerdef.done;
      begin
        if assigned(pointertype.def) and
           (pointertype.def^.deftype=forwarddef) then
         begin
           dispose(pointertype.def,done);
           pointertype.reset;
         end;
        inherited done;
      end;


    procedure tpointerdef.deref;
      begin
        inherited deref;
        pointertype.resolve;
      end;


    procedure tpointerdef.write;
      begin
         inherited write;
         pointertype.write;
         writebyte(byte(is_far));
         current_ppu^.writeentry(ibpointerdef);
      end;


{$ifdef GDB}
    function tpointerdef.stabstring : pchar;
      begin
        stabstring := strpnew('*'+pstoreddef(pointertype.def)^.numberstring);
      end;


    procedure tpointerdef.concatstabto(asmlist : taasmoutput);
      var st,nb : string;
          sym_line_no : longint;
      begin
      if assigned(pointertype.def) and
         (pointertype.def^.deftype=forwarddef) then
        exit;

      if ( (typesym=nil) or ptypesym(typesym)^.isusedinstab or (cs_gdb_dbx in aktglobalswitches)) and
         (is_def_stab_written = not_written) then
        begin
          is_def_stab_written := being_written;
        if assigned(pointertype.def) and
           (pointertype.def^.deftype in [recorddef,objectdef]) then
          begin
            nb:=pstoreddef(pointertype.def)^.numberstring;
            {to avoid infinite recursion in record with next-like fields }
            if pstoreddef(pointertype.def)^.is_def_stab_written = being_written then
              begin
                if assigned(pointertype.def^.typesym) then
                  begin
                    if assigned(typesym) then
                      begin
                         st := ptypesym(typesym)^.name;
                         sym_line_no:=ptypesym(typesym)^.fileinfo.line;
                      end
                    else
                      begin
                         st := ' ';
                         sym_line_no:=0;
                      end;
                    st := '"'+st+':t'+numberstring+'=*'+nb
                          +'=xs'+pointertype.def^.typesym^.name+':",'+tostr(N_LSYM)+',0,'+tostr(sym_line_no)+',0';
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
          gettypename:='^'+pointertype.def^.typename+';far'
         else
          gettypename:='^'+pointertype.def^.typename;
      end;

{****************************************************************************
                              TCLASSREFDEF
****************************************************************************}

    constructor tclassrefdef.init(def : pdef);
      begin
         inherited initdef(def);
         deftype:=classrefdef;
      end;


    constructor tclassrefdef.load;
      begin
         { be careful, tclassdefref inherits from tpointerdef }
         tstoreddef.load;
         deftype:=classrefdef;
         pointertype.load;
         is_far:=false;
         savesize:=target_os.size_of_pointer;
      end;


    procedure tclassrefdef.write;
      begin
         { be careful, tclassdefref inherits from tpointerdef }
         tstoreddef.write;
         pointertype.write;
         current_ppu^.writeentry(ibclassrefdef);
      end;


{$ifdef GDB}
    function tclassrefdef.stabstring : pchar;
      begin
         stabstring:=strpnew(pvmtdef^.numberstring+';');
      end;


    procedure tclassrefdef.concatstabto(asmlist : taasmoutput);
      begin
        inherited concatstabto(asmlist);
      end;
{$endif GDB}

    function tclassrefdef.gettypename : string;

      begin
         gettypename:='Class Of '+pointertype.def^.typename;
      end;


{***************************************************************************
                                   TSETDEF
***************************************************************************}

{ For i386 smallsets work,
  for m68k there are problems
  can be test by compiling with -dusesmallset PM }
{$ifdef i386}
{$define usesmallset}
{$endif i386}

    constructor tsetdef.init(s : pdef;high : longint);
      begin
         inherited init;
         deftype:=setdef;
         elementtype.setdef(s);
{$ifdef usesmallset}
         { small sets only working for i386 PM }
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
{$endif usesmallset}
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


    constructor tsetdef.load;
      begin
         inherited load;
         deftype:=setdef;
         elementtype.load;
         settype:=tsettype(readbyte);
         case settype of
            normset : savesize:=32;
            varset : savesize:=readlong;
            smallset : savesize:=Sizeof(longint);
         end;
      end;


    destructor tsetdef.done;
      begin
        inherited done;
      end;


    procedure tsetdef.write;
      begin
         inherited write;
         elementtype.write;
         writebyte(byte(settype));
         if settype=varset then
           writelong(savesize);
         current_ppu^.writeentry(ibsetdef);
      end;


{$ifdef GDB}
    function tsetdef.stabstring : pchar;
      begin
         { For small sets write a longint, which can at least be seen
           in the current GDB's (PFV)
           this is obsolete with GDBPAS !!
           and anyhow creates problems with version 4.18!! PM
         if settype=smallset then
           stabstring := strpnew('r'+s32bitdef^.numberstring+';0;0xffffffff;')
         else }
           stabstring := strpnew('S'+pstoreddef(elementtype.def)^.numberstring);
      end;


    procedure tsetdef.concatstabto(asmlist : taasmoutput);
      begin
      if ( not assigned(typesym) or ptypesym(typesym)^.isusedinstab or (cs_gdb_dbx in aktglobalswitches)) and
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


    procedure tsetdef.write_rtti_data;
      begin
         rttiList.concat(Tai_const.Create_8bit(tkSet));
         write_rtti_name;
         rttiList.concat(Tai_const.Create_8bit(otULong));
         rttiList.concat(Tai_const_symbol.Createname(elementtype.def^.get_rtti_label));
      end;


    procedure tsetdef.write_child_rtti_data;
      begin
         elementtype.def^.get_rtti_label;
      end;


    function tsetdef.is_publishable : boolean;
      begin
         is_publishable:=settype=smallset;
      end;

    function tsetdef.gettypename : string;

      begin
         if assigned(elementtype.def) then
          gettypename:='Set Of '+elementtype.def^.typename
         else
          gettypename:='Empty Set';
      end;


{***************************************************************************
                                 TFORMALDEF
***************************************************************************}

    constructor tformaldef.init;
      var
         stregdef : boolean;
      begin
         stregdef:=registerdef;
         registerdef:=false;
         inherited init;
         deftype:=formaldef;
         registerdef:=stregdef;
         { formaldef must be registered at unit level !! }
         if registerdef and assigned(current_module) then
            if assigned(current_module.localsymtable) then
              psymtable(current_module.localsymtable)^.registerdef(@self)
            else if assigned(current_module.globalsymtable) then
              psymtable(current_module.globalsymtable)^.registerdef(@self);
         savesize:=target_os.size_of_pointer;
      end;


    constructor tformaldef.load;
      begin
         inherited load;
         deftype:=formaldef;
         savesize:=target_os.size_of_pointer;
      end;


    procedure tformaldef.write;
      begin
         inherited write;
         current_ppu^.writeentry(ibformaldef);
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

    constructor tarraydef.init(l,h : longint;rd : pdef);
      begin
         inherited init;
         deftype:=arraydef;
         lowrange:=l;
         highrange:=h;
         rangetype.setdef(rd);
         elementtype.reset;
         IsVariant:=false;
         IsConstructor:=false;
         IsArrayOfConst:=false;
         IsDynamicArray:=false;
         rangenr:=0;
      end;


    constructor tarraydef.load;
      begin
         inherited load;
         deftype:=arraydef;
         { the addresses are calculated later }
         elementtype.load;
         rangetype.load;
         lowrange:=readlong;
         highrange:=readlong;
         IsArrayOfConst:=boolean(readbyte);
         IsVariant:=false;
         IsConstructor:=false;
         IsDynamicArray:=false;
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


    procedure tarraydef.write;
      begin
         inherited write;
         elementtype.write;
         rangetype.write;
         writelong(lowrange);
         writelong(highrange);
         writebyte(byte(IsArrayOfConst));
         current_ppu^.writeentry(ibarraydef);
      end;


{$ifdef GDB}
    function tarraydef.stabstring : pchar;
      begin
      stabstring := strpnew('ar'+pstoreddef(rangetype.def)^.numberstring+';'
                    +tostr(lowrange)+';'+tostr(highrange)+';'+pstoreddef(elementtype.def)^.numberstring);
      end;


    procedure tarraydef.concatstabto(asmlist : taasmoutput);
      begin
      if (not assigned(typesym) or ptypesym(typesym)^.isusedinstab or (cs_gdb_dbx in aktglobalswitches))
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
        if ((lowrange=0) and
            (highrange=-1) and
            (not IsArrayOfConst) and
            (not IsVariant) and
            (not IsDynamicArray)) or
           IsConstructor then
         begin
           { strings are stored by address only }
           case elementtype.def^.deftype of
             stringdef :
               elesize:=4;
             else
               elesize:=elementtype.def^.size;
           end;
         end
        else
         elesize:=elementtype.def^.size;
      end;


    function tarraydef.size : longint;
      begin
        if IsDynamicArray then
          begin
            size:=4;
            exit;
          end;
        {Tarraydef.size may never be called for an open array!}
        if highrange<lowrange then
            internalerror(99080501);
        If (elesize>0) and
           (
            (highrange-lowrange = $7fffffff) or
            { () are needed around elesize-1 to avoid a possible
              integer overflow for elesize=1 !! PM }
            (($7fffffff div elesize + (elesize -1)) < (highrange - lowrange))
           ) Then
          Begin
            Message(sym_e_segment_too_large);
            size := 4
          End
        Else size:=(highrange-lowrange+1)*elesize;
      end;


    function tarraydef.alignment : longint;
      begin
         { alignment is the size of the elements }
         if elementtype.def^.deftype=recorddef then
          alignment:=elementtype.def^.alignment
         else
          alignment:=elesize;
      end;


    function tarraydef.needs_inittable : boolean;
      begin
         needs_inittable:=IsDynamicArray or elementtype.def^.needs_inittable;
      end;


    procedure tarraydef.write_child_rtti_data;
      begin
         elementtype.def^.get_rtti_label;
      end;


    procedure tarraydef.write_rtti_data;
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
         rttiList.concat(Tai_const_symbol.Createname(elementtype.def^.get_rtti_label));
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
               gettypename:='Array Of '+elementtype.def^.typename;
           end
         else if ((highrange=-1) and (lowrange=0)) or IsDynamicArray then
           gettypename:='Array Of '+elementtype.def^.typename
         else
           begin
              if rangetype.def^.deftype=enumdef then
                gettypename:='Array['+rangetype.def^.typename+'] Of '+elementtype.def^.typename
              else
                gettypename:='Array['+tostr(lowrange)+'..'+
                  tostr(highrange)+'] Of '+elementtype.def^.typename
           end;
      end;

{***************************************************************************
                                  trecorddef
***************************************************************************}

    constructor trecorddef.init(p : psymtable);
      begin
         inherited init;
         deftype:=recorddef;
         symtable:=p;
         symtable^.defowner := @self;
         symtable^.dataalignment:=packrecordalignment[aktpackrecords];
      end;


    constructor trecorddef.load;
      var
         oldread_member : boolean;
      begin
         inherited load;
         deftype:=recorddef;
         savesize:=readlong;
         oldread_member:=read_member;
         read_member:=true;
         symtable:=new(pstoredsymtable,loadas(recordsymtable));
         read_member:=oldread_member;
         symtable^.defowner := @self;
      end;


    destructor trecorddef.done;
      begin
         if assigned(symtable) then
           dispose(symtable,done);
         inherited done;
      end;

    function trecorddef.getsymtable(t:tgetsymtable):psymtable;
      begin
         if t=gs_record then
         getsymtable:=symtable
        else
         getsymtable:=nil;
      end;

    var
       binittable : boolean;

    procedure check_rec_inittable(s : pnamedindexobject);

      begin
         if (not binittable) and
            (psym(s)^.typ=varsym) and
            assigned(pvarsym(s)^.vartype.def) then
          begin
            if (pvarsym(s)^.vartype.def^.deftype<>objectdef) or
               not(is_class(pdef(pvarsym(s)^.vartype.def))) then
             binittable:=pvarsym(s)^.vartype.def^.needs_inittable;
          end;
      end;


    function trecorddef.needs_inittable : boolean;
      var
         oldb : boolean;
      begin
         { there are recursive calls to needs_rtti possible, }
         { so we have to change to old value how else should }
         { we do that ? check_rec_rtti can't be a nested     }
         { procedure of needs_rtti !                         }
         oldb:=binittable;
         binittable:=false;
         symtable^.foreach({$ifdef FPCPROCVAR}@{$endif}check_rec_inittable);
         needs_inittable:=binittable;
         binittable:=oldb;
      end;


    procedure trecorddef.deref;
      var
         oldrecsyms : psymtable;
      begin
         inherited deref;
         oldrecsyms:=aktrecordsymtable;
         aktrecordsymtable:=symtable;
         { now dereference the definitions }
         pstoredsymtable(symtable)^.deref;
         aktrecordsymtable:=oldrecsyms;
      end;


    procedure trecorddef.write;
      var
         oldread_member : boolean;
      begin
         oldread_member:=read_member;
         read_member:=true;
         inherited write;
         writelong(savesize);
         current_ppu^.writeentry(ibrecorddef);
         pstoredsymtable(symtable)^.writeas;
         read_member:=oldread_member;
      end;

    function trecorddef.size:longint;
      begin
        size:=symtable^.datasize;
      end;


    function trecorddef.alignment:longint;
      var
        l  : longint;
        hp : pvarsym;
      begin
        { also check the first symbol for it's size, because a
          packed record has dataalignment of 1, but the first
          sym could be a longint which should be aligned on 4 bytes,
          this is compatible with C record packing (PFV) }
        hp:=pvarsym(symtable^.symindex^.first);
        if assigned(hp) then
         begin
           l:=hp^.vartype.def^.size;
           if l>symtable^.dataalignment then
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
            alignment:=symtable^.dataalignment;
         end
        else
         alignment:=symtable^.dataalignment;
      end;

{$ifdef GDB}
    Const StabRecString : pchar = Nil;
          StabRecSize : longint = 0;
          RecOffset : Longint = 0;

    procedure addname(p : pnamedindexobject);
    var
      news, newrec : pchar;
      spec : string[3];
      size : longint;
    begin
    { static variables from objects are like global objects }
    if (sp_static in psym(p)^.symoptions) then
      exit;
    If psym(p)^.typ = varsym then
       begin
         if (sp_protected in psym(p)^.symoptions) then
           spec:='/1'
         else if (sp_private in psym(p)^.symoptions) then
           spec:='/0'
         else
           spec:='';
         if not assigned(pvarsym(p)^.vartype.def) then
          writeln(pvarsym(p)^.name);
         { class fields are pointers PM, obsolete now PM }
         {if (pvarsym(p)^.vartype.def^.deftype=objectdef) and
            pobjectdef(pvarsym(p)^.vartype.def)^.is_class then
            spec:=spec+'*'; }
         size:=pvarsym(p)^.vartype.def^.size;
         { open arrays made overflows !! }
         if size>$fffffff then
           size:=$fffffff;
         newrec := strpnew(p^.name+':'+spec+pstoreddef(pvarsym(p)^.vartype.def)^.numberstring
                       +','+tostr(pvarsym(p)^.address*8)+','
                       +tostr(size*8)+';');
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
         RecOffset := RecOffset + pvarsym(p)^.vartype.def^.size;
       end;
    end;


    function trecorddef.stabstring : pchar;
      Var oldrec : pchar;
          oldsize,oldrecoffset : longint;
      begin
        oldrec := stabrecstring;
        oldsize:=stabrecsize;
        GetMem(stabrecstring,memsizeinc);
        stabrecsize:=memsizeinc;
        strpcopy(stabRecString,'s'+tostr(size));
        OldRecOffset:=RecOffset;
        RecOffset := 0;
        symtable^.foreach({$ifdef FPCPROCVAR}@{$endif}addname);
        strpcopy(strend(StabRecString),';');
        stabstring := strnew(StabRecString);
        Freemem(stabrecstring,stabrecsize);
        stabrecstring := oldrec;
        stabrecsize:=oldsize;
        RecOffset:=OldRecOffset;
      end;


    procedure trecorddef.concatstabto(asmlist : taasmoutput);
      begin
        if (not assigned(typesym) or ptypesym(typesym)^.isusedinstab or (cs_gdb_dbx in aktglobalswitches)) and
           (is_def_stab_written = not_written)  then
          inherited concatstabto(asmlist);
      end;

{$endif GDB}

    var
       count : longint;

    procedure count_inittable_fields(sym : pnamedindexobject);
      begin
         if ((psym(sym)^.typ=varsym) and
             pvarsym(sym)^.vartype.def^.needs_inittable) {and
             (pvarsym(sym)^.vartype.def^.deftype<>objectdef) or
             not(is_class(pdef(pvarsym(sym)^.vartype.def)))} then
           inc(count);
      end;


    procedure count_fields(sym : pnamedindexobject);
      begin
            inc(count);
      end;


    procedure write_field_inittable(sym : pnamedindexobject);
      begin
         if ((psym(sym)^.typ=varsym) and
            pvarsym(sym)^.vartype.def^.needs_inittable) and
            ((pvarsym(sym)^.vartype.def^.deftype<>objectdef) or
             not(is_class(pvarsym(sym)^.vartype.def))) then
           begin
              rttiList.concat(Tai_const_symbol.Create(pstoreddef(pvarsym(sym)^.vartype.def)^.get_inittable_label));
              rttiList.concat(Tai_const.Create_32bit(pvarsym(sym)^.address));
           end;
      end;


    procedure write_field_rtti(sym : pnamedindexobject);
      begin
         rttiList.concat(Tai_const_symbol.Createname(pvarsym(sym)^.vartype.def^.get_rtti_label));
         rttiList.concat(Tai_const.Create_32bit(pvarsym(sym)^.address));
      end;


    procedure generate_child_inittable(sym:pnamedindexobject);
      begin
         if (psym(sym)^.typ=varsym) and
            pvarsym(sym)^.vartype.def^.needs_inittable then
         { force inittable generation }
           pstoreddef(pvarsym(sym)^.vartype.def)^.get_inittable_label;
      end;


    procedure generate_child_rtti(sym : pnamedindexobject);
      begin
         pvarsym(sym)^.vartype.def^.get_rtti_label;
      end;


    procedure trecorddef.write_child_rtti_data;
      begin
         symtable^.foreach({$ifdef FPCPROCVAR}@{$endif}generate_child_rtti);
      end;


    procedure trecorddef.write_child_init_data;
      begin
         symtable^.foreach({$ifdef FPCPROCVAR}@{$endif}generate_child_inittable);
      end;


    procedure trecorddef.write_rtti_data;
      begin
         rttiList.concat(Tai_const.Create_8bit(tkrecord));
         write_rtti_name;
         rttiList.concat(Tai_const.Create_32bit(size));
         count:=0;
         symtable^.foreach({$ifdef FPCPROCVAR}@{$endif}count_fields);
         rttiList.concat(Tai_const.Create_32bit(count));
         symtable^.foreach({$ifdef FPCPROCVAR}@{$endif}write_field_rtti);
      end;


    procedure trecorddef.write_init_data;
      begin
         rttiList.concat(Tai_const.Create_8bit(tkrecord));
         write_rtti_name;
         rttiList.concat(Tai_const.Create_32bit(size));
         count:=0;
         symtable^.foreach({$ifdef FPCPROCVAR}@{$endif}count_inittable_fields);
         rttiList.concat(Tai_const.Create_32bit(count));
         symtable^.foreach({$ifdef FPCPROCVAR}@{$endif}write_field_inittable);
      end;

    function trecorddef.gettypename : string;

      begin
         gettypename:='<record type>'
      end;


{***************************************************************************
                       TABSTRACTPROCDEF
***************************************************************************}

    constructor tabstractprocdef.init;
      begin
         inherited init;
         para:=TParaLinkedList.Create;
         minparacount:=0;
         maxparacount:=0;
         fpu_used:=0;
         proctypeoption:=potype_none;
         proccalloptions:=[];
         procoptions:=[];
         rettype.setdef(voiddef);
         symtablelevel:=0;
         savesize:=target_os.size_of_pointer;
      end;


    destructor tabstractprocdef.done;
      begin
         Para.Free;
         inherited done;
      end;


    procedure tabstractprocdef.concatpara(tt:ttype;vsp : tvarspez;defval:psym);
      var
        hp : TParaItem;
      begin
        hp:=TParaItem.Create;
        hp.paratyp:=vsp;
        hp.paratype:=tt;
        hp.register:=R_NO;
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
            (rettype.def^.deftype=floatdef) and
            (pfloatdef(rettype.def)^.typ<>f32bit) then
           fpu_used:=2;
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
            resolvesym(psym(hp.defaultvalue));
            hp:=TParaItem(hp.next);
          end;
      end;


    constructor tabstractprocdef.load;
      var
         hp : TParaItem;
         count,i : word;
      begin
         inherited load;
         Para:=TParaLinkedList.Create;
         minparacount:=0;
         maxparacount:=0;
         rettype.load;
         fpu_used:=readbyte;
         proctypeoption:=tproctypeoption(readlong);
         readsmallset(proccalloptions);
         readsmallset(procoptions);
         count:=readword;
         savesize:=target_os.size_of_pointer;
         for i:=1 to count do
          begin
            hp:=TParaItem.Create;
            hp.paratyp:=tvarspez(readbyte);
            { hp.register:=tregister(readbyte); }
            hp.register:=R_NO;
            hp.paratype.load;
            hp.defaultvalue:=psym(readderef);
            if not assigned(hp.defaultvalue) then
             inc(minparacount);
            inc(maxparacount);
            Para.concat(hp);
          end;
      end;


    procedure tabstractprocdef.write;
      var
        hp : TParaItem;
        oldintfcrc : boolean;
      begin
         inherited write;
         rettype.write;
         oldintfcrc:=current_ppu^.do_interface_crc;
         current_ppu^.do_interface_crc:=false;
         writebyte(fpu_used);
         writelong(ord(proctypeoption));
         writesmallset(proccalloptions);
         writesmallset(procoptions);
         current_ppu^.do_interface_crc:=oldintfcrc;
         writeword(maxparacount);
         hp:=TParaItem(Para.first);
         while assigned(hp) do
          begin
            writebyte(byte(hp.paratyp));
            { writebyte(byte(hp.register)); }
            hp.paratype.write;
            writederef(hp.defaultvalue);
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
              vs_var   : inc(l,target_os.size_of_pointer);
              vs_value,
              vs_const : if push_addr_param(pdc.paratype.def) then
                          inc(l,target_os.size_of_pointer)
                         else
                          inc(l,pdc.paratype.def^.size);
            end;
            l:=align(l,alignsize);
            pdc:=TParaItem(pdc.next);
          end;
         para_size:=l;
      end;


    function tabstractprocdef.demangled_paras : string;
      var
        hs,s : string;
        hp : TParaItem;
        hpc : pconstsym;
      begin
        hp:=TParaItem(Para.last);
        if not(assigned(hp)) then
          begin
             demangled_paras:='';
             exit;
          end;
        s:='(';
        while assigned(hp) do
         begin
           if assigned(hp.paratype.def^.typesym) then
             s:=s+hp.paratype.def^.typesym^.realname
           else if hp.paratyp=vs_var then
             s:=s+'var'
           else if hp.paratyp=vs_const then
             s:=s+'const'
           else if hp.paratyp=vs_out then
             s:=s+'out';
           { default value }
           if assigned(hp.defaultvalue) then
            begin
              hpc:=pconstsym(hp.defaultvalue);
              hs:='';
              case hpc^.consttyp of
                conststring,
                constresourcestring :
                  hs:=strpas(pchar(tpointerord(hpc^.value)));
                constreal :
                  str(pbestreal(tpointerord(hpc^.value))^,hs);
                constord,
                constpointer :
                  hs:=tostr(hpc^.value);
                constbool :
                  begin
                    if hpc^.value<>0 then
                     hs:='TRUE'
                    else
                     hs:='FALSE';
                  end;
                constnil :
                  hs:='nil';
                constchar :
                  hs:=chr(hpc^.value);
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
        demangled_paras:=s;
      end;


    function tabstractprocdef.proccalloption2str : string;
      type
        tproccallopt=record
          mask : tproccalloption;
          str  : string[30];
        end;
      const
        proccallopts=13;
        proccallopt : array[1..proccallopts] of tproccallopt=(
           (mask:pocall_none;         str:''),
           (mask:pocall_clearstack;   str:'ClearStack'),
           (mask:pocall_leftright;    str:'LeftRight'),
           (mask:pocall_cdecl;        str:'CDecl'),
           (mask:pocall_register;     str:'Register'),
           (mask:pocall_stdcall;      str:'StdCall'),
           (mask:pocall_safecall;     str:'SafeCall'),
           (mask:pocall_palmossyscall;str:'PalmOSSysCall'),
           (mask:pocall_system;       str:'System'),
           (mask:pocall_inline;       str:'Inline'),
           (mask:pocall_internproc;   str:'InternProc'),
           (mask:pocall_internconst;  str:'InternConst'),
           (mask:pocall_cdecl;        str:'CPPDecl')
        );
      var
        s : string;
        i : longint;
        first : boolean;
      begin
        s:='';
        first:=true;
        for i:=1to proccallopts do
         if (proccallopt[i].mask in proccalloptions) then
          begin
            if first then
              first:=false
            else
              s:=s+';';
            s:=s+proccallopt[i].str;
          end;
        proccalloption2str:=s;
      end;


{$ifdef GDB}
    function tabstractprocdef.stabstring : pchar;
      begin
        stabstring := strpnew('abstractproc'+numberstring+';');
      end;


    procedure tabstractprocdef.concatstabto(asmlist : taasmoutput);
      begin
         if (not assigned(typesym) or ptypesym(typesym)^.isusedinstab or (cs_gdb_dbx in aktglobalswitches))
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

    constructor tprocdef.init;
      begin
         inherited init;
         deftype:=procdef;
         _mangledname:=nil;
         nextoverloaded:=nil;
         fileinfo:=aktfilepos;
         extnumber:=-1;
         aliasnames:=tstringlist.create;
         localst:=new(pstoredsymtable,init(localsymtable));
         parast:=new(pstoredsymtable,init(parasymtable));
         localst^.defowner:=@self;
         parast^.defowner:=@self;
         { this is used by insert
          to check same names in parast and localst }
         localst^.next:=parast;
         defref:=nil;
         crossref:=nil;
         lastwritten:=nil;
         refcount:=0;
         if (cs_browser in aktmoduleswitches) and make_ref then
          begin
            defref:=new(pref,init(defref,@akttokenpos));
            inc(refcount);
          end;
         lastref:=defref;
       { first, we assume that all registers are used }
{$ifdef newcg}
         usedregisters:=[firstreg..lastreg];
{$else newcg}
{$ifdef i386}
         usedregisters:=$ff;
{$endif i386}
{$ifdef m68k}
         usedregisters:=$FFFF;
{$endif}
{$endif newcg}
         forwarddef:=true;
         interfacedef:=false;
         hasforward:=false;
         _class := nil;
         code:=nil;
         regvarinfo := nil;
         count:=false;
         is_used:=false;
      end;


    constructor tprocdef.load;
      begin
         inherited load;
         deftype:=procdef;

{$ifdef newcg}
         readnormalset(usedregisters);
{$else newcg}
{$ifdef i386}
         usedregisters:=readbyte;
{$endif i386}
{$ifdef m68k}
         usedregisters:=readword;
{$endif}
{$endif newcg}
         _mangledname:=stringdup(readstring);

         extnumber:=readlong;
         nextoverloaded:=pprocdef(readderef);
         _class := pobjectdef(readderef);
         readposinfo(fileinfo);

         if (cs_link_deffile in aktglobalswitches) and
            (tf_need_export in target_info.flags) and
            (po_exports in procoptions) then
           deffile.AddExport(mangledname);

         aliasnames:=tstringlist.create;

         parast:=new(pstoredsymtable,loadas(parasymtable));
         parast^.defowner:=@self;
         {new(localst,loadas(localsymtable));
         localst^.defowner:=@self;
         parast^.next:=localst;
         localst^.next:=owner;}

         forwarddef:=false;
         interfacedef:=false;
         hasforward:=false;
         code := nil;
         regvarinfo := nil;
         lastref:=nil;
         lastwritten:=nil;
         defref:=nil;
         refcount:=0;
         count:=true;
         is_used:=false;
      end;


    function tprocdef.fullprocname:string;
      var
        s : string;
      begin
        s:='';
        if assigned(_class) then
         s:=_class^.objname^+'.';
        s:=s+procsym^.realname+demangled_paras;
        fullprocname:=s;
      end;


    function tprocdef.fullprocnamewithret:string;
      var
        s : string;
      begin
        s:=fullprocname;
        if assigned(rettype.def) and
          not(is_equal(rettype.def,voiddef)) then
               s:=s+' : '+rettype.def^.gettypename;
        fullprocnamewithret:=s;
      end;


    function tprocdef.getsymtable(t:tgetsymtable):psymtable;
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

Const local_symtable_index : longint = $8001;

    procedure tprocdef.load_references;
      var
        pos : tfileposinfo;
{$ifndef NOLOCALBROWSER}
        oldsymtablestack,
        st : psymtable;
{$endif ndef NOLOCALBROWSER}
        move_last : boolean;
      begin
        move_last:=lastwritten=lastref;
        while (not current_ppu^.endofentry) do
         begin
           readposinfo(pos);
           inc(refcount);
           lastref:=new(pref,init(lastref,@pos));
           lastref^.is_written:=true;
           if refcount=1 then
            defref:=lastref;
         end;
        if move_last then
          lastwritten:=lastref;
        if ((current_module.flags and uf_local_browser)<>0)
           and is_in_current then
          begin
{$ifndef NOLOCALBROWSER}
             oldsymtablestack:=symtablestack;
             st:=aktlocalsymtable;
             parast:=new(pstoredsymtable,loadas(parasymtable));
             parast^.defowner:=@self;
             aktlocalsymtable:=parast;
             pstoredsymtable(parast)^.deref;
             parast^.next:=owner;
             pstoredsymtable(parast)^.load_browser;
             aktlocalsymtable:=st;
             localst:=new(pstoredsymtable,loadas(localsymtable));
             localst^.defowner:=@self;
             aktlocalsymtable:=localst;
             symtablestack:=parast;
             pstoredsymtable(localst)^.deref;
             localst^.next:=parast;
             pstoredsymtable(localst)^.load_browser;
             aktlocalsymtable:=st;
             symtablestack:=oldsymtablestack;
{$endif ndef NOLOCALBROWSER}
          end;
      end;


    function tprocdef.write_references : boolean;
      var
        ref : pref;
{$ifndef NOLOCALBROWSER}
        st : psymtable;
        pdo : pobjectdef;
{$endif ndef NOLOCALBROWSER}
        move_last : boolean;
      begin
        move_last:=lastwritten=lastref;
        if move_last and (((current_module.flags and uf_local_browser)=0)
           or not is_in_current) then
          exit;
      { write address of this symbol }
        writederef(@self);
      { write refs }
        if assigned(lastwritten) then
          ref:=lastwritten
        else
          ref:=defref;
        while assigned(ref) do
         begin
           if ref^.moduleindex=current_module.unit_index then
             begin
                writeposinfo(ref^.posinfo);
                ref^.is_written:=true;
                if move_last then
                  lastwritten:=ref;
             end
           else if not ref^.is_written then
             move_last:=false
           else if move_last then
             lastwritten:=ref;
           ref:=ref^.nextref;
         end;
        current_ppu^.writeentry(ibdefref);
        write_references:=true;
        if ((current_module.flags and uf_local_browser)<>0)
           and is_in_current then
          begin
{$ifndef NOLOCALBROWSER}
             pdo:=_class;
             if (owner^.symtabletype<>localsymtable) then
               while assigned(pdo) do
                 begin
                    if pdo^.symtable<>aktrecordsymtable then
                      begin
                         pdo^.symtable^.unitid:=local_symtable_index;
                         inc(local_symtable_index);
                      end;
                    pdo:=pdo^.childof;
                 end;

             { we need TESTLOCALBROWSER para and local symtables
               PPU files are then easier to read PM }
             if not assigned(parast) then
               parast:=new(pstoredsymtable,init(parasymtable));
             parast^.defowner:=@self;
             st:=aktlocalsymtable;
             aktlocalsymtable:=parast;
             pstoredsymtable(parast)^.writeas;
             parast^.unitid:=local_symtable_index;
             inc(local_symtable_index);
             pstoredsymtable(parast)^.write_browser;
             if not assigned(localst) then
               localst:=new(pstoredsymtable,init(localsymtable));
             localst^.defowner:=@self;
             aktlocalsymtable:=localst;
             pstoredsymtable(localst)^.writeas;
             localst^.unitid:=local_symtable_index;
             inc(local_symtable_index);
             pstoredsymtable(localst)^.write_browser;
             aktlocalsymtable:=st;
             { decrement for }
             local_symtable_index:=local_symtable_index-2;
             pdo:=_class;
             if (owner^.symtabletype<>localsymtable) then
               while assigned(pdo) do
                 begin
                    if pdo^.symtable<>aktrecordsymtable then
                      dec(local_symtable_index);
                    pdo:=pdo^.childof;
                 end;
{$endif ndef NOLOCALBROWSER}
          end;
      end;


    destructor tprocdef.done;
      begin
         if assigned(defref) then
           begin
             defref^.freechain;
             dispose(defref,done);
           end;
         aliasnames.free;
         if assigned(parast) then
           dispose(parast,done);
         if assigned(localst) and (localst^.symtabletype<>staticsymtable) then
           dispose(localst,done);
         if (pocall_inline in proccalloptions) and assigned(code) then
           tnode(code).free;
         if assigned(regvarinfo) then
           dispose(pregvarinfo(regvarinfo));
         if (po_msgstr in procoptions) then
           strdispose(messageinf.str);
         if assigned(_mangledname) then
           stringdispose(_mangledname);
         inherited done;
      end;


    procedure tprocdef.write;
      var
        oldintfcrc : boolean;
      begin
         inherited write;
         oldintfcrc:=current_ppu^.do_interface_crc;
         current_ppu^.do_interface_crc:=false;
   { set all registers to used for simplified compilation PM }
         if simplify_ppu then
           begin
{$ifdef newcg}
             usedregisters:=[firstreg..lastreg];
{$else newcg}
{$ifdef i386}
             usedregisters:=$ff;
{$endif i386}
{$ifdef m68k}
             usedregisters:=$ffff;
{$endif}
{$endif newcg}
           end;

{$ifdef newcg}
         writenormalset(usedregisters);
{$else newcg}
{$ifdef i386}
         writebyte(usedregisters);
{$endif i386}
{$ifdef m68k}
         writeword(usedregisters);
{$endif}
{$endif newcg}
         current_ppu^.do_interface_crc:=oldintfcrc;
         writestring(mangledname);
         writelong(extnumber);
         if (proctypeoption<>potype_operator) then
           writederef(nextoverloaded)
         else
           begin
              { only write the overloads from the same unit }
              if assigned(nextoverloaded) and
                 (nextoverloaded^.owner=owner) then
                writederef(nextoverloaded)
              else
                writederef(nil);
           end;
         writederef(_class);
         writeposinfo(fileinfo);
         if (pocall_inline in proccalloptions) then
           begin
              { we need to save
                - the para and the local symtable
                - the code ptree !! PM
               writesymtable(parast);
               writesymtable(localst);
               writeptree(ptree(code));
               }
           end;
         current_ppu^.writeentry(ibprocdef);

         { Save the para and local symtable, for easier reading
           save both always, they don't influence the interface crc }
         oldintfcrc:=current_ppu^.do_interface_crc;
         current_ppu^.do_interface_crc:=false;
         if not assigned(parast) then
          begin
            parast:=new(pstoredsymtable,init(parasymtable));
            parast^.defowner:=@self;
          end;
         pstoredsymtable(parast)^.writeas;
         {if not assigned(localst) then
          begin
            localst:=new(pstoredsymtable,init(localsymtable));
            localst^.defowner:=@self;
          end;
         localst^.writeas;}
         current_ppu^.do_interface_crc:=oldintfcrc;
      end;


    function tprocdef.haspara:boolean;
      begin
        haspara:=assigned(parast^.symindex^.first);
      end;


{$ifdef GDB}
    procedure addparaname(p : psym);
      var vs : char;
      begin
      if pvarsym(p)^.varspez = vs_value then vs := '1'
        else vs := '0';
      strpcopy(strend(StabRecString),p^.name+':'+pstoreddef(pvarsym(p)^.vartype.def)^.numberstring+','+vs+';');
      end;


    function tprocdef.stabstring : pchar;
      var
          i : longint;
          oldrec : pchar;
      begin
      oldrec := stabrecstring;
      getmem(StabRecString,1024);
      strpcopy(StabRecString,'f'+pstoreddef(rettype.def)^.numberstring);
      i:=maxparacount;
      if i>0 then
        begin
        strpcopy(strend(StabRecString),','+tostr(i)+';');
        (* confuse gdb !! PM
        if assigned(parast) then
          parast^.foreach({$ifdef FPCPROCVAR}@{$endif}addparaname)
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
               +':'+param^.paratype.def^.numberstring+','+vartyp+';');
            param := param^.next;
            end;
          end;   *)
        {strpcopy(strend(StabRecString),';');}
        end;
      stabstring := strnew(stabrecstring);
      freemem(stabrecstring,1024);
      stabrecstring := oldrec;
      end;


    procedure tprocdef.concatstabto(asmlist : taasmoutput);
      begin
      end;
{$endif GDB}

    procedure tprocdef.deref;
      var
        oldsymtablestack,
        oldlocalsymtable : psymtable;
      begin
         inherited deref;
         resolvedef(pdef(nextoverloaded));
         resolvedef(pdef(_class));
         { parast }
         oldsymtablestack:=symtablestack;
         oldlocalsymtable:=aktlocalsymtable;
         aktlocalsymtable:=parast;
         pstoredsymtable(parast)^.deref;
         {symtablestack:=parast;
         aktlocalsymtable:=localst;
         localst^.deref;}
         aktlocalsymtable:=oldlocalsymtable;
         symtablestack:=oldsymtablestack;
      end;


    function tprocdef.mangledname : string;
      begin
         if assigned(_mangledname) then
           mangledname:=_mangledname^
         else
           mangledname:='';
         if count then
           is_used:=true;
      end;


    function tprocdef.cplusplusmangledname : string;

      function getcppparaname(p : pdef) : string;

        const
           ordtype2str : array[tbasetype] of string[2] = (
             '','','c',
             'Uc','Us','Ui',
             'Sc','s','i',
             'b','b','b',
             'Us','x','w');

        var
           s : string;

        begin
           case p^.deftype of
              orddef:
                s:=ordtype2str[porddef(p)^.typ];
              pointerdef:
                s:='P'+getcppparaname(ppointerdef(p)^.pointertype.def);
              else
                internalerror(2103001);
           end;
           getcppparaname:=s;
        end;

      var
         s,s2 : string;
         param : TParaItem;

      begin
         s := procsym^.realname;
         if procsym^.owner^.symtabletype=objectsymtable then
           begin
              s2:=upper(pobjectdef(procsym^.owner^.defowner)^.typesym^.realname);
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
         if assigned(_mangledname) then
           begin
{$ifdef MEMDEBUG}
              dec(manglenamesize,length(_mangledname^));
{$endif}
              stringdispose(_mangledname);
           end;
         _mangledname:=stringdup(s);
{$ifdef MEMDEBUG}
         inc(manglenamesize,length(s));
{$endif}
{$ifdef EXTDEBUG}
         if assigned(parast) then
           begin
              stringdispose(parast^.name);
              parast^.name:=stringdup('args of '+s);
           end;
         if assigned(localst) then
           begin
              stringdispose(localst^.name);
              localst^.name:=stringdup('locals of '+s);
           end;
{$endif}
      end;


{***************************************************************************
                                 TPROCVARDEF
***************************************************************************}

    constructor tprocvardef.init;
      begin
         inherited init;
         deftype:=procvardef;
      end;


    constructor tprocvardef.load;
      begin
         inherited load;
         deftype:=procvardef;
      end;


    procedure tprocvardef.write;
      begin
         { here we cannot get a real good value so just give something }
         { plausible (PM) }
         { a more secure way would be
           to allways store in a temp }
         if is_fpu(rettype.def) then
           fpu_used:=2
         else
           fpu_used:=0;
         inherited write;
         current_ppu^.writeentry(ibprocvardef);
      end;


    function tprocvardef.size : longint;
      begin
         if (po_methodpointer in procoptions) then
           size:=2*target_os.size_of_pointer
         else
           size:=target_os.size_of_pointer;
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

        strpcopy(nss,'*f'+pstoreddef(rettype.def)^.numberstring{+','+tostr(i)}+';');
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
          pst := strpnew('p'+tostr(i)+':'+param^.paratype.def^.numberstring+','+vartyp+';');
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
         if ( not assigned(typesym) or ptypesym(typesym)^.isusedinstab or (cs_gdb_dbx in aktglobalswitches))
           and (is_def_stab_written = not_written)  then
           inherited concatstabto(asmlist);
         is_def_stab_written:=written;
      end;
{$endif GDB}


    procedure tprocvardef.write_rtti_data;
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
             if rettype.def = pdef(voiddef) then    { ### typecast shoudln't be necessary! (sg) }
               methodkind := mkProcedure
             else
               methodkind := mkFunction;
             rttiList.concat(Tai_const.Create_8bit(methodkind));

             { get # of parameters }
             rttiList.concat(Tai_const.Create_8bit(maxparacount));

             { write parameter info. The parameters must be written in reverse order
               if this method uses right to left parameter pushing! }
             if (pocall_leftright in proccalloptions) then
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
                 pstoreddef(pdc.paratype.def)^.write_rtti_name;

                 if (pocall_leftright in proccalloptions) then
                  pdc:=TParaItem(pdc.previous)
                 else
                  pdc:=TParaItem(pdc.next);
               end;

             { write name of result type }
             pstoreddef(rettype.def)^.write_rtti_name;
          end;
      end;


    procedure tprocvardef.write_child_rtti_data;
      begin
         {!!!!!!!!}
      end;


    function tprocvardef.is_publishable : boolean;
      begin
         is_publishable:=(po_methodpointer in procoptions);
      end;

    function tprocvardef.gettypename : string;
      begin
         if assigned(rettype.def) and
            (rettype.def<>pdef(voiddef)) then
           gettypename:='<procedure variable type of function'+demangled_paras+
             ':'+rettype.def^.gettypename+';'+proccalloption2str+'>'
         else
           gettypename:='<procedure variable type of procedure'+demangled_paras+
             ';'+proccalloption2str+'>';
      end;


{***************************************************************************
                              TOBJECTDEF
***************************************************************************}

{$ifdef GDB}
    const
       vtabletype : word = 0;
       vtableassigned : boolean = false;
{$endif GDB}

   constructor tobjectdef.init(ot : tobjectdeftype;const n : string;c : pobjectdef);
     begin
        inherited init;
        objecttype:=ot;
        deftype:=objectdef;
        objectoptions:=[];
        childof:=nil;
        symtable:=new(pstoredsymtable,init(objectsymtable));
        symtable^.name := stringdup(n);
        { create space for vmt !! }
        vmt_offset:=0;
        symtable^.datasize:=0;
        symtable^.defowner:=@self;
        symtable^.dataalignment:=packrecordalignment[aktpackrecords];
        lastvtableindex:=0;
        set_parent(c);
        objname:=stringdup(n);

        { set up guid }
        isiidguidvalid:=true; { default null guid }
        fillchar(iidguid,sizeof(iidguid),0); { default null guid }
        iidstr:=stringdup(''); { default is empty string }

        { set£p implemented interfaces }
        if objecttype in [odt_class,odt_interfacecorba] then
          new(implementedinterfaces,init)
        else
          implementedinterfaces:=nil;

{$ifdef GDB}
        writing_class_record_stab:=false;
{$endif GDB}
     end;


    constructor tobjectdef.load;
      var
         oldread_member : boolean;
         i,implintfcount: longint;
      begin
         inherited load;
         deftype:=objectdef;
         objecttype:=tobjectdeftype(readbyte);
         savesize:=readlong;
         vmt_offset:=readlong;
         objname:=stringdup(readstring);
         childof:=pobjectdef(readderef);
         readsmallset(objectoptions);
         has_rtti:=boolean(readbyte);

         { load guid }
         iidstr:=nil;
         if objecttype in [odt_interfacecom,odt_interfacecorba] then
           begin
              isiidguidvalid:=boolean(readbyte);
              readguid(iidguid);
              iidstr:=stringdup(readstring);
              lastvtableindex:=readlong;
           end;

         { load implemented interfaces }
         if objecttype in [odt_class,odt_interfacecorba] then
           begin
             new(implementedinterfaces,init);
             implintfcount:=readlong;
             for i:=1 to implintfcount do
               begin
                  implementedinterfaces^.addintfref(pdef(readderef));
                  implementedinterfaces^.ioffsets(i)^:=readlong;
               end;
           end
         else
           implementedinterfaces:=nil;

         oldread_member:=read_member;
         read_member:=true;
         symtable:=new(pstoredsymtable,loadas(objectsymtable));
         read_member:=oldread_member;

         symtable^.defowner:=@self;
         symtable^.name := stringdup(objname^);

         { handles the predefined class tobject  }
         { the last TOBJECT which is loaded gets }
         { it !                                  }
         if (childof=nil) and
            (objecttype=odt_class) and
            (upper(objname^)='TOBJECT') then
           class_tobject:=@self;
         if (childof=nil) and
            (objecttype=odt_interfacecom) and
            (upper(objname^)='IUNKNOWN') then
           interface_iunknown:=@self;
{$ifdef GDB}
         writing_class_record_stab:=false;
{$endif GDB}
       end;


   destructor tobjectdef.done;
     begin
        if assigned(symtable) then
          dispose(symtable,done);
        if (oo_is_forward in objectoptions) then
          Message1(sym_e_class_forward_not_resolved,objname^);
        stringdispose(objname);
        stringdispose(iidstr);
        if assigned(implementedinterfaces) then
          dispose(implementedinterfaces,done);
        inherited done;
     end;


    procedure tobjectdef.write;
      var
         oldread_member : boolean;
         implintfcount : longint;
         i : longint;
      begin
         inherited write;
         writebyte(byte(objecttype));
         writelong(size);
         writelong(vmt_offset);
         writestring(objname^);
         writederef(childof);
         writesmallset(objectoptions);
         writebyte(byte(has_rtti));
         if objecttype in [odt_interfacecom,odt_interfacecorba] then
           begin
              writebyte(byte(isiidguidvalid));
              writeguid(iidguid);
              writestring(iidstr^);
              writelong(lastvtableindex);
           end;

         if objecttype in [odt_class,odt_interfacecorba] then
           begin
              implintfcount:=implementedinterfaces^.count;
              writelong(implintfcount);
              for i:=1 to implintfcount do
                begin
                   writederef(implementedinterfaces^.interfaces(i));
                   writelong(implementedinterfaces^.ioffsets(i)^);
                end;
           end;

         current_ppu^.writeentry(ibobjectdef);

         oldread_member:=read_member;
         read_member:=true;
         pstoredsymtable(symtable)^.writeas;
         read_member:=oldread_member;
      end;


    function tobjectdef.getsymtable(t:tgetsymtable):psymtable;
      begin
        if t=gs_record then
         getsymtable:=symtable
        else
         getsymtable:=nil;
      end;

    procedure tobjectdef.deref;
      var
         oldrecsyms : psymtable;
      begin
         inherited deref;
         resolvedef(pdef(childof));
         oldrecsyms:=aktrecordsymtable;
         aktrecordsymtable:=symtable;
         pstoredsymtable(symtable)^.deref;
         aktrecordsymtable:=oldrecsyms;
         if objecttype in [odt_class,odt_interfacecorba] then
           implementedinterfaces^.deref;
      end;


    procedure tobjectdef.set_parent( c : pobjectdef);
      begin
        { nothing to do if the parent was not forward !}
        if assigned(childof) then
          exit;
        childof:=c;
        { some options are inherited !! }
        if assigned(c) then
          begin
             { only important for classes }
             lastvtableindex:=c^.lastvtableindex;
             objectoptions:=objectoptions+(c^.objectoptions*
               [oo_has_virtual,oo_has_private,oo_has_protected,oo_has_constructor,oo_has_destructor]);
             if not (objecttype in [odt_interfacecom,odt_interfacecorba]) then
               begin
                  { add the data of the anchestor class }
                  inc(symtable^.datasize,c^.symtable^.datasize);
                  if (oo_has_vmt in objectoptions) and
                     (oo_has_vmt in c^.objectoptions) then
                    dec(symtable^.datasize,target_os.size_of_pointer);
                  { if parent has a vmt field then
                    the offset is the same for the child PM }
                  if (oo_has_vmt in c^.objectoptions) or is_class(@self) then
                    begin
                       vmt_offset:=c^.vmt_offset;
                       include(objectoptions,oo_has_vmt);
                    end;
               end;
          end;
        savesize := symtable^.datasize;
      end;


   procedure tobjectdef.insertvmt;
     begin
        if objecttype in [odt_interfacecom,odt_interfacecorba] then exit;
        if (oo_has_vmt in objectoptions) then
          internalerror(12345)
        else
          begin
             { first round up to multiple of 4 }
             if (symtable^.dataalignment=2) then
               begin
                 if (symtable^.datasize and 1)<>0 then
                   inc(symtable^.datasize);
               end
             else
              if (symtable^.dataalignment>=4) then
               begin
                 if (symtable^.datasize mod 4) <> 0 then
                   inc(symtable^.datasize,4-(symtable^.datasize mod 4));
               end;
             vmt_offset:=symtable^.datasize;
             inc(symtable^.datasize,target_os.size_of_pointer);
             include(objectoptions,oo_has_vmt);
          end;
     end;


   procedure tobjectdef.check_forwards;
     begin
        if objecttype in [odt_interfacecom,odt_interfacecorba] then exit; { Kaz: ??? }
        pstoredsymtable(symtable)^.check_forwards;
        if (oo_is_forward in objectoptions) then
          begin
             { ok, in future, the forward can be resolved }
             Message1(sym_e_class_forward_not_resolved,objname^);
             exclude(objectoptions,oo_is_forward);
          end;
     end;


   { true, if self inherits from d (or if they are equal) }
   function tobjectdef.is_related(d : pobjectdef) : boolean;
     var
        hp : pobjectdef;
     begin
        hp:=@self;
        while assigned(hp) do
          begin
             if hp=d then
               begin
                  is_related:=true;
                  exit;
               end;
             hp:=hp^.childof;
          end;
        is_related:=false;
     end;

   var
      sd : pprocdef;

   procedure _searchdestructor(sym : pnamedindexobject);

     var
        p : pprocdef;

     begin
        { if we found already a destructor, then we exit }
        if assigned(sd) then
          exit;
        if psym(sym)^.typ=procsym then
          begin
             p:=pprocsym(sym)^.definition;
             while assigned(p) do
               begin
                  if p^.proctypeoption=potype_destructor then
                    begin
                       sd:=p;
                       exit;
                    end;
                  p:=p^.nextoverloaded;
               end;
          end;
     end;

   function tobjectdef.searchdestructor : pprocdef;

     var
        o : pobjectdef;

     begin
        searchdestructor:=nil;
        o:=@self;
        sd:=nil;
        while assigned(o) do
          begin
             symtable^.foreach({$ifdef FPCPROCVAR}@{$endif}_searchdestructor);
             if assigned(sd) then
               begin
                  searchdestructor:=sd;
                  exit;
               end;
             o:=o^.childof;
          end;
     end;

    function tobjectdef.size : longint;
      begin
        if objecttype in [odt_class,odt_interfacecom,odt_interfacecorba] then
          size:=target_os.size_of_pointer
        else
          size:=symtable^.datasize;
      end;


    function tobjectdef.alignment:longint;
      begin
        alignment:=symtable^.dataalignment;
      end;


    function tobjectdef.vmtmethodoffset(index:longint):longint;
      begin
        { for offset of methods for classes, see rtl/inc/objpash.inc }
        case objecttype of
        odt_class:
          vmtmethodoffset:=(index+12)*target_os.size_of_pointer;
        odt_interfacecom,odt_interfacecorba:
          vmtmethodoffset:=index*target_os.size_of_pointer;
        else
{$ifdef WITHDMT}
          vmtmethodoffset:=(index+4)*target_os.size_of_pointer;
{$else WITHDMT}
          vmtmethodoffset:=(index+3)*target_os.size_of_pointer;
{$endif WITHDMT}
        end;
      end;


    function tobjectdef.vmt_mangledname : string;
    {DM: I get a nil pointer on the owner name. I don't know if this
     may happen, and I have therefore fixed the problem by doing nil pointer
     checks.}
    var
      s1,s2:string;
    begin
        if not(oo_has_vmt in objectoptions) then
          Message1(parser_object_has_no_vmt,objname^);
        if owner^.name=nil then
          s1:=''
        else
          s1:=upper(owner^.name^);
        if objname=nil then
          s2:=''
        else
          s2:=Upper(objname^);
        vmt_mangledname:='VMT_'+s1+'$_'+s2;
    end;


    function tobjectdef.rtti_name : string;
    var
      s1,s2:string;
    begin
       if owner^.name=nil then
         s1:=''
       else
         s1:=upper(owner^.name^);
       if objname=nil then
         s2:=''
       else
         s2:=Upper(objname^);
       rtti_name:='RTTI_'+s1+'$_'+s2;
    end;


{$ifdef GDB}
    procedure addprocname(p :pnamedindexobject);
    var virtualind,argnames : string;
        news, newrec : pchar;
        pd,ipd : pprocdef;
        lindex : longint;
        para : TParaItem;
        arglength : byte;
        sp : char;

    begin
      If psym(p)^.typ = procsym then
       begin
                pd := pprocsym(p)^.definition;
                { this will be used for full implementation of object stabs
                not yet done }
                ipd := pd;
                while assigned(ipd^.nextoverloaded) do ipd := ipd^.nextoverloaded;
                if (po_virtualmethod in pd^.procoptions) then
                  begin
                    lindex := pd^.extnumber;
                    {doesnt seem to be necessary
                    lindex := lindex or $80000000;}
                    virtualind := '*'+tostr(lindex)+';'+ipd^._class^.classnumberstring+';'
                  end
                 else
                  virtualind := '.';

                 { used by gdbpas to recognize constructor and destructors }
                 if (pd^.proctypeoption=potype_constructor) then
                   argnames:='__ct__'
                 else if (pd^.proctypeoption=potype_destructor) then
                   argnames:='__dt__'
                 else
                   argnames := '';

                { arguments are not listed here }
                {we don't need another definition}
                 para := TParaItem(pd^.Para.first);
                 while assigned(para) do
                   begin
                   if Para.paratype.def^.deftype = formaldef then
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
                     if assigned(Para.paratype.def^.typesym) then
                       begin
                          arglength := length(Para.paratype.def^.typesym^.name);
                          argnames := argnames + tostr(arglength)+Para.paratype.def^.typesym^.name;
                       end
                     else
                       begin
                          argnames:=argnames+'11unnamedtype';
                       end;
                     end;
                   para := TParaItem(Para.next);
                   end;
                ipd^.is_def_stab_written := written;
                { here 2A must be changed for private and protected }
                { 0 is private 1 protected and 2 public }
                if (sp_private in psym(p)^.symoptions) then sp:='0'
                else if (sp_protected in psym(p)^.symoptions) then sp:='1'
                else sp:='2';
                newrec := strpnew(p^.name+'::'+ipd^.numberstring
                     +'=##'+pstoreddef(pd^.rettype.def)^.numberstring+';:'+argnames+';'+sp+'A'
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
               RecOffset := RecOffset + pd^.size;}
       end;
    end;


    function tobjectdef.stabstring : pchar;
      var anc : pobjectdef;
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
            strpcopy(stabRecString,'s'+tostr(symtable^.datasize));
            if assigned(childof) then
              begin
                {only one ancestor not virtual, public, at base offset 0 }
                {       !1           ,    0       2         0    ,       }
                strpcopy(strend(stabrecstring),'!1,020,'+childof^.classnumberstring+';');
              end;
            {virtual table to implement yet}
            OldRecOffset:=RecOffset;
            RecOffset := 0;
            symtable^.foreach({$ifdef FPCPROCVAR}@{$endif}addname);
            RecOffset:=OldRecOffset;
            if (oo_has_vmt in objectoptions) then
              if not assigned(childof) or not(oo_has_vmt in childof^.objectoptions) then
                 begin
                    strpcopy(strend(stabrecstring),'$vf'+classnumberstring+':'+typeglobalnumber('vtblarray')
                      +','+tostr(vmt_offset*8)+';');
                 end;
            symtable^.foreach({$ifdef FPCPROCVAR}@{$endif}addprocname);
            if (oo_has_vmt in objectoptions) then
              begin
                 anc := @self;
                 while assigned(anc^.childof) and (oo_has_vmt in anc^.childof^.objectoptions) do
                   anc := anc^.childof;
                 { just in case anc = self }
                 str_end:=';~%'+anc^.classnumberstring+';';
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
           sname := typesym^.name;
           sym_line_no:=typesym^.fileinfo.line;
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

      if ((typesym=nil) or ptypesym(typesym)^.isusedinstab or (cs_gdb_dbx in aktglobalswitches)) and
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
              st:=typesym^._name;
              typesym^._name:=stringdup(' ');
            end;
          inherited concatstabto(asmlist);
          if assigned(typesym) then
            begin
              stringdispose(typesym^._name);
              typesym^._name:=st;
            end;
        end;
      end;
{$endif GDB}


    procedure tobjectdef.write_child_init_data;
      begin
         symtable^.foreach({$ifdef FPCPROCVAR}@{$endif}generate_child_inittable);
      end;


    procedure tobjectdef.write_init_data;
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
         rttiList.concat(Tai_const.Create_8bit(length(objname^)));
         rttiList.concat(Tai_string.Create(objname^));

         rttiList.concat(Tai_const.Create_32bit(size));
         count:=0;
         if objecttype in [odt_interfacecom,odt_interfacecorba] then
           begin
           end
         else
           begin
              symtable^.foreach({$ifdef FPCPROCVAR}@{$endif}count_inittable_fields);
              rttiList.concat(Tai_const.Create_32bit(count));
              symtable^.foreach({$ifdef FPCPROCVAR}@{$endif}write_field_inittable);
           end;
      end;


    function tobjectdef.needs_inittable : boolean;
      var
         oldb : boolean;
      begin
         case objecttype of
            odt_interfacecom: needs_inittable:=true;
            odt_interfacecorba:
              needs_inittable:=is_related(interface_iunknown);
            odt_object:
              begin
                 { there are recursive calls to needs_inittable possible, }
                 { so we have to change to old value how else should      }
                 { we do that ? check_rec_rtti can't be a nested          }
                 { procedure of needs_rtti !                              }
                 oldb:=binittable;
                 binittable:=false;
                 symtable^.foreach({$ifdef FPCPROCVAR}@{$endif}check_rec_inittable);
                 needs_inittable:=binittable;
                 binittable:=oldb;
              end;
            else needs_inittable:=false;
         end;
      end;


    procedure count_published_properties(sym:pnamedindexobject);
      begin
         if needs_prop_entry(psym(sym)) and
          (psym(sym)^.typ<>varsym) then
           inc(count);
      end;


    procedure write_property_info(sym : pnamedindexobject);
      var
         proctypesinfo : byte;

      procedure writeproc(proc : psymlist; shiftvalue : byte);

        var
           typvalue : byte;
           hp : psymlistitem;
           address : longint;

        begin
           if not(assigned(proc) and assigned(proc^.firstsym))  then
             begin
                rttiList.concat(Tai_const.Create_32bit(1));
                typvalue:=3;
             end
           else if proc^.firstsym^.sym^.typ=varsym then
             begin
                address:=0;
                hp:=proc^.firstsym;
                while assigned(hp) do
                  begin
                     inc(address,pvarsym(hp^.sym)^.address);
                     hp:=hp^.next;
                  end;
                rttiList.concat(Tai_const.Create_32bit(address));
                typvalue:=0;
             end
           else
             begin
                if not(po_virtualmethod in pprocdef(proc^.def)^.procoptions) then
                  begin
                     rttiList.concat(Tai_const_symbol.Createname(pprocdef(proc^.def)^.mangledname));
                     typvalue:=1;
                  end
                else
                  begin
                     { virtual method, write vmt offset }
                     rttiList.concat(Tai_const.Create_32bit(
                       pprocdef(proc^.def)^._class^.vmtmethodoffset(pprocdef(proc^.def)^.extnumber)));
                     typvalue:=2;
                  end;
             end;
           proctypesinfo:=proctypesinfo or (typvalue shl shiftvalue);
        end;

      begin
         if needs_prop_entry(psym(sym)) then
           case psym(sym)^.typ of
              varsym:
                begin
{$ifdef dummy}
                   if not(pvarsym(sym)^.vartype.def^.deftype=objectdef) or
                     not(pobjectdef(pvarsym(sym)^.vartype.def)^.is_class) then
                     internalerror(1509992);
                   { access to implicit class property as field }
                   proctypesinfo:=(0 shl 0) or (0 shl 2) or (0 shl 4);
                   rttiList.concat(Tai_const_symbol.Createname(pvarsym(sym^.vartype.def^.get_rtti_label)));
                   rttiList.concat(Tai_const.Create_32bit(pvarsym(sym^.address)));
                   rttiList.concat(Tai_const.Create_32bit(pvarsym(sym^.address)));
                   { per default stored }
                   rttiList.concat(Tai_const.Create_32bit(1));
                   { index as well as ... }
                   rttiList.concat(Tai_const.Create_32bit(0));
                   { default value are zero }
                   rttiList.concat(Tai_const.Create_32bit(0));
                   rttiList.concat(Tai_const.Create_16bit(count));
                   inc(count);
                   rttiList.concat(Tai_const.Create_8bit(proctypesinfo));
                   rttiList.concat(Tai_const.Create_8bit(length(pvarsym(sym^.realname))));
                   rttiList.concat(Tai_string.Create(pvarsym(sym^.realname)));
{$endif dummy}
                end;
              propertysym:
                begin
                   if ppo_indexed in ppropertysym(sym)^.propoptions then
                     proctypesinfo:=$40
                   else
                     proctypesinfo:=0;
                   rttiList.concat(Tai_const_symbol.Createname(ppropertysym(sym)^.proptype.def^.get_rtti_label));
                   writeproc(ppropertysym(sym)^.readaccess,0);
                   writeproc(ppropertysym(sym)^.writeaccess,2);
                   { isn't it stored ? }
                   if not(ppo_stored in ppropertysym(sym)^.propoptions) then
                     begin
                        rttiList.concat(Tai_const.Create_32bit(0));
                        proctypesinfo:=proctypesinfo or (3 shl 4);
                     end
                   else
                     writeproc(ppropertysym(sym)^.storedaccess,4);
                   rttiList.concat(Tai_const.Create_32bit(ppropertysym(sym)^.index));
                   rttiList.concat(Tai_const.Create_32bit(ppropertysym(sym)^.default));
                   rttiList.concat(Tai_const.Create_16bit(count));
                   inc(count);
                   rttiList.concat(Tai_const.Create_8bit(proctypesinfo));
                   rttiList.concat(Tai_const.Create_8bit(length(ppropertysym(sym)^.realname)));
                   rttiList.concat(Tai_string.Create(ppropertysym(sym)^.realname));
                end;
              else internalerror(1509992);
           end;
      end;


    procedure generate_published_child_rtti(sym : pnamedindexobject);
      begin
         if needs_prop_entry(psym(sym)) then
           case psym(sym)^.typ of
              varsym:
                ;
                { now ignored:
                pvarsym(sym)^.vartype.def^.get_rtti_label;
                }
              propertysym:
                ppropertysym(sym)^.proptype.def^.get_rtti_label;
              else
                internalerror(1509991);
           end;
      end;


    procedure tobjectdef.write_child_rtti_data;
      begin
         symtable^.foreach({$ifdef FPCPROCVAR}@{$endif}generate_published_child_rtti);
      end;


    procedure tobjectdef.generate_rtti;
      begin
         if not has_rtti then
          begin
            has_rtti:=true;
            getdatalabel(rtti_label);
            write_child_rtti_data;
            rttiList.concat(Tai_symbol.Createname_global(rtti_name,0));
            rttiList.concat(Tai_label.Create(rtti_label));
            write_rtti_data;
            rttiList.concat(Tai_symbol_end.Createname(rtti_name));
          end;
      end;

    type
       tclasslistitem = class(tlinkedlistitem)
          index : longint;
          p : pobjectdef;
       end;

    var
       classtablelist : tlinkedlist;
       tablecount : longint;

    function searchclasstablelist(p : pobjectdef) : tclasslistitem;

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

    procedure count_published_fields(sym:pnamedindexobject);
      var
         hp : tclasslistitem;
      begin
         if needs_prop_entry(psym(sym)) and
          (psym(sym)^.typ=varsym) then
          begin
             if pvarsym(sym)^.vartype.def^.deftype<>objectdef then
               internalerror(0206001);
             hp:=searchclasstablelist(pobjectdef(pvarsym(sym)^.vartype.def));
             if not(assigned(hp)) then
               begin
                  hp:=tclasslistitem.create;
                  hp.p:=pobjectdef(pvarsym(sym)^.vartype.def);
                  hp.index:=tablecount;
                  classtablelist.concat(hp);
                  inc(tablecount);
               end;
             inc(count);
          end;
      end;

    procedure writefields(sym:pnamedindexobject);
      var
         hp : tclasslistitem;
      begin
         if needs_prop_entry(psym(sym)) and
          (psym(sym)^.typ=varsym) then
          begin
             rttiList.concat(Tai_const.Create_32bit(pvarsym(sym)^.address));
             hp:=searchclasstablelist(pobjectdef(pvarsym(sym)^.vartype.def));
             if not(assigned(hp)) then
               internalerror(0206002);
             rttiList.concat(Tai_const.Create_16bit(hp.index));
             rttiList.concat(Tai_const.Create_8bit(length(pvarsym(sym)^.realname)));
             rttiList.concat(Tai_string.Create(pvarsym(sym)^.realname));
          end;
      end;

    function tobjectdef.generate_field_table : pasmlabel;

      var
         fieldtable,
         classtable : pasmlabel;
         hp : tclasslistitem;

      begin
         classtablelist:=TLinkedList.Create;
         getdatalabel(fieldtable);
         getdatalabel(classtable);
         count:=0;
         tablecount:=0;
         symtable^.foreach({$ifdef FPC}@{$endif}count_published_fields);
         rttiList.concat(Tai_label.Create(fieldtable));
         rttiList.concat(Tai_const.Create_16bit(count));
         rttiList.concat(Tai_const_symbol.Create(classtable));
         symtable^.foreach({$ifdef FPC}@{$endif}writefields);

         { generate the class table }
         rttiList.concat(Tai_label.Create(classtable));
         rttiList.concat(Tai_const.Create_16bit(tablecount));
         hp:=tclasslistitem(classtablelist.first);
         while assigned(hp) do
           begin
              rttiList.concat(Tai_const_symbol.Createname(pobjectdef(hp.p)^.vmt_mangledname));
              hp:=tclasslistitem(hp.next);
           end;

         generate_field_table:=fieldtable;
         classtablelist.free;
      end;

    function tobjectdef.next_free_name_index : longint;
      var
         i : longint;
      begin
         if assigned(childof) and (oo_can_have_published in childof^.objectoptions) then
           i:=childof^.next_free_name_index
         else
           i:=0;
         count:=0;
         symtable^.foreach({$ifdef FPCPROCVAR}@{$endif}count_published_properties);
         next_free_name_index:=i+count;
      end;


    procedure tobjectdef.write_rtti_data;
      begin
         case objecttype of
           odt_class: rttiList.concat(Tai_const.Create_8bit(tkclass));
           odt_object: rttiList.concat(Tai_const.Create_8bit(tkobject));
           odt_interfacecom: rttiList.concat(Tai_const.Create_8bit(tkinterface));
           odt_interfacecorba: rttiList.concat(Tai_const.Create_8bit(tkinterfaceCorba));
         else
           exit;
         end;


         { generate the name }
         rttiList.concat(Tai_const.Create_8bit(length(objname^)));
         rttiList.concat(Tai_string.Create(objname^));

         if objecttype in [odt_interfacecom,odt_interfacecorba] then
           rttiList.concat(Tai_const.Create_32bit(0))
         else
           rttiList.concat(Tai_const_symbol.Createname(vmt_mangledname));

         { write owner typeinfo }
         if assigned(childof) and (oo_can_have_published in childof^.objectoptions) then
           rttiList.concat(Tai_const_symbol.Createname(childof^.get_rtti_label))
         else
           rttiList.concat(Tai_const.Create_32bit(0));

         { count total number of properties }
         if assigned(childof) and (oo_can_have_published in childof^.objectoptions) then
           count:=childof^.next_free_name_index
         else
           count:=0;

         { write it }
         symtable^.foreach({$ifdef FPCPROCVAR}@{$endif}count_published_properties);
         rttiList.concat(Tai_const.Create_16bit(count));

         { write unit name }
         rttiList.concat(Tai_const.Create_8bit(length(current_module.realmodulename^)));
         rttiList.concat(Tai_string.Create(current_module.realmodulename^));

         { write published properties count }
         count:=0;
         symtable^.foreach({$ifdef FPCPROCVAR}@{$endif}count_published_properties);
         rttiList.concat(Tai_const.Create_16bit(count));

         { count is used to write nameindex   }

         { but we need an offset of the owner }
         { to give each property an own slot  }
         if assigned(childof) and (oo_can_have_published in childof^.objectoptions) then
           count:=childof^.next_free_name_index
         else
           count:=0;

         symtable^.foreach({$ifdef FPCPROCVAR}@{$endif}write_property_info);
      end;


    function tobjectdef.is_publishable : boolean;
      begin
         is_publishable:=objecttype in [odt_class,odt_interfacecom,odt_interfacecorba];
      end;

    function  tobjectdef.get_rtti_label : string;

      begin
         generate_rtti;
         get_rtti_label:=rtti_name;
      end;

{****************************************************************************
                             TIMPLEMENTEDINTERFACES
****************************************************************************}
    type
      pnamemap = ^tnamemap;
      tnamemap = object(tnamedindexobject)
        newname: pstring;
        constructor init(const aname, anewname: string);
        destructor  done; virtual;
      end;

    constructor tnamemap.init(const aname, anewname: string);
      begin
        inherited initname(name);
        newname:=stringdup(anewname);
      end;

    destructor  tnamemap.done;
      begin
        stringdispose(newname);
        inherited done;
      end;


    type
      pprocdefstore = ^tprocdefstore;
      tprocdefstore = object(tnamedindexobject)
        procdef: pprocdef;
        constructor init(aprocdef: pprocdef);
      end;

    constructor tprocdefstore.init(aprocdef: pprocdef);
      begin
        inherited init;
        procdef:=aprocdef;
      end;


    type
      pimplintfentry = ^timplintfentry;
      timplintfentry = object(tnamedindexobject)
        intf: pobjectdef;
        ioffs: longint;
        namemappings: pdictionary;
        procdefs: pindexarray;
        constructor init(aintf: pobjectdef);
        destructor  done; virtual;
      end;

    constructor timplintfentry.init(aintf: pobjectdef);
      begin
        inherited init;
        intf:=aintf;
        ioffs:=-1;
        namemappings:=nil;
        procdefs:=nil;
      end;

    destructor  timplintfentry.done;
      begin
        if assigned(namemappings) then
          dispose(namemappings,done);
        if assigned(procdefs) then
          dispose(procdefs,done);
        inherited done;
      end;


    constructor timplementedinterfaces.init;
      begin
        finterfaces.init(1);
      end;

    destructor  timplementedinterfaces.done;
      begin
        finterfaces.done;
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

    function  timplementedinterfaces.interfaces(intfindex: longint): pobjectdef;
      begin
        checkindex(intfindex);
        interfaces:=pimplintfentry(finterfaces.search(intfindex))^.intf;
      end;

    function  timplementedinterfaces.ioffsets(intfindex: longint): plongint;
      begin
        checkindex(intfindex);
        ioffsets:=@pimplintfentry(finterfaces.search(intfindex))^.ioffs;
      end;

    function  timplementedinterfaces.searchintf(def: pdef): longint;
      var
        i: longint;
      begin
        i:=1;
        while (i<=count) and (pdef(interfaces(i))<>def) do inc(i);
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
          with pimplintfentry(finterfaces.search(i))^ do
            resolvedef(pdef(intf));
      end;

    procedure timplementedinterfaces.addintfref(def: pdef);
      begin
        finterfaces.insert(new(pimplintfentry,init(pobjectdef(def))));
      end;

    procedure timplementedinterfaces.addintf(def: pdef);
      begin
        if not assigned(def) or (searchintf(def)<>-1) or (def^.deftype<>objectdef) or
           not (pobjectdef(def)^.objecttype in [odt_interfacecom,odt_interfacecorba]) then
          internalerror(200006124);
        finterfaces.insert(new(pimplintfentry,init(pobjectdef(def))));
      end;

    procedure timplementedinterfaces.clearmappings;
      var
        i: longint;
      begin
        for i:=1 to count do
          with pimplintfentry(finterfaces.search(i))^ do
            begin
             if assigned(namemappings) then
               dispose(namemappings,done);
             namemappings:=nil;
            end;
      end;

    procedure timplementedinterfaces.addmappings(intfindex: longint; const name, newname: string);
      begin
        checkindex(intfindex);
        with pimplintfentry(finterfaces.search(intfindex))^ do
          begin
            if not assigned(namemappings) then
              new(namemappings,init);
            namemappings^.insert(new(pnamemap,init(name,newname)));
          end;
      end;

    function  timplementedinterfaces.getmappings(intfindex: longint; const name: string; var nextexist: pointer): string;
      begin
        checkindex(intfindex);
        if not assigned(nextexist) then
          with pimplintfentry(finterfaces.search(intfindex))^ do
            begin
              if assigned(namemappings) then
                nextexist:=namemappings^.search(name)
              else
                nextexist:=nil;
            end;
        if assigned(nextexist) then
          begin
            getmappings:=pnamemap(nextexist)^.newname^;
            nextexist:=pnamemap(nextexist)^.listnext;
          end
        else
          getmappings:='';
      end;

    procedure timplementedinterfaces.clearimplprocs;
      var
        i: longint;
      begin
        for i:=1 to count do
          with pimplintfentry(finterfaces.search(i))^ do
            begin
              if assigned(procdefs) then
                dispose(procdefs,done);
              procdefs:=nil;
            end;
      end;

    procedure timplementedinterfaces.addimplproc(intfindex: longint; procdef: pprocdef);
      begin
        checkindex(intfindex);
        with pimplintfentry(finterfaces.search(intfindex))^ do
          begin
            if not assigned(procdefs) then
              new(procdefs,init(4));
            procdefs^.insert(new(pprocdefstore,init(procdef)));
          end;
      end;

    function  timplementedinterfaces.implproccount(intfindex: longint): longint;
      begin
        checkindex(intfindex);
        with pimplintfentry(finterfaces.search(intfindex))^ do
          if assigned(procdefs) then
            implproccount:=procdefs^.count
          else
            implproccount:=0;
      end;

    function  timplementedinterfaces.implprocs(intfindex: longint; procindex: longint): pprocdef;
      begin
        checkindex(intfindex);
        with pimplintfentry(finterfaces.search(intfindex))^ do
          if assigned(procdefs) then
            implprocs:=pprocdefstore(procdefs^.search(procindex))^.procdef
          else
            internalerror(200006131);
      end;

    function  timplementedinterfaces.isimplmergepossible(intfindex, remainindex: longint; var weight: longint): boolean;
      var
        possible: boolean;
        i: longint;
        iiep1: pindexarray;
        iiep2: pindexarray;
      begin
        checkindex(intfindex);
        checkindex(remainindex);
        iiep1:=pimplintfentry(finterfaces.search(intfindex))^.procdefs;
        iiep2:=pimplintfentry(finterfaces.search(remainindex))^.procdefs;
        if not assigned(iiep1) then { empty interface is mergeable :-) }
          begin
            possible:=true;
            weight:=0;
          end
        else
          begin
            possible:=assigned(iiep2) and (iiep1^.count<=iiep2^.count);
            i:=1;
            while (possible) and (i<=iiep1^.count) do
              begin
                possible:=
                  pprocdefstore(iiep1^.search(i))^.procdef=
                  pprocdefstore(iiep2^.search(i))^.procdef;
                inc(i);
              end;
            if possible then
              weight:=iiep1^.count;
          end;
        isimplmergepossible:=possible;
      end;

{****************************************************************************
                                TFORWARDDEF
****************************************************************************}

   constructor tforwarddef.init(const s:string;const pos : tfileposinfo);
     var
       oldregisterdef : boolean;
     begin
        { never register the forwarddefs, they are disposed at the
          end of the type declaration block }
        oldregisterdef:=registerdef;
        registerdef:=false;
        inherited init;
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

   constructor terrordef.init;
     begin
        inherited init;
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
          symt : psymtable;
          old_make_ref : boolean;
      begin
         old_make_ref:=make_ref;
         make_ref:=false;
         typeglobalnumber := '0';
         srsym := nil;
         if pos('.',s) > 0 then
           begin
           st := copy(s,1,pos('.',s)-1);
           getsym(st,false);
           st := copy(s,pos('.',s)+1,255);
           if assigned(srsym) then
             begin
             if srsym^.typ = unitsym then
               begin
               symt := punitsym(srsym)^.unitsymtable;
               srsym := psym(symt^.search(st));
               end else srsym := nil;
             end;
           end else st := s;
         if srsym = nil then getsym(st,true);
         if srsym^.typ<>typesym then
           begin
             Message(type_e_type_id_expected);
             exit;
           end;
         typeglobalnumber := pstoreddef(ptypesym(srsym)^.restype.def)^.numberstring;
         make_ref:=old_make_ref;
      end;
{$endif GDB}


{****************************************************************************
                           Definition Helpers
****************************************************************************}

   procedure reset_global_defs;
     var
       def     : pstoreddef;
{$ifdef debug}
       prevdef : pstoreddef;
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
            if assigned(def^.typesym) then
              ptypesym(def^.typesym)^.isusedinstab:=false;
            def^.is_def_stab_written:=not_written;
{$endif GDB}
            {if not current_module.in_implementation then}
              begin
                { reset rangenr's }
                case def^.deftype of
                  orddef   : porddef(def)^.rangenr:=0;
                  enumdef  : penumdef(def)^.rangenr:=0;
                  arraydef : parraydef(def)^.rangenr:=0;
                end;
                if def^.deftype<>objectdef then
                  def^.has_rtti:=false;
                def^.has_inittable:=false;
              end;
{$ifdef debug}
            prevdef:=def;
{$endif debug}
            def:=def^.nextglobal;
          end;
     end;

    function is_interfacecom(def: pdef): boolean;
      begin
        is_interfacecom:=
          assigned(def) and
          (def^.deftype=objectdef) and
          (pobjectdef(def)^.objecttype=odt_interfacecom);
      end;

    function is_interfacecorba(def: pdef): boolean;
      begin
        is_interfacecorba:=
          assigned(def) and
          (def^.deftype=objectdef) and
          (pobjectdef(def)^.objecttype=odt_interfacecorba);
      end;

    function is_interface(def: pdef): boolean;
      begin
        is_interface:=
          assigned(def) and
          (def^.deftype=objectdef) and
          (pobjectdef(def)^.objecttype in [odt_interfacecom,odt_interfacecorba]);
      end;


    function is_class(def: pdef): boolean;
      begin
        is_class:=
          assigned(def) and
          (def^.deftype=objectdef) and
          (pobjectdef(def)^.objecttype=odt_class);
      end;

    function is_object(def: pdef): boolean;
      begin
        is_object:=
          assigned(def) and
          (def^.deftype=objectdef) and
          (pobjectdef(def)^.objecttype=odt_object);
      end;

    function is_cppclass(def: pdef): boolean;
      begin
        is_cppclass:=
          assigned(def) and
          (def^.deftype=objectdef) and
          (pobjectdef(def)^.objecttype=odt_cppclass);
      end;

    function is_class_or_interface(def: pdef): boolean;
      begin
        is_class_or_interface:=
          assigned(def) and
          (def^.deftype=objectdef) and
          (pobjectdef(def)^.objecttype in [odt_class,odt_interfacecom,odt_interfacecorba]);
      end;

end.
{
  $Log$
  Revision 1.19  2000-12-25 00:07:29  peter
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
    * fixed bug where the original resulttype wasn't restored correctly
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
