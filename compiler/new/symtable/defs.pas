{
    $Id$

    Copyright (C) 1998-2000 by Daniel Mantione
     and other members of the Free Pascal development team

    This unit handles definitions

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
{$ifdef TP}
  {$N+,E+,F+}
{$endif}

unit defs;

interface

uses    symtable,objects,{$IFDEF TP}xobjects,{$ENDIF}
        cobjects,symtablt,globtype
{$ifdef i386}
        ,cpubase
{$endif}
{$ifdef m68k}
        ,m68k
{$endif}
{$ifdef alpha}
        ,alpha
{$endif};

type    Targconvtyp=(act_convertable,act_equal,act_exact);

        Tvarspez=(vs_value,vs_const,vs_var);

        Tobjoption=(oo_has_abstract,         {The object/class has
                                             an abstract method => no
                                             instances can be created.}
                    oo_is_class,            {The object is a class.}
                    oo_has_virtual,         {The object/class has
                                             virtual methods.}
                    oo_isforward,           {The class is only a forward
                                             declared yet.}
                    oo_can_have_published,  {True, if the class has rtti, i.e.
                                             you can publish properties.}
                    oo_has_constructor,     {The object/class has a
                                             constructor.}
                    oo_has_destructor,      {The object/class has a
                                             destructor.}

                    {When has_virtual is set, has_vmt is also set....
                    oo_has_vmt,             The object/class has a vmt.}
                    oo_has_msgstr,
                    oo_has_msgint,
                    oo_cppvmt);             {The object/class uses an C++
                                             compatible vmt, all members of
                                             the same class tree, must use
                                             then a C++ compatible vmt.}
        Tobjoptionset=set of Tobjoption;

        {Calling convention for tprocdef and Tprocvardef.}
        Tproccalloption=(po_call_none,
                         po_call_clearstack,    {Use IBM flat calling
                                                 convention. (Used by GCC.)}
                         po_call_leftright,     {Push parameters from left to
                                                 right.}
                         po_call_cdecl,         {Procedure uses C styled
                                                 calling.}
                         po_call_register,      {Procedure uses register
                                                 (fastcall) calling.}
                         po_call_stdcall,       {Procedure uses stdcall
                                                 call.}
                         po_call_safecall,      {Safe call calling
                                                 conventions.}
                         po_call_palmossyscall, {Procedure is a PalmOS
                                                 system call.}
                         po_call_system,
                         po_call_inline,        {Procedure is an assembler
                                                 macro.}
                         po_call_internproc,    {Procedure has compiler
                                                 magic.}
                         po_call_internconst);  {Procedure has constant
                                                 evaluator intern.}
        Tproccalloptionset=set of Tproccalloption;

        {Basic type for tprocdef and tprocvardef }
        Tproctypeoption=(po_type_none,
                         po_type_proginit,      {Program initialization.}
                         po_type_unitinit,      {Unit initialization.}
                         po_type_unitfinalize,  {Unit finalization.}
                         po_type_constructor,   {Procedure is a constructor.}
                         po_type_destructor,    {Procedure is a destructor.}
                         po_type_operator);     {Procedure defines an
                                                 operator.}

        {Other options for Tprocdef and Tprocvardef.}
        Tprocoption=(po_none,
            po_classmethod,         {Class method.}
            po_virtualmethod,       {Procedure is a virtual method.}
            po_abstractmethod,      {Procedure is an abstract method.}
            po_staticmethod,        {Static method.}
            po_overridingmethod,    {Method with override directive.}
            po_methodpointer,       {Method pointer, only in procvardef, also
                                     used for 'with object do'.}
            po_containsself,        {Self is passed explicit to the
                                     compiler.}
            po_interrupt,           {Procedure is an interrupt handler.}
            po_iocheck,             {IO checking should be done after a call
                                     to the procedure.}
            po_assembler,           {Procedure is written in assembler.}
            po_msgstr,              {Method for string message handling.}
            po_msgint,              {Method for int message handling.}
            po_exports,             {Procedure has export directive (needed
                                     for OS/2).}
            po_external,            {Procedure is external (in other object
                                     or lib).}
            po_savestdregs,         {Save std regs cdecl and stdcall need
                                     that!}
            po_saveregisters);      {Save all registers }
        Tprocoptionset=set of Tprocoption;

        Tarrayoption=(ap_variant,ap_constructor,ap_arrayofconst);
        Tarrayoptionset=set of Tarrayoption;

        Pparameter=^Tparameter;
        Tparameter=object(Tobject)
            data:Psym;
            paratyp:Tvarspez;
            argconvtyp:Targconvtyp;
            convertlevel:byte;
            register:Tregister;
        end;

        Tfiletype=(ft_text,ft_typed,ft_untyped);

        Pfiledef=^Tfiledef;
        Tfiledef=object(Tdef)
            filetype:Tfiletype;
            definition:Pdef;
            constructor init(Aowner:Pcontainingsymtable;
                             ft:Tfiletype;tas:Pdef);
            constructor load(var s:Tstream);
            procedure deref;virtual;
            function gettypename:string;virtual;
            procedure setsize;
{$ifdef GDB}
            function stabstring:Pchar;virtual;
            procedure concatstabto(asmlist:Paasmoutput);virtual;
{$endif GDB}
            procedure store(var s:Tstream);virtual;
        end;

        Pformaldef=^Tformaldef;
        Tformaldef=object(Tdef)
            constructor init(Aowner:Pcontainingsymtable);
            constructor load(var s:Tstream);
            procedure store(var s:Tstream);virtual;
{$ifdef GDB}
            function stabstring:Pchar;virtual;
            procedure concatstabto(asmlist:Paasmoutput);virtual;
{$endif GDB}
            function gettypename:string;virtual;
        end;

        Perrordef=^Terrordef;
        Terrordef=object(Tdef)
{$IFDEF TP}
            constructor init(Aowner:Pcontainingsymtable);
{$ENDIF}
{$ifdef GDB}
            function stabstring:Pchar;virtual;
{$endif GDB}
            function gettypename:string;virtual;
        end;

        Pabstractpointerdef=^Tabstractpointerdef;
        Tabstractpointerdef=object(Tdef)
            definition:Pdef;
            defsym:Psym;
            constructor init(Aowner:Pcontainingsymtable;def:Pdef);
            constructor load(var s:Tstream);
            procedure deref;virtual;
            procedure store(var s:Tstream);virtual;
{$ifdef GDB}
            function  stabstring:Pchar;virtual;
            procedure concatstabto(asmlist:Paasmoutput);virtual;
{$endif GDB}
        end;

        Ppointerdef=^Tpointerdef;
        Tpointerdef=object(Tabstractpointerdef)
            is_far:boolean;
            constructor initfar(Aowner:Pcontainingsymtable;def:Pdef);
            constructor load(var s:Tstream);
            procedure store(var s:Tstream);virtual;
            function gettypename:string;virtual;
        end;

        Pclassrefdef=^Tclassrefdef;
        Tclassrefdef=object(Tpointerdef)
{$IFDEF TP}
            constructor init(Aowner:Pcontainingsymtable;def:Pdef);
{$ENDIF TP}
{$ifdef GDB}
            function stabstring : pchar;virtual;
            procedure concatstabto(asmlist : paasmoutput);virtual;
{$endif GDB}
            function gettypename:string;virtual;
        end;

        Pvmtentry=^Tvmtentry;
        Pglobalvmtentry=^Tglobalvmtentry;
        Plocalvmtentry=^Tlocalvmtentry;
        Pobjectdef=^Tobjectdef;
        Pabstractprocdef=^Pabstractprocdef;
        Pprocvardef=^Tprocvardef;
        Pprocdef = ^Tprocdef;

        Tvmtentry=object(Tobject)
            owner:Pobjectdef;
            constructor init(Aowner:Pobjectdef);
            function mangledname:string;virtual;
        end;

        Tglobalvmtentry=object(Tvmtentry)
            constructor init(Aowner:Pobjectdef;proc:Pprocdef);
            function mangledname:string;virtual;
        private
            def:Pprocdef;
        end;

        Tlocalvmtentry=object(Tvmtentry)
            constructor init(Aowner:Pobjectdef;proc:Pprocdef);
            function mangledname:string;virtual;
        private
            name:Pstring;
        end;

        Tobjectdef=object(Tdef)
            childof:Pobjectdef;
            objname:Pstring;
            privatesyms,
            protectedsyms,
            publicsyms:Pobjectsymtable;
            options:Tobjoptionset;
            {To be able to have a variable vmt position
             and no vmt field for objects without virtuals.}
            vmt_offset:longint;
            {Contains Tvmtentry objects to describe the layout of the vmt.}
            vmt_layout:Pcollection;
            constructor init(const n:string;Aowner:Pcontainingsymtable;
                             parent:Pobjectdef;isclass:boolean);
            constructor load(var s:Tstream);
            procedure check_forwards;
            function insert(Asym:Psym):boolean;
            procedure insertvmt;
            function is_related(d:Pobjectdef):boolean;
            function search(const s:string;search_protected:boolean):Psym;
            function speedsearch(const s:string;speedvalue:longint;
                                 search_protected:boolean):Psym;virtual;
            function size:longint;virtual;
            procedure store(var s:Tstream);virtual;
            function vmt_mangledname : string;
            function rtti_name : string;

            procedure set_parent(parent:Pobjectdef);
{$ifdef GDB}
            function stabstring : pchar;virtual;
{$endif GDB}
            procedure deref;virtual;

            function  needs_inittable:boolean;virtual;
            procedure write_init_data;virtual;
            procedure write_child_init_data;virtual;

            {Rtti }
            function  get_rtti_label:string;virtual;
            procedure generate_rtti;virtual;
            procedure write_rtti_data;virtual;
            procedure write_child_rtti_data;virtual;
            function next_free_name_index:longint;
            function is_publishable:boolean;virtual;
            destructor done;virtual;
        end;

        Parraydef=^Tarraydef;
        Tarraydef=object(Tdef)
            lowrange,
            highrange:Tconstant;
            definition:Pdef;
            rangedef:Pdef;
            options:Tarrayoptionset;
            constructor init(const l,h:Tconstant;rd:Pdef;
                             Aowner:Pcontainingsymtable);
            constructor load(var s:Tstream);
            function elesize:longint;
            function gettypename:string;virtual;
            procedure store(var s:Tstream);virtual;
{$ifdef GDB}
            function stabstring : pchar;virtual;
            procedure concatstabto(asmlist : paasmoutput);virtual;
{$endif GDB}
            procedure deref;virtual;
            function size : longint;virtual;
            { generates the ranges needed by the asm instruction BOUND (i386)
              or CMP2 (Motorola) }
            procedure genrangecheck;

            { returns the label of the range check string }
            function getrangecheckstring : string;
            function needs_inittable : boolean;virtual;
            procedure write_rtti_data;virtual;
            procedure write_child_rtti_data;virtual;
        private
            rangenr:longint;
        end;

        Penumdef=^Tenumdef;
        Tenumdef=object(Tdef)
            rangenr,
            minval,
            maxval:longint;
            has_jumps:boolean;
            symbols:Pcollection;
            basedef:Penumdef;
            constructor init(Aowner:Pcontainingsymtable);
            constructor init_subrange(Abasedef:Penumdef;Amin,Amax:longint;
                                      Aowner:Pcontainingsymtable);
            constructor load(var s:Tstream);
            procedure deref;virtual;
            procedure calcsavesize;
            function getrangecheckstring:string;
            procedure genrangecheck;
            procedure setmax(Amax:longint);
            procedure setmin(Amin:longint);
            procedure store(var s:Tstream);virtual;
{$ifdef GDB}
            function stabstring:Pchar;virtual;
{$endif GDB}
            procedure write_child_rtti_data;virtual;
            procedure write_rtti_data;virtual;
            function is_publishable : boolean;virtual;
            function  gettypename:string;virtual;
        end;

        Tbasetype=(uauto,uvoid,uchar,
                   u8bit,u16bit,u32bit,
                   s8bit,s16bit,s32bit,
                   bool8bit,bool16bit,bool32bit,
                   s64bit,u64bit,s64bitint,uwidechar);

        Porddef=^Torddef;
        Torddef=object(Tdef)
            low,high:Tconstant;
            rangenr:longint;
            typ:Tbasetype;
            constructor init(t:tbasetype;l,h:Tconstant;
                             Aowner:Pcontainingsymtable);
            constructor load(var s:Tstream);
            procedure store(var s:Tstream);virtual;
            procedure setsize;

            { generates the ranges needed by the asm instruction BOUND }
            { or CMP2 (Motorola)                                       }
            procedure genrangecheck;
            { returns the label of the range check string }
            function getrangecheckstring : string;
            procedure write_rtti_data;virtual;
            function is_publishable:boolean;virtual;
            function gettypename:string;virtual;
{$ifdef GDB}
            function stabstring:Pchar;virtual;
{$endif GDB}
        end;

        {S80real is dependant on the cpu, s64comp is also
         dependant on the size (tp = 80bit for both)
         The EXTENDED format exists on the motorola FPU
         but it uses 96 bits instead of 80, with some
         unused bits within the number itself! Pretty
         complicated to support, so no support for the
         moment.
         S64comp is considered as a real because all
         calculations are done by the fpu.}

        Tfloattype=(s32real,s64real,s80real,s64comp,f16bit,f32bit);

        Pfloatdef=^Tfloatdef;
        Tfloatdef=object(tdef)
            typ:Tfloattype;
            constructor init(t:Tfloattype;Aowner:Pcontainingsymtable);
            constructor load(var s:Tstream);
            function is_publishable : boolean;virtual;
            procedure setsize;
{$ifdef GDB}
            function stabstring:Pchar;virtual;
{$endif GDB}
            procedure store(var s:Tstream);virtual;
            procedure write_rtti_data;virtual;
            function gettypename:string;virtual;
        end;

        Tsettype=(normset,smallset,varset);

        Psetdef=^Tsetdef;
        Tsetdef=object(Tdef)
            definition:Pdef;
            settype:Tsettype;
            constructor init(s:Pdef;high:longint;Aowner:Pcontainingsymtable);
            constructor load(var s:Tstream);
            procedure store(var s:Tstream);virtual;
{$ifdef GDB}
            function stabstring : pchar;virtual;
            procedure concatstabto(asmlist : paasmoutput);virtual;
{$endif GDB}
            procedure deref;virtual;
            function is_publishable : boolean;virtual;
            procedure write_rtti_data;virtual;
            procedure write_child_rtti_data;virtual;
            function gettypename:string;virtual;
        end;

        Precorddef=^Trecorddef;
        Trecorddef=object(Tdef)
            symtable:Precordsymtable;
            constructor init(s:Precordsymtable;Aowner:Pcontainingsymtable);
            constructor load(var s:Tstream);
            procedure store(var s:Tstream);virtual;
{$ifdef GDB}
            function stabstring : pchar;virtual;
            procedure concatstabto(asmlist : paasmoutput);virtual;
{$endif GDB}
            procedure deref;virtual;
            function  needs_inittable : boolean;virtual;
            procedure write_rtti_data;virtual;
            procedure write_init_data;virtual;
            procedure write_child_rtti_data;virtual;
            procedure write_child_init_data;virtual;
            function gettypename:string;virtual;
            destructor done;virtual;
        end;

        {String types}
        Tstringtype=(st_default,st_shortstring,st_longstring,
                     st_ansistring,st_widestring);

        {This object needs to be splitted into multiple objects,
         one for each stringtype. This is because all code in this
         object is different for all string types.}
        Pstringdef=^Tstringdef;
        Tstringdef=object(Tdef)
            string_typ:Tstringtype;
            len:longint;
            constructor shortinit(l:byte;Aowner:Pcontainingsymtable);
            constructor shortload(var s:Tstream);
            constructor longinit(l:longint;Aowner:Pcontainingsymtable);
            constructor longload(var s:Tstream);
            constructor ansiinit(l:longint;Aowner:Pcontainingsymtable);
            constructor ansiload(var s:Tstream);
            constructor wideinit(l:longint;Aowner:Pcontainingsymtable);
            constructor wideload(var s:Tstream);
            function  stringtypname:string;
            function  size:longint;virtual;
            procedure store(var s:Tstream);virtual;
            function  gettypename:string;virtual;
            function  is_publishable : boolean;virtual;
            { debug }
        {$ifdef GDB}
            function  stabstring:Pchar;virtual;
            procedure concatstabto(asmlist : Paasmoutput);virtual;
        {$endif GDB}
            { init/final }
            function  needs_inittable : boolean;virtual;
            { rtti }
            procedure write_rtti_data;virtual;
        end;

        Tabstractprocdef=object(Tdef)
            {Saves a definition to the return type }
            retdef:Pdef;
            fpu_used:byte;              {How many stack fpu must be empty.}
            proctype:Tproctypeoption;
            options:Tprocoptionset;     {Save the procedure options.}
            calloptions:Tproccalloptionset;
            parameters:Pcollection;
            constructor init(Aowner:Pcontainingsymtable);
            constructor load(var s:Tstream);
            destructor done;virtual;
            procedure deref;virtual;
            function demangled_paras:string;
            function para_size:longint;
            procedure store(var s:Tstream);virtual;
            procedure test_if_fpu_result;
{$ifdef GDB}
            function stabstring : pchar;virtual;
            procedure concatstabto(asmlist : paasmoutput);virtual;
{$endif GDB}
        end;

        Tprocvardef=object(Tabstractprocdef)
{$IFDEF TP}
            constructor init(Aowner:Pcontainingsymtable);
{$ENDIF TP}
            function size:longint;virtual;
{$ifdef GDB}
            function stabstring:Pchar;virtual;
            procedure concatstabto(asmlist:Paasmoutput); virtual;
{$endif GDB}
            procedure write_child_rtti_data;virtual;
            function is_publishable:boolean;virtual;
            procedure write_rtti_data;virtual;
            function gettypename:string;virtual;
        end;

        {This datastructure is used to store the message information
         when a procedure is declared as:
          ;message 'str';
          ;message int;
          ;virtual int;
        }
        Tmessageinf=record
            case integer of
                0:(str:Pchar);
                1:(i:longint);
        end;

        {This object can be splitted into a Tprocdef, for normal procedures,
         a Tmethoddef for methods, and a Tinlinedprocdef and a
         Tinlinedmethoddef for inlined procedures.}
        Tprocdef = object(tabstractprocdef)
           messageinf:Tmessageinf;
           { where is this function defined, needed here because there
             is only one symbol for all overloaded functions }
           fileinfo:Tfileposinfo;
           { pointer to the local symbol table }
           localst:Pprocsymtable;
           _mangledname:Pstring;
           { it's a tree, but this not easy to handle }
           { used for inlined procs                   }
           code : pointer;
           vmt_index:longint;
           { true, if the procedure is only declared }
           { (forward procedure) }
           references:Pcollection;
           forwarddef,
           { true if the procedure is declared in the interface }
           interfacedef : boolean;
           { check the problems of manglednames }
           count      : boolean;
           is_used    : boolean;
           { set which contains the modified registers }
           usedregisters:Tregisterset;
           constructor init(Aowner:Pcontainingsymtable);
           constructor load(var s:Tstream);
           procedure store(var s:Tstream);virtual;
{$ifdef GDB}
           function cplusplusmangledname : string;
           function stabstring : pchar;virtual;
           procedure concatstabto(asmlist : paasmoutput);virtual;
{$endif GDB}
           procedure deref;virtual;
           function mangledname:string;
           procedure setmangledname(const s:string);
           procedure load_references;
           function  write_references:boolean;
           destructor done;virtual;
        end;

        Pforwarddef=^Tforwarddef;
        Tforwarddef=object(Tdef)
           tosymname:string;
           forwardpos:Tfileposinfo;
           constructor init(Aowner:Pcontainingsymtable;
                            const s:string;const pos:Tfileposinfo);
           function gettypename:string;virtual;
        end;

        {Relevant options for assigning a proc or a procvar to a procvar.}
const   po_compatibility_options=[
          po_classmethod,
          po_staticmethod,
          po_methodpointer,
          po_containsself,
          po_interrupt,
          po_iocheck,
          po_exports
        ];

var     cformaldef:Pformaldef;      {Unique formal definition.}
        voiddef:Porddef;            {Pointer to void (procedure) type.}
        cchardef:Porddef;           {Pointer to char type.}
        booldef:Porddef;            {Pointer to boolean type.}
        u8bitdef:Porddef;           {Pointer to 8-bit unsigned type.}
        u16bitdef:Porddef;          {Pointer to 16-bit unsigned type.}
        u32bitdef:Porddef;          {Pointer to 32-bit unsigned type.}
        s32bitdef:Porddef;          {Pointer to 32-bit signed type.}
        cu64bitdef:Porddef;         {Pointer to 64 bit unsigned def.}
        cs64bitdef:Porddef;         {Pointer to 64 bit signed def.}

        voidpointerdef,             {Pointer for Void-Pointerdef.}
        charpointerdef,             {Pointer for Char-Pointerdef.}
        voidfarpointerdef:ppointerdef;


        s32floatdef : pfloatdef;    {Pointer for realconstn.}
        s64floatdef : pfloatdef;    {Pointer for realconstn.}
        s80floatdef : pfloatdef;    {Pointer to type of temp. floats.}
        s32fixeddef : pfloatdef;    {Pointer to type of temp. fixed.}

        cshortstringdef,            {Pointer to type of short string const.}
        openshortstringdef,         {Pointer to type of an openshortstring,
                                     needed for readln().}
        clongstringdef,             {Pointer to type of long string const.}
        cansistringdef,             {Pointer to type of ansi string const.}
        cwidestringdef:Pstringdef;  {Pointer to type of wide string const.}
        openchararraydef:Parraydef; {Pointer to type of an open array of
                                     char, needed for readln().}

        cfiledef:Pfiledef;          {Get the same definition for all files
                                     used for stabs.}

implementation

uses    systems,symbols,verbose,globals,aasm,files,strings;

const   {If you change one of the following contants,
         you have also to change the typinfo unit
         and the rtl/i386,template/rttip.inc files.}
        tkunknown       = 0;
        tkinteger       = 1;
        tkchar          = 2;
        tkenumeration   = 3;
        tkfloat         = 4;
        tkset           = 5;
        tkmethod        = 6;
        tksstring       = 7;
        tkstring        = tksstring;
        tklstring       = 8;
        tkastring       = 9;
        tkwstring       = 10;
        tkvariant       = 11;
        tkarray         = 12;
        tkrecord        = 13;
        tkinterface     = 14;
        tkclass         = 15;
        tkobject        = 16;
        tkwchar         = 17;
        tkbool          = 18;

        otsbyte         = 0;
        otubyte         = 1;
        otsword         = 2;
        otuword         = 3;
        otslong         = 4;
        otulong         = 5;

        ftsingle        = 0;
        ftdouble        = 1;
        ftextended      = 2;
        ftcomp          = 3;
        ftcurr          = 4;
        ftfixed16       = 5;
        ftfixed32       = 6;

{****************************************************************************
                                Tfiledef
****************************************************************************}

constructor Tfiledef.init(Aowner:Pcontainingsymtable;ft:Tfiletype;tas:Pdef);

begin
    inherited init(Aowner);
    {$IFDEF TP}setparent(typeof(Tdef));{$ENDIF}
    filetype:=ft;
    definition:=tas;
    setsize;
end;

constructor Tfiledef.load(var s:Tstream);

begin
    inherited load(s);
{   filetype:=tfiletype(readbyte);
    if filetype=ft_typed then
        typed_as:=readdefref
    else
        typed_as:=nil;}
    setsize;
end;


procedure Tfiledef.deref;

begin
{   if filetype=ft_typed then
        resolvedef(typed_as);}
end;


procedure Tfiledef.setsize;

begin
    case filetype of
        ft_text:
            savesize:=572;
        ft_typed,ft_untyped:
            savesize:=316;
    end;
end;


procedure Tfiledef.store(var s:Tstream);

begin
{   inherited store(s);
    writebyte(byte(filetype));
    if filetype=ft_typed then
        writedefref(typed_as);
    current_ppu^.writeentry(ibfiledef);}
end;


function Tfiledef.gettypename : string;

begin
    case filetype of
        ft_untyped:
            gettypename:='File';
        ft_typed:
            gettypename:='File Of '+definition^.typename;
        ft_text:
            gettypename:='Text'
    end;
end;

{****************************************************************************
                                Tformaldef
****************************************************************************}

{Tformaldef is used for var parameters without a type.}

constructor Tformaldef.init(Aowner:Pcontainingsymtable);

begin
    inherited init(Aowner);
    {$IFDEF TP}setparent(typeof(Tdef));{$ENDIF}
    savesize:=target_os.size_of_pointer;
end;


constructor Tformaldef.load(var s:Tstream);

begin
    inherited load(s);
    savesize:=target_os.size_of_pointer;
end;


procedure Tformaldef.store(var s:Tstream);

begin
    inherited store(s);
{   current_ppu^.writeentry(ibformaldef);}
end;

function Tformaldef.gettypename:string;

begin
    gettypename:='Var';
end;

{****************************************************************************
                                  Terrordef
****************************************************************************}

{$IFDEF TP}
constructor Terrordef.init(Aowner:Pcontainingsymtable);

begin
    inherited init(Aowner);
    setparent(typeof(Tdef));
end;
{$ENDIF TP}

function Terrordef.gettypename:string;

begin
    gettypename:='<erroneous type>';
end;

{****************************************************************************
                             Tabstractpointerdef
****************************************************************************}

constructor Tabstractpointerdef.init(Aowner:Pcontainingsymtable;def:Pdef);

begin
    inherited init(Aowner);
    {$IFDEF TP}setparent(typeof(Tdef));{$ENDIF}
    include(properties,dp_ret_in_acc);
    definition:=def;
    savesize:=target_os.size_of_pointer;
end;

constructor Tabstractpointerdef.load(var s:Tstream);

begin
    inherited load(s);
(*  {The real address in memory is calculated later (deref).}
    definition:=readdefref;             *)
    savesize:=target_os.size_of_pointer;
end;


procedure Tabstractpointerdef.deref;

begin
{   resolvedef(definition);}
end;


procedure Tabstractpointerdef.store(var s:Tstream);

begin
    inherited store(s);
{   writedefref(definition);
    current_ppu^.writeentry(ibpointerdef);}
end;


{****************************************************************************
                                 Tpointerdef
****************************************************************************}

constructor Tpointerdef.initfar(Aowner:Pcontainingsymtable;def:Pdef);

begin
   inherited init(Aowner,def);
    {$IFDEF TP}setparent(typeof(Tabstractpointerdef));{$ENDIF}
   is_far:=true;
end;

constructor Tpointerdef.load(var s:Tstream);

begin
    inherited load(s);
{   is_far:=(readbyte<>0);}
end;

function Tpointerdef.gettypename : string;

begin
   gettypename:='^'+definition^.typename;
end;

procedure Tpointerdef.store(var s:Tstream);

begin
    inherited store(s);
{   writebyte(byte(is_far));}
end;

{****************************************************************************
                              Tclassrefdef
****************************************************************************}

{$IFDEF TP}
constructor Tclassrefdef.init(Aowner:Pcontainingsymtable;def:Pdef);

begin
    inherited init(Aowner,def);
    setparent(typeof(Tpointerdef));
end;
{$ENDIF TP}

function Tclassrefdef.gettypename:string;

begin
   gettypename:='Class of '+definition^.typename;
end;

{***************************************************************************
                                TVMTENTRY
***************************************************************************}

constructor Tvmtentry.init(Aowner:Pobjectdef);

begin
    inherited init;
    {$IFDEF TP}setparent(typeof(Tobject));{$ENDIF}
    owner:=Aowner;
end;

function Tvmtentry.mangledname:string;

begin
    abstract;
end;

{***************************************************************************
                             TGLOBALVMTENTRY
******************************************************* *******************}

constructor Tglobalvmtentry.init(Aowner:Pobjectdef;proc:Pprocdef);

begin
    inherited init(Aowner);
    {$IFDEF TP}setparent(typeof(Tvmtentry));{$ENDIF TP}
    def:=proc;
end;

function Tglobalvmtentry.mangledname:string;

begin
    mangledname:=def^.mangledname;
end;

{***************************************************************************
                              TLOCALVMTENTRY
***************************************************************************}

constructor Tlocalvmtentry.init(Aowner:Pobjectdef;proc:Pprocdef);

begin
    inherited init(Aowner);
    {$IFDEF TP}setparent(typeof(Tvmtentry));{$ENDIF TP}
    if po_abstractmethod in proc^.options then
        name:=stringdup('FPC_ABSTRACTERROR')
    else
        name:=stringdup(proc^.mangledname);
end;

function Tlocalvmtentry.mangledname:string;

begin
    mangledname:=name^;
end;

{***************************************************************************
                                TOBJECTDEF
***************************************************************************}

constructor Tobjectdef.init(const n:string;Aowner:Pcontainingsymtable;
                            parent:Pobjectdef;isclass:boolean);

begin
    inherited init(Aowner);
    {$IFDEF TP}setparent(typeof(Tdef));{$ENDIF}
    new(publicsyms,init);
    publicsyms^.name:=stringdup(n);
    publicsyms^.defowner:=@self;
    set_parent(parent);
    objname:=stringdup(n);
    if isclass then
        begin
            include(properties,dp_ret_in_acc);
            include(options,oo_is_class);
        end;
end;


procedure tobjectdef.set_parent(parent:Pobjectdef);

const   inherited_options=[oo_has_virtual,
                           oo_has_constructor,oo_has_destructor];

begin
    {Nothing to do if the parent was not forward !}
    if childof=nil then
        begin
            childof:=parent;
            {Some options are inherited...}
            if parent<>nil then
                begin
                    options:=options+parent^.options*inherited_options;
                    {Add the data of the anchestor class.}
                    inc(publicsyms^.datasize,parent^.publicsyms^.datasize);
                    if parent^.privatesyms<>nil then
                        begin
                            if privatesyms=nil then
                                new(privatesyms,init);
                            inc(privatesyms^.datasize,
                             parent^.privatesyms^.datasize);
                        end;
                    if parent^.protectedsyms<>nil then
                        begin
                            if protectedsyms<>nil then
                                new(protectedsyms,init);
                            inc(protectedsyms^.datasize,
                             parent^.protectedsyms^.datasize);
                        end;
                    if oo_has_virtual in (options*parent^.options) then
                        publicsyms^.datasize:=publicsyms^.datasize-
                         target_os.size_of_pointer;
                    {If parent has a vmt field then
                     the offset is the same for the child PM }
                     if [oo_has_virtual,oo_is_class]*parent^.options<>[] then
                        begin
                            vmt_offset:=parent^.vmt_offset;
                            include(options,oo_has_virtual);
                        end;
                end;
            savesize:=publicsyms^.datasize;
        end;
end;

constructor Tobjectdef.load(var s:Tstream);

var oldread_member:boolean;

begin
    inherited load(s);
(*  savesize:=readlong;
    vmt_offset:=readlong;
    objname:=stringdup(readstring);
    childof:=pobjectdef(readdefref);
    options:=readlong;
    oldread_member:=read_member;
    read_member:=true;
    publicsyms:=new(psymtable,loadas(objectsymtable));
    read_member:=oldread_member;
    publicsyms^.defowner:=@self;
    { publicsyms^.datasize:=savesize; }
    publicsyms^.name := stringdup(objname^);

    { handles the predefined class tobject  }
    { the last TOBJECT which is loaded gets }
    { it !                                  }
    if (objname^='TOBJECT') and
      isclass and (childof=nil) then
      class_tobject:=@self;
    has_rtti:=true;*)
end;


procedure Tobjectdef.insertvmt;

var o:Pobjectdef;
    c:Pcollection;
    i:word;

begin
    if vmt_layout<>nil then
        internalerror($990803);
    {Make room for a vmtlink in the object.
     First round up to aktpakrecords.}
    publicsyms^.datasize:=align(publicsyms^.datasize,
     packrecordalignment[aktpackrecords]);
    vmt_offset:=publicsyms^.datasize;
    publicsyms^.datasize:=publicsyms^.datasize+
     target_os.size_of_pointer;
    {Set up the vmt layout collection.
     First search for a vmt in a parent object.}
    o:=childof;
    c:=nil;
    while o<>nil do
        begin
            if o^.vmt_layout<>nil then
                begin
                    c:=vmt_layout;
                    break;
                end;
            o:=o^.childof;
        end;
    if c=nil then
        new(vmt_layout,init(8,8))
    else
        begin
            {We should copy the vmt layout of our parent object. Our vmt
             layout will change as soon as methods are overridden or when
             new virtual methods are added.}
            new(vmt_layout,init(c^.limit,8));
            for i:=0 to c^.count-1 do
                vmt_layout^.insert(c^.at(i));
        end;
end;

procedure Tobjectdef.check_forwards;

begin
    publicsyms^.check_forwards;
    if oo_isforward in options then
        begin
            { ok, in future, the forward can be resolved }
            message1(sym_e_class_forward_not_resolved,objname^);
            exclude(options,oo_isforward);
        end;
end;

{ true, if self inherits from d (or if they are equal) }
function Tobjectdef.is_related(d:Pobjectdef):boolean;

var hp:Pobjectdef;

begin
    hp:=@self;
    is_related:=false;
    while assigned(hp) do
        begin
            if hp=d then
                begin
                    is_related:=true;
                    break;
                end;
            hp:=hp^.childof;
        end;
end;

function Tobjectdef.insert(Asym:Psym):boolean;

var speedvalue:longint;
    s:Psym;
    op:Tobjpropset;

begin
    {First check if the symbol already exists.}
    s:=privatesyms^.speedsearch(Asym^.name,Asym^.speedvalue);
    if s=nil then
        protectedsyms^.speedsearch(Asym^.name,Asym^.speedvalue);
    if s=nil then
        publicsyms^.speedsearch(Asym^.name,Asym^.speedvalue);
    if s<>nil then
        duplicatesym(sym)
    else
        begin
            {Asym is a Tprocsym, Tvarsym or Tpropertysym.}
            if Asym^.is_object(typeof(Tprocsym)) then
                op:=Pprocsym(Asym)^.objprop
            else if Asym^.is_object(typeof(Tvarsym)) then
                op:=Pvarsym(Asym)^.objprop
            else if Asym^.is_object(typeof(Tpropertysym)) then
                op:=Ppropertysym(Asym)^.objprop;
            if sp_private in op then
               insert:=privatesyms^.insert(Asym)
            else if sp_protected in op then
               insert:=protectedsyms^.insert(Asym)
            else if sp_public in op then
               insert:=publicsyms^.insert(Asym);
        end;
end;

function Tobjectdef.search(const s:string;search_protected:boolean):Psym;

begin
    search:=speedsearch(s,getspeedvalue(s),search_protected);
end;

function Tobjectdef.speedsearch(const s:string;speedvalue:longint;
                                search_protected:boolean):Psym;

var r:Psym;

begin
    r:=publicsyms^.speedsearch(s,speedvalue);
    {Privatesyms should be set to nil after compilation of the unit.
     This way, private syms are not found by objects in other units.}
    if (r=nil) and (privatesyms<>nil) then
        r:=privatesyms^.speedsearch(s,speedvalue);
    if (r=nil) and search_protected and (protectedsyms<>nil) then
        r:=protectedsyms^.speedsearch(s,speedvalue);
end;

function Tobjectdef.size:longint;

begin
    if oo_is_class in options then
        size:=target_os.size_of_pointer
    else
        size:=publicsyms^.datasize;
end;


procedure tobjectdef.deref;

var oldrecsyms:Psymtable;

begin
{   resolvedef(pdef(childof));
    oldrecsyms:=aktrecordsymtable;
    aktrecordsymtable:=publicsyms;
    publicsyms^.deref;
    aktrecordsymtable:=oldrecsyms;}
end;


function Tobjectdef.vmt_mangledname:string;

begin
    if not(oo_has_virtual in options) then
        message1(parser_object_has_no_vmt,objname^);
    vmt_mangledname:='VMT_'+owner^.name^+'$_'+objname^;
end;

function Tobjectdef.rtti_name:string;

begin
    rtti_name:='RTTI_'+owner^.name^+'$_'+objname^;
end;

procedure Tobjectdef.store(var s:Tstream);

var oldread_member:boolean;

begin
    inherited store(s);
(*  writelong(size);
    writelong(vmt_offset);
    writestring(objname^);
    writedefref(childof);
    writelong(options);
    current_ppu^.writeentry(ibobjectdef);

    oldread_member:=read_member;
    read_member:=true;
    publicsyms^.writeas;
    read_member:=oldread_member;*)
end;

procedure tobjectdef.write_child_init_data;

begin
end;


procedure Tobjectdef.write_init_data;

var b:byte;

begin
    if oo_is_class in options then
        b:=tkclass
    else
        b:=tkobject;
    rttilist^.concat(new(Pai_const,init_8bit(b)));

    { generate the name }
    rttilist^.concat(new(Pai_const,init_8bit(length(objname^))));
    rttilist^.concat(new(Pai_string,init(objname^)));

(*  rttilist^.concat(new(Pai_const,init_32bit(size)));
    publicsyms^.foreach({$ifndef TP}@{$endif}count_inittable_fields);
    rttilist^.concat(new(Pai_const,init_32bit(count)));
    publicsyms^.foreach({$ifndef TP}@{$endif}write_field_inittable);*)
end;


function Tobjectdef.needs_inittable:boolean;

var oldb:boolean;

begin
    { there are recursive calls to needs_inittable possible, }
    { so we have to change to old value how else should      }
    { we do that ? check_rec_rtti can't be a nested          }
    { procedure of needs_rtti !                              }
(*  oldb:=binittable;
    binittable:=false;
    publicsyms^.foreach({$ifndef TP}@{$endif}check_rec_inittable);
    needs_inittable:=binittable;
    binittable:=oldb;*)
end;

destructor Tobjectdef.done;

var i:longint;
    ve:Pvmtentry;

begin
    {We should be carefull when disposing the vmt_layout; there are
     vmt entries in it which are from methods of our ancestor, we
     should not dispose these. So first set them to nil.}
    for i:=0 to vmt_layout^.count do
        if Pvmtentry(vmt_layout^.at(i))^.owner<>@self then
            vmt_layout^.atput(i,nil);
    dispose(vmt_layout,done);

    if publicsyms<>nil then
        dispose(publicsyms,done);
    if privatesyms<>nil then
        dispose(privatesyms,done);
    if protectedsyms<>nil then
        dispose(protectedsyms,done);
    if oo_isforward in options then
        message1(sym_e_class_forward_not_resolved,objname^);
    stringdispose(objname);
    inherited done;
end;

var count:longint;

procedure count_published_properties(sym:Pnamedindexobject);
                                    {$ifndef fpc}far;{$endif}

begin
    if sym^.is_object(typeof(Tpropertysym)) and
     (ppo_published in Ppropertysym(sym)^.properties) then
        inc(count);
end;


procedure write_property_info(sym:Pnamedindexobject);{$ifndef fpc}far;{$endif}

var proctypesinfo:byte;

    procedure writeproc(proc:Pcollection;shiftvalue:byte);

    var typvalue:byte;

    begin
        if proc=nil then
            begin
                rttilist^.concat(new(pai_const,init_32bit(1)));
                typvalue:=3;
            end
        else if Psym(proc^.at(0))^.is_object(typeof(Tvarsym)) then
            begin
                rttilist^.concat(new(pai_const,init_32bit(
                 Pvarsym(sym)^.address)));
                typvalue:=0;
            end
        else
            begin
    (*          if (pprocdef(def)^.options and povirtualmethod)=0 then
                    begin
                        rttilist^.concat(new(pai_const_symbol,initname(pprocdef(def)^.mangledname)));
                        typvalue:=1;
                    end
                else
                    begin
                        {Virtual method, write vmt offset.}
                        rttilist^.concat(new(pai_const,
                         init_32bit(Pprocdef(def)^.extnumber*4+12)));
                        typvalue:=2;
                    end;*)
            end;
        proctypesinfo:=proctypesinfo or (typvalue shl shiftvalue);
    end;

begin
    if (typeof(sym^)=typeof(Tpropertysym)) and
     (ppo_indexed in Ppropertysym(sym)^.properties) then
        proctypesinfo:=$40
    else
        proctypesinfo:=0;
    if (typeof(sym^)=typeof(Tpropertysym)) and
            (ppo_published in Ppropertysym(sym)^.properties) then
        begin
            rttilist^.concat(new(pai_const_symbol,initname(
             Ppropertysym(sym)^.definition^.get_rtti_label)));
            writeproc(Ppropertysym(sym)^.readaccess,0);
            writeproc(Ppropertysym(sym)^.writeaccess,2);
            { isn't it stored ? }
            if (ppo_stored in Ppropertysym(sym)^.properties) then
                begin
                    rttilist^.concat(new(pai_const,init_32bit(1)));
                    proctypesinfo:=proctypesinfo or (3 shl 4);
                end
            else
                writeproc(ppropertysym(sym)^.storedaccess,4);
            rttilist^.concat(new(pai_const,
             init_32bit(ppropertysym(sym)^.index)));
            rttilist^.concat(new(pai_const,
             init_32bit(ppropertysym(sym)^.default)));
            rttilist^.concat(new(pai_const,
             init_16bit(count)));
            inc(count);
            rttilist^.concat(new(pai_const,init_8bit(proctypesinfo)));
            rttilist^.concat(new(pai_const,
             init_8bit(length(ppropertysym(sym)^.name))));
            rttilist^.concat(new(pai_string,init(ppropertysym(sym)^.name)));
        end;
end;


procedure generate_published_child_rtti(sym:Pnamedindexobject);
                                        {$ifndef fpc}far;{$endif}

begin
    if (typeof(sym^)=typeof(Tpropertysym)) and
     (ppo_published in Ppropertysym(sym)^.properties) then
        Ppropertysym(sym)^.definition^.get_rtti_label;
end;


procedure tobjectdef.write_child_rtti_data;

begin
    publicsyms^.foreach({$ifndef TP}@{$endif}generate_published_child_rtti);
end;


procedure Tobjectdef.generate_rtti;

begin
{   getdatalabel(rtti_label);
    write_child_rtti_data;
    rttilist^.concat(new(pai_symbol,initname_global(rtti_name)));
    rttilist^.concat(new(pai_label,init(rtti_label)));
    write_rtti_data;}
end;


function Tobjectdef.next_free_name_index : longint;

var i:longint;

begin
    if (childof<>nil) and (oo_can_have_published in childof^.options) then
        i:=childof^.next_free_name_index
    else
        i:=0;
    count:=0;
    publicsyms^.foreach({$ifndef TP}@{$endif}count_published_properties);
    next_free_name_index:=i+count;
end;


procedure tobjectdef.write_rtti_data;

begin
    if oo_is_class in options then
        rttilist^.concat(new(pai_const,init_8bit(tkclass)))
    else
        rttilist^.concat(new(pai_const,init_8bit(tkobject)));

    {Generate the name }
    rttilist^.concat(new(pai_const,init_8bit(length(objname^))));
    rttilist^.concat(new(pai_string,init(objname^)));

    {Write class type }
    rttilist^.concat(new(pai_const_symbol,initname(vmt_mangledname)));

    { write owner typeinfo }
    if (childof<>nil) and (oo_can_have_published in childof^.options) then
        rttilist^.concat(new(pai_const_symbol,
         initname(childof^.get_rtti_label)))
    else
        rttilist^.concat(new(pai_const,init_32bit(0)));

    {Count total number of properties }
    if (childof<>nil) and (oo_can_have_published in childof^.options) then
        count:=childof^.next_free_name_index
    else
        count:=0;

    {Write it>}
    publicsyms^.foreach({$ifndef TP}@{$endif}count_published_properties);
    rttilist^.concat(new(Pai_const,init_16bit(count)));

    { write unit name }
    if owner^.name<>nil then
        begin
            rttilist^.concat(new(Pai_const,init_8bit(length(owner^.name^))));
            rttilist^.concat(new(Pai_string,init(owner^.name^)));
        end
    else
        rttilist^.concat(new(Pai_const,init_8bit(0)));

    { write published properties count }
    count:=0;
    publicsyms^.foreach({$ifndef TP}@{$endif}count_published_properties);
    rttilist^.concat(new(pai_const,init_16bit(count)));

    { count is used to write nameindex   }
    { but we need an offset of the owner }
    { to give each property an own slot  }
    if (childof<>nil) and (oo_can_have_published in childof^.options) then
        count:=childof^.next_free_name_index
    else
        count:=0;
    publicsyms^.foreach({$ifndef TP}@{$endif}write_property_info);
end;


function Tobjectdef.is_publishable:boolean;

begin
    is_publishable:=oo_is_class in options;
end;

function Tobjectdef.get_rtti_label:string;

begin
    get_rtti_label:=rtti_name;
end;

{***************************************************************************
                           TARRAYDEF
***************************************************************************}

constructor Tarraydef.init(const l,h:Tconstant;rd:Pdef;
                           Aowner:Pcontainingsymtable);

begin
    inherited init(Aowner);
    {$IFDEF TP}setparent(typeof(Tdef));{$ENDIF}
    lowrange:=l;
    highrange:=h;
    rangedef:=rd;
end;


constructor Tarraydef.load(var s:Tstream);

begin
    inherited load(s);
(*  deftype:=arraydef;
    { the addresses are calculated later }
    definition:=readdefref;
    rangedef:=readdefref;
    lowrange:=readlong;
    highrange:=readlong;
    IsArrayOfConst:=boolean(readbyte);*)
end;


function Tarraydef.getrangecheckstring:string;

begin
    if (cs_create_smart in aktmoduleswitches) then
        getrangecheckstring:='R_'+current_module^.modulename^+tostr(rangenr)
    else
        getrangecheckstring:='R_'+tostr(rangenr);
end;


procedure Tarraydef.genrangecheck;

begin
    if rangenr=0 then
        begin
            {Generates the data for range checking }
            getlabelnr(rangenr);
            if (cs_create_smart in aktmoduleswitches) then
                datasegment^.concat(new(pai_symbol,
                 initname_global(getrangecheckstring,10)))
            else
                datasegment^.concat(new(pai_symbol,
                 initname(getrangecheckstring,10)));
            datasegment^.concat(new(Pai_const,
             init_8bit(byte(lowrange.signed))));
            datasegment^.concat(new(Pai_const,
             init_32bit(lowrange.values)));
            datasegment^.concat(new(Pai_const,
             init_8bit(byte(highrange.signed))));
            datasegment^.concat(new(Pai_const,
             init_32bit(highrange.values)));
        end;
end;


procedure Tarraydef.deref;

begin
{   resolvedef(definition);
    resolvedef(rangedef);}
end;

procedure Tarraydef.store(var s:Tstream);

begin
    inherited store(s);
(*  writedefref(definition);
    writedefref(rangedef);
    writelong(lowrange);
    writelong(highrange);
    writebyte(byte(IsArrayOfConst));
    current_ppu^.writeentry(ibarraydef);*)
end;

function Tarraydef.elesize:longint;

begin
    elesize:=definition^.size;
end;


function Tarraydef.size:longint;

begin
    if (lowrange.signed) and (lowrange.values=-1) then
        internalerror($990804);
    if highrange.signed then
        begin
            {Check for overflow.}
            if (highrange.values-lowrange.values=$7fffffff) or
              (($7fffffff div elesize+elesize-1)>
               (highrange.values-lowrange.values)) then
                begin
{                   message(sym_segment_too_large);}
                    size:=1;
                end
            else
                size:=(highrange.values-lowrange.values+1)*elesize;
        end
    else
        begin
            {Check for overflow.}
            if (highrange.valueu-lowrange.valueu=$7fffffff) or
              (($7fffffff div elesize+elesize-1)>
               (highrange.valueu-lowrange.valueu)) then
                begin
{                   message(sym_segment_too_small);}
                    size:=1;
                end
            else
                size:=(highrange.valueu-lowrange.valueu+1)*elesize;
        end;
end;


function Tarraydef.needs_inittable:boolean;

begin
    needs_inittable:=definition^.needs_inittable;
end;


procedure Tarraydef.write_child_rtti_data;

begin
    definition^.get_rtti_label;
end;


procedure tarraydef.write_rtti_data;

begin
    rttilist^.concat(new(Pai_const,init_8bit(13)));
    write_rtti_name;
    { size of elements }
    rttilist^.concat(new(Pai_const,init_32bit(definition^.size)));
    { count of elements }
    rttilist^.concat(new(Pai_const,
     init_32bit(highrange.values-lowrange.values+1)));
    { element type }
    rttilist^.concat(new(Pai_const_symbol,
     initname(definition^.get_rtti_label)));
end;

function Tarraydef.gettypename:string;

var r:string;

begin
    if [ap_arrayofconst,ap_constructor]*options<>[] then
        gettypename:='array of const'
    else if (lowrange.signed) and (lowrange.values=-1) then
        gettypename:='Array Of '+definition^.typename
    else
        begin
            r:='array[$1..$2 Of $3]';
            if typeof(rangedef^)=typeof(Tenumdef) then
                with Penumdef(rangedef)^.symbols^ do
                    begin
                        replace(r,'$1',Penumsym(at(0))^.name);
                        replace(r,'$2',Penumsym(at(count-1))^.name);
                    end
            else
                begin
                    if lowrange.signed then
                        replace(r,'$1',tostr(lowrange.values))
                    else
                        replace(r,'$1',tostru(lowrange.valueu));
                    if highrange.signed then
                        replace(r,'$2',tostr(highrange.values))
                    else
                        replace(r,'$2',tostr(highrange.valueu));
                    replace(r,'$3',definition^.typename);
                end;
            gettypename:=r;
        end;
end;

{****************************************************************************
                                 Tenumdef
****************************************************************************}

constructor Tenumdef.init(Aowner:Pcontainingsymtable);

begin
    inherited init(Aowner);
    {$IFDEF TP}setparent(typeof(Tdef));{$ENDIF}
    include(properties,dp_ret_in_acc);
    new(symbols,init(8,8));
    calcsavesize;
end;

constructor Tenumdef.init_subrange(Abasedef:Penumdef;Amin,Amax:longint;
                                   Aowner:Pcontainingsymtable);

begin
    inherited init(Aowner);
    minval:=Amin;
    maxval:=Amax;
    basedef:=Abasedef;
    symbols:=Abasedef^.symbols;
    calcsavesize;
end;


constructor Tenumdef.load(var s:Tstream);

begin
    inherited load(s);
(*  basedef:=penumdef(readdefref);
    minval:=readlong;
    maxval:=readlong;
    savesize:=readlong;*)
end;


procedure Tenumdef.calcsavesize;

begin
    if (aktpackenum=4) or (minval<0) or (maxval>65535) then
        savesize:=4
    else if (aktpackenum=2) or (minval<0) or (maxval>255) then
        savesize:=2
    else
        savesize:=1;
end;


procedure Tenumdef.setmax(Amax:longint);

begin
    maxval:=Amax;
    calcsavesize;
end;


procedure Tenumdef.setmin(Amin:longint);

begin
    minval:=Amin;
    calcsavesize;
end;


procedure tenumdef.deref;

begin
{   resolvedef(pdef(basedef));}
end;


procedure Tenumdef.store(var s:Tstream);

begin
    inherited store(s);
(*  writedefref(basedef);
    writelong(min);
    writelong(max);
    writelong(savesize);
    current_ppu^.writeentry(ibenumdef);*)
end;


function tenumdef.getrangecheckstring : string;
begin
   if (cs_create_smart in aktmoduleswitches) then
     getrangecheckstring:='R_'+current_module^.modulename^+tostr(rangenr)
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
          datasegment^.concat(new(Pai_symbol,
                              initname_global(getrangecheckstring,8)))
        else
          datasegment^.concat(new(Pai_symbol,
                              initname(getrangecheckstring,8)));
        datasegment^.concat(new(pai_const,init_32bit(minval)));
        datasegment^.concat(new(pai_const,init_32bit(maxval)));
     end;
end;

procedure Tenumdef.write_child_rtti_data;

begin
   if assigned(basedef) then
        basedef^.get_rtti_label;
end;


procedure Tenumdef.write_rtti_data;

var i:word;

begin
    rttilist^.concat(new(pai_const,init_8bit(tkEnumeration)));
    write_rtti_name;
    case savesize of
        1:
            rttilist^.concat(new(Pai_const,init_8bit(otUByte)));
        2:
            rttilist^.concat(new(Pai_const,init_8bit(otUWord)));
        4:
            rttilist^.concat(new(Pai_const,init_8bit(otULong)));
    end;
    rttilist^.concat(new(pai_const,init_32bit(minval)));
    rttilist^.concat(new(pai_const,init_32bit(maxval)));
    if assigned(basedef) then
        rttilist^.concat(new(pai_const_symbol,initname(basedef^.get_rtti_label)))
    else
        rttilist^.concat(new(pai_const,init_32bit(0)));
    for i:=0 to symbols^.count-1 do
        begin
            rttilist^.concat(new(Pai_const,
             init_8bit(length(Penumsym(symbols^.at(i))^.name))));
            rttilist^.concat(new(Pai_string,
             init(globals.lower(Penumsym(symbols^.at(i))^.name))));
        end;
    rttilist^.concat(new(pai_const,init_8bit(0)));
end;


function Tenumdef.is_publishable:boolean;

begin
    is_publishable:=true;
end;

function Tenumdef.gettypename:string;

var i:word;
    v:longint;
    r:string;

begin
    r:='(';
    for i:=0 to symbols^.count-1 do
        begin
            v:=Penumsym(symbols^.at(i))^.value;
            if (v>=minval) and (v<=maxval) then
                r:=r+Penumsym(symbols^.at(i))^.name+',';
        end;
    {Turn ',' into ')'.}
    r[length(r)]:=')';
end;

{****************************************************************************
                                 Torddef
****************************************************************************}


constructor Torddef.init(t:Tbasetype;l,h:Tconstant;
                         Aowner:Pcontainingsymtable);

begin
    inherited init(Aowner);
    {$IFDEF TP}setparent(typeof(Tdef));{$ENDIF}
    include(properties,dp_ret_in_acc);
    low:=l;
    high:=h;
    typ:=t;
    setsize;
end;

constructor Torddef.load(var s:Tstream);

begin
    inherited load(s);
(*  typ:=tbasetype(readbyte);
    low:=readlong;
    high:=readlong;*)
    setsize;
end;


procedure Torddef.setsize;

begin
   if typ=uauto then
        begin
            {Generate a unsigned range if high<0 and low>=0 }
            if (low.values>=0) and (high.values<=255) then
                typ:=u8bit
            else if (low.signed) and (low.values>=-128) and (high.values<=127) then
                typ:=s8bit
            else if (low.values>=0) and (high.values<=65536) then
                typ:=u16bit
            else if (low.signed) and (low.values>=-32768) and (high.values<=32767) then
                typ:=s16bit
            else if low.signed then
                typ:=s32bit
            else
                typ:=u32bit
        end;
    case typ of
        u8bit,s8bit,uchar,bool8bit:
            savesize:=1;

        u16bit,s16bit,bool16bit:
            savesize:=2;

        s32bit,u32bit,bool32bit:
            savesize:=4;

        u64bit,s64bitint:
            savesize:=8;
        else
            savesize:=0;
    end;
   rangenr:=0;
end;

function Torddef.getrangecheckstring:string;

begin
    if (cs_create_smart in aktmoduleswitches) then
        getrangecheckstring:='R_'+current_module^.modulename^+tostr(rangenr)
    else
        getrangecheckstring:='R_'+tostr(rangenr);
end;

procedure Torddef.genrangecheck;

begin
   if rangenr=0 then
        begin
            {Generate two constant for bounds.}
            getlabelnr(rangenr);
            if (cs_create_smart in aktmoduleswitches) then
              datasegment^.concat(new(Pai_symbol,
               initname_global(getrangecheckstring,10)))
            else
              datasegment^.concat(new(Pai_symbol,
               initname(getrangecheckstring,10)));
            datasegment^.concat(new(Pai_const,init_8bit(byte(low.signed))));
            datasegment^.concat(new(Pai_const,init_32bit(low.values)));
            datasegment^.concat(new(Pai_const,init_8bit(byte(high.signed))));
            datasegment^.concat(new(Pai_const,init_32bit(high.values)));
        end;
end;


procedure Torddef.store(var s:Tstream);

begin
    inherited store(s);
(*  writebyte(byte(typ));
    writelong(low);
    writelong(high);
    current_ppu^.writeentry(iborddef);*)
end;


procedure Torddef.write_rtti_data;

const   trans:array[uchar..bool8bit] of byte=
            (otubyte,otubyte,otuword,otulong,
             otsbyte,otsword,otslong,otubyte);

begin
    case typ of
        bool8bit:
            rttilist^.concat(new(Pai_const,init_8bit(tkbool)));
        uchar:
            rttilist^.concat(new(Pai_const,init_8bit(tkchar)));
        else
            rttilist^.concat(new(Pai_const,init_8bit(tkinteger)));
    end;
    write_rtti_name;
    rttilist^.concat(new(Pai_const,init_8bit(byte(trans[typ]))));
    rttilist^.concat(new(Pai_const,init_32bit(low.values)));
    rttilist^.concat(new(Pai_const,init_32bit(high.values)));
end;

function Torddef.is_publishable:boolean;

begin
    is_publishable:=typ in [uchar..bool8bit];
end;

function Torddef.gettypename:string;

const   names:array[Tbasetype] of string[20]=('<unknown type>',
                'untyped','char','byte','word','dword','shortInt',
                'smallint','longInt','boolean','wordbool',
                'longbool','qword','int64','card64','widechar');

begin
    gettypename:=names[typ];
end;

{****************************************************************************
                                Tfloatdef
****************************************************************************}

constructor Tfloatdef.init(t:Tfloattype;Aowner:Pcontainingsymtable);

begin
    inherited init(Aowner);
    {$IFDEF TP}setparent(typeof(Tdef));{$ENDIF}
    if t=f32bit then
        include(properties,dp_ret_in_acc);
    typ:=t;
    setsize;
end;


constructor Tfloatdef.load(var s:Tstream);

begin
    inherited load(s);
(*  typ:=Tfloattype(readbyte);*)
    setsize;
end;


procedure tfloatdef.setsize;

begin
    case typ of
        f16bit:
            savesize:=2;
        f32bit,
        s32real:
            savesize:=4;
        s64real:
            savesize:=8;
        s80real:
            savesize:=extended_size;
        s64comp:
            savesize:=8;
        else
            savesize:=0;
    end;
end;


procedure Tfloatdef.store(var s:Tstream);

begin
    inherited store(s);
(*  writebyte(byte(typ));
    current_ppu^.writeentry(ibfloatdef);*)
end;

procedure Tfloatdef.write_rtti_data;

const   translate:array[Tfloattype] of byte=
            (ftsingle,ftdouble,ftextended,ftcomp,ftfixed16,ftfixed32);
begin
    rttilist^.concat(new(Pai_const,init_8bit(tkfloat)));
    write_rtti_name;
    rttilist^.concat(new(Pai_const,init_8bit(translate[typ])));
end;


function Tfloatdef.is_publishable:boolean;

begin
    is_publishable:=true;
end;

function Tfloatdef.gettypename:string;

const   names:array[Tfloattype] of string[20]=(
            'single','double','extended','comp','fixed','shortfixed');

begin
   gettypename:=names[typ];
end;

{***************************************************************************
                                   Tsetdef
***************************************************************************}

{ For i386 smallsets work,
  for m68k there are problems
  can be test by compiling with -dusesmallset PM }
{$ifdef i386}
{$define usesmallset}
{$endif i386}

constructor Tsetdef.init(s:Pdef;high:longint;Aowner:Pcontainingsymtable);

begin
    inherited init(Aowner);
    {$IFDEF TP}setparent(typeof(Tdef));{$ENDIF}
    definition:=s;
    if high<32 then
        begin
            settype:=smallset;
            savesize:=4;
            include(properties,dp_ret_in_acc);
        end
    else if high<256 then
            begin
                settype:=normset;
                savesize:=32;
            end
{$ifdef testvarsets}
    else if high<$10000 then
        begin
            settype:=varset;
            savesize:=4*((high+31) div 32);
        end
{$endif testvarsets}
    else
        message(sym_e_ill_type_decl_set);
end;


constructor Tsetdef.load(var s:Tstream);

begin
    inherited load(s);
(*  setof:=readdefref;
    settype:=tsettype(readbyte);
    case settype of
        normset:
            savesize:=32;
        varset:
            savesize:=readlong;
        smallset:
            savesize:=sizeof(longint);
    end;*)
end;


procedure Tsetdef.store(var s:Tstream);

begin
    inherited store(s);
(*  writedefref(setof);
    writebyte(byte(settype));
    if settype=varset then
        writelong(savesize);
    current_ppu^.writeentry(ibsetdef);*)
end;


procedure Tsetdef.deref;

begin
{   resolvedef(setof);}
end;


procedure Tsetdef.write_rtti_data;

begin
    rttilist^.concat(new(pai_const,init_8bit(tkset)));
    write_rtti_name;
    rttilist^.concat(new(pai_const,init_8bit(otuLong)));
    rttilist^.concat(new(pai_const_symbol,initname(definition^.get_rtti_label)));
end;


procedure Tsetdef.write_child_rtti_data;

begin
    definition^.get_rtti_label;
end;


function Tsetdef.is_publishable:boolean;

begin
    is_publishable:=settype=smallset;
end;

function Tsetdef.gettypename:string;

begin
   gettypename:='set of '+definition^.typename;
end;
{***************************************************************************
                                  Trecorddef
***************************************************************************}

constructor Trecorddef.init(s:Precordsymtable;Aowner:Pcontainingsymtable);

begin
    inherited init(Aowner);
    {$IFDEF TP}setparent(typeof(Tdef));{$ENDIF}
    symtable:=s;
    savesize:=symtable^.datasize;
end;


constructor Trecorddef.load(var s:Tstream);

var oldread_member:boolean;
begin
(*  inherited load(s);
    savesize:=readlong;
    oldread_member:=read_member;
    read_member:=true;
    symtable:=new(psymtable,loadas(recordsymtable));
    read_member:=oldread_member;
    symtable^.defowner := @self;*)
end;


destructor Trecorddef.done;

begin
    if symtable<>nil then
        dispose(symtable,done);
    inherited done;
end;

var
 binittable : boolean;

procedure check_rec_inittable(s:Pnamedindexobject);

begin
    if (typeof(s^)=typeof(Tvarsym)) and
     ((typeof((Pvarsym(s)^.definition^))<>typeof(Tobjectdef)) or
      not (oo_is_class in Pobjectdef(Pvarsym(s)^.definition)^.options)) then
        binittable:=pvarsym(s)^.definition^.needs_inittable;
end;


function Trecorddef.needs_inittable:boolean;

var oldb:boolean;

begin
    { there are recursive calls to needs_rtti possible, }
    { so we have to change to old value how else should }
    { we do that ? check_rec_rtti can't be a nested     }
    { procedure of needs_rtti !                         }
    oldb:=binittable;
    binittable:=false;
    symtable^.foreach({$ifndef TP}@{$endif}check_rec_inittable);
    needs_inittable:=binittable;
    binittable:=oldb;
end;


procedure Trecorddef.deref;

var oldrecsyms:Psymtable;

begin
(*   oldrecsyms:=aktrecordsymtable;
   aktrecordsymtable:=symtable;
   { now dereference the definitions }
   symtable^.deref;
   aktrecordsymtable:=oldrecsyms;*)
end;


procedure Trecorddef.store(var s:Tstream);

var oldread_member:boolean;

begin
(*  oldread_member:=read_member;
    read_member:=true;
    inherited store(s);
    writelong(savesize);
    current_ppu^.writeentry(ibrecorddef);
    self.symtable^.writeas;
    read_member:=oldread_member;*)
end;

procedure count_inittable_fields(sym:Pnamedindexobject);
                                {$ifndef fpc}far;{$endif}

begin
   if (typeof(sym^)=typeof(Tvarsym)) and
    (Pvarsym(sym)^.definition^.needs_inittable) then
        inc(count);
end;


procedure count_fields(sym:Pnamedindexobject);{$ifndef fpc}far;{$endif}

begin
   inc(count);
end;


procedure write_field_inittable(sym:Pnamedindexobject);
                                {$ifndef fpc}far;{$endif}

begin
    if (typeof(sym^)=typeof(Tvarsym)) and
     Pvarsym(sym)^.definition^.needs_inittable then
        begin
            rttilist^.concat(new(Pai_const_symbol,
             init(pvarsym(sym)^.definition^.get_inittable_label)));
            rttilist^.concat(new(Pai_const,
             init_32bit(pvarsym(sym)^.address)));
        end;
end;


procedure write_field_rtti(sym:Pnamedindexobject);{$ifndef fpc}far;{$endif}

begin
    rttilist^.concat(new(Pai_const_symbol,
     initname(Pvarsym(sym)^.definition^.get_rtti_label)));
    rttilist^.concat(new(Pai_const,
     init_32bit(Pvarsym(sym)^.address)));
end;


procedure generate_child_inittable(sym:Pnamedindexobject);
                                   {$ifndef fpc}far;{$endif}


begin
    if (typeof(sym^)=typeof(Tvarsym)) and
        Pvarsym(sym)^.definition^.needs_inittable then
    {Force inittable generation }
        Pvarsym(sym)^.definition^.get_inittable_label;
end;


procedure generate_child_rtti(sym:Pnamedindexobject);
                              {$ifndef fpc}far;{$endif}

begin
    Pvarsym(sym)^.definition^.get_rtti_label;
end;

procedure Trecorddef.write_child_rtti_data;

begin
    symtable^.foreach({$ifndef TP}@{$endif}generate_child_rtti);
end;


procedure Trecorddef.write_child_init_data;

begin
    symtable^.foreach({$ifndef TP}@{$endif}generate_child_inittable);
end;


procedure Trecorddef.write_rtti_data;

begin
    rttilist^.concat(new(pai_const,init_8bit(tkrecord)));
    write_rtti_name;
    rttilist^.concat(new(pai_const,init_32bit(size)));
    count:=0;
    symtable^.foreach({$ifndef TP}@{$endif}count_fields);
    rttilist^.concat(new(pai_const,init_32bit(count)));
    symtable^.foreach({$ifndef TP}@{$endif}write_field_rtti);
end;


procedure Trecorddef.write_init_data;

begin
    rttilist^.concat(new(pai_const,init_8bit(14)));
    write_rtti_name;
    rttilist^.concat(new(pai_const,init_32bit(size)));
    count:=0;
    symtable^.foreach({$ifndef TP}@{$endif}count_inittable_fields);
    rttilist^.concat(new(pai_const,init_32bit(count)));
    symtable^.foreach({$ifndef TP}@{$endif}write_field_inittable);
end;

function Trecorddef.gettypename:string;

begin
    gettypename:='<record type>'
end;

{***************************************************************************
                             Tstringprocdef
***************************************************************************}

constructor Tstringdef.shortinit(l:byte;Aowner:Pcontainingsymtable);

begin
    inherited init(Aowner);
    {$IFDEF TP}setparent(typeof(Tdef));{$ENDIF}
    string_typ:=st_shortstring;
    len:=l;
    savesize:=len+1;
end;


constructor Tstringdef.shortload(var s:Tstream);

begin
    inherited load(s);
    string_typ:=st_shortstring;
{   len:=readbyte;
    savesize:=len+1;}
end;


constructor Tstringdef.longinit(l:longint;Aowner:Pcontainingsymtable);

begin
    inherited init(Aowner);
    string_typ:=st_longstring;
    len:=l;
    savesize:=target_os.size_of_pointer;
end;


constructor Tstringdef.longload(var s:Tstream);

begin
    inherited load(s);
    string_typ:=st_longstring;
{   len:=readlong;
    savesize:=target_os.size_of_pointer;}
end;


constructor tstringdef.ansiinit(l:longint;Aowner:Pcontainingsymtable);

begin
    inherited init(Aowner);
    include(properties,dp_ret_in_acc);
    string_typ:=st_ansistring;
    len:=l;
    savesize:=target_os.size_of_pointer;
end;


constructor Tstringdef.ansiload(var s:Tstream);

begin
    inherited load(s);
    string_typ:=st_ansistring;
{   len:=readlong;
    savesize:=target_os.size_of_pointer;}
end;


constructor Tstringdef.wideinit(l:longint;Aowner:Pcontainingsymtable);
begin
    inherited init(Aowner);
    include(properties,dp_ret_in_acc);
    string_typ:=st_widestring;
    len:=l;
    savesize:=target_os.size_of_pointer;
end;


constructor Tstringdef.wideload(var s:Tstream);

begin
    inherited load(s);
    string_typ:=st_widestring;
{   len:=readlong;
    savesize:=target_os.size_of_pointer;}
end;


function Tstringdef.stringtypname:string;

const   typname:array[tstringtype] of string[8]=
            ('','SHORTSTR','LONGSTR','ANSISTR','WIDESTR');

begin
    stringtypname:=typname[string_typ];
end;


function tstringdef.size:longint;

begin
    size:=savesize;
end;


procedure Tstringdef.store(var s:Tstream);

begin
    inherited store(s);
{   if string_typ=st_shortstring then
        writebyte(len)
    else
        writelong(len);
    case string_typ of
        st_shortstring:
            current_ppu^.writeentry(ibshortstringdef);
        st_longstring:
            current_ppu^.writeentry(iblongstringdef);
        st_ansistring:
            current_ppu^.writeentry(ibansistringdef);
        st_widestring:
            current_ppu^.writeentry(ibwidestringdef);
    end;}
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


procedure tstringdef.concatstabto(asmlist : paasmoutput);
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
           rttilist^.concat(new(pai_const,init_8bit(tkAString)));
           write_rtti_name;
        end;
      st_widestring:
        begin
           rttilist^.concat(new(pai_const,init_8bit(tkWString)));
           write_rtti_name;
        end;
      st_longstring:
        begin
           rttilist^.concat(new(pai_const,init_8bit(tkLString)));
           write_rtti_name;
        end;
      st_shortstring:
        begin
           rttilist^.concat(new(pai_const,init_8bit(tkSString)));
           write_rtti_name;
           rttilist^.concat(new(pai_const,init_8bit(len)));
        end;
   end;
end;


function tstringdef.is_publishable : boolean;
begin
   is_publishable:=true;
end;


{***************************************************************************
                            Tabstractprocdef
***************************************************************************}

constructor Tabstractprocdef.init(Aowner:Pcontainingsymtable);

begin
    inherited init(Aowner);
    {$IFDEF TP}setparent(typeof(Tdef));{$ENDIF}
    include(properties,dp_ret_in_acc);
    retdef:=voiddef;
    savesize:=target_os.size_of_pointer;
end;

constructor Tabstractprocdef.load(var s:Tstream);

var count,i:word;

begin
    inherited load(s);
(*  retdef:=readdefref;
    fpu_used:=readbyte;
    options:=readlong;
    count:=readword;
    new(parameters);
    savesize:=target_os.size_of_pointer;
    for i:=1 to count do
        parameters^.readsymref;*)
end;

{ all functions returning in FPU are
  assume to use 2 FPU registers
  until the function implementation
  is processed   PM }
procedure Tabstractprocdef.test_if_fpu_result;

begin
    if (retdef<>nil) and (typeof(retdef^)=typeof(Tfloatdef)) and
     (Pfloatdef(retdef)^.typ in [f32bit,f16bit]) then
        fpu_used:=2;
end;

procedure Tabstractprocdef.deref;

var i:longint;

begin
    inherited deref;
{   resolvedef(retdef);}
    for i:=0 to parameters^.count-1 do
        Psym(parameters^.at(i))^.deref;
end;

function Tabstractprocdef.para_size:longint;

var i,l:longint;

begin
    l:=0;
    for i:=0 to parameters^.count-1 do
        inc(l,Pparamsym(parameters^.at(i))^.getpushsize);
    para_size:=l;
end;

procedure Tabstractprocdef.store(var s:Tstream);

var count,i:word;

begin
    inherited store(s);
{   writedefref(retdef);
    current_ppu^.do_interface_crc:=false;
    writebyte(fpu_used);
    writelong(options);
    writeword(parameters^.count);
    for i:=0 to parameters^.count-1 do
        begin
            writebyte(byte(hp^.paratyp));
            writesymfref(hp^.data);
        end;}
end;


function Tabstractprocdef.demangled_paras:string;

var i:longint;
    s:string;

procedure doconcat(p:Pparameter);

begin
    s:=s+p^.data^.name;
    if p^.paratyp=vs_var then
        s:=s+'var'
    else if p^.paratyp=vs_const then
        s:=s+'const';
end;

begin
    s:='(';
    for i:=0 to parameters^.count-1 do
        doconcat(parameters^.at(i));
    s[length(s)]:=')';
    demangled_paras:=s;
end;

destructor Tabstractprocdef.done;

begin
    dispose(parameters,done);
    inherited done;
end;

{***************************************************************************
                                  TPROCDEF
***************************************************************************}

constructor Tprocdef.init(Aowner:Pcontainingsymtable);

begin
    inherited init(Aowner);
    {$IFDEF TP}setparent(typeof(Tabstractprocdef));{$ENDIF}
    fileinfo:=aktfilepos;
    vmt_index:=-1;
    new(localst,init);
    if (cs_browser in aktmoduleswitches) and make_ref then
        begin
            new(references,init(2*owner^.index_growsize,
                                owner^.index_growsize));
            references^.insert(new(Pref,init(tokenpos)));
        end;
    {First, we assume that all registers are used }
    usedregisters:=[low(Tregister)..high(Tregister)];
    forwarddef:=true;
end;


constructor Tprocdef.load(var s:Tstream);

var a:string;

begin
    inherited load(s);
(*  usedregisters:=readlong;

    a:=readstring;
    setstring(_mangledname,s);

    extnumber:=readlong;
    nextoerloaded:=pprocdef(readdefref);
    _class := pobjectdef(readdefref);
    readposinfo(fileinfo);

    if (cs_link_deffile in aktglobalswitches)
     and (poexports in options) then
        deffile.ddexport(mangledname);

    count:=true;*)
end;


const local_symtable_index : longint = $8001;

procedure tprocdef.load_references;

var pos:Tfileposinfo;
    pdo:Pobjectdef;
    move_last:boolean;

begin
(*  move_last:=lastwritten=lastref;
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
    if ((current_module^.flags and uf_local_browser)<>0)
     and is_in_current then
        begin
{$ifndef NOLOCALBROWSER}
            pdo:=_class;
            new(parast,loadas(parasymtable));
            parast^.next:=owner;
            parast^.load_browser;
            new(localst,loadas(localsymtable));
            localst^.next:=parast;
            localst^.load_browser;
{$endif NOLOCALBROWSER}
        end;*)
end;


function Tprocdef.write_references:boolean;

var ref:Pref;
    pdo:Pobjectdef;
    move_last:boolean;

begin
(*  move_last:=lastwritten=lastref;
    if move_last and (((current_module^.flags and uf_local_browser)=0)
     or not is_in_current) then
        exit;
    {Write address of this symbol }
    writedefref(@self);
    {Write refs }
    if assigned(lastwritten) then
        ref:=lastwritten
    else
        ref:=defref;
    while assigned(ref) do
        begin
            if ref^.moduleindex=current_module^.unit_index then
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
    if ((current_module^.flags and uf_local_browser)<>0)
     and is_in_current then
        begin
            pdo:=_class;
            if (owner^.symtabletype<>localsymtable) then
            while assigned(pdo) do
                begin
                    if pdo^.publicsyms<>aktrecordsymtable then
                        begin
                            pdo^.publicsyms^.unitid:=local_symtable_index;
                            inc(local_symtable_index);
                        end;
                    pdo:=pdo^.childof;
                end;

            {We need TESTLOCALBROWSER para and local symtables
             PPU files are then easier to read PM.}
            inc(local_symtable_index);
            parast^.write_browser;
            if not assigned(localst) then
                localst:=new(psymtable,init);
            localst^.writeas;
            localst^.unitid:=local_symtable_index;
            inc(local_symtable_index);
            localst^.write_browser;
            {Decrement for.}
            local_symtable_index:=local_symtable_index-2;
            pdo:=_class;
            if (owner^.symtabletype<>localsymtable) then
                while assigned(pdo) do
                    begin
                        if pdo^.publicsyms<>aktrecordsymtable then
                            dec(local_symtable_index);
                        pdo:=pdo^.childof;
                    end;
        end;*)
end;

destructor Tprocdef.done;

begin
    if po_msgstr in options then
        strdispose(messageinf.str);
    if references<>nil then
        dispose(references,done);
    if (localst<>nil) and (typeof(localst^)<>typeof(Timplsymtable)) then
        dispose(localst,done);
{   if (poinline in options) and (code,nil) then
        disposetree(ptree(code));}
    if _mangledname<>nil then
        disposestr(_mangledname);
    inherited done;
end;


procedure Tprocdef.store(var s:Tstream);

begin
(*  inherited store(s);
    current_ppu^.do_interface_crc:=false;
    writelong(usedregisters);
    writestring(mangledname);
    current_ppu^.do_interface_crc:=true;
    writelong(extnumber);
    if (options and pooperator) = 0 then
        writedefref(nextoverloaded)
    else
        begin
            {Only write the overloads from the same unit }
            if assigned(nextoverloaded) and
             (nextoverloaded^.owner=owner) then
                writedefref(nextoverloaded)
            else
                writedefref(nil);
        end;
    writedefref(_class);
    writeposinfo(fileinfo);
    if (poinline and options) then
        begin
            {We need to save
                - the para and the local symtable
                - the code ptree !! PM
               writesymtable(parast);
               writesymtable(localst);
               writeptree(ptree(code));
               }
        end;
    current_ppu^.writeentry(ibprocdef);*)
end;

procedure Tprocdef.deref;

begin
{   inherited deref;
    resolvedef(pdef(nextoverloaded));
    resolvedef(pdef(_class));}
end;


function Tprocdef.mangledname:string;

var i:word;
    a:byte;
    s:Pprocsym;
    r:string;

begin
    if _mangledname<>nil then
        mangledname:=_mangledname^
    else
        begin
            {If the procedure is in a unit, we start with the unitname.}
            if current_module^.is_unit then
                r:='_'+current_module^.modulename^
            else
                r:='';
            a:=length(r);
            {If we are a method we add the name of the object we are
             belonging to.}
            if (Pprocsym(sym)^._class<>nil) then
                r:=r+'_M'+Pprocsym(sym)^._class^.sym^.name+'_M';
            {Then we add the names of the procedures we are defined in
             (for the case we are a nested procedure).}
            s:=Pprocsym(sym)^.sub_of;
            while typeof(s^.owner^)=typeof(Tprocsymtable) do
                begin
                    insert('_$'+s^.name,r,a);
                    s:=s^.sub_of;
                end;
            r:=r+'_'+sym^.name;
            {Add the types of all parameters.}
            for i:=0 to parameters^.count-1 do
                begin
                    r:=r+'$'+Pparameter(parameters^.at(i))^.data^.name;
                end;
        end;
end;

procedure Tprocdef.setmangledname(const s:string);

begin
    if _mangledname<>nil then
        disposestr(_mangledname);
    _mangledname:=stringdup(s);
    if localst<>nil then
        begin
            stringdispose(localst^.name);
            localst^.name:=stringdup('locals of '+s);
        end;
end;

{***************************************************************************
                                 Tprocvardef
***************************************************************************}

{$IFDEF TP}
constructor Tprocvardef.init(Aowner:Pcontainingsymtable);

begin
    setparent(typeof(Tabstractprocdef));
end;
{$ENDIF TP}


function Tprocvardef.size:longint;

begin
    if po_methodpointer in options then
        size:=2*target_os.size_of_pointer
    else
        size:=target_os.size_of_pointer;
end;


{$ifdef GDB}
function tprocvardef.stabstring : pchar;
var
   nss : pchar;
   i : word;
   param : pdefcoll;
begin
  i := 0;
  param := para1;
  while assigned(param) do
    begin
    inc(i);
    param := param^.next;
    end;
  getmem(nss,1024);
  { it is not a function but a function pointer !! (PM) }

  strpcopy(nss,'*f'+retdef^.numberstring{+','+tostr(i)}+';');
  param := para1;
  i := 0;
  { this confuses gdb !!
    we should use 'F' instead of 'f' but
    as we use c++ language mode
    it does not like that either
    Please do not remove this part
    might be used once
    gdb for pascal is ready PM }
  (* while assigned(param) do
    begin
    inc(i);
    if param^.paratyp = vs_value then vartyp := '1' else vartyp := '0';
    {Here we have lost the parameter names !!}
    pst := strpnew('p'+tostr(i)+':'+param^.data^.numberstring+','+vartyp+';');
    strcat(nss,pst);
    strdispose(pst);
    param := param^.next;
    end; *)
  {strpcopy(strend(nss),';');}
  stabstring := strnew(nss);
  freemem(nss,1024);
end;


procedure tprocvardef.concatstabto(asmlist : paasmoutput);
begin
   if ( not assigned(sym) or sym^.isusedinstab or (cs_gdb_dbx in aktglobalswitches))
     and not is_def_stab_written then
     inherited concatstabto(asmlist);
   is_def_stab_written:=true;
end;
{$endif GDB}


procedure Tprocvardef.write_rtti_data;
begin
   {!!!!!!!}
end;


procedure Tprocvardef.write_child_rtti_data;
begin
   {!!!!!!!!}
end;


function Tprocvardef.is_publishable:boolean;

begin
    is_publishable:=po_methodpointer in options;
end;

function Tprocvardef.gettypename:string;

begin
   gettypename:='<procedure variable type>'
end;

{****************************************************************************
                                Tforwarddef
****************************************************************************}

constructor tforwarddef.init(Aowner:Pcontainingsymtable;
                             const s:string;const pos:Tfileposinfo);

var oldregisterdef:boolean;

begin
    { never register the forwarddefs, they are disposed at the
      end of the type declaration block }
{   oldregisterdef:=registerdef;
    registerdef:=false;}
    inherited init(Aowner);
    {$IFDEF TP}setparent(typeof(Tdef));{$ENDIF}
{   registerdef:=oldregisterdef;}
    tosymname:=s;
    forwardpos:=pos;
end;


function tforwarddef.gettypename:string;

begin
    gettypename:='unresolved forward to '+tosymname;
end;

end.

{
  $Log$
  Revision 1.2  2002-05-16 19:46:52  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.1  2000/07/13 06:30:13  michael
  + Initial import

  Revision 1.6  2000/03/16 12:52:47  daniel
    *  Changed names of procedures flags
    *  Changed VMT generation

  Revision 1.5  2000/03/11 21:11:24  daniel
    * Ported hcgdata to new symtable.
    * Alignment code changed as suggested by Peter
    + Usage of my is operator replacement, is_object

  Revision 1.4  2000/03/01 11:43:55  daniel
  * Some more work on the new symtable.
  + Symtable stack unit 'symstack' added.

}
