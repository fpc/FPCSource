{
    $Id$

    Copyright (C) 1998-2000 by Daniel Mantione
     and other members of the Free Pascal development team

    This unit handles symbols

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

unit symbols;

interface

uses    symtable,aasm,objects,cobjects,defs,cpubase,tokens;

{Note: It is forbidden to add the symtablt unit. A symbol should not now in
 which symtable it is.}

{The tokens unit is only needed for the overloaded operators array. This
 array can better be moved into another unit.}

type    Ttypeprop=(sp_primary_typesym);
        Ttypepropset=set of Ttypeprop;

        Tobjprop=(sp_public,sp_private,sp_protected,sp_published,sp_static);
        Tobjpropset=set of Tobjprop;

        Tpropprop=(ppo_indexed,ppo_defaultproperty,
                   ppo_stored,ppo_published,ppo_hasparameters);
        Tproppropset=set of Tpropprop;

        Tvarprop=(vo_regable,vo_fpuregable,vo_is_C_var,vo_is_external,
                  vo_is_dll_var,vo_is_thread_var);
        Tvarpropset=set of Tvarprop;

        {State of a variable, if it's declared, assigned or used.}
        Tvarstate=(vs_none,vs_declared,vs_declared_and_first_found,
                   vs_set_but_first_not_passed,vs_assigned,vs_used);

        Plabelsym=^Tlabelsym;
        Tlabelsym=object(Tsym)
            lab:Pasmlabel;
            defined:boolean;
            constructor init(const n:string;l:Pasmlabel);
            constructor load(var s:Tstream);
            function mangledname:string;virtual;
            procedure store(var s:Tstream);virtual;
        end;

{       Punitsym=^Tunitsym;
        Tunitsym=object(Tsym)
            unitsymtable : punitsymtable;
            prevsym : punitsym;
            refs : longint;
            constructor init(const n : string;ref : punitsymtable);
            constructor load(var s:Tstream);
            destructor done;virtual;
            procedure store(var s:Tstream);virtual;
        end;}

        Perrorsym=^Terrorsym;
        Terrorsym=object(tsym)
            constructor init;
        end;

        Pprocsym=^Tprocsym;
        Tprocsym=object(Tsym)
            definitions:Pobject;    {Is Pprocdef when procedure not
                                     overloaded, or a Pcollection of
                                     Pprocdef when it is overloaded.
                                     Since most procedures are not
                                     overloaded, this saves a lot of
                                     memory.}
            objprop:Tobjpropset;    {All overloaded procedures should
                                     have the same scope, so the object
                                     scope information is put in the
                                     symbol.}
            sub_of:Pprocsym;
            _class:Pobjectdef;
            constructor init(const n:string;Asub_of:Pprocsym);
            constructor load(var s:Tstream);
            function count:word;
            function firstthat(action:pointer):Pprocdef;
            procedure foreach(action:pointer);
            procedure insert(def:Pprocdef);
            function mangledname:string;virtual; {Causes internalerror.}
            {Writes all declarations.}
            procedure write_parameter_lists;
            {Tests, if all procedures definitions are defined and not
             just available as forward,}
            procedure check_forward;
            procedure store(var s:Tstream);virtual;
            procedure deref;virtual;
            procedure load_references;virtual;
            function  write_references:boolean;virtual;
            destructor done;virtual;
        end;

        Ptypesym=^Ttypesym;
        Ttypesym=object(Tsym)
            definition:Pdef;
            forwardpointers:Pcollection;    {Contains the forwardpointers.}
            properties:Ttypepropset;
            synonym:Ptypesym;
            constructor init(const n:string;d:Pdef);
            constructor load(var s:Tstream);
{           procedure addforwardpointer(p:Ppointerdef);}
            procedure deref;virtual;
            procedure store(var s:Tstream);virtual;
            procedure load_references;virtual;
            procedure updateforwarddef(p:pdef);
            function  write_references:boolean;virtual;
            destructor done;virtual;
        end;

        Psyssym=^Tsyssym;
        Tsyssym=object(Tsym)
            number:longint;
            constructor init(const n:string;l:longint);
            constructor load(var s:Tstream);
            procedure store(var s:Tstream);virtual;
        end;

        Pmacrosym=^Tmacrosym;
        Tmacrosym=object(Tsym)
            defined,is_used:boolean;
            buftext:Pchar;
            buflen:longint;
            {Macros aren't written to PPU files !}
            constructor init(const n:string);
            destructor done;virtual;
        end;

        Penumsym=^Tenumsym;
        Tenumsym=object(tsym)
            value:longint;
            definition:Penumdef;
            nextenum:Penumsym;
            constructor init(const n:string;def:Penumdef;v:longint);
            constructor load(var s:Tstream);
            procedure store(var s:Tstream);virtual;
            procedure deref;virtual;
            procedure order;
        end;

        Pprogramsym=^Tprogramsym;
        Tprogramsym=object(Tsym)
        end;

        Pvarsym=^Tvarsym;
        Tvarsym=object(tsym)
            address:longint;
            localvarsym:Pvarsym;
            islocalcopy:boolean;
            definition:Pdef;
            refs:longint;
            properties:Tvarpropset;
            state:Tvarstate;
            objprop:Tobjpropset;
            _mangledname:Pstring;
            reg:Tregister;  {If reg<>R_NO, then the variable is an register
                             variable }
            constructor init(const n:string;p:Pdef);
            constructor init_dll(const n:string;p:Pdef);
            constructor init_C(const n,mangled:string;p:Pdef);
            constructor load(var s:Tstream);
            procedure concatdata(const n:string;len:longint);
            procedure deref;virtual;
            function getsize:longint;virtual;
            function mangledname:string;virtual;
            procedure insert_in_data;virtual;
            procedure setmangledname(const s:string);
            procedure store(var s:Tstream);virtual;
            destructor done;virtual;
        end;

        Pparamsym=^Tparamsym;
        Tparamsym=object(Tvarsym)
            varspez:Tvarspez;
            pushaddress:longint;
            constructor init(const n:string;p:Pdef;vs:Tvarspez);
            function getsize:longint;virtual;
            function getpushsize:longint;virtual;
            procedure insert_in_data;virtual;
        end;

        Ptypedconstsym=^Ttypedconstsym;
        Ttypedconstsym=object(Tsym)
            prefix:Pstring;
            definition:Pdef;
            is_really_const:boolean;
            constructor init(const n:string;p:Pdef;really_const:boolean);
            constructor load(var s:Tstream);
            destructor done;virtual;
            function mangledname:string;virtual;
            procedure store(var s:Tstream);virtual;
            procedure deref;virtual;
            function getsize:longint;
            procedure insert_in_data;virtual;
        end;

        Tconsttype=(constord,conststring,constreal,constbool,
                    constint,constchar,constset,constnil);

        Pconstsym=^Tconstsym;
        Tconstsym=object(Tsym)
           definition:Pdef;
           consttype:Tconsttype;
           value,len:longint;   {Len is needed for string length.}
           constructor init(const n:string;t:Tconsttype;v:longint);
           constructor init_def(const n:string;t:Tconsttype;v:longint;
                                def:Pdef);
           constructor init_string(const n:string;t:Tconsttype;
                                   str:Pchar;l:longint);
           constructor load(var s:Tstream);
           procedure deref;virtual;
           procedure store(var s:Tstream);virtual;
           destructor done;virtual;
        end;

        absolutetyp = (tovar,toasm,toaddr);

        Pabsolutesym = ^tabsolutesym;
        Tabsolutesym = object(tvarsym)
            abstyp:absolutetyp;
            absseg:boolean;
            ref:Psym;
            asmname:Pstring;
            constructor load(var s:Tstream);
            procedure deref;virtual;
            function mangledname : string;virtual;
            procedure store(var s:Tstream);virtual;
        end;

        Pfuncretsym=^Tfuncretsym;
        Tfuncretsym=object(tsym)
            funcretprocinfo:pointer{Pprocinfo};
            definition:Pdef;
            address:longint;
            constructor init(const n:string;approcinfo:pointer{pprocinfo});
            constructor load(var s:Tstream);
            procedure insert_in_data;virtual;
            procedure store(var s:Tstream);virtual;
            procedure deref;virtual;
        end;

        Ppropertysym=^Tpropertysym;
        Tpropertysym=object(Tsym)
            properties:Tproppropset;
            definition:Pdef;
            objprop:Tobjpropset;
            rangedef:Pdef;  {Type of the range for array properties.}
            {For record property's like property x read a.b.c, the
             collection contains a as first element, b as second element,
             and c as the third element.}
            readaccess,
            writeaccess,
            storedaccess:Pcollection;
            index,default:longint;
            constructor load(var s:Tstream);
            function getsize:longint;virtual;
            procedure store(var s:Tstream);virtual;
            procedure deref;virtual;
        end;

const   {Last and first operators which can be overloaded.}
        first_overloaded = _PLUS;
        last_overloaded  = _ASSIGNMENT;
        overloaded_names : array [first_overloaded..
                                  last_overloaded] of string[16] =
             ('plus','minus','star','slash',
              'equal','greater','lower','greater_or_equal',
              'lower_or_equal','sym_diff','starstar','as',
              'is','in','or','and',
              'div','mod','shl','shr',
              'xor','assign');

var current_object_option:Tobjprop;
    current_type_option:Ttypepropset;

    aktprocsym:Pprocsym;    {Pointer to the symbol for the
                             currently parsed procedure.}
    aktprocdef:Pprocdef;    {Pointer to the defnition for the
                             currently parsed procedure.}
    aktvarsym:Pvarsym;      {Pointer to the symbol for the
                             currently read var, only used
                             for variable directives.}

    overloaded_operators:array[first_overloaded..
                               last_overloaded] of Pprocsym;
       { unequal is not equal}

implementation

uses    callspec,verbose,globals,systems,globtype,types;

{****************************************************************************
                                 Tlabelsym
****************************************************************************}

constructor Tlabelsym.init(const n:string;l:Pasmlabel);

begin
    inherited init(n);
    {$IFDEF TP}setparent(typeof(Tsym));{$ENDIF}
    lab:=l;
    defined:=false;
end;

constructor Tlabelsym.load(var s:Tstream);

begin
    inherited load(s);
    defined:=true;
end;

function Tlabelsym.mangledname:string;

begin
    mangledname:=lab^.name;
end;

procedure Tlabelsym.store(var s:Tstream);

begin
    inherited store(s);
{   current_ppu^.writeentry(iblabelsym);}
end;

{****************************************************************************
                                  Terrorsym
****************************************************************************}

constructor terrorsym.init;

begin
    inherited init('');
    {$IFDEF TP}setparent(typeof(Tsym));{$ENDIF}
end;
{****************************************************************************
                                  Tprocsym
****************************************************************************}

constructor Tprocsym.init(const n:string;Asub_of:Pprocsym);

begin
    inherited init(n);
    {$IFDEF TP}setparent(typeof(Tsym));{$ENDIF}
    sub_of:=Asub_of;
end;

constructor Tprocsym.load(var s:Tstream);

begin
    inherited load(s);
{   definition:=Pprocdef(readdefref);}
end;

function Tprocsym.count:word;

begin
    if typeof(definitions^)=typeof(Tcollection) then
        count:=Pcollection(definitions)^.count
    else
        count:=1;
end;

function Tprocsym.firstthat(action:pointer):Pprocdef;

begin
    firstthat:=nil;
    if definitions<>nil then
        if typeof(definitions^)=typeof(Tcollection) then
            firstthat:=Pcollection(definitions)^.firstthat(action)
        else if boolean(byte(longint(callpointerlocal(action,
         previousframepointer,definitions)))) then
            firstthat:=Pprocdef(definitions);
end;

procedure Tprocsym.foreach(action:pointer);

begin
    if definitions<>nil then
        begin
            if typeof(definitions^)=typeof(Tcollection) then
                Pcollection(definitions)^.foreach(action)
            else
                callpointerlocal(action,previousframepointer,definitions);
        end;
end;

procedure Tprocsym.insert(def:Pprocdef);

    function matchparas(item:pointer):boolean;{$IFDEF TP}far;{$ENDIF}

    begin
        matchparas:=equal_paras(Pprocdef(item)^.parameters,
         Pprocdef(def)^.parameters,false);
    end;

var c:Pcollection;
    ovs:Pprocsym;
    ovd:Pprocdef;
    ve:Pvmtentry;
    errparam:string;

begin
    if _class<>nil then
        begin
            {Update object information.}
            if po_virtualmethod in def^.options then
                include(_class^.options,oo_has_virtual);
            if po_abstractmethod in def^.options then
                include(_class^.options,oo_has_abstract);
            if def^.proctype=po_type_constructor then
                include(_class^.options,oo_has_constructor);
            if def^.proctype=po_type_destructor then
                include(_class^.options,oo_has_destructor);
            {Check if we are overriding an existing method.}
            ovs:=Pprocsym(_class^.childof^.search(name,true));
            ovd:=ovs^.firstthat(@matchparas);
            if ovd<>nil then
                begin
                    errparam:=_class^.objname^+'.'+name;
                    {If the old method is virtual and we are not, we
                     refuse this for objects, and warn for classes.}
                    if (po_virtualmethod in ovd^.options) then
                        if  (po_virtualmethod in Pprocdef(def)^.options) then
                            if oo_is_class in _class^.options then
                                message1(parser_w_should_use_override,errparam)
                            else
                                message1(parser_w_overloaded_are_not_both_virtual,errparam)
                        else
                            {Both are virtual.
                             The flags have to match except abstract,
                             assembler and override.}
                            if (def^.calloptions<>ovd^.calloptions) or
                             (def^.proctype<>ovd^.proctype) or
                             ((def^.options-[po_abstractmethod,po_overridingmethod,po_assembler])<>
                             (ovd^.options-[po_abstractmethod,po_overridingmethod,po_assembler])) then
                                message1(parser_e_header_dont_match_forward,errparam);
                    {Error if the return types aren't equal.}
                    if not(is_equal(def^.retdef,ovd^.retdef)) and
                     not(def^.retdef^.is_object(typeof(Tobjectdef)) and
                      Pprocdef(ovd)^.retdef^.is_object(typeof(Tobjectdef)) and
                      (oo_is_class in Pobjectdef(def^.retdef)^.options) and
                      (oo_is_class in Pobjectdef(ovd^.retdef)^.options) and
                      (pobjectdef(def^.retdef)^.is_related(pobjectdef(ovd^.retdef)))) then
                        message1(parser_e_overloaded_methodes_not_same_ret,errparam);
                    if po_virtualmethod in def^.options then
                        begin
                            if not(oo_has_constructor in _class^.options) then
                                message1(parser_w_virtual_without_constructor,_class^.objname^);
                            {We change the the vmt layout so we are called instead
                             of our ancestor.}
                            if sp_private in objprop then
                                ve:=new(Plocalvmtentry,init(_class,def))
                            else
                                ve:=new(Pglobalvmtentry,init(_class,def));
                            _class^.vmt_layout^.atput(ovd^.vmt_index,ve);
                            def^.vmt_index:=ovd^.vmt_index;
                        end;
                end
            else
                begin
                    if not(oo_has_constructor in _class^.options) then
                        message1(parser_w_virtual_without_constructor,_class^.objname^);
                    {The method is not overridden; if it is virtual we should
                     generate a vmt entry.}
                    if po_virtualmethod in def^.options then
                        begin
                            if sp_private in objprop then
                                ve:=new(Plocalvmtentry,init(_class,def))
                            else
                                ve:=new(Pglobalvmtentry,init(_class,def));
                            _class^.vmt_layout^.insert(ve);
                            def^.vmt_index:=_class^.vmt_layout^.count-1;
                        end;
                end;
        end;
    if definitions=nil then
        definitions:=def
    else
        if typeof(definitions^)=typeof(Tcollection) then
            Pcollection(def)^.insert(def)
        else
            begin
                c:=new(Pcollection,init(8,4));
                c^.insert(definitions);
                definitions:=c;
            end;
end;

function Tprocsym.mangledname:string;

{This function calls internalerror, because procsyms can be overloaded.
 Procedures should use the foreach to check for the right overloaded procsym
 and then call mangledname on that procsym.}

begin
    internalerror($99080201);
end;

procedure Tprocsym.write_parameter_lists;

{var    p:Pprocdef;}

begin
(*  p:=definition;
    while assigned(p) do
        begin
            {Force the error to be printed.}
            verbose.message1(sym_b_param_list,name+p^.demangled_paras);
            p:=p^.nextoverloaded;
        end;*)
end;

procedure tprocsym.check_forward;

{var    pd:Pprocdef;}

begin
(*  pd:=definition;
    while assigned(pd) do
        begin
            if pd^.forwarddef then
                begin
                    if assigned(pd^._class) then
                        messagepos1(fileinfo,sym_e_forward_not_resolved,
                         pd^._class^.objname^+'.'+name+
                         demangledparas(pd^.demangled_paras))
                    else
                        messagepos1(fileinfo,sym_e_forward_not_resolved,
                         name+pd^.demangled_paras);
                    {Turn futher error messages off.}
                    pd^.forwarddef:=false;
                end;

                pd:=pd^.nextoverloaded;
        end;*)
end;


procedure tprocsym.deref;

{var    t:ttoken;
    last:Pprocdef;}

begin
(*
    resolvedef(pdef(definition));
    if (definition^.options and pooperator) <> 0 then
        begin
            last:=definition;
            while assigned(last^.nextoverloaded) do
                last:=last^.nextoverloaded;
            for t:=first_overloaded to last_overloaded do
                if (name=overloaded_names[t]) then
                    begin
                        if assigned(overloaded_operators[t]) then
                            last^.nextoverloaded:=overloaded_operators[t]^.definition;
                        overloaded_operators[t]:=@self;
                    end;
        end;*)
end;

procedure Tprocsym.store(var s:Tstream);

begin
    inherited store(s);
{   writedefref(pdef(definition));
    current_ppu^.writeentry(ibprocsym);}
end;


procedure tprocsym.load_references;

begin
    inherited load_references;
end;

function Tprocsym.write_references:boolean;

{var    prdef:Pprocdef;}

begin
(*  write_references:=false;
    if not inherited write_references then
        exit;
    write_references:=true;
    prdef:=definition;
    while assigned(prdef) and (prdef^.owner=definition^.owner) do
        begin
            prdef^.write_references;
            prdef:=prdef^.nextoverloaded;
        end;*)
end;

destructor Tprocsym.done;

begin
    {Don't check if errors !!}
    if errorcount=0 then
        check_forward;
    inherited done;
end;

{****************************************************************************
                                  Ttypesym
****************************************************************************}

constructor Ttypesym.init(const n:string;d:Pdef);

begin
    inherited init(n);
    {$IFDEF TP}setparent(typeof(Tsym));{$ENDIF}
    definition:=d;
    if assigned(definition) then
        begin
            if definition^.sym<>nil then
                begin
                    definition^.sym:=@self;
                    properties:=[sp_primary_typesym];
                end
            else
                begin
                    synonym:=Ptypesym(definition^.sym)^.synonym;
                    Ptypesym(definition^.sym)^.synonym:=@self;
                end;
        end;
end;

constructor Ttypesym.load(var s:Tstream);

begin
    inherited load(s);
{   definition:=readdefref;}
end;

{procedure Ttypesym.addforwardpointer(p:Ppointerdef);

begin
    if forwardpointers=nil then
        new(forwardpointers,init(8,4));
    forwardpointers^.insert(p);
end;}

procedure ttypesym.deref;

begin
(*  resolvedef(definition);
    if assigned(definition) then
        begin
            if properties=sp_primary_typesym then
                begin
                    if definition^.sym<>@self then
                        synonym:=definition^.sym;
                    definition^.sym:=@self;
                end
            else
                begin
                    if assigned(definition^.sym) then
                        begin
                            synonym:=definition^.sym^.synonym;
                            if definition^.sym<>@self then
                                definition^.sym^.synonym:=@self;
                        end
                    else
                        definition^.sym:=@self;
                end;
            if (definition^.deftype=recorddef) and
             assigned(precdef(definition)^.symtable) and
             (definition^.sym=@self) then
                precdef(definition)^.symtable^.name:=stringdup('record '+name);
        end;*)
end;


procedure ttypesym.store(var s:Tstream);

begin
    inherited store(s);
{   writedefref(definition);
    current_ppu^.writeentry(ibtypesym);}
end;


procedure ttypesym.load_references;

begin
    inherited load_references;
{   if typeof(definition^)=typeof(Trecorddef) then
       Precdef(definition)^.symtable^.load_browser;
    if typeof(definition^)=typeof(Tobjectdef) then
       Pobjectdef(definition)^.publicsyms^.load_browser;}
end;


function ttypesym.write_references : boolean;

begin
(*  if not inherited write_references then
    {Write address of this symbol if record or object
     even if no real refs are there
     because we need it for the symtable }
    if (definition^.deftype=recorddef) or
     (definition^.deftype=objectdef) then
        begin
            writesymref(@self);
            current_ppu^.writeentry(ibsymref);
        end;
    write_references:=true;
    if (definition^.deftype=recorddef) then
        precdef(definition)^.symtable^.write_browser;
    if (definition^.deftype=objectdef) then
        pobjectdef(definition)^.publicsyms^.write_browser;*)
end;


procedure ttypesym.updateforwarddef(p:pdef);

var i:word;

begin
    if definition<>nil then
        internalerror($99080203)
    else
        definition:=p;
    properties:=current_type_option;
    fileinfo:=tokenpos;
    if assigned(definition) and not(assigned(definition^.sym)) then
        definition^.sym:=@self;
    {Update all forwardpointers to this definition.}
{   for i:=1 to forwardpointers^.count do
        Ppointerdef(forwardpointers^.at(i))^.definition:=definition;}
    forwardpointers^.deleteall;
    dispose(forwardpointers,done);
    forwardpointers:=nil;
end;

destructor Ttypesym.done;

var prevsym:Ptypesym;

begin
    if assigned(definition) then
        begin
            prevsym:=Ptypesym(definition^.sym);
            if prevsym=@self then
                definition^.sym:=synonym;
            while assigned(prevsym) do
                begin
                    if (prevsym^.synonym=@self) then
                        begin
                            prevsym^.synonym:=synonym;
                            break;
                     end;
                    prevsym:=prevsym^.synonym;
                end;
           end;
    synonym:=nil;
    definition:=nil;
    inherited done;
end;

{****************************************************************************
                                  Tsyssym
****************************************************************************}

constructor Tsyssym.init(const n:string;l:longint);

begin
    inherited init(n);
    {$IFDEF TP}setparent(typeof(Tsym));{$ENDIF}
    number:=l;
end;

constructor Tsyssym.load(var s:Tstream);

begin
     inherited load(s);
{    number:=readlong;}
end;

procedure tsyssym.store(var s:Tstream);

begin
    Tsym.store(s);
{   writelong(number);
    current_ppu^.writeentry(ibsyssym);}
end;
{****************************************************************************
                                  Tenumsym
****************************************************************************}

constructor Tenumsym.init(const n:string;def:Penumdef;v:longint);

begin
    inherited init(n);
    {$IFDEF TP}setparent(typeof(Tsym));{$ENDIF}
    definition:=def;
    value:=v;
    if def^.minval>v then
        def^.setmin(v);
    if def^.maxval<v then
        def^.setmax(v);
    order;
end;

constructor Tenumsym.load(var s:Tstream);

begin
    inherited load(s);
{   definition:=Penumdef(readdefref);
    value:=readlong;}
end;

procedure Tenumsym.deref;

begin
{   resolvedef(pdef(definition));
    order;}
end;

procedure Tenumsym.order;

var i:word;

label   inserted;

begin
    {Keep the enum symbols ordered by value...}
    with definition^.symbols^ do
        begin
            {Most of the time, enums are defined in order, so we count down.}
            for i:=count-1 downto 0 do
                begin
                    if Penumsym(at(i))^.value<value then
                        begin
                            atinsert(i+1,@self);
                            {We have to use goto to keep the
                             code efficient :( }
                            goto inserted;
                        end;
                end;
            atinsert(0,@self);
        inserted:
        end;
end;


procedure Tenumsym.store(var s:Tstream);

begin
    inherited store(s);
(*  writedefref(definition);
    writelong(value);
    current_ppu^.writeentry(ibenumsym);*)
end;

{****************************************************************************
                                  Tmacrosym
****************************************************************************}

constructor Tmacrosym.init(const n:string);

begin
    inherited init(n);
    defined:=true;
end;

destructor Tmacrosym.done;

begin
    if assigned(buftext) then
        freemem(buftext,buflen);
    inherited done;
end;

{****************************************************************************
                                  Tprogramsym
****************************************************************************}

{****************************************************************************
                                    Tvarsym
****************************************************************************}


constructor Tvarsym.init(const n:string;p:Pdef);

begin
    inherited init(n);
    {$IFDEF TP}setparent(typeof(Tsym));{$ENDIF}
    definition:=p;
    {Can we load the value into a register ? }
    if dp_regable in p^.properties then
        include(properties,vo_regable);
    reg:=R_NO;
end;

constructor Tvarsym.init_dll(const n:string;p:Pdef);

begin
    init(n,p);
    include(properties,vo_is_dll_var);
end;


constructor Tvarsym.init_C(const n,mangled:string;p:Pdef);

begin
    init(n,p);
    include(properties,vo_is_C_var);
    setmangledname(mangled);
end;

procedure Tvarsym.concatdata(const n:string;len:longint);

begin
end;

constructor Tvarsym.load(var s:Tstream);

begin
    inherited load(s);
    reg:=R_NO;
{   if read_member then
        address:=readlong
    else
        address:=0;
    definition:=readdefref;
    var_options:=readbyte;
    if (var_options and vo_is_C_var)<>0 then
        setmangledname(readstring);}
end;

function Tvarsym.getsize:longint;

begin
    if definition<>nil then
        getsize:=definition^.size
    else
        getsize:=0;
end;

procedure Tvarsym.deref;

begin
{   resolvedef(definition);}
end;


procedure Tvarsym.store(var s:Tstream);

begin
(*  inherited store(s);
    if read_member then
        writelong(address);
    writedefref(definition);
    { symbols which are load are never candidates for a register,
      turn of the regable }
    writebyte(var_options and (not vo_regable));
    if (var_options and vo_is_C_var)<>0 then
        writestring(mangledname);
    current_ppu^.writeentry(ibvarsym);*)
end;


procedure Tvarsym.setmangledname(const s:string);

begin
    _mangledname:=stringdup(s);
end;


function Tvarsym.mangledname:string;

var prefix:string;

begin
    if assigned(_mangledname) then
        mangledname:=_mangledname^
    else
        mangledname:=owner^.varsymprefix+name;
end;

procedure Tvarsym.insert_in_data;

var l,ali,modulo:longint;
    storefilepos:Tfileposinfo;

begin
    if (vo_is_external in properties) then
        begin
            {Handle static variables of objects especially }
            if read_member and (sp_static in objprop) then
                begin
                    {The data field is generated in parser.pas
                     with a tobject_FIELDNAME variable, so we do
                     not need to do it in this procedure.}

                    {This symbol can't be loaded to a register.}
                    exclude(properties,vo_regable);
                end
            else
                if not(read_member) then
                    begin
                        storefilepos:=aktfilepos;
                        aktfilepos:=tokenpos;
                        if (vo_is_thread_var in properties) then
                            l:=4
                        else
                            l:=getsize;
                        address:=owner^.varsymtodata(@self,l);
                        aktfilepos:=storefilepos;
                    end;
        end;
end;

destructor Tvarsym.done;

begin
    disposestr(_mangledname);
    inherited done;
end;

{****************************************************************************
                                Tparamsym
****************************************************************************}

constructor Tparamsym.init(const n:string;p:Pdef;vs:Tvarspez);

begin
    inherited init(n,p);
    {$IFDEF TP}setparent(typeof(Tsym));{$ENDIF}
    varspez:=vs;
end;

function Tparamsym.getsize:longint;

begin
    if (definition<>nil) and (varspez=vs_value) then
        getsize:=definition^.size
    else
        getsize:=0;
end;

function Tparamsym.getpushsize:longint;

begin
    if assigned(definition) then
        begin
            case varspez of
                vs_var:
                    getpushsize:=target_os.size_of_pointer;
                vs_value,vs_const:
                     if dp_pointer_param in definition^.properties then
                         getpushsize:=target_os.size_of_pointer
                     else
                         getpushsize:=definition^.size;
            end;
        end
    else
        getpushsize:=0;
end;

procedure Tparamsym.insert_in_data;

var storefilepos:Tfileposinfo;

begin
    storefilepos:=aktfilepos;
    if not(read_member) then
        pushaddress:=owner^.varsymtodata(@self,getpushsize);
    if (varspez=vs_var) then
        address:=0
    else if (varspez=vs_value) then
        if dp_pointer_param in definition^.properties then
            begin
                {Allocate local space.}
                address:=owner^.datasize;
                inc(owner^.datasize,getsize);
            end
        else
            address:=pushaddress
    else
        {vs_const}
        if dp_pointer_param in definition^.properties then
            address:=0
        else
            address:=pushaddress;
    aktfilepos:=storefilepos;
end;

{****************************************************************************
                             Ttypedconstsym
*****************************************************************************}

constructor Ttypedconstsym.init(const n:string;p:Pdef;really_const:boolean);

begin
   inherited init(n);
    {$IFDEF TP}setparent(typeof(Tsym));{$ENDIF}
   definition:=p;
   is_really_const:=really_const;
   prefix:=stringdup(procprefix);
end;

constructor Ttypedconstsym.load(var s:Tstream);

begin
    inherited load(s);
(*  definition:=readdefref;
{$ifdef DELPHI_CONST_IN_RODATA}
    is_really_const:=boolean(readbyte);
{$else DELPHI_CONST_IN_RODATA}
    is_really_const:=false;
{$endif DELPHI_CONST_IN_RODATA}
    prefix:=stringdup(readstring);*)
end;

procedure Ttypedconstsym.deref;

begin
{   resolvedef(definition);}
end;

function Ttypedconstsym.mangledname:string;

begin
    mangledname:='TC_'+prefix^+'_'+name;
end;


function Ttypedconstsym.getsize:longint;

begin
    if assigned(definition) then
        getsize:=definition^.size
    else
        getsize:=0;
end;

procedure Ttypedconstsym.store(var s:Tstream);

begin
   inherited store(s);
(*   writedefref(definition);
   writestring(prefix^);
{$ifdef DELPHI_CONST_IN_RODATA}
   writebyte(byte(is_really_const));
{$endif DELPHI_CONST_IN_RODATA}
   current_ppu^.writeentry(ibtypedconstsym);*)
end;

{ for most symbol types ther is nothing to do at all }
procedure Ttypedconstsym.insert_in_data;

var constsegment:Paasmoutput;
    l,ali,modulo:longint;
    storefilepos:Tfileposinfo;

begin
    storefilepos:=aktfilepos;
    aktfilepos:=tokenpos;
    owner^.tconstsymtodata(@self,getsize);
    aktfilepos:=storefilepos;
end;

destructor Ttypedconstsym.done;

begin
    stringdispose(prefix);
    inherited done;
end;

{****************************************************************************
                                  TCONSTSYM
****************************************************************************}

constructor Tconstsym.init(const n : string;t : tconsttype;v : longint);

begin
    inherited init(n);
    {$IFDEF TP}setparent(typeof(Tsym));{$ENDIF}
    consttype:=t;
    value:=v;
end;


constructor Tconstsym.init_def(const n:string;t:Tconsttype;
                               v:longint;def:Pdef);

begin
    inherited init(n);
    consttype:=t;
    value:=v;
    definition:=def;
end;


constructor Tconstsym.init_string(const n:string;t:Tconsttype;str:Pchar;l:longint);

begin
    inherited init(n);
    consttype:=t;
    value:=longint(str);
    len:=l;
end;

constructor Tconstsym.load(var s:Tstream);

var pd:Pbestreal;
    ps:Pnormalset;

begin
    inherited load(s);
(*  consttype:=tconsttype(readbyte);
    case consttype of
      constint,
      constbool,
      constchar : value:=readlong;
      constord :
        begin
          definition:=readdefref;
          value:=readlong;
        end;
      conststring :
        begin
          len:=readlong;
          getmem(pchar(value),len+1);
          current_ppu^.getdata(pchar(value)^,len);
        end;
      constreal :
        begin
          new(pd);
          pd^:=readreal;
          value:=longint(pd);
        end;
      constset :
        begin
          definition:=readdefref;
          new(ps);
          readnormalset(ps^);
          value:=longint(ps);
        end;
      constnil : ;
      else
        Message1(unit_f_ppu_invalid_entry,tostr(ord(consttype)));
   end;*)
end;

procedure Tconstsym.deref;

begin
{   if consttype in [constord,constset] then
        resolvedef(pdef(definition));}
end;


procedure Tconstsym.store(var s:Tstream);

begin
(*  inherited store(s);
    writebyte(byte(consttype));
    case consttype of
      constnil : ;
      constint,
      constbool,
      constchar :
        writelong(value);
      constord :
        begin
          writedefref(definition);
          writelong(value);
        end;
      conststring :
        begin
          writelong(len);
          current_ppu^.putdata(pchar(value)^,len);
        end;
      constreal :
        writereal(pbestreal(value)^);
      constset :
        begin
          writedefref(definition);
          writenormalset(pointer(value)^);
        end;
    else
      internalerror(13);
    end;
    current_ppu^.writeentry(ibconstsym);*)
end;

destructor Tconstsym.done;

begin
    case consttype of
        conststring:
            freemem(Pchar(value),len+1);
        constreal:
            dispose(Pbestreal(value));
        constset:
            dispose(Pnormalset(value));
    end;
    inherited done;
end;

{****************************************************************************
                                Tabsolutesym
****************************************************************************}

constructor Tabsolutesym.load(var s:Tstream);

begin
    inherited load(s);
(*  typ:=absolutesym;
    abstyp:=absolutetyp(readbyte);
    case abstyp of
      tovar :
        begin
          asmname:=stringdup(readstring);
          ref:=srsym;
        end;
      toasm :
        asmname:=stringdup(readstring);
      toaddr :
        begin
          address:=readlong;
          absseg:=boolean(readbyte);
        end;
    end;*)
end;


procedure tabsolutesym.store(var s:Tstream);

begin
    inherited store(s);
(*  writebyte(byte(varspez));
    if read_member then
      writelong(address);
    writedefref(definition);
    writebyte(var_options and (not vo_regable));
    writebyte(byte(abstyp));
    case abstyp of
      tovar :
        writestring(ref^.name);
      toasm :
        writestring(asmname^);
      toaddr :
        begin
          writelong(address);
          writebyte(byte(absseg));
        end;
    end;
    current_ppu^.writeentry(ibabsolutesym);*)
end;


procedure tabsolutesym.deref;

begin
(*  resolvedef(definition);
    if (abstyp=tovar) and (asmname<>nil) then
        begin
            { search previous loaded symtables }
            getsym(asmname^,false);
            if not(assigned(srsym)) then
            getsymonlyin(owner,asmname^);
            if not(assigned(srsym)) then
                srsym:=generrorsym;
            ref:=srsym;
            stringdispose(asmname);
       end;*)
end;


function Tabsolutesym.mangledname : string;

begin
    case abstyp of
        tovar :
            mangledname:=ref^.mangledname;
        toasm :
            mangledname:=asmname^;
        toaddr :
            mangledname:='$'+tostr(address);
        else
            internalerror(10002);
    end;
end;

{****************************************************************************
                                  Tfuncretsym
****************************************************************************}

constructor Tfuncretsym.init(const n:string;approcinfo:pointer{pprocinfo});

begin
    inherited init(n);
    {$IFDEF TP}setparent(typeof(Tsym));{$ENDIF}
    funcretprocinfo:=approcinfo;
{   funcretdef:=Pprocinfo(approcinfo)^.retdef;}
    { address valid for ret in param only }
    { otherwise set by insert             }
{   address:=pprocinfo(approcinfo)^.retoffset;}
end;

constructor Tfuncretsym.load(var s:Tstream);

begin
    inherited load(s);
{   funcretdef:=readdefref;
    address:=readlong;
    funcretprocinfo:=nil;
    typ:=funcretsym;}
end;

procedure Tfuncretsym.store(var s:Tstream);

begin
     (*
      Normally all references are
      transfered to the function symbol itself !! PM *)
    inherited store(s);
{   writedefref(funcretdef);
    writelong(address);

    current_ppu^.writeentry(ibfuncretsym);}
end;

procedure Tfuncretsym.deref;

begin
    {resolvedef(funcretdef);}
end;

procedure Tfuncretsym.insert_in_data;

var l:longint;

begin
    {Allocate space in local if ret in acc or in fpu.}
{   if dp_ret_in_acc in procinfo.retdef^.properties
     or (procinfo.retdef^.deftype=floatdef) then
        begin
            l:=funcretdef^.size;
            adress:=owner^.varsymtodata('',l);
            procinfo.retoffset:=-owner^.datasize;
        end;}
end;

{****************************************************************************
                                Tpropertysym
****************************************************************************}

constructor tpropertysym.load(var s:Tstream);

begin
    inherited load(s);
(*  proptype:=readdefref;
    options:=readlong;
    index:=readlong;
    default:=readlong;
    { it's hack ... }
    readaccesssym:=psym(stringdup(readstring));
    writeaccesssym:=psym(stringdup(readstring));
    storedsym:=psym(stringdup(readstring));
    { now the defs: }
    readaccessdef:=readdefref;
    writeaccessdef:=readdefref;
    storeddef:=readdefref;*)
end;

procedure Tpropertysym.deref;

begin
(*  resolvedef(proptype);
    resolvedef(readaccessdef);
    resolvedef(writeaccessdef);
    resolvedef(storeddef);
    { solve the hack we did in load: }
    if pstring(readaccesssym)^<>'' then
      begin
         srsym:=search_class_member(pobjectdef(owner^.defowner),pstring(readaccesssym)^);
         if not(assigned(srsym)) then
           srsym:=generrorsym;
      end
    else
      srsym:=nil;
    stringdispose(pstring(readaccesssym));
    readaccesssym:=srsym;

    if pstring(writeaccesssym)^<>'' then
      begin
         srsym:=search_class_member(pobjectdef(owner^.defowner),pstring(writeaccesssym)^);
         if not(assigned(srsym)) then
           srsym:=generrorsym;
      end
    else
      srsym:=nil;
    stringdispose(pstring(writeaccesssym));
    writeaccesssym:=srsym;

    if pstring(storedsym)^<>'' then
      begin
         srsym:=search_class_member(pobjectdef(owner^.defowner),pstring(storedsym)^);
         if not(assigned(srsym)) then
           srsym:=generrorsym;
      end
    else
      srsym:=nil;
    stringdispose(pstring(storedsym));
    storedsym:=srsym;*)
end;

function Tpropertysym.getsize:longint;

begin
    getsize:=0;
end;

procedure Tpropertysym.store(var s:Tstream);

begin
    Tsym.store(s);
(*  writedefref(proptype);
    writelong(options);
    writelong(index);
    writelong(default);
    if assigned(readaccesssym) then
        writestring(readaccesssym^.name)
    else
        writestring('');
    if assigned(writeaccesssym) then
      writestring(writeaccesssym^.name)
    else
      writestring('');
    if assigned(storedsym) then
      writestring(storedsym^.name)
    else
      writestring('');
    writedefref(readaccessdef);
    writedefref(writeaccessdef);
    writedefref(storeddef);
    current_ppu^.writeentry(ibpropertysym);*)
end;

end.

{
  $Log$
  Revision 1.1  2000-07-13 06:30:13  michael
  + Initial import

  Revision 1.6  2000/03/16 12:52:48  daniel
    *  Changed names of procedures flags
    *  Changed VMT generation

  Revision 1.5  2000/03/11 21:11:25  daniel
    * Ported hcgdata to new symtable.
    * Alignment code changed as suggested by Peter
    + Usage of my is operator replacement, is_object

  Revision 1.4  2000/03/01 11:43:56  daniel
  * Some more work on the new symtable.
  + Symtable stack unit 'symstack' added.

}