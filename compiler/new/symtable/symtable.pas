{
    $Id$
    Copyright (C) 1998-2000 by Florian Klaempfl, Daniel Mantione,
     Pierre Muller and other members of the Free Pascal development team

    This unit handles the symbol tables

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
unit symtable;

interface

uses    objects{$IFDEF TP},xobjects{$ENDIF}
        ,cobjects,aasm,globtype,cpubase;


type    Tdefprop=(dp_regable,           {Can be stored into a register.}
                  dp_pointer_param,     {A pointer should be used
                                         instead of the value for
                                         parameters of this definition.}
                  dp_ret_in_acc);       {Function results of this
                                         definition can be returned into
                                         the accumulator.}

        Tdefpropset=set of Tdefprop;
        Psymtable=^Tsymtable;
        Pcontainingsymtable=^Tcontainingsymtable;
        Pref=^Tref;
        Psymtableentry=^Tsymtableentry;
        Psym=^Tsym;
        Pdef=^Tdef;

        Tsymtable=object(Tobject)
            name:Pstring;
            datasize:longint;
        {$IFDEF TP}
            constructor init;
        {$ENDIF TP}
            procedure foreach(proc2call:Tnamedindexcallback);virtual;
            function insert(sym:Psym):boolean;virtual;
            function search(const s:stringid):Psym;
            function speedsearch(const s:stringid;
                                 speedvalue:longint):Psym;virtual;
            function tconstsymtodata(sym:Psym;len:longint):longint;virtual;
            function varsymprefix:string;virtual;
            function varsymtodata(sym:Psym;len:longint):longint;virtual;
        end;

        Tcontainingsymtable=object(Tsymtable)
            alignment:byte;         {Aligment used in this symtable.}
            index_growsize:word;    {The delta of the defindex collection.}
            defindex:Pcollection;   {Contains all definitions in symtable.}
            symsearch:Pdictionary;
            constructor init;
            constructor load(var s:Tstream);
            procedure set_contents(s:Pdictionary;d:Pcollection);
            {Get_contents disposes the symtable object!!}
            procedure get_contents(var s:Pdictionary;var d:Pcollection);
            {Checks if all variabeles are used.}
            procedure check_vars;
            {Checks if all forwards resolved.}
            procedure check_forwards;
            {Checks if all labels used.}
            procedure check_labels;
            procedure foreach(proc2call:Tnamedindexcallback);virtual;
            function insert(sym:Psym):boolean;virtual;
            function speedsearch(const s:stringid;
                                 speedvalue:longint):Psym;virtual;
            procedure store(var s:Tstream);virtual;
            procedure registerdef(p:Pdef);
            destructor done;virtual;
        end;

        Tref=object(Tobject)
            posinfo:Tfileposinfo;
            moduleindex:word;
            constructor init(const pos:Tfileposinfo);
            destructor done;virtual;
        end;

        Tsymtableentry=object(Tnamedindexobject)
            owner:Pcontainingsymtable;
        {$IFDEF TP}
            constructor init(const n:string);
        {$ENDIF TP}
        end;

        Tsymprop=byte;

        Tsym=object(Tsymtableentry)
            fileinfo:Tfileposinfo;
            references:Pcollection;     {Contains all references to symbol.}
            constructor init(const n : string);
            constructor load(var s:Tstream);
            procedure deref;virtual;
            procedure make_reference;
            function mangledname:string;virtual;
            procedure insert_in_data;virtual;
            procedure load_references;virtual;
            procedure register_defs;virtual;
            procedure store(var s:Tstream);virtual;
            function write_references:boolean;virtual;
{$ifdef BrowserLog}
            procedure add_to_browserlog;virtual;
{$endif BrowserLog}
            destructor done;virtual;
        end;

        Tdef=object(Tobject)
            savesize:longint;
            sym:Psym;
            owner:Pcontainingsymtable;
            properties:Tdefpropset;
            inittable:Pasmlabel;        {Nil, or pointer to inittable.}
            rtti:Pasmlabel;             {Nil, or pointer to rtti.}
            constructor init(Aowner:Pcontainingsymtable);
            constructor load(var s:Tstream);
            destructor done;virtual;
            {procedure correct_owner_symtable; REMOVED
             enumdefs can be safely in a record or object symtable,
             but the enum symbols must be in owners symtable.}
            procedure store(var s:Tstream);virtual;
            {Returns the typename of this definition.}
            function typename:string;virtual;
            procedure deref;virtual;
            function size:longint;virtual;
            procedure symderef;virtual;
            {Init. tables }
            function  needs_inittable:boolean;virtual;
            procedure generate_inittable;
            function get_inittable_label:Pasmlabel;
            {The default implemenation calls write_rtti_data
             if init and rtti data is different these procedures
             must be overloaded.}
            procedure write_init_data;virtual;
            {Writes rtti of child to avoid mixup of rtti.}
            procedure write_child_init_data;virtual;

            {Rtti}
            procedure write_rtti_name;
            function  get_rtti_label:string;virtual;
            procedure generate_rtti;virtual;
            procedure write_rtti_data;virtual;
            procedure write_child_rtti_data;virtual;

            { returns true, if the definition can be published }
            function is_publishable : boolean;virtual;
            function gettypename:string;virtual;
        end;

const   systemunit:Psymtable            = nil; {Pointer to the system unit.}
        objpasunit:Psymtable            = nil; {Pointer to the objpas unit.}
        macros:Psymtable                = nil; {Pointer to macro list.}

const   defalignment=4;

var     read_member : boolean;      {True, wenn Members aus einer PPU-
                                     Datei gelesen werden, d.h. ein
                                     varsym seine Adresse einlesen soll }
        procprefix:stringid;

        generrorsym:Psym;           {Jokersymbol, wenn das richtige
                                     symbol nicht gefunden wird.}
        generrordef:Pdef;           {Jokersymbol for eine fehlerhafte
                                     typdefinition.}
procedure duplicatesym(sym:psym);

{**************************************************************************}

implementation

{**************************************************************************}

uses    symtablt,files,verbose,globals;


{****************************************************************************
                                Tsymtable
****************************************************************************}

{$IFDEF TP}
constructor Tsymtable.init;

begin
    setparent(typeof(Tobject));
end;
{$ENDIF TP}

procedure Tsymtable.foreach(proc2call:Tnamedindexcallback);

begin
    abstract;
end;

function Tsymtable.insert(sym:Psym):boolean;

begin
    abstract;
end;

function Tsymtable.search(const s:stringid):Psym;

begin
    search:=speedsearch(s,getspeedvalue(s));
end;

function Tsymtable.speedsearch(const s:stringid;speedvalue:longint):Psym;

begin
    abstract;
end;

function Tsymtable.tconstsymtodata(sym:Psym;len:longint):longint;

begin
    tconstsymtodata:=datasize;
    inc(datasize,len);
end;

function Tsymtable.varsymprefix:string;

begin
    abstract;
end;

function Tsymtable.varsymtodata(sym:Psym;len:longint):longint;

begin
    varsymtodata:=datasize;
    inc(datasize,len);
end;

{****************************************************************************
                            Tcontainingsymtable
****************************************************************************}

constructor Tcontainingsymtable.init;

var indexgrow:word;

begin
    inherited init;
    {$IFDEF TP}setparent(typeof(Tsymtable));{$ENDIF}
    indexgrow:=index_growsize;
    new(defindex,init(2*indexgrow,indexgrow));
    new(symsearch,init);
    alignment:=defalignment;
    index_growsize:=16;
end;

constructor Tcontainingsymtable.load;

begin
end;

procedure Tcontainingsymtable.get_contents(var s:Pdictionary;
                                           var d:Pcollection);

begin
    s:=symsearch;
    d:=defindex;
    free;
end;

procedure Tcontainingsymtable.store(var s:Tstream);

begin
end;

procedure Tcontainingsymtable.check_vars;

begin
end;

procedure Tcontainingsymtable.check_forwards;

begin
end;

procedure Tcontainingsymtable.check_labels;

begin
end;

procedure Tcontainingsymtable.foreach(proc2call:Tnamedindexcallback);

begin
    symsearch^.foreach(proc2call);
end;

function Tcontainingsymtable.insert(sym:Psym):boolean;

begin
    insert:=true;
    if symsearch^.insert(sym)<>Pnamedindexobject(sym) then
        begin
            duplicatesym(sym);
            insert:=false;
        end
    else
        begin
            sym^.owner:=@self;
            sym^.register_defs;
        end;
end;

procedure Tcontainingsymtable.set_contents(s:Pdictionary;d:Pcollection);

begin
    dispose(defindex,done);
    dispose(symsearch,done);
    defindex:=d;
    symsearch:=s;
end;

function Tcontainingsymtable.speedsearch(const s:stringid;
                                         speedvalue:longint):Psym;

var r:Psym;

begin
    r:=Psym(symsearch^.speedsearch(s,speedvalue));
    {Make a notice that the symbol is referenced.}
    if (r<>nil) and (cs_browser in aktmoduleswitches) and make_ref then
        r^.make_reference;
    speedsearch:=r;
end;

procedure Tcontainingsymtable.registerdef(p:Pdef);

begin
    defindex^.insert(p);
    p^.owner:=@self;
end;

destructor Tcontainingsymtable.done;

begin
    dispose(defindex,done);
    dispose(symsearch,done);
    inherited done;
end;

{****************************************************************************
                                    Tref
****************************************************************************}

constructor Tref.init(const pos:Tfileposinfo);

begin
    inherited init;
    {$IFDEF TP}setparent(typeof(Tobject));{$ENDIF}
    posinfo:=pos;
    moduleindex:=current_module^.unit_index;
end;

destructor Tref.done;

var inputfile:Pinputfile;

begin
    inputfile:=get_source_file(moduleindex,posinfo.fileindex);
    if inputfile<>nil then
        dec(inputfile^.ref_count);
end;

procedure duplicatesym(sym:Psym);

begin
    message1(sym_e_duplicate_id,sym^.name);
    with sym^.fileinfo do
        message2(sym_h_duplicate_id_where,
         current_module^.sourcefiles^.get_file_name(fileindex),tostr(line));
end;

{****************************************************************************
                                Tsymtableentry
****************************************************************************}

{$IFDEF TP}
constructor Tsymtableentry.init(const n:string);

begin
    inherited init(n);
    setparent(typeof(Tnamedindexobject));
end;
{$ENDIF TP}

{****************************************************************************
                                    Tsym
****************************************************************************}

constructor Tsym.init(const n:string);

begin
    inherited init(n);
    {$IFDEF TP}setparent(typeof(Tsymtableentry));{$ENDIF}
    fileinfo:=tokenpos;
    if cs_browser in aktmoduleswitches then
        new(references,init(32,16));
    {The place where a symbol is defined is also a reference. You can safely
     assume that the first reference in the references collection is the
     place where the symbol is defined.}
    make_reference;
end;

constructor Tsym.load(var s:Tstream);

begin
end;

procedure Tsym.deref;

begin
    abstract;
end;

procedure Tsym.insert_in_data;

begin
end;

procedure Tsym.make_reference;

begin
    if (cs_browser in aktmoduleswitches) and make_ref then
        references^.insert(new(Pref,init(tokenpos)));
end;

function Tsym.mangledname:string;

begin
    mangledname:=name;
end;

procedure Tsym.register_defs;

begin
end;

procedure Tsym.store(var s:Tstream);

begin
end;

destructor Tsym.done;

begin
    if references<>nil then
        dispose(references,done);
    inherited done;
end;
procedure Tsym.load_references;

begin
end;

function Tsym.write_references:boolean;

begin
end;

{****************************************************************************
                                    Tdef
****************************************************************************}

constructor Tdef.init(Aowner:Pcontainingsymtable);

begin
    inherited init;
    {$IFDEF TP}setparent(typeof(Tobject));{$ENDIF}
    Aowner^.registerdef(@self);
    owner:=Aowner;
end;

constructor Tdef.load;

begin
end;

procedure Tdef.store(var s:Tstream);

begin
end;

function Tdef.typename:string;

begin
    typename:='<unknown type>';
end;

procedure Tdef.deref;

begin
end;

function Tdef.size:longint;

begin
    size:=savesize;
end;

procedure Tdef.symderef;

begin
end;

function Tdef.needs_inittable:boolean;

begin
end;

procedure Tdef.generate_inittable;

begin
end;

function Tdef.get_inittable_label:Pasmlabel;

begin
end;

procedure Tdef.write_init_data;

begin
end;

procedure Tdef.write_child_init_data;

begin
end;

procedure Tdef.write_rtti_name;

begin
end;

function  Tdef.get_rtti_label:string;

begin
end;

procedure Tdef.generate_rtti;

begin
end;

procedure Tdef.write_rtti_data;

begin
end;

procedure Tdef.write_child_rtti_data;

begin
end;


function Tdef.is_publishable:boolean;

begin
    is_publishable:=false;
end;


function Tdef.gettypename:string;

begin
    gettypename:='<unknown type>';
end;

destructor Tdef.done;

{var    s:Ptypesym;}

begin
{   s:=sym;
    while s<>nil do
        begin
            s^.definition:=nil;
            s:=s^.synonym;
        end;}
    inherited done;
end;

end.