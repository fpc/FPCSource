{
    $Id$

    This unit implements the different types of symbol tables

    Copyright (C) 1998-2000 by Daniel Mantione,
     member of the Free Pascal development team

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

unit symtablt;

interface

uses    objects,cobjects,symtable,globtype;


type    Pglobalsymtable=^Tglobalsymtable;
        Pinterfacesymtable=^Tinterfacesymtable;
        Pimplsymtable=^Tsymtable;
        Pprocsymtable=^Tprocsymtable;
        Punitsymtable=^Tunitsymtable;
        Pobjectsymtable=^Tobjectsymtable;
        Pwithsymtable=^Twithsymtable;

        Tglobalsymtable=object(Tcontainingsymtable)
            constructor init;
            {Checks if all used units are used.}
            procedure check_units;
            function tconstsymtodata(sym:Psym;len:longint):longint;virtual;
            function varsymtodata(sym:Psym;len:longint):longint;virtual;
        end;

        Tinterfacesymtable=object(Tglobalsymtable)
            unitid:word;
            function varsymprefix:string;virtual;
        end;

        Timplsymtable=object(Tglobalsymtable)
            unitid:word;
            function varsymprefix:string;virtual;
        end;

        Tabstractrecordsymtable=object(Tcontainingsymtable)
            function varsymtodata(sym:Psym;len:longint):longint;virtual;
        end;

        Precordsymtable=^Trecordsymtable;
        Trecordsymtable=object(Tabstractrecordsymtable)
        end;

        Tobjectsymtable=object(Tabstractrecordsymtable)
            defowner:Pobjectsymtable;
{           function speedsearch(const s:stringid;
                                 speedvalue:longint):Psym;virtual;}
        end;

        Tprocsymtable=object(Tcontainingsymtable)
            {Replaces the old local and paramsymtables.}
            lexlevel:byte;
            paramdatasize:longint;
            {If this is a method, this points to the objectdef. It is
             possible to make another Tmethodsymtable and move this field
             to it, but I think the advantage is not worth it. (DM)}
            method:Pdef;
            function insert(sym:Psym):boolean;virtual;
            function speedsearch(const s:stringid;
                                 speedvalue:longint):Psym;virtual;
            function varsymtodata(sym:Psym;len:longint):longint;virtual;
        end;

        Tunitsymtable=object(Tcontainingsymtable)
            unittypecount:word;
            unitsym:Psym;
            constructor init(const n:string);
            {Checks if all used units are used.}
            procedure check_units;
            function speedsearch(const s:stringid;
                                 speedvalue:longint):Psym;virtual;
            function tconstsymtodata(sym:Psym;len:longint):longint;virtual;
            function varsymprefix:string;virtual;
            destructor done;virtual;
        end;

        Twithsymtable=object(Tsymtable)
            link:Pcontainingsymtable;
            {If with a^.b.c is encountered, withrefnode points to a tree
             a^.b.c .}
            withrefnode:pointer;
            constructor init(Alink:Pcontainingsymtable);
            function speedsearch(const s:stringid;
                                 speedvalue:longint):Psym;virtual;
        end;

implementation

uses    symbols,files,globals,aasm,systems,defs,verbose;

function data_align(length:longint):longint;

begin
    if length>2 then
        data_align:=4
    else if length>1 then
        data_align:=2
    else
        data_align:=1;
end;

{****************************************************************************
                              Tglobalsymtable
****************************************************************************}

constructor Tglobalsymtable.init;

begin
    inherited init;
    index_growsize:=128;
end;

procedure Tglobalsymtable.check_units;

begin
end;

function Tglobalsymtable.tconstsymtodata(sym:Psym;len:longint):longint;

var ali:longint;
    segment:Paasmoutput;

begin
    if Ptypedconstsym(sym)^.is_really_const then
        segment:=consts
    else
        segment:=datasegment;
    if (cs_create_smart in aktmoduleswitches) then
        segment^.concat(new(Pai_cut,init));
    ali:=data_align(len);
    align(datasize,ali);
{$ifdef GDB}
    if cs_debuginfo in aktmoduleswitches then
        concatstabto(segment);
{$endif GDB}
    segment^.concat(new(Pai_symbol,initname_global(sym^.mangledname,len)));
end;

function Tglobalsymtable.varsymtodata(sym:Psym;len:longint):longint;

var ali:longint;

begin
    if (cs_create_smart in aktmoduleswitches) then
        bsssegment^.concat(new(Pai_cut,init));
    ali:=data_align(len);
    align(datasize,ali);
{$ifdef GDB}
    if cs_debuginfo in aktmoduleswitches then
        concatstabto(bsssegment);
{$endif GDB}
    bsssegment^.concat(new(Pai_datablock,
     init_global(sym^.mangledname,len)));
    varsymtodata:=inherited varsymtodata(sym,len);
    {This symbol can't be loaded to a register.}
    exclude(Pvarsym(sym)^.properties,vo_regable);
end;

{****************************************************************************
                               Timplsymtable
****************************************************************************}


function Timplsymtable.varsymprefix:string;

begin
    varsymprefix:='U_'+name^+'_';
end;

{****************************************************************************
                            Tinterfacesymtable
****************************************************************************}

function Tinterfacesymtable.varsymprefix:string;

begin
    varsymprefix:='_'+name^+'$$$'+'_';
end;

{****************************************************************************
                        Tabstractrecordsymtable
****************************************************************************}

function Tabstractrecordsymtable.varsymtodata(sym:Psym;
                                             len:longint):longint;

begin
    datasize:=(datasize+(packrecordalignment[aktpackrecords]-1))
     and not (packrecordalignment[aktpackrecords]-1);
    varsymtodata:=inherited varsymtodata(sym,len);
end;

{****************************************************************************
                             Trecordsymtable
****************************************************************************}

{****************************************************************************
                             Tobjectsymtable
****************************************************************************}

{This is not going to work this way, because the definition isn't known yet
 when the symbol hasn't been found. For procsyms the object properties
 are stored in the definitions, because they can be overloaded.

function Tobjectsymtable.speedsearch(const s:stringid;
                                     speedvalue:longint):Psym;

var r:Psym;

begin
    r:=inherited speedsearch(s,speedvalue);
    if (r<>nil) and (Pprocdef(r)^.objprop=sp_static) and
     allow_only_static then
        begin
            message(sym_e_only_static_in_static);
            speedsearch:=nil;
        end
    else
        speedsearch:=r;
end;}

{****************************************************************************
                             Tprocsymsymtable
****************************************************************************}

function Tprocsymtable.insert(sym:Psym):boolean;

begin
    if (method<>nil) and (Pobjectdef(method)^.search(sym^.name)<>nil) then
        insert:=inherited insert(sym)
    else
        duplicatesym(sym);
end;

function Tprocsymtable.speedsearch(const s:stringid;
                                   speedvalue:longint):Psym;

begin
    speedsearch:=inherited speedsearch(s,speedvalue);
end;

function Tprocsymtable.varsymtodata(sym:Psym;
                                    len:longint):longint;

var modulo:longint;

begin
    if typeof(sym^)=typeof(Tparamsym) then
        begin
            varsymtodata:=paramdatasize;
            paramdatasize:=align(datasize+len,target_os.stackalignment);
        end
    else
        begin
            {Sym must be a varsym.}
            {Align datastructures >=4 on a dword.}
            if len>=4 then
                align(len,4)
            else
{$ifdef m68k}
                {Align datastructures with size 1,2,3 on a word.}
                align(len,2);
{$else}
                {Align datastructures with size 2 or 3 on a word.}
                if len>=2 then
                    align(len,2);
{$endif}
            varsymtodata:=inherited varsymtodata(sym,len);
        end;
end;

{****************************************************************************
                               Tunitsymtable
****************************************************************************}

constructor Tunitsymtable.init(const n:string);

begin
    inherited init;
    name:=stringdup(n);
    index_growsize:=128;
end;

procedure Tunitsymtable.check_units;

begin
end;

function Tunitsymtable.speedsearch(const s:stringid;
                                   speedvalue:longint):Psym;

var r:Psym;

begin
    r:=inherited speedsearch(s,speedvalue);
{   if unitsym<>nil then
        Punitsym(unitsym)^.refs;}
{   if (r^.typ=unitsym) and assigned(current_module) and
     (current_module^.interfacesymtable<>@self) then
        r:=nil;}
    speedsearch:=r;
end;

function Tunitsymtable.tconstsymtodata(sym:Psym;len:longint):longint;

var ali:longint;
    segment:Paasmoutput;

begin
    if Ptypedconstsym(sym)^.is_really_const then
        segment:=consts
    else
        segment:=datasegment;
    if (cs_create_smart in aktmoduleswitches) then
        segment^.concat(new(Pai_cut,init));
    ali:=data_align(len);
    align(datasize,ali);
{$ifdef GDB}
    if cs_debuginfo in aktmoduleswitches then
        concatstabto(segment);
{$endif GDB}
    if (cs_create_smart in aktmoduleswitches) then
        segment^.concat(new(Pai_symbol,
                        initname_global(sym^.mangledname,len)))
    else
        segment^.concat(new(Pai_symbol,
                        initname(sym^.mangledname,len)));
end;

function Tunitsymtable.varsymprefix:string;

begin
    varsymprefix:='U_'+name^+'_';
end;

destructor Tunitsymtable.done;

begin
    stringdispose(name);
    inherited done;
end;

{****************************************************************************
                               Twithsymtable
****************************************************************************}

constructor Twithsymtable.init(Alink:Pcontainingsymtable);

begin
    inherited init;
    link:=Alink;
end;

function Twithsymtable.speedsearch(const s:stringid;speedvalue:longint):Psym;

begin
    speedsearch:=link^.speedsearch(s,speedvalue);
end;

end.
