{
    $Id$
    Copyright (c) 1998-2000 by Daniel Mantione
     member of the Free Pascal development team

    Commandline compiler for Free Pascal

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

 ***************************************************************************}

unit symstack;

interface

uses    objects,symtable,globtype;

const   cachesize=64;   {This should be a power of 2.}

type    Tsymtablestack=object(Tobject)
            srsym:Psym;                 {Result of the last search.}
            srsymtable:Psymtable;
            lastsrsym:Psym;             {Last sym found in statement.}
            lastsrsymtable:Psymtable;
            constructor init;
            procedure clearcache;
            procedure insert(s:Psym;addtocache:boolean);
            function pop:Psymtable;
            procedure push(s:Psymtable);
            procedure search(const s:stringid;notfounderror:boolean);
            function search_a_symtable(const symbol:stringid;symtabletype:pointer):Psym;
            function top:Psymtable;
            procedure topfree;
            destructor done;virtual;
        private
            cache:array[1..cachesize] of Psym;
            cachetables:array[1..cachesize] of Psymtable;
            symtablestack:Tcollection;  {For speed reasons this is not
                                         a pointer. A Tcollection is not
                                         the perfect data structure for
                                         a stack; it could be a good idea
                                         to write an abstract stack object.}
            procedure decache(s:Psymtable);
        end;

{$IFDEF STATISTICS}
var hits,misses:longint;
{$ENDIF STATISTICS}

implementation

uses    cobjects,symtablt,verbose,symbols,defs;

var oldexit:pointer;

constructor Tsymtablestack.init;

begin
    symtablestack.init(16,8);
    clearcache;
end;

procedure Tsymtablestack.clearcache;

begin
    fillchar(cache,sizeof(cache),0);
    fillchar(cachetables,sizeof(cache),0);
end;

procedure Tsymtablestack.decache(s:Psymtable);

var p,endp:^Psymtable;
    q:^Psym;

begin
    {Must be fast, otherwise the speed advantage is lost!
     Therefore, the cache should not be too large...}
    p:=@cachetables;
    endp:=pointer(longint(@cachetables)+cachesize*sizeof(pointer));
    q:=@cache;
    repeat
        if p^=s then
            begin
                p^:=nil;
                q^:=nil;
            end;
        inc(p);
        inc(q);
    until p=endp;
end;

procedure Tsymtablestack.search(const s:stringid;notfounderror:boolean);

var speedvalue,entry:longint;
    i:word;

begin
    speedvalue:=getspeedvalue(s);
    lastsrsym:=nil;
    {Check the cache.}
    entry:=(speedvalue and cachesize-1)+1;
    if (cache[entry]<>nil) and (cache[entry]^.speedvalue=speedvalue) and
     (cache[entry]^.name=s) then
        begin
            {Cache hit!}
            srsym:=cache[entry];
            srsymtable:=cachetables[entry];
            {$IFDEF STATISTICS}
            inc(hits);
            {$ENDIF STATISTICS}
        end
    else
        begin
            {Cache miss. :( }
            {$IFDEF STATISTICS}
            inc(misses);
            {$ENDIF STATISTICS}
            for i:=symtablestack.count-1 downto 0 do
                begin
                    srsymtable:=Psymtable(symtablestack.at(i));
                    srsym:=srsymtable^.speedsearch(s,speedvalue);
                    if srsym<>nil then
                        begin
                            {Found! Place it in the cache.}
                            cache[entry]:=srsym;
                            cachetables[entry]:=srsymtable;
                            exit;
                        end
                end;
            {Not found...}
            srsym:=nil;
            if notfounderror then
                begin
                    message1(sym_e_id_not_found,s);
                    srsym:=generrorsym;
                end;
        end;
end;

function Tsymtablestack.pop:Psymtable;

var r:Psymtable;

begin
    r:=symtablestack.at(symtablestack.count);
    decache(r);
    pop:=r;
    symtablestack.atdelete(symtablestack.count);
end;

procedure Tsymtablestack.push(s:Psymtable);

begin
    symtablestack.insert(s);
end;

procedure Tsymtablestack.insert(s:Psym;addtocache:boolean);

var pretop,sttop:Psymtable;
    hsym:Psym;
    entry:longint;

begin
    sttop:=Psymtable(symtablestack.at(symtablestack.count));
    pretop:=Psymtable(symtablestack.at(symtablestack.count-1));
    if typeof(sttop^)=typeof(Timplsymtable) then
        begin
            {There must also be an interface symtable...}
            if pretop^.speedsearch(s^.name,s^.speedvalue)<>nil then
                duplicatesym(s);
        end;
    {Check for duplicate field id in inherited classes.}
    if sttop^.is_object(typeof(Tobjectsymtable)) and
     (Pobjectsymtable(sttop)^.defowner<>nil) then
        begin
            {Even though the private symtable is disposed and set to nil
             after the unit has been compiled, we will still have to check
             for a private sym, because of interdependend units.}
            hsym:=Pobjectdef(Pobjectsymtable(sttop)^.defowner)^.
             speedsearch(s^.name,s^.speedvalue);
            if (hsym<>nil) and
             (hsym^.is_object(typeof(Tprocsym))
              and (sp_private in Pprocsym(hsym)^.objprop)) and
             (hsym^.is_object(typeof(Tvarsym))
              and (sp_private in Pvarsym(hsym)^.objprop)) then
                duplicatesym(hsym);
        end;
    entry:=(s^.speedvalue and cachesize-1)+1;
    if s^.is_object(typeof(Tenumsym)) and
     sttop^.is_object(Tabstractrecordsymtable)) then
        begin
            if pretop^.insert(s) and addtocache then
                begin
                    cache[entry]:=s;
                    cachetables[entry]:=pretop;
                end;
        end
    else
        begin
            if sttop^.insert(s) and addtocache then
                begin
                    cache[entry]:=s;
                    cachetables[entry]:=top;
                end;
        end;
end;

function Tsymtablestack.top:Psymtable;

begin
    top:=symtablestack.at(symtablestack.count);
end;

function Tsymtablestack.search_a_symtable(const symbol:stringid;symtabletype:pointer):Psym;

{Search for a symbol in a specified symbol table. Returns nil if
 the symtable is not found, and also if the symbol cannot be found
 in the desired symtable.}

var hsymtab:Psymtable;
    res:Psym;
    i:word;

begin
    res:=nil;
    for i:=symtablestack.count-1 downto 0 do
        if typeof((Psymtable(symtablestack.at(i))^))=symtabletype then
            begin
                {We found the desired symtable. Now check if the symbol we
                 search for is defined in it }
                res:=hsymtab^.search(symbol);
                break;
            end;
    search_a_symtable:=res;
end;

procedure Tsymtablestack.topfree;

begin
    decache(symtablestack.at(symtablestack.count));
    symtablestack.atfree(symtablestack.count);
end;

destructor Tsymtablestack.done;

begin
    symtablestack.done;
end;

{$IFDEF STATISTICS}

procedure exitprocedure;{$IFDEF TP}far;{$ENDIF}

begin
    writeln('Symbol cache statistics:');
    writeln('------------------------');
    writeln;
    writeln('Hits:             ',hits);
    writeln('Misses:           ',misses);
    writeln;
    writeln('Hit percentage:   ',(hits*100) div (hits+misses),'%');
    exitproc:=oldexit;
end;

begin
    hits:=0;
    misses:=0;
    oldexit:=exitproc;
    exitproc:=@exitprocedure;
{$ENDIF STATISTICS}
end.
