{
    Copyright (c) 1998-2002 by Daniel Mantione

    This unit contains support routines for the state tracker

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

unit nstate;

{$i fpcdefs.inc}

interface

uses    cclasses,node;

type    Tstate_entry=class(Tlinkedlistitem)
            what:Tnode;
            value:Tnode;
            constructor create(w,v:Tnode);
        end;

        Tstate_storage=class
            storage:Tlinkedlist;
            constructor create;
            procedure store_fact(w,v:Tnode);
            function find_fact(what:Tnode):Tnode;
            procedure delete_fact(what:Tnode);
        end;

var     aktstate:Tstate_storage;

implementation

constructor Tstate_entry.create(w,v:Tnode);

begin
    inherited create;
    what:=w;
    value:=v;
end;

constructor Tstate_storage.create;

begin
    storage:=Tlinkedlist.create;
end;

procedure Tstate_storage.store_fact(w,v:Tnode);

var se:Tstate_entry;

begin
{    writeln('fact:');
    writenode(w);
    writeln('=');
    writenode(v);}
    se:=Tstate_entry(storage.first);
    while assigned(se) do
        begin
            if se.what.isequal(w) then
                begin
                    storage.remove(se);
                    se.destroy;
                    break;
                end;
            se:=Tstate_entry(se.next);
        end;
    se:=Tstate_entry.create(w,v);
    storage.concat(se);
end;

function Tstate_storage.find_fact(what:Tnode):Tnode;

var se:Tstate_entry;

begin
    find_fact:=nil;
    se:=storage.first as Tstate_entry;
    while assigned(se) do
        begin
            if se.what.isequal(what) then
                begin
                    find_fact:=se.value;
                    break;
                end;
            se:=se.next as Tstate_entry;
        end;
end;

procedure Tstate_storage.delete_fact(what:Tnode);

var se:Tstate_entry;

begin
    se:=storage.first as Tstate_entry;
    while assigned(se) do
        begin
            if se.what.isequal(what) then
                begin
                    storage.remove(se);
                    se.destroy;
                    break;
                end;
            se:=se.next as Tstate_entry;
        end;
end;

end.
