{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

    This module provides some basic objects

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
unit cobjects;

{$i defines.inc}

interface

    uses
      cutils;

    const
       { the real size will be [-hasharray..hasharray] ! }
       hasharraysize = 2047;

    type
       pmemdebug = ^tmemdebug;
       tmemdebug = object
          constructor init(const s:string);
          destructor  done;
          procedure show;
       private
          startmem : longint;
          infostr  : string[40];
       end;

       { namedindexobect for use with dictionary and indexarray }
       Pnamedindexobject=^Tnamedindexobject;
       Tnamedindexobject=object
       { indexarray }
         indexnr    : longint;
         indexnext  : Pnamedindexobject;
       { dictionary }
         _name      : Pstring;
         _valuename : Pstring; { uppercase name }
         left,right : Pnamedindexobject;
         speedvalue : longint;
       { singlelist }
         listnext   : Pnamedindexobject;
         constructor init;
         constructor initname(const n:string);
         destructor  done;virtual;
         procedure setname(const n:string);virtual;
         function  name:string;virtual;
       end;

       Pdictionaryhasharray=^Tdictionaryhasharray;
       Tdictionaryhasharray=array[-hasharraysize..hasharraysize] of Pnamedindexobject;

       Tnamedindexcallback = procedure(p:Pnamedindexobject);

       Pdictionary=^Tdictionary;
       Tdictionary=object
         noclear   : boolean;
         replace_existing : boolean;
         delete_doubles : boolean;
         constructor init;
         destructor  done;virtual;
         procedure usehash;
         procedure clear;
         function delete(const s:string):Pnamedindexobject;
         function  empty:boolean;
         procedure foreach(proc2call:Tnamedindexcallback);
         function  insert(obj:Pnamedindexobject):Pnamedindexobject;
         function  rename(const olds,news : string):Pnamedindexobject;
         function  search(const s:string):Pnamedindexobject;
         function  speedsearch(const s:string;speedvalue:longint):Pnamedindexobject;
       private
         root      : Pnamedindexobject;
         hasharray : Pdictionaryhasharray;
         procedure cleartree(obj:Pnamedindexobject);
         function  insertnode(newnode:Pnamedindexobject;var currnode:Pnamedindexobject):Pnamedindexobject;
         procedure inserttree(currtree,currroot:Pnamedindexobject);
       end;

       psinglelist=^tsinglelist;
       tsinglelist=object
         first,
         last    : Pnamedindexobject;
         constructor init;
         destructor  done;
         procedure reset;
         procedure clear;
         procedure insert(p:Pnamedindexobject);
       end;

      tindexobjectarray=array[1..16000] of Pnamedindexobject;
      Pnamedindexobjectarray=^tindexobjectarray;

      pindexarray=^tindexarray;
      tindexarray=object
        noclear : boolean;
        first   : Pnamedindexobject;
        count   : longint;
        constructor init(Agrowsize:longint);
        destructor  done;
        procedure clear;
        procedure foreach(proc2call : Tnamedindexcallback);
        procedure deleteindex(p:Pnamedindexobject);
        procedure delete(var p:Pnamedindexobject);
        procedure insert(p:Pnamedindexobject);
        function  search(nr:longint):Pnamedindexobject;
      private
        growsize,
        size  : longint;
        data  : Pnamedindexobjectarray;
        procedure grow(gsize:longint);
      end;

{$ifdef fixLeaksOnError}
    PStackItem = ^TStackItem;
    TStackItem = record
      next: PStackItem;
      data: pointer;
    end;

    PStack = ^TStack;
    TStack = object
      constructor init;
      destructor done;
      procedure push(p: pointer);
      function pop: pointer;
      function top: pointer;
      function isEmpty: boolean;
     private
      head: PStackItem;
    end;
{$endif fixLeaksOnError}


implementation

{*****************************************************************************
                                    Memory debug
*****************************************************************************}

    constructor tmemdebug.init(const s:string);
      begin
        infostr:=s;
{$ifdef Delphi}
        startmem:=0;
{$else}
        startmem:=memavail;
{$endif Delphi}
      end;

    procedure tmemdebug.show;
{$ifndef Delphi}
      var
        l : longint;
{$endif}
      begin
{$ifndef Delphi}
        write('memory [',infostr,'] ');
        l:=memavail;
        if l>startmem then
         writeln(l-startmem,' released')
        else
         writeln(startmem-l,' allocated');
{$endif Delphi}
      end;

    destructor tmemdebug.done;
      begin
        show;
      end;


{*****************************************************************************
                                 Stack
*****************************************************************************}

{$ifdef fixLeaksOnError}
constructor TStack.init;
begin
  head := nil;
end;

procedure TStack.push(p: pointer);
var s: PStackItem;
begin
  new(s);
  s^.data := p;
  s^.next := head;
  head := s;
end;

function TStack.pop: pointer;
var s: PStackItem;
begin
  pop := top;
  if assigned(head) then
    begin
      s := head^.next;
      dispose(head);
      head := s;
    end
end;

function TStack.top: pointer;
begin
  if not isEmpty then
    top := head^.data
  else top := NIL;
end;

function TStack.isEmpty: boolean;
begin
  isEmpty := head = nil;
end;

destructor TStack.done;
var temp: PStackItem;
begin
  while head <> nil do
    begin
      temp := head^.next;
      dispose(head);
      head := temp;
    end;
end;
{$endif fixLeaksOnError}


{****************************************************************************
                               Tnamedindexobject
 ****************************************************************************}

constructor Tnamedindexobject.init;
begin
  { index }
  indexnr:=-1;
  indexnext:=nil;
  { dictionary }
  left:=nil;
  right:=nil;
  _name:=nil;
  speedvalue:=-1;
  { list }
  listnext:=nil;
end;

constructor Tnamedindexobject.initname(const n:string);
begin
  { index }
  indexnr:=-1;
  indexnext:=nil;
  { dictionary }
  left:=nil;
  right:=nil;
  speedvalue:=-1;
  _name:=stringdup(n);
  { list }
  listnext:=nil;
end;

destructor Tnamedindexobject.done;
begin
  stringdispose(_name);
end;

procedure Tnamedindexobject.setname(const n:string);
begin
  if speedvalue=-1 then
   begin
     if assigned(_name) then
       stringdispose(_name);
     _name:=stringdup(n);
   end;
end;

function Tnamedindexobject.name:string;
begin
  if assigned(_name) then
   name:=_name^
  else
   name:='';
end;


{****************************************************************************
                               TDICTIONARY
****************************************************************************}

    constructor Tdictionary.init;
      begin
        root:=nil;
        hasharray:=nil;
        noclear:=false;
        replace_existing:=false;
        delete_doubles:=false;
      end;


    procedure Tdictionary.usehash;
      begin
        if not(assigned(root)) and
           not(assigned(hasharray)) then
         begin
           new(hasharray);
           fillchar(hasharray^,sizeof(hasharray^),0);
         end;
      end;


    destructor Tdictionary.done;
      begin
        if not noclear then
         clear;
        if assigned(hasharray) then
         dispose(hasharray);
      end;


    procedure Tdictionary.cleartree(obj:Pnamedindexobject);
      begin
        if assigned(obj^.left) then
          cleartree(obj^.left);
        if assigned(obj^.right) then
          cleartree(obj^.right);
        dispose(obj,done);
        obj:=nil;
      end;


    procedure Tdictionary.clear;
      var
        w : longint;
      begin
        if assigned(root) then
          cleartree(root);
        if assigned(hasharray) then
         for w:=-hasharraysize to hasharraysize do
          if assigned(hasharray^[w]) then
           cleartree(hasharray^[w]);
      end;

    function Tdictionary.delete(const s:string):Pnamedindexobject;

    var p,speedvalue:longint;
        n:Pnamedindexobject;

        procedure insert_right_bottom(var root,Atree:Pnamedindexobject);

        begin
            while root^.right<>nil do
                root:=root^.right;
            root^.right:=Atree;
        end;

        function delete_from_tree(root:Pnamedindexobject):Pnamedindexobject;

        type    leftright=(left,right);

        var lr:leftright;
            oldroot:Pnamedindexobject;

        begin
            oldroot:=nil;
            while (root<>nil) and (root^.speedvalue<>speedvalue) do
                begin
                    oldroot:=root;
                    if speedvalue<root^.speedvalue then
                        begin
                            root:=root^.right;
                            lr:=right;
                        end
                    else
                        begin
                            root:=root^.left;
                            lr:=left;
                        end;
                end;
            while (root<>nil) and (root^._name^<>s) do
                begin
                    oldroot:=root;
                    if s<root^._name^ then
                        begin
                            root:=root^.right;
                            lr:=right;
                        end
                    else
                        begin
                            root:=root^.left;
                            lr:=left;
                        end;
                end;
            if root^.left<>nil then
                begin
                    {Now the node pointing to root must point to the left
                     subtree of root. The right subtree of root must be
                     connected to the right bottom of the left subtree.}
                    if lr=left then
                        oldroot^.left:=root^.left
                    else
                        oldroot^.right:=root^.left;
                    if root^.right<>nil then
                        insert_right_bottom(root^.left,root^.right);
                end
            else
                {There is no left subtree. So we can just replace the node to
                 delete with the right subtree.}
                if lr=left then
                    oldroot^.left:=root^.right
                else
                    oldroot^.right:=root^.right;
            delete_from_tree:=root;
        end;

    begin
        speedvalue:=getspeedvalue(s);
        n:=root;
        if assigned(hasharray) then
            begin
                {First, check if the node to delete directly located under
                 the hasharray.}
                p:=speedvalue mod hasharraysize;
                n:=hasharray^[p];
                if (n<>nil) and (n^.speedvalue=speedvalue) and
                 (n^._name^=s) then
                    begin
                        {The node to delete is directly located under the
                         hasharray. Make the hasharray point to the left
                         subtree of the node and place the right subtree on
                         the right-bottom of the left subtree.}
                        if n^.left<>nil then
                            begin
                                hasharray^[p]:=n^.left;
                                if n^.right<>nil then
                                    insert_right_bottom(n^.left,n^.right);
                            end
                        else
                            hasharray^[p]:=n^.right;
                        delete:=n;
                        exit;
                    end;
            end
        else
            begin
                {First check if the node to delete is the root.}
                if (root<>nil) and (n^.speedvalue=speedvalue)
                 and (n^._name^=s) then
                    begin
                        if n^.left<>nil then
                            begin
                                root:=n^.left;
                                if n^.right<>nil then
                                    insert_right_bottom(n^.left,n^.right);
                            end
                        else
                            root:=n^.right;
                        delete:=n;
                        exit;
                    end;
            end;
        delete:=delete_from_tree(n);
    end;

    function Tdictionary.empty:boolean;
      var
        w : longint;
      begin
        if assigned(hasharray) then
         begin
           empty:=false;
           for w:=-hasharraysize to hasharraysize do
            if assigned(hasharray^[w]) then
             exit;
           empty:=true;
         end
        else
         empty:=(root=nil);
      end;


    procedure Tdictionary.foreach(proc2call:Tnamedindexcallback);

        procedure a(p:Pnamedindexobject);
        begin
          proc2call(p);
          if assigned(p^.left) then
           a(p^.left);
          if assigned(p^.right) then
           a(p^.right);
        end;

      var
        i : longint;
      begin
        if assigned(hasharray) then
         begin
           for i:=-hasharraysize to hasharraysize do
            if assigned(hasharray^[i]) then
             a(hasharray^[i]);
         end
        else
         if assigned(root) then
          a(root);
      end;


    function Tdictionary.insert(obj:Pnamedindexobject):Pnamedindexobject;
      begin
        obj^.speedvalue:=getspeedvalue(obj^._name^);
        if assigned(hasharray) then
         insert:=insertnode(obj,hasharray^[obj^.speedvalue mod hasharraysize])
        else
         insert:=insertnode(obj,root);
      end;


    function tdictionary.insertnode(newnode:Pnamedindexobject;var currnode:Pnamedindexobject):Pnamedindexobject;
      begin
        if currnode=nil then
         begin
           currnode:=newnode;
           insertnode:=newnode;
         end
        { first check speedvalue, to allow a fast insert }
        else
         if currnode^.speedvalue>newnode^.speedvalue then
          insertnode:=insertnode(newnode,currnode^.right)
        else
         if currnode^.speedvalue<newnode^.speedvalue then
          insertnode:=insertnode(newnode,currnode^.left)
        else
         begin
           if currnode^._name^>newnode^._name^ then
            insertnode:=insertnode(newnode,currnode^.right)
           else
            if currnode^._name^<newnode^._name^ then
             insertnode:=insertnode(newnode,currnode^.left)
           else
            begin
              if replace_existing and
                 assigned(currnode) then
                begin
                  newnode^.left:=currnode^.left;
                  newnode^.right:=currnode^.right;
                  if delete_doubles then
                    begin
                      currnode^.left:=nil;
                      currnode^.right:=nil;
                      dispose(currnode,done);
                    end;
                  currnode:=newnode;
                  insertnode:=newnode;
                end
              else
               begin
                 insertnode:=currnode;
                 dispose(newnode,done);
               end;
             end;
         end;
      end;


    procedure tdictionary.inserttree(currtree,currroot:Pnamedindexobject);
      begin
        if assigned(currtree) then
         begin
           inserttree(currtree^.left,currroot);
           inserttree(currtree^.right,currroot);
           currtree^.right:=nil;
           currtree^.left:=nil;
           insertnode(currtree,currroot);
         end;
      end;


    function tdictionary.rename(const olds,news : string):Pnamedindexobject;
      var
        spdval : longint;
        lasthp,
        hp,hp2,hp3 : Pnamedindexobject;
      begin
        spdval:=getspeedvalue(olds);
        if assigned(hasharray) then
         hp:=hasharray^[spdval mod hasharraysize]
        else
         hp:=root;
        lasthp:=nil;
        while assigned(hp) do
          begin
            if spdval>hp^.speedvalue then
             begin
               lasthp:=hp;
               hp:=hp^.left
             end
            else
             if spdval<hp^.speedvalue then
              begin
                lasthp:=hp;
                hp:=hp^.right
              end
            else
             begin
               if (hp^.name=olds) then
                begin
                  { get in hp2 the replacer for the root or hasharr }
                  hp2:=hp^.left;
                  hp3:=hp^.right;
                  if not assigned(hp2) then
                   begin
                     hp2:=hp^.right;
                     hp3:=hp^.left;
                   end;
                  { remove entry from the tree }
                  if assigned(lasthp) then
                   begin
                     if lasthp^.left=hp then
                      lasthp^.left:=hp2
                     else
                      lasthp^.right:=hp2;
                   end
                  else
                   begin
                     if assigned(hasharray) then
                      hasharray^[spdval mod hasharraysize]:=hp2
                     else
                      root:=hp2;
                   end;
                  { reinsert the hp3 in the tree from hp2 }
                  inserttree(hp3,hp2);
                  { reset node with new values }
                  stringdispose(hp^._name);
                  hp^._name:=stringdup(news);
                  hp^.speedvalue:=getspeedvalue(news);
                  hp^.left:=nil;
                  hp^.right:=nil;
                  { reinsert }
                  if assigned(hasharray) then
                   rename:=insertnode(hp,hasharray^[hp^.speedvalue mod hasharraysize])
                  else
                   rename:=insertnode(hp,root);
                  exit;
                end
               else
                if olds>hp^.name then
                 begin
                   lasthp:=hp;
                   hp:=hp^.left
                 end
                else
                 begin
                   lasthp:=hp;
                   hp:=hp^.right;
                 end;
             end;
          end;
      end;


    function Tdictionary.search(const s:string):Pnamedindexobject;
      begin
        search:=speedsearch(s,getspeedvalue(s));
      end;


    function Tdictionary.speedsearch(const s:string;speedvalue:longint):Pnamedindexobject;
      var
        newnode:Pnamedindexobject;
      begin
        if assigned(hasharray) then
         newnode:=hasharray^[speedvalue mod hasharraysize]
        else
         newnode:=root;
        while assigned(newnode) do
         begin
           if speedvalue>newnode^.speedvalue then
            newnode:=newnode^.left
           else
            if speedvalue<newnode^.speedvalue then
             newnode:=newnode^.right
           else
            begin
              if (newnode^._name^=s) then
               begin
                 speedsearch:=newnode;
                 exit;
               end
              else
               if s>newnode^._name^ then
                newnode:=newnode^.left
              else
               newnode:=newnode^.right;
            end;
         end;
        speedsearch:=nil;
      end;


{****************************************************************************
                               tsinglelist
 ****************************************************************************}

    constructor tsinglelist.init;
      begin
        first:=nil;
        last:=nil;
      end;


    destructor tsinglelist.done;
      begin
      end;


    procedure tsinglelist.reset;
      begin
        first:=nil;
        last:=nil;
      end;


    procedure tsinglelist.clear;
      var
        hp,hp2 : pnamedindexobject;
      begin
        hp:=first;
        while assigned(hp) do
         begin
           hp2:=hp;
           hp:=hp^.listnext;
           dispose(hp2,done);
         end;
        first:=nil;
        last:=nil;
      end;


    procedure tsinglelist.insert(p:Pnamedindexobject);
      begin
        if not assigned(first) then
         first:=p
        else
         last^.listnext:=p;
        last:=p;
        p^.listnext:=nil;
      end;


{****************************************************************************
                               tindexarray
 ****************************************************************************}

    constructor tindexarray.init(Agrowsize:longint);
      begin
        growsize:=Agrowsize;
        size:=0;
        count:=0;
        data:=nil;
        first:=nil;
        noclear:=false;
      end;

    destructor tindexarray.done;
      begin
        if assigned(data) then
          begin
             if not noclear then
              clear;
             freemem(data,size*4);
             data:=nil;
          end;
      end;

    function tindexarray.search(nr:longint):Pnamedindexobject;
      begin
        if nr<=count then
         search:=data^[nr]
        else
         search:=nil;
      end;


    procedure tindexarray.clear;
      var
        i : longint;
      begin
        for i:=1 to count do
         if assigned(data^[i]) then
          begin
            dispose(data^[i],done);
            data^[i]:=nil;
          end;
        count:=0;
        first:=nil;
      end;


    procedure tindexarray.foreach(proc2call : Tnamedindexcallback);
      var
        i : longint;
      begin
        for i:=1 to count do
         if assigned(data^[i]) then
          proc2call(data^[i]);
      end;


    procedure tindexarray.grow(gsize:longint);
      var
        osize : longint;
      begin
        osize:=size;
        inc(size,gsize);
        reallocmem(data,size*4);
        fillchar(data^[osize+1],gsize*4,0);
      end;


    procedure tindexarray.deleteindex(p:Pnamedindexobject);
      var
        i : longint;
      begin
        i:=p^.indexnr;
        { update counter }
        if i=count then
         dec(count);
        { update linked list }
        while (i>0) do
         begin
           dec(i);
           if (i>0) and assigned(data^[i]) then
            begin
              data^[i]^.indexnext:=data^[p^.indexnr]^.indexnext;
              break;
            end;
         end;
        if i=0 then
         first:=p^.indexnext;
        data^[p^.indexnr]:=nil;
        { clear entry }
        p^.indexnr:=-1;
        p^.indexnext:=nil;
      end;


    procedure tindexarray.delete(var p:Pnamedindexobject);
      begin
        deleteindex(p);
        dispose(p,done);
        p:=nil;
      end;


    procedure tindexarray.insert(p:Pnamedindexobject);
      var
        i  : longint;
      begin
        if p^.indexnr=-1 then
         begin
           inc(count);
           p^.indexnr:=count;
         end;
        if p^.indexnr>count then
         count:=p^.indexnr;
        if count>size then
         grow(((count div growsize)+1)*growsize);
        {$ifdef Delphi}
        Assert(not assigned(data^[p^.indexnr]) or (p=data^[p^.indexnr]));
        {$endif}
        data^[p^.indexnr]:=p;
        { update linked list backward }
        i:=p^.indexnr;
        while (i>0) do
         begin
           dec(i);
           if (i>0) and assigned(data^[i]) then
            begin
              data^[i]^.indexnext:=p;
              break;
            end;
         end;
        if i=0 then
         first:=p;
        { update linked list forward }
        i:=p^.indexnr;
        while (i<=count) do
         begin
           inc(i);
           if (i<=count) and assigned(data^[i]) then
            begin
              p^.indexnext:=data^[i];
              exit;
            end;
         end;
        if i>count then
         p^.indexnext:=nil;
      end;

end.
{
  $Log$
  Revision 1.23  2001-03-25 12:28:22  peter
    * memleak fixes (merged)

  Revision 1.22  2000/12/25 00:07:25  peter
    + new tlinkedlist class (merge of old tstringqueue,tcontainer and
      tlinkedlist objects)

  Revision 1.21  2000/12/24 12:25:31  peter
    + cstreams unit
    * dynamicarray object to class

  Revision 1.19  2000/11/12 22:20:37  peter
    * create generic toutputsection for binary writers

  Revision 1.18  2000/11/04 14:25:19  florian
    + merged Attila's changes for interfaces, not tested yet

  Revision 1.17  2000/11/03 19:41:06  jonas
    * fixed bug in tdynamicarray.align (merged)

  Revision 1.16  2000/10/31 22:02:46  peter
    * symtable splitted, no real code changes

  Revision 1.15  2000/10/14 10:14:46  peter
    * moehrendorf oct 2000 rewrite

  Revision 1.14  2000/09/24 21:19:50  peter
    * delphi compile fixes

  Revision 1.13  2000/09/24 15:06:12  peter
    * use defines.inc

  Revision 1.12  2000/08/27 20:19:38  peter
    * store strings with case in ppu, when an internal symbol is created
      a '$' is prefixed so it's not automatic uppercased

  Revision 1.11  2000/08/27 16:11:50  peter
    * moved some util functions from globals,cobjects to cutils
    * splitted files into finput,fmodule

  Revision 1.10  2000/08/19 18:44:27  peter
    * new tdynamicarray implementation using blocks instead of
      reallocmem (merged)

  Revision 1.9  2000/08/16 18:33:53  peter
    * splitted namedobjectitem.next into indexnext and listnext so it
      can be used in both lists
    * don't allow "word = word" type definitions (merged)

  Revision 1.8  2000/08/13 08:41:57  peter
    * fixed typo in tsinglelist.clear (merged)

  Revision 1.7  2000/08/12 15:34:22  peter
    + usedasmsymbollist to check and reset only the used symbols (merged)

  Revision 1.6  2000/08/10 12:20:44  jonas
    * reallocmem is now also used under Delphi (merged from fixes branch)

  Revision 1.5  2000/08/09 12:09:45  jonas
    * tidexarray and tdynamicarray now use reallocmem() under FPC for
      growing (merged from fixes branch)

  Revision 1.4  2000/08/06 19:42:40  peter
    * removed note

  Revision 1.3  2000/08/02 19:49:58  peter
    * first things for default parameters

  Revision 1.2  2000/07/13 11:32:38  michael
  + removed logs

}
