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

{$ifdef tp}
  {$E+,N+,D+,F+}
{$endif}
{$I-}
{$R-}{ necessary for crc calculation and dynamicblock acessing }

{$ifdef fpc}
{$define USEREALLOCMEM}
{$endif fpc}

{$ifdef delphi}
{$define USEREALLOCMEM}
{$endif delphi}

unit cobjects;

  interface

    uses
      cutils;

    const
       { the real size will be [-hasharray..hasharray] ! }
{$ifdef TP}
       hasharraysize = 127;
{$else}
       hasharraysize = 2047;
{$endif}

    type
       pfileposinfo = ^tfileposinfo;
       tfileposinfo = record
         line      : longint;
         column    : word;
         fileindex : word;
       end;

       pmemdebug = ^tmemdebug;
       tmemdebug = object
          constructor init(const s:string);
          destructor  done;
          procedure show;
       private
          startmem : longint;
          infostr  : string[40];
       end;

       plinkedlist_item = ^tlinkedlist_item;
       tlinkedlist_item = object
          next,previous : plinkedlist_item;
          { does nothing }
          constructor init;
          destructor done;virtual;
          function getcopy:plinkedlist_item;virtual;
       end;

       pstring_item = ^tstring_item;
       tstring_item = object(tlinkedlist_item)
          str : pstring;
          constructor init(const s : string);
          destructor done;virtual;
       end;


       { this implements a double linked list }
       plinkedlist = ^tlinkedlist;
       tlinkedlist = object
          first,last : plinkedlist_item;
          constructor init;
          destructor done;

          { disposes the items of the list }
          procedure clear;

          { concats a new item at the end }
          procedure concat(p : plinkedlist_item);

          { inserts a new item at the begin }
          procedure insert(p : plinkedlist_item);

          { inserts another list at the begin and make this list empty }
          procedure insertlist(p : plinkedlist);

          { concats another list at the end and make this list empty }
          procedure concatlist(p : plinkedlist);

          procedure concatlistcopy(p : plinkedlist);

          { removes p from the list (p isn't disposed) }
          { it's not tested if p is in the list !      }
          procedure remove(p : plinkedlist_item);

          { is the linkedlist empty ? }
          function  empty:boolean;

          { items in the list }
          function  count:longint;
       end;

       { some help data types }
       pstringqueueitem = ^tstringqueueitem;
       tstringqueueitem = object
          data : pstring;
          next : pstringqueueitem;
       end;

       { String Queue}
       PStringQueue=^TStringQueue;
       TStringQueue=object
         first,last : PStringqueueItem;
         constructor Init;
         destructor Done;
         function Empty:boolean;
         function Get:string;
         function Find(const s:string):PStringqueueItem;
         function Delete(const s:string):boolean;
         procedure Insert(const s:string);
         procedure Concat(const s:string);
         procedure Clear;
       end;

       { containeritem }
       pcontaineritem = ^tcontaineritem;
       tcontaineritem = object
          next : pcontaineritem;
          constructor init;
          destructor  done;virtual;
       end;

       { container }
       pcontainer = ^tcontainer;
       tcontainer = object
          root,
          last    : pcontaineritem;
          constructor init;
          destructor  done;
          { true when the container is empty }
          function  empty:boolean;
          { amount of strings in the container }
          function  count:longint;
          { inserts a string }
          procedure insert(item:pcontaineritem);
          { gets a string }
          function  get:pcontaineritem;
          { deletes all items }
          procedure clear;
       end;

       { containeritem }
       pstringcontaineritem = ^tstringcontaineritem;
       tstringcontaineritem = object(tcontaineritem)
          data : pstring;
          file_info : tfileposinfo;
          constructor init(const s:string);
          constructor Init_TokenInfo(const s:string;const pos:tfileposinfo);
          destructor  done;virtual;
       end;

       { string container }
       pstringcontainer = ^tstringcontainer;
       tstringcontainer = object(tcontainer)
          doubles : boolean;  { if this is set to true, doubles are allowed }
          constructor init;
          constructor init_no_double;
          procedure insert(const s : string);
          procedure insert_with_tokeninfo(const s : string;const file_info : tfileposinfo);
          { gets a string }
          function get : string;
          function get_with_tokeninfo(var file_info : tfileposinfo) : string;
          { true if string is in the container }
          function find(const s:string):boolean;
       end;


       { namedindexobject for use with dictionary and indexarray }
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
         noclear : boolean;
         first,
         last    : Pnamedindexobject;
         constructor init;
         destructor  done;
         procedure clear;
         procedure insert(p:Pnamedindexobject);
       end;

     const
       dynamicblockbasesize = 12;

     type
       pdynamicblock = ^tdynamicblock;
       tdynamicblock = record
         pos,
         used : longint;
         next : pdynamicblock;
         data : array[0..1] of byte;
       end;

       pdynamicarray = ^tdynamicarray;
       tdynamicarray = object
         blocksize  : longint;
         firstblock,
         lastblock  : pdynamicblock;
         constructor init(Ablocksize:longint);
         destructor  done;
         function  size:longint;
         procedure align(i:longint);
         procedure seek(i:longint);
         procedure write(var d;len:longint);
         function  read(var d;len:longint):longint;
         procedure blockwrite(var f:file);
       private
         posn      : longint;
         posnblock : pdynamicblock;
         procedure grow;
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
        procedure delete(p:Pnamedindexobject);
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
      var
        l : longint;
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
                                  TStringQueue
****************************************************************************}

constructor TStringQueue.Init;
begin
  first:=nil;
  last:=nil;
end;


function TStringQueue.Empty:boolean;
begin
  Empty:=(first=nil);
end;


function TStringQueue.Get:string;
var
  newnode : pstringqueueitem;
begin
  if first=nil then
   begin
     Get:='';
     exit;
   end;
  Get:=first^.data^;
  stringdispose(first^.data);
  newnode:=first;
  first:=first^.next;
  dispose(newnode);
end;


function TStringQueue.Find(const s:string):PStringqueueItem;
var
  p : PStringqueueItem;
begin
  p:=first;
  while assigned(p) do
   begin
     if p^.data^=s then
      break;
     p:=p^.next;
   end;
  Find:=p;
end;


function TStringQueue.Delete(const s:string):boolean;
var
  prev,p : PStringqueueItem;
begin
  Delete:=false;
  prev:=nil;
  p:=first;
  while assigned(p) do
   begin
     if p^.data^=s then
      begin
        if p=last then
          last:=prev;
        if assigned(prev) then
         prev^.next:=p^.next
        else
         first:=p^.next;
        dispose(p);
        Delete:=true;
        exit;
      end;
     prev:=p;
     p:=p^.next;
   end;
end;


procedure TStringQueue.Insert(const s:string);
var
  newnode : pstringqueueitem;
begin
  new(newnode);
  newnode^.next:=first;
  newnode^.data:=stringdup(s);
  first:=newnode;
  if last=nil then
   last:=newnode;
end;


procedure TStringQueue.Concat(const s:string);
var
  newnode : pstringqueueitem;
begin
  new(newnode);
  newnode^.next:=nil;
  newnode^.data:=stringdup(s);
  if first=nil then
   first:=newnode
  else
   last^.next:=newnode;
  last:=newnode;
end;


procedure TStringQueue.Clear;
var
  newnode : pstringqueueitem;
begin
  while (first<>nil) do
   begin
     newnode:=first;
     stringdispose(first^.data);
     first:=first^.next;
     dispose(newnode);
   end;
  last:=nil;
end;


destructor TStringQueue.Done;
begin
  Clear;
end;


{****************************************************************************
                                TContainerItem
 ****************************************************************************}

constructor TContainerItem.Init;
begin
end;


destructor TContainerItem.Done;
begin
end;


{****************************************************************************
                             TStringContainerItem
 ****************************************************************************}

constructor TStringContainerItem.Init(const s:string);
begin
  inherited Init;
  data:=stringdup(s);
  file_info.fileindex:=0;
  file_info.line:=0;
  file_info.column:=0;
end;


constructor TStringContainerItem.Init_TokenInfo(const s:string;const pos:tfileposinfo);
begin
  inherited Init;
  data:=stringdup(s);
  file_info:=pos;
end;


destructor TStringContainerItem.Done;
begin
  stringdispose(data);
end;



{****************************************************************************
                                   TCONTAINER
 ****************************************************************************}

    constructor tcontainer.init;
      begin
         root:=nil;
         last:=nil;
      end;


    destructor tcontainer.done;
      begin
         clear;
      end;


    function tcontainer.empty:boolean;
      begin
        empty:=(root=nil);
      end;


    function tcontainer.count:longint;
      var
        i : longint;
        p : pcontaineritem;
      begin
        i:=0;
        p:=root;
        while assigned(p) do
         begin
           p:=p^.next;
           inc(i);
         end;
        count:=i;
      end;


    procedure tcontainer.insert(item:pcontaineritem);
      begin
         item^.next:=nil;
         if root=nil then
          root:=item
         else
          last^.next:=item;
         last:=item;
      end;


    procedure tcontainer.clear;
      var
         newnode : pcontaineritem;
      begin
         newnode:=root;
         while assigned(newnode) do
           begin
              root:=newnode^.next;
              dispose(newnode,done);
              newnode:=root;
           end;
         last:=nil;
         root:=nil;
      end;


    function tcontainer.get:pcontaineritem;
      begin
         if root=nil then
          get:=nil
         else
          begin
            get:=root;
            root:=root^.next;
          end;
      end;


{****************************************************************************
                           TSTRINGCONTAINER
 ****************************************************************************}

    constructor tstringcontainer.init;
      begin
         inherited init;
         doubles:=true;
      end;


    constructor tstringcontainer.init_no_double;
      begin
         inherited init;
         doubles:=false;
      end;


    procedure tstringcontainer.insert(const s : string);
      var
        newnode : pstringcontaineritem;
      begin
         if (s='') or
            ((not doubles) and find(s)) then
          exit;
         new(newnode,init(s));
         inherited insert(newnode);
      end;


    procedure tstringcontainer.insert_with_tokeninfo(const s : string; const file_info : tfileposinfo);
      var
        newnode : pstringcontaineritem;
      begin
         if (not doubles) and find(s) then
          exit;
         new(newnode,init_tokeninfo(s,file_info));
         inherited insert(newnode);
      end;


    function tstringcontainer.get : string;
      var
         p : pstringcontaineritem;
      begin
         p:=pstringcontaineritem(inherited get);
         if p=nil then
          get:=''
         else
          begin
            get:=p^.data^;
            dispose(p,done);
          end;
      end;


    function tstringcontainer.get_with_tokeninfo(var file_info : tfileposinfo) : string;
      var
         p : pstringcontaineritem;
      begin
         p:=pstringcontaineritem(inherited get);
         if p=nil then
          begin
            get_with_tokeninfo:='';
            file_info.fileindex:=0;
            file_info.line:=0;
            file_info.column:=0;
          end
         else
          begin
            get_with_tokeninfo:=p^.data^;
            file_info:=p^.file_info;
            dispose(p,done);
          end;
      end;


    function tstringcontainer.find(const s:string):boolean;
      var
        newnode : pstringcontaineritem;
      begin
        find:=false;
        newnode:=pstringcontaineritem(root);
        while assigned(newnode) do
         begin
           if newnode^.data^=s then
            begin
              find:=true;
              exit;
            end;
           newnode:=pstringcontaineritem(newnode^.next);
         end;
      end;


{****************************************************************************
                            TLINKEDLIST_ITEM
 ****************************************************************************}

    constructor tlinkedlist_item.init;
      begin
        previous:=nil;
        next:=nil;
      end;


    destructor tlinkedlist_item.done;
      begin
      end;


    function tlinkedlist_item.getcopy:plinkedlist_item;
      var
        l : longint;
        p : plinkedlist_item;
      begin
        l:=sizeof(self);
        getmem(p,l);
        move(self,p^,l);
        getcopy:=p;
      end;


{****************************************************************************
                            TSTRING_ITEM
 ****************************************************************************}

    constructor tstring_item.init(const s : string);
      begin
         str:=stringdup(s);
      end;


    destructor tstring_item.done;
      begin
         stringdispose(str);
         inherited done;
      end;


{****************************************************************************
                               TLINKEDLIST
 ****************************************************************************}

    constructor tlinkedlist.init;
      begin
         first:=nil;
         last:=nil;
      end;


    destructor tlinkedlist.done;
      begin
         clear;
      end;


    procedure tlinkedlist.clear;
      var
         newnode : plinkedlist_item;
      begin
         newnode:=first;
         while assigned(newnode) do
           begin
              first:=newnode^.next;
              dispose(newnode,done);
              newnode:=first;
           end;
      end;


    procedure tlinkedlist.insertlist(p : plinkedlist);
      begin
         { empty list ? }
         if not(assigned(p^.first)) then
           exit;

         p^.last^.next:=first;

         { we have a double linked list }
         if assigned(first) then
           first^.previous:=p^.last;

         first:=p^.first;

         if not(assigned(last)) then
           last:=p^.last;

         { p becomes empty }
         p^.first:=nil;
         p^.last:=nil;
      end;


    procedure tlinkedlist.concat(p : plinkedlist_item);
      begin
        if not(assigned(first)) then
         begin
           first:=p;
           p^.previous:=nil;
           p^.next:=nil;
         end
        else
         begin
           last^.next:=p;
           p^.previous:=last;
           p^.next:=nil;
         end;
        last:=p;
      end;


    procedure tlinkedlist.insert(p : plinkedlist_item);
      begin
         if not(assigned(first)) then
          begin
            last:=p;
            p^.previous:=nil;
            p^.next:=nil;
          end
         else
          begin
            first^.previous:=p;
            p^.previous:=nil;
            p^.next:=first;
          end;
         first:=p;
      end;


    procedure tlinkedlist.remove(p : plinkedlist_item);
      begin
         if not(assigned(p)) then
           exit;
         if (first=p) and (last=p) then
           begin
              first:=nil;
              last:=nil;
           end
         else if first=p then
           begin
              first:=p^.next;
              if assigned(first) then
                first^.previous:=nil;
           end
         else if last=p then
           begin
              last:=last^.previous;
              if assigned(last) then
                last^.next:=nil;
           end
         else
           begin
              p^.previous^.next:=p^.next;
              p^.next^.previous:=p^.previous;
           end;
         p^.next:=nil;
         p^.previous:=nil;
      end;


    procedure tlinkedlist.concatlist(p : plinkedlist);
     begin
         if not(assigned(p^.first)) then
           exit;

         if not(assigned(first)) then
           first:=p^.first
           else
             begin
                last^.next:=p^.first;
                p^.first^.previous:=last;
             end;

         last:=p^.last;

         { make p empty }
         p^.last:=nil;
         p^.first:=nil;
      end;


    procedure tlinkedlist.concatlistcopy(p : plinkedlist);
      var
        newnode,newnode2 : plinkedlist_item;
      begin
         newnode:=p^.first;
         while assigned(newnode) do
          begin
            newnode2:=newnode^.getcopy;
            if assigned(newnode2) then
             begin
               if not(assigned(first)) then
                begin
                  first:=newnode2;
                  newnode2^.previous:=nil;
                  newnode2^.next:=nil;
                end
               else
                begin
                  last^.next:=newnode2;
                  newnode2^.previous:=last;
                  newnode2^.next:=nil;
                end;
               last:=newnode2;
             end;
            newnode:=newnode^.next;
          end;
      end;


    function tlinkedlist.empty:boolean;
      begin
        empty:=(first=nil);
      end;


    function tlinkedlist.count:longint;
      var
        i : longint;
        hp : plinkedlist_item;
      begin
        hp:=first;
        i:=0;
        while assigned(hp) do
         begin
           inc(i);
           hp:=hp^.next;
         end;
        count:=i;
      end;


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
                  currnode:=newnode;
                  insertnode:=newnode;
                end
              else
               insertnode:=currnode;
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
        noclear:=false;
      end;


    destructor tsinglelist.done;
      begin
        if not noclear then
         clear;
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
                                tdynamicarray
****************************************************************************}

    constructor tdynamicarray.init(Ablocksize:longint);
      begin
        posn:=0;
        posnblock:=nil;
        firstblock:=nil;
        lastblock:=nil;
        blocksize:=Ablocksize;
        grow;
      end;


    function  tdynamicarray.size:longint;
      begin
        if assigned(lastblock) then
         size:=lastblock^.pos+lastblock^.used
        else
         size:=0;
      end;


    procedure tdynamicarray.grow;
      var
        nblock : pdynamicblock;
      begin
        getmem(nblock,blocksize+dynamicblockbasesize);
        if not assigned(firstblock) then
         begin
           firstblock:=nblock;
           posnblock:=nblock;
           nblock^.pos:=0;
         end
        else
         begin
           lastblock^.next:=nblock;
           nblock^.pos:=lastblock^.pos+lastblock^.used;
         end;
        nblock^.used:=0;
        nblock^.next:=nil;
        fillchar(nblock^.data,blocksize,0);
        lastblock:=nblock;
      end;


    procedure tdynamicarray.align(i:longint);
      var
        j : longint;
      begin
        j:=(posn mod i);
        if j<>0 then
         begin
           j:=i-j;
           if posnblock^.used+j>blocksize then
            begin
              posnblock^.used:=blocksize;
              dec(j,blocksize-posnblock^.used);
              grow;
              posnblock:=lastblock;
            end;
           inc(posnblock^.used,j);
           inc(posn,j);
         end;
      end;


    procedure tdynamicarray.seek(i:longint);
      begin
        if (i<posnblock^.pos) or (i>posnblock^.pos+blocksize) then
         begin
           { set posnblock correct if the size is bigger then
             the current block }
           if posnblock^.pos>i then
            posnblock:=firstblock;
           while assigned(posnblock) do
            begin
              if posnblock^.pos+blocksize>i then
               break;
              posnblock:=posnblock^.next;
            end;
           { not found ? then increase blocks }
           if not assigned(posnblock) then
            begin
              { the current lastblock is now also fully used }
              lastblock^.used:=blocksize;
              repeat
                grow;
                posnblock:=lastblock;
              until posnblock^.pos+blocksize>=i;
            end;
         end;
        posn:=i;
        if posn mod blocksize>posnblock^.used then
         posnblock^.used:=posn mod blocksize;
      end;


    procedure tdynamicarray.write(var d;len:longint);
      var
        p : pchar;
        i,j : longint;
      begin
        p:=pchar(@d);
        while (len>0) do
         begin
           i:=posn mod blocksize;
           if i+len>=blocksize then
            begin
              j:=blocksize-i;
              move(p^,posnblock^.data[i],j);
              inc(p,j);
              inc(posn,j);
              dec(len,j);
              posnblock^.used:=blocksize;
              if assigned(posnblock^.next) then
               posnblock:=posnblock^.next
              else
               begin
                 grow;
                 posnblock:=lastblock;
               end;
            end
           else
            begin
              move(p^,posnblock^.data[i],len);
              inc(p,len);
              inc(posn,len);
              i:=posn mod blocksize;
              if i>posnblock^.used then
               posnblock^.used:=i;
              len:=0;
            end;
         end;
      end;


    function tdynamicarray.read(var d;len:longint):longint;
      var
        p : pchar;
        i,j,res : longint;
      begin
        res:=0;
        p:=pchar(@d);
        while (len>0) do
         begin
           i:=posn mod blocksize;
           if i+len>=posnblock^.used then
            begin
              j:=posnblock^.used-i;
              move(posnblock^.data[i],p^,j);
              inc(p,j);
              inc(posn,j);
              inc(res,j);
              dec(len,j);
              if assigned(posnblock^.next) then
               posnblock:=posnblock^.next
              else
               break;
            end
           else
            begin
              move(posnblock^.data[i],p^,len);
              inc(p,len);
              inc(posn,len);
              inc(res,len);
              len:=0;
            end;
         end;
        read:=res;
      end;


    procedure tdynamicarray.blockwrite(var f:file);
      var
        hp : pdynamicblock;
      begin
        hp:=firstblock;
        while assigned(hp) do
         begin
           system.blockwrite(f,hp^.data,hp^.used);
           hp:=hp^.next;
         end;
      end;


    destructor tdynamicarray.done;
      var
        hp : pdynamicblock;
      begin
        while assigned(firstblock) do
         begin
           hp:=firstblock;
           firstblock:=firstblock^.next;
           freemem(hp,blocksize+dynamicblockbasesize);
         end;
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
{$ifndef USEREALLOCMEM}
        odata : Pnamedindexobjectarray;
{$endif USEREALLOCMEM}
      begin
        osize:=size;
        inc(size,gsize);
{$ifndef USEREALLOCMEM}
        odata:=data;
        getmem(data,size*4);
        if assigned(odata) then
         begin
           move(odata^,data^,osize*4);
           freemem(odata,osize*4);
         end;
{$else USEREALLOCMEM}
        reallocmem(data,size*4);
{$endif USEREALLOCMEM}
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


    procedure tindexarray.delete(p:Pnamedindexobject);
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
  Revision 1.11  2000-08-27 16:11:50  peter
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
