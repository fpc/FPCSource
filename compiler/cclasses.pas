{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl and Peter Vreman

    This module provides some basic classes

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
unit cclasses;

{$i defines.inc}

interface

    uses
      cutils,cstreams;

{$ifdef OLD}
    type
       pmemdebug = ^tmemdebug;
       tmemdebug = object
          constructor init(const s:string);
          destructor  done;
          procedure show;
       private
          startmem : integer;
          infostr  : string[40];
       end;
{$endif OLD}

{********************************************
                TLinkedList
********************************************}

    type
       TLinkedListItem = class
       public
          Previous,
          Next : TLinkedListItem;
          Constructor Create;
          Destructor Destroy;override;
          Function GetCopy:TLinkedListItem;virtual;
       end;

       TLinkedListItemClass = class of TLinkedListItem;

       TLinkedList = class
       private
          FCount : integer;
          FFirst,
          FLast  : TLinkedListItem;
       public
          constructor Create;
          destructor  Destroy;override;
          { true when the List is empty }
          function  Empty:boolean;
          { deletes all Items }
          procedure Clear;
          { inserts an Item }
          procedure Insert(Item:TLinkedListItem);
          { concats an Item }
          procedure Concat(Item:TLinkedListItem);
          { deletes an Item }
          procedure Remove(Item:TLinkedListItem);
          { Gets First Item }
          function  GetFirst:TLinkedListItem;
          { Gets last Item }
          function  GetLast:TLinkedListItem;
          { inserts another List at the begin and make this List empty }
          procedure insertList(p : TLinkedList);
          { concats another List at the end and make this List empty }
          procedure concatList(p : TLinkedList);
          { concats another List at the end and makes a copy }
          procedure insertListcopy(p : TLinkedList);
          { concats another List at the end and makes a copy }
          procedure concatListcopy(p : TLinkedList);
          property First:TLinkedListItem read FFirst;
          property Last:TLinkedListItem read FLast;
          property Count:Integer read FCount;
       end;

{********************************************
                TStringList
********************************************}

       { string containerItem }
       TStringListItem = class(TLinkedListItem)
          FPStr : PString;
       public
          constructor Create(const s:string);
          destructor  Destroy;override;
          function GetCopy:TLinkedListItem;override;
          function Str:string;
       end;

       { string container }
       TStringList = class(TLinkedList)
       private
          FDoubles : boolean;  { if this is set to true, doubles are allowed }
       public
          constructor Create;
          constructor Create_No_Double;
          { inserts an Item }
          procedure Insert(const s:string);
          { concats an Item }
          procedure Concat(const s:string);
          { deletes an Item }
          procedure Remove(const s:string);
          { Gets First Item }
          function  GetFirst:string;
          { Gets last Item }
          function  GetLast:string;
          { true if string is in the container }
          function Find(const s:string):TStringListItem;
          { inserts an object }
          procedure InsertItem(item:TStringListItem);
          { concats an object }
          procedure ConcatItem(item:TStringListItem);
          property Doubles:boolean read FDoubles write FDoubles;
       end;

{$ifdef NODIC}
{********************************************
                Dictionary
********************************************}

    const
       { the real size will be [-hasharray..hasharray] ! }
       hasharraysize = 2047;

    type
       { namedindexobect for use with dictionary and indexarray }
       Tnamedindexobject=class
       { indexarray }
         indexnr    : integer;
         indexNext  : TNamedIndexObject;
       { dictionary }
         _name      : Pstring;
         _valuename : Pstring; { uppercase name }
         left,right : TNamedIndexObject;
         speedvalue : integer;
       { singleList }
         ListNext   : TNamedIndexObject;
         constructor create;
         constructor createname(const n:string);
         destructor  destroy;override;
         procedure setname(const n:string);virtual;
         function  name:string;virtual;
       end;

       Pdictionaryhasharray=^Tdictionaryhasharray;
       Tdictionaryhasharray=array[-hasharraysize..hasharraysize] of TNamedIndexObject;

       Tnamedindexcallback = procedure(p:TNamedIndexObject) of object;

       Tdictionary=class
         noclear   : boolean;
         replace_existing : boolean;
         constructor Create;
         destructor  Destroy;override;
         procedure usehash;
         procedure clear;
         function  delete(const s:string):TNamedIndexObject;
         function  empty:boolean;
         procedure foreach(proc2call:Tnamedindexcallback);
         function  insert(obj:TNamedIndexObject):TNamedIndexObject;
         function  rename(const olds,News : string):TNamedIndexObject;
         function  search(const s:string):TNamedIndexObject;
         function  speedsearch(const s:string;speedvalue:integer):TNamedIndexObject;
       private
         root      : TNamedIndexObject;
         hasharray : Pdictionaryhasharray;
         procedure cleartree(obj:TNamedIndexObject);
         function  insertNode(NewNode:TNamedIndexObject;var currNode:TNamedIndexObject):TNamedIndexObject;
         procedure inserttree(currtree,currroot:TNamedIndexObject);
       end;

       psingleList=^tsingleList;
       tsingleList=class
         First,
         last    : TNamedIndexObject;
         constructor Create;
         procedure reset;
         procedure clear;
         procedure insert(p:TNamedIndexObject);
       end;

      tindexobjectarray=array[1..16000] of TNamedIndexObject;
      TNamedIndexObjectarray=^tindexobjectarray;

      pindexarray=^tindexarray;
      tindexarray=class
        noclear : boolean;
        First   : TNamedIndexObject;
        count   : integer;
        constructor Create(Agrowsize:integer);
        destructor  destroy;override;
        procedure clear;
        procedure foreach(proc2call : Tnamedindexcallback);
        procedure deleteindex(p:TNamedIndexObject);
        procedure delete(var p:TNamedIndexObject);
        procedure insert(p:TNamedIndexObject);
        function  search(nr:integer):TNamedIndexObject;
      private
        growsize,
        size  : integer;
        data  : TNamedIndexObjectarray;
        procedure grow(gsize:integer);
      end;
{$endif NODIC}


{********************************************
              DynamicArray
********************************************}

     const
       dynamicblockbasesize = 12;

     type
       pdynamicblock = ^tdynamicblock;
       tdynamicblock = record
         pos,
         used : integer;
         Next : pdynamicblock;
         data : array[0..high(integer)-20] of byte;
       end;

       pdynamicarray = ^tdynamicarray;
       tdynamicarray = class
       private
         FPosn       : integer;
         FPosnblock  : pdynamicblock;
         FBlocksize  : integer;
         FFirstblock,
         FLastblock  : pdynamicblock;
         procedure grow;
       public
         constructor Create(Ablocksize:integer);
         destructor  Destroy;override;
         function  size:integer;
         procedure align(i:integer);
         procedure seek(i:integer);
         function  read(var d;len:integer):integer;
         procedure write(const d;len:integer);
         procedure writestr(const s:string);
         procedure readstream(f:TCStream);
         procedure writestream(f:TCStream);
         property  BlockSize : integer read FBlocksize;
         property  FirstBlock : PDynamicBlock read FFirstBlock;
       end;

implementation


{$ifdef OLD}

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
        l : integer;
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
  New(s);
  s^.data := p;
  s^.Next := head;
  head := s;
end;

function TStack.pop: pointer;
var s: PStackItem;
begin
  pop := top;
  if assigned(head) then
    begin
      s := head^.Next;
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
      temp := head^.Next;
      dispose(head);
      head := temp;
    end;
end;
{$endif fixLeaksOnError}


{$endif OLD}

{****************************************************************************
                             TLinkedListItem
 ****************************************************************************}

    constructor TLinkedListItem.Create;
      begin
        Previous:=nil;
        Next:=nil;
      end;


    destructor TLinkedListItem.Destroy;
      begin
      end;


    function TLinkedListItem.GetCopy:TLinkedListItem;
      var
        p : TLinkedListItem;
        l : integer;
      begin
        p:=TLinkedListItemClass(ClassType).Create;
        l:=InstanceSize;
        Move(pointer(self)^,pointer(p)^,l);
        Result:=p;
      end;


{****************************************************************************
                                   TLinkedList
 ****************************************************************************}

    constructor TLinkedList.Create;
      begin
        FFirst:=nil;
        Flast:=nil;
        FCount:=0;
      end;


    destructor TLinkedList.destroy;
      begin
        Clear;
      end;


    function TLinkedList.empty:boolean;
      begin
        Empty:=(FFirst=nil);
      end;


    procedure TLinkedList.Insert(Item:TLinkedListItem);
      begin
        if FFirst=nil then
         begin
           FLast:=Item;
           Item.Previous:=nil;
           Item.Next:=nil;
         end
        else
         begin
           FFirst.Previous:=Item;
           Item.Previous:=nil;
           Item.Next:=FFirst;
         end;
        FFirst:=Item;
        inc(FCount);
      end;


    procedure TLinkedList.Concat(Item:TLinkedListItem);
      begin
        if FFirst=nil then
         begin
           FFirst:=Item;
           Item.Previous:=nil;
           Item.Next:=nil;
         end
        else
         begin
           Flast.Next:=Item;
           Item.Previous:=Flast;
           Item.Next:=nil;
         end;
        Flast:=Item;
        inc(FCount);
      end;


    procedure TLinkedList.remove(Item:TLinkedListItem);
      begin
         if Item=nil then
           exit;
         if (FFirst=Item) and (Flast=Item) then
           begin
              FFirst:=nil;
              Flast:=nil;
           end
         else if FFirst=Item then
           begin
              FFirst:=Item.Next;
              if assigned(FFirst) then
                FFirst.Previous:=nil;
           end
         else if Flast=Item then
           begin
              Flast:=Flast.Previous;
              if assigned(Flast) then
                Flast.Next:=nil;
           end
         else
           begin
              Item.Previous.Next:=Item.Next;
              Item.Next.Previous:=Item.Previous;
           end;
         Item.Next:=nil;
         Item.Previous:=nil;
         dec(FCount);
      end;


    procedure TLinkedList.clear;
      var
        NewNode : TLinkedListItem;
      begin
        NewNode:=FFirst;
        while assigned(NewNode) do
         begin
           FFirst:=NewNode.Next;
           NewNode.Free;
           NewNode:=FFirst;
          end;
        FLast:=nil;
        FFirst:=nil;
        FCount:=0;
      end;


    function TLinkedList.GetFirst:TLinkedListItem;
      begin
         if FFirst=nil then
          GetFirst:=nil
         else
          begin
            GetFirst:=FFirst;
            if FFirst=FLast then
             FLast:=nil;
            FFirst:=FFirst.Next;
            dec(FCount);
          end;
      end;


    function TLinkedList.GetLast:TLinkedListItem;
      begin
         if FLast=nil then
          Getlast:=nil
         else
          begin
            Getlast:=FLast;
            if FLast=FFirst then
             FFirst:=nil;
            FLast:=FLast.Previous;
            dec(FCount);
          end;
      end;


    procedure TLinkedList.insertList(p : TLinkedList);
      begin
         { empty List ? }
         if (p.FFirst=nil) then
           exit;
         p.Flast.Next:=FFirst;
         { we have a double Linked List }
         if assigned(FFirst) then
           FFirst.Previous:=p.Flast;
         FFirst:=p.FFirst;
         if (FLast=nil) then
           Flast:=p.Flast;
         { p becomes empty }
         p.FFirst:=nil;
         p.Flast:=nil;
      end;


    procedure TLinkedList.concatList(p : TLinkedList);
      begin
        if (p.FFirst=nil) then
         exit;
        if FFirst=nil then
         FFirst:=p.FFirst
        else
         begin
           FLast.Next:=p.FFirst;
           p.FFirst.Previous:=Flast;
         end;
        Flast:=p.Flast;
        { make p empty }
        p.Flast:=nil;
        p.FFirst:=nil;
      end;


    procedure TLinkedList.insertListcopy(p : TLinkedList);
      var
        NewNode,NewNode2 : TLinkedListItem;
      begin
        NewNode:=p.First;
        while assigned(NewNode) do
         begin
           NewNode2:=NewNode.Getcopy;
           if assigned(NewNode2) then
            Insert(NewNode2);
           NewNode:=NewNode.Next;
         end;
      end;


    procedure TLinkedList.concatListcopy(p : TLinkedList);
      var
        NewNode,NewNode2 : TLinkedListItem;
      begin
        NewNode:=p.First;
        while assigned(NewNode) do
         begin
           NewNode2:=NewNode.Getcopy;
           if assigned(NewNode2) then
            Concat(NewNode2);
           NewNode:=NewNode.Next;
         end;
      end;


{****************************************************************************
                             TStringListItem
 ****************************************************************************}

    constructor TStringListItem.Create(const s:string);
      begin
        inherited Create;
        FPStr:=stringdup(s);
      end;


    destructor TStringListItem.Destroy;
      begin
        stringdispose(FPStr);
      end;


    function TStringListItem.Str:string;
      begin
        Str:=FPStr^;
      end;


    function TStringListItem.GetCopy:TLinkedListItem;
      begin
        Result:=(inherited GetCopy);
        TStringListItem(Result).FPStr:=stringdup(FPstr^);
      end;


{****************************************************************************
                           TSTRINGList
 ****************************************************************************}

    constructor tstringList.Create;
      begin
         inherited Create;
         FDoubles:=true;
      end;


    constructor tstringList.Create_no_double;
      begin
         inherited Create;
         FDoubles:=false;
      end;


    procedure tstringList.insert(const s : string);
      begin
         if (s='') or
            ((not FDoubles) and (find(s)<>nil)) then
          exit;
         inherited insert(tstringListItem.create(s));
      end;


    procedure tstringList.concat(const s : string);
      begin
         if (s='') or
            ((not FDoubles) and (find(s)<>nil)) then
          exit;
         inherited concat(tstringListItem.create(s));
      end;


    procedure tstringList.remove(const s : string);
      var
        p : tstringListItem;
      begin
        if s='' then
         exit;
        p:=find(s);
        if assigned(p) then
         inherited Remove(p);
      end;


    function tstringList.GetFirst : string;
      var
         p : tstringListItem;
      begin
         p:=tstringListItem(inherited GetFirst);
         if p=nil then
          GetFirst:=''
         else
          begin
            GetFirst:=p.FPStr^;
            p.free;
          end;
      end;


    function tstringList.Getlast : string;
      var
         p : tstringListItem;
      begin
         p:=tstringListItem(inherited Getlast);
         if p=nil then
          Getlast:=''
         else
          begin
            Getlast:=p.FPStr^;
            p.free;
          end;
      end;


    function tstringList.find(const s:string):TstringListItem;
      var
        NewNode : tstringListItem;
      begin
        find:=nil;
        if s='' then
         exit;
        NewNode:=tstringListItem(FFirst);
        while assigned(NewNode) do
         begin
           if NewNode.FPStr^=s then
            begin
              find:=NewNode;
              exit;
            end;
           NewNode:=tstringListItem(NewNode.Next);
         end;
      end;


    procedure TStringList.InsertItem(item:TStringListItem);
      begin
        inherited Insert(item);
      end;


    procedure TStringList.ConcatItem(item:TStringListItem);
      begin
        inherited Concat(item);
      end;


{$ifdef NODIC}
{****************************************************************************
                               Tnamedindexobject
 ****************************************************************************}

constructor Tnamedindexobject.Create;
begin
  { index }
  indexnr:=-1;
  indexNext:=nil;
  { dictionary }
  left:=nil;
  right:=nil;
  _name:=nil;
  speedvalue:=-1;
  { List }
  ListNext:=nil;
end;

constructor Tnamedindexobject.Createname(const n:string);
begin
  { index }
  indexnr:=-1;
  indexNext:=nil;
  { dictionary }
  left:=nil;
  right:=nil;
  speedvalue:=-1;
  _name:=stringdup(n);
  { List }
  ListNext:=nil;
end;


destructor Tnamedindexobject.destroy;
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

    constructor Tdictionary.Create;
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
           New(hasharray);
           fillchar(hasharray^,sizeof(hasharray^),0);
         end;
      end;


    destructor Tdictionary.destroy;
      begin
        if not noclear then
         clear;
        if assigned(hasharray) then
         dispose(hasharray);
      end;


    procedure Tdictionary.cleartree(obj:TNamedIndexObject);
      begin
        if assigned(obj.left) then
          cleartree(obj.left);
        if assigned(obj.right) then
          cleartree(obj.right);
        obj.free;
        obj:=nil;
      end;


    procedure Tdictionary.clear;
      var
        w : integer;
      begin
        if assigned(root) then
          cleartree(root);
        if assigned(hasharray) then
         for w:=-hasharraysize to hasharraysize do
          if assigned(hasharray^[w]) then
           cleartree(hasharray^[w]);
      end;


    function Tdictionary.delete(const s:string):TNamedIndexObject;
    var
      p,speedvalue : integer;
      n : TNamedIndexObject;

        procedure insert_right_bottom(var root,Atree:TNamedIndexObject);
        begin
          while root.right<>nil do
           root:=root.right;
          root.right:=Atree;
        end;

        function delete_from_tree(root:TNamedIndexObject):TNamedIndexObject;
        type
          leftright=(left,right);
        var
          lr : leftright;
          oldroot : TNamedIndexObject;
        begin
          oldroot:=nil;
          while (root<>nil) and (root.speedvalue<>speedvalue) do
           begin
             oldroot:=root;
             if speedvalue<root.speedvalue then
              begin
                root:=root.right;
                lr:=right;
              end
             else
              begin
                root:=root.left;
                lr:=left;
              end;
           end;
          while (root<>nil) and (root._name^<>s) do
           begin
             oldroot:=root;
             if s<root._name^ then
              begin
                root:=root.right;
                lr:=right;
              end
             else
              begin
                root:=root.left;
                lr:=left;
              end;
           end;
          if root.left<>nil then
           begin
             { Now the Node pointing to root must point to the left
               subtree of root. The right subtree of root must be
               connected to the right bottom of the left subtree.}
             if lr=left then
              oldroot.left:=root.left
             else
              oldroot.right:=root.left;
             if root.right<>nil then
              insert_right_bottom(root.left,root.right);
           end
          else
           begin
             { There is no left subtree. So we can just replace the Node to
               delete with the right subtree.}
             if lr=left then
              oldroot.left:=root.right
             else
              oldroot.right:=root.right;
           end;
          delete_from_tree:=root;
        end;

    begin
      speedvalue:=Getspeedvalue(s);
      n:=root;
      if assigned(hasharray) then
       begin
         { First, check if the Node to delete directly located under
           the hasharray.}
         p:=speedvalue mod hasharraysize;
         n:=hasharray^[p];
         if (n<>nil) and (n.speedvalue=speedvalue) and
            (n._name^=s) then
          begin
            { The Node to delete is directly located under the
              hasharray. Make the hasharray point to the left
              subtree of the Node and place the right subtree on
              the right-bottom of the left subtree.}
            if n.left<>nil then
             begin
               hasharray^[p]:=n.left;
               if n.right<>nil then
                insert_right_bottom(n.left,n.right);
             end
            else
             hasharray^[p]:=n.right;
            delete:=n;
            exit;
          end;
       end
      else
       begin
         { First check if the Node to delete is the root.}
         if (root<>nil) and (n.speedvalue=speedvalue) and
            (n._name^=s) then
          begin
            if n.left<>nil then
             begin
               root:=n.left;
               if n.right<>nil then
                insert_right_bottom(n.left,n.right);
             end
            else
             root:=n.right;
            delete:=n;
            exit;
          end;
       end;
      delete:=delete_from_tree(n);
    end;

    function Tdictionary.empty:boolean;
      var
        w : integer;
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

        procedure a(p:TNamedIndexObject);
        begin
          proc2call(p);
          if assigned(p.left) then
           a(p.left);
          if assigned(p.right) then
           a(p.right);
        end;

      var
        i : integer;
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


    function Tdictionary.insert(obj:TNamedIndexObject):TNamedIndexObject;
      begin
        obj.speedvalue:=Getspeedvalue(obj._name^);
        if assigned(hasharray) then
         insert:=insertNode(obj,hasharray^[obj.speedvalue mod hasharraysize])
        else
         insert:=insertNode(obj,root);
      end;


    function tdictionary.insertNode(NewNode:TNamedIndexObject;var currNode:TNamedIndexObject):TNamedIndexObject;
      begin
        if currNode=nil then
         begin
           currNode:=NewNode;
           insertNode:=NewNode;
         end
        { First check speedvalue, to allow a fast insert }
        else
         if currNode.speedvalue>NewNode.speedvalue then
          insertNode:=insertNode(NewNode,currNode.right)
        else
         if currNode.speedvalue<NewNode.speedvalue then
          insertNode:=insertNode(NewNode,currNode.left)
        else
         begin
           if currNode._name^>NewNode._name^ then
            insertNode:=insertNode(NewNode,currNode.right)
           else
            if currNode._name^<NewNode._name^ then
             insertNode:=insertNode(NewNode,currNode.left)
           else
            begin
              if replace_existing and
                 assigned(currNode) then
                begin
                  NewNode.left:=currNode.left;
                  NewNode.right:=currNode.right;
                  currNode:=NewNode;
                  insertNode:=NewNode;
                end
              else
               insertNode:=currNode;
             end;
         end;
      end;


    procedure tdictionary.inserttree(currtree,currroot:TNamedIndexObject);
      begin
        if assigned(currtree) then
         begin
           inserttree(currtree.left,currroot);
           inserttree(currtree.right,currroot);
           currtree.right:=nil;
           currtree.left:=nil;
           insertNode(currtree,currroot);
         end;
      end;


    function tdictionary.rename(const olds,News : string):TNamedIndexObject;
      var
        spdval : integer;
        lasthp,
        hp,hp2,hp3 : TNamedIndexObject;
      begin
        spdval:=Getspeedvalue(olds);
        if assigned(hasharray) then
         hp:=hasharray^[spdval mod hasharraysize]
        else
         hp:=root;
        lasthp:=nil;
        while assigned(hp) do
          begin
            if spdval>hp.speedvalue then
             begin
               lasthp:=hp;
               hp:=hp.left
             end
            else
             if spdval<hp.speedvalue then
              begin
                lasthp:=hp;
                hp:=hp.right
              end
            else
             begin
               if (hp.name=olds) then
                begin
                  { Get in hp2 the replacer for the root or hasharr }
                  hp2:=hp.left;
                  hp3:=hp.right;
                  if not assigned(hp2) then
                   begin
                     hp2:=hp.right;
                     hp3:=hp.left;
                   end;
                  { remove entry from the tree }
                  if assigned(lasthp) then
                   begin
                     if lasthp.left=hp then
                      lasthp.left:=hp2
                     else
                      lasthp.right:=hp2;
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
                  { reset Node with New values }
                  stringdispose(hp._name);
                  hp._name:=stringdup(News);
                  hp.speedvalue:=Getspeedvalue(News);
                  hp.left:=nil;
                  hp.right:=nil;
                  { reinsert }
                  if assigned(hasharray) then
                   rename:=insertNode(hp,hasharray^[hp.speedvalue mod hasharraysize])
                  else
                   rename:=insertNode(hp,root);
                  exit;
                end
               else
                if olds>hp.name then
                 begin
                   lasthp:=hp;
                   hp:=hp.left
                 end
                else
                 begin
                   lasthp:=hp;
                   hp:=hp.right;
                 end;
             end;
          end;
      end;


    function Tdictionary.search(const s:string):TNamedIndexObject;
      begin
        search:=speedsearch(s,Getspeedvalue(s));
      end;


    function Tdictionary.speedsearch(const s:string;speedvalue:integer):TNamedIndexObject;
      var
        NewNode:TNamedIndexObject;
      begin
        if assigned(hasharray) then
         NewNode:=hasharray^[speedvalue mod hasharraysize]
        else
         NewNode:=root;
        while assigned(NewNode) do
         begin
           if speedvalue>NewNode.speedvalue then
            NewNode:=NewNode.left
           else
            if speedvalue<NewNode.speedvalue then
             NewNode:=NewNode.right
           else
            begin
              if (NewNode._name^=s) then
               begin
                 speedsearch:=NewNode;
                 exit;
               end
              else
               if s>NewNode._name^ then
                NewNode:=NewNode.left
              else
               NewNode:=NewNode.right;
            end;
         end;
        speedsearch:=nil;
      end;


{****************************************************************************
                               tsingleList
 ****************************************************************************}

    constructor tsingleList.create;
      begin
        First:=nil;
        last:=nil;
      end;


    procedure tsingleList.reset;
      begin
        First:=nil;
        last:=nil;
      end;


    procedure tsingleList.clear;
      var
        hp,hp2 : TNamedIndexObject;
      begin
        hp:=First;
        while assigned(hp) do
         begin
           hp2:=hp;
           hp:=hp.ListNext;
           hp2.free;
         end;
        First:=nil;
        last:=nil;
      end;


    procedure tsingleList.insert(p:TNamedIndexObject);
      begin
        if not assigned(First) then
         First:=p
        else
         last.ListNext:=p;
        last:=p;
        p.ListNext:=nil;
      end;


{****************************************************************************
                               tindexarray
 ****************************************************************************}

    constructor tindexarray.create(Agrowsize:integer);
      begin
        growsize:=Agrowsize;
        size:=0;
        count:=0;
        data:=nil;
        First:=nil;
        noclear:=false;
      end;


    destructor tindexarray.destroy;
      begin
        if assigned(data) then
          begin
             if not noclear then
              clear;
             freemem(data);
             data:=nil;
          end;
      end;


    function tindexarray.search(nr:integer):TNamedIndexObject;
      begin
        if nr<=count then
         search:=data^[nr]
        else
         search:=nil;
      end;


    procedure tindexarray.clear;
      var
        i : integer;
      begin
        for i:=1 to count do
         if assigned(data^[i]) then
          begin
            data^[i].free;
            data^[i]:=nil;
          end;
        count:=0;
        First:=nil;
      end;


    procedure tindexarray.foreach(proc2call : Tnamedindexcallback);
      var
        i : integer;
      begin
        for i:=1 to count do
         if assigned(data^[i]) then
          proc2call(data^[i]);
      end;


    procedure tindexarray.grow(gsize:integer);
      var
        osize : integer;
      begin
        osize:=size;
        inc(size,gsize);
        reallocmem(data,size*4);
        fillchar(data^[osize+1],gsize*4,0);
      end;


    procedure tindexarray.deleteindex(p:TNamedIndexObject);
      var
        i : integer;
      begin
        i:=p.indexnr;
        { update counter }
        if i=count then
         dec(count);
        { update Linked List }
        while (i>0) do
         begin
           dec(i);
           if (i>0) and assigned(data^[i]) then
            begin
              data^[i].indexNext:=data^[p.indexnr].indexNext;
              break;
            end;
         end;
        if i=0 then
         First:=p.indexNext;
        data^[p.indexnr]:=nil;
        { clear entry }
        p.indexnr:=-1;
        p.indexNext:=nil;
      end;


    procedure tindexarray.delete(var p:TNamedIndexObject);
      begin
        deleteindex(p);
        p.free;
        p:=nil;
      end;


    procedure tindexarray.insert(p:TNamedIndexObject);
      var
        i  : integer;
      begin
        if p.indexnr=-1 then
         begin
           inc(count);
           p.indexnr:=count;
         end;
        if p.indexnr>count then
         count:=p.indexnr;
        if count>size then
         grow(((count div growsize)+1)*growsize);
        Assert(not assigned(data^[p.indexnr]) or (p=data^[p.indexnr]));
        data^[p.indexnr]:=p;
        { update Linked List backward }
        i:=p.indexnr;
        while (i>0) do
         begin
           dec(i);
           if (i>0) and assigned(data^[i]) then
            begin
              data^[i].indexNext:=p;
              break;
            end;
         end;
        if i=0 then
         First:=p;
        { update Linked List forward }
        i:=p.indexnr;
        while (i<=count) do
         begin
           inc(i);
           if (i<=count) and assigned(data^[i]) then
            begin
              p.indexNext:=data^[i];
              exit;
            end;
         end;
        if i>count then
         p.indexNext:=nil;
      end;
{$endif NODIC}

{****************************************************************************
                                tdynamicarray
****************************************************************************}

    constructor tdynamicarray.create(Ablocksize:integer);
      begin
        FPosn:=0;
        FPosnblock:=nil;
        FFirstblock:=nil;
        FLastblock:=nil;
        Fblocksize:=Ablocksize;
        grow;
      end;


    destructor tdynamicarray.destroy;
      var
        hp : pdynamicblock;
      begin
        while assigned(FFirstblock) do
         begin
           hp:=FFirstblock;
           FFirstblock:=FFirstblock^.Next;
           freemem(hp,blocksize+dynamicblockbasesize);
         end;
      end;


    function  tdynamicarray.size:integer;
      begin
        if assigned(FLastblock) then
         size:=FLastblock^.pos+FLastblock^.used
        else
         size:=0;
      end;


    procedure tdynamicarray.grow;
      var
        nblock : pdynamicblock;
      begin
        Getmem(nblock,blocksize+dynamicblockbasesize);
        if not assigned(FFirstblock) then
         begin
           FFirstblock:=nblock;
           FPosnblock:=nblock;
           nblock^.pos:=0;
         end
        else
         begin
           FLastblock^.Next:=nblock;
           nblock^.pos:=FLastblock^.pos+FLastblock^.used;
         end;
        nblock^.used:=0;
        nblock^.Next:=nil;
        fillchar(nblock^.data,blocksize,0);
        FLastblock:=nblock;
      end;


    procedure tdynamicarray.align(i:integer);
      var
        j : integer;
      begin
        j:=(FPosn mod i);
        if j<>0 then
         begin
           j:=i-j;
           if FPosnblock^.used+j>blocksize then
            begin
              dec(j,blocksize-FPosnblock^.used);
              FPosnblock^.used:=blocksize;
              grow;
              FPosnblock:=FLastblock;
            end;
           inc(FPosnblock^.used,j);
           inc(FPosn,j);
         end;
      end;


    procedure tdynamicarray.seek(i:integer);
      begin
        if (i<FPosnblock^.pos) or (i>FPosnblock^.pos+blocksize) then
         begin
           { set FPosnblock correct if the size is bigger then
             the current block }
           if FPosnblock^.pos>i then
            FPosnblock:=FFirstblock;
           while assigned(FPosnblock) do
            begin
              if FPosnblock^.pos+blocksize>i then
               break;
              FPosnblock:=FPosnblock^.Next;
            end;
           { not found ? then increase blocks }
           if not assigned(FPosnblock) then
            begin
              { the current FLastblock is now also fully used }
              FLastblock^.used:=blocksize;
              repeat
                grow;
                FPosnblock:=FLastblock;
              until FPosnblock^.pos+blocksize>=i;
            end;
         end;
        FPosn:=i;
        if FPosn mod blocksize>FPosnblock^.used then
         FPosnblock^.used:=FPosn mod blocksize;
      end;


    procedure tdynamicarray.write(const d;len:integer);
      var
        p : pchar;
        i,j : integer;
      begin
        p:=pchar(@d);
        while (len>0) do
         begin
           i:=FPosn mod blocksize;
           if i+len>=blocksize then
            begin
              j:=blocksize-i;
              move(p^,FPosnblock^.data[i],j);
              inc(p,j);
              inc(FPosn,j);
              dec(len,j);
              FPosnblock^.used:=blocksize;
              if assigned(FPosnblock^.Next) then
               FPosnblock:=FPosnblock^.Next
              else
               begin
                 grow;
                 FPosnblock:=FLastblock;
               end;
            end
           else
            begin
              move(p^,FPosnblock^.data[i],len);
              inc(p,len);
              inc(FPosn,len);
              i:=FPosn mod blocksize;
              if i>FPosnblock^.used then
               FPosnblock^.used:=i;
              len:=0;
            end;
         end;
      end;


    procedure tdynamicarray.writestr(const s:string);
      begin
        write(s[1],length(s));
      end;


    function tdynamicarray.read(var d;len:integer):integer;
      var
        p : pchar;
        i,j,res : integer;
      begin
        res:=0;
        p:=pchar(@d);
        while (len>0) do
         begin
           i:=FPosn mod blocksize;
           if i+len>=FPosnblock^.used then
            begin
              j:=FPosnblock^.used-i;
              move(FPosnblock^.data[i],p^,j);
              inc(p,j);
              inc(FPosn,j);
              inc(res,j);
              dec(len,j);
              if assigned(FPosnblock^.Next) then
               FPosnblock:=FPosnblock^.Next
              else
               break;
            end
           else
            begin
              move(FPosnblock^.data[i],p^,len);
              inc(p,len);
              inc(FPosn,len);
              inc(res,len);
              len:=0;
            end;
         end;
        read:=res;
      end;


    procedure tdynamicarray.readstream(f:TCStream);
      var
        i,left : integer;
      begin
        repeat
          left:=blocksize-FPosnblock^.used;
          i:=f.Read(FPosnblock^.data[FPosnblock^.used],left);
          inc(FPosnblock^.used,i);
          if FPosnblock^.used=blocksize then
           begin
             if assigned(FPosnblock^.Next) then
              FPosnblock:=FPosnblock^.Next
             else
              begin
                grow;
                FPosnblock:=FLastblock;
              end;
           end;
        until (i<left);
      end;


    procedure tdynamicarray.writestream(f:TCStream);
      var
        hp : pdynamicblock;
      begin
        hp:=FFirstblock;
        while assigned(hp) do
         begin
           f.Write(hp^.data,hp^.used);
           hp:=hp^.Next;
         end;
      end;


end.
{
  $Log$
  Revision 1.3  2000-12-29 21:57:27  peter
    * 'classified' tdictionary, but leave it within an define

  Revision 1.2  2000/12/25 00:07:25  peter
    + new tlinkedlist class (merge of old tstringqueue,tcontainer and
      tlinkedlist objects)

  Revision 1.1  2000/12/24 12:25:31  peter
    + cstreams unit
    * dynamicarray object to class

}
