{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl and Peter Vreman

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

{$i fpcdefs.inc}

interface

    uses
      cutils,cstreams;

{********************************************
                TMemDebug
********************************************}

    type
       tmemdebug = class
       private
          startmem : integer;
          infostr  : string[40];
       public
          constructor Create(const s:string);
          destructor  Destroy;override;
          procedure show;
       end;

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
          { inserts an item }
          procedure InsertItem(item:TStringListItem);
          { concats an item }
          procedure ConcatItem(item:TStringListItem);
          property Doubles:boolean read FDoubles write FDoubles;
       end;


{********************************************
                Dictionary
********************************************}

    const
       { the real size will be [0..hasharray-1] ! }
       hasharraysize = 512;

    type
       { namedindexobect for use with dictionary and indexarray }
       TNamedIndexItem=class
       private
       { indexarray }
         FIndexNr    : integer;
         FIndexNext  : TNamedIndexItem;
       { dictionary }
         FLeft,
         FRight      : TNamedIndexItem;
         FSpeedValue : cardinal;
       { singleList }
         FListNext   : TNamedIndexItem;
       protected
         function  GetName:string;virtual;
         procedure SetName(const n:string);virtual;
       public
         FName       : Pstring;
         constructor Create;
         constructor CreateName(const n:string);
         destructor  Destroy;override;
         property IndexNr:integer read FIndexNr write FIndexNr;
         property IndexNext:TNamedIndexItem read FIndexNext write FIndexNext;
         property Name:string read GetName write SetName;
         property SpeedValue:cardinal read FSpeedValue;
         property ListNext:TNamedIndexItem read FListNext;
         property Left:TNamedIndexItem read FLeft write FLeft;
         property Right:TNamedIndexItem read FRight write FRight;
       end;

       Pdictionaryhasharray=^Tdictionaryhasharray;
       Tdictionaryhasharray=array[0..hasharraysize-1] of TNamedIndexItem;

       TnamedIndexCallback = procedure(p:TNamedIndexItem;arg:pointer) of object;
       TnamedIndexStaticCallback = procedure(p:TNamedIndexItem;arg:pointer);

       Tdictionary=class
       private
         FRoot      : TNamedIndexItem;
         FHashArray : Pdictionaryhasharray;
         procedure cleartree(obj:TNamedIndexItem);
         function  insertNode(NewNode:TNamedIndexItem;var currNode:TNamedIndexItem):TNamedIndexItem;
         procedure inserttree(currtree,currroot:TNamedIndexItem);
       public
         noclear   : boolean;
         replace_existing : boolean;
         delete_doubles : boolean;
         constructor Create;
         destructor  Destroy;override;
         procedure usehash;
         procedure clear;
         function  delete(const s:string):TNamedIndexItem;
         function  empty:boolean;
         procedure foreach(proc2call:TNamedIndexcallback;arg:pointer);
         procedure foreach_static(proc2call:TNamedIndexStaticCallback;arg:pointer);
         function  insert(obj:TNamedIndexItem):TNamedIndexItem;
         function  rename(const olds,News : string):TNamedIndexItem;
         function  search(const s:string):TNamedIndexItem;
         function  speedsearch(const s:string;SpeedValue:cardinal):TNamedIndexItem;
         property  Items[const s:string]:TNamedIndexItem read Search;default;
       end;

       tsingleList=class
         First,
         last    : TNamedIndexItem;
         constructor Create;
         procedure reset;
         procedure clear;
         procedure insert(p:TNamedIndexItem);
       end;

      tindexobjectarray=array[1..16000] of TNamedIndexItem;
      pnamedindexobjectarray=^tindexobjectarray;

      tindexarray=class
        noclear : boolean;
        First   : TNamedIndexItem;
        count   : integer;
        constructor Create(Agrowsize:integer);
        destructor  destroy;override;
        procedure clear;
        procedure foreach(proc2call : Tnamedindexcallback;arg:pointer);
        procedure foreach_static(proc2call : Tnamedindexstaticcallback;arg:pointer);
        procedure deleteindex(p:TNamedIndexItem);
        procedure delete(var p:TNamedIndexItem);
        procedure insert(p:TNamedIndexItem);
        function  search(nr:integer):TNamedIndexItem;
      private
        growsize,
        size  : integer;
        data  : pnamedindexobjectarray;
        procedure grow(gsize:integer);
      end;


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
         procedure readstream(f:TCStream;maxlen:longint);
         procedure writestream(f:TCStream);
         property  BlockSize : integer read FBlocksize;
         property  FirstBlock : PDynamicBlock read FFirstBlock;
       end;


implementation


{*****************************************************************************
                                    Memory debug
*****************************************************************************}

    constructor tmemdebug.create(const s:string);
      begin
        infostr:=s;
{$ifdef Delphi}
        startmem:=0;
{$else}
        startmem:=memavail;
{$endif Delphi}
      end;


    destructor tmemdebug.destroy;
      begin
        show;
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
         begin
           inherited Remove(p);
           p.Free;
         end;
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


{****************************************************************************
                               TNamedIndexItem
 ****************************************************************************}

    constructor TNamedIndexItem.Create;
      begin
        { index }
        Findexnr:=-1;
        FindexNext:=nil;
        { dictionary }
        Fleft:=nil;
        Fright:=nil;
        FName:=nil;
        Fspeedvalue:=cardinal($ffffffff);
        { List }
        FListNext:=nil;
      end;

    constructor TNamedIndexItem.Createname(const n:string);
      begin
        { index }
        Findexnr:=-1;
        FindexNext:=nil;
        { dictionary }
        Fleft:=nil;
        Fright:=nil;
        Fspeedvalue:=cardinal($ffffffff);
        FName:=stringdup(n);
        { List }
        FListNext:=nil;
      end;


    destructor TNamedIndexItem.destroy;
      begin
        stringdispose(FName);
      end;


    procedure TNamedIndexItem.setname(const n:string);
      begin
        if speedvalue=cardinal($ffffffff) then
         begin
           if assigned(FName) then
             stringdispose(FName);
           FName:=stringdup(n);
         end;
      end;


    function TNamedIndexItem.GetName:string;
      begin
        if assigned(FName) then
         Getname:=FName^
        else
         Getname:='';
      end;


{****************************************************************************
                               TDICTIONARY
****************************************************************************}

    constructor Tdictionary.Create;
      begin
        FRoot:=nil;
        FHashArray:=nil;
        noclear:=false;
        replace_existing:=false;
        delete_doubles:=false;
      end;


    procedure Tdictionary.usehash;
      begin
        if not(assigned(FRoot)) and
           not(assigned(FHashArray)) then
         begin
           New(FHashArray);
           fillchar(FHashArray^,sizeof(FHashArray^),0);
         end;
      end;


    function counttree(p: tnamedindexitem): longint;
      begin
        if not assigned(p) then
          exit(0);
        result := 1;
        inc(result,counttree(p.fleft));
        inc(result,counttree(p.fright));
      end;

    destructor Tdictionary.destroy;
{$ifdef hashdebug}
      var
        i, unused, slots_with_col, collissions, treecount, maxcol: longint;
{$endif hashdebug}
      begin
        if not noclear then
         clear;
        if assigned(FHashArray) then
         begin
{$ifdef hashdebug}
           unused := 0;
           collissions := 0;
           maxcol := 0;
           slots_with_col := 0;
           for i := low(fhasharray^) to high(fhasharray^) do
             if assigned(fhasharray^[i]) then
               begin
                 treecount := counttree(fhasharray^[i]);
                 inc(collissions,sqr(treecount-1));
                 if treecount > maxcol then
                   maxcol := treecount;
                 inc(slots_with_col,ord(treecount>1));
               end
             else
               inc(unused);
           writeln('Slots unused: ',unused,' out of ',hasharraysize,
             ' (',slots_with_col,' with >1 items)');
           writeln('Mean number of collissions: ',
             (sqrt(collissions / extended(hasharraysize-1))):0:3,' (max: ',maxcol,')');
           writeln;
{$endif hashdebug}
           dispose(FHashArray);
         end;
      end;


    procedure Tdictionary.cleartree(obj:TNamedIndexItem);
      begin
        if assigned(obj.Fleft) then
          cleartree(obj.FLeft);
        if assigned(obj.FRight) then
          cleartree(obj.FRight);
        obj.free;
        obj:=nil;
      end;


    procedure Tdictionary.clear;
      var
        w : integer;
      begin
        if assigned(FRoot) then
          cleartree(FRoot);
        if assigned(FHashArray) then
         for w:= low(FHashArray^) to high(FHashArray^) do
          if assigned(FHashArray^[w]) then
           cleartree(FHashArray^[w]);
      end;


    function Tdictionary.delete(const s:string):TNamedIndexItem;
      var
        p,SpeedValue : cardinal;
        n : TNamedIndexItem;

        procedure insert_right_bottom(var root,Atree:TNamedIndexItem);
          begin
            while root.FRight<>nil do
             root:=root.FRight;
            root.FRight:=Atree;
          end;

        function delete_from_tree(root:TNamedIndexItem):TNamedIndexItem;
          type
            leftright=(left,right);
          var
            lr : leftright;
            oldroot : TNamedIndexItem;
          begin
            oldroot:=nil;
            while (root<>nil) and (root.SpeedValue<>SpeedValue) do
             begin
               oldroot:=root;
               if SpeedValue<root.SpeedValue then
                begin
                  root:=root.FRight;
                  lr:=right;
                end
               else
                begin
                  root:=root.FLeft;
                  lr:=left;
                end;
             end;
            while (root<>nil) and (root.FName^<>s) do
             begin
               oldroot:=root;
               if s<root.FName^ then
                begin
                  root:=root.FRight;
                  lr:=right;
                end
               else
                begin
                  root:=root.FLeft;
                  lr:=left;
                end;
             end;
            if root.FLeft<>nil then
             begin
               { Now the Node pointing to root must point to the left
                 subtree of root. The right subtree of root must be
                 connected to the right bottom of the left subtree.}
               if lr=left then
                oldroot.FLeft:=root.FLeft
               else
                oldroot.FRight:=root.FLeft;
               if root.FRight<>nil then
                insert_right_bottom(root.FLeft,root.FRight);
             end
            else
             begin
               { There is no left subtree. So we can just replace the Node to
                 delete with the right subtree.}
               if lr=left then
                oldroot.FLeft:=root.FRight
               else
                oldroot.FRight:=root.FRight;
             end;
            delete_from_tree:=root;
          end;

      begin
        SpeedValue:=GetSpeedValue(s);
        n:=FRoot;
        if assigned(FHashArray) then
         begin
           { First, check if the Node to delete directly located under
             the hasharray.}
           p:=SpeedValue mod hasharraysize;
           n:=FHashArray^[p];
           if (n<>nil) and (n.SpeedValue=SpeedValue) and
              (n.FName^=s) then
            begin
              { The Node to delete is directly located under the
                hasharray. Make the hasharray point to the left
                subtree of the Node and place the right subtree on
                the right-bottom of the left subtree.}
              if n.FLeft<>nil then
               begin
                 FHashArray^[p]:=n.FLeft;
                 if n.FRight<>nil then
                  insert_right_bottom(n.FLeft,n.FRight);
               end
              else
               FHashArray^[p]:=n.FRight;
              delete:=n;
              exit;
            end;
         end
        else
         begin
           { First check if the Node to delete is the root.}
           if (FRoot<>nil) and (n.SpeedValue=SpeedValue) and
              (n.FName^=s) then
            begin
              if n.FLeft<>nil then
               begin
                 FRoot:=n.FLeft;
                 if n.FRight<>nil then
                  insert_right_bottom(n.FLeft,n.FRight);
               end
              else
               FRoot:=n.FRight;
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
        if assigned(FHashArray) then
         begin
           empty:=false;
           for w:=low(FHashArray^) to high(FHashArray^) do
            if assigned(FHashArray^[w]) then
             exit;
           empty:=true;
         end
        else
         empty:=(FRoot=nil);
      end;


    procedure Tdictionary.foreach(proc2call:TNamedIndexcallback;arg:pointer);

        procedure a(p:TNamedIndexItem;arg:pointer);
        begin
          proc2call(p,arg);
          if assigned(p.FLeft) then
           a(p.FLeft,arg);
          if assigned(p.FRight) then
           a(p.FRight,arg);
        end;

      var
        i : integer;
      begin
        if assigned(FHashArray) then
         begin
           for i:=low(FHashArray^) to high(FHashArray^) do
            if assigned(FHashArray^[i]) then
             a(FHashArray^[i],arg);
         end
        else
         if assigned(FRoot) then
          a(FRoot,arg);
      end;


    procedure Tdictionary.foreach_static(proc2call:TNamedIndexStaticCallback;arg:pointer);

        procedure a(p:TNamedIndexItem;arg:pointer);
        begin
          proc2call(p,arg);
          if assigned(p.FLeft) then
           a(p.FLeft,arg);
          if assigned(p.FRight) then
           a(p.FRight,arg);
        end;

      var
        i : integer;
      begin
        if assigned(FHashArray) then
         begin
           for i:=low(FHashArray^) to high(FHashArray^) do
            if assigned(FHashArray^[i]) then
             a(FHashArray^[i],arg);
         end
        else
         if assigned(FRoot) then
          a(FRoot,arg);
      end;


    function Tdictionary.insert(obj:TNamedIndexItem):TNamedIndexItem;
      begin
        obj.FSpeedValue:=GetSpeedValue(obj.FName^);
        if assigned(FHashArray) then
         insert:=insertNode(obj,FHashArray^[obj.SpeedValue mod hasharraysize])
        else
         insert:=insertNode(obj,FRoot);
      end;


    function tdictionary.insertNode(NewNode:TNamedIndexItem;var currNode:TNamedIndexItem):TNamedIndexItem;
      begin
        if currNode=nil then
         begin
           currNode:=NewNode;
           insertNode:=NewNode;
         end
        { First check SpeedValue, to allow a fast insert }
        else
         if currNode.SpeedValue>NewNode.SpeedValue then
          insertNode:=insertNode(NewNode,currNode.FRight)
        else
         if currNode.SpeedValue<NewNode.SpeedValue then
          insertNode:=insertNode(NewNode,currNode.FLeft)
        else
         begin
           if currNode.FName^>NewNode.FName^ then
            insertNode:=insertNode(NewNode,currNode.FRight)
           else
            if currNode.FName^<NewNode.FName^ then
             insertNode:=insertNode(NewNode,currNode.FLeft)
           else
            begin
              if (replace_existing or delete_doubles) and
                 assigned(currNode) then
                begin
                  NewNode.FLeft:=currNode.FLeft;
                  NewNode.FRight:=currNode.FRight;
                  if delete_doubles then
                    begin
                      currnode.FLeft:=nil;
                      currnode.FRight:=nil;
                      currnode.free;
                    end;
                  currNode:=NewNode;
                  insertNode:=NewNode;
                end
              else
               insertNode:=currNode;
             end;
         end;
      end;


    procedure tdictionary.inserttree(currtree,currroot:TNamedIndexItem);
      begin
        if assigned(currtree) then
         begin
           inserttree(currtree.FLeft,currroot);
           inserttree(currtree.FRight,currroot);
           currtree.FRight:=nil;
           currtree.FLeft:=nil;
           insertNode(currtree,currroot);
         end;
      end;


    function tdictionary.rename(const olds,News : string):TNamedIndexItem;
      var
        spdval : cardinal;
        lasthp,
        hp,hp2,hp3 : TNamedIndexItem;
      begin
        spdval:=GetSpeedValue(olds);
        if assigned(FHashArray) then
         hp:=FHashArray^[spdval mod hasharraysize]
        else
         hp:=FRoot;
        lasthp:=nil;
        while assigned(hp) do
          begin
            if spdval>hp.SpeedValue then
             begin
               lasthp:=hp;
               hp:=hp.FLeft
             end
            else
             if spdval<hp.SpeedValue then
              begin
                lasthp:=hp;
                hp:=hp.FRight
              end
            else
             begin
               if (hp.FName^=olds) then
                begin
                  { Get in hp2 the replacer for the root or hasharr }
                  hp2:=hp.FLeft;
                  hp3:=hp.FRight;
                  if not assigned(hp2) then
                   begin
                     hp2:=hp.FRight;
                     hp3:=hp.FLeft;
                   end;
                  { remove entry from the tree }
                  if assigned(lasthp) then
                   begin
                     if lasthp.FLeft=hp then
                      lasthp.FLeft:=hp2
                     else
                      lasthp.FRight:=hp2;
                   end
                  else
                   begin
                     if assigned(FHashArray) then
                      FHashArray^[spdval mod hasharraysize]:=hp2
                     else
                      FRoot:=hp2;
                   end;
                  { reinsert the hp3 in the tree from hp2 }
                  inserttree(hp3,hp2);
                  { reset Node with New values }
                  hp.FLeft:=nil;
                  hp.FRight:=nil;
                  stringdispose(hp.FName);
                  hp.FName:=stringdup(newS);
                  hp.FSpeedValue:=GetSpeedValue(newS);
                  { reinsert }
                  if assigned(FHashArray) then
                   rename:=insertNode(hp,FHashArray^[hp.SpeedValue mod hasharraysize])
                  else
                   rename:=insertNode(hp,FRoot);
                  exit;
                end
               else
                if olds>hp.FName^ then
                 begin
                   lasthp:=hp;
                   hp:=hp.FLeft
                 end
                else
                 begin
                   lasthp:=hp;
                   hp:=hp.FRight;
                 end;
             end;
          end;
      end;


    function Tdictionary.search(const s:string):TNamedIndexItem;
      begin
        search:=speedsearch(s,GetSpeedValue(s));
      end;


    function Tdictionary.speedsearch(const s:string;SpeedValue:cardinal):TNamedIndexItem;
      var
        NewNode:TNamedIndexItem;
      begin
        if assigned(FHashArray) then
         NewNode:=FHashArray^[SpeedValue mod hasharraysize]
        else
         NewNode:=FRoot;
        while assigned(NewNode) do
         begin
           if SpeedValue>NewNode.SpeedValue then
            NewNode:=NewNode.FLeft
           else
            if SpeedValue<NewNode.SpeedValue then
             NewNode:=NewNode.FRight
           else
            begin
              if (NewNode.FName^=s) then
               begin
                 speedsearch:=NewNode;
                 exit;
               end
              else
               if s>NewNode.FName^ then
                NewNode:=NewNode.FLeft
              else
               NewNode:=NewNode.FRight;
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
        hp,hp2 : TNamedIndexItem;
      begin
        hp:=First;
        while assigned(hp) do
         begin
           hp2:=hp;
           hp:=hp.FListNext;
           hp2.free;
         end;
        First:=nil;
        last:=nil;
      end;


    procedure tsingleList.insert(p:TNamedIndexItem);
      begin
        if not assigned(First) then
         First:=p
        else
         last.FListNext:=p;
        last:=p;
        p.FListNext:=nil;
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


    function tindexarray.search(nr:integer):TNamedIndexItem;
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


    procedure tindexarray.foreach(proc2call : Tnamedindexcallback;arg:pointer);
      var
        i : integer;
      begin
        for i:=1 to count do
         if assigned(data^[i]) then
          proc2call(data^[i],arg);
      end;


    procedure tindexarray.foreach_static(proc2call : Tnamedindexstaticcallback;arg:pointer);
      var
        i : integer;
      begin
        for i:=1 to count do
         if assigned(data^[i]) then
          proc2call(data^[i],arg);
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


    procedure tindexarray.deleteindex(p:TNamedIndexItem);
      var
        i : integer;
      begin
        i:=p.Findexnr;
        { update counter }
        if i=count then
         dec(count);
        { update Linked List }
        while (i>0) do
         begin
           dec(i);
           if (i>0) and assigned(data^[i]) then
            begin
              data^[i].FindexNext:=data^[p.Findexnr].FindexNext;
              break;
            end;
         end;
        if i=0 then
         First:=p.FindexNext;
        data^[p.FIndexnr]:=nil;
        { clear entry }
        p.FIndexnr:=-1;
        p.FIndexNext:=nil;
      end;


    procedure tindexarray.delete(var p:TNamedIndexItem);
      begin
        deleteindex(p);
        p.free;
        p:=nil;
      end;


    procedure tindexarray.insert(p:TNamedIndexItem);
      var
        i  : integer;
      begin
        if p.FIndexnr=-1 then
         begin
           inc(count);
           p.FIndexnr:=count;
         end;
        if p.FIndexnr>count then
         count:=p.FIndexnr;
        if count>size then
         grow(((count div growsize)+1)*growsize);
        Assert(not assigned(data^[p.FIndexnr]) or (p=data^[p.FIndexnr]));
        data^[p.FIndexnr]:=p;
        { update Linked List backward }
        i:=p.FIndexnr;
        while (i>0) do
         begin
           dec(i);
           if (i>0) and assigned(data^[i]) then
            begin
              data^[i].FIndexNext:=p;
              break;
            end;
         end;
        if i=0 then
         First:=p;
        { update Linked List forward }
        i:=p.FIndexnr;
        while (i<=count) do
         begin
           inc(i);
           if (i<=count) and assigned(data^[i]) then
            begin
              p.FIndexNext:=data^[i];
              exit;
            end;
         end;
        if i>count then
         p.FIndexNext:=nil;
      end;


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
        if (i<FPosnblock^.pos) or (i>=FPosnblock^.pos+blocksize) then
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


    procedure tdynamicarray.readstream(f:TCStream;maxlen:longint);
      var
        i,left : integer;
      begin
        if maxlen=-1 then
         maxlen:=maxlongint;
        repeat
          left:=blocksize-FPosnblock^.used;
          if left>maxlen then
           left:=maxlen;
          i:=f.Read(FPosnblock^.data[FPosnblock^.used],left);
          dec(maxlen,i);
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
        until (i<left) or (maxlen=0);
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
  Revision 1.13  2002-05-18 13:34:05  peter
    * readded missing revisions

  Revision 1.12  2002/05/16 19:46:35  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.10  2002/05/12 16:53:04  peter
    * moved entry and exitcode to ncgutil and cgobj
    * foreach gets extra argument for passing local data to the
      iterator function
    * -CR checks also class typecasts at runtime by changing them
      into as
    * fixed compiler to cycle with the -CR option
    * fixed stabs with elf writer, finally the global variables can
      be watched
    * removed a lot of routines from cga unit and replaced them by
      calls to cgobj
    * u32bit-s32bit updates for and,or,xor nodes. When one element is
      u32bit then the other is typecasted also to u32bit without giving
      a rangecheck warning/error.
    * fixed pascal calling method with reversing also the high tree in
      the parast, detected by tcalcst3 test

}
