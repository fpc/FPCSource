{
    Copyright (c) 2001 by Peter Vreman

    TDictionary class

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$ifdef fpc}{$mode objfpc}{$endif}
{$H+}
unit fpcmdic;
interface

    const       { the real size will be [-hasharray..hasharray] ! }
       hasharraysize = 2047;

    type
       { namedindexobect for use with dictionary and indexarray }
       TDictionaryItem=class
       private
         Fname      : string;
         FSpeedValue : cardinal;
       protected
         procedure SetName(const n:string);
       public
         left,
         right : TDictionaryItem;
         constructor create(const n:string);
         property Name:string read FName write SetName;
         property SpeedValue:cardinal read FSpeedValue;
       end;

       Pdictionaryhasharray=^Tdictionaryhasharray;
       Tdictionaryhasharray=array[-hasharraysize..hasharraysize] of TDictionaryItem;

       Tnamedindexcallback = procedure(p:TDictionaryItem) of object;

       Tdictionary=class
       private
         FRoot      : TDictionaryItem;
         FHashArray : Pdictionaryhasharray;
         procedure cleartree(obj:TDictionaryItem);
         function  insertNode(NewNode:TDictionaryItem;var currNode:TDictionaryItem):TDictionaryItem;
         procedure inserttree(currtree,currroot:TDictionaryItem);
       public
         noclear   : boolean;
         replace_existing : boolean;
         constructor Create;
         destructor  Destroy;override;
         procedure usehash;
         procedure clear;
         function  delete(const s:string):TDictionaryItem;
         function  empty:boolean;
         procedure foreach(proc2call:Tnamedindexcallback);
         function  insert(obj:TDictionaryItem):TDictionaryItem;
         function  rename(const olds,News : string):TDictionaryItem;
         function  search(const s:string):TDictionaryItem;
         function  speedsearch(const s:string;SpeedValue:Cardinal):TDictionaryItem;
         property  Items[const s:string]:TDictionaryItem read Search;default;
       end;

    { Speed/Hash value }
    Function GetSpeedValue(Const s:String):cardinal;


implementation


{*****************************************************************************
                               GetSpeedValue
*****************************************************************************}

var
  Crc32Tbl : array[0..255] of cardinal;

procedure MakeCRC32Tbl;
var
  crc : cardinal;
  i,n : integer;
begin
  for i:=0 to 255 do
   begin
     crc:=i;
     for n:=1 to 8 do
      if odd(crc) then
       crc:=(crc shr 1) xor $edb88320
      else
       crc:=crc shr 1;
     Crc32Tbl[i]:=crc;
   end;
end;


Function GetSpeedValue(Const s:String):cardinal;
var
  i : integer;
  InitCrc : cardinal;
begin
  if Crc32Tbl[1]=0 then
   MakeCrc32Tbl;
  InitCrc:=$ffffffff;
  for i:=1 to Length(s) do
   InitCrc:=Crc32Tbl[byte(InitCrc) xor ord(s[i])] xor (InitCrc shr 8);
  GetSpeedValue:=InitCrc;
end;


{****************************************************************************
                               TDictionaryItem
 ****************************************************************************}

constructor TDictionaryItem.Create(const n:string);
begin
  left:=nil;
  right:=nil;
  FSpeedValue:=$ffffffff;
  FName:=n;
end;


procedure TDictionaryItem.Setname(const n:string);
begin
  if FSpeedValue=$ffffffff then
   FName:=n;
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


    destructor Tdictionary.destroy;
      begin
        if not noclear then
         clear;
        if assigned(FHashArray) then
         dispose(FHashArray);
      end;


    procedure Tdictionary.cleartree(obj:TDictionaryItem);
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
        if assigned(FRoot) then
          cleartree(FRoot);
        if assigned(FHashArray) then
         for w:=-hasharraysize to hasharraysize do
          if assigned(FHashArray^[w]) then
           cleartree(FHashArray^[w]);
      end;


    function Tdictionary.delete(const s:string):TDictionaryItem;
    var
      p,SpeedValue : cardinal;
      n : TDictionaryItem;

        procedure insert_right_bottom(var root,Atree:TDictionaryItem);
        begin
          while root.right<>nil do
           root:=root.right;
          root.right:=Atree;
        end;

        function delete_from_tree(root:TDictionaryItem):TDictionaryItem;
        type
          leftright=(left,right);
        var
          lr : leftright;
          oldroot : TDictionaryItem;
        begin
          oldroot:=nil;
          while (root<>nil) and (root.SpeedValue<>SpeedValue) do
           begin
             oldroot:=root;
             if SpeedValue<root.SpeedValue then
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
          while (root<>nil) and (root.name<>s) do
           begin
             oldroot:=root;
             if s<root.name then
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
      SpeedValue:=GetSpeedValue(s);
      n:=FRoot;
      if assigned(FHashArray) then
       begin
         { First, check if the Node to delete directly located under
           the hasharray.}
         p:=SpeedValue mod hasharraysize;
         n:=FHashArray^[p];
         if (n<>nil) and (n.SpeedValue=SpeedValue) and
            (n.name=s) then
          begin
            { The Node to delete is directly located under the
              hasharray. Make the hasharray point to the left
              subtree of the Node and place the right subtree on
              the right-bottom of the left subtree.}
            if n.left<>nil then
             begin
               FHashArray^[p]:=n.left;
               if n.right<>nil then
                insert_right_bottom(n.left,n.right);
             end
            else
             FHashArray^[p]:=n.right;
            delete:=n;
            exit;
          end;
       end
      else
       begin
         { First check if the Node to delete is the root.}
         if (FRoot<>nil) and (n.SpeedValue=SpeedValue) and
            (n.name=s) then
          begin
            if n.left<>nil then
             begin
               FRoot:=n.left;
               if n.right<>nil then
                insert_right_bottom(n.left,n.right);
             end
            else
             FRoot:=n.right;
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
           for w:=-hasharraysize to hasharraysize do
            if assigned(FHashArray^[w]) then
             exit;
           empty:=true;
         end
        else
         empty:=(FRoot=nil);
      end;


    procedure Tdictionary.foreach(proc2call:Tnamedindexcallback);

        procedure a(p:TDictionaryItem);
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
        if assigned(FHashArray) then
         begin
           for i:=-hasharraysize to hasharraysize do
            if assigned(FHashArray^[i]) then
             a(FHashArray^[i]);
         end
        else
         if assigned(FRoot) then
          a(FRoot);
      end;


    function Tdictionary.insert(obj:TDictionaryItem):TDictionaryItem;
      begin
        obj.FSpeedValue:=GetSpeedValue(obj.name);
        if assigned(FHashArray) then
         insert:=insertNode(obj,FHashArray^[obj.SpeedValue mod hasharraysize])
        else
         insert:=insertNode(obj,FRoot);
      end;


    function tdictionary.insertNode(NewNode:TDictionaryItem;var currNode:TDictionaryItem):TDictionaryItem;
      begin
        if currNode=nil then
         begin
           currNode:=NewNode;
           insertNode:=NewNode;
         end
        { First check SpeedValue, to allow a fast insert }
        else
         if currNode.SpeedValue>NewNode.SpeedValue then
          insertNode:=insertNode(NewNode,currNode.right)
        else
         if currNode.SpeedValue<NewNode.SpeedValue then
          insertNode:=insertNode(NewNode,currNode.left)
        else
         begin
           if currNode.name>NewNode.name then
            insertNode:=insertNode(NewNode,currNode.right)
           else
            if currNode.name<NewNode.name then
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


    procedure tdictionary.inserttree(currtree,currroot:TDictionaryItem);
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


    function tdictionary.rename(const olds,News : string):TDictionaryItem;
      var
        spdval : Cardinal;
        lasthp,
        hp,hp2,hp3 : TDictionaryItem;
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
               hp:=hp.left
             end
            else
             if spdval<hp.SpeedValue then
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
                     if assigned(FHashArray) then
                      FHashArray^[spdval mod hasharraysize]:=hp2
                     else
                      FRoot:=hp2;
                   end;
                  { reinsert the hp3 in the tree from hp2 }
                  inserttree(hp3,hp2);
                  { reset Node with New values }
                  hp.name:=newS;
                  hp.FSpeedValue:=GetSpeedValue(newS);
                  hp.left:=nil;
                  hp.right:=nil;
                  { reinsert }
                  if assigned(FHashArray) then
                   rename:=insertNode(hp,FHashArray^[hp.SpeedValue mod hasharraysize])
                  else
                   rename:=insertNode(hp,FRoot);
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


    function Tdictionary.search(const s:string):TDictionaryItem;
      begin
        search:=speedsearch(s,GetSpeedValue(s));
      end;


    function Tdictionary.speedsearch(const s:string;SpeedValue:Cardinal):TDictionaryItem;
      var
        NewNode:TDictionaryItem;
      begin
        if assigned(FHashArray) then
         NewNode:=FHashArray^[SpeedValue mod hasharraysize]
        else
         NewNode:=FRoot;
        while assigned(NewNode) do
         begin
           if SpeedValue>NewNode.SpeedValue then
            NewNode:=NewNode.left
           else
            if SpeedValue<NewNode.SpeedValue then
             NewNode:=NewNode.right
           else
            begin
              if (NewNode.name=s) then
               begin
                 speedsearch:=NewNode;
                 exit;
               end
              else
               if s>NewNode.name then
                NewNode:=NewNode.left
              else
               NewNode:=NewNode.right;
            end;
         end;
        speedsearch:=nil;
      end;



end.
