Program AvlTreeTest;
{
    This file is a demo of the Free Component Library (FCL)
    Copyright (c) 2009 by Marco van de Voort.
  
    A demo/test of straightforward unit Avl_Tree usage.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright. Alternately you may also
    use this file under a BSD license.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
// Simple test of avl_tree unit.
// It adds 1000 randomly generated numbers to both a TBits and an avltree,
//  no dupes are allowed in the avltree. Then it compares and deallocates.
//
// While doing this, it counts the total number of compares.


{$mode ObjFPC}{$H+}

Uses avl_tree,Sysutils,Classes;

Const 
    NumberOfValues = 10000;

Type TDataObject = Class
		    Name:String;
                    value : integer; 
		   end;

Var objcompares : integer =0;
    keycompares : integer =0;

Function CompareProcObj(Node1, Node2: Pointer): integer;
begin
  inc(objcompares);
  Result := CompareStr(TDataObject(Node2).name,TDataObject(Node1).name);
end;

Function CompareProcKey(Node1:pointer; Node2: Pointer): integer;
begin
  inc(keycompares);
  Result := CompareStr(TDataObject(Node2).name,ansistring(Node1));
end;

var
   Tree      : TAVLTree;
   i,value   : Integer;
   valueStr  : String;
   Allocated : TBits;
   obj       : TDataObject;
   AVLNode   : TAVLTreeNode;
begin
 Randomize; 
 Tree :=TAVLTree.Create(@CompareProcObj);
 Allocated:=TBits.Create(NumberOfValues);

 // note that the compareproc is different from above.
 For I:=0 to NumberOfValues-1 do
  begin
    value:=Random(NumberOfValues);
    valueStr:=inttostr(Value);  
    If not assigned(Tree.FindKey(pointer(valueStr),@CompareProcKey)) Then
      begin
        obj:=TDataObject.Create;
        obj.name:=ValueStr;
        obj.value:=value;
        Tree.Add(Obj);
        Allocated[value]:=true;
      end;
  end;
  // Key compares (which should be about sum(n=1..NumberOfValues,log(n)/log(2))
  //   seems to be about 2log(n)-2. I haven't calculated the limit yet.
  writeln(Tree.Count, ' unique nodes in the tree');
  Writeln('object compares (insert):',objcompares,' (/Tree.count): ',floattostrf(objcompares/tree.count,fffixed,10,2));
  Writeln('key    compares (find  ):',keycompares,' (/',NumberOfValues,'): ',floattostrf(keycompares/NumberOfValues,fffixed,10,2));

  // iterating and comparing with the TBits.
  AVLNode:=Tree.FindLowest;
  while (AVLNode<>nil) do 
     begin
       value:=TDataObject(AVLNode.Data).value;
       if not Allocated[value] then
         writeln('Oops, missed:',value);
      AVLNode:=Tree.FindSuccessor(AVLNode)
     end;

  // Iterating is compareless as it should be, despite 
  //   the "FINDsuccessor" method name.
  Writeln('object compares (insert):',objcompares);
  Writeln('key    compares (find  ):',keycompares);

  // Clean up.

  Tree.FreeAndClear;
  FreeAndNil(Tree);
  FreeAndNil(Allocated);

End.