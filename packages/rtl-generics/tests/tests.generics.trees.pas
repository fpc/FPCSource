{
    This file is part of the Free Pascal/NewPascal run time library.
    Copyright (c) 2018 by Maciej Izak (hnb),
    member of the NewPascal development team (http://newpascal.org)

    Copyright(c) 2004-2018 DaThoX

    It contains tests for the Free Pascal generics library

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Acknowledgment

    Thanks to Sphere 10 Software (http://sphere10.com) for sponsoring
    many new types, tests and major refactoring of entire library

 **********************************************************************}

unit tests.generics.trees;

{$mode delphi}

interface

uses
  fpcunit, testregistry, testutils, tests.generics.utils,
  Classes, SysUtils, Generics.Collections;

type

  { TTestArrayHelper }

  { TTestTrees }

  TTestTrees = class(TTestCollections)
  published
    procedure Test_IndexedAVLTree_Add_General;
    procedure Test_IndexedAVLTree_Add;
    procedure Test_IndexedAVLTree_Delete;
    procedure Test_IndexedAVLTree_Items;

    procedure Test_TAVLTreeMap_Notification;
  end;

implementation

type
  TStringsTree = TIndexedAVLTree<string>;
  TMapTree = TAVLTreeMap<string, Integer>;

{ TTestTrees }

procedure TTestTrees.Test_IndexedAVLTree_Add_General;
const
  _COUNT = 999;
var
  LNumbers: THashSet<Integer>;
  i, j: Integer;
  LTree: TStringsTree;
  LNodes: TList<TStringsTree.PNode>;
  n: TStringsTree.PNode;
begin
  LNumbers := THashSet<Integer>.Create;
  LTree := TStringsTree.Create;
  LNodes := TList<TStringsTree.PNode>.Create;

  try
    // check consistency of adding new nodes to Indexed AVL
    for i := 0 to _COUNT do
    begin
      LNodes.Add(LTree.Add('0'+i.ToString));
      LNumbers.Clear;
      for n in LTree.Nodes do
        Check(LNumbers.Add(LTree.NodeToIndex(n)), 'Wrong index (duplicate) of '+ i.ToString + ' for node ' + n.Key);
      for j := 0 to LNodes.Count - 1 do
        Check(LNumbers.Contains(j), 'Missing index ' + j.ToString + ' for i = ' + i.ToString);
      LTree.ConsistencyCheck;
      CheckEquals(i+1, LTree.Count, 'Wrong tree count');
    end;
  finally
    LNodes.Free;
    LTree.Free;
    LNumbers.Free;
  end;
end;

procedure TTestTrees.Test_IndexedAVLTree_Add;
var
  LTree: TStringsTree;
begin
  LTree := TStringsTree.Create;

  try
    LTree.Duplicates:=dupAccept;
    LTree.Add('Aaa');
  finally
    LTree.Free;
  end;
end;

procedure TTestTrees.Test_IndexedAVLTree_Delete;
const
  _COUNT = 999;
var
  LNumbers: THashSet<Integer>;
  i, j: Integer;
  LTree: TStringsTree;
  LNodes: TList<TStringsTree.PNode>;
  n: TStringsTree.PNode;
begin
  LNumbers := THashSet<Integer>.Create;
  LTree := TStringsTree.Create;
  LNodes := TList<TStringsTree.PNode>.Create;

  try
    for i := 0 to _COUNT do
      LNodes.Add(LTree.Add('0'+i.ToString));

    // check consistency of deleting nodes from Indexed AVL
    for i := 0 to _COUNT do
    begin
      LTree.Delete(LNodes.ExtractIndex(Random(LNodes.count)));
      LNumbers.Clear;
      for n in LTree.Nodes do
        Check(LNumbers.Add(LTree.NodeToIndex(n)), 'Wrong index (duplicate) of '+ i.ToString + ' for node ' + n.Key);
      for j := 0 to LNodes.Count - 1 do
        Check(LNumbers.Contains(j), 'Missing index ' + j.ToString + ' for i = ' + i.ToString);
      LTree.ConsistencyCheck;
      CheckEquals(_COUNT-i, LTree.Count, 'Wrong tree count');
    end;
  finally
    LNodes.Free;
    LTree.Free;
    LNumbers.Free;
  end;
end;

procedure TTestTrees.Test_IndexedAVLTree_Items;
var
  LTree: TMapTree;
begin
  LTree := TMapTree.Create;
  try
    Check(LTree.Add('A', 1)<>nil);
    Check(LTree.Add('B', 2)<>nil);
    Check(LTree.Add('C', 3)<>nil);
    CheckEquals(LTree.Items['A'], 1);
    CheckEquals(LTree.Items['B'], 2);
    CheckEquals(LTree.Items['C'], 3);
    LTree.Items['A'] := 4;
    LTree.Items['B'] := 5;
    LTree.Items['C'] := 6;
    CheckEquals(LTree.Items['A'], 4);
    CheckEquals(LTree.Items['B'], 5);
    CheckEquals(LTree.Items['C'], 6);
  finally
    LTree.Free;
  end;
end;

procedure TTestTrees.Test_TAVLTreeMap_Notification;
var
  LTree: TAVLTreeMap<string, string>;
  LNode, LA, LC: TAVLTreeMap<string, string>.PNode;
begin
  LTree := TAVLTreeMap<string, string>.Create;
  LTree.OnKeyNotify := NotifyTestStr;
  LTree.OnValueNotify := NotifyTestStr;
  LTree.OnNodeNotify := NotifyTestNodeStr;
  try
    // simple add
    NotificationAdd(LTree, ['Aaa', 'Bbb'], cnAdded);
    NotificationAdd(LTree, 'Aaa', 'Bbb', nil, cnAdded, false, true);
    LA := LTree.Add('Aaa', 'Bbb');
    AssertNotificationsExecutedNodeStr;
    AssertNotificationsExecutedStr;

    // pair add
    NotificationAdd(LTree, ['Ccc', 'Ddd'], cnAdded);
    NotificationAdd(LTree, 'Ccc', 'Ddd', nil, cnAdded, false, true);
    LC := LTree.Add(TAVLTreeMap<string, string>.TTreePair.Create('Ccc', 'Ddd'));
    AssertNotificationsExecutedNodeStr;
    AssertNotificationsExecutedStr;

    // AddNode;
    LNode := LTree.NewNode;
    LNode.Key := 'Eee';
    LNode.Value := 'Fff';
    NotificationAdd(LTree, ['Eee', 'Fff'], cnAdded);
    NotificationAdd(LTree, 'Eee', 'Fff', LNode, cnAdded, false, false);
    AssertTrue(LTree.AddNode(LNode));
    AssertNotificationsExecutedNodeStr;
    AssertNotificationsExecutedStr;

    // Delete
    NotificationAdd(LTree, ['Eee', 'Fff'], cnRemoved);
    NotificationAdd(LTree, 'Eee', 'Fff', LNode, cnRemoved, false, false);
    LTree.Delete(LNode, false);
    AssertNotificationsExecutedNodeStr;
    AssertNotificationsExecutedStr;
    LTree.DisposeNode(LNode);

    // remove
    NotificationAdd(LTree, ['Aaa', 'Bbb'], cnRemoved);
    NotificationAdd(LTree, 'Aaa', 'Bbb', LA, cnRemoved, true, false);
    LTree.Remove('Aaa');
    AssertNotificationsExecutedNodeStr;
    AssertNotificationsExecutedStr;


    // free
    NotificationAdd(LTree, ['Ccc', 'Ddd'], cnRemoved);
    NotificationAdd(LTree, 'Ccc', 'Ddd', LC, cnRemoved, true, false);
  finally
    LTree.Free;
    AssertNotificationsExecutedNodeStr;
    AssertNotificationsExecutedStr;
  end;
end;

begin
  RegisterTest(TTestTrees);
end.

