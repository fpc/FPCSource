program gtreetest;

{$mode objfpc}{$H+}

uses
  gtree;

procedure WriteIntegerCallback(const i: Integer);
begin
  Write(i,' ');
end;

type
  TIntegerTreeNode = specialize TTreeNode<Integer>;
  TIntegerTree = specialize TTree<Integer>;
var
  Tree: TIntegerTree;
  Node,Tmp: TIntegerTreeNode;
  i: Integer;
begin
  Node := TIntegerTreeNode.Create(0);
  for i := 1 to 3 do begin
    Tmp := TIntegerTreeNode.Create(i);
    Node.Children.PushBack(Tmp);
  end;
  Tmp := Node;
  Node := TIntegerTreeNode.Create(4);
  Node.Children.PushBack(Tmp);
  for i := 5 to 7 do begin
    Tmp := TIntegerTreeNode.Create(i);
    Node.Children.PushBack(Tmp);
  end;
  Tmp := Node;
  Node := TIntegerTreeNode.Create(8);
  Node.Children.PushBack(Tmp);
  for i := 9 to 10 do begin
    Tmp := TIntegerTreeNode.Create(i);
    Node.Children.PushBack(Tmp);
  end;

  Tree := TIntegerTree.Create;
  Tree.Root := Node;

  WriteLn('Depth first:');
  Tree.DepthFirstTraverse(@WriteIntegerCallback);WriteLn;
  WriteLn('Breadth first:');
  Tree.BreadthFirstTraverse(@WriteIntegerCallback);WriteLn;

  Tree.Free;
end.
