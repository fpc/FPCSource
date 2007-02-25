{
    $Id: header,v 1.1 2000/07/13 06:33:45 michael Exp $
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2000 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
program poolmm2;

{$mode objfpc}

uses
  pooledmm;

type
  // basic tree node
  PNode = ^TNode;
  TNode = record
    l, r: PNode;
    Value: Longint;
  end;

  { TTree }

  TTree = class
  private
    FMM: TNonFreePooledMemManager;
    FRoot: PNode;
    function CreateNode(l2, r2: PNode; const AValue: Integer): PNode;
    function Make(AValue, depth: Integer): PNode;
  public
    constructor Create(AValue, depth: Integer);
    destructor Destroy;override;
    function Check: Integer;
  end;

function CheckNode(ANode: PNode): Integer;
begin
  if ANode^.l = nil then
    Result:=ANode^.Value
  else
    Result:=CheckNode(ANode^.l) + ANode^.Value - CheckNode(ANode^.r);
end;

{ TTree }

constructor TTree.Create(AValue, depth: Integer);
begin
  FMM := TNonFreePooledMemManager.Create(SizeOf(TNode));
  FRoot := Make(AValue, depth);
end;

destructor TTree.Destroy;
begin
  FMM.Free; // frees all nodes, so no need to free the nodes recursively
  inherited Destroy;
end;

function TTree.Check: Integer;
begin
  Result := CheckNode(FRoot);
end;

function TTree.CreateNode(l2, r2: PNode; const AValue: Integer): PNode;
begin
  // Normally one would do something like this:
  // Result := GetMem(Sizeof(TNode));
  // But now we ask the a new item from the NonFree memory manager.
  Result := FMM.NewItem();
  Result^.l:=l2;
  Result^.r:=r2;
  Result^.Value:=AValue;
end;

function TTree.Make(AValue, depth: Integer): PNode;
begin
  if depth = 0 then
    Result:=CreateNode(nil, nil, AValue)
  else
    Result:=CreateNode(Make(2 * AValue - 1, depth - 1), Make(2 * AValue, depth - 1), AValue);
end;

const
  MinDepth = 10;

var
  MaxDepth   : Integer;
  c, i       : Integer;
  aa, bb, llt: TTree;

begin
  MaxDepth := 12;
  if ParamCount = 1 then
    Val(ParamStr(1), MaxDepth);


  if (MinDepth + 2) > MaxDepth then
    MaxDepth := MinDepth + 2;

  // Create a tree of certain depth
  llt:=TTree.Create(0, MaxDepth);

  c := 0;
  for i:=1 to 100 do begin
    aa:=TTree.Create(i, MinDepth);
    bb:=TTree.Create(-i, MinDepth);
    c:=c + aa.Check + bb.Check;
    aa.Free;
    bb.Free;
  end;

  llt.Free;
end.
