{ The Great Computer Language Shootout
  http://shootout.alioth.debian.org

  contributed by Ales Katona
}

program BinaryTrees;

{$mode objfpc}

type
  PNode = ^TNode;
  TNode = record
    l, r: PNode;
    i: Longint;
  end;

function CreateNode(l2, r2: PNode; const i2: Longint): PNode;
begin
  Result := GetMem(SizeOf(TNode));
  Result^.l:=l2;
  Result^.r:=r2;
  Result^.i:=i2;
end;

procedure DestroyNode(ANode: PNode);
begin
  if ANode^.l <> nil then begin
    DestroyNode(ANode^.l);
    DestroyNode(ANode^.r);
  end;
  FreeMem(ANode, SizeOf(TNode));
end;

function CheckNode(ANode: PNode): Longint;
begin
  if ANode^.l = nil then
    Result:=ANode^.i
  else
    Result:=CheckNode(ANode^.l) + ANode^.i - CheckNode(ANode^.r);
end;

function Make(i, d: Longint): PNode;
begin
  if d = 0 then Result:=CreateNode(nil, nil, i)
  else Result:=CreateNode(Make(2 * i - 1, d - 1), Make(2 * i, d - 1), i);
end;

const
  mind = 4;

var
  maxd : Longint = 10;
  strd,
  iter,
  c, d, i : Longint;
  tree, llt : PNode;

begin
  if ParamCount = 1 then
    Val(ParamStr(1), maxd);

  if maxd < mind+2 then
     maxd := mind + 2;

  strd:=maxd + 1;
  tree:=Make(0, strd);
  Writeln('stretch tree of depth ', strd, #9' check: ', CheckNode(tree));
  DestroyNode(tree);

  llt:=Make(0, maxd);

  d:=mind;
  while d <= maxd do begin
    iter:=1 shl (maxd - d + mind);
    c:=0;
    for i:=1 to Iter do begin
      tree:=Make(i, d);
      c:=c + CheckNode(tree);
      DestroyNode(tree);
      tree:=Make(-i, d);
      c:=c + CheckNode(tree);
      DestroyNode(tree);
    end;
    Writeln(2 * Iter, #9' trees of depth ', d, #9' check: ', c);
    Inc(d, 2);
  end;

  Writeln('long lived tree of depth ', maxd, #9' check: ', CheckNode(llt));
  DestroyNode(llt);
end.
