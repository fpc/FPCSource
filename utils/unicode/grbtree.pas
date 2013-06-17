{   Red Black Tree implementation.

    Copyright (c) 2013 by Inoussa OUEDRAOGO

    Inspired by ideas of Julienne Walker
      see http://www.eternallyconfuzzled.com/tuts/datastructures/jsw_tut_bst1.aspx

    The source code is distributed under the Library GNU
    General Public License with the following modification:

        - object files and libraries linked into an application may be
          distributed without source code.

    If you didn't receive a copy of the file COPYING, contact:
          Free Software Foundation
          675 Mass Ave
          Cambridge, MA  02139
          USA

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. }

unit grbtree;

{$ifdef FPC}
  {$mode delphi}
  {$H+}
{$endif FPC}
{$TYPEDADDRESS ON}
{$define RB_DEBUG}

interface

const
  HEIGHT_LIMIT = 64;

type


   {KCOMP = class
    public
      // Return
      //    * if A>B then  1
      //    * if A=B then  0
      //    * if A<B then -1
      class function Compare(const A, B : TRBTreeNodeData) : Integer;
    end;    }

  TRBTree<T, KCOMP> = class
  public type
    TRBTreeNodeData = T;
    PRBTreeNode = ^TRBTreeNode;
    PRBTreeAllocator = ^TRBTreeAllocator;
    TRBTreeNode = record
      Links : array[Boolean] of PRBTreeNode;
      Data  : TRBTreeNodeData;
      Red   : Boolean;
    end;
    TRBTreeNodeCreator = function(AContext : Pointer) : PRBTreeNode;
    TRBTreeNodeDestructor = procedure(ANode : PRBTreeNode; AContext : Pointer);
    TRBTreeAllocator = record
      CreateNode : TRBTreeNodeCreator;
      FreeNode   : TRBTreeNodeDestructor;
    end;

    TRBTreeNodeComparator = KCOMP;
    ThisType = TRBTree<T,KCOMP>;

  private type
    TBaseIterator = record
      Tree         : ThisType;
      StartingNode : PRBTreeNode;
      StartingDir  : Boolean;
      Current      : PRBTreeNode;
      Top          : NativeInt;
      Path         : array[0..(HEIGHT_LIMIT-1)] of PRBTreeNode;
    end;
    PBaseIterator = ^TBaseIterator;
  public type

    TIterator = class
    private
      FHandle : PBaseIterator;
      FResetState : Boolean;
    private
    public
      constructor Create(AHandle : PBaseIterator);
      destructor Destroy;override;
      procedure Reset();
      function MoveNext() : Boolean;inline;
      function MovePrevious() : Boolean;inline;
      function GetCurrent : TRBTreeNodeData;inline;
      function GetCurrentNode : PRBTreeNode;inline;
    end;

  public var
    Root       : PRBTreeNode;
    //FSize      : Integer;
    Allocator  : TRBTreeAllocator;
    Comparator : TRBTreeNodeComparator;
  private
    class function TreeCreateIterator() : PBaseIterator;static;inline;
    class procedure TreeFreeIterator(AItem : PBaseIterator);static;inline;
    class procedure TreeInitIterator(
            AIterator     : PBaseIterator;
      const ATree         : ThisType;
      const AStartingNode : PRBTreeNode;
      const ADirection    : Boolean
    );static;
    class function TreeIteratorMove(
      AIterator     : PBaseIterator;
      ADirection    : Boolean
    ) : PRBTreeNode;static;
    class function TreeIteratorMoveNext(AIterator : PBaseIterator) : PRBTreeNode;static;inline;
    class function TreeIteratorMovePrevious(AIterator : PBaseIterator) : PRBTreeNode;static;inline;
    function CreateIterator(
      const ANode      : PRBTreeNode;
      const ADirection : Boolean
    ) : TIterator;inline;
  private
    class function DefaultCreateNode(AContext : Pointer) : PRBTreeNode;static;
    class procedure DefaultFreeNode(ANode : PRBTreeNode; AContext : Pointer);static;

    function InitNode(ANode : PRBTreeNode; AData : TRBTreeNodeData) : PRBTreeNode;inline;
    function IsRed(ANode : PRBTreeNode): Boolean;inline;
    function RotateDouble(ARoot : PRBTreeNode; const ADir : Boolean) : PRBTreeNode;inline;
    function RotateSingle(ARoot : PRBTreeNode; const ADir : Boolean) : PRBTreeNode;
  public
    constructor Create(const AAllocator  : PRBTreeAllocator);overload;
    constructor Create();overload;
    destructor Destroy;override;
    procedure Clear();
    function FindNode(const AData : TRBTreeNodeData) : PRBTreeNode;
    function Insert(const AData : TRBTreeNodeData) : PRBTreeNode;
    function Remove(const AData : TRBTreeNodeData) : Boolean;
    function CreateForwardIterator(const ANode : PRBTreeNode) : TIterator;overload;inline;
    function CreateForwardIterator() : TIterator;overload;inline;
    function CreateBackwardIterator(const ANode : PRBTreeNode) : TIterator;overload;inline;
    function CreateBackwardIterator() : TIterator;overload;inline;
{$ifdef RB_DEBUG}
    function SelfAssert(ARoot : PRBTreeNode; var AErrorMessage : string) : Boolean;overload;
    function SelfAssert(var AErrorMessage : string) : Boolean;overload;
{$endif RB_DEBUG}
  end;

  TOrdinalComparator<T> = class
  public type
    TOrdinalType = T;
  public
    // Return
    //    * if A>B then  1
    //    * if A=B then  0
    //    * if A<B then -1
    class function Compare(const A, B : TOrdinalType) : Integer;static;inline;
  end;

implementation

{ TRBTree<T> }

function TRBTree<T,KCOMP>.IsRed(ANode : PRBTreeNode): Boolean;inline;
begin
  Result := (ANode <> nil) and ANode^.Red;
end;

function TRBTree<T,KCOMP>.InitNode(ANode: PRBTreeNode; AData: TRBTreeNodeData): PRBTreeNode;inline;
begin
  Result := ANode;
  Result^.Data := AData;
  Result^.Red := True;
  Result^.Links[False] := nil;
  Result^.Links[True] := nil;
end;

function TRBTree<T,KCOMP>.RotateDouble(ARoot: PRBTreeNode; const ADir: Boolean): PRBTreeNode;inline;
begin
  ARoot^.Links[not ADir] := RotateSingle(ARoot^.Links[not ADir], not ADir );
  Result := RotateSingle(ARoot,ADir);
end;

function TRBTree<T,KCOMP>.RotateSingle(ARoot: PRBTreeNode; const ADir: Boolean): PRBTreeNode;
var
  t : PRBTreeNode;
begin
  t := ARoot^.Links[not ADir];

  ARoot^.Links[not ADir] := t^.Links[ADir];
  t^.Links[ADir] := ARoot;

  ARoot^.Red := True;
  t^.Red := False;

  Result := t;
end;

class function TRBTree<T,KCOMP>.TreeCreateIterator() : PBaseIterator;static;
begin
  Result := AllocMem(SizeOf(TBaseIterator));
end;

class procedure TRBTree<T,KCOMP>.TreeFreeIterator(AItem : PBaseIterator);static;
begin
  if (AItem <> nil) then
    FreeMem(AItem,SizeOf(AItem^));
end;

class procedure TRBTree<T,KCOMP>.TreeInitIterator(
        AIterator     : PBaseIterator;
  const ATree         : ThisType;
  const AStartingNode : PRBTreeNode;
  const ADirection    : Boolean
);static;
begin
  AIterator^.Tree := ATree;
  AIterator^.StartingNode := AStartingNode;
  AIterator^.StartingDir := ADirection;
  if (AStartingNode = nil) then
    AIterator^.Current := AIterator^.Tree.Root
  else
    AIterator^.Current := AStartingNode;
  AIterator^.Top := 0;

  // Save the path for later traversal
  if (AIterator^.Current <> nil) then begin
    while (AIterator^.Current^.Links[ADirection] <> nil) do begin
      AIterator^.Path[AIterator^.Top] := AIterator^.Current;
      Inc(AIterator^.Top);
      AIterator^.Current := AIterator^.Current^.Links[ADirection];
    end;
  end;
end;

class function TRBTree<T,KCOMP>.TreeIteratorMove(
  AIterator  : PBaseIterator;
  ADirection : Boolean
) : PRBTreeNode;static;
var
  last : PRBTreeNode;
begin
  Result := nil;
  if (AIterator^.Current = nil) then
    exit;

  if (AIterator^.Current^.Links[ADirection] <> nil) then begin
    // Continue down this branch
    AIterator^.Path[AIterator^.Top] := AIterator^.Current;
    Inc(AIterator^.Top);
    AIterator^.Current := AIterator^.Current^.Links[ADirection];

    while ( AIterator^.Current^.Links[not ADirection] <> nil) do begin
      AIterator^.Path[AIterator^.Top] := AIterator^.Current;
      Inc(AIterator^.Top);
      AIterator^.Current := AIterator^.Current^.Links[not ADirection];
    end;
  end else begin
    // Move to the next branch
    repeat
      if (AIterator^.Top = 0) then begin
        AIterator^.Current := nil;
        break;
      end;

      last := AIterator^.Current;
      Dec(AIterator^.Top);
      AIterator^.Current := AIterator^.Path[AIterator^.Top];
    until (last <> AIterator^.Current^.Links[ADirection]);
  end;

  Result := AIterator^.Current;
end;

class function TRBTree<T,KCOMP>.TreeIteratorMoveNext(
  AIterator : PBaseIterator
) : PRBTreeNode;static;
begin
  Result := TreeIteratorMove(AIterator,True);
end;

class function TRBTree<T,KCOMP>.TreeIteratorMovePrevious(
  AIterator : PBaseIterator
) : PRBTreeNode;static;
begin
  Result := TreeIteratorMove(AIterator,False);
end;

function TRBTree<T,KCOMP>.CreateIterator(
  const ANode      : PRBTreeNode;
  const ADirection : Boolean
) : TIterator;
var
  h : PBaseIterator;
begin
  h := TreeCreateIterator();
  TreeInitIterator(h,Self,ANode,ADirection);
  Result := TIterator.Create(h);
end;

class function TRBTree<T,KCOMP>.DefaultCreateNode(AContext: Pointer): PRBTreeNode;
begin
  New(Result);
end;

class procedure TRBTree<T,KCOMP>.DefaultFreeNode(ANode: PRBTreeNode; AContext: Pointer);
begin
  Dispose(ANode);
end;

constructor TRBTree<T,KCOMP>.Create(const AAllocator  : PRBTreeAllocator);
begin
  Root := nil;
  Allocator := AAllocator^;
  //Comparator := TRBTreeNodeComparator.Create();
end;

constructor TRBTree < T, KCOMP > .Create();
var
  a : TRBTreeAllocator;
begin
  a.CreateNode := TRBTreeNodeCreator(DefaultCreateNode);
  a.FreeNode := TRBTreeNodeDestructor(DefaultFreeNode);
  Create(@a);
end;

destructor TRBTree<T,KCOMP>.Destroy;
begin
  Clear();
  //Comparator.Free();
  inherited;
end;

procedure TRBTree<T,KCOMP>.Clear();
var
  it, save : PRBTreeNode;
begin
  it := Root;

  while (it <> nil) do begin
    if (it^.Links[False] <> nil) then begin
      // Right rotation
      save := it^.Links[False];
      it^.Links[False] := save^.Links[True];
      save^.Links[True] := it;
    end else begin
      save := it^.Links[True];
      Allocator.FreeNode(it,Self);
    end;
    it := save;
  end;
end;

function TRBTree<T,KCOMP>.FindNode(const AData: TRBTreeNodeData): PRBTreeNode;
var
  it : PRBTreeNode;
  cp : TRBTreeNodeComparator;
  dir : Boolean;
begin
  Result := nil;
  it := Root;
  if (it = nil) then
    exit;
  cp := Comparator;
  while (it <> nil) do begin
    if (cp.Compare(it^.Data,AData) = 0) then begin
      Result := it;
      Break;
    end;
    dir := (cp.Compare(it^.Data,AData) < 0);
    it := it^.Links[dir];
  end;
end;

function TRBTree<T,KCOMP>.Insert(const AData: TRBTreeNodeData): PRBTreeNode;
var
  head : TRBTreeNode;
  g, t : PRBTreeNode; // Grandparent & parent
  p, q : PRBTreeNode; // Iterator & parent
  dir, last, dir2 : Boolean;
  cp : TRBTreeNodeComparator;
begin
  if (Root = nil) then begin
    // Empty tree case
    Root := InitNode(Allocator.CreateNode(Self),AData);
    Result := Root;
  end else begin
    FillChar(head,SizeOf(head),0); // False tree root

    dir := False;
    last := False;

    // Set up helpers
    t := @head;
    g := nil;
    p := nil;
    t^.Links[True] := Root;
    q := t^.Links[True];

  // Search down the tree
    cp := Comparator;
    while True do begin
      if (q = nil) then begin
        // Insert new node at the bottom
        q := InitNode(Allocator.CreateNode(Self),AData);
        p^.Links[dir] := q;
      end else if IsRed(q^.Links[False]) and IsRed(q^.Links[True]) then begin
        // Color flip
        q^.Red := True;
        q^.Links[False]^.Red := False;
        q^.Links[True]^.Red := False;
      end;

      // Fix red violation
      if IsRed(q) and IsRed(p) then begin
        dir2 := (t^.Links[True] = g);
        if (q = p^.Links[last]) then
          t^.Links[dir2] := RotateSingle(g, not last)
        else
          t^.Links[dir2] := RotateDouble(g, not last );
      end;

      // Stop if found
      if (cp.Compare(q^.Data,AData) = 0) then
        break;

      last := dir;
      dir := (cp.Compare(q^.Data,AData) < 0);

      // Update helpers
      if (g <> nil) then
        t := g;
      g := p;
      p := q;
      q := q^.Links[dir];
    end;

    // Update root
     Root := head.Links[True];
  end;

  // Make root black
  Root^.Red := False;
end;

function TRBTree<T,KCOMP>.Remove(const AData: TRBTreeNodeData): Boolean;
var
  head : TRBTreeNode;
  q, p, g, f, s : PRBTreeNode;
  dir, last, dir2 : Boolean;
  cp : TRBTreeNodeComparator;
begin
  Result := False;
  if (Root = nil) then
    exit;

  FillChar(head,SizeOf(head),0); // False tree root
  f := nil;
  dir := True;

  // Set up helpers
  q := @head;
  p := nil;
  g := nil;
  q^.Links[True] := Root;

  // Search and push a red down
  cp := Comparator;
  while (q^.Links[dir] <> nil) do begin
    last := dir;

    // Update helpers
    g := p;
    p := q;
    q := q^.Links[dir];
    dir := (cp.Compare(q^.Data,AData) < 0);

    // Save found node
    if (cp.Compare(q^.Data,AData) = 0) then
      f := q;

    // Push the red node down
    if not(IsRed(q)) and not(IsRed(q^.Links[dir])) then begin
      if IsRed(q^.Links[not dir]) then begin
        p^.Links[last] := RotateSingle(q,dir);
        p := p^.Links[last];
      end else if not IsRed(q^.Links[not dir]) then begin
        s := p^.Links[not last];

        if (s <> nil) then begin
          if not(IsRed(s^.Links[not last])) and not(IsRed(s^.Links[last])) then begin
            // Color flip
            p^.Red := False;
            s^.Red := True;
            q^.Red := True;
          end else begin
            dir2 := (g^.Links[True] = p);

            if IsRed(s^.Links[last]) then
              g^.Links[dir2] := RotateDouble(p,last)
            else if IsRed(s^.Links[not last]) then
              g^.Links[dir2] := RotateSingle(p,last);

            // Ensure correct coloring
            g^.Links[dir2]^.Red := True;
            q^.Red := g^.Links[dir2]^.Red;
            g^.Links[dir2]^.Links[False]^.Red := False;
            g^.Links[dir2]^.Links[True]^.Red := False;
          end;
        end;
      end;
    end;
  end;

  // Replace and remove if found
  if (f <> nil) then begin
    f^.Data := q^.Data;
    p^.Links[(p^.Links[True] = q)] :=
      q^.Links[(q^.Links[False] = nil)];
    Allocator.FreeNode(q,Self);
    Result := True;
  end;

  // Update root and make it black
  Root := head.Links[True];
  if (Root <> nil) then
    Root^.Red := False;
end;

function TRBTree<T,KCOMP>.CreateForwardIterator(const ANode : PRBTreeNode) : TIterator;
begin
  Result := CreateIterator(ANode,False);
end;

function TRBTree<T,KCOMP>.CreateForwardIterator() : TIterator;
begin
  Result := CreateForwardIterator(Root);
end;

function TRBTree<T,KCOMP>.CreateBackwardIterator(const ANode : PRBTreeNode) : TIterator;
begin
  Result := CreateIterator(ANode,True);
end;

function TRBTree<T,KCOMP>.CreateBackwardIterator() : TIterator;
begin
  Result := CreateBackwardIterator(Root);
end;

{$ifdef RB_DEBUG}
function TRBTree<T,KCOMP>.SelfAssert(ARoot : PRBTreeNode; var AErrorMessage: string): Boolean;
var
  lh, rh : Boolean;
  ln, rn : PRBTreeNode;
  e : string;
begin
  AErrorMessage := '';
  if (ARoot = nil) then begin
    Result := True;
    exit;
  end;

  e := '';
  ln := ARoot^.Links[False];
  rn := ARoot^.Links[True];

  // Consecutive red links
  if IsRed(ARoot) then begin
    if IsRed(ln) or IsRed(rn) then begin
      AErrorMessage := 'Red violation';
      Result := False;
      exit;
    end;
  end;

  lh := SelfAssert(ln,e);
  AErrorMessage := AErrorMessage + ' ' + e;

  rh := SelfAssert(rn,e);
  AErrorMessage := AErrorMessage + ' ' + e;

  // Invalid binary search tree
  if ( ( (ln <> nil) and (Comparator.Compare(ln^.Data,ARoot^.Data) >= 0) ) or
     ( (rn <> nil) and (Comparator.Compare(rn^.Data,ARoot^.Data) <= 0) ) )
  then begin
    AErrorMessage := AErrorMessage + ' ' + 'Binary tree violation';
    Result := False;
    Exit;
  end;

  // Black height mismatch
  if ( lh and rh and (lh <> rh) ) then begin
    AErrorMessage := AErrorMessage + ' ' + 'Black violation';
    Result := False;
    Exit;
  end;

  Result := lh and rh;
end;

function TRBTree<T,KCOMP>.SelfAssert(var AErrorMessage: string): Boolean;
begin
  Result := Self.SelfAssert(Root, AErrorMessage);
end;

{$endif RB_DEBUG}

constructor TRBTree<T,KCOMP>.TIterator.Create(AHandle : PBaseIterator);
begin
  inherited Create();
  FHandle := AHandle;
  FResetState := True;
end;

destructor TRBTree<T,KCOMP>.TIterator.Destroy();
begin
  TreeFreeIterator(FHandle);
  inherited Destroy;
end;

function TRBTree<T,KCOMP>.TIterator.MoveNext : Boolean;
begin
  if FResetState then begin
    FResetState := False;
    Result := (FHandle^.Current <> nil);
    exit;
  end;
  Result := (TreeIteratorMoveNext(FHandle) <> nil);
end;

function TRBTree<T,KCOMP>.TIterator.MovePrevious : Boolean;
begin
  if FResetState then begin
    FResetState := False;
    Result := (FHandle^.Current <> nil);
    exit;
  end;
  Result := (TreeIteratorMovePrevious(FHandle) <> nil);
end;

function TRBTree<T,KCOMP>.TIterator.GetCurrent : TRBTreeNodeData;
begin
  Result := GetCurrentNode()^.Data;
end;

function TRBTree<T,KCOMP>.TIterator.GetCurrentNode : PRBTreeNode;
begin
  Result := FHandle^.Current;
end;

procedure TRBTree<T,KCOMP>.TIterator.Reset();
begin
  FResetState := True;
  TreeInitIterator(FHandle,FHandle^.Tree,FHandle^.StartingNode,FHandle^.StartingDir)
end;

{ TOrdinalComparator<T> }

class function TOrdinalComparator<T>.Compare(const A, B: TOrdinalType): Integer;
begin
  if (A = B) then
    exit(0);
  if (A > B) then
    exit(1);
  exit(-1);
end;

end.

