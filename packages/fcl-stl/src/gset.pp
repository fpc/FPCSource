{
   This file is part of the Free Pascal FCL library.
   BSD parts (c) 2011 Vlado Boza

   See the file COPYING.FPC, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY;without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

**********************************************************************}
{$mode objfpc}

unit gset;

interface

const RED=true;
const BLACK=false;

type
  generic TSetIterator<T, TNode>=class
    public
    type PNode=^TNode;
    var FNode:PNode;
    function GetData:T;
    function Next:boolean;
    function Prev:boolean;
    property Data:T read GetData;
  end;

  generic TSet<T, TCompare>=class
  public
  type 
    PNode=^Node;
    Node=record
      Data:T;
      Left,Right:PNode;
      Parent:PNode;
      Color:boolean;
    end;
    TIterator=specialize TSetIterator<T, Node>;
  var
  private
    FBase:PNode;
    FSize:SizeUInt;
    
    function CreateNode(Data:T):PNode;inline;
    procedure DestroyNodeAndChilds(nod:PNode);
    procedure DestroyNode(nod:PNode);
    function RotateRight(nod:PNode):PNode;inline;
    function RotateLeft(nod:PNode):PNode;inline;
    procedure FlipColors(nod:PNode);inline;
    function IsRed(nod:PNode):boolean;inline;
    function Insert(value:T; nod:PNode; var position:PNode):PNode;
    function FixUp(nod:PNode):PNode;inline;
    function MoveRedLeft(nod:PNode):PNode;inline;
    function MoveRedRight(nod:PNode):PNode;inline;
    function DeleteMin(nod:PNode):PNode;
    function Delete(value:T; nod:PNode):PNode;
    function Min(nod:PNode):PNode;inline; 
  public
    function Find(value:T):TIterator;inline;
    function FindLess(value:T):TIterator;inline;
    function FindLessEqual(value:T):TIterator;inline;
    function FindGreater(value:T):TIterator;inline;
    function FindGreaterEqual(value:T):TIterator;inline;
    function InsertAndGetIterator(value:T):TIterator;inline;
    procedure Insert(value:T);inline;
    function Min:TIterator;inline;
    function Max:TIterator;inline;
    procedure Delete(value:T);inline;
    public constructor Create;
    public destructor Destroy;override;
    function Size:SizeUInt;
    function IsEmpty:boolean;

    function NFind(value:T):PNode;inline;
    function NFindLess(value:T):PNode;inline;
    function NFindLessEqual(value:T):PNode;inline;
    function NFindGreater(value:T):PNode;inline;
    function NFindGreaterEqual(value:T):PNode;inline;
    function NInsert(value:T):PNode;inline;
    function NMin:PNode;inline;
    function NMax:PNode;inline;
  end;

implementation

constructor TSet.Create;
begin
  FBase:=nil;
  FSize:=0;
end;

destructor TSet.Destroy;
begin
  DestroyNodeAndChilds(FBase);
end;

function TSet.Size:SizeUInt;
begin
  Size:=FSize;
end;

function TSet.IsEmpty:boolean;
begin
  IsEmpty := FSize=0;
end;

procedure TSet.DestroyNodeAndChilds(nod:PNode);
begin
  if nod = nil then exit;
  DestroyNodeAndChilds(nod^.left);
  DestroyNodeAndChilds(nod^.right);
  DestroyNode(nod);
end;

procedure TSet.DestroyNode(nod:PNode);
begin
  Finalize(nod^.Data);
  dispose(nod);
  dec(FSize);
end;

function TSet.CreateNode(Data:T):PNode;inline;
var temp:PNode;
begin
  temp:=new(PNode);
  Initialize(temp^.Data);
  temp^.Data:=Data;
  temp^.Left:=nil;
  temp^.Right:=nil;
  temp^.Parent:=nil;
  temp^.Color:=RED;
  inc(FSize);
  CreateNode:=temp;
end;

function TSet.RotateRight(nod:PNode):PNode;inline;
var temp:PNode;
begin
  temp:=nod^.Left;

  temp^.Parent:=nod^.Parent;
  nod^.Parent:=temp;

  nod^.Left:=temp^.Right;
  temp^.Right:=nod;

  if(nod^.Left<>nil) then nod^.Left^.Parent:=nod;

  temp^.Color:=nod^.Color;
  nod^.Color:=RED;
  exit(temp);
end;

function TSet.RotateLeft(nod:PNode):PNode;inline;
var temp:PNode;
begin
  temp:=nod^.Right;

  temp^.Parent:=nod^.Parent;
  nod^.Parent:=temp;

  nod^.Right:=temp^.Left;
  temp^.Left:=nod;

  if(nod^.Right<>nil) then nod^.Right^.Parent:=nod;

  temp^.Color:=nod^.Color;
  nod^.Color:=RED;
  exit(temp);
end;

procedure TSet.FlipColors(nod:PNode);inline;
begin
  nod^.Color:= not nod^.Color;
  nod^.Left^.Color := not nod^.Left^.Color;
  nod^.Right^.Color := not nod^.Right^.Color;
end;


function TSet.FixUp(nod:PNode):PNode;inline;
begin
  if(IsRed(nod^.Right)) and (not IsRed(nod^.Left)) then nod := rotateLeft(nod);
  if(IsRed(nod^.Left)) and (IsRed(nod^.Left^.Left)) then nod := rotateRight(nod);
  if(IsRed(nod^.Right)) and (IsRed(nod^.Left)) then flipColors(nod);
  FixUp:=nod;
end;

function TSet.MoveRedLeft(nod:PNode):PNode;inline;
begin
  flipColors(nod);
    if (IsRed(nod^.Right^.Left)) then begin 
          nod^.Right := rotateRight(nod^.Right);
          nod := rotateLeft(nod);
          flipColors(nod);
    end;
  MoveRedLeft:=nod;
end;

function TSet.MoveRedRight(nod:PNode):PNode;inline;
begin
  flipColors(nod);
    if (IsRed(nod^.Left^.Left)) then begin 
          nod := rotateRight(nod);
          flipColors(nod);
    end;
  MoveRedRight:=nod;
end;

function TSet.DeleteMin(nod:PNode):PNode;
begin
  if (nod^.Left = nil) then begin
    DestroyNode(nod);
    exit(nil);
  end;

  if ((not IsRed(nod^.Left)) and (not IsRed(nod^.Left^.Left))) then nod := MoveRedLeft(nod);

  nod^.Left := DeleteMin(nod^.Left);

  exit(FixUp(nod));
end;

function TSet.Delete(value:T; nod:PNode):PNode;
begin
  if (TCompare.c(value, nod^.Data)) then begin
    if (nod^.Left=nil) then exit(nod);
    if ((not IsRed(nod^.Left)) and ( not IsRed(nod^.Left^.Left))) then 
      nod := MoveRedLeft(nod);
    nod^.Left := Delete(value, nod^.Left);
  end
  else begin
    if (IsRed(nod^.Left)) then begin 
      nod := rotateRight(nod);
    end;
    if ((not TCompare.c(value,nod^.Data)) and (not TCompare.c(nod^.Data,value)) and (nod^.Right = nil)) then 
    begin
      DestroyNode(nod);
      exit(nil);
    end;
    if (nod^.Right=nil) then exit(nod);
    if ((not IsRed(nod^.Right)) and (not IsRed(nod^.Right^.Left))) then nod := MoveRedRight(nod);
    if ((not TCompare.c(value,nod^.Data)) and (not TCompare.c(nod^.Data,value))) then begin
      nod^.Data := Min(nod^.Right)^.Data;
      nod^.Right := DeleteMin(nod^.Right);
    end
    else nod^.Right := Delete(value, nod^.Right);
  end;
  exit(FixUp(nod));
end;

procedure TSet.Delete(value:T);inline;
begin
  if(FBase<>nil) then FBase:=Delete(value, FBase);
  if(FBase<>nil) then FBase^.Color:=BLACK;
end;


function TSet.Find(value:T):TIterator;inline;
var ret:TIterator; x:PNode;
begin
  x := NFind(value);
  if x = nil then exit(nil);
  ret := TIterator.create;
  ret.FNode := x;
  Find := ret;
end;

function TSet.NFind(value:T):PNode;inline;
var x:PNode;
begin
  x:=FBase;
  while(x <> nil) do begin
    if(TCompare.c(value,x^.Data)) then x:=x^.Left
    else if(TCompare.c(x^.Data,value)) then x:=x^.Right
    else begin
      exit(x);
    end;
  end;
  exit(nil);
end;

function TSet.FindLess(value:T):TIterator;inline;
var ret:TIterator; x:PNode;
begin
  x := NFindLess(value);
  if x = nil then exit(nil);
  ret := TIterator.create;
  ret.FNode := x;
  FindLess := ret;
end;

function TSet.NFindLess(value:T):PNode;inline;
var x,cur:PNode;
begin
  x:=nil;
  cur:=FBase;
  while (cur <> nil) do begin
    if (TCompare.c(cur^.Data, value)) then
    begin
      x:=cur;
      cur:=cur^.right;
    end else
      cur:=cur^.left;
  end;
  NFindLess := x;
end;

function TSet.FindLessEqual(value:T):TIterator;inline;
var ret:TIterator; x:PNode;
begin
  x := NFindLessEqual(value);
  if x = nil then exit(nil);
  ret := TIterator.create;
  ret.FNode := x;
  FindLessEqual := ret;
end;

function TSet.NFindLessEqual(value:T):PNode;inline;
var x,cur:PNode;
begin
  x:=nil;
  cur:=FBase;
  while (cur <> nil) do begin
    if (not TCompare.c(value, cur^.data)) then
    begin
      x:=cur;
      cur:=cur^.right;
    end else
      cur:=cur^.left;
  end;
  NFindLessEqual := x
end;

function TSet.FindGreater(value:T):TIterator;inline;
var ret:TIterator; x:PNode;
begin
  x := NFindGreater(value);
  if x = nil then exit(nil);
  ret := TIterator.create;
  ret.FNode := x;
  FindGreater := ret;
end;

function TSet.NFindGreater(value:T):PNode;inline;
var x,cur:PNode;
begin
  x:=nil;
  cur:=FBase;
  while (cur <> nil) do begin
    if (TCompare.c(value, cur^.Data)) then
    begin
      x:=cur;
      cur:=cur^.left;
    end else
      cur:=cur^.right;
  end;
  NFindGreater := x;
end;

function TSet.FindGreaterEqual(value:T):TIterator;inline;
var ret:TIterator; x:PNode;
begin
  x := NFindGreaterEqual(value);
  if x = nil then exit(nil);
  ret := TIterator.create;
  ret.FNode := x;
  FindGreaterEqual := ret;
end;

function TSet.NFindGreaterEqual(value:T):PNode;inline;
var x,cur:PNode;
begin
  x:=nil;
  cur:=FBase;
  while (cur <> nil) do begin
    if (not TCompare.c(cur^.Data, value)) then
    begin
      x:=cur;
      cur:=cur^.left;
    end else
      cur:=cur^.right;
  end;
  NFindGreaterEqual := x;
end;

procedure TSet.Insert(value:T);inline;
var position:PNode;
begin
  FBase:=Insert(value, FBase, position);
  FBase^.Color:=BLACK;
end;

function TSet.NInsert(value:T):PNode;inline;
var position:PNode;
begin
  FBase:=Insert(value, FBase, position);
  FBase^.Color:=BLACK;
  NInsert := position;
end;

function TSet.InsertAndGetIterator(value:T):TIterator;inline;
var position:PNode; ret:TIterator;
begin
  FBase:=Insert(value, FBase, position);
  FBase^.Color:=BLACK;
  ret := TIterator.create;
  ret.FNode := position;
  InsertAndGetIterator := ret;
end;

function TSet.Insert(value:T; nod:PNode; var position:PNode):PNode;
begin
  if(nod=nil) then begin
    nod:=CreateNode(value);
    position:=nod;
    exit(nod);
  end;
  if(TCompare.c(value,nod^.Data)) then begin 
    nod^.Left:=Insert(value, nod^.Left, position);
    nod^.Left^.Parent:=nod; 
  end
  else if TCompare.c(nod^.Data,value) then begin 
    nod^.Right:=Insert(value, nod^.Right, position);
    nod^.Right^.Parent:=nod;
  end
  else begin
    position:=nod;
    exit(nod);
  end;

  if(IsRed(nod^.Right)) and (not IsRed(nod^.Left)) then nod := rotateLeft(nod);
  if(IsRed(nod^.Left)) and (IsRed(nod^.Left^.Left)) then nod := rotateRight(nod);
  if(IsRed(nod^.Right)) and (IsRed(nod^.Left)) then flipColors(nod);
  
  Insert:=nod;
end;

function TSet.IsRed(nod:PNode):boolean;inline;
begin
  if(nod=nil) then exit(false);
  exit(nod^.Color);
end;

function TSet.Min(nod:PNode):PNode;inline;
var temp:PNode;
begin
  temp:=nod;
  while(temp^.Left<>nil) do temp:=temp^.Left;
  exit(temp);
end;

function TSet.NMin:PNode;inline;
var nod:PNode;
begin
  if FBase=nil then exit(nil);
  nod:=Min(FBase);
  if (nod = nil) then exit(nil);
  NMin := nod;
end;

function TSet.Min:TIterator;inline;
var nod:PNode;
    ret:TIterator;
begin
  nod:=NMin;
  if (nod = nil) then exit(nil);
  ret := TIterator.create;
  ret.FNode := nod;
  Min := ret;
end;

function TSet.NMax:PNode;inline;
var temp:PNode; 
begin
  if FBase=nil then exit(nil);
  temp:=FBase;
  while(temp^.Right<>nil) do temp:=temp^.Right;
  
  NMax := temp;
end;

function TSet.Max:TIterator;inline;
var temp:PNode; ret:TIterator;
begin
  if FBase=nil then exit(nil);
  temp:=FBase;
  while(temp^.Right<>nil) do temp:=temp^.Right;
  
  ret := TIterator.create;
  ret.FNode := temp;
  Max := ret;
end;

function TSetIterator.GetData:T;
begin
  GetData:= FNode^.Data;
end;

function TSetIterator.Next:boolean;
var temp:PNode;
begin
  if(FNode=nil) then exit(false);
  if(FNode^.Right<>nil) then begin
    temp:=FNode^.Right;
    while(temp^.Left<>nil) do temp:=temp^.Left;
  end
  else begin
    temp:=FNode;
    while(true) do begin
      if(temp^.Parent=nil) then begin temp:=temp^.Parent; break; end;
      if(temp^.Parent^.Left=temp) then begin temp:=temp^.Parent; break; end;
      temp:=temp^.Parent;
    end;
  end;
  if (temp = nil) then exit(false);
  FNode:=temp;
  Next:=true;
end;

function TSetIterator.Prev:boolean;
var temp:PNode;
begin
  if(FNode=nil) then exit(false);
  if(FNode^.Left<>nil) then begin
    temp:=FNode^.Left;
    while(temp^.Right<>nil) do temp:=temp^.Right;
  end
  else begin
    temp:=FNode;
    while(true) do begin
      if(temp^.Parent=nil) then begin temp:=temp^.Parent; break; end;
      if(temp^.Parent^.Right=temp) then begin temp:=temp^.Parent; break; end;
      temp:=temp^.Parent;
    end;
  end;
  if (temp = nil) then exit(false);
  FNode:=temp;
  Prev:=true;
end;

end.
