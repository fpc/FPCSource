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
    function Find(value:T):PNode;inline;
    function FindLess(value:T):PNode;inline;
    function FindLessEqual(value:T):PNode;inline;
    function FindGreater(value:T):PNode;inline;
    function FindGreaterEqual(value:T):PNode;inline;
    function Insert(value:T):PNode;inline;
    function Min:PNode;inline;
    function Max:PNode;inline;
    function Next(x:PNode):PNode;inline;
    function Prev(x:PNode):PNode;inline;
    procedure Delete(value:T);inline;
    public constructor Create;
    public destructor Destroy;override;
    function Size:SizeUInt;
    function IsEmpty:boolean;
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


function TSet.Find(value:T):PNode;inline;
var x:PNode;
begin
  x:=FBase;
  while(x <> nil) do begin
    if(TCompare.c(value,x^.Data)) then x:=x^.Left
    else if(TCompare.c(x^.Data,value)) then x:=x^.Right
    else exit(x);
  end;
  exit(nil);
end;

function TSet.FindLess(value:T):PNode;inline;
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
  FindLess:=x;
end;

function TSet.FindLessEqual(value:T):PNode;inline;
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
  FindLessEqual:=x;
end;

function TSet.FindGreater(value:T):PNode;inline;
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
  FindGreater:=x;
end;

function TSet.FindGreaterEqual(value:T):PNode;inline;
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
  FindGreaterEqual:=x;
end;

function TSet.Insert(value:T):PNode;inline;
var position:PNode;
begin
  FBase:=Insert(value, FBase, position);
  FBase^.Color:=BLACK;
  Insert:=position;
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

function TSet.Min:PNode;inline;
begin
  if FBase=nil then exit(nil);
  Min:=Min(FBase);
end;

function TSet.Max:PNode;inline;
var temp:PNode;
begin
  if FBase=nil then exit(nil);
  temp:=FBase;
  while(temp^.Right<>nil) do temp:=temp^.Right;
  exit(temp);
end;

function TSet.Next(x:PNode):PNode;inline;
var temp:PNode;
begin
  if(x=nil) then exit(nil);
  if(x^.Right<>nil) then begin
    temp:=x^.Right;
    while(temp^.Left<>nil) do temp:=temp^.Left;
  end
  else begin
    temp:=x;
    while(true) do begin
      if(temp^.Parent=nil) then begin temp:=temp^.Parent; break; end;
      if(temp^.Parent^.Left=temp) then begin temp:=temp^.Parent; break; end;
      temp:=temp^.Parent;
    end;
  end;
  exit(temp);
end;

function TSet.Prev(x:PNode):PNode;inline;
var temp:PNode;
begin
  if(x=nil) then exit(nil);
  if(x^.Left<>nil) then begin
    temp:=x^.Left;
    while(temp^.Right<>nil) do temp:=temp^.Right;
  end
  else begin
    temp:=x;
    while(true) do begin
      if(temp^.Parent=nil) then begin temp:=temp^.Parent; break; end;
      if(temp^.Parent^.Right=temp) then begin temp:=temp^.Parent; break; end;
      temp:=temp^.Parent;
    end;
  end;
  exit(temp);
end;

end.
