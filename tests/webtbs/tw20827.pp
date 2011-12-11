
// TObject-descended enumerators together with Level 2 optimization
// were generating invalid code on targets with non-fixed stack (i386-win32, i386-linux).

{$MODE OBJFPC}
{$INTERFACES CORBA}
{$COPERATORS ON}
{$INLINE ON}
{$OPTIMIZATION ON}


type TNode = class
  ID : byte;
  MyType : byte;
  Amount : word;
  constructor Create();
  function IsOK : boolean;
end;

type TContainer = class;

type TContainerEnumerator = class
  Contents : TNode;
  IsDone : boolean;
  constructor Create(Parent : TContainer);
  function MoveNext : Boolean;
  function GetCurrent : TNode;
  property Current : TNode read GetCurrent;
end;

type TContainer = class
  Contents : TNode;
  constructor Create(node : TNode);
  function Seek(SeekID : byte) : TNode;
  function GetEnumerator : TContainerEnumerator;
end;

constructor TNode.Create();
begin
  ID := 20;
  MyType := 5;
  Amount := 24;
end;

function TNode.IsOK : boolean;
begin
  Exit(MyType = 5);
end;


constructor TContainerEnumerator.Create(Parent : TContainer);
begin
  Contents := Parent.Contents;
  IsDone := false;
end;

function TContainerEnumerator.MoveNext : Boolean;
begin
  if IsDone then Exit(false);
  IsDone := true;
  Exit(true);
end;

function TContainerEnumerator.GetCurrent : TNode;
begin
  Exit(Contents);
end;

constructor TContainer.Create(node : TNode);
begin
  Contents := node;
end;

function TContainer.Seek(SeekID : byte) : TNode;
var node : TNode;
    amount : word;
begin
  Seek := nil;
  amount := 255;

  for node in Self do
    if node.IsOK then
      if node.ID = SeekID then
        if node.Amount < amount then
        begin
          Seek := node;
          amount := node.Amount;
        end;
end;

function TContainer.GetEnumerator : TContainerEnumerator;
begin
  Exit(TContainerEnumerator.Create(Self))
end;

var node : TNode;
    container : TContainer;
begin
  node := TNode.Create();
  container := TContainer.Create(node);
  if container.Seek(20) = nil then Halt(1)
  else writeln('success');
end.
