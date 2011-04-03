{$mode objfpc}

unit gstack;

interface

uses gvector;

type 
  generic TStack<T>=class
    private
    type TContainer= specialize TVector<T>;
    var FData:TContainer;
    public
    procedure Push(x:T);inline;
    procedure Pop();inline;
    function Top():T;inline;
    function Size():longint;inline;
    function IsEmpty():boolean;inline;
    constructor Create;
    destructor Destroy;override;
end;

implementation

constructor TStack.Create;
begin
  FData:=TContainer.Create;
end;

destructor TStack.Destroy;
begin
  FData.Destroy;
end;

procedure TStack.Push(x:T);inline;
begin
  FData.PushBack(x);
end;

procedure TStack.Pop;inline;
begin
  FData.PopBack;
end;

function TStack.Top:T;inline;
begin
  Top:=FData.Back;
end;

function TStack.Size:longint;inline;
begin
  Size:=FData.Size;
end;

function TStack.IsEmpty:boolean;inline;
begin
  IsEmpty:=FData.IsEmpty;
end;

end.
