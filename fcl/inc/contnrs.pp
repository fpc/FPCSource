{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2002 by Florian Klaempfl

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$ifdef fpc}
{$mode objfpc}
{$endif}
unit contnrs;

interface

uses
  SysUtils,Classes;

Type

  TObjectList = class(TList)
  private
    ffreeobjects : boolean;
  Protected
    Procedure Notify(Ptr: Pointer; Action: TListNotification); override;
    function GetItem(Index: Integer): TObject;
    Procedure SetItem(Index: Integer; AObject: TObject);
  public
    constructor create;
    constructor create(freeobjects : boolean);
    function Add(AObject: TObject): Integer;
    function Extract(Item: TObject): TObject;
    function Remove(AObject: TObject): Integer;
    function IndexOf(AObject: TObject): Integer;
    function FindInstanceOf(AClass: TClass; AExact: Boolean; AStartAt: Integer): Integer;
    Procedure Insert(Index: Integer; AObject: TObject);
    function First: TObject;
    Function Last: TObject;
    property OwnsObjects: Boolean read FFreeObjects write FFreeObjects;
    property Items[Index: Integer]: TObject read GetItem write SetItem; default;
  end;

  TComponentList = class(TObjectList)
  Private
    FNotifier : TComponent;
  Protected
    Procedure Notify(Ptr: Pointer; Action: TListNotification); override;
    Function GetItems(Index: Integer): TComponent;
    Procedure SetItems(Index: Integer; AComponent: TComponent);
    Procedure HandleFreeNotify(Sender: TObject; AComponent: TComponent);
  public
    destructor Destroy; override;
    Function Add(AComponent: TComponent): Integer;
    Function Extract(Item: TComponent): TComponent;
    Function Remove(AComponent: TComponent): Integer;
    Function IndexOf(AComponent: TComponent): Integer;
    Function First: TComponent;
    Function Last: TComponent;
    Procedure Insert(Index: Integer; AComponent: TComponent);
    property Items[Index: Integer]: TComponent read GetItems write SetItems; default;
  end;

  TClassList = class(TList)
  protected
    Function GetItems(Index: Integer): TClass;
    Procedure SetItems(Index: Integer; AClass: TClass);
  public
    Function Add(AClass: TClass): Integer;
    Function Extract(Item: TClass): TClass;
    Function Remove(AClass: TClass): Integer;
    Function IndexOf(AClass: TClass): Integer;
    Function First: TClass;
    Function Last: TClass;
    Procedure Insert(Index: Integer; AClass: TClass);
    property Items[Index: Integer]: TClass read GetItems write SetItems; default;
  end;

  TOrderedList = class(TObject)
  private
    FList: TList;
  protected
    Procedure PushItem(AItem: Pointer); virtual; abstract;
    Function PopItem: Pointer; virtual;
    Function PeekItem: Pointer; virtual;
    property List: TList read FList;
  public
    constructor Create;
    destructor Destroy; override;
    Function Count: Integer;
    Function AtLeast(ACount: Integer): Boolean;
    Function Push(AItem: Pointer): Pointer;
    Function Pop: Pointer;
    Function Peek: Pointer;
  end;

{ TStack class }

  TStack = class(TOrderedList)
  protected
    Procedure PushItem(AItem: Pointer); override;
  end;

{ TObjectStack class }

  TObjectStack = class(TStack)
  public
    Function Push(AObject: TObject): TObject;
    Function Pop: TObject;
    Function Peek: TObject;
  end;

{ TQueue class }

  TQueue = class(TOrderedList)
  protected
    Procedure PushItem(AItem: Pointer); override;
  end;

{ TObjectQueue class }

  TObjectQueue = class(TQueue)
  public
    Function Push(AObject: TObject): TObject;
    Function Pop: TObject;
    Function Peek: TObject;
  end;

implementation

constructor tobjectlist.create(freeobjects : boolean);

begin
  inherited create;
  ffreeobjects:=freeobjects;
end;

Constructor tobjectlist.create;

begin
  inherited create;
  ffreeobjects:=True;
end;

Procedure TObjectList.Notify(Ptr: Pointer; Action: TListNotification);

begin
  if FFreeObjects then
    if (Action=lnDeleted) then
      TObject(Ptr).Free;
  inherited Notify(Ptr,Action);
end;


Function TObjectList.GetItem(Index: Integer): TObject;

begin
  Result:=TObject(Inherited Get(Index));
end;


Procedure TObjectList.SetItem(Index: Integer; AObject: TObject);

Var
  O : TObject;

begin
  if OwnsObjects then
    begin
    O:=GetItem(Index);
    O.Free;
    end;
  Put(Index,Pointer(AObject));
end;


Function TObjectList.Add(AObject: TObject): Integer;

begin
  Result:=Inherited Add(Pointer(AObject));
end;


Function TObjectList.Extract(Item: TObject): TObject;

begin
  Result:=Tobject(Inherited Extract(Pointer(Item)));
end;


Function TObjectList.Remove(AObject: TObject): Integer;

begin
  Result:=Inherited Remove(Pointer(AObject));
end;


Function TObjectList.IndexOf(AObject: TObject): Integer;

begin
  Result:=Inherited indexOF(Pointer(AObject));
end;


Function TObjectList.FindInstanceOf(AClass: TClass; AExact: Boolean; AStartAt : Integer): Integer;

Var
  I : Integer;

begin
  I:=AStartAt;
  Result:=-1;
  If AExact then
    While (I<Count) and (Result=-1) do
      If Items[i].ClassType=AClass then
        Result:=I
      else
        Inc(I)
  else
    While (I<Count) and (Result=-1) do
      If Items[i].InheritsFrom(AClass) then
        Result:=I
      else
        Inc(I);
end;


Procedure TObjectList.Insert(Index: Integer; AObject: TObject);

begin
  Inherited Insert(Index,Pointer(AObject));
end;


Function TObjectList.First: TObject;

begin
  Result := TObject(Inherited First);
end;


Function TObjectList.Last: TObject;

begin
  Result := TObject(Inherited Last);
end;

{ TListComponent }

Type
  TlistComponent = Class(TComponent)
  Private
    Flist : TComponentList;
  Public
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  end;

procedure TlistComponent.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  If (Operation=opremove) then
    Flist.HandleFreeNotify(Self,AComponent);
  inherited;
end;

{ TComponentList }

Function TComponentList.Add(AComponent: TComponent): Integer;
begin
  Result:=Inherited Add(AComponent);
end;

destructor TComponentList.Destroy;
begin
  FNotifier.Free;
  inherited;
end;

Function TComponentList.Extract(Item: TComponent): TComponent;
begin
  Result:=TComponent(Inherited Extract(Item));
end;

Function TComponentList.First: TComponent;
begin
  Result:=TComponent(Inherited First);
end;

Function TComponentList.GetItems(Index: Integer): TComponent;
begin
  Result:=TComponent(Inherited Items[Index]);
end;

Procedure TComponentList.HandleFreeNotify(Sender: TObject;
  AComponent: TComponent);
begin
  Extract(Acomponent);
end;

Function TComponentList.IndexOf(AComponent: TComponent): Integer;
begin
  Result:=Inherited IndexOf(AComponent);
end;

Procedure TComponentList.Insert(Index: Integer; AComponent: TComponent);
begin
  Inherited Insert(Index,Acomponent)
end;

Function TComponentList.Last: TComponent;
begin
  Result:=TComponent(Inherited Last);
end;

Procedure TComponentList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  If FNotifier=NIl then
    begin
    FNotifier:=TlistComponent.Create(nil);
    TlistComponent(FNotifier).FList:=Self;
    end;
  If Assigned(Ptr) then
    With TComponent(Ptr) do
      case Action of
        lnAdded : FreeNotification(FNotifier);
        lnExtracted, lnDeleted: RemoveFreeNotification(FNotifier);
      end;
  inherited Notify(Ptr, Action);
end;

Function TComponentList.Remove(AComponent: TComponent): Integer;
begin
  Result:=Inherited Remove(AComponent);
end;

Procedure TComponentList.SetItems(Index: Integer; AComponent: TComponent);
begin
  Put(Index,AComponent);
end;

{ TClassList }

Function TClassList.Add(AClass: TClass): Integer;
begin
  Result:=Inherited Add(Pointer(AClass));
end;

Function TClassList.Extract(Item: TClass): TClass;
begin
  Result:=TClass(Inherited Extract(Pointer(Item)));
end;

Function TClassList.First: TClass;
begin
  Result:=TClass(Inherited First);
end;

Function TClassList.GetItems(Index: Integer): TClass;
begin
  Result:=TClass(Inherited Items[Index]);
end;

Function TClassList.IndexOf(AClass: TClass): Integer;
begin
  Result:=Inherited IndexOf(Pointer(AClass));
end;

Procedure TClassList.Insert(Index: Integer; AClass: TClass);
begin
  Inherited Insert(index,Pointer(AClass));
end;

Function TClassList.Last: TClass;
begin
  Result:=TClass(Inherited Last);
end;

Function TClassList.Remove(AClass: TClass): Integer;
begin
  Result:=Inherited Remove(Pointer(AClass));
end;

Procedure TClassList.SetItems(Index: Integer; AClass: TClass);
begin
  Put(Index,Pointer(Aclass));
end;

{ TOrderedList }

Function TOrderedList.AtLeast(ACount: Integer): Boolean;
begin
  Result:=(FList.Count>=Acount)
end;

Function TOrderedList.Count: Integer;
begin
  Result:=FList.Count;
end;

constructor TOrderedList.Create;
begin
  FList:=Tlist.Create;
end;

destructor TOrderedList.Destroy;
begin
  FList.Free;
end;

Function TOrderedList.Peek: Pointer;
begin
  If AtLeast(1) then
    Result:=PeekItem
  else
    Result:=Nil;
end;

Function TOrderedList.PeekItem: Pointer;
begin
  With Flist do
    Result:=Items[Count-1]
end;

Function TOrderedList.Pop: Pointer;
begin
  If Atleast(1) then
    Result:=PopItem
  else
    Result:=Nil;
end;

Function TOrderedList.PopItem: Pointer;
begin
  With FList do
    If Count>0 then
      begin
      Result:=Items[Count-1];
      Delete(Count-1);
      end
    else
      Result:=Nil;
end;

Function TOrderedList.Push(AItem: Pointer): Pointer;
begin
  PushItem(Aitem);
  Result:=AItem;
end;

{ TStack }

Procedure TStack.PushItem(AItem: Pointer);
begin
  FList.Add(Aitem);
end;

{ TObjectStack }

Function TObjectStack.Peek: TObject;
begin
  Result:=TObject(Inherited Peek);
end;

Function TObjectStack.Pop: TObject;
begin
  Result:=TObject(Inherited Pop);
end;

Function TObjectStack.Push(AObject: TObject): TObject;
begin
  Result:=TObject(Inherited Push(Pointer(AObject)));
end;

{ TQueue }

Procedure TQueue.PushItem(AItem: Pointer);
begin
  With Flist Do
    Insert(0,AItem);
end;

{ TObjectQueue }

Function TObjectQueue.Peek: TObject;
begin
  Result:=TObject(Inherited Peek);
end;

Function TObjectQueue.Pop: TObject;
begin
  Result:=TObject(Inherited Pop);
end;

Function TObjectQueue.Push(AObject: TObject): TObject;
begin
  Result:=TObject(Inherited Push(Pointer(Aobject)));
end;

end.
