unit Testll;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, fplists;

type

  { TTestLinkedList }

  TTestLinkedList= class(TTestCase)
  published
    procedure TestCreate;
    procedure TestAdd;
    procedure TestAdd2;
    procedure TestClear;
    procedure TestRemove;
    procedure TestRemove2;
    procedure TestRemove3;
    Procedure TestVisit;
  end;

implementation


procedure TTestLinkedList.TestCreate;

Var
  LL : TLinkedList;

begin
  LL:=TLinkedList.Create(TLinkedListItem);
  Try
    AssertEquals('Item class is TLinkedListItem.',TLinkedListItem,LL.ItemClass);
    AssertEquals('Item count is 0',0,LL.Count);
    If (LL.Root<>Nil) then
      Fail('Root is not nil')
  Finally
    LL.Free;
  end;
end;

procedure TTestLinkedList.TestAdd;

Var
  LL : TLinkedList;
  I  : TLinkedListItem;
  
begin
  LL:=TLinkedList.Create(TLinkedListItem);
  Try
    I:=LL.Add;
    AssertEquals('Add result is TLinkedListItem.',TLinkedListItem,I.ClassType);
    AssertEquals('Item count is 1',1,LL.Count);
    If (I<>LL.Root) then
      Fail('Root item is not added item');
  Finally
    LL.Free;
  end;
end;

procedure TTestLinkedList.TestClear;

Var
  LL : TLinkedList;
  I  : Integer;

begin
  LL:=TLinkedList.Create(TLinkedListItem);
  Try
    For I:=1 to 3 do
      LL.Add;
    LL.Clear;
    AssertEquals('Item count after clear is 0',0,LL.Count);
  Finally
    LL.Free;
  end;
end;

procedure TTestLinkedList.TestAdd2;

Var
  LL : TLinkedList;
  I1,I2  : TLinkedListItem;
  
begin
  LL:=TLinkedList.Create(TLinkedListItem);
  Try
    I1:=LL.Add;
    I2:=LL.Add;
    If (I2<>LL.Root) then
      Fail('Root item is not last added item');
    If (I2.Next<>I1) then
      Fail('Items ordered in the wrong way');
  Finally
    LL.Free;
  end;
end;

procedure TTestLinkedList.TestRemove;

Var
  LL : TLinkedList;
  I  : TLinkedListItem;

begin
  LL:=TLinkedList.Create(TLinkedListItem);
  Try
    I:=LL.Add;
    Try
      LL.RemoveItem(I);
      AssertEquals('After remove Item count is 0',0,LL.Count);
      If (Nil<>LL.Root) then
        Fail('Root item is not nil after last removed item');
    Finally
      I.Free;
    end;
  Finally
    LL.Free;
  end;
end;

procedure TTestLinkedList.TestRemove2;

Var
  LL : TLinkedList;
  I1,I2  : TLinkedListItem;

begin
  LL:=TLinkedList.Create(TLinkedListItem);
  Try
    I1:=LL.Add;
    Try
      I2:=LL.Add;
      LL.RemoveItem(I1);
      AssertEquals('After remove first Item count is 1',1,LL.Count);
      If (I2<>LL.Root) then
        Fail('Root item is not I2 after remove of I1');
    Finally
      I1.Free;
    end;
  Finally
    LL.Free;
  end;
end;

procedure TTestLinkedList.TestRemove3;

Var
  LL : TLinkedList;
  I1,I2, I3  : TLinkedListItem;

begin
  LL:=TLinkedList.Create(TLinkedListItem);
  Try
    I1:=LL.Add;
    I2:=LL.Add;
    I3:=LL.Add;
    LL.RemoveItem(I2);
    Try
      AssertEquals('After remove I2 Item count is 2',2,LL.Count);
      If (I3.Next<>I1) then
        Fail('After Remove of I2, I3.Next<>I1');
    Finally
      I2.Free;
    end;
  Finally
    LL.Free;
  end;
end;


Type

  { TCountVisitor }

  TCountVisitor = Class(TLinkedListVisitor)
    FCount : integer;
    FMax : integer;
    Function Visit(Item : TLinkedListItem) : Boolean; override;
    Constructor Create(AMax : integer);
  end;

{ TCountVisitor }

function TCountVisitor.Visit(Item: TLinkedListItem): Boolean;
begin
  Inc(FCount);
  Result:=(FMax=-1) or (FCount<FMax);
end;

constructor TCountVisitor.Create(AMax: integer);
begin
  FMax:=AMax;
end;

procedure TTestLinkedList.TestVisit;

Var
  I  : Integer;
  V  : TCountVisitor;
  LL : TLinkedList;

begin
  LL:=TLinkedList.Create(TLinkedListItem);
  Try
    For I:=1 to 5 do
      LL.Add;
    V:=TCountVisitor.Create(-1);
    Try
      LL.Foreach(V);
      AssertEquals('Counter visited all items',5,V.FCount);
    Finally
      V.Free;
    end;
    V:=TCountVisitor.Create(3);
    Try
      LL.Foreach(V);
      AssertEquals('Counter visited 3 items',3,V.FCount);
    Finally
      V.Free;
    end;
  Finally
    LL.Free;
  end;

end;


initialization

  RegisterTest(TTestLinkedList); 
end.

