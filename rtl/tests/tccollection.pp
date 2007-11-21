unit tccollection;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry; 

type

  { TMyItem }

  TMyItem = Class(TCollectionItem)
  private
    FNr: integer;
  protected
    // Expose
    function GetOwner: TPersistent; override;
  published
    Property Nr : integer Read FNr Write FNr;
  end;
  
  { TMyCollection }

  TMyCollection = Class(TCollection)
  Private
    FOwner : TPersistent;
    FUpdateCount : Integer;
    FLastNotifyItem,
    FLastUpdate : TCollectionItem;
    FNotifyCount : Integer;
    FLastNotify : TCollectionNotification;
    Function GetOwner : TPersistent; override;
  Public
    procedure Update(Item: TCollectionItem); override;
    procedure Notify(Item: TCollectionItem;Action: TCollectionNotification); override;
    Procedure ResetUpdate;
    Procedure ResetNotify;
    property PropName;
  end;
  
  
  { TTestTCollection }

  TTestTCollection= class(TTestCase)
  private
  protected
    FColl : TMyCollection;
    Function MyItem(I : integer) : TMyItem;
    procedure AddItems(ACount : Integer);
    procedure SetUp; override; 
    procedure TearDown; override; 
  published
    procedure TestCreate;
    procedure TestAdd;
    procedure TestItemCollection;
    procedure TestAddTwo;
    Procedure TestDelete;
    procedure TestClear;
    Procedure TestFreeItem;
    Procedure TestMoveForward;
    Procedure TestMoveBackward;
    Procedure TestID;
    Procedure TestItemOwner;
    Procedure TestDisplayName;
    procedure TestOwnerNamePath;
    Procedure TestItemNamePath;
    Procedure TestOwnerItemNamePath;
    Procedure TestChangeCollection;
    Procedure TestUpdateAdd;
    Procedure TestUpdateDelete;
    Procedure TestUpdateDisplayName;
    Procedure TestUpdateCount;
    Procedure TestUpdateCountNested;
    Procedure TestUpdateMove;
    Procedure TestNotifyAdd;
    Procedure TestNotifyDelete;
  end;

implementation

procedure TTestTCollection.TestCreate;
begin
  AssertEquals('Item count 0 at create',0,FColl.Count);
  AssertEquals('ItemClass is TMyItem',TMyItem,FColl.ItemClass);
end;

procedure TTestTCollection.TestAdd;
begin
  AddItems(1);
  AssertEquals('Item count is 1 after add',1,FColl.Count);
  AssertEquals('Item class is correct',FColl.ItemClass,FColl.Items[0].ClassType);
  AssertEquals('Item index is 0',0,FColl.Items[0].Index);
  AssertEquals('Item ID is 0',0,FColl.Items[0].Id);
end;

procedure TTestTCollection.TestItemCollection;
begin
  AddItems(1);
  If MyItem(0).Collection<>FColl then
    Fail('Item''s Collection is not collection');
end;

procedure TTestTCollection.TestAddTwo;

Var
  I: Integer;
  
begin
  AddItems(3);
  AssertEquals('Item count is 3 after add',3,FColl.Count);
  For I:=0 to 2 do
    begin
    AssertEquals(Format('Item %d class is correct',[i]),FColl.ItemClass,FColl.Items[i].ClassType);
    AssertEquals(Format('Item %d index is 0',[i]),i,FColl.Items[i].Index);
    AssertEquals(Format('Item %d ID is 0',[i]),i,FColl.Items[i].Id);
    AssertEquals(Format('Item %d ID is %d',[i,i+1]),i+1,MyItem(i).Nr);
    end;
end;

procedure TTestTCollection.TestDelete;
begin
  AddItems(3);
  FColl.Delete(1);
  AssertEquals('Item count after delete',2,FColl.Count);
  AssertEquals('Item 0 ok after delete',1,MyItem(0).Nr);
  AssertEquals('Item 1 ok after delete',3,MyItem(1).Nr);
end;

procedure TTestTCollection.TestClear;
begin
  AddItems(3);
  FColl.Clear;
  AssertEquals('Item count after clear',0,FColl.Count);
end;

procedure TTestTCollection.TestFreeItem;
begin
  AddItems(3);
  MyItem(1).Free;
  AssertEquals('Item count after free',2,FColl.Count);
  AssertEquals('Item 0 ok after free',1,MyItem(0).Nr);
  AssertEquals('Item 1 ok after free',3,MyItem(1).Nr);
end;

procedure TTestTCollection.TestMoveForward;
begin
  AddItems(5);
  MyItem(4).Index:=1;
  AssertEquals('Item 0 ok after move',1,MyItem(0).Nr);
  AssertEquals('Item 1 ok after move',5,MyItem(1).Nr);
  AssertEquals('Item 2 ok after move',2,MyItem(2).Nr);
  AssertEquals('Item 3 ok after move',3,MyItem(3).Nr);
  AssertEquals('Item 4 ok after move',4,MyItem(4).Nr);
end;

procedure TTestTCollection.TestMoveBackward;

begin
  AddItems(5);
  MyItem(1).Index:=3;
  AssertEquals('Item 0 ok after move',1,MyItem(0).Nr);
  AssertEquals('Item 1 ok after move',3,MyItem(1).Nr);
  AssertEquals('Item 2 ok after move',4,MyItem(2).Nr);
  AssertEquals('Item 3 ok after move',2,MyItem(3).Nr);
  AssertEquals('Item 4 ok after move',5,MyItem(4).Nr);
end;

procedure TTestTCollection.TestID;

Var
  I : TMyItem;
  
begin
  AddItems(5);
  FColl.Delete(2);
  FColl.Delete(2);
  I:=TMyItem(FColl.Add);
  AssertEquals('ID keeps counting up',5,I.Id)
end;

procedure TTestTCollection.TestItemOwner;
begin
  AddItems(1);
  If (MyItem(0).GetOwner<>FColl) then
    Fail('Item owner is not collection');
end;

procedure TTestTCollection.TestDisplayName;
begin
  AddItems(1);
  AssertEquals('Displayname is classname','TMyItem',MyItem(0).DisplayName);
end;

procedure TTestTCollection.TestItemNamePath;
begin
  AddItems(2);
  AssertEquals('Item namepath is collection namepath+index',FColl.GetNamePath+'[0]',MyItem(0).GetNamePath);
  AssertEquals('Item namepath is collection namepath+index',FColl.GetNamePath+'[1]',MyItem(1).GetNamePath);
end;

procedure TTestTCollection.TestOwnerItemNamePath;

Var
  P : TPersistent;

begin
  P:=TPersistent.Create;
  try
    TMyCollection(FColl).FOwner:=P;
    AddItems(2);
    TMyCollection(FColl).PropName:='Something';
    AssertEquals('Item namepath is collection namepath+index','TPersistent.Something[0]',MyItem(0).GetNamePath);
  finally
    P.Free;
  end;
end;

procedure TTestTCollection.TestOwnerNamePath;

Var
  P : TPersistent;

begin
  P:=TPersistent.Create;
  try
    TMyCollection(FColl).FOwner:=P;
    AddItems(2);
    TMyCollection(FColl).PropName:='Something';
    AssertEquals('Namepath is collection namepath+index','TPersistent.Something',FColl.GetNamePath);
  finally
    P.Free;
  end;
end;

procedure TTestTCollection.TestChangeCollection;

Var
  FCol2 : TCollection;
  I : TCollectionItem;
  
begin
  AddItems(2);
  FCol2:=TCollection.Create(TMyItem);
  try
    I:=FCol2.Add;
    I.Collection:=FColl;
    AssertEquals('Moved item, count of source is zero',0,FCol2.Count);
    AssertEquals('Moved item, count of dest is 1',3,FColl.Count);
    AssertEquals('Moved item, index is 2',2,I.Index);
    If (FColl.Items[0].Collection<>FColl) then
      Fail('Collection owner is not set correctly after move');
    AssertEquals('Moved item, ID is 2',2,I.ID);
  finally
    FCol2.free;
  end;
end;

procedure TTestTCollection.TestUpdateAdd;
begin
  AddItems(1);
  If (FColl.FLastUpdate<>Nil) then
    Fail('update item found !');
  AssertEquals('Update count is 1',1,FColl.FUpdateCount);

end;

procedure TTestTCollection.TestUpdateDelete;
begin
  AddItems(1);
  FColl.ResetUpdate;
  FColl.Delete(0);
  If (FColl.FLastUpdate<>Nil) then
    Fail('update item found !');
  AssertEquals('Update count is 1',1,FColl.FUpdateCount);

end;

procedure TTestTCollection.TestUpdateDisplayName;
begin
  AddItems(1);
  FColl.ResetUpdate;
  MyItem(0).DisplayName:='Something';
  AssertEquals('Display name notification. Update count is 1',1,FColl.FUpdateCount);
  If (FColl.FLastUpdate<>MyItem(0)) then
    Fail('No displayname update');
end;

procedure TTestTCollection.TestUpdateCount;
begin
  FColl.BeginUpdate;
  Try
    AddItems(2);
    
    AssertEquals('Beginupdate; adds. Update count is 0',0,FColl.FUpdateCount);
    If (FColl.FLastUpdate<>Nil) then
      Fail('Beginupdate; FlastUpdate not nil');
  finally
    FColl.EndUpdate;
  end;
  AssertEquals('Endupdate; adds. Update count is 1',1,FColl.FUpdateCount);
  If (FColl.FLastUpdate<>Nil) then
    Fail('Endupdate; FlastUpdate not nil');
end;

procedure TTestTCollection.TestUpdateCountNested;
begin
  FColl.BeginUpdate;
  Try
    AddItems(2);
    FColl.BeginUpdate;
    Try
      AddItems(2);
      AssertEquals('Beginupdate 2; adds. Update count is 0',0,FColl.FUpdateCount);
      If (FColl.FLastUpdate<>Nil) then
        Fail('Beginupdate 2; FlastUpdate not nil');
    finally
      FColl.EndUpdate;
    end;
    AssertEquals('Endupdate 1; Update count is 0',0,FColl.FUpdateCount);
    If (FColl.FLastUpdate<>Nil) then
      Fail('EndUpdate 1; FlastUpdate not nil');
  finally
    FColl.EndUpdate;
  end;
  AssertEquals('Endupdate 2; adds. Update count is 1',1,FColl.FUpdateCount);
  If (FColl.FLastUpdate<>Nil) then
    Fail('Endupdate 2; FlastUpdate not nil');
end;

procedure TTestTCollection.TestUpdateMove;
begin
  AddItems(5);
  FColl.ResetUpdate;
  MyItem(4).Index:=2;
  AssertEquals('Moved item. Update count is 1',1,FColl.FUpdateCount);
  If (FColl.FLastUpdate<>Nil) then
    Fail('Moved item notification - not all items updated');
end;

procedure TTestTCollection.TestNotifyAdd;
begin
  AddItems(1);
  If (FColl.FLastNotifyItem<>MyItem(0)) then
    Fail('No notify item found !');
  AssertEquals('Notify count is 1',1,FColl.FNotifyCount);
  AssertEquals('Notify action is cnAdded',Ord(cnAdded),Ord(FColl.FLastNotify));
end;

procedure TTestTCollection.TestNotifyDelete;

Var
  I : TMyItem;

begin
  AddItems(3);
  FColl.ResetNotify;
  FColl.Delete(1);
  // cnDeleting/cnExtracing. Can't currently test for 2 events...
  AssertEquals('Notify count is 2',2,FColl.FNotifyCount);
  AssertEquals('Notify action is cnExtracted',Ord(cnExtracting),Ord(FColl.FLastNotify));
end;

function TTestTCollection.MyItem(I: integer): TMyItem;
begin
  Result:=TMyItem(FColl.Items[i]);
end;

procedure TTestTCollection.AddItems(ACount: Integer);

Var
  I : integer;
  
begin
  For I:=1 to ACount do
    TMyItem(FColl.Add).Nr:=I;
end;

procedure TTestTCollection.SetUp; 
begin
  FColl:=TMyCollection.Create(TMyItem);
end; 

procedure TTestTCollection.TearDown; 
begin
   FreeAndNil(FColl);
end; 

{ TMyItem }

function TMyItem.GetOwner: TPersistent;
begin
  Result:=inherited GetOwner;
end;

{ TMyCollection }

function TMyCollection.GetOwner: TPersistent;
begin
  Result:=FOwner;
  If (Result=Nil) then
    Result:=Inherited GetOwner;
end;

procedure TMyCollection.Update(Item: TCollectionItem);
begin
  Inc(FUpdateCount);
  FLastUpdate:=Item;
end;

procedure TMyCollection.Notify(Item: TCollectionItem;
  Action: TCollectionNotification);
begin
  Inc(FNotifyCount);
  FLastNotify:=Action;
  FLastNotifyItem:=Item;
end;

procedure TMyCollection.ResetUpdate;
begin
  FUpdateCount:=0;
  FLastUpdate:=Nil;
end;

procedure TMyCollection.ResetNotify;
begin
  FNotifyCount:=0;
  FLastNotifyItem:=Nil;
end;

initialization

  RegisterTest(TTestTCollection); 
end.

