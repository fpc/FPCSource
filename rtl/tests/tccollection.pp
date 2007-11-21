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
  
  { TTestTCollection }

  TTestTCollection= class(TTestCase)
  private
  protected
    FColl : TCollection;
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
    Procedure TestItemNamePath;
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
  FColl:=TCollection.Create(TMyItem);
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

initialization

  RegisterTest(TTestTCollection); 
end.

