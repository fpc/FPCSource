unit tcbucketlist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,
  contnrs;

type
  TMyBucketList = Class(TBucketList)
  public
    property BucketCount;
  end;

  PPtrInt= ^PtrInt;

  { TTestBucketList }

  TTestBucketList= class(TTestCase)
  Private
    FList : TMyBucketList;
    FData : Array[1..10] of PtrInt;
    FPointers : Array[1..10] of PPtrInt;
    procedure DoDuplicate;
    Procedure AddPointers(ACount : Integer);
    function GetCount: Integer;
  protected
    procedure SetUp; override; 
    procedure TearDown; override; 
  published
    procedure TestCreate;
    procedure TestAdd;
    procedure TestAdd2;
    procedure TestAddDuplicate;
    procedure TestDelete;
    procedure TestDelete2;
    procedure TestClear;
  end;

implementation

Type

{ TCounter }

TCounter = Class(TObject)
private
  FCount: Integer;
Public
  procedure CountItems(AItem, AData: Pointer; out AContinue: Boolean);
  Property Count : Integer Read FCount;
end;


procedure TCounter.CountItems(AItem, AData: Pointer; out AContinue: Boolean);

begin
  Inc(FCount);
end;

procedure TTestBucketList.TestAdd;

Var
  P : POinter;

begin
  P:=FList.add(FPointers[1],Pointer(FData[1]));
  AssertSame('Add returns data pointer',P,Pointer(FData[1]));
  AssertEquals('Item count is 1',1,GetCount);
end;

procedure TTestBucketList.TestAdd2;

begin
  AddPointers(2);
  With TCounter.Create do
    try
      FList.ForEach(@CountItems);
      AssertEquals('Item count is 1',2,Count);
    Finally
      Free;
    end;
end;

procedure TTestBucketList.DoDuplicate;

begin
  AddPointers(1);
end;

Function TTestBucketList.GetCount : Integer;

begin
  With TCounter.Create do
    try
      FList.ForEach(@CountItems);
      Result:=Count;
    Finally
      Free;
    end;
end;


procedure TTestBucketList.AddPointers(ACount: Integer);

Var
  I : Integer;

begin
  For I:=1 to ACount do
    FList.Add(FPointers[I],Pointer(FData[i]));
end;

procedure TTestBucketList.TestAddDuplicate;

begin
  AddPointers(1);
  self.AssertException('Adding duplicate raises exception',EListError,@DoDuplicate);
end;

procedure TTestBucketList.TestDelete;

begin
  AddPointers(3);
  FList.Remove(FPointers[3]);
  AssertEquals('Deleted no longer exists',False,Flist.Exists(FPointers[3]));
  AssertEquals('Remaining 2 exists',True,Flist.Exists(FPointers[2]));
  AssertEquals('Remaining 1 exists',True,Flist.Exists(FPointers[1]));
end;

procedure TTestBucketList.TestDelete2;

begin
  AddPointers(3);
  FList.Remove(FPointers[3]);
  AssertEquals('Count after delete is 2',2,GetCount);
  FList.Remove(FPointers[2]);
  AssertEquals('Count after delete is 2',1,GetCount);
  FList.Remove(FPointers[1]);
  AssertEquals('Count after delete is 2',0,GetCount);
end;

procedure TTestBucketList.TestClear;
begin
  AddPointers(10);
  FList.Clear;
  AssertEquals('Count after Clear is 0',0,GetCount);
end;

procedure TTestBucketList.TestCreate;

begin
  AssertEquals('Count should be 64',64,Flist.BucketCount);
  AssertEquals('Item count is 0',0,GetCount);
end;

procedure TTestBucketList.SetUp; 

Var
  I : integer;

begin
  FList:=TMyBucketList.create(bl64);
  For I:=1 to 10 do
    begin
    FData[i]:=I;
    FPointers[i]:=@FData[i];
    end;
end; 

procedure TTestBucketList.TearDown; 
begin
  FreeAndNil(FList);
end; 

initialization

  RegisterTest(TTestBucketList); 
end.

