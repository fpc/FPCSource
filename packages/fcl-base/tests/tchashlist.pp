unit tchashlist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, contnrs;

type

  { TItemObject }

  TItemObject = Class(TObject)
  private
    FIndex: Integer;
  Public
    Constructor Create(AIndex : integer);
    Property Index : Integer Read FIndex Write FIndex;
  end;

  { TTestHash }

  TTestHash = class(TTestCase)
  Protected
    FH : TFPHashList;
    FItems : TStringList;
    Procedure Setup; override;
    Procedure TearDown; override;
    Procedure AddItem(I : Integer);
    Procedure AssertItem(AItemIndex: Integer; AItem : Pointer);
    Procedure AssertItem(AItemIndex,AHAshIndex : Integer);
    Procedure AssertCount(ACount : Integer);
    Procedure AssertCapacity(ACapacity : Integer);
  published
    procedure TestEmpty;
    Procedure TestAdd;
    Procedure TestGrow;
    Procedure TestDelete;
    Procedure TestFind;
  end;

implementation

{ TItemObject }

constructor TItemObject.Create(AIndex: integer);
begin
  FIndex:=AIndex;
end;

procedure TTestHash.Setup;

Var
  I : integer;
begin
  Inherited;
  FH:=TFPHashList.Create;
  FItems:=TStringList.Create;
  For I:=0 to 1000 do
    FItems.AddObject(IntToStr(I),TItemObject.Create(i));
end;

procedure TTestHash.TearDown;
begin
  FreeAndNil(FItems);
  FreeAndNil(FH);
  Inherited;
end;

procedure TTestHash.AddItem(I: Integer);
begin
  FH.Add(FItems[i],FItems.Objects[i]);
end;

procedure TTestHash.AssertItem(AItemIndex: Integer; AItem: Pointer);
begin
  if not (AItemindex<FItems.Count) then
    Fail(Format('Incorrect item index : %d >= %d',[AItemIndex,FItems.Count]));
  AssertSame(Format('Object %d',[AItemIndex]),FItems.Objects[AItemIndex],AItem);
end;

procedure TTestHash.AssertItem(AItemIndex, AHAshIndex: Integer);
begin
  if not (AItemindex<FItems.Count) then
    Fail(Format('Incorrect item index : %d >= %d',[AItemIndex,FItems.Count]));
  if not (AHashIndex<FH.Count) then
    Fail(Format('Incorrect hash index : %d >= %d',[AItemIndex,FItems.Count]));
  AssertSame(Format('Object %d',[AItemIndex]),FItems.Objects[AItemIndex],FH.Items[AHashIndex]);
end;

procedure TTestHash.AssertCount(ACount: Integer);
begin
  AssertEquals('Hash list item count',ACount,FH.Count);
end;

procedure TTestHash.AssertCapacity(ACapacity: Integer);
begin
  AssertEquals('Hash list capacity',ACapacity,FH.Capacity);
end;

procedure TTestHash.TestEmpty;
begin
  AssertCount(0);
  AssertCapacity(0);
end;

Const
  CS  = 2*SizeOf(ptrint);
  CS2 = SizeOf(ptrint);

procedure TTestHash.TestAdd;
begin
  AddItem(0);
  AssertCount(1);
  AssertCapacity(CS);
  AssertItem(0,0);
end;

procedure TTestHash.TestGrow;

Var
  I : Integer;

begin
  For I:=0 to CS do
    AddItem(i);
  AssertCount(CS+1);
  AssertCapacity(CS+CS+CS2);
end;

procedure TTestHash.TestDelete;

Var
  I : Integer;

begin
  For I:=0 to 9 do
    AddItem(i);
  FH.Delete(3);
  AssertCount(9);
  For I:=0 to 2 do
    AssertItem(I,I);
  For I:=4 to 9 do
    AssertItem(I,I-1);
end;

procedure TTestHash.TestFind;

Var
  I : integer;

begin
  For I:=0 to FItems.Count-1 do
    AddItem(I);
  For I:=0 to FItems.Count-1 do
    AssertItem(I,FH.FindIndexOf(FItems[i]));
  For I:=0 to FItems.Count-1 do
    AssertItem(I,FH.Find(FItems[i]));
  AssertNull('Not existing not found',FH.Find('XYZ'));
  FH.Delete(0);
  AssertNull('Deleted is not found',FH.Find('0'))
end;



initialization

  RegisterTest(TTestHash);
end.

