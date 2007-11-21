unit tclist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry; 

type

  { TTestTList }


  TTestTList= class(TTestCase)
  private
    procedure AssertEquals(Msg: String; P1, P2: Pointer); overload;
    procedure DeleteNegativeIndex;
    procedure DeleteTooBigIndex;
    procedure ExchangeNegativeIndex1;
    procedure ExchangeNegativeIndex2;
    procedure ExchangeTooBigIndex1;
    procedure ExchangeTooBigIndex2;
    procedure AccessNegativeIndex;
    procedure AccessTooBigIndex;
    procedure Shuffle;
  protected
    List : TList;
    Pointers : Packed Array[0..20] of Byte;
    procedure SetUp; override; 
    procedure TearDown; override; 
    Procedure FillList(ACount : Integer);
  published
    procedure TestCreate;
    procedure TestAdd;
    procedure TestAddIndex;
    procedure TestAdd2;
    procedure TestInsertFirst;
    Procedure TestInsertMiddle;
    procedure TestDelete;
    Procedure TestClear;
    Procedure TestIndexOf;
    procedure TestExchange;
    procedure TestAccesIndexOutOfBounds;
    procedure TestDeleteIndexOutOfBounds;
    procedure TestExchangeIndexOutOfBounds;
    Procedure TestSort;
    procedure TestExtractCount;
    procedure TestExtractResult;
    procedure TestExtractNonExisting;
    procedure TestExtractNonExistingResult;
    procedure TestExtractOnlyFirst;
    Procedure TestNotifyAdd;
    Procedure TestNotifyDelete;
    Procedure TestNotifyExtract;
    Procedure TestPack;
  end;

  { TMyList }

  TMyList = Class(TList)
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
    FLastPointer : Pointer;
    FLastAction : TListNotification;
  end;

implementation



procedure TTestTList.SetUp;

Var
  I : Integer;

begin
  List:=TMyList.Create;
  For I:=0 to 20 do
    Pointers[i]:=I; // Zero serves as sentinel.
end; 

procedure TTestTList.TearDown; 
begin
  FreeAndNil(List);
end;

procedure TTestTList.TestCreate;
begin
  AssertEquals('Empty list has count 0',0,List.Count);
end;

procedure TTestTList.AssertEquals(Msg : String; P1,P2 : Pointer);

begin
  If (P1<>P2) then
    Fail(Format('%s: Pointers differ. Expected <%x>, got: <%x>',[Msg,PtrInt(P1),PtrInt(P2)]));
end;

procedure TTestTList.TestAdd;

begin
  FillList(1);
  AssertEquals('Add 1 element, count is 1',1,List.Count);
  AssertEquals('Add 1 element, last element is Ptrint(1)',@Pointers[1],List[0]);
end;

procedure TTestTList.TestAddIndex;

begin
  AssertEquals('Add first element at index 0',0,List.Add(Nil));
  AssertEquals('Add second element, at index 1',1,List.Add(Nil));
end;

procedure TTestTList.TestAdd2;

begin
  FillList(2);
  AssertEquals('Add 2 elements, count is 2',2,List.Count);
  AssertEquals('Add 2 elements, first element is Pointers[1]',@Pointers[1],List[0]);
  AssertEquals('Add 2 elements, second element is Pointers[2]',@Pointers[2],List[1]);
end;

procedure TTestTList.TestInsertFirst;
begin
  FillList(3);
  List.Insert(0,@Pointers[0]);
  AssertEquals('Insert 1 in 3, count is 4',4,List.Count);
  AssertEquals('Insert 1 in 3, first is inserted',@Pointers[0],List[0]);
  AssertEquals('Insert 1 in 3, second is old first',@Pointers[1],List[1]);
end;

procedure TTestTList.TestInsertMiddle;
begin
  FillList(3);
  List.Insert(1,@Pointers[0]);
  AssertEquals('Insert 1 in 3, count is 4',4,List.Count);
  AssertEquals('Insert 1 in 3, 1 is inserted',@Pointers[0],List[1]);
  AssertEquals('Insert 1 in 3, 2 is old 2',@Pointers[2],List[2]);
  AssertEquals('Insert 1 in 3, 0 is untouched',@Pointers[1],List[0]);
end;

procedure TTestTList.TestClear;
begin
  FillList(3);
  List.Clear;
  AssertEquals('Clear: count is 0',0,List.Count);
end;

procedure TTestTList.TestIndexOf;
begin
  FillList(11);
  AssertEquals('Find third element',2,List.IndexOf(@Pointers[3]));
end;

procedure TTestTList.TestDelete;

begin
  FillList(3);
  List.Delete(1);
  AssertEquals('Delete 1 from 3, count is 2',2,List.Count);
  AssertEquals('Delete 1 from 3, first is pointers[1]',@Pointers[1],List[0]);
  AssertEquals('Delete 1 from 3, second is "pointers[3]',@Pointers[3],List[1]);
end;

procedure TTestTList.TestExchange;

begin
  FillList(3);
  List.Exchange(0,2);
  AssertEquals('Exchange 0 and 2, count is 3',3,List.Count);
  AssertEquals('Exchange 0 and 2, first is Pointers[3]',@Pointers[3],List[0]);
  AssertEquals('Exchange 0 and 2, second is Pointers[2]',@Pointers[2],List[1]);
  AssertEquals('Exchange 0 and 2, third is Pointers[1]',@Pointers[1],List[2]);
end;

procedure TTestTList.DeleteNegativeIndex;
begin
  List.Delete(-1);
end;

procedure TTestTList.DeleteTooBigIndex;
begin
  List.Delete(3);
end;

procedure TTestTList.ExchangeNegativeIndex1;
begin
  List.Exchange(-1,2);
end;

procedure TTestTList.ExchangeTooBigIndex1;
begin
  List.Exchange(3,2);
end;

procedure TTestTList.ExchangeNegativeIndex2;
begin
  List.Exchange(2,-1);

end;

procedure TTestTList.ExchangeTooBigIndex2;
begin
  List.Exchange(2,3);
end;

procedure TTestTList.AccessNegativeIndex;

begin
  List[-1];
end;

procedure TTestTList.AccessTooBigIndex;

begin
  List[3];
end;

procedure TTestTList.Shuffle;

Var
  I,I1,I2 : Integer;

begin
  For I:=1 to List.Count* 2 do
    begin
    I1:=Random(List.Count);
    I2:=Random(List.Count);
    if I1<>I2 then
      List.Exchange(I1,I2);
    end;
end;

procedure TTestTList.TestAccesIndexOutOfBounds;
begin
  FillList(3);
  AssertException('Access Negative Index',EListError,@AccessNegativeIndex);
  AssertException('Access Index too big',EListError,@AccessTooBigIndex);
end;

procedure TTestTList.TestDeleteIndexOutOfBounds;
begin
  FillList(3);
  AssertException('Delete Negative Index',EListError,@DeleteNegativeIndex);
  AssertException('Delete Index too big',EListError,@DeleteTooBigIndex);
end;

procedure TTestTList.TestExchangeIndexOutOfBounds;
begin
  FillList(3);
  AssertException('Exchange Negative first index',EListError,@ExchangeNegativeIndex1);
  AssertException('Exchange Negative second index',EListError,@ExchangeNegativeIndex2);
  AssertException('Exchange first Index too big',EListError,@ExchangeTooBigIndex1);
  AssertException('Exchange second Index too big',EListError,@ExchangeTooBigIndex2);
end;

Function CompareBytePointers(P1,P2 : Pointer) : Integer;

begin
  Result:=PByte(P1)^-PByte(P2)^;
end;

procedure TTestTList.TestSort;

Var
  I : Integer;

begin
  FillList(9);
  Shuffle;
  List.Sort(@CompareBytePointers);
  For I:=0 to List.Count-1 do
    If (List[i]<>@Pointers[i+1]) then
      Fail(Format('Item at position %d is out of place (%d)',[I,PByte(List[i])^]));
end;

procedure TTestTList.TestExtractResult;

Var
  I : Integer;

begin
  FillList(9);
  AssertEquals('Extracting pointers[4]',@Pointers[4],List.Extract(@Pointers[4]));
end;

procedure TTestTList.TestExtractCount;

Var
  I : Integer;

begin
  FillList(9);
  List.Extract(@Pointers[4]);
  AssertEquals('Extracting pointers[4], count is 8',8,List.Count);
end;

procedure TTestTList.TestExtractNonExisting;

Var
  I : Integer;

begin
  FillList(9);
  List.Extract(@List);
  AssertEquals('Extracting unexisting, count remains 9',9,List.Count);
end;

procedure TTestTList.TestExtractNonExistingResult;

Var
  I : Integer;

begin
  FillList(9);
  AssertEquals('Extracting unexisting, result is nil',Nil,List.Extract(@List));
end;

procedure TTestTList.TestExtractOnlyFirst;

Var
  I : Integer;

begin
  FillList(9);
  List.Insert(0,@Pointers[4]);
  List.Extract(@Pointers[4]);
  AssertEquals('Extracting pointers[4], result is nil',3,List.IndexOf(@Pointers[4]));
end;

procedure TTestTList.TestNotifyAdd;
begin
  List.Add(@Pointers[1]);
  AssertEquals('Add notification, pointer is pointer[1]',@Pointers[1],TMyList(List).FLastPointer);
  AssertEquals('Add notification, action is lnAdded',ord(lnAdded),Ord(TMyList(List).FLastAction));
end;

procedure TTestTList.TestNotifyDelete;
begin
  FillList(9);
  List.Delete(3);
  AssertEquals('Delete notification, pointer is pointer[4]',@Pointers[4],TMyList(List).FLastPointer);
  AssertEquals('Delete notification, action is lnDeleted',ord(lnDeleted),Ord(TMyList(List).FLastAction));
end;

procedure TTestTList.TestNotifyExtract;
begin
  FillList(9);
  List.Extract(@Pointers[4]);
  AssertEquals('Extract notification, pointer is pointer[4]',@Pointers[4],TMyList(List).FLastPointer);
  AssertEquals('Extract notification, action is lnExtracted',ord(lnExtracted),Ord(TMyList(List).FLastAction));
end;

procedure TTestTList.TestPack;

Var
  I : integer;

begin
  FillList(9);
  List[3]:=Nil;
  List[6]:=Nil;
  List.Pack;
  AssertEquals('Pack, count is 7',7,List.Count);
  For I:=0 to List.Count-1 do
    If (List[i]=Nil) then
      Fail(Format('Packed list contains nil pointer at position %d',[i]));
  AssertEquals('Packed list[3] is @pointer[5]',@Pointers[5],List[3]);
  AssertEquals('Packed list[6] is @pointer[9]',@pointers[9],List[6]);
end;


procedure TTestTList.FillList(ACount: Integer);

Var
  I : integer;

begin
  If ACount>20 then
    Fail('Too many elements added to list. Max is 20');
  For I:=1 to ACount do
    List.Add(@Pointers[i]);
end;

{ TMyList }

procedure TMyList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  inherited Notify(Ptr, Action);
  FLastAction:=Action;
  FLastPointer:=Ptr;
end;

initialization

  RegisterTest(TTestTList); 
end.

