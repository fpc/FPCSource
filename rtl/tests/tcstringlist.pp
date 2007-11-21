unit tcstringlist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry; 

type

  { TTestTStringList }

  TTestTStringList= class(TTestCase)
  private
    procedure AddB;
    procedure DeleteNegativeIndex;
    procedure DeleteTooBigIndex;
    procedure ExchangeNegativeIndex1;
    procedure ExchangeTooBigIndex1;
    procedure ExchangeNegativeIndex2;
    procedure ExchangeTooBigIndex2;
    procedure AccessNegativeIndex;
    procedure AccessTooBigIndex;
    Procedure Shuffle;
  protected
    List : TStringList;
    Procedure FillList(ACount : Integer);
    procedure SetUp; override; 
    procedure TearDown; override; 
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
    Procedure TestSorted;
    Procedure TestSortedAdd;
    Procedure TestSortedAddAll;
    Procedure TestSortedDupError;
    procedure TestSortedAddDuplicate;
    Procedure TestSortedIndexOf;
    Procedure TestChange;
    procedure TestChangeAgain;
    procedure TestChangeCount;
    procedure TestChangeClear;
    Procedure TestSetText;
    procedure TestSetTextEOL;
    procedure TestSetTextEmpty;
    procedure TestSetTextEOLEmpty;
  end;

  { TEventSink }

  TEventSink = Class(TObject)
  private
    FCOunt: Integer;
    FSender: TObject;
  public
    Procedure Change(Sender : TObject);
    Procedure Reset;
    Property ChangeCount : Integer Read FCOunt;
    Property LastSender : TObject Read FSender;
  end;

implementation

procedure TTestTStringList.TestCreate;
begin
  AssertEquals('Empty list has count 0',0,List.Count);
  AssertEquals('Empty list has sorted false',False,List.Sorted);
  If List.Duplicates<>dupIgnore then
    Fail('Empty list has duplicates=dupIgnore');
end;

procedure TTestTStringList.TestAdd;

begin
  FillList(1);
  AssertEquals('Add 1 element, count is 1',1,List.Count);
  AssertEquals('Add 1 element, last element is "Item 1"','Item 1',List[0]);
end;

procedure TTestTStringList.TestAddIndex;

begin
  AssertEquals('Add first element at index 0',0,List.Add('First'));
  AssertEquals('Add second element, at index 1',1,List.Add('second'));
end;

procedure TTestTStringList.TestAdd2;

begin
  FillList(2);
  AssertEquals('Add 2 elements, count is 2',2,List.Count);
  AssertEquals('Add 2 elements, first element is "Item 1"','Item 1',List[0]);
  AssertEquals('Add 2 elements, second element is "Item 2"','Item 2',List[1]);
end;

procedure TTestTStringList.TestInsertFirst;
begin
  FillList(3);
  List.Insert(0,'New');
  AssertEquals('Insert 1 in 3, count is 4',4,List.Count);
  AssertEquals('Insert 1 in 3, first is inserted','New',List[0]);
  AssertEquals('Insert 1 in 3, second is old first','Item 1',List[1]);
end;

procedure TTestTStringList.TestInsertMiddle;
begin
  FillList(3);
  List.Insert(1,'New');
  AssertEquals('Insert 1 in 3, count is 4',4,List.Count);
  AssertEquals('Insert 1 in 3, 1 is inserted','New',List[1]);
  AssertEquals('Insert 1 in 3, 2 is old 2','Item 2',List[2]);
  AssertEquals('Insert 1 in 3, 0 is untouched','Item 1',List[0]);
end;

procedure TTestTStringList.TestClear;
begin
  FillList(3);
  List.Clear;
  AssertEquals('Clear: count is 0',0,List.Count);
end;

procedure TTestTStringList.TestIndexOf;
begin
  FillList(11);
  AssertEquals('Find third element',2,List.IndexOf('Item 3'));
  AssertEquals('Find third element, wrong case',2,List.IndexOf('ITEM 3'));
end;

procedure TTestTStringList.TestDelete;

begin
  FillList(3);
  List.Delete(1);
  AssertEquals('Delete 1 from 3, count is 2',2,List.Count);
  AssertEquals('Delete 1 from 3, first is "Item 1"','Item 1',List[0]);
  AssertEquals('Delete 1 from 3, second is "Item 3"','Item 3',List[1]);
end;

procedure TTestTStringList.TestExchange;

begin
  FillList(3);
  List.Exchange(0,2);
  AssertEquals('Exchange 0 and 2, count is 3',3,List.Count);
  AssertEquals('Exchange 0 and 2, first is "Item 3"','Item 3',List[0]);
  AssertEquals('Exchange 0 and 2, second is "Item 2"','Item 2',List[1]);
  AssertEquals('Exchange 0 and 2, third is "Item 1"','Item 1',List[2]);
end;

procedure TTestTStringList.DeleteNegativeIndex;
begin
  List.Delete(-1);
end;

procedure TTestTStringList.DeleteTooBigIndex;
begin
  List.Delete(3);
end;

procedure TTestTStringList.ExchangeNegativeIndex1;
begin
  List.Exchange(-1,2);
end;

procedure TTestTStringList.ExchangeTooBigIndex1;
begin
  List.Exchange(3,2);
end;

procedure TTestTStringList.ExchangeNegativeIndex2;
begin
  List.Exchange(2,-1);

end;

procedure TTestTStringList.ExchangeTooBigIndex2;
begin
  List.Exchange(2,3);
end;

procedure TTestTStringList.AccessNegativeIndex;

begin
  List[-1];
end;

procedure TTestTStringList.AccessTooBigIndex;

begin
  List[3];
end;

procedure TTestTStringList.Shuffle;

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

procedure TTestTStringList.TestAccesIndexOutOfBounds;
begin
  FillList(3);
  AssertException('Access Negative Index',EStringListError,@AccessNegativeIndex);
  AssertException('Access Index too big',EStringListError,@AccessTooBigIndex);
end;

procedure TTestTStringList.TestDeleteIndexOutOfBounds;
begin
  FillList(3);
  AssertException('Delete Negative Index',EStringListError,@DeleteNegativeIndex);
  AssertException('Delete Index too big',EStringListError,@DeleteTooBigIndex);
end;

procedure TTestTStringList.TestExchangeIndexOutOfBounds;
begin
  FillList(3);
  AssertException('Exchange Negative first index',EStringListError,@ExchangeNegativeIndex1);
  AssertException('Exchange Negative second index',EStringListError,@ExchangeNegativeIndex2);
  AssertException('Exchange first Index too big',EStringListError,@ExchangeTooBigIndex1);
  AssertException('Exchange second Index too big',EStringListError,@ExchangeTooBigIndex2);
end;

procedure TTestTStringList.TestSort;

Var
  I : Integer;

begin
  FillList(9);
  Shuffle;
  List.Sort;
  For I:=0 to List.Count-1 do
    If (List[i]<>'Item '+IntToStr(I+1)) then
      Fail(Format('Item at position %d is out of place (%s)',[I,List[i]]));
end;

procedure TTestTStringList.TestSorted;

Var
  I : Integer;
begin
  FillList(9);
  Shuffle;
  List.Sorted:=True;
  For I:=0 to List.Count-1 do
    If (List[i]<>'Item '+IntToStr(I+1)) then
      Fail(Format('Item at position %d is out of place (%s)',[I,List[i]]));
end;

procedure TTestTStringList.TestSortedAdd;
begin
  List.Sorted:=True;
  List.Add('B');
  AssertEquals('Add second element at first location in sorted list',0,List.Add('A'));
  AssertEquals('Add third element at first location in sorted list',1,List.Add('AB'));
  AssertEquals('Add fourth element at last location in sorted list',3,List.Add('C'));
end;

procedure TTestTStringList.TestSortedAddAll;

Var
  I : Integer;
  
begin
  List.Sorted:=True;
  FillList(9);
  For I:=0 to List.Count-1 do
    If (List[i]<>'Item '+IntToStr(I+1)) then
      Fail(Format('Item at position %d is out of place (%s)',[I,List[i]]));
end;

procedure TTestTStringList.AddB;

begin
  List.Add('B');
end;

procedure TTestTStringList.TestSortedDupError;
begin
  List.Sorted:=True;
  List.Duplicates:=dupError;
  List.Add('B');
  AssertEquals('Add second element at first location in sorted list',0,List.Add('A'));
  AssertException(EStringListError,@AddB);
end;

procedure TTestTStringList.TestSortedAddDuplicate;

begin
  List.Sorted:=True;
  List.Duplicates:=dupAccept;
  List.Add('B');
  AssertEquals('Add second element at first location in sorted list',0,List.Add('A'));
  AssertEquals('Add third element at first location in sorted list',1,List.Add('B'));
  AssertEquals('Add fourth element at last location in sorted list',3,List.Add('C'));
end;

procedure TTestTStringList.TestSortedIndexOf;

// Tests find, as find is called in case of sorted index

begin
  List.Sorted:=True;
  FillList(11);
  // 1 10 11 2 3 - so index 4
  AssertEquals('Find third element',4,List.IndexOf('Item 3'));
  AssertEquals('Find third element, wrong case',4,List.IndexOf('ITEM 3'));
end;

procedure TTestTStringList.TestChange;

Var
  S : TEventSink;

begin
  S:=TEventSink.Create;
  try
    List.OnChange:=@S.Change;
    List.Add('new');
    AssertEquals('Change count equals 1 after add',1,S.ChangeCount);
    If List<>S.LastSender then
      Fail('Sender is list');
  finally
    S.Free;
  end;
end;

procedure TTestTStringList.TestChangeAgain;

Var
  S : TEventSink;

begin
  S:=TEventSink.Create;
  try
    List.BeginUpdate;
    Try
    List.OnChange:=@S.Change;
    List.Add('new');
      AssertEquals('Change count equals 0 after add (beginupdate)',0,S.ChangeCount);
      If (Nil<>S.LastSender) then
        Fail('Sender is nil');
    Finally
      List.EndUpdate;
    end;
    AssertEquals('Change count equals 1 after add endupdate',1,S.ChangeCount);
    If List<>S.LastSender then
      Fail('Sender is list');
  finally
    S.Free;
  end;
end;

procedure TTestTStringList.TestChangeCount;

Var
  S : TEventSink;

begin
  S:=TEventSink.Create;
  try
    List.BeginUpdate;
    Try
      // Count is 1, no notification
      List.OnChange:=@S.Change;
      List.Add('new');
      AssertEquals('Change count equals 0 after add (1st beginupdate)',0,S.ChangeCount);
      If (Nil<>S.LastSender) then
        Fail('Sender is nil');
      List.BeginUpdate;
      Try
        List.Add('new2');
        // Count is 2, no notification
        AssertEquals('Change count equals 0 after add (2nd beginupdate)',0,S.ChangeCount);
        If (Nil<>S.LastSender) then
          Fail('Sender is nil');
      Finally
        List.EndUpdate;
      end;
      // Count is 1 again, no notification
      AssertEquals('Change count equals 0 after first endupdate',0,S.ChangeCount);
      If (Nil<>S.LastSender) then
        Fail('Sender is nil after first endupdate');
    Finally
      List.EndUpdate;
    end;
    AssertEquals('Change count equals 1 after add endupdate',1,S.ChangeCount);
    If List<>S.LastSender then
      Fail('Sender is list');
  finally
    S.Free;
  end;
end;

procedure TTestTStringList.TestChangeClear;

Var
  S : TEventSink;
  
begin
  FillList(9);
  S:=TEventSink.Create;
  try
    List.OnChange:=@S.Change;
    List.Clear;
    AssertEquals('Change count equals 1 after clear',1,S.ChangeCount);
  finally
    S.Free;
  end;
end;

procedure TTestTStringList.TestSetText;

Const
  Lines = 'Line 1'+sLineBreak+'Line 2'+sLineBreak+'Line 3';

begin
  List.Text:=Lines;
  AssertEquals('3 lines set',3,List.Count);
  AssertEquals('First line is "Line 1"','Line 1',List[0]);
  AssertEquals('Second line is "Line 2"','Line 2',List[1]);
  AssertEquals('Third line is "Line 3"','Line 3',List[2]);
end;

procedure TTestTStringList.TestSetTextEOL;

Const
  Lines = 'Line 1'+sLineBreak+'Line 2'+sLineBreak;

begin
  List.Text:=Lines;
  AssertEquals('2 lines set',2,List.Count);
  AssertEquals('First line is "Line 1"','Line 1',List[0]);
  AssertEquals('Second line is "Line 2"','Line 2',List[1]);
end;

procedure TTestTStringList.TestSetTextEOLEmpty;

Const
  Lines = 'Line 1'+sLineBreak+'Line 2'+sLineBreak+slineBreak;

begin
  List.Text:=Lines;
  AssertEquals('3 lines set',3,List.Count);
  AssertEquals('First line is "Line 1"','Line 1',List[0]);
  AssertEquals('Second line is "Line 2"','Line 2',List[1]);
  AssertEquals('Third line is empty','',List[2]);
end;

procedure TTestTStringList.TestSetTextEmpty;

Const
  Lines = 'Line 1'+sLineBreak+sLineBreak+SlineBreak+'Line 2';

begin
  List.Text:=Lines;
  AssertEquals('4 lines set',4,List.Count);
  AssertEquals('First line is "Line 1"','Line 1',List[0]);
  AssertEquals('Second line is empty','',List[1]);
  AssertEquals('Third line is empty','',List[2]);
  AssertEquals('Fourth line is "Line 2"','Line 2',List[3]);
end;


procedure TTestTStringList.FillList(ACount: Integer);

Var
  I : integer;

begin
  For I:=1 to ACount do
    List.Add('Item '+IntToStr(I));
end;

procedure TTestTStringList.SetUp; 
begin
  List:=TStringList.Create;
end; 

procedure TTestTStringList.TearDown; 
begin
  FreeAndNil(List);
end;

{ TEventSink }

procedure TEventSink.Change(Sender: TObject);
begin
  Inc(FCount);
  FSender:=Sender;
end;

procedure TEventSink.Reset;
begin
  FCount:=0;
  FSender:=Nil;
end;

initialization
  RegisterTest(TTestTStringList);
end.

