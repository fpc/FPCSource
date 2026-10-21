{
    This file is part of the Free Pascal/NewPascal run time library.
    Copyright (c) 2018 by Maciej Izak (hnb),
    member of the NewPascal development team (http://newpascal.org)

    Copyright(c) 2004-2018 DaThoX

    It contains tests for the Free Pascal generics library

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Acknowledgment

    Thanks to Sphere 10 Software (http://sphere10.com) for sponsoring
    many new types, tests and major refactoring of entire library

 **********************************************************************}

unit tests.generics.arrayhelper;

{$mode delphi}

interface

uses
  fpcunit, testregistry, testutils,
  Classes, SysUtils, Generics.Defaults, Generics.Collections;

type
  TTestEnum = (teOne, teTwo);
  TTestSet = set of TTestEnum;
  TTestByteSet = set of Byte;
  TTestProc = procedure;
  TTestIntArray = TArray<Integer>;

  TTestRecord = record
    A: Integer;
    B: string;
  end;

  // Object with its own ToString.
  TTestNamed = class
  public
    // Returns 'named'.
    function ToString: string; override;
  end;

  // Object that counts its destruction and optionally raises in its destructor.
  TTestTracked = class
  public
    FRaise: Boolean;
    // Increments the destroy count, raises Exception if FRaise is set.
    destructor Destroy; override;
  end;

  TTestTrackedRecord = record
    Obj: TTestTracked;
  end;

  TIndexOfKind =(ikIndexOfIndex, ikIndexOfRange, ikLastIndexOfIndex, ikLastIndexOfRange);

  { TTestArrayHelper }

  TTestArrayHelper = class(TTestCase)
  protected
    procedure CheckBinarySearch(const AArray: TArray<Integer>;
      AValue: Integer; AExpectedResult: boolean; out ASearchResult: TBinarySearchResult);
    procedure CheckSearchResult(const ASearchResult: TBinarySearchResult;
      AValue: Integer; ACandidateIndex, AFoundIndex: SizeInt; ACompareResult: Boolean);
    // Result of an IndexOf/LastIndexOf overload for item 1, -2 if it raises EArgumentOutOfRangeException.
    function RangeIndexOf(const aValues: TArray<Integer>; aKind: TIndexOfKind; aIndex: SizeInt; aCount: SizeInt = 0): SizeInt;
    // Format settings with fixed separators and date/time formats.
    function TestFormatSettings: TFormatSettings;
  published
    procedure Test_BinarySearch_Integers;
    procedure Test_BinarySearch_EmptyArray;
    procedure Test_IndexOf;
    procedure Test_FirstIndexOf;
    procedure Test_LastIndexOf;
    procedure Test_Min;
    procedure Test_Max;
    procedure Test_Contains;
    procedure Test_Reverse;
    // IndexOf starting at an index.
    procedure Test_IndexOf_Index;
    // IndexOf in an index/count range.
    procedure Test_IndexOf_Range;
    // LastIndexOf starting at an index.
    procedure Test_LastIndexOf_Index;
    // LastIndexOf in an index/count range.
    procedure Test_LastIndexOf_Range;
    // A nil comparer uses the default comparer.
    procedure Test_NilComparer;
    // Contains with the default comparer.
    procedure Test_Contains_Default;
    // ToString separators and delimiters.
    procedure Test_ToString_Delimiters;
    // ToString of ordinal types.
    procedure Test_ToString_Ordinals;
    // ToString of character and string types.
    procedure Test_ToString_Strings;
    // ToString of float types.
    procedure Test_ToString_Floats;
    // ToString of TDateTime, TDate and TTime.
    procedure Test_ToString_DateTime;
    // ToString of objects, classes, pointers and variants.
    procedure Test_ToString_References;
    // ToString of types without a text form.
    procedure Test_ToString_Unsupported;
    // FreeValues on a dynamic array.
    procedure Test_FreeValues_Dynamic;
    // FreeValues on a static array.
    procedure Test_FreeValues_Static;
    // FreeValues stops at a destructor that raises.
    procedure Test_FreeValues_Raising;
    // FreeValues on empty arrays and non-class element types.
    procedure Test_FreeValues_NonClass;
  end;

implementation

var
  gDestroyCount: Integer;

destructor TTestTracked.Destroy;

begin
  Inc(gDestroyCount);
  inherited Destroy;
  if FRaise then
    raise Exception.Create('destructor raised');
end;


function TTestNamed.ToString: string;

begin
  Result:='named';
end;


{ TTestArrayHelper }

function TTestArrayHelper.RangeIndexOf(const aValues: TArray<Integer>; aKind: TIndexOfKind; aIndex: SizeInt; aCount: SizeInt): SizeInt;

begin
  try
    case aKind of
      ikIndexOfIndex:
        Result:=TArrayHelper<Integer>.IndexOf(aValues,1,aIndex);
      ikIndexOfRange:
        Result:=TArrayHelper<Integer>.IndexOf(aValues,1,TComparer<Integer>.Default,aIndex,aCount);
      ikLastIndexOfIndex:
        Result:=TArrayHelper<Integer>.LastIndexOf(aValues,1,aIndex);
      ikLastIndexOfRange:
        Result:=TArrayHelper<Integer>.LastIndexOf(aValues,1,TComparer<Integer>.Default,aIndex,aCount);
    end;
  except
    on EArgumentOutOfRangeException do
      Result:=-2;
  end;
end;


function TTestArrayHelper.TestFormatSettings: TFormatSettings;

begin
  Result:=DefaultFormatSettings;
  Result.DecimalSeparator:=',';
  Result.ThousandSeparator:='.';
  Result.DateSeparator:='-';
  Result.TimeSeparator:=':';
  Result.ShortDateFormat:='yyyy-mm-dd';
  Result.LongTimeFormat:='hh:nn:ss';
end;


procedure TTestArrayHelper.CheckBinarySearch(const AArray: TArray<Integer>;
  AValue: Integer; AExpectedResult: boolean; out
  ASearchResult: TBinarySearchResult);
begin
  CheckEquals(AExpectedResult,
    TArrayHelper<Integer>.BinarySearch(AArray,AValue,ASearchResult),
    'Wrong BinarySearch result for ' + AValue.ToString);
end;

procedure TTestArrayHelper.CheckSearchResult(const
  ASearchResult: TBinarySearchResult; AValue: Integer; ACandidateIndex,
  AFoundIndex: SizeInt; ACompareResult: Boolean);
begin
  with ASearchResult do
  begin
    CheckEquals(ACandidateIndex, CandidateIndex, 'Wrong binary search result (CandidateIndex) for ' + AValue.ToString);
    CheckEquals(AFoundIndex, FoundIndex, 'Wrong binary search result (FoundIndex) for ' + AValue.ToString);
    Check(ACompareResult, 'Wrong binary search result (CompareResult) for ' + AValue.ToString);
  end;
end;

procedure TTestArrayHelper.Test_BinarySearch_Integers;
var
  a: TArray<Integer>;
  LSearchResult: TBinarySearchResult;
begin
  a := TArray<Integer>.Create(1,3,5,7,9,11,13,15,20);

  CheckBinarySearch(a, 10, False, LSearchResult);
  CheckSearchResult(LSearchResult, 10, 5, -1, LSearchResult.CompareResult>0);

  CheckBinarySearch(a, 20, True, LSearchResult);
  CheckSearchResult(LSearchResult, 20, 8, 8, LSearchResult.CompareResult=0);
end;

procedure TTestArrayHelper.Test_BinarySearch_EmptyArray;
var
  LSearchResult: TBinarySearchResult;
begin
  CheckBinarySearch(nil, 1, False, LSearchResult);
  CheckSearchResult(LSearchResult, 1, -1, -1, LSearchResult.CompareResult=0);
end;

procedure TTestArrayHelper.Test_IndexOf;
var
  a: TArray<Integer>;
begin
  a := TArray<Integer>.Create(1,3,5,7,9,11,13,15,20);
  AssertEquals('Correct find result for 9', 4, TArrayHelper<Integer>.IndexOf(A,9,TComparer<Integer>.Default));
  AssertEquals('Correctfind result for 33', -1, TArrayHelper<Integer>.IndexOf(A,33,TComparer<Integer>.Default));
  AssertEquals('Correct find result for 9 using default comparer', 4, TArrayHelper<Integer>.IndexOf(A,9));
end;

procedure TTestArrayHelper.Test_FirstIndexOf;

var
  a: TArray<Integer>;
begin
  a := TArray<Integer>.Create(1,3,5,7,9,11,13,9,20);
  AssertEquals('Correct find result for 9', 4, TArrayHelper<Integer>.FirstIndexOf(A,9,TComparer<Integer>.Default));
  AssertEquals('Correct find result for 9 using default comparer', 4, TArrayHelper<Integer>.FirstIndexOf(A,9));
end;

procedure TTestArrayHelper.Test_LastIndexOf;
var
  a: TArray<Integer>;
begin
  a := TArray<Integer>.Create(1,3,5,7,9,11,13,9,20);
  AssertEquals('Correct find result for 9', 7, TArrayHelper<Integer>.LastIndexOf(A,9,TComparer<Integer>.Default));
  AssertEquals('Correct find result for 9 using default comparer', 7, TArrayHelper<Integer>.LastIndexOf(A,9));
end;

procedure TTestArrayHelper.Test_Min;
var
  a: TArray<Integer>;
begin
  a := TArray<Integer>.Create(1,3,5,7,9,11,13,15,20);
  AssertEquals('Correct min', 1, TArrayHelper<Integer>.Min(A,TComparer<Integer>.Default,-1));
  AssertEquals('Correct min using default comparer', 1, TArrayHelper<Integer>.Min(A,-1));
  a:=[];
  AssertEquals('No min', -1, TArrayHelper<Integer>.Min(A,TComparer<Integer>.Default,-1));
  AssertEquals('No min using default comparer', -1, TArrayHelper<Integer>.Min(A,-1));
end;

procedure TTestArrayHelper.Test_Max;
var
  a: TArray<Integer>;
begin
  a := TArray<Integer>.Create(1,3,5,7,9,11,13,15,20);
  AssertEquals('Correct max', 20, TArrayHelper<Integer>.Max(A,TComparer<Integer>.Default,-1));
  AssertEquals('Correct max using default comparer', 20, TArrayHelper<Integer>.Max(A,-1));
  a:=[];
  AssertEquals('No max ', -1, TArrayHelper<Integer>.Max(A,TComparer<Integer>.Default,-1));
  AssertEquals('No max using default comparer', -1, TArrayHelper<Integer>.Max(A,-1));
end;

procedure TTestArrayHelper.Test_Contains;
var
  a: TArray<Integer>;
begin
  a := TArray<Integer>.Create(1,3,5,7,9,11,13,15,20);
  AssertTrue('Have 15', TArrayHelper<Integer>.Contains(A,15,TComparer<Integer>.Default));
  AssertFalse('Does not have 15',TArrayHelper<Integer>.Contains(A,14,TComparer<Integer>.Default));
  a:=[];
  AssertFalse('Empty does not have 15', TArrayHelper<Integer>.Contains(A,15,TComparer<Integer>.Default));
end;

procedure TTestArrayHelper.Test_Reverse;
var
  b,a: TArray<Integer>;
  len,I : integer;
begin
  a := TArray<Integer>.Create(1,3,5,7,9,11,13,15,20);
  len:=Length(a);
  TArrayHelper<Integer>.Reverse(a,b);
  for I:=0 to Length(a)-1 do
    AssertEquals('At index i',a[len-i-1],b[i]);
  a := TArray<Integer>.Create(1,3,5,7,9,11,13,15,20);
  b:=a;
  len:=Length(a);
  TArrayHelper<Integer>.Reverse(a,a);
  for I:=0 to Length(b)-1 do
    AssertEquals('At index i',a[len-i-1],b[i]);
end;


procedure TTestArrayHelper.Test_IndexOf_Index;

var
  a: TArray<Integer>;

begin
  a:=[1,2,1,2,1];
  AssertEquals('From index 0', 0, RangeIndexOf(a,ikIndexOfIndex,0));
  AssertEquals('From index 1', 2, RangeIndexOf(a,ikIndexOfIndex,1));
  AssertEquals('From the last index', 4, RangeIndexOf(a,ikIndexOfIndex,4));
  AssertEquals('Not found after the index', -1, TArrayHelper<Integer>.IndexOf(a,2,4));
  AssertEquals('Index = Length raises', -2, RangeIndexOf(a,ikIndexOfIndex,5));
  AssertEquals('Negative index raises', -2, RangeIndexOf(a,ikIndexOfIndex,-1));
  AssertEquals('Empty array raises', -2, RangeIndexOf(nil,ikIndexOfIndex,0));
end;


procedure TTestArrayHelper.Test_IndexOf_Range;

var
  a: TArray<Integer>;

begin
  a:=[1,2,1,2,1];
  AssertEquals('Whole array', 0, RangeIndexOf(a,ikIndexOfRange,0,5));
  AssertEquals('Range 1..2', 2, RangeIndexOf(a,ikIndexOfRange,1,2));
  AssertEquals('Range 1..1 does not contain the item', -1, RangeIndexOf(a,ikIndexOfRange,1,1));
  AssertEquals('Range 3..4', 4, RangeIndexOf(a,ikIndexOfRange,3,2));
  AssertEquals('Empty range', -1, RangeIndexOf(a,ikIndexOfRange,1,0));
  AssertEquals('Empty range at the end', -1, RangeIndexOf(a,ikIndexOfRange,5,0));
  AssertEquals('Empty range in an empty array', -1, RangeIndexOf(nil,ikIndexOfRange,0,0));
  AssertEquals('Range past the end raises', -2, RangeIndexOf(a,ikIndexOfRange,3,3));
  AssertEquals('Index past the end raises', -2, RangeIndexOf(a,ikIndexOfRange,6,0));
  AssertEquals('Negative index raises', -2, RangeIndexOf(a,ikIndexOfRange,-1,2));
  AssertEquals('Negative count raises', -2, RangeIndexOf(a,ikIndexOfRange,0,-1));
end;


procedure TTestArrayHelper.Test_LastIndexOf_Index;

var
  a: TArray<Integer>;

begin
  a:=[1,2,1,2,1];
  AssertEquals('From the last index', 4, RangeIndexOf(a,ikLastIndexOfIndex,4));
  AssertEquals('From index 3', 2, RangeIndexOf(a,ikLastIndexOfIndex,3));
  AssertEquals('From index 0', 0, RangeIndexOf(a,ikLastIndexOfIndex,0));
  AssertEquals('Not found before the index', -1, TArrayHelper<Integer>.LastIndexOf(a,2,0));
  AssertEquals('Index = Length raises', -2, RangeIndexOf(a,ikLastIndexOfIndex,5));
  AssertEquals('Negative index raises', -2, RangeIndexOf(a,ikLastIndexOfIndex,-1));
  AssertEquals('Empty array raises', -2, RangeIndexOf(nil,ikLastIndexOfIndex,0));
end;


procedure TTestArrayHelper.Test_LastIndexOf_Range;

var
  a: TArray<Integer>;

begin
  a:=[1,2,1,2,1];
  AssertEquals('Whole array', 4, RangeIndexOf(a,ikLastIndexOfRange,4,5));
  AssertEquals('Range 3 down to 2', 2, RangeIndexOf(a,ikLastIndexOfRange,3,2));
  AssertEquals('Range 0 down to 0', 0, RangeIndexOf(a,ikLastIndexOfRange,0,1));
  AssertEquals('Range 1 down to 1 does not contain the item', -1, RangeIndexOf(a,ikLastIndexOfRange,1,1));
  AssertEquals('Item 2 in range 4 down to 3', 3, TArrayHelper<Integer>.LastIndexOf(a,2,TComparer<Integer>.Default,4,2));
  AssertEquals('Item 2 in range 4 down to 4', -1, TArrayHelper<Integer>.LastIndexOf(a,2,TComparer<Integer>.Default,4,1));
  AssertEquals('Empty range', -1, RangeIndexOf(a,ikLastIndexOfRange,4,0));
  AssertEquals('Negative count', -1, RangeIndexOf(a,ikLastIndexOfRange,4,-1));
  AssertEquals('Range below index 0 raises', -2, RangeIndexOf(a,ikLastIndexOfRange,4,6));
  AssertEquals('Range 2 down to -1 raises', -2, RangeIndexOf(a,ikLastIndexOfRange,2,4));
  AssertEquals('Index = Length raises', -2, RangeIndexOf(a,ikLastIndexOfRange,5,1));
  AssertEquals('Index = Length with empty range raises', -2, RangeIndexOf(a,ikLastIndexOfRange,5,0));
  AssertEquals('Negative index raises', -2, RangeIndexOf(a,ikLastIndexOfRange,-1,0));
  AssertEquals('Empty array raises', -2, RangeIndexOf(nil,ikLastIndexOfRange,0,0));
end;


procedure TTestArrayHelper.Test_NilComparer;

var
  a: TArray<Integer>;

begin
  a:=[1,2,1,2,1];
  AssertEquals('IndexOf range', 1, TArrayHelper<Integer>.IndexOf(a,2,nil,0,5));
  AssertEquals('LastIndexOf range', 3, TArrayHelper<Integer>.LastIndexOf(a,2,nil,4,5));
  AssertEquals('IndexOf', 1, TArrayHelper<Integer>.IndexOf(a,2,nil));
  AssertEquals('LastIndexOf', 3, TArrayHelper<Integer>.LastIndexOf(a,2,nil));
  AssertTrue('Contains', TArrayHelper<Integer>.Contains(a,2,nil));
end;


procedure TTestArrayHelper.Test_Contains_Default;

var
  a: TArray<Integer>;

begin
  a:=[1,3,5];
  AssertTrue('Has 3', TArrayHelper<Integer>.Contains(a,3));
  AssertFalse('Does not have 4', TArrayHelper<Integer>.Contains(a,4));
  AssertFalse('Empty array', TArrayHelper<Integer>.Contains([],3));
  AssertFalse('Strings are compared case-sensitively', TArrayHelper<string>.Contains(['a','B'],'b'));
  AssertTrue('Has string', TArrayHelper<string>.Contains(['a','B'],'B'));
end;


procedure TTestArrayHelper.Test_ToString_Delimiters;

var
  a: TArray<Integer>;

begin
  a:=[1,2,1,2,1];
  AssertEquals('Default separator', '1,2,1,2,1', TArrayHelper<Integer>.ToString(a));
  AssertEquals('Separator and delimiters', '[1]; [2]; [1]; [2]; [1]', TArrayHelper<Integer>.ToString(a,'; ','[',']'));
  AssertEquals('Empty array', '', TArrayHelper<Integer>.ToString([],'; ','[',']'));
  AssertEquals('One element', '<7>', TArrayHelper<Integer>.ToString([7],'; ','<','>'));
  AssertEquals('Quoted strings', '"a", "b"', TArrayHelper<string>.ToString(['a','b'],', ','"','"'));
  AssertEquals('With format settings', '1;2', TArrayHelper<Integer>.ToString([1,2],TestFormatSettings,';'));
end;


procedure TTestArrayHelper.Test_ToString_Ordinals;

begin
  AssertEquals('ShortInt', '-5', TArrayHelper<ShortInt>.ToString([-5]));
  AssertEquals('Byte', '255', TArrayHelper<Byte>.ToString([255]));
  AssertEquals('Cardinal', '4294967295', TArrayHelper<Cardinal>.ToString([High(Cardinal)]));
  AssertEquals('Int64', '9223372036854775807,-9223372036854775808', TArrayHelper<Int64>.ToString([High(Int64),Low(Int64)]));
  AssertEquals('QWord', '18446744073709551615', TArrayHelper<QWord>.ToString([High(QWord)]));
  AssertEquals('Enumeration', 'teOne,teTwo', TArrayHelper<TTestEnum>.ToString([teOne,teTwo]));
  AssertEquals('Boolean', 'True,False', TArrayHelper<Boolean>.ToString([True,False]));
  AssertEquals('ByteBool', 'True', TArrayHelper<ByteBool>.ToString([True]));
  AssertEquals('Set', '[teOne],[]', TArrayHelper<TTestSet>.ToString([[teOne],[]]));
  AssertEquals('Set of Byte', '[1,3]', TArrayHelper<TTestByteSet>.ToString([[1,3]]));
end;


procedure TTestArrayHelper.Test_ToString_Strings;

begin
  AssertEquals('Char', 'x,y', TArrayHelper<AnsiChar>.ToString(['x','y']));
  AssertEquals('WideChar', 'w', TArrayHelper<WideChar>.ToString([WideChar('w')]));
  AssertEquals('String', 'a|b c|', TArrayHelper<string>.ToString(['a','b c',''],'|'));
  AssertEquals('ShortString', 's', TArrayHelper<ShortString>.ToString(['s']));
  AssertEquals('UnicodeString', 'u', TArrayHelper<UnicodeString>.ToString(['u']));
  AssertEquals('WideString', 'w', TArrayHelper<WideString>.ToString(['w']));
end;


procedure TTestArrayHelper.Test_ToString_Floats;

begin
  AssertEquals('Double', '1,5;-2;0,1', TArrayHelper<Double>.ToString([1.5,-2,0.1],TestFormatSettings,';'));
  AssertEquals('Single', '1,5;0,100000001490116', TArrayHelper<Single>.ToString([1.5,0.1],TestFormatSettings,';'));
  AssertEquals('Extended', '1,5', TArrayHelper<Extended>.ToString([1.5],TestFormatSettings));
  AssertEquals('Currency', '1,5;2', TArrayHelper<Currency>.ToString([1.5,2],TestFormatSettings,';'));
  AssertEquals('Comp', '5', TArrayHelper<Comp>.ToString([5],TestFormatSettings));
end;


procedure TTestArrayHelper.Test_ToString_DateTime;

var
  lDate: TDateTime;

begin
  lDate:=EncodeDate(2024,1,2);
  AssertEquals('TDateTime', '2024-01-02 12:00:00', TArrayHelper<TDateTime>.ToString([lDate+0.5],TestFormatSettings));
  AssertEquals('TDateTime without time', '2024-01-02', TArrayHelper<TDateTime>.ToString([lDate],TestFormatSettings));
  AssertEquals('TDate', '2024-01-02', TArrayHelper<TDate>.ToString([TDate(lDate)],TestFormatSettings));
  AssertEquals('TTime', '18:00:00', TArrayHelper<TTime>.ToString([TTime(0.75)],TestFormatSettings));
end;


procedure TTestArrayHelper.Test_ToString_References;

var
  lNamed: TTestNamed;
  lObject: TObject;

begin
  lNamed:=TTestNamed.Create;
  lObject:=TObject.Create;
  try
    AssertEquals('Objects', 'named,TObject,nil', TArrayHelper<TObject>.ToString([lNamed,lObject,nil]));
    AssertEquals('Typed objects', 'named,nil', TArrayHelper<TTestNamed>.ToString([lNamed,nil]));
  finally
    lObject.Free;
    lNamed.Free;
  end;
  AssertEquals('Classes', 'nil,TStringList', TArrayHelper<TClass>.ToString([nil,TStringList]));
{$IF SizeOf(Pointer)=8}
  AssertEquals('Pointers', '$0000000000000000,$0000000000001234', TArrayHelper<Pointer>.ToString([nil,Pointer($1234)]));
{$ELSE}
  AssertEquals('Pointers', '$00000000,$00001234', TArrayHelper<Pointer>.ToString([nil,Pointer($1234)]));
{$ENDIF}
  AssertEquals('Variants', '1,a', TArrayHelper<Variant>.ToString([1,'a']));
end;


procedure TTestArrayHelper.Test_ToString_Unsupported;

var
  lRec: TTestRecord;
  lIntf: IInterface;
  lProc: TTestProc;

begin
  lRec.A:=1;
  lRec.B:='x';
  lIntf:=TInterfacedObject.Create;
  lProc:=nil;
  AssertEquals('Record', '?', TArrayHelper<TTestRecord>.ToString([lRec]));
  AssertEquals('Dynamic array', '?', TArrayHelper<TTestIntArray>.ToString([TTestIntArray.Create(1)]));
  AssertEquals('Interface', '?,?', TArrayHelper<IInterface>.ToString([lIntf,nil]));
  AssertEquals('Procedure variable', '?', TArrayHelper<TTestProc>.ToString([lProc]));
end;


procedure TTestArrayHelper.Test_FreeValues_Dynamic;

var
  a: TArray<TTestTracked>;

begin
  gDestroyCount:=0;
  a:=[TTestTracked.Create,nil,TTestTracked.Create];
  TArrayHelper<TTestTracked>.FreeValues(a);
  AssertEquals('Objects freed', 2, gDestroyCount);
  AssertEquals('Length unchanged', 3, Length(a));
  AssertNull('Element 0 is nil', a[0]);
  AssertNull('Element 1 is nil', a[1]);
  AssertNull('Element 2 is nil', a[2]);
end;


procedure TTestArrayHelper.Test_FreeValues_Static;

var
  a: array[0..1] of TTestTracked;

begin
  gDestroyCount:=0;
  a[0]:=TTestTracked.Create;
  a[1]:=TTestTracked.Create;
  TArrayHelper<TTestTracked>.FreeValues(a);
  AssertEquals('Objects freed', 2, gDestroyCount);
  AssertNull('Element 0 is nil', a[0]);
  AssertNull('Element 1 is nil', a[1]);
end;


procedure TTestArrayHelper.Test_FreeValues_Raising;

var
  a: TArray<TTestTracked>;
  lRaised: Boolean;

begin
  gDestroyCount:=0;
  a:=[TTestTracked.Create,TTestTracked.Create,TTestTracked.Create];
  a[1].FRaise:=True;
  lRaised:=False;
  try
    TArrayHelper<TTestTracked>.FreeValues(a);
  except
    on E: Exception do
      lRaised:=E.Message='destructor raised';
  end;
  AssertTrue('Exception of the destructor is passed on', lRaised);
  AssertEquals('Freeing stops at the raising destructor', 2, gDestroyCount);
  AssertNull('Element before the raising one is nil', a[0]);
  AssertNull('Raising element is nil', a[1]);
  AssertNotNull('Element after the raising one is kept', a[2]);
  a[2].Free;
end;


procedure TTestArrayHelper.Test_FreeValues_NonClass;

var
  lInts: TArray<Integer>;
  lRecs: TArray<TTestTrackedRecord>;
  lEmpty: TArray<TTestTracked>;

begin
  gDestroyCount:=0;
  lInts:=[1,2];
  TArrayHelper<Integer>.FreeValues(lInts);
  AssertEquals('Integer array length unchanged', 2, Length(lInts));
  AssertEquals('Integer element unchanged', 2, lInts[1]);
  SetLength(lRecs,1);
  lRecs[0].Obj:=TTestTracked.Create;
  TArrayHelper<TTestTrackedRecord>.FreeValues(lRecs);
  AssertEquals('Objects inside records are not freed', 0, gDestroyCount);
  AssertNotNull('Record field unchanged', lRecs[0].Obj);
  lRecs[0].Obj.Free;
  gDestroyCount:=0;
  lEmpty:=nil;
  TArrayHelper<TTestTracked>.FreeValues(lEmpty);
  TArrayHelper<TTestTracked>.FreeValues([]);
  AssertEquals('Empty arrays free nothing', 0, gDestroyCount);
end;

begin
  RegisterTest(TTestArrayHelper);
end.

