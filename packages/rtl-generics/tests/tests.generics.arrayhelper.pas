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

  { TTestArrayHelper }

  TTestArrayHelper = class(TTestCase)
  protected
    procedure CheckBinarySearch(const AArray: TArray<Integer>;
      AValue: Integer; AExpectedResult: boolean; out ASearchResult: TBinarySearchResult);
    procedure CheckSearchResult(const ASearchResult: TBinarySearchResult;
      AValue: Integer; ACandidateIndex, AFoundIndex: SizeInt; ACompareResult: Boolean);
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
  end;

implementation

{ TTestArrayHelper }

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

begin
  RegisterTest(TTestArrayHelper);
end.

