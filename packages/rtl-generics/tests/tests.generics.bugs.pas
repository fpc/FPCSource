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

unit tests.generics.bugs;

{$mode delphi}

interface

uses
  fpcunit, testregistry, testutils,
  Classes, SysUtils, Generics.Collections, Generics.Defaults;

type

  { TTestBugs }

  TTestBugs = class(TTestCase)
  published
    procedure Test_QuadraticProbing_InfinityLoop;
    procedure Test_GetEqualityComparer;
    procedure TestBinaryOrdering;
    procedure TestAnsiStringBinaryOrdering;
    Procedure TestDictionaryWithStringKeys;
    Procedure TestListIndexOf;
    Procedure TestContainsValue;
    Procedure TestIStringComparerHashList;
    Procedure TestHashMapIStringComparer;
    Procedure TestListBinarySearchInsertIndex;
    Procedure TestListBinarySearchEmpty;
  end;

implementation

{ TTestBugs }

procedure TTestBugs.Test_QuadraticProbing_InfinityLoop;
// https://github.com/maciej-izak/generics.collections/issues/4
var
  LMap: TOpenAddressingQP<string, pointer, TDelphiHashFactory>;
begin
  LMap := TOpenAddressingQP<string, pointer, TDelphiHashFactory>.Create();
  LMap.Add(#178#178#107#141#143#151#168#39#172#38#83#194#130#90#101, nil);
  LMap.Add(#193#190#172#41#144#231#52#62#45#117#108#45#217#71#77, nil);
  LMap.Add(#49#116#202#160#38#131#41#37#217#171#227#215#122#151#71, nil);
  LMap.Add(#148#159#199#71#198#97#69#201#116#45#195#184#178#129#200, nil);
  CheckEquals(false, LMap.ContainsKey(#$E6'h=fzb'#$E5#$B4#$A0#$C4#$E6'B6r>'));
  LMap.Free;
end;

procedure TTestBugs.Test_GetEqualityComparer;
begin
  TDelphiQuadrupleHashFactory.GetHashService.LookupEqualityComparer(TypeInfo(Integer), SizeOf(Integer));
end;

procedure TTestBugs.TestBinaryOrdering;
{ Binary comparison means 'Z' (90) < 'a' (97).
  Locale-aware comparison would sort 'a' before 'Z'. }
var
  Cmp: IComparer<string>;
begin
  Cmp := TComparer<string>.Default;
  AssertTrue('Z < a in binary order', Cmp.Compare('Z', 'a') < 0);
  AssertTrue('a > Z in binary order', Cmp.Compare('a', 'Z') > 0);
  AssertTrue('equal strings', Cmp.Compare('hello', 'hello') = 0);
  AssertTrue('empty = empty', Cmp.Compare('', '') = 0);
  AssertTrue('empty < non-empty', Cmp.Compare('', 'a') < 0);
  AssertTrue('non-empty > empty', Cmp.Compare('a', '') > 0);
  AssertTrue('shorter prefix < longer', Cmp.Compare('abc', 'abcd') < 0);
end;

procedure TTestBugs.TestAnsiStringBinaryOrdering;
var
  Cmp: IComparer<AnsiString>;
begin
  Cmp := TComparer<AnsiString>.Default;
  AssertTrue('AnsiString: Z < a', Cmp.Compare('Z', 'a') < 0);
  AssertTrue('AnsiString: equal', Cmp.Compare('test', 'test') = 0);
  AssertTrue('AnsiString: abc < abd', Cmp.Compare('abc', 'abd') < 0);
end;

procedure TTestBugs.TestDictionaryWithStringKeys;
var
  Dict: TDictionary<string, Integer>;
begin
  Dict := TDictionary<string, Integer>.Create;
  try
    Dict.Add('alpha', 1);
    Dict.Add('beta', 2);
    Dict.Add('gamma', 3);
    AssertTrue('Dict contains alpha', Dict.ContainsKey('alpha'));
    AssertTrue('Dict contains beta', Dict.ContainsKey('beta'));
    AssertTrue('Dict does not contain delta', not Dict.ContainsKey('delta'));
    AssertTrue('Dict[alpha] = 1', Dict['alpha'] = 1);
    AssertTrue('Dict[gamma] = 3', Dict['gamma'] = 3);
  finally
    Dict.Free;
  end;
end;

procedure TTestBugs.TestListIndexOf;
var
  List: TList<string>;
begin
  List := TList<string>.Create;
  try
    List.Add('first');
    List.Add('second');
    List.Add('third');
    AssertTrue('IndexOf first = 0', List.IndexOf('first') = 0);
    AssertTrue('IndexOf third = 2', List.IndexOf('third') = 2);
    AssertTrue('IndexOf missing = -1', List.IndexOf('missing') = -1);
  finally
    List.Free;
  end;
end;

procedure TTestBugs.TestContainsValue;
var
  Dict: TDictionary<Integer, string>;
begin
  Dict := TDictionary<Integer, string>.Create;
  try
    Dict.Add(1, 'one');
    Dict.Add(2, 'two');
    Dict.Add(3, 'three');
    AssertTrue('ContainsValue one', Dict.ContainsValue('one'));
    AssertTrue('ContainsValue three', Dict.ContainsValue('three'));
    AssertTrue('Not ContainsValue four', not Dict.ContainsValue('four'));
  finally
    Dict.Free;
  end;
end;

procedure TTestBugs.TestIStringComparerHashList;
// https://gitlab.com/freepascal.org/fpc/source/-/issues/40483
var
  Cmp: TGOrdinalIStringComparer<String>;
  H1, H2: array[0..1] of UInt32;
begin
  AssertTrue('TDelphiQuadrupleHashFactory is an extended hash factory',
    TDelphiQuadrupleHashFactory.InheritsFrom(TExtendedHashFactory));
  Cmp := TGOrdinalIStringComparer<String>.Create;
  try
    // Element 0 is the number of requested hashes
    H1[0] := 1;
    H1[1] := 0;
    H2[0] := 1;
    H2[1] := 0;
    Cmp.GetHashList('test', @H1[0]);
    Cmp.GetHashList('TEST', @H2[0]);
    AssertTrue('GetHashList returns a hash', H1[1] <> 0);
    AssertEquals('GetHashList ignores case', H1[1], H2[1]);
  finally
    Cmp.Free;
  end;
end;

procedure TTestBugs.TestHashMapIStringComparer;
// https://gitlab.com/freepascal.org/fpc/source/-/issues/40483
var
  Map: THashMap<String, TEmptyRecord>;
  Raised: Boolean;
begin
  Map := THashMap<String, TEmptyRecord>.Create(TIStringComparer.Ordinal);
  try
    Map.Add('Cat', EmptyRecord);
    AssertEquals('Count after Add', 1, Map.Count);
    AssertTrue('ContainsKey Cat', Map.ContainsKey('Cat'));
    AssertTrue('ContainsKey cat ignores case', Map.ContainsKey('cat'));
    AssertTrue('ContainsKey CAT ignores case', Map.ContainsKey('CAT'));
    AssertFalse('ContainsKey Dog', Map.ContainsKey('Dog'));
    Raised := False;
    try
      Map.Add('CAT', EmptyRecord);
    except
      on EListError do
        Raised := True;
    end;
    AssertTrue('Adding CAT after Cat raises EListError', Raised);
    AssertEquals('Count after duplicate Add', 1, Map.Count);
  finally
    Map.Free;
  end;
end;

procedure TTestBugs.TestListBinarySearchInsertIndex;
// https://gitlab.com/freepascal.org/fpc/source/-/issues/41165
var
  List: TList<Integer>;
  Idx: SizeInt;
begin
  List := TList<Integer>.Create;
  try
    List.Add(3);
    List.Add(5);
    AssertFalse('BinarySearch 4 not found', List.BinarySearch(4, Idx));
    AssertEquals('BinarySearch 4 gives the index of 5', 1, Idx);
    AssertTrue('BinarySearch 5 found', List.BinarySearch(5, Idx));
    AssertEquals('BinarySearch 5 gives index 1', 1, Idx);
    AssertTrue('BinarySearch 3 found', List.BinarySearch(3, Idx));
    AssertEquals('BinarySearch 3 gives index 0', 0, Idx);
    AssertFalse('BinarySearch 1 not found', List.BinarySearch(1, Idx));
    AssertEquals('BinarySearch 1 gives index 0', 0, Idx);
    AssertFalse('BinarySearch 7 not found', List.BinarySearch(7, Idx));
    AssertEquals('BinarySearch 7 gives Count', 2, Idx);
    List.Add(9);
    AssertFalse('BinarySearch 6 in 3,5,9 not found', List.BinarySearch(6, Idx));
    AssertEquals('BinarySearch 6 in 3,5,9 gives index 2', 2, Idx);
  finally
    List.Free;
  end;
end;

procedure TTestBugs.TestListBinarySearchEmpty;
// https://gitlab.com/freepascal.org/fpc/source/-/issues/41165
var
  List: TList<Integer>;
  Idx: SizeInt;
begin
  List := TList<Integer>.Create;
  try
    AssertFalse('BinarySearch in empty list not found', List.BinarySearch(5, Idx));
    AssertEquals('BinarySearch in empty list gives index 0', 0, Idx);
    List.Capacity := 4;
    AssertFalse('BinarySearch in empty list with capacity not found', List.BinarySearch(5, Idx));
    AssertEquals('BinarySearch in empty list with capacity gives index 0', 0, Idx);
  finally
    List.Free;
  end;
end;

begin
  RegisterTest(TTestBugs);
end.

