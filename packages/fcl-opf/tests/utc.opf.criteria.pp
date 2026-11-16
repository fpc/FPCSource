unit utc.opf.criteria;

{$mode objfpc}{$H+}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, FpcUnit.Test, FpcUnit.TestRegistry;
{$ELSE}
  Classes, SysUtils, fpcunit, testregistry;
{$ENDIF}

type

  { TTestOPCriteria }

  TTestOPCriteria = class(TTestCase)
  published
    // Basic comparison operators
    procedure TestWhereEqualInt;
    procedure TestWhereEqualString;
    procedure TestWhereEqualBoolean;
    procedure TestWhereEqualDateTime;
    procedure TestWhereNotEqual;
    procedure TestWhereLess;
    procedure TestWhereLessOrEqual;
    procedure TestWhereGreater;
    procedure TestWhereGreaterOrEqual;
    procedure TestWhereLike;
    procedure TestWhereNotLike;
    procedure TestWhereContains;
    // Special conditions
    procedure TestWhereIsNull;
    procedure TestWhereIsNotNull;
    procedure TestWhereIn;
    procedure TestWhereNotIn;
    procedure TestWhereBetween;
    // Logical composition
    procedure TestAddAnd;
    procedure TestAddOr;
    procedure TestAddAndSubCriteria;
    procedure TestAddOrSubCriteria;
    procedure TestNestedSubCriteria;
    procedure TestNot;
    // Convenience methods
    procedure TestAddAndIsNull;
    procedure TestAddOrIsNotNull;
    procedure TestAddAndIn;
    procedure TestAddAndBetween;
    // Ordering and pagination
    procedure TestOrderByAsc;
    procedure TestOrderByDesc;
    procedure TestOrderByMultiple;
    procedure TestLimit;
    procedure TestLimitOffset;
    // IncludeDeleted
    procedure TestIncludeDeleted;
    procedure TestNoIncludeDeletedByDefault;
    // Empty criteria
    procedure TestEmptyCriteria;
    procedure TestEmptyWithLimit;
    // TDateTime overloads
    procedure TestWhereBetweenDateTime;
    procedure TestWhereInDateTime;
    procedure TestAddAndBetweenDateTime;
    // Parameter types
    procedure TestParamTypeInteger;
    procedure TestParamTypeString;
    procedure TestParamTypeBoolean;
    procedure TestParamTypeDateTime;
    procedure TestParamCountNested;
    // Field map resolution
    procedure TestFieldMapResolution;
  end;

implementation

uses Opf.Criteria, Opf.Fields, DB;

{ TTestOPCriteria }

procedure TTestOPCriteria.TestWhereEqualInt;
var
  C: TOPCriteria;
begin
  C := TOPCriteria.Create;
  try
    C.Where('Age', opEqual, 25);
    AssertEquals('SQL', 'Age = :p1', C.ToWhereSQL(Nil));
    AssertEquals('Param', 25, C.Params[0].AsInteger);
    AssertEquals('DataType', Ord(ftInteger), Ord(C.Params[0].DataType));
  finally
    C.Free;
  end;
end;

procedure TTestOPCriteria.TestWhereEqualString;
var
  C: TOPCriteria;
begin
  C := TOPCriteria.Create;
  try
    C.Where('Name', opEqual, 'Alice');
    AssertEquals('SQL', 'Name = :p1', C.ToWhereSQL(Nil));
    AssertEquals('Param', 'Alice', C.Params[0].AsString);
    AssertEquals('DataType', Ord(ftString), Ord(C.Params[0].DataType));
  finally
    C.Free;
  end;
end;

procedure TTestOPCriteria.TestWhereEqualBoolean;
var
  C: TOPCriteria;
begin
  C := TOPCriteria.Create;
  try
    C.Where('Active', opEqual, True);
    AssertEquals('SQL', 'Active = :p1', C.ToWhereSQL(Nil));
    AssertTrue('Param', C.Params[0].AsBoolean);
    AssertEquals('DataType', Ord(ftBoolean), Ord(C.Params[0].DataType));
  finally
    C.Free;
  end;
end;

procedure TTestOPCriteria.TestWhereEqualDateTime;
var
  C: TOPCriteria;
  DT: TDateTime;
begin
  C := TOPCriteria.Create;
  try
    DT := EncodeDate(2026, 1, 15);
    C.Where('CreatedAt', opEqual, DT);
    AssertEquals('SQL', 'CreatedAt = :p1', C.ToWhereSQL(Nil));
    AssertEquals('DataType', Ord(ftDateTime), Ord(C.Params[0].DataType));
  finally
    C.Free;
  end;
end;

procedure TTestOPCriteria.TestWhereNotEqual;
var
  C: TOPCriteria;
begin
  C := TOPCriteria.Create;
  try
    C.Where('Status', opNotEqual, 'Closed');
    AssertEquals('SQL', 'Status <> :p1', C.ToWhereSQL(Nil));
  finally
    C.Free;
  end;
end;

procedure TTestOPCriteria.TestWhereLess;
var
  C: TOPCriteria;
begin
  C := TOPCriteria.Create;
  try
    C.Where('Age', opLess, 18);
    AssertEquals('SQL', 'Age < :p1', C.ToWhereSQL(Nil));
  finally
    C.Free;
  end;
end;

procedure TTestOPCriteria.TestWhereLessOrEqual;
var
  C: TOPCriteria;
begin
  C := TOPCriteria.Create;
  try
    C.Where('Score', opLessOrEqual, 100);
    AssertEquals('SQL', 'Score <= :p1', C.ToWhereSQL(Nil));
  finally
    C.Free;
  end;
end;

procedure TTestOPCriteria.TestWhereGreater;
var
  C: TOPCriteria;
begin
  C := TOPCriteria.Create;
  try
    C.Where('Age', opGreater, 18);
    AssertEquals('SQL', 'Age > :p1', C.ToWhereSQL(Nil));
  finally
    C.Free;
  end;
end;

procedure TTestOPCriteria.TestWhereGreaterOrEqual;
var
  C: TOPCriteria;
begin
  C := TOPCriteria.Create;
  try
    C.Where('Price', opGreaterOrEqual, Double(9.99));
    AssertEquals('SQL', 'Price >= :p1', C.ToWhereSQL(Nil));
  finally
    C.Free;
  end;
end;

procedure TTestOPCriteria.TestWhereLike;
var
  C: TOPCriteria;
begin
  C := TOPCriteria.Create;
  try
    C.Where('Name', opLike, 'A%');
    AssertEquals('SQL', 'Name LIKE :p1', C.ToWhereSQL(Nil));
    AssertEquals('Param', 'A%', C.Params[0].AsString);
  finally
    C.Free;
  end;
end;

procedure TTestOPCriteria.TestWhereNotLike;
var
  C: TOPCriteria;
begin
  C := TOPCriteria.Create;
  try
    C.Where('Email', opNotLike, '%spam%');
    AssertEquals('SQL', 'Email NOT LIKE :p1', C.ToWhereSQL(Nil));
  finally
    C.Free;
  end;
end;

procedure TTestOPCriteria.TestWhereContains;
var
  C: TOPCriteria;
begin
  C := TOPCriteria.Create;
  try
    C.Where('Name', opContains, 'ali');
    AssertEquals('SQL', 'Name LIKE :p1 ESCAPE ''!''', C.ToWhereSQL(Nil));
    AssertEquals('Param wrapped', '%ali%', C.Params[0].AsString);
  finally
    C.Free;
  end;
end;

procedure TTestOPCriteria.TestWhereIsNull;
var
  C: TOPCriteria;
begin
  C := TOPCriteria.Create;
  try
    C.WhereIsNull('DeletedAt');
    AssertEquals('SQL', 'DeletedAt IS NULL', C.ToWhereSQL(Nil));
    AssertEquals('No params', 0, C.Params.Count);
  finally
    C.Free;
  end;
end;

procedure TTestOPCriteria.TestWhereIsNotNull;
var
  C: TOPCriteria;
begin
  C := TOPCriteria.Create;
  try
    C.WhereIsNotNull('Email');
    AssertEquals('SQL', 'Email IS NOT NULL', C.ToWhereSQL(Nil));
  finally
    C.Free;
  end;
end;

procedure TTestOPCriteria.TestWhereIn;
var
  C: TOPCriteria;
begin
  C := TOPCriteria.Create;
  try
    C.WhereIn('Status', ['Open', 'Pending', 'Review']);
    AssertEquals('SQL', 'Status IN (:p1,:p2,:p3)', C.ToWhereSQL(Nil));
    AssertEquals('Param count', 3, C.Params.Count);
    AssertEquals('p1', 'Open', C.Params[0].AsString);
    AssertEquals('p2', 'Pending', C.Params[1].AsString);
    AssertEquals('p3', 'Review', C.Params[2].AsString);
  finally
    C.Free;
  end;
end;

procedure TTestOPCriteria.TestWhereNotIn;
var
  C: TOPCriteria;
begin
  C := TOPCriteria.Create;
  try
    C.WhereNotIn('Role', ['banned', 'suspended']);
    AssertEquals('SQL', 'Role NOT IN (:p1,:p2)', C.ToWhereSQL(Nil));
  finally
    C.Free;
  end;
end;

procedure TTestOPCriteria.TestWhereBetween;
var
  C: TOPCriteria;
begin
  C := TOPCriteria.Create;
  try
    C.WhereBetween('Age', [18, 65]);
    AssertEquals('SQL', 'Age BETWEEN :p1 AND :p2', C.ToWhereSQL(Nil));
    AssertEquals('Low', 18, C.Params[0].AsInteger);
    AssertEquals('High', 65, C.Params[1].AsInteger);
  finally
    C.Free;
  end;
end;

procedure TTestOPCriteria.TestAddAnd;
var
  C: TOPCriteria;
begin
  C := TOPCriteria.Create;
  try
    C.Where('Name', opEqual, 'Alice').AddAnd('Age', opGreater, 18);
    AssertEquals('SQL', 'Name = :p1 AND Age > :p2', C.ToWhereSQL(Nil));
  finally
    C.Free;
  end;
end;

procedure TTestOPCriteria.TestAddOr;
var
  C: TOPCriteria;
begin
  C := TOPCriteria.Create;
  try
    C.Where('Status', opEqual, 'Open').AddOr('Status', opEqual, 'Pending');
    AssertEquals('SQL', 'Status = :p1 OR Status = :p2', C.ToWhereSQL(Nil));
  finally
    C.Free;
  end;
end;

procedure TTestOPCriteria.TestAddAndSubCriteria;
var
  C, Sub: TOPCriteria;
begin
  C := TOPCriteria.Create;
  try
    Sub := TOPCriteria.Create;
    Sub.Where('B', opEqual, 2).AddOr('C', opEqual, 3);
    C.Where('A', opEqual, 1).AddAnd(Sub);
    AssertEquals('SQL', 'A = :p1 AND (B = :p2 OR C = :p3)', C.ToWhereSQL(Nil));
    AssertEquals('Param count', 3, C.Params.Count);
  finally
    C.Free;
  end;
end;

procedure TTestOPCriteria.TestAddOrSubCriteria;
var
  C, Sub: TOPCriteria;
begin
  C := TOPCriteria.Create;
  try
    Sub := TOPCriteria.Create;
    Sub.Where('X', opEqual, 10).AddAnd('Y', opEqual, 20);
    C.Where('Z', opEqual, 30).AddOr(Sub);
    AssertEquals('SQL', 'Z = :p1 OR (X = :p2 AND Y = :p3)', C.ToWhereSQL(Nil));
  finally
    C.Free;
  end;
end;

procedure TTestOPCriteria.TestNestedSubCriteria;
var
  C, Sub1, Sub2: TOPCriteria;
begin
  C := TOPCriteria.Create;
  try
    Sub2 := TOPCriteria.Create;
    Sub2.Where('D', opEqual, 4).AddOr('E', opEqual, 5);
    Sub1 := TOPCriteria.Create;
    Sub1.Where('B', opEqual, 2).AddAnd(Sub2);
    C.Where('A', opEqual, 1).AddAnd(Sub1);
    AssertEquals('SQL', 'A = :p1 AND (B = :p2 AND (D = :p3 OR E = :p4))', C.ToWhereSQL(Nil));
    AssertEquals('Param count', 4, C.Params.Count);
  finally
    C.Free;
  end;
end;

procedure TTestOPCriteria.TestNot;
var
  C: TOPCriteria;
begin
  C := TOPCriteria.Create;
  try
    C.Where('Active', opEqual, True).Not_;
    AssertEquals('SQL', 'NOT (Active = :p1)', C.ToWhereSQL(Nil));
  finally
    C.Free;
  end;
end;

procedure TTestOPCriteria.TestAddAndIsNull;
var
  C: TOPCriteria;
begin
  C := TOPCriteria.Create;
  try
    C.Where('Name', opEqual, 'Alice').AddAndIsNull('DeletedAt');
    AssertEquals('SQL', 'Name = :p1 AND DeletedAt IS NULL', C.ToWhereSQL(Nil));
  finally
    C.Free;
  end;
end;

procedure TTestOPCriteria.TestAddOrIsNotNull;
var
  C: TOPCriteria;
begin
  C := TOPCriteria.Create;
  try
    C.WhereIsNull('Email').AddOrIsNotNull('Phone');
    AssertEquals('SQL', 'Email IS NULL OR Phone IS NOT NULL', C.ToWhereSQL(Nil));
  finally
    C.Free;
  end;
end;

procedure TTestOPCriteria.TestAddAndIn;
var
  C: TOPCriteria;
begin
  C := TOPCriteria.Create;
  try
    C.Where('Active', opEqual, True).AddAndIn('Category', ['A', 'B']);
    AssertEquals('SQL', 'Active = :p1 AND Category IN (:p2,:p3)', C.ToWhereSQL(Nil));
  finally
    C.Free;
  end;
end;

procedure TTestOPCriteria.TestAddAndBetween;
var
  C: TOPCriteria;
begin
  C := TOPCriteria.Create;
  try
    C.Where('Active', opEqual, True).AddAndBetween('Age', [18, 65]);
    AssertEquals('SQL', 'Active = :p1 AND Age BETWEEN :p2 AND :p3', C.ToWhereSQL(Nil));
  finally
    C.Free;
  end;
end;

procedure TTestOPCriteria.TestOrderByAsc;
var
  C: TOPCriteria;
begin
  C := TOPCriteria.Create;
  try
    C.OrderBy('Name');
    AssertEquals('SQL', 'ORDER BY Name', C.ToOrderBySQL(Nil));
  finally
    C.Free;
  end;
end;

procedure TTestOPCriteria.TestOrderByDesc;
var
  C: TOPCriteria;
begin
  C := TOPCriteria.Create;
  try
    C.OrderBy('Score', True);
    AssertEquals('SQL', 'ORDER BY Score DESC', C.ToOrderBySQL(Nil));
  finally
    C.Free;
  end;
end;

procedure TTestOPCriteria.TestOrderByMultiple;
var
  C: TOPCriteria;
begin
  C := TOPCriteria.Create;
  try
    C.OrderBy('Last').OrderBy('First');
    AssertEquals('SQL', 'ORDER BY Last, First', C.ToOrderBySQL(Nil));
  finally
    C.Free;
  end;
end;

procedure TTestOPCriteria.TestLimit;
var
  C: TOPCriteria;
begin
  C := TOPCriteria.Create;
  try
    C.Limit(10);
    AssertEquals('Limit', 10, C.GetLimitValue);
    AssertEquals('Offset', -1, C.GetOffsetValue);
  finally
    C.Free;
  end;
end;

procedure TTestOPCriteria.TestLimitOffset;
var
  C: TOPCriteria;
begin
  C := TOPCriteria.Create;
  try
    C.Limit(10).Offset(20);
    AssertEquals('Limit', 10, C.GetLimitValue);
    AssertEquals('Offset', 20, C.GetOffsetValue);
  finally
    C.Free;
  end;
end;

procedure TTestOPCriteria.TestIncludeDeleted;
var
  C: TOPCriteria;
begin
  C := TOPCriteria.Create;
  try
    C.Where('Name', opEqual, 'Alice').IncludeDeleted;
    AssertTrue('HasIncludeDeleted', C.HasIncludeDeleted);
  finally
    C.Free;
  end;
end;

procedure TTestOPCriteria.TestNoIncludeDeletedByDefault;
var
  C: TOPCriteria;
begin
  C := TOPCriteria.Create;
  try
    C.Where('Name', opEqual, 'Alice');
    AssertFalse('Not included by default', C.HasIncludeDeleted);
  finally
    C.Free;
  end;
end;

procedure TTestOPCriteria.TestEmptyCriteria;
var
  C: TOPCriteria;
begin
  C := TOPCriteria.Create;
  try
    AssertFalse('No WHERE', C.HasWhereClause);
    AssertEquals('Empty SQL', '', C.ToWhereSQL(Nil));
    AssertEquals('Empty OrderBy', '', C.ToOrderBySQL(Nil));
  finally
    C.Free;
  end;
end;

procedure TTestOPCriteria.TestEmptyWithLimit;
var
  C: TOPCriteria;
begin
  C := TOPCriteria.Create;
  try
    C.Limit(5);
    AssertFalse('No WHERE', C.HasWhereClause);
    AssertEquals('Limit set', 5, C.GetLimitValue);
  finally
    C.Free;
  end;
end;

procedure TTestOPCriteria.TestWhereBetweenDateTime;
var
  C: TOPCriteria;
  DT1, DT2: TDateTime;
begin
  C := TOPCriteria.Create;
  try
    DT1 := EncodeDate(2026, 1, 1);
    DT2 := EncodeDate(2026, 12, 31);
    C.WhereBetween('CreatedAt', DT1, DT2);
    AssertEquals('SQL', 'CreatedAt BETWEEN :p1 AND :p2', C.ToWhereSQL(Nil));
    AssertEquals('p1 type', Ord(ftDateTime), Ord(C.Params[0].DataType));
    AssertEquals('p2 type', Ord(ftDateTime), Ord(C.Params[1].DataType));
  finally
    C.Free;
  end;
end;

procedure TTestOPCriteria.TestWhereInDateTime;
var
  C: TOPCriteria;
  Dates: array of TDateTime;
begin
  C := TOPCriteria.Create;
  try
    SetLength(Dates, 2);
    Dates[0] := EncodeDate(2026, 3, 15);
    Dates[1] := EncodeDate(2026, 6, 20);
    C.WhereIn('BirthDate', Dates);
    AssertEquals('SQL', 'BirthDate IN (:p1,:p2)', C.ToWhereSQL(Nil));
    AssertEquals('Param count', 2, C.Params.Count);
    AssertEquals('p1 type', Ord(ftDateTime), Ord(C.Params[0].DataType));
    AssertEquals('p2 type', Ord(ftDateTime), Ord(C.Params[1].DataType));
  finally
    C.Free;
  end;
end;

procedure TTestOPCriteria.TestAddAndBetweenDateTime;
var
  C: TOPCriteria;
  DT1, DT2: TDateTime;
begin
  C := TOPCriteria.Create;
  try
    DT1 := EncodeDate(2026, 1, 1);
    DT2 := EncodeDate(2026, 12, 31);
    C.Where('Active', opEqual, True).AddAndBetween('HireDate', DT1, DT2);
    AssertEquals('SQL', 'Active = :p1 AND HireDate BETWEEN :p2 AND :p3', C.ToWhereSQL(Nil));
    AssertEquals('p2 type', Ord(ftDateTime), Ord(C.Params[1].DataType));
    AssertEquals('p3 type', Ord(ftDateTime), Ord(C.Params[2].DataType));
  finally
    C.Free;
  end;
end;

procedure TTestOPCriteria.TestParamTypeInteger;
var
  C: TOPCriteria;
begin
  C := TOPCriteria.Create;
  try
    C.Where('ID', opEqual, 42);
    AssertEquals('DataType', Ord(ftInteger), Ord(C.Params[0].DataType));
    AssertEquals('Value', 42, C.Params[0].AsInteger);
  finally
    C.Free;
  end;
end;

procedure TTestOPCriteria.TestParamTypeString;
var
  C: TOPCriteria;
begin
  C := TOPCriteria.Create;
  try
    C.Where('Name', opEqual, 'Bob');
    AssertEquals('DataType', Ord(ftString), Ord(C.Params[0].DataType));
    AssertEquals('Value', 'Bob', C.Params[0].AsString);
  finally
    C.Free;
  end;
end;

procedure TTestOPCriteria.TestParamTypeBoolean;
var
  C: TOPCriteria;
begin
  C := TOPCriteria.Create;
  try
    C.Where('Active', opEqual, False);
    AssertEquals('DataType', Ord(ftBoolean), Ord(C.Params[0].DataType));
    AssertFalse('Value', C.Params[0].AsBoolean);
  finally
    C.Free;
  end;
end;

procedure TTestOPCriteria.TestParamTypeDateTime;
var
  C: TOPCriteria;
  DT: TDateTime;
begin
  C := TOPCriteria.Create;
  try
    DT := EncodeDate(2026, 6, 15);
    C.Where('BirthDate', opGreater, DT);
    AssertEquals('DataType', Ord(ftDateTime), Ord(C.Params[0].DataType));
  finally
    C.Free;
  end;
end;

procedure TTestOPCriteria.TestParamCountNested;
var
  C, Sub: TOPCriteria;
begin
  C := TOPCriteria.Create;
  try
    Sub := TOPCriteria.Create;
    Sub.Where('X', opEqual, 10);
    C.Where('A', opEqual, 1).AddAnd(Sub);
    AssertEquals('2 params after merge', 2, C.Params.Count);
  finally
    C.Free;
  end;
end;

procedure TTestOPCriteria.TestFieldMapResolution;
var
  C: TOPCriteria;
  FM: TOPFieldMapList;
  F: TOPFieldMap;
begin
  C := TOPCriteria.Create;
  FM := TOPFieldMapList.Create(TOPFieldMap);
  try
    F := FM.Add as TOPFieldMap;
    F.PropertyName := 'FirstName';
    F.FieldName := 'first_name';
    F.PropertyKind := tkAString;
    C.Where('FirstName', opEqual, 'Alice');
    AssertEquals('Resolved', 'first_name = :p1', C.ToWhereSQL(FM));
  finally
    FM.Free;
    C.Free;
  end;
end;

initialization
  RegisterTest(TTestOPCriteria);
end.
