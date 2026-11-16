unit utc.opf.linq;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
{$INTERFACES COM}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, FpcUnit.Test, FpcUnit.TestRegistry;
{$ELSE}
  Classes, SysUtils, fpcunit, testregistry;
{$ENDIF}

type

  { TTestOPLinq }

  TTestOPLinq = class(TTestCase)
  published
    // Basic comparisons
    procedure TestEqualInteger;
    procedure TestEqualString;
    procedure TestEqualBoolean;
    procedure TestEqualDateTime;
    procedure TestNotEqualInteger;
    procedure TestGreaterInteger;
    procedure TestLessInteger;
    procedure TestGreaterOrEqualInteger;
    procedure TestLessOrEqualInteger;
    // Logical operators
    procedure TestAnd;
    procedure TestOr;
    procedure TestNot;
    procedure TestAndOrNested;
    procedure TestTripleAnd;
    // Special conditions
    procedure TestIsNull;
    procedure TestIsNotNull;
    procedure TestLike;
    procedure TestNotLike;
    procedure TestContains;
    // Complex
    procedure TestRealisticQuery;
    // Parameters
    procedure TestParamTypes;
    procedure TestParamCountNested;
    // Field map resolution
    procedure TestFieldMapResolution;
    // Memory: no leaks verified by heaptrc externally
  end;

implementation

uses
  DB, Opf.Criteria, Opf.Fields, Opf.Linq;

{ TTestOPLinq }

procedure TTestOPLinq.TestEqualInteger;
var
  E: TOPLinqExpr;
  Cr: TOPCriteria;
begin
  E := Prop('Age') = 25;
  Cr := E.FCtx.GetCriteria;
  AssertEquals('SQL', 'Age = :p1', Cr.ToWhereSQL(nil));
  AssertEquals('Param', 25, Cr.Params[0].AsInteger);
end;

procedure TTestOPLinq.TestEqualString;
var
  E: TOPLinqExpr;
  Cr: TOPCriteria;
begin
  E := Prop('Name') = 'Alice';
  Cr := E.FCtx.GetCriteria;
  AssertEquals('SQL', 'Name = :p1', Cr.ToWhereSQL(nil));
  AssertEquals('Param', 'Alice', Cr.Params[0].AsString);
end;

procedure TTestOPLinq.TestEqualBoolean;
var
  E: TOPLinqExpr;
  Cr: TOPCriteria;
begin
  E := Prop('Active') = True;
  Cr := E.FCtx.GetCriteria;
  AssertEquals('SQL', 'Active = :p1', Cr.ToWhereSQL(nil));
  AssertTrue('Param', Cr.Params[0].AsBoolean);
  AssertEquals('DataType', Ord(ftBoolean), Ord(Cr.Params[0].DataType));
end;

procedure TTestOPLinq.TestEqualDateTime;
var
  E: TOPLinqExpr;
  Cr: TOPCriteria;
  DT: TDateTime;
begin
  DT := EncodeDate(2026, 6, 15);
  E := Prop('Created') = DT;
  Cr := E.FCtx.GetCriteria;
  AssertEquals('SQL', 'Created = :p1', Cr.ToWhereSQL(nil));
  AssertEquals('DataType', Ord(ftDateTime), Ord(Cr.Params[0].DataType));
end;

procedure TTestOPLinq.TestNotEqualInteger;
var
  E: TOPLinqExpr;
begin
  E := Prop('Status') <> 5;
  AssertEquals('SQL', 'Status <> :p1', E.FCtx.GetCriteria.ToWhereSQL(nil));
end;

procedure TTestOPLinq.TestGreaterInteger;
var
  E: TOPLinqExpr;
begin
  E := Prop('Age') > 18;
  AssertEquals('SQL', 'Age > :p1', E.FCtx.GetCriteria.ToWhereSQL(nil));
end;

procedure TTestOPLinq.TestLessInteger;
var
  E: TOPLinqExpr;
begin
  E := Prop('Age') < 65;
  AssertEquals('SQL', 'Age < :p1', E.FCtx.GetCriteria.ToWhereSQL(nil));
end;

procedure TTestOPLinq.TestGreaterOrEqualInteger;
var
  E: TOPLinqExpr;
begin
  E := Prop('Score') >= 90;
  AssertEquals('SQL', 'Score >= :p1', E.FCtx.GetCriteria.ToWhereSQL(nil));
end;

procedure TTestOPLinq.TestLessOrEqualInteger;
var
  E: TOPLinqExpr;
begin
  E := Prop('Price') <= 100;
  AssertEquals('SQL', 'Price <= :p1', E.FCtx.GetCriteria.ToWhereSQL(nil));
end;

procedure TTestOPLinq.TestAnd;
var
  E: TOPLinqExpr;
begin
  E := (Prop('Name') = 'Alice') and (Prop('Age') > 18);
  AssertEquals('SQL', 'Name = :p1 AND (Age > :p2)', E.FCtx.GetCriteria.ToWhereSQL(nil));
end;

procedure TTestOPLinq.TestOr;
var
  E: TOPLinqExpr;
begin
  E := (Prop('Dept') = 'Eng') or (Prop('Dept') = 'Sales');
  AssertEquals('SQL', 'Dept = :p1 OR (Dept = :p2)', E.FCtx.GetCriteria.ToWhereSQL(nil));
end;

procedure TTestOPLinq.TestNot;
var
  E: TOPLinqExpr;
begin
  E := not (Prop('Status') = 'Closed');
  AssertEquals('SQL', 'NOT (Status = :p1)', E.FCtx.GetCriteria.ToWhereSQL(nil));
end;

procedure TTestOPLinq.TestAndOrNested;
var
  E: TOPLinqExpr;
begin
  E := (Prop('Active') = True) and ((Prop('Age') > 18) or (Prop('VIP') = True));
  AssertEquals('SQL', 'Active = :p1 AND (Age > :p2 OR (VIP = :p3))',
    E.FCtx.GetCriteria.ToWhereSQL(nil));
end;

procedure TTestOPLinq.TestTripleAnd;
var
  E: TOPLinqExpr;
begin
  E := (Prop('A') = 1) and (Prop('B') = 2) and (Prop('C') = 3);
  AssertEquals('SQL', 'A = :p1 AND (B = :p2) AND (C = :p3)',
    E.FCtx.GetCriteria.ToWhereSQL(nil));
end;

procedure TTestOPLinq.TestIsNull;
var
  E: TOPLinqExpr;
begin
  E := Prop('DeletedAt').IsNull;
  AssertEquals('SQL', 'DeletedAt IS NULL', E.FCtx.GetCriteria.ToWhereSQL(nil));
  AssertEquals('No params', 0, E.FCtx.GetCriteria.Params.Count);
end;

procedure TTestOPLinq.TestIsNotNull;
var
  E: TOPLinqExpr;
begin
  E := Prop('Email').IsNotNull;
  AssertEquals('SQL', 'Email IS NOT NULL', E.FCtx.GetCriteria.ToWhereSQL(nil));
end;

procedure TTestOPLinq.TestLike;
var
  E: TOPLinqExpr;
begin
  E := Prop('Name').Like('A%');
  AssertEquals('SQL', 'Name LIKE :p1', E.FCtx.GetCriteria.ToWhereSQL(nil));
  AssertEquals('Param', 'A%', E.FCtx.GetCriteria.Params[0].AsString);
end;

procedure TTestOPLinq.TestNotLike;
var
  E: TOPLinqExpr;
begin
  E := Prop('Email').NotLike('%spam%');
  AssertEquals('SQL', 'Email NOT LIKE :p1', E.FCtx.GetCriteria.ToWhereSQL(nil));
end;

procedure TTestOPLinq.TestContains;
var
  E: TOPLinqExpr;
begin
  E := Prop('Name').Contains('ali');
  AssertEquals('SQL', 'Name LIKE :p1 ESCAPE ''!''', E.FCtx.GetCriteria.ToWhereSQL(nil));
  AssertEquals('Wrapped', '%ali%', E.FCtx.GetCriteria.Params[0].AsString);
end;

procedure TTestOPLinq.TestRealisticQuery;
var
  E: TOPLinqExpr;
  Cr: TOPCriteria;
begin
  E := (Prop('Active') = True)
       and ((Prop('Dept') = 'Eng') or (Prop('Dept') = 'Sales'))
       and (Prop('Age') >= 21)
       and Prop('Name').Contains('sm');
  Cr := E.FCtx.GetCriteria;
  AssertEquals('SQL',
    'Active = :p1 AND (Dept = :p2 OR (Dept = :p3)) AND (Age >= :p4) AND (Name LIKE :p5 ESCAPE ''!'')',
    Cr.ToWhereSQL(nil));
  AssertEquals('5 params', 5, Cr.Params.Count);
end;

procedure TTestOPLinq.TestParamTypes;
var
  E: TOPLinqExpr;
  Cr: TOPCriteria;
begin
  E := (Prop('Name') = 'Alice') and (Prop('Age') > 18) and (Prop('Active') = True);
  Cr := E.FCtx.GetCriteria;
  AssertEquals('p1 type', Ord(ftString), Ord(Cr.Params[0].DataType));
  AssertEquals('p2 type', Ord(ftInteger), Ord(Cr.Params[1].DataType));
  AssertEquals('p3 type', Ord(ftBoolean), Ord(Cr.Params[2].DataType));
end;

procedure TTestOPLinq.TestParamCountNested;
var
  E: TOPLinqExpr;
begin
  E := (Prop('A') = 1) and ((Prop('B') = 2) or (Prop('C') = 3));
  AssertEquals('3 params', 3, E.FCtx.GetCriteria.Params.Count);
end;

procedure TTestOPLinq.TestFieldMapResolution;
var
  E: TOPLinqExpr;
  FM: TOPFieldMapList;
  F: TOPFieldMap;
begin
  FM := TOPFieldMapList.Create(TOPFieldMap);
  try
    F := FM.Add as TOPFieldMap;
    F.PropertyName := 'FirstName';
    F.FieldName := 'first_name';
    F.PropertyKind := tkAString;
    E := Prop('FirstName') = 'Alice';
    AssertEquals('Resolved', 'first_name = :p1', E.FCtx.GetCriteria.ToWhereSQL(FM));
  finally
    FM.Free;
  end;
end;

initialization
  RegisterTest(TTestOPLinq);
end.
