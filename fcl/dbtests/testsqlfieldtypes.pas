unit TestSQLFieldTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,
  db;

type

  { TTestFieldTypes }

  TTestFieldTypes= class(TTestCase)
  private
    procedure TestFldType(ADatatype: TFieldType; ADataSize: integer; ASQLTypeDecl: string; ATestValuesStr: array of string; ATestValues: array of variant);
  protected
    procedure SetUp; override; 
    procedure TearDown; override;
    procedure RunTest; override;
  published
    procedure TestInt;
    procedure TestString;
    procedure TestDate;
  end;

implementation

uses sqldbtoolsunit,toolsunit, variants;

procedure TTestFieldTypes.TestInt;

const
  testValuesCount = 17;
  testValuesInt : Array[0..testValuesCount-1] of integer = (-maxInt,-maxSmallint-1,-maxSmallint,-256,-255,-128,-127,-1,0,1,127,128,255,256,maxSmallint,maxSmallint+1,MaxInt);

var
  TestValues    : array[0..testValuesCount-1] of variant;
  TestValuesStr : array[0..testValuesCount-1] of string;
  i             : byte;

begin
  for i := 0 to testValuesCount-1 do
    begin
    TestValues[i] := testValuesInt[i];
    TestValuesStr[i] := inttostr(testValuesInt[i]);
    end;
  TestFldType(ftInteger,4,'INT',TestValuesStr,TestValues);
end;

procedure TTestFieldTypes.TestString;

const
  testValuesCount = 15;
  testValuesS : Array[0..testValuesCount-1] of string = (
    'a',
    'ab',
    'abc',
    'abcd',
    'abcde',
    'abcdef',
    'abcdefg',
    'abcdefgh',
    'abcdefghi',
    'abcdefghij',
    'lMnOpQrStU',
    '1234567890',
    '_!@#$%^&*(',
    ')-;:/?.<>',
    '~`|{}-='
  );

var
  TestValues    : array[0..testValuesCount-1] of variant;
  TestValuesStr : array[0..testValuesCount-1] of string;
  i             : byte;

begin
  for i := 0 to testValuesCount-1 do
    begin
    TestValuesStr[i] := '''' + testValuesS[i] + '''';
    TestValues[i] := testValuesS[i];
    end;
  TestFldType(ftString,11,'VARCHAR(10)',TestValuesStr,TestValues);
end;



procedure TTestFieldTypes.TestDate;

const
  testValuesCount = 14;

var
  TestValues    : array[0..testValuesCount-1] of variant;
  TestValuesStr : array[0..testValuesCount-1] of string;
  i             : byte;
s : tdatetime;
begin
  TestValues[0] := EncodeDate(2000,1,1);
  TestValues[1] := EncodeDate(1999,12,31);
  TestValues[2] := EncodeDate(2004,2,29);
  TestValues[3] := EncodeDate(2004,3,1);
  TestValues[4] := EncodeDate(1991,2,28);
  TestValues[5] := EncodeDate(1991,3,1);
  TestValues[6] := EncodeDate(2040,10,16);
  TestValues[7] := EncodeDate(1977,9,29);
  TestValues[8] := EncodeDate(1800,3,30);
  TestValues[9] := EncodeDate(1650,5,1);
  TestValues[10] := EncodeDate(1754,6,4);
  TestValues[11] := EncodeDate(904,4,12);
  TestValues[12] := EncodeDate(199,7,9);
  TestValues[13] := EncodeDate(1,1,1);
  for i := 0 to testValuesCount-1 do
    TestValuesStr[i] := '''' + FormatDateTime('yyyy/mm/dd',testValues[i]) + '''';
  TestFldType(ftDate,8,'DATE',TestValuesStr,TestValues);
end;

procedure TTestFieldTypes.TestFldType(ADatatype: TFieldType; ADataSize: integer;
  ASQLTypeDecl: string; ATestValuesStr: array of string; ATestValues: array of variant);

var
  i               : byte;
  testValuesCount : integer;

  s : string;

begin
  testValuesCount := length(ATestValues);

  TSQLDBConnector(DBConnector).Connection.ExecuteDirect('create table FPDEV2 (FT ' +ASQLTypeDecl+ ')');

// Firebird/Interbase need a commit after a DDL statement. Not necessary for the other connections
  TSQLDBConnector(DBConnector).Transaction.CommitRetaining;

  with TSQLDBConnector(DBConnector).Query do
    begin
    SQL.Clear;
    SQL.Add('select * from FPDEV2');

    Open;
    AssertEquals(1,FieldCount);
    AssertEquals('FT',fields[0].FieldName);
    AssertEquals(ADataSize,fields[0].DataSize);
    AssertTrue(ADatatype=fields[0].DataType);
    Close;

    for i := 0 to testValuesCount-1 do
      TSQLDBConnector(DBConnector).Connection.ExecuteDirect('insert into FPDEV2 (FT) values (' + AtestValuesStr[i] + ')');
//  TSQLDBConnector(DBConnector).Transaction.CommitRetaining; // For debug-purposes
    Open;
    for i := 0 to testValuesCount-1 do
      begin
      AssertEquals(string(AtestValues[i]),fields[0].Value);
      Next;
      end;
    close;
    end;
  TSQLDBConnector(DBConnector).Transaction.Rollback;
end;

procedure TTestFieldTypes.SetUp; 
begin
  InitialiseDBConnector;
end;

procedure TTestFieldTypes.TearDown; 
begin
  FreeAndNil(DBConnector);
end;

procedure TTestFieldTypes.RunTest;
begin
  if dbtype = 'interbase' then
    inherited RunTest;
end;

initialization

  RegisterTest(TTestFieldTypes); 
end.

