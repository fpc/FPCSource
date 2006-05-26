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
    procedure CreateTableWithFieldType(ADatatype : TFieldType; ASQLTypeDecl : string);
    procedure TestFieldDeclaration(ADatatype: TFieldType; ADataSize: integer);
  protected
    procedure SetUp; override; 
    procedure TearDown; override;
    procedure RunTest; override;
  published
    procedure TestInt;
    procedure TestString;
    procedure TestUnlVarChar;
    procedure TestDate;
    procedure TestDateTime;       // bug 6925
  end;

implementation

uses sqldbtoolsunit,toolsunit, variants;

procedure TTestFieldTypes.TestInt;

const
  testValuesCount = 17;
  testValues : Array[0..testValuesCount-1] of integer = (-maxInt,-maxSmallint-1,-maxSmallint,-256,-255,-128,-127,-1,0,1,127,128,255,256,maxSmallint,maxSmallint+1,MaxInt);

var
  i          : byte;

begin
  CreateTableWithFieldType(ftInteger,'INT');
  TestFieldDeclaration(ftInteger,4);

  for i := 0 to testValuesCount-1 do
    TSQLDBConnector(DBConnector).Connection.ExecuteDirect('insert into FPDEV2 (FT) values (' + inttostr(testValues[i]) + ')');

  with TSQLDBConnector(DBConnector).Query do
    begin
    Open;
    for i := 0 to testValuesCount-1 do
      begin
      AssertEquals(testValues[i],fields[0].AsInteger);
      Next;
      end;
    close;
    end;
end;

procedure TTestFieldTypes.TestString;

const
  testValuesCount = 19;
  testValues : Array[0..testValuesCount-1] of string = (
    '',
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
    '~`|{}- =',    // note that there's no \  (backslash) since some db's uses that as escape-character
    '  WRaP  ',
    'wRaP  ',
    ' wRAP'
  );

var
  i             : byte;

begin
  CreateTableWithFieldType(ftString,'VARCHAR(10)');
  TestFieldDeclaration(ftString,11);

  for i := 0 to testValuesCount-1 do
    TSQLDBConnector(DBConnector).Connection.ExecuteDirect('insert into FPDEV2 (FT) values (''' + testValues[i] + ''')');

  with TSQLDBConnector(DBConnector).Query do
    begin
    Open;
    for i := 0 to testValuesCount-1 do
      begin
      AssertEquals(testValues[i],fields[0].AsString);
      Next;
      end;
    close;
    end;
end;

procedure TTestFieldTypes.TestUnlVarChar;

const
  testValuesCount = 21;
  testValues : Array[0..testValuesCount-1] of string = (
    '',
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
    '~`|{}- =',
    '  WRaP  ',
    'wRaP  ',
    ' wRAP',
    '0123456789',
    'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 !@#$%^&*()_+-=][|}{;:,./<>?abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 !@#$%^&*()_+-=][|}{;:,./<>?' + 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 !@#$%^&*()_+-=][|}{;:,./<>?'
    + 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 !@#$%^&*()_+-=][|}{;:,./<>?abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 !@#$%^&*()_+-=][|}{;:,./<>?' + 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 !@#$%^&*()_+-=][|}{;:,./<>?'
  );

var
  i             : byte;

begin
  CreateTableWithFieldType(ftString,'VARCHAR');
  TestFieldDeclaration(ftString,dsMaxStringSize+1);

  for i := 0 to testValuesCount-1 do
    TSQLDBConnector(DBConnector).Connection.ExecuteDirect('insert into FPDEV2 (FT) values (''' + testValues[i] + ''')');

  with TSQLDBConnector(DBConnector).Query do
    begin
    Open;
    for i := 0 to testValuesCount-1 do
      begin
      AssertEquals(testValues[i],fields[0].AsString);
      Next;
      end;
    close;
    end;
end;

procedure TTestFieldTypes.TestDate;

const
  testValuesCount = 18;
  testValues : Array[0..testValuesCount-1] of string = (
    '2000-01-01',
    '1999-12-31',
    '2004-02-29',
    '2004-03-01',
    '1991-02-28',
    '1991-03-01',
    '2040-10-16',
    '1977-09-29',
    '1800-03-30',
    '1650-05-10',
    '1754-06-04',
    '0904-04-12',
    '0199-07-09',
    '0001-01-01',
    '1899-12-29',
    '1899-12-30',
    '1899-12-31',
    '1900-01-01'
  );

var
  i             : byte;

begin
  CreateTableWithFieldType(ftDate,'DATE');
  TestFieldDeclaration(ftDate,8);

  for i := 0 to testValuesCount-1 do
    TSQLDBConnector(DBConnector).Connection.ExecuteDirect('insert into FPDEV2 (FT) values (''' + testValues[i] + ''')');

//  TSQLDBConnector(DBConnector).Transaction.CommitRetaining; // For debug-purposes

  with TSQLDBConnector(DBConnector).Query do
    begin
    Open;
    for i := 0 to testValuesCount-1 do
      begin
      AssertEquals(testValues[i],FormatDateTime('yyyy/mm/dd',fields[0].AsDateTime));
      Next;
      end;
    close;
    end;

end;

procedure TTestFieldTypes.TestDateTime;

const
  testValuesCount = 31;
  testValues : Array[0..testValuesCount-1] of string = (
    '2000-01-01',
    '1999-12-31',
    '2004-02-29',
    '2004-03-01',
    '1991-02-28',
    '1991-03-01',
    '2040-10-16',
    '1977-09-29',
    '1800-03-30',
    '1650-05-10',
    '1754-06-04',
    '0904-04-12',
    '0199-07-09',
    '0001-01-01',
    '1899-12-29',
    '1899-12-30',
    '1899-12-31',
    '1900-01-01',
    '1899-12-30 18:00:51',
    '1899-12-30 04:00:51',
    '1899-12-29 04:00:51',
    '1899-12-29 18:00:51',
    '2000-01-01 10:00:00',
    '2000-01-01 23:59:59',
    '2100-01-01 01:01:01',
    '1400-02-03 12:21:53',
    '1333-02-03 21:44:21',
    '1994-03-06 11:54:30',
    '1903-04-02 01:04:02',
    '1815-09-24 03:47:22',
    '0354-11-20 21:25:15'
  );

var
  i             : byte;

begin
  CreateTableWithFieldType(ftDateTime,'TIMESTAMP');
  TestFieldDeclaration(ftDateTime,8);

  for i := 0 to testValuesCount-1 do
    TSQLDBConnector(DBConnector).Connection.ExecuteDirect('insert into FPDEV2 (FT) values (''' + testValues[i] + ''')');

  with TSQLDBConnector(DBConnector).Query do
    begin
    Open;
    for i := 0 to testValuesCount-1 do
      begin
      if length(testValues[i]) < 12 then
        AssertEquals(testValues[i],FormatDateTime('yyyy/mm/dd',fields[0].AsDateTime))
      else
        AssertEquals(testValues[i],FormatDateTime('yyyy/mm/dd hh:mm:ss',fields[0].AsDateTime));
      Next;
      end;
    close;
    end;
end;

procedure TTestFieldTypes.CreateTableWithFieldType(ADatatype: TFieldType;
  ASQLTypeDecl: string);
begin
  TSQLDBConnector(DBConnector).Connection.ExecuteDirect('create table FPDEV2 (FT ' +ASQLTypeDecl+ ')');

// Firebird/Interbase need a commit after a DDL statement. Not necessary for the other connections
  TSQLDBConnector(DBConnector).Transaction.CommitRetaining;
end;

procedure TTestFieldTypes.TestFieldDeclaration(ADatatype: TFieldType;
  ADataSize: integer);
begin
  with TSQLDBConnector(DBConnector).Query do
    begin
    SQL.Clear;
    SQL.Add('select * from FPDEV2');

    Open;
    AssertEquals(1,FieldCount);
    AssertTrue(CompareText('FT',fields[0].FieldName)=0);
    AssertEquals(ADataSize,fields[0].DataSize);
    AssertTrue(ADatatype=fields[0].DataType);
    Close;
    end;
end;

procedure TTestFieldTypes.SetUp; 
begin
  InitialiseDBConnector;
end;

procedure TTestFieldTypes.TearDown; 
begin
  if assigned(DBConnector) then
    TSQLDBConnector(DBConnector).Transaction.Rollback;
  FreeAndNil(DBConnector);
end;

procedure TTestFieldTypes.RunTest;
begin
  if (dbtype = 'interbase') or
     (dbtype = 'postgresql') then
    inherited RunTest;
end;

initialization
  RegisterTest(TTestFieldTypes);
end.

