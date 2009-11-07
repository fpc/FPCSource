unit TestFieldTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,
  db;

type


  TParamProc = procedure(AParam:TParam; i : integer);
  TFieldProc = procedure(AField:TField; i : integer);

  { TTestFieldTypes }

  TTestFieldTypes= class(TTestCase)
  private
    procedure CreateTableWithFieldType(ADatatype : TFieldType; ASQLTypeDecl : string);
    procedure TestFieldDeclaration(ADatatype: TFieldType; ADataSize: integer);
    procedure TestXXParamQuery(ADatatype : TFieldType; ASQLTypeDecl : string; testValuescount : integer; Cross : boolean = false);
    procedure TestSetBlobAsParam(asWhat : integer);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    procedure RunTest; override;
  published
    procedure TestEmptyUpdateQuery; // bug 13654
    procedure TestClearUpdateableStatus;
    procedure TestReadOnlyParseSQL; // bug 9254
    procedure TestParseJoins; // bug 10148
    procedure TestDoubleFieldNames; // bug 8457
    procedure TestParseUnion; // bug 8442
    procedure TestInsertLargeStrFields; // bug 9600
    procedure TestNumericNames; // Bug9661
    procedure TestApplyUpdFieldnames; // Bug 12275;
    procedure Test11Params;
    procedure TestRowsAffected; // bug 9758
    procedure TestStringsReplace;
    procedure TestCircularParams;
    procedure TestBug9744;
    procedure TestCrossStringDateParam;
    procedure TestGetFieldNames;
    procedure TestGetTables;
    procedure TestUpdateIndexDefs;
    procedure TestSetBlobAsMemoParam;
    procedure TestSetBlobAsBlobParam;
    procedure TestSetBlobAsStringParam;
    procedure TestNonNullableParams;
    procedure TestGetIndexDefs;
    procedure TestDblQuoteEscComments;
    procedure TestpfInUpdateFlag; // bug 7565
    procedure TestInt;
    procedure TestScript;
    procedure TestInsertReturningQuery;

    procedure TestTemporaryTable;
    procedure TestRefresh;

    procedure TestParametersAndDates;
    procedure TestExceptOnsecClose;
    procedure TestErrorOnEmptyStatement;

    procedure TestBlob;
    procedure TestChangeBlob;
    procedure TestBlobGetText;
    procedure TestBlobSize;

    procedure TestLargeRecordSize;
    procedure TestNumeric;
    procedure TestFloat;
    procedure TestDateTime;       // bug 6925
    procedure TestString;
    procedure TestUnlVarChar;
    procedure TestDate;

    procedure TestNullValues;
    procedure TestParamQuery;
    procedure TestStringParamQuery;
    procedure TestFixedStringParamQuery;
    procedure TestDateParamQuery;
    procedure TestIntParamQuery;
    procedure TestFloatParamQuery;
    procedure TestBCDParamQuery;
    procedure TestAggregates;

    procedure TestStringLargerThen8192;

    // SchemaType tests
    procedure TestTableNames;
    procedure TestFieldNames;
  end;

implementation

uses sqldbtoolsunit,toolsunit, variants, sqldb, bufdataset, strutils, dbconst;

Type HackedDataset = class(TDataset);

const
  testFloatValuesCount = 21;
  testFloatValues : Array[0..testFloatValuesCount-1] of double = (-maxSmallint-1,-maxSmallint,-256,-255,-128,-127,-1,0,1,127,128,255,256,maxSmallint,maxSmallint+1,0.123456,-0.123456,4.35,12.434E7,9.876e-5,123.45678);

  testBCDValuesCount = 10;
  testBCDValues : Array[0..testBCDValuesCount-1] of currency = (-100,54.53,1.2345,123.5345,0,1,-1,0,1.42,1324.4324);

  testIntValuesCount = 17;
  testIntValues : Array[0..testIntValuesCount-1] of integer = (-maxInt,-maxSmallint-1,-maxSmallint,-256,-255,-128,-127,-1,0,1,127,128,255,256,maxSmallint,maxSmallint+1,MaxInt);

  testDateValuesCount = 18;
  testDateValues : Array[0..testDateValuesCount-1] of string = (
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


procedure TTestFieldTypes.TestpfInUpdateFlag;
var ds   : TBufDataset;
    AFld1, AFld2, AFld3 : Tfield;
begin
  ds := (DBConnector.GetNDataset(True,5) as TBufDataset);
  with ds do
    begin
    AFld1 := TIntegerField.Create(ds);
    AFld1.FieldName := 'ID';
    AFld1.DataSet := ds;
    AFld1.ProviderFlags := AFld1.ProviderFlags + [pfInKey];

    AFld2 := TStringField.Create(ds);
    AFld2.FieldName := 'NAME';
    AFld2.DataSet := ds;

    AFld3 := TIntegerField.Create(ds);
    AFld3.FieldName := 'CALCFLD';
    AFld3.DataSet := ds;
    Afld3.FieldKind := fkCalculated;
    AFld3.ProviderFlags := [];

    Open;
    Edit;
    FieldByName('ID').AsInteger := 254;
    Post;
    ApplyUpdates;
    Append;
    FieldByName('ID').AsInteger := 255;
    Post;
    ApplyUpdates;
    Close;
    AFld1.Free;
    AFld2.Free;
    AFld3.Free;
    end;
end;

procedure TTestFieldTypes.TestScript;

var Ascript : TSQLScript;

begin
  Ascript := tsqlscript.create(nil);
  try
    with Ascript do
      begin
      DataBase := TSQLDBConnector(DBConnector).Connection;
      transaction := TSQLDBConnector(DBConnector).Transaction;
      script.clear;
      script.append('create table a (id int);');
      script.append('create table b (id int);');
      ExecuteScript;
      // Firebird/Interbase need a commit after a DDL statement. Not necessary for the other connections
      if SQLDbType=interbase then TSQLDBConnector(DBConnector).Transaction.CommitRetaining;

      end;
  finally
    TSQLDBConnector(DBConnector).Connection.ExecuteDirect('drop table a');
    TSQLDBConnector(DBConnector).Connection.ExecuteDirect('drop table b');
    // Firebird/Interbase need a commit after a DDL statement. Not necessary for the other connections
    if SQLDbType=interbase then TSQLDBConnector(DBConnector).Transaction.CommitRetaining;
  end;
end;

procedure TTestFieldTypes.TestInt;

var
  i          : byte;

begin
  CreateTableWithFieldType(ftInteger,'INT');
  TestFieldDeclaration(ftInteger,4);

  for i := 0 to testIntValuesCount-1 do
    TSQLDBConnector(DBConnector).Connection.ExecuteDirect('insert into FPDEV2 (FT) values (' + inttostr(testIntValues[i]) + ')');

  with TSQLDBConnector(DBConnector).Query do
    begin
    Open;
    for i := 0 to testIntValuesCount-1 do
      begin
      AssertEquals(testIntValues[i],fields[0].AsInteger);
      Next;
      end;
    close;
    end;
end;

procedure TTestFieldTypes.TestLargeRecordSize;

var
  i          : byte;

begin
  TSQLDBConnector(DBConnector).Connection.ExecuteDirect('create table FPDEV2 (plant varchar(8192),sampling_type varchar(8192),area varchar(8192), area_description varchar(8192), batch varchar(8192), sampling_datetime timestamp, status varchar(8192), batch_commentary varchar(8192))');

// Firebird/Interbase need a commit after a DDL statement. Not necessary for the other connections
  if UpperCase(dbconnectorparams)='INTERBASE' then TSQLDBConnector(DBConnector).Transaction.CommitRetaining;

  with TSQLDBConnector(DBConnector).Query do
    begin
    sql.clear;
    sql.append('insert into FPDEV2 (plant,sampling_type,batch,sampling_datetime,status,batch_commentary) values (''ZUBNE PASTE'',''OTISCI POVR￿INA'',''000037756'',''2005-07-01'',''NE ODGOVARA'',''Ovdje se upisuje komentar o kontrolnom broju..............'')');
    ExecSQL;

    sql.clear;
    sql.append('select * from FPDEV2');
    open;
    AssertEquals('ZUBNE PASTE',FieldByName('plant').AsString);
    AssertEquals(EncodeDate(2005,07,01),FieldByName('sampling_datetime').AsDateTime);
    close;
    end;
end;


procedure TTestFieldTypes.TestNumeric;

const
  testValuesCount = 13;
  testValues : Array[0..testValuesCount-1] of currency = (-123456.789,-10200,-10000,-1875.25,-10,-0.5,0,0.5,10,1875.25,10000,10200,123456.789);

var
  i          : byte;

begin
  CreateTableWithFieldType(ftBCD,'NUMERIC(10,4)');
  TestFieldDeclaration(ftBCD,sizeof(Currency));

  for i := 0 to testValuesCount-1 do
    TSQLDBConnector(DBConnector).Connection.ExecuteDirect('insert into FPDEV2 (FT) values (' + CurrToStrF(testValues[i],ffFixed,3) + ')');

  with TSQLDBConnector(DBConnector).Query do
    begin
    Open;
    for i := 0 to testValuesCount-1 do
      begin
      AssertEquals(testValues[i],fields[0].AsCurrency);
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
      if (SQLDbType in [mysql40,mysql41]) then
        AssertEquals(TrimRight(testValues[i]),fields[0].AsString) // MySQL < 5.0.3 automatically trims strings
      else
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
  if SQLDbType<>postgresql then Ignore('This test does only apply to Postgres, since others don''t support varchars without length given');

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

var
  i             : byte;

begin
  CreateTableWithFieldType(ftDate,'DATE');
  TestFieldDeclaration(ftDate,8);

  for i := 0 to testDateValuesCount-1 do
    if SQLDbType=oracle then
      TSQLDBConnector(DBConnector).Connection.ExecuteDirect('insert into FPDEV2 (FT) values (to_date (''' + testDateValues[i] + ''',''YYYY-MM-DD''))')
    else
      TSQLDBConnector(DBConnector).Connection.ExecuteDirect('insert into FPDEV2 (FT) values (''' + testDateValues[i] + ''')');

//  TSQLDBConnector(DBConnector).Transaction.CommitRetaining; // For debug-purposes

  with TSQLDBConnector(DBConnector).Query do
    begin
    Open;
    for i := 0 to testDateValuesCount-1 do
      begin
      AssertEquals(testDateValues[i],FormatDateTime('yyyy/mm/dd',fields[0].AsDateTime));
      Next;
      end;
    close;
    end;

end;

procedure TTestFieldTypes.TestChangeBlob;

var s : string;

begin
  TSQLDBConnector(DBConnector).Connection.ExecuteDirect('create table FPDEV2 (ID int,FT '+FieldtypeDefinitions[ftblob]+')');
  TSQLDBConnector(DBConnector).Transaction.CommitRetaining; // For interbase

  TSQLDBConnector(DBConnector).Connection.ExecuteDirect('insert into FPDEV2 (ID,FT) values (1,''Test deze blob'')');

  with TSQLDBConnector(DBConnector).Query do
    begin
    sql.clear;
    sql.add('select * from FPDEV2');
    Open;
    fields[1].ProviderFlags := [pfInUpdate]; // blob niet in de where
    UpdateMode := upWhereAll;

    AssertEquals('Test deze blob',fields[1].AsString);
    edit;
// Dat werkt niet lekker, omdat de stream vernield wordt...
//    fields[0].asstring := 'Deze blob is gewijzigd!';

    With Createblobstream(fields[1],bmwrite) do
      begin
      s := 'Deze blob is gewijzigd!';
      WriteBuffer(Pointer(s)^,Length(s));
      post;
      free;
      end;
    AssertEquals('Deze blob is gewijzigd!',fields[1].AsString);

    ApplyUpdates(0);

    TSQLDBConnector(DBConnector).Transaction.CommitRetaining; // For debug-purposes

    close;

    open;
    AssertEquals('Deze blob is gewijzigd!',fields[1].AsString);
    close;
    end;
end;

procedure TTestFieldTypes.TestBlobGetText;
begin
  CreateTableWithFieldType(ftBlob,FieldtypeDefinitions[ftBlob]);
  TestFieldDeclaration(ftBlob,0);

  TSQLDBConnector(DBConnector).Connection.ExecuteDirect('insert into FPDEV2 (FT) values (''Test deze blob'')');
  TSQLDBConnector(DBConnector).Connection.ExecuteDirect('insert into FPDEV2 (FT) values (Null)');

//  TSQLDBConnector(DBConnector).Transaction.CommitRetaining; // For debug-purposes

  with TSQLDBConnector(DBConnector).Query do
    begin
    Open;
    AssertFalse(fields[0].IsNull);
    AssertEquals('(BLOB)',fields[0].DisplayText);
    AssertEquals('Test deze blob',fields[0].AsString);
    Next;
    AssertTrue(fields[0].IsNull);
    AssertEquals('(blob)',fields[0].Text);
    AssertEquals('',fields[0].AsString);
    close;
    end;
end;

procedure TTestFieldTypes.TestBlobSize;
begin
  CreateTableWithFieldType(ftBlob,FieldtypeDefinitions[ftBlob]);

  TSQLDBConnector(DBConnector).Connection.ExecuteDirect('insert into FPDEV2 (FT) values (''Test deze blob'')');

  with TSQLDBConnector(DBConnector).Query do
    begin
    sql.text := 'select * from FPDEV2';
    Open;
    AssertEquals(14,TBlobField(fields[0]).BlobSize);
    close;
    end;
end;

procedure TTestFieldTypes.TestSetBlobAsStringParam;

begin
  TestSetBlobAsParam(1);
end;


procedure TTestFieldTypes.TestBlob;

begin
  CreateTableWithFieldType(ftBlob,FieldtypeDefinitions[ftBlob]);
  TestFieldDeclaration(ftBlob,0);

  TSQLDBConnector(DBConnector).Connection.ExecuteDirect('insert into FPDEV2 (FT) values (''Test deze blob'')');

//  TSQLDBConnector(DBConnector).Transaction.CommitRetaining; // For debug-purposes

  with TSQLDBConnector(DBConnector).Query do
    begin
    Open;
    AssertEquals('Test deze blob',fields[0].AsString);
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
    '1977-09-29',
    '2000-01-01 10:00:00',
    '2000-01-01 23:59:59',
    '1994-03-06 11:54:30',
    '2040-10-16',                   // MySQL 4.0 doesn't support datetimes before 1970 or after 2038
    '1400-02-03 12:21:53',
    '0354-11-20 21:25:15',
    '1333-02-03 21:44:21',
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
    '1903-04-02 01:04:02',
    '1815-09-24 03:47:22',
    '2100-01-01 01:01:01'
  );

var
  i, corrTestValueCount : byte;

begin
  CreateTableWithFieldType(ftDateTime,FieldtypeDefinitions[ftDateTime]);
  TestFieldDeclaration(ftDateTime,8);

  if SQLDbType=mysql40 then corrTestValueCount := testValuesCount-21
    else corrTestValueCount := testValuesCount;

  for i := 0 to corrTestValueCount-1 do
    if SQLDbType=oracle then
      TSQLDBConnector(DBConnector).Connection.ExecuteDirect('insert into FPDEV2 (FT) values (to_date (''' + testValues[i] + ''',''YYYY-MM-DD HH24:MI:SS''))')
    else
      TSQLDBConnector(DBConnector).Connection.ExecuteDirect('insert into FPDEV2 (FT) values (''' + testValues[i] + ''')');

  with TSQLDBConnector(DBConnector).Query do
    begin
    Open;
    for i := 0 to corrTestValueCount-1 do
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

procedure TTestFieldTypes.TestFloat;
const
  testValuesCount = 21;
  testValues : Array[0..testValuesCount-1] of double = (-maxSmallint-1,-maxSmallint,-256,-255,-128,-127,-1,0,1,127,128,255,256,maxSmallint,maxSmallint+1,0.123456,-0.123456,4.35,12.434E7,9.876e-5,123.45678);

var
  i          : byte;

begin
  CreateTableWithFieldType(ftFloat,'FLOAT');
  TestFieldDeclaration(ftFloat,sizeof(double));

  for i := 0 to testValuesCount-1 do
    TSQLDBConnector(DBConnector).Connection.ExecuteDirect('insert into FPDEV2 (FT) values (' + floattostr(testValues[i]) + ')');

  with TSQLDBConnector(DBConnector).Query do
    begin
    Open;
    for i := 0 to testValuesCount-1 do
      begin
      AssertEquals(testValues[i],fields[0].AsFloat);
      Next;
      end;
    close;
    end;
end;

procedure TTestFieldTypes.TestNullValues;
begin
  TSQLDBConnector(DBConnector).Connection.ExecuteDirect('create table FPDEV2 (FIELD1 INT, FIELD2 INT)');
// Firebird/Interbase need a commit after a DDL statement. Not necessary for the other connections
  TSQLDBConnector(DBConnector).Transaction.CommitRetaining;

  TSQLDBConnector(DBConnector).Connection.ExecuteDirect('insert into FPDEV2 (FIELD1) values (1)');

  with TSQLDBConnector(DBConnector).Query do
    begin
    sql.clear;
    sql.append('select * from FPDEV2');
    open;
    AssertEquals(1,FieldByName('FIELD1').AsInteger);
    AssertTrue('Null-values test failed',FieldByName('FIELD2').IsNull);
    close;
    end;
end;


procedure TTestFieldTypes.TestParamQuery;
begin
  TSQLDBConnector(DBConnector).Connection.ExecuteDirect('create table FPDEV2 (FIELD1 INT, FIELD2 INT, FIELD3 INT, DECOY VARCHAR(30))');

// Firebird/Interbase need a commit after a DDL statement. Not necessary for the other connections
  TSQLDBConnector(DBConnector).Transaction.CommitRetaining;

  with TSQLDBConnector(DBConnector).Query do
    begin
    sql.clear;
    sql.append('insert into FPDEV2 (field1) values (:field1)');
    Params.ParamByName('field1').AsInteger := 1;
    ExecSQL;

    sql.clear;
    sql.append('insert into FPDEV2 (field1,field2,decoy) values (:field1,:field2,''decoytest'')');
    Params.ParamByName('field1').AsInteger := 2;
    Params.ParamByName('field2').DataType := ftInteger;
    Params.ParamByName('field2').Value := Null;
    ExecSQL;

    sql.clear;
    sql.append('insert into FPDEV2 (field1,field2,field3) values (:field1,:field2,:field3)');
    Params.ParamByName('field1').AsInteger := 3;
    Params.ParamByName('field2').AsInteger := 2;
    Params.ParamByName('field3').AsInteger := 3;
    ExecSQL;

    sql.clear;
    sql.append('insert into FPDEV2 (field1,field2,field3,decoy) values (:field1,:field2,:field3,'':decoy ::test $decoy2 $$2'')');
    Params.ParamByName('field1').AsInteger := 4;
    Params.ParamByName('field2').AsInteger := 2;
    Params.ParamByName('field3').AsInteger := 3;
    ExecSQL;

    sql.clear;
    sql.append('insert into FPDEV2 (field1,field2,field3) values (:field1,:field2,:field1)');
    Params.ParamByName('field1').AsInteger := 5;
    Params.ParamByName('field2').AsInteger := 2;
    ExecSQL;

    sql.clear;
    sql.append('select * from FPDEV2 order by FIELD1');
    open;
    AssertEquals(1,FieldByName('FIELD1').asinteger);
    AssertTrue(FieldByName('FIELD2').IsNull);
    AssertTrue(FieldByName('FIELD3').IsNull);
    AssertTrue(FieldByName('DECOY').IsNull);
    next;
    AssertEquals(2,FieldByName('FIELD1').asinteger);
    AssertTrue(FieldByName('FIELD2').IsNull);
    AssertTrue(FieldByName('FIELD3').IsNull);
    AssertEquals('decoytest',FieldByName('DECOY').AsString);
    next;
    AssertEquals(3,FieldByName('FIELD1').asinteger);
    AssertEquals(2,FieldByName('FIELD2').asinteger);
    AssertEquals(3,FieldByName('FIELD3').asinteger);
    AssertTrue(FieldByName('DECOY').IsNull);
    next;
    AssertEquals(4,FieldByName('FIELD1').asinteger);
    AssertEquals(2,FieldByName('FIELD2').asinteger);
    AssertEquals(3,FieldByName('FIELD3').asinteger);
    AssertEquals(':decoy ::test $decoy2 $$2',FieldByName('DECOY').AsString);
    next;
    AssertEquals(5,FieldByName('FIELD1').asinteger);
    AssertEquals(2,FieldByName('FIELD2').asinteger);
    AssertEquals(5,FieldByName('FIELD3').asinteger);
    AssertTrue(FieldByName('DECOY').IsNull);
    close;

    end;
  TSQLDBConnector(DBConnector).Transaction.CommitRetaining;


end;

procedure TTestFieldTypes.TestIntParamQuery;

begin
  TestXXParamQuery(ftInteger,'INT',testIntValuesCount);
end;

procedure TTestFieldTypes.TestFloatParamQuery;

begin
  TestXXParamQuery(ftFloat,'FLOAT',testFloatValuesCount);
end;

procedure TTestFieldTypes.TestBCDParamQuery;
begin
  TestXXParamQuery(ftBCD,'NUMERIC(10,4)',testBCDValuesCount);
end;

procedure TTestFieldTypes.TestStringParamQuery;

begin
  TestXXParamQuery(ftString,'VARCHAR(10)',testValuesCount);
end;

procedure TTestFieldTypes.TestFixedStringParamQuery;
begin
  TestXXParamQuery(ftFixedChar,'CHAR(10)',testValuesCount);
end;

procedure TTestFieldTypes.TestDateParamQuery;

begin
  TestXXParamQuery(ftDate,'DATE',testDateValuesCount);
end;


procedure TTestFieldTypes.TestXXParamQuery(ADatatype : TFieldType; ASQLTypeDecl : string; testValuescount : integer; Cross : boolean = false);

var i : integer;

begin
  TSQLDBConnector(DBConnector).Connection.ExecuteDirect('create table FPDEV2 (ID INT, FIELD1 '+ASQLTypeDecl+')');

// Firebird/Interbase need a commit after a DDL statement. Not necessary for the other connections
  if SQLDbType=interbase then TSQLDBConnector(DBConnector).Transaction.CommitRetaining;

  with TSQLDBConnector(DBConnector).Query do
    begin
    PacketRecords := -1;
    sql.clear;
    sql.append('insert into FPDEV2 (ID,FIELD1) values (:id,:field1)');

    ShortDateFormat := 'yyyy-mm-dd';

    for i := 0 to testValuesCount -1 do
      begin
      Params.ParamByName('id').AsInteger := i;
      case ADataType of
        ftInteger: Params.ParamByName('field1').asinteger := testIntValues[i];
        ftFloat  : Params.ParamByName('field1').AsFloat   := testFloatValues[i];
        ftBCD    : Params.ParamByName('field1').AsCurrency:= testBCDValues[i];
        ftFixedChar,
        ftString : Params.ParamByName('field1').AsString  := testStringValues[i];
        ftDate   : if cross then
                     Params.ParamByName('field1').AsString:= testDateValues[i]
                   else
                     Params.ParamByName('field1').AsDateTime:= StrToDate(testDateValues[i]);
      else
        AssertTrue('no test for paramtype available',False);
      end;
      ExecSQL;
      end;
    TSQLDBConnector(DBConnector).Transaction.CommitRetaining;

    sql.clear;
    sql.append('select * from FPDEV2 order by ID');
    open;

    for i := 0 to testValuesCount -1 do
      begin
      AssertEquals(i,FieldByName('ID').AsInteger);
      case ADataType of
        ftInteger: AssertEquals(testIntValues[i],FieldByName('FIELD1').AsInteger);
        ftFloat  : AssertEquals(testFloatValues[i],FieldByName('FIELD1').AsFloat);
        ftBCD    : AssertEquals(testBCDValues[i],FieldByName('FIELD1').AsCurrency);
        ftFixedChar :
                   begin
                   if FieldByName('FIELD1').isnull then
                     AssertEquals(testStringValues[i],FieldByName('FIELD1').AsString)
                   else
                     AssertEquals(PadRight(testStringValues[i],10),FieldByName('FIELD1').AsString);
                   end;
        ftString : AssertEquals(testStringValues[i],FieldByName('FIELD1').AsString);
        ftdate   : AssertEquals(testDateValues[i],FormatDateTime('yyyy/mm/dd',FieldByName('FIELD1').AsDateTime));
      else
        AssertTrue('no test for paramtype available',False);
      end;
      Next;
      end;
    close;
    end;
  TSQLDBConnector(DBConnector).Transaction.CommitRetaining;
end;

procedure TTestFieldTypes.TestSetBlobAsParam(asWhat: integer);
var
  i             : byte;
  ASQL          : TSQLQuery;

begin
  CreateTableWithFieldType(ftBlob,FieldtypeDefinitions[ftBlob]);
  TestFieldDeclaration(ftBlob,0);

  ASQL := DBConnector.GetNDataset(True,1) as tsqlquery;
  with ASql  do
    begin
    sql.Text := 'insert into FPDEV2 (FT) values (:BlobParam)';
    case asWhat of
      0: Params.ParamByName('blobParam').AsMemo := 'Test deze BLob';
      1: Params.ParamByName('blobParam').AsString := 'Test deze BLob';
      2: Params.ParamByName('blobParam').AsBlob := 'Test deze BLob';
    end;
    ExecSQL;
    end;

  with TSQLDBConnector(DBConnector).Query do
    begin
    Open;
    if not eof then
      AssertEquals('Test deze BLob',fields[0].AsString);
    close;
    end;
end;

procedure TTestFieldTypes.TestAggregates;
begin
  TSQLDBConnector(DBConnector).Connection.ExecuteDirect('create table FPDEV2 (FIELD1 INT, FIELD2 INT)');
// Firebird/Interbase need a commit after a DDL statement. Not necessary for the other connections
  TSQLDBConnector(DBConnector).Transaction.CommitRetaining;

  TSQLDBConnector(DBConnector).Connection.ExecuteDirect('insert into FPDEV2 values (1,1)');
  TSQLDBConnector(DBConnector).Connection.ExecuteDirect('insert into FPDEV2 values (2,3)');
  TSQLDBConnector(DBConnector).Connection.ExecuteDirect('insert into FPDEV2 values (3,4)');
  TSQLDBConnector(DBConnector).Connection.ExecuteDirect('insert into FPDEV2 values (4,4)');

  TSQLDBConnector(DBConnector).Transaction.CommitRetaining;

  with TSQLDBConnector(DBConnector).Query do
    begin
    sql.clear;
    sql.append('select count(*) from FPDEV2');
    open;
    AssertEquals(4,Fields[0].AsInteger);
    close;

    sql.clear;
    sql.append('select sum(FIELD1) from FPDEV2');
    open;
    AssertEquals(10,Fields[0].AsInteger);
    close;

    sql.clear;
    sql.append('select avg(FIELD2) from FPDEV2');
    open;
    AssertEquals(3,Fields[0].AsInteger);
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
//  if (SQLDbType in TSQLDBTypes) then
    inherited RunTest;
end;

procedure TTestFieldTypes.TestRefresh;
var ADataset: TDataset;
    i: integer;
    AFldID, AFldName: TField;
begin
  ADataset := TSQLDBConnector(DBConnector).GetNDataset(true,5);

  Adataset.Open;
  AFldId:=Adataset.Fields[0];
  AFldName:=Adataset.Fields[1];
  for i := 1 to 5 do
    begin
    AssertEquals(i,AFldID.asinteger);
    AssertEquals('TestName'+inttostr(i),AFldName.asstring);
    ADataset.Next;
    end;

  ADataset.Next;
  AssertTrue(ADataset.EOF);
  TSQLDBConnector(DBConnector).Connection.ExecuteDirect('update FPDEV set NAME=''test'' where ID=2');

  ADataset.Refresh;

  ADataset.First;
  for i := 1 to 5 do
    begin
    AssertEquals(i,AFldID.AsInteger);
    if i = 2 then
      AssertEquals('test',AFldName.AsString)
    else
      AssertEquals('TestName'+inttostr(i),AFldName.AsString);
    ADataset.Next;
    end;
  ADataset.Next;
  AssertTrue(ADataset.EOF);
end;

procedure TTestFieldTypes.TestEmptyUpdateQuery;
begin
  TSQLDBConnector(DBConnector).Connection.ExecuteDirect('update FPDEV set name=''nothing'' where (1=0)');
end;

procedure TTestFieldTypes.TestNonNullableParams;
var ASQLQuery : TSQLQuery;
    Passed: Boolean;
begin
  // Check for an exception when a null value is stored into a non-nullable
  // field using a parameter
  // There was a bug in IBConnection so that in this case the last used value
  // for the parameter was used.

  // To make sure that any changes are cancelled in the case the test fails
  TSQLDBConnector(DBConnector).GetNDataset(true,5);

  ASQLQuery := TSQLDBConnector(DBConnector).Query;
  ASQLQuery.SQL.text := 'update fpdev set ID=:ID1 where id = :ID2';
  ASQLQuery.Params[0].Clear;
  ASQLQuery.Params[1].AsInteger := 1;
  AssertTrue(ASQLQuery.Params[0].IsNull);
  Passed:=False;
  try
    @ASQLQuery.ExecSQL;
  except
    on E: Exception do
      if E.ClassType.InheritsFrom(EDatabaseError) then
        Passed := true;
  end;
  AssertTrue(Passed);
end;

procedure TTestFieldTypes.TestStringLargerThen8192;

var
  s             : string;
  i             : integer;

begin
  CreateTableWithFieldType(ftString,'VARCHAR(9000)');
  TestFieldDeclaration(ftString,9001);

  setlength(s,9000);
  for i := 1 to 9000 do
    s[i]:=chr((i mod 10)+ord('a'));
  TSQLDBConnector(DBConnector).Connection.ExecuteDirect('insert into FPDEV2 (FT) values (''' + s + ''')');

  with TSQLDBConnector(DBConnector).Query do
    begin
    Open;
    AssertEquals(s,fields[0].AsString);
    close;
    end;
end;

procedure TTestFieldTypes.TestTableNames;
var TableList : TStringList;
    i         : integer;
begin
  TableList := TStringList.Create;
  try
    TSQLDBConnector(DBConnector).Connection.GetTableNames(TableList);
    AssertTrue(TableList.Find('fpdev',i));
  finally
    TableList.Free;
  end;
end;

procedure TTestFieldTypes.TestFieldNames;
var FieldList : TStringList;
    i         : integer;
begin
  FieldList := TStringList.Create;
  try
    TSQLDBConnector(DBConnector).Connection.GetFieldNames('fpdev',FieldList);
    AssertTrue(FieldList.Find('id',i));
  finally
    FieldList.Free;
  end;
end;

procedure TTestFieldTypes.TestInsertReturningQuery;
begin
  if (SQLDbType <> interbase) then Ignore('This test does only apply to Firebird.');
  with TSQLDBConnector(DBConnector) do
    begin
    // This only works with databases that supports 'insert into .. returning'
    // for example, Firebird version 2.0 and up
    CreateTableWithFieldType(ftInteger,'int');
    Query.SQL.Text:='insert into FPDEV2 values(154) returning FT';
    Query.Open;
    AssertEquals('FT',Query.fields[0].FieldName);
    AssertEquals(154,Query.fields[0].AsInteger);
    Query.Close;
    end;
end;

procedure TTestFieldTypes.TestClearUpdateableStatus;
// Test if CanModify is correctly disabled in case of a select query without
// a from-statement.
begin
  if not (SQLDbType in MySQLdbTypes) then Ignore('This test does only apply to MySQL because the used SQL-statement is MySQL only.');
  with TSQLDBConnector(DBConnector) do
    begin
    with (GetNDataset(false,5) as TSQLQuery) do
      begin
      Open;
      AssertEquals(True,CanModify);
      Close;
      SQL.Text:='select last_insert_id();';
      Open;
      AssertEquals(False,CanModify);
      close;
      end;
    end;
end;

procedure TTestFieldTypes.TestReadOnlyParseSQL;
begin
  with TSQLDBConnector(DBConnector) do
    begin

    GetFieldDataset(True);
    with query do
      begin
      AssertFalse(ReadOnly);
      AssertTrue(ParseSQL);

      // If ParseSQL is false, and no update-queries are given, the query
      // shouldn't be updateable after open.
      ParseSQL := False;
      AssertFalse(ParseSQL);
      AssertFalse(ReadOnly);
      SQL.Text := 'select * from FPDEV;';
      open;
      AssertFalse(ParseSQL);
      AssertFalse(ReadOnly);
      AssertFalse(CanModify);
      close;

      // If ParseSQL is true, the query should be updateable after open.
      ReadOnly := False;
      ParseSQL := True;
      AssertTrue(ParseSQL);
      AssertFalse(ReadOnly);
      SQL.Text := 'select * from FPDEV;';
      open;
      AssertTrue(ParseSQL);
      AssertFalse(ReadOnly);
      AssertTrue(CanModify);
      edit;
      FieldByName('ID').AsInteger:=321;
      post;
      Applyupdates;
      close;

      // If ParseSQL is true, but the supplied query isn't updateable, then
      // the query shouldn't be updateable after open.
      ReadOnly := False;
      SQL.Text:='select ID,NAME from FPDEV where ID<5';
      sql.Add('union');
      sql.Add('select ID,NAME from FPDEV where ID>5');
      AssertTrue(ParseSQL);
      AssertFalse(ReadOnly);
      open;
      AssertTrue(ParseSQL);
      AssertFalse(ReadOnly);
      AssertFalse(CanModify);
      close;

      // As above, but now with an update-query, so that the query should
      // be updateable again.
      ReadOnly := False;
      AssertTrue(ParseSQL);
      AssertFalse(ReadOnly);
      UpdateSQL.Text:='update FPDEV set ID=:ID where ID=:OLD_ID';
      open;
      AssertTrue(ParseSQL);
      AssertFalse(ReadOnly);
      AssertTrue(CanModify);
      edit;
      post;
      Applyupdates;
      close;

      // Also if ParseSQL is False, the query should be updateable if a update-
      // query is given.
      ReadOnly := False;
      ParseSQL := False;
      AssertFalse(ParseSQL);
      AssertFalse(ReadOnly);
      open;
      AssertFalse(ParseSQL);
      AssertFalse(ReadOnly);
      AssertTrue(CanModify);
      edit;
      FieldByName('ID').AsInteger:=1;
      post;
      Applyupdates;
      close;

      // But if ReadOnly is true, then CanModify should always be false
      ReadOnly := True;
      ParseSQL := False;
      AssertFalse(ParseSQL);
      AssertTrue(ReadOnly);
      open;
      AssertFalse(ParseSQL);
      AssertTrue(ReadOnly);
      AssertFalse(CanModify);
      close;
      end;
    end;
end;

procedure TTestFieldTypes.TestParseJoins;
begin
  with TSQLDBConnector(DBConnector) do
    begin
    with query do
      begin
      SQL.Text:='select TT.NAME from FPDEV left join FPDEV TT on TT.ID=FPDEV.ID';
      Open;
      close;
      end;
    end;
end;

procedure TTestFieldTypes.TestDoubleFieldNames;
begin
  with TSQLDBConnector(DBConnector) do
    begin
    with query do
      begin
      SQL.Text:='select FPDEV.*,TT.* from FPDEV left join FPDEV TT on TT.ID=FPDEV.ID';
      Open;
      AssertTrue(assigned(FindField('ID')));
      AssertTrue (assigned(FindField('ID_1')));
      AssertTrue(assigned(FindField('NAME')));
      AssertTrue(assigned(FindField('NAME_1')));

      AssertEquals(1,fieldbyname('ID').AsInteger);
      AssertEquals(1,fieldbyname('ID_1').AsInteger);
      AssertEquals('TestName1',fieldbyname('NAME').AsString);
      AssertEquals('TestName1',fieldbyname('NAME_1').AsString);
      close;
      end;
    end;
end;

procedure TTestFieldTypes.TestParseUnion;
begin
  with TSQLDBConnector(DBConnector) do
    begin
    with query do
      begin
      SQL.Text:='select NAME from FPDEV where ID<5';
      sql.Add('union');
      sql.Add('select NAME from FPDEV where ID>5');
      Open;
      close;
      end;
    end;
end;

procedure TTestFieldTypes.TestInsertLargeStrFields;
begin
  with TSQLDBConnector(DBConnector) do
    begin
    Connection.ExecuteDirect('create table FPDEV2 (         ' +
                              '  ID INT NOT NULL          , ' +
                              '  NAME VARCHAR(16000),       ' +
                              '  PRIMARY KEY (ID)           ' +
                              ')                            ');
// Firebird/Interbase need a commit after a DDL statement. Not necessary for the other connections
    TSQLDBConnector(DBConnector).Transaction.CommitRetaining;
    Query.SQL.Text := 'insert into FPDEV2(ID,NAME) values (1,''test1'')';
    Query.ExecSQL;
    query.sql.Text:='select * from FPDEV2';
    Query.Open;
    AssertEquals(query.FieldByName('NAME').AsString,'test1');
    Query.insert;
    query.fields[1].AsString:='11';
    query.Close;
    end;
end;

procedure TTestFieldTypes.TestNumericNames;
begin
  with TSQLDBConnector(DBConnector) do
    begin
    if not (SQLDbType in MySQLdbTypes) then
      Connection.ExecuteDirect('create table FPDEV2 (         ' +
                                '  "2ID" INT NOT NULL            , ' +
                                '  "3TEST" VARCHAR(10),     ' +
                                '  PRIMARY KEY ("2ID")           ' +
                                ')                            ')
    else
      Connection.ExecuteDirect('create table FPDEV2 (         ' +
                                '  2ID INT NOT NULL            , ' +
                                '  3TEST VARCHAR(10),     ' +
                                '  PRIMARY KEY (2ID)           ' +
                                ')                            ');
// Firebird/Interbase need a commit after a DDL statement. Not necessary for the other connections
    TSQLDBConnector(DBConnector).Transaction.CommitRetaining;
    with query do
      begin
      SQL.Text:='select * from FPDEV2';
      Open;
      Edit;
      fieldbyname('2ID').AsInteger:=1;
      fieldbyname('3TEST').AsString:='3test';
      Post;
      ApplyUpdates(0);
      close;
      open;
      AssertEquals('3test',FieldByName('3TEST').AsString);
      Edit;
      fieldbyname('3TEST').AsString:='test3';
      Post;
      ApplyUpdates(0);
      open;
      AssertEquals('test3',FieldByName('3TEST').AsString);
      close;
      end;
    end;
end;

procedure TTestFieldTypes.TestApplyUpdFieldnames;
begin
  with TSQLDBConnector(DBConnector) do
    begin
    AssertEquals(-1,query.RowsAffected);
    Connection.ExecuteDirect('create table FPDEV2 (         ' +
                              '  ID INT NOT NULL            , ' +
                              '  "NAME-TEST" VARCHAR(250),  ' +
                              '  PRIMARY KEY (ID)           ' +
                              ')                            ');
// Firebird/Interbase need a commit after a DDL statement. Not necessary for the other connections
    TSQLDBConnector(DBConnector).Transaction.CommitRetaining;
    Connection.ExecuteDirect('insert into FPDEV2(ID,"NAME-TEST") values (1,''test1'')');
    Query.SQL.Text := 'select * from FPDEV2';
    Query.Open;
    AssertEquals(1,Query.FieldByName('ID').AsInteger);
    AssertEquals('test1',Query.FieldByName('NAME-TEST').AsString);
    Query.Edit;
    Query.FieldByName('NAME-TEST').AsString:='Edited';
    Query.Post;
    Query.ApplyUpdates;
    Query.Close;
    Query.Open;
    AssertEquals(1,Query.FieldByName('ID').AsInteger);
    AssertEquals('Edited',Query.FieldByName('NAME-TEST').AsString);
    Query.Close;
    end;
end;

procedure TTestFieldTypes.TestRowsAffected;
begin
  with TSQLDBConnector(DBConnector) do
    begin
    AssertEquals(-1,query.RowsAffected);
    Connection.ExecuteDirect('create table FPDEV2 (         ' +
                              '  ID INT NOT NULL            , ' +
                              '  NAME VARCHAR(250),         ' +
                              '  PRIMARY KEY (ID)           ' +
                              ')                            ');
// Firebird/Interbase need a commit after a DDL statement. Not necessary for the other connections
    TSQLDBConnector(DBConnector).Transaction.CommitRetaining;
    Query.SQL.Text := 'insert into FPDEV2(ID,NAME) values (1,''test1'')';
    Query.ExecSQL;
    AssertEquals(1,query.RowsAffected);
    Query.SQL.Text := 'insert into FPDEV2(ID,NAME) values (2,''test2'')';
    Query.ExecSQL;
    AssertEquals(1,query.RowsAffected);
    Query.SQL.Text := 'update FPDEV2 set NAME=''NewTest''';
    Query.ExecSQL;
    AssertEquals(2,query.RowsAffected);
    Query.SQL.Text := 'select * from FPDEV2';
    Query.Open;
    AssertTrue(query.RowsAffected<>0); // It should return -1 or the number of selected rows.
    query.Close;
    AssertTrue(query.RowsAffected<>0); // It should return -1 or the same as the last time it was called.
    if (SQLDbType = sqlite3) then  // sqlite doesn't count the rowsaffected if there is no where-clause
      Query.SQL.Text := 'delete from FPDEV2 where 1'
    else
      Query.SQL.Text := 'delete from FPDEV2';
    Query.ExecSQL;
    AssertEquals(2,query.RowsAffected);
    Query.SQL.Text := 'delete from FPDEV2';
    Query.ExecSQL;
    AssertEquals(0,query.RowsAffected);
    end;
end;

procedure TTestFieldTypes.TestStringsReplace;
begin
  AssertEquals('dit is een string',StringsReplace('dit was een string',['was'],['is'],[]));
  AssertEquals('dit is een string was een string',StringsReplace('dit was een string was een string',['was'],['is'],[]));
  AssertEquals('dit is een string is een string',StringsReplace('dit was een string was een string',['was'],['is'],[rfReplaceAll]));

  AssertEquals('dit is een char is een char',StringsReplace('dit was een string was een string',['was','string'],['is','char'],[rfReplaceAll]));
  AssertEquals('dit is een string was een string',StringsReplace('dit was een string was een string',['string','was'],['char','is'],[]));

  AssertEquals('dit is een char is een strin',StringsReplace('dit was een string was een strin',['string','was'],['char','is'],[rfReplaceAll]));

  AssertEquals('dit Was een char is een char',StringsReplace('dit Was een string was een string',['was','string'],['is','char'],[rfReplaceAll]));
  AssertEquals('dit wAs een char is een char',StringsReplace('dit wAs een string was een string',['was','string'],['is','char'],[rfReplaceAll]));
  AssertEquals('dit is een char is een char',StringsReplace('dit Was een sTring was een string',['was','string'],['is','char'],[rfReplaceAll,rfIgnoreCase]));
  AssertEquals('dit is een char is een char',StringsReplace('dit wAs een STRING was een string',['was','string'],['is','char'],[rfReplaceAll,rfIgnoreCase]));

  AssertEquals('dit was een si was een sa',StringsReplace('dit was een string was een straat',['straat','string'],['sa','si'],[rfReplaceAll]));
  AssertEquals('dit was een si was een sa',StringsReplace('dit was een string was een straat',['string','straat'],['si','sa'],[rfReplaceAll]));

  AssertEquals('dit was een sing was een saat',StringsReplace('dit was een string was een straat',['str','string'],['s','si'],[rfReplaceAll]));
  AssertEquals('dit was een si was een saat',StringsReplace('dit was een string was een straat',['string','str'],['si','s'],[rfReplaceAll]));

  AssertEquals('dit was een string was een string',StringsReplace('dit was een string was een string',[''],['is'],[rfReplaceAll]));
  AssertEquals('dit  een string  een string',StringsReplace('dit was een string was een string',['was'],[''],[rfReplaceAll]));
end;

procedure TTestFieldTypes.TestCircularParams;
begin
  with TSQLDBConnector(dbconnector) do
    begin
    Connection.ExecuteDirect('create table FPDEV2 (id1 int, id2 int,vchar varchar(10))');
    // Firebird/Interbase need a commit after a DDL statement. Not necessary for the other connections
    TSQLDBConnector(DBConnector).Transaction.CommitRetaining;

    Query.sql.Text := 'insert into FPDEV2 values(:id1,:id2,:vchar)';
    query.params[0].asinteger := 1;
    query.params[1].asinteger := 1;
    query.params[2].asstring := '$1 :id2 $';
    query.ExecSQL;
    query.sql.text := 'select * from FPDEV2';
    query.open;
    AssertEquals(1,query.fields[0].asinteger);
    AssertEquals(1,query.fields[1].asinteger);
    AssertEquals('$1 :id2 $',query.fields[2].AsString);
    query.close;
    end;
end;

procedure TTestFieldTypes.Test11Params;
var i : integer;
begin
  with TSQLDBConnector(dbconnector) do
    begin
    Connection.ExecuteDirect('create table FPDEV2 (id1 int, id2 int, id3 int, id4 int,id5 int,id6 int,id7 int,id8 int, id9 int, id10 int, id11 int)');
    // Firebird/Interbase need a commit after a DDL statement. Not necessary for the other connections
    TSQLDBConnector(DBConnector).Transaction.CommitRetaining;

    Query.sql.Text := 'insert into FPDEV2 values(:id1,:id2,:id3,:id4,:id5,:id6,:id7,:id8,:id9,:id10,:id11)';
    for i := 0 to 10 do
      query.params[i].asinteger := 1;
    query.ExecSQL;
    query.sql.text := 'select * from FPDEV2';
    query.open;
    for i := 0 to 10 do
      AssertEquals(1,query.fields[i].asinteger);
    query.close;
    end;
end;

procedure TTestFieldTypes.TestBug9744;
var i : integer;
begin
  if SQLDbType in [interbase,postgresql] then Ignore('This test does not apply to this db-engine, since it has no double field-type');

  with TSQLDBConnector(DBConnector) do
    begin
    try
      Connection.ExecuteDirect('create table TTTOBJ (         ' +
                                '  ID INT NOT NULL,           ' +
                                '  NAME VARCHAR(250),         ' +
                                '  PRIMARY KEY (ID)           ' +
                                ')                            ');
      Connection.ExecuteDirect('create table TTTXY (          ' +
                                '  ID INT NOT NULL,           ' +
                                '  NP INT NOT NULL,           ' +
                                '  X DOUBLE,                  ' +
                                '  Y DOUBLE,                  ' +
                                '  PRIMARY KEY (ID,NP)        ' +
                                ')                            ');
      for i := 0 to 7 do
        begin
        connection.ExecuteDirect('insert into TTTOBJ(ID,NAME) values ('+inttostr(i)+',''A'+inttostr(i)+''')');
        connection.ExecuteDirect('insert into TTTXY(ID,NP,X,Y) values ('+inttostr(i)+',1,1,1)');
        connection.ExecuteDirect('insert into TTTXY(ID,NP,X,Y) values ('+inttostr(i)+',2,2,2)');
        end;
      Query.SQL.Text := 'select OBJ.ID, OBJ.NAME, count(XY.NP) as NPF from TTTOBJ as OBJ, TTTXY as XY where (OBJ.ID=XY.ID) group by OBJ.ID';
      query.Prepare;
      query.open;
      query.close;
    finally
      Connection.ExecuteDirect('drop table TTTXY');
      Connection.ExecuteDirect('drop table TTTOBJ');
      end
    end;
end;

procedure TTestFieldTypes.TestCrossStringDateParam;
begin
  TestXXParamQuery(ftDate,'DATE',testDateValuesCount,True);
end;

procedure TTestFieldTypes.TestGetFieldNames;
var FieldNames : TStringList;
begin
  with TSQLDBConnector(DBConnector) do
    begin
    FieldNames := TStringList.Create;
    try
      if SQLDbType in MySQLdbTypes then
        Connection.GetFieldNames('FPDEV',FieldNames)
      else
        Connection.GetFieldNames('fpDEv',FieldNames);
      AssertEquals(2,FieldNames.Count);
      AssertEquals('ID',UpperCase(FieldNames[0]));
      AssertEquals('NAME',UpperCase(FieldNames[1]));
    finally
      FieldNames.Free;
      end;
    end;
end;

procedure TTestFieldTypes.TestGetTables;
var TableNames : TStringList;
begin
  with TSQLDBConnector(DBConnector) do
    begin
    TableNames := TStringList.Create;
    try
      Connection.GetTableNames(TableNames);
      AssertTrue(TableNames.Count>0);
      AssertTrue(TableNames.IndexOf('FPDEV')>-1);
      AssertTrue(TableNames.IndexOf('FPDEV_FIELD')>-1);
    finally
      TableNames.Free;
      end;
    end;
end;

procedure TTestFieldTypes.TestUpdateIndexDefs;
var ds : TSQLQuery;
begin
  ds := DBConnector.GetNDataset(1) as TSQLQuery;
  ds.Prepare;
  ds.ServerIndexDefs.Update;
  AssertEquals(1,ds.ServerIndexDefs.count);
  AssertTrue(CompareText('ID',ds.ServerIndexDefs[0].Fields)=0);
  Asserttrue(ds.ServerIndexDefs[0].Options=[ixPrimary,ixUnique]);
  ds.ServerIndexDefs.Update;
  AssertEquals(1,ds.ServerIndexDefs.count);
  AssertTrue(CompareText('ID',ds.ServerIndexDefs[0].Fields)=0);
  Asserttrue(ds.ServerIndexDefs[0].Options=[ixPrimary,ixUnique]);
end;

procedure TTestFieldTypes.TestSetBlobAsMemoParam;
begin
  TestSetBlobAsParam(0);
end;

procedure TTestFieldTypes.TestSetBlobAsBlobParam;
begin
  TestSetBlobAsParam(2);
end;

procedure TTestFieldTypes.TestTemporaryTable;
begin
  if SQLDbType=interbase then Ignore('This test does not apply to Interbase/Firebird, since it doesn''t support temporary tables');

  with TSQLDBConnector(DBConnector).Query do
    begin
    SQL.Clear;
    SQL.Add('CREATE TEMPORARY TABLE TEMP1 (id int)');
    ExecSQL;
    SQL.Text :=  'INSERT INTO TEMP1(id) values (5)';
    ExecSQL;
    SQL.Text := 'SELECT * FROM TEMP1';
    Open;
    AssertEquals(5,fields[0].AsInteger);
    Close;
    end;
end;

procedure TTestFieldTypes.TestGetIndexDefs;

var ds : TSQLQuery;
    inddefs : TIndexDefs;

begin
  ds := DBConnector.GetNDataset(1) as TSQLQuery;
  ds.Open;
  AssertEquals(1,ds.ServerIndexDefs.count);
  inddefs := HackedDataset(ds).GetIndexDefs(ds.ServerIndexDefs,[ixPrimary]);
  AssertEquals(1,inddefs.count);
  AssertTrue(CompareText('ID',inddefs[0].Fields)=0);
  Asserttrue(inddefs[0].Options=[ixPrimary,ixUnique]);
  inddefs.Free;

  inddefs := HackedDataset(ds).GetIndexDefs(ds.ServerIndexDefs,[ixPrimary,ixUnique]);
  AssertEquals(1,inddefs.count);
  AssertTrue(CompareText('ID',inddefs[0].Fields)=0);
  Asserttrue(inddefs[0].Options=[ixPrimary,ixUnique]);
  inddefs.Free;

  inddefs := HackedDataset(ds).GetIndexDefs(ds.ServerIndexDefs,[ixDescending]);
  AssertEquals(0,inddefs.count);
  inddefs.Free;
end;

procedure TTestFieldTypes.TestDblQuoteEscComments;
begin
  with TSQLDBConnector(DBConnector).Query do
    begin
    SQL.Clear;
    SQL.Add('select * from FPDEV where name=''test '''' and :ThisIsNotAParameter  ''');
    open;
    close;
    end;
end;

procedure TTestFieldTypes.TestParametersAndDates;
// See bug 7205
var ADateStr : String;
begin
  if SQLDbType in [interbase,mysql40,mysql41,mysql50,sqlite3] then Ignore('This test does not apply to this sqldb-connection type, since it doesn''t use semicolons for casts');

  with TSQLDBConnector(DBConnector).Query do
    begin
    SQL.Clear;
    sql.add('select now()::date as current_date where 1=1');
    open;
    first;
    ADateStr:=fields[0].asstring; // return the correct date
    // writeln(fields[0].asstring);
    close;

    sql.clear;
    sql.add('select now()::date as current_date where cast(1 as integer) = :PARAM1');
    params.parambyname('PARAM1').asinteger:= 1;
    open;
    first;
    AssertEquals(ADateStr,fields[0].asstring); // return invalid date
    // writeln(fields[0].asstring);
    close;

    end
end;

procedure TTestFieldTypes.TestExceptOnsecClose;

var passed : boolean;

begin
  with TSQLDBConnector(DBConnector).Query do
    begin
    SQL.Clear;
    SQL.Add('select * from FPDEV');

    Open;
    close;

    SQL.Clear;
    SQL.Add('select blaise from FPDEV');
    passed := false;
    try
      open;
    except
      on E: Exception do
        passed := (E.ClassType.InheritsFrom(EDatabaseError))
      end;
    AssertTrue(passed);

    Close;
    end;
end;

procedure TTestFieldTypes.TestErrorOnEmptyStatement;
var PassException : boolean;
begin
  PassException:=False;
  with TSQLDBConnector(DBConnector).Query do
    begin
    sql.Text := '';
    try
      Open;
    except
      on E:EDatabaseError do
        if Pos(SErrNoStatement,E.Message) > 0 then
          PassException := True;
    end;
    AssertTrue(PassException);
    end;
end;

initialization
  if uppercase(dbconnectorname)='SQL' then RegisterTest(TTestFieldTypes);
end.

