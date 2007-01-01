 unit TestSQLFieldTypes;

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
    procedure TestXXParamQuery(ADatatype : TFieldType; ASQLTypeDecl : string; testValuescount : integer);
  protected
    procedure SetUp; override; 
    procedure TearDown; override;
    procedure RunTest; override;
  published
  
    procedure TestpfInUpdateFlag; // bug 7565
    procedure TestInt;
    procedure TestScript;

    procedure TestParametersAndDates;
    procedure TestExceptionOnsecondClose;

    procedure TestBlob;
    procedure TestChangeBlob;

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
    procedure TestDateParamQuery;
    procedure TestIntParamQuery;
    procedure TestFloatParamQuery;
  published
    procedure TestAggregates;
  end;

implementation

uses sqldbtoolsunit,toolsunit, variants, sqldb, bufdataset;

const
  testFloatValuesCount = 21;
  testFloatValues : Array[0..testFloatValuesCount-1] of double = (-maxSmallint-1,-maxSmallint,-256,-255,-128,-127,-1,0,1,127,128,255,256,maxSmallint,maxSmallint+1,0.123456,-0.123456,4.35,12.434E7,9.876e-5,123.45678);

  testIntValuesCount = 17;
  testIntValues : Array[0..testIntValuesCount-1] of integer = (-maxInt,-maxSmallint-1,-maxSmallint,-256,-255,-128,-127,-1,0,1,127,128,255,256,maxSmallint,maxSmallint+1,MaxInt);

  testStringValuesCount = 20;
  testStringValues : Array[0..testStringValuesCount-1] of string = (
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
    ' ''quotes'' ',
    ')-;:/?.<>',
    '~`|{}- =',    // note that there's no \  (backslash) since some db's uses that as escape-character
    '  WRaP  ',
    'wRaP  ',
    ' wRAP'
  );

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
    AFld1.ProviderFlags := [pfInKey];

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
  with Ascript do
    begin
    DataBase := TSQLDBConnector(DBConnector).Connection;
    transaction := TSQLDBConnector(DBConnector).Transaction;
    script.clear;
    script.append('create table a (id int);');
    script.append('create table b (id int);');
    ExecuteScript;
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
      if (SQLDbType in MySQLdbTypes) then
        AssertEquals(TrimRight(testValues[i]),fields[0].AsString) // MySQL automatically trims strings
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
//  AssertTrue(SIgnoreAssertion,SQLDbType = postgresql); // Only postgres accept this type-definition
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
  TSQLDBConnector(DBConnector).Connection.ExecuteDirect('create table FPDEV2 (ID int,FT blob)');
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


procedure TTestFieldTypes.TestBlob;

var
  i             : byte;

begin
//  CreateTableWithFieldType(ftBlob,'BLOB');
  CreateTableWithFieldType(ftBlob,'TEXT');
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
  CreateTableWithFieldType(ftDateTime,'TIMESTAMP');
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

procedure TTestFieldTypes.TestStringParamQuery;

begin
  TestXXParamQuery(ftString,'VARCHAR(10)',testStringValuesCount);
end;

procedure TTestFieldTypes.TestDateParamQuery;

begin
  TestXXParamQuery(ftDate,'DATE',testDateValuesCount);
end;


procedure TTestFieldTypes.TestXXParamQuery(ADatatype : TFieldType; ASQLTypeDecl : string; testValuescount : integer);

var i : integer;

begin
  TSQLDBConnector(DBConnector).Connection.ExecuteDirect('create table FPDEV2 (ID INT, FIELD1 '+ASQLTypeDecl+')');

// Firebird/Interbase need a commit after a DDL statement. Not necessary for the other connections
  if SQLDbType=interbase then TSQLDBConnector(DBConnector).Transaction.CommitRetaining;

  with TSQLDBConnector(DBConnector).Query do
    begin
    sql.clear;
    sql.append('insert into FPDEV2 (ID,FIELD1) values (:id,:field1)');

    ShortDateFormat := 'yyyy-mm-dd';

    for i := 0 to testValuesCount -1 do
      begin
      Params.ParamByName('id').AsInteger := i;
      case ADataType of
        ftInteger: Params.ParamByName('field1').asinteger := testIntValues[i];
        ftFloat  : Params.ParamByName('field1').AsFloat   := testFloatValues[i];
        ftString : Params.ParamByName('field1').AsString  := testStringValues[i];
        ftDate   : Params.ParamByName('field1').AsDateTime:= StrToDate(testDateValues[i]);
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

procedure TTestFieldTypes.TestParametersAndDates;
begin
  with TSQLDBConnector(DBConnector).Query do
    begin
    SQL.Clear;
    sql.add('select now()::date as current_date where 1=1');
    open;
    first;
    writeln(fields[0].asstring); // return the correct date
    close;

    sql.clear;
    sql.add('select now()::date as current_date where cast(1 as integer) = :PARAM1');
    params.parambyname('PARAM1').asinteger:= 1;
    open;
    first;
    writeln(fields[0].asstring); // return invalid date
    close;

    end
end;

procedure TTestFieldTypes.TestExceptionOnsecondClose;
begin
  with TSQLDBConnector(DBConnector).Query do
    begin
    SQL.Clear;
    SQL.Add('select * from FPDEV');

    Open;
    close;
    
    SQL.Clear;
    SQL.Add('select blaise from FPDEV');
{$IFDEF FPC}
//    AssertException(EIBDatabaseError,@Open);
{$ELSE}
//    AssertException(EIBDatabaseError,Open);
{$ENDIF}

    Open;

    Close;
    end;
end;

initialization
  if uppercase(dbconnectorname)='SQL' then RegisterTest(TTestFieldTypes);
end.

