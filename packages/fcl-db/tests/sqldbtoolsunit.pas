unit SQLDBToolsUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, toolsunit
  ,db, sqldb
  ,mysql40conn, mysql41conn, mysql50conn, mysql51conn, mysql55conn, mysql56conn
  ,ibconnection
  ,pqconnection
  ,odbcconn
  {$IFNDEF WIN64}
  {See packages\fcl-db\fpmake.pp: Oracle connector not built yet on Win64}
  ,oracleconnection
  {$ENDIF WIN64}
  ,sqlite3conn
  ,mssqlconn
  ;

type
  TSQLConnType = (mysql40,mysql41,mysql50,mysql51,mysql55,mysql56,postgresql,interbase,odbc,oracle,sqlite3,mssql,sybase);
  TSQLServerType = (ssFirebird, ssInterbase, ssMSSQL, ssMySQL, ssOracle, ssPostgreSQL, ssSQLite, ssSybase, ssUnknown);

const
  MySQLConnTypes = [mysql40,mysql41,mysql50,mysql51,mysql55,mysql56];
  SQLConnTypesNames : Array [TSQLConnType] of String[19] =
        ('MYSQL40','MYSQL41','MYSQL50','MYSQL51','MYSQL55','MYSQL56','POSTGRESQL','INTERBASE','ODBC','ORACLE','SQLITE3','MSSQL','SYBASE');
             
  STestNotApplicable = 'This test does not apply to this sqldb connection type';


type
{ TSQLDBConnector }
  TSQLDBConnector = class(TDBConnector)
  private
    FConnection    : TSQLConnection;
    FTransaction   : TSQLTransaction;
    FQuery         : TSQLQuery;
    FUniDirectional: boolean;
    procedure CreateFConnection;
    procedure CreateFTransaction;
    Function CreateQuery : TSQLQuery;
  protected
    procedure SetTestUniDirectional(const AValue: boolean); override;
    function GetTestUniDirectional: boolean; override;
    procedure CreateNDatasets; override;
    procedure CreateFieldDataset; override;
    procedure DropNDatasets; override;
    procedure DropFieldDataset; override;
    Function InternalGetNDataset(n : integer) : TDataset; override;
    Function InternalGetFieldDataset : TDataSet; override;
    procedure TryDropIfExist(ATableName : String);
  public
    destructor Destroy; override;
    constructor Create; override;
    procedure ExecuteDirect(const SQL: string);
    procedure CommitDDL;
    property Connection : TSQLConnection read FConnection;
    property Transaction : TSQLTransaction read FTransaction;
    property Query : TSQLQuery read FQuery;
  end;

var SQLConnType : TSQLConnType;
    SQLServerType : TSQLServerType;
    FieldtypeDefinitions : Array [TFieldType] of String[20];
    
implementation

uses StrUtils;

type
  TSQLServerTypesMapItem = record
    s: string;
    t: TSQLServerType;
  end;

const
  FieldtypeDefinitionsConst : Array [TFieldType] of String[20] =
    (
      '',
      'VARCHAR(10)',
      'SMALLINT',
      'INTEGER',
      '',             // ftWord
      'BOOLEAN',
      'DOUBLE PRECISION', // ftFloat
      '',             // ftCurrency
      'DECIMAL(18,4)',// ftBCD
      'DATE',
      'TIME',
      'TIMESTAMP',    // ftDateTime
      '',             // ftBytes
      '',             // ftVarBytes
      '',             // ftAutoInc
      'BLOB',         // ftBlob
      'BLOB',         // ftMemo
      'BLOB',         // ftGraphic
      '',
      '',
      '',
      '',
      '',
      'CHAR(10)',     // ftFixedChar
      '',             // ftWideString
      'BIGINT',       // ftLargeInt
      '',
      '',
      '',
      '',
      '',
      '',
      '',
      '',
      '',
      '',             // ftGuid
      'TIMESTAMP',    // ftTimestamp
      'NUMERIC(18,6)',// ftFmtBCD
      '',             // ftFixedWideChar
      ''              // ftWideMemo
    );

  // names as returned by ODBC SQLGetInfo(..., SQL_DBMS_NAME, ...) and GetConnectionInfo(citServerType)
  SQLServerTypesMap : array [0..7] of TSQLServerTypesMapItem = (
    (s: 'Firebird'; t: ssFirebird),
    (s: 'Interbase'; t: ssInterbase),
    (s: 'Microsoft SQL Server'; t: ssMSSQL),
    (s: 'MySQL'; t: ssMySQL),
    (s: 'Oracle'; t: ssOracle),
    (s: 'PostgreSQL'; t: ssPostgreSQL),
    (s: 'SQLite3'; t: ssSQLite),
    (s: 'ASE'; t: ssSybase)
  );

  // fall back mapping (e.g. in case GetConnectionInfo(citServerType) is not implemented)
  SQLConnTypeToServerTypeMap : array[TSQLConnType] of TSQLServerType =
    (ssMySQL,ssMySQL,ssMySQL,ssMySQL,ssMySQL,ssMySQL,ssPostgreSQL,ssFirebird,ssUnknown,ssOracle,ssSQLite,ssMSSQL,ssSybase);


{ TSQLDBConnector }

procedure TSQLDBConnector.CreateFConnection;
var t : TSQLConnType;
    i : integer;
    s : string;
begin
  for t := low(SQLConnTypesNames) to high(SQLConnTypesNames) do
    if UpperCase(dbconnectorparams) = SQLConnTypesNames[t] then SQLConnType := t;

  if SQLConnType = MYSQL40 then Fconnection := TMySQL40Connection.Create(nil);
  if SQLConnType = MYSQL41 then Fconnection := TMySQL41Connection.Create(nil);
  if SQLConnType = MYSQL50 then Fconnection := TMySQL50Connection.Create(nil);
  if SQLConnType = MYSQL51 then Fconnection := TMySQL51Connection.Create(nil);
  if SQLConnType = MYSQL55 then Fconnection := TMySQL55Connection.Create(nil);
  if SQLConnType = MYSQL56 then Fconnection := TMySQL56Connection.Create(nil);
  if SQLConnType = SQLITE3 then Fconnection := TSQLite3Connection.Create(nil);
  if SQLConnType = POSTGRESQL then Fconnection := TPQConnection.Create(nil);
  if SQLConnType = INTERBASE then Fconnection := TIBConnection.Create(nil);
  if SQLConnType = ODBC then Fconnection := TODBCConnection.Create(nil);
  {$IFNDEF Win64}
  if SQLConnType = ORACLE then Fconnection := TOracleConnection.Create(nil);
  {$ENDIF Win64}
  if SQLConnType = MSSQL then Fconnection := TMSSQLConnection.Create(nil);
  if SQLConnType = SYBASE then Fconnection := TSybaseConnection.Create(nil);

  if not assigned(Fconnection) then writeln('Invalid database type, check if a valid database type for your achitecture was provided in the file ''database.ini''');

  with Fconnection do
  begin
    DatabaseName := dbname;
    UserName := dbuser;
    Password := dbpassword;
    HostName := dbhostname;
    if (dbhostname='') and (SQLConnType=interbase) then
    begin
      // Firebird embedded: create database file if it doesn't yet exist
      // Note: pagesize parameter has influence on behavior. We're using
      // Firebird default here.
      if not(fileexists(dbname)) then
        CreateDB; //Create testdb
    end;

    if length(dbQuoteChars)>1 then
      FieldNameQuoteChars:=dbQuoteChars;

    Open;
  end;

  // determine remote SQL Server to which we are connected
  s := Fconnection.GetConnectionInfo(citServerType);
  if s = '' then
    SQLServerType := SQLConnTypeToServerTypeMap[SQLConnType] // if citServerType isn't implemented
  else
    for i := low(SQLServerTypesMap) to high(SQLServerTypesMap) do
      if SQLServerTypesMap[i].s = s then
        SQLServerType := SQLServerTypesMap[i].t;

  FieldtypeDefinitions := FieldtypeDefinitionsConst;

  case SQLServerType of
    ssFirebird:
      begin
      FieldtypeDefinitions[ftBoolean] := '';
      FieldtypeDefinitions[ftMemo]    := 'BLOB SUB_TYPE TEXT';
      end;
    ssInterbase:
      begin
      FieldtypeDefinitions[ftMemo]     := 'BLOB SUB_TYPE TEXT';
      FieldtypeDefinitions[ftLargeInt] := 'NUMERIC(18,0)';
      end;
    ssMSSQL, ssSybase:
      // todo: Sybase: copied over MSSQL; verify correctness
      begin
      FieldtypeDefinitions[ftBoolean] := 'BIT';
      FieldtypeDefinitions[ftFloat]   := 'FLOAT';
      FieldtypeDefinitions[ftCurrency]:= 'MONEY';
      FieldtypeDefinitions[ftDate]    := 'DATETIME';
      FieldtypeDefinitions[ftTime]    := '';
      FieldtypeDefinitions[ftDateTime]:= 'DATETIME';
      FieldtypeDefinitions[ftBytes]   := 'BINARY(5)';
      FieldtypeDefinitions[ftVarBytes]:= 'VARBINARY(10)';
      FieldtypeDefinitions[ftBlob]    := 'IMAGE';
      FieldtypeDefinitions[ftMemo]    := 'TEXT';
      FieldtypeDefinitions[ftGraphic] := '';
      FieldtypeDefinitions[ftWideString] := 'NVARCHAR(10)';
      FieldtypeDefinitions[ftFixedWideChar] := 'NCHAR(10)';
      //FieldtypeDefinitions[ftWideMemo] := 'NTEXT'; // Sybase has UNITEXT?
      end;
    ssMySQL:
      begin
      // Add into my.ini: sql-mode="...,PAD_CHAR_TO_FULL_LENGTH,ANSI_QUOTES"
      FieldtypeDefinitions[ftWord] := 'SMALLINT UNSIGNED';
      // MySQL recognizes BOOLEAN, but as synonym for TINYINT, not true sql boolean datatype
      FieldtypeDefinitions[ftBoolean]  := '';
      // Use 'DATETIME' for datetime-fields instead of timestamp, because
      // mysql's timestamps are only valid in the range 1970-2038.
      // Downside is that fields defined as 'TIMESTAMP' aren't tested
      FieldtypeDefinitions[ftDateTime] := 'DATETIME';
      FieldtypeDefinitions[ftBytes]    := 'BINARY(5)';
      FieldtypeDefinitions[ftVarBytes] := 'VARBINARY(10)';
      FieldtypeDefinitions[ftMemo]     := 'TEXT';
      end;
    ssOracle:
      begin
      FieldtypeDefinitions[ftBoolean]  := '';
      // At least Oracle 10, 11 do not support a BIGINT field:
      FieldtypeDefinitions[ftLargeInt] := 'NUMBER(19,0)';
      FieldtypeDefinitions[ftTime]     := 'TIMESTAMP';
      FieldtypeDefinitions[ftMemo]     := 'CLOB';
      end;
    ssPostgreSQL:
      begin
      FieldtypeDefinitions[ftCurrency] := 'MONEY'; // ODBC?!
      FieldtypeDefinitions[ftBlob]     := 'BYTEA';
      FieldtypeDefinitions[ftMemo]     := 'TEXT';
      FieldtypeDefinitions[ftGraphic]  := '';
      FieldtypeDefinitions[ftGuid]     := 'UUID';
      end;
    ssSQLite:
      begin
      FieldtypeDefinitions[ftWord] := 'WORD';
      FieldtypeDefinitions[ftCurrency] := 'CURRENCY';
      FieldtypeDefinitions[ftBytes] := 'BINARY(5)';
      FieldtypeDefinitions[ftVarBytes] := 'VARBINARY(10)';
      FieldtypeDefinitions[ftMemo] := 'CLOB'; //or TEXT SQLite supports both, but CLOB is sql standard (TEXT not)
      FieldtypeDefinitions[ftWideString] := 'NVARCHAR(10)';
      FieldtypeDefinitions[ftFixedWideChar] := 'NCHAR(10)';
      FieldtypeDefinitions[ftWideMemo] := 'NCLOB';
      end;
  end;

  if SQLConnType in [mysql40,mysql41] then
    begin
    // Mysql versions prior to 5.0.3 removes the trailing spaces on varchar
    // fields on insertion. So to test properly, we have to do the same
    for i := 0 to testValuesCount-1 do
      testStringValues[i] := TrimRight(testStringValues[i]);
    end;

  if SQLServerType in [ssMySQL] then
    begin
    // Some DB's do not support milliseconds in datetime and time fields.
    for i := 0 to testValuesCount-1 do
      begin
      testTimeValues[i] := copy(testTimeValues[i],1,8)+'.000';
      testValues[ftTime,i] := copy(testTimeValues[i],1,8)+'.000';
      if length(testValues[ftDateTime,i]) > 19 then
        testValues[ftDateTime,i] := copy(testValues[ftDateTime,i],1,19)+'.000';
      end;
    end;

  if SQLServerType in [ssFirebird, ssInterbase, ssMSSQL, ssPostgreSQL, ssSybase] then
    begin
    // Some db's do not support times > 24:00:00
    testTimeValues[3]:='13:25:15.000';
    testValues[ftTime,3]:='13:25:15.000';
    if SQLServerType in [ssFirebird, ssInterbase] then
      begin
      // Firebird does not support time = 24:00:00
      testTimeValues[2]:='23:00:00.000';
      testValues[ftTime,2]:='23:00:00.000';
      end;
    end;

  if SQLServerType in [ssMSSQL, ssSybase] then
    // Some DB's do not support datetime values before 1753-01-01
    for i := 18 to testValuesCount-1 do
      begin
      testValues[ftDate,i] := testValues[ftDate,0];
      testValues[ftDateTime,i] := testValues[ftDateTime,0];
      end;

  // DecimalSeparator must correspond to monetary locale (lc_monetary) set on PostgreSQL server
  // Here we assume, that locale on client side is same as locale on server
  if SQLServerType in [ssPostgreSQL] then
    for i := 0 to testValuesCount-1 do
      testValues[ftCurrency,i] := QuotedStr(CurrToStr(testCurrencyValues[i]));

  // SQLite does not support fixed length CHAR datatype
  // MySQL by default trimms trailing spaces on retrieval; so set sql-mode="PAD_CHAR_TO_FULL_LENGTH" - supported from MySQL 5.1.20
  // MSSQL set SET ANSI_PADDING ON
  // todo: verify Sybase behaviour
  if SQLServerType in [ssSQLite] then
    for i := 0 to testValuesCount-1 do
      testValues[ftFixedChar,i] := PadRight(testValues[ftFixedChar,i], 10);
end;

procedure TSQLDBConnector.CreateFTransaction;

begin
  Ftransaction := tsqltransaction.create(nil);
  with Ftransaction do
    database := Fconnection;
end;

function TSQLDBConnector.CreateQuery: TSQLQuery;

begin
  Result := TSQLQuery.create(nil);
  with Result do
    begin
    database := Fconnection;
    transaction := Ftransaction;
    PacketRecords := -1;  // To avoid: "Connection is busy with results for another hstmt" (ODBC,MSSQL)
    end;
end;

procedure TSQLDBConnector.SetTestUniDirectional(const AValue: boolean);
begin
  FUniDirectional:=avalue;
  FQuery.UniDirectional:=AValue;
end;

function TSQLDBConnector.GetTestUniDirectional: boolean;
begin
  result := FUniDirectional;
end;

procedure TSQLDBConnector.CreateNDatasets;
var CountID : Integer;
begin
  try
    Ftransaction.StartTransaction;
    TryDropIfExist('FPDEV');
    Fconnection.ExecuteDirect('create table FPDEV (' +
                              '  ID INT NOT NULL,  ' +
                              '  NAME VARCHAR(50), ' +
                              '  PRIMARY KEY (ID)  ' +
                              ')');

    FTransaction.CommitRetaining;

    for countID := 1 to MaxDataSet do
      Fconnection.ExecuteDirect('insert into FPDEV (ID,NAME)' +
                                'values ('+inttostr(countID)+',''TestName'+inttostr(countID)+''')');

    Ftransaction.Commit;
  except
    if Ftransaction.Active then Ftransaction.Rollback
  end;
end;

procedure TSQLDBConnector.CreateFieldDataset;
var CountID : Integer;
    FType   : TFieldType;
    Sql,sql1: String;
begin
  try
    Ftransaction.StartTransaction;
    TryDropIfExist('FPDEV_FIELD');

    Sql := 'create table FPDEV_FIELD (ID INT NOT NULL,';
    for FType := low(TFieldType)to high(TFieldType) do
      if FieldtypeDefinitions[FType]<>'' then
        sql := sql + 'F' + Fieldtypenames[FType] + ' ' +FieldtypeDefinitions[FType]+ ',';
    Sql := Sql + 'PRIMARY KEY (ID))';

    FConnection.ExecuteDirect(Sql);

    FTransaction.CommitRetaining;

    for countID := 0 to testValuesCount-1 do
      begin
      Sql :=  'insert into FPDEV_FIELD (ID';
      Sql1 := 'values ('+IntToStr(countID);
      for FType := low(TFieldType)to high(TFieldType) do
        if FieldtypeDefinitions[FType]<>'' then
          begin
          sql := sql + ',F' + Fieldtypenames[FType];
          if testValues[FType,CountID] <> '' then
            case FType of
              ftCurrency:
                sql1 := sql1 + ',' + testValues[FType,CountID];
              ftDate:
                // Oracle requires date conversion; otherwise
                // ORA-01861: literal does not match format string
                if SQLServerType in [ssOracle] then
                  // ANSI/ISO date literal:
                  sql1 := sql1 + ', DATE ' + QuotedStr(testValues[FType,CountID])
                else
                  sql1 := sql1 + ',' + QuotedStr(testValues[FType,CountID]);
              ftDateTime:
                // similar to ftDate handling
                if SQLServerType in [ssOracle] then
                begin
                  // Could be a real date+time or only date. Does not consider only time.
                  if pos(' ',testValues[FType,CountID])>0 then
                    sql1 := sql1 + ', TIMESTAMP ' + QuotedStr(testValues[FType,CountID])
                  else
                    sql1 := sql1 + ', DATE ' + QuotedStr(testValues[FType,CountID]);
                end
                else
                  sql1 := sql1 + ',' + QuotedStr(testValues[FType,CountID]);
              ftTime:
                // similar to ftDate handling
                if SQLServerType in [ssOracle] then
                  // More or less arbitrary default time; there is no time-only data type in Oracle.
                  sql1 := sql1 + ', TIMESTAMP ' + QuotedStr('0001-01-01 '+testValues[FType,CountID])
                else
                  sql1 := sql1 + ',' + QuotedStr(testValues[FType,CountID]);
              else
                sql1 := sql1 + ',' + QuotedStr(testValues[FType,CountID])
            end
          else
            sql1 := sql1 + ',NULL';
          end;
      Sql := sql + ')';
      Sql1 := sql1+ ')';

      Fconnection.ExecuteDirect(sql + ' ' + sql1);
      end;

    Ftransaction.Commit;
  except
    on E: Exception do begin
      //writeln(E.Message);
      if Ftransaction.Active then Ftransaction.Rollback;
    end;
  end;
end;

procedure TSQLDBConnector.DropNDatasets;
begin
  if assigned(FTransaction) then
    begin
    try
      if Ftransaction.Active then Ftransaction.Rollback;
      Ftransaction.StartTransaction;
      Fconnection.ExecuteDirect('DROP TABLE FPDEV');
      Ftransaction.Commit;
    Except
      if Ftransaction.Active then Ftransaction.Rollback
    end;
    end;
end;

procedure TSQLDBConnector.DropFieldDataset;
begin
  if assigned(FTransaction) then
    begin
    try
      if Ftransaction.Active then Ftransaction.Rollback;
      Ftransaction.StartTransaction;
      Fconnection.ExecuteDirect('DROP TABLE FPDEV_FIELD');
      Ftransaction.Commit;
    Except
      if Ftransaction.Active then Ftransaction.Rollback
    end;
    end;
end;

function TSQLDBConnector.InternalGetNDataset(n: integer): TDataset;
begin
  Result := CreateQuery;
  with (Result as TSQLQuery) do
    begin
    sql.clear;
    sql.add('SELECT * FROM FPDEV WHERE ID < '+inttostr(n+1));
    UniDirectional:=TestUniDirectional;
    end;
end;

function TSQLDBConnector.InternalGetFieldDataset: TDataSet;
begin
  Result := CreateQuery;
  with (Result as TSQLQuery) do
    begin
    sql.clear;
    sql.add('SELECT * FROM FPDEV_FIELD');
    UniDirectional:=TestUniDirectional;
    end;
end;

procedure TSQLDBConnector.TryDropIfExist(ATableName: String);
begin
  // This makes life soo much easier, since it avoids the exception if the table already
  // exists. And while this exeption is in a try..except statement, the debugger
  // always shows the exception, which is pretty annoying.
  try
    case SQLServerType of
      ssFirebird:
        begin
        // This only works with Firebird 2+
        FConnection.ExecuteDirect('execute block as begin if (exists (select 1 from rdb$relations where rdb$relation_name=''' + ATableName + ''')) '+
          'then execute statement ''drop table ' + ATableName + ';'';end');
        FTransaction.CommitRetaining;
        end;
      ssMSSQL:
        begin
        // Checking is needed here to avoid getting "auto rollback" of a subsequent CREATE TABLE statement
        // which leads to the rollback not referring to the right transaction=>SQL error
        // Use SQL92 ISO standard INFORMATION_SCHEMA:
        FConnection.ExecuteDirect(
          'if exists (select * from INFORMATION_SCHEMA.TABLES where TABLE_TYPE=''BASE TABLE'' AND TABLE_NAME=''' + ATableName + ''') '+
          'begin '+
          'drop table ' + ATableName + ' '+
          'end');
        end;
      ssSybase:
        begin
        // Checking is needed here to avoid getting "auto rollback" of a subsequent CREATE TABLE statement
        // which leads to the rollback not referring to the right transaction=>SQL error
        // Can't use SQL standard information_schema; instead query sysobjects for User tables
        FConnection.ExecuteDirect(
          'if exists (select * from sysobjects where type = ''U'' and name=''' + ATableName + ''') '+
          'begin '+
          'drop table ' + ATableName + ' '+
          'end');
        end;
    end;
  except
    FTransaction.RollbackRetaining;
  end;
end;

procedure TSQLDBConnector.ExecuteDirect(const SQL: string);
begin
  Connection.ExecuteDirect(SQL);
end;

procedure TSQLDBConnector.CommitDDL;
begin
  // Commits schema definition and manipulation statements;
  // Firebird/Interbase need a commit after a DDL statement. Not necessary for the other connections
  if SQLServerType in [ssFirebird, ssInterbase] then
    Transaction.CommitRetaining;
end;

destructor TSQLDBConnector.Destroy;
begin
  if assigned(FTransaction) then
    begin
    try
      if Ftransaction.Active then Ftransaction.Rollback;
      Ftransaction.StartTransaction;
      Fconnection.ExecuteDirect('DROP TABLE FPDEV2');
      Ftransaction.Commit;
    Except
      if Ftransaction.Active then Ftransaction.Rollback;
    end; // try
    end;
  inherited Destroy;
  FreeAndNil(FQuery);
  FreeAndNil(FTransaction);
  FreeAndNil(FConnection);
end;

constructor TSQLDBConnector.Create;
begin
  FConnection := nil;
  CreateFConnection;
  CreateFTransaction;
  FQuery := CreateQuery;
  FConnection.Transaction := FTransaction;
  Inherited;
end;

initialization
  RegisterClass(TSQLDBConnector);
end.

