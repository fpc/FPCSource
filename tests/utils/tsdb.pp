{$mode objfpc}
{$H+}

unit tsdb;

Interface

Uses
  sqldb, types, tstypes, tsstring, tsutils, pqconnection;

const
  // Ini file constants
  DefaultDBConfigFileName = '/etc/dbdigest.ini';
  SSection    = 'Database';
  KeyName     = 'Name';
  KeyHost     = 'Host';
  KeyUser     = 'UserName';
  KeyPassword = 'Password';
  KeyPort     = 'Port';

  // Query to run to get all test run test results.
  // For test results that did not change, the last test result ID is returned.
  // Needs formatting with 2 IDS : Run ID, Platform ID
  SQLTestResultIDS =
   'with testrunresultids as ( ' +
   '  select ' +
   '    tr_id as theid ' +
   '  from ' +
   '    testresults ' +
   '  where ' +
   '    (tr_testrun_fk=%d) ' +
   '  union ' +
   '  select ' +
   '    tl_testresults_fk as theid ' +
   '  from ' +
   '    tests ' +
   '    inner join testlastresults on (tl_test_fk=t_id) and (tl_platform_fk=%d) ' +
   ')';

  // Get all test results for a testrun (but not compile/run log)

  SQLSelectTestResults =
    SQLTestResultIDS +
    'select ' +
    '  T_ID as Id,T_NAME as Filename,TR_SKIP as Skipped, TR_OK as OK,TR_RESULT as Result ' +
    'from ' +
    '  testrunresultids ' +
    '  left join testresults on (tr_id=theid) ' +
    '  inner join tests on (tr_test_fk=t_id)';

Type
  TMapType = (mtCPU, mtOS, mtVersion);

  { TTestSQL }

  TTestSQL = class(TObject)
  Const
    Bools : Array[Boolean] of String = ('f','t');
  private
    FRelSrcDir: String;
    FTestSrcDir: string;
    FConnection : TPQConnection;
    FDatabaseName : String;
    FHost : String;
    FUser : String;
    FPassword : String;
    FPort : Word;
    Flogprefix : String;
    Class Procedure FreeQueryResult (Var aQry : TSQLQuery);
    Class Function  GetIntResultField (aQry : TSQLQuery; aFieldIndex : Integer) : Integer;
    Class Function  GetInt64ResultField (aQry : TSQLQuery; aFieldIndex : Integer) : Int64;
    Class Function  GetStrResultField (aQry : TSQLQuery; aFieldIndex : Integer) : String;
    function InsertTestHistory(TestRunID, TestPreviousID: Integer): boolean;
    // Overload adds prefix to actual call
    procedure Verbose(aLevel : TVerboseLevel; const aMsg : string);
    { ---------------------------------------------------------------------
        Low-level DB access.
      ---------------------------------------------------------------------}

    // create and open a query, return in Res.
    Function  OpenQuery (Qry : String; Out Res : TSQLQuery; Silent : Boolean) : Boolean ;
  Public
    { ---------------------------------------------------------------------
      High-level access
      ---------------------------------------------------------------------}
    // Constructor.
    Constructor create(aDatabaseName,aHost,aUser,aPassword : String; aPort : Word);
    // Destructor
    Destructor destroy; override;
    // Try to connect to database with params given in constructor.
    Function ConnectToDatabase : Boolean;
    // Disconnect from database
    Procedure DisconnectDatabase;
    // Create query object.
    function CreateQuery(const ASQL: String): TSQLQuery;
    // Execute a query, return true if it executed without error.
    Function  ExecuteQuery (Qry : String; Silent : Boolean) : Boolean ;
    // Run query, return first field as integer. -1 on error or no data.
    Function  IDQuery(Qry : String) : Integer;
    // Run query, return first field as int64. -1 on error or no data.
    Function  ID64Query(Qry : String) : Int64;
    // Run query, return first field as string. Empty string on error or no data.
    Function  StringQuery(Qry : String) : String;
    Function CreateMap(aType : TMapType) : TIntegerDynArray;
    // Adding things
    // Add a category.
    Function AddCategory(const aName : String) : Integer;
    // Add a CPU.
    Function AddCPU(const aName : String) : Integer;
    // Add an OS.
    Function AddOS(const aName : String) : Integer;
    // Add a compiler version.
    function AddVersion(const aName: String; aReleaseDate: TDateTime): Integer;
    // Add a platform.
    Function AddPlatform(const aData : TTestRunData) : Integer;
    // Add a test and return the ID. If the test already exists, return it's ID
    Function AddTest(Name : String; AddSource : Boolean) : Integer;
    // Add a test run. Return the test run ID.
    function AddRun(const aData: TTestRunData): Int64;
    // Ad test result and return ID. If a result exists already for the given run/test, update and return ID.
    Function AddTestResult(aData : TTestResultData) : Int64;
    // Add LastTestResult. If it exists already with given platform/test, update result ID.
    function AddLastResult(TestID, PlatformID: Integer; ResultID: Int64): Boolean;
    // Add previousTestResult. If it exists already with given platform/test, update result ID.
    function AddPreviousResult(TestID, PlatformID: Integer; ResultID: Int64): Boolean;
    //
    // Get ID based on key. All keys are case sensitive. If a key does not exist, -1 is returned.
    //
    // Get test ID based on test name.
    Function GetTestID(aName : string) : Integer;
    Function GetTestName(aID : Integer) : string;
    Function GetTestFileName(aID : Integer) : String;
    Function GetTestSource(aID : Integer) : String;
    // Get OS ID based on OS name.
    Function GetOSID(aName : String) : Integer;
    Function GetOSName(aID : Integer) : String;
    // Get CPU ID based on CPU name.
    Function GetCPUID(Name : String) : Integer;
    Function GetCPUName(aID : Integer) : String;

    // Get category ID based on Category name.
    Function GetCategoryID(aName : String) : Integer;
    Function GetCategoryName(aID : Integer) : String;
    // Get version ID based on version name.
    Function GetVersionID(aName : String) : Integer;
    Function GetVersionName(aID : Integer) : string;
    // Get platform ID based on OS, cpu, category, config.
    function GetPlatformID(aData: TTestRunData; aAllowCreate: Boolean): Integer;
    function GetPlatformID(aVersionID, aOSID, aCPUID, aCategoryID : Integer; const aMachine, aConfig : String): Integer;
    function GetPlatformID(aRunID : Int64): Integer;
    // Get run ID based on platform/date.
    Function GetRunID(aData : TTestRunData) : Int64;
    function GetNextRunID(RunID: Int64): Int64;
    function GetPreviousRunID(RunID: Int64): Int64;
    Function GetRunData(aID : Int64; out aData : TTestRunData) : Boolean;
    Function GetLastRunByPlatformAndDate(aPLatformID : Integer; aDate : TDateTime) : Integer;
    // Get testinfo based on test ID
    function GetTestInfo(aID: Int64; out aInfo: TTestInfo): Boolean;
    // Get last test result ID based on platform/test.
    function GetLastTestResult(aTestID, aPlatFormID: Integer): TTestResultData;
    function GetFailCount(aRunID : Int64) : Int64;
    // Update tests
    Function UpdateTest(ID : Integer; Info : TConfig; Const Source : String) : Boolean;
    function UpdateTestResult(aData: TTestResultData): Int64;
    function UpdateTestRun(aData : TTestRunData): Boolean;
    Function GetFailCount(aRunID : Integer) : Int64;

    // Create test if it does not exist yet.
    Function RequireTestID(const aName : String): Integer;
    // Delete all results from a test run.
    Function CleanTestRun(ID : Integer) : Boolean;
    // Escape SQL (quotes etc.
    Class Function  EscapeSQL(Const S : String) : String;
    // return SQL date
    Class Function  SQLDate(D : TDateTime) : String;
    // Rel src dir
    Property RelSrcDir : String Read FRelSrcDir Write FRelSrcDir;
    // test src dir.
    Property TestSrcDir : string read FTestSrcDir Write FTestSrcDir;
    // Prefix to use when logging (in case of multi-thread)
    Property LogPrefix : String Read FLogPrefix Write FLogPrefix;
  end;


Implementation

Uses
  SysUtils;

{ ---------------------------------------------------------------------
    Low-level DB access.
  ---------------------------------------------------------------------}

function TTestSQL.ConnectToDatabase: Boolean;

begin
  Result:=False;
  Verbose(V_SQL,'Connection params : '+FDatabaseName+' '+FHost+' '+FUser+' '+IntToStr(FPort));
  FConnection:=TPQConnection.Create(Nil);
  try
    FConnection.Hostname:=FHost;
    FConnection.DatabaseName:=FDatabaseName;
    FConnection.Username:=FUser;
    FConnection.Password:=FPassword;
    FConnection.Connected:=true;
    FConnection.Transaction:=TSQLTransaction.Create(FConnection);
    if (FPort<>0) then
      FConnection.Params.Values['Port']:=IntToStr(FPort);
    FConnection.Connected:=True;
    Result:=True
  except
    On E : Exception do
      begin
      Verbose(V_ERROR,'Failed to connect to database : '+E.Message);
      FreeAndNil(FConnection);
      end;
  end;
end;

procedure TTestSQL.DisconnectDatabase;

begin
  FreeAndNil(FConnection);
end;

function TTestSQL.AddCategory(const aName: String): Integer;

Const
  SQLInsert = 'INSERT INTO TESTCATEGORY (TA_NAME) VALUES (''%s'') RETURNING TA_ID';

begin
  Result:=IDQuery(Format(SQLInsert,[EscapeSQL(aName)]));
end;

function TTestSQL.AddCPU(const aName: String): Integer;

Const
  SQLInsert = 'INSERT INTO TESTCPU (TC_NAME) VALUES (''%s'') RETURNING TC_ID';

begin
  Result:=IDQuery(Format(SQLInsert,[EscapeSQL(aName)]));
end;

function TTestSQL.AddOS(const aName: String): Integer;
Const
  SQLInsert = 'INSERT INTO TESTOS (TO_NAME) VALUES (''%s'') RETURNING TO_ID';

begin
  Result:=IDQuery(Format(SQLInsert,[EscapeSQL(aName)]));
end;

function TTestSQL.AddVersion(const aName: String; aReleaseDate : TDateTime): Integer;
Const
  SQLInsert = 'INSERT INTO TESTVERSION (TV_VERSION,TV_RELEASEDATE) VALUES (''%s'',''%s'') RETURNING TV_ID';

begin
  Result:=IDQuery(Format(SQLInsert,[EscapeSQL(aName),SQLDate(aReleaseDate)]));
end;


function TTestSQL.CreateQuery(const ASQL: String): TSQLQuery;

begin
  Result:=TSQLQuery.Create(FConnection);
  Result.Database:=FConnection;
  Result.Transaction:=FConnection.Transaction;
  Result.SQL.Text:=ASQL;
end;


function TTestSQL.ExecuteQuery(Qry: String; Silent: Boolean): Boolean;

begin
  Verbose(V_SQL,'Executing query:'+Qry);
  Result:=False;
  try
    With CreateQuery(Qry) do
      try
        ExecSQL;
        Result:=True;
        (Transaction as TSQLTransaction).Commit;
      finally
        Free;
      end;
  except
    On E : exception do
      begin
      FConnection.Transaction.RollBack;
      if not Silent then
        Verbose(V_WARNING,'Query : '+Qry+'Failed : '+E.Message);
      end;
  end;
end;

function TTestSQL.OpenQuery(Qry: String; out Res: TSQLQuery; Silent: Boolean): Boolean;

begin
  Result:=False;
  Verbose(V_SQL,'Running query:'+Qry);
  Res:=CreateQuery(Qry);
  try
    Res.Open;
    Result:=True;
  except
    On E : exception do
      begin
      FreeAndNil(Res);
      Try
        FConnection.Transaction.RollBack;
      except
      end;
      if not Silent then
        Verbose(V_WARNING,'Query : '+Qry+'Failed : '+E.Message);
      end;
  end;
end;

class function TTestSQL.GetIntResultField(aQry: TSQLQuery; aFieldIndex: Integer): Integer;


begin
  If (aQry=Nil) or (aQry.IsEmpty) or (aFieldIndex>=aQry.Fields.Count) then
    Result:=-1
  else
    Result:=aQry.Fields[aFieldIndex].AsInteger;
  tsutils.Verbose(V_SQL,'Field value '+IntToStr(Result));
end;

class function TTestSQL.GetInt64ResultField(aQry: TSQLQuery; aFieldIndex: Integer): Int64;
begin
  If (aQry=Nil) or (aQry.IsEmpty) or (aFieldIndex>=aQry.Fields.Count) then
    Result:=-1
  else
    Result:=aQry.Fields[aFieldIndex].AsLargeInt;
  tsutils.Verbose(V_SQL,'Field value '+IntToStr(Result));
end;

class function TTestSQL.GetStrResultField(aQry: TSQLQuery; aFieldIndex: Integer): String;
begin
  If (aQry=Nil) or (aQry.IsEmpty) or (aFieldIndex>=aQry.Fields.Count) then
    Result:=''
  else
    Result:=aQry.Fields[aFieldIndex].AsString;
  tsutils.Verbose(V_SQL,'Field value '+Result);
end;

procedure TTestSQL.Verbose(aLevel: TVerboseLevel; const aMsg: string);
begin
  tsutils.Verbose(aLevel,logPrefix+aMsg);
end;

function TTestSQL.AddPlatform(const aData : TTestRunData) : Integer;

const
  SQLInsert = 'INSERT INTO TESTPLATFORM (TP_CPU_FK, TP_OS_FK, TP_VERSION_FK, TP_CATEGORY_FK, TP_CONFIG, TP_MACHINE) '+
              ' VALUES (%d, %d, %d, %d, ''%s'', ''%s'') '+
              '  RETURNING TP_ID';

begin
  With aData do
    Result:=IDQuery(Format(SQLInsert,[CPUID,OSID,VersionID,CategoryID,EscapeSQL(config),EscapeSQL(machine)]));
end;

class procedure TTestSQL.FreeQueryResult(var aQry: TSQLQuery);

begin
  if Assigned(aQry) and Assigned(aQry.Transaction) then
    aQry.SQLTransaction.Commit;
  FreeAndNil(aQry);
end;

function TTestSQL.IDQuery(Qry: String): Integer;

Var
  Res : TSQLQuery;

begin
  Result:=-1;
  If OpenQuery(Qry,Res,False) then
    try
      Result:=GetIntResultField(Res,0);
    finally
      FreeQueryResult(Res);
    end;
end;

function TTestSQL.ID64Query(Qry: String): Int64;
Var
  Res : TSQLQuery;

begin
  Result:=-1;
  If OpenQuery(Qry,Res,False) then
    try
      Result:=GetInt64ResultField(Res,0);
    finally
      FreeQueryResult(Res);
    end;
end;

function TTestSQL.StringQuery(Qry: String): String;

Var
  Res : TSQLQuery;

begin
  Result:='';
  If OpenQuery(Qry,Res,False) then
    try
      Result:=GetStrResultField(Res,0);
    finally
      FreeQueryResult(Res);
    end;
end;

function TTestSQL.CreateMap(aType: TMapType): TIntegerDynArray;
var
  Qry : TSQLQuery;
  lSQL : string;
  lIdx : Integer;

begin
  Result:=[];
  Case aType of
    mtCPU : lSQL:='SELECT TC_ID FROM TESTCPU order by TC_ID';
    mtOS  : lSQL:='SELECT TO_ID FROM TESTOS order by TO_ID';
    mtVersion  : lSQL:='SELECT TV_ID FROM TESTVERSION order by TV_ID';
  end;
  Qry:=CreateQuery(lSQL);
  try
    Qry.PacketRecords:=-1;
    Qry.Open;
    SetLength(Result,Qry.RecordCount);
    lIDx:=0;
    While not Qry.EOF do
      begin
      Result[lIdx]:=Qry.Fields[0].AsInteger;
      inc(lIdx);
      Qry.Next;
      end;
  finally
    Qry.Free;
  end;
end;

constructor TTestSQL.create(aDatabaseName, aHost, aUser, aPassword: String; aPort: Word);
begin
  FDatabaseName:=aDatabaseName;
  FHost:=aHost;
  FUser:=aUser;
  FPassword:=aPassword;
  FPort:=aPort;
end;

destructor TTestSQL.destroy;
begin
  DisconnectDatabase;
  inherited destroy;
end;

class function TTestSQL.EscapeSQL(const S: String): String;

begin
//  Result:=StringReplace(S,'\','\\',[rfReplaceAll]);
  Result:=StringReplace(S,'''','''''',[rfReplaceAll]);
  tsutils.Verbose(V_SQL,'EscapeSQL : "'+S+'" -> "'+Result+'"');
end;


class function TTestSQL.SQLDate(D: TDateTime): String;

begin
  Result:=FormatDateTime('YYYY/MM/DD hh:nn:ss',D);
end;

{ ---------------------------------------------------------------------
  High-level access
  ---------------------------------------------------------------------}


function TTestSQL.GetTestID(aName: string): Integer;

Const
  SFromName = 'SELECT T_ID FROM TESTS WHERE (T_NAME=''%s'')';

begin
  Result:=IDQuery(Format(SFromName,[aName]));
end;

function TTestSQL.GetTestName(aID: Integer): string;
begin
  Result:=StringQuery(Format('SELECT T_NAME FROM TESTCPU WHERE (T_ID=%d)',[aID]));
end;

function TTestSQL.GetTestFileName(aID: Integer): String;
begin
  Result:=StringQuery(Format('SELECT T_NAME FROM TESTS WHERE (T_ID=%d)',[aID]));
end;

function TTestSQL.GetTestSource(aID: Integer): String;
begin
  Result:=StringQuery(Format('select T_SOURCE from TESTS where (T_ID=%d)',[aid]));
end;

function TTestSQL.GetOSID(aName: String): Integer;

Const
  SFromName = 'SELECT TO_ID FROM TESTOS WHERE (TO_NAME=''%s'')';

begin
  Result:=IDQuery(Format(SFromName,[aName]));
end;

function TTestSQL.GetOSName(aID: Integer): String;
begin
  Result:=StringQuery(Format('SELECT TO_NAME FROM TESTOS WHERE (TO_ID=%d)',[aID]));
end;

function TTestSQL.GetVersionID(aName: String): Integer;

Const
  SFromName = 'SELECT TV_ID FROM TESTVERSION WHERE (TV_VERSION=''%s'')';

begin
  Result:=IDQuery(Format(SFromName,[aName]));
end;

function TTestSQL.GetVersionName(aID: Integer): string;

const
  SQLSelectVersion = 'SELECT TV_VERSION FROM TESTVERSION WHERE (TV_ID=%d)';

begin
  Result:=StringQuery(Format(SQLSelectVersion,[aID]));
end;

function TTestSQL.GetPlatformID(aData: TTestRunData; aAllowCreate: Boolean): Integer;

Const
  SQLSelect = 'SELECT TP_ID FROM TESTPLATFORM ' +
             ' WHERE ' +
             '  (TP_VERSION_FK=%d)' +
             '  AND (TP_OS_FK=%d)' +
             '  AND (TP_CPU_FK=%d)' +
             '  AND (TP_CATEGORY_FK=%d)' +
             '  AND (TP_CONFIG=''%s'')' +
             '  AND (TP_MACHINE=''%s'')';

begin
  With aData do
    Result:=IDQuery(Format(SQLSelect,[VersionID,OSID,CPUID,CategoryID,Config,Machine]));
  if (Result=-1) and aAllowCreate then
    Result:=AddPlatform(aData)
end;

function TTestSQL.GetPlatformID(aVersionID, aOSID, aCPUID, aCategoryID: Integer; const aMachine, aConfig: String): Integer;
var
  TR : TTestRunData;
begin
  TR:=Default(TTestRunData);
  TR.VersionID:=aVersionID;
  TR.OSID:=aOSID;
  TR.CPUID:=aCPUID;
  TR.CategoryID:=aCategoryID;
  TR.config:=aConfig;
  TR.Machine:=aMachine;
  Result:=GetPlatformID(TR,False);
end;

function TTestSQL.GetPlatformID(aRunID: Int64): Integer;
Const
  SFromID = 'SELECT TU_PLATFORM_FK FROM TESTRUN WHERE (TU_ID=%d)';
begin
  Result:=IDQuery(Format(SFromID,[aRunID]));
end;

function TTestSQL.GetCPUID(Name: String): Integer;

Const
  SFromName = 'SELECT TC_ID FROM TESTCPU WHERE (TC_NAME=''%s'')';

begin
  Result:=IDQuery(Format(SFromName,[Name]));
end;

function TTestSQL.GetCPUName(aID: Integer): String;
begin
  Result:=StringQuery(Format('SELECT TC_NAME FROM TESTCPU WHERE (TC_ID=%d)',[aID]));
end;

function TTestSQL.GetCategoryID(aName: String): Integer;

Const
  SFromName = 'SELECT TA_ID FROM TESTCATEGORY WHERE (TA_NAME=''%s'')';

begin
  Result:=IDQuery(Format(SFromName,[aName]));
end;

function TTestSQL.GetCategoryName(aID: Integer): String;
begin
  Result:=StringQuery(Format('SELECT TA_NAME FROM TESTCATEGORY WHERE (TA_ID=%d)',[aID]));
end;

function TTestSQL.GetRunID(aData: TTestRunData): Int64;


Const
  SFromIDS = 'SELECT TU_ID FROM TESTRUN WHERE '+
             ' (TU_PLATFORM_FK=%d) '+
             ' AND (TU_DATE=''%s'')';

begin
  With aData do
    Result:=ID64Query(Format(SFromIDS,[PlatFormID,SQLDate(Date)]));
end;

function TTestSQL.GetRunData(aID: Int64; out aData: TTestRunData): Boolean;

const
  SQLSelectRUNData =
             'select ' +
             '  TA_NAME, TV_VERSION, TV_RELEASEDATE, TV_ID, TC_NAME, TO_NAME, TestPlatform.* , TESTRUN.* ' +
             'from  ' +
             '  TESTRUN  ' +
             '  INNER JOIN TESTPLATFORM ON (TP_ID=TU_PLATFORM_FK) ' +
             '  INNER JOIN TESTOS ON (TO_ID=TP_OS_FK) ' +
             '  INNER JOIN TESTCPU ON (TC_ID=TP_CPU_FK) ' +
             '  INNER JOIN TESTCATEGORY ON (TA_ID=TP_CATEGORY_FK) ' +
             '  INNER JOIN TESTVERSION ON (TV_ID=TP_VERSION_FK) ' +
             'WHERE (TU_ID=%d)';

var
  Qry : TSQLQuery;
  ST : TTestStatus;

begin
  Qry:=CreateQuery(Format(SQLSelectRunData,[aID]));
  try
    Qry.Open;
    Result:=Not Qry.IsEmpty;
    if Result then
      With Qry do
        begin
        aData.RunID:=aID;
        aData.os:=FieldByName('TO_NAME').AsString;
        aData.OSID:=FieldByName('TP_OS_FK').AsInteger;
        aData.cpu:=FieldByName('TC_NAME').AsString;
        aData.CPUID:=FieldByName('TP_CPU_FK').AsInteger;
        aData.version:=FieldByName('TV_VERSION').AsString;
        aData.versionID:=FieldByName('TV_ID').asInteger;
        aData.category:=FieldByName('TA_NAME').AsString;
        aData.PlatformID:=FieldByName('TP_ID').AsInteger;
        aData.Config:=FieldByName('TP_CONFIG').AsString;
        aData.Machine:=FieldByName('TP_MACHINE').AsString;
        aData.submitter:=FieldByName('TU_SUBMITTER').AsString;
        // aData.:=FieldByName('TV_RELEASEDATE').AsString;
        aData.Date:=FieldByName('TU_DATE').AsDateTime;
        aData.CompilerDate:=FieldByName('TU_COMPILERDATE').AsString;
        aData.CompilerFullversion:=FieldByName('TU_COMPILERFULLVERSION').AsString;
        aData.CompilerRevision:=FieldByName('TU_COMPILERREVISION').AsString;
        aData.TestsRevision:=FieldByName('TU_TESTSREVISION').AsString;
        aData.RTLRevision:=FieldByName('TU_RTLREVISION').AsString;
        aData.PackagesRevision:=FieldByName('TU_PACKAGESREVISION').AsString;
        for ST in TTestStatus do
          aData.StatusCount[ST]:=FieldByName(SQLField[ST]).AsInteger;
        end;
  finally
    Qry.Free;
  end;
end;

function TTestSQL.GetLastRunByPlatformAndDate(aPLatformID: Integer; aDate: TDateTime): Integer;
const
  SQLSelect =
    'select '+
    '  TU_ID '+
    'from '+
    '  testrun '+
    'where '+
    '  (tu_platform_fk=%d) '+
    '  and (tu_date<''%s'') '+
    'order by '+
    '  tu_date desc '+
    'limit 1';
begin
  Result:=ID64Query(Format(SQLSelect,[aPlatformID,SQLDate(aDate)]));
end;

function TTestSQL.GetTestInfo(aID: Int64; out aInfo: TTestInfo): Boolean;

  function splitID(aString : String) : TIntegerDynArray;
  var
    lArray : TStringDynArray;
    i,count : integer;
    S : string;
  begin
    Result:=[];
    lArray:=aString.Split(',');
    SetLength(Result,Length(lArray));
    count:=0;
    for S in lArray do
      if TryStrToInt(Trim(S),i) then
        begin
        Result[Count]:=I;
        inc(count);
        end;
    SetLength(Result,Count);
  end;

const
  SQLSelect = 'SELECT * FROM TESTS WHERE (T_ID=%d)';

var
  Qry : TSQLQuery;

begin
  aInfo:=Default(TTestInfo);
  Qry:=CreateQuery(Format(SQLSelect,[aID]));
  try
    Qry.Open;
    Result:=Not Qry.IsEmpty;
    if Not Result then
      exit;

    aInfo.Name:=Qry.FieldByname('T_Name').AsString;
    aInfo.CPU:=Qry.FieldByname('T_CPU').AsString;
    aInfo.OS:=Qry.FieldByname('T_OS').Asstring;
    aInfo.Version:=Qry.FieldByname('T_VERSION').Asstring;
    aInfo.AddDate:=Qry.FieldByname('T_ADDDATE').AsDateTime;
    aInfo.Graph:=Qry.FieldByname('T_GRAPH').Asboolean;
    aInfo.Interactive:=Qry.FieldByname('T_INTERACTIVE').Asboolean;
    aInfo.Result:=Qry.FieldByname('T_RESULT').AsInteger;
    aInfo.Fail:=Qry.FieldByname('T_FAIL').Asboolean;
    aInfo.ReCompile:=Qry.FieldByname('T_RECOMPILE').Asboolean;
    aInfo.NoRun:=Qry.FieldByname('T_NORUN').Asboolean;
    aInfo.NeedLibrary:=Qry.FieldByname('T_NEEDLIBRARY').Asboolean;
    aInfo.KnownRunError:=Qry.FieldByname('T_KNOWNRUNERROR').AsInteger;
    aInfo.Known:=Qry.FieldByname('T_Known').Asboolean;
    aInfo.Note:=Qry.FieldByname('T_NOTE').AsString;
    aInfo.Description:=Qry.FieldByname('T_DESCRIPTION').AsString;
    aInfo.Source:=Qry.FieldByname('T_SOURCE').AsString;
    aInfo.Opts:=Qry.FieldByname('T_OPTS').AsString;
    aInfo.DelOptions:=Qry.FieldByname('T_DELOPTS').AsString;
    aInfo.SkipCPU:=Qry.FieldByname('T_SKIPCPU').AsString;
    aInfo.SkipEmu:=Qry.FieldByname('T_SKIPEMU').AsString;
    aInfo.NeedTarget:=Qry.FieldByname('T_NEEDTARGET').AsString;
    aInfo.SkipTarget:=Qry.FieldByname('T_SKIPTARGET').AsString;
    aInfo.MaxVersion:=Qry.FieldByname('T_MAXVERSION').AsString;
    aInfo.KnownRunNote:=Qry.FieldByname('T_KNOWNRUNNOTE').AsString;
    aInfo.KnownCompileNote:=Qry.FieldByname('T_KNOWNCOMPILENOTE').AsString;
    aInfo.RecompileOpt:=Qry.FieldByname('T_RECOMPILEOPT').AsString;
    aInfo.KnownCompileError:=Qry.FieldByname('T_KNOWNCOMPILEERROR').AsInteger;
    aInfo.NeededAfter:=Qry.FieldByname('T_NEEDEDAFTER').AsBoolean;
    aInfo.IsKnownRunError:=Qry.FieldByname('T_ISKNOWNRUNERROR').AsBoolean;
    aInfo.Timeout:=Qry.FieldByname('T_TIMEOUT').AsInteger;
    aInfo.Category:=Qry.FieldByname('T_CATEGORY').AsString;
    aInfo.Files:=Qry.FieldByname('T_FILES').AsString;
    aInfo.ConfigFileSrc:=Qry.FieldByname('T_CONFIGFILESRC').AsString;
    aInfo.ConfigFileDst:=Qry.FieldByname('T_CONFIGFILEDST').AsString;
    aInfo.WpoParas:=Qry.FieldByname('T_WPOPARAS').AsString;
    aInfo.WpoPasses:=Qry.FieldByname('T_WPOPASSES').AsInteger;
    aInfo.DelFiles:=Qry.FieldByname('T_DELFILES').AsString;
    aInfo.ExpectMsgs:=SplitID(Qry.FieldByname('T_EXPECTMSGS').AsString);

  finally
    Qry.Free;
  end;
end;

function TTestSQL.AddRun(const aData : TTestRunData): Int64;

Const
  SInsertRun = 'INSERT INTO TESTRUN '+
               '(TU_PLATFORM_FK, TU_SUBMITTER, TU_DATE, '+
               ' TU_COMPILERDATE, TU_COMPILERFULLVERSION, TU_COMPILERREVISION, '+
               ' TU_TESTSREVISION, TU_RTLREVISION, TU_PACKAGESREVISION  )'+
               ' VALUES '+
               '(%d,''%s'',''%s'', '+
               ' ''%s'',''%s'',''%s'', '+
               ' ''%s'',''%s'',''%s'' '+
               ') RETURNING TU_ID';

var
  Qry : string;
  PreviousID : Int64;

begin
  With aData do
    qry:=Format(SInsertRun,[PlatformID,
                            EscapeSQL(Submitter),
                            SQLDate(Date),
                            EscapeSQL(CompilerDate),
                            EscapeSQL(CompilerFullVersion),
                            EscapeSQL(CompilerRevision),
                            EscapeSQL(TestsRevision),
                            EscapeSQL(RTLRevision),
                            EscapeSQL(PackagesRevision)]);
  Result:=IDQuery(Qry);
  PreviousID:=GetLastRunByPlatformAndDate(aData.PlatformID,aData.Date);
  if PreviousID<>-1 then
    InsertTestHistory(Result,PreviousID);
end;


function TTestSQL.InsertTestHistory(TestRunID,TestPreviousID : Integer) : boolean;

const
  SQLInsert = 'INSERT INTO TESTRUNHISTORY '+
              '  (TH_ID_FK,TH_PREVIOUS_FK) '+
              'VALUES '+
              '  (%d,%d) '+
              'ON CONFLICT (TH_ID_FK) '+
              'DO UPDATE SET '+
              '  TH_PREVIOUS_FK=EXCLUDED.TH_PREVIOUS_FK';
var
  qry : string;

begin
  Qry:=format(SQLInsert,[TestRunID,TestPreviousID]);
  Result:=ExecuteQuery(Qry,False);
end;




function TTestSQL.AddTest(Name: String; AddSource: Boolean): Integer;

Const
  SInsertTest = 'INSERT INTO TESTS (T_NAME,T_ADDDATE)'+
                ' VALUES (''%s'',NOW()) ON CONFLICT (T_NAME) DO UPDATE SET T_ADDDATE=NOW() RETURNING T_ID';

Var
  Info : TConfig;
  lSrcDir : String;
  lFileName : string;
begin
  Info:=Default(TConfig);
  Result:=-1;
  lSrcDir:=IncludeTrailingPathDelimiter(TestSrcDir+RelSrcDir);
  lFileName:=ExpandFileName(lSrcDir+Name);
  Verbose(V_Debug,'Checking test filename: '+lFileName);
  Result:=IDQuery(Format(SInsertTest,[Name]));
  If Result=-1 then
    begin
    Verbose(V_WARNING,'Could not add test!');
    exit;
    end;
  If (FileExists(lFileName) and GetConfig(logprefix,lFileName,Info))
     or GetUnitTestConfig(logprefix,Name,lSrcDir,Info) then
    begin
    If AddSource then
      UpdateTest(Result,Info,tsutils.GetFileContents(Name))
    else
      UpdateTest(Result,Info,'');
    end
  else
    Verbose(V_WARNING,'Could not find test "'+Name+'" or info about this test.');
end;


function TTestSQL.UpdateTest(ID: Integer; Info: TConfig; const Source: String): Boolean;

Const
  SQLUpdateTest = 'Update TESTS SET '+
                  ' %s '+
                  'WHERE'+
                  ' (T_ID=%d)';

  function JoinIDS(IDS: Array of Integer) : string;
  var
    S : String;
    I : Integer;
  begin
    S:='';
    For I:=0 to Length(IDS)-1 do
      begin
      if I>0 then
        S:=S+',';
      S:=S+IntToStr(IDS[i]);
      end;
    Result:=S;
  end;

  procedure AddField(var S : String; const aName,aValue : String);
  begin
    if S<>'' then
      S:=S+', ';
    S:=S+Format('%s = ''%s''',[aName,EscapeSQl(aValue)])
  end;
  procedure AddField(var S : String; const aName : String; aValue : Integer);
  begin
    if S<>'' then
      S:=S+', ';
    S:=S+Format('%s = %d',[aName,aValue])
  end;

Var
  Qry :  String;

begin
  Qry:='';
  With Info do
    begin
    AddField(Qry,'T_CPU',NeedCPU);
    AddField(Qry,'T_OS',OS);
    AddField(Qry,'T_VERSION',MinVersion);
    AddField(Qry,'T_GRAPH',Bools[usesGraph]);
    AddField(Qry,'T_INTERACTIVE',Bools[IsInteractive]);
    AddField(Qry,'T_RESULT',ResultCode);
    AddField(Qry,'T_FAIL',Bools[ShouldFail]);
    AddField(Qry,'T_RECOMPILE',Bools[NeedRecompile]);
    AddField(Qry,'T_NORUN',Bools[NoRun]);
    AddField(Qry,'T_DESCRIPTION',Description);
    AddField(Qry,'T_NEEDLIBRARY',Bools[NeedLibrary]);
    AddField(Qry,'T_KNOWNRUNERROR',KnownRunError);
    AddField(Qry,'T_KNOWN',Bools[IsKnownCompileError]);
    AddField(Qry,'T_Note',Note);
    AddField(Qry,'T_OPTS',NeedOptions);
    AddField(Qry,'T_DELOPTS',DelOptions);
    AddField(Qry,'T_SKIPCPU',SkipCPU);
    AddField(Qry,'T_SKIPEMU',SkipEmu);
    AddField(Qry,'T_NEEDTARGET',NeedTarget);
    AddField(Qry,'T_SKIPTARGET',SkipTarget);
    AddField(Qry,'T_MAXVERSION',MaxVersion);
    AddField(Qry,'T_KNOWNRUNNOTE',KnownRunNote);
    AddField(Qry,'T_KNOWNCOMPILENOTE',KnownCompileNote);
    AddField(Qry,'T_RECOMPILEOPT',RecompileOpt);
    AddField(Qry,'T_KNOWNCOMPILEERROR',KnownCompileError);
    AddField(Qry,'T_NEEDEDAFTER',Bools[NeededAfter]);
    AddField(Qry,'T_ISKNOWNRUNERROR',Bools[IsKnownRunError]);
    AddField(Qry,'T_TIMEOUT', Timeout);
    AddField(Qry,'T_CATEGORY',Category);
    AddField(Qry,'T_FILES',Files);
    AddField(Qry,'T_CONFIGFILESRC',ConfigFileSrc);
    AddField(Qry,'T_CONFIGFILEDST',ConfigFileDst);
    AddField(Qry,'T_WPOPARAS',WpoParas);
    AddField(Qry,'T_WPOPASSES',WpoPasses);
    AddField(Qry,'T_DELFILES',DelFiles);
    AddField(Qry,'T_EXPECTMSGS',JoinIDS(ExpectMsgs));
    If (Source<>'') then
      AddField(Qry,'T_SOURCE',Source);
    end;
  Qry:=Format(SQLUpdateTest,[Qry,ID]);
  Result:=ExecuteQuery(Qry,False);
end;

function TTestSQL.UpdateTestResult(aData: TTestResultData): Int64;

const
  SQLUpdate = 'UPDATE TESTRESULTS SET '+
              '  TR_RESULT = %d, '+
              '  TR_TESTRUN_FK = %d, '+
              '  TR_OK = ''%s'', '+
              '  TR_SKIP = ''%s'', '+
              '  TR_LOG = ''%s'' '+
              'WHERE (TR_ID=%d)';
var
  Qry : String;
  OK, Skipped : Boolean;

begin
  with aData do
    begin
    OK:=TestOK[TestResult];
    Skipped:=TestSkipped[TestResult];
    Qry:=Format(SQLUpdate, [Ord(TestResult),RunID,Bools[OK],Bools[Skipped],EscapeSQL(Log),aData.ID]);
    Result:=aData.ID;
    end;
  ExecuteQuery(Qry,False);
end;

function TTestSQL.AddTestResult(aData: TTestResultData): Int64;

Const
  SQLInsert = 'Insert into TESTRESULTS '+
              '  (TR_TEST_FK,TR_TESTRUN_FK,TR_OK,TR_SKIP,TR_RESULT,TR_LOG) '+
              'VALUES '+
              '  (%d,%d,''%s'',''%s'',%d, ''%s'') '+
              'ON CONFLICT (TR_TEST_FK,TR_TESTRUN_FK) '+
              'DO UPDATE SET '+
              '  TR_OK = EXCLUDED.TR_OK, '+
              '  TR_SKIP = EXCLUDED.TR_SKIP, '+
              '  TR_RESULT = EXCLUDED.TR_RESULT, '+
              '  TR_LOG = EXCLUDED.TR_LOG '+
              'RETURNING TR_ID ';

Var
  Qry : String;
  OK, Skipped : Boolean;

begin
  Result:=-1;
  With aData do
    begin
    OK:=TestOK[TestResult];
    Skipped:=TestSkipped[TestResult];
    Qry:=Format(SQLInsert, [TestID,RunID,Bools[OK],Bools[Skipped],Ord(TestResult),EscapeSQL(Log)]);
    end;
  Result:=ID64Query(Qry);
end;

function TTestSQL.GetLastTestResult(aTestID, aPlatFormID: Integer): TTestResultData;

Const
  SQLSelect = 'SELECT TESTRESULTS.*, TU_DATE FROM '+
              ' TESTLASTRESULTS '+
              ' INNER JOIN TESTRESULTS ON (TL_TESTRESULTS_FK=TR_ID) '+
              ' INNER JOIN TESTRUN ON (TR_TESTRUN_FK=TU_ID) '+
              'WHERE '+
              ' (TL_TEST_FK=%d) '+
              ' AND (TL_PLATFORM_FK=%d)';

var
  Qry : TSQLQuery;

begin
  Result:=Default(TTestResultData);
  Result.TestID:=aTestID;
  Result.PlatformID:=aPlatformID;
  Qry:=CreateQuery(Format(SQLSelect,[aTestID,aPlatformID]));
  try
    Qry.Open;
    If not Qry.IsEmpty then
      begin
      Result.ID:=Qry.FieldByName('TR_ID').AsLargeInt;
      Result.TestResult:=TTestStatus(Qry.FieldByName('TR_RESULT').AsInteger);
      Result.RunID:=Qry.FieldByName('TR_TESTRUN_FK').AsLargeInt;
      Result.Log:=Qry.FieldByName('TR_LOG').AsString;
      Result.Date:=Qry.FieldByName('TU_DATE').AsDateTime;
      end
    else
      Result.ID:=-1;
  finally
    if Qry.SQLTransaction.Active then
      Qry.SQLTransaction.Commit;
    Qry.Free;
  end;

end;

function TTestSQL.GetFailCount(aRunID: Int64): Int64;
const
  SQLSelectFailCount =
               'SELECT (TU_FAILEDTOCOMPILE + TU_FAILEDTOFAIL + TU_FAILEDTORUN) as thecount '+
              ' FROM TESTRUN WHERE (TU_ID=%d)';

begin
  Result:=ID64Query(Format(SQLSelectFailCount,[aRunID]));
end;

function TTestSQL.AddLastResult(TestID, PlatformID: Integer; ResultID: Int64) : Boolean;

const
  SQLInsert = 'Insert into TESTLASTRESULTS '+
             '  (TL_TEST_FK,TL_PLATFORM_FK,TL_TESTRESULTS_FK) '+
             'VALUES '+
             '  (%d,%d,%d) '+
             'ON CONFLICT (TL_TEST_FK,TL_PLATFORM_FK) '+
             'DO UPDATE SET TL_TESTRESULTS_FK = EXCLUDED.TL_TESTRESULTS_FK ';

begin
  Result:=ExecuteQuery(Format(SQLInsert,[TestId,PlatFormID,ResultID]),False);
end;

function TTestSQL.AddPreviousResult(TestID, PlatformID: Integer; ResultID: Int64): Boolean;
const
  SQLInsert = 'Insert into TESTPREVIOUSRESULTS '+
             '  (TPR_TEST_FK,TPR_PLATFORM_FK,TPR_TESTRESULTS_FK) '+
             'VALUES '+
             '  (%d,%d,%d) '+
             'ON CONFLICT (TPR_TEST_FK,TPR_PLATFORM_FK) '+
             'DO UPDATE SET TPR_TESTRESULTS_FK = EXCLUDED.TPR_TESTRESULTS_FK ';

begin
  Result:=ExecuteQuery(Format(SQLInsert,[TestId,PlatFormID,ResultID]),False);
end;

function TTestSQL.UpdateTestRun(aData: TTestRunData): Boolean;
var
  Qry : string;
  I : TTestStatus;

  Procedure AddTo(S : String);

  begin
    if Qry<>'' then
      Qry:=Qry+' , ';
    Qry:=Qry+S;
  end;

begin
  Qry:='';
  for i:=low(TTestStatus) to high(TTestStatus) do
    AddTo(format('%s=%d',[SQLField[i],aData.StatusCount[i]]));
  qry:='UPDATE TESTRUN SET '+Qry+' WHERE TU_ID='+format('%d',[aData.RunID]);
  ExecuteQuery(Qry,False);
  Result:=True;
end;

function TTestSQL.GetFailCount(aRunID: Integer): Int64;
begin
  Result:=ID64Query(Format('SELECT (TU_FAILEDTOCOMPILE + TU_FAILEDTOFAIL + TU_FAILEDTORUN) FROM TESTRUN WHERE (TU_ID=%d)',[aRunID]));
end;

function TTestSQL.RequireTestID(const aName: String): Integer;

begin
  Result:=GetTestID(aName);
  If Result=-1 then
    Result:=AddTest(aName,True);
  If Result=-1 then
    Verbose(V_WARNING,'Could not find or create entry for test '+aName);
end;

function TTestSQL.CleanTestRun(ID: Integer): Boolean;

Const
  SDeleteRun = 'DELETE FROM TESTRESULTS WHERE TR_TESTRUN_FK=%d';

begin
  Result:=ExecuteQuery(Format(SDeleteRun,[ID]),False);
end;

function TTestSQL.GetPreviousRunID(RunID: Int64): Int64;

begin
  Result:=ID64Query(Format('SELECT TH_PREVIOUS_FK FROM TESTRUNHISTORY WHERE (TH_ID_FK=%d)',[RunID]));
end;

function TTestSQL.GetNextRunID(RunID: Int64): Int64;

begin
  Result:=ID64Query(Format('SELECT TH_ID_FK FROM TESTRUNHISTORY WHERE (TH_PREVIOUS_FK=%d)',[RunID]));
end;


end.
