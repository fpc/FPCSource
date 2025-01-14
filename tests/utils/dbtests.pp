{$mode objfpc}
{$H+}

unit dbtests;

Interface

Uses
  sqldb, tresults, testu, pqconnection;

const
  // Ini file constants
  SSection    = 'Database';
  KeyName     = 'Name';
  KeyHost     = 'Host';
  KeyUser     = 'UserName';
  KeyPassword = 'Password';
  KeyPort     = 'Port';

Type

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

    Class Procedure FreeQueryResult (Var Res : TSQLQuery);
    Class Function  GetIntResultField (Res : TSQLQuery; Id : Integer) : Integer;
    Class Function  GetInt64ResultField (Res : TSQLQuery; Id : Integer) : Int64;
    Class Function  GetStrResultField (Res : TSQLQuery; Id : Integer) : String;
    { ---------------------------------------------------------------------
        Low-level DB access.
      ---------------------------------------------------------------------}

    function CreateQuery(const ASQL: String): TSQLQuery;
    Function  OpenQuery (Qry : String; Out Res : TSQLQuery; Silent : Boolean) : Boolean ;
    Function  IDQuery(Qry : String) : Integer;
    Function  ID64Query(Qry : String) : Int64;
    Function  StringQuery(Qry : String) : String;
  Public
    { ---------------------------------------------------------------------
      High-level access
      ---------------------------------------------------------------------}
    Constructor create(aDatabaseName,aHost,aUser,aPassword : String; aPort : Word);
    Destructor destroy; override;
    Function ConnectToDatabase : Boolean;
    Function  ExecuteQuery (Qry : String; Silent : Boolean) : Boolean ;
    Procedure DisconnectDatabase;
    // Adding things
    Function AddCategory(const aName : String) : Integer;
    Function AddCPU(const aName : String) : Integer;
    Function AddOS(const aName : String) : Integer;
    function AddVersion(const aName: String; aReleaseDate: TDateTime): Integer;
    Function AddPlatform(const aData : TTestRunData) : Integer;
    Function AddTest(Name : String; AddSource : Boolean) : Integer;
    function AddRun(const aData: TTestRunData): Int64;
    Function AddTestResult(aData : TTestResultData) : Int64;
    function AddLastResult(TestID, PlatformID: Integer; ResultID: Int64): Boolean;
    // Get ID based on key
    Function GetTestID(Name : string) : Integer;
    Function GetOSID(Name : String) : Integer;
    Function GetCPUID(Name : String) : Integer;
    Function GetCategoryID(Name : String) : Integer;
    Function GetVersionID(Name : String) : Integer;
    function GetPlatformID(aData: TTestRunData; aAllowCreate: Boolean): Integer;
    Function GetRunID(aData : TTestRunData) : Int64;
    function GetLastTestResult(aTestID, aPlatFormID: Integer): TTestResultData;
    //
    Function UpdateTest(ID : Integer; Info : TConfig; Source : String) : Boolean;
    function UpdateTestResult(aData: TTestResultData): Int64;
    function UpdateTestRun(aData : TTestRunData): Boolean;
    Function RequireTestID(Name : String): Integer;
    Function CleanTestRun(ID : Integer) : Boolean;
    Class Function  EscapeSQL(S : String) : String;
    Class Function  SQLDate(D : TDateTime) : String;
    Property RelSrcDir : String Read FRelSrcDir Write FRelSrcDir;
    Property TestSrcDir : string read FTestSrcDir Write FTestSrcDir;

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

class function TTestSQL.GetIntResultField(Res: TSQLQuery; Id: Integer): Integer;


begin
  If (Res=Nil) or (res.IsEmpty) or (ID>=Res.Fields.Count) then
    Result:=-1
  else
    Result:=Res.Fields[ID].AsInteger;
  Verbose(V_SQL,'Field value '+IntToStr(Result));
end;

class function TTestSQL.GetInt64ResultField(Res: TSQLQuery; Id: Integer): Int64;
begin
  If (Res=Nil) or (ID>=Res.Fields.Count) then
    Result:=-1
  else
    Result:=Res.Fields[ID].AsLargeInt;
  Verbose(V_SQL,'Field value '+IntToStr(Result));
end;

class function TTestSQL.GetStrResultField(Res: TSQLQuery; Id: Integer): String;
begin
  If (Res=Nil) or (ID>=Res.Fields.Count) then
    Result:=''
  else
    Result:=Res.Fields[ID].AsString;
  Verbose(V_SQL,'Field value '+Result);
end;

function TTestSQL.AddPlatform(const aData : TTestRunData) : Integer;

const
  SQLInsert = 'INSERT INTO TESTPLATFORM (TP_CPU_FK, TP_OS_FK, TP_VERSION_FK, TP_CATEGORY_FK, TP_CONFIG) '+
              ' VALUES (%d, %d, %d, %d, ''%s'') '+
              '  RETURNING TP_ID';

begin
  With aData do
    Result:=IDQuery(Format(SQLInsert,[CPUID,OSID,VersionID,CategoryID,EscapeSQL(config)]));
end;

class procedure TTestSQL.FreeQueryResult(var Res: TSQLQuery);

begin
  if Assigned(Res) and Assigned(Res.Transaction) then
    (Res.Transaction as TSQLTransaction).Commit;
  FreeAndNil(Res);
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

class function TTestSQL.EscapeSQL(S: String): String;

begin
//  Result:=StringReplace(S,'\','\\',[rfReplaceAll]);
  Result:=StringReplace(S,'''','''''',[rfReplaceAll]);
  Verbose(V_SQL,'EscapeSQL : "'+S+'" -> "'+Result+'"');
end;


class function TTestSQL.SQLDate(D: TDateTime): String;

begin
  Result:=FormatDateTime('YYYY/MM/DD hh:nn:ss',D);
end;

{ ---------------------------------------------------------------------
  High-level access
  ---------------------------------------------------------------------}


function TTestSQL.GetTestID(Name: string): Integer;

Const
  SFromName = 'SELECT T_ID FROM TESTS WHERE (T_NAME=''%s'')';

begin
  Result:=IDQuery(Format(SFromName,[Name]));
end;

function TTestSQL.GetOSID(Name: String): Integer;

Const
  SFromName = 'SELECT TO_ID FROM TESTOS WHERE (TO_NAME=''%s'')';

begin
  Result:=IDQuery(Format(SFromName,[Name]));
end;

function TTestSQL.GetVersionID(Name: String): Integer;

Const
  SFromName = 'SELECT TV_ID FROM TESTVERSION WHERE (TV_VERSION=''%s'')';

begin
  Result:=IDQuery(Format(SFromName,[Name]));
end;

function TTestSQL.GetPlatformID(aData: TTestRunData; aAllowCreate: Boolean): Integer;

Const
  SQLSelect = 'SELECT TP_ID FROM TESTPLATFORM ' +
             ' WHERE ' +
             '  (TP_VERSION_FK=%d)' +
             '  AND (TP_OS_FK=%d)' +
             '  AND (TP_CPU_FK=%d)' +
             '  AND (TP_CATEGORY_FK=%d)' +
             '  AND (TP_CONFIG=''%s'')';

begin
  With aData do
    Result:=IDQuery(Format(SQLSelect,[VersionID,OSID,CPUID,CategoryID,Config]));
  if (Result=-1) and aAllowCreate then
    Result:=AddPlatform(aData)
end;

function TTestSQL.GetCPUID(Name: String): Integer;

Const
  SFromName = 'SELECT TC_ID FROM TESTCPU WHERE (TC_NAME=''%s'')';

begin
  Result:=IDQuery(Format(SFromName,[Name]));
end;

function TTestSQL.GetCategoryID(Name: String): Integer;

Const
  SFromName = 'SELECT TA_ID FROM TESTCATEGORY WHERE (TA_NAME=''%s'')';

begin
  Result:=IDQuery(Format(SFromName,[Name]));
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

function TTestSQL.AddRun(const aData : TTestRunData): Int64;

Const
  SInsertRun = 'INSERT INTO TESTRUN '+
               '(TU_PLATFORM_FK, TU_MACHINE, TU_SUBMITTER, TU_DATE, '+
               ' TU_COMPILERDATE, TU_COMPILERFULLVERSION, TU_COMPILERREVISION, '+
               ' TU_TESTSREVISION, TU_RTLREVISION, TU_PACKAGESREVISION  )'+
               ' VALUES '+
               '(%d,''%s'',''%s'',''%s'', '+
               ' ''%s'',''%s'',''%s'', '+
               ' ''%s'',''%s'',''%s'' '+
               ') RETURNING TU_ID';

var
  Qry : string;
begin
  With aData do
    qry:=Format(SInsertRun,[PlatformID,
                            EscapeSQL(Machine),
                            EscapeSQL(Submitter),
                            SQLDate(Date),
                            EscapeSQL(CompilerDate),
                            EscapeSQL(CompilerFullVersion),
                            EscapeSQL(CompilerRevision),
                            EscapeSQL(TestsRevision),
                            EscapeSQL(RTLRevision),
                            EscapeSQL(PackagesRevision)]);
  Result:=IDQuery(Qry);
end;



function TTestSQL.AddTest(Name: String; AddSource: Boolean): Integer;

Const
  SInsertTest = 'INSERT INTO TESTS (T_NAME,T_ADDDATE)'+
                ' VALUES (''%s'',NOW()) RETURNING T_ID';

Var
  Info : TConfig;
  lSrcDir : String;
  lFileName : string;
begin
  Info:=Default(TConfig);
  Result:=-1;
  lSrcDir:=IncludeTrailingPathDelimiter(TestSrcDir+RelSrcDir);
  lFileName:=ExpandFileName(lSrcDir+Name);
  Verbose(V_Normal,'Checking test filename: '+lFileName);
  If (FileExists(lFileName) and GetConfig(lFileName,Info))
     or GetUnitTestConfig(Name,lSrcDir,Info) then
    begin
    Result:=IDQuery(Format(SInsertTest,[Name]));
    If Result=-1 then
      Verbose(V_WARNING,'Could not add test!')
    else If AddSource then
      UpdateTest(Result,Info,testu.GetFileContents(Name))
    else
      UpdateTest(Result,Info,'');
    end
  else
    Verbose(V_WARNING,'Could not find test "'+Name+'" or info about this test.');
end;


function TTestSQL.UpdateTest(ID: Integer; Info: TConfig; Source: String): Boolean;

Const
  SUpdateTest = 'Update TESTS SET '+
                ' T_CPU=''%s'', T_OS=''%s'', T_VERSION=''%s'','+
                ' T_GRAPH=''%s'', T_INTERACTIVE=''%s'', T_RESULT=%d,'+
                ' T_FAIL=''%s'', T_RECOMPILE=''%s'', T_NORUN=''%s'','+
                ' T_NEEDLIBRARY=''%s'', T_KNOWNRUNERROR=%d,'+
                ' T_KNOWN=''%s'', T_NOTE=''%s'', T_OPTS = ''%s'''+
                ' %s '+
                'WHERE'+
                ' T_ID=%d';


Var
  Qry : String;

begin
  If Source<>'' then
    begin
    Source:=EscapeSQL(Source);
    Source:=', T_SOURCE='''+Source+'''';
    end;
  With Info do
    Qry:=Format(SUpdateTest,[EscapeSQL(NeedCPU),'',EscapeSQL(MinVersion),
                             Bools[usesGraph],Bools[IsInteractive],ResultCode,
                             Bools[ShouldFail],Bools[NeedRecompile],Bools[NoRun],
                             Bools[NeedLibrary],KnownRunError,
                             Bools[IsKnownCompileError],EscapeSQL(Note),EscapeSQL(NeedOptions),
                             Source,
                             ID
     ]);
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
  aData.ID:=Result;
end;

function TTestSQL.GetLastTestResult(aTestID, aPlatFormID: Integer): TTestResultData;

Const
  SQLSelect = 'SELECT TESTRESULTS.* FROM '+
              ' TESTLASTRESULTS INNER JOIN TESTRESULTS ON (TL_TESTRESULTS_FK=TR_ID) '+
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
      end
    else
      Result.ID:=-1;
  finally
    if Qry.SQLTransaction.Active then
      Qry.SQLTransaction.Commit;
    Qry.Free;
  end;

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

function TTestSQL.RequireTestID(Name: String): Integer;

begin
  Result:=GetTestID(Name);
  If Result=-1 then
    Result:=AddTest(Name,True);
  If Result=-1 then
    Verbose(V_WARNING,'Could not find or create entry for test '+Name);
end;

function TTestSQL.CleanTestRun(ID: Integer): Boolean;

Const
  SDeleteRun = 'DELETE FROM TESTRESULTS WHERE TR_TESTRUN_FK=%d';

begin
  Result:=ExecuteQuery(Format(SDeleteRun,[ID]),False);
end;


end.
