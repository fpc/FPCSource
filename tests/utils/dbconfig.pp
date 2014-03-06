{
    This file is part of the Free Pascal test suite.
    Copyright (c) 2002 by the Free Pascal development team.

    This program iupdates TESTCONFIG anf TESTRUNHISTORY tables
    with the last tests run.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$mode objfpc}
{$h+}
{$ifndef win32}
  {$linklib pthread}
{$endif}

program dbconfig;

uses
  sysutils,teststr,testu,tresults,
  mysql55dyn,dbtests;


Var
  StatusCount : Array[TTestStatus] of Integer;
  UnknownLines : integer;


Procedure ExtractTestFileName(Var Line : string);

Var I : integer;

begin
  I:=Pos(' ',Line);
  If (I<>0) then
    Line:=Copy(Line,1,I-1);
end;

Function Analyse(Var Line : string; Var Status : TTestStatus) : Boolean;

Var
  TS : TTestStatus;

begin
  Result:=False;
  For TS:=FirstStatus to LastStatus do
    begin
    Result:=Pos(StatusText[TS],Line)=1;
    If Result then
      begin
      Status:=TS;
      Delete(Line,1,Length(StatusText[TS]));
      ExtractTestFileName(Line);
      Break;
      end;
    end;
end;

Type

TConfigOpt = (
  coDatabaseName,
  coHost,
  coUserName,
  coPassword,
  coPort,
  coLogFile,
  coLongLogFile,
  coOS,
  coCPU,
  coCategory,
  coVersion,
  coDate,
  coSubmitter,
  coMachine,
  coComment,
  coTestSrcDir,
  coRelSrcDir,
  coVerbose,
  coOffset
 );

{ Additional options only for dbdigest.cfg file }

TConfigAddOpt = (
  coCompilerDate,
  coCompilerFullVersion,
  coSvnCompilerRevision,
  coSvnTestsRevision,
  coSvnRTLRevision,
  coSvnPackagesRevision
 );

Const

ConfigStrings : Array [TConfigOpt] of string = (
  'databasename',
  'host',
  'username',
  'password',
  'port',
  'logfile',
  'longlogfile',
  'os',
  'cpu',
  'category',
  'version',
  'date',
  'submitter',
  'machine',
  'comment',
  'testsrcdir',
  'relsrcdir',
  'verbose',
  'offset'
);

ConfigOpts : Array[TConfigOpt] of char =(
 'd', {  coDatabaseName }
 'h', {  coHost }
 'u', {  coUserName }
 'p', {  coPassword }
 'P', {  coPort }
 'l', {  coLogFile }
 'L', {  coLongLogFile }
 'o', {  coOS }
 'c', {  coCPU }
 'a', {  coCategory }
 'v', {  coVersion }
 't', {  coDate }
 's', {  coSubmitter }
 'm', {  coMachine }
 'C', {  coComment }
 'S', {  coTestSrcDir }
 'r', {  coRelSrcDir }
 'V', {  coVerbose }
 'O'  { coOffset }
);

ConfigAddStrings : Array [TConfigAddOpt] of string = (
  'compilerdate',
  'compilerfullversion',
  'svncompilerrevision',
  'svntestsrevision',
  'svnrtlrevision',
  'svnpackagesrevision'
 );

ConfigAddCols : Array [TConfigAddOpt] of string = (
  'TU_COMPILERDATE',
  'TU_COMPILERFULLVERSION',
  'TU_SVNCOMPILERREVISION',
  'TU_SVNTESTSREVISION',
  'TU_SVNRTLREVISION',
  'TU_SVNPACKAGESREVISION'
 );


Var
  TestOS,
  TestCPU,
  TestVersion,
  TestCategory,
  DatabaseName,
  HostName,
  UserName,
  Password,
  Port,
  LongLogFileName,
  LogFileName,
  Submitter,
  Machine,
  Comment,
  OffsetString : String;
  TestDate : TDateTime;
  TestCompilerDate,
  TestCompilerFullVersion,
  TestSvnCompilerRevision,
  TestSvnTestsRevision,
  TestSvnRTLRevision,
  TestSvnPackagesRevision : String;
  ConfigID : Integer;

Procedure SetAddOpt (O : TConfigAddOpt; Value : string);
begin
  Case O of
    coCompilerDate:
      TestCompilerDate:=Value;
    coCompilerFullVersion:
      TestCompilerFullVersion:=Value;
    coSvnCompilerRevision:
      TestSvnCompilerRevision:=Value;
    coSvnTestsRevision:
      TestSvnTestsRevision:=Value;
    coSvnRTLRevision:
      TestSvnRTLRevision:=Value;
    coSvnPackagesRevision:
      TestSvnPackagesRevision:=Value;
  end;
end;

Procedure SetOpt (O : TConfigOpt; Value : string);
var
  year,month,day,min,hour : word;
begin
  Case O of
    coDatabaseName : DatabaseName:=Value;
    coHost         : HostName:=Value;
    coUserName     : UserName:=Value;
    coPassword     : Password:=Value;
    coPort         : Port:=Value;
    coLogFile      : LogFileName:=Value;
    coLongLogFile  : LongLogFileName:=Value;
    coOS           : TestOS:=Value;
    coCPU          : TestCPU:=Value;
    coCategory     : TestCategory:=Value;
    coVersion      : TestVersion:=Value;
    coDate         :
      begin
        { Formated like YYYYMMDDhhmm }
	if Length(value)=12 then
	  begin
	    year:=StrToInt(Copy(value,1,4));
	    month:=StrToInt(Copy(value,5,2));
	    day:=StrToInt(Copy(Value,7,2));
	    hour:=StrToInt(Copy(Value,9,2));
	    min:=StrToInt(Copy(Value,11,2));
	    TestDate:=EncodeDate(year,month,day)+EncodeTime(hour,min,0,0);
	  end
	else
	  Verbose(V_Error,'Error in date format, use YYYYMMDDhhmm');
      end;
    coSubmitter    : Submitter:=Value;
    coMachine      : Machine:=Value;
    coComment      : Comment:=Value;
    coOffset       : OffsetString:=Value;
    coVerbose      : DoVerbose:=true;
    coTestSrcDir   :
      begin
        TestSrcDir:=Value;
	if (TestSrcDir<>'') and (TestSrcDir[length(TestSrcDir)]<>'/') then
	  TestSrcDir:=TestSrcDir+'/';
      end;
    coRelSrcDir   :
      begin
        RelSrcDir:=Value;
	if (RelSrcDir<>'') and (RelSrcDir[length(RelSrcDir)]<>'/') then
	  RelSrcDir:=RelSrcDir+'/';
	if (RelSrcDir<>'') and (RelSrcDir[1]='/') then
	  RelSrcDir:=copy(RelSrcDir,2,length(RelSrcDir)-1);
      end;
  end;
end;

Function ProcessOption(S: String) : Boolean;

Var
  N : String;
  I : Integer;
  co : TConfigOpt;
  coa : TConfigAddOpt;

begin
  Verbose(V_DEBUG,'Processing option: '+S);
  I:=Pos('=',S);
  Result:=(I<>0);
  If Result then
    begin
    N:=Copy(S,1,I-1);
    Delete(S,1,I);
    For co:=low(TConfigOpt) to high(TConfigOpt) do
      begin
      Result:=CompareText(ConfigStrings[co],N)=0;
      If Result then
        begin
          SetOpt(co,S);
          Exit;
        end;
      end;
    For coa:=low(TConfigAddOpt) to high(TConfigAddOpt) do
      begin
      Result:=CompareText(ConfigAddStrings[coa],N)=0;
      If Result then
        begin
          SetAddOpt(coa,S);
          Exit;
        end;
      end;
    end;
  Verbose(V_ERROR,'Unknown option : '+n+S);
end;

Procedure ProcessConfigfile(FN : String);

Var
  F : Text;
  S : String;
  I : Integer;

begin
  // Set the default value for old digests without RelSrcDir to the rtl/compiler
  // testsuite
  RelSrcDir:='tests/';
  If Not FileExists(FN) Then
    Exit;
  Verbose(V_DEBUG,'Parsing config file: '+FN);
  Assign(F,FN);
  {$i-}
  Reset(F);
  If IOResult<>0 then
    Exit;
  {$I+}
  While not(EOF(F)) do
    begin
    ReadLn(F,S);
    S:=trim(S);
    I:=Pos('#',S);
    If I<>0 then
      S:=Copy(S,1,I-1);
    If (S<>'') then
      ProcessOption(S);
    end;
  Close(F);
end;

Procedure ProcessCommandLine;

Var
  I : Integer;
  O : String;
  c,co : TConfigOpt;
  ShortOptFound, Found : Boolean;

begin
  I:=1;
  While I<=ParamCount do
    begin
    O:=Paramstr(I);
    ShortOptFound:=(Length(O)=2) and (O[1]='-');
    If ShortOptFound then
      For co:=low(TConfigOpt) to high(TConfigOpt) do
        begin
        Found:=(O[2]=ConfigOpts[co]);
        If Found then
          begin
          c:=co;
          Break;
          end;
        end;
    If not ShortOptFound then
      begin
        Found:=false;
        { accept long options }
        if (copy(O,1,2)='--') then
          begin
            { remove -- }
            O:=copy(O,3,length(O));
            For co:=low(TConfigOpt) to high(TConfigOpt) do
              begin
              Found:=(O=ConfigStrings[co]);
              If Found then
                begin
                c:=co;
                Break;
                end;
              end;
          end
      end;
    if not Found then
      Verbose(V_ERROR,'Illegal command-line option : '+O)
    else
      begin
      Found:=(I<ParamCount);
      If Not found then
        Verbose(V_ERROR,'Option requires argument : '+O)
      else
        begin
        inc(I);
        O:=Paramstr(I);
        SetOpt(c,o);
        end;
      end;
    Inc(I);
    end;
end;

function GetTestRunFieldID(const name : string; TestRunID : Integer) : Integer;
begin
  GetTestRunFieldID:=IDQuery(
    format('SELECT %s FROM TESTRUN WHERE TU_ID=%d',[name,TestRunID]));
end;
function GetTestRunStringFieldID(const name : string; TestRunID : Integer) : String;
begin
  GetTestRunStringFieldID:=StringQuery(
    format('SELECT %s FROM TESTRUN WHERE TU_ID=%d',[name,TestRunID]));
end;


function GetSubmitter(TestRunID:Integer) : String;
begin
  GetSubmitter:=GetTestRunStringFieldID('TU_SUBMITTER',TestRunID);
end;

function GetComment(TestRunID:Integer) : String;
begin
  GetComment:=GetTestRunStringFieldID('TU_COMMENT',TestRunID);
end;

function GetMachine(TestRunID:Integer) : String;
begin
  GetMachine:=GetTestRunStringFieldID('TU_MACHINE',TestRunID);
end;

function GetDate(TestRunID:Integer) : String;
begin
  GetDate:=GetTestRunStringFieldID('TU_DATE',TestRunID);
end;


function GetTestConfigId(TestRunID : Integer) : Integer;
var
  qry : string;
begin
  qry:='SELECT TCONF_ID FROM TESTCONFIG WHERE ' +
       'TCONF_CPU_FK=%d AND ' +
       'TCONF_OS_FK=%d AND ' +
       'TCONF_VERSION_FK=%d AND ' +
       'TCONF_CATEGORY_FK=%d AND ' +
       'TCONF_SUBMITTER="%s" AND ' +
       'TCONF_MACHINE="%s" AND ' +
       'TCONF_COMMENT="%s" ';
  ConfigID:=IDQuery(format(qry,[
                     GetTestRunFieldID('TU_CPU_FK',TestRunID),
                     GetTestRunFieldID('TU_OS_FK',TestRunID),
                     GetTestRunFieldID('TU_VERSION_FK',TestRunID),
                     GetTestRunFieldID('TU_CATEGORY_FK',TestRunID),
                     GetSubmitter(TestRunID),
                     GetMachine(TestRunID),
                     GetComment(TestRunID)]));
  GetTestConfigID:=ConfigID;
end;

function UpdateTestConfigID(TestRunID : Integer) : boolean;
var
  qry : string;
  firstRunID, lastRunID,PrevRunID : Integer;
  RunCount : Integer;
  res : TQueryResult;
  AddCount : boolean;
begin
  AddCount:=false;
  UpdateTestConfigID:=false;
  qry:=format('SELECT TCONF_FIRST_RUN_FK FROM TESTCONFIG WHERE TCONF_ID=%d',[ConfigID]);
  FirstRunID:=IDQuery(qry);
  if TestRunID<FirstRunID then
    begin
      Verbose(V_Warning,format('FirstRunID changed from %d to %d',[FirstRunID,TestRunID]));
      qry:=format('UPDATE TESTCONFIG SET TCONF_FIRST_RUN_FK=%d WHERE TCONF_ID=%d',
                  [TestRunID,ConfigID]);
      if RunQuery(qry,res) then
        FreeQueryResult(res)
      else
        Verbose(V_Warning,'Update of LastRunID failed');
    end;
  qry:=format('SELECT TCONF_LAST_RUN_FK FROM TESTCONFIG WHERE TCONF_ID=%d',[ConfigID]);
  LastRunID:=IDQuery(qry);
  if TestRunID>LastRunID then
    begin
      qry:=format('UPDATE TESTCONFIG SET TCONF_LAST_RUN_FK=%d WHERE TCONF_ID=%d',
                  [TestRunID,ConfigID]);
      if RunQuery(qry,res) then
        FreeQueryResult(res)
      else
        Verbose(V_Warning,'Update of LastRunID failed');
    end
   else
    Verbose(V_Warning,format('LastRunID %di,new %d',[LastRunID,TestRunID]));
  qry:=format('SELECT TCONF_NEW_RUN_FK FROM TESTCONFIG WHERE TCONF_ID=%d',[ConfigID]);
  PrevRunID:=IDQuery(qry);
  if TestRunID<>PrevRunID then
    begin
      qry:=format('UPDATE TESTCONFIG SET TCONF_NEW_RUN_FK=%d WHERE TCONF_ID=%d',
                  [TestRunID,ConfigID]);
      if RunQuery(qry,res) then
        FreeQueryResult(res)
      else
        Verbose(V_Warning,'Update of LastRunID failed');
      AddTestHistoryEntry(TestRunID,PrevRunID);
      AddCount:=true;
    end
  else
    Verbose(V_Warning,'TestRunID is equal to last!');
  qry:=format('SELECT TCONF_COUNT_RUNS FROM TESTCONFIG WHERE TCONF_ID=%d',[ConfigID]);
  RunCount:=IDQuery(qry);
  { Add one to run count }
  if AddCount then
    begin
      Inc(RunCount);
      qry:=format('UPDATE TESTCONFIG SET TCONF_COUNT_RUNS=%d WHERE TCONF_ID=%d',
                  [RunCount,ConfigID]);
      if RunQuery(qry,res) then
        FreeQueryResult(res)
      else
        Verbose(V_Warning,'Update of TU_COUNT_RUNS failed');
    end;
end;

function InsertNewTestConfigId(TestRunID: Integer) : longint;
var
  qry : string;
  TestDate : string;
begin
  TestDate:=GetDate(TestRunID);
  qry:='INSERT INTO TESTCONFIG '+
        '(TCONF_NEW_RUN_FK,TCONF_FIRST_RUN_FK,TCONF_LAST_RUN_FK,' +
         'TCONF_CPU_FK,TCONF_OS_FK,TCONF_VERSION_FK,TCONF_CATEGORY_FK,'+
         'TCONF_SUBMITTER,TCONF_MACHINE,TCONF_COMMENT,'+
         'TCONF_NEW_DATE,TCONF_FIRST_DATE,TCONF_LAST_DATE) ';
    qry:=qry+format(' VALUES(%d,%d,%d,%d,%d,%d,%d,"%s","%s","%s","%s","%s","%s") ',
                    [TestRunID,TestRunID,TestRunID,
                     GetTestRunFieldID('TU_CPU_FK',TestRunID),
                     GetTestRunFieldID('TU_OS_FK',TestRunID),
                     GetTestRunFieldID('TU_VERSION_FK',TestRunID),
                     GetTestRunFieldID('TU_CATEGORY_FK',TestRunID),
                     GetSubmitter(TestRunID),
                     GetMachine(TestRunID),
                     GetComment(TestRunID),
                     TestDate,TestDate,TestDate]);
  Result:=InsertQuery(qry);
  AddTestHistoryEntry(TestRunID,0);
end;

Procedure InsertRunsIntoConfigAndHistory(var GlobalRes : TQueryResult);

var
  i,fid, num_fields : Integer;
  Row : PPchar;
  s : string;
  runid,previd : Integer;
begin
  with GlobalRes^ do
    begin
      num_fields:=mysql_num_fields(GlobalRes);
      Writeln('Row count=',row_count);
      for i:=0 to row_count-1 do
        begin
          row:=mysql_fetch_row(GlobalRes);
          runid:=StrToIntDef(strpas(Row[0]),-1);
          previd:=GetTestPreviousRunHistoryID(RunID);
          if previd>=0 then
            begin
              Writeln(format('RunID=%d already handled prevID=%d',[runID,prevID]));
              continue;
            end
          else
            begin
              if GetTestConfigId(runid)=-1 then
                begin
                   InsertNewTestConfigId(RunID);
                end
              else
                UpdateTestConfigID(RunID);
            end;
        end;
    end;
end;

Procedure GetAllTestRuns(var GlobalRes : TQueryResult);
var
  qry : string;
begin
  qry:='SELECT * FROM TESTRUN ORDER BY TU_ID';
  if OffsetString<>'' then
    qry:=qry+' LIMIT 1000 OFFSET '+OffsetString;
  if not RunQuery(qry,GlobalRes) then
    Verbose(V_Warning,'Failed to fetch testrun content');
end;


var
  GlobalRes : TQueryResult;

begin
  ProcessConfigFile('dbdigest.cfg');
  ProcessCommandLine;
  ConnectToDatabase(DatabaseName,HostName,UserName,Password,Port);
  GetAllTestRuns(GlobalRes);
  InsertRunsIntoConfigAndHistory(GlobalRes);
end.
