{
    This file is part of the Free Pascal test suite.
    Copyright (c) 2002 by the Free Pascal development team.

    This program inserts the last tests run 
    into TESTSUITE database.

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

program dbdigest;

uses
  types, classes, custapp, sysutils, inifiles, teststr, testu, tresults, dbtests, digestanalyst;

Type

  { TDBDigestApplication }

  TDBDigestApplication = class(TCustomApplication)
  Const
     ShortOpts =
      'd'+ {  coDatabaseName }
      'h'+ {  coHost }
      'u'+ {  coUserName }
      'p'+ {  coPassword }
      'P'+ {  coPort }
      'l'+ {  coLogFile }
      'L'+ {  coLongLogFile }
      'o'+ {  coOS }
      'c'+ {  coCPU }
      'a'+ {  coCategory }
      'v'+ {  coVersion }
      't'+ {  coDate }
      's'+ {  coSubmitter }
      'm'+ {  coMachine }
      'C'+ {  coComment }
      'S'+ {  coTestSrcDir }
      'r'+ {  coRelSrcDir }
      'V'+ {  coVerbose }
      'Q'  {  coSQL }
     ;

    LongOpts : Array of string = (
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
      'sql',
      'compilerdate',
      'compilerfullversion',
      'svncompilerrevision',
      'svntestsrevision',
      'svnrtlrevision',
      'svnpackagesrevision'
    );
    // Return true if we can continue
    function ProcessCommandLine(var aConfig: TDigestConfig; var aData: TTestRunData): Boolean;
  private
    class function ExtractDate(aValue: string): TDateTime;
    procedure Analyze(const aConfig: TDigestConfig; const aData: TTestRunData);
    procedure ProcessConfigfile(const aFileName: String; var aConfig: TDigestConfig; var aData: TTestRunData);
    function ProcessOption(const aOption: String; aValue: String; var aConfig: TDigestConfig; var aData: TTestRunData): Boolean;
    procedure ReadSystemDBConfig(var aConfig: TDigestConfig);
  protected
    procedure DoRun; override;
    procedure Usage(const aMsg: String);
  end;

class Function TDBDigestApplication.ExtractDate(aValue : string) : TDateTime;

var
  year,month,day,min,hour : word;

begin
  if Length(avalue)=12 then
    begin
      year:=StrToInt(Copy(avalue,1,4));
      month:=StrToInt(Copy(avalue,5,2));
      day:=StrToInt(Copy(aValue,7,2));
      hour:=StrToInt(Copy(aValue,9,2));
      min:=StrToInt(Copy(aValue,11,2));
      Result:=EncodeDate(year,month,day)+EncodeTime(hour,min,0,0);
    end
  else
    Verbose(V_Error,'Error in date format, use YYYYMMDDhhmm');
end;

Function TDBDigestApplication.ProcessOption(const aOption : String; aValue: String; var aConfig : TDigestConfig; var aData : TTestRunData) : Boolean;

begin
  Result:=True;
  Verbose(V_DEBUG,'Processing option: '+aOption);
  Case aOption of
    'd','databasename' : aConfig.databasename:=aValue;
    'h','host' : aConfig.host:=aValue;
    'u','username': aConfig.username:=aValue;
    'p','password': aConfig.password:=aValue;
    'P','port': aConfig.port:=StrToIntDef(aValue,0);
    'l','logfile': aData.logfile:=aValue;
    'L','longlogfile': aData.longlogfile:=aValue;
    'o','os': aData.os:=aValue;
    'c','cpu': aData.cpu:=aValue;
    'a','category': aData.category:=aValue;
    'v','version': aData.version:=aValue;
    't','date': aData.date:=ExtractDate(aValue);
    's','submitter': aData.submitter:=aValue;
    'm','machine': aData.machine:=aValue;
    'C','comment': aData.config:=aValue;
    'D','description': aData.description:=aValue;
    'S','testsrcdir': aConfig.testsrcdir:=aValue;
    'r','relsrcdir': aConfig.relsrcdir:=aValue;
    'V','verbose': DoVerbose:=True;
    //  'S','sql': aConfig.sql:=aValue;
    'compilerdate': aData.CompilerDate:=aValue;
    'compilerfullversion': aData.CompilerFullVersion:=aValue;
    'svncompilerrevision': aData.CompilerRevision:=aValue;
    'svntestsrevision': aData.TestsRevision:=aValue;
    'svnrtlrevision': aData.RTLRevision:=aValue;
    'svnpackagesrevision' : aData.PackagesRevision:=aValue;
  else
    Verbose(V_ERROR,'Unknown processing option: '+aOption);
  end;
end;

Procedure TDBDigestApplication.ProcessConfigfile(const aFileName : String; var aConfig : TDigestConfig; var aData : TTestRunData);

Var
  Cfg : TStrings;
  aLine,S,N,V : String;
  I : Integer;

begin
  // Set the default value for old digests without RelSrcDir to the rtl/compiler
  // testsuite
  If Not FileExists(aFileName) Then
    Exit;
  Verbose(V_DEBUG,'Parsing config file: '+aFileName);
  Cfg:=TStringList.Create;
  try
    Cfg.LoadFromFile(aFileName);
    For aLine in Cfg do
      begin
      S:=Trim(aLine);
      I:=Pos('#',S);
      If I<>0 then
        S:=Copy(S,1,I-1);
      If (S<>'') then
        begin
        I:=Pos('=',S);
        if (I=0) then
          Verbose(V_ERROR,'Unknown processing option: '+S)
        else
          begin
          N:=LowerCase(Copy(S,1,I-1));
          V:=Copy(S,I+1,Length(S)-I);
          ProcessOption(N,V,aConfig,aData);
          end;
        end;
      end;

  finally
    Cfg.Free;
  end;
end;

{ TDBDigestApplication }

Procedure TDBDigestApplication.Usage(const aMsg : String);

begin
  if (aMsg<>'') then
    Writeln('Error : ',aMsg);
  Writeln('Usage: ',ExeName,' [options] [test run data options]');
  Writeln('Configuration options:');
  Writeln('-H --help                         show this help');
  Writeln('-d --databasename=NAME            database name');
  Writeln('-f --config=FILENAME              config file. If not set, dbdigest.cfg is used.');
  Writeln('-h --host=HOST                    database hostname');
  Writeln('-p --password=PWD                 database user password');
  Writeln('-P --port=NNN                     database connection port');
  Writeln('-r --relsrcdir                    relative source dir');
  Writeln('-S --testsrcdir                   test source dir');
  Writeln('-u --username=USER                database user name');
  Writeln('-V --verbose                      be more verbose');
  Writeln('Test run data:');
  Writeln('-l --logfile=FILE                 set log file to analyse');
  Writeln('-L --longlogfile=FILE             set long log filename (logs of run tests)');
  Writeln('-o --os=OS                        set OS for testrun');
  Writeln('-c --cpu=CPU                      set CPU');
  Writeln('-a --category=CAT                 set category');
  Writeln('-v --version=VER                  set compiler version');
  Writeln('-t --date=DATE                    date in YYYMMDD(hhmmnn) format');
  Writeln('-s --submitter=NAME               submitter name');
  Writeln('-m --machine=NAME                 set machine name on which testsuite was run');
  Writeln('-C --compile-flags=FLAGS          set used compilation flags');
  Writeln('   --comment=FLAGS                backwards compatible way to set compilation flags (deprecated)');
  Writeln('-D --description=DESC             set config description (helpful comment)');
  Writeln('   --compilerdate=DATE            set compiler date');
  Writeln('   --compilerfullversion=VERSION  set full compiler version');
  Writeln('   --svncompilerrevision=REV      set revision of used compiler');
  Writeln('   --svntestsrevision=REV         set revision of testsuite files');
  Writeln('   --svnrtlrevision=REV           set revision of RTL');
  Writeln('   --svnpackagesrevision=REV      set revison of packages');
  Writeln('');
  Writeln('The config file can contain the same options as the command-line in the form.');
  Writeln('option=value');
  Writeln('where option is the long or short version of the option');
  Writeln('comments may be included using the # character.');
  ExitCode:=Ord(aMsg<>'');
end;

function TDBDigestApplication.ProcessCommandLine(var aConfig: TDigestConfig; var aData : TTestRunData): Boolean;

  Function MakeOpts(s : string) : string;
  var
    C : char;
  begin
    Result:='';
    For C in s do
      begin
      Result:=Result+C;
      if not (C in ['V','Q']) then
        Result:=Result+':';
      end;
  end;

  Function MakeLongOpts(s : array of string) : TStringDynArray;
  var
    I : Integer;
  begin
    Result:=['help'];
    SetLength(Result,1+Length(S));
    For I:=0 to Length(S)-1 do
      Result[1+i]:=S[I]+':'
  end;

var
  Long,ErrMsg,lValue : String;
  Short : Char;
  I : integer;
  lHas : boolean;

begin
  ErrMsg:=CheckOptions(MakeOpts(ShortOpts)+'H',MakeLongOpts(LongOpts));
  Result:=(ErrMsg='');
  if (not Result) or HasOption('H','help') then
    begin
    Usage(ErrMsg);
    Exit(false);
    end;
  I:=0;
  For Long in LongOpts do
    begin
    Inc(I);
    if I<=Length(ShortOpts) then
      begin
      Short:=ShortOpts[I];
      if Short='r' then
        Writeln('ag');
      lHas:=HasOption(Short,Long);
      lValue:=GetOptionValue(Short,Long);
      end
    else
      begin
      Short:=#0;
      lHas:=HasOption(Long);
      lValue:=GetOptionValue(Long);
      end;
    if lHas then
      ProcessOption(long,lValue,aConfig,aData);
    end;
  Result:=True;
end;

procedure TDBDigestApplication.Analyze(const aConfig : TDigestConfig; const aData : TTestRunData);

var
  lSQL : TTestSQL;
  lDigest : TDBDigestAnalyzer;

begin
  lDigest:=Nil;
  With aConfig do
    lSQL:=TTestSQL.create(databasename,host,username,password,port);
  try
    lSQL.ConnectToDatabase;
    lDigest:=TDBDigestAnalyzer.Create(lSQL);
    lDigest.Analyse(aConfig,aData);
  finally
    lDigest.Free;
    lSQL.Free;
  end;
end;

procedure TDBDigestApplication.ReadSystemDBConfig(var aConfig : TDigestConfig);

// Keep filename in sync with algorithm in dbadd

var
  lFileName : String;
  Ini : TCustomIniFile;

begin
  lFileName:='/etc/dbdigest.ini';
  if not FileExists(lFileName) then exit;
  Ini:=TMemIniFile.Create(lFileName);
  With Ini do
    try
      aConfig.DatabaseName:=ReadString(SSection,KeyName,'testsuite');
      aConfig.Host:=ReadString(SSection,KeyHost,'localhost');
      aConfig.UserName:=ReadString(SSection,KeyUser,'');
      aConfig.Password:=ReadString(SSection,KeyPassword,'');
      aConfig.Port:=ReadInteger(SSection,KeyPort,0);
    finally
      Ini.Free;
    end;
end;

procedure TDBDigestApplication.DoRun;

var
  lConfigFile : String;
  lConfig : TDigestConfig;
  lData : TTestRunData;
begin
  Terminate;
  lConfigFile:=GetOptionValue('f','config');
  if lConfigFile='' then
    lConfigFile:='dbdigest.cfg';
  lData:=Default(TTestRunData);
  lConfig:=Default(TDigestConfig);
  lConfig.RelSrcDir:='tests/';
  ReadSystemDBConfig(lConfig);
  ProcessConfigFile(lConfigFile,lConfig,lData);
  if ProcessCommandLine(lConfig,lData) then
    Analyze(lConfig,lData);
end;

var
  Application : TDBDigestApplication;

begin
  Application:=TDBDigestApplication.Create(Nil);
  Application.Initialize;
  Application.Run;
  Application.Free;
end.
