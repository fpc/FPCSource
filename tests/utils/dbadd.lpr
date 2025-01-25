program dbadd;

{$mode objfpc}
{$H+}
{$modeswitch typehelpers}

uses
  Classes, SysUtils, CustApp, tsdb, tsutils, inifiles;

type
  TValueType = (vtCategory,vtCPU,vtOS,vtVersion);

  { TDigestValueApplication }

  TDigestValueApplication = class(TCustomApplication)
  private
    procedure ConnectToDB;
    function ProcessParams: boolean;
  protected
    FDB : TTestSQL;
    FDatabaseName : String;
    FHostName : String;
    FUserName : string;
    FPassword : string;
    FPort : Word;
    FType : TValueType;
    FValue : String;
    FDate : TDateTime;
    procedure DoRun; override;
    function ReadConfig(const aFileName: String): boolean;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Usage(const aMsg : String);
  end;

const
  ValueNames : Array[TValueType] of string = ('Category','CPU','OS','Version');

{ TDigestValueApplication }

function TDigestValueApplication.ProcessParams : boolean;

  function GetDate(S : String) : TDateTime;
  var
    Y,M,d : Integer;
  begin
    if (s='') then
      exit(Date);
    Y:=StrToIntDef(Copy(S,1,4),0);
    M:=StrToIntDef(Copy(S,5,2),0);
    D:=StrToIntDef(Copy(S,7,2),0);
    if (Y>0) and (M>0) and (D>0) then
      if not TryEncodeDate(Y,M,D,Result) then
        Result:=0
  end;

begin
  if not ReadConfig(GetOptionValue('c','config')) then
    Exit(False);
  Case LowerCase(GetOptionValue('t','type')) of
  'category' : FType:=vtCategory;
  'cpu'      : FType:=vtCPU;
  'os'       : FType:=vtOS;
  'version'  : FType:=vtVersion;
  else
    Usage('Unknown value type : '+GetOptionValue('t','type'));
    Exit(False);
  end;
  FValue:=GetOptionValue('v','value');
  FDate:=GetDate(GetOptionValue('d','date'));
  if FDate=0 then
    begin
    Usage('Invalid date : '+GetOptionValue('d','date'));
    Exit(False);
    end;
  if FValue='' then
    begin
    Usage('Empty value is not allowed');
    Exit(False);
    end;
  Result:=True;
end;

procedure TDigestValueApplication.ConnectToDB;

begin
  FDB:=TTestSQL.create(FDatabaseName,FHostName,FUserName,FPassword,FPort);
  if not FDB.ConnectToDatabase then
    Writeln('Failed to connect to database');
end;

procedure TDigestValueApplication.DoRun;

var
  ErrorMsg: String;
  lID : Integer;
begin
  Terminate;
  // quick check parameters
  ErrorMsg:=CheckOptions('hc:t:v:d:', ['help','type:','value:','date:','config:']);
  if (ErrorMsg<>'') or HasOption('h', 'help') then
    begin
    Usage(ErrorMsg);
    Exit;
    end;
  if not ProcessParams then
    exit;
  ConnectToDB;
  Case FType of
    vtCPU : lID:=FDB.AddCPU(FValue);
    vtOS : lID:=FDB.AddOS(FValue);
    vtCategory : lID:=FDB.AddCategory(FValue);
    vtVersion: lID:=FDB.AddVersion(FValue,FDate);
  end;
  Writeln('Inserted ',ValueNames[FType],' "',FValue,'" with ID: ',lID);
end;

function TDigestValueApplication.ReadConfig(const aFileName: String) : boolean;

var
  lFileName : string;
  Ini : TCustomIniFile;

begin
  lFilename:=aFileName;
  if lFileName='' then
    lFileName:='/etc/dbdigest.ini';
  Ini:=TMemIniFile.Create(lFileName);
  With Ini do
    try
      FDatabaseName:=ReadString(SSection,KeyName,'testsuite');
      FHostName:=ReadString(SSection,KeyHost,'localhost');
      FUserName:=ReadString(SSection,KeyUser,'');
      FPassword:=ReadString(SSection,KeyPassword,'');
      FPort:=ReadInteger(SSection,KeyPort,0);
    finally
      Free;
    end;
  Result:=False;
  if FDatabaseName='' then
    Usage('Database name not set')
  else if (FHostName='') then
    Usage('Database hostname not set')
  else if (FUserName='') then
    Usage('Database username not set')
  else if (FPassword='') then
    Usage('Database user password not set')
  else
    Result:=True;
end;

constructor TDigestValueApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TDigestValueApplication.Destroy;
begin
  FreeAndNil(FDB);
  inherited Destroy;
end;

procedure TDigestValueApplication.Usage(const aMsg: String);
begin
  if aMsg<>'' then
    Writeln('Error: ',aMsg);
  writeln('Usage: ', ExeName, '[options]');
  Writeln('Where options is one or more of:');
  Writeln('-d --date=YYYYMMDD   Date for version. If omitted, today is used');
  Writeln('-c --config=FILE     Config file with database connection info (.ini file).');
  Writeln('-h --help            This message');
  Writeln('-t --type=ATYPE      Type of value to insert in db. This is one of:');
  Writeln('                       category');
  Writeln('                       cpu');
  Writeln('                       os');
  Writeln('                       version');
  Writeln('-v --value=VALUE     Value to insert in database');
  Writeln('');
  Writeln('If -h is not specified, options -c -t -v are required.');
  Writeln('Config file is an .ini file with the following keys:');
  Writeln('[Database]');
  Writeln('Name=name');
  Writeln('Host=hostname');
  Writeln('UserName=user');
  Writeln('Password=pwd');
  Writeln('Port=1234');
  ExitCode:=Ord(aMsg<>'');
end;

var
  Application: TDigestValueApplication;
begin
  Application:=TDigestValueApplication.Create(nil);
  Application.Title:='Digest add value Application';
  Application.Run;
  Application.Free;
end.

