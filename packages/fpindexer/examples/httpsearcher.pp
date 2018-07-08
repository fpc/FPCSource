unit httpsearcher;

// You can remove the support you do not need.
{$DEFINE USEFIREBIRD}
{$DEFINE USESQLITE}
{$DEFINE USEPOSTGRES}
{$mode objfpc}{$H+}

{$IFDEF USEFIREBIRD}
{$DEFINE USESQLDB}
{$ENDIF}
{$IFDEF USEPOSTGRES}
{$DEFINE USESQLDB}
{$ENDIF}

interface

uses
  Classes, SysUtils, DateUtils, sqldb,
{$IFDEF USESQLDB}
  SQLDBindexDB,
{$ENDIF}
{$IFDEF USEFIREBIRD}
  FBindexDB, // Firebird support
{$ENDIF}
{$IFDEF USESQLITE}
  sqliteindexdb, // sqlite 3 support
{$ENDIF}
{$IFDEF USEPOSTGRES}
  pgindexdb, // Postgres support
{$ENDIF}
  memindexdb, // Custom Memory file. Always enabled
  fpIndexer, inifiles, httpdefs, fpjson;

Type
  { THTTPSearcher }

  THTTPSearcher = Class(TComponent)
  private
    FAllowCors: Boolean;
    FDB : TCustomIndexDB;
    FSearch : TFPSearch;
    FDefaultMinRank : Integer;
    FMinRank : Integer;
    FFormattedJSON : Boolean;
    FDefaultMetadata,
    FIncludeMetaData : Boolean;
    FDefaultAvailable : TAvailableMatch;
    FMetadata : TJSONObject;
    FWordsMetadata : TJSONObject;
    procedure ConfigSearch(aRequest: TRequest; aResponse: TResponse);
    procedure ConfigWordList(aRequest: TRequest; out aContaining : UTF8string; Out Partial : TAvailableMatch; Out aSimple : Boolean);
    function SearchDataToJSON(aID: Integer; const aRes: TSearchWordData): TJSONObject;
    procedure SendJSON(J: TJSONObject; aResponse: TResponse);
    procedure SetupMetadata;
  Protected
    function InitSearch(aResponse: TResponse): Boolean;
    function SetupDB(aIni: TCustomIniFile): TCustomIndexDB;
    Property DB : TCustomIndexDB Read FDB;
    Property Search : TFPSearch Read FSearch;
    Property MinRank : Integer Read FMinRank;
    Property FormattedJSON : Boolean Read FFormattedJSON;
    Property AllowCors : Boolean Read FAllowCors;
  Public
    Function CheckParams(aRequest : TRequest; aResponse : TResponse) : Boolean;
    Function CheckSearchParams(aRequest : TRequest; aResponse : TResponse) : Boolean;
    Procedure HTMLSearch(aRequest : TRequest; aResponse : TResponse);
    Procedure WordList(aRequest : TRequest; aResponse : TResponse);
  end;


implementation

function THTTPSearcher.SetupDB(aini :TCustomIniFile) : TCustomIndexDB;

Const
  SDatabase = 'Database';
  KeyType = 'Type';
  KeyDatabaseName = 'DatabaseName';
  {$IFDEF USESQLDB}
  KeyHostName = 'HostName';
  KeyUser = 'User';
  KeyPassword = 'Password';
  {$ENDIF}

{$IFDEF USESQLDB}
  Procedure ConfigSQLDB(DB : TSQLDBIndexDB);

  begin
    DB.HostName:= aIni.ReadString(SDatabase,KeyHostName,DB.HostName);
    DB.DatabasePath := aIni.ReadString(SDatabase,KeyDatabaseName,DB.DatabasePath);
    DB.UserName := aIni.ReadString(SDatabase,KeyUser,DB.UserName);
    DB.Password := aIni.ReadString(SDatabase,KeyPassword,DB.Password);
  end;
{$ENDIF USESQLDB}

{$IFDEF USESQLLITE}
  Procedure ConfigSQLIte(SDB : TSQLiteIndexDB);

  begin
    SDB.FileName := aIni.ReadString(SDatabase,KeyDatabaseName,SDB.FileName);
  end;
{$ENDIF}

  Procedure ConfigFile(FDB : TFileIndexDB);

  begin
    FDB.FileName := aIni.ReadString(SDatabase,KeyDatabaseName,FDB.FileName);
  end;

Var
  {$IFDEF USESQLDB}
  QDB : TSQLDBIndexDB;
  {$ENDIF}
  {$IFDEF USESQLLITE}
  SDB : TSQLiteIndexDB;
  {$ENDIF}
  MDB :  TFileIndexDB;
  aType : String;

begin
  Result:=nil;
  aType:=aIni.ReadString(SDatabase,KeyType,'PostGres');
  Case lowercase(aType) of
{$IFDEF USEPOSTGRES}
    'postgres' :
        begin
        QDB := TPGIndexDB.Create(nil);
        ConfigSQLDB(QDB);
        Result:=QDB;
        end;
{$ENDIF}
{$IFDEF USEFIREBIRD}
    'firebird' :
        begin
        QDB := TFBIndexDB.Create(nil);
        ConfigSQLDB(QDB);
        Result:=QDB;
        end;
{$ENDIF}
{$IFDEF USESQLITE}
    'sqlite' :
        begin
        SDB := TSQLiteIndexDB.Create(nil);
        ConfigSQLite(SDB);
        Result:=SDB;
        end;
{$ENDIF}
    'file' :
        begin
        MDB := TFileIndexDB.Create(nil);
        ConfigFile(MDB);
        Result:=MDB;
        end;
  else
    Raise Exception.CreateFmt('Unknown database type: "%s" ',[aType]);
  end;
end;

function THTTPSearcher.CheckParams(aRequest: TRequest; aResponse: TResponse): Boolean;

Var
  S : String;
  B : Boolean;

begin
  S:=aRequest.QueryFields.Values['q'];
  Result:=S<>'';
  if not Result then
    begin
    aResponse.Code:=400;
    aResponse.CodeText:='Missing q param';
    aResponse.SendResponse;
    end;
  S:=aRequest.QueryFields.Values['r'];
  Result:=(S='') or (StrToIntDef(S,-1)<>-1);
  if not Result then
    begin
    aResponse.Code:=400;
    aResponse.CodeText:='Wrong value for r';
    aResponse.SendResponse;
    end;
  S:=aRequest.QueryFields.Values['c'];
  Result:=(S='') or TryStrToBool(S,B);
  if not Result then
    begin
    aResponse.Code:=400;
    aResponse.CodeText:='Wrong value for c';
    aResponse.SendResponse;
    end;
  S:=aRequest.QueryFields.Values['m'];
  Result:=(S='') or TryStrToBool(S,B);
  if not Result then
    begin
    aResponse.Code:=400;
    aResponse.CodeText:='Wrong value for m';
    aResponse.SendResponse;
    end;
end;

function THTTPSearcher.CheckSearchParams(aRequest: TRequest; aResponse: TResponse): Boolean;

Var
  m,S : String;
  B : Boolean;

begin
  S:=aRequest.QueryFields.Values['q'];
  M:=aRequest.QueryFields.Values['t'];
  Result:=(M='');
  if not Result then
    case lowercase(M) of
      'all' :
        if S<>'' then
          begin
          aResponse.Code:=400;
          aResponse.CodeText:='Q must be empty';
          aResponse.SendResponse;
          end;
      'contains',
      'exact',
      'startswith' :
        if S='' then
          begin
          aResponse.Code:=400;
          aResponse.CodeText:='Q may not be empty';
          aResponse.SendResponse;
          end;
    else
      aResponse.Code:=400;
      aResponse.CodeText:='Wrong value for t';
      aResponse.SendResponse;
    end;
  S:=aRequest.QueryFields.Values['s'];
  Result:=(S='') or TryStrToBool(S,B);
  if not Result then
    begin
    aResponse.Code:=400;
    aResponse.CodeText:='Wrong value for s';
    aResponse.SendResponse;
    end;
  if not B then
    begin
    S:=aRequest.QueryFields.Values['m'];
    Result:=(S='') or TryStrToBool(S,B);
    if not Result then
      begin
      aResponse.Code:=400;
      aResponse.CodeText:='Wrong value for m';
      aResponse.SendResponse;
      end;
    end;
end;

Procedure THTTPSearcher.SetupMetadata;

begin
  FMetadata:=TJSONObject.Create([
    'root', 'data',
    'idField','id',
    'fields',TJSONArray.Create([
      TJSONObject.Create(['name','id','type','int']),
      TJSONObject.Create(['name','rank','type','int']),
      TJSONObject.Create(['name','url','type','string','maxlen',100]),
      TJSONObject.Create(['name','context','type','string','maxlen',MaxContextLen]),
      TJSONObject.Create(['name','date','type','date'])
     ])
  ]);
  FWordsMetadata:=TJSONObject.Create([
    'root', 'data',
    'idField','id',
    'fields',TJSONArray.Create([
      TJSONObject.Create(['name','id','type','int']),
      TJSONObject.Create(['name','word','type','string','maxlen',100])
     ])
  ]);
end;

Function THTTPSearcher.InitSearch(aResponse : TResponse): Boolean;

Const
  BaseName ='docsearch.ini';

  Function TestCfg(aDir : string) : String;

  begin
    Result:=aDir+BaseName;
    if not FileExists(Result) then
      Result:='';
  end;

Var
  CFN : String;
  aIni: TMemIniFile;

begin
  Result:=False;
  if FDB<>Nil then
    exit(True);
  try
    CFN:=TestCfg(GetAppConfigDir(true));
    if (CFN='') then
      CFN:=TestCfg(GetAppConfigDir(False));
    if (CFN='') then
      CFN:=TestCfg('config/');
    if (CFN='') then
      CFN:=TestCfg(ExtractFilePath(ParamStr(0)));
    if (CFN='') then
      CFN:=TestCfg('');
    if (CFN='') then
      Raise Exception.Create('No config file found');
    aIni:=TMemIniFile.Create(CFN);
    try
      FDB:=SetupDB(aIni);
      FFormattedJSON:=aIni.ReadBool('search','formatjson',False);
      FDefaultMinRank:=aIni.ReadInteger('search','minrank',1);
      FDefaultMetadata:=aIni.ReadBool('search','metadata',true);
      FAllowCors:=aIni.ReadBool('search','allowcors',true);
    finally
      aIni.Free;
    end;
    SetupMetadata;
    FSearch:=TFPSearch.Create(Self);
    FSearch.Database:=FDB;
    Result:=True;
  except
    On E : Exception do
      begin
      aResponse.Code:=500;
      aResponse.CodeText:='Could not set up search: '+E.Message;
      aResponse.SendResponse;
      end;
  end;
end;

Procedure THTTPSearcher.ConfigSearch(aRequest : TRequest; aResponse : TResponse);

Var
  S : string;
  O : TSearchOptions;
  B : Boolean;

begin
  FMinRank:=StrToIntDef(aRequest.QueryFields.Values['r'],0);
  if FMinRank=0 then
    FMinRank:=FDefaultMinRank;
  S:=aRequest.QueryFields.Values['m'];
  if (S='') or Not TryStrToBool(S,FIncludeMetaData)  then
    FIncludeMetaData:=FDefaultMetaData;
  FSearch.SetSearchWord(aRequest.QueryFields.Values['q']);
  O:=[];
  S:=aRequest.QueryFields.Values['c'];
  if (S<>'') and TryStrToBool(S,B) and B then
    Include(O,soContains);
  FSearch.Options:=O;
end;

procedure THTTPSearcher.ConfigWordList(aRequest: TRequest; out aContaining: UTF8string; out Partial: TAvailableMatch; out aSimple: Boolean);

Var
  m,S : String;

begin
  aContaining:=aRequest.QueryFields.Values['q'];
  M:=aRequest.QueryFields.Values['t'];
  case lowercase(M) of
    'all' : Partial:=amAll;
    'contains' : Partial:=amContains;
    'exact' : Partial:=amExact;
    'startswith' : Partial:=amStartsWith;
  else
    Partial:=FDefaultAvailable;
    if (Partial<>amAll) and (aContaining='') then
      Partial:=amAll;
  end;
  S:=aRequest.QueryFields.Values['s'];
  if (S='') then
    aSimple:=False
  else
    aSimple:=StrToBool(S);
  if ASimple then
    FIncludeMetadata:=False
  else
    begin
    FIncludeMetaData:=FDefaultMetaData;
    S:=aRequest.QueryFields.Values['m'];
    if (S<>'') then
      TryStrToBool(S,FIncludeMetaData);
    end
end;

Function THTTPSearcher.SearchDataToJSON(aID : Integer;const aRes : TSearchWordData) : TJSONObject;

begin
  Result:=TJSONObject.Create([
    'id',aID,
    'rank',aRes.Rank,
    'url',aRes.URL,
    'context',ares.Context,
    'date',FormatDateTime('yyyy"-"mm"-"dd"T"hh":"nn":"ss',aRes.FileDate)
  ]);
end;

procedure THTTPSearcher.HTMLSearch(aRequest: TRequest; aResponse: TResponse);

Var
  I : Integer;
  J : TJSONObject;
  A : TJSONArray;

begin
  aResponse.ContentType:='application/json';
  if AllowCORS then
    AResponse.SetCustomHeader('Access-Control-Allow-Origin','*');
  if not CheckParams(aRequest,aResponse) then
    exit;
  if not InitSearch(aResponse) then
    exit;
  ConfigSearch(aRequest,aResponse);
  FSearch.Execute;
  A:=nil;
  J:=TJSONObject.Create;
  try
    if FIncludeMetadata then
      J.Add('metaData',FMetadata.Clone);
    A:=TJSONArray.Create;
    For I:=0 to Search.RankedCount-1 do
      begin
      if Search.RankedResults[I].Rank>=MinRank then
        A.Add(SearchDataToJSON(I+1,Search.RankedResults[I]));
      end;
    J.Add('data',A);
    SendJSON(J,aResponse);
  finally
    J.Free;
  end;
end;

procedure THTTPSearcher.SendJSON(J : TJSONObject; aResponse: TResponse);

begin
  if FormattedJSON then
    aResponse.Content:=J.FormatJSON()
  else
    aResponse.Content:=J.AsJSON;
  aResponse.ContentLength:=Length(aResponse.Content);
  aResponse.SendContent;
end;

procedure THTTPSearcher.WordList(aRequest: TRequest; aResponse: TResponse);
Var
  I : Integer;
  J : TJSONObject;
  A : TJSONArray;
  w,aContaining : UTF8String;
  aPartial : TAvailableMatch;
  aSimple : Boolean;
  aList : TUTF8StringArray;


begin
  aResponse.ContentType:='application/json';
  if AllowCORS then
    AResponse.SetCustomHeader('Access-Control-Allow-Origin','*');
  if not CheckSearchParams(aRequest,aResponse) then
    exit;
  if not InitSearch(aResponse) then
    exit;
  ConfigWordList(aRequest,aContaining,aPartial,aSimple);
  FSearch.GetAvailableWords(aList,aContaining,aPartial);
  J:=TJSONObject.Create;
  try
    if FIncludeMetadata then
      J.Add('metaData',FWordsMetadata.Clone);
    A:=TJSONArray.Create;
    if aSimple then
      For W in aList do
        A.Add(W)
      else
        begin
        For I:=0 to Length(aList)-1 do
          A.Add(TJSONObject.Create(['id',I+1,'word',aList[i]]));
        end;
    J.Add('data',A);
    SendJSON(J,aResponse);
  finally
    J.Free;
  end;
end;


end.

