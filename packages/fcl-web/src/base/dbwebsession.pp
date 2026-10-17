{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by the Free Pascal development team

    Web session that stores session data in a database using SQLDB.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$IFNDEF FPC_DOTTEDUNITS}
unit DBWebSession;
{$ENDIF FPC_DOTTEDUNITS}

{$mode objfpc}{$H+}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Classes, System.SysUtils, Data.Db, Data.Sqldb, FpJson.Data, FpWeb.Http.Base, FpWeb.Http.Defs;
{$ELSE FPC_DOTTEDUNITS}
uses
  Classes, SysUtils, db, sqldb, fpjson, fphttp, httpdefs;
{$ENDIF FPC_DOTTEDUNITS}

{
  Database layout.

  CreateTables creates the tables and indexes needed for the current StorageType. 
  GetCreateTablesSQL returns the statements without executing them.

  All storage types use a session table (SessionTableName) with fields:
    SessionIDFieldName : string, at least 36 characters, primary key.
    ExpiresFieldName   : timestamp. Time at which the session expires.
    DataFieldName      : text/blob. Only for stJSONBlob and stIniBlob.
    VersionFieldName   : string, at least 36 characters. Only for stJSONBlob and
                         stIniBlob, and only if VersionFieldName is not empty.

  stKeyValue additionally uses a values table (ValuesTableName) with fields:
    SessionIDFieldName : string, at least 36 characters.
    KeyFieldName       : string, variable name.
    ValueFieldName     : string/text, variable value.
  with a unique index on (SessionIDFieldName,KeyFieldName) and a foreign key
  on SessionIDFieldName to the session table.

  stJSONBlob stores the variables as a JSON object in the data field.
  stIniBlob stores the variables in the [Data] section of an .ini file in the data field.

  Concurrent requests for the same session (blob storage types only):

  Every write of the data field stores a new version ID (a GUID) in the version field. 

  It only succeeds if the version field still contains the version that was read. 

  If another request wrote the session in between, the data in the database is merged into the current data: 
  variables that exist in the database but not in the current data are added, 
  unless they were removed in the current request. 

  Then the write is retried, at most MaxWriteAttempts times. 
  If the session no longer exists in the database (terminated or expired by another request), 
  the changes are discarded and  the session is treated as terminated.
}

Type
  TDBSessionStorageType = (stKeyValue, stJSONBlob, stIniBlob);
  // Columns, used to determine column types when creating tables.
  TDBSessionColumn = (scSessionID, scExpires, scData, scVersion, scKey, scValue);
  // Database objects created by CreateTables, used to name them.
  TDBSessionDBObject = (soSessionPK, soExpiresIndex, soValuesUniqueIndex, soValuesFK);
  // Database dialect, used to determine column types.
  TDBSessionDialect = (sdUnknown, sdPostgreSQL, sdSQLite, sdFirebird, sdMSSQL, sdMySQL, sdOracle);

  TDBSessionFactory = Class;

  { TDBWebSession }

  TDBWebSession = Class(TCustomSession)
  Private
    FFactory : TDBSessionFactory;
    FCached: Boolean;
    FData : TJSONObject;
    FDataDirty : Boolean;
    FExpiresDirty : Boolean;
    FNewRow : Boolean;
    FStoredTimeOut : Integer;
    FVersion : String;
    FDirtyKeys : TStrings;
    FDeletedKeys : TStrings;
    FTerminated : Boolean;
    SID : String;
    procedure ClearChanges;
    procedure MarkChanged(const aName: String; aDeleted: Boolean);
  Protected
    Procedure FreeData;
    Procedure CheckSession;
    // Write pending changes to the database.
    Procedure Flush; virtual;
    Function GetSessionID : String; override;
    Function GetSessionVariable(const VarName : String) : String; override;
    procedure SetSessionVariable(const VarName : String; const AValue: String); override;
    Property Factory : TDBSessionFactory Read FFactory Write FFactory;
    Property Cached : Boolean Read FCached Write FCached;
    Property Data : TJSONObject Read FData;
  Public
    Destructor Destroy; override;
    Procedure Terminate; override;
    procedure RegenerateSessionID(aResponse : TResponse); override;
    function SessionVariableExists(const VarName: String): Boolean; override;
    Procedure UpdateResponse(AResponse : TResponse); override;
    Procedure InitSession(ARequest : TRequest; OnNewSession, OnExpired: TNotifyEvent); override;
    Procedure InitResponse(AResponse : TResponse); override;
    Procedure RemoveVariable(const VariableName : String); override;
  end;
  TDBWebSessionClass = Class of TDBWebSession;

  { TDBSessionFactory }

  TDBSessionFactory = Class(TSessionFactory)
  private
    FCached: Boolean;
    FConnection: TSQLConnection;
    FTransaction: TSQLTransaction;
    FOwnTransaction : TSQLTransaction;
    FStorageType: TDBSessionStorageType;
    FSessionTableName: String;
    FValuesTableName: String;
    FSessionIDFieldName: String;
    FExpiresFieldName: String;
    FDataFieldName: String;
    FKeyFieldName: String;
    FValueFieldName: String;
    FVersionFieldName: String;
    FMaxWriteAttempts: Integer;
    FLock : TRTLCriticalSection;
    FStartedTransaction : Boolean;
    procedure SetConnection(const AValue: TSQLConnection);
    procedure SetTransaction(const AValue: TSQLTransaction);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    // Start/end a series of database operations. Serializes access to the connection.
    procedure StartWork; virtual;
    procedure EndWork(aSuccess : Boolean); virtual;
    function GetTransaction : TSQLTransaction;
    function CreateQuery(const aSQL : String) : TSQLQuery; virtual;
    procedure ExecSQL(const aSQL : String; const aNames : Array of String; const aValues : Array of String); virtual;
    function Expiry(aSession : TCustomSession) : TDateTime;
    // Is the version field used to detect concurrent writes ?
    function UseVersion : Boolean;
    function NewVersion : String; virtual;
    // Determine the dialect from the connection.
    function GetDialect : TDBSessionDialect; virtual;
    // Column type used in CREATE TABLE statements.
    function GetColumnType(aColumn : TDBSessionColumn) : String; virtual;
    // Name of index or constraint created by CreateTables.
    function GetDBObjectName(aObject : TDBSessionDBObject) : String; virtual;
    // Load session. Returns False if the session does not exist.
    function LoadSession(const aID: String; aData: TJSONObject; out aExpires: TDateTime; out aVersion : String): Boolean; virtual;
    // Add variables of aDBData that are not in the session data and were not removed in this session.
    procedure MergeData(aSession : TDBWebSession; aDBData : TJSONObject); virtual;
    // Returns False if the session no longer exists in the database.
    function StoreSession(aSession : TDBWebSession; aInsert : Boolean) : Boolean; virtual;
    procedure StoreValues(aSession : TDBWebSession); virtual;
    procedure DeleteSession(const aID : String); virtual;
    procedure RenameSession(const aOldID, aNewID : String); virtual;
    // Serialization of variables for blob storage types.
    function DataToString(aData : TJSONObject) : String; virtual;
    procedure StringToData(const aValue : String; aData : TJSONObject); virtual;
    Function DoCreateSession(ARequest : TRequest) : TCustomSession; override;
    // Delete expired sessions from the database.
    procedure DoCleanupSessions; override;
    Procedure DoDoneSession(Var ASession : TCustomSession); override;
  Public
    constructor Create(aOwner : TComponent); override;
    destructor Destroy; override;
    // Add the statements needed to create the tables and indexes for the current StorageType to aList.
    procedure GetCreateTablesSQL(aList : TStrings); virtual;
    // Create the tables and indexes for the current StorageType.
    procedure CreateTables; virtual;
    // Connection to use. Must be set by the user.
    // Access to it is serialized, but it should not be used by other threads at the same time.
    Property Connection : TSQLConnection Read FConnection Write SetConnection;
    // Transaction to use. If not set, an internal transaction is created.
    // If the transaction is already active, it is not committed by the factory.
    Property Transaction : TSQLTransaction Read FTransaction Write SetTransaction;
    // How are session variables stored.
    Property StorageType : TDBSessionStorageType Read FStorageType Write FStorageType;
    // If True, changes are written once, when the response is sent.
    // If False, every change is written immediately.
    Property Cached : Boolean Read FCached Write FCached;
    Property SessionTableName : String Read FSessionTableName Write FSessionTableName;
    Property ValuesTableName : String Read FValuesTableName Write FValuesTableName;
    Property SessionIDFieldName : String Read FSessionIDFieldName Write FSessionIDFieldName;
    Property ExpiresFieldName : String Read FExpiresFieldName Write FExpiresFieldName;
    Property DataFieldName : String Read FDataFieldName Write FDataFieldName;
    Property KeyFieldName : String Read FKeyFieldName Write FKeyFieldName;
    Property ValueFieldName : String Read FValueFieldName Write FValueFieldName;
    // Field with the version ID of the session data, used to detect concurrent writes.
    // Only used for blob storage types. Set to empty to disable the check.
    Property VersionFieldName : String Read FVersionFieldName Write FVersionFieldName;
    // Maximum number of attempts to write session data when concurrent writes are detected.
    Property MaxWriteAttempts : Integer Read FMaxWriteAttempts Write FMaxWriteAttempts;
  end;

Var
  DBWebSessionClass : TDBWebSessionClass = Nil;

implementation

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.IniFiles;
{$ELSE FPC_DOTTEDUNITS}
uses
  inifiles;
{$ENDIF FPC_DOTTEDUNITS}

 Const
  // Section in ini blob
  SData = 'Data';

resourcestring
  SErrSessionTerminated = 'No web session active: Session was terminated';
  SErrNoSession         = 'No web session active: Session was not started';
  SErrNoConnection      = 'No database connection set for database session factory';
  SErrConcurrentWrite   = 'Could not write session %s after %d attempts: concurrent updates';

{ TDBSessionFactory }

constructor TDBSessionFactory.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  InitCriticalSection(FLock);
  FStorageType:=stKeyValue;
  FSessionTableName:='FPWebSessions';
  FValuesTableName:='FPWebSessionValues';
  FSessionIDFieldName:='SessionID';
  FExpiresFieldName:='Expires';
  FDataFieldName:='SessionData';
  FKeyFieldName:='VarName';
  FValueFieldName:='VarValue';
  FVersionFieldName:='SessionVersion';
  FMaxWriteAttempts:=5;
end;

destructor TDBSessionFactory.Destroy;
begin
  Connection:=Nil;
  Transaction:=Nil;
  FreeAndNil(FOwnTransaction);
  DoneCriticalSection(FLock);
  inherited Destroy;
end;

procedure TDBSessionFactory.SetConnection(const AValue: TSQLConnection);
begin
  if FConnection=AValue then exit;
  if Assigned(FConnection) then
    FConnection.RemoveFreeNotification(Self);
  FConnection:=AValue;
  if Assigned(FConnection) then
    FConnection.FreeNotification(Self);
  if Assigned(FOwnTransaction) then
    FOwnTransaction.DataBase:=FConnection;
end;

procedure TDBSessionFactory.SetTransaction(const AValue: TSQLTransaction);
begin
  if FTransaction=AValue then exit;
  if Assigned(FTransaction) then
    FTransaction.RemoveFreeNotification(Self);
  FTransaction:=AValue;
  if Assigned(FTransaction) then
    FTransaction.FreeNotification(Self);
end;

procedure TDBSessionFactory.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation=opRemove then
    begin
    if AComponent=FConnection then
      begin
      FConnection:=Nil;
      if Assigned(FOwnTransaction) then
        FOwnTransaction.DataBase:=Nil;
      end
    else if AComponent=FTransaction then
      FTransaction:=Nil;
    end;
end;

function TDBSessionFactory.GetTransaction: TSQLTransaction;
begin
  Result:=FTransaction;
  if Result=Nil then
    begin
    if FOwnTransaction=Nil then
      begin
      FOwnTransaction:=TSQLTransaction.Create(Nil);
      FOwnTransaction.DataBase:=FConnection;
      end;
    Result:=FOwnTransaction;
    end;
end;

procedure TDBSessionFactory.StartWork;

var
  T : TSQLTransaction;

begin
  EnterCriticalSection(FLock);
  try
    if FConnection=Nil then
      Raise EWebSessionError.Create(SErrNoConnection);
    if not FConnection.Connected then
      FConnection.Connected:=True;
    T:=GetTransaction;
    FStartedTransaction:=not T.Active;
    if FStartedTransaction then
      T.StartTransaction;
  except
    LeaveCriticalSection(FLock);
    Raise;
  end;
end;

procedure TDBSessionFactory.EndWork(aSuccess: Boolean);
begin
  try
    if FStartedTransaction then
      if aSuccess then
        GetTransaction.Commit
      else
        GetTransaction.Rollback;
  finally
    FStartedTransaction:=False;
    LeaveCriticalSection(FLock);
  end;
end;

function TDBSessionFactory.CreateQuery(const aSQL: String): TSQLQuery;
begin
  Result:=TSQLQuery.Create(Nil);
  try
    Result.DataBase:=FConnection;
    Result.Transaction:=GetTransaction;
    Result.SQL.Text:=aSQL;
  except
    Result.Free;
    Raise;
  end;
end;

procedure TDBSessionFactory.ExecSQL(const aSQL: String; const aNames: array of String; const aValues: array of String);

var
  Q : TSQLQuery;
  I : Integer;

begin
  Q:=CreateQuery(aSQL);
  try
    For I:=0 to Length(aNames)-1 do
      Q.ParamByName(aNames[I]).AsString:=aValues[I];
    Q.ExecSQL;
  finally
    Q.Free;
  end;
end;

function TDBSessionFactory.Expiry(aSession: TCustomSession): TDateTime;
begin
  Result:=Now+(aSession.TimeOutMinutes/(24*60));
end;

function TDBSessionFactory.UseVersion: Boolean;
begin
  Result:=(StorageType<>stKeyValue) and (VersionFieldName<>'');
end;

function TDBSessionFactory.NewVersion: String;

var
  G : TGUID;

begin
  CreateGUID(G);
  Result:=GUIDToString(G);
  Result:=Copy(Result,2,Length(Result)-2);
end;

function TDBSessionFactory.GetDialect: TDBSessionDialect;

var
  S : String;

begin
  Result:=sdUnknown;
  if FConnection=Nil then
    exit;
  // Not all connections report a server type: fall back to the class name.
  S:=LowerCase(FConnection.GetConnectionInfo(citServerType)+' '+FConnection.ClassName);
  if Pos('postgres',S)>0 then
    Result:=sdPostgreSQL
  else if Pos('sqlite',S)>0 then
    Result:=sdSQLite
  else if (Pos('firebird',S)>0) or (Pos('interbase',S)>0) or (Pos('tibconnection',S)>0) then
    Result:=sdFirebird
  else if (Pos('sql server',S)>0) or (Pos('ase',S)=1) or (Pos('mssql',S)>0) or (Pos('sybase',S)>0) then
    Result:=sdMSSQL
  else if (Pos('mysql',S)>0) or (Pos('mariadb',S)>0) then
    Result:=sdMySQL
  else if Pos('oracle',S)>0 then
    Result:=sdOracle;
end;

function TDBSessionFactory.GetColumnType(aColumn: TDBSessionColumn): String;

var
  D : TDBSessionDialect;

begin
  D:=GetDialect;
  Case aColumn of
    scSessionID,
    scVersion :
      Result:='VARCHAR(40)';
    scKey :
      Result:='VARCHAR(100)';
    scExpires :
      Case D of
        sdSQLite,
        sdMSSQL,
        sdMySQL : Result:='DATETIME';
      else
        Result:='TIMESTAMP';
      end;
    scData :
      Case D of
        sdPostgreSQL,
        sdSQLite : Result:='TEXT';
        sdFirebird : Result:='BLOB SUB_TYPE TEXT';
        sdMSSQL : Result:='VARCHAR(MAX)';
        sdMySQL : Result:='LONGTEXT';
      else
        Result:='CLOB';
      end;
    scValue :
      Case D of
        sdPostgreSQL,
        sdSQLite : Result:='TEXT';
      else
        Result:='VARCHAR(4000)';
      end;
  end;
  if D=sdOracle then
    Result:=StringReplace(Result,'VARCHAR(','VARCHAR2(',[]);
end;

function TDBSessionFactory.GetDBObjectName(aObject: TDBSessionDBObject): String;
begin
  Case aObject of
    soSessionPK : Result:='PK_'+SessionTableName;
    soExpiresIndex : Result:='IDX_'+SessionTableName+'_EXP';
    soValuesUniqueIndex : Result:='UQ_'+ValuesTableName+'_KEY';
    soValuesFK : Result:='FK_'+ValuesTableName+'_SES';
  end;
  // Older Oracle and Firebird versions limit names to 30 characters.
  Result:=Copy(Result,1,30);
end;

procedure TDBSessionFactory.GetCreateTablesSQL(aList: TStrings);

var
  S : String;

begin
  S:='CREATE TABLE '+SessionTableName+' ('
     +SessionIDFieldName+' '+GetColumnType(scSessionID)+' NOT NULL, '
     +ExpiresFieldName+' '+GetColumnType(scExpires)+' NOT NULL';
  if StorageType<>stKeyValue then
    S:=S+', '+DataFieldName+' '+GetColumnType(scData);
  if UseVersion then
    S:=S+', '+VersionFieldName+' '+GetColumnType(scVersion);
  S:=S+', CONSTRAINT '+GetDBObjectName(soSessionPK)+' PRIMARY KEY ('+SessionIDFieldName+'))';
  aList.Add(S);
  aList.Add('CREATE INDEX '+GetDBObjectName(soExpiresIndex)+' ON '+SessionTableName+' ('+ExpiresFieldName+')');
  if StorageType<>stKeyValue then
    exit;
  // The foreign key is part of CREATE TABLE: SQLite cannot add it later.
  aList.Add('CREATE TABLE '+ValuesTableName+' ('
            +SessionIDFieldName+' '+GetColumnType(scSessionID)+' NOT NULL, '
            +KeyFieldName+' '+GetColumnType(scKey)+' NOT NULL, '
            +ValueFieldName+' '+GetColumnType(scValue)+', '
            +'CONSTRAINT '+GetDBObjectName(soValuesFK)+' FOREIGN KEY ('+SessionIDFieldName+') '
            +'REFERENCES '+SessionTableName+' ('+SessionIDFieldName+'))');
  aList.Add('CREATE UNIQUE INDEX '+GetDBObjectName(soValuesUniqueIndex)+' ON '+ValuesTableName
            +' ('+SessionIDFieldName+', '+KeyFieldName+')');
end;

procedure TDBSessionFactory.CreateTables;

var
  L : TStringList;
  S : String;
  OK : Boolean;

begin
  L:=TStringList.Create;
  try
    GetCreateTablesSQL(L);
    // Commit each statement: some databases do not allow using new objects in the same transaction.
    For S in L do
      begin
      OK:=False;
      StartWork;
      try
        ExecSQL(S,[],[]);
        OK:=True;
      finally
        EndWork(OK);
      end;
      end;
  finally
    L.Free;
  end;
end;

function TDBSessionFactory.DataToString(aData: TJSONObject): String;

var
  Ini : TMemIniFile;
  L : TStringList;
  I : Integer;

begin
  if StorageType=stJSONBlob then
    Exit(aData.AsJSON);
  L:=Nil;
  Ini:=TMemIniFile.Create('');
  try
    For I:=0 to aData.Count-1 do
      Ini.WriteString(SData,aData.Names[I],aData.Items[I].AsString);
    L:=TStringList.Create;
    Ini.GetStrings(L);
    Result:=L.Text;
  finally
    L.Free;
    Ini.Free;
  end;
end;

procedure TDBSessionFactory.StringToData(const aValue: String; aData: TJSONObject);

var
  D : TJSONData;
  Ini : TMemIniFile;
  L : TStringList;
  I : Integer;

begin
  aData.Clear;
  if Trim(aValue)='' then
    exit;
  if StorageType=stJSONBlob then
    begin
    D:=GetJSON(aValue);
    try
      if D is TJSONObject then
        For I:=0 to TJSONObject(D).Count-1 do
          aData.Strings[TJSONObject(D).Names[I]]:=TJSONObject(D).Items[I].AsString;
    finally
      D.Free;
    end;
    exit;
    end;
  Ini:=Nil;
  L:=TStringList.Create;
  try
    L.Text:=aValue;
    Ini:=TMemIniFile.Create('');
    Ini.SetStrings(L);
    L.Clear;
    Ini.ReadSection(SData,L);
    For I:=0 to L.Count-1 do
      aData.Strings[L[I]]:=Ini.ReadString(SData,L[I],'');
  finally
    Ini.Free;
    L.Free;
  end;
end;

function TDBSessionFactory.LoadSession(const aID: String; aData: TJSONObject; out aExpires: TDateTime; out aVersion: String): Boolean;

var
  Q : TSQLQuery;
  S : String;

begin
  aExpires:=0;
  aVersion:='';
  S:='SELECT '+ExpiresFieldName;
  if StorageType<>stKeyValue then
    S:=S+', '+DataFieldName;
  if UseVersion then
    S:=S+', '+VersionFieldName;
  S:=S+' FROM '+SessionTableName+' WHERE '+SessionIDFieldName+' = :ID';
  Q:=CreateQuery(S);
  try
    Q.ParamByName('ID').AsString:=aID;
    Q.Open;
    Result:=not Q.IsEmpty;
    if not Result then
      exit;
    aExpires:=Q.Fields[0].AsDateTime;
    if StorageType<>stKeyValue then
      StringToData(Q.Fields[1].AsString,aData);
    if UseVersion then
      aVersion:=Q.Fields[2].AsString;
    Q.Close;
    if StorageType=stKeyValue then
      begin
      Q.SQL.Text:='SELECT '+KeyFieldName+', '+ValueFieldName+' FROM '+ValuesTableName
                  +' WHERE '+SessionIDFieldName+' = :ID';
      Q.ParamByName('ID').AsString:=aID;
      Q.Open;
      While not Q.EOF do
        begin
        aData.Strings[Q.Fields[0].AsString]:=Q.Fields[1].AsString;
        Q.Next;
        end;
      end;
  finally
    Q.Free;
  end;
end;

procedure TDBSessionFactory.MergeData(aSession: TDBWebSession; aDBData: TJSONObject);

var
  I : Integer;
  N : String;

begin
  For I:=0 to aDBData.Count-1 do
    begin
    N:=aDBData.Names[I];
    if (aSession.Data.IndexOfName(N)<0)
       and not (Assigned(aSession.FDeletedKeys) and (aSession.FDeletedKeys.IndexOf(N)>=0)) then
      aSession.Data.Strings[N]:=aDBData.Items[I].AsString;
    end;
end;

function TDBSessionFactory.StoreSession(aSession: TDBWebSession; aInsert: Boolean): Boolean;

var
  WithData,Versioned : Boolean;

  function GetSQL : String;
  begin
    if aInsert then
      begin
      Result:='INSERT INTO '+SessionTableName+' ('+SessionIDFieldName+', '+ExpiresFieldName;
      if WithData then
        Result:=Result+', '+DataFieldName;
      if Versioned then
        Result:=Result+', '+VersionFieldName;
      Result:=Result+') VALUES (:ID, :EXPIRES';
      if WithData then
        Result:=Result+', :DATA';
      if Versioned then
        Result:=Result+', :NEWVERSION';
      Result:=Result+')';
      end
    else
      begin
      Result:='UPDATE '+SessionTableName+' SET '+ExpiresFieldName+' = :EXPIRES';
      if WithData then
        Result:=Result+', '+DataFieldName+' = :DATA';
      if Versioned then
        Result:=Result+', '+VersionFieldName+' = :NEWVERSION';
      Result:=Result+' WHERE '+SessionIDFieldName+' = :ID';
      if Versioned then
        if aSession.FVersion='' then
          // Row written without version, e.g. before the version field was added.
          Result:=Result+' AND '+VersionFieldName+' IS NULL'
        else
          Result:=Result+' AND '+VersionFieldName+' = :VERSION';
      end;
  end;

var
  Q : TSQLQuery;
  lVersion,DBVersion : String;
  Attempt : Integer;
  Written : Boolean;
  DBData : TJSONObject;
  DBExpires : TDateTime;

begin
  Result:=True;
  WithData:=(StorageType<>stKeyValue) and (aInsert or aSession.FDataDirty);
  Versioned:=WithData and UseVersion;
  Attempt:=0;
  Repeat
    Inc(Attempt);
    lVersion:='';
    Q:=CreateQuery(GetSQL);
    try
      Q.ParamByName('ID').AsString:=aSession.SessionID;
      Q.ParamByName('EXPIRES').AsDateTime:=Expiry(aSession);
      if WithData then
        Q.ParamByName('DATA').AsMemo:=DataToString(aSession.Data);
      if Versioned then
        begin
        lVersion:=NewVersion;
        Q.ParamByName('NEWVERSION').AsString:=lVersion;
        if not aInsert and (aSession.FVersion<>'') then
          Q.ParamByName('VERSION').AsString:=aSession.FVersion;
        end;
      Q.ExecSQL;
      Written:=not Versioned or aInsert or (Q.RowsAffected>0);
    finally
      Q.Free;
    end;
    if Written then
      begin
      if Versioned then
        aSession.FVersion:=lVersion;
      Exit;
      end;
    // Another request wrote the session in the meantime: merge its data and try again.
    if Attempt>=MaxWriteAttempts then
      Raise EWebSessionError.CreateFmt(SErrConcurrentWrite,[aSession.SessionID,Attempt]);
    DBData:=TJSONObject.Create;
    try
      if not LoadSession(aSession.SessionID,DBData,DBExpires,DBVersion) then
        Exit(False);
      MergeData(aSession,DBData);
    finally
      DBData.Free;
    end;
    aSession.FVersion:=DBVersion;
  until False;
end;

procedure TDBSessionFactory.StoreValues(aSession: TDBWebSession);

var
  S,N : String;
  I : Integer;

begin
  S:='DELETE FROM '+ValuesTableName+' WHERE '+SessionIDFieldName+' = :ID AND '+KeyFieldName+' = :NAME';
  For N in aSession.FDeletedKeys do
    ExecSQL(S,['ID','NAME'],[aSession.SessionID,N]);
  For N in aSession.FDirtyKeys do
    begin
    // Delete and insert: works on all databases.
    ExecSQL(S,['ID','NAME'],[aSession.SessionID,N]);
    I:=aSession.Data.IndexOfName(N);
    if I>=0 then
      ExecSQL('INSERT INTO '+ValuesTableName+' ('+SessionIDFieldName+', '+KeyFieldName+', '+ValueFieldName
              +') VALUES (:ID, :NAME, :VALUE)',
              ['ID','NAME','VALUE'],[aSession.SessionID,N,aSession.Data.Items[I].AsString]);
    end;
end;

procedure TDBSessionFactory.DeleteSession(const aID: String);
begin
  if StorageType=stKeyValue then
    ExecSQL('DELETE FROM '+ValuesTableName+' WHERE '+SessionIDFieldName+' = :ID',['ID'],[aID]);
  ExecSQL('DELETE FROM '+SessionTableName+' WHERE '+SessionIDFieldName+' = :ID',['ID'],[aID]);
end;

procedure TDBSessionFactory.RenameSession(const aOldID, aNewID: String);

var
  Q,QI : TSQLQuery;
  Cols,Pars : String;
  I : Integer;

begin
  // Copy the session row to the new ID, move the values, delete the old row.
  // This keeps the foreign key of the values table valid at all times.
  Cols:=ExpiresFieldName;
  if StorageType<>stKeyValue then
    Cols:=Cols+', '+DataFieldName;
  if UseVersion then
    Cols:=Cols+', '+VersionFieldName;
  Pars:=':EXPIRES';
  if StorageType<>stKeyValue then
    Pars:=Pars+', :DATA';
  if UseVersion then
    Pars:=Pars+', :VERSION';
  QI:=Nil;
  Q:=CreateQuery('SELECT '+Cols+' FROM '+SessionTableName+' WHERE '+SessionIDFieldName+' = :ID');
  try
    Q.ParamByName('ID').AsString:=aOldID;
    Q.Open;
    if Q.IsEmpty then
      exit;
    QI:=CreateQuery('INSERT INTO '+SessionTableName+' ('+SessionIDFieldName+', '+Cols+') VALUES (:ID, '+Pars+')');
    QI.ParamByName('ID').AsString:=aNewID;
    QI.ParamByName('EXPIRES').AsDateTime:=Q.Fields[0].AsDateTime;
    I:=1;
    if StorageType<>stKeyValue then
      begin
      QI.ParamByName('DATA').AsMemo:=Q.Fields[I].AsString;
      Inc(I);
      end;
    if UseVersion then
      if Q.Fields[I].IsNull then
        begin
        QI.ParamByName('VERSION').DataType:=ftString;
        QI.ParamByName('VERSION').Clear;
        end
      else
        QI.ParamByName('VERSION').AsString:=Q.Fields[I].AsString;
    Q.Close;
    QI.ExecSQL;
  finally
    QI.Free;
    Q.Free;
  end;
  if StorageType=stKeyValue then
    ExecSQL('UPDATE '+ValuesTableName+' SET '+SessionIDFieldName+' = :NEWID WHERE '+SessionIDFieldName+' = :ID',
            ['NEWID','ID'],[aNewID,aOldID]);
  ExecSQL('DELETE FROM '+SessionTableName+' WHERE '+SessionIDFieldName+' = :ID',['ID'],[aOldID]);
end;

function TDBSessionFactory.DoCreateSession(ARequest: TRequest): TCustomSession;

Var
  S : TDBWebSession;

begin
  if DBWebSessionClass=Nil then
    S:=TDBWebSession.Create(Nil)
  else
    S:=DBWebSessionClass.Create(Nil);
  S.Factory:=Self;
  S.SessionCookie:=SessionCookie;
  S.SessionCookiePath:=SessionCookiePath;
  S.Cached:=Cached;
  Result:=S;
end;

procedure TDBSessionFactory.DoCleanupSessions;

var
  Q : TSQLQuery;
  OK : Boolean;

begin
  StartWork;
  OK:=False;
  try
    Q:=CreateQuery('');
    try
      if StorageType=stKeyValue then
        begin
        Q.SQL.Text:='DELETE FROM '+ValuesTableName+' WHERE '+SessionIDFieldName+' IN (SELECT '
                    +SessionIDFieldName+' FROM '+SessionTableName+' WHERE '+ExpiresFieldName+' < :NOW)';
        Q.ParamByName('NOW').AsDateTime:=Now;
        Q.ExecSQL;
        end;
      Q.SQL.Text:='DELETE FROM '+SessionTableName+' WHERE '+ExpiresFieldName+' < :NOW';
      Q.ParamByName('NOW').AsDateTime:=Now;
      Q.ExecSQL;
    finally
      Q.Free;
    end;
    OK:=True;
  finally
    EndWork(OK);
  end;
end;

procedure TDBSessionFactory.DoDoneSession(var ASession: TCustomSession);
begin
  FreeAndNil(ASession);
end;

{ TDBWebSession }

destructor TDBWebSession.Destroy;
begin
  // In case an exception occurred and UpdateResponse is not called,
  // write the updates to the database.
  try
    FreeData;
  finally
    FreeAndNil(FDirtyKeys);
    FreeAndNil(FDeletedKeys);
    inherited Destroy;
  end;
end;

procedure TDBWebSession.ClearChanges;
begin
  FDataDirty:=False;
  FExpiresDirty:=False;
  FNewRow:=False;
  if Assigned(FDirtyKeys) then
    FDirtyKeys.Clear;
  if Assigned(FDeletedKeys) then
    FDeletedKeys.Clear;
end;

procedure TDBWebSession.MarkChanged(const aName: String; aDeleted: Boolean);

  Procedure Remove(L : TStrings);
  var
    I : Integer;
  begin
    I:=L.IndexOf(aName);
    if I>=0 then
      L.Delete(I);
  end;

  Procedure Add(L : TStrings);
  begin
    if L.IndexOf(aName)<0 then
      L.Add(aName);
  end;

begin
  FDataDirty:=True;
  if FDirtyKeys=Nil then
    begin
    FDirtyKeys:=TStringList.Create;
    FDeletedKeys:=TStringList.Create;
    end;
  if aDeleted then
    begin
    Remove(FDirtyKeys);
    Add(FDeletedKeys);
    end
  else
    begin
    Remove(FDeletedKeys);
    Add(FDirtyKeys);
    end;
end;

procedure TDBWebSession.Flush;

var
  OK,Stored : Boolean;

begin
  // Timeout changed after the session row was written: update expiry.
  if TimeOutMinutes<>FStoredTimeOut then
    FExpiresDirty:=True;
  if FTerminated or not (FNewRow or FExpiresDirty or FDataDirty) then
    exit;
  OK:=False;
  Stored:=True;
  FFactory.StartWork;
  try
    if FNewRow or FExpiresDirty or (FDataDirty and (FFactory.StorageType<>stKeyValue)) then
      Stored:=FFactory.StoreSession(Self,FNewRow);
    if Stored and FDataDirty and (FFactory.StorageType=stKeyValue) then
      FFactory.StoreValues(Self);
    OK:=True;
  finally
    FFactory.EndWork(OK);
  end;
  ClearChanges;
  // Session was removed by another request: discard changes, treat as terminated.
  if not Stored then
    FTerminated:=True;
  FStoredTimeOut:=TimeOutMinutes;
end;

procedure TDBWebSession.FreeData;
begin
  try
    if Assigned(FData) and not FTerminated then
      Flush;
  finally
    FreeAndNil(FData);
    ClearChanges;
  end;
end;

procedure TDBWebSession.CheckSession;
begin
  If Not Assigned(FData) then
    if FTerminated then
      Raise EWebSessionError.Create(SErrSessionTerminated)
    else
      Raise EWebSessionError.Create(SErrNoSession)
end;

function TDBWebSession.GetSessionID: String;
begin
  If (SID='') then
    SID:=inherited GetSessionID;
  Result:=SID;
end;

function TDBWebSession.GetSessionVariable(const VarName: String): String;

var
  I : Integer;

begin
  CheckSession;
  I:=FData.IndexOfName(VarName);
  if I<0 then
    Result:=''
  else
    Result:=FData.Items[I].AsString;
end;

procedure TDBWebSession.SetSessionVariable(const VarName: String; const AValue: String);
begin
  CheckSession;
  FData.Strings[VarName]:=AValue;
  MarkChanged(VarName,False);
  If Not Cached then
    Flush;
end;

function TDBWebSession.SessionVariableExists(const VarName: String): Boolean;
begin
  CheckSession;
  Result:=FData.IndexOfName(VarName)>=0;
end;

procedure TDBWebSession.RemoveVariable(const VariableName: String);

var
  I : Integer;

begin
  CheckSession;
  I:=FData.IndexOfName(VariableName);
  if I<0 then
    exit;
  FData.Delete(I);
  MarkChanged(VariableName,True);
  If Not Cached then
    Flush;
end;

procedure TDBWebSession.Terminate;

var
  OK : Boolean;

begin
  FTerminated:=True;
  If Assigned(FData) Then
    begin
    if not FNewRow then
      begin
      OK:=False;
      FFactory.StartWork;
      try
        FFactory.DeleteSession(SID);
        OK:=True;
      finally
        FFactory.EndWork(OK);
      end;
      end;
    FreeAndNil(FData);
    ClearChanges;
    end;
  RemoveFromSessionState(ssActive);
  RemoveFromSessionState(ssNew);
  RemoveFromSessionState(ssExpired);
end;

procedure TDBWebSession.RegenerateSessionID(aResponse: TResponse);

var
  OldID : String;
  OK : Boolean;

begin
  CheckSession;
  OldID:=SID;
  SID:='';
  GetSessionID;
  if not FNewRow then
    begin
    OK:=False;
    FFactory.StartWork;
    try
      FFactory.RenameSession(OldID,SID);
      OK:=True;
    finally
      FFactory.EndWork(OK);
    end;
    end;
  If Not Cached then
    Flush;
  InitResponse(aResponse);
end;

procedure TDBWebSession.UpdateResponse(AResponse: TResponse);
begin
  // Do nothing. Init has done the job.
  FreeData;
end;

procedure TDBWebSession.InitSession(ARequest: TRequest; OnNewSession, OnExpired: TNotifyEvent);

Var
  S : String;
  Expires : TDateTime;
  Found,OK : Boolean;

begin
  // First initialize all session-dependent properties to their default, because
  // in Apache-modules or fcgi programs the session-instance is re-used
  SID:='';
  FVersion:='';
  FTerminated:=False;
  // If an exception occurred during a prior request FData is still not freed
  if Assigned(FData) then
    FreeData;
  FData:=TJSONObject.Create;
  ClearChanges;
  If (SessionCookie='') then
    SessionCookie:=SFPWebSession;
  S:=ARequest.CookieFields.Values[SessionCookie];
  // have session cookie ?
  If (S<>'') then
    if not IsValidSessionID(S) then
      S:=''
    else
      begin
      OK:=False;
      FFactory.StartWork;
      try
        Found:=FFactory.LoadSession(S,FData,Expires,FVersion);
        if Found and (Expires<Now) then
          FFactory.DeleteSession(S);
        OK:=True;
      finally
        FFactory.EndWork(OK);
      end;
      if not Found or (Expires<Now) then
        begin
        AddToSessionState(ssExpired);
        // Expire session.
        If Assigned(OnExpired) then
          OnExpired(Self);
        FData.Clear;
        S:='';
        end
      else
        begin
        SID:=S;
        FExpiresDirty:=True;
        end;
      end;
  If (S='') then
    begin
    AddToSessionState(ssNew);
    GetSessionID;
    FNewRow:=True;
    If Assigned(OnNewSession) then
      OnNewSession(Self);
    end;
  If not Cached then
    Flush;
  AddToSessionState(ssActive);
end;

procedure TDBWebSession.InitResponse(AResponse: TResponse);

Var
  C : TCookie;

begin
  C:=AResponse.Cookies.FindCookie(SessionCookie);
  If (C=Nil) then
    begin
    C:=AResponse.Cookies.Add;
    C.Name:=SessionCookie;
    end;
  If FTerminated then
    C.Value:=''
  else
    begin
    C.Value:=SID;
    C.Path:=SessionCookiePath;
    end;
  C.HttpOnly:=True;
  C.SameSite:=SameSitePolicy;
  C.Secure:=SecureSession;
  AddToSessionState(ssResponseInitialized);
end;

initialization
  SessionFactoryClass:=TDBSessionFactory;
end.
