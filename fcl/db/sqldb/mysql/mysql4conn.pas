unit mysql4conn;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,sqldb,mysql4,mysql4_com,db;

Type
  TMySQLTransaction = Class(TSQLHandle)
  protected
  end;

  TMySQLCursor = Class(TSQLHandle)
  protected
    FRes: PMYSQL_RES;                   { Record pointer }
    FNeedData : Boolean;
    FStatement : String;
    Row : TMYSQL_ROW;
    RowsAffected : Int64;
    LastInsertID : Int64;
  end;

  TMySQLConnection = class (TSQLConnection)
  private
    FDialect: integer;
    FHostInfo: String;
    FServerInfo: String;
    FMySQL : PMySQL;
    function GetClientInfo: string;
    function GetServerStatus: String;
  protected
    Procedure ConnectToServer; virtual;
    Procedure SelectDatabase; virtual;
    function MySQLDataType(AType: enum_field_types; ASize: Integer; var NewType: TFieldType; var NewSize: Integer): Boolean;
    function MySQLWriteData(AType: enum_field_types; ASize: Integer; Source, Dest: PChar): Integer;
    // SQLConnection methods
    procedure DoInternalConnect; override;
    procedure DoInternalDisconnect; override;
    function GetHandle : pointer; override;

    Function AllocateCursorHandle : TSQLHandle; override;
    Function AllocateTransactionHandle : TSQLHandle; override;

    procedure FreeStatement(cursor : TSQLHandle); override;
    procedure PrepareStatement(cursor: TSQLHandle;ATransaction : TSQLTransaction;buf : string); override;
    procedure FreeFldBuffers(cursor : TSQLHandle); override;
    procedure Execute(cursor: TSQLHandle;atransaction:tSQLtransaction); override;
    procedure AddFieldDefs(cursor: TSQLHandle; FieldDefs : TfieldDefs); override;
    function Fetch(cursor : TSQLHandle) : boolean; override;
    function LoadField(cursor : TSQLHandle;FieldDef : TfieldDef;buffer : pointer) : boolean; override;
    function GetTransactionHandle(trans : TSQLHandle): pointer; override;
    function Commit(trans : TSQLHandle) : boolean; override;
    function RollBack(trans : TSQLHandle) : boolean; override;
    function StartdbTransaction(trans : TSQLHandle) : boolean; override;
    procedure CommitRetaining(trans : TSQLHandle); override;
    procedure RollBackRetaining(trans : TSQLHandle); override;
  Public
    Property ServerInfo : String Read FServerInfo;
    Property HostInfo : String Read FHostInfo;
    property ClientInfo: string read GetClientInfo;
    property ServerStatus : String read GetServerStatus;
  published
    property Dialect  : integer read FDialect write FDialect;
    property DatabaseName;
    property HostName;
    property KeepConnection;
    property LoginPrompt;
    property Params;
    property OnLogin;
  end;

  EMySQLError = Class(Exception);

implementation

{ TMySQLConnection }

Resourcestring
  SErrServerConnectFailed = 'Server connect failed.';
  SErrDatabaseSelectFailed = 'failed to select database: %s';
  SErrDatabaseCreate = 'Failed to create database: %s';
  SErrDatabaseDrop = 'Failed to drop database: %s';
  SErrNoData = 'No data for record';
  SErrExecuting = 'Error executing query: %s';
  SErrFetchingdata = 'Error fetching row data: %s';
  SErrGettingResult = 'Error getting result set: %s';
  SErrNoQueryResult = 'No result from query.';
  
Procedure MySQlError(R : PMySQL;Msg: String;Comp : TComponent);

Var
  MySQLMsg : String;

begin
 If (R<>Nil) then
   begin
   MySQLMsg:=Strpas(mysql_error(R));
   DatabaseErrorFmt(Msg,[MySQLMsg],Comp);
   end
 else
   DatabaseError(Msg,Comp);
end;

function TMySQLConnection.GetClientInfo: string;
begin
  CheckConnected;
  Result:=strpas(mysql_get_client_info);
end;

function TMySQLConnection.GetServerStatus: String;
begin
  CheckConnected;
  Result := mysql_stat(FMYSQL);
end;

procedure TMySQLConnection.ConnectToServer;
Var
  H,U,P : String;

begin
  H:=HostName;
  U:=UserName;
  P:=Password;
  if (FMySQL=Nil) then
    New(FMySQL);
  mysql_init(FMySQL);
  FMySQL:=mysql_real_connect(FMySQL,PChar(H),PChar(U),Pchar(P),Nil,0,Nil,0);
  If (FMySQL=Nil) then
    MySQlError(Nil,SErrServerConnectFailed,Self);
  FServerInfo := strpas(mysql_get_server_info(FMYSQL));
  FHostInfo := strpas(mysql_get_host_info(FMYSQL));
end;

procedure TMySQLConnection.SelectDatabase;
begin
  if mysql_select_db(FMySQL,pchar(DatabaseName))<>0 then
    MySQLError(FMySQL,SErrDatabaseSelectFailed,Self);
end;

procedure TMySQLConnection.DoInternalConnect;
begin
  inherited DoInternalConnect;
  ConnectToServer;
  SelectDatabase;
end;

procedure TMySQLConnection.DoInternalDisconnect;
begin
  inherited DoInternalDisconnect;
  mysql_close(FMySQL);
  FMySQL:=Nil;
end;

function TMySQLConnection.GetHandle: pointer;
begin
  Result:=FMySQL;
end;

function TMySQLConnection.AllocateCursorHandle: TSQLHandle;
begin
  Result:=TMySQLCursor.Create;
end;

function TMySQLConnection.AllocateTransactionHandle: TSQLHandle;
begin
  Result:=TMySQLTransaction.Create;
end;

procedure TMySQLConnection.FreeStatement(cursor: TSQLHandle);

Var
  C : TMySQLCursor;

begin
  C:=Cursor as TMysqlCursor;
  if c.StatementType=stSelect then
    c.FNeedData:=False;
  If (C.FRes<>Nil) then
    begin
    C.FRes:=Nil;
    end;
end;

procedure TMySQLConnection.PrepareStatement(cursor: TSQLHandle;
  ATransaction: TSQLTransaction; buf: string);
begin
  With Cursor as TMysqlCursor do
    begin
    FStatement:=Buf;
    if StatementType=stSelect then
      FNeedData:=True;
    end
end;

procedure TMySQLConnection.FreeFldBuffers(cursor: TSQLHandle);

Var
  C : TMySQLCursor;
  
begin
  C:=Cursor as TMysqlCursor;
  If (C.FRes<>Nil) then
    begin
    Mysql_free_result(C.FRes);
    C.FRes:=Nil;
    end;
end;

procedure TMySQLConnection.Execute(cursor: TSQLHandle;
  atransaction: tSQLtransaction);
  
Var
  C : TMySQLCursor;

begin
  C:=Cursor as TMysqlCursor;
  If (C.FRes=Nil) then
    begin
    if mysql_query(FMySQL,Pchar(C.FStatement))<>0 then
      MySQLError(FMYSQL,Format(SErrExecuting,[StrPas(mysql_error(FMySQL))]),Self)
    else
      begin
      C.RowsAffected := mysql_affected_rows(FMYSQL);
      C.LastInsertID := mysql_insert_id(FMYSQL);
      if C.FNeedData then
        C.FRes:=mysql_use_result(FMySQL);
      end;
    end;
end;

function TMySQLConnection.MySQLDataType(AType: enum_field_types; ASize: Integer;
   var NewType: TFieldType; var NewSize: Integer): Boolean;
begin
  Result := True;
  case AType of
    FIELD_TYPE_TINY, FIELD_TYPE_SHORT, FIELD_TYPE_LONG, FIELD_TYPE_LONGLONG,
    FIELD_TYPE_INT24:
      begin
      NewType := ftInteger;
      NewSize := 0;
      end;
    FIELD_TYPE_DECIMAL, FIELD_TYPE_FLOAT, FIELD_TYPE_DOUBLE:
      begin
      NewType := ftFloat;
      NewSize := 0;
      end;
    FIELD_TYPE_TIMESTAMP, FIELD_TYPE_DATETIME:
      begin
      NewType := ftDateTime;
      NewSize := 0;
      end;
    FIELD_TYPE_DATE:
      begin
      NewType := ftDate;
      NewSize := 0;
      end;
    FIELD_TYPE_TIME:
      begin
      NewType := ftTime;
      NewSize := 0;
      end;
    FIELD_TYPE_VAR_STRING, FIELD_TYPE_STRING, FIELD_TYPE_ENUM, FIELD_TYPE_SET:
      begin
      NewType := ftString;
      NewSize := ASize;
      end;
  else
    Result := False;
  end;
end;

procedure TMySQLConnection.AddFieldDefs(cursor: TSQLHandle;
  FieldDefs: TfieldDefs);
  
var
  C : TMySQLCursor;
  I, FC: Integer;
  field: PMYSQL_FIELD;
  DFT: TFieldType;
  DFS: Integer;
  WasClosed: Boolean;

begin
//  Writeln('MySQL: Adding fielddefs');
  C:=(Cursor as TMySQLCursor);
  If (C.FRes=Nil) then
    begin
//    Writeln('res is nil');
    MySQLError(FMySQL,SErrNoQueryResult,Self);
    end;
//  Writeln('MySQL: have result');
  FC:=mysql_num_fields(C.FRes);
  For I:= 0 to FC-1 do
    begin
    field := mysql_fetch_field_direct(C.FRES, I);
//    Writeln('MySQL: creating fielddef ',I+1);

    if MySQLDataType(field^.ftype, field^.length, DFT, DFS) then
      TFieldDef.Create(FieldDefs, field^.name, DFT, DFS, False, I+1);
    end;
//  Writeln('MySQL: Finished adding fielddefs');
end;

function TMySQLConnection.Fetch(cursor: TSQLHandle): boolean;

Var
  C : TMySQLCursor;

begin
  C:=Cursor as TMySQLCursor;
  C.Row:=MySQL_Fetch_row(C.FRes);
  Result:=(C.Row<>Nil);
end;

function TMySQLConnection.LoadField(cursor : TSQLHandle;
  FieldDef : TfieldDef;buffer : pointer) : boolean;

var
  I, FC, CT: Integer;
  field: PMYSQL_FIELD;
  row : TMYSQL_ROW;
  C : TMySQLCursor;

begin
//  Writeln('LoadFieldsFromBuffer');
  C:=Cursor as TMySQLCursor;
  if C.Row=nil then
     begin
  //   Writeln('LoadFieldsFromBuffer: row=nil');
     MySQLError(FMySQL,SErrFetchingData,Self);
     end;
  Row:=C.Row;
  FC := mysql_num_fields(C.FRES);

  for I := 0 to FC-1 do
    begin
    field := mysql_fetch_field_direct(C.FRES, I);
    if field^.name=FieldDef.name then break;
    Inc(Row);
    end;

  CT := MySQLWriteData(field^.ftype, field^.length, Row^, Buffer);
  result := true;
end;

function InternalStrToFloat(S: string): Extended;

var
  I: Integer;
  Tmp: string;

begin
  Tmp := '';
  for I := 1 to Length(S) do
    begin
    if not (S[I] in ['0'..'9', '+', '-', 'E', 'e']) then
      Tmp := Tmp + DecimalSeparator
    else
      Tmp := Tmp + S[I];
    end;
  Result := StrToFloat(Tmp);
end;

function InternalStrToDate(S: string): TDateTime;

var
  EY, EM, ED: Word;

begin
  EY := StrToInt(Copy(S,1,4));
  EM := StrToInt(Copy(S,6,2));
  ED := StrToInt(Copy(S,9,2));
  if (EY = 0) or (EM = 0) or (ED = 0) then
    Result:=0
  else
    Result:=EncodeDate(EY, EM, ED);
end;

function InternalStrToDateTime(S: string): TDateTime;

var
  EY, EM, ED: Word;
  EH, EN, ES: Word;

begin
  EY := StrToInt(Copy(S, 1, 4));
  EM := StrToInt(Copy(S, 6, 2));
  ED := StrToInt(Copy(S, 9, 2));
  EH := StrToInt(Copy(S, 11, 2));
  EN := StrToInt(Copy(S, 14, 2));
  ES := StrToInt(Copy(S, 17, 2));
  if (EY = 0) or (EM = 0) or (ED = 0) then
    Result := 0
  else
    Result := EncodeDate(EY, EM, ED);
  Result := Result + EncodeTime(EH, EN, ES, 0);
end;

function InternalStrToTime(S: string): TDateTime;

var
  EH, EM, ES: Word;

begin
  EH := StrToInt(Copy(S, 1, 2));
  EM := StrToInt(Copy(S, 4, 2));
  ES := StrToInt(Copy(S, 7, 2));
  Result := EncodeTime(EH, EM, ES, 0);
end;

function InternalStrToTimeStamp(S: string): TDateTime;

var
  EY, EM, ED: Word;
  EH, EN, ES: Word;

begin
  EY := StrToInt(Copy(S, 1, 4));
  EM := StrToInt(Copy(S, 5, 2));
  ED := StrToInt(Copy(S, 7, 2));
  EH := StrToInt(Copy(S, 9, 2));
  EN := StrToInt(Copy(S, 11, 2));
  ES := StrToInt(Copy(S, 13, 2));
  if (EY = 0) or (EM = 0) or (ED = 0) then
    Result := 0
  else
    Result := EncodeDate(EY, EM, ED);
  Result := Result + EncodeTime(EH, EN, ES, 0);;
end;

function TMySQLConnection.MySQLWriteData(AType: enum_field_types;ASize: Integer; Source, Dest: PChar): Integer;

var
  VI: Integer;
  VF: Double;
  VD: TDateTime;
  Src : String;

begin
  Result := 0;
  If (Source<>Nil) Then
    Src:=StrPas(Source)
  else
    Src:='';
  case AType of
    FIELD_TYPE_TINY, FIELD_TYPE_SHORT, FIELD_TYPE_LONG, FIELD_TYPE_LONGLONG,
    FIELD_TYPE_INT24:
      begin
      Result:=SizeOf(Integer);
      if (Src<>'') then
        VI := StrToInt(Src)
      else
        VI := 0;
      Move(VI, Dest^, Result);
      end;
    FIELD_TYPE_DECIMAL, FIELD_TYPE_FLOAT, FIELD_TYPE_DOUBLE:
      begin
      Result := SizeOf(Double);
      if Src <> '' then
        VF := InternalStrToFloat(Src)
      else
        VF := 0;
      Move(VF, Dest^, Result);
      end;
    FIELD_TYPE_TIMESTAMP:
      begin
      Result := SizeOf(TDateTime);
      if Src <> '' then
        VD := InternalStrToTimeStamp(Src)
      else
        VD := 0;
      Move(VD, Dest^, Result);
      end;
    FIELD_TYPE_DATETIME:
      begin
      Result := SizeOf(TDateTime);
      if Src <> '' then
        VD := InternalStrToDateTime(Src)
      else
        VD := 0;
      Move(VD, Dest^, Result);
      end;
    FIELD_TYPE_DATE:
      begin
      Result := SizeOf(TDateTime);
      if Src <> '' then
        VD := InternalStrToDate(Src)
      else
        VD := 0;
      Move(VD, Dest^, Result);
      end;
    FIELD_TYPE_TIME:
      begin
      Result := SizeOf(TDateTime);
      if Src <> '' then
        VD := InternalStrToTime(Src)
      else
        VD := 0;
      Move(VD, Dest^, Result);
      end;
    FIELD_TYPE_VAR_STRING, FIELD_TYPE_STRING, FIELD_TYPE_ENUM, FIELD_TYPE_SET:
      begin
      Result := ASize;
{      Write('Moving string of size ',asize,' : ');
      P:=Source;
      If (P<>nil) then
        While P[0]<>#0 do
          begin
          Write(p[0]);
          inc(p);
          end;
      Writeln;
}      if Src<> '' then
        Move(Source^, Dest^, Result)
      else
        Dest^ := #0;
      end;
  end;
end;

Function GetSQLStatementType(SQL : String) : TStatementType;

Var
  L : Integer;
  cmt : boolean;
  P,PE,PP : PChar;
  S : string;
  T : TStatementType;

begin
  Result:=stNone;
  L:=Length(SQL);
  If (L=0) then
    Exit;
  P:=Pchar(SQL);
  PP:=P;
  Cmt:=False;
  While ((P-PP)<L) do
    begin
    if not (P^ in [' ',#13,#10,#9]) then
      begin
      if not Cmt then
        begin
        // Check for comment.
        Cmt:=(P^='/') and (((P-PP)<=L) and (P[1]='*'));
        if not (cmt) then
          Break;
        end
      else
        begin
        // Check for end of comment.
         Cmt:=Not( (P^='*') and (((P-PP)<=L) and (P[1]='/')) );
        If not cmt then
          Inc(p);
        end;
      end;
    inc(P);
    end;
  PE:=P+1;
  While ((PE-PP)<L) and (PE^ in ['0'..'9','a'..'z','A'..'Z','_']) do
   Inc(PE);
  Setlength(S,PE-P);
  Move(P^,S[1],(PE-P));
  S:=Lowercase(s);
  For t:=stselect to strollback do
    if (S=StatementTokens[t]) then
      Exit(t);
end;

function TMySQLConnection.GetTransactionHandle(trans: TSQLHandle): pointer;
begin
  Result:=Nil;
end;

function TMySQLConnection.Commit(trans: TSQLHandle): boolean;
begin
  // Do nothing.
end;

function TMySQLConnection.RollBack(trans: TSQLHandle): boolean;
begin
  // Do nothing
end;

function TMySQLConnection.StartdbTransaction(trans: TSQLHandle): boolean;
begin
  // Do nothing
end;

procedure TMySQLConnection.CommitRetaining(trans: TSQLHandle);
begin
  // Do nothing
end;

procedure TMySQLConnection.RollBackRetaining(trans: TSQLHandle);
begin
  // Do nothing
end;

end.

