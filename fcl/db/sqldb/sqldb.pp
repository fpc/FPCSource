{   $Id$

    Copyright (c) 2004 by Joost van der Sluis


    SQL database & dataset

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit sqldb;

{$mode objfpc}
{$H+}
{$M+}   // ### remove this!!!

interface

uses SysUtils, Classes, DB;

type TSchemaType = (stNoSchema, stTables, stSysTables, stProcedures, stColumns, stProcedureParams, stIndexes, stPackages);

type
  TSQLConnection = class;
  TSQLTransaction = class;
  TSQLQuery = class;

  TStatementType = (stNone, stSelect, stInsert, stUpdate, stDelete,
    stDDL, stGetSegment, stPutSegment, stExecProcedure,
    stStartTrans, stCommit, stRollback, stSelectForUpd);

  TSQLHandle = Class(TObject)
    protected
    StatementType       : TStatementType;
  end;


const
 StatementTokens : Array[TStatementType] of string = ('(none)', 'select',
                  'insert', 'update', 'delete',
                  'create', 'get', 'put', 'execute',
                  'start','commit','rollback', '?'
                 );


{ TSQLConnection }
type
  TSQLConnection = class (TDatabase)
  private
    FPassword            : string;
    FTransaction         : TSQLTransaction;
    FUserName            : string;
    FHostName            : string;
    FCharSet             : string;
    FRole                : String;

    procedure SetTransaction(Value : TSQLTransaction);
  protected
    function StrToStatementType(s : string) : TStatementType; virtual;
    procedure DoInternalConnect; override;
    procedure DoInternalDisconnect; override;
    function GetAsSQLText(Field : TField) : string; virtual;
    function GetHandle : pointer; virtual; abstract;

    Function AllocateCursorHandle : TSQLHandle; virtual; abstract;
    Function AllocateTransactionHandle : TSQLHandle; virtual; abstract;

    procedure FreeStatement(cursor : TSQLHandle); virtual; abstract;
    procedure PrepareStatement(cursor: TSQLHandle;ATransaction : TSQLTransaction;buf : string); virtual; abstract;
    procedure FreeFldBuffers(cursor : TSQLHandle); virtual; abstract;
    procedure Execute(cursor: TSQLHandle;atransaction:tSQLtransaction); virtual; abstract;
    procedure AddFieldDefs(cursor: TSQLHandle; FieldDefs : TfieldDefs); virtual; abstract;
    function Fetch(cursor : TSQLHandle) : boolean; virtual; abstract;
    function LoadField(cursor : TSQLHandle;FieldDef : TfieldDef;buffer : pointer) : boolean; virtual; abstract;
    function GetTransactionHandle(trans : TSQLHandle): pointer; virtual; abstract;
    function Commit(trans : TSQLHandle) : boolean; virtual; abstract;
    function RollBack(trans : TSQLHandle) : boolean; virtual; abstract;
    function StartdbTransaction(trans : TSQLHandle) : boolean; virtual; abstract;
    procedure CommitRetaining(trans : TSQLHandle); virtual; abstract;
    procedure RollBackRetaining(trans : TSQLHandle); virtual; abstract;
    procedure UpdateIndexDefs(var IndexDefs : TIndexDefs;TableName : string); virtual;
    function GetSchemaInfoSQL(SchemaType : TSchemaType; SchemaObjectName, SchemaPattern : string) : string; virtual;
  public
    property Handle: Pointer read GetHandle;
    destructor Destroy; override;
  published
    property Password : string read FPassword write FPassword;
    property Transaction : TSQLTransaction read FTransaction write SetTransaction;
    property UserName : string read FUserName write FUserName;
    property CharSet : string read FCharSet write FCharSet;
    property HostName : string Read FHostName Write FHostName;

    property Connected;
    Property Role :  String read FRole write FRole;
    property DatabaseName;
    property KeepConnection;
    property LoginPrompt;
    property Params;
    property OnLogin;
  end;

{ TSQLTransaction }

  TCommitRollbackAction = (caNone, caCommit, caCommitRetaining, caRollback,
    caRollbackRetaining);

  TSQLTransaction = class (TDBTransaction)
  private
    FTrans               : TSQLHandle;
    FAction              : TCommitRollbackAction;
  protected
    function GetHandle : Pointer; virtual;
  public
    procedure Commit; virtual;
    procedure CommitRetaining; virtual;
    procedure Rollback; virtual;
    procedure RollbackRetaining; virtual;
    procedure StartTransaction; override;
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    property Handle: Pointer read GetHandle;
    procedure EndTransaction; override;
  published
    property Action : TCommitRollbackAction read FAction write FAction;
    property Database;
  end;

{ TSQLQuery }

  TSQLQuery = class (Tbufdataset)
  private
    FCursor              : TSQLHandle;
    FUpdateable          : boolean;
    FTableName           : string;
    FSQL                 : TStrings;
    FIsEOF               : boolean;
    FLoadingFieldDefs    : boolean;
    FIndexDefs           : TIndexDefs;
    FReadOnly            : boolean;
    FUpdateMode          : TUpdateMode;
    FusePrimaryKeyAsKey  : Boolean;
//    FSchemaInfo          : TSchemaInfo;

    procedure FreeStatement;
    procedure PrepareStatement;
    procedure FreeFldBuffers;
    procedure InitUpdates(SQL : string);
    function GetIndexDefs : TIndexDefs;
    procedure SetIndexDefs(AValue : TIndexDefs);
    procedure SetReadOnly(AValue : Boolean);
    procedure SetUsePrimaryKeyAsKey(AValue : Boolean);
    procedure SetUpdateMode(AValue : TUpdateMode);

    procedure Execute;

  protected
    // abstract & virtual methods of TBufDataset
    function Fetch : boolean; override;
    function LoadField(FieldDef : TFieldDef;buffer : pointer) : boolean; override;
    // abstract & virtual methods of TDataset
    procedure UpdateIndexDefs; override;
    procedure SetDatabase(Value : TDatabase); override;
    procedure InternalAddRecord(Buffer: Pointer; AAppend: Boolean); override;
    procedure InternalClose; override;
    procedure InternalHandleException; override;
    procedure InternalInitFieldDefs; override;
    procedure InternalOpen; override;
    function  GetCanModify: Boolean; override;
    Function GetSQLStatementType(SQL : String) : TStatementType; virtual;
    function ApplyRecUpdate(UpdateKind : TUpdateKind) : boolean; override;
  public
    procedure ExecSQL; virtual;
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure SetSchemaInfo( SchemaType : TSchemaType; SchemaObjectName, SchemaPattern : string); virtual;
  published
    // redeclared data set properties
    property Active;
//    property Filter;
//    property Filtered;
//    property FilterOptions;
    property BeforeOpen;
    property AfterOpen;
    property BeforeClose;
    property AfterClose;
    property BeforeInsert;
    property AfterInsert;
    property BeforeEdit;
    property AfterEdit;
    property BeforePost;
    property AfterPost;
    property BeforeCancel;
    property AfterCancel;
    property BeforeDelete;
    property AfterDelete;
    property BeforeScroll;
    property AfterScroll;
    property OnCalcFields;
    property OnDeleteError;
    property OnEditError;
    property OnFilterRecord;
    property OnNewRecord;
    property OnPostError;
    property AutoCalcFields;
    property Database;

    property Transaction;
    property ReadOnly : Boolean read FReadOnly write SetReadOnly;
    property SQL : TStrings read FSQL write FSQL;
    property IndexDefs : TIndexDefs read GetIndexDefs;
    property UpdateMode : TUpdateMode read FUpdateMode write SetUpdateMode;
    property UsePrimaryKeyAsKey : boolean read FUsePrimaryKeyAsKey write SetUsePrimaryKeyAsKey;
//    property SchemaInfo : TSchemaInfo read FSchemaInfo default stNoSchema;
  end;

implementation

uses dbconst;

{ TSQLConnection }

function TSQLConnection.StrToStatementType(s : string) : TStatementType;

var T : TStatementType;

begin
  S:=Lowercase(s);
  For t:=stselect to strollback do
    if (S=StatementTokens[t]) then
      Exit(t);
end;

procedure TSQLConnection.SetTransaction(Value : TSQLTransaction);
begin
  if FTransaction = nil then
  begin
    FTransaction := Value;
    if Assigned(FTransaction) then
      FTransaction.Database := Self;
    exit;
  end;

  if (Value <> FTransaction) and (Value <> nil) then
    if (not FTransaction.Active) then
    begin
      FTransaction := Value;
      FTransaction.Database := Self;
    end
    else
      DatabaseError(SErrAssTransaction);
end;

procedure TSQLConnection.UpdateIndexDefs(var IndexDefs : TIndexDefs;TableName : string);

begin
// Empty abstract
end;

procedure TSQLConnection.DoInternalConnect;
begin
// Empty abstract
end;

procedure TSQLConnection.DoInternalDisconnect;
begin
end;

destructor TSQLConnection.Destroy;
begin
  inherited Destroy;
end;

function TSQLConnection.GetAsSQLText(Field : TField) : string;

begin
  if not assigned(field) then Result := 'Null'
  else case field.DataType of
    ftString   : Result := '''' + field.asstring + '''';
    ftDate     : Result := '''' + FormatDateTime('yyyy-mm-dd',Field.AsDateTime) + '''';
    ftDateTime : Result := '''' + FormatDateTime('yyyy-mm-dd hh:mm:ss',Field.AsDateTime) + ''''
  else
    Result := field.asstring;
  end; {case}
end;


function TSQLConnection.GetSchemaInfoSQL( SchemaType : TSchemaType; SchemaObjectName, SchemaPattern : string) : string;

begin
  DatabaseError(SMetadataUnavailable);
end;


{ TSQLTransaction }
procedure TSQLTransaction.EndTransaction;

begin
  rollback;
end;

function TSQLTransaction.GetHandle: pointer;
begin
  Result := (Database as tsqlconnection).GetTransactionHandle(FTrans);
end;

procedure TSQLTransaction.Commit;
begin
  if active then
    begin
    closedatasets;
    if (Database as tsqlconnection).commit(FTrans) then
      begin
      closeTrans;
      FreeAndNil(FTrans);
      end;
    end;
end;

procedure TSQLTransaction.CommitRetaining;
begin
  if active then
    (Database as tsqlconnection).commitRetaining(FTrans);
end;

procedure TSQLTransaction.Rollback;
begin
  if active then
    begin
    closedatasets;
    if (Database as tsqlconnection).RollBack(FTrans) then
      begin
      CloseTrans;
      FreeAndNil(FTrans);
      end;
    end;
end;

procedure TSQLTransaction.RollbackRetaining;
begin
  if active then
    (Database as tsqlconnection).RollBackRetaining(FTrans);
end;

procedure TSQLTransaction.StartTransaction;

var db : TSQLConnection;

begin
  if Active then
    DatabaseError(SErrTransAlreadyActive);

  db := (Database as tsqlconnection);

  if Db = nil then
    DatabaseError(SErrDatabasenAssigned);

  if not Db.Connected then
    Db.Open;
  if not assigned(FTrans) then FTrans := Db.AllocateTransactionHandle;

  if Db.StartdbTransaction(FTrans) then OpenTrans;
end;

constructor TSQLTransaction.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
end;

destructor TSQLTransaction.Destroy;
begin
  Rollback;
  inherited Destroy;
end;

{ TSQLQuery }
procedure TSQLQuery.SetDatabase(Value : TDatabase);

var db : tsqlconnection;

begin
  if (Database <> Value) then
    begin
    db := value as tsqlconnection;
    inherited setdatabase(value);
    if assigned(value) and (Transaction = nil) and (Assigned(db.Transaction)) then
      transaction := Db.Transaction;
    end;
end;

procedure TSQLQuery.FreeStatement;
begin
  if assigned(FCursor) then
    begin
    (Database as tsqlconnection).FreeStatement(FCursor);
//    FreeAndNil(FCursor);
    end;
end;

procedure TSQLQuery.PrepareStatement;
var
  Buf   : string;
  x     : integer;
  db    : tsqlconnection;
  sqltr : tsqltransaction;

begin
  db := (Database as tsqlconnection);
  if not assigned(Db) then
    DatabaseError(SErrDatabasenAssigned);
  if not Db.Connected then
    db.Open;
  if not assigned(Transaction) then
    DatabaseError(SErrTransactionnSet);

  sqltr := (transaction as tsqltransaction);
  if not sqltr.Active then sqltr.StartTransaction;

  if assigned(fcursor) then FreeAndNil(fcursor);
  FCursor := Db.AllocateCursorHandle;

  Buf := '';
  for x := 0 to FSQL.Count - 1 do
    Buf := Buf + FSQL[x] + ' ';

  if Buf='' then
    begin
    DatabaseError(SErrNoStatement);
    exit;
    end;
  FCursor.StatementType := GetSQLStatementType(buf);
  if (FCursor.StatementType = stSelect) and not ReadOnly then InitUpdates(Buf);
  Db.PrepareStatement(Fcursor,sqltr,buf);
end;

procedure TSQLQuery.FreeFldBuffers;
begin
  if assigned(FCursor) then (Database as tsqlconnection).FreeFldBuffers(FCursor);
end;

function TSQLQuery.Fetch : boolean;
begin
  if not (Fcursor.StatementType in [stSelect]) then
    Exit;

  if not FIsEof then FIsEOF := not (Database as tsqlconnection).Fetch(Fcursor);
  Result := not FIsEOF;
end;

procedure TSQLQuery.Execute;
begin
  (Database as tsqlconnection).execute(Fcursor,Transaction as tsqltransaction);
end;

function TSQLQuery.LoadField(FieldDef : TFieldDef;buffer : pointer) : boolean;

begin
  result := (Database as tSQLConnection).LoadField(FCursor,FieldDef,buffer)
end;

procedure TSQLQuery.InternalAddRecord(Buffer: Pointer; AAppend: Boolean);
begin
  // not implemented - sql dataset
end;

procedure TSQLQuery.InternalClose;
begin
  FreeFldBuffers;
  FreeStatement;
  if DefaultFields then
    DestroyFields;
  FIsEOF := False;
//  FRecordSize := 0;
  inherited internalclose;
end;

procedure TSQLQuery.InternalHandleException;
begin
end;

procedure TSQLQuery.InternalInitFieldDefs;
begin
  if FLoadingFieldDefs then
    Exit;

  FLoadingFieldDefs := True;

  try
    FieldDefs.Clear;

    (Database as tsqlconnection).AddFieldDefs(fcursor,FieldDefs);
  finally
    FLoadingFieldDefs := False;
  end;
end;

procedure TSQLQuery.InitUpdates(SQL : string);

Var
  L       : Integer;
  P,PP    : PChar;
  PS: PChar;
  S       : string;

  function GetStatement(var StartP : PChar) : PChar;

  var p        : pchar;
      Cmt, Stm : boolean;

  begin
    p := StartP;
    Cmt := false;
    Stm := False;
    While ((P-PP)<L) do
      begin
      if Cmt then
        begin
        end
      else if (p^ in [',',' ','(',')',#13,#10,#9]) then
        begin
        if stm then break;
        end
      else if not stm then
        begin
        StartP := p;
        stm := true;
        end;
      inc(p);
      end;
    Result := P;
  end;

begin
  FUpdateable := False;

  L:=Length(SQL);

  PP:=Pchar(SQL);
  P := pp;
  PS := pp;

// select-keyword
  P := GetStatement(PS);

  Setlength(S,P-PS);
  Move(PS^,S[1],(P-PS));
  S:=Lowercase(S);

  if (S) <> 'select' then exit;

// select-part

  While ((P-PP)<L) and  (S <> 'from') do
    begin
    repeat
    PS := P;
    P := GetStatement(PS);
    until P^ <> ',';

    Setlength(S,P-PS);
    Move(PS^,S[1],(P-PS));
    S:=Lowercase(S);

    end;

// from-part

  PS := P;
  P := GetStatement(PS);

  Setlength(FTableName,P-PS);
  Move(PS^,FTableName[1],(P-PS));

  While ((P-PP)<L)  do
    begin
    PS := P;
    P := GetStatement(PS);

    if P^ = ',' then exit; // select-statements from more then one table are not updateable

    Setlength(S,P-PS);
    Move(PS^,S[1],(P-PS));
    S:=Lowercase(S);

    if (s = 'where') or (s='order') then break;
    end;

  FUpdateable := True;
end;

procedure TSQLQuery.InternalOpen;

var tel : integer;
    f   : TField;
s : string;
begin
  try
    PrepareStatement;
    if Fcursor.StatementType in [stSelect] then
      begin
      Execute;
      InternalInitFieldDefs;
      if DefaultFields then
        begin
        CreateFields;

        if FUpdateable and FusePrimaryKeyAsKey then
          begin
          UpdateIndexDefs;
          for tel := 0 to indexdefs.count-1 do {with indexdefs[tel] do}
            begin
            if ixPrimary in indexdefs[tel].options then
              begin
              // Todo: If there is more then one field in the key, that must be parsed
              s := indexdefs[tel].fields;
              F := fieldbyname(s);
              F.ProviderFlags := F.ProviderFlags + [pfInKey];
              end;
            end;
          end;
        end;
      end
    else
      DatabaseError(SErrNoSelectStatement,Self);
  except
    on E:Exception do
      raise;
  end;
  inherited InternalOpen;
end;

// public part

procedure TSQLQuery.ExecSQL;
begin
  try
    PrepareStatement;
    Execute;
  finally
    FreeStatement;
  end;
end;

constructor TSQLQuery.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FSQL := TStringList.Create;
  FIndexDefs := TIndexDefs.Create(Self);
  FReadOnly := false;
// Delphi has upWhereAll as default, but since strings and oldvalue's don't work yet
// (variants) set it to upWhereKeyOnly
  FUpdateMode := upWhereKeyOnly;

  FUsePrimaryKeyAsKey := True;
end;

destructor TSQLQuery.Destroy;
begin
  if Active then Close;
//  if assigned(FCursor) then FCursor.destroy;
  FreeAndNil(FSQL);
  inherited Destroy;
end;

Function TSQLQuery.GetSQLStatementType(SQL : String) : TStatementType;

Var
  L       : Integer;
  cmt     : boolean;
  P,PE,PP : PChar;
  S       : string;

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
  result := (DataBase as TSQLConnection).StrToStatementType(s);
end;

procedure TSQLQuery.SetReadOnly(AValue : Boolean);

begin
  if not Active then FReadOnly := AValue
  else
    begin
    // Just temporary, this should be possible in the future
    DatabaseError(SActiveDataset);
    end;
end;

procedure TSQLQuery.SetUsePrimaryKeyAsKey(AValue : Boolean);

begin
  if not Active then FusePrimaryKeyAsKey := AValue
  else
    begin
    // Just temporary, this should be possible in the future
    DatabaseError(SActiveDataset);
    end;
end;

Procedure TSQLQuery.UpdateIndexDefs;

begin
  if assigned(DataBase) then
    (DataBase as TSQLConnection).UpdateIndexDefs(FIndexDefs,FTableName);
end;

function TSQLQuery.ApplyRecUpdate(UpdateKind : TUpdateKind) : boolean;

var
    sql_tables : string;
    s : string;

  procedure UpdateWherePart(var sql_where : string;x : integer);

  begin
    if (pfInKey in Fields[x].ProviderFlags) or
       ((FUpdateMode = upWhereAll) and (pfInWhere in Fields[x].ProviderFlags)) or
       ((FUpdateMode = UpWhereChanged) and (pfInWhere in Fields[x].ProviderFlags) and (fields[x].value <> fields[x].oldvalue)) then
      begin
      // This should be converted to something like GetAsSQLText, but better wait until variants (oldvalue) are working for strings
      s := fields[x].oldvalue; // This directly int the line below raises a variant-error
      sql_where := sql_where + '(' + fields[x].FieldName + '=' + s + ') and ';
      end;
  end;

  function ModifyRecQuery : string;

  var x          : integer;
      sql_set    : string;
      sql_where  : string;

  begin
    sql_tables := FTableName;
    sql_set := '';
    sql_where := '';
    for x := 0 to Fields.Count -1 do
      begin
      UpdateWherePart(sql_where,x);

      if (pfInUpdate in Fields[x].ProviderFlags) then
        if fields[x].IsNull then // check for null
          sql_set := sql_set + fields[x].FieldName + '=' + (Database as TSQLConnection).GetAsSQLText(nil) + ','
        else
          sql_set := sql_set + fields[x].FieldName + '=' + (Database as TSQLConnection).GetAsSQLText(fields[x]) + ',';
      end;

    setlength(sql_set,length(sql_set)-1);
    setlength(sql_where,length(sql_where)-5);
    result := 'update ' + sql_tables + ' set ' + sql_set + ' where ' + sql_where;

  end;

  function InsertRecQuery : string;

  var x          : integer;
      sql_fields : string;
      sql_values : string;

  begin
    sql_tables := FTableName;
    sql_fields := '';
    sql_values := '';
    for x := 0 to Fields.Count -1 do
      begin
      if not fields[x].IsNull then
        begin
        sql_fields := sql_fields + fields[x].DisplayName + ',';
        sql_values := sql_values + (Database as TSQLConnection).GetAsSQLText(fields[x]) + ',';
        end;
      end;
    setlength(sql_fields,length(sql_fields)-1);
    setlength(sql_values,length(sql_values)-1);

    result := 'insert into ' + sql_tables + ' (' + sql_fields + ') values (' + sql_values + ')';
  end;

  function DeleteRecQuery : string;

  var x          : integer;
      sql_where  : string;

  begin
    sql_tables := FTableName;

    sql_where := '';
    for x := 0 to Fields.Count -1 do
      UpdateWherePart(sql_where,x);

    setlength(sql_where,length(sql_where)-5);

    result := 'delete from ' + sql_tables + ' where ' + sql_where;
  end;

begin
  Result := False;
  with tsqlquery.Create(nil) do
    begin
    DataBase := self.Database;
    transaction := self.transaction;
    sql.clear;
    case UpdateKind of
      ukModify : s := ModifyRecQuery;
      ukInsert : s := InsertRecQuery;
      ukDelete : s := DeleteRecQuery;
    end; {case}
    sql.add(s);
    ExecSQL;
    Result := true;
    Free;
    end;
end;


Function TSQLQuery.GetCanModify: Boolean;

begin
  if FCursor.StatementType = stSelect then
    Result:= Active and  FUpdateable and (not FReadOnly)
  else
    Result := False;
end;

function TSQLQuery.GetIndexDefs : TIndexDefs;

begin
  Result := FIndexDefs;
end;

procedure TSQLQuery.SetIndexDefs(AValue : TIndexDefs);

begin
  FIndexDefs := AValue;
end;

procedure TSQLQuery.SetUpdateMode(AValue : TUpdateMode);

begin
  FUpdateMode := AValue;
end;

procedure TSQLQuery.SetSchemaInfo( SchemaType : TSchemaType; SchemaObjectName, SchemaPattern : string);

begin
  SQL.Clear;
  SQL.Add((DataBase as tsqlconnection).GetSchemaInfoSQL(SchemaType, SchemaObjectName, SchemaPattern));
end;


end.

{
  $Log$
  Revision 1.14  2005-02-14 17:13:12  peter
    * truncate log

  Revision 1.13  2005/02/07 11:23:41  joost
    - implemented TSQLQuery.SetSchemaInfo
    - added support for delete and insert

  Revision 1.12  2005/01/24 10:52:43  michael
    * Patch from Joost van der Sluis
    - Made it possible to run 'show' queries for MySQL

  Revision 1.11  2005/01/12 10:30:33  michael
   * Patch from Joost Van der Sluis:
     - implemented TSQLQuery.UpdateIndexDefs
     - implemented TSQLQuery.ReadOnly
     - implemented TSQLQuery.IndexDefs
     - implemented TSQLQuery.UpdateMode
     - implemented TSQLQuery.UsePrimaryKeyAsKey (Set pfInKey in the
       providerflags
       of fields that are in the primary index of the underlying table)
     - Added support for updates on date-fields

}
