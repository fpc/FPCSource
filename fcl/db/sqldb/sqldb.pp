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

type
  TSQLConnection = class;
  TSQLTransaction = class;
  TSQLQuery = class;

  ESQLdbError = class(Exception);

  TStatementType = (stNone, stSelect, stInsert, stUpdate, stDelete,
    stDDL, stGetSegment, stPutSegment, stExecProcedure,
    stStartTrans, stCommit, stRollback, stSelectForUpd);

{ TSQLConnection }

  TSQLConnection = class (TDatabase)
  private
    FPassword            : string;
    FTransaction         : TSQLTransaction;
    FUserName            : string;
    FCharSet             : string;
    FRole                : String;
    
    procedure SetTransaction(Value : TSQLTransaction);
  protected
    procedure DoInternalConnect; override;
    procedure DoInternalDisconnect; override;
    function GetHandle : pointer; virtual; abstract;
  public
    procedure StartTransaction; override;
    procedure EndTransaction; override;
    destructor Destroy; override;
    function GetCursor : pointer; virtual; abstract;
    procedure FreeCursor(cursor : pointer); virtual; abstract;
    function GetTrans : pointer; virtual; abstract;
    procedure FreeTrans(trans : pointer); virtual; abstract;
    procedure AllocStatement(cursor : pointer); virtual; abstract;
    procedure FreeStatement(cursor : pointer); virtual; abstract;
    procedure PrepareStatement(cursor: pointer;ATransaction : TSQLTransaction;buf : string); virtual; abstract;
    procedure DescribeStatement(cursor : pointer); virtual; abstract;
    procedure AllocFldBuffers(cursor : pointer); virtual; abstract;
    procedure FreeFldBuffers(cursor : pointer); virtual; abstract;
    procedure Execute(cursor: pointer;atransaction:tSQLtransaction); virtual; abstract;
    procedure AddFieldDefs(cursor: pointer; FieldDefs : TfieldDefs); virtual; abstract;
    function GetFieldSizes(cursor : pointer) : integer; virtual; abstract;
    function Fetch(cursor : pointer) : boolean; virtual; abstract;
    procedure LoadFieldsFromBuffer(cursor : pointer;buffer : pchar); virtual; abstract;
    function GetFieldData(cursor : pointer; Field: TField; Buffer: Pointer;currbuff : pchar): Boolean; virtual; abstract;
    function GetStatementType(cursor : pointer) : tStatementType; virtual; abstract;
    function GetTransactionHandle(trans : pointer): pointer; virtual; abstract;
    function Commit(trans : pointer) : boolean; virtual; abstract;
    function RollBack(trans : pointer) : boolean; virtual; abstract;
    function StartTransaction(trans : pointer) : boolean; virtual; abstract;
    procedure CommitRetaining(trans : pointer); virtual; abstract;
    procedure RollBackRetaining(trans : pointer); virtual; abstract;

    property Handle: Pointer read GetHandle;
  published
    property Password : string read FPassword write FPassword;
    property Transaction : TSQLTransaction read FTransaction write SetTransaction;
    property UserName : string read FUserName write FUserName;
    property CharSet : string read FCharSet write FCharSet;

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

  TSQLTransaction = class (TComponent)
  private
    FTrans               : pointer;
    FAction              : TCommitRollbackAction;
    FActive              : boolean;
    FDatabase            : TSQLConnection;

    procedure SetActive(Value : boolean);
  protected
    function GetHandle : pointer; virtual;
  public
    procedure Commit; virtual;
    procedure CommitRetaining; virtual;
    procedure Rollback; virtual;
    procedure RollbackRetaining; virtual;
    procedure StartTransaction;
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    property Handle: Pointer read GetHandle;
  published
    property Action : TCommitRollbackAction read FAction write FAction;
    property Active : boolean read FActive write SetActive;
    property Database : TSQLConnection read FDatabase write FDatabase;
  end;

{ TSQLQuery }

  TSQLQuery = class (Tbufdataset)
  private
    FCursor              : pointer;
    FOpen                : Boolean;
    FTransaction         : TSQLTransaction;
    FDatabase            : TSQLConnection;
    FSQL                 : TStrings;
    FIsEOF               : boolean;
    FStatementType       : TStatementType;
    FLoadingFieldDefs    : boolean;
    FRecordSize          : Integer;

    procedure SetDatabase(Value : TSQLConnection);
    procedure SetTransaction(Value : TSQLTransaction);
    procedure AllocStatement;
    procedure FreeStatement;
    procedure PrepareStatement;
    procedure DescribeStatement;
    procedure AllocFldBuffers;
    procedure FreeFldBuffers;
    procedure Fetch;
    function LoadBuffer(Buffer : PChar): TGetResult;
    procedure GetStatementType;
    procedure SetFieldSizes;

    procedure ExecuteImmediate;
    procedure ExecuteParams;
    procedure Execute;

  protected
    // abstract & virual methods of TDataset
    function AllocRecord: PChar; override;
    procedure FreeRecord(var Buffer: PChar); override;
    function GetFieldData(Field: TField; Buffer: Pointer): Boolean; override;
    function GetNextRecord(Buffer : pchar) : TGetResult; override;
    function GetRecordSize: Word; override;
    procedure InternalAddRecord(Buffer: Pointer; AAppend: Boolean); override;
    procedure InternalClose; override;
    procedure InternalDelete; override;
    procedure InternalHandleException; override;
    procedure InternalInitFieldDefs; override;
    procedure InternalInitRecord(Buffer: PChar); override;
    procedure InternalOpen; override;
    procedure InternalPost; override;
    function IsCursorOpen: Boolean; override;
    procedure SetFieldData(Field: TField; Buffer: Pointer); override;
  public
    procedure ExecSQL; virtual;
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
  published
    property Transaction : TSQLTransaction read FTransaction write SetTransaction;
    property Database    : TSQLConnection read FDatabase write SetDatabase;
    property SQL         : TStrings read FSQL write FSQL;
    // Publish TDataset properties.
    property Active;
    property AutoCalcFields;
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
  end;

implementation

{ TSQLConnection }

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
      raise ESQLdbError.Create('Cannot assign transaction while old transaction active!');
end;

procedure TSQLConnection.DoInternalConnect;
begin
  if Connected then
    Close;
end;

procedure TSQLConnection.DoInternalDisconnect;
begin
end;

procedure TSQLConnection.StartTransaction;
begin
  if FTransaction = nil then
    raise EDatabaseError.Create('TSQLConnection.StartTransaction: Transaction not set');
  FTransaction.Active := True;
end;

procedure TSQLConnection.EndTransaction;
begin
  if FTransaction = nil then
    raise EDatabaseError.Create('TSQLConnection.EndTransaction: Transaction not set');
  FTransaction.Active := False;
end;

destructor TSQLConnection.Destroy;
begin
  if FTransaction <> nil then
  begin
    FTransaction.Active := False;
    FTransaction.Database := nil;
  end;
  inherited Destroy;
end;

{ TSQLTransaction }

procedure TSQLTransaction.SetActive(Value : boolean);
begin
  if FActive and (not Value) then
    Rollback
  else if (not FActive) and Value then
    StartTransaction;
end;

function TSQLTransaction.GetHandle: pointer;
begin
  Result := FDatabase.GetTransactionHandle(FTrans);
end;

procedure TSQLTransaction.Commit;
begin
  if not FActive then Exit;
  if FDatabase.commit(FTrans) then FActive := false;
end;

procedure TSQLTransaction.CommitRetaining;
begin
  if not FActive then Exit;
  FDatabase.commitRetaining(FTrans);
end;

procedure TSQLTransaction.Rollback;
begin
  if not FActive then Exit;
  if FDatabase.RollBack(FTrans) then FActive := false;
end;

procedure TSQLTransaction.RollbackRetaining;
begin
  if not FActive then Exit;
  FDatabase.RollBackRetaining(FTrans);
end;

procedure TSQLTransaction.StartTransaction;
begin
  if Active then Active := False;

  if FDatabase = nil then
    raise ESQLdbError.Create('TSQLTransaction.StartTransaction: Database not assigned!');

  if not Database.Connected then
    Database.Open;
  if not assigned(FTrans) then FTrans := FDatabase.GetTrans;

  if FDatabase.StartTransaction(FTrans) then FActive := true;
end;

constructor TSQLTransaction.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
end;

destructor TSQLTransaction.Destroy;
begin
  // This will also do a Rollback, if the transaction is currently active
  Active := False;
  
  Database.FreeTrans(FTrans);

  if Database <> nil then
    Database.Transaction := nil;
  inherited Destroy;
end;

{ TSQLQuery }

procedure TSQLQuery.AllocStatement;

begin
  if FDatabase = nil then
    raise ESQLdbError.Create('TSQLQuery.Allocstatement: Database not assigned!');

  if not FDatabase.Connected then
    Fdatabase.Open;
  FDatabase.AllocStatement(Fcursor);
end;

procedure TSQLQuery.SetTransaction(Value : TSQLTransaction);
begin
  CheckInactive;
  if (FTransaction <> Value) then
    FTransaction := Value;
end;

procedure TSQLQuery.SetDatabase(Value : TSQLConnection);
begin
  CheckInactive;
  if (FDatabase <> Value) then
  begin
    FDatabase := Value;
    if (FTransaction = nil) and (Assigned(FDatabase.Transaction)) then
      SetTransaction(FDatabase.Transaction);
    if assigned(fcursor) then freemem(FCursor);
    FCursor := FDatabase.getcursor;
  end;
end;

procedure TSQLQuery.FreeStatement;
begin
  FDatabase.FreeStatement(FCursor);
end;

procedure TSQLQuery.PrepareStatement;
var
  Buf : string;
  x   : integer;
begin
  if FTransaction = nil then
    raise EDatabaseError.Create('TSQLQuery.Execute: Transaction not set');
  if not FTransaction.Active then
    FTransaction.StartTransaction;

  for x := 0 to FSQL.Count - 1 do
    Buf := Buf + FSQL[x] + ' ';

  if Buf='' then
    begin
    DatabaseError('TSQLQuery: SQL statement not set');
    exit;
    end;
  FDatabase.PrepareStatement(Fcursor,FTransaction,buf);
end;

procedure TSQLQuery.DescribeStatement;
begin
  FDatabase.DescribeStatement(FCursor);
end;

procedure TSQLQuery.AllocFldBuffers;

begin
  FDatabase.AllocFldBuffers(FCursor);
end;

procedure TSQLQuery.FreeFldBuffers;
begin
  FDatabase.FreeFldBuffers(FCursor);
end;

procedure TSQLQuery.Fetch;
begin
  if not (FStatementType in [stSelect]) then
    Exit;

  FIsEof := FDatabase.Fetch(Fcursor);
end;

function TSQLQuery.LoadBuffer(Buffer : PChar): TGetResult;
begin
  Fetch;
  if FIsEOF then
  begin
    Result := grEOF;
    Exit;
  end;
  FDatabase.LoadFieldsFromBuffer(FCursor,buffer);
  Result := grOK;
end;

procedure TSQLQuery.GetStatementType;
begin
  FStatementType := FDatabase.GetStatementType(Fcursor);
end;

procedure TSQLQuery.SetFieldSizes;
begin
  FRecordSize := FDatabase.GetfieldSizes(Fcursor);
end;

procedure TSQLQuery.ExecuteImmediate;
begin
end;

procedure TSQLQuery.ExecuteParams;
begin
  //!! to be implemented
end;

procedure TSQLQuery.Execute;
begin
  if FTransaction = nil then
    raise EDatabaseError.Create('TSQLQuery.Execute: Transaction not set');
  if not FTransaction.Active then
    FTransaction.StartTransaction;
  FDatabase.execute(Fcursor,FTransaction);
end;

function TSQLQuery.AllocRecord: PChar;
begin
  writeln('AllocRecord, Recordsize:' + inttostr(FRecordSize));
  Result := AllocMem(FRecordSize);
end;

procedure TSQLQuery.FreeRecord(var Buffer: PChar);
begin
  if Assigned(@Buffer) then
    FreeMem(Buffer);
end;

function TSQLQuery.GetFieldData(Field: TField; Buffer: Pointer): Boolean;
begin
  result := FDatabase.GetFieldData(Fcursor,Field,buffer,activebuffer);
end;

function TSQLQuery.GetNextRecord(Buffer: PChar): TGetResult;
begin
  if FIsEOF then
    Result := grEof
  else
    Result := LoadBuffer(Buffer);
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
  FRecordSize := 0;
  FOpen:=False;
  inherited internalclose;
end;

procedure TSQLQuery.InternalDelete;
begin
  // not implemented - sql dataset
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
    
    FDatabase.AddFieldDefs(fcursor,FieldDefs);
  finally
    FLoadingFieldDefs := False;
  end;
end;

procedure TSQLQuery.InternalInitRecord(Buffer: PChar);
begin
  FillChar(Buffer^, FRecordSize, #0);
end;

procedure TSQLQuery.InternalOpen;
begin
  try
    AllocStatement;
    PrepareStatement;
    GetStatementType;
    if FStatementType in [stSelect] then
    begin
      DescribeStatement;
      AllocFldBuffers;
      Execute;
      FOpen:=True;
      InternalInitFieldDefs;
      if DefaultFields then
        CreateFields;
      SetFieldSizes;
      BindFields(True);
    end
    else Execute;
  except
    on E:Exception do
      raise;
  end;
  inherited InternalOpen;
end;

procedure TSQLQuery.InternalPost;
begin
  // not implemented - sql dataset
end;

function TSQLQuery.IsCursorOpen: Boolean;
begin
  Result := FOpen;
end;

procedure TSQLQuery.SetFieldData(Field: TField; Buffer: Pointer);
begin
end;

// public part

procedure TSQLQuery.ExecSQL;
begin
  AllocStatement;
  try
    PrepareStatement;
    GetStatementType;
    Execute;
  finally
    FreeStatement;
  end;
end;

constructor TSQLQuery.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FSQL := TStringList.Create;
end;

destructor TSQLQuery.Destroy;
begin
  if Active then Close;
// This gives the strangest results?
//  if assigned(Fdatabase) then FDatabase.freecursor(FCursor);
  FSQL.Free;
  inherited Destroy;
end;

function TSQLQuery.getrecordsize : Word;

begin
  result := FRecordSize;
end;

end.

{
  $Log$
  Revision 1.1  2004-08-31 09:49:47  michael
  + initial implementation of TSQLQuery

}
