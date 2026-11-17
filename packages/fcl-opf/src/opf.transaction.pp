{
    This file is part of the Free Pascal component library.
    Copyright (c) 2026 by Michael Van Canneyt
    member of the Free Pascal development team

    Transaction management

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit Opf.Transaction;

{$mode objfpc}{$H+}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, Data.Db, Data.Sqldb,
{$ELSE}
  Classes, SysUtils, db, sqldb,
{$ENDIF}
  Opf.Types, Opf.Consts;

Type

  { What to do when a connection is registered while its own
    Connection.Transaction is already active:
    atpRaise: raise EOPFTransaction before anything is changed (default).
    atpJoin: use the active transaction. It is committed or rolled back
             at the outermost level together with the other connections,
             but it is not freed. Any pending work in it is affected. }

  TOPActiveTransactionPolicy = (atpRaise, atpJoin);

  { Called when a commit failed after other connections were committed,
    before EOPFPartialCommit is raised: e.g. to log it or start compensation.
    An exception raised by the handler is logged and ignored. }

  TOPPartialCommitEvent = procedure(Sender: TObject; E: EOPFPartialCommit) of object;

  TOPConnectionArray = array of TSQLConnection;

  { TOPFTransaction: transaction created by the handler.
    ForceInactive marks the transaction inactive after a failed rollback,
    so it can be detached from its connection and freed.
    It only changes the client side: if the server did not end the
    transaction, it stays open there until the connection is closed (SQLite
    then refuses the next BEGIN, MySQL's next START TRANSACTION commits it).
    That is why TTransactionHandler.ResetConnectionOnRollbackError closes
    and reopens the connection after a failed rollback. }

  TOPFTransaction = Class(TSQLTransaction)
  Public
    // Marks the transaction as no longer active (client side only)
    procedure ForceInactive;
  end;

  { TTransactionHandler:
    reference-counted transaction handler.
    Tracks multiple connections, each with its own transaction.
    Created per BeginTransaction scope on the manager, not a singleton.

    There is no two-phase commit: with multiple connections, the
    transactions are committed one after the other. If a commit fails
    after other connections were already committed, EOPFPartialCommit
    is raised and the remaining transactions are rolled back.

    Errors raised by a failed rollback carry the ErrorCode, SQLState and
    OriginalClassName (EOPF) of the first database error.

    After a failed rollback of a transaction the handler started, the
    connection is closed and reopened (ResetConnectionOnRollbackError), so
    that no transaction can stay open on the server. Closing also closes the
    datasets and ends the other transactions open on that connection. An
    in-memory SQLite database (see IsSQLiteMemoryDatabase) is never reset:
    closing would lose it. }

  TTransactionHandler = Class(TObject)
  Private
    FCounter: Integer;
    FRollbackPending: Boolean;
    FEntries: TFPList;          // TOPTxEntry, owned
    FWatcher: TComponent;       // Free notification for tracked connections and transactions
    FActiveTransactionPolicy: TOPActiveTransactionPolicy;
    FOnPartialCommit: TOPPartialCommitEvent;
    FSender: TObject;
    FCommitted: TOPConnectionArray;
    FResetConnectionOnRollbackError: Boolean;
    function GetConnectionCount: Integer;
    function ResetConnection(aConnection: TSQLConnection): String;
    function IndexOfConnection(aConnection: TSQLConnection): Integer;
    function ReleaseEntry(aIndex: Integer; aRollback: Boolean; aError: EOPF): String;
    function ReleaseTransactions(aFromIndex: Integer; aError: EOPF): String;
    procedure RaiseRollbackError(const aFmt, aMsg: String; aError: EOPF);
    procedure CommitTransactions;
    procedure NotifyPartialCommit(E: EOPFPartialCommit);
    procedure ConnectionFreed(aConnection: TComponent);
    procedure TransactionFreed(aTransaction: TComponent);
  Public
    // Creates a transaction handler that tracks no connections yet
    Constructor Create;
    // Rolls back any open transactions and frees the handler
    Destructor Destroy; override;
    // Starts a transaction level; levels can be nested
    procedure BeginTransaction;
    // Ends a level; at the outermost level commits all connections. True if that happened
    function Commit: Boolean;
    // Ends a level; at the outermost level rolls back all connections. True if that happened
    function Rollback: Boolean;
    // Starts a transaction on the connection, unless it is already tracked
    procedure EnsureConnection(aConnection: TSQLConnection);
    // True if a transaction level is open
    function InTransaction: Boolean;
    // True if the handler has a transaction on the given connection
    function IsTracking(aConnection: TSQLConnection): Boolean;
    // The number of open transaction levels
    Property Counter: Integer Read FCounter;
    // True if an inner level was rolled back, so the outermost commit will roll back instead
    Property RollbackPending: Boolean Read FRollbackPending;
    // The number of connections with a transaction in this handler
    Property ConnectionCount: Integer Read GetConnectionCount;
    // What to do when a connection's own transaction is already active
    Property ActiveTransactionPolicy: TOPActiveTransactionPolicy Read FActiveTransactionPolicy Write FActiveTransactionPolicy;
    // Called when a commit failed after other connections were already committed
    Property OnPartialCommit: TOPPartialCommitEvent Read FOnPartialCommit Write FOnPartialCommit;
    // Sender passed to OnPartialCommit (the manager); the handler when not set
    Property Sender: TObject Read FSender Write FSender;
    // The connections committed by the last outermost commit. After an
    // EOPFPartialCommit these are the ones whose changes are in the database.
    Property CommittedConnections: TOPConnectionArray Read FCommitted;
    // Close and reopen a connection whose rollback failed (default True).
    // False leaves the connection as it is: the server may then still have
    // the transaction open.
    Property ResetConnectionOnRollbackError: Boolean Read FResetConnectionOnRollbackError Write FResetConnectionOnRollbackError;
  end;

{ True when aConnection is a SQLite connection (TSQLite3Connection, or a
  TSQLConnector of type SQLite3) to a database that closing would lose: an
  empty DatabaseName (temporary database), ':memory:', a URI file name
  'file::memory:' or with 'mode=memory', or OpenFlags with sofMemory. }
function IsSQLiteMemoryDatabase(aConnection: TSQLConnection): Boolean;

implementation

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.TypInfo,
{$ELSE}
  TypInfo,
{$ENDIF}
  Opf.Logging;

resourcestring
  SWarnCommitReleaseFailed = 'Commit succeeded, but restoring the connection transaction failed: %s';
  SWarnConnectionReset = 'Connection %s closed and reopened after a failed rollback';
  SErrConnectionResetFailed = 'Resetting the connection after a failed rollback failed: %s';

Type

  { One tracked connection }

  TOPTxEntry = Class(TObject)
    Connection: TSQLConnection;
    Transaction: TSQLTransaction; // Our transaction, or the joined one
    Saved: TSQLTransaction;       // Original Connection.Transaction
    Owned: Boolean;               // False when joined (atpJoin)
  end;

  { Removes entries of connections that are freed while tracked }

  TOPTxWatcher = Class(TComponent)
  Private
    FHandler: TTransactionHandler;
  Protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  end;

{ TOPFTransaction }

procedure TOPFTransaction.ForceInactive;
begin
  CloseTrans;
end;

{ TOPTxWatcher }

procedure TOPTxWatcher.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation=opRemove) and Assigned(FHandler) then
    if AComponent is TSQLConnection then
      FHandler.ConnectionFreed(AComponent)
    else
      FHandler.TransactionFreed(AComponent);
end;

{ TTransactionHandler }

constructor TTransactionHandler.Create;
begin
  inherited Create;
  FCounter:=0;
  FRollbackPending:=False;
  FEntries:=TFPList.Create;
  FResetConnectionOnRollbackError:=True;
  FWatcher:=TOPTxWatcher.Create(Nil);
  TOPTxWatcher(FWatcher).FHandler:=Self;
end;

destructor TTransactionHandler.Destroy;
var
  Msg: String;
begin
  // Rollback any active transactions and restore original state.
  // A destructor must not raise, so rollback errors are only logged.
  Msg:=ReleaseTransactions(0, Nil);
  if Msg<>'' then
    OPLogger.LogWarning(Self, SErrRollbackFailed, [Msg]);
  TOPTxWatcher(FWatcher).FHandler:=Nil;
  FreeAndNil(FWatcher);
  FreeAndNil(FEntries);
  inherited Destroy;
end;

function TTransactionHandler.GetConnectionCount: Integer;
begin
  Result:=FEntries.Count;
end;

function TTransactionHandler.IndexOfConnection(aConnection: TSQLConnection): Integer;
begin
  Result:=FEntries.Count-1;
  while (Result>=0) and (TOPTxEntry(FEntries[Result]).Connection<>aConnection) do
    Dec(Result);
end;

function TTransactionHandler.IsTracking(aConnection: TSQLConnection): Boolean;
begin
  Result:=IndexOfConnection(aConnection)>=0;
end;

{ Release entry aIndex: optionally roll back, restore the original connection
  transaction and free ours. The entry is removed from the list.
  Never raises: all error messages are collected in the result. When aError
  is given, it takes the details (error code, SQL state) of the first error. }

function TTransactionHandler.ReleaseEntry(aIndex: Integer; aRollback: Boolean; aError: EOPF): String;

  procedure AddError(E: Exception);
  begin
    if Result<>'' then
      Result:=Result+LineEnding;
    Result:=Result+E.Message;
    if Assigned(aError) and (aError.OriginalClassName='') then
      aError.TakeDetails(E);
  end;

  // Stop watching aComponent, unless another entry still refers to it
  procedure Unwatch(aComponent: TComponent);
  var
    I: Integer;
    E: TOPTxEntry;
  begin
    if aComponent=Nil then
      Exit;
    for I:=0 to FEntries.Count-1 do
    begin
      E:=TOPTxEntry(FEntries[I]);
      if (E.Connection=aComponent) or (E.Saved=aComponent)
         or (not E.Owned and (E.Transaction=aComponent)) then
        Exit;
    end;
    aComponent.RemoveFreeNotification(FWatcher);
  end;

var
  Entry: TOPTxEntry;
  Trans: TSQLTransaction;
  RollbackFailed: Boolean;
  Msg: String;
begin
  Result:='';
  RollbackFailed:=False;
  Entry:=TOPTxEntry(FEntries[aIndex]);
  FEntries.Delete(aIndex);
  try
    Trans:=Entry.Transaction;
    if aRollback and Assigned(Trans) and Trans.Active then
      try
        Trans.Rollback;
      except
        on E: Exception do
        begin
          RollbackFailed:=True;
          AddError(E);
        end;
      end;
    Unwatch(Entry.Connection);
    Unwatch(Entry.Saved);
    if not Entry.Owned then
    begin
      Unwatch(Entry.Transaction);
      Exit; // Joined transaction: nothing to restore or free
    end;
    // A failed rollback leaves the transaction active, which would make
    // restoring the original transaction and freeing ours fail as well.
    if Assigned(Trans) and Trans.Active and (Trans is TOPFTransaction) then
      TOPFTransaction(Trans).ForceInactive;
    if Assigned(Entry.Connection) then
    begin
      try
        if Entry.Connection.Transaction<>Entry.Saved then
          Entry.Connection.Transaction:=Entry.Saved;
      except
        on E: Exception do
          AddError(E);
      end;
    end;
    try
      Trans.Free;
    except
      on E: Exception do
        AddError(E);
    end;
    // The server may still have the transaction open: only closing the
    // connection is sure to end it
    if RollbackFailed and FResetConnectionOnRollbackError and Assigned(Entry.Connection) then
    begin
      Msg:=ResetConnection(Entry.Connection);
      if Msg<>'' then
      begin
        if Result<>'' then
          Result:=Result+LineEnding;
        Result:=Result+Msg;
      end;
    end;
  finally
    Entry.Free;
  end;
end;

{ Roll back and release all transactions starting at aFromIndex.
  Entries before aFromIndex must already have been released.
  Returns all rollback error messages, or an empty string. }

function TTransactionHandler.ReleaseTransactions(aFromIndex: Integer; aError: EOPF): String;
var
  Msg: String;
begin
  Result:='';
  while FEntries.Count>aFromIndex do
  begin
    Msg:=ReleaseEntry(FEntries.Count-1, True, aError);
    if Msg<>'' then
    begin
      if Result<>'' then
        Result:=Result+LineEnding;
      Result:=Result+Msg;
    end;
  end;
end;

procedure TTransactionHandler.BeginTransaction;
begin
  Inc(FCounter);
end;

function ConnectionDisplayName(aConnection: TSQLConnection): String;
begin
  if aConnection=Nil then
    Result:=''
  else if aConnection.Name<>'' then
    Result:=aConnection.Name
  else
    Result:=aConnection.DatabaseName;
end;

{ True for TSQLite3Connection and descendants, without using its unit }

function IsSQLiteConnectionClass(aConnection: TSQLConnection): Boolean;
var
  C: TClass;
begin
  C:=aConnection.ClassType;
  while Assigned(C) do
  begin
    if C.ClassName='TSQLite3Connection' then
      Exit(True);
    C:=C.ClassParent;
  end;
  Result:=False;
end;

function IsSQLiteMemoryDatabase(aConnection: TSQLConnection): Boolean;
var
  DBName: String;
begin
  Result:=False;
  if aConnection=Nil then
    Exit;
  if aConnection is TSQLConnector then
  begin
    if not SameText(TSQLConnector(aConnection).ConnectorType, 'SQLite3') then
      Exit;
  end
  else if not IsSQLiteConnectionClass(aConnection) then
    Exit;
  // '' is a temporary database, deleted when it is closed
  DBName:=LowerCase(Trim(aConnection.DatabaseName));
  if (DBName='') or (DBName=':memory:') then
    Exit(True);
  // URI file names: file::memory:, file:name?mode=memory
  if (Copy(DBName, 1, 5)='file:')
     and ((Copy(DBName, 6, 8)=':memory:') or (Pos('mode=memory', DBName)>0)) then
    Exit(True);
  // TSQLite3Connection.OpenFlags with sofMemory (not reachable through a TSQLConnector)
  if IsPublishedProp(aConnection, 'OpenFlags')
     and (Pos('sofMemory', GetSetProp(aConnection, 'OpenFlags', False))>0) then
    Exit(True);
end;

{ Close and reopen aConnection. Returns an error message, or an empty string. }

function TTransactionHandler.ResetConnection(aConnection: TSQLConnection): String;
begin
  Result:='';
  if not aConnection.Connected then
    Exit;
  // Closing an in-memory SQLite database would lose all of its data
  if IsSQLiteMemoryDatabase(aConnection) then
    Exit;
  try
    aConnection.Close;
    aConnection.Open;
    OPLogger.LogWarning(Self, SWarnConnectionReset, [ConnectionDisplayName(aConnection)]);
  except
    on E: Exception do
      Result:=Format(SErrConnectionResetFailed, [E.Message]);
  end;
end;

procedure TTransactionHandler.CommitTransactions;
var
  Total, I: Integer;
  Trans: TSQLTransaction;
  Msg, ErrMsg, Failed: String;
  Committed, NotCommitted: TStringArray;
  PE: EOPFPartialCommit;
begin
  Total:=FEntries.Count;
  Committed:=Nil;
  FCommitted:=Nil;
  while FEntries.Count>0 do
  begin
    Trans:=TOPTxEntry(FEntries[0]).Transaction;
    try
      // Nil: a joined transaction that was freed while tracked
      if Assigned(Trans) and Trans.Active then
        Trans.Commit;
    except
      on E: Exception do
      begin
        // Commit failed - roll back this and the remaining transactions.
        // Names first: releasing frees the entries.
        Failed:=ConnectionDisplayName(TOPTxEntry(FEntries[0]).Connection);
        NotCommitted:=Nil;
        for I:=1 to FEntries.Count-1 do
          NotCommitted:=Concat(NotCommitted, [ConnectionDisplayName(TOPTxEntry(FEntries[I]).Connection)]);
        ErrMsg:=E.Message;
        Msg:=ReleaseTransactions(0, Nil);
        if Msg<>'' then
          ErrMsg:=ErrMsg+LineEnding+Format(SErrRollbackFailed, [Msg]);
        if Length(Committed)>0 then
        begin
          PE:=EOPFPartialCommit.CreateFmt(SErrPartialCommit, [Length(Committed), Total, ErrMsg]);
          PE.CommittedConnections:=Committed;
          PE.FailedConnection:=Failed;
          PE.NotCommittedConnections:=NotCommitted;
          PE.OriginalErrorClass:=E.ClassName;
          PE.OriginalMessage:=E.Message;
          // Error code and SQL state of the failed commit
          PE.TakeDetails(E);
          NotifyPartialCommit(PE);
          raise PE;
        end;
        E.Message:=ErrMsg;
        raise;
      end;
    end;
    Committed:=Concat(Committed, [ConnectionDisplayName(TOPTxEntry(FEntries[0]).Connection)]);
    FCommitted:=Concat(FCommitted, [TOPTxEntry(FEntries[0]).Connection]);
    // Committed, so only restores and frees. The commit stands: a failure
    // here must not be reported as a failed commit.
    Msg:=ReleaseEntry(0, False, Nil);
    if Msg<>'' then
      OPLogger.LogWarning(Self, SWarnCommitReleaseFailed, [Msg]);
  end;
end;

procedure TTransactionHandler.NotifyPartialCommit(E: EOPFPartialCommit);
var
  S: TObject;
begin
  {$IFNDEF OPF_SKIP_LOGGING}
  OPLogger.LogError(Self, E.Message);
  {$ENDIF}
  if not Assigned(FOnPartialCommit) then
    Exit;
  S:=FSender;
  if S=Nil then
    S:=Self;
  try
    FOnPartialCommit(S, E);
  except
    // The partial commit is the error to report
    on HE: Exception do
      OPLogger.LogError(Self, SErrPartialCommitHandler, [HE.Message]);
  end;
end;

function TTransactionHandler.Commit: Boolean;
var
  Msg: String;
  Err: EOPFTransaction;
begin
  if FCounter <= 0 then
    raise EOPFTransaction.Create(SErrNoActiveTransaction);
  Dec(FCounter);
  Result:=FCounter = 0;
  if FCounter > 0 then
    Exit;
  // Counter reached zero - finalize
  if FRollbackPending then
  begin
    // Doom flag set: rollback everything
    FRollbackPending:=False;
    Err:=EOPFTransaction.Create(SErrTransactionDoomedRollback);
    Msg:=ReleaseTransactions(0, Err);
    if Msg<>'' then
      RaiseRollbackError(SErrTransactionDoomedRollbackFailed, Msg, Err);
    raise Err;
  end;
  CommitTransactions;
end;

{ Raise aError (which holds the details of the first rollback error) with the
  message aFmt formatted with aMsg. }

procedure TTransactionHandler.RaiseRollbackError(const aFmt, aMsg: String; aError: EOPF);
begin
  aError.Message:=Format(aFmt, [aMsg]);
  raise aError;
end;

function TTransactionHandler.Rollback: Boolean;
var
  Msg: String;
  Err: EOPFTransaction;
begin
  if FCounter <= 0 then
    raise EOPFTransaction.Create(SErrNoActiveTransaction);
  Dec(FCounter);
  Result:=FCounter = 0;
  if FCounter > 0 then
  begin
    FRollbackPending:=True;
    Exit;
  end;
  // Counter reached zero - rollback all
  FRollbackPending:=False;
  Err:=EOPFTransaction.Create(SErrRollbackFailed);
  Msg:=ReleaseTransactions(0, Err);
  if Msg<>'' then
    RaiseRollbackError(SErrRollbackFailed, Msg, Err);
  Err.Free;
end;

{ Start a transaction on aConnection, unless it is already tracked.
  Nothing is tracked or changed on the connection unless all steps succeed. }

procedure TTransactionHandler.EnsureConnection(aConnection: TSQLConnection);
var
  Saved: TSQLTransaction;
  Trans: TOPFTransaction;
  Entry: TOPTxEntry;
begin
  if aConnection = Nil then
    Exit;
  if IndexOfConnection(aConnection) >= 0 then
    Exit;
  Saved:=aConnection.Transaction;
  if Assigned(Saved) and Saved.Active then
  begin
    if FActiveTransactionPolicy=atpRaise then
      raise EOPFTransaction.CreateFmt(SErrConnectionTransactionActive, [aConnection.Name, Saved.Name]);
    // atpJoin: use the active transaction without owning it
    Entry:=TOPTxEntry.Create;
    Entry.Connection:=aConnection;
    Entry.Transaction:=Saved;
    Entry.Saved:=Saved;
    Entry.Owned:=False;
    FEntries.Add(Entry);
    aConnection.FreeNotification(FWatcher);
    Saved.FreeNotification(FWatcher);
    Exit;
  end;
  Trans:=TOPFTransaction.Create(Nil);
  try
    Trans.Database:=aConnection;
    aConnection.Transaction:=Trans;
    if not aConnection.Connected then
      aConnection.Open;
    Trans.StartTransaction;
  except
    // Restore the original state; Trans is not active, so this cannot fail
    // because of it.
    if aConnection.Transaction=Trans then
      aConnection.Transaction:=Saved;
    Trans.Free;
    raise;
  end;
  Entry:=TOPTxEntry.Create;
  Entry.Connection:=aConnection;
  Entry.Transaction:=Trans;
  Entry.Saved:=Saved;
  Entry.Owned:=True;
  FEntries.Add(Entry);
  aConnection.FreeNotification(FWatcher);
  // Restored at the end: it must not be restored once it is freed
  if Assigned(Saved) then
    Saved.FreeNotification(FWatcher);
end;

{ A transaction we refer to is being freed: a saved one is no longer
  restored, a joined one no longer committed or rolled back. }

procedure TTransactionHandler.TransactionFreed(aTransaction: TComponent);
var
  I: Integer;
  Entry: TOPTxEntry;
begin
  for I:=0 to FEntries.Count-1 do
  begin
    Entry:=TOPTxEntry(FEntries[I]);
    if Entry.Saved=aTransaction then
      Entry.Saved:=Nil;
    if not Entry.Owned and (Entry.Transaction=aTransaction) then
      Entry.Transaction:=Nil;
  end;
end;

{ A tracked connection is being freed: TDatabase.Destroy has already ended
  and detached its transactions, so only our own objects remain to be freed. }

procedure TTransactionHandler.ConnectionFreed(aConnection: TComponent);
var
  I: Integer;
  Entry: TOPTxEntry;
begin
  for I:=FEntries.Count-1 downto 0 do
  begin
    Entry:=TOPTxEntry(FEntries[I]);
    if Entry.Connection=aConnection then
    begin
      FEntries.Delete(I);
      try
        if Entry.Owned then
        begin
          if Entry.Transaction.Active and (Entry.Transaction is TOPFTransaction) then
            TOPFTransaction(Entry.Transaction).ForceInactive;
          try
            Entry.Transaction.Free;
          except
            on E: Exception do
              OPLogger.LogWarning(Self, SErrRollbackFailed, [E.Message]);
          end;
        end;
      finally
        Entry.Free;
      end;
    end;
  end;
end;

function TTransactionHandler.InTransaction: Boolean;
begin
  Result:=FCounter > 0;
end;

end.
