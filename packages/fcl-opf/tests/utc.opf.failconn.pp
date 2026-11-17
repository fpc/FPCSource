unit utc.opf.failconn;

{ SQLite connection that can be told to fail at specific points,
  used to test error recovery of the transaction handler and the manager. }

{$mode objfpc}{$H+}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, Data.Db, Data.Sqldb,
  Data.SqlDb.Sqlite3;
{$ELSE}
  Classes, SysUtils, db, sqldb,
  sqlite3conn;
{$ENDIF}

Type

  EFailingConnection = Class(EDatabaseError);

  { TFailingSQLite3Connection }

  TFailingSQLite3Connection = Class(TSQLite3Connection)
  Private
    FFailCommit: Boolean;
    FFailCommitAfter: Integer;
    FFailConnect: Boolean;
    FFailRollback: Boolean;
    FFailRollbackKeepOpen: Boolean;
    FFailStart: Boolean;
    FCommitCount: Integer;
  Protected
    procedure DoInternalConnect; override;
    function Commit(trans : TSQLHandle) : boolean; override;
    function RollBack(trans : TSQLHandle) : boolean; override;
    function StartDBTransaction(trans : TSQLHandle; aParams : string) : boolean; override;
  Public
    // Raise on connect
    Property FailConnect: Boolean Read FFailConnect Write FFailConnect;
    // Raise when a transaction is started
    Property FailStart: Boolean Read FFailStart Write FFailStart;
    // Raise on commit, without committing
    Property FailCommit: Boolean Read FFailCommit Write FFailCommit;
    // When > 0: commits succeed until this many were done, then fail
    Property FailCommitAfter: Integer Read FFailCommitAfter Write FFailCommitAfter;
    // Roll back, then raise: the transaction object stays active
    Property FailRollback: Boolean Read FFailRollback Write FFailRollback;
    // Raise without rolling back: the transaction stays open in the database
    Property FailRollbackKeepOpen: Boolean Read FFailRollbackKeepOpen Write FFailRollbackKeepOpen;
    Property CommitCount: Integer Read FCommitCount;
  end;

implementation

{ TFailingSQLite3Connection }

procedure TFailingSQLite3Connection.DoInternalConnect;
begin
  if FFailConnect then
    raise EFailingConnection.Create('Simulated connect failure');
  inherited DoInternalConnect;
end;

function TFailingSQLite3Connection.Commit(trans: TSQLHandle): boolean;
begin
  if FFailCommit then
    raise EFailingConnection.Create('Simulated commit failure');
  if (FFailCommitAfter>0) and (FCommitCount>=FFailCommitAfter) then
    raise EFailingConnection.Create('Simulated commit failure');
  Result:=inherited Commit(trans);
  Inc(FCommitCount);
end;

function TFailingSQLite3Connection.RollBack(trans: TSQLHandle): boolean;
begin
  if FFailRollbackKeepOpen then
    raise EFailingConnection.Create('Simulated rollback failure, transaction left open');
  Result:=inherited RollBack(trans);
  if FFailRollback then
    raise EFailingConnection.Create('Simulated rollback failure');
end;

function TFailingSQLite3Connection.StartDBTransaction(trans: TSQLHandle; aParams: string): boolean;
begin
  if FFailStart then
    raise EFailingConnection.Create('Simulated start transaction failure');
  Result:=inherited StartDBTransaction(trans, aParams);
end;

end.
