{ demo_transactions - Implicit and explicit transaction support
  ===============================================================
  This demo shows two transaction modes in fcl-opf:

  1. IMPLICIT transactions: Each Save/Delete is automatically wrapped
     in its own transaction. If the operation fails, it rolls back.
     You don't write any transaction code.

  2. EXPLICIT transactions: You call BeginTransaction/Commit/Rollback
     to group multiple operations into a single atomic unit.
     Either all operations succeed (Commit) or none do (Rollback).

  Compile:  fpc demo_transactions.lpr -Fu../../src
  Run:      ./demo_transactions
}
program demo_transactions;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

uses
  SysUtils, Classes, db, sqldb, sqlite3conn, contnrs,
  Opf.Manager,
  Opf.Registry,
  Opf.Fields,
  Opf.Sql,
  Opf.Driver,
  Opf.Persister.Rtti;

const
  DB_FILE = 'demo_transactions.db';

type
  { TAccount - simple bank account model }
  TAccount = class(TObject, IOPPersistenceState)
  private
    FID: Integer;
    FOwner: String;
    FBalance: Currency;
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  published
    property ID: Integer read FID write FID;
    property Owner: String read FOwner write FOwner;
    property Balance: Currency read FBalance write FBalance;
  end;

function TAccount.GetPersistenceState: TObjectPersistenceState;
begin
  Result := FState;
end;

procedure TAccount.SetPersistenceState(aValue: TObjectPersistenceState);
begin
  FState := aValue;
end;

var
  Conn: TSQLite3Connection;
  SetupTrans: TSQLTransaction;

procedure ExecSQL(const aSQL: String);
var
  Q: TSQLQuery;
begin
  Q := TSQLQuery.Create(nil);
  try
    Q.Database := Conn;
    Q.Transaction := SetupTrans;
    Q.SQL.Text := aSQL;
    if not SetupTrans.Active then
      SetupTrans.StartTransaction;
    Q.ExecSQL;
    SetupTrans.Commit;
  finally
    Q.Free;
  end;
end;

procedure SetupDatabase;
var
  OpConn: TOPConnection;
begin
  if FileExists(DB_FILE) then
    DeleteFile(DB_FILE);
  Conn := TSQLite3Connection.Create(nil);
  Conn.DatabaseName := DB_FILE;
  SetupTrans := TSQLTransaction.Create(Conn);
  Conn.Transaction := SetupTrans;
  Conn.Open;

  ExecSQL('CREATE TABLE Account (ID INTEGER PRIMARY KEY, Owner TEXT, Balance REAL)');

  OPRegistry.RegisterPersister(TAccount, TRTTIPersister);
  OPSQLBuilder.SQLDialect := dSQLite;

  TOPManager.ClearInstance;
  OpConn := OPManager.Connections.Add as TOPConnection;
  OpConn.SingleConnection := Conn;
  OpConn.IsDefault := True;
end;

procedure TeardownDatabase;
begin
  TOPManager.ClearInstance;
  TOPRegistry.ClearInstance;
  if Assigned(SetupTrans) then
  begin
    SetupTrans.Free;
    SetupTrans := nil;
  end;
  if Assigned(Conn) then
  begin
    if Conn.Connected then
      Conn.Close;
    Conn.Free;
  end;
  if FileExists(DB_FILE) then
    DeleteFile(DB_FILE);
end;

{ Load an account by ID and print its balance }
procedure PrintBalance(aID: Integer);
var
  Acc: TAccount;
begin
  Acc := TAccount.Create;
  try
    Acc.ID := aID;
    OPManager.Load(Acc);
    WriteLn('  ', Acc.Owner, ': $', CurrToStr(Acc.Balance));
  finally
    Acc.Free;
  end;
end;

procedure PrintAllBalances;
var
  List: TObjectList;
  Acc: TAccount;
  I: Integer;
begin
  List := TObjectList.Create(True);
  try
    OPManager.Load(List, TAccount);
    for I := 0 to List.Count - 1 do
    begin
      Acc := TAccount(List[I]);
      WriteLn('  ', Acc.Owner, ': $', CurrToStr(Acc.Balance));
    end;
  finally
    List.Free;
  end;
end;

var
  Acc1, Acc2: TAccount;
begin
  SetupDatabase;
  try
    { Implicit transactions:
      Each Save call is automatically wrapped in a transaction.
      If the database operation succeeds, it commits. If it fails, it rolls back.
      You don't write any transaction code. }
    WriteLn('=== IMPLICIT TRANSACTIONS ===');
    WriteLn('Each Save is automatically wrapped in its own transaction.');
    WriteLn;

    Acc1 := TAccount.Create;
    try
      Acc1.ID := 1;
      Acc1.Owner := 'Alice';
      Acc1.Balance := 1000.00;
      Acc1.FState := opsNew;
      OPManager.Save(Acc1);  { Auto-wrapped in BEGIN/COMMIT }
      WriteLn('Saved Alice with $', CurrToStr(Acc1.Balance));
    finally
      Acc1.Free;
    end;

    Acc2 := TAccount.Create;
    try
      Acc2.ID := 2;
      Acc2.Owner := 'Bob';
      Acc2.Balance := 500.00;
      Acc2.FState := opsNew;
      OPManager.Save(Acc2);  { Auto-wrapped in BEGIN/COMMIT }
      WriteLn('Saved Bob with $', CurrToStr(Acc2.Balance));
    finally
      Acc2.Free;
    end;
    WriteLn;

    WriteLn('Current balances:');
    PrintAllBalances;
    WriteLn;

    { Explicit transactions:
      Use BeginTransaction/Commit to group multiple operations.
      A money transfer must debit one account and credit another atomically.
      If either operation fails, neither should persist. }
    WriteLn('=== EXPLICIT TRANSACTION: COMMIT ===');
    WriteLn('Transfer $200 from Alice to Bob.');
    WriteLn;

    OPManager.BeginTransaction;
    { Debit Alice }
    Acc1 := TAccount.Create;
    try
      Acc1.ID := 1;
      OPManager.Load(Acc1);
      Acc1.Balance := Acc1.Balance - 200.00;
      Acc1.FState := opsUpdate;
      OPManager.Save(Acc1);
    finally
      Acc1.Free;
    end;
    { Credit Bob }
    Acc2 := TAccount.Create;
    try
      Acc2.ID := 2;
      OPManager.Load(Acc2);
      Acc2.Balance := Acc2.Balance + 200.00;
      Acc2.FState := opsUpdate;
      OPManager.Save(Acc2);
    finally
      Acc2.Free;
    end;
    { Both operations succeed - commit the transaction. }
    OPManager.Commit;
    WriteLn('Transaction committed.');

    WriteLn('Balances after transfer:');
    PrintAllBalances;
    WriteLn;

    { Explicit transaction: Rollback
      Start a transaction, make changes, then roll back.
      This discards all changes made since BeginTransaction. }
    WriteLn('=== EXPLICIT TRANSACTION: ROLLBACK ===');
    WriteLn('Attempt transfer $300 from Bob to Alice, then rollback.');
    WriteLn;

    OPManager.BeginTransaction;
    { Debit Bob }
    Acc2 := TAccount.Create;
    try
      Acc2.ID := 2;
      OPManager.Load(Acc2);
      Acc2.Balance := Acc2.Balance - 300.00;
      Acc2.FState := opsUpdate;
      OPManager.Save(Acc2);
    finally
      Acc2.Free;
    end;
    { Credit Alice }
    Acc1 := TAccount.Create;
    try
      Acc1.ID := 1;
      OPManager.Load(Acc1);
      Acc1.Balance := Acc1.Balance + 300.00;
      Acc1.FState := opsUpdate;
      OPManager.Save(Acc1);
    finally
      Acc1.Free;
    end;
    { Something went wrong - rollback discards both changes. }
    OPManager.Rollback;
    WriteLn('Transaction rolled back.');

    WriteLn('Balances after rollback (unchanged):');
    PrintAllBalances;
    WriteLn;

    WriteLn('Demo completed successfully.');
  finally
    TeardownDatabase;
  end;
end.
