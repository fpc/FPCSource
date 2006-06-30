unit SQLDBToolsUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, toolsunit,
  db,
  sqldb, ibconnection, mysql40conn, mysql41conn, mysql50conn, pqconnection,odbcconn,oracleconnection;

const SQLDBdbTypes = [mysql40,mysql41,mysql50,postgresql,interbase,odbc,oracle];
      MySQLdbTypes = [mysql40,mysql41,mysql50];

type

{ TSQLDBConnector }

     TSQLDBConnector = class(TDBConnector)
       FConnection   : TSQLConnection;
       FTransaction  : TSQLTransaction;
       FQuery        : TSQLQuery;
     private
       procedure CreateFConnection;
       procedure CreateFTransaction;
       Function CreateQuery : TSQLQuery;
     protected
       Procedure FreeNDataset(var ds : TDataset); override;
       Function CreateNDataset(n : integer) : TDataset; override;
     public
       destructor Destroy; override;
       constructor Create;
       property Connection : TSQLConnection read FConnection;
       property Transaction : TSQLTransaction read FTransaction;
       property Query : TSQLQuery read FQuery;
     end;

implementation

procedure TSQLDBConnector.CreateFConnection;

begin
  if dbtype = mysql40 then Fconnection := tMySQL40Connection.Create(nil);
  if dbtype = mysql41 then Fconnection := tMySQL41Connection.Create(nil);
  if dbtype = mysql50 then Fconnection := tMySQL50Connection.Create(nil);
  if dbtype = postgresql then Fconnection := tpqConnection.Create(nil);
  if dbtype = interbase then Fconnection := tIBConnection.Create(nil);
  if dbtype = odbc then Fconnection := tODBCConnection.Create(nil);
  if dbtype = oracle then Fconnection := TOracleConnection.Create(nil);

  if not assigned(Fconnection) then writeln('Invalid database-type, check if a valid database-type was provided in the file ''database.ini''');

  with Fconnection do
    begin
    DatabaseName := dbname;
    UserName := dbuser;
    Password := dbpassword;
    HostName := dbhostname;
    open;
    end;
end;

{ TSQLDBConnector }

procedure TSQLDBConnector.CreateFTransaction;

begin
  Ftransaction := tsqltransaction.create(nil);
  with Ftransaction do
    database := Fconnection;
end;

Function TSQLDBConnector.CreateQuery : TSQLQuery;

begin
  Result := TSQLQuery.create(nil);
  with Result do
    begin
    database := Fconnection;
    transaction := Ftransaction;
    end;
end;

destructor TSQLDBConnector.Destroy;
begin
  try
    if Ftransaction.Active then Ftransaction.Rollback;
    Ftransaction.StartTransaction;
    Fconnection.ExecuteDirect('DROP TABLE FPDEV');
    Ftransaction.Commit;
  Except
    if Ftransaction.Active then Ftransaction.Rollback
  end;
  try
    if Ftransaction.Active then Ftransaction.Rollback;
    Ftransaction.StartTransaction;
    Fconnection.ExecuteDirect('DROP TABLE FPDEV2');
    Ftransaction.Commit;
  Except
    if Ftransaction.Active then Ftransaction.Rollback
  end;

  FreeAndNil(FQuery);
  FreeAndNil(FTransaction);
  FreeAndNil(FConnection);
  inherited Destroy;
end;

constructor TSQLDBConnector.Create;

var countID : integer;

begin
  CreateFConnection;
  CreateFTransaction;
  FQuery := CreateQuery;
  FConnection.Transaction := FTransaction;

  try
    Ftransaction.StartTransaction;
    Fconnection.ExecuteDirect('create table FPDEV (       ' +
                              '  ID INT NOT NULL,           ' +
                              '  NAME VARCHAR(50)          ' +
                              ')                            ');

    FTransaction.CommitRetaining;

    for countID := 1 to 35 do
      Fconnection.ExecuteDirect('insert into FPDEV (ID,NAME)' +
                                'values ('+inttostr(countID)+',''TestName'+inttostr(countID)+''')');

    Ftransaction.Commit;
  except
    if Ftransaction.Active then Ftransaction.Rollback
  end;
end;

function TSQLDBConnector.CreateNDataset(n: integer): TDataset;
begin
  result := CreateQuery;
  with result as TSQLQuery do
    begin
    sql.clear;
    sql.add('SELECT ID,NAME FROM FPDEV WHERE ID<'+inttostr(n+1));
    end;
end;

procedure TSQLDBConnector.FreeNDataset(var ds: TDataset);
begin
  if ds.active then ds.Close;
  FreeAndNil(ds);
end;

end.

