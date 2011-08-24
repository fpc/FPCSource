(******************************************************************************
 *                                                                            *
 *  (c) 2005 CNOC v.o.f.                                                      *
 *                                                                            *
 *  File:        aListTables.pp                                               *
 *  Author:      Joost van der Sluis (joost@cnoc.nl)                          *
 *  Description: SQLDB example and test program                               *
 *  License:     GPL                                                          *
 *                                                                            *
 ******************************************************************************)

program aListTables;

{$mode objfpc}{$H+}

uses
  Classes,
  sqldb, pqconnection, mysql40conn, mysql41conn, mysql50conn, IBConnection, ODBCConn,
  SqldbExampleUnit;

var Tables : TStringList;
    i      : integer;

begin
  ReadIniFile;

// create FConnection
  if dbtype = 'mysql40' then Fconnection := tMySQL40Connection.Create(nil);
  if dbtype = 'mysql41' then Fconnection := tMySQL41Connection.Create(nil);
  if dbtype = 'mysql50' then Fconnection := tMySQL50Connection.Create(nil);
  if dbtype = 'postgresql' then Fconnection := tpqConnection.Create(nil);
  if dbtype = 'interbase' then Fconnection := tIBConnection.Create(nil);
  if dbtype = 'odbc' then Fconnection := tODBCConnection.Create(nil);

  if not assigned(Fconnection) then ExitWithError('Invalid database-type, check if a valid database-type was provided in the file ''database.ini''');

  with Fconnection do
    begin
      if dbhost<>'' then
        hostname:=dbhost;
      DatabaseName := dbname;
      UserName := dbuser;
      Password := dbpassword;
      open;
    end;

// create FTransaction
  Ftransaction := tsqltransaction.create(nil);
  with Ftransaction do
    begin
    database := Fconnection;
    StartTransaction;
    end;

  Fconnection.Transaction := Ftransaction;

  Tables := TStringList.Create;
  Fconnection.GetTableNames(Tables);
  for i := 0 to Tables.Count -1 do writeln(Tables[i]);

  Tables.Free;
  Ftransaction.Free;
  Fconnection.Free;
end.
