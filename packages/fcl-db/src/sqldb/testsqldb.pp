program testsqldb;

{ A very simple example for sqldb, written by Joost van der Sluis (2004)

  The following parameters are used, in given order:

    parameter1 = databasetype (mysql,interbase,postgresql - case sensitive)
    parameter2 = databasename
    parameter3 = tablename
    parameter4 = username, optional
    parameter5 = password, optional

  This example will only display the data for each record in the given table.
  Examples:

  ./testsqldb postgresql testdb fpdev
  ./testsqldb interbase /home/firebird/dbtest.fdb fpdev sysdba 123456
}

{$mode objfpc}{$H+}

uses
  Classes,
  pqconnection,
  mysql51conn,
  IBConnection,
  sqlite3conn,
  sqldb;

var connection  : tSQLConnection;
    transaction : tSQLTransaction;
    query       : tSQLQuery;
    tel         : integer;
    dbtype      : string;

begin
  dbtype := paramstr(1);
  if dbtype = 'mysql' then connection := tMySQL51Connection.Create(nil);
  if dbtype = 'postgresql' then connection := tpqConnection.Create(nil);
  if dbtype = 'interbase' then connection := tIBConnection.Create(nil);
  if dbtype = 'sqlite3' then connection := tSQLite3Connection.Create(nil);

  if not assigned(connection) then exit; // probably an invalid database type given

  connection.DatabaseName := paramstr(2);
  connection.UserName := paramstr(4);
  connection.Password := paramstr(5);
  connection.open;

  transaction := tsqltransaction.create(nil);
  transaction.database := connection;

  query := tsqlquery.Create(nil);
  query.DataBase := connection;
  query.transaction := transaction;
  with query do
    begin
      SQL.clear;
      sql.add('select * from ' + paramstr(3));
      ReadOnly := True; // If the query is writeable, a transaction must be assigned
                        // to the database.
      open;

      while not eof do
        begin
        for tel := 0 to query.FieldCount -1 do
          write(fields[tel].asstring+' ');
        writeln;
        next;
        end;

      close;
      query.free;
    end;
  transaction.free;
  connection.free;
end.
