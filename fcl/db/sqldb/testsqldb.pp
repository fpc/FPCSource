program testsqldb;

{ A very simple example for sqldb, written by Joost van der Sluis (2004)

  Usage:
  remove the defines for the databases whose clients aren't installed, or
  the linking will fail.
  
  The following parameters are used, in given order:

    parameter1 = databasetype (mysql,interbase,postgresql - case sensitive)
    parameter2 = databasename
    parameter3 = tablename
    parameter4 = username, optional
    parameter5 = password, optional
    
  This example will only display the data for each record in the given table.
  Examples:
  
  ./testsqldb postgresql testdb fpdev
  ./testsqldb interbase /home/firebird/dbtest.fdb sysdba 123456
}

{$mode objfpc}{$H+}

{$define pqconnection}
{$define MySQLConnection}
{$define IBConnection}

uses
  Classes,
{$ifdef pqconnection}     pqconnection, {$endif}
{$ifdef MySQLConnection}  mysql4conn, {$endif}
{$ifdef IBConnection}     IBConnection, {$endif}
  sqldb;

var connection  : tSQLConnection;
    transaction : tSQLTransaction;
    query       : tSQLQuery;
    tel         : integer;
    dbtype      : string;

begin
  dbtype := paramstr(1);
{$ifdef MySQLConnection}
  if dbtype = 'mysql' then connection := tMySQLConnection.Create(nil);
{$endif}
{$ifdef pqconnection}
  if dbtype = 'postgresql' then connection := tpqConnection.Create(nil);
{$endif}
{$ifdef IBConnection}
  if dbtype = 'interbase' then connection := tIBConnection.Create(nil);
{$endif}
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

