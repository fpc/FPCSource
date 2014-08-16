program sqlite3loadlib;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, sysutils,
  sqldb,
  sqldblib, sqlite3conn;

var
  LibLoader: TSQLDBLibraryLoader;
  Conn: TSQlite3Connection;
  Tran: TSQLTransaction;
  Q: TSQLQuery;
  Existed: boolean;
begin
  LibLoader:=TSQLDBLibraryLoader.Create(nil);
  try
    LibLoader.ConnectionType:='SQLite3';
    LibLoader.LibraryName:='d:\auxinst\sqlite\sqlite3.dll';
    LibLoader.Enabled := true;
    LibLoader.LoadLibrary;
  finally
    LibLoader.Free;
  end;

  Conn:=TSQlite3Connection.create(nil);
  try
    Tran:=TSQLTransaction.create(nil);
    Q:=TSQLQuery.Create(nil);
    Conn.DatabaseName:='test.sqlite';
    existed:=fileexists(conn.databasename);
    Conn.Transaction:=Tran;
    Q.DataBase:=Conn;
    Conn.Open;
    Tran.StartTransaction;
    if not(existed) then
    begin
      // create test table
      Conn.ExecuteDirect('create table test (id integer, name varchar(255))');
    end;
    Q.SQL.Text:='select id,name from test ';
    Q.Open;
    Q.Last; //force recordcount update
    writeln('recordcount: '+inttostr(q.RecordCount));
    Tran.Commit;
    Q.Close;
    Conn.Close;
  finally
    Q.Free;
    Tran.Free;
    Conn.Free;
    //LibLoader.Free;
  end;
  writeln('Program complete. Press a key to continue.');
  readln;
end.
