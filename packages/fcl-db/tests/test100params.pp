program test100params;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils,
  db, sqldb, pqconnection;

var
  Conn: TPQConnection;
  Tran: TSQLTransaction;
  Q: TSQLQuery;

  sql: string;
  i: integer;

begin
  Conn:=TPQConnection.Create(nil);
  Conn.HostName:='localhost';
  Conn.DatabaseName:='postgres';
  Conn.UserName:='postgres';
  Conn.Password:='postgres';

  Tran:=TSQLTransaction.Create(nil);
  Tran.DataBase:=Conn;

  Q:=TSQLQuery.Create(nil);
  Q.DataBase:=Conn;

  Conn.Open;
  writeln('Connected');

  sql:='';
  for i:=1 to 101 do
  begin
    if sql<>'' then sql:=sql+',';
    sql:=sql+format('f%d integer', [i]);
  end;
  Conn.ExecuteDirect(format('CREATE TEMPORARY TABLE t (%s)', [sql]));
  writeln('Table created');

  sql:='';
  for i:=1 to 101 do
  begin
    if sql<>'' then sql:=sql+',';
    sql:=sql+format(':f%d', [i]);
  end;
  Q.SQL.Text:=format('INSERT INTO t VALUES(%s)', [sql]);
  for i:=1 to 101 do
  begin
    Q.ParamByName('f'+inttostr(i)).AsInteger:=i;
  end;
  Q.ExecSQL;

  Q.SQL.Text:='SELECT * FROM t';
  Q.Open;
  for i:=90 to 101 do
    writeln(Q.FieldByName('f'+inttostr(i)).AsInteger, ',');
  Q.Close;

  //END
  Tran.Commit;
  Conn.Close;

  Q.Free;
  Tran.Free;
  Conn.Free;
  writeln('End. Press any key');
  readln;
end.

