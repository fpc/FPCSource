program testWhereNULL;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils,
  db, sqldb, sqlite3conn, variants;


var
  Conn: TSQLite3Connection;
  Tran: TSQLTransaction;
  Q: TSQLQuery;

  sql: string;
  i: integer;

begin
  Conn:=TSQLite3Connection.Create(nil);
  Conn.DatabaseName:='test.db';

  Tran:=TSQLTransaction.Create(nil);
  Tran.DataBase:=Conn;

  Q:=TSQLQuery.Create(nil);
  Q.DataBase:=Conn;

  Conn.Open;
  writeln('Connected');

  Conn.ExecuteDirect('CREATE TEMPORARY TABLE t (int_field INT, string_field VARCHAR(30))');
  writeln('Temporary table created');

  Q.SQL.Text:='SELECT * FROM t';
  Q.UpdateMode:=upWhereAll; // <-- UpdateMode is upWhereAll or upWhereCahnged
  Q.Open;
  Q.AppendRecord([NULL,'a']);
  Q.AppendRecord([2,'c']);
  Q.ApplyUpdates;
  Q.Close;

  writeln('1. Bug: second row has instead of 2 in first column NULL');
  Q.Open;
  Q.Next;
  writeln('Value of ', Q.Fields[0].FieldName,' is: ', Q.Fields[0].AsString, ' expected: 2');
  Q.Close;

  writeln;
  writeln('2. Case update of record, where some value is null (upWhereAll or upWhereChanged)');
  Q.Open;
  Q.Edit;
  Q.Fields[1].AsString:='b';
  Q.Post;
  Q.ApplyUpdates;
  Q.Close;

  Q.Open;
  writeln('Value of ', Q.Fields[1].FieldName,' is: ', Q.Fields[1].AsString,' expected: b');
  Q.Close;

  writeln;
  writeln('3. Case delete of record, where some value is null (upWhereAll or upWhereChanged)');
  Q.Open;
  Q.Delete;
  Q.ApplyUpdates;
  Q.Close;

  Q.Open;
  writeln('Number of rows: ', Q.RecordCount, ' expected: 1');
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

