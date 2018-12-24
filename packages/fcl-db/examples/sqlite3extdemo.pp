program test;

{$mode objfpc}{$H+}

uses
  sysutils,
  sqlite3conn,
  sqlite3ext,
  sqldb;

const
  SharedPrefix = {$ifdef mswindows}''{$else}'lib'{$endif};

var
  con: TSQLite3Connection;
  trans: TSQLTransaction;
  q: TSQLQuery;
begin
  con := TSQLite3Connection.Create(nil);
  trans := TSQLTransaction.Create(con);
  q := TSQLQuery.Create(con);
  try
    trans.DataBase := con;
    q.DataBase := con;
    q.Transaction := trans;
    con.DatabaseName := 'test.sqlite3';
    con.Open;
    con.LoadExtension(ExtractFilePath(ParamStr(0)) +
      SharedPrefix + 'myext.' + SharedSuffix);
    q.SQL.Text := 'SELECT mysum(2, 3);';
    q.Open;
    WriteLn('MYSUM: ', q.Fields[0].AsInteger); // prints "MYSUM: 5"
    q.Close;
    q.SQL.Text := 'SELECT myconcat(''abc'', ''123'');';
    q.Open;
    WriteLn('MYCONCAT: ', q.Fields[0].AsString); // prints "MYCONCAT: abc123"
  finally
    con.Free;
  end;
end.
