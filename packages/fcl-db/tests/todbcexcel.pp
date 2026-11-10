{
    Manual test for TODBCConnection with a driver without transaction support (issue #40935).
    Creates an Excel workbook through the Microsoft Excel ODBC driver, edits it with TSQLQuery and reads it back.
    Usage: todbcexcel [DRIVERNAME [FILENAME]]
}
program todbcexcel;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, db, sqldb, odbcconn;

const
{$IFDEF CPU64}
  cDefaultDriver = 'Microsoft Excel Driver (*.xls, *.xlsx, *.xlsm, *.xlsb)';
{$ELSE}
  cDefaultDriver = 'Microsoft Excel Driver (*.xls)';
{$ENDIF}

var
  Failures: Integer = 0;

// Write a PASS or FAIL line for aWhat
procedure Check(aCondition: Boolean; const aWhat: String);

begin
  if aCondition then
    Writeln('PASS: ',aWhat)
  else
    begin
    Writeln('FAIL: ',aWhat);
    Inc(Failures);
    end;
end;


// Name of the person with id aID, read with a fresh query
function NameOf(aConn: TODBCConnection; aTrans: TSQLTransaction; aID: Integer): String;

var
  lQuery: TSQLQuery;

begin
  Result:='';
  lQuery:=TSQLQuery.Create(nil);
  try
    lQuery.Database:=aConn;
    lQuery.Transaction:=aTrans;
    lQuery.SQL.Text:='SELECT id, name FROM people WHERE id='+IntToStr(aID);
    lQuery.Open;
    if not lQuery.EOF then
      Result:=lQuery.FieldByName('name').AsString;
    lQuery.Close;
  finally
    lQuery.Free;
  end;
end;


var
  lConn: TODBCConnection;
  lTrans: TSQLTransaction;
  lQuery: TSQLQuery;
  lDriver, lFileName: String;
  lCount: Integer;

begin
  lDriver:=cDefaultDriver;
  if ParamCount>=1 then
    lDriver:=ParamStr(1);
  if ParamCount>=2 then
    lFileName:=ExpandFileName(ParamStr(2))
  else
    lFileName:=IncludeTrailingPathDelimiter(GetTempDir(False))+'todbcexcel.xls';
  if FileExists(lFileName) then
    DeleteFile(lFileName);
  Writeln('Driver   : ',lDriver);
  Writeln('Workbook : ',lFileName);
  lConn:=TODBCConnection.Create(nil);
  lTrans:=TSQLTransaction.Create(nil);
  lQuery:=TSQLQuery.Create(nil);
  try
    try
      lConn.Driver:=lDriver;
      lConn.Params.Add('DBQ='+lFileName);
      lConn.Params.Add('ReadOnly=0');
      lConn.Transaction:=lTrans;
      lConn.Open;
      Check(lConn.Connected,'connected');

      lTrans.StartTransaction;
      Check(lTrans.Active,'transaction started and active');
      lConn.ExecuteDirect('CREATE TABLE people (id NUMBER, name TEXT)');
      lConn.ExecuteDirect('INSERT INTO people (id, name) VALUES (1, ''Alice'')');
      lConn.ExecuteDirect('INSERT INTO people (id, name) VALUES (2, ''Bob'')');
      lConn.ExecuteDirect('INSERT INTO people (id, name) VALUES (3, ''Carol'')');
      lTrans.Commit;
      Check(not lTrans.Active,'commit after create and insert');

      lQuery.Database:=lConn;
      lQuery.Transaction:=lTrans;
      lQuery.UsePrimaryKeyAsKey:=False;
      lQuery.UpdateMode:=upWhereAll;
      lQuery.SQL.Text:='SELECT id, name FROM people';
      lQuery.Open;
      lCount:=0;
      while not lQuery.EOF do
        begin
        Inc(lCount);
        lQuery.Next;
        end;
      Check(lCount=3,'three rows read back, got '+IntToStr(lCount));
      Check(lQuery.Locate('id',2,[]),'row with id 2 found');
      lQuery.Edit;
      lQuery.FieldByName('name').AsString:='Robert';
      lQuery.Post;
      lQuery.ApplyUpdates;
      lTrans.Commit;
      Check(not lTrans.Active,'commit after ApplyUpdates');
      Check(NameOf(lConn,lTrans,2)='Robert','updated name read back');

      if not lTrans.Active then
        lTrans.StartTransaction;
      lTrans.Rollback;
      Check(not lTrans.Active,'rollback without error');
      lConn.Close;
      Check(FileExists(lFileName),'workbook file exists');
    except
      on E: Exception do
        begin
        Writeln('FAIL: exception ',E.ClassName,': ',E.Message);
        Inc(Failures);
        end;
    end;
  finally
    lQuery.Free;
    lTrans.Free;
    lConn.Free;
  end;
  if Failures=0 then
    Writeln('All checks passed')
  else
    Writeln(Failures,' check(s) failed');
  ExitCode:=Failures;
end.
