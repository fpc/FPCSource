program TestDBindexer;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, sqldb, dbIndexer, ibconnection, fbindexdb, fpindexer;

{$R *.res}

Function SetupDB : TCustomIndexDB;

Var
  IB: TFBIndexDB;

begin
  IB := TFBIndexDB.Create(nil);
  try
    IB.DatabasePath := Paramstr(2);
    IB.UserName := 'username';
    IB.Password := 'secret';
    if not FileExists(IB.DatabasePath) then
      IB.CreateDB
    else
      begin
      IB.Connect;
      IB.CreateIndexerTables;
      end;
  except
    FreeAndNil(IB);
    Raise;
  end;
  Result:=IB;
end;

Var
  DBI : TDBIndexer;
  Start : TDateTime;

begin
  If ParamCount<>2 then
    begin
    Writeln('Usage ',ExtractFileName(ParamStr(0)),' dbpath indexdbpath');
    Halt(1);
    end;
  DBI:=TIBIndexer.Create(Nil);
  try
    DBI.IndexDB:=SetupDB;
    try
      DBI.Database:=TIBConnection.Create(DBI);
      DBI.Database.Transaction:=TSQLTransaction.Create(DBI);
      DBI.Database.DatabaseName:=ParamStr(1);
      DBI.Database.UserName:='username';
      DBI.Database.Password:='SysteemD';
      DBI.Database.Connected:=True;
      DBI.SkipTables.add('AANWEZIGHEIDSREGISTER');
      DBI.SkipTables.add('EDISONZENDING');
      Start:=Now;
      DBI.IndexDatabase;
      Writeln('Finished : ',FormatDateTime('hh:nn:ss',Now-Start));
    finally
      DBI.IndexDB.Free;
    end;
  finally
    DBI.Free;
  end;
end.

