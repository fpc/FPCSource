{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2007 by Michael Van Canneyt, member of the
    Free Pascal development team

    SQLite3 Data Dictionary Engine Implementation.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpddsqlite3;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, fpdatadict, fpddsqldb;
  
Type
  { TSQLDBSQLite3DDEngine }

  TSQLDBSQLite3DDEngine = Class(TSQLDBDDEngine)
  Protected
    Function CreateConnection(AConnectString  : String) : TSQLConnection; override;
  Public
    Class function Description : string; override;
    Class function DBType : String; override;
  end;

Procedure RegisterSQLite3DDEngine;
Procedure UnRegisterSQLite3DDEngine;

implementation

uses sqlite3conn;

procedure RegisterSQLite3DDEngine;
begin
  RegisterDictionaryEngine(TSQLDBSQLITE3DDEngine);
end;

procedure UnRegisterSQLite3DDEngine;
begin
  UnRegisterDictionaryEngine(TSQLDBSQLITE3DDEngine);
end;

{ TSQLDBSQLite3DDEngine }

function TSQLDBSQLite3DDEngine.CreateConnection(AConnectString: String
  ): TSQLConnection;
begin
  Result:=TSQLITE3Connection.Create(Self);
end;

class function TSQLDBSQLite3DDEngine.Description: string;
begin
  Result:='SQLite 3 database using SQLDB';
end;

class function TSQLDBSQLite3DDEngine.DBType: String;
begin
  Result:='SQLITE3';
end;

end.

