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
{$IFNDEF FPC_DOTTEDUNITS}
unit FPDDSQLite3;
{$ENDIF FPC_DOTTEDUNITS}

{$mode objfpc}{$H+}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Classes, System.SysUtils, Data.Sqldb, Data.Dict.Base, Data.Dict.Sqldb;
{$ELSE FPC_DOTTEDUNITS}
uses
  Classes, SysUtils, sqldb, fpdatadict, fpddsqldb;
{$ENDIF FPC_DOTTEDUNITS}
  
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

{$IFDEF FPC_DOTTEDUNITS}
uses Data.SqlDb.Sqlite3;
{$ELSE FPC_DOTTEDUNITS}
uses sqlite3conn;
{$ENDIF FPC_DOTTEDUNITS}

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

