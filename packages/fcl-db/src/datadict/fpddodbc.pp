{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2007 by Michael Van Canneyt, member of the
    Free Pascal development team

    ODBC Data Dictionary Engine Implementation.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpddodbc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, fpdatadict, fpddsqldb;

Type
  { TSQLDBODBCDDEngine }

  TSQLDBODBCDDEngine = Class(TSQLDBDDEngine)
  Protected
    Function CreateConnection(AConnectString  : String) : TSQLConnection; override;
  Public
    Class function Description : string; override;
    Class function DBType : String; override;
  end;

Procedure RegisterODBCDDengine;
Procedure UnRegisterODBCDDengine;

implementation

uses odbcconn;

procedure RegisterODBCDDengine;
begin
  RegisterDictionaryEngine(TSQLDBODBCDDEngine);
end;

procedure UnRegisterODBCDDengine;
begin
  UnRegisterDictionaryEngine(TSQLDBODBCDDEngine);
end;

{ TSQLDBODBCDDEngine }

function TSQLDBODBCDDEngine.CreateConnection(AConnectString: String
  ): TSQLConnection;
begin
  Result:=TODBCConnection.Create(Self);
end;

class function TSQLDBODBCDDEngine.Description: string;
begin
  Result:='ODBC connection using SQLDB';
end;

class function TSQLDBODBCDDEngine.DBType: String;
begin
  Result:='ODBC';
end;

end.

