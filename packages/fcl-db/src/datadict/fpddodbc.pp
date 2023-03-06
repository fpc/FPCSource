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
{$IFNDEF FPC_DOTTEDUNITS}
unit FPDDODBC;
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

{$IFDEF FPC_DOTTEDUNITS}
uses Data.SqlDb.Odbc;
{$ELSE FPC_DOTTEDUNITS}
uses odbcconn;
{$ENDIF FPC_DOTTEDUNITS}

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

