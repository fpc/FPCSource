{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2007 by Michael Van Canneyt, member of the
    Free Pascal development team

    Postgresql Data Dictionary Engine Implementation.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$IFNDEF FPC_DOTTEDUNITS}
unit FPDDPQ;
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
  { TSQLDBPostGreSQLDDEngine }

  TSQLDBPostGreSQLDDEngine = Class(TSQLDBDDEngine)
  Protected
    Function CreateConnection(AConnectString  : String) : TSQLConnection; override;
  Public
    Class function Description : string; override;
    Class function DBType : String; override;
  end;

Procedure RegisterPostgreSQLDDengine;
Procedure UnRegisterPostgreSQLDDengine;

implementation

{$IFDEF FPC_DOTTEDUNITS}
uses Data.SqlDb.Pq;
{$ELSE FPC_DOTTEDUNITS}
uses pqconnection;
{$ENDIF FPC_DOTTEDUNITS}

procedure RegisterPostgreSQLDDengine;
begin
  RegisterDictionaryEngine(TSQLDBPostGreSQLDDEngine);
end;

procedure UnRegisterPostgreSQLDDengine;
begin
  UnRegisterDictionaryEngine(TSQLDBPostGreSQLDDEngine);
end;

{ TSQLDBPostGreSQLDDEngine }

function TSQLDBPostGreSQLDDEngine.CreateConnection(AConnectString: String
  ): TSQLConnection;
begin
  Result:=TPQConnection.Create(Self);
end;

class function TSQLDBPostGreSQLDDEngine.Description: string;
begin
  Result:='PostGreSQL using SQLDB';
end;

class function TSQLDBPostGreSQLDDEngine.DBType: String;
begin
  Result:='PostGreSQL';
end;

end.

