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
unit fpddpq;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, fpdatadict, fpddsqldb;

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

uses pqconnection;

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

