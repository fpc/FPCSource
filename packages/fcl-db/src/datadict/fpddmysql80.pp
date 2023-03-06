{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2007 by Michael Van Canneyt, member of the
    Free Pascal development team

    MySQL 8.0 Data Dictionary Engine Implementation.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$IFNDEF FPC_DOTTEDUNITS}
unit fpddmysql80;
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
  { TSQLDBMySql80DDEngine }

  TSQLDBMySql80DDEngine = Class(TSQLDBDDEngine)
  Protected
    Function CreateConnection(AConnectString  : String) : TSQLConnection; override;
  Public
    Class function Description : string; override;
    Class function DBType : String; override;
  end;

Procedure RegisterMySQL80DDEngine;
Procedure UnRegisterMySQL80DDEngine;

implementation

{$IFDEF FPC_DOTTEDUNITS}
uses Data.SqlDb.Mysql80;
{$ELSE FPC_DOTTEDUNITS}
uses mysql80conn;
{$ENDIF FPC_DOTTEDUNITS}

Procedure RegisterMySQL80DDEngine;

begin
  RegisterDictionaryEngine(TSQLDBMySQL80DDEngine);
end;

Procedure UnRegisterMySQL80DDEngine;

begin
  UnRegisterDictionaryEngine(TSQLDBMySQL80DDEngine);
end;

{ TSQLDBMySql80DDEngine }

function TSQLDBMySql80DDEngine.CreateConnection(AConnectString: String
  ): TSQLConnection;
begin
  Result:={$IFDEF FPC_DOTTEDUNITS}Data.SqlDb.Mysql80{$ELSE}mysql80conn{$ENDIF}.TMySQL80Connection.Create(Self);
end;

class function TSQLDBMySql80DDEngine.Description: string;
begin
  Result:='Mysql 8.0 connection using SQLDB';
end;

class function TSQLDBMySql80DDEngine.DBType: String;
begin
  Result:='MySQL 8.0';
end;

end.

