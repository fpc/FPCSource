{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2007 by Michael Van Canneyt, member of the
    Free Pascal development team

    MySQL 5.7 Data Dictionary Engine Implementation.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$IFNDEF FPC_DOTTEDUNITS}
unit fpddmysql57;
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
  { TSQLDBMySql57DDEngine }

  TSQLDBMySql57DDEngine = Class(TSQLDBDDEngine)
  Protected
    Function CreateConnection(AConnectString  : String) : TSQLConnection; override;
  Public
    Class function Description : string; override;
    Class function DBType : String; override;
  end;

Procedure RegisterMySQL57DDEngine;
Procedure UnRegisterMySQL57DDEngine;

implementation

{$IFDEF FPC_DOTTEDUNITS}
uses Data.SqlDb.Mysql57;
{$ELSE FPC_DOTTEDUNITS}
uses mysql57conn;
{$ENDIF FPC_DOTTEDUNITS}

Procedure RegisterMySQL57DDEngine;

begin
  RegisterDictionaryEngine(TSQLDBMySQL57DDEngine);
end;

Procedure UnRegisterMySQL57DDEngine;

begin
  UnRegisterDictionaryEngine(TSQLDBMySQL57DDEngine);
end;

{ TSQLDBMySql57DDEngine }

function TSQLDBMySql57DDEngine.CreateConnection(AConnectString: String
  ): TSQLConnection;
begin
  Result:={$IFDEF FPC_DOTTEDUNITS}Data.SqlDb.Mysql57{$ELSE}mysql57conn{$ENDIF}.TMySQL57Connection.Create(Self);
end;

class function TSQLDBMySql57DDEngine.Description: string;
begin
  Result:='Mysql 5.7 connection using SQLDB';
end;

class function TSQLDBMySql57DDEngine.DBType: String;
begin
  Result:='MySQL 5.7';
end;

end.

