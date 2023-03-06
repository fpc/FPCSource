{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2007 by Michael Van Canneyt, member of the
    Free Pascal development team

    MySQL 4.0 Data Dictionary Engine Implementation.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$IFNDEF FPC_DOTTEDUNITS}
unit FPDDMySQL40;
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
  { TSQLDBMySql40DDEngine }

  TSQLDBMySql40DDEngine = Class(TSQLDBDDEngine)
  Protected
    Function CreateConnection(AConnectString  : String) : TSQLConnection; override;
  Public
    Class function Description : string; override;
    Class function DBType : String; override;
  end;
  
Procedure RegisterMySQL40DDEngine;
Procedure UnRegisterMySQL40DDEngine;

implementation

{$IFDEF FPC_DOTTEDUNITS}
uses Data.SqlDb.Mysql40;
{$ELSE FPC_DOTTEDUNITS}
uses mysql40conn;
{$ENDIF FPC_DOTTEDUNITS}

procedure RegisterMySQL40DDEngine;
begin
  RegisterDictionaryEngine(TSQLDBMySQL40DDEngine);
end;

procedure UnRegisterMySQL40DDEngine;
begin
  UnRegisterDictionaryEngine(TSQLDBMySQL40DDEngine);
end;


{ TSQLDBMySql40DDEngine }

function TSQLDBMySql40DDEngine.CreateConnection(AConnectString: String
  ): TSQLConnection;
begin
  Result:={$IFDEF FPC_DOTTEDUNITS}Data.SqlDb.Mysql40{$ELSE}Mysql40conn{$ENDIF}.TMySQL40Connection.Create(Self);
end;

class function TSQLDBMySql40DDEngine.Description: string;
begin
  Result:='Mysql 4.0 connection using SQLDB';
end;

class function TSQLDBMySql40DDEngine.DBType: String;
begin
  Result:='MySQL 4.0';
end;

end.

