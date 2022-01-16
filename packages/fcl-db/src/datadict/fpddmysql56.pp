{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2007 by Michael Van Canneyt, member of the
    Free Pascal development team

    MySQL 5.6 Data Dictionary Engine Implementation.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpddmysql56;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, fpdatadict, fpddsqldb;
  
Type
  { TSQLDBMySql56DDEngine }

  TSQLDBMySql56DDEngine = Class(TSQLDBDDEngine)
  Protected
    Function CreateConnection(AConnectString  : String) : TSQLConnection; override;
  Public
    Class function Description : string; override;
    Class function DBType : String; override;
  end;

Procedure RegisterMySQL56DDEngine;
Procedure UnRegisterMySQL56DDEngine;

implementation

uses mysql56conn;

Procedure RegisterMySQL56DDEngine;

begin
  RegisterDictionaryEngine(TSQLDBMySQL56DDEngine);
end;

Procedure UnRegisterMySQL56DDEngine;

begin
  UnRegisterDictionaryEngine(TSQLDBMySQL56DDEngine);
end;

{ TSQLDBMySql56DDEngine }

function TSQLDBMySql56DDEngine.CreateConnection(AConnectString: String
  ): TSQLConnection;
begin
  Result:=mysql56conn.TMySQL56Connection.Create(Self);
end;

class function TSQLDBMySql56DDEngine.Description: string;
begin
  Result:='Mysql 5.6 connection using SQLDB';
end;

class function TSQLDBMySql56DDEngine.DBType: String;
begin
  Result:='MySQL 5.6';
end;

end.

