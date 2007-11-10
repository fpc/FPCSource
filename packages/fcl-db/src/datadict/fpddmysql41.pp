{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2007 by Michael Van Canneyt, member of the
    Free Pascal development team

    MySQL 4.1 Data Dictionary Engine Implementation.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpddmysql41;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, fpDataDict,fpddsqldb;
  
Type
  { TSQLDBMySql41DDEngine }

  TSQLDBMySql41DDEngine = Class(TSQLDBDDEngine)
  Protected
    Function CreateConnection(AConnectString  : String) : TSQLConnection; override;
  Public
    Class function Description : string; override;
    Class function DBType : String; override;
  end;

Procedure RegisterMySQL41DDEngine;
Procedure UnRegisterMySQL41DDEngine;

implementation

uses mysql41conn;

Procedure RegisterMySQL41DDEngine;

begin
  RegisterDictionaryEngine(TSQLDBMySQL41DDEngine);
end;

Procedure UnRegisterMySQL41DDEngine;

begin
  UnRegisterDictionaryEngine(TSQLDBMySQL41DDEngine);
end;

{ TSQLDBMySql41DDEngine }

function TSQLDBMySql41DDEngine.CreateConnection(AConnectString: String
  ): TSQLConnection;
begin
  Result:=mysql41conn.TMySQL41Connection.Create(Self);
end;

class function TSQLDBMySql41DDEngine.Description: string;
begin
  Result:='Mysql 4.1 connection using SQLDB';
end;

class function TSQLDBMySql41DDEngine.DBType: String;
begin
  Result:='MySQL 4.1';
end;

end.

