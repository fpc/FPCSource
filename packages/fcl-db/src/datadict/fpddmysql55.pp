{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2007 by Michael Van Canneyt, member of the
    Free Pascal development team

    MySQL 5.5 Data Dictionary Engine Implementation.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpddmysql55;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, fpdatadict, fpddsqldb;
  
Type
  { TSQLDBMySql55DDEngine }

  TSQLDBMySql55DDEngine = Class(TSQLDBDDEngine)
  Protected
    Function CreateConnection(AConnectString  : String) : TSQLConnection; override;
  Public
    Class function Description : string; override;
    Class function DBType : String; override;
  end;

Procedure RegisterMySQL55DDEngine;
Procedure UnRegisterMySQL55DDEngine;

implementation

uses mysql55conn;

Procedure RegisterMySQL55DDEngine;

begin
  RegisterDictionaryEngine(TSQLDBMySQL55DDEngine);
end;

Procedure UnRegisterMySQL55DDEngine;

begin
  UnRegisterDictionaryEngine(TSQLDBMySQL55DDEngine);
end;

{ TSQLDBMySql55DDEngine }

function TSQLDBMySql55DDEngine.CreateConnection(AConnectString: String
  ): TSQLConnection;
begin
  Result:=mysql55conn.TMySQL55Connection.Create(Self);
end;

class function TSQLDBMySql55DDEngine.Description: string;
begin
  Result:='Mysql 5.5 connection using SQLDB';
end;

class function TSQLDBMySql55DDEngine.DBType: String;
begin
  Result:='MySQL 5.5';
end;

end.

