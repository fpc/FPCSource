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
unit fpddmysql57;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, fpdatadict, fpddsqldb;
  
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

uses mysql57conn;

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
  Result:=mysql57conn.TMySQL57Connection.Create(Self);
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

