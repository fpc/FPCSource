{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2007 by Michael Van Canneyt, member of the
    Free Pascal development team

    MySQL 5.0 Data Dictionary Engine Implementation.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpddmysql50;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, fpdatadict, fpddsqldb;
  
Type
  { TSQLDBMySql5DDEngine }

  TSQLDBMySql5DDEngine = Class(TSQLDBDDEngine)
  Protected
    Function CreateConnection(AConnectString  : String) : TSQLConnection; override;
  Public
    Class function Description : string; override;
    Class function DBType : String; override;
  end;

Procedure RegisterMySQL50DDEngine;
Procedure UnRegisterMySQL50DDEngine;

implementation

uses mysql50conn;

Procedure RegisterMySQL50DDEngine;

begin
  RegisterDictionaryEngine(TSQLDBMySQL5DDEngine);
end;

Procedure UnRegisterMySQL50DDEngine;

begin
  UnRegisterDictionaryEngine(TSQLDBMySQL5DDEngine);
end;

{ TSQLDBMySql5DDEngine }

function TSQLDBMySql5DDEngine.CreateConnection(AConnectString: String
  ): TSQLConnection;
begin
  Result:=mysql50conn.TMySQL50Connection.Create(Self);
end;

class function TSQLDBMySql5DDEngine.Description: string;
begin
  Result:='Mysql 5.0 connection using SQLDB';
end;

class function TSQLDBMySql5DDEngine.DBType: String;
begin
  Result:='MySQL 5.0';
end;

end.

