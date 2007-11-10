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
unit fpddmysql40;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, fpdatadict, fpddsqldb;
  
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

uses mysql40conn;

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
  Result:=mysql40conn.TMySQL40Connection.Create(Self);
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

