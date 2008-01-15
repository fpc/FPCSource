{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2007 by Michael Van Canneyt, member of the
    Free Pascal development team

    Oracle Data Dictionary Engine Implementation.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit fpddoracle;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, fpdatadict, fpddsqldb;
  
Type
  { TSQLDBORACLEEngine }
  TSQLDBOracleDDEngine = Class(TSQLDBDDEngine)
  Protected
    Function CreateConnection(AConnectString  : String) : TSQLConnection; override;
  Public
    Class function Description : string; override;
    Class function DBType : String; override;
  end;

Procedure RegisterOracleDDEngine;
Procedure UnRegisterOracleDDEngine;


implementation

uses oracleconnection;

procedure RegisterOracleDDEngine;
begin
  RegisterDictionaryEngine(TSQLDBORACLEDDEngine);
end;

procedure UnRegisterOracleDDEngine;
begin
  UnRegisterDictionaryEngine(TSQLDBORACLEDDEngine);
end;


{ TSQLDBORACLEDDEngine }

function TSQLDBORACLEDDEngine.CreateConnection(AConnectString: String
  ): TSQLConnection;
begin
  Result:=TOracleConnection.Create(Self);
end;

class function TSQLDBORACLEDDEngine.Description: string;
begin
  Result:='Oracle connection using SQLDB';
end;

class function TSQLDBORACLEDDEngine.DBType: String;
begin
  Result:='Oracle';
end;

end.

