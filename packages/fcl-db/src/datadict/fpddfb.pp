{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2007 by Michael Van Canneyt, member of the
    Free Pascal development team

    Firebird/Interbase Data Dictionary Engine Implementation.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpddfb;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, fpdatadict, fpddsqldb;
  
Type

  { TSQLDBFBDDEngine }
  
  TSQLDBFBDDEngine = Class(TSQLDBDDEngine)
  Protected
    Function CreateConnection(AConnectString  : String) : TSQLConnection; override;
  Public
    Class function Description : string; override;
    Class function DBType : String; override;
  end;

  // Backwards compatibility
  TSQLDBIBDDEngine = TSQLDBFBDDEngine;
  

Procedure RegisterFBDDEngine;
Procedure UnRegisterFBDDEngine;

implementation

uses ibconnection;

Procedure RegisterFBDDEngine;

begin
  RegisterDictionaryEngine(TSQLDBFBDDEngine);
end;

Procedure UnRegisterFBDDEngine;

begin
  UnRegisterDictionaryEngine(TSQLDBFBDDEngine);
end;

{ TSQLDBFBDDEngine }

function TSQLDBFBDDEngine.CreateConnection(AConnectString: String
  ): TSQLConnection;
begin
  Result:=TIBConnection.Create(Self);
end;

class function TSQLDBFBDDEngine.Description: string;
begin
  Result:='Firebird/Interbase connection using SQLDB';
end;

class function TSQLDBFBDDEngine.DBType: String;
begin
  Result:='Firebird/Interbase';
end;

end.

