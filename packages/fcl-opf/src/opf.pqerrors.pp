{
    This file is part of the Free Pascal component library.
    Copyright (c) 2026 by Michael Van Canneyt
    member of the Free Pascal development team

    PostgreSQL error details for EOPF

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit Opf.PQErrors;

{ The PostgreSQL connection raises EPQDatabaseError, which does not descend
  from ESQLDatabaseError. Using this unit makes EOPF.SQLState hold the
  PostgreSQL SQLSTATE (e.g. 23505 for a unique violation). }

{$mode objfpc}{$H+}

interface

implementation

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.SysUtils, Data.SqlDb.Pq,
{$ELSE}
  SysUtils, pqconnection,
{$ENDIF}
  Opf.Types;

function PQErrorDetails(aCause: Exception; var aErrorCode: Integer; var aSQLState: String): Boolean;
begin
  Result:=aCause is EPQDatabaseError;
  if Result then
    aSQLState:=EPQDatabaseError(aCause).SQLSTATE;
end;

initialization
  OPFRegisterErrorDetailsHandler(@PQErrorDetails);
end.
