{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2022 by Michael van Canneyt and other members of the
    Free Pascal development team

    Sql Types

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$IFNDEF FPC_DOTTEDUNITS}
unit SQLTypes;
{$ENDIF FPC_DOTTEDUNITS}

{$mode objfpc}
{$H+}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses System.Classes, System.SysUtils;
{$ELSE FPC_DOTTEDUNITS}
uses classes, sysutils;
{$ENDIF FPC_DOTTEDUNITS}

type
  TSchemaType = (stNoSchema, stTables, stSysTables, stProcedures, stColumns, stProcedureParams, stIndexes, stPackages, stSchemata, stSequences);


type
  TStatementType = (stUnknown, stSelect, stInsert, stUpdate, stDelete,
    stDDL, stGetSegment, stPutSegment, stExecProcedure,
    stStartTrans, stCommit, stRollback, stSelectForUpd);

  TDBEventType = (detCustom, detPrepare, detExecute, detFetch, detCommit, detRollBack, detParamValue, detActualSQL);
  TDBEventTypes = set of TDBEventType;

  TQuoteChars = array[0..1] of AnsiChar;

  TSqlObjectIdentifierList = class;

  { TSqlObjectIdenfier }

  TSqlObjectIdenfier = class(TCollectionItem)
  private
    FObjectName: String;
    FSchemaName: String;
  public
    constructor Create(ACollection: TSqlObjectIdentifierList; Const AObjectName: String; Const ASchemaName: String = '');
    function FullName: String;
    property SchemaName: String read FSchemaName write FSchemaName;
    property ObjectName: String read FObjectName write FObjectName;
  end;

  { TSqlObjectIdentifierList }

  TSqlObjectIdentifierList = class(TCollection)
  private
    function GetIdentifier(Index: integer): TSqlObjectIdenfier;
    procedure SetIdentifier(Index: integer; AValue: TSqlObjectIdenfier);
  public
    function AddIdentifier: TSqlObjectIdenfier; overload;
    function AddIdentifier(Const AObjectName: String; Const ASchemaName: String = ''): TSqlObjectIdenfier; overload;
    property Identifiers[Index: integer]: TSqlObjectIdenfier read GetIdentifier write SetIdentifier; default;
  end;


implementation

{ TSqlObjectIdenfier }

constructor TSqlObjectIdenfier.Create(ACollection: TSqlObjectIdentifierList; const AObjectName: String; const ASchemaName: String = '');

begin
  inherited Create(ACollection);
  FSchemaName:=ASchemaName;
  FObjectName:=AObjectName;
end;

function TSqlObjectIdenfier.FullName: String;

begin
  Result:=FObjectName;
  if FSchemaName<>'' then
    Result:=FSchemaName+'.'+FObjectName;
end;

{ TSqlObjectIdentifierList }

function TSqlObjectIdentifierList.GetIdentifier(Index: integer): TSqlObjectIdenfier;
begin
  Result := Items[Index] as TSqlObjectIdenfier;
end;

procedure TSqlObjectIdentifierList.SetIdentifier(Index: integer; AValue: TSqlObjectIdenfier);
begin
  Items[Index] := AValue;
end;

function TSqlObjectIdentifierList.AddIdentifier: TSqlObjectIdenfier;
begin
  Result:=Add as TSqlObjectIdenfier;
end;

function TSqlObjectIdentifierList.AddIdentifier(Const AObjectName: String;
  Const ASchemaName: String = ''): TSqlObjectIdenfier;
begin
  Result:=AddIdentifier();
  Result.SchemaName:=ASchemaName;
  Result.ObjectName:=AObjectName;
end;

end.
