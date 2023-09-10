{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2019 by the Free Pascal development team

    SQLDB REST bridge JSON input/output.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$IFNDEF FPC_DOTTEDUNITS}
unit sqldbrestjson;
{$ENDIF FPC_DOTTEDUNITS}

{$mode objfpc}{$H+}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Classes, System.SysUtils, FpJson.Data, Data.Db, FpWeb.RestBridge.IO, FpWeb.RestBridge.Schema;
{$ELSE FPC_DOTTEDUNITS}
uses
  Classes, SysUtils, fpjson, db, sqldbrestio, sqldbrestschema;
{$ENDIF FPC_DOTTEDUNITS}

Type
  { TJSONInputStreamer }

  TJSONInputStreamer = Class(TRestInputStreamer)
  private
    FJSON: TJSONData;
  Protected
    Property JSON : TJSONData Read FJSON;
  Public
    Destructor Destroy; override;
    Function SelectObject(aIndex : Integer) : Boolean; override;
    function GetContentField(aName: UTF8string): TJSONData; override;
    Function HaveInputData(aName: UTF8string): Boolean; override;
    procedure InitStreaming; override;
  end;

  { TJSONOutputStreamer }
  TJSONOutputStreamer = Class(TRestOutputStreamer)
  Private
    FJSON : TJSONObject;
    FData : TJSONArray;
    FRow: TJSONData;
  Public
    procedure EndData; override;
    procedure EndRow; override;
    procedure FinalizeOutput; override;
    procedure StartData; override;
    procedure StartRow; override;
    // Return Nil for null field.
    function FieldToJSON(aPair: TRestFieldPair): TJSONData; virtual;
    procedure WriteField(aPair: TRestFieldPair); override;
    procedure WriteMetadata(aFieldList: TRestFieldPairArray); override;
    Procedure CreateErrorContent(aCode : Integer; Const aMessage: String); override;
    Property JSON : TJSONObject Read FJSON;
    Property Data : TJSONArray Read FData;
    Property Row : TJSONData Read FRow;
  Public
    Destructor Destroy; override;
    Class Function GetContentType: String; override;
    Class Function FileExtension : String; override;
    procedure InitStreaming; override;
  end;

implementation

{$IFDEF FPC_DOTTEDUNITS}
uses System.DateUtils, FpWeb.RestBridge.Consts;
{$ELSE FPC_DOTTEDUNITS}
uses DateUtils, sqldbrestconst;
{$ENDIF FPC_DOTTEDUNITS}

{ TJSONInputStreamer }

procedure TJSONInputStreamer.InitStreaming;

Var
  Msg : String;

begin
  FreeAndNil(FJSON);
  if (Stream.Size>0) then
    begin
    try
      FJSON:=GetJSON(Stream);
    except
      On E : Exception do
        begin
        Msg:=E.Message;
        FJSON:=Nil;
        end;
    end;
    if (FJSON=Nil)  then
      Raise ESQLDBRest.CreateFmt(Statuses.GetStatusCode(rsInvalidContent),'Invalid JSON input: %s',[Msg]);
    end;
end;

destructor TJSONInputStreamer.Destroy;
begin
  FreeAndNil(FJSON);
  inherited Destroy;
end;

function TJSONInputStreamer.SelectObject(aIndex: Integer): Boolean;
begin
  Result:=(aIndex=0) and (FJSON<>Nil)  and (FJSON is TJSONObject)
end;

function TJSONInputStreamer.GetContentField(aName: UTF8string): TJSONData;

Var
  D : TJSONData;

begin
  D:=Nil;
  if Assigned(FJSON) then
    D:=(FJSON as TJSONObject).Find(aName);
  if D<>nil then
    Result:=D.Clone
  else
    Result:=nil;
end;

function TJSONInputStreamer.HaveInputData(aName: UTF8string): Boolean;
begin
  Result:=(FJSON as TJSONObject).Find(aName)<>Nil;
end;

{ TJSONOutputStreamer }


procedure TJSONOutputStreamer.EndData;
begin
  FData:=Nil;
end;

procedure TJSONOutputStreamer.EndRow;
begin
  FRow:=Nil;
end;

procedure TJSONOutputStreamer.FinalizeOutput;

Var
  S : TJSONStringType;
begin
  if ooHumanReadable in OutputOptions then
    S:=FJSON.FormatJSON()
  else
    S:=FJSON.AsJSON;
  Stream.WriteBuffer(S[1],Length(S)*SizeOf(TJSONCharType));
  FreeAndNil(FJSON);
end;

procedure TJSONOutputStreamer.StartData;
begin
  FData:=TJSONArray.Create;
  FJSON.Add(GetString(rpDataRoot),FData);
end;

procedure TJSONOutputStreamer.StartRow;
begin
  if (FRow<>Nil) then
    Raise ESQLDBRest.Create(Statuses.GetStatusCode(rsError),SErrDoubleRowStart);
  FRow:=TJSONObject.Create;
  FData.Add(FRow);
end;


Function TJSONOutputStreamer.FieldToJSON(aPair: TRestFieldPair) : TJSONData;

Var
  F : TField;

begin
  Result:=Nil;
  F:=aPair.DBField;;
  If (aPair.RestField.FieldType=rftUnknown) then
    raise ESQLDBRest.CreateFmt(Statuses.GetStatusCode(rsError),SErrUnsupportedRestFieldType, [aPair.RestField.PublicName]);
  If (F.IsNull) then
    Exit;
    Case aPair.RestField.FieldType of
      rftInteger : Result:=TJSONIntegerNumber.Create(F.AsInteger);
      rftLargeInt : Result:=TJSONInt64Number.Create(F.AsLargeInt);
      rftFloat : Result:=TJSONFloatNumber.Create(F.AsFloat);
      rftDate : Result:=TJSONString.Create(FormatDateTime(GetString(rpDateFormat),DateOf(F.AsDateTime)));
      rftTime : Result:=TJSONString.Create(FormatDateTime(GetString(rpTimeFormat),TimeOf(F.AsDateTime)));
      rftDateTime : Result:=TJSONString.Create(FormatDateTime(GetString(rpDateTimeFormat),F.AsDateTime));
      rftString : Result:=TJSONString.Create(F.AsString);
      rftBoolean : Result:=TJSONBoolean.Create(F.AsBoolean);
      rftBlob : Result:=TJSONString.Create(FieldToBase64(F));
    else
      Result:=Nil;
    end;
end;

procedure TJSONOutputStreamer.WriteField(aPair: TRestFieldPair);

Var
  D : TJSONData;
  N : UTF8String;

begin
  N:=aPair.RestField.PublicName;
  if FRow=Nil then
    Raise ESQLDBRest.CreateFmt(Statuses.GetStatusCode(rsError),SErrFieldWithoutRow,[N]);
  D:=FieldToJSON(aPair);
  if (D=Nil) and ((FRow is TJSONArray) or not HasOption(ooSparse)) then
    D:=TJSONNull.Create;
  if D<>Nil then
    If FRow is TJSONArray then
      TJSONArray(FRow).Add(D)
    else if FRow is TJSONObject then
      TJSONObject(FRow).Add(N,D);
end;

procedure TJSONOutputStreamer.WriteMetadata(aFieldList: TRestFieldPairArray);

Var
  A : TJSONArray;
  F : TJSONObject;
  P : TREstFieldPair;

begin
  A:=TJSONArray.Create;
  FJSON.Add(GetString(rpMetaDataRoot),TJSOnObject.Create([GetString(rpMetaDataFields),A]));
  For P in aFieldList do
    begin
    F:=TJSONObject.Create([GetString(rpFieldNameProp),P.RestField.PublicName,GetString(rpFieldTypeProp),typenames[P.RestField.FieldType]]);
    A.Add(F);
    Case P.RestField.FieldType of
      rftDate : F.Add(GetString(rpFieldDateFormatProp),GetString(rpDateFormat));
      rftTime : F.Add(GetString(rpFieldDateFormatProp),GetString(rpTimeFormat));
      rftDateTime : F.Add(GetString(rpFieldDateFormatProp),GetString(rpDateTimeFormat));
      rftString : F.Add(GetString(rpFieldMaxLenProp),P.DBField.Size);
    else
      ;
    end;
    end;
end;

Class function TJSONOutputStreamer.GetContentType: String;
begin
  Result:='application/json';
end;

Class Function TJSONOutputStreamer.FileExtension : String; 
begin
  Result:='.json';
end;

procedure TJSONOutputStreamer.CreateErrorContent(aCode: Integer; const aMessage: String);

Var
  ErrorObj : TJSONObject;

begin
  ErrorObj:=TJSONObject.Create([GetString(rpErrorCode),aCode,GetString(rpErrorMessage),aMessage]);
  FJSON.Add(GetString(rpErrorRoot),ErrorObj);
end;

destructor TJSONOutputStreamer.Destroy;
begin
  FreeAndNil(FJSON);
  inherited Destroy;
end;

procedure TJSONOutputStreamer.InitStreaming;
begin
  FJSON:=TJSONObject.Create;
end;

initialization
  TJSONInputStreamer.RegisterStreamer('json');
  TJSONOutputStreamer.RegisterStreamer('json');
end.

