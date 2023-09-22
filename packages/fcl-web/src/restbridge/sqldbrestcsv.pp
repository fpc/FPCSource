{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2019 by the Free Pascal development team

    SQLDB REST CSV input/output

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$IFNDEF FPC_DOTTEDUNITS}
unit sqldbrestcsv;
{$ENDIF FPC_DOTTEDUNITS}

{$mode objfpc}{$H+}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Classes, System.SysUtils, FpWeb.RestBridge.IO, FpJson.Data, FpWeb.RestBridge.Schema, Fcl.Csv.ReadWrite;
{$ELSE FPC_DOTTEDUNITS}
uses
  Classes, SysUtils, sqldbrestio, fpjson, sqldbrestschema, csvreadwrite;
{$ENDIF FPC_DOTTEDUNITS}

Type
  { TCSVInputStreamer }

  TCSVInputStreamer = Class(TRestInputStreamer)
  private
    FCSV: TCSVParser;
    FValues,
    FFields : TStrings;
  Protected
    Property CSV : TCSVParser Read FCSV;
  Public
    Destructor Destroy; override;
    Function SelectObject(aIndex : Integer) : Boolean; override;
    function GetContentField(aName: UTF8string): TJSONData; override;
    procedure InitStreaming; override;
  end;

  { TCSVOutputStreamer }
  TCSVOutputStreamer = Class(TRestOutputStreamer)
  Private
    FCSV : TCSVBuilder;
    FField : integer;
    FRow : Integer;
  Public
    procedure EndData; override;
    procedure EndRow; override;
    procedure FinalizeOutput; override;
    procedure StartData; override;
    procedure StartRow; override;
    // Return Nil for null field.
    procedure WriteField(aPair: TRestFieldPair); override;
    procedure WriteMetadata(aFieldList: TRestFieldPairArray); override;
    Procedure CreateErrorContent(aCode : Integer; Const aMessage: String); override;
    Property CSV : TCSVBuilder Read FCSV;
  Public
    Destructor Destroy; override;
    Class Function GetContentType: String; override;
    Class Function FileExtension : String; override;
    procedure InitStreaming; override;
  end;

implementation

{$IFDEF FPC_DOTTEDUNITS}
uses System.DateUtils;
{$ELSE FPC_DOTTEDUNITS}
uses DateUtils;
{$ENDIF FPC_DOTTEDUNITS}

{ TCSVInputStreamer }

procedure TCSVInputStreamer.InitStreaming;

begin
  FreeAndNil(FCSV);
  FreeAndNil(FFields);
  FCSV:=TCSVParser.Create;
  FCSV.SetSource(Stream);
  FCSV.QuoteChar:='"';
  FCSV.Delimiter:=',';
  FCSV.LineEnding:=LineEnding;//
  FFields:=TStringList.Create;
  FValues:=TStringList.Create;
  While FCSV.ParseNextCell and (FCSV.CurrentRow=0) do
    FFields.Add(FCSV.CurrentCellText);
end;

destructor TCSVInputStreamer.Destroy;
begin
  FreeAndNil(FCSV);
  FreeAndNil(FValues);
  FreeAndNil(FFields);
  inherited Destroy;
end;

function TCSVInputStreamer.SelectObject(aIndex: Integer): Boolean;

begin
  Result:=(aIndex=0) and (FCSV<>Nil) and (FCSV.CurrentRow=1);
  if Not Result then
    exit;
  Repeat
   // We are on the first cell
   FValues.Add(FCSV.CurrentCellText);
  until Not (FCSV.ParseNextCell) or (FCSV.CurrentRow=2);
end;

function TCSVInputStreamer.GetContentField(aName: UTF8string): TJSONData;

Var
  Idx : Integer;

begin
  Idx:=FFields.IndexOf(aName);
  if (Idx>=0) and (Idx<FValues.Count) then
    Result:=TJSONString.Create(FValues[Idx])
  else
    Result:=nil;
end;

{ TCSVOutputStreamer }


procedure TCSVOutputStreamer.EndData;
begin
  FRow:=0;
end;

procedure TCSVOutputStreamer.EndRow;
begin
  if FField=0 then exit;
  inc(FRow);
  FCSV.AppendRow;
  FField:=0;
end;

procedure TCSVOutputStreamer.FinalizeOutput;


begin
  // Nothing needs to be done.
  FreeAndNil(FCSV);
end;

procedure TCSVOutputStreamer.StartData;
begin
  FRow:=0;
end;

procedure TCSVOutputStreamer.StartRow;
begin
  Inc(FRow);
end;

procedure TCSVOutputStreamer.WriteField(aPair: TRestFieldPair);

Var
  S : UTF8String;

begin
  S:=FieldToString(aPair.RestField.FieldType,aPair.DBField);
  FCSV.AppendCell(S);
  Inc(FField);
end;

procedure TCSVOutputStreamer.WriteMetadata(aFieldList: TRestFieldPairArray);

Var
  P : TREstFieldPair;

begin
  For P in aFieldList do
    FCSV.AppendCell(P.RestField.PublicName);
  FCSV.AppendRow;
end;

Class function TCSVOutputStreamer.GetContentType: String;
begin
  Result:='text/csv';
end;

Class Function TCSVOutputStreamer.FileExtension : String; 
begin
  Result:='.csv';
end;

procedure TCSVOutputStreamer.CreateErrorContent(aCode: Integer; const aMessage: String);

Var
  S : String;

begin
  S:=Format('<html><title>Error %d: %s</title>',[aCode,aMessage]);
  S:=S+Format('<body><h1>Error %d : %s</h1></body></html>',[aCode,aMessage]);
  Stream.WriteBuffer(S[1],Length(S));
end;

destructor TCSVOutputStreamer.Destroy;
begin
  FreeAndNil(FCSV);
  inherited Destroy;
end;

procedure TCSVOutputStreamer.InitStreaming;
begin
  FCSV:=TCSVBuilder.Create;
  FCSV.SetOutput(Stream);
  FCSV.QuoteChar:='"';
  FCSV.Delimiter:=',';
  FCSV.QuoteOuterWhitespace:=True;
end;

initialization
  TCSVInputStreamer.RegisterStreamer('CSV');
  TCSVOutputStreamer.RegisterStreamer('CSV');
end.

