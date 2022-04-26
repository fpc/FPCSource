{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2022 by Michael van Canneyt and other members of the
    Free Pascal development team

    report data csv

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpreportdatacsv;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, bufdataset, csvdataset, fpjson, fpreportdata;

Const
  keyFileName = 'filename';
  keyFirstLineHasFieldNames = 'firstLineHasFieldNames';
  keyCustomFieldNames = 'customFieldNames';
  keyDelimiter = 'delimiter';
  keyQuoteChar = 'quoteChar';

  DefFirstLineFieldNames = True;
  DefDelimiter = ',';
  DefQuoteChar = '"';

Type
  TCSVReportDataHandler = Class(TFPReportDataHandler)
    Function CreateDataset(AOwner : TComponent; AConfig : TJSONObject) : TDataset; override;
    Class Function CheckConfig(AConfig: TJSONObject): String; override;
    Class Function DataType : String; override;
    Class Function DataTypeDescription : String; override;
  end;

Resourcestring
  SFileNameDoesNotExist = 'Filename does not exist: "%s"';
  SErrNeedFileName = 'Need a CSV file name';

implementation


{ TCSVReportDataHandler }

Type

  { TMyCSVDataset }

  TMyCSVDataset = Class(TCSVDataset)
  private
    FCSVFileName: String;
  Protected
    function GetPacketReader(const Format: TDataPacketFormat; const AStream: TStream): TDataPacketReader; override;
    Procedure InternalOpen; override;
  Public
    Property CSVFileName : String Read FCSVFileName Write FCSVFileName;
  end;


{ TMyCSVDataset }

function TMyCSVDataset.GetPacketReader(const Format: TDataPacketFormat; const AStream: TStream): TDataPacketReader;
begin
  Result:=inherited GetPacketReader(Format, AStream);
  if (Result is TCSVDataPacketReader) and (FieldDefs.Count>0) then
     TCSVDataPacketReader(Result).CreateFieldDefs:=FieldDefs;
end;

procedure TMyCSVDataset.InternalOpen;

begin
  FileName:=CSVFileName;
  Inherited;
  FileName:='';
end;

function TCSVReportDataHandler.CreateDataset(AOwner: TComponent; AConfig: TJSONObject): TDataset;

Var
  C : TMyCSVDataset;
  A : TJSONArray;
  I : Integer;

begin
  C:=TMyCSVDataset.Create(AOWner);
  C.CSVOptions.FirstLineAsFieldNames:=AConfig.Get(keyFirstLineHasFieldNames,DefFirstLineFieldNames);
  C.CSVOptions.Delimiter:=AConfig.Get(KeyDelimiter,defDelimiter)[1];
  C.CSVOptions.quoteChar:=AConfig.Get(KeyQuoteChar,defQuoteChar)[1];
  if not C.CSVOptions.FirstLineAsFieldNames then
    begin
    A:=AConfig.Get(keyCustomFieldNames,TJSONArray(Nil));
    If Assigned(A) then
      For I:=0 to A.Count-1 do
        C.FieldDefs.Add(A.Strings[i],ftString,255);
    end;
  C.ReadOnly:=True;
  C.CSVFileName:=AConfig.Get(KeyFileName,'');
  Result:=C;
end;

class function TCSVReportDataHandler.CheckConfig(AConfig: TJSONObject): String;

Var
  FN : UTF8String;

begin
  Result:='';
  FN:=AConfig.Get(KeyFileName,'');
  if FN='' then
    Result:=SErrNeedFileName
  else if not FileExists(FN) then
    Result:=Format(SFileNameDoesNotExist,[FN]);
end;

class function TCSVReportDataHandler.DataType: String;
begin
  Result:='CSV'
end;

class function TCSVReportDataHandler.DataTypeDescription: String;
begin
  Result:='Comma-separated values text file';
end;


initialization
  TCSVReportDataHandler.RegisterHandler;
end.

