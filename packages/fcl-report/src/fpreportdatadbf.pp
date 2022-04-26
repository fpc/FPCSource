{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2022 by Michael van Canneyt and other members of the
    Free Pascal development team

    report data dbf

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpreportdatadbf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, dbf, fpjson, fpreportdata;


Const
  keyFileName = 'filename';

Type
  TDBFReportDataHandler = Class(TFPReportDataHandler)
    Function CreateDataset(AOwner : TComponent; AConfig : TJSONObject) : TDataset; override;
    Class Function CheckConfig(AConfig: TJSONObject): String; override;
    Class Function DataType : String; override;
    Class Function DataTypeDescription : String; override;
  end;

Resourcestring
  SErrNeedFileName = 'Need a DBF file name';
  SFileNameDoesNotExist = 'Filename does not exist: "%s"';

implementation

function TDBFReportDataHandler.CreateDataset(AOwner: TComponent; AConfig: TJSONObject): TDataset;

Var
  C : TDBF;

begin
  C:=TDBF.Create(AOWner);
  C.TableName:=AConfig.Get(KeyFileName,'');
  C.ReadOnly:=True;
  Result:=C;
end;

class function TDBFReportDataHandler.CheckConfig(AConfig: TJSONObject): String;

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

class function TDBFReportDataHandler.DataType: String;
begin
  Result:='DBF'
end;

class function TDBFReportDataHandler.DataTypeDescription: String;
begin
  Result:='DBase data file';
end;

initialization
  TDBFReportDataHandler.RegisterHandler;
end.

