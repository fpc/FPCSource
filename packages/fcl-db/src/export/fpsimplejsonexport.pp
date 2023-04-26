unit fpsimplejsonexport;
{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2022 by Michael van Canney and other members of the
    Free Pascal development team

    Simple Json Export code

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, fpDBExport;
  
Type
  TJSONRowFormat = (rfArray,rfObject);
  TJSONColumnFormat = (cfObject,cfArray);

  { TSimpleJSONFormatSettings }

  TSimpleJSONFormatSettings = Class(TExportFormatSettings)
  private
    FColumnFormat: TJSONColumnFormat;
    FIndentSize: Integer;
    FRowElementName: String;
    FRowFormat: TJSONRowFormat;
  Public
    Procedure Assign(Source : TPersistent); override;
  Published
    Property RowElementName : String Read FRowElementName Write FRowElementName;
    Property RowFormat : TJSONRowFormat Read FRowFormat Write FRowFormat;
    Property ColumnFormat : TJSONColumnFormat Read FColumnFormat Write FColumnFormat;
    Property IndentSize : Integer Read FIndentSize Write FIndentSize;
  end;

  { TCustomSimpleJSONExporter }

  TCustomSimpleJSONExporter = Class(TCustomFileExporter)
  Private
    FCurrentRow : String;
    FIndent : String;
    FIS : Integer;
    FREN : String;
    FCF : TJSONColumnFormat;
    FRF : TJSONRowFormat;
    FRC : Int64;
    procedure DecIndent;
    function GetJSONFormatsettings: TSimpleJSONFormatSettings;
    procedure IncIndent;
    procedure OutputRow(const ARow: String);
    procedure SetJSONFormatSettings(const AValue: TSimpleJSONFormatSettings);
    function TextString(const S: String): String;
  Protected
    Function  CreateFormatSettings : TCustomExportFormatSettings; override;
    Procedure DoBeforeExecute; override;
    Procedure DoAfterExecute; override;
    Procedure DoDataRowStart; override;
    Procedure DoDataHeader; override;
    Procedure DoDataFooter; override;
    Procedure ExportField(EF : TExportFieldItem); override;
    Procedure DoDataRowEnd; override;
  Public
    Property FormatSettings : TSimpleJSONFormatSettings Read GetJSONFormatsettings Write SetJSONFormatSettings;
  end;

  TSimpleJSONExporter = Class(TCustomSimpleJSONExporter)
  Published
    Property FileName;
    Property Dataset;
    Property ExportFields;
    Property FromCurrent;
    Property RestorePosition;
    Property FormatSettings;
    Property OnExportRow;
  end;

Procedure RegisterSimpleJSONExportFormat;
Procedure UnRegisterSimpleJSONExportFormat;

Const
  SSimpleJSON            = 'SimpleJSON';
  SSimpleJSONExtensions  = '.json';
  
Resourcestring
  SSimpleJSONDescription = 'Simple ASCII JSON file';

implementation


{ TSimpleJSONFormatSettings }

procedure TSimpleJSONFormatSettings.Assign(Source: TPersistent);

Var
  FS : TSimpleJSONFormatSettings;

begin
  if (Source is TSimpleJSONFormatSettings) then
    begin
    FS:=Source as TSimpleJSONFormatSettings;
    FColumnFormat:=FS.FColumnFormat;
    FRowElementName:=FS.FRowElementName;
    FRowFormat:=FS.FRowFormat;
    FIndentSize:=FS.IndentSize;
    end;
  inherited Assign(Source);
end;

{ TCustomSimpleJSONExporter }

procedure TCustomSimpleJSONExporter.DecIndent;
begin
  If (FIS>0) and (length(FIndent)>=FIS) then
    Delete(FIndent,1,FIS);
end;

function TCustomSimpleJSONExporter.GetJSONFormatsettings: TSimpleJSONFormatSettings;
begin
  Result:=TSimpleJSONFormatSettings(Inherited formatsettings)
end;

procedure TCustomSimpleJSONExporter.IncIndent;
begin
  If FIS>0 then
    FIndent:=FIndent+StringOfChar(' ',FIS);
end;

procedure TCustomSimpleJSONExporter.OutputRow(const ARow: String);
begin
  Writeln(TextFile,FIndent,ARow);
end;

procedure TCustomSimpleJSONExporter.SetJSONFormatSettings(
  const AValue: TSimpleJSONFormatSettings);
begin
  Inherited FormatSettings.Assign(AValue);
end;

function TCustomSimpleJSONExporter.TextString(const S: String): String;

Var
  I,J,L : Integer;
  P : Pchar;

begin
  I:=1;
  J:=1;
  Result:='';
  L:=Length(S);
  P:=PChar(S);
  While I<=L do
    begin
    if (P^ in ['"','/','\',#8,#9,#10,#12,#13]) then
      begin
      Result:=Result+Copy(S,J,I-J);
      Case P^ of
        '\' : Result:=Result+'\\';
        '/' : Result:=Result+'\/';
        '"' : Result:=Result+'\"';
        #8  : Result:=Result+'\b';
        #9  : Result:=Result+'\t';
        #10 : Result:=Result+'\n';
        #12 : Result:=Result+'\f';
        #13 : Result:=Result+'\r';
      end;
      J:=I+1;
      end;
    Inc(I);
    Inc(P);
    end;
  Result:=Result+Copy(S,J,I-1);
end;

function TCustomSimpleJSONExporter.CreateFormatSettings: TCustomExportFormatSettings;
begin
  Result:=TSimpleJSONFormatSettings.Create(False);
end;

procedure TCustomSimpleJSONExporter.DoBeforeExecute;
begin
  inherited DoBeforeExecute;
  OpenTextFile;
  FREN:=FormatSettings.RowElementName;
  FRF:=FormatSettings.RowFormat;
  FCF:=FormatSettings.ColumnFormat;
  If (FREN='') and (FRF=rfObject) then
    FREN:='ROW';
  FIS:=FormatSettings.IndentSize;
  FIndent:='';
  FRC:=0;
end;

procedure TCustomSimpleJSONExporter.DoAfterExecute;
begin
  CloseTextFile;
  inherited DoAfterExecute;
end;

procedure TCustomSimpleJSONExporter.DoDataRowStart;
begin
  Inc(FRC);
  FCurrentRow:='';
end;

procedure TCustomSimpleJSONExporter.DoDataHeader;
begin
  If FRF=rfObject then
    OutputRow('{')
  else
    OutputRow('[');
  IncIndent;
end;

procedure TCustomSimpleJSONExporter.DoDataFooter;
begin
  DecIndent;
  If FRF=rfObject then
    OutputRow('}')
  else
    OutputRow(']');
end;

procedure TCustomSimpleJSONExporter.ExportField(EF: TExportFieldItem);

Var
  S : String;

begin
  if EF.Field.IsNull then
    S:='null' // do not localize
  else if EF.Field.DataType=ftBoolean then
    begin
    If EF.FIeld.AsBoolean then
      S:='true' // Do not localize
    else
      S:='false';// Do not localize
    end
  else if EF.Field.DataType=ftFloat then
    Str(EF.FIeld.asFloat,S)
  else
    S:=FormatField(EF.Field);
  if not (EF.Field.isnull or (ef.field.Datatype in (ordFieldTypes+[ftFloat]))) then
    S:='"'+TextString(S)+'"';
  If FCF=cfObject then
    S:='"'+EF.ExportedName+'" : '+S;
  If (FCurrentRow<>'') then
    FCurrentRow:=FCurrentRow+', ';
  FCurrentRow:=FCurrentRow+S;
end;

procedure TCustomSimpleJSONExporter.DoDataRowEnd;

begin
  If FcF=cfObject then
    FCurrentRow:='{ '+FCurrentRow+' }'
  else
    FCurrentRow:='[ '+FCurrentRow+' ]';
  If FRF = rfObject then
    FCurrentRow:=Format('"%s%d" : %s',[FREN,FRC,FCurrentRow]);
  if not Dataset.EOF then
    If FRF=rfObject then
      FCurrentRow:=FCurrentRow+';'
    else
      FCurrentRow:=FCurrentRow+',';
  OutputRow(FCurrentRow);
end;

Procedure RegisterSimpleJSONExportFormat;

begin
  ExportFormats.RegisterExportFormat(SSimpleJSON,SSimpleJSONDescription,SSimpleJSONExtensions,TSimpleJSONExporter);
end;
Procedure UnRegisterSimpleJSONExportFormat;

begin
  ExportFormats.UnRegisterExportFormat(SSimpleJSON);
end;

end.

