unit fptexexport;
{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2022 by Michael van Canney and other members of the
    Free Pascal development team

    TeX Export code

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, fpdbexport;

Type
  TTeXExportOption = (teHeaderRow,teTableEnvironment,teHeaderLine,teTopLine,teBottomLine,teUseWidths,teCreateDocument);
  TTeXExportOptions = Set of TTeXExportOption;
  TTexTabularEnvironment = (ttTabular,ttTabularX,ttLongtable,ttSuperTabular);
  TTexUnits = (tuEm,tuMM);
  
  { TTeXExportFormatSettings }

  TTeXExportFormatSettings = Class(TExportFormatSettings)
  Private
    FOptions : TTeXExportOptions;
    FUnits : TTexUnits;
    FTabular : TTexTabularEnvironment;
  Public
    Constructor Create(DoInitSettings : Boolean); override;
    Procedure Assign(Source : TPersistent); override;
  Published
    // Properties
    Property Options : TTeXExportOptions Read FOptions Write FOptions;
    Property Units : TTexUnits Read FUnits Write FUnits;
    Property Tabular : TTexTabularEnvironment Read FTabular Write FTabular;
  end;

  { TTeXExportFieldItem }
  TTeXExportFieldItem = Class(TExportFieldItem)
  private
    FLineAfter: Boolean;
    FLineBefore: Boolean;
    FWidth: Integer;
    FAlign: TAlignment;
  Public
    Procedure Assign(Source : TPersistent); override;
  Published
    Property Width : Integer Read FWidth Write FWidth;
    Property Align: TAlignment Read FAlign write FAlign;
    Property LineBefore : Boolean Read FLineBefore Write FLineBefore;
    Property LineAfter : Boolean Read FLineAfter Write FLineAfter;
  end;

  { TCustomTeXExporter }

  TCustomTeXExporter = Class(TCustomFileExporter)
  Private
    FCurrentRow : String;
    FEO : TTeXExportOptions;
    FTD : String; // Tabular(X) Table definition string
    FTH : String; // Table header row
    FTN : String; // Tabular environment name (for closing)
    function GetTeXFormatsettings: TTexExportFormatSettings;
    procedure SetTeXFormatSettings(const AValue: TTexExportFormatSettings);
  Protected
    function EscapeLaTeX(const S: String): String;
    procedure OutputRow(const ARow: String); virtual;
    procedure OutputTableEnd; virtual;
    procedure OutputTableStart; virtual;
    procedure CloseDocument;  virtual;
    procedure OpenDocument; virtual;
    Function CreateFormatSettings : TCustomExportFormatSettings; override;
    Procedure BuildDefaultFieldMap(AMap : TExportFields); override;
    Function  CreateExportFields : TExportFields; override;
    Procedure DoDataHeader; override;
    Procedure DoDataFooter; override;
    Procedure DoBeforeExecute; override;
    Procedure DoAfterExecute; override;
    Procedure DoDataRowStart; override;
    Procedure ExportField(EF : TExportFieldItem); override;
    Procedure DoDataRowEnd; override;
  Public
    Property FormatSettings : TTexExportFormatSettings Read GetTeXFormatsettings Write SetTeXFormatSettings;
  end;

  TTeXExporter = Class(TCustomTeXExporter)
  Published
    Property FileName;
    Property Dataset;
    Property ExportFields;
    Property FromCurrent;
    Property RestorePosition;
    Property FormatSettings;
    Property OnExportRow;
  end;

Procedure RegisterTexExportFormat;
Procedure UnRegisterTexExportFormat;

Const
  STeXExport    = 'TeX export';
  STeXExportExt = '.tex';
  
  TabularPackageNames  : Array[TTexTabularEnvironment] of string
                       = ('array','tabularx','longtable','supertabular');
  TabularNames : Array[TTexTabularEnvironment] of string
               = ('tabular','tabularx','longtable','supertabular');
  TeXUnitNames : array[TTexUnits] of string = ('em','cm');

Resourcestring
  STeXExportDescr = 'Export to LaTeX table';

implementation

procedure RegisterTexExportFormat;
begin
  ExportFormats.RegisterExportFormat(STeXExport,STeXExportDescr,STexExportExt,TTexExporter);
end;

procedure UnRegisterTexExportFormat;
begin
  ExportFormats.UnRegisterExportFormat(STeXExport);
end;

{ TCustomTeXExporter }
function TCustomTexExporter.EscapeLaTeX(const S: String): String;

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
    if (P^ in ['&','{','}','#','_','$','%']) then
      begin
      Result:=Result+Copy(S,J,I-J)+'\'+P^;
      J:=I+1;
      end
    else if (P^ in ['~','^']) then
      begin
      Result:=Result+Copy(S,J,I-J)+'\'+P^+' ';
      J:=I+1;
      end
    else if (P^='\') then
      begin
      Result:=Result+Copy(S,J,I-J)+'$\backslash$';
      J:=I+1;
      end;
    Inc(I);
    Inc(P);
    end;
  Result:=Result+Copy(S,J,I-1);
end;

function TCustomTeXExporter.GetTeXFormatsettings: TTexExportFormatSettings;
begin
  Result:=TTexExportFormatSettings(Inherited FormatSettings)
end;

procedure TCustomTeXExporter.OutputRow(const ARow: String);
begin
  Writeln(TextFile,ARow);
end;

procedure TCustomTeXExporter.BuildDefaultFieldMap(AMap: TExportFields);

Const
  FieldWidths : Array[TFieldType] of integer
              = (-1,0,3,10,5,
                  1,20,20,20,10,8,20,
                  0,0,10,0,0,0,0,
                  0,0,0,0,0,
                  0,0,0,0,0,
                  0,0,0,0,0,
                  0,0,0,0,0,0,
                  0,0,10,4,1,20,8);

Var
  I  : Integer;
  FL : TTexExportFieldItem;
  F : TField;
  W : Integer;
  
begin
  inherited BuildDefaultFieldMap(AMap);
  For I:=0 to AMap.Count-1 do
    begin
    FL:=TTexExportFieldItem(AMAP[i]);
    F:=Dataset.Fields[i];
    W:= FieldWidths[F.DataType];
    If (W>0) then
      FL.Width:=W
    else if (W=0) then
      begin
      if (F.DataType in StringFieldTypes) then
        FL.Width:=F.Size;
      end;
    If (F.DataType in IntFieldTypes) then
      Fl.Align:=taRightJustify;
    end;

end;

function TCustomTeXExporter.CreateExportFields: TExportFields;
begin
  Result:=TExportFields.Create(TTexExportFieldItem);
end;

procedure TCustomTeXExporter.DoDataHeader;

Const
  AlChars : Array[TAlignment] of char = 'lcr';

Var
  I,TW : Integer;
  B1,B2 : Boolean;
  EF : TTeXExportFieldItem;
  UN,S,FTW : String;
  
begin
  B1:=teUseWidths in FEO;
  B2:=teHeaderRow in FEO;
  UN:=TexUnitnames[FormatSettings.Units];
  S:='';
  TW:=0;
  For I:=0 to ExportFields.Count-1 do
    begin
    EF:=TTexExportFieldItem(ExportFields[i]);
    If EF.Enabled then
      begin
      If EF.LineBefore then
        S:=S+'|';
      if B1 then
        begin
        TW:=TW+EF.Width;
        S:=S+'p{'+IntToStr(EF.Width)+'}'+UN;
        end
      else
        S:=S+ALChars[EF.Align];
      If EF.LineAfter then
        S:=S+'|';
      If B2 THEN
        begin
        If (FTH<>'') then
          FTH:=FTH+' & ';
        FTH:=FTH+EscapeLaTeX(EF.ExportedName);
        end;
      end;
    end;
 If FormatSettings.Tabular=ttTabularx then
   if Not B1 then
     FTW:='{\textwidth}'
   else
     FTW:=Format('{\%d%s}',[TW,UN]);
  FTD:=Format('\begin{%s}%s{%s}',[FTN,FTW,S]);
  If B2 then
    FTH:=FTH+'\\';
  OutPutTableStart;
  inherited DoDataHeader;
end;

procedure TCustomTeXExporter.DoDataFooter;

begin
  OutPutTableEnd;
  Inherited DoDataFooter;
end;

procedure TCustomTeXExporter.OutputTableEnd;

begin
  If teBottomLine in FEO then
    OutputRow('\hline');
  OutputRow(Format('\end{%s}',[FTN]));
  if (teTableEnvironment in FEO) then
    OutputRow('\end{table}');
end;

procedure TCustomTeXExporter.OutputTableStart;

Var
  S : String;
  I : Integer;

begin
  S:='';
  if (teTableEnvironment in FEO) then
    OutputRow('\begin{table}');
  OutputRow(FTD);
  If teHeaderRow in FEO then
    begin
    if (TeHeaderLine in FEO) then
      OutputRow('\hline');
    OutputRow(FTH);
    end;
  if (TeTopLine in FEO) then
    OutputRow('\hline');
end;

procedure TCustomTeXExporter.SetTeXFormatSettings(
  const AValue: TTexExportFormatSettings);
begin
  Inherited FormatSettings:=AValue
end;

function TCustomTeXExporter.CreateFormatSettings: TCustomExportFormatSettings;
begin
  Result:=TTexExportFormatSettings.Create(False);
end;

procedure TCustomTeXExporter.DoBeforeExecute;
begin
  inherited DoBeforeExecute;
  OpenTextFile;
  FEO:=FormatSettings.Options;
  FTD:='';
  FTH:='';
  FTN:=TabularNames[FormatSettings.Tabular];
  If teCreateDocument in FEO then
    OpenDocument;
end;

procedure TCustomTeXExporter.OpenDocument;

Var
  S : string;
  
begin
  OutputRow(Format('\documentclass%s{%s}',['','article']));
  S:=TabularPackageNames[FormatSettings.Tabular];
  If (S<>'') then
    OutputRow(Format('\usepackage{%s}',[s]));
  OutputRow('\begin{document}');
end;

procedure TCustomTeXExporter.CloseDocument;

begin
  OutputRow('\end{document}');
end;

procedure TCustomTeXExporter.DoAfterExecute;
begin
  If teCreateDocument in FEO then
    CloseDocument;
  CloseTextFile;
  inherited DoAfterExecute;
end;

procedure TCustomTeXExporter.DoDataRowStart;
begin
  FCurrentRow:='';
  inherited DoDataRowStart;
end;

procedure TCustomTeXExporter.ExportField(EF: TExportFieldItem);

Var
  S : String;
  
begin
  S:=FormatField(EF.Field);
  If (FCurrentRow<>'') then
    FCurrentRow:=FCurrentRow+' & ';
  FCurrentRow:=FCurrentRow+EscapeLaTex(S);
end;

procedure TCustomTeXExporter.DoDataRowEnd;
begin
  FCurrentRow:=FCurrentRow+' \\';
  OutputRow(FCurrentRow);
end;

{ TTeXExportFormatSettings }

constructor TTeXExportFormatSettings.Create(DoInitSettings: Boolean);
begin
  inherited Create(DoInitSettings);
  FOptions:=[teHeaderRow,teTableEnvironment,teTopLine,teBottomLine]
end;

procedure TTeXExportFormatSettings.Assign(Source: TPersistent);

Var
  FS : TTeXExportFormatSettings;

begin
  If (Source is TTeXExportFormatSettings) then
    begin
    FS:=Source as TTeXExportFormatSettings;
    Options:=FS.OPtions;
    Units:=FS.Units;
    Tabular:=FS.Tabular;
    end;
  inherited Assign(Source);
end;

{ TTeXExportFieldItem }

procedure TTeXExportFieldItem.Assign(Source: TPersistent);

Var
  Fi : TTeXExportFieldItem;

begin
  If (Source is TTeXExportFieldItem) then
    begin
    FI:=Source as TTeXExportFieldItem;
    Width:=FI.Width;
    Align:=FI.Align;
    LineBefore:=FI.LineBefore;
    LineAfter:=FI.LineAfter;
    end;
  inherited Assign(Source);
end;

end.

