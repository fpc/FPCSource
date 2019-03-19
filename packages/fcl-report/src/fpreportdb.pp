{
    This file is part of the Free Component Library.
    Copyright (c) 2017 Michael Van Canneyt, member of the Free Pascal development team

    Report Data loop classes based on TDataset.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpreportdb;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpreport, db;

Type

  { TFPReportDatasetData }

  TFPReportDatasetData = class(TFPReportData)
  private
    FDataSet: TDataSet;
    procedure SetDataSet(AValue: TDataSet);
  protected
    function GetIsOpened: boolean; override;
    Procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DoGetValue(const AFieldName: string; var AValue: variant); override;
    procedure DoInitDataFields; override;
    procedure DoOpen; override;
    procedure DoFirst; override;
    procedure DoNext; override;
    procedure DoClose; override;
    function  DoEOF: boolean; override;
  Public
    property  DataFields;
    Procedure StartDesigning; override;
    Procedure EndDesigning; override;
  published
    property  DataSet: TDataSet read FDataSet write SetDataSet;
  end;

implementation

resourcestring
  SErrNoDataSetAssigned  = 'No dataset has been assigned.';
  SErrDatasetNotOpen = 'Dataset has not been opened yet';
  SErrFieldTypeMisMatch = 'Field type for field "%s" changed. Expected "%s", got "%s"';
  SErrUnknownFieldInDataset = 'Unexpected field in dataset: "%s"';

{ TFPReportDatasetData }

procedure TFPReportDatasetData.SetDataSet(AValue: TDataSet);
begin
  if FDataSet=AValue then Exit;
  if Assigned(FDataset) then
    FDataset.RemoveFreeNotification(Self);
  FDataSet:=AValue;
  if Assigned(FDataset) then
    FDataset.FreeNotification(Self);
end;

function TFPReportDatasetData.GetIsOpened: boolean;
begin
  Result:=inherited GetIsOpened;
  if Result then
    Result:=FDataset.Active; // Can be closed because of master-detail.
end;

procedure TFPReportDatasetData.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation=opRemove) and (AComponent=FDataset) then
    FDataset:=Nil;
end;

procedure TFPReportDatasetData.DoGetValue(const AFieldName: string; var AValue: variant);
var
  ms: TMemoryStream;
begin
  inherited DoGetValue(AFieldName, AValue);
  try
    if FieldTypes[AFieldName] = rfkStream then
    begin
      ms := TMemoryStream.Create;
      try
        TBlobField(FDataSet.FieldByName(AFieldName)).SaveToStream(ms);
        AValue := FPReportStreamToMIMEEncodeString(ms);
      finally
        ms.Free;
      end;
    end
    else
    begin
      AValue := FDataSet.FieldByName(AFieldName).Value;
    end;
  except
    on E: EDatabaseError do
    begin
      // no nothing - it's probably an expression, which will be handled in CustomBand.ExpandMacro()
    end;
  end;
end;

procedure TFPReportDatasetData.DoInitDataFields;
var
  i: integer;

  function DatabaseKindToReportKind(const AType: TFieldType): TFPReportFieldKind;
  begin
    case AType of
      ftUnknown:        Result := rfkString;
      ftString:         Result := rfkString;
      ftSmallint:       Result := rfkInteger;
      ftInteger:        Result := rfkInteger;
      ftWord:           Result := rfkInteger;
      ftBoolean:        Result := rfkBoolean;
      ftFloat:          Result := rfkFloat;
      ftCurrency:       Result := rfkCurrency;
      ftBCD:            Result := rfkFloat;
      ftDate:           Result := rfkDateTime;
      ftTime:           Result := rfkDateTime;
      ftDateTime:       Result := rfkDateTime;
      ftBytes:          Result := rfkStream;
      ftVarBytes:       Result := rfkStream;
      ftAutoInc:        Result := rfkInteger;
      ftBlob:           Result := rfkStream;
      ftMemo:           Result := rfkStream;
      ftGraphic:        Result := rfkStream;
      ftFmtMemo:        Result := rfkString;
      //ftParadoxOle:
      //ftDBaseOle:
      ftTypedBinary:    Result := rfkStream;
      //ftCursor:
      ftFixedChar:      Result := rfkString;
      ftWideString:     Result := rfkString;
      ftLargeint:       Result := rfkInteger;
      //ftADT:
      //ftArray:
      //ftReference:
      //ftDataSet:
      ftOraBlob:        Result := rfkStream;
      ftOraClob:        Result := rfkStream;
      ftVariant:        Result := rfkString;
      //ftInterface:
      //ftIDispatch:
      ftGuid:           Result := rfkString;
      ftTimeStamp:      Result := rfkDateTime;
      //ftFMTBcd:
      ftFixedWideChar:  Result := rfkString;
      ftWideMemo:       Result := rfkString;
      else
        Result := rfkString;
    end;
  end;

Var
  B,AllowNew : Boolean;
  F : TFPReportDataField;
  Rfk  : TFPReportFieldKind;

begin
  inherited DoInitDataFields;
  B:=FDataset.FieldDefs.Count=0;
  if B then
    FDataset.Open;
  try
     if (DataFields.Count>0) and (DataFields.Count<>FDataset.FieldDefs.Count) then
       // Reset totally
       DataFields.Clear;
    AllowNew:=(Datafields.Count=0);
    for i := 0 to FDataSet.FieldDefs.Count-1 do
      begin
      RFK:=DatabaseKindToReportKind(FDataset.FieldDefs[i].DataType);
      F:=Datafields.FindField(FDataset.FieldDefs[i].Name);
      if (F=Nil) then
        if AllowNew then
          DataFields.AddField(FDataset.FieldDefs[i].Name, RFK)
        else
          Raise EReportError.CreateFmt(SErrUnknownFieldInDataset,[F.FieldName])
      else
        if (F.FieldKind<>RFK) then
          Raise EReportError.CreateFmt(SErrFieldTypeMisMatch,[F.FieldName,ReportFieldKindNames[F.FieldKind],ReportFieldKindNames[RFK]]);
      end
  finally
    if B then
      FDataset.Close;
  end;
end;

procedure TFPReportDatasetData.DoOpen;
begin
  inherited DoOpen;
  if not Assigned(FDataSet) then
    ReportError(SErrNoDataSetAssigned);
  FDataSet.Open;
end;

procedure TFPReportDatasetData.DoFirst;
begin
  if not Assigned(FDataSet) then
    ReportError(SErrNoDataSetAssigned);
  if not FDataSet.Active then
    ReportError(SErrDatasetNotOpen);
  inherited DoFirst;
  FDataSet.First;
end;

procedure TFPReportDatasetData.DoNext;
begin
  inherited DoNext;
  FDataSet.Next;
end;

procedure TFPReportDatasetData.DoClose;
begin
  inherited DoClose;
  FDataSet.Close;
end;

function TFPReportDatasetData.DoEOF: boolean;
begin
  Result := FDataSet.EOF;
end;

Type
  TMyDataset = Class(TDataset);

procedure TFPReportDatasetData.StartDesigning;

begin
  Inherited;
  if Assigned(DataSet) then
    // Dirty hack!!
    TMyDataset(Dataset).SetDesigning(True,True);
end;

procedure TFPReportDatasetData.EndDesigning;
begin
  if Assigned(DataSet) then
    // Dirty hack!!
    TMyDataset(Dataset).SetDesigning(False,True);
  Inherited;
end;

end.

