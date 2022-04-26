{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2022 by Michael van Canneyt and other members of the
    Free Pascal development team

    report data 

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit fpreportdata;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, fpjson, fpreport, fpreportdb;

Type
  EReportDataError = class(EReportError);

  { TFPReportDataHandler }

  TFPReportDataHandler = Class(TObject)
  Public
    Class Procedure RegisterHandler;
    Class Procedure UnRegisterHandler;
    Class Procedure RegisterConfigClass(aClass : TComponentClass);
    // Override this to return a dataset which is owned by AOwner, and configured by AConfig.
    // The dataset must not be opened.
    Function CreateDataset(AOwner : TComponent; AConfig : TJSONObject) : TDataset; virtual; abstract;
    // By default, master-detail is not possible. Let this return 'True' indicate that a dataset allows master-detail.
    Class Function AllowMasterDetail : Boolean; virtual;
    // This is called to set the master dataset on Detail
    Class Procedure SetMasterDataset(ADetail,AMaster : TDataset); virtual;
    // Check if the configuration is valid. Return a string that describes the error(s)
    // If the return is an empty string, the data designer will not close.
    Class Function CheckConfig(AConfig : TJSONObject) : String; virtual;
    // Called before and after rendering of a report in the designer.
    // Can be used to start/stop transactions
    Class Procedure StartRender(ADataset : TDataset); virtual;
    Class Procedure EndRender(ADataset : TDataset); virtual;
    // Configuration component. This is normally a visual class.
    Class Function ConfigFrameClass : TComponentClass; virtual;
    Class Function DataType : String; virtual; abstract;
    Class Function DataTypeDescription : String; virtual;
  end;
  TFPReportDataHandlerClass = Class of TFPReportDataHandler;

  { TFPReportDataDefinitionItem }

  TFPReportDataDefinitionItem = Class(TCollectionItem)
  private
    FConfig: TJSONObject;
    FDataType: String;
    FMaster: String;
    FName: String;
    FReportData: TFPReportDatasetData;
    FRunReportDataItem: TFPReportDataItem;
    function GetJSONConfig: TJSONStringType;
    procedure SetConfig(AValue: TJSONObject);
    procedure SetJSONConfig(AValue: TJSONStringType);
    procedure SetMaster(AValue: String);
    procedure SetName(AValue: String);
  Protected
    // To hold temporary references
    Property RunReportData : TFPReportDatasetData Read FReportData Write FReportData;
    Property RunReportDataItem : TFPReportDataItem Read FRunReportDataItem Write FRunReportDataItem;
  Public
    Constructor Create(ACollection: TCollection); override;
    Destructor Destroy; override;
    Procedure Assign(Source : TPersistent); override;
    Procedure SaveToJSON(O : TJSONObject); virtual;
    procedure LoadFromJSON(O: TJSONObject); virtual;
    // Clone this
    Function Clone(aNewName : String) : TFPReportDataDefinitionItem;
    // Create a dataset.
    Function CreateDataSet(AOwner : TComponent) : TDataset;
    // Check if the configuration is OK.
    Function Check : String;
    Property Config : TJSONObject Read FConfig Write SetConfig;
  Published
    property Name : String Read FName Write SetName;
    Property DataType : String Read FDataType Write FDataType;
    property Master : String Read FMaster Write SetMaster;
    Property JSONConfig : TJSONStringType Read GetJSONConfig Write SetJSONConfig;
  end;

  { TFPReportDataDefinitions }

  TFPReportDataDefinitions = Class(TCollection)
  private
    function GetD(Aindex : Integer): TFPReportDataDefinitionItem;
    procedure SetD(Aindex : Integer; AValue: TFPReportDataDefinitionItem);
  Public
    Function IndexOfRunData(aData : TFPReportDatasetData) : integer;
    Function IndexOfName(const aName : String): Integer;
    Procedure CheckCircularReference(aMasterName : String; aItem : TFPReportDataDefinitionItem);
    Function FindDataByName(const aName : String): TFPReportDataDefinitionItem;
    Function AddData(const aName : String) : TFPReportDataDefinitionItem;
    Procedure SaveToJSON(O : TJSONObject);
    Procedure LoadFromJSON(O : TJSONObject);
    Property Data [Aindex : Integer] : TFPReportDataDefinitionItem Read GetD Write SetD; default;
  end;

  { TFPCustomReportDataManager }

  TFPCustomReportDataManager = class(TComponent)
  private
    Class Var
      FTypesList : TStrings;
    Type
      { THDef }

      THDef = Class(TObject)
        TheClass : TFPReportDataHandlerClass;
        TheConfigClass : TComponentClass;
        Constructor Create(aClass : TFPReportDataHandlerClass; aConfigClass : TComponentClass);
      end;
    procedure ClearReportDatasetReference(aDataset: TFPReportDatasetData);
    Class Function FindDef(aDataType: String) : THDef;
    Class Function GetDef(aDataType: String) : THDef;
  Private
    FDataParent: TComponent;
    FMyParent : TComponent;
    FDefinitions: TFPReportDataDefinitions;
    FReport: TFPReport;
    procedure SetDataParent(AValue: TComponent);
    procedure SetDefinitions(AValue: TFPReportDataDefinitions);
    procedure SetReport(AValue: TFPReport);
  Protected
    Class Function TypeList : TStrings;
    Class procedure RemoveHandler(aDataType: String);
    Class Procedure RegisterHandler(aClass: TFPReportDataHandlerClass); virtual;
    Class Procedure UnRegisterHandler(aClass: TFPReportDataHandlerClass); virtual;
    Procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    Function CreateDataDefinitions : TFPReportDataDefinitions; virtual;
    Function GetDatasetParent : TComponent;
    Property DataDefinitions : TFPReportDataDefinitions Read FDefinitions Write SetDefinitions;
  Public
    Class Function GetRegisteredTypes(AList : Tstrings) : Integer;
    Class Procedure RegisterConfigFrameClass(aTypeName : String; aClass : TComponentClass);
    Class Procedure UnRegisterConfigFrameClass(aTypeName : String);
    Class Function GetTypeHandlerClass(aTypeName : String) : TFPReportDataHandlerClass;
    Class Function GetTypeHandler(aTypeName : String) : TFPReportDataHandler;
    Class Function GetConfigFrameClass(aTypeName : String) : TComponentClass;
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
    Procedure SaveToJSON(O : TJSONObject);
    Procedure LoadFromJSON(O : TJSONObject);
    procedure RemoveFromReport(aReport: TFPReport);virtual;
    procedure RemoveFromReport;
    procedure ApplyToReport(aReport: TFPReport; Errors: TStrings); virtual;
    Procedure ApplyToReport(Errors : TStrings);
    // call this to start/stop a render, so dataset handlers can do cleanup, refetch data, reset transactions.
    Procedure StartRender;
    Procedure EndRender;
    Property Report : TFPReport Read FReport Write SetReport;
    Property DataParent : TComponent Read FDataParent Write SetDataParent;
  end;

  TFPReportDataManager = Class(TFPCustomReportDataManager)
  Public
    Property DataDefinitions;
  end;

Resourcestring
  SErrDuplicateData = 'Duplicate data set name: "%s"';
  SErrInvalidDataName = 'Invalid data set name: "%s"';
  SErrNeedName = 'Data set needs a name';
  SErrNeedDataType = 'Data set needs a type';
  SErrInvalidDataType = 'Invalid data type: "%s"';
  SErrInvalidJSONConfig = '%s: Invalid JSON Configuration';
  SErrUnknownDataType =   'Unknown report data type: %s';
  SErrNoMasterDetailSupport = 'No master-detail support for class "%s"';
  SErrOpeningDataset = 'Error opening data "%s" : Exception %s with message %s';
  SErrInvalidDatasourceName = 'Invalid data source name : "%s"';
  SErrCircularReference = 'Invalid master data source "%s", circular references not allowed.';

implementation

Const
  DatasetNamePrefix = '__DS__';

{ TFPCustomReportDataManager }

procedure TFPCustomReportDataManager.SetDefinitions(AValue: TFPReportDataDefinitions);
begin
  if FDefinitions=AValue then Exit;
  FDefinitions.Assign(AValue);
end;

procedure TFPCustomReportDataManager.SetDataParent(AValue: TComponent);
begin
  if FDataParent=AValue then Exit;
  If Assigned(FDataParent) then
    FDataParent.RemoveFreeNotification(Self);
  FDataParent:=AValue;
  If Assigned(FDataParent) then
    FDataParent.FreeNotification(Self);
  FreeAndNil(FMyParent);
end;

class function TFPCustomReportDataManager.FindDef(aDataType: String): THDef;

var
  I : Integer;

begin
  I:=TypeList.IndexOf(aDataType);
  if (I<>-1) then
    Result:=TypeList.Objects[i] as THDef
  else
    Result:=Nil;
end;

class function TFPCustomReportDataManager.GetDef(aDataType: String): THDef;
begin
  Result:=FindDef(aDataType);
  if Result=Nil then
    Raise EReportDataError.CreateFmt(SErrUnknownDataType,[aDataType]);
end;

procedure TFPCustomReportDataManager.SetReport(AValue: TFPReport);
begin
  if FReport=AValue then Exit;
  If Assigned(FReport) then
    FReport.RemoveFreeNotification(Self);
  FReport:=AValue;
  If Assigned(FReport) then
    FReport.FreeNotification(Self);
end;

class procedure TFPCustomReportDataManager.RegisterHandler(aClass: TFPReportDataHandlerClass);

Var
  N : String;
  C : TComponentClass;

begin
  N:=aClass.DataType;
  RemoveHandler(N);
  C:=aClass.ConfigFrameClass;
  TypeList.AddObject(N, THDef.Create(aClass,C));
end;

class procedure TFPCustomReportDataManager.RemoveHandler(aDataType : String);

Var
  I : Integer;
  O : TObject;

begin
  I:=TypeList.IndexOf(aDataType);
  if (I<>-1) then
    begin
    O:=TypeList.Objects[i];
    TypeList.Delete(I);
    O.Free;
    end;
end;

class procedure TFPCustomReportDataManager.UnRegisterHandler(aClass: TFPReportDataHandlerClass);

begin
  RemoveHandler(aClass.DataType);
end;

procedure TFPCustomReportDataManager.ClearReportDatasetReference(aDataset : TFPReportDatasetData);

Var
  I : Integer;

begin
  if Assigned(FDefinitions) then
    begin
    I:=FDefinitions.IndexOfRunData(aDataset);
    if (I<>-1) then
      FDefinitions[i].RunReportData:=nil;
    end;
end;

procedure TFPCustomReportDataManager.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation=opRemove then
    begin
    if AComponent=FDataParent then
      FDataParent:=Nil
    else if AComponent=FReport then
      FReport:=Nil
    else if (aComponent is TFPReportDatasetData) then
      ClearReportDatasetReference(aComponent as TFPReportDatasetData);
    end;
end;

Class function TFPCustomReportDataManager.TypeList: TStrings;

Var
  SL : TStringList;

begin
  If (FTypesList=nil) then
    begin
    SL:=TStringList.Create;
    SL.Sorted:=True;
    SL.Duplicates:=dupError;
    SL.OwnsObjects:=True;
    FTypesList:=SL;
    end;
  Result:=FTypesList;
end;

function TFPCustomReportDataManager.CreateDataDefinitions: TFPReportDataDefinitions;
begin
  Result:=TFPReportDataDefinitions.Create(TFPReportDataDefinitionItem);
end;

function TFPCustomReportDataManager.GetDatasetParent: TComponent;
begin
  Result:=FDataParent;
  if Result=Nil then
    begin
    If (FMyParent=Nil) then
      FMyParent:=TComponent.Create(Nil);
    Result:=FMyParent;
    end;
end;

constructor TFPCustomReportDataManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefinitions:=CreateDataDefinitions;
end;

destructor TFPCustomReportDataManager.Destroy;
begin
  FreeAndNil(FDefinitions);
  FreeAndNil(FMyParent);
  inherited Destroy;
end;

procedure TFPCustomReportDataManager.SaveToJSON(O: TJSONObject);
begin
  DataDefinitions.SaveToJSON(O);
end;

procedure TFPCustomReportDataManager.LoadFromJSON(O: TJSONObject);
begin
  DataDefinitions.LoadFromJSON(O);
end;


procedure TFPCustomReportDataManager.RemoveFromReport(aReport: TFPReport);

Var
  DD : TFPReportDataDefinitionItem;
  RD : TFPReportDatasetData;
  D : TDataset;
  I : Integer;

begin
  aReport.SaveDataToNames;
  For I:=0 to DataDefinitions.Count-1 do
    begin
    DD:=DataDefinitions[i];
    RD:=DD.RunReportData;
    if (RD<>Nil) then
      if (aReport.ReportData.IndexOfReportData(RD)<>-1) then
        begin
        D:=RD.Dataset;
        FreeAndNil(D);
        FreeAndNil(RD); // Should nil due to freenotification...
        DD.RunReportData:=Nil;
        end;
    end;
end;

procedure TFPCustomReportDataManager.RemoveFromReport;
begin
  RemoveFromReport(FReport);
end;

procedure TFPCustomReportDataManager.ApplyToReport(Errors: TStrings);

begin
  ApplyToReport(FReport,Errors);
end;

procedure TFPCustomReportDataManager.StartRender;

Var
  I : integer;
  D : TFPReportDataDefinitionItem;
  H : TFPReportDataHandlerClass;

begin
  // Create all datasets
  For I:=0 to DataDefinitions.Count-1 do
    begin
    D:=DataDefinitions[i];
    if Assigned(D.RunReportData) then
      begin
      H:=TFPCustomReportDataManager.GetTypeHandlerClass(D.DataType);
      H.StartRender(D.RunReportData.DataSet);
      end;
    end;
end;

procedure TFPCustomReportDataManager.EndRender;

Var
  I : integer;
  D : TFPReportDataDefinitionItem;
  H : TFPReportDataHandlerClass;

begin
  // Create all datasets
  For I:=0 to DataDefinitions.Count-1 do
    begin
    D:=DataDefinitions[i];
    if Assigned(D.RunReportData) then
      begin
      H:=TFPCustomReportDataManager.GetTypeHandlerClass(D.DataType);
      H.EndRender(D.RunReportData.DataSet);
      end;
    end;
end;

procedure TFPCustomReportDataManager.ApplyToReport(aReport : TFPReport; Errors: TStrings);


Var
  I : Integer;
  MasterD,DesignD : TFPReportDataDefinitionItem;
  DatasetD : TFPReportDatasetData;
  H : TFPReportDataHandlerClass;
  L : TFPList;
  P : TComponent;
  DDS,MDS : TDataset;

begin
  P:=GetDatasetParent;
  aReport.ReportData.Clear;
  // Create all datasets
  For I:=0 to DataDefinitions.Count-1 do
    begin
    DesignD:=DataDefinitions[i];
    DatasetD:=TFPReportDatasetData.Create(P);
    DesignD.RunReportData:=DatasetD;
    DatasetD.FreeNotification(Self);
    DatasetD.Dataset:=DesignD.CreateDataSet(P);
    Try
      DatasetD.InitFieldDefs;
    except
      On E : Exception do
        If Assigned(Errors) then
          Errors.Add(Format(SErrOpeningDataset, [DesignD.Name, E.ClassName, E.Message]))
        else
          Raise;
    end;
    DatasetD.Name:=DesignD.Name;
    DatasetD.Dataset.Name:=DatasetNamePrefix+DesignD.Name;
    DatasetD.StartDesigning;    // set designing flag, or OI will not show reference to it.
    DesignD.RunReportDataItem:=aReport.ReportData.AddReportData(DatasetD);
    end;
  // Set master-detail relations
  For I:=0 to DataDefinitions.Count-1 do
    begin
    DesignD:=DataDefinitions[i];
    if (DesignD.Master<>'') then
      begin
      H:=TFPCustomReportDataManager.GetTypeHandlerClass(DesignD.DataType);
      MasterD:=DataDefinitions.FindDataByName(DesignD.Master);
      DDS:=(DesignD.RunReportDataItem.Data as TFPReportDatasetData).DataSet;
      MDS:=(MasterD.RunReportDataItem.Data as TFPReportDatasetData).DataSet;
      H.SetMasterDataset(DDS,MDS);
      end;
    end;
end;

class function TFPCustomReportDataManager.GetRegisteredTypes(AList: Tstrings): Integer;
begin
  // Don't use assign or addstrings, it will copy the THRefs too, possibly leading to errors
  AList.Text:=TypeList.Text;
  Result:=AList.Count;
end;

class procedure TFPCustomReportDataManager.RegisterConfigFrameClass(aTypeName: String; aClass: TComponentClass);

Var
  H : THDef;

begin
  H:=GetDef(aTypeName);
  H.TheConfigClass:=aClass;
end;

class procedure TFPCustomReportDataManager.UnRegisterConfigFrameClass(aTypeName: String);

Var
  H : THDef;

begin
  H:=FindDef(aTypeName);
  if Assigned(H) then
    H.TheConfigClass:=Nil;
end;

class function TFPCustomReportDataManager.GetTypeHandlerClass(aTypeName: String): TFPReportDataHandlerClass;

Var
  H : THDef;

begin
  H:=GetDef(aTypeName);
  Result:=H.TheClass;
end;

class function TFPCustomReportDataManager.GetTypeHandler(aTypeName: String): TFPReportDataHandler;

begin
  Result:=GetTypeHandlerClass(aTypeName).Create;
end;

class function TFPCustomReportDataManager.GetConfigFrameClass(aTypeName: String): TComponentClass;

Var
  H : THDef;

begin
  H:=GetDef(aTypeName);
  Result:=H.TheConfigClass;
end;

{ THDef }

constructor TFPCustomReportDataManager.THDef.Create(aClass: TFPReportDataHandlerClass; aConfigClass : TComponentClass);

begin
  TheClass:=AClass;
  TheConfigClass:=aConfigClass;
end;

{ TFPReportDataDefinitions }

function TFPReportDataDefinitions.GetD(Aindex : Integer): TFPReportDataDefinitionItem;
begin
  Result:=Items[Aindex] as TFPReportDataDefinitionItem;
end;

procedure TFPReportDataDefinitions.SetD(Aindex : Integer; AValue: TFPReportDataDefinitionItem);
begin
  Items[Aindex]:=AValue;
end;

function TFPReportDataDefinitions.IndexOfRunData(aData: TFPReportDatasetData): integer;
begin
  Result:=Count-1;
  While (Result>=0) and (GetD(Result).RunReportData<>aData) do
    Dec(Result);
end;

function TFPReportDataDefinitions.IndexOfName(const aName: String): Integer;
begin
  Result:=Count-1;
  While (Result>=0) and (CompareText(AName,GetD(Result).Name)<>0) do
    Dec(Result);
end;

procedure TFPReportDataDefinitions.CheckCircularReference(aMasterName: String; aItem: TFPReportDataDefinitionItem);

Var
  DD : TFPReportDataDefinitionItem;

begin
  While (aMasterName<>'') do
    begin
    DD:=FindDataByName(aMasterName);
    if (DD=Nil) then
      raise EReportDataError.CreateFmt(SErrInvalidDatasourceName, [aMasterName]);
    If (DD=aItem) then
      raise EReportDataError.CreateFmt(SErrCircularReference, [aMasterName]);
    aMasterName:=DD.Master;
    end;

end;

function TFPReportDataDefinitions.FindDataByName(const aName: String): TFPReportDataDefinitionItem;

var
  I : Integer;

begin
  I:=indexOfname(aName);
  if I=-1 then
    Result:=Nil
  else
    Result:=GetD(I);
end;

function TFPReportDataDefinitions.AddData(const aName: String): TFPReportDataDefinitionItem;

begin
  if (IndexOfName(aName)<>-1) then
    raise EReportDataError.CreateFmt(SErrDuplicateData, [aName]);
  Result:=add as TFPReportDataDefinitionItem;
  Result.Name:=aName;
end;

procedure TFPReportDataDefinitions.SaveToJSON(O: TJSONObject);

Var
  A : TJSONArray;
  DS : TJSONObject;
  I : Integer;

begin
  A:=TJSONArray.Create;
  O.Add('datasets',a);
  For I:=0 to Count-1 do
    begin
    DS:=TJSONObject.Create;
    A.Add(DS);
    Data[i].SaveToJSON(DS);
    end;
end;

procedure TFPReportDataDefinitions.LoadFromJSON(O: TJSONObject);
Var
  A : TJSONArray;
  DS : TFPReportDataDefinitionItem;
  I : Integer;

begin
  Clear;
  A:=O.Get('datasets',TJSONArray(Nil));
  if Assigned(A) then
    For I:=0 to A.Count-1 do
      if A.Types[i]=jtObject then
        begin
        DS:=Add as TFPReportDataDefinitionItem;
        DS.LoadFromJSON(A.Objects[i]);
        end;
end;



class procedure TFPReportDataHandler.RegisterHandler;
begin
  TFPCustomReportDataManager.RegisterHandler(Self);
end;

class procedure TFPReportDataHandler.UnRegisterHandler;

begin
  TFPCustomReportDataManager.UnRegisterHandler(Self);
end;

class procedure TFPReportDataHandler.RegisterConfigClass(aClass: TComponentClass);
begin
  TFPCustomReportDataManager.RegisterConfigFrameClass(DataType,aClass);
end;

class function TFPReportDataHandler.AllowMasterDetail: Boolean;
begin
  Result:=False;
end;

Class procedure TFPReportDataHandler.SetMasterDataset(ADetail, AMaster: TDataset);
begin
  Raise EReportDataError.CreateFmt(SErrNoMasterDetailSupport,[ADetail.ClassName]);
end;


class function TFPReportDataHandler.CheckConfig(AConfig: TJSONObject): String;
begin
  Result:='';
end;

class procedure TFPReportDataHandler.StartRender(ADataset: TDataset);
begin
  // Do nothing
end;

class procedure TFPReportDataHandler.EndRender(ADataset: TDataset);
begin
  // Do nothing
end;

class function TFPReportDataHandler.ConfigFrameClass: TComponentClass;
begin
  Result:=Nil;
end;

class function TFPReportDataHandler.DataTypeDescription: String;
begin
  Result:=DataType
end;

{ TFPReportDataDefinitionItem }

procedure TFPReportDataDefinitionItem.SetConfig(AValue: TJSONObject);
begin
  if FConfig=AValue then Exit;
  FreeAndNil(FConfig);
  FConfig:=AValue.Clone as TJSONObject;
end;

function TFPReportDataDefinitionItem.GetJSONConfig: TJSONStringType;
begin
  Result:=FConfig.AsJSON;
end;

procedure TFPReportDataDefinitionItem.SetJSONConfig(AValue: TJSONStringType);

Var
  D : TJSONData;

begin
  D:=GetJSON(aValue);
  if D is TJSONObject then
    begin
    FreeAndNil(FConfig);
    FConfig:=D as TJSONObject;
    end
  else
    begin
    FreeAndNil(D);
    Raise EReportDataError.CreateFmt(SErrInvalidJSONConfig,[Name]);
    end;
end;

procedure TFPReportDataDefinitionItem.SetMaster(AValue: String);
begin
  if FMaster=AValue then Exit;
  FMaster:=AValue;
end;

procedure TFPReportDataDefinitionItem.SetName(AValue: String);
begin
  if FName=AValue then Exit;
  {$IF FPC_FULLVERSION < 30002}
  if Not IsValidIdent(aValue) then
  {$ELSE}
  if Not IsValidIdent(aValue,True,true) then
  {$ENDIF}
    raise EReportDataError.CreateFmt(SErrInvalidDataName, [aValue]);
  if (Collection is TFPReportVariables) then
    If ((Collection as TFPReportVariables).FindVariable(AValue)<>Nil) then
      raise EReportDataError.CreateFmt(SErrDuplicateData, [aValue]);
  FName:=AValue;
end;

procedure TFPReportDataDefinitionItem.Assign(Source: TPersistent);

Var
  D : TFPReportDataDefinitionItem;
begin
  if (Source is TFPReportDataDefinitionItem) then
    begin
    D:=Source as TFPReportDataDefinitionItem;
    Config:=D.Config;
    Name:=D.Name;
    DataType:=D.DataType;
    Master:=D.Master;
    end
  else
    inherited Assign(Source);
end;

procedure TFPReportDataDefinitionItem.SaveToJSON(O: TJSONObject);
begin
  O.Add('name',Name);
  O.Add('type',DataType);
  O.Add('config',Config.Clone);
  O.Add('master',Master);
end;

procedure TFPReportDataDefinitionItem.LoadFromJSON(O: TJSONObject);

Var
  C : TJSONObject;

begin
  Name:=O.Get('name',Name);
  DataType:=O.Get('type',DataType);
  C:=O.Get('config',TJSONObject(Nil));
  if Assigned(C) then
    Config:=C;
  Master:=O.Get('master','');
end;

function TFPReportDataDefinitionItem.Clone(aNewName: String): TFPReportDataDefinitionItem;
begin
  Result:=Collection.Add as TFPReportDataDefinitionItem;
  Result.Assign(Self);
  Result.Name:=aNewName;
end;

function TFPReportDataDefinitionItem.CreateDataSet(AOwner: TComponent): TDataset;

Var
  H : TFPReportDataHandler;

begin
  H:=TFPCustomReportDataManager.GetTypeHandler(DataType);
  try
    Result:=H.CreateDataset(AOwner,Config);
  finally
    H.Free;
  end;
end;


function TFPReportDataDefinitionItem.Check: String;

Var
  H : TFPReportDataHandler;

begin
  If (Name='') then
    Result:=SErrNeedName
  else if (DataType='') then
    Result:=SErrNeedDataType
  else
    begin
    H:=TFPCustomReportDataManager.GetTypeHandler(DataType);
    try
      if H=Nil then
        Result:=Format(SErrInvalidDataType,[DataType])
      else
        Result:=H.CheckConfig(Config);
    Finally
      H.Free;
    end;
    end;
end;

constructor TFPReportDataDefinitionItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FConfig:=TJSONObject.Create;
end;

destructor TFPReportDataDefinitionItem.Destroy;
begin
  FreeAndNil(FConfig);
  inherited Destroy;
end;

Finalization
  FreeAndNil(TFPCustomReportDataManager.FTypesList);
end.

