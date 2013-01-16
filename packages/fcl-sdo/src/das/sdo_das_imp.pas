{
    This file is part of the Free Pascal Class Library SDO Implementation
    Copyright (c) 2012 by Inoussa OUEDRAOGO
    Free Pascal development team

    This unit implements a SDO DAS implementation.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$INCLUDE sdo_global.inc}
unit sdo_das_imp;

interface
uses
  SysUtils, Classes, Contnrs, DB,
  sdo, sdo_das, sdo_consts, sdo_types, data_acces_intf;

type

  TSDODAS = class(TInterfacedObject,ISDODAS)
  private
    FFactory : ISDODataFactory;
    FObjectHandlerList : TObjectList;
  private
    procedure AddDatasetType();
    function IndexOfHandler(const AType : ISDOType) : Integer;
  protected
    function ExecuteQuery(
      const ADac : TDataAccessInterface;
      const AQuery : string;
      const AQueryParams : array of Variant;
      const ARowType : ISDOType;
      const AResList : ISDODataObjectList;
      const AOptions : TSDODASOptions
    ) : ISDODataObjectList;overload;
    function ExecuteQuery(
      const ADac : TDataAccessInterface;
      const AQuery : string;
      const ARowType : ISDOType;
      const AResList : ISDODataObjectList;
      const AOptions : TSDODASOptions
    ) : ISDODataObjectList;overload; 
    function ExecuteQuery(
      const ADac : TDataAccessInterface;
      const AQuery : string;     
      const AQueryParams : array of Variant 
    ) : ISDODataObject;overload; 
    function ExecuteQuery(
      const ADac : TDataAccessInterface;
      const AQuery : string
    ) : ISDODataObject;overload;
    function ExecuteQuery(
      const ADac          : TDataAccessInterface;
      const AQuery        : string;
      const AContainer    : ISDODataObject;
      const ADestListName : string;
      const AOptions      : TSDODASOptions
    ) : ISDODataObjectList; overload;
    procedure UpdateDataStore(
      const ADac : TDataAccessInterface;
      const AChanges : ISDOChangeSummary
    );

    function GetDataFactory() : ISDODataFactory;
    procedure RegisterObjectHandler(
      const ATargetObjectType : ISDOType;
      const AHandler : ISDODASObjectHandler
    );
    function FindObjectHandler(const ATargetObjectType : ISDOType) : ISDODASObjectHandler;
  public
    constructor Create(AFactory : ISDODataFactory);
    destructor Destroy(); override;
  end;

  TSDODASSqlInfo = class(TPersistent)
  private
    FKeyFieldName: string;
    FTableName: string;
  published
    property TableName : string read FTableName write FTableName;
    property KeyFieldName : string read FKeyFieldName write FKeyFieldName;
  end;

  TSDODASSqlObjectHandler = class(TInterfacedObject,ISDODASObjectHandler)
  private
    FSqlInfo : TSDODASSqlInfo;
  protected
    procedure HandleCreate(
      const ADas : ISDODAS;
      const AObject : ISDODataObject
    );
    procedure HandleDelete(
      const ADas : ISDODAS;
      const AObject : ISDODataObject
    );
    procedure HandleUpdate(
      const ADas : ISDODAS;
      const AObject : ISDODataObject
    );

    procedure UpdateDataStore(
      const ADas : ISDODAS;
      const AObject : ISDODataObject;
      const AUpdateKind : TChangeType
    );
  public
    constructor Create(ASqlInfo : TSDODASSqlInfo);
    destructor Destroy(); override;
  end;

  
resourcestring
  SDODAS_MSG_NO_HANDLER_FOUND = 'No handler found for this type : "%s".';
  SDODAS_MSG_INALID_PARAMS = 'Invalid parameter(s) : "%s".';

implementation
uses
  sdo_das_utils, sdo_imp_utils;

function Creator(AFactory : ISDODataFactory) : ISDODAS;
begin
  Result := TSDODAS.Create(AFactory) as ISDODAS;
end;

{ TSDODAS }

procedure TSDODAS.AddDatasetType();
var
  locDatasetType : ISDOType;
begin
  FFactory.AddType(s_db_namespace,s_DataRowType,[tfIsOpen]);
  FFactory.AddType(s_db_namespace,s_DataSetType,[tfIsOpen]);
  locDatasetType := FFactory.getType(s_db_namespace,s_DataSetType);
  FFactory.addProperty(
    locDatasetType,s_RowCount,FFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[IntegerType]),
    []
  );
  FFactory.addProperty(
    locDatasetType,s_Row,FFactory.getType(s_db_namespace,s_DataRowType),
    [pfIsMany,pfIsContainment]
  );
  FFactory.addProperty(
    locDatasetType,s_changeSummary,sdo_namespace,s_changeSummary,[pfIsReadOnly]
  );
end;

constructor TSDODAS.Create(AFactory: ISDODataFactory);
begin
  Assert(AFactory <> nil);
  FFactory := AFactory;
  if ( FFactory.getTypes().find(s_db_namespace,s_DataSetType) = nil ) then
    AddDatasetType();
  FObjectHandlerList := TObjectList.Create(True);
end;    

function TSDODAS.ExecuteQuery(
  const ADac : TDataAccessInterface;
  const AQuery: string;
  const ARowType : ISDOType;
  const AResList: ISDODataObjectList;
  const AOptions : TSDODASOptions
) : ISDODataObjectList;  
begin
  Result := ExecuteQuery(ADac,AQuery,[],ARowType,AResList,AOptions);
end;

function TSDODAS.ExecuteQuery(
  const ADac : TDataAccessInterface;
  const AQuery: string;         
  const AQueryParams : array of Variant;
  const ARowType : ISDOType;
  const AResList: ISDODataObjectList;
  const AOptions : TSDODASOptions
) : ISDODataObjectList;
var
  ds : TDataset;
  f : TField;
  i, fieldCount : PtrInt;
  objRec : ISDODataObject;
  p : ISDOProperty;
  pl : ISDOPropertyList;
  locAddProp : Boolean;
  locPropMap : array of ISDOProperty;
begin
  Assert(ADac <> nil);
  Assert(ARowType <> nil);
  Assert(AResList <> nil);
  locAddProp := ARowType.isOpenType() and ( sdoAddProperty in AOptions );
  if (Length(AQueryParams) = 0) then
    ds := ADac.ExecuteDataset(AQuery)
  else  
    ds := ADac.ExecuteDataset(AQuery,AQueryParams);
  try
    if not ds.IsEmpty() then begin
      fieldCount := ds.FieldCount;
      SetLength(locPropMap,fieldCount);
      pl := ARowType.getProperties();
      for i := 0 to Pred(fieldCount) do begin
        locPropMap[i] := pl.find(ds.Fields[i].FieldName);
      end;
      ds.First();
      while not ds.Eof do begin
        objRec := FFactory.createNew(ARowType);
        for i := 0 to Pred(fieldCount) do begin
          f := ds.Fields[i];
          p := locPropMap[i];
          if ( p = nil ) and locAddProp then begin
            FFactory.addProperty(
              objRec,f.FieldName,
              FFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[FieldTypeToSDOTypeMAP[f.DataType]]),
              []
            );
            p := objRec.getProperty(f.FieldName);
          end;
          if ( p <> nil ) then
            AssignValue(objRec,p,f);
        end;
        AResList.append(objRec);
        ds.Next();
      end;
    end;
  finally
    ds.Free();
    SetLength(locPropMap,0);
  end;
  Result := AResList;
end;

destructor TSDODAS.Destroy;
begin
  FObjectHandlerList.Free();
  inherited;
end;  

function TSDODAS.ExecuteQuery(
  const ADac: TDataAccessInterface;
  const AQuery: string
) : ISDODataObject;
begin
  Result := ExecuteQuery(ADac,AQuery,[]);
end;

function TSDODAS.ExecuteQuery(
  const ADac: TDataAccessInterface;
  const AQuery: string;     
  const AQueryParams : array of Variant 
) : ISDODataObject;
var
  dsType : ISDOType;
  ls : ISDODataObjectList;
begin
  dsType := FFactory.getTypes().find(s_db_namespace,s_DataSetType);
  if ( dsType = nil ) then begin
    AddDatasetType();
    dsType := FFactory.getType(s_db_namespace,s_DataSetType);
  end;
  Result := FFactory.createNew(dsType);
  ls := Result.getList(s_Row);
  ExecuteQuery(ADac,AQuery,AQueryParams,FFactory.getType(s_db_namespace,s_DataRowType),ls,[sdoAddProperty]);
  Result.setInteger(s_RowCount,ls.size());
end;

procedure TSDODAS.RegisterObjectHandler(const ATargetObjectType: ISDOType;
  const AHandler: ISDODASObjectHandler);
begin

end;

procedure TSDODAS.UpdateDataStore(
  const ADac : TDataAccessInterface;
  const AChanges : ISDOChangeSummary
);
var
  ls : ISDOChangedDataObjectList;
  locSelfIntf : ISDODAS;
  c : PtrInt;

  procedure HandleUpdateType(const AChangeType : TChangeType);
  var
    k : PtrInt;
    objHandler : ISDODASObjectHandler;
    obj : ISDODataObject;
  begin
    for k := 0 to Pred(c) do begin
      if ( ls.getType(k) = AChangeType ) then begin
        obj := ls.getDataObject(k);
        objHandler := FindObjectHandler(obj.getType());
        if ( objHandler = nil ) then
          raise ESDODASException.CreateFmt(SDODAS_MSG_NO_HANDLER_FOUND,[obj.getType().getURI() + '#' + obj.getType().getName()]);
        objHandler.UpdateDataStore(locSelfIntf,obj,AChangeType);
      end;
    end;
  end;

begin
  if ( AChanges = nil ) then
    Exit;
  ls := AChanges.getChangedDataObjects();
  c := ls.size();
  if ( c > 0 ) then begin
    locSelfIntf := Self as ISDODAS;
    HandleUpdateType(ctDelete);
    HandleUpdateType(ctChange);
    HandleUpdateType(ctCreate);
  end;
end;


function TSDODAS.IndexOfHandler(const AType: ISDOType): Integer;
var
  i, c : Integer;
begin
  Result := -1;
  c := FObjectHandlerList.Count;
  if ( c > 0 ) then begin
    for i := 0 to Pred(c) do begin
      if TSDODASHandlerItem(FObjectHandlerList[i]).TargetType.equals(AType) then begin
        Result := i;
        Break;
      end;
    end;
  end;
end;

function TSDODAS.FindObjectHandler(const ATargetObjectType: ISDOType): ISDODASObjectHandler;
var
  i : Integer;
begin
  i := IndexOfHandler(ATargetObjectType);
  if ( i > -1 ) then
    Result := TSDODASHandlerItem(FObjectHandlerList[i]).Handler
  else
    Result := nil;
end;

function TSDODAS.GetDataFactory() : ISDODataFactory;
begin
  Result := FFactory;
end;

function TSDODAS.ExecuteQuery(
  const ADac          : TDataAccessInterface;
  const AQuery        : string;
  const AContainer    : ISDODataObject;
  const ADestListName : string;
  const AOptions      : TSDODASOptions
): ISDODataObjectList;

  procedure AddProp();
  var
    f : ISDODataFactory;
  begin
    f := GetDataFactory();
    f.addProperty(AContainer,ADestListName,f.getType(s_db_namespace,s_DataRowType),[pfIsMany,pfIsContainment,pfIsNotNullable]);
  end;

var
  locList : ISDODataObjectList;
begin
  if ( AContainer.getInstanceProperties().find(ADestListName) = nil ) then
    AddProp();
  locList := AContainer.getList(ADestListName);
  Result := ExecuteQuery(
              ADac,
              AQuery,
              FFactory.getType(s_db_namespace,s_DataRowType),
              locList,
              AOptions
            );
end;

{ TSDODASSqlObjectHandler }

constructor TSDODASSqlObjectHandler.Create(ASqlInfo: TSDODASSqlInfo);
begin
  if ( ASqlInfo = nil ) then
    raise ESDODASException.CreateFmt(SDODAS_MSG_INALID_PARAMS,['ASqlInfo']);
  inherited Create();
  FSqlInfo := ASqlInfo;
end;

destructor TSDODASSqlObjectHandler.Destroy();
begin
  FSqlInfo.Free();
  inherited;
end;

procedure TSDODASSqlObjectHandler.HandleCreate(const ADas: ISDODAS;
  const AObject: ISDODataObject);
begin

end;

procedure TSDODASSqlObjectHandler.HandleDelete(
  const ADas : ISDODAS;
  const AObject : ISDODataObject
);
begin

end;

procedure TSDODASSqlObjectHandler.HandleUpdate(
  const ADas: ISDODAS;
  const AObject: ISDODataObject
);
begin

end;

procedure TSDODASSqlObjectHandler.UpdateDataStore(
  const ADas: ISDODAS;
  const AObject: ISDODataObject;
  const AUpdateKind: TChangeType
);
begin
  case AUpdateKind of
    ctCreate : HandleCreate(ADas,AObject);
    ctChange : HandleUpdate(ADas,AObject);
    ctDelete : HandleDelete(ADas,AObject);
    else
      raise ESDODASException.CreateFmt(SDODAS_MSG_INALID_PARAMS,['AUpdateKind']);
  end;
end;

initialization
  DefaultImplementor := {$IFDEF FPC}@{$ENDIF}Creator;

finalization
  DefaultImplementor := nil;
  
end.
