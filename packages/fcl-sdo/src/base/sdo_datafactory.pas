{
    This file is part of the Free Pascal Class Library SDO Implementation
    Copyright (c) 2012 by Inoussa OUEDRAOGO
    Free Pascal development team

    This unit implements a SDO data factory

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$INCLUDE sdo_global.inc}
unit sdo_datafactory;

interface
uses SysUtils, Classes,
     sdo, sdo_type, sdo_types;

type

  { TSDOBaseDataFactory }

  TSDOBaseDataFactory = class(TInterfacedObject,IInterface,ISDODataFactory)
  private
    FTypeList : ISDOTypeListEx;
  private
    procedure Clear();
  protected
    function findType(const ATypeURI, ATypeName : string) : ISDOTypeEx;{$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure CheckCircularDependencies(const AType : ISDOType);
  protected
		//SDO_API virtual DataFactoryPtr clone();
    function createNew(const AType : ISDOType) : ISDODataObject;overload;
    function createNew(const AUri, ATypeName : string) : ISDODataObject;overload;

    function getType(const AUri, ATypeName : string) : ISDOType;
		function getTypes() : ISDOTypeList;
    procedure AddType(
      const AUri,
            ATypeName      : string;
      const AisSequenced,
            AisOpen,
            AisAbstract,
            AisDataType    : Boolean
    );overload;
    procedure AddType(
      const AUri,
            ATypeName : string;
      const AFlags    : TTypeFlags
    );overload;
    procedure setBaseType(const AType, ABase : ISDOType);overload;
    procedure setBaseType(
      const ATypeURI, ATypeName,
            ABaseURI, ABaseName : string
    );overload;
    procedure setAlias(const ATypeURI, ATypeName, AAlias : string);overload;
    procedure addProperty(
      const ATypeURI, ATypeName,
            APropName, APropTypeUri, APropTypeName : string;
      const AFlags : TPropertyFlags
    );overload;
    procedure addProperty(
      const ATypeURI, ATypeName,
            APropName            : string;
      const APropType : ISDOType;
      const AFlags : TPropertyFlags
    );overload;
    procedure addProperty(
      const AType : ISDOType;
      const APropName, APropTypeUri, APropTypeName : string;
      const AFlags : TPropertyFlags
    );overload;
    procedure addProperty(
      const AType     : ISDOType;
      const APropName : string;
      const APropType : ISDOType;
      const AFlags : TPropertyFlags
    );overload;
    procedure setAlias(const ATypeUri, ATypeName, APropName, AAlias : string);overload;
    procedure addProperty(
            ADataObject : ISDODataObject;
      const APropName : string;
      const APropType : ISDOType;
      const AFlags : TPropertyFlags
    );overload;
    function CreateList(AType : ISDOType) : ISDODataObjectList;overload;
    function CreateList(const AUri, ATypeName : string) : ISDODataObjectList;overload;
  public
    constructor Create();
    destructor Destroy();override;
  end;

  TSDODataFactory = class(TSDOBaseDataFactory,IInterface,ISDODataFactory)
  end;

implementation

uses
  sdo_imp_utils, sdo_dataobject, sdo_consts;

{ TSDOBaseDataFactory }

procedure TSDOBaseDataFactory.AddType(
  const AUri, ATypeName: string;
  const AisSequenced, AisOpen, AisAbstract, AisDataType: Boolean
);
var
  typ : ISDOType;
begin
  if not IsValidName(ATypeName) then
    raise ESDOIllegalArgumentException.Create('ATypeName');
  if AisDataType then begin
    if AisSequenced then
      raise ESDOIllegalArgumentException.Create('AisSequenced');
    if AisOpen then
      raise ESDOIllegalArgumentException.Create('AisOpen');
  end;
  if ( findType(AUri,ATypeName) <> nil ) then
    raise ESDODuplicatedItemException.Create(AUri + ':' + ATypeName);
  if AisDataType then
    typ := TSDOUserDefinedSimpleType.Create(Self as ISDODataFactory, ATypeName,AUri,AisAbstract)
  else
    typ := TSDOObjectType.Create(Self as ISDODataFactory, ATypeName,AUri,AisSequenced,AisOpen,AisAbstract);
  FTypeList.insert(typ);
end;

type
  TCircularDependencyStackItem = packed record
    _theType : ISDOType;
    _property : ISDOProperty;
    _listIndex : PtrInt;
  end;
procedure TSDOBaseDataFactory.addProperty(
  const ATypeURI, ATypeName,
        APropName, APropTypeUri, APropTypeName: string;
  const AFlags: TPropertyFlags
);
begin
  addProperty(
    getType(ATypeURI, ATypeName),
    APropName, APropTypeUri, APropTypeName,
    AFlags
  );
end;

procedure TSDOBaseDataFactory.AddType(
  const AUri, ATypeName: string;
  const AFlags: TTypeFlags
);
begin
  AddType(
    AUri,
    ATypeName,
    tfIsSequenced in AFlags,
    tfIsOpen      in AFlags,
    tfIsAbstract  in AFlags,
    tfIsDataType  in AFlags
  );
end;

procedure TSDOBaseDataFactory.CheckCircularDependencies(const AType: ISDOType);
const
  MAX_DEPT = 256;
var
  stack : array[0..(MAX_DEPT - 1)] of TCircularDependencyStackItem;
    stackIndex : PtrInt;
  typeList : array[0..(MAX_DEPT - 1)] of ISDOType;
    typeListIndex : PtrInt;
  currentType : ISDOType;

  procedure StackPush(const ALocType : ISDOType; const AProperty : ISDOProperty);{$IFDEF USE_INLINE}inline;{$ENDIF}
  begin
    Inc(stackIndex);
    with stack[stackIndex] do begin
      _theType := ALocType;
      _property := AProperty;
      _listIndex := typeListIndex;
    end;
  end;

  function StackPop():Boolean ;{$IFDEF USE_INLINE}inline;{$ENDIF}
  begin
    Result := ( stackIndex >= 0 );
    if Result then begin
      with stack[stackIndex] do begin
        currentType := _theType;
        typeListIndex := _listIndex;
      end;
      Dec(stackIndex);
    end;
  end;

  procedure LocalAddType(const ALocType : ISDOType);{$IFDEF USE_INLINE}inline;{$ENDIF}
  var
    k : PtrInt;
    k_found : Boolean;
    k_type : ISDOObjectType;
  begin
    k_type := ALocType as ISDOObjectType;
    while ( k_type <> nil ) do begin
      k_found := False;
      if ( typeListIndex < 0 ) then begin
        typeListIndex := -1;
      end else begin
        for k := 0 to typeListIndex do begin
          if ( typeList[k] = k_type ) then begin
            k_found := True;
            Break;
          end;
        end;
      end;
      if not k_found then begin
        Inc(typeListIndex);
        typeList[typeListIndex] := k_type;
      end;
      k_type := k_type.getBaseType() as ISDOObjectType;
    end;
  end;

  function isInTypeList(const ALocType : ISDOType) : Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}
  var
    k : PtrInt;
  begin
    Result := False;
    if ( typeListIndex >= 0 ) then begin
      for k := 0 to typeListIndex do begin
        if ( typeList[k] = ALocType ) then begin
          Result := True;
          Break;
        end;
      end;
    end;
  end;

  function isInStack(const ALocType : ISDOType) : Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}
  var
    k : PtrInt;
  begin
    Result := False;
    if ( stackIndex >= 0 ) then begin
      for k := 0 to stackIndex do begin
        if ( stack[k]._theType = ALocType ) then begin
          Result := True;
          Break;
        end;
      end;
    end;
  end;

  function getErrorPath(const ACurrentProp : ISDOProperty) : string;
  var
    locPath : string;
  begin
    Result := '';
    while ( stackIndex > 0 ) do begin
      if ( stack[stackIndex]._property <> nil ) then
        locPath := stack[stackIndex]._property.getName()
      else
        locPath := '(Parent)';
      Result := Format('%s->%s',[locPath,Result]);
      Dec(stackIndex);
    end;
    if ( ACurrentProp <> nil ) then
      Result := Format('%s->%s',[ACurrentProp.getName(),Result]);
    if ( Length(Result) > 0 ) then
      Delete(Result,Pred(Length(Result)),2);
  end;
  
var
  typeX : ISDOTypeEx;
  prpLs : ISDOPropertyList;
  prp : ISDOProperty;
  prpType : ISDOType;
  i, c : PtrInt;
begin
  if ( AType <> nil ) and AType.isDataObjectType() then begin
    typeX := AType as ISDOTypeEx;
    if ( not typeX.isUsed ) then begin
      stackIndex := -1;
      typeListIndex := -1;

      try
        StackPush(AType,nil);
        while StackPop() do begin
          if currentType.isDataObjectType() then begin
            LocalAddType(currentType);
            prpLs := currentType.getProperties();
            c := prpLs.getCount();
            for i := 0 to Pred(c) do begin
              prp := prpLs.getItem(i);
              prpType := prp.getType();
              if ( not prpType.equals(currentType) ) and isInTypeList(prpType) then begin
                raise ESDOCircularDependencyTypeException.Create(
                        currentType.getName(),
                        prp.getName(),
                        prpType.getName()
                      );
              end;
              if ( not prpType.equals(currentType) ) and ( not isInStack(prpType) ) then
                StackPush(prpType,prp);
            end;
          end;
        end;
      except
        on e : ESDOCircularDependencyTypeException do begin
          e.Message := Format('%s%sPath : %s',[e.Message,sLineBreak,getErrorPath(prp)]);
          raise;
        end;
      end;
    end;
  end;
end;

procedure TSDOBaseDataFactory.Clear();
var
  i, c : PtrInt;
  t : ISDOTypeEx;
  toX : ISDOObjectType;
begin
  if ( FTypeList <> nil ) then begin
    c := FTypeList.getCount();
    if ( c > 0 ) then begin
      for i := Pred(c) downto 0 do begin
        t := FTypeList.getItem(i) as ISDOTypeEx;
        if t.isDataObjectType() and ( not t.isUsed() ) then begin
          toX := t as ISDOObjectType;
          toX.Clear();
        end;
      end;
    end;
  end;
end;

constructor TSDOBaseDataFactory.Create();
  procedure AddPrimitiveTypes(const ATypeList : ISDOTypeListEx);
    procedure AddAlias(
      const AType  : ISDOTypeEx;
      const AALias : array of TSDOString
    );
    var
      k : Integer;
    begin
      for k := Low(AALias) to High(AALias) do
        AType.setAlias(AALias[k]);
    end;

  var
    selfIntf : ISDODataFactory;
    typeRef : ISDOTypeEx;
  begin
    selfIntf := Self as ISDODataFactory;
    typeRef := TSDOBooleanType.Create(selfIntf);
    ATypeList.insert(typeRef);
      typeRef.setAlias('boolean');
    typeRef := TSDOByteType.Create(selfIntf);
    ATypeList.insert(typeRef);   
      typeRef.setAlias('byte');
{$IFDEF HAS_SDO_BYTES}
    ATypeList.insert(TSDOBytesType.Create(selfIntf));
{$ENDIF HAS_SDO_BYTES}
    ATypeList.insert(TSDOChangeSummaryType.Create(selfIntf));
{$IFDEF HAS_SDO_CHAR}
    ATypeList.insert(TSDOCharacterType.Create(selfIntf));
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
    typeRef := TSDOCurrencyType.Create(selfIntf);
    ATypeList.insert(typeRef);
      AddAlias(typeRef,['decimal']);
{$ENDIF HAS_SDO_CURRENCY}
{$IFDEF HAS_SDO_DOUBLE}
    typeRef := TSDODoubleType.Create(selfIntf);
    ATypeList.insert(typeRef);
      AddAlias(typeRef,['double','decimal']);
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
    typeRef := TSDOFloatType.Create(selfIntf);
    ATypeList.insert(typeRef);
      AddAlias(typeRef,['float']);
{$ENDIF HAS_SDO_FLOAT}
    typeRef := TSDODateTimeType.Create(selfIntf);
    ATypeList.insert(typeRef);
      AddAlias(typeRef,['dateTime']);
    typeRef := TSDOIntegerType.Create(selfIntf);
    ATypeList.insert(typeRef);
      AddAlias(typeRef,['int']);
{$IFDEF HAS_SDO_LONG}             
    typeRef := TSDOLongType.Create(selfIntf);
    ATypeList.insert(typeRef);
      AddAlias(typeRef,['long']);
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
    typeRef := TSDOShortType.Create(selfIntf);
    ATypeList.insert(typeRef);
      AddAlias(typeRef,['short']);
{$ENDIF HAS_SDO_SHORT}
    ATypeList.insert(TSDOObjectType.Create(selfIntf,SDOTypeDefaultTypeNames[ObjectType],sdo_namespace,False,False,True));
    typeRef := TSDOStringType.Create(selfIntf);
    ATypeList.insert(typeRef);
      AddAlias(typeRef,['string','anyURI','ID','token','language']);
  end;

begin
  inherited;
  FTypeList := TSDOTypeList.Create();
  AddPrimitiveTypes(FTypeList);
  AddType(sdo_namespace,s_datagraph,[]);
  AddType(sdo_namespace,s_xsd,[]);
  AddType(sdo_namespace,s_changeSummaryListObject,[tfIsOpen]);
end;

function TSDOBaseDataFactory.createNew(const AType : ISDOType): ISDODataObject;
var
  typ : ISDOType;
  objType : ISDOObjectType;
begin
  if ( AType = nil ) then
    raise ESDOIllegalArgumentException.Create('AType');
  typ := getType(AType.getURI(),AType.getName());
  if not Supports(typ,ISDOObjectType,objType) then
    raise ESDOIllegalArgumentException.Create('AType');
  if typ.isAbstractType() then
    raise ESDOAbstractTypeException.Create(typ.getName());
  CheckCircularDependencies(AType);
  if AType.isOpenType() then
    Result := TSDOOpenedDataObject.Create(objType,nil,nil) as ISDODataObject
  else
    Result := TSDODataObject.Create(objType,nil,nil) as ISDODataObject;
end;

function TSDOBaseDataFactory.createNew(const AUri,ATypeName: string): ISDODataObject;
begin
  Result := createNew(getType(AUri,ATypeName));
end;

destructor TSDOBaseDataFactory.Destroy();
begin
  Clear();
  FTypeList := nil;
  inherited;
end;

function TSDOBaseDataFactory.findType(const ATypeURI,ATypeName: string): ISDOTypeEx;
begin
  Result := getTypes().find(ATypeURI,ATypeName) as ISDOTypeEx;
end;

function TSDOBaseDataFactory.getType(const AUri,ATypeName: string): ISDOType;
begin
  Result := findType(AUri,ATypeName);
  if ( Result = nil ) then
    raise ESDOTypeNotFoundException.Create(ATypeName);
end;

function TSDOBaseDataFactory.getTypes() : ISDOTypeList;
begin
  Result := FTypeList;
end;

procedure TSDOBaseDataFactory.setAlias(const ATypeUri, ATypeName, APropName, AAlias: string);
var
  typ : ISDOTypeEx;
  prp : ISDOPropertyEx;
begin
  typ := findType(ATypeURI,ATypeName);
  if ( typ = nil ) then
    raise ESDOTypeNotFoundException.Create(ATypeName);
  if typ.isUsed() then
    raise ESDOUnsupportedOperationException.Create('setAlias');
  prp := typ.getProperty(APropName) as ISDOPropertyEx;
  prp.setAlias(AAlias);
end;

procedure TSDOBaseDataFactory.setAlias(const ATypeURI, ATypeName, AAlias: string);
var
  typ : ISDOTypeEx;
begin
  typ := findType(ATypeURI,ATypeName);
  if ( typ = nil ) then
    raise ESDOTypeNotFoundException.Create(ATypeName);
  if typ.isUsed() then
    raise ESDOUnsupportedOperationException.Create('setAlias');
  typ.setAlias(AAlias);
end;

procedure TSDOBaseDataFactory.setBaseType(const AType, ABase: ISDOType);
var
  typ : ISDOTypeEx;
begin
  if ( AType = nil ) then
    raise ESDOIllegalArgumentException.Create('AType');
  typ := findType(AType.getURI(),AType.getName());
  if ( typ = nil ) then
    raise ESDOTypeNotFoundException.Create(AType.getName());
  if typ.isUsed() then
    raise ESDOUnsupportedOperationException.Create('setAlias');
  typ.setBaseType(ABase);
end;

procedure TSDOBaseDataFactory.setBaseType(
  const ATypeURI, ATypeName, ABaseURI, ABaseName: string
);
var
  typ : ISDOTypeEx;
begin
  typ := findType(ATypeURI,ATypeName);
  if ( typ = nil ) then
    raise ESDOTypeNotFoundException.Create(ATypeName);
  if typ.isUsed() then
    raise ESDOUnsupportedOperationException.Create('setAlias');
  typ.setBaseType(getType(ABaseURI,ABaseName));
end;


procedure TSDOBaseDataFactory.addProperty(
  const ATypeURI, ATypeName,
        APropName: string;
  const APropType: ISDOType;
  const AFlags: TPropertyFlags
);
begin
  addProperty(
    getType(ATypeURI, ATypeName),
    APropName, APropType,
    AFlags
  );
end;

procedure TSDOBaseDataFactory.addProperty(
  const AType: ISDOType;
  const APropName: string;
  const APropType: ISDOType;
  const AFlags: TPropertyFlags
);
var
  typ : ISDOTypeEx;
  prpList : ISDOPropertyListEx;
begin
  if ( AType = nil ) then
    raise ESDOIllegalArgumentException.Create('AType');
  if IsStrEmpty(APropName) then
    raise ESDOIllegalArgumentException.Create('APropType');
  if ( APropType = nil ) then
    raise ESDOIllegalArgumentException.Create('APropType');
  typ := findType(AType.getURI(),AType.getName());
  if ( typ = nil ) then
    raise ESDOIllegalArgumentException.Create('AType');
  if ( typ <> AType ) then
    raise ESDOIllegalArgumentException.Create('AType');
  if typ.isUsed() then
    raise ESDOUnsupportedOperationException.Create('addProperty');
  prpList := typ.getProperties() as ISDOPropertyListEx;
  if ( prpList.find(APropName) <> nil ) then
    raise ESDODuplicatedItemException.Create(APropName);
  if APropType.isDataType() and ( pfIsContainment in AFlags ) then
    raise ESDOIllegalArgumentException.Create('AIsContainment');
  if APropType.isChangeSummaryType() then begin
    if ( pfIsMany in AFlags ) then
      raise ESDOIllegalArgumentException.Create('AIsMany');
    if not ( pfIsReadOnly in AFlags ) then
      raise ESDOIllegalArgumentException.Create('AIsReadOnly');  
    if (pfIsAttribute in AFlags) then
      raise ESDOIllegalArgumentException.Create('AIsAttribute');
  end;
  prpList.add(TSDOProperty.Create(APropName,APropType,AFlags,typ));
end;

procedure TSDOBaseDataFactory.addProperty(
  const AType: ISDOType;
  const APropName, APropTypeUri, APropTypeName: string;
  const AFlags: TPropertyFlags
);
begin
  addProperty(
    AType,
    APropName, getType(APropTypeUri, APropTypeName),
    AFlags
  );
end;

procedure TSDOBaseDataFactory.addProperty(
        ADataObject: ISDODataObject;
  const APropName: string; const APropType: ISDOType; const AFlags: TPropertyFlags
);
var
 oX : ISDODataObjectEx;
begin
  if ( ADataObject = nil ) or
     ( not ADataObject.getType().isOpenType() ) or
     ( not Supports(ADataObject,ISDODataObjectEx,oX) )
  then begin
    raise ESDOIllegalArgumentException.Create('ADataObject');
  end;
  oX.addProperty(APropName,APropType,AFlags);
end;

function TSDOBaseDataFactory.CreateList(AType : ISDOType) : ISDODataObjectList;
begin
  Result := TSDODataObjectList.Create(AType) as ISDODataObjectList;
end;

function TSDOBaseDataFactory.CreateList(
  const AUri, ATypeName : string
) : ISDODataObjectList;
begin
  Result := TSDODataObjectList.Create(getType(AUri,ATypeName)) as ISDODataObjectList;
end;

end.
