{
    This file is part of the Free Pascal Class Library SDO Implementation
    Copyright (c) 2012 by Inoussa OUEDRAOGO
    Free Pascal development team

    This unit implements basic SDO data type definitions

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$INCLUDE sdo_global.inc}
unit sdo_type;

interface
uses SysUtils, Classes,
     sdo_types, sdo;

const
  VALUE_STATUS_LENGTH = 1;

type

  ISDOTypeEx = interface(ISDOType)
    ['{19C39BF1-397C-4A10-98B2-EB4D23CB027C}']
    procedure setBaseType(const ABaseType : ISDOType);
    procedure setAlias(const AAlias : string);
    procedure setUsedFlag(const AUsed : Boolean);
    function isUsed() : Boolean;
    function getFieldSize() : PtrUInt;
    function inherits(const AType : ISDOType) : Boolean;
  end;

  ISDOObjectType = interface(ISDOTypeEx)
    ['{19C39BF1-397C-4A10-98B2-EB4D23CB027C}']
    procedure AddProperty(
      const AName : string;
      const AType : ISDOType;
      const AFlags  : TPropertyFlags
    );
    procedure DropProperty(const AProperty : ISDOProperty);
    function IsOwnerOf(const AProp : ISDOProperty) : Boolean;
    procedure Clear();
  end;

  ISDOTypeListEx = interface(ISDOTypeList)
    ['{7C23199E-F9CB-4704-AD51-93C7FB22BB6A}']
    procedure insert(const AType : ISDOType);
  end;

  TSDOTypeAsbtract = class(TInterfacedObject,IInterface,ISDOType,ISDOTypeEx)
  private
    FOwner : Pointer; //ISDODataFactory;
    FName : string;
    FURI : string;
    FALiasList : TStringList;
    FFlags : TTypeFlags;
    FUsed : Boolean;
  protected
    procedure CheckNotUsed(const AOperationName : string);{$IFDEF USE_INLINE}inline;{$ENDIF}
  protected
    function getName() : string;
	  function getAlias(const AIndex : PtrInt) : string;
	  function getAliasCount() : PtrInt;
	  function getBaseType() : ISDOType; virtual;
	  function getURI() : string;virtual;
	  function getProperties() : ISDOPropertyList;virtual;abstract;
    function getProperty(const APropertyName : string) : ISDOProperty;overload;virtual;abstract;
    function getProperty(const APropertyIndex : Integer) : ISDOProperty;overload;virtual;abstract;
	  function getPropertyIndex(const APropertyName : string) : Integer; virtual;abstract;
	  function isDataObjectType() : Boolean;virtual;abstract;
	  function isSequencedType() : Boolean; 
	  function isOpenType() : Boolean; 
	  function isAbstractType() : Boolean;
    function isDataType() : Boolean;
    function isChangeSummaryType() : Boolean;virtual;abstract;
    function getTypeEnum() : TSDOTypeKind; virtual;abstract;
    function equals(const AOther : ISDOType) : Boolean; virtual;
    function getFlags() : TTypeFlags;
    // ISDOTypeEx
    procedure setBaseType(const ABaseType : ISDOType);virtual;
    procedure setAlias(const AAlias : string);
    procedure setUsedFlag(const AUsed : Boolean);virtual;
    function isUsed() : Boolean;
    function getFieldSize() : PtrUInt;virtual;abstract;
    function inherits(const AType : ISDOType) : Boolean;virtual;
    function getOwner() : ISDODataFactory;
  public
    constructor Create(const AOwner : ISDODataFactory; const AName, AURI : string);
    destructor Destroy();override;
  end;

  TSDOSimpleType = class(TSDOTypeAsbtract)
  protected
    function getProperties() : ISDOPropertyList;override;
    function getProperty(const APropertyName : string) : ISDOProperty;overload;override;
    function getProperty(const APropertyIndex : Integer) : ISDOProperty;overload;override;
	  function getPropertyIndex(const APropertyName : string) : Integer; override;
	  function isDataObjectType() : Boolean;override;
	  //function isAbstractType() : Boolean;override;
    function isChangeSummaryType() : Boolean;override;
    //function getTypeEnum() : TSDOTypeKind; override;
  public
    constructor Create(const AOwner : ISDODataFactory; const AName, AURI : string);
  end;

  TSDOChangeSummaryType = class(TSDOTypeAsbtract)
  protected
	  function getProperties() : ISDOPropertyList;override;
    function getProperty(const APropertyName : string) : ISDOProperty;overload;override;
    function getProperty(const APropertyIndex : Integer) : ISDOProperty;overload;override;
	  function getPropertyIndex(const APropertyName : string) : Integer; override;
	  function isDataObjectType() : Boolean;override;
    function isChangeSummaryType() : Boolean;override;
    function getTypeEnum() : TSDOTypeKind; override;
    // ISDOTypeEx
    procedure setBaseType(const ABaseType : ISDOType);override;
    //procedure setAlias(const AAlias : string);
    //procedure setUsedFlag(const AUsed : Boolean);virtual;
    //function isUsed() : Boolean;
    function getFieldSize() : PtrUInt;override;
  public
    constructor Create(const AOwner : ISDODataFactory);
  end;

  TSDOConcreteSimpleType = class(TSDOSimpleType)
  end;

  TSDOUserDefinedSimpleType = class(TSDOConcreteSimpleType)
  private
    FBaseType : ISDOType;
  protected
    function getBaseType() : ISDOType; override;
    function getTypeEnum() : TSDOTypeKind; override;
    procedure setBaseType(const ABaseType : ISDOType);override;
    function getFieldSize() : PtrUInt;override;
  public
    constructor Create(
      const AOwner : ISDODataFactory;
      const AName, AURI : string;
      const AIsAbstract : Boolean
    );
  end;

  TSDONativeSimpleType = class(TSDOConcreteSimpleType)
  protected
    procedure setBaseType(const ABaseType : ISDOType);override;
  end;

  TSDOBooleanType = class(TSDONativeSimpleType)
  protected
	  function getTypeEnum() : TSDOTypeKind; override;
    function getFieldSize() : PtrUInt;override;
  public
    constructor Create(const AOwner : ISDODataFactory);
  end;

  TSDOByteType = class(TSDONativeSimpleType)
  protected
	  function getTypeEnum() : TSDOTypeKind; override;
    function getFieldSize() : PtrUInt;override;
  public
    constructor Create(const AOwner : ISDODataFactory);
  end;

{$IFDEF HAS_SDO_BYTES}
  TSDOBytesType = class(TSDONativeSimpleType)
  protected
	  function getTypeEnum() : TSDOTypeKind; override;
    function getFieldSize() : PtrUInt;override;
  public
    constructor Create(const AOwner : ISDODataFactory);
  end;
{$ENDIF HAS_SDO_BYTES}

{$IFDEF HAS_SDO_CHAR}
  TSDOCharacterType = class(TSDONativeSimpleType)
  protected
	  function getTypeEnum() : TSDOTypeKind; override;
    function getFieldSize() : PtrUInt;override;
  public
    constructor Create(const AOwner : ISDODataFactory);
  end;
{$ENDIF HAS_SDO_CHAR}

  TSDOBaseNumericType = class(TSDONativeSimpleType)
  end;

{$IFDEF HAS_SDO_CURRENCY}
  TSDOCurrencyType = class(TSDOBaseNumericType)
  protected
	  function getTypeEnum() : TSDOTypeKind; override;
    function getFieldSize() : PtrUInt;override;
  public
    constructor Create(const AOwner : ISDODataFactory);
  end;
{$ENDIF HAS_SDO_CURRENCY}

  TSDOBaseStringType = class(TSDONativeSimpleType)
  end;

  TSDODateTimeType = class(TSDONativeSimpleType)
  protected
	  function getTypeEnum() : TSDOTypeKind; override;
    function getFieldSize() : PtrUInt;override;
  public
    constructor Create(const AOwner : ISDODataFactory);
  end;

{$IFDEF HAS_SDO_DOUBLE}
  TSDODoubleType = class(TSDOBaseNumericType)
  protected
	  function getTypeEnum() : TSDOTypeKind; override;
    function getFieldSize() : PtrUInt;override;
  public
    constructor Create(const AOwner : ISDODataFactory);
  end;
{$ENDIF HAS_SDO_DOUBLE}

{$IFDEF HAS_SDO_FLOAT}
  TSDOFloatType = class(TSDOBaseNumericType)
  protected
	  function getTypeEnum() : TSDOTypeKind; override;
    function getFieldSize() : PtrUInt;override;
  public
    constructor Create(const AOwner : ISDODataFactory);
  end;
{$ENDIF HAS_SDO_FLOAT}

  TSDOBaseOrdinalType = class(TSDONativeSimpleType)
  end;

  TSDOIntegerType = class(TSDOBaseOrdinalType)
  protected
	  function getTypeEnum() : TSDOTypeKind; override;
    function getFieldSize() : PtrUInt;override;
  public
    constructor Create(const AOwner : ISDODataFactory);
  end;

{$IFDEF HAS_SDO_LONG}
  TSDOLongType = class(TSDOBaseOrdinalType)
  protected
	  function getTypeEnum() : TSDOTypeKind; override;
    function getFieldSize() : PtrUInt;override;
  public
    constructor Create(const AOwner : ISDODataFactory);
  end;
{$ENDIF HAS_SDO_LONG}  

{$IFDEF HAS_SDO_SHORT}
  TSDOShortType = class(TSDOBaseOrdinalType)
  protected
	  function getTypeEnum() : TSDOTypeKind; override;
    function getFieldSize() : PtrUInt;override;
  public
    constructor Create(const AOwner : ISDODataFactory);
  end;
{$ENDIF HAS_SDO_SHORT}

  TSDOStringType = class(TSDOBaseStringType)
  protected
	  function getTypeEnum() : TSDOTypeKind; override;
    function getFieldSize() : PtrUInt;override;
  public
    constructor Create(const AOwner : ISDODataFactory);
  end;

  TSDOBaseObjectType = class(
    TSDOTypeAsbtract,
    IInterface,
    ISDOType,
    ISDOTypeEx,
    ISDOObjectType
  )
  private
    FBaseType : ISDOObjectType;
    FProperties : ISDOPropertyList;
  protected
	  function getBaseType() : ISDOType; override;
	  function getProperties() : ISDOPropertyList; override;
    function getProperty(const APropertyName : string) : ISDOProperty;overload; override;
    function getProperty(const APropertyIndex : Integer) : ISDOProperty;overload; override;
	  function getPropertyIndex(const APropertyName : string) : Integer;  override;
	  function isDataObjectType() : Boolean; override;
    function isChangeSummaryType() : Boolean;override;
    function getTypeEnum() : TSDOTypeKind;  override;
    procedure setBaseType(const ABaseType : ISDOType);override;
    procedure AddProperty(
      const AName : string;
      const AType : ISDOType;
      const AFlags  : TPropertyFlags
    );
    function getFieldSize() : PtrUInt;override;

    //ISDOTypeEx
    procedure setUsedFlag(const AUsed : Boolean);override;
    procedure DropProperty(const AProperty : ISDOProperty);
    function IsOwnerOf(const AProp : ISDOProperty) : Boolean;
    function inherits(const AType : ISDOType) : Boolean;override;
    procedure Clear();
  public
    constructor Create(
      const AOwner : ISDODataFactory;
      const AName, AURI : string;
      const ASequenced  : Boolean;
      const AOpened     : Boolean;
      const AAbstract   : Boolean
    );
    destructor Destroy();override;
  end;

  TSDOObjectType = class(
    TSDOBaseObjectType
  )
  end;

  TSDOTypeList = class(
    TInterfacedObject,
    IInterface,
    ISDOTypeList,
    ISDOTypeListEx
  )
  private
    FList : IInterfaceList;
  protected
    function getCount() : Integer;
    function getItem(const AIndex : Integer) : ISDOType;
    procedure insert(const AType : ISDOType);
    function find(const AUri, AName : string) : ISDOType;
    function getIndex(const AUri, AName : string) : Integer;
  public
    constructor Create();
    destructor Destroy();override;
  end;

  ISDOPropertyEx = interface(ISDOProperty)
    ['{C0ECCD8C-5727-4E1A-A389-BE15F3F8B350}']
    procedure setAlias(const AAlias : string);
    function getBufferOffset() : PtrUInt;
    function setBufferOffset(const AStartingAddress : PtrUInt) : PtrUInt;
    // do not use this : it is only for houskeeping
    procedure ClearType();
  end;

  ISDOPropertyListEx = interface(ISDOPropertyList)
    ['{C67F5513-7833-4DE1-870F-DFBEDFFCF5BA}']
    procedure add(const AProperty : ISDOProperty);
    function SetBase(const ABaseList : ISDOPropertyList) : ISDOPropertyList;
    procedure drop(const AProperty : ISDOProperty);
  end;

  TSDOPropertyAsbtract = class(
    TInterfacedObject,
    IInterface,
    ISDOProperty,
    ISDOPropertyEx
  )
  private
    FName : string;
    FALiasList : TStrings;
    FType : Pointer; //ISDOType;
    FContainingType : Pointer;
    FBufferOffset : PtrUInt;
    FDefault : TValueBuffer;
    FDefaultSet : Boolean;
    FFlags : TPropertyFlags;
    FWeakReferenceToType : Boolean;
    FTypeKind : TSDOTypeKind;
  private
    procedure InternalSetBufferOffset(const AOffset : PtrUInt);{$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure FreeDefaultBuffer();
    procedure CheckIfContainingTypeIsUsed(const AOperation : string);{$IFDEF USE_INLINE}inline;{$ENDIF}
  protected
 	  function getName() : string;
    function getAlias(const AIndex : Integer) : string;
    function getAliasCount() : Integer;
    function getType() : ISDOType;
    function getTypeEnum() : TSDOTypeKind;
    function isMany() : Boolean;
    function isContainment() : Boolean;
    function isReference() : Boolean;
    function getContainingType() : ISDOType;
    function isReadOnly() : Boolean;
    function getOpposite() : ISDOProperty; virtual;
	  function isDefaulted() : Boolean;
    function getStringDefault() : TSDOString;
{$IFDEF HAS_SDO_BYTES}
    function getBytesDefault() : TSDOBytes;
{$ENDIF HAS_SDO_BYTES}
    function getBooleanDefault() : TSDOBoolean;
    function getByteDefault() : TSDOByte;
{$IFDEF HAS_SDO_CHAR}
    function getCharacterDefault() : TSDOChar;
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
    function getCurrencyDefault() : TSDOCurrency;
{$ENDIF HAS_SDO_CURRENCY}
    function getDateDefault() : TSDODate;
{$IFDEF HAS_SDO_DOUBLE}
    function getDoubleDefault() : TSDODouble;
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
    function getFloatDefault() : TSDOFloat;
{$ENDIF HAS_SDO_FLOAT}
    function getIntegerDefault() : TSDOInteger;
{$IFDEF HAS_SDO_LONG}
    function getLongDefault() : TSDOLong;
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
    function getShortDefault() : TSDOShort;
{$ENDIF HAS_SDO_SHORT}
    procedure setDefault(const AValue : TSDOBoolean);overload;
    procedure setDefault(const AValue : TSDOByte);overload;
{$IFDEF HAS_SDO_BYTES}
    procedure setDefault(AValue : TSDOBytes);overload;
{$ENDIF HAS_SDO_BYTES}
{$IFDEF HAS_SDO_CHAR}
    procedure setDefault(const AValue : TSDOChar);overload;
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY }
    procedure setDefaultCurrency(const AValue : TSDOCurrency);
{$ENDIF HAS_SDO_CURRENCY }
    procedure setDefault(const AValue : TSDODate);overload;
{$IFDEF HAS_SDO_DOUBLE}
    procedure setDefault(const AValue : TSDODouble);overload;
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
    procedure setDefault(const AValue : TSDOFloat);overload;
{$ENDIF HAS_SDO_FLOAT}
    procedure setDefault(const AValue : TSDOInteger);overload;
{$IFDEF HAS_SDO_LONG}
    procedure setDefault(const AValue : TSDOLong);overload;
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
    procedure setDefault(const AValue : TSDOShort);overload;
{$ENDIF HAS_SDO_SHORT}
    procedure setDefault(const AValue : TSDOString);overload;

    function isNullable() : Boolean;
    function isAttribute() : Boolean;

    // ISDOPropertyEx
    procedure setAlias(const AAlias : string);
    function setBufferOffset(const AStartingAddress : PtrUInt) : PtrUInt;
    function getBufferOffset() : PtrUInt;
    procedure ClearType();
  public
    constructor Create(
      const AName  : string;
      const AType  : ISDOType;
      const AFlags  : TPropertyFlags;
      const AContainingType : ISDOType
    );
    destructor Destroy();override;
  end;

  TSDOProperty = class(TSDOPropertyAsbtract,IInterface,ISDOProperty)
  end;

  TSDOPropertyList = class(
    TInterfacedObject,
    IInterface,
    ISDOPropertyList,
    ISDOPropertyListEx
  )
  private
    FList : IInterfaceList;
    FBaseList : ISDOPropertyList;
  protected
    function getCount() : Integer;
    function getItem(const AIndex : Integer) : ISDOProperty;
    procedure add(const AProperty : ISDOProperty);
    function find(const AName : string) : ISDOProperty;
    function getIndex(const APropertyName : string) : Integer;
    function SetBase(const ABaseList : ISDOPropertyList) : ISDOPropertyList;
    procedure drop(const AProperty : ISDOProperty);
  public
    constructor Create(const ABaseList : ISDOPropertyList);
    destructor Destroy();override;
  end;

implementation
uses
  sdo_imp_utils, sdo_date_utils;


{ TSDOTypeAsbtract }

procedure TSDOTypeAsbtract.CheckNotUsed(const AOperationName : string);
begin
  if FUsed then
    raise ESDOInvalidStateOperationException.Create(AOperationName);
end;

constructor TSDOTypeAsbtract.Create(const AOwner : ISDODataFactory; const AName, AURI: string);
begin
  FOwner := Pointer(AOwner);
  FName := AName;
  FURI := AURI;
  FALiasList := TStringList.Create();
  FALiasList.Duplicates := dupIgnore;
  //FALiasList.Sorted := True;
  FALiasList.CaseSensitive := False;
end;

destructor TSDOTypeAsbtract.Destroy;
begin
  FreeAndNil(FALiasList);
  inherited;
end;

function TSDOTypeAsbtract.equals(const AOther: ISDOType): Boolean;
var
  i, c : PtrInt;
begin
  Result := False;
  if AnsiSameText(Self.getURI(),AOther.getURI()) then begin
    if AnsiSameText(Self.getName(),AOther.getName()) or
       ( Self.FALiasList.IndexOf(AOther.getName()) > -1 )
    then begin
      Result := True;
    end else begin
      c := AOther.getAliasCount();
      if ( c > 0 ) then begin
        for i := 0 to Pred(c) do begin
          if AnsiSameText(FName,AOther.getAlias(i)) then begin
            Result := True;
            Break;
          end;
        end;
      end;
    end;
  end;
end;

function TSDOTypeAsbtract.getAlias(const AIndex: PtrInt): string;
begin
  if ( AIndex >= 0 ) and ( AIndex < FALiasList.Count ) then
    Result := FALiasList[AIndex]
  else
    raise ESDOIndexOutOfRangeException.Create(AIndex);
end;

function TSDOTypeAsbtract.getAliasCount() : PtrInt;
begin
  Result := FALiasList.Count;
end;

function TSDOTypeAsbtract.getBaseType() : ISDOType;
begin
  Result := nil;
end;

function TSDOTypeAsbtract.getFlags() : TTypeFlags;
begin
  Result := FFlags;
end;

function TSDOTypeAsbtract.getName() : string;
begin
  Result := FName;
end;

function TSDOTypeAsbtract.getOwner() : ISDODataFactory;
begin
  Result := ISDODataFactory(FOwner);
end;

function TSDOTypeAsbtract.getURI() : string;
begin
  Result := FURI;
end;

function TSDOTypeAsbtract.inherits(const AType: ISDOType): Boolean;
begin
  Result := False;
end;

function TSDOTypeAsbtract.isAbstractType() : Boolean;
begin
  Result := ( tfIsAbstract in FFlags );
end;

function TSDOTypeAsbtract.isDataType() : Boolean;
begin
  Result := ( tfIsDataType in FFlags );
end;

function TSDOTypeAsbtract.isOpenType() : Boolean;
begin
  Result := ( tfIsOpen in FFlags );
end;

function TSDOTypeAsbtract.isSequencedType() : Boolean;
begin
  Result := ( tfIsSequenced in FFlags );
end;

function TSDOTypeAsbtract.isUsed() : Boolean;
begin
  Result := FUsed;
end;

procedure TSDOTypeAsbtract.setAlias(const AAlias: string);
var
  i : Integer;
begin
  CheckNotUsed('AAlias');
  for i := 0 to FALiasList.Count - 1 do begin
    if (FALiasList[i] = AAlias) then
      Exit;
  end;
  FALiasList.Add(AAlias);
end;

procedure TSDOTypeAsbtract.setBaseType(const ABaseType: ISDOType);
begin
  CheckNotUsed('setBaseType');
end;

procedure TSDOTypeAsbtract.setUsedFlag(const AUsed: Boolean);
begin
  FUsed := AUsed;
end;

{ TSDOSimpleType }

constructor TSDOSimpleType.Create(const AOwner: ISDODataFactory; const AName, AURI: string);
begin
  inherited Create(AOwner, AName, AURI);
  Include(FFlags, tfIsDataType);
end;

{$WARNINGS OFF}
function TSDOSimpleType.getProperties() : ISDOPropertyList;
begin
  raise ESDOUnsupportedOperationException.Create('getProperties');
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function TSDOSimpleType.getProperty(const APropertyName: string): ISDOProperty;
begin
  raise ESDOUnsupportedOperationException.Create('getProperty');
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function TSDOSimpleType.getProperty(const APropertyIndex: Integer): ISDOProperty;
begin
  raise ESDOUnsupportedOperationException.Create('getProperty');
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function TSDOSimpleType.getPropertyIndex(const APropertyName: string): Integer;
begin
  raise ESDOUnsupportedOperationException.Create('getPropertyIndex');
end;
{$WARNINGS ON}

function TSDOSimpleType.isChangeSummaryType() : Boolean;
begin
  Result := False;
end;

function TSDOSimpleType.isDataObjectType() : Boolean;
begin
  Result := False;
end;

{ TSDOBooleanType }

constructor TSDOBooleanType.Create(const AOwner : ISDODataFactory);
begin
  inherited Create(AOwner, 'Boolean', sdo_namespace);
end;

function TSDOBooleanType.getFieldSize() : PtrUInt;
begin
  Result := VALUE_STATUS_LENGTH + SizeOf(Byte);
end;

function TSDOBooleanType.getTypeEnum() : TSDOTypeKind;
begin
  Result := BooleanType;
end;

{ TSDOByteType }

constructor TSDOByteType.Create(const AOwner : ISDODataFactory);
begin
  inherited Create(AOwner, 'Byte',sdo_namespace);
end;

function TSDOByteType.getFieldSize() : PtrUInt;
begin
  Result := VALUE_STATUS_LENGTH + SizeOf(TSDOByte);
end;

function TSDOByteType.getTypeEnum() : TSDOTypeKind;
begin
  Result := ByteType;
end;

{$IFDEF HAS_SDO_BYTES}
{ TSDOBytesType }

constructor TSDOBytesType.Create(const AOwner : ISDODataFactory);
begin
  inherited Create(AOwner, 'Bytes',sdo_namespace);
  setAlias('hexBinary');
end;

function TSDOBytesType.getFieldSize() : PtrUInt;
begin
  Result := VALUE_STATUS_LENGTH + SizeOf(PPSDOBytes);
end;

function TSDOBytesType.getTypeEnum() : TSDOTypeKind;
begin
  Result := BytesType;
end;
{$ENDIF HAS_SDO_BYTES}

{$IFDEF HAS_SDO_CHAR}
{ TSDOCharacterType }

constructor TSDOCharacterType.Create(const AOwner : ISDODataFactory);
begin
  inherited Create(AOwner, 'Character',sdo_namespace);
  setAlias('Char');
  setAlias('WideChar');
  setAlias('AnsiChar');
end;

function TSDOCharacterType.getFieldSize() : PtrUInt;
begin
  Result := VALUE_STATUS_LENGTH + SizeOf(TSDOChar);
end;

function TSDOCharacterType.getTypeEnum: TSDOTypeKind;
begin
  Result := CharacterType;
end;
{$ENDIF HAS_SDO_CHAR}

{$IFDEF HAS_SDO_CURRENCY}
{ TSDOCurrencyType }

constructor TSDOCurrencyType.Create(const AOwner : ISDODataFactory);
begin
  inherited Create(AOwner, 'Currency',sdo_namespace);
end;

function TSDOCurrencyType.getFieldSize() : PtrUInt;
begin
  Result := VALUE_STATUS_LENGTH + SizeOf(TSDOCurrency);
end;

function TSDOCurrencyType.getTypeEnum() : TSDOTypeKind;
begin
  Result := CurrencyType;
end;
{$ENDIF HAS_SDO_CURRENCY}

{ TSDODateTimeType }

constructor TSDODateTimeType.Create(const AOwner : ISDODataFactory);
begin
  inherited Create(AOwner, 'DateTime',sdo_namespace);
  setAlias('Date');
end;

function TSDODateTimeType.getFieldSize() : PtrUInt;
begin
  Result := VALUE_STATUS_LENGTH + SizeOf(TSDODateTime);
end;

function TSDODateTimeType.getTypeEnum() : TSDOTypeKind;
begin
  Result := DateTimeType;
end;

{$IFDEF HAS_SDO_DOUBLE}
{ TSDODoubleType }

constructor TSDODoubleType.Create(const AOwner : ISDODataFactory);
begin
  inherited Create(AOwner, 'Double',sdo_namespace);
end;

function TSDODoubleType.getFieldSize() : PtrUInt;
begin
  Result := VALUE_STATUS_LENGTH + SizeOf(TSDODouble);
end;

function TSDODoubleType.getTypeEnum() : TSDOTypeKind;
begin
  Result := DoubleType;
end;
{$ENDIF HAS_SDO_DOUBLE}

{$IFDEF HAS_SDO_FLOAT}
{ TSDOFloatType }

constructor TSDOFloatType.Create(const AOwner : ISDODataFactory);
begin
  inherited Create(AOwner, 'Float',sdo_namespace);
end;

function TSDOFloatType.getFieldSize() : PtrUInt;
begin
  Result := VALUE_STATUS_LENGTH + SizeOf(TSDOFloat);
end;

function TSDOFloatType.getTypeEnum() : TSDOTypeKind;
begin
  Result := FloatType;
end;
{$ENDIF HAS_SDO_FLOAT}

{ TSDOIntegerType }

constructor TSDOIntegerType.Create(const AOwner : ISDODataFactory);
begin
  inherited Create(AOwner, 'Integer',sdo_namespace);
  setAlias('Int');
end;

function TSDOIntegerType.getFieldSize() : PtrUInt;
begin
  Result := VALUE_STATUS_LENGTH + SizeOf(TSDOInteger);
end;

function TSDOIntegerType.getTypeEnum: TSDOTypeKind;
begin
  Result := IntegerType;
end;

{ TSDOStringType }

constructor TSDOStringType.Create(const AOwner : ISDODataFactory);
begin
  inherited Create(AOwner, 'String',sdo_namespace);
  setAlias('Strings');
end;

function TSDOStringType.getFieldSize() : PtrUInt;
begin
  Result := VALUE_STATUS_LENGTH + SizeOf(PPSDOString);
end;

function TSDOStringType.getTypeEnum() : TSDOTypeKind;
begin
  Result := StringType;
end;

{$IFDEF HAS_SDO_LONG}
{ TSDOLongType }

constructor TSDOLongType.Create(const AOwner : ISDODataFactory);
begin
  inherited Create(AOwner, 'Long',sdo_namespace);
end;

function TSDOLongType.getFieldSize() : PtrUInt;
begin
  Result := VALUE_STATUS_LENGTH + SizeOf(TSDOLong);
end;

function TSDOLongType.getTypeEnum() : TSDOTypeKind;
begin
  Result := LongType;
end;
{$ENDIF HAS_SDO_LONG}

{$IFDEF HAS_SDO_SHORT}
{ TSDOShortType }

constructor TSDOShortType.Create(const AOwner : ISDODataFactory);
begin
  inherited Create(AOwner, 'Short',sdo_namespace);
end;

function TSDOShortType.getFieldSize() : PtrUInt;
begin
  Result := VALUE_STATUS_LENGTH + SizeOf(TSDOShort);
end;

function TSDOShortType.getTypeEnum() : TSDOTypeKind;
begin
  Result := ShortType;
end;
{$ENDIF HAS_SDO_SHORT}

{ TSDOBaseObjectType }

procedure TSDOBaseObjectType.AddProperty(
  const AName: string;
  const AType: ISDOType;
  const AFlags  : TPropertyFlags
);
var
  prpList : ISDOPropertyListEx;
begin
  CheckNotUsed('AddProperty');
  if ( AType = nil ) then
    raise ESDOIllegalArgumentException.Create('AType');
  if ( pfIsContainment in AFlags ) and ( not AType.isDataObjectType() ) then
    raise ESDOIllegalArgumentException.Create('AIsContainment');
  prpList := getProperties() as ISDOPropertyListEx;
  if ( prpList.getIndex(AName) > -1 ) then
    raise ESDOIllegalArgumentException.Create('AName');
  if ( pfIsMany in AFlags ) and AType.isChangeSummaryType() then
    raise ESDOIllegalArgumentException.Create('AIsMany');
  prpList.add(
    TSDOProperty.Create(AName,AType,AFlags,Self as ISDOType) as ISDOProperty
  );
end;

procedure TSDOBaseObjectType.Clear();

  procedure NilPropsBase();
  var
    exProps : ISDOPropertyListEx;
  begin
    exProps := FProperties as ISDOPropertyListEx;
    exProps.SetBase(nil)
  end;

var
  i, c, j : PtrInt;
begin
  setUsedFlag(False);
  if ( FProperties <> nil ) then begin
    c := FProperties.getCount();
    if ( FBaseType <> nil ) then
      j := FBaseType.getProperties().getCount()
    else
      j := 0;
    if ( c > 0 ) then begin
      for i := Pred(c) downto j do begin
        DropProperty(FProperties.getItem(i));
      end;
    end;
    if ( FBaseType <> nil ) then
      NilPropsBase();
  end;
  FBaseType := nil;
end;

constructor TSDOBaseObjectType.Create(
  const AOwner : ISDODataFactory;
  const AName, AURI : string;
  const ASequenced, AOpened, AAbstract: Boolean
);
begin
  inherited Create(AOwner, AName,AURI);
  if ASequenced then
    Include(FFlags, tfIsSequenced);
  if AOpened then
    Include(FFlags, tfIsOpen);
  if AAbstract then
    Include(FFlags, tfIsAbstract);
  FProperties := TSDOPropertyList.Create(nil) as ISDOPropertyList;
end;

destructor TSDOBaseObjectType.Destroy();
begin
  Clear();
  FBaseType := nil;
  FProperties := nil;
  inherited;
end;

procedure TSDOBaseObjectType.DropProperty(const AProperty: ISDOProperty);
var
  lsX : ISDOPropertyListEx;
begin
  CheckNotUsed('DropProperty');
  lsX := FProperties as ISDOPropertyListEx;
  lsX.drop(AProperty);
end;

function TSDOBaseObjectType.getBaseType() : ISDOType;
begin
  Result := FBaseType;
end;

function TSDOBaseObjectType.getFieldSize() : PtrUInt;
begin
  Result := VALUE_STATUS_LENGTH + SizeOf(PPSDODataObject);
end;

function TSDOBaseObjectType.getProperties() : ISDOPropertyList;
begin
  Result := FProperties;
end;

function TSDOBaseObjectType.getProperty(const APropertyName: string): ISDOProperty;
begin
  Result := getProperties().find(APropertyName);
  if ( Result = nil ) then
    raise ESDOPropertyNotFoundException.Create(APropertyName);
end;

function TSDOBaseObjectType.getProperty(const APropertyIndex: Integer): ISDOProperty;
begin
  Result := getProperties().getItem(APropertyIndex);
end;

function TSDOBaseObjectType.getPropertyIndex(const APropertyName: string): Integer;
begin
  Result := getProperties().getIndex(APropertyName);
  if ( Result = -1 ) then
    raise ESDOPropertyNotFoundException.Create(APropertyName);
end;

function TSDOBaseObjectType.getTypeEnum() : TSDOTypeKind;
begin
  Result := ObjectType;
end;

function TSDOBaseObjectType.inherits(const AType: ISDOType): Boolean;
var
  locType : ISDOType;
begin
  Result := False;
  if ( AType <> nil ) and AType.isDataObjectType() then begin
    if AnsiSameText(sdo_namespace,AType.getURI()) and
       AnsiSameText(SDOTypeDefaultTypeNames[ObjectType],AType.getName())
    then begin
      Result := True;
    end else begin
      locType := Self as ISDOType;
      while ( locType <> nil ) do begin
        if locType.equals(AType) then begin
          Result := True;
          break;
        end;
        locType := locType.getBaseType();
      end;
    end;
  end;
end;

function TSDOBaseObjectType.isChangeSummaryType() : Boolean;
begin
  Result := False;
end;

function TSDOBaseObjectType.isDataObjectType() : Boolean;
begin
  Result := True;
end;

function TSDOBaseObjectType.IsOwnerOf(const AProp: ISDOProperty): Boolean;
begin
  Result := (AProp <> nil) and
            ( ( ( AProp.getContainingType() as ISDOType ) = ( Self as ISDOType ) ) or
              ( (FBaseType <> nil) and FBaseType.IsOwnerOf(AProp) )
            );

end;

procedure TSDOBaseObjectType.setBaseType(const ABaseType: ISDOType);
var
  exProps : ISDOPropertyListEx;
begin
  inherited setBaseType(ABaseType);
  FBaseType := ABaseType as ISDOObjectType;
  exProps := FProperties as ISDOPropertyListEx;
  if ( ABaseType = nil ) then
    exProps.SetBase(nil)
  else
    exProps.SetBase(ABaseType.getProperties());
end;

procedure TSDOBaseObjectType.setUsedFlag(const AUsed: Boolean);
var
  i : PtrInt;
  typ : ISDOTypeEx;
begin
  inherited setUsedFlag(AUsed);
  if AUsed then begin
    if Assigned(FBaseType) then begin
      typ := FBaseType as ISDOTypeEx;
      if not typ.isUsed() then
        typ.setUsedFlag(True);
    end;

    for i := 0 to Pred(FProperties.getCount()) do begin
      typ := FProperties.getItem(i).getType() as ISDOTypeEx;
      if not typ.isUsed() then
        typ.setUsedFlag(True);
    end;
  end;
end;

{ TSDOTypeList }

constructor TSDOTypeList.Create();
begin
  inherited;
  FList := TInterfaceList.Create();
end;

destructor TSDOTypeList.Destroy();
begin
  FList := nil;
  inherited;
end;

function TSDOTypeList.find(const AUri, AName: string): ISDOType;
var
  i : Integer;
begin
  i := getIndex(AUri,AName);
  if ( i > -1  ) then
    Result := getItem(i)
  else
    Result := nil;
end;

function TSDOTypeList.getCount() : Integer;
begin
  Result := FList.Count;
end;

function TSDOTypeList.getIndex(const AUri, AName: string): Integer;
var
  i, j, c : PtrInt;
  typ : ISDOType;
begin
  Result := -1;
  for i := 0 to Pred(getCount()) do begin
    typ := getItem(i);
    if AnsiSameText(AUri,typ.getURI()) and
       AnsiSameText(AName,typ.getName())
    then begin
      Result := i;
      Break;
    end;
    c := typ.getAliasCount();
    if ( c > 0 ) then begin
      for j := 0 to Pred(c) do begin
        if AnsiSameText(AUri,typ.getURI()) and
           AnsiSameText(AName,typ.getAlias(j))
        then begin
          Result := i;
          Break;
        end;
      end;
    end;
  end;
end;

function TSDOTypeList.getItem(const AIndex: Integer) : ISDOType;
begin
  if ( AIndex >= 0 ) and ( AIndex < FList.Count ) then begin
    Result := FList[AIndex] as ISDOType;
  end else begin
    raise ESDOIndexOutOfRangeException.Create(AIndex);
  end;
end;

procedure TSDOTypeList.insert(const AType : ISDOType);
begin
  if ( AType = nil ) then
    raise ESDOIllegalArgumentException.Create('AType');
  if ( find(AType.getURI(),AType.getName()) = nil ) then begin
    FList.Add(AType);
  end;
end;



{ TSDONativeSimpleType }

procedure TSDONativeSimpleType.setBaseType(const ABaseType: ISDOType);
begin
  raise ESDOUnsupportedOperationException.Create('SetBaseType');
end;

{ TSDOUserDefinedSimpleType }

constructor TSDOUserDefinedSimpleType.Create(
  const AOwner : ISDODataFactory;
  const AName, AURI: string;
  const AIsAbstract: Boolean
);
begin
  inherited Create(AOwner, AName, AURI);
  if AIsAbstract then
    Include(FFlags, tfIsAbstract);
end;

function TSDOUserDefinedSimpleType.getBaseType() : ISDOType;
begin
  Result := FBaseType;
end;

function TSDOUserDefinedSimpleType.getFieldSize() : PtrUInt;
var
  fb : ISDOType;
  fbX : ISDOTypeEx;
begin
  fb := getBaseType();
  if ( fb = nil ) then
    raise ESDOIncompleteTypeException.Create(Self.ClassName());
  fbX := fb as ISDOTypeEx;
  Result := fbX.getFieldSize();
end;

function TSDOUserDefinedSimpleType.getTypeEnum() : TSDOTypeKind;
var
  fb : ISDOType;
begin
  fb := getBaseType();
  if ( fb = nil ) then
    raise ESDOIncompleteTypeException.Create(Self.ClassName());
  Result := fb.getTypeEnum();
end;

procedure TSDOUserDefinedSimpleType.setBaseType(const ABaseType: ISDOType);
begin
  inherited setBaseType(ABaseType);
  if ( ABaseType <> nil ) then begin
    if not (
         ABaseType.isDataType() or
         ( ABaseType = ( Self as ISDOType ) )
       )
    then begin
      raise ESDOIllegalArgumentException.Create(ABaseType.getName());
    end;
  end;
  FBaseType := ABaseType;
end;

{ TSDOChangeSummaryType }

constructor TSDOChangeSummaryType.Create(const AOwner : ISDODataFactory);
begin
  inherited Create(AOwner, SDOTypeDefaultTypeNames[ChangeSummaryType],sdo_namespace);
  Include(FFlags, tfIsDataType);
end;

function TSDOChangeSummaryType.getFieldSize() : PtrUInt;
begin
  Result := VALUE_STATUS_LENGTH + SizeOf(PPSDOChangeSummary);
end;

{$WARNINGS OFF}
function TSDOChangeSummaryType.getProperties() : ISDOPropertyList;
begin
  raise ESDOUnsupportedOperationException.Create('getProperties');
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function TSDOChangeSummaryType.getProperty(const APropertyName: string): ISDOProperty;
begin
  raise ESDOUnsupportedOperationException.Create('getProperty');
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function TSDOChangeSummaryType.getProperty(const APropertyIndex: Integer): ISDOProperty;
begin
  raise ESDOUnsupportedOperationException.Create('getProperty');
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function TSDOChangeSummaryType.getPropertyIndex(const APropertyName: string): Integer;
begin
  raise ESDOUnsupportedOperationException.Create('getPropertyIndex');
end;
{$WARNINGS ON}

function TSDOChangeSummaryType.getTypeEnum() : TSDOTypeKind;
begin
  Result := ChangeSummaryType;
end;

function TSDOChangeSummaryType.isChangeSummaryType() : Boolean;
begin
  Result := True;
end;

function TSDOChangeSummaryType.isDataObjectType() : Boolean;
begin
  Result := False;
end;

{$WARNINGS OFF}
procedure TSDOChangeSummaryType.setBaseType(const ABaseType: ISDOType);
begin
  raise ESDOUnsupportedOperationException.Create('setBaseType');
end;
{$WARNINGS ON}

{ TSDOPropertyAsbtract }

procedure TSDOPropertyAsbtract.CheckIfContainingTypeIsUsed(const AOperation : string);
var
  tX : ISDOTypeEx;
begin
  if ( FContainingType <> nil ) then begin
    tX := ISDOType(FContainingType) as ISDOTypeEx;
    if tX.isUsed() then
      raise ESDOUnsupportedOperationException.Create(AOperation);
  end;
end;

procedure TSDOPropertyAsbtract.ClearType();
begin
  FreeDefaultBuffer();
  if FWeakReferenceToType then
    FType := nil
  else
    ISDOType(FType) := nil;
end;

constructor TSDOPropertyAsbtract.Create(
  const AName  : string;
  const AType  : ISDOType;
  const AFlags  : TPropertyFlags;
  const AContainingType : ISDOType
);
var
  locName : string;
begin
  locName := Trim(AName);
  if not IsValidName(locName) then
    raise ESDOIllegalArgumentException.Create('AName');
  if ( AContainingType = nil ) then
    raise ESDOIllegalArgumentException.Create('AContainingType');
  if ( AType = nil ) then
    raise ESDOIllegalArgumentException.Create('AType');
  if ( pfIsContainment in AFlags ) and ( not AType.isDataObjectType() ) then
    raise ESDOIllegalArgumentException.Create('AContainingType');
  FName := AName;
  FALiasList := TStringList.Create();
  FWeakReferenceToType := AType.equals(AContainingType);
  if FWeakReferenceToType then
    FType := Pointer(AType)
  else
    ISDOType(FType) := AType;
  FFlags := AFlags;
  FContainingType := Pointer(AContainingType);
  FTypeKind := AType.getTypeEnum();
end;

destructor TSDOPropertyAsbtract.Destroy;
begin
  FreeDefaultBuffer();
  FContainingType := nil;
  if FWeakReferenceToType then
    FType := nil
  else
    ISDOType(FType) := nil;
  FreeAndNil(FALiasList);
  inherited;
end;

procedure TSDOPropertyAsbtract.FreeDefaultBuffer();
begin
  if ( FType <> nil ) then begin
    case FTypeKind of
{$IFDEF HAS_SDO_BYTES}
      BytesType :
        begin
          if ( FDefault.BytesValue <> nil ) then begin
            Dispose(FDefault.BytesValue);
          end;
        end;
{$ENDIF HAS_SDO_BYTES}
      ObjectType :
        begin
          if ( FDefault.ObjectValue <> nil ) then begin
            Dispose(FDefault.ObjectValue);
          end;
        end;
      StringType :
        begin
          if ( FDefault.StringValue <> nil ) then begin
            Dispose(FDefault.StringValue);
          end;
        end;
    end;
    FillChar(FDefault,SizeOf(FDefault),#0);
    FDefaultSet := False;
  end;
end;

function TSDOPropertyAsbtract.getAlias(const AIndex: Integer): string;
begin
  if ( AIndex >= 0 ) and ( AIndex < FALiasList.Count ) then
    Result := FALiasList[AIndex]
  else
    raise ESDOException.CreateFmt('Invalid alias index, Type = %s, Index = %d.',[ClassName,AIndex]);
end;

function TSDOPropertyAsbtract.getAliasCount() : Integer;
begin
  Result := FALiasList.Count;
end;

function TSDOPropertyAsbtract.getBooleanDefault(): TSDOBoolean;
begin
  if FDefaultSet then begin
    case getTypeEnum() of
      BooleanType   : Result := FDefault.BooleanValue;
      ByteType      : Result := TSDOConvertHelper.ByteToBool(FDefault.ByteValue);
{$IFDEF HAS_SDO_CHAR}
      CharacterType : Result := TSDOConvertHelper.CharToBool(FDefault.CharValue);
{$ENDIF HAS_SDO_CHAR}
      IntegerType   : Result := TSDOConvertHelper.IntegerToBool(FDefault.IntegerValue);
{$IFDEF HAS_SDO_LONG}
      LongType      : Result := TSDOConvertHelper.LongToBool(FDefault.LongValue);
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
      ShortType     : Result := TSDOConvertHelper.ShortToBool(FDefault.ShortValue);
{$ENDIF HAS_SDO_SHORT}
      StringType    : Result := TSDOConvertHelper.StringToBool(FDefault.StringValue^);
      else
        Result := False;
    end;
  end else begin
    Result := False;
  end;
end;

function TSDOPropertyAsbtract.getBufferOffset() : PtrUInt;
begin
  Result := FBufferOffset;
end;

function TSDOPropertyAsbtract.getByteDefault(): TSDOByte;
begin
  if FDefaultSet then begin
    case getTypeEnum() of
      BooleanType   : Result := TSDOConvertHelper.BoolToByte(FDefault.BooleanValue);
      ByteType      : Result := FDefault.ByteValue;
{$IFDEF HAS_SDO_CHAR}
      CharacterType : Result := TSDOConvertHelper.CharToByte(FDefault.CharValue);
{$ENDIF HAS_SDO_CHAR}
      IntegerType   : Result := FDefault.IntegerValue;
{$IFDEF HAS_SDO_LONG}
      LongType      : Result := FDefault.LongValue;
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
      ShortType     : Result := FDefault.ShortValue;
{$ENDIF HAS_SDO_SHORT}
      StringType    : Result := TSDOConvertHelper.StringToByte(FDefault.StringValue^);
      else
        Result := 0;
    end;
  end else begin
    Result := 0;
  end;
end;

{$IFDEF HAS_SDO_BYTES}
function TSDOPropertyAsbtract.getBytesDefault() : TSDOBytes;
begin
  if FDefaultSet then begin
    case getTypeEnum() of
      BytesType       : Result := FDefault.BytesValue^;
      StringType      : Result := TSDOConvertHelper.StringToBytes(FDefault.StringValue^);
    end;
  end;
end;
{$ENDIF HAS_SDO_BYTES}

{$IFDEF HAS_SDO_CHAR}
function TSDOPropertyAsbtract.getCharacterDefault() : TSDOChar;
begin
  if FDefaultSet then begin
    case getTypeEnum() of
      BooleanType   : Result := TSDOConvertHelper.BoolToChar(FDefault.BooleanValue);
      ByteType      : Result := TSDOConvertHelper.ByteToChar(FDefault.ByteValue);
      CharacterType : Result := FDefault.CharValue;
      IntegerType   : Result := TSDOConvertHelper.IntegerToChar(FDefault.IntegerValue);
{$IFDEF HAS_SDO_LONG}
      LongType      : Result := TSDOConvertHelper.LongToChar(FDefault.LongValue);
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
      ShortType     : Result := TSDOConvertHelper.ShortToChar(FDefault.ShortValue);
{$ENDIF HAS_SDO_SHORT}
      StringType    : Result := TSDOConvertHelper.StringToChar(FDefault.StringValue^);
      else
        Result := #0;
    end;
  end else begin
    Result := #0;
  end;
end;
{$ENDIF HAS_SDO_CHAR}

{$IFDEF HAS_SDO_CURRENCY }
function TSDOPropertyAsbtract.getCurrencyDefault() : TSDOCurrency;
begin
  if FDefaultSet then begin
    case getTypeEnum() of
      ByteType      : Result := FDefault.ByteValue;
      CurrencyType  : Result := FDefault.CurrencyValue;
{$IFDEF HAS_SDO_DOUBLE}
      DoubleType    : Result := FDefault.DoubleValue;
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT }
      FloatType     : Result := FDefault.FloatValue;
{$ENDIF HAS_SDO_FLOAT }
      IntegerType   : Result := FDefault.IntegerValue;
{$IFDEF HAS_SDO_LONG}
      LongType      : Result := FDefault.LongValue;
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
      ShortType     : Result := FDefault.ShortValue;
{$ENDIF HAS_SDO_SHORT}
      StringType    : Result := TSDOConvertHelper.StringToFloat(FDefault.StringValue^);
      else
        Result := 0;
    end;
  end else begin
    Result := 0;
  end;
end;
{$ENDIF HAS_SDO_CURRENCY }

function TSDOPropertyAsbtract.getContainingType() : ISDOType;
begin
  Result := ISDOType(FContainingType);
end;

function TSDOPropertyAsbtract.getDateDefault() : TSDODate;
begin
  if FDefaultSet and ( getTypeEnum() = DateTimeType ) then
    Result := FDefault.DateValue
  else
    Result := ZERO_DATE;
end;

{$IFDEF HAS_SDO_DOUBLE}
function TSDOPropertyAsbtract.getDoubleDefault() : TSDODouble;
begin
  if FDefaultSet then begin
    case getTypeEnum() of
      ByteType      : Result := FDefault.ByteValue;
{$IFDEF HAS_SDO_CURRENCY }
      CurrencyType  : Result := FDefault.CurrencyValue;
{$ENDIF HAS_SDO_CURRENCY }
      DoubleType    : Result := FDefault.DoubleValue;
{$IFDEF HAS_SDO_FLOAT}
      FloatType     : Result := FDefault.FloatValue;
{$ENDIF HAS_SDO_FLOAT}
      IntegerType   : Result := FDefault.IntegerValue;
{$IFDEF HAS_SDO_LONG}
      LongType      : Result := FDefault.LongValue;
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
      ShortType     : Result := FDefault.ShortValue;
{$ENDIF HAS_SDO_SHORT}
      StringType    : Result := TSDOConvertHelper.StringToFloat(FDefault.StringValue^);
      else
        Result := 0;
    end;
  end else begin
    Result := 0;
  end;
end;
{$ENDIF HAS_SDO_DOUBLE}

{$IFDEF HAS_SDO_FLOAT}
function TSDOPropertyAsbtract.getFloatDefault() : TSDOFloat;
begin
  if FDefaultSet then begin
    case getTypeEnum() of
      ByteType      : Result := FDefault.ByteValue;
{$IFDEF HAS_SDO_CURRENCY }
      CurrencyType  : Result := FDefault.CurrencyValue;
{$ENDIF HAS_SDO_CURRENCY }
{$IFDEF HAS_SDO_DOUBLE}
      DoubleType    : Result := FDefault.DoubleValue;
{$ENDIF HAS_SDO_DOUBLE}
      FloatType     : Result := FDefault.FloatValue;
      IntegerType   : Result := FDefault.IntegerValue;
{$IFDEF HAS_SDO_LONG}
      LongType      : Result := FDefault.LongValue;
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
      ShortType     : Result := FDefault.ShortValue;
{$ENDIF HAS_SDO_SHORT}
      StringType    : Result := TSDOConvertHelper.StringToFloat(FDefault.StringValue^);
      else
        Result := 0;
    end;
  end else begin
    Result := 0;
  end;
end;
{$ENDIF HAS_SDO_FLOAT}

function TSDOPropertyAsbtract.getIntegerDefault() : TSDOInteger;
begin
  if FDefaultSet then begin
    case getTypeEnum() of
      BooleanType   : Result := TSDOConvertHelper.BoolToInteger(FDefault.BooleanValue);
      ByteType      : Result := FDefault.ByteValue;
{$IFDEF HAS_SDO_CHAR}
      CharacterType : Result := TSDOConvertHelper.CharToInteger(FDefault.CharValue);
{$ENDIF HAS_SDO_CHAR}
      IntegerType   : Result := FDefault.IntegerValue;
{$IFDEF HAS_SDO_LONG}
      LongType      : Result := FDefault.LongValue;
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
      ShortType     : Result := FDefault.ShortValue;
{$ENDIF HAS_SDO_SHORT}
      StringType    : Result := TSDOConvertHelper.StringToInteger(FDefault.StringValue^);
      else
        Result := 0;
    end;
  end else begin
    Result := 0;
  end;
end;

function TSDOPropertyAsbtract.getLongDefault() : TSDOLong;
begin
  if FDefaultSet then begin
    case getTypeEnum() of
      BooleanType   : Result := TSDOConvertHelper.BoolToLong(FDefault.BooleanValue);
      ByteType      : Result := FDefault.ByteValue;
{$IFDEF HAS_SDO_CHAR}
      CharacterType : Result := TSDOConvertHelper.CharToLong(FDefault.CharValue);
{$ENDIF HAS_SDO_CHAR}
      IntegerType   : Result := FDefault.IntegerValue;
{$IFDEF HAS_SDO_LONG}
      LongType      : Result := FDefault.LongValue;
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
      ShortType     : Result := FDefault.ShortValue;
{$ENDIF HAS_SDO_SHORT}
      StringType    : Result := TSDOConvertHelper.StringToLong(FDefault.StringValue^);
      else
        Result := 0;
    end;
  end else begin
    Result := 0;
  end;
end;

function TSDOPropertyAsbtract.getName() : string;
begin
  Result := FName;
end;

function TSDOPropertyAsbtract.getOpposite() : ISDOProperty;
begin
  Result := nil;
end;

function TSDOPropertyAsbtract.getShortDefault() : TSDOShort;
begin
  if FDefaultSet then begin
    case getTypeEnum() of
      BooleanType   : Result := TSDOConvertHelper.BoolToShort(FDefault.BooleanValue);
      ByteType      : Result := FDefault.ByteValue;
{$IFDEF HAS_SDO_CHAR}
      CharacterType : Result := TSDOConvertHelper.CharToShort(FDefault.CharValue);
{$ENDIF HAS_SDO_CHAR}
      IntegerType   : Result := FDefault.IntegerValue;
{$IFDEF HAS_SDO_LONG}
      LongType      : Result := FDefault.LongValue;
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
      ShortType     : Result := FDefault.ShortValue;
{$ENDIF HAS_SDO_SHORT}
      StringType    : Result := TSDOConvertHelper.StringToShort(FDefault.StringValue^);
      else
        Result := 0;
    end;
  end else begin
    Result := 0;
  end;
end;

function TSDOPropertyAsbtract.getStringDefault() : TSDOString;
begin
  if FDefaultSet then begin
    case getTypeEnum() of
      BooleanType     : Result := TSDOConvertHelper.BoolToString(FDefault.BooleanValue);
      ByteType        : Result := TSDOConvertHelper.ByteToString(FDefault.ByteValue);
{$IFDEF HAS_SDO_BYTES}
      BytesType       : Result := TSDOConvertHelper.BytesToString(FDefault.BytesValue^);
{$ENDIF HAS_SDO_BYTES}
{$IFDEF HAS_SDO_CHAR}
      CharacterType   : Result := FDefault.CharValue;
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_DOUBLE}
      DoubleType      : Result := TSDOConvertHelper.FloatToString(FDefault.DoubleValue);
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
      FloatType       : Result := TSDOConvertHelper.FloatToString(FDefault.FloatValue);
{$ENDIF HAS_SDO_FLOAT}
      IntegerType     : Result := TSDOConvertHelper.IntegerToString(FDefault.IntegerValue);
{$IFDEF HAS_SDO_LONG}
      LongType        : Result := TSDOConvertHelper.LongToString(FDefault.LongValue);
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
      ShortType       : Result := TSDOConvertHelper.ShortToString(FDefault.ShortValue);
{$ENDIF HAS_SDO_SHORT}
      StringType      : Result := FDefault.StringValue^;
    end;
  end;
end;

function TSDOPropertyAsbtract.getType() : ISDOType;
begin
  Result := ISDOType(FType);
end;

function TSDOPropertyAsbtract.getTypeEnum() : TSDOTypeKind;
begin
  Result := getType().getTypeEnum();
end;

procedure TSDOPropertyAsbtract.InternalSetBufferOffset(const AOffset: PtrUInt);
begin
  FBufferOffset := AOffset;
end;

function TSDOPropertyAsbtract.isContainment() : Boolean;
begin
  Result := ( pfIsContainment in FFlags );
end;

function TSDOPropertyAsbtract.isDefaulted() : Boolean;
begin
  Result := FDefaultSet;
end;

function TSDOPropertyAsbtract.isMany() : Boolean;
begin
  Result := ( pfIsMany in FFlags );;
end;

function TSDOPropertyAsbtract.isNullable() : Boolean;
begin
  Result := not ( pfIsNotNullable in FFlags );
end;

function TSDOPropertyAsbtract.isReadOnly() : Boolean;
begin
  Result := ( pfIsReadOnly in FFlags );
end;

function TSDOPropertyAsbtract.isReference() : Boolean;
begin
  Result := ( not isContainment() ) and ( getType().isDataObjectType() );
end;

procedure TSDOPropertyAsbtract.setAlias(const AAlias: string);
begin
  if ( FALiasList.IndexOf(AAlias) = -1 ) then
    FALiasList.Add(AAlias);
end;

function TSDOPropertyAsbtract.setBufferOffset(const AStartingAddress: PtrUInt): PtrUInt;
var
  typ : ISDOTypeEx;
  bufferLen : PtrInt;
begin
  Result := AStartingAddress;
  InternalSetBufferOffset(Result);
  if isMany() then begin
    bufferLen := SizeOf(Pointer);
  end else begin
    typ := getType() as ISDOTypeEx;
    bufferLen := typ.getFieldSize();
  end;
  Inc(Result,bufferLen);
end;

procedure TSDOPropertyAsbtract.setDefault(const AValue: TSDOBoolean);
begin
  if getType().isDataObjectType or getType().isChangeSummaryType() then
    raise ESDOUnsupportedOperationException.Create('setDefault');
  CheckIfContainingTypeIsUsed('setDefault');
  if not ( getTypeEnum() in [
             BooleanType,ByteType
             {$IFDEF HAS_SDO_CHAR},CharacterType{$ENDIF}
             {$IFDEF HAS_SDO_LONG},LongType{$ENDIF}
             {$IFDEF HAS_SDO_SHORT},ShortType{$ENDIF}
             ,IntegerType,StringType
          ]
         )
  then begin
    raise ESDOUnsupportedOperationException.Create('setDefault');
  end;
  FreeDefaultBuffer();
  FDefaultSet := True;
  case getTypeEnum() of
    BooleanType    : FDefault.BooleanValue := AValue;
    ByteType       : FDefault.ByteValue := TSDOConvertHelper.BoolToByte(AValue);
{$IFDEF HAS_SDO_CHAR}
    CharacterType  : FDefault.CharValue := TSDOConvertHelper.BoolToChar(AValue);
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_LONG}
    LongType       : FDefault.LongValue := TSDOConvertHelper.BoolToLong(AValue);
{$ENDIF HAS_SDO_LONG}
    IntegerType    : FDefault.IntegerValue := TSDOConvertHelper.BoolToInteger(AValue);
{$IFDEF HAS_SDO_SHORT}
    ShortType      : FDefault.ShortValue := TSDOConvertHelper.BoolToShort(AValue);
{$ENDIF HAS_SDO_SHORT}
    StringType   :
      begin
        New(FDefault.StringValue);
        FDefault.StringValue^ := TSDOConvertHelper.BoolToString(AValue);
      end;
  end;
end;

{$IFDEF HAS_SDO_CURRENCY }
procedure TSDOPropertyAsbtract.setDefaultCurrency(const AValue: TSDOCurrency);
begin
  if getType().isDataObjectType or getType().isChangeSummaryType() then
    raise ESDOUnsupportedOperationException.Create('setDefault');
  CheckIfContainingTypeIsUsed('setDefault');
  if not ( getTypeEnum() in [DoubleType,FloatType,CurrencyType,StringType] ) then
    raise ESDOUnsupportedOperationException.Create('setDefault');

  FreeDefaultBuffer();
  FDefaultSet := True;
  case getTypeEnum() of
    CurrencyType  : FDefault.CurrencyValue := AValue;
{$IFDEF HAS_SDO_DOUBLE }
    DoubleType    : FDefault.DoubleValue := AValue;
{$ENDIF HAS_SDO_DOUBLE }
{$IFDEF HAS_SDO_FLOAT }
    FloatType     : FDefault.FloatValue := AValue;
{$ENDIF HAS_SDO_FLOAT }
    StringType   :
      begin
        New(FDefault.StringValue);
        FDefault.StringValue^ := TSDOConvertHelper.FloatToString(AValue);
      end;
  end;
end;
{$ENDIF HAS_SDO_CURRENCY }

{$IFDEF HAS_SDO_DOUBLE }
procedure TSDOPropertyAsbtract.setDefault(const AValue: TSDODouble);
begin
  if getType().isDataObjectType or getType().isChangeSummaryType() then
    raise ESDOUnsupportedOperationException.Create('setDefault');
  CheckIfContainingTypeIsUsed('setDefault');
  if not ( getTypeEnum() in [DoubleType,FloatType,CurrencyType,StringType] ) then
    raise ESDOUnsupportedOperationException.Create('setDefault');

  FreeDefaultBuffer();
  FDefaultSet := True;
  case getTypeEnum() of
{$IFDEF HAS_SDO_CURRENCY }
    CurrencyType  : FDefault.CurrencyValue := AValue;
{$ENDIF HAS_SDO_CURRENCY }
    DoubleType    : FDefault.DoubleValue := AValue;
{$IFDEF HAS_SDO_FLOAT }
    FloatType     : FDefault.FloatValue := AValue;
{$ENDIF HAS_SDO_FLOAT }
    StringType   :
      begin
        New(FDefault.StringValue);
        FDefault.StringValue^ := TSDOConvertHelper.FloatToString(AValue);
      end;
  end;
end;
{$ENDIF HAS_SDO_DOUBLE }

{$IFDEF HAS_SDO_FLOAT}
procedure TSDOPropertyAsbtract.setDefault(const AValue: TSDOFloat);
begin
  if getType().isDataObjectType or getType().isChangeSummaryType() then
    raise ESDOUnsupportedOperationException.Create('setDefault');
  CheckIfContainingTypeIsUsed('setDefault');
  if not ( getTypeEnum() in [DoubleType,FloatType,CurrencyType,StringType] ) then
    raise ESDOUnsupportedOperationException.Create('setDefault');

  FreeDefaultBuffer();
  FDefaultSet := True;
  case getTypeEnum() of
{$IFDEF HAS_SDO_CURRENCY }
    CurrencyType  : FDefault.CurrencyValue := AValue;
{$ENDIF HAS_SDO_CURRENCY }
{$IFDEF HAS_SDO_FLOAT}
    DoubleType    : FDefault.DoubleValue := AValue;
{$ENDIF HAS_SDO_FLOAT}
    FloatType     : FDefault.FloatValue := AValue;
    StringType   :
      begin
        New(FDefault.StringValue);
        FDefault.StringValue^ := TSDOConvertHelper.FloatToString(AValue);
      end;
  end;
end;
{$ENDIF HAS_SDO_FLOAT}

procedure TSDOPropertyAsbtract.setDefault(const AValue: TSDOInteger);
begin
  if getType().isDataObjectType or getType().isChangeSummaryType() then
    raise ESDOUnsupportedOperationException.Create('setDefault');
  CheckIfContainingTypeIsUsed('setDefault');
  if not ( getTypeEnum() in [BooleanType,ByteType,IntegerType,StringType] ) then
    raise ESDOUnsupportedOperationException.Create('setDefault');

  FreeDefaultBuffer();
  FDefaultSet := True;
  case getTypeEnum() of
    BooleanType  : FDefault.BooleanValue := ( AValue <> 0 );
    ByteType     : FDefault.ByteValue := AValue;
    IntegerType  : FDefault.IntegerValue := AValue;
    StringType   :
      begin
        New(FDefault.StringValue);
        FDefault.StringValue^ := IntToStr(AValue);
      end;
  end;
end;

procedure TSDOPropertyAsbtract.setDefault(const AValue: TSDOString);
begin
  if getType().isDataObjectType or getType().isChangeSummaryType() then
    raise ESDOUnsupportedOperationException.Create('setDefault');
  CheckIfContainingTypeIsUsed('setDefault');
  FreeDefaultBuffer();
  FDefaultSet := True;
  case getTypeEnum() of
    BooleanType    : FDefault.BooleanValue := TSDOConvertHelper.StringToBool(AValue);
    ByteType       : FDefault.ByteValue := TSDOConvertHelper.StringToByte(AValue);
{$IFDEF HAS_SDO_CHAR}
    CharacterType  : FDefault.CharValue := TSDOConvertHelper.StringToChar(AValue);
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_LONG}
    LongType       : FDefault.LongValue := TSDOConvertHelper.StringToLong(AValue);
{$ENDIF HAS_SDO_LONG}
    DateTimeType   : FDefault.DateValue := TSDOConvertHelper.StringToDate(AValue);
    IntegerType    : FDefault.IntegerValue := TSDOConvertHelper.StringToInteger(AValue);
{$IFDEF HAS_SDO_SHORT}
    ShortType      : FDefault.ShortValue := TSDOConvertHelper.StringToShort(AValue);
{$ENDIF HAS_SDO_SHORT}
    StringType     :
      begin
        New(FDefault.StringValue);
        FDefault.StringValue^ := AValue;
      end;
  end;
end;

procedure TSDOPropertyAsbtract.setDefault(const AValue: TSDOByte);
begin
  if getType().isDataObjectType or getType().isChangeSummaryType() then
    raise ESDOUnsupportedOperationException.Create('setDefault');
  CheckIfContainingTypeIsUsed('setDefault');
  if not ( getTypeEnum() in [
             BooleanType,ByteType
             {$IFDEF HAS_SDO_CHAR},CharacterType{$ENDIF}
             {$IFDEF HAS_SDO_LONG},LongType{$ENDIF}
             {$IFDEF HAS_SDO_SHORT},ShortType{$ENDIF}
             ,IntegerType,StringType
          ]
         )
  then begin
    raise ESDOUnsupportedOperationException.Create('setDefault');
  end;

  FreeDefaultBuffer();
  FDefaultSet := True;
  case getTypeEnum() of
    BooleanType    : FDefault.BooleanValue := TSDOConvertHelper.ByteToBool(AValue);
    ByteType       : FDefault.ByteValue := AValue;
{$IFDEF HAS_SDO_CHAR}
    CharacterType  : FDefault.CharValue := TSDOConvertHelper.ByteToChar(AValue);
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_LONG}
    LongType       : FDefault.LongValue := AValue;
{$ENDIF HAS_SDO_LONG}
    IntegerType    : FDefault.IntegerValue := AValue;
{$IFDEF HAS_SDO_SHORT}
    ShortType      : FDefault.ShortValue := AValue;
{$ENDIF HAS_SDO_SHORT}
    StringType     :
      begin
        New(FDefault.StringValue);
        FDefault.StringValue^ := TSDOConvertHelper.ByteToString(AValue);
      end;
  end;
end;

{$IFDEF HAS_SDO_BYTES}
procedure TSDOPropertyAsbtract.setDefault(AValue: TSDOBytes);
begin
  if getType().isDataObjectType or getType().isChangeSummaryType() then
    raise ESDOUnsupportedOperationException.Create('setDefault');
  CheckIfContainingTypeIsUsed('setDefault');
  if not ( getTypeEnum() in [BytesType,StringType] ) then begin
    raise ESDOUnsupportedOperationException.Create('setDefault');
  end;

  FreeDefaultBuffer();
  FDefaultSet := True;
  case getTypeEnum() of
    BytesType     :
      begin
        New(FDefault.BytesValue);
        FDefault.BytesValue^ := Copy(AValue);
      end;
    StringType     :
      begin
        New(FDefault.StringValue);
        FDefault.StringValue^ := TSDOConvertHelper.BytesToString(AValue);
      end;
  end;
end;
{$ENDIF HAS_SDO_BYTES}

{$IFDEF HAS_SDO_CHAR}
procedure TSDOPropertyAsbtract.setDefault(const AValue: TSDOChar);
begin
  if getType().isDataObjectType or getType().isChangeSummaryType() then
    raise ESDOUnsupportedOperationException.Create('setDefault');
  CheckIfContainingTypeIsUsed('setDefault');
  if not ( getTypeEnum() in [
             BooleanType,ByteType
             {$IFDEF HAS_SDO_CHAR},CharacterType{$ENDIF}
             {$IFDEF HAS_SDO_LONG},LongType{$ENDIF}
             {$IFDEF HAS_SDO_SHORT},ShortType{$ENDIF}
             ,IntegerType,StringType
          ]
         )
  then begin
    raise ESDOUnsupportedOperationException.Create('setDefault');
  end;

  FreeDefaultBuffer();
  FDefaultSet := True;
  case getTypeEnum() of
    BooleanType    : FDefault.BooleanValue := TSDOConvertHelper.CharToBool(AValue);
    ByteType       : FDefault.ByteValue := TSDOConvertHelper.CharToByte(AValue);
{$IFDEF HAS_SDO_CHAR}
    CharacterType  : FDefault.CharValue := AValue;
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_LONG}
    LongType       : FDefault.LongValue := TSDOConvertHelper.CharToLong(AValue);
{$ENDIF HAS_SDO_LONG}
    IntegerType    : FDefault.IntegerValue := TSDOConvertHelper.CharToInteger(AValue);
{$IFDEF HAS_SDO_SHORT}
    ShortType      : FDefault.ShortValue := TSDOConvertHelper.CharToShort(AValue);
{$ENDIF HAS_SDO_SHORT}
    StringType     :
      begin
        New(FDefault.StringValue);
        FDefault.StringValue^ := AValue;
      end;
  end;
end;
{$ENDIF HAS_SDO_CHAR}

{$IFDEF HAS_SDO_LONG}
procedure TSDOPropertyAsbtract.setDefault(const AValue: TSDOLong);
begin
  if getType().isDataObjectType or getType().isChangeSummaryType() then
    raise ESDOUnsupportedOperationException.Create('setDefault');
  CheckIfContainingTypeIsUsed('setDefault');
  if not ( getTypeEnum() in [
             BooleanType,ByteType
             {$IFDEF HAS_SDO_CHAR},CharacterType{$ENDIF}
             {$IFDEF HAS_SDO_LONG},LongType{$ENDIF}
             {$IFDEF HAS_SDO_SHORT},ShortType{$ENDIF}
             ,IntegerType,StringType
          ]
         )
  then begin
    raise ESDOUnsupportedOperationException.Create('setDefault');
  end;

  FreeDefaultBuffer();
  FDefaultSet := True;
  case getTypeEnum() of
    BooleanType    : FDefault.BooleanValue := TSDOConvertHelper.LongToBool(AValue);
    ByteType       : FDefault.ByteValue := AValue;
{$IFDEF HAS_SDO_CHAR}
    CharacterType  : FDefault.CharValue := TSDOConvertHelper.LongToChar(AValue);
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_LONG}
    LongType       : FDefault.LongValue := AValue;
{$ENDIF HAS_SDO_LONG}
    IntegerType    : FDefault.IntegerValue := AValue;
{$IFDEF HAS_SDO_SHORT}
    ShortType      : FDefault.ShortValue := AValue;
{$ENDIF HAS_SDO_SHORT}
    StringType     :
      begin
        New(FDefault.StringValue);
        FDefault.StringValue^ := TSDOConvertHelper.LongToString(AValue);
      end;
  end;
end;
{$ENDIF HAS_SDO_LONG}

{$IFDEF HAS_SDO_SHORT}
procedure TSDOPropertyAsbtract.setDefault(const AValue: TSDOShort);
begin
  if getType().isDataObjectType or getType().isChangeSummaryType() then
    raise ESDOUnsupportedOperationException.Create('setDefault');
  CheckIfContainingTypeIsUsed('setDefault');
  if not ( getTypeEnum() in [
             BooleanType,ByteType
             {$IFDEF HAS_SDO_CHAR},CharacterType{$ENDIF}
             {$IFDEF HAS_SDO_LONG},LongType{$ENDIF}
             {$IFDEF HAS_SDO_SHORT},ShortType{$ENDIF}
             ,IntegerType,StringType
          ]
         )
  then begin
    raise ESDOUnsupportedOperationException.Create('setDefault');
  end;

  FreeDefaultBuffer();
  FDefaultSet := True;
  case getTypeEnum() of
    BooleanType    : FDefault.BooleanValue := TSDOConvertHelper.ShortToBool(AValue);
    ByteType       : FDefault.ByteValue := AValue;
{$IFDEF HAS_SDO_CHAR}
    CharacterType  : FDefault.CharValue := TSDOConvertHelper.ShortToChar(AValue);
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_LONG}
    LongType       : FDefault.LongValue := AValue;
{$ENDIF HAS_SDO_LONG}
    IntegerType    : FDefault.IntegerValue := AValue;
{$IFDEF HAS_SDO_SHORT}
    ShortType      : FDefault.ShortValue := AValue;
{$ENDIF HAS_SDO_SHORT}
    StringType     :
      begin
        New(FDefault.StringValue);
        FDefault.StringValue^ := TSDOConvertHelper.ShortToString(AValue);
      end;
  end;
end;
{$ENDIF HAS_SDO_SHORT}

procedure TSDOPropertyAsbtract.setDefault(const AValue: TSDODate);
begin
  if getType().isDataObjectType or getType().isChangeSummaryType() then
    raise ESDOUnsupportedOperationException.Create('setDefault');
  CheckIfContainingTypeIsUsed('setDefault');
  if not ( getTypeEnum() in [DateTimeType,StringType] ) then
    raise ESDOUnsupportedOperationException.Create('setDefault');
  FreeDefaultBuffer();
  FDefaultSet := True;
  case getTypeEnum() of
    DateTimeType : FDefault.DateValue := AValue;
    StringType   :
      begin
        New(FDefault.StringValue);
        FDefault.StringValue^ := TSDOConvertHelper.DateToString(AValue);
      end;
  end;
end;

function TSDOPropertyAsbtract.isAttribute() : Boolean;
begin
  Result := (pfIsAttribute in FFlags);
end;

{ TSDOPropertyList }

constructor TSDOPropertyList.Create(const ABaseList : ISDOPropertyList);
begin
  FList := TInterfaceList.Create();
  FBaseList := ABaseList;
end;

destructor TSDOPropertyList.Destroy();
begin
  FBaseList := nil;
  FList := nil;
  inherited;
end;

procedure TSDOPropertyList.drop(const AProperty: ISDOProperty);
var
  locProp : ISDOProperty;
  //locPropX : ISDOPropertyEx;
begin
  if ( AProperty = nil ) then
    raise ESDOIllegalArgumentException.Create('AProperty');
  locProp := AProperty as ISDOProperty;
  if ( FList.Remove(AProperty) < 0 ) then
    raise ESDOPropertyNotFoundException.Create(AProperty.getName());
  //locPropX := locProp as ISDOPropertyEx;
  //locPropX.ClearType();
end;

function TSDOPropertyList.find(const AName: string): ISDOProperty;
var
  i : Integer;
begin
  i := getIndex(AName);
  if ( i >= 0 ) then
    Result := GetItem(i)
  else
    Result := nil;
end;

function TSDOPropertyList.getCount() : Integer;
begin
  Result := FList.Count;
  if ( FBaseList <> nil ) then
    Inc(Result,FBaseList.getCount());
end;

function TSDOPropertyList.getIndex(const APropertyName: string): Integer;
var
  i : Integer;
  prp : ISDOProperty;
begin
  if ( FBaseList <> nil ) then
    Result := FBaseList.getIndex(APropertyName)
  else
    Result := -1;
  if ( Result = -1 ) then begin
    for i := 0 to Pred(FList.Count) do begin
      prp := FList[i] as ISDOProperty;
      if AnsiSameText(APropertyName,prp.getName()) then begin
        Result := i;
        if ( FBaseList <> nil ) then
          Inc(Result,FBaseList.getCount());
        Break;
      end;
    end;
  end;
end;

function TSDOPropertyList.getItem(const AIndex: Integer): ISDOProperty;
begin
  if ( AIndex >= 0 ) and ( AIndex < getCount() ) then begin
    if ( FBaseList <> nil ) then begin
      if ( AIndex < FBaseList.getCount() ) then
        Result := FBaseList.getItem(AIndex)
      else
        Result := FList[AIndex - FBaseList.getCount()] as ISDOProperty;
    end else begin
      Result := FList[AIndex] as ISDOProperty;
    end;
  end else begin
    raise ESDOIndexOutOfRangeException.Create(AIndex);
  end;
end;

procedure TSDOPropertyList.add(const AProperty: ISDOProperty);
var
  locProp : ISDOProperty;
begin
  if ( AProperty = nil ) then
    raise ESDOIllegalArgumentException.Create('AProperty');
  if ( find(AProperty.getName()) = nil ) then begin
    locProp := AProperty as ISDOProperty;
    FList.Add(locProp);
  end;
end;

function TSDOPropertyList.SetBase(const ABaseList: ISDOPropertyList): ISDOPropertyList;
begin
  Result := FBaseList;
  FBaseList := ABaseList;
end;

end.
