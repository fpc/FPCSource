{
    This file is part of the Free Pascal Class Library SDO Implementation
    Copyright (c) 2012 by Inoussa OUEDRAOGO
    Free Pascal development team

    This unit implements the basic SDO objects

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$INCLUDE sdo_global.inc}
unit sdo_dataobject;

interface
uses
  SysUtils, Classes, Contnrs,
  sdo_types, sdo, sdo_type, sdo_changesummary, sdo_xpath_helper, sdo_linked_list,
  sdo_field_imp;

type

  IDataObjectObserver = interface
    ['{EF23F339-EF10-4312-B202-43AA5C743841}']
    procedure NotifyDeletion(
      const ADataObject : ISDODataObject;
      const AProperty : ISDOProperty
    );
  end;

  ISDODataObjectEx = interface(ISDODataObject)
    ['{2EA33304-D190-425F-A952-685619896AB2}']
    //Return TRUE if this instance is an instance or a derived class
    function IsInstanceOf(const AType : ISDOType) : Boolean;

    function IsAncestorOf(const AObject : ISDODataObject) : Boolean;
    procedure setContainer(
      const AContainer : ISDODataObject;
      const AContainerProperty : ISDOProperty
    );
    procedure AddReference(
      const AReferencer : ISDODataObject;
      const AReferenceProperty : ISDOProperty
    );
    procedure RemoveReference(
      const AReferencer : ISDODataObject;
      const AReferenceProperty : ISDOProperty
    );
    procedure NotifyContainedObjectsForDeletion(const ACallFromParent : Boolean);// logical deletion

    // for open Type support
    procedure addProperty(
      const APropName : string;
      const APropType : ISDOType;
      const AFlags : TPropertyFlags
    );
  end;

  TObserverInfo = class
  private
    FDataObject  : Pointer;
    FRefProperty : Pointer;
  public
    constructor Create(const ADataObject : ISDODataObject; const ARefProperty : ISDOProperty);
    function GetDataObject() : ISDODataObject;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function GetRefProperty() : ISDOProperty;{$IFDEF USE_INLINE}inline;{$ENDIF}
  end;

  TDataObjectObserverList = class
  private
    FList : TObjectList;
  public
    constructor Create();
    destructor Destroy();override;
    procedure Add(const ADataObject : ISDODataObject; const ARefProperty : ISDOProperty);{$IFDEF USE_INLINE}inline;{$ENDIF}
    function IndexOf(const ADataObject : ISDODataObject; const ARefProperty : ISDOProperty) : PtrInt;
    function Find(const ADataObject : ISDODataObject; const ARefProperty : ISDOProperty) : TObserverInfo;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function GetCount() : PtrInt;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function GetItem(const AIndex : PtrInt) : TObserverInfo;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function Extract(const AIndex : PtrInt) : TObserverInfo;{$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure Delete(const AIndex : PtrInt);{$IFDEF USE_INLINE}inline;{$ENDIF}
  end;

  TSDOBaseDataObjectClass = class of TSDOBaseDataObject;
  TSDOBaseDataObject = class(
    TInterfacedObject,
    IInterface,
    ISDODataObject,
    ISDODataObjectEx,
    IDataObjectObserver
  )
  private
    FDestroying : Boolean;
    FType : ISDOObjectType;
    FContainer : Pointer;
    FContainerProperty : Pointer;
    FBuffer : TSDOFieldBuffer;
    FBufferLength : PtrUInt;
    FChangeSummary : Pointer;//ISDOChangeSummaryEx;
    FReferenceLinkList : TDataObjectObserverList;
  private
    FXPathExpression : TXPathExpression;
    FXPathProcessor : TXPathProcessor;
  protected
    function _Release: LongInt; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    procedure PrepareDataBuffer();
    procedure FreeBuffer();
    function parsePropertyPath(const APath : string) : TXPathExecContext;
    function IsOwnerOf(const AProp : ISDOProperty) : Boolean;virtual;//{$IFDEF USE_INLINE}inline;{$ENDIF}
    function CreateList(const AProperty : ISDOProperty) : ISDODataObjectList;{$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure InitializeDefaultValues();
  protected
    procedure RecordChange(const AProperty : ISDOProperty);{$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure NotifyReferencersForDeletion();
  protected
    // ISDODataObjectEx
    function IsInstanceOf(const AType : ISDOType) : Boolean;
    function IsAncestorOf(const AObject : ISDODataObject) : Boolean;
    procedure setContainer(
      const AContainer : ISDODataObject;
      const AContainerProperty : ISDOProperty
    );

    //ISDODataObject
    function getPropertyIndex(const AProperty : ISDOProperty) : PtrInt;
    function getInstanceProperties() : ISDOPropertyList;virtual;

    function getProperty(const AIndex : PtrUInt) : ISDOProperty;overload;
    function getProperty(const AProp : string) : ISDOProperty;overload;

    function getContainer() : ISDODataObject;
    function getContainmentProperty() : ISDOProperty;
    function getType() : ISDOType;
    function getTypeEnum() : TSDOTypeKind;

    function getList(const APath : string) : ISDODataObjectList; overload;
    function getList(const APropertyIndex : PtrUInt) : ISDODataObjectList; overload;
    function getList(const AProperty : ISDOProperty) : ISDODataObjectList; overload;

    function getDataObject(const APath : string) : ISDODataObject; overload;
    function getDataObject(const APropertyIndex : PtrUInt) : ISDODataObject; overload;
    function getDataObject(const AProperty : ISDOProperty) : ISDODataObject; overload;

    procedure setDataObject(const APath : string; AValue : ISDODataObject); overload; virtual;
    procedure setDataObject(const APropertyIndex : PtrUInt; AValue : ISDODataObject); overload;
    procedure setDataObject(const AProperty : ISDOProperty; AValue : ISDODataObject); overload;

    function getBoolean(const APath : string) : TSDOBoolean; overload;
    function getBoolean(const APropertyIndex : PtrUInt) : TSDOBoolean; overload;
    function getBoolean(const AProperty : ISDOProperty) : TSDOBoolean; overload;

    procedure setBoolean(const APath : string; const AValue : TSDOBoolean); overload; virtual;
    procedure setBoolean(const APropertyIndex : PtrUInt; const AValue : TSDOBoolean); overload;
    procedure setBoolean(const AProperty : ISDOProperty; const AValue : TSDOBoolean); overload;

    function getByte(const APath : string) : TSDOByte;overload;
    function getByte(const APropertyIndex : PtrUInt) : TSDOByte;overload;
    function getByte(const AProperty : ISDOProperty) : TSDOByte;overload;

    procedure setByte(const APath : string; const AValue : TSDOByte);overload; virtual;
    procedure setByte(const APropertyIndex : PtrUInt; const AValue : TSDOByte);overload;
    procedure setByte(const AProperty : ISDOProperty; const AValue : TSDOByte);overload;

{$IFDEF HAS_SDO_CHAR}
    function getCharacter(const APath : string) : TSDOChar;overload;
    function getCharacter(const APropertyIndex : PtrUInt) : TSDOChar;overload;
    function getCharacter(const AProperty : ISDOProperty) : TSDOChar;overload;

    procedure setCharacter(const APath : string; const AValue : TSDOChar);overload; virtual;
    procedure setCharacter(const APropertyIndex : PtrUInt; const AValue : TSDOChar);overload;
    procedure setCharacter(const AProperty : ISDOProperty; const AValue : TSDOChar);overload;
{$ENDIF HAS_SDO_CHAR}

{$IFDEF HAS_SDO_BYTES}
    function getBytes(const APath : string) : TSDOBytes;overload;
    function getBytes(const APropertyIndex : PtrUInt) : TSDOBytes;overload;
    function getBytes(const AProperty : ISDOProperty) : TSDOBytes;overload;

    procedure setBytes(const APath : string; AValue : TSDOBytes);overload; virtual;
    procedure setBytes(const APropertyIndex : PtrUInt; AValue : TSDOBytes);overload;
    procedure setBytes(const AProperty : ISDOProperty; AValue : TSDOBytes);overload;
{$ENDIF HAS_SDO_BYTES}

{$IFDEF HAS_SDO_CURRENCY}
    function getCurrency(const APath : string) : TSDOCurrency;overload;
    function getCurrency(const APropertyIndex : PtrUInt) : TSDOCurrency;overload;
    function getCurrency(const AProperty : ISDOProperty) : TSDOCurrency;overload;

    procedure setCurrency(const APath : string; const AValue : TSDOCurrency);overload;  virtual;
    procedure setCurrency(const APropertyIndex : PtrUInt; const AValue : TSDOCurrency);overload;
    procedure setCurrency(const AProperty : ISDOProperty; const AValue : TSDOCurrency);overload;
{$ENDIF HAS_SDO_CURRENCY}

    function getString(const APath : string) : TSDOString;overload;
    function getString(const APropertyIndex : PtrUInt) : TSDOString;overload;
    function getString(const AProperty : ISDOProperty) : TSDOString;overload;

    procedure setString(const APath : string; const AValue : TSDOString);overload;virtual;
    procedure setString(const APropertyIndex : PtrUInt; const AValue : TSDOString);overload;
    procedure setString(const AProperty : ISDOProperty; const AValue : TSDOString);overload;

    function getDate(const APath : string) : TSDODate;overload;
    function getDate(const APropertyIndex : PtrUInt) : TSDODate;overload;
    function getDate(const AProperty : ISDOProperty) : TSDODate;overload;

    procedure setDate(const APath : string; const AValue : TSDODate);overload;virtual;
    procedure setDate(const APropertyIndex : PtrUInt; const AValue : TSDODate);overload;
    procedure setDate(const AProperty : ISDOProperty; const AValue : TSDODate);overload;

{$IFDEF HAS_SDO_DOUBLE}
    function getDouble(const APath : string) : TSDODouble;overload;
    function getDouble(const APropertyIndex : PtrUInt) : TSDODouble;overload;
    function getDouble(const AProperty : ISDOProperty) : TSDODouble;overload;

    procedure setDouble(const APath : string; const AValue : TSDODouble);overload; virtual;
    procedure setDouble(const APropertyIndex : PtrUInt; const AValue : TSDODouble);overload;
    procedure setDouble(const AProperty : ISDOProperty; const AValue : TSDODouble);overload; 
{$ENDIF HAS_SDO_DOUBLE}

{$IFDEF HAS_SDO_FLOAT}
    function getFloat(const APath : string) : TSDOFloat;overload;
    function getFloat(const APropertyIndex : PtrUInt) : TSDOFloat;overload;
    function getFloat(const AProperty : ISDOProperty) : TSDOFloat;overload;

    procedure setFloat(const APath : string; const AValue : TSDOFloat);overload; virtual;
    procedure setFloat(const APropertyIndex : PtrUInt; const AValue : TSDOFloat);overload;
    procedure setFloat(const AProperty : ISDOProperty; const AValue : TSDOFloat);overload;
{$ENDIF HAS_SDO_FLOAT}

    function getInteger(const APath : string) : TSDOInteger;overload;
    function getInteger(const APropertyIndex : PtrUInt) : TSDOInteger;overload;
    function getInteger(const AProperty : ISDOProperty) : TSDOInteger;overload;

    procedure setInteger(const APath : string; const AValue : TSDOInteger);overload;virtual;
    procedure setInteger(const APropertyIndex : PtrUInt; const AValue : TSDOInteger);overload;
    procedure setInteger(const AProperty : ISDOProperty; const AValue : TSDOInteger);overload;

{$IFDEF HAS_SDO_LONG}
    function getLong(const APath : string) : TSDOLong;overload;
    function getLong(const APropertyIndex : PtrUInt) : TSDOLong;overload;
    function getLong(const AProperty : ISDOProperty) : TSDOLong;overload;

    procedure setLong(const APath : string; const AValue : TSDOLong);overload; virtual;
    procedure setLong(const APropertyIndex : PtrUInt; const AValue : TSDOLong);overload;
    procedure setLong(const AProperty : ISDOProperty; const AValue : TSDOLong);overload; 
{$ENDIF HAS_SDO_LONG}

{$IFDEF HAS_SDO_SHORT}
    function getShort(const APath : string) : TSDOShort;overload;
    function getShort(const APropertyIndex : PtrUInt) : TSDOShort;overload;
    function getShort(const AProperty : ISDOProperty) : TSDOShort;overload;

    procedure setShort(const APath : string; const AValue : TSDOShort);overload; virtual;
    procedure setShort(const APropertyIndex : PtrUInt; const AValue : TSDOShort);overload;
    procedure setShort(const AProperty : ISDOProperty; const AValue : TSDOShort);overload; 
{$ENDIF HAS_SDO_SHORT}

    function getVariant(const APath : string) : TSDOVariant;overload;
    function getVariant(const APropertyIndex : PtrUInt) : TSDOVariant;overload;
    function getVariant(const AProperty : ISDOProperty) : TSDOVariant;overload;

    procedure setVariant(const APath : string; const AValue : TSDOVariant);overload;
    procedure setVariant(const APropertyIndex : PtrUInt; const AValue : TSDOVariant);overload;
    procedure setVariant(const AProperty : ISDOProperty; const AValue : TSDOVariant);overload;

    procedure setNull(const APath : string);overload;
    procedure setNull(const APropertyIndex : PtrUInt);overload;
    procedure setNull(const AProperty : ISDOProperty);overload;

    function isNull(const APath : string) : Boolean;overload;
    function isNull(const APropertyIndex : PtrUInt) : Boolean;overload;
    function isNull(const AProperty : ISDOProperty) : Boolean;overload;

    function isSet(const APath : string) : Boolean;overload;
    function isSet(const APropertyIndex : PtrUInt) : Boolean;overload;
    function isSet(const AProperty : ISDOProperty) : Boolean;overload;

    procedure unset(const APath : string);overload;
    procedure unset(const APropertyIndex : PtrUInt);overload;
    procedure unset(const AProperty : ISDOProperty);overload;

    function createDataObject(const APath : string) : ISDODataObject; overload;
    function createDataObject(const APropertyIndex : PtrUInt) : ISDODataObject; overload;
    function createDataObject(const AProperty : ISDOProperty) : ISDODataObject; overload;

    function getChangeSummary() : ISDOChangeSummary;overload;
    {function getChangeSummary(const APath : string) : ISDOChangeSummary;overload;
    function getChangeSummary(const APropIndex : PtrUInt) : ISDOChangeSummary;overload;
    function getChangeSummary(const AProp : ISDOProperty ) : ISDOChangeSummary;overload;
    }
    procedure clear();

    // IDataObjectObserver implementation
    procedure NotifyDeletion(
      const ADataObject : ISDODataObject;
      const AProperty : ISDOProperty
    );
    procedure AddReference(
      const AReferencer : ISDODataObject;
      const AReferenceProperty : ISDOProperty
    );
    procedure RemoveReference(
      const AReferencer : ISDODataObject;
      const AReferenceProperty : ISDOProperty
    );
    procedure NotifyContainedObjectsForDeletion(const ACallFromParent : Boolean);// logical deletion
    // for open Type support
    procedure addProperty(
      const APropName : string;
      const APropType : ISDOType;
      const AFlags : TPropertyFlags
    );
  public
    constructor Create(
      const AType : ISDOTypeEx;
      const AContainer : ISDODataObject;
      const AContainerProperty : ISDOProperty
    );virtual;
    destructor Destroy();override;
  end;

  TSDODataObject = class(TSDOBaseDataObject, IInterface, ISDODataObject)
  end;

  TSDOOpenedDataObject = class(
    TSDOBaseDataObject,
    IInterface,
    ISDODataObject,
    ISDODataObjectEx
  )
  private
    FInstanceProperties : ISDOPropertyListEx;
  protected
    function IsOwnerOf(const AProp : ISDOProperty) : Boolean; override;
    function getInstanceProperties() : ISDOPropertyList;override;
    procedure setBoolean(const APath : string; const AValue : TSDOBoolean); overload; override;
    procedure setByte(const APath : string; const AValue : TSDOByte); overload; override;
{$IFDEF HAS_SDO_BYTES}
    procedure setBytes(const APath : string; AValue : TSDOBytes); overload; override;
{$ENDIF HAS_SDO_BYTES}
{$IFDEF HAS_SDO_CHAR}
    procedure setCharacter(const APath : string; const AValue : TSDOChar); overload; override;
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
    procedure setCurrency(const APath : string; const AValue : Currency); overload; override;
{$ENDIF HAS_SDO_CURRENCY}
    procedure setDate(const APath : string; const AValue : TSDODate);overload;override;
{$IFDEF HAS_SDO_DOUBLE}
    procedure setDouble(const APath : string; const AValue : TSDODouble); overload; override;
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
    procedure setFloat(const APath : string; const AValue : TSDOFloat); overload; override;
{$ENDIF HAS_SDO_FLOAT}
    procedure setInteger(const APath : string; const AValue : TSDOInteger);overload;override;
{$IFDEF HAS_SDO_LONG}
    procedure setLong(const APath : string; const AValue : TSDOLong); overload; override;
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
    procedure setShort(const APath : string; const AValue : TSDOShort); overload; override;
{$ENDIF HAS_SDO_SHORT}
    procedure setString(const APath : string; const AValue : TSDOString);overload;override;
    procedure setDataObject(const APath : string; AValue : ISDODataObject); overload; override;

    // for open Type support
    procedure addProperty(
      const APropName : string;
      const APropType : ISDOType;
      const AFlags : TPropertyFlags
    );
  public
    constructor Create(
      const AType : ISDOTypeEx;
      const AContainer : ISDODataObject;
      const AContainerProperty : ISDOProperty
    );override;
  end;

  TSDODataObjectList = class(TInterfacedObject,IInterface,ISDODataObjectList)
  private
    FItemType : ISDOType;
    FData : TDoubleLinkedList;
    FCursor : ILinkedListCursor;
    FField : ISDOField;
  private
    procedure Clear();
    procedure InternalDelete(const AData : PLinkedNode);{$IFDEF USE_INLINE}inline;{$ENDIF}
    function InternalAppend() : PLinkedNode;{$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure CheckCursorPosition();{$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure MoveTo(const AIndex : PtrInt);{$IFDEF USE_INLINE}inline;{$ENDIF}
  protected
    function getItemType() : ISDOType;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function size() : PtrInt;
    function getCursor() : ILinkedListCursor;

    {These operations use the cursor location}
    function getBoolean() : TSDOBoolean;overload;
    function getByte() : TSDOByte;overload;
{$IFDEF HAS_SDO_BYTES}
    function getBytes() : TSDOBytes;overload;
{$ENDIF HAS_SDO_BYTES}
{$IFDEF HAS_SDO_CHAR}
    function getCharacter() : TSDOChar;overload;
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
    function getCurrency() : TSDOCurrency;overload;
{$ENDIF HAS_SDO_CURRENCY}
    function getDate() : TSDODate;overload;
{$IFDEF HAS_SDO_DOUBLE}
    function getDouble() : TSDODouble;overload;
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
    function getFloat() : TSDOFloat;overload;
{$ENDIF HAS_SDO_FLOAT}
    function getInteger() : TSDOInteger;overload;
{$IFDEF HAS_SDO_LONG}
    function getLong() : TSDOLong;overload;
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
    function getShort() : TSDOShort;overload;
{$ENDIF HAS_SDO_SHORT}
    function getString() : TSDOString;overload;
    function getDataObject() : ISDODataObject;overload;
    function getVariant() : TSDOVariant;overload;


    procedure setBoolean(const AValue : TSDOBoolean);overload;
    procedure setByte(const AValue : TSDOByte);overload;
{$IFDEF HAS_SDO_BYTES}
   procedure setBytes(AValue : TSDOBytes);overload;
{$ENDIF HAS_SDO_BYTES}
{$IFDEF HAS_SDO_CHAR}
    procedure setCharacter(const AValue : TSDOChar);overload;
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
    procedure setCurrency(const AValue : TSDOCurrency);overload;
{$ENDIF HAS_SDO_CURRENCY}
    procedure setDate(const AValue : TSDODate);overload;
{$IFDEF HAS_SDO_DOUBLE}
    procedure setDouble(const AValue : TSDODouble);overload;
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
    procedure setFloat(const AValue : TSDOFloat);overload;
{$ENDIF HAS_SDO_FLOAT}
    procedure setInteger(const AValue : TSDOInteger);overload;
{$IFDEF HAS_SDO_LONG}
    procedure setLong(const AValue : TSDOLong);overload;
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
    procedure setShort(const AValue : TSDOShort);overload;
{$ENDIF HAS_SDO_SHORT}
    procedure setString(const AValue : TSDOString);overload;
    procedure setDataObject(AValue : ISDODataObject);overload;
    procedure setVariant(const AValue : TSDOVariant);overload;


    function getBoolean(const AIndex : PtrInt) : TSDOBoolean;overload;
    function getByte(const AIndex : PtrInt) : TSDOByte;overload;
{$IFDEF HAS_SDO_BYTES}
    function getBytes(const AIndex : PtrInt) : TSDOBytes;overload;
{$ENDIF HAS_SDO_BYTES}
{$IFDEF HAS_SDO_CHAR}
    function getCharacter(const AIndex : PtrInt) : TSDOChar;overload;
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
    function getCurrency(const AIndex : PtrInt) : TSDOCurrency;overload;
{$ENDIF HAS_SDO_CURRENCY}
    function getDate(const AIndex : PtrInt) : TSDODate;overload;
{$IFDEF HAS_SDO_DOUBLE}
    function getDouble(const AIndex : PtrInt) : TSDODouble;overload;
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
    function getFloat(const AIndex : PtrInt) : TSDOFloat;overload;
{$ENDIF HAS_SDO_FLOAT}
    function getInteger(const AIndex : PtrInt) : TSDOInteger;overload;
{$IFDEF HAS_SDO_LONG}
    function getLong(const AIndex : PtrInt) : TSDOLong;overload;
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
    function getShort(const AIndex : PtrInt) : TSDOShort;overload;
{$ENDIF HAS_SDO_SHORT}
    function getString(const AIndex : PtrInt) : TSDOString;overload;
    function getDataObject(const AIndex : PtrInt) : ISDODataObject;overload;
    function getVariant(const AIndex : PtrInt) : TSDOVariant;overload;


    procedure setBoolean(const AIndex : PtrInt; const AValue : TSDOBoolean);overload;
    procedure setByte(const AIndex : PtrInt; const AValue : TSDOByte);overload;
{$IFDEF HAS_SDO_BYTES}
    procedure setBytes(const AIndex : PtrInt; AValue : TSDOBytes);overload;
{$ENDIF HAS_SDO_BYTES}
{$IFDEF HAS_SDO_CHAR}
    procedure setCharacter(const AIndex : PtrInt; const AValue : TSDOChar);overload;
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
    procedure setCurrency(const AIndex : PtrInt; const AValue : TSDOCurrency);overload;
{$ENDIF HAS_SDO_CURRENCY}
    procedure setDate(const AIndex : PtrInt; const AValue : TSDODate);overload;
{$IFDEF HAS_SDO_DOUBLE}
    procedure setDouble(const AIndex : PtrInt; const AValue : TSDODouble);overload;
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
    procedure setFloat(const AIndex : PtrInt; const AValue : TSDOFloat);overload;
{$ENDIF HAS_SDO_FLOAT}
    procedure setInteger(const AIndex : PtrInt; const AValue : TSDOInteger);overload;
{$IFDEF HAS_SDO_LONG}
    procedure setLong(const AIndex : PtrInt; const AValue : TSDOLong);overload;
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
    procedure setShort(const AIndex : PtrInt; const AValue : TSDOShort);overload;
{$ENDIF HAS_SDO_SHORT}
    procedure setString(const AIndex : PtrInt; const AValue : TSDOString);overload;
    procedure setVariant(const AIndex : PtrInt; const AValue : TSDOVariant);overload;
    procedure setDataObject(const AIndex : PtrInt; AValue : ISDODataObject);overload;


    procedure insert(const AIndex : PtrInt; const AValue : TSDOBoolean);overload;
    procedure insert(const AIndex : PtrInt; const AValue : TSDOByte);overload;
{$IFDEF HAS_SDO_BYTES}
    procedure insertBytes(const AIndex : PtrInt; AValue : TSDOBytes);overload;
{$ENDIF HAS_SDO_BYTES}
{$IFDEF HAS_SDO_CHAR}
    procedure insert(const AIndex : PtrInt; const AValue : TSDOChar);overload;
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
    procedure insertCurrency(const AIndex : PtrInt; const AValue : TSDOCurrency);
{$ENDIF HAS_SDO_CURRENCY}
    procedure insert(const AIndex : PtrInt; const AValue : TSDODate);overload;
{$IFDEF HAS_SDO_DOUBLE}
    procedure insert(const AIndex : PtrInt; const AValue : TSDODouble);overload;
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
    procedure insert(const AIndex : PtrInt; const AValue : TSDOFloat);overload;
{$ENDIF HAS_SDO_FLOAT}
    procedure insert(const AIndex : PtrInt; const AValue : TSDOInteger);overload;
{$IFDEF HAS_SDO_LONG}
    procedure insert(const AIndex : PtrInt; const AValue : TSDOLong);overload;
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
    procedure insert(const AIndex : PtrInt; const AValue : TSDOShort);overload;
{$ENDIF HAS_SDO_SHORT}
    procedure insert(const AIndex : PtrInt; const AValue : TSDOString);overload;
    procedure insert(const AIndex : PtrInt; AValue : ISDODataObject);overload;


    procedure append(const AValue : TSDOBoolean);overload;
    procedure append(const AValue : TSDOByte);overload;
{$IFDEF HAS_SDO_BYTES}
    procedure appendBytes(AValue : TSDOBytes);overload;
{$ENDIF HAS_SDO_BYTES}
{$IFDEF HAS_SDO_CHAR}
    procedure append(const AValue : TSDOChar);overload;
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
    procedure appendCurrency(const AValue : TSDOCurrency);
{$ENDIF HAS_SDO_CURRENCY}
    procedure append(const AValue : TSDODate);overload;
{$IFDEF HAS_SDO_DOUBLE}
    procedure append(const AValue : TSDODouble);overload;
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
    procedure append(const AValue : TSDOFloat);overload;
{$ENDIF HAS_SDO_FLOAT}
    procedure append(const AValue : TSDOInteger);overload;
{$IFDEF HAS_SDO_LONG}
    procedure append(const AValue : TSDOLong);overload;
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
    procedure append(const AValue : TSDOShort);overload;
{$ENDIF HAS_SDO_SHORT}
    procedure append(const AValue : TSDOString);overload;
    procedure append(AValue : ISDODataObject);overload;


    procedure delete(const AIndex : PtrInt);overload;
    procedure delete();overload;
  public
    constructor Create(const AItemType : ISDOType);
    destructor Destroy();override;
  end;

  TSDOOwnedDataObjectList = class(TSDODataObjectList,IInterface,ISDODataObjectList)
  private
    FOwner : Pointer; // do not add a reference count!
    FOwnerProperty : ISDOProperty;
  protected
    procedure RecordChange(const AChange : TManyValuePropAction);overload;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function getOwner() : ISDODataObject;{$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure InternalSetDataObject(const AValue : ISDODataObject; const ADoRecordChange : Boolean);
  protected
    procedure setBoolean(const AValue : TSDOBoolean);overload;
    procedure setByte(const AValue : TSDOByte);overload;
{$IFDEF HAS_SDO_BYTES}
    procedure setBytes(AValue : TSDOBytes);overload;
{$ENDIF HAS_SDO_BYTES}
{$IFDEF HAS_SDO_CHAR}
    procedure setCharacter(const AValue : TSDOChar);overload;
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
    procedure setCurrency(const AValue : TSDOCurrency);overload;
{$ENDIF HAS_SDO_CURRENCY}
    procedure setDate(const AValue : TSDODate);overload;
{$IFDEF HAS_SDO_DOUBLE}
    procedure setDouble(const AValue : TSDODouble);overload;
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
    procedure setFloat(const AValue : TSDOFloat);overload;
{$ENDIF HAS_SDO_FLOAT}
    procedure setInteger(const AValue : TSDOInteger);overload;
{$IFDEF HAS_SDO_LONG}
    procedure setLong(const AValue : TSDOLong);overload;
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
    procedure setShort(const AValue : TSDOShort);overload;
{$ENDIF HAS_SDO_SHORT}
    procedure setString(const AValue : TSDOString);overload;
    procedure setDataObject(AValue : ISDODataObject);overload;
    procedure setVariant(const AValue : TSDOVariant);overload;

    procedure setBoolean(const AIndex : PtrInt; const AValue : TSDOBoolean);overload;
    procedure setByte(const AIndex : PtrInt; const AValue : TSDOByte);overload;
{$IFDEF HAS_SDO_BYTES}
    procedure setBytes(const AIndex : PtrInt; AValue : TSDOBytes);overload;
{$ENDIF HAS_SDO_BYTES}
{$IFDEF HAS_SDO_CHAR}
    procedure setCharacter(const AIndex : PtrInt; const AValue : TSDOChar);overload;
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
    procedure setCurrency(const AIndex : PtrInt; const AValue : TSDOCurrency);overload;
{$ENDIF HAS_SDO_CURRENCY}
    procedure setDate(const AIndex : PtrInt; const AValue : TSDODate);overload;
{$IFDEF HAS_SDO_DOUBLE}
    procedure setDouble(const AIndex : PtrInt; const AValue : TSDODouble);overload;
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
    procedure setFloat(const AIndex : PtrInt; const AValue : TSDOFloat);overload;
{$ENDIF HAS_SDO_FLOAT}
    procedure setInteger(const AIndex : PtrInt; const AValue : TSDOInteger);overload;
{$IFDEF HAS_SDO_LONG}
    procedure setLong(const AIndex : PtrInt; const AValue : TSDOLong);overload;
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
    procedure setShort(const AIndex : PtrInt; const AValue : TSDOShort);overload;
{$ENDIF HAS_SDO_SHORT}
    procedure setString(const AIndex : PtrInt; const AValue : TSDOString);overload;
    procedure setDataObject(const AIndex : PtrInt; AValue : ISDODataObject);overload;
    procedure setVariant(const AIndex : PtrInt; const AValue : TSDOVariant);overload;

    procedure insert(const AIndex : PtrInt; const AValue : TSDOBoolean);overload;
    procedure insert(const AIndex : PtrInt; const AValue : TSDOByte);overload;
{$IFDEF HAS_SDO_BYTES}
    procedure insertBytes(const AIndex : PtrInt; AValue : TSDOBytes);overload;
{$ENDIF HAS_SDO_BYTES}
{$IFDEF HAS_SDO_CHAR}
    procedure insert(const AIndex : PtrInt; const AValue : TSDOChar);overload;
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
    procedure insertCurrency(const AIndex : PtrInt; const AValue : TSDOCurrency);
{$ENDIF HAS_SDO_CURRENCY}
    procedure insert(const AIndex : PtrInt; const AValue : TSDODate);overload;
{$IFDEF HAS_SDO_DOUBLE}
    procedure insert(const AIndex : PtrInt; const AValue : TSDODouble);overload;
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
    procedure insert(const AIndex : PtrInt; const AValue : TSDOFloat);overload;
{$ENDIF HAS_SDO_FLOAT}
    procedure insert(const AIndex : PtrInt; const AValue : TSDOInteger);overload;
{$IFDEF HAS_SDO_LONG}
    procedure insert(const AIndex : PtrInt; const AValue : TSDOLong);overload;
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
    procedure insert(const AIndex : PtrInt; const AValue : TSDOShort);overload;
{$ENDIF HAS_SDO_SHORT}
    procedure insert(const AIndex : PtrInt; const AValue : TSDOString);overload;
    procedure insert(const AIndex : PtrInt; AValue : ISDODataObject);overload;

    procedure append(const AValue : TSDOBoolean);overload;
    procedure append(const AValue : TSDOByte);overload;
{$IFDEF HAS_SDO_BYTES}
    procedure appendBytes(AValue : TSDOBytes);overload;
{$ENDIF HAS_SDO_BYTES}
{$IFDEF HAS_SDO_CHAR}
    procedure append(const AValue : TSDOChar);overload;
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
    procedure appendCurrency(const AValue : TSDOCurrency);
{$ENDIF HAS_SDO_CURRENCY}
    procedure append(const AValue : TSDODate);overload;
{$IFDEF HAS_SDO_DOUBLE}
    procedure append(const AValue : TSDODouble);overload;
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
    procedure append(const AValue : TSDOFloat);overload;
{$ENDIF HAS_SDO_FLOAT}
    procedure append(const AValue : TSDOInteger);overload;
{$IFDEF HAS_SDO_LONG}
    procedure append(const AValue : TSDOLong);overload;
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
    procedure append(const AValue : TSDOShort);overload;
{$ENDIF HAS_SDO_SHORT}
    procedure append(AValue : ISDODataObject);overload;
    procedure append(const AValue : TSDOString);overload;

    procedure delete(const AIndex : PtrInt);overload;
    procedure delete();overload;
  public
    constructor Create(
      const AOwner : ISDODataObject;
      const AProperty : ISDOProperty
    );
  end;

implementation

uses
  sdo_imp_utils, sdo_utils;

{ TSDOBaseDataObject }

procedure TSDOBaseDataObject.clear;
begin

end;

constructor TSDOBaseDataObject.Create(
  const AType: ISDOTypeEx;
  const AContainer : ISDODataObject;
  const AContainerProperty : ISDOProperty
);
begin
  if ( AType = nil ) then
    raise ESDOIllegalArgumentException.Create('AType');
  if ( AContainer <> nil ) and ( AContainerProperty = nil ) then
    raise ESDOIllegalArgumentException.Create('AContainerProperty');
  FType := AType as ISDOObjectType;
  FContainer := Pointer(AContainer);
  FContainerProperty := Pointer(AContainerProperty);
  FXPathExpression := TXPathExpression.Create();
  FXPathProcessor := TXPathProcessor.Create();
  PrepareDataBuffer();
  InitializeDefaultValues();
  FType.setUsedFlag(True);
end;

function TSDOBaseDataObject.createDataObject(const APath: string): ISDODataObject;
var
  locCtx : TXPathExecContext;
begin
  locCtx := parsePropertyPath(APath);
  if ( locCtx.PropertyOwner = nil ) then
    raise ESDOInvalidPathException.Create(APath);
  Result := locCtx.PropertyOwner.createDataObject(locCtx.CurrentProperty);
end;

function TSDOBaseDataObject.createDataObject(const AProperty: ISDOProperty) : ISDODataObject;
var
  prp : ISDOPropertyEx;
begin
  if ( AProperty = nil ) or ( not Self.IsOwnerOf(AProperty) ) then
    raise ESDOIllegalArgumentException.Create('AProperty');
  prp := AProperty as ISDOPropertyEx;
  Result := TSDOBaseDataObjectClass(Self.ClassType).Create(
              prp.getType() as ISDOTypeEx,
              nil, //Self as ISDODataObject,
              prp
            ) as ISDODataObject;
  if not AProperty.isMany() then
    setDataObject(prp,Result);
end;

function TSDOBaseDataObject.createDataObject(const APropertyIndex: PtrUInt): ISDODataObject;
begin
  Result := createDataObject(getProperty(APropertyIndex));
end;

destructor TSDOBaseDataObject.Destroy();
begin
  FDestroying := True;
  FreeBuffer();
  FType := nil;
  FContainer := nil;
  FContainerProperty := nil;
  FChangeSummary := nil;
  FreeAndNil(FReferenceLinkList);
  FreeAndNil(FXPathExpression);
  FreeAndNil(FXPathProcessor);
  inherited;
end;

function TSDOBaseDataObject.getBoolean(const APath: string): TSDOBoolean;
var
  locCtx : TXPathExecContext;
begin
  locCtx := parsePropertyPath(APath);
  if ( locCtx.PropertyOwner = nil ) then
    raise ESDOInvalidPathException.Create(APath);
  if ( locCtx.ContentKind = xckList ) then
    Result := locCtx.ListItem.getBoolean()
  else
    Result := locCtx.PropertyOwner.getBoolean(locCtx.CurrentProperty);
end;

function TSDOBaseDataObject.getBoolean(const APropertyIndex: PtrUInt): TSDOBoolean;
begin
  Result := getBoolean(getProperty(APropertyIndex));
end;

function TSDOBaseDataObject.getBoolean(const AProperty: ISDOProperty): TSDOBoolean;
var
  prp : ISDOPropertyEx;
begin
  if ( AProperty = nil ) or
     ( not Self.IsOwnerOf(AProperty) ) or
     ( AProperty.isMany() )
  then begin
    raise ESDOIllegalArgumentException.Create('AProperty');
  end;
  prp := AProperty as ISDOPropertyEx;
  Result := getField(prp.getTypeEnum()).getBoolean(FBuffer,prp.getBufferOffset());
end;

function TSDOBaseDataObject.getByte(const APath: string): TSDOByte;
var
  locCtx : TXPathExecContext;
begin
  locCtx := parsePropertyPath(APath);
  if ( locCtx.PropertyOwner = nil ) then
    raise ESDOInvalidPathException.Create(APath);
  if ( locCtx.ContentKind = xckList ) then
    Result := locCtx.ListItem.getByte()
  else
    Result := locCtx.PropertyOwner.getByte(locCtx.CurrentProperty);
end;

function TSDOBaseDataObject.getByte(const APropertyIndex: PtrUInt): TSDOByte;
begin
  Result := getByte(getProperty(APropertyIndex));
end;

function TSDOBaseDataObject.getByte(const AProperty: ISDOProperty): TSDOByte;
var
  prp : ISDOPropertyEx;
begin
  if ( AProperty = nil ) or
     ( not Self.IsOwnerOf(AProperty) ) or
     ( AProperty.isMany() )
  then begin
    raise ESDOIllegalArgumentException.Create('AProperty');
  end;
  prp := AProperty as ISDOPropertyEx;
  Result := getField(prp.getTypeEnum()).getByte(FBuffer,prp.getBufferOffset());
end;

{$IFDEF HAS_SDO_BYTES}
function TSDOBaseDataObject.getBytes(const AProperty: ISDOProperty): TSDOBytes;
var
  prp : ISDOPropertyEx;
begin
  if ( AProperty = nil ) or
     ( not Self.IsOwnerOf(AProperty) ) or
     ( AProperty.isMany() )
  then begin
    raise ESDOIllegalArgumentException.Create('AProperty');
  end;
  prp := AProperty as ISDOPropertyEx;
  Result := getField(prp.getTypeEnum()).getBytes(FBuffer,prp.getBufferOffset());
end;

function TSDOBaseDataObject.getBytes(const APath: string): TSDOBytes;
var
  locCtx : TXPathExecContext;
begin
  locCtx := parsePropertyPath(APath);
  if ( locCtx.PropertyOwner = nil ) then
    raise ESDOInvalidPathException.Create(APath);
  if ( locCtx.ContentKind = xckList ) then
    Result := locCtx.ListItem.getBytes()
  else
    Result := locCtx.PropertyOwner.getBytes(locCtx.CurrentProperty);
end;

function TSDOBaseDataObject.getBytes(const APropertyIndex: PtrUInt): TSDOBytes;
begin
  Result := getBytes(getProperty(APropertyIndex));
end;
{$ENDIF HAS_SDO_BYTES}

function TSDOBaseDataObject.getCharacter(const AProperty: ISDOProperty): TSDOChar;
var
  prp : ISDOPropertyEx;
begin
  if ( AProperty = nil ) or
     ( not Self.IsOwnerOf(AProperty) ) or
     ( AProperty.isMany() )
  then begin
    raise ESDOIllegalArgumentException.Create('AProperty');
  end;
  prp := AProperty as ISDOPropertyEx;
  Result := getField(prp.getTypeEnum()).getCharacter(FBuffer,prp.getBufferOffset());
end;

function TSDOBaseDataObject.getCharacter(const APropertyIndex: PtrUInt): TSDOChar;
begin
  Result := getCharacter(getProperty(APropertyIndex));
end;

function TSDOBaseDataObject.getCharacter(const APath: string): TSDOChar;
var
  locCtx : TXPathExecContext;
begin
  locCtx := parsePropertyPath(APath);
  if ( locCtx.PropertyOwner = nil ) then
    raise ESDOInvalidPathException.Create(APath);
  if ( locCtx.ContentKind = xckList ) then
    Result := locCtx.ListItem.getCharacter()
  else
    Result := locCtx.PropertyOwner.getCharacter(locCtx.CurrentProperty);
end;

function TSDOBaseDataObject.getContainer() : ISDODataObject;
begin
  Result := ISDODataObject(FContainer);
end;

function TSDOBaseDataObject.getContainmentProperty() : ISDOProperty;
begin
  if not Assigned(FContainerProperty) then
    raise ESDOPropertyNotFoundException.Create('ContainmentProperty');
  Result := ISDOProperty(FContainerProperty);
end;

{$IFDEF HAS_SDO_CURRENCY }
function TSDOBaseDataObject.getCurrency(const AProperty: ISDOProperty): TSDOCurrency;
var
  prp : ISDOPropertyEx;
begin
  if ( AProperty = nil ) or
     ( not Self.IsOwnerOf(AProperty) ) or
     ( AProperty.isMany() )
  then begin
    raise ESDOIllegalArgumentException.Create('AProperty');
  end;
  prp := AProperty as ISDOPropertyEx;
  Result := getField(prp.getTypeEnum()).getCurrency(FBuffer,prp.getBufferOffset());
end;

function TSDOBaseDataObject.getCurrency(const APropertyIndex: PtrUInt): TSDOCurrency;
begin
  Result := getCurrency(getProperty(APropertyIndex));
end;

function TSDOBaseDataObject.getCurrency(const APath: string): TSDOCurrency;
var
  locCtx : TXPathExecContext;
begin
  locCtx := parsePropertyPath(APath);
  if ( locCtx.PropertyOwner = nil ) then
    raise ESDOInvalidPathException.Create(APath);
  if ( locCtx.ContentKind = xckList ) then
    Result := locCtx.ListItem.getCurrency()
  else
    Result := locCtx.PropertyOwner.getCurrency(locCtx.CurrentProperty);
end;
{$ENDIF HAS_SDO_CURRENCY }

function TSDOBaseDataObject.getDataObject(const APath: string): ISDODataObject;
var
  locCtx : TXPathExecContext;
begin
  locCtx := parsePropertyPath(APath);
  if ( locCtx.PropertyOwner = nil ) then
    raise ESDOInvalidPathException.Create(APath);
  Result := locCtx.ObjectItem;
  {if ( locCtx.ContentKind = xckList ) then
    Result := locCtx.ListItem.getDataObject()
  else
    Result := locCtx.PropertyOwner.getDataObject(locCtx.CurrentProperty);}
end;

function TSDOBaseDataObject.getDataObject(const APropertyIndex: PtrUInt): ISDODataObject;
begin
  Result := getDataObject(getProperty(APropertyIndex));
end;

function TSDOBaseDataObject.getDataObject(const AProperty: ISDOProperty): ISDODataObject;
var
  prp : ISDOPropertyEx;
begin
  if ( AProperty = nil ) or ( not Self.IsOwnerOf(AProperty) ) then
    raise ESDOIllegalArgumentException.Create('AProperty');
  if AProperty.isMany() then begin
    Result := getList(AProperty).getDataObject();
  end else begin
    prp := AProperty as ISDOPropertyEx;
    Result := getField(prp.getTypeEnum()).getDataObject(FBuffer,prp.getBufferOffset());
  end;
end;

function TSDOBaseDataObject.getDate(const APropertyIndex: PtrUInt): TSDODate;
begin
  Result := getDate(getProperty(APropertyIndex));
end;

function TSDOBaseDataObject.getDate(const APath: string): TSDODate;
var
  locCtx : TXPathExecContext;
begin
  locCtx := parsePropertyPath(APath);
  if ( locCtx.PropertyOwner = nil ) then
    raise ESDOInvalidPathException.Create(APath);
  if ( locCtx.ContentKind = xckList ) then
    Result := locCtx.ListItem.getDate()
  else
    Result := locCtx.PropertyOwner.getDate(locCtx.CurrentProperty);
end;

function TSDOBaseDataObject.getDate(const AProperty: ISDOProperty): TSDODate;
var
  prp : ISDOPropertyEx;
begin
  if ( AProperty = nil ) or
     ( not Self.IsOwnerOf(AProperty) ) or
     ( AProperty.isMany() )
  then begin
    raise ESDOIllegalArgumentException.Create('AProperty');
  end;
  prp := AProperty as ISDOPropertyEx;
  Result := getField(prp.getTypeEnum()).getDate(FBuffer,prp.getBufferOffset());
end;

{$IFDEF HAS_SDO_DOUBLE }
function TSDOBaseDataObject.getDouble(const AProperty: ISDOProperty): TSDODouble;
var
  prp : ISDOPropertyEx;
begin
  if ( AProperty = nil ) or
     ( not Self.IsOwnerOf(AProperty) ) or
     ( AProperty.isMany() )
  then begin
    raise ESDOIllegalArgumentException.Create('AProperty');
  end;
  prp := AProperty as ISDOPropertyEx;
  Result := getField(prp.getTypeEnum()).getDouble(FBuffer,prp.getBufferOffset());
end;

function TSDOBaseDataObject.getDouble(const APropertyIndex: PtrUInt): TSDODouble;
begin
  Result := getDouble(getProperty(APropertyIndex));
end;

function TSDOBaseDataObject.getDouble(const APath: string): TSDODouble;
var
  locCtx : TXPathExecContext;
begin
  locCtx := parsePropertyPath(APath);
  if ( locCtx.PropertyOwner = nil ) then
    raise ESDOInvalidPathException.Create(APath);
  if ( locCtx.ContentKind = xckList ) then
    Result := locCtx.ListItem.getDouble()
  else
    Result := locCtx.PropertyOwner.getDouble(locCtx.CurrentProperty);
end;
{$ENDIF HAS_SDO_DOUBLE }

{$IFDEF HAS_SDO_FLOAT }
function TSDOBaseDataObject.getFloat(const AProperty: ISDOProperty): TSDOFloat;
var
  prp : ISDOPropertyEx;
begin
  if ( AProperty = nil ) or
     ( not Self.IsOwnerOf(AProperty) ) or
     ( AProperty.isMany() )
  then begin
    raise ESDOIllegalArgumentException.Create('AProperty');
  end;
  prp := AProperty as ISDOPropertyEx;
  Result := getField(prp.getTypeEnum()).getFloat(FBuffer,prp.getBufferOffset());
end;

function TSDOBaseDataObject.getFloat(const APropertyIndex: PtrUInt): TSDOFloat;
begin
  Result := getFloat(getProperty(APropertyIndex));
end;

function TSDOBaseDataObject.getFloat(const APath: string): TSDOFloat;
var
  locCtx : TXPathExecContext;
begin
  locCtx := parsePropertyPath(APath);
  if ( locCtx.PropertyOwner = nil ) then
    raise ESDOInvalidPathException.Create(APath);
  if ( locCtx.ContentKind = xckList ) then
    Result := locCtx.ListItem.getFloat()
  else
    Result := locCtx.PropertyOwner.getFloat(locCtx.CurrentProperty);
end;
{$ENDIF HAS_SDO_FLOAT }

function TSDOBaseDataObject.getInstanceProperties() : ISDOPropertyList;
begin
  Result := FType.getProperties();
end;

function TSDOBaseDataObject.getInteger(const AProperty: ISDOProperty): TSDOInteger;
var
  prp : ISDOPropertyEx;
begin
  if ( AProperty = nil ) or
     ( not Self.IsOwnerOf(AProperty) ) or
     ( AProperty.isMany() )
  then begin
    raise ESDOIllegalArgumentException.Create('AProperty');
  end;
  prp := AProperty as ISDOPropertyEx;
  Result := getField(prp.getTypeEnum()).getInteger(FBuffer,prp.getBufferOffset());
end;

function TSDOBaseDataObject.getInteger(const APropertyIndex: PtrUInt): TSDOInteger;
begin
  Result := getInteger(getProperty(APropertyIndex));
end;

function TSDOBaseDataObject.getInteger(const APath: string): TSDOInteger;
var
  locCtx : TXPathExecContext;
begin
  locCtx := parsePropertyPath(APath);
  if ( locCtx.PropertyOwner = nil ) then
    raise ESDOInvalidPathException.Create(APath);
  if ( locCtx.ContentKind = xckList ) then
    Result := locCtx.ListItem.getInteger()
  else
    Result := locCtx.PropertyOwner.getInteger(locCtx.CurrentProperty);
end;

function TSDOBaseDataObject.getLong(const AProperty: ISDOProperty): TSDOLong;
var
  prp : ISDOPropertyEx;
begin
  if ( AProperty = nil ) or
     ( not Self.IsOwnerOf(AProperty) ) or
     ( AProperty.isMany() )
  then begin
    raise ESDOIllegalArgumentException.Create('AProperty');
  end;
  prp := AProperty as ISDOPropertyEx;
  Result := getField(prp.getTypeEnum()).getLong(FBuffer,prp.getBufferOffset());
end;

function TSDOBaseDataObject.getLong(const APropertyIndex: PtrUInt): TSDOLong;
begin
   Result := getLong(getProperty(APropertyIndex));
end;

function TSDOBaseDataObject.getLong(const APath: string): TSDOLong;
var
  locCtx : TXPathExecContext;
begin
  locCtx := parsePropertyPath(APath);
  if ( locCtx.PropertyOwner = nil ) then
    raise ESDOInvalidPathException.Create(APath);
  if ( locCtx.ContentKind = xckList ) then
    Result := locCtx.ListItem.getLong()
  else
    Result := locCtx.PropertyOwner.getLong(locCtx.CurrentProperty);
end;

function TSDOBaseDataObject.getProperty(const AIndex: PtrUInt): ISDOProperty;
begin
  Result := getInstanceProperties().getItem(AIndex);//FType.getProperty(AIndex);
end;

function TSDOBaseDataObject.getProperty(const AProp: string): ISDOProperty;
begin
  Result := getInstanceProperties().find(AProp);
  if ( Result = nil ) then
    raise ESDOPropertyNotFoundException.Create(AProp);
end;

function TSDOBaseDataObject.getPropertyIndex(const AProperty: ISDOProperty): PtrInt;
begin
  Result := FType.getPropertyIndex(AProperty.getName());
end;

function TSDOBaseDataObject.getShort(const APropertyIndex: PtrUInt): TSDOShort;
begin
  Result := getShort(getProperty(APropertyIndex));
end;

function TSDOBaseDataObject.getShort(const APath: string): TSDOShort;
var
  locCtx : TXPathExecContext;
begin
  locCtx := parsePropertyPath(APath);
  if ( locCtx.PropertyOwner = nil ) then
    raise ESDOInvalidPathException.Create(APath);
  if ( locCtx.ContentKind = xckList ) then
    Result := locCtx.ListItem.getShort()
  else
    Result := locCtx.PropertyOwner.getShort(locCtx.CurrentProperty);
end;

function TSDOBaseDataObject.getShort(const AProperty: ISDOProperty): TSDOShort;
var
  prp : ISDOPropertyEx;
begin
  if ( AProperty = nil ) or
     ( not Self.IsOwnerOf(AProperty) ) or
     ( AProperty.isMany() )
  then begin
    raise ESDOIllegalArgumentException.Create('AProperty');
  end;
  prp := AProperty as ISDOPropertyEx;
  Result := getField(prp.getTypeEnum()).getShort(FBuffer,prp.getBufferOffset());
end;

function TSDOBaseDataObject.getString(const AProperty: ISDOProperty): TSDOString;
var
  prp : ISDOPropertyEx;
begin
  if ( AProperty = nil ) or
     ( not Self.IsOwnerOf(AProperty) ) or
     ( AProperty.isMany() )
  then begin
    raise ESDOIllegalArgumentException.Create('AProperty');
  end;
  prp := AProperty as ISDOPropertyEx;
  Result := getField(prp.getTypeEnum()).getString(FBuffer,prp.getBufferOffset());
end;

function TSDOBaseDataObject.getString(const APropertyIndex: PtrUInt): TSDOString;
begin
  Result := getString(getProperty(APropertyIndex));
end;

function TSDOBaseDataObject.getString(const APath: string): TSDOString;
var
  locCtx : TXPathExecContext;
begin
  locCtx := parsePropertyPath(APath);
  if ( locCtx.PropertyOwner = nil ) then
    raise ESDOInvalidPathException.Create(APath);
  if ( locCtx.ContentKind = xckList ) then
    Result := locCtx.ListItem.getString()
  else
    Result := locCtx.PropertyOwner.getString(locCtx.CurrentProperty);
end;

function TSDOBaseDataObject.getType() : ISDOType;
begin
  Result := FType as ISDOType;
end;

function TSDOBaseDataObject.getTypeEnum() : TSDOTypeKind;
begin
  Result := FType.getTypeEnum();
end;

function TSDOBaseDataObject.isNull(const APath: string): Boolean;
var
  locCtx : TXPathExecContext;
begin
  locCtx := parsePropertyPath(APath);
  if ( locCtx.PropertyOwner = nil ) then
    raise ESDOInvalidPathException.Create(APath);
  case locCtx.ContentKind of
    xckNull    : Result := True;
    xckObject  : Result := False;
    xckList    :
      begin
        Result := locCtx.CurrentProperty.getType().isDataObjectType and ( locCtx.ListItem.getDataObject() = nil );
      end;
    xckValue : Result := locCtx.ObjectItem.isNull(locCtx.CurrentProperty);
    else
      Result := False;
  end;
end;

function TSDOBaseDataObject.isNull(const AProperty: ISDOProperty): Boolean;
var
  prp : ISDOPropertyEx;
begin
  if ( AProperty = nil ) or ( not Self.IsOwnerOf(AProperty) ) then
    raise ESDOIllegalArgumentException.Create('AProperty');
  if AProperty.isMany() then begin
    Result := ( getList(AProperty).size > 0 );
  end else begin
    prp := AProperty as ISDOPropertyEx;
    Result := getField(prp.getTypeEnum()).isNull(FBuffer,prp.getBufferOffset());
  end;
end;

function TSDOBaseDataObject.isNull(const APropertyIndex: PtrUInt): Boolean;
begin
  Result := isNull(getProperty(APropertyIndex));
end;

function TSDOBaseDataObject.isSet(const APropertyIndex: PtrUInt): Boolean;
begin
  Result := isSet(getProperty(APropertyIndex));
end;

function TSDOBaseDataObject.isSet(const APath: string): Boolean;
var
  locCtx : TXPathExecContext;
begin
  locCtx := parsePropertyPath(APath);
  if ( locCtx.PropertyOwner = nil ) then
    raise ESDOInvalidPathException.Create(APath);
  case locCtx.ContentKind of
    xckNull    : Result := not locCtx.PropertyOwner.getType().isDataObjectType();
    xckObject  : Result := locCtx.PropertyOwner.isSet(locCtx.CurrentProperty);
    xckList    : Result := ( locCtx.ListItem.size() > 0 );
    xckValue   : Result := locCtx.ObjectItem.isSet(locCtx.CurrentProperty);
    else
      Result := False;    
  end;
end;

function TSDOBaseDataObject.isSet(const AProperty: ISDOProperty): Boolean;
var
  prp : ISDOPropertyEx;
begin
  if ( AProperty = nil ) or ( not Self.IsOwnerOf(AProperty) ) then
    raise ESDOIllegalArgumentException.Create('AProperty');
  if AProperty.isMany() then begin
    Result := ( getList(AProperty).size > 0 );
  end else begin
    prp := AProperty as ISDOPropertyEx;
    Result := getField(prp.getTypeEnum()).isSet(FBuffer,prp.getBufferOffset());
  end;
end;

procedure TSDOBaseDataObject.setBoolean(const APropertyIndex: PtrUInt;const AValue: TSDOBoolean);
begin
  setBoolean(getProperty(APropertyIndex),AValue);
end;

procedure TSDOBaseDataObject.PrepareDataBuffer();
var
  prpCount : PtrInt;
  prpList : ISDOPropertyList;
  csPrp : ISDOPropertyEx;

  procedure InitializeListsBuffer();
  var
    k : PtrInt;
    locPrp : ISDOPropertyEx;
    locBuffer : PPSDODataObjectList;
  begin
    for k := 0 to Pred(prpCount) do begin
      locPrp := prpList.getItem(k) as ISDOPropertyEx;
      if locPrp.isMany() then begin
        locBuffer := PPSDODataObjectList( PtrUInt(FBuffer) + locPrp.getBufferOffset() );
        GetMem(locBuffer^,SizeOf(Pointer));
        FillChar(locBuffer^^,SizeOf(ISDODataObjectList),#0);
        locBuffer^^ := CreateList(locPrp);
      end;
    end;
  end;

  procedure InitChangeSummary();
  var
    locStore : ISDOChangedDataObjectList;
    locCS : ISDOChangeSummary;
    locCSX : ISDOChangeSummaryEx;
  begin
    locStore := TSDOChangedDataObjectList.Create() as ISDOChangedDataObjectList;
    locCSX := TSDOChangeSummary.Create(locStore) as ISDOChangeSummaryEx;//ISDOChangeSummaryEx
    FChangeSummary := Pointer(locCSX);
    locCS := locCSX as ISDOChangeSummary;
    getField(ChangeSummaryType).setChangeSummary(FBuffer,csPrp.getBufferOffset(),locCS);
  end;

var
  i, loc_fieldAddress : PtrUInt;
  prp : ISDOPropertyEx;
  prpType : ISDOTypeEx;
  prpStorageLength : PtrUInt;
  multiPrpCount : PtrInt;
begin
  csPrp := nil;
  prpList := getType().getProperties();
  prpCount := prpList.getCount();
  FBufferLength := 0;
  loc_fieldAddress := 0;
  multiPrpCount := 0;
  if ( prpCount > 0 ) then begin
    { TODO -oInoussa :
    The property BufferOffset  computation should be moved to the type
    definition and done _only once_ }
    for i := 0 to Pred(prpCount) do begin
      prp := prpList.getItem(i) as ISDOPropertyEx;
      if prp.isMany() then begin
        prpStorageLength := SizeOf(Pointer);
        Inc(multiPrpCount);
      end else begin
        prpType := prp.getType() as ISDOTypeEx;
        prpStorageLength := prpType.getFieldSize();
        if ( csPrp = nil ) and prpType.isChangeSummaryType() then
          csPrp := prp;
      end;
      Inc(FBufferLength,prpStorageLength);
      loc_fieldAddress := prp.setBufferOffset(loc_fieldAddress);
    end;
    GetMem(FBuffer,FBufferLength);
    FillChar(FBuffer^,FBufferLength,#0);
    if ( multiPrpCount > 0 ) then
      InitializeListsBuffer();
    if ( csPrp <> nil ) then
      InitChangeSummary();
  end;
end;

procedure TSDOBaseDataObject.setBoolean(
  const AProperty: ISDOProperty;
  const AValue: TSDOBoolean
);
var
  prp : ISDOPropertyEx;
begin
  if ( AProperty = nil ) or
     ( not Self.IsOwnerOf(AProperty) ) or
     ( AProperty.isMany() )
  then begin
    raise ESDOIllegalArgumentException.Create('AProperty');
  end;
  prp := AProperty as ISDOPropertyEx;
  RecordChange(AProperty);
  getField(prp.getTypeEnum()).setBoolean(FBuffer,prp.getBufferOffset(),AValue);
end;

procedure TSDOBaseDataObject.setBoolean(const APath: string; const AValue: TSDOBoolean);
var
  locCtx : TXPathExecContext;
begin
  locCtx := parsePropertyPath(APath);
  if ( locCtx.PropertyOwner = nil ) then
    raise ESDOInvalidPathException.Create(APath);
  if ( locCtx.ContentKind = xckList ) then
    locCtx.ListItem.setBoolean(AValue)
  else
    locCtx.PropertyOwner.setBoolean(locCtx.CurrentProperty,AValue);
end;

procedure TSDOBaseDataObject.setByte(
  const APath: string;
  const AValue: TSDOByte
);
var
  locCtx : TXPathExecContext;
begin
  locCtx := parsePropertyPath(APath);
  if ( locCtx.PropertyOwner = nil ) then
    raise ESDOInvalidPathException.Create(APath);
  if ( locCtx.ContentKind = xckList ) then
    locCtx.ListItem.setByte(AValue)
  else
    locCtx.PropertyOwner.setByte(locCtx.CurrentProperty,AValue);
end;

procedure TSDOBaseDataObject.setByte(
  const AProperty: ISDOProperty;
  const AValue: TSDOByte
);
var
  prp : ISDOPropertyEx;
begin
  if ( AProperty = nil ) or
     ( not Self.IsOwnerOf(AProperty) ) or
     ( AProperty.isMany() )
  then begin
    raise ESDOIllegalArgumentException.Create('AProperty');
  end;
  prp := AProperty as ISDOPropertyEx;
  RecordChange(AProperty);
  getField(prp.getTypeEnum()).setByte(FBuffer,prp.getBufferOffset(),AValue);
end;

procedure TSDOBaseDataObject.setByte(
  const APropertyIndex: PtrUInt;
  const AValue: TSDOByte
);
begin
  setByte(getProperty(APropertyIndex),AValue);
end;

{$IFDEF HAS_SDO_BYTES}
procedure TSDOBaseDataObject.setBytes(const AProperty: ISDOProperty; AValue: TSDOBytes);
var
  prp : ISDOPropertyEx;
begin
  if ( AProperty = nil ) or
     ( not Self.IsOwnerOf(AProperty) ) or
     ( AProperty.isMany() )
  then begin
    raise ESDOIllegalArgumentException.Create('AProperty');
  end;
  prp := AProperty as ISDOPropertyEx;
  RecordChange(AProperty);
  getField(prp.getTypeEnum()).setBytes(FBuffer,prp.getBufferOffset(),AValue);
end;

procedure TSDOBaseDataObject.setBytes(const APropertyIndex: PtrUInt; AValue : TSDOBytes);
begin
  setBytes(getProperty(APropertyIndex),AValue);
end;

procedure TSDOBaseDataObject.setBytes(const APath: string; AValue : TSDOBytes);
var
  locCtx : TXPathExecContext;
begin
  locCtx := parsePropertyPath(APath);
  if ( locCtx.PropertyOwner = nil ) then
    raise ESDOInvalidPathException.Create(APath);
  if ( locCtx.ContentKind = xckList ) then
    locCtx.ListItem.setBytes(AValue)
  else
    locCtx.PropertyOwner.setBytes(locCtx.CurrentProperty,AValue);
end;
{$ENDIF HAS_SDO_BYTES}

procedure TSDOBaseDataObject.setCharacter(const AProperty: ISDOProperty; const AValue: TSDOChar);
var
  prp : ISDOPropertyEx;
begin
  if ( AProperty = nil ) or
     ( not Self.IsOwnerOf(AProperty) ) or
     ( AProperty.isMany() )
  then begin
    raise ESDOIllegalArgumentException.Create('AProperty');
  end;
  prp := AProperty as ISDOPropertyEx;
  RecordChange(AProperty);
  getField(prp.getTypeEnum()).setCharacter(FBuffer,prp.getBufferOffset(),AValue);
end;

procedure TSDOBaseDataObject.setCharacter(const APropertyIndex: PtrUInt; const AValue: TSDOChar);
begin
  setCharacter(getProperty(APropertyIndex),AValue);
end;

procedure TSDOBaseDataObject.setCharacter(const APath: string; const AValue: TSDOChar);
var
  locCtx : TXPathExecContext;
begin
  locCtx := parsePropertyPath(APath);
  if ( locCtx.PropertyOwner = nil ) then
    raise ESDOInvalidPathException.Create(APath);
  if ( locCtx.ContentKind = xckList ) then
    locCtx.ListItem.setCharacter(AValue)
  else
    locCtx.PropertyOwner.setCharacter(locCtx.CurrentProperty,AValue);
end;

{$IFDEF HAS_SDO_CURRENCY}
procedure TSDOBaseDataObject.setCurrency(const AProperty: ISDOProperty; const AValue: TSDOCurrency);
var
  prp : ISDOPropertyEx;
begin
  if ( AProperty = nil ) or
     ( not Self.IsOwnerOf(AProperty) ) or
     ( AProperty.isMany() )
  then begin
    raise ESDOIllegalArgumentException.Create('AProperty');
  end;
  prp := AProperty as ISDOPropertyEx;
  RecordChange(AProperty);
  getField(prp.getTypeEnum()).setCurrency(FBuffer,prp.getBufferOffset(),AValue);
end;

procedure TSDOBaseDataObject.setCurrency(const APropertyIndex: PtrUInt; const AValue: TSDOCurrency);
begin
  setCurrency(getProperty(APropertyIndex),AValue);
end;

procedure TSDOBaseDataObject.setCurrency(const APath: string; const AValue: TSDOCurrency);
var
  locCtx : TXPathExecContext;
begin
  locCtx := parsePropertyPath(APath);
  if ( locCtx.PropertyOwner = nil ) then
    raise ESDOInvalidPathException.Create(APath);
  if ( locCtx.ContentKind = xckList ) then
    locCtx.ListItem.setCurrency(AValue)
  else
    locCtx.PropertyOwner.setCurrency(locCtx.CurrentProperty,AValue);
end;
{$ENDIF HAS_SDO_CURRENCY}

procedure TSDOBaseDataObject.setDataObject(
  const APath: string;
        AValue: ISDODataObject
);
var
  locCtx : TXPathExecContext;
begin
  locCtx := parsePropertyPath(APath);
  if ( locCtx.PropertyOwner = nil ) then
    raise ESDOInvalidPathException.Create(APath);
  if ( locCtx.ContentKind = xckList ) then
    locCtx.ListItem.setDataObject(AValue)
  else
    locCtx.PropertyOwner.setDataObject(locCtx.CurrentProperty,AValue);
end;

procedure TSDOBaseDataObject.setDataObject(
  const APropertyIndex: PtrUInt;
        AValue: ISDODataObject
);
begin
  setDataObject(getProperty(APropertyIndex),AValue);
end;

procedure TSDOBaseDataObject.setDataObject(
  const AProperty: ISDOProperty;
        AValue: ISDODataObject
);
var
  prp : ISDOPropertyEx;
  fld : ISDOField;
  oldObj, newObj : ISDODataObjectEx;
  off : PtrInt;
  selfIntf, oldContainer : ISDODataObject;
begin
  if ( AProperty = nil ) or ( not Self.IsOwnerOf(AProperty) ) then
    raise ESDOIllegalArgumentException.Create('AProperty');

  if AProperty.isMany() then begin
    getList(AProperty).setDataObject(AValue);
  end else begin
    prp := AProperty as ISDOPropertyEx;
    selfIntf := Self as ISDODataObject;
    if ( AValue <> nil ) then begin
      newObj := AValue as ISDODataObjectEx;
      if not newObj.IsInstanceOf(prp.getType()) then
        raise ESDOIllegalArgumentException.Create('AProperty');
      if prp.isContainment() then begin
        if newObj.IsAncestorOf(selfIntf) then
          raise ESDOCycleContainmentException.Create('AValue');
      end;
    end;

    fld := getField(prp.getTypeEnum());
    off := prp.getBufferOffset();

    oldObj := fld.getDataObject(FBuffer,off) as ISDODataObjectEx;
    if Assigned(oldObj) then begin
      if prp.isContainment() then
        oldObj.setContainer(nil,nil);
      if prp.isReference() then
        oldObj.RemoveReference(selfIntf,AProperty);
    end;

    RecordChange(AProperty);
    if ( AValue <> nil ) then begin
      if prp.isContainment() then begin
        oldContainer := newObj.getContainer();
        if Assigned(oldContainer) then
          oldContainer.setDataObject(newObj.getContainmentProperty(),nil);
      end;
      fld.setDataObject(FBuffer,off,AValue);
      if prp.isContainment() then
        newObj.setContainer(selfIntf,AProperty);
      if prp.isReference() then
        newObj.AddReference(selfIntf,AProperty);
    end else begin;
      fld.setDataObject(FBuffer,off,nil);
    end;
  end;
end;

procedure TSDOBaseDataObject.setDate(const AProperty: ISDOProperty; const AValue: TSDODate);
var
  prp : ISDOPropertyEx;
begin
  if ( AProperty = nil ) or
     ( not Self.IsOwnerOf(AProperty) ) or
     ( AProperty.isMany() )
  then begin
    raise ESDOIllegalArgumentException.Create('AProperty');
  end;
  prp := AProperty as ISDOPropertyEx;
  RecordChange(AProperty);
  getField(prp.getTypeEnum()).setDate(FBuffer,prp.getBufferOffset(),AValue);
end;

procedure TSDOBaseDataObject.setDate(const APropertyIndex: PtrUInt; const AValue: TSDODate);
begin
  setDate(getProperty(APropertyIndex),AValue);
end;

procedure TSDOBaseDataObject.setDate(const APath: string; const AValue: TSDODate);
var
  locCtx : TXPathExecContext;
begin
  locCtx := parsePropertyPath(APath);
  if ( locCtx.PropertyOwner = nil ) then
    raise ESDOInvalidPathException.Create(APath);
  if ( locCtx.ContentKind = xckList ) then
    locCtx.ListItem.setDate(AValue)
  else
    locCtx.PropertyOwner.setDate(locCtx.CurrentProperty,AValue);
end;

{$IFDEF HAS_SDO_DOUBLE}
procedure TSDOBaseDataObject.setDouble(const AProperty: ISDOProperty; const AValue: TSDODouble);
var
  prp : ISDOPropertyEx;
begin
  if ( AProperty = nil ) or
     ( not Self.IsOwnerOf(AProperty) ) or
     ( AProperty.isMany() )
  then begin
    raise ESDOIllegalArgumentException.Create('AProperty');
  end;
  prp := AProperty as ISDOPropertyEx;
  RecordChange(AProperty);
  getField(prp.getTypeEnum()).setDouble(FBuffer,prp.getBufferOffset(),AValue);
end;

procedure TSDOBaseDataObject.setDouble(const APropertyIndex: PtrUInt; const AValue: TSDODouble);
begin
  setDouble(getProperty(APropertyIndex),AValue);
end;

procedure TSDOBaseDataObject.setDouble(const APath: string; const AValue: TSDODouble);
var
  locCtx : TXPathExecContext;
begin
  locCtx := parsePropertyPath(APath);
  if ( locCtx.PropertyOwner = nil ) then
    raise ESDOInvalidPathException.Create(APath);
  if ( locCtx.ContentKind = xckList ) then
    locCtx.ListItem.setDouble(AValue)
  else
    locCtx.PropertyOwner.setDouble(locCtx.CurrentProperty,AValue);
end;
{$ENDIF HAS_SDO_DOUBLE}

{$IFDEF HAS_SDO_FLOAT}
procedure TSDOBaseDataObject.setFloat(const AProperty: ISDOProperty; const AValue: TSDOFloat);
var
  prp : ISDOPropertyEx;
begin
  if ( AProperty = nil ) or
     ( not Self.IsOwnerOf(AProperty) ) or
     ( AProperty.isMany() )
  then begin
    raise ESDOIllegalArgumentException.Create('AProperty');
  end;
  prp := AProperty as ISDOPropertyEx;
  RecordChange(AProperty);
  getField(prp.getTypeEnum()).setFloat(FBuffer,prp.getBufferOffset(),AValue);
end;

procedure TSDOBaseDataObject.setFloat(const APropertyIndex: PtrUInt; const AValue: TSDOFloat);
begin
  setFloat(getProperty(APropertyIndex),AValue);
end;

procedure TSDOBaseDataObject.setFloat(const APath: string; const AValue: TSDOFloat);
var
  locCtx : TXPathExecContext;
begin
  locCtx := parsePropertyPath(APath);
  if ( locCtx.PropertyOwner = nil ) then
    raise ESDOInvalidPathException.Create(APath);
  if ( locCtx.ContentKind = xckList ) then
    locCtx.ListItem.setFloat(AValue)
  else
    locCtx.PropertyOwner.setFloat(locCtx.CurrentProperty,AValue);
end;
{$ENDIF HAS_SDO_FLOAT}

procedure TSDOBaseDataObject.setInteger(
  const APath: string;
  const AValue: TSDOInteger
);
var
  locCtx : TXPathExecContext;
begin
  locCtx := parsePropertyPath(APath);
  if ( locCtx.PropertyOwner = nil ) then
    raise ESDOInvalidPathException.Create(APath);
  if ( locCtx.ContentKind = xckList ) then
    locCtx.ListItem.setInteger(AValue)
  else
    locCtx.PropertyOwner.setInteger(locCtx.CurrentProperty,AValue);
end;

procedure TSDOBaseDataObject.setInteger(
  const APropertyIndex: PtrUInt;
  const AValue: TSDOInteger
);
begin
  setInteger(getProperty(APropertyIndex),AValue);
end;

procedure TSDOBaseDataObject.setInteger(
  const AProperty: ISDOProperty;
  const AValue: TSDOInteger
);
var
  prp : ISDOPropertyEx;
begin
  if ( AProperty = nil ) or
     ( not Self.IsOwnerOf(AProperty) ) or
     ( AProperty.isMany() )
  then begin
    raise ESDOIllegalArgumentException.Create('AProperty');
  end;
  prp := AProperty as ISDOPropertyEx;
  RecordChange(AProperty);
  getField(prp.getTypeEnum()).setInteger(FBuffer,prp.getBufferOffset(),AValue);
end;

procedure TSDOBaseDataObject.setLong(const APath: string; const AValue: TSDOLong);
var
  locCtx : TXPathExecContext;
begin
  locCtx := parsePropertyPath(APath);
  if ( locCtx.PropertyOwner = nil ) then
    raise ESDOInvalidPathException.Create(APath);
  if ( locCtx.ContentKind = xckList ) then
    locCtx.ListItem.setLong(AValue)
  else
    locCtx.PropertyOwner.setLong(locCtx.CurrentProperty,AValue);
end;

procedure TSDOBaseDataObject.setLong(const APropertyIndex: PtrUInt; const AValue: TSDOLong);
begin
  setLong(getProperty(APropertyIndex),AValue);
end;

procedure TSDOBaseDataObject.setLong(const AProperty: ISDOProperty; const AValue: TSDOLong);
var
  prp : ISDOPropertyEx;
begin
  if ( AProperty = nil ) or
     ( not Self.IsOwnerOf(AProperty) ) or
     ( AProperty.isMany() )
  then begin
    raise ESDOIllegalArgumentException.Create('AProperty');
  end;
  prp := AProperty as ISDOPropertyEx;
  RecordChange(AProperty);
  getField(prp.getTypeEnum()).setLong(FBuffer,prp.getBufferOffset(),AValue);
end;

procedure TSDOBaseDataObject.setNull(const AProperty: ISDOProperty);
var
  prp : ISDOPropertyEx;
  oldObj : ISDODataObjectEx;
  selfIntf : ISDODataObject;
  fld : ISDOField;
  off : PtrInt;
begin
  if ( AProperty = nil ) or ( not Self.IsOwnerOf(AProperty) ) then
    raise ESDOIllegalArgumentException.Create('AProperty');
  RecordChange(AProperty);

  if AProperty.isMany() then begin
    ClearList(getList(AProperty));
  end else begin
    prp := AProperty as ISDOPropertyEx;
    fld := getField(prp.getTypeEnum());
    off := prp.getBufferOffset();
    if prp.getType().isDataObjectType() then begin
      oldObj := fld.getDataObject(FBuffer,off) as ISDODataObjectEx;
      if Assigned(oldObj) then begin
        if prp.isContainment() then
          oldObj.setContainer(nil,nil);
        if prp.isReference() then begin
          selfIntf := Self as ISDODataObject;
          oldObj.RemoveReference(selfIntf,AProperty);
        end;
      end;
    end;
    fld.setNull(FBuffer,off);
  end;
end;

procedure TSDOBaseDataObject.setNull(const APropertyIndex: PtrUInt);
begin
  setNull(getProperty(APropertyIndex));
end;

procedure TSDOBaseDataObject.setNull(const APath: string);
var
  locCtx : TXPathExecContext;
begin
  locCtx := parsePropertyPath(APath);
  if ( locCtx.PropertyOwner = nil ) then
    raise ESDOInvalidPathException.Create(APath);
  locCtx.PropertyOwner.setNull(locCtx.CurrentProperty);
end;

procedure TSDOBaseDataObject.setShort(const APath: string; const AValue: TSDOShort);
var
  locCtx : TXPathExecContext;
begin
  locCtx := parsePropertyPath(APath);
  if ( locCtx.PropertyOwner = nil ) then
    raise ESDOInvalidPathException.Create(APath);
  if ( locCtx.ContentKind = xckList ) then
    locCtx.ListItem.setShort(AValue)
  else
    locCtx.PropertyOwner.setShort(locCtx.CurrentProperty,AValue);
end;

procedure TSDOBaseDataObject.setShort(const APropertyIndex: PtrUInt; const AValue: TSDOShort);
begin
  setShort(getProperty(APropertyIndex),AValue);
end;

procedure TSDOBaseDataObject.setShort(const AProperty: ISDOProperty; const AValue: TSDOShort);
var
  prp : ISDOPropertyEx;
begin
  if ( AProperty = nil ) or
     ( not Self.IsOwnerOf(AProperty) ) or
     ( AProperty.isMany() )
  then begin
    raise ESDOIllegalArgumentException.Create('AProperty');
  end;
  prp := AProperty as ISDOPropertyEx;
  RecordChange(AProperty);
  getField(prp.getTypeEnum()).setShort(FBuffer,prp.getBufferOffset(),AValue);
end;

procedure TSDOBaseDataObject.setString(const APath: string; const AValue: TSDOString);
var
  locCtx : TXPathExecContext;
begin
  locCtx := parsePropertyPath(APath);
  if ( locCtx.PropertyOwner = nil ) then
    raise ESDOInvalidPathException.Create(APath);
  if ( locCtx.ContentKind = xckList ) then
    locCtx.ListItem.setString(AValue)
  else
    locCtx.PropertyOwner.setString(locCtx.CurrentProperty,AValue);
end;

procedure TSDOBaseDataObject.setString(
  const APropertyIndex: PtrUInt;
  const AValue: TSDOString
);
begin
  setString(getProperty(APropertyIndex),AValue);
end;

procedure TSDOBaseDataObject.setString(
  const AProperty: ISDOProperty;
  const AValue: TSDOString
);
var
  prp : ISDOPropertyEx;
begin
  if ( AProperty = nil ) or
     ( not Self.IsOwnerOf(AProperty) ) or
     ( AProperty.isMany() )
  then begin
    raise ESDOIllegalArgumentException.Create('AProperty');
  end;
  prp := AProperty as ISDOPropertyEx;
  RecordChange(AProperty);
  getField(prp.getTypeEnum()).setString(FBuffer,prp.getBufferOffset(),AValue);
end;

procedure TSDOBaseDataObject.unset(const APath: string);
var
  locCtx : TXPathExecContext;
begin
  locCtx := parsePropertyPath(APath);
  if ( locCtx.PropertyOwner = nil ) then
    raise ESDOInvalidPathException.Create(APath);
  locCtx.PropertyOwner.unset(locCtx.CurrentProperty);
end;

procedure TSDOBaseDataObject.unset(const APropertyIndex: PtrUInt);
begin
  unset(getProperty(APropertyIndex));
end;

procedure TSDOBaseDataObject.unset(const AProperty: ISDOProperty);
var
  prp : ISDOPropertyEx;
  fld : ISDOField;
  typeKind : TSDOTypeKind;
  off : PtrInt;
  oldObj : ISDODataObjectEx;
  selfIntf : ISDODataObject;
begin
  if ( AProperty = nil ) or ( not Self.IsOwnerOf(AProperty) ) then
    raise ESDOIllegalArgumentException.Create('AProperty');
  if AProperty.getType().isChangeSummaryType() then
    raise ESDOIllegalArgumentException.Create('AProperty');
  RecordChange(AProperty);
  if AProperty.isMany() then begin
    ClearList(getList(AProperty));
  end else begin
    prp := AProperty as ISDOPropertyEx;
    typeKind := prp.getTypeEnum();
    fld := getField(typeKind);
    off := prp.getBufferOffset();
    case typeKind of
      BooleanType   : fld.setBoolean(FBuffer,off,prp.getBooleanDefault());
      ByteType      : fld.setByte(FBuffer,off,prp.getByteDefault());
{$IFDEF HAS_SDO_BYTES}
      BytesType     : fld.setBytes(FBuffer,off,prp.getBytesDefault());
{$ENDIF HAS_SDO_BYTES}
{$IFDEF HAS_SDO_CHAR}
      CharacterType : fld.setCharacter(FBuffer,off,prp.getCharacterDefault());
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
      CurrencyType  : fld.setCurrency(FBuffer,off,prp.getCurrencyDefault());
{$ENDIF HAS_SDO_CURRENCY}
      DateTimeType  : fld.setDate(FBuffer,off,prp.getDateDefault());
{$IFDEF HAS_SDO_DOUBLE}
      DoubleType    : fld.setDouble(FBuffer,off,prp.getDoubleDefault());
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
      FloatType     : fld.setFloat(FBuffer,off,prp.getFloatDefault());
{$ENDIF HAS_SDO_FLOAT}
      IntegerType   : fld.setInteger(FBuffer,off,prp.getIntegerDefault());
{$IFDEF HAS_SDO_LONG}
      LongType      : fld.setLong(FBuffer,off,prp.getLongDefault());
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
      ShortType     : fld.setShort(FBuffer,off,prp.getShortDefault());
{$ENDIF HAS_SDO_SHORT}
      StringType    : fld.setString(FBuffer,off,prp.getStringDefault());
      ObjectType    :
        begin
          if prp.getType().isDataObjectType() then begin
            oldObj := fld.getDataObject(FBuffer,off) as ISDODataObjectEx;
            if Assigned(oldObj) then begin
              if prp.isContainment() then
                oldObj.setContainer(nil,nil);
              if prp.isReference() then begin
                selfIntf := Self as ISDODataObject;
                oldObj.RemoveReference(selfIntf,AProperty);
              end;
            end;
          end;
          fld.setDataObject(FBuffer,off,nil);
        end;
      else
        raise ESDOUnsupportedOperationException.Create('NOT-IMPLEMENTED-YET');
    end;
    fld.unset(FBuffer,off);
  end;
end;

procedure TSDOBaseDataObject.FreeBuffer();
var
  prpCount , i: PtrInt;
  prpList : ISDOPropertyList;
  prp : ISDOProperty;
  prpX : ISDOPropertyEx;
  lstObjRef : PPSDODataObjectList;
  obj, selfIntf : ISDODataObject;
  objX : ISDODataObjectEx;
  crs : ILinkedListCursor;
  lstObj : ISDODataObjectList;
begin
  if ( FBuffer <> nil ) then begin
    prpList := getInstanceProperties();
    prpCount := prpList.getCount();
    if ( prpCount > 0 ) then begin
      selfIntf := Self as ISDODataObject;
      for i := 0 to Pred(prpCount) do begin
        prp := prpList.getItem(i);
        prpX := prp as ISDOPropertyEx;
        if prpX.isMany() then begin
          lstObjRef := PPSDODataObjectList( PtrUInt(FBuffer) + prpX.getBufferOffset() );
          if prpX.getType().isDataObjectType() and prpX.isReference() then begin
            lstObj := lstObjRef^^;
            crs := lstObj.getCursor();
            crs.Reset();
            while crs.MoveNext() do begin
              obj := lstObj.getDataObject();
              if ( obj <> nil ) then begin
                objX := obj as ISDODataObjectEx;
                objX.RemoveReference(selfIntf,prp);
                objX := nil;
                obj := nil;
              end;
            end;
          end;
          lstObj := nil;
          lstObjRef^^ := nil;
          FreeMem(lstObjRef^,SizeOf(Pointer));
        end else begin
          if prpX.getType().isDataObjectType() and prpX.isReference() then begin
            obj := getField(prpX.getTypeEnum()).getDataObject(FBuffer,prpX.getBufferOffset());
            if ( obj <> nil ) then begin
              objX := obj as ISDODataObjectEx;
              objX.RemoveReference(selfIntf,prp);
              objX := nil;
              obj := nil;
            end;
          end;
          getField(prpX.getTypeEnum()).setNull(FBuffer,prpX.getBufferOffset());
        end;
      end;
      selfIntf := nil;
      FreeMem(FBuffer,FBufferLength);
    end;
    FBuffer := nil;
    FBufferLength := 0;
  end;
end;

function TSDOBaseDataObject.parsePropertyPath(const APath: string): TXPathExecContext;
var
  locSelf : ISDODataObject;
begin
  FXPathExpression.SetRoot(ParseXPath(APath));
  locSelf := Self as ISDODataObject;
  FXPathProcessor.Context.SetObject(locSelf,nil);
  FXPathProcessor.Execute(FXPathExpression);
  if ( FXPathProcessor.Context.CurrentProperty = nil ) then
    raise ESDOInvalidPathException.Create(APath);
  Result := FXPathProcessor.Context;
end;

procedure TSDOBaseDataObject.setContainer(
  const AContainer: ISDODataObject;
  const AContainerProperty: ISDOProperty
);
var
  locCS : ISDOChangeSummary;
  locCSx : ISDOChangeSummaryEx;
  locIsCreating : Boolean;
begin
  if Assigned(AContainerProperty) then begin
    if not Assigned(AContainer) then
      raise ESDOIllegalArgumentException.Create('AContainer');
    if not sdo_utils.InheritsFrom(AContainer.getType(), AContainerProperty.getContainingType()) then //if ( AContainerProperty.getContainingType() <> AContainer.getType() ) then
      raise ESDOIllegalArgumentException.Create('AContainerProperty');
  end;
  if Self.IsAncestorOf(AContainer) then
    raise ESDOCycleContainmentException.Create('AContainer');
  if ( AContainer = nil ) and ( FContainer <> nil ) then begin
    //deleting the current object
    {NotifyReferencersForDeletion();
    NotifyContainedObjectsForDeletion(False); }
    if not getContainmentProperty().isMany() then begin
      locCS := getChangeSummary();
      if ( locCS <> nil ) and locCS.isLogging() then begin
        locCSx := locCS as ISDOChangeSummaryEx;
        locCSx.getRecorder().recordDeletion(Self as ISDODataObject);
      end;
    end;
    NotifyReferencersForDeletion();
    NotifyContainedObjectsForDeletion(False);
    FChangeSummary := nil;
  end;
  locIsCreating := ( FContainer = nil ) and ( AContainer <> nil );
  FContainer := Pointer(AContainer);
  FContainerProperty := Pointer(AContainerProperty);
  if locIsCreating then begin
    //creating the current object
    if not AContainerProperty.isMany() then begin
      locCS := AContainer.getChangeSummary();
      if ( locCS <> nil ) and locCS.isLogging() then begin
        locCSx := locCS as ISDOChangeSummaryEx;
        locCSx.getRecorder().recordCreation(Self as ISDODataObject);
      end;
    end;
  end;
end;

function TSDOBaseDataObject.IsAncestorOf(const AObject: ISDODataObject): Boolean;
var
  locSelf, locObj : ISDODataObject;
begin
  locObj := AObject as ISDODataObject;
  locSelf := Self as ISDODataObject;
  while Assigned(locObj) and ( locObj <> locSelf ) do begin
    locObj := locObj.getContainer();
  end;
  Result := Assigned(AObject) and Assigned(locObj);
end;

function TSDOBaseDataObject.IsInstanceOf(const AType: ISDOType) : Boolean;
var
  locType : ISDOTypeEx;
begin
  locType := getType() as ISDOTypeEx;
  Result := locType.inherits(AType);
end;

function TSDOBaseDataObject.IsOwnerOf(const AProp: ISDOProperty): Boolean;
begin
  Result := Assigned(AProp) and
            ( ( ( AProp.getContainingType() as ISDOType ) = ( FType as ISDOType ) ) or
              FType.IsOwnerOf(AProp)
            );
end;

function TSDOBaseDataObject.CreateList(const AProperty : ISDOProperty) : ISDODataObjectList;
begin
  //Result := TSDODataObjectList.Create(AItemType) as ISDODataObjectList;
  Result := TSDOOwnedDataObjectList.Create(Self as ISDODataObject, AProperty) as ISDODataObjectList;
end;

function TSDOBaseDataObject.getList(const APath: string): ISDODataObjectList;
var
  locCtx : TXPathExecContext;
begin
  locCtx := parsePropertyPath(APath);
  if ( locCtx.PropertyOwner = nil ) then
    raise ESDOInvalidPathException.Create(APath);
  if ( locCtx.ContentKind <> xckList ) then
    raise ESDOIllegalArgumentException.Create('APath');
  Result := locCtx.ListItem;
end;

function TSDOBaseDataObject.getList(const APropertyIndex: PtrUInt): ISDODataObjectList;
begin
  Result := getList(getProperty(APropertyIndex));
end;

function TSDOBaseDataObject.getList(const AProperty: ISDOProperty): ISDODataObjectList;
var
  prp : ISDOPropertyEx;
begin
  if ( AProperty = nil ) or
     ( not Self.IsOwnerOf(AProperty) ) or
     ( not AProperty.isMany() )
  then begin
    raise ESDOUnsupportedOperationException.Create('getList');
  end;
  prp := AProperty as ISDOPropertyEx;
  Result := PPSDODataObjectList( PtrUInt(FBuffer) + prp.getBufferOffset() )^^;
end;

function TSDOBaseDataObject.getChangeSummary() : ISDOChangeSummary;
var
  locCS : ISDOChangeSummary;
  locCSX : ISDOChangeSummaryEx;
begin
  if ( FChangeSummary = nil ) then begin
    if ( FContainer <> nil ) then begin
      locCS := getContainer().getChangeSummary();
      if ( locCS <> nil ) then begin
        locCSX := locCS as ISDOChangeSummaryEx;
        FChangeSummary := Pointer(locCSX);
      end;
    end;
  end;
  Result := ISDOChangeSummaryEx(FChangeSummary) as ISDOChangeSummary;
end;

procedure TSDOBaseDataObject.RecordChange(const AProperty : ISDOProperty);
var
  locCS : ISDOChangeSummaryEx;
begin
  if ( FChangeSummary = nil ) then
    getChangeSummary();
  locCS := ISDOChangeSummaryEx(FChangeSummary);
  if Assigned(locCS) and locCS.isLogging() then begin
    locCS.getRecorder().recordChange(Self as ISDODataObject,AProperty);
  end;
end;

procedure TSDOBaseDataObject.NotifyDeletion(
  const ADataObject: ISDODataObject;
  const AProperty: ISDOProperty
);
var
  cs : ISDOChangeSummary;
begin
  cs := getChangeSummary();
  if ( ( cs = nil ) or ( not cs.isDeleted(Self as ISDODataObject) ) ) and
     ( getDataObject(AProperty) = ADataObject )
  then
    setDataObject(AProperty,nil);
end;

procedure TSDOBaseDataObject.AddReference(
  const AReferencer: ISDODataObject;
  const AReferenceProperty: ISDOProperty
);
begin
  if ( AReferencer = nil ) then
    raise ESDOIllegalArgumentException.Create('AReferencer');
  if ( AReferenceProperty = nil ) then
    raise ESDOIllegalArgumentException.Create('AReferenceProperty');
  if ( FReferenceLinkList = nil ) then
    FReferenceLinkList := TDataObjectObserverList.Create();
  FReferenceLinkList.Add(AReferencer,AReferenceProperty);
end;

procedure TSDOBaseDataObject.NotifyReferencersForDeletion();
var
  i, c : PtrInt;
  itm : TObserverInfo;
  oX : IDataObjectObserver;
  locSelf : ISDODataObject;
begin
  if ( FReferenceLinkList <> nil ) then begin
    c := FReferenceLinkList.GetCount();
    if ( c > 0 ) then begin
      locSelf := Self as ISDODataObject;
      itm := nil;
      try
        for i := Pred(c) downto 0 do begin
          itm := FReferenceLinkList.Extract(i);
          oX := itm.GetDataObject() as IDataObjectObserver;
          oX.NotifyDeletion(locSelf,itm.GetRefProperty());
          FreeAndNil(itm);
        end;
      finally
        FreeAndNil(itm);
      end;
    end;
  end;
end;

procedure TSDOBaseDataObject.RemoveReference(
  const AReferencer: ISDODataObject;
  const AReferenceProperty: ISDOProperty
);
var
  i : PtrInt;
begin
  if ( FReferenceLinkList <> nil ) then begin
    i := FReferenceLinkList.IndexOf(AReferencer,AReferenceProperty);
    if ( i > -1 ) then
      FReferenceLinkList.Delete(i);
  end;
end;

procedure TSDOBaseDataObject.NotifyContainedObjectsForDeletion(const ACallFromParent : Boolean);
var
  i, c : PtrInt;
  ls : ISDOPropertyList;
  prp : ISDOProperty;
  po : ISDODataObject;
  poX : ISDODataObjectEx;
  ols : ISDODataObjectList;
  ocrs : ILinkedListCursor;
  oldPos : TLinkedListBookmark;
begin
  if ACallFromParent then
    NotifyReferencersForDeletion();
  ls := getInstanceProperties();
  c := ls.getCount();
  if ( c > 0 ) then begin
    for i := 0 to Pred(c) do begin
      prp := ls.getItem(i);
      if prp.isContainment() and ( prp.getType().isDataObjectType ) then begin
        if prp.isMany() then begin
          ols := getList(prp);
          if ( ols.size() > 0 ) then begin
            ocrs := ols.getCursor();
            oldPos := ocrs.GetBookmark();
            try
              ocrs.Reset();
              while ocrs.MoveNext() do begin
                po := getDataObject(prp);
                if ( po <> nil ) then begin
                  poX := po as ISDODataObjectEx;
                  poX.NotifyContainedObjectsForDeletion(True);
                end;
              end;
            finally
              ocrs.GotoBookmark(oldPos);
            end;
          end;
        end else begin
          po := getDataObject(prp);
          if ( po <> nil ) then begin
            poX := po as ISDODataObjectEx;
            poX.NotifyContainedObjectsForDeletion(True);
          end;
        end;
      end;
    end;
  end;
end;

procedure TSDOBaseDataObject.InitializeDefaultValues();
var
  i, c : PtrInt;
  pls : ISDOPropertyList;
  p : ISDOPropertyEx;
  f : ISDOField;
begin
  pls := FType.getProperties();
  c := pls.getCount();
  if ( c > 0 ) then begin
    for i := 0 to Pred(c) do begin
      p := pls.getItem(i) as ISDOPropertyEx;
      if p.isDefaulted() then begin
        case p.getTypeEnum() of
          BooleanType    :
            begin
              f := getField(BooleanType);
              f.setBoolean(FBuffer,p.getBufferOffset(),p.getBooleanDefault());
              f.unset(FBuffer,p.getBufferOffset());
            end;
          ByteType    :
            begin
              f := getField(ByteType);
              f.setByte(FBuffer,p.getBufferOffset(),p.getByteDefault());
              f.unset(FBuffer,p.getBufferOffset());
            end;
{$IFDEF HAS_SDO_BYTES}
          BytesType :
            begin
              f := getField(BytesType);
              f.setBytes(FBuffer,p.getBufferOffset(),p.getBytesDefault());
              f.unset(FBuffer,p.getBufferOffset());
            end;
{$ENDIF HAS_SDO_BYTES}
{$IFDEF HAS_SDO_CHAR}
          CharacterType :
            begin
              f := getField(CharacterType);
              f.setCharacter(FBuffer,p.getBufferOffset(),p.getCharacterDefault());
              f.unset(FBuffer,p.getBufferOffset());
            end;
{$ENDIF HAS_SDO_CHAR}
          DateTimeType :
            begin
              f := getField(DateTimeType);
              f.setDate(FBuffer,p.getBufferOffset(),p.getDateDefault());
              f.unset(FBuffer,p.getBufferOffset());
            end;
{$IFDEF HAS_SDO_CURRENCY}
          CurrencyType :
            begin
              f := getField(CurrencyType);
              f.setCurrency(FBuffer,p.getBufferOffset(),p.getCurrencyDefault());
              f.unset(FBuffer,p.getBufferOffset());
            end;
{$ENDIF HAS_SDO_CURRENCY}
{$IFDEF HAS_SDO_DOUBLE}
          DoubleType :
            begin
              f := getField(DoubleType);
              f.setDouble(FBuffer,p.getBufferOffset(),p.getDoubleDefault());
              f.unset(FBuffer,p.getBufferOffset());
            end;
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
          FloatType :
            begin
              f := getField(FloatType);
              f.setFloat(FBuffer,p.getBufferOffset(),p.getFloatDefault());
              f.unset(FBuffer,p.getBufferOffset());
            end;
{$ENDIF HAS_SDO_FLOAT}
          IntegerType    :
            begin
              f := getField(IntegerType);
              f.setInteger(FBuffer,p.getBufferOffset(),p.getIntegerDefault());
              f.unset(FBuffer,p.getBufferOffset());
            end;
{$IFDEF HAS_SDO_LONG}
          LongType :
            begin
              f := getField(LongType);
              f.setLong(FBuffer,p.getBufferOffset(),p.getLongDefault());
              f.unset(FBuffer,p.getBufferOffset());
            end;
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
          ShortType :
            begin
              f := getField(ShortType);
              f.setShort(FBuffer,p.getBufferOffset(),p.getShortDefault());
              f.unset(FBuffer,p.getBufferOffset());
            end;
{$ENDIF HAS_SDO_SHORT}
          StringType  :
            begin
              f := getField(StringType);
              f.setString(FBuffer,p.getBufferOffset(),p.getStringDefault());
              f.unset(FBuffer,p.getBufferOffset());
            end;
          else
            raise ESDONotImplementedException.CreateFmt('NOT IMPLEMENTED for this type : "%s"',[SDOTypeDefaultTypeNames[p.getTypeEnum()]]);
        end;
      end;
    end;
  end;
end;

procedure TSDOBaseDataObject.addProperty(
  const APropName : string;
  const APropType : ISDOType;
  const AFlags    : TPropertyFlags
);
begin
  raise ESDOUnsupportedOperationException.Create('addProperty');
end;

function TSDOBaseDataObject._Release(): LongInt; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  if not FDestroying then
    Result := inherited _Release()
  else
    Result := 0;
end;

function TSDOBaseDataObject.getVariant(const AProperty: ISDOProperty): TSDOVariant;
var
  prp : ISDOPropertyEx;
begin
  if ( AProperty = nil ) or
     ( not Self.IsOwnerOf(AProperty) ) or
     ( AProperty.isMany() )
  then begin
    raise ESDOIllegalArgumentException.Create('AProperty');
  end;
  prp := AProperty as ISDOPropertyEx;
  Result := getField(prp.getTypeEnum()).getVariant(FBuffer,prp.getBufferOffset());
end;

function TSDOBaseDataObject.getVariant(const APropertyIndex: PtrUInt): TSDOVariant;
begin
  Result := getVariant(getProperty(APropertyIndex));
end;

function TSDOBaseDataObject.getVariant(const APath: string): TSDOVariant;
var
  locCtx : TXPathExecContext;
begin
  locCtx := parsePropertyPath(APath);
  if ( locCtx.PropertyOwner = nil ) then
    raise ESDOInvalidPathException.Create(APath);
  if ( locCtx.ContentKind = xckList ) then
    Result := locCtx.ListItem.getVariant()
  else
    Result := locCtx.PropertyOwner.getVariant(locCtx.CurrentProperty);
end;

procedure TSDOBaseDataObject.setVariant(const AProperty: ISDOProperty; const AValue: TSDOVariant);
var
  prp : ISDOPropertyEx;
begin
  if ( AProperty = nil ) or
     ( not Self.IsOwnerOf(AProperty) ) or
     ( AProperty.isMany() )
  then begin
    raise ESDOIllegalArgumentException.Create('AProperty');
  end;
  prp := AProperty as ISDOPropertyEx;
  RecordChange(AProperty);
  getField(prp.getTypeEnum()).setVariant(FBuffer,prp.getBufferOffset(),AValue);
end;

procedure TSDOBaseDataObject.setVariant(const APropertyIndex: PtrUInt; const AValue: TSDOVariant);
begin
  setVariant(getProperty(APropertyIndex),AValue);
end;

procedure TSDOBaseDataObject.setVariant(const APath: string; const AValue: TSDOVariant);
var
  locCtx : TXPathExecContext;
begin
  locCtx := parsePropertyPath(APath);
  if ( locCtx.PropertyOwner = nil ) then
    raise ESDOInvalidPathException.Create(APath);
  if ( locCtx.ContentKind = xckList ) then
    locCtx.ListItem.setVariant(AValue)
  else
    locCtx.PropertyOwner.setVariant(locCtx.CurrentProperty,AValue);
end;

{ TObserverInfo }

constructor TObserverInfo.Create(
  const ADataObject: ISDODataObject;
  const ARefProperty: ISDOProperty
);
begin
  FDataObject := Pointer(ADataObject);
  FRefProperty := Pointer(ARefProperty);
end;

function TObserverInfo.GetDataObject(): ISDODataObject;
begin
  Result := ISDODataObject(FDataObject);
end;

function TObserverInfo.GetRefProperty() : ISDOProperty;
begin
  Result := ISDOProperty(FRefProperty);
end;

{ TDataObjectObserverList }

procedure TDataObjectObserverList.Add(
  const ADataObject : ISDODataObject;
  const ARefProperty : ISDOProperty
);
begin
  if ( Find(ADataObject,ARefProperty) = nil ) then
    FList.Add(TObserverInfo.Create(ADataObject,ARefProperty));
end;

constructor TDataObjectObserverList.Create();
begin
  FList := TObjectList.Create(True);
end;

procedure TDataObjectObserverList.Delete(const AIndex: PtrInt);
begin
  FList.Delete(AIndex);
end;

destructor TDataObjectObserverList.Destroy();
begin
  FreeAndNil(FList);
  inherited;
end;

function TDataObjectObserverList.Extract(const AIndex: PtrInt): TObserverInfo;
begin
  Result := TObserverInfo(FList.Extract(GetItem(AIndex)));
end;

function TDataObjectObserverList.Find(
  const ADataObject : ISDODataObject;
  const ARefProperty : ISDOProperty
) : TObserverInfo;
var
  i : PtrInt;
begin
  i := IndexOf(ADataObject,ARefProperty);
  if ( i > -1  ) then
    Result := GetItem(i)
  else
    Result := nil;
end;

function TDataObjectObserverList.GetCount() : PtrInt;
begin
  Result := FList.Count;
end;

function TDataObjectObserverList.GetItem(const AIndex: PtrInt): TObserverInfo;
begin
  if ( AIndex < 0 ) or ( AIndex >= GetCount() ) then
    raise ESDOIndexOutOfRangeException.Create(AIndex);
  Result := TObserverInfo(FList[AIndex]);
end;

function TDataObjectObserverList.IndexOf(
  const ADataObject: ISDODataObject;
  const ARefProperty: ISDOProperty
) : PtrInt;
var
  i, c : PtrInt;
  locInfo : TObserverInfo;
  locObj : ISDODataObject;
  locP : ISDOProperty;
begin
  Result := -1;
  c := FList.Count;
  for i := 0 to Pred(c) do begin
    locInfo := TObserverInfo(FList[i]);
    locObj := locInfo.GetDataObject();
    locP := locInfo.GetRefProperty();
    if ( locObj = ADataObject ) and ( locP = ARefProperty ) then begin
      Result := i;
      Break;
    end;
  end;
end;

{ TSDOOpenedDataObject }

procedure TSDOOpenedDataObject.addProperty(
  const APropName: string;
  const APropType: ISDOType;
  const AFlags: TPropertyFlags
);
var
  p : ISDOPropertyEx;
  pt : ISDOTypeEx;
  fs, oldLength : PtrUInt;
  locBuffer : PPSDODataObjectList;
begin
  if ( FInstanceProperties.find(APropName) <> nil ) then
    raise ESDODuplicatedItemException.Create(APropName);
  p := TSDOProperty.Create(
         APropName,
         APropType,
         AFlags,
         getType()
       ) as ISDOPropertyEx;
  oldLength := FBufferLength;
  p.setBufferOffset(oldLength);
  pt := p.getType() as ISDOTypeEx;
  if p.isMany() then
    fs := SizeOf(Pointer)
  else
    fs := pt.getFieldSize();
  ReallocMem(FBuffer, ( oldLength + fs ));
  FBufferLength := oldLength + fs;
  FillChar(
    Pointer(PtrUInt(FBuffer) + oldLength)^,
    fs,
    #0
  );
  if p.isMany() then begin
    locBuffer := PPSDODataObjectList( PtrUInt(FBuffer) + p.getBufferOffset() );
    GetMem(locBuffer^,SizeOf(Pointer));
    FillChar(locBuffer^^,SizeOf(ISDODataObjectList),#0);
    locBuffer^^ := CreateList(p);
  end;
  FInstanceProperties.add((p as ISDOProperty));
end;

constructor TSDOOpenedDataObject.Create(
  const AType: ISDOTypeEx;
  const AContainer: ISDODataObject;
  const AContainerProperty: ISDOProperty
);
begin
  if not AType.isOpenType() then
    raise ESDOIllegalArgumentException.Create('AType');
  inherited;
  FInstanceProperties := TSDOPropertyList.Create(AType.getProperties());
end;

function TSDOOpenedDataObject.getInstanceProperties() : ISDOPropertyList;
begin
  Result := FInstanceProperties as ISDOPropertyList;
end;

function TSDOOpenedDataObject.IsOwnerOf(const AProp: ISDOProperty): Boolean;
var
  i, c : PtrInt;
  locProp : ISDOProperty;
begin
  Result := False;
  if ( AProp <> nil ) then begin
    c := FInstanceProperties.getCount();
    if ( c > 0 ) then begin
      locProp := AProp as ISDOProperty;
      for i := 0 to ( c - 1 ) do begin
        if ( locProp = FInstanceProperties.getItem(i) ) then begin
          Result := True;
          Break;
        end;
      end;
    end;
  end;
end;

procedure TSDOOpenedDataObject.setBoolean(const APath: string; const AValue: TSDOBoolean);
begin
  try
    inherited;
  except
    on e : ESDOPropertyNotFoundException do begin
      if ( Pos('/', APath) = 0 ) and ( not IsStrEmpty(APath) ) then begin
        addProperty(Trim(APath),FType.getOwner().getType(sdo_namespace,SDOTypeDefaultTypeNames[BooleanType]),[pfIsAttribute]);
        inherited;
      end else begin
        raise;
      end;
    end;
  end;
end;

procedure TSDOOpenedDataObject.setByte(const APath: string; const AValue: TSDOByte);
begin
  try
    inherited;
  except
    on e : ESDOPropertyNotFoundException do begin
      if ( Pos('/', APath) = 0 ) and ( not IsStrEmpty(APath) ) then begin
        addProperty(Trim(APath),FType.getOwner().getType(sdo_namespace,SDOTypeDefaultTypeNames[ByteType]),[pfIsAttribute]);
        inherited;
      end else begin
        raise;
      end;
    end;
  end;
end;

{$IFDEF HAS_SDO_BYTES}
procedure TSDOOpenedDataObject.setBytes(const APath: string; AValue : TSDOBytes);
begin
  try
    inherited;
  except
    on e : ESDOPropertyNotFoundException do begin
      if ( Pos('/', APath) = 0 ) and ( not IsStrEmpty(APath) ) then begin
        addProperty(Trim(APath),FType.getOwner().getType(sdo_namespace,SDOTypeDefaultTypeNames[BytesType]),[pfIsAttribute]);
        inherited;
      end else begin
        raise;
      end;
    end;
  end;
end;
{$ENDIF HAS_SDO_BYTES}

{$IFDEF HAS_SDO_CHAR}
procedure TSDOOpenedDataObject.setCharacter(const APath: string; const AValue: TSDOChar);
begin
  try
    inherited;
  except
    on e : ESDOPropertyNotFoundException do begin
      if ( Pos('/', APath) = 0 ) and ( not IsStrEmpty(APath) ) then begin
        addProperty(Trim(APath),FType.getOwner().getType(sdo_namespace,SDOTypeDefaultTypeNames[CharacterType]),[pfIsAttribute]);
        inherited;
      end else begin
        raise;
      end;
    end;
  end;
end;
{$ENDIF HAS_SDO_CHAR}

{$IFDEF HAS_SDO_CURRENCY}
procedure TSDOOpenedDataObject.setCurrency(const APath: string; const AValue: Currency);
begin
  try
    inherited;
  except
    on e : ESDOPropertyNotFoundException do begin
      if ( Pos('/', APath) = 0 ) and ( not IsStrEmpty(APath) ) then begin
        addProperty(Trim(APath),FType.getOwner().getType(sdo_namespace,SDOTypeDefaultTypeNames[CurrencyType]),[pfIsAttribute]);
        inherited;
      end else begin
        raise;
      end;
    end;
  end;
end;
{$ENDIF HAS_SDO_CURRENCY}

procedure TSDOOpenedDataObject.setDataObject(const APath: string; AValue: ISDODataObject);
var
  pf : TPropertyFlags;
begin
  try
    inherited;
  except
    on e : ESDOPropertyNotFoundException do begin
      if ( Pos('/', APath) = 0 ) and ( not IsStrEmpty(APath) ) then begin
        if ( AValue = nil ) then
          raise;
        pf := [];
        if ( AValue.getContainer() = nil ) then
          Include(pf,pfIsContainment);
        addProperty(Trim(APath),AValue.getType(),pf);
        inherited;
      end else begin
        raise;
      end;
    end;
  end;
end;

procedure TSDOOpenedDataObject.setDate(const APath: string; const AValue: TSDODate);
begin
  try
    inherited;
  except
    on e : ESDOPropertyNotFoundException do begin
      if ( Pos('/', APath) = 0 ) and ( not IsStrEmpty(APath) ) then begin
        addProperty(Trim(APath),FType.getOwner().getType(sdo_namespace,SDOTypeDefaultTypeNames[DateType]),[pfIsAttribute]);
        inherited;
      end else begin
        raise;
      end;
    end;
  end;
end;

{$IFDEF HAS_SDO_DOUBLE}
procedure TSDOOpenedDataObject.setDouble(const APath: string; const AValue: Double);
begin
  try
    inherited;
  except
    on e : ESDOPropertyNotFoundException do begin
      if ( Pos('/', APath) = 0 ) and ( not IsStrEmpty(APath) ) then begin
        addProperty(Trim(APath),FType.getOwner().getType(sdo_namespace,SDOTypeDefaultTypeNames[DoubleType]),[pfIsAttribute]);
        inherited;
      end else begin
        raise;
      end;
    end;
  end;
end;
{$ENDIF HAS_SDO_DOUBLE}

{$IFDEF HAS_SDO_FLOAT}
procedure TSDOOpenedDataObject.setFloat(const APath: string; const AValue: TSDOFloat);
begin
  try
    inherited;
  except
    on e : ESDOPropertyNotFoundException do begin
      if ( Pos('/', APath) = 0 ) and ( not IsStrEmpty(APath) ) then begin
        addProperty(Trim(APath),FType.getOwner().getType(sdo_namespace,SDOTypeDefaultTypeNames[FloatType]),[pfIsAttribute]);
        inherited;
      end else begin
        raise;
      end;
    end;
  end;
end;
{$ENDIF HAS_SDO_FLOAT}

procedure TSDOOpenedDataObject.setInteger(const APath: string; const AValue: TSDOInteger);
begin
  try
    inherited;
  except
    on e : ESDOPropertyNotFoundException do begin
      if ( Pos('/', APath) = 0 ) and ( not IsStrEmpty(APath) ) then begin
        addProperty(Trim(APath),FType.getOwner().getType(sdo_namespace,SDOTypeDefaultTypeNames[IntegerType]),[pfIsAttribute]);
        inherited;
      end else begin
        raise;
      end;
    end;
  end;
end;

{$IFDEF HAS_SDO_LONG}
procedure TSDOOpenedDataObject.setLong(const APath: string; const AValue: TSDOLong);
begin
  try
    inherited;
  except
    on e : ESDOPropertyNotFoundException do begin
      if ( Pos('/', APath) = 0 ) and ( not IsStrEmpty(APath) ) then begin
        addProperty(Trim(APath),FType.getOwner().getType(sdo_namespace,SDOTypeDefaultTypeNames[LongType]),[pfIsAttribute]);
        inherited;
      end else begin
        raise;
      end;
    end;
  end;
end;
{$ENDIF HAS_SDO_LONG}

{$IFDEF HAS_SDO_SHORT}
procedure TSDOOpenedDataObject.setShort(const APath: string; const AValue: TSDOShort);
begin
  try
    inherited;
  except
    on e : ESDOPropertyNotFoundException do begin
      if ( Pos('/', APath) = 0 ) and ( not IsStrEmpty(APath) ) then begin
        addProperty(Trim(APath),FType.getOwner().getType(sdo_namespace,SDOTypeDefaultTypeNames[ShortType]),[pfIsAttribute]);
        inherited;
      end else begin
        raise;
      end;
    end;
  end;
end;
{$ENDIF HAS_SDO_SHORT}

procedure TSDOOpenedDataObject.setString(const APath: string; const AValue: TSDOString);
begin
  try
    inherited;
  except
    on e : ESDOPropertyNotFoundException do begin
      if ( Pos('/', APath) = 0 ) and ( not IsStrEmpty(APath) ) then begin
        addProperty(Trim(APath),FType.getOwner().getType(sdo_namespace,SDOTypeDefaultTypeNames[StringType]),[pfIsAttribute]);
        inherited;
      end else begin
        raise;
      end;
    end;
  end;
end;

const
  data_offset = 0;            { TSDODataObjectList }

procedure TSDODataObjectList.append(const AValue: TSDOBoolean);
begin
  FField.setBoolean(GetData(InternalAppend()),0,AValue);
end;

procedure TSDODataObjectList.append(const AValue: TSDOInteger);
begin
  FField.setInteger(GetData(InternalAppend()),0,AValue);
end;

procedure TSDODataObjectList.append(const AValue: TSDOString);
begin
  FField.setString(GetData(InternalAppend()),0,AValue);
end;

procedure TSDODataObjectList.Clear();
var
  i : PtrInt;
begin
  if Assigned(FData) then begin
    for i := 0 to Pred(FData.GetLength()) do begin
      InternalDelete(FData.GetFirst());
    end;
  end;
end;

constructor TSDODataObjectList.Create(const AItemType: ISDOType);
var
  typEx : ISDOTypeEx;
begin
  if ( AItemType = nil ) or ( not Supports(AItemType,ISDOTypeEx,typEx) ) then
    raise ESDOIllegalArgumentException.Create('AItemType');
  FItemType := AItemType;
  FField := getField(FItemType.getTypeEnum());
  FData := TDoubleLinkedList.Create(typEx.getFieldSize());
  FCursor := CreateIterator(FData);
end;

destructor TSDODataObjectList.Destroy();
begin
  Clear();
  FCursor := nil;
  FItemType := nil;
  FreeAndNil(FData);
  inherited;
end;

function TSDODataObjectList.getBoolean(const AIndex: PtrInt): TSDOBoolean;
begin
  MoveTo(AIndex);
  Result := getBoolean();
end;

function TSDODataObjectList.getInteger(const AIndex: PtrInt): TSDOInteger;
begin
  MoveTo(AIndex);
  Result := getInteger();
end;

function TSDODataObjectList.getString(const AIndex: PtrInt): TSDOString;
begin
  MoveTo(AIndex);
  Result := getString();
end;

procedure TSDODataObjectList.delete(const AIndex: PtrInt);
begin
  MoveTo(AIndex);
  delete();
end;

procedure TSDODataObjectList.setBoolean(const AIndex: PtrInt; const AValue: TSDOBoolean);
begin
  MoveTo(AIndex);
  setBoolean(AValue);
end;

procedure TSDODataObjectList.setInteger(const AIndex: PtrInt; const AValue: TSDOInteger);
begin
  MoveTo(AIndex);
  setInteger(AValue);
end;

procedure TSDODataObjectList.setString(const AIndex: PtrInt; const AValue: TSDOString);
begin
  MoveTo(AIndex);
  setString(AValue);
end;

function TSDODataObjectList.size() : PtrInt;
begin
  Result := FData.GetLength();
end;

procedure TSDODataObjectList.InternalDelete(const AData: PLinkedNode);
begin
  FField.setNull(GetData(AData),0);
  FData.Delete(AData);
end;

function TSDODataObjectList.getCursor() : ILinkedListCursor;
begin
  Result := FCursor;
end;

function TSDODataObjectList.getBoolean() : TSDOBoolean;
begin
  CheckCursorPosition();
  Result := FField.getBoolean(GetData(FCursor.GetCurrent()),0);
end;

function TSDODataObjectList.getInteger() : TSDOInteger;
begin
  CheckCursorPosition();
  Result := FField.getInteger(GetData(FCursor.GetCurrent()),0);
end;

function TSDODataObjectList.getString() : TSDOString;
begin
  CheckCursorPosition();
  Result := FField.getString(GetData(FCursor.GetCurrent()),0);
end;

function TSDODataObjectList.InternalAppend() : PLinkedNode;
begin
  Result := FData.Append();
  FCursor.MoveLast();
end;

procedure TSDODataObjectList.CheckCursorPosition();
begin
  if not FCursor.IsPosValid() then
    raise ESDOIllegalArgumentException.Create('Cursor');
end;

procedure TSDODataObjectList.setBoolean(const AValue: TSDOBoolean);
begin
  CheckCursorPosition();
  FField.setBoolean(GetData(FCursor.GetCurrent()),0,AValue);
end;

procedure TSDODataObjectList.setInteger(const AValue: TSDOInteger);
begin
  CheckCursorPosition();
  FField.setInteger(GetData(FCursor.GetCurrent()),0,AValue);
end;

procedure TSDODataObjectList.setString(const AValue: TSDOString);
begin
  CheckCursorPosition();
  FField.setString(GetData(FCursor.GetCurrent()),0,AValue);
end;

procedure TSDODataObjectList.delete();
var
  p : PLinkedNode;
begin
  CheckCursorPosition();
  p := FCursor.GetCurrent();
  if FCursor.Bof() then
    FCursor.MoveNext()
  else if FCursor.Eof() then
    FCursor.MovePrevious();
  InternalDelete(p);
end;

{procedure TSDODataObjectList.MoveTo(const AIndex: PtrInt);
var
  c , j : PtrInt;
begin
  c := FData.GetLength();
  if ( AIndex < 0 ) or ( AIndex >= c ) then
    raise ESDOIndexOutOfRangeException.Create(AIndex);
  j := AIndex;
  if ( j <= ( Pred(c) div 2 ) ) then begin
    if FCursor.MoveFirst() then begin
      while ( j > 0 ) and FCursor.MoveNext() do begin
        Dec(j);
      end;
    end;
  end else begin
    if FCursor.MoveLast() then begin
      j := Pred(c) - j;
      while ( j > 0 ) and FCursor.MovePrevious() do begin
        Dec(j);
      end;
    end;
  end;
  if ( j > 0 ) then
    raise ESDOIndexOutOfRangeException.Create(j);
end; }
procedure TSDODataObjectList.MoveTo(const AIndex: PtrInt);
begin
  if not FCursor.MoveTo(AIndex) then
    raise ESDOIndexOutOfRangeException.Create(AIndex);
end;

procedure TSDODataObjectList.append(AValue: ISDODataObject);
var
  newObj : ISDODataObjectEx;
begin
  if ( AValue <> nil ) then begin
    newObj := AValue as ISDODataObjectEx;
    if not newObj.IsInstanceOf(FItemType) then
      raise ESDOIllegalArgumentException.Create('AProperty');
  end;
  FField.setDataObject(GetData(InternalAppend()),0,AValue);
end;

function TSDODataObjectList.getDataObject(const AIndex: PtrInt): ISDODataObject;
begin
  MoveTo(AIndex);
  Result := getDataObject();
end;

function TSDODataObjectList.getDataObject() : ISDODataObject;
begin
  CheckCursorPosition();
  Result := FField.getDataObject(GetData(FCursor.GetCurrent()),0);
end;

procedure TSDODataObjectList.setDataObject(AValue: ISDODataObject);
begin
  CheckCursorPosition();
  FField.setDataObject(GetData(FCursor.GetCurrent()),0,AValue);
end;

procedure TSDODataObjectList.setDataObject(const AIndex: PtrInt; AValue: ISDODataObject);
begin
  MoveTo(AIndex);
  setDataObject(AValue);
end;

procedure TSDODataObjectList.insert(const AIndex: PtrInt; const AValue: TSDOInteger);
begin
  if ( AIndex = 0 ) then begin
    FField.setInteger(GetData(FData.InsertFirst()),0,AValue);
  end else begin
    if ( AIndex = FData.GetLength() ) then begin
      append(AValue);
    end else begin
      MoveTo(AIndex);
      FField.setInteger(GetData(FData.InsertBefore(FCursor.GetCurrent())),0,AValue);
    end;
  end;
  MoveTo(AIndex);
end;

procedure TSDODataObjectList.insert(const AIndex: PtrInt; const AValue: TSDOBoolean);
begin
  if ( AIndex = 0 ) then begin
    FField.setBoolean(GetData(FData.InsertFirst()),0,AValue);
  end else begin
    if ( AIndex = FData.GetLength() ) then begin
      append(AValue);
    end else begin
      MoveTo(AIndex);
      FField.setBoolean(GetData(FData.InsertBefore(FCursor.GetCurrent())),0,AValue);
    end;
  end;
  MoveTo(AIndex);
end;

procedure TSDODataObjectList.insert(const AIndex: PtrInt; AValue: ISDODataObject);
begin
  if ( AIndex = 0 ) then begin
    FField.setDataObject(GetData(FData.InsertFirst()),0,AValue);
  end else begin
    if ( AIndex = FData.GetLength() ) then begin
      append(AValue);
    end else begin
      MoveTo(AIndex);
      FField.setDataObject(GetData(FData.InsertBefore(FCursor.GetCurrent())),0,AValue);
    end;
  end;
  MoveTo(AIndex);
end;

procedure TSDODataObjectList.insert(const AIndex: PtrInt; const AValue: TSDOString);
begin
  if ( AIndex = 0 ) then begin
    FField.setString(GetData(FData.InsertFirst()),0,AValue);
  end else begin
    if ( AIndex = FData.GetLength() ) then begin
      append(AValue);
    end else begin
      MoveTo(AIndex);
      FField.setString(GetData(FData.InsertBefore(FCursor.GetCurrent())),0,AValue);
    end;
  end;
  MoveTo(AIndex);
end;

function TSDODataObjectList.getItemType() : ISDOType;
begin
  Result := FItemType;
end;

procedure TSDODataObjectList.append(const AValue: TSDOByte);
begin
  FField.setByte(GetData(InternalAppend()),0,AValue);
end;

function TSDODataObjectList.getByte() : TSDOByte;
begin
  CheckCursorPosition();
  Result := FField.getByte(GetData(FCursor.GetCurrent()),0);
end;

{$IFDEF HAS_SDO_BYTES}
function TSDODataObjectList.getBytes() : TSDOBytes;
begin
  CheckCursorPosition();
  Result := FField.getBytes(GetData(FCursor.GetCurrent()),0);
end;

function TSDODataObjectList.getBytes(const AIndex: PtrInt): TSDOBytes;
begin
  MoveTo(AIndex);
  Result := getBytes();
end;

procedure TSDODataObjectList.setBytes(AValue: TSDOBytes);
begin
  CheckCursorPosition();
  FField.setBytes(GetData(FCursor.GetCurrent()),0,AValue);
end;

procedure TSDODataObjectList.setBytes(const AIndex: PtrInt; AValue: TSDOBytes);
begin
  MoveTo(AIndex);
  setBytes(AValue);
end;

procedure TSDODataObjectList.insertBytes(const AIndex: PtrInt; AValue: TSDOBytes);
begin
  if ( AIndex = 0 ) then begin
    FField.setBytes(GetData(FData.InsertFirst()),0,AValue);
  end else begin
    if ( AIndex = FData.GetLength() ) then begin
      appendBytes(AValue);
    end else begin
      MoveTo(AIndex);
      FField.setBytes(GetData(FData.InsertBefore(FCursor.GetCurrent())),0,AValue);
    end;
  end;
  MoveTo(AIndex);
end;

procedure TSDODataObjectList.appendBytes(AValue: TSDOBytes);
begin
  FField.setBytes(GetData(InternalAppend()),0,AValue);
end;
{$ENDIF HAS_SDO_BYTES}

procedure TSDODataObjectList.setByte(const AValue: TSDOByte);
begin
  CheckCursorPosition();
  FField.setByte(GetData(FCursor.GetCurrent()),0,AValue);
end;

function TSDODataObjectList.getByte(const AIndex: PtrInt): TSDOByte;
begin
  MoveTo(AIndex);
  Result := getByte();
end;

procedure TSDODataObjectList.setByte(const AIndex: PtrInt; const AValue: TSDOByte);
begin
  MoveTo(AIndex);
  setByte(AValue);
end;

procedure TSDODataObjectList.insert(const AIndex: PtrInt; const AValue: TSDOByte);
begin
  if ( AIndex = 0 ) then begin
    FField.setByte(GetData(FData.InsertFirst()),0,AValue);
  end else begin
    if ( AIndex = FData.GetLength() ) then begin
      append(AValue);
    end else begin
      MoveTo(AIndex);
      FField.setByte(GetData(FData.InsertBefore(FCursor.GetCurrent())),0,AValue);
    end;
  end;
  MoveTo(AIndex);
end;

{$IFDEF HAS_SDO_CHAR}
procedure TSDODataObjectList.append(const AValue: TSDOChar);
begin
  FField.setCharacter(GetData(InternalAppend()),0,AValue);
end;

function TSDODataObjectList.getCharacter() : TSDOChar;
begin
  CheckCursorPosition();
  Result := FField.getCharacter(GetData(FCursor.GetCurrent()),0);
end;

procedure TSDODataObjectList.setCharacter(const AValue: TSDOChar);
begin
  CheckCursorPosition();
  FField.setCharacter(GetData(FCursor.GetCurrent()),0,AValue);
end;

function TSDODataObjectList.getCharacter(const AIndex: PtrInt): TSDOChar;
begin
  MoveTo(AIndex);
  Result := getCharacter();
end;

procedure TSDODataObjectList.setCharacter(const AIndex: PtrInt; const AValue: TSDOChar);
begin
  MoveTo(AIndex);
  setCharacter(AValue);
end;

procedure TSDODataObjectList.insert(const AIndex: PtrInt; const AValue: TSDOChar);
begin
  if ( AIndex = 0 ) then begin
    FField.setCharacter(GetData(FData.InsertFirst()),0,AValue);
  end else begin
    if ( AIndex = FData.GetLength() ) then begin
      append(AValue);
    end else begin
      MoveTo(AIndex);
      FField.setCharacter(GetData(FData.InsertBefore(FCursor.GetCurrent())),0,AValue);
    end;
  end;
  MoveTo(AIndex);
end;
{$ENDIF HAS_SDO_CHAR}

{$IFDEF HAS_SDO_CURRENCY}
procedure TSDODataObjectList.appendCurrency(const AValue: TSDOCurrency);
begin
  FField.setCurrency(GetData(InternalAppend()),0,AValue);
end;

function TSDODataObjectList.getCurrency() : TSDOCurrency;
begin
  CheckCursorPosition();
  Result := FField.getCurrency(GetData(FCursor.GetCurrent()),0);
end;

procedure TSDODataObjectList.setCurrency(const AValue: TSDOCurrency);
begin
  CheckCursorPosition();
  FField.setCurrency(GetData(FCursor.GetCurrent()),0,AValue);
end;

function TSDODataObjectList.getCurrency(const AIndex: PtrInt): TSDOCurrency;
begin
  MoveTo(AIndex);
  Result := getCurrency();
end;

procedure TSDODataObjectList.setCurrency(const AIndex: PtrInt; const AValue: TSDOCurrency);
begin
  MoveTo(AIndex);
  setCurrency(AValue);
end;

procedure TSDODataObjectList.insertCurrency(const AIndex: PtrInt; const AValue: TSDOCurrency);
begin
  if ( AIndex = 0 ) then begin
    FField.setCurrency(GetData(FData.InsertFirst()),0,AValue);
  end else begin
    if ( AIndex = FData.GetLength() ) then begin
      appendCurrency(AValue);
    end else begin
      MoveTo(AIndex);
      FField.setCurrency(GetData(FData.InsertBefore(FCursor.GetCurrent())),0,AValue);
    end;
  end;
  MoveTo(AIndex);
end;
{$ENDIF HAS_SDO_CURRENCY}

{$IFDEF HAS_SDO_DOUBLE}
procedure TSDODataObjectList.append(const AValue: TSDODouble);
begin
  FField.setDouble(GetData(InternalAppend()),0,AValue);
end;

function TSDODataObjectList.getDouble() : TSDODouble;
begin
  CheckCursorPosition();
  Result := FField.getDouble(GetData(FCursor.GetCurrent()),0);
end;

procedure TSDODataObjectList.setDouble(const AValue: TSDODouble);
begin
  CheckCursorPosition();
  FField.setDouble(GetData(FCursor.GetCurrent()),0,AValue);
end;

function TSDODataObjectList.getDouble(const AIndex: PtrInt): TSDODouble;
begin
  MoveTo(AIndex);
  Result := getDouble();
end;

procedure TSDODataObjectList.setDouble(const AIndex: PtrInt; const AValue: TSDODouble);
begin
  MoveTo(AIndex);
  setDouble(AValue);
end;

procedure TSDODataObjectList.insert(const AIndex: PtrInt; const AValue: TSDODouble);
begin
  if ( AIndex = 0 ) then begin
    FField.setDouble(GetData(FData.InsertFirst()),0,AValue);
  end else begin
    if ( AIndex = FData.GetLength() ) then begin
      append(AValue);
    end else begin
      MoveTo(AIndex);
      FField.setDouble(GetData(FData.InsertBefore(FCursor.GetCurrent())),0,AValue);
    end;
  end;
  MoveTo(AIndex);
end;
{$ENDIF HAS_SDO_DOUBLE}

{$IFDEF HAS_SDO_FLOAT }
procedure TSDODataObjectList.append(const AValue: TSDOFloat);
begin
  FField.setFloat(GetData(InternalAppend()),0,AValue);
end;

function TSDODataObjectList.getFloat() : TSDOFloat;
begin
  CheckCursorPosition();
  Result := FField.getFloat(GetData(FCursor.GetCurrent()),0);
end;

procedure TSDODataObjectList.setFloat(const AValue: TSDOFloat);
begin
  CheckCursorPosition();
  FField.setFloat(GetData(FCursor.GetCurrent()),0,AValue);
end;

function TSDODataObjectList.getFloat(const AIndex: PtrInt): TSDOFloat;
begin
  MoveTo(AIndex);
  Result := getFloat();
end;

procedure TSDODataObjectList.setFloat(const AIndex: PtrInt; const AValue: TSDOFloat);
begin
  MoveTo(AIndex);
  setFloat(AValue);
end;

procedure TSDODataObjectList.insert(const AIndex: PtrInt; const AValue: TSDOFloat);
begin
  if ( AIndex = 0 ) then begin
    FField.setFloat(GetData(FData.InsertFirst()),0,AValue);
  end else begin
    if ( AIndex = FData.GetLength() ) then begin
      append(AValue);
    end else begin
      MoveTo(AIndex);
      FField.setFloat(GetData(FData.InsertBefore(FCursor.GetCurrent())),0,AValue);
    end;
  end;
  MoveTo(AIndex);
end;
{$ENDIF HAS_SDO_FLOAT }

procedure TSDODataObjectList.append(const AValue: TSDODate);
begin
  FField.setDate(GetData(InternalAppend()),0,AValue);
end;

function TSDODataObjectList.getDate() : TSDODate;
begin
  CheckCursorPosition();
  Result := FField.getDate(GetData(FCursor.GetCurrent()),0);
end;

function TSDODataObjectList.getDate(const AIndex: PtrInt): TSDODate;
begin
  MoveTo(AIndex);
  Result := getDate();
end;

procedure TSDODataObjectList.insert(const AIndex: PtrInt; const AValue: TSDODate);
begin
  if ( AIndex = 0 ) then begin
    FField.setDate(GetData(FData.InsertFirst()),0,AValue);
  end else begin
    if ( AIndex = FData.GetLength() ) then begin
      append(AValue);
    end else begin
      MoveTo(AIndex);
      FField.setDate(GetData(FData.InsertBefore(FCursor.GetCurrent())),0,AValue);
    end;
  end;
  MoveTo(AIndex);
end;

procedure TSDODataObjectList.setDate(const AIndex: PtrInt; const AValue: TSDODate);
begin
  MoveTo(AIndex);
  setDate(AValue);
end;

procedure TSDODataObjectList.setDate(const AValue: TSDODate);
begin
  CheckCursorPosition();
  FField.setDate(GetData(FCursor.GetCurrent()),0,AValue);
end;

{$IFDEF HAS_SDO_LONG}
procedure TSDODataObjectList.append(const AValue: TSDOLong);
begin
  FField.setLong(GetData(InternalAppend()),0,AValue);
end;

function TSDODataObjectList.getLong() : TSDOLong;
begin
  CheckCursorPosition();
  Result := FField.getLong(GetData(FCursor.GetCurrent()),0);
end;

procedure TSDODataObjectList.setLong(const AValue: TSDOLong);
begin
  CheckCursorPosition();
  FField.setLong(GetData(FCursor.GetCurrent()),0,AValue);
end;

function TSDODataObjectList.getLong(const AIndex: PtrInt): TSDOLong;
begin
  MoveTo(AIndex);
  Result := getLong();
end;

procedure TSDODataObjectList.setLong(const AIndex: PtrInt; const AValue: TSDOLong);
begin
  MoveTo(AIndex);
  setLong(AValue);
end;

procedure TSDODataObjectList.insert(const AIndex: PtrInt; const AValue: TSDOLong);
begin
  if ( AIndex = 0 ) then begin
    FField.setLong(GetData(FData.InsertFirst()),0,AValue);
  end else begin
    if ( AIndex = FData.GetLength() ) then begin
      append(AValue);
    end else begin
      MoveTo(AIndex);
      FField.setLong(GetData(FData.InsertBefore(FCursor.GetCurrent())),0,AValue);
    end;
  end;
  MoveTo(AIndex);
end;
{$ENDIF HAS_SDO_LONG}

{$IFDEF HAS_SDO_SHORT}
procedure TSDODataObjectList.append(const AValue: TSDOShort);
begin
  FField.setShort(GetData(InternalAppend()),0,AValue);
end;

function TSDODataObjectList.getShort() : TSDOShort;
begin
  CheckCursorPosition();
  Result := FField.getShort(GetData(FCursor.GetCurrent()),0);
end;

procedure TSDODataObjectList.setShort(const AValue: TSDOShort);
begin
  CheckCursorPosition();
  FField.setShort(GetData(FCursor.GetCurrent()),0,AValue);
end;

function TSDODataObjectList.getShort(const AIndex: PtrInt): TSDOShort;
begin
  MoveTo(AIndex);
  Result := getShort();
end;

procedure TSDODataObjectList.setShort(const AIndex: PtrInt; const AValue: TSDOShort);
begin
  MoveTo(AIndex);
  setShort(AValue);
end;

procedure TSDODataObjectList.insert(const AIndex: PtrInt; const AValue: TSDOShort);
begin
  if ( AIndex = 0 ) then begin
    FField.setShort(GetData(FData.InsertFirst()),0,AValue);
  end else begin
    if ( AIndex = FData.GetLength() ) then begin
      append(AValue);
    end else begin
      MoveTo(AIndex);
      FField.setShort(GetData(FData.InsertBefore(FCursor.GetCurrent())),0,AValue);
    end;
  end;
  MoveTo(AIndex);
end;
{$ENDIF HAS_SDO_SHORT}

function TSDODataObjectList.getVariant: TSDOVariant;
begin
  CheckCursorPosition();
  Result := FField.getVariant(GetData(FCursor.GetCurrent()),0);
end;

procedure TSDODataObjectList.setVariant(const AValue: TSDOVariant);
begin
  CheckCursorPosition();
  FField.setVariant(GetData(FCursor.GetCurrent()),0,AValue);
end;

function TSDODataObjectList.getVariant(const AIndex: PtrInt): TSDOVariant;
begin
  MoveTo(AIndex);
  Result := getVariant();
end;

procedure TSDODataObjectList.setVariant(const AIndex: PtrInt; const AValue: TSDOVariant);
begin
  MoveTo(AIndex);
  setVariant(AValue);
end;

{ TSDOOwnedDataObjectList }

procedure TSDOOwnedDataObjectList.append(AValue: ISDODataObject);
var
  newObj : ISDODataObjectEx;
  prp : ISDOProperty;
  ownerIntf : ISDODataObject;
begin
  ownerIntf := getOwner();
  prp := FOwnerProperty;
  if ( AValue <> nil ) then begin
    newObj := AValue as ISDODataObjectEx;
    if not newObj.IsInstanceOf(getItemType()) then
      raise ESDOIllegalArgumentException.Create('AProperty');
    if prp.isContainment() then begin
      if newObj.IsAncestorOf(ownerIntf) then
        raise ESDOCycleContainmentException.Create('AValue');
    end;
  end;

  inherited append(NIL_OBJECT);
  InternalSetDataObject(AValue,False);
  RecordChange(mvpaAppend);
end;

procedure TSDOOwnedDataObjectList.append(const AValue: TSDOInteger);
begin
  inherited append(AValue);
  RecordChange(mvpaAppend);
end;

constructor TSDOOwnedDataObjectList.Create(
  const AOwner : ISDODataObject;
  const AProperty: ISDOProperty
);
begin
  if ( AProperty = nil ) then
    raise ESDOIllegalArgumentException.Create('AProperty');
  if ( AOwner = nil ) then
    raise ESDOIllegalArgumentException.Create('AOwner');
  inherited Create(AProperty.getType());
  FOwnerProperty := AProperty;
  FOwner := Pointer(AOwner);
end;

procedure TSDOOwnedDataObjectList.delete(const AIndex: PtrInt);
begin
  MoveTo(AIndex);
  delete();
end;

procedure TSDOOwnedDataObjectList.delete();
begin
  RecordChange(mvpaDelete);
  if getItemType().isDataObjectType() then
    InternalSetDataObject(nil, False);
  inherited delete();
end;

function TSDOOwnedDataObjectList.getOwner() : ISDODataObject;
begin
  Result := ISDODataObject(FOwner);
end;

procedure TSDOOwnedDataObjectList.insert(const AIndex: PtrInt; const AValue: TSDOInteger);
begin
  inherited insert(AIndex, AValue);
  RecordChange(mvpaInsert);
end;

procedure TSDOOwnedDataObjectList.insert(const AIndex: PtrInt; AValue: ISDODataObject);
begin
  inherited insert(AIndex, NIL_OBJECT);
  InternalSetDataObject(AValue, False);
  RecordChange(mvpaInsert);
end;

procedure TSDOOwnedDataObjectList.InternalSetDataObject(
  const AValue: ISDODataObject;
  const ADoRecordChange: Boolean
);
var
  prp : ISDOProperty;
  fld : ISDOField;
  oldObj, newObj : ISDODataObjectEx;
  ownerIntf, oldContainer : ISDODataObject;
  locBuffer : Pointer;
  locCS : ISDOChangeSummaryEx;
begin
  CheckCursorPosition();
  locBuffer := GetData(FCursor.GetCurrent());

  ownerIntf := getOwner();
  prp := FOwnerProperty;
  if ( AValue <> nil ) then begin
    newObj := AValue as ISDODataObjectEx;
    if not newObj.IsInstanceOf(getItemType()) then
      raise ESDOIllegalArgumentException.Create('AProperty');
    if prp.isContainment() then begin
      if newObj.IsAncestorOf(ownerIntf) then
        raise ESDOCycleContainmentException.Create('AValue');
    end;
  end;

  fld := FField;

  oldObj := fld.getDataObject(locBuffer,data_offset) as ISDODataObjectEx;
  if Assigned(oldObj) then begin
    if prp.isContainment() then begin
      if ADoRecordChange then begin
        locCS := ownerIntf.getChangeSummary() as ISDOChangeSummaryEx;
        if ( locCS <> nil ) and locCS.isLogging() then begin
          locCS.getRecorder().recordDeletion(oldObj as ISDODataObject,getCursor().GetPosition());
        end;
      end;
      oldObj.setContainer(nil,nil);
    end;
    if prp.isReference() then
      oldObj.RemoveReference(ownerIntf,prp);
  end;

  if ADoRecordChange then
    RecordChange(mvpaChange);
  if ( AValue <> nil ) then begin
    if prp.isContainment() then begin
      oldContainer := newObj.getContainer();
      if Assigned(oldContainer) then
        oldContainer.setDataObject(newObj.getContainmentProperty(),nil);
    end;
    fld.setDataObject(locBuffer,data_offset,AValue);
    if prp.isContainment() then begin
      newObj.setContainer(ownerIntf,prp);
      if ADoRecordChange then begin
        if ( locCS = nil ) then
          locCS := ownerIntf.getChangeSummary() as ISDOChangeSummaryEx;
        if ( locCS <> nil ) and locCS.isLogging() then begin
          locCS.getRecorder().recordCreation(newObj as ISDODataObject,getCursor().GetPosition());
        end;
      end;
    end;
    if prp.isReference() then
      newObj.AddReference(ownerIntf,prp);
  end else begin;
    fld.setDataObject(locBuffer,data_offset,nil);
  end;
end;

procedure TSDOOwnedDataObjectList.RecordChange(const AChange : TManyValuePropAction);
var
  locCS : ISDOChangeSummaryEx;
begin
  locCS := getOwner().getChangeSummary() as ISDOChangeSummaryEx;
  if ( locCS <> nil ) and locCS.isLogging() then begin
    locCS.getRecorder().recordChange(getOwner(),FOwnerProperty,getCursor().GetPosition(),AChange);
  end;
end;

procedure TSDOOwnedDataObjectList.setDataObject(AValue: ISDODataObject);
begin
  InternalSetDataObject(AValue,True);
end;

procedure TSDOOwnedDataObjectList.setInteger(const AValue: TSDOInteger);
begin
  RecordChange(mvpaChange);
  inherited setInteger(AValue);
end;

procedure TSDOOwnedDataObjectList.setDataObject(const AIndex: PtrInt; AValue: ISDODataObject);
begin
  MoveTo(AIndex);
  setDataObject(AValue);
end;

procedure TSDOOwnedDataObjectList.setInteger(const AIndex: PtrInt; const AValue: TSDOInteger);
begin
  MoveTo(AIndex);
  setInteger(AValue);
end;

procedure TSDOOwnedDataObjectList.setBoolean(const AValue: TSDOBoolean);
begin
  RecordChange(mvpaChange);
  inherited setBoolean(AValue);
end;

procedure TSDOOwnedDataObjectList.setString(const AValue: TSDOString);
begin
  RecordChange(mvpaChange);
  inherited setString(AValue);
end;

procedure TSDOOwnedDataObjectList.setBoolean(const AIndex: PtrInt; const AValue: TSDOBoolean);
begin
  MoveTo(AIndex);
  setBoolean(AValue);
end;

procedure TSDOOwnedDataObjectList.setString(const AIndex: PtrInt; const AValue: TSDOString);
begin
  MoveTo(AIndex);
  setString(AValue);
end;

procedure TSDOOwnedDataObjectList.insert(const AIndex: PtrInt; const AValue: TSDOBoolean);
begin
  inherited insert(AIndex, AValue);
  RecordChange(mvpaInsert);
end;

procedure TSDOOwnedDataObjectList.insert(const AIndex: PtrInt; const AValue: TSDOString);
begin
  inherited insert(AIndex, AValue);
  RecordChange(mvpaInsert);
end;

procedure TSDOOwnedDataObjectList.append(const AValue: TSDOBoolean);
begin
  inherited append(AValue);
  RecordChange(mvpaAppend);
end;

procedure TSDOOwnedDataObjectList.append(const AValue: TSDOString);
begin
  inherited append(AValue);
  RecordChange(mvpaAppend);
end;

procedure TSDOOwnedDataObjectList.setByte(const AValue: TSDOByte);
begin
  RecordChange(mvpaChange);
  inherited setByte(AValue);
end;

procedure TSDOOwnedDataObjectList.setByte(const AIndex: PtrInt; const AValue: TSDOByte);
begin
  MoveTo(AIndex);
  setByte(AValue);
end;

procedure TSDOOwnedDataObjectList.insert(const AIndex: PtrInt; const AValue: TSDOByte);
begin
  inherited insert(AIndex, AValue);
  RecordChange(mvpaInsert);
end;

procedure TSDOOwnedDataObjectList.append(const AValue: TSDOByte);
begin
  inherited append(AValue);
  RecordChange(mvpaAppend);
end;

{$IFDEF HAS_SDO_BYTES}
procedure TSDOOwnedDataObjectList.setBytes(AValue: TSDOBytes);
begin
  RecordChange(mvpaChange);
  inherited setBytes(AValue);
end;

procedure TSDOOwnedDataObjectList.setBytes(const AIndex: PtrInt; AValue: TSDOBytes);
begin
  MoveTo(AIndex);
  setBytes(AValue);
end;

procedure TSDOOwnedDataObjectList.insertBytes(const AIndex: PtrInt; AValue : TSDOBytes);
begin
  inherited insertBytes(AIndex, AValue);
  RecordChange(mvpaInsert);
end;

procedure TSDOOwnedDataObjectList.appendBytes(AValue : TSDOBytes);
begin
  inherited appendBytes(AValue);
  RecordChange(mvpaAppend);
end;
{$ENDIF HAS_SDO_BYTES}

procedure TSDOOwnedDataObjectList.setDate(const AValue: TSDODate);
begin
  RecordChange(mvpaChange);
  inherited setDate(AValue);
end;

procedure TSDOOwnedDataObjectList.setDate(const AIndex: PtrInt; const AValue: TSDODate);
begin
  MoveTo(AIndex);
  setDate(AValue);
end;

procedure TSDOOwnedDataObjectList.insert(const AIndex: PtrInt; const AValue: TSDODate);
begin
  inherited insert(AIndex, AValue);
  RecordChange(mvpaInsert);
end;

procedure TSDOOwnedDataObjectList.append(const AValue: TSDODate);
begin
  inherited append(AValue);
  RecordChange(mvpaAppend);
end;

{$IFDEF HAS_SDO_CHAR}
procedure TSDOOwnedDataObjectList.setCharacter(const AValue: TSDOChar);
begin
  RecordChange(mvpaChange);
  inherited setCharacter(AValue);
end;

procedure TSDOOwnedDataObjectList.setCharacter(const AIndex: PtrInt; const AValue: TSDOChar);
begin
  MoveTo(AIndex);
  setCharacter(AValue);
end;

procedure TSDOOwnedDataObjectList.insert(const AIndex: PtrInt; const AValue: TSDOChar);
begin
  inherited insert(AIndex, AValue);
  RecordChange(mvpaInsert);
end;

procedure TSDOOwnedDataObjectList.append(const AValue: TSDOChar);
begin
  inherited append(AValue);
  RecordChange(mvpaAppend);
end;
{$ENDIF HAS_SDO_CHAR}

{$IFDEF HAS_SDO_CURRENCY}
procedure TSDOOwnedDataObjectList.setCurrency(const AValue: TSDOCurrency);
begin
  RecordChange(mvpaChange);
  inherited setCurrency(AValue);
end;

procedure TSDOOwnedDataObjectList.setCurrency(const AIndex: PtrInt; const AValue: TSDOCurrency);
begin
  MoveTo(AIndex);
  setCurrency(AValue);
end;

procedure TSDOOwnedDataObjectList.insertCurrency(const AIndex: PtrInt; const AValue: TSDOCurrency);
begin
  inherited insertCurrency(AIndex, AValue);
  RecordChange(mvpaInsert);
end;

procedure TSDOOwnedDataObjectList.appendCurrency(const AValue: TSDOCurrency);
begin
  inherited appendCurrency(AValue);
  RecordChange(mvpaAppend);
end;
{$ENDIF HAS_SDO_CURRENCY}

{$IFDEF HAS_SDO_DOUBLE}
procedure TSDOOwnedDataObjectList.setDouble(const AValue: TSDODouble);
begin
  RecordChange(mvpaChange);
  inherited setDouble(AValue);
end;

procedure TSDOOwnedDataObjectList.setDouble(const AIndex: PtrInt; const AValue: TSDODouble);
begin
  MoveTo(AIndex);
  setDouble(AValue);
end;

procedure TSDOOwnedDataObjectList.insert(const AIndex: PtrInt; const AValue: TSDODouble);
begin
  inherited insert(AIndex, AValue);
  RecordChange(mvpaInsert);
end;

procedure TSDOOwnedDataObjectList.append(const AValue: TSDODouble);
begin
  inherited append(AValue);
  RecordChange(mvpaAppend);
end;
{$ENDIF HAS_SDO_DOUBLE}

{$IFDEF HAS_SDO_FLOAT}
procedure TSDOOwnedDataObjectList.setFloat(const AValue: TSDOFloat);
begin
  RecordChange(mvpaChange);
  inherited setFloat(AValue);
end;

procedure TSDOOwnedDataObjectList.setFloat(const AIndex: PtrInt; const AValue: TSDOFloat);
begin
  MoveTo(AIndex);
  setFloat(AValue);
end;

procedure TSDOOwnedDataObjectList.insert(const AIndex: PtrInt; const AValue: TSDOFloat);
begin
  inherited insert(AIndex, AValue);
  RecordChange(mvpaInsert);
end;

procedure TSDOOwnedDataObjectList.append(const AValue: TSDOFloat);
begin
  inherited append(AValue);
  RecordChange(mvpaAppend);
end;
{$ENDIF HAS_SDO_FLOAT}

{$IFDEF HAS_SDO_LONG}
procedure TSDOOwnedDataObjectList.setLong(const AValue: TSDOLong);
begin
  RecordChange(mvpaChange);
  inherited setLong(AValue);
end;

procedure TSDOOwnedDataObjectList.setLong(const AIndex: PtrInt; const AValue: TSDOLong);
begin
  MoveTo(AIndex);
  setLong(AValue);
end;

procedure TSDOOwnedDataObjectList.insert(const AIndex: PtrInt; const AValue: TSDOLong);
begin
  inherited insert(AIndex, AValue);
  RecordChange(mvpaInsert);
end;

procedure TSDOOwnedDataObjectList.append(const AValue: TSDOLong);
begin
  inherited append(AValue);
  RecordChange(mvpaAppend);
end;
{$ENDIF HAS_SDO_LONG}

{$IFDEF HAS_SDO_SHORT}
procedure TSDOOwnedDataObjectList.setShort(const AValue: TSDOShort);
begin
  RecordChange(mvpaChange);
  inherited setShort(AValue);
end;

procedure TSDOOwnedDataObjectList.setShort(const AIndex: PtrInt; const AValue: TSDOShort);
begin
  MoveTo(AIndex);
  setShort(AValue);
end;

procedure TSDOOwnedDataObjectList.insert(const AIndex: PtrInt; const AValue: TSDOShort);
begin
  inherited insert(AIndex, AValue);
  RecordChange(mvpaInsert);
end;

procedure TSDOOwnedDataObjectList.append(const AValue: TSDOShort);
begin
  inherited append(AValue);
  RecordChange(mvpaAppend);
end;
{$ENDIF HAS_SDO_SHORT}


procedure TSDOOwnedDataObjectList.setVariant(const AValue: TSDOVariant);
begin
  RecordChange(mvpaChange);
  inherited setVariant(AValue);
end;

procedure TSDOOwnedDataObjectList.setVariant(const AIndex: PtrInt; const AValue: TSDOVariant);
begin
  MoveTo(AIndex);
  setVariant(AValue);
end;

end.
