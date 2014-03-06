{
    This file is part of the Free Pascal Class Library SDO Implementation
    Copyright (c) 2012 by Inoussa OUEDRAOGO
    Free Pascal development team

    This unit implements basic SDO definitions

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$INCLUDE sdo_global.inc}
unit sdo;

interface
uses
  SysUtils, Classes, Types,
  sdo_types, sdo_linked_list, sdo_date_utils;

const
  sdo_namespace = 'commonj.sdo';

type

  TSDOTypeKind = (
    //OtherTypes , // for 'unknown type; - all data objects'
    BooleanType,
    ByteType,
{$IFDEF HAS_SDO_BYTES}
    BytesType,
{$ENDIF HAS_SDO_BYTES}
    ChangeSummaryType,
{$IFDEF HAS_SDO_CHAR}
    CharacterType,
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
    CurrencyType,
{$ENDIF HAS_SDO_CURRENCY}
    DateTimeType,
    //DayType,
{$IFDEF HAS_SDO_DOUBLE}
    DoubleType,
{$ENDIF HAS_SDO_DOUBLE}
    //DurationType,
{$IFDEF HAS_SDO_FLOAT}
    FloatType,
{$ENDIF HAS_SDO_FLOAT}
    IntegerType,
{$IFDEF HAS_SDO_LONG}
    LongType,
{$ENDIF HAS_SDO_LONG}
    //MonthType,
    //MonthDayType,
    ObjectType,
{$IFDEF HAS_SDO_SHORT}
    ShortType,
{$ENDIF HAS_SDO_SHORT}
    StringType
    //TimeType,
    //UriType,
    //YearType,
    //YearMonthType,
    //YearMonthDayType
  );

const
  DateType = DateTimeType;
  IntType = IntegerType;
  StringsType = StringType;
  
  SDOTypeDefaultTypeNames : array[TSDOTypeKind] of string = (
    //OtherTypes , // for 'unknown type; - all data objects'
    'Boolean',
    'Byte',
{$IFDEF HAS_SDO_BYTES}
    'Bytes',
{$ENDIF HAS_SDO_BYTES}
    'ChangeSummary',
{$IFDEF HAS_SDO_CHAR}
    'Character',
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
    'Currency',
{$ENDIF HAS_SDO_CURRENCY}
    'DateTime',
{$IFDEF HAS_SDO_DOUBLE}
    'Double',
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
    'Float',
{$ENDIF HAS_SDO_FLOAT}
    'Integer',
{$IFDEF HAS_SDO_LONG}
    'Long',
{$ENDIF HAS_SDO_LONG}
    'Object',
{$IFDEF HAS_SDO_SHORT}
    'Short',
{$ENDIF HAS_SDO_SHORT}
    'String'
  );
  SDODataTypeKinds = [Low(TSDOTypeKind)..High(TSDOTypeKind)] - [ ChangeSummaryType, ObjectType ];

type

  PSDOBoolean = ^TSDOBoolean;
{$IFDEF HAS_SDO_BYTES}
  PSDOBytes = ^TSDOBytes;
  PPSDOBytes = ^PSDOBytes;
{$ENDIF HAS_SDO_BYTES}
  PPSDOChangeSummary = ^PSDOChangeSummary;
  PSDOChangeSummary = ^ISDOChangeSummary;
{$IFDEF HAS_SDO_CHAR}
  PSDOChar = ^TSDOChar;
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
  PSDOCurrency = ^TSDOCurrency;
{$ENDIF HAS_SDO_CURRENCY}
  PSDODate = ^TSDODate;
  PSDODateTime = ^TSDODateTime;
{$IFDEF HAS_SDO_DOUBLE}
  PSDODouble = ^TSDODouble;
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
  PSDOFloat = ^TSDOFloat;
{$ENDIF HAS_SDO_FLOAT}
  PSDOInteger = ^TSDOInteger;
{$IFDEF HAS_SDO_LONG}
  PSDOLong = ^TSDOLong;
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
  PSDOShort = ^TSDOShort;
{$ENDIF HAS_SDO_SHORT}
  PPSDOString = ^PSDOString;
  PSDOString = ^TSDOString;
  PSDODataObject = ^ISDODataObject;
  PPSDODataObject = ^PSDODataObject;
  PPSDODataObjectList = ^PSDODataObjectList;
  PSDODataObjectList = ^ISDODataObjectList;

  TSDOFieldBuffer = Pointer;
  TSDOBoolean = Boolean;
  TSDOByte = Byte;
  TSDOBytes = TByteDynArray;
{$IFDEF HAS_SDO_CHAR}
  {$IFDEF USE_UNICODE}
    TSDOChar = WideChar;
  {$ELSE USE_UNICODE}
    TSDOChar = Char;
  {$ENDIF USE_UNICODE}
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
  TSDOCurrency = Currency;
{$ENDIF HAS_SDO_CURRENCY}
  TSDODateTime = TDateTimeRec;
  TSDODate = TSDODateTime;
{$IFDEF HAS_SDO_DOUBLE}
  TSDODouble = Double;
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
  TSDOFloat = Single;
{$ENDIF HAS_SDO_FLOAT}  
  TSDOInteger = Integer;
{$IFDEF HAS_SDO_LONG}  
  TSDOLong = Int64;
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
  TSDOShort = SmallInt;
{$ENDIF HAS_SDO_SHORT}
{$IFDEF USE_UNICODE}
  TSDOString = UnicodeString;
{$ELSE USE_UNICODE}
  TSDOString = AnsiString;
{$ENDIF USE_UNICODE}
  TSDOVariant = Variant;

  TChangeType = ( ctUndefined, ctCreate, ctChange, ctDelete );
  TTypeFlag = ( tfIsSequenced, tfIsOpen, tfIsAbstract, tfIsDataType );
  TTypeFlags = set of TTypeFlag;
  TPropertyFlag = (
    pfIsMany, pfIsReadOnly, pfIsContainment, pfIsNotNullable,
    //This is an extension
    pfIsAttribute
  );
  TPropertyFlags = set of TPropertyFlag;
  TSerializerOption = (soExcludeSchema);
  TSerializerOptions = set of TSerializerOption;

  ESDOException = class(Exception)
  end;

  ESDONotImplementedException = class(ESDOException)
  end;

  ESDOUnsupportedOperationException = class(ESDOException)
  private
    FOperation: string;
  public
    constructor Create(const AOperation : string);
    property Operation : string read FOperation write FOperation;
  end;
    ESDOInvalidStateOperationException = class(ESDOUnsupportedOperationException) end;

  ESDOPropertyNotFoundException = class(ESDOException)
  private
    FPropertyName: string;
  public
    constructor Create(const APropName : string);
    property PropertyName : string read FPropertyName write FPropertyName;
  end;

  ESDOIllegalArgumentException = class(ESDOException)
  private
    FArgumentName: string;
  public
    constructor Create(const AArgumentName : string);
    property ArgumentName : string read FArgumentName write FArgumentName;
  end;
    ESDOCycleContainmentException = class(ESDOIllegalArgumentException) end;
    //ESDOPropertyNotBelongToObjectException = class(ESDOIllegalArgumentException) end;

  ESDOIndexOutOfRangeException = class(ESDOException)
  private
    FValue: Integer;
  public
    constructor Create(const AValue : Integer);
    property Value : Integer read FValue write FValue;
  end;

  ESDOTypeNotFoundException = class(ESDOException)
  private
    FName: string;
  public
    constructor Create(const AName : string);
    property Name : string read FName write FName;
  end;

  ESDOCircularDependencyTypeException = class(ESDOIllegalArgumentException)
  private
    FTypeName: string;
    FPropertyType: string;
    FPropertyName: string;
  public
    constructor Create(
      const ATypeName,
            APropertyName,
            APropertyType : string
    );
    property TypeName : string read FTypeName write FTypeName;
    property PropertyName : string read FPropertyName write FPropertyName;
    property PropertyType : string read FPropertyType write FPropertyType;
  end;

  ESDOIncompleteTypeException = class(ESDOException)
  private
    FTypeName: string;
  public
    constructor Create(const ATypeName : string);
    property TypeName : string read FTypeName write FTypeName;
  end;
    ESDOAbstractTypeException = class(ESDOIncompleteTypeException) end;

  ESDODuplicatedItemException = class(ESDOException)
  private
    FName: string;
  public
    constructor Create(const AName : string);
    property Name : string read FName write FName;
  end;

  ESDOInvalidConversionException = class(ESDOException)
  private
    FName: string;
  public
    constructor Create(const AName : string);
    property Name : string read FName write FName;
  end;

  ESDOInvalidPathException = class(ESDOException)
  private
    FPath: string;
  public
    constructor Create(const APath : string);
    property Path : string read FPath write FPath;
  end;

  ISDOType = interface;
  ISDOProperty = interface;
  ISDOPropertyList = interface;
  ISDODataObject = interface;
  ISDODataObjectList = interface;
  ISDOChangeSummary = interface;
  ISDODataFactory = interface;

  ISDOType = interface
    ['{76F00A3A-A6F8-4D78-8514-570207853453}']
	{  getName returns the name of the type
     *
	 * This method returns a const char* name of the type.
	 }
    function getName() : string;

	{  getAlias returns the n'th alias
     *
	 * This method returns a const char* corresponding to the
	 * alias at index n of the list of aliases. Use getAliasCount to
	 * discover the size of the list.
	 }
	  function getAlias(const AIndex : PtrInt) : string;

	{  getAliasCount  returns the number of aliases
     *
	 * This method returns the number of aliases for this type
	 }
	  function getAliasCount() : PtrInt;

	{  getBaseType returns the base if there is one
	 *
	 * This method returns a const Type* corresponding to the
	 * base Type for this type. The base type is the one which
	 * this type inherits from.
	 }
	  function getBaseType() : ISDOType;

	{  getURI  returns the URI for this type
	 *
	 * This method returns the URI for this type. The URI may be
	 * null.
	 }
	  function getURI() : string;


	{  getProperties returns a list of properties for this type
	 *
	 * This method returns the list of properties for this type.
	 * Instances of open types may have more properties than appear
	 * in this list.
	 * See the propertylist API.
	 }
    function getProperties() : ISDOPropertyList;

	{  getProperty returns a property for this type
	 *
	 * This method returns a property, by index or by name
	 }
   function getProperty(const APropertyName : string) : ISDOProperty;overload;
   function getProperty(const APropertyIndex : Integer) : ISDOProperty;overload;

	{  getPropertyIndex returns an index
	 *
	 * This method returns a property index for a named property
	 }
	  function getPropertyIndex(const APropertyName : string) : Integer;

	{  isDataObjectType true if not a DataType
	 *
	 * This method returns true if the type is not a DataType, and is therefore
	 * a DataObjectType with properties.
	 }
	  function isDataObjectType() : Boolean;


	{  isSequencedType true if the type is sequenced
	 *
	 * This method returns true if the type is sequenced, and is therefore
	 * objects of this type can be manipulate via their sequence interface.
	 }
	  function isSequencedType() : Boolean;


	{  isOpenType true if the type is open
	 *
	 * Normal types have a predefined list of properties. Trying to set
	 * properties which do not exist will cause an exception to be thrown.
	 * Open types, on the other hand, may have properties added to their
	 * instances runtime. These properties get added silently when setting a
	 * property value for a property which does not exist.
	 * Different instances of these objects may have different lists of
	 * open properties.
	 }
	  function isOpenType() : Boolean;

	{  isAbstractType true if the type is not instantiable.
	 *
	 * An abstract type may not be instantiated. It is useful only as
	 * a base type to some other non-abstract type.
	 }
	  function isAbstractType() : Boolean;

	{  isDataType true if the type is not an object.
	 *
	 * A DataType is anything which is not a DataObjectType. This method
	 * is the opposite of isDataObjectType().
	 }
    function isDataType() : Boolean;

	{  isChangeSummaryType true if the type is a change summary.
	 *
	 * There is only one type called ChangeSummary. This method serves
	 * no purpose in the C++ implementation.
	 }
    function isChangeSummaryType() : Boolean;

	{  getTypeEnum gets the enum for this type.
	 *
	 * Each DataType has a defined value in the list of Types.
	 }
    function getTypeEnum() : TSDOTypeKind;

	{  equals compares uri and name.
	 *
	 * The types are equal if the URI and Name are equal.
	 }
    function equals(const AOther : ISDOType) : Boolean;

    function getFlags() : TTypeFlags;

    //Implementation extension
    function getOwner() : ISDODataFactory;
  end;


  ISDOProperty = interface
    ['{F4649B56-366D-42CC-B818-46E2577E87FD}']
	{  getName gets the name of the property
	 *
	 * Returns the name of the property.
	 }
 	  function getName() : string;
  
	{  getAlias returns the n'th alias
     *
	 * This method returns a const char* corresponding to the
	 * alias at index n of the list of aliases. Use getAliasCount to 
	 * discover the size of the list.
	 }
    function getAlias(const AIndex : Integer) : string;

	{  getAliasCount  returns the number of aliases
     *
	 * This method returns the number of aliases for this type
	 }
    function getAliasCount() : Integer;

	{  getType returns the type of this property
	 *
	 * This method returns the type, which may be a DataType or a
	 * DataObjectType
	 }
    function getType() : ISDOType;

	{  getTypeEnum gets the enum for this type. 
	 *
	 * Each DataType has a defined value in the list of Types.
	 }
    function getTypeEnum() : TSDOTypeKind;

 	{  isMany is true if the property is a list
	 *
	 * IsMany returns true if this property represents a list of
	 * values, and should be accessed via the getList DataObjectAPI.
	 }
    function isMany() : Boolean;

 	{  isContainment is true if the property value is contained
	 *
	 * IsContainment returns true if this property represents a DataObjectType,
	 * and that DataObjectType is contained. I.E the property value is not a pointer
	 * to a DataObject somewhere else in the graph, it is an actual value.
	 }
   function isContainment() : Boolean;

 	{  isReference is true if the property value is not contained 
	 *
	 * IsReference returns true if this property represents a DataObjectType,
	 * and that DataObjectType is not contained. I.E the property value is a pointer
	 * to a DataObject somewhere else in the graph not an actual value.
	 }
   function isReference() : Boolean;
  
	{  getContainingType give the type which holds this property.
	 *
	 * Although many types may have a property of the same name, any given 
	 * instance of a property belongs to only one type.
	 * This method returns the type which holds this proeprty.
	 }
   function getContainingType() : ISDOType;


	{  isReadOnly returns true if the property is unmodifiable.
	 *
	 * NOT IMPLEMENTED
     * Returns true if values for this Property cannot be modified using the SDO APIs.
     * When true, DataObject.set(Property property, Object value) throws an exception.
     * Values may change due to other factors, such as services operating on DataObjects.
	 }
   function isReadOnly() : Boolean;

	{  getOpposite  returns the opposite property or zero.
	 *
	 * NOT IMPLEMENTED
	 }
   function getOpposite() : ISDOProperty;

	{  isDefaulted is true if a default has been set.
	 *
	 * A property value may be set or unset. If unset, requests to the
	 * data object for the value will return a default if there is one.
	 * If the property is not defaulted, an un specified value will be
	 * returned. (Thism value will probably be zero).
	 }

	  function isDefaulted() : Boolean ;

	{  setDefault sets the right sort of default.
	 *
	 * The many overrides of this method allow the setting
	 * of a default value for any DataType property.
	 *}
    procedure setDefault(const AValue : TSDOBoolean);overload;
    procedure setDefault(const AValue : TSDOByte);overload;
{$IFDEF HAS_SDO_BYTES}
    procedure setDefault(AValue : TSDOBytes);overload;
{$ENDIF HAS_SDO_BYTES}
{$IFDEF HAS_SDO_CHAR}
    procedure setDefault(const AValue : TSDOChar);overload;
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
    procedure setDefaultCurrency(const AValue : TSDOCurrency);
{$ENDIF HAS_SDO_CURRENCY}
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
	{  getDefault gets the right sort of default.
	 *
	 * The many overrides of this method allow the getting
	 * of a default value for any DataType property.
	 }
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

    function isNullable() : Boolean;
    //Extensions ...
    function isAttribute() : Boolean;
  end;

  ISDOPropertyList = interface
    ['{1253EA5C-D19C-434E-ADE5-A7639D582102}']
    function getCount() : Integer;
    function getItem(const AIndex : Integer) : ISDOProperty;
    function find(const APropertyName : string) : ISDOProperty;
    function getIndex(const APropertyName : string) : Integer;
  end;

  ISDOTypeList = interface
    ['{7A07FC8B-FA82-41AF-87F3-8BE3C2F228E1}']
    function getCount() : Integer;
    function getItem(const AIndex : Integer) : ISDOType;
    function find(const AUri, AName : string) : ISDOType;
    function getIndex(const AUri, AName : string) : Integer;
  end;

  ISDODataFactory = interface
    ['{8898CB64-1E83-464D-AE50-4DDB6D7770FE}']
		{
		 *  clone copies a data factory
		 *
		 * Copy the data factory, and return a new data factory which
		 * has the same properties and types, but is still able to have
		 * new types added to it.
		 }

		//SDO_API virtual DataFactoryPtr clone();

		{
		 *  DataFactory::create creates a data object.
		 *
		 * Create a data object based on the type specified as a parameter
		 * Once a data object has been created by this factory, the metadata
		 * (types and properties) may no longer be altered.
		 }
    function CreateNew(const AType : ISDOType) : ISDODataObject;overload;
    function CreateNew(const AUri, ATypeName : string) : ISDODataObject;overload;

		{
		 *  DataFactory::getType gets a type back from the factory.
		 *
		 * Get a type as specified in the data factory. Useful for creating
		 * data objects or querying properties.
		 }
    function getType(const AUri, ATypeName : string) : ISDOType;

		{
		 *  DataFactory::getTypes gets a list of types back from the factory.
		 *
		 * Get all the types available within this data factory. Useful for
		 * validating whether a data object is of the correct type to be
		 * usable.
		 }

		function getTypes() : ISDOTypeList;

		{
		 *  DataFactory::addType adds a type definition.
		 *
		 * Add a type defintion to the factory. (Properties may be added later).
		 * The type is defined by its uri and name.
		 * -# The type may be sequenced - and therefore work with a sequence API.
		 * -# The type may be open, indicating that it may have extra properties
		 * added at runtime.
		 * -# The type may be abstract, so the data factory will not permit creation
		 * of instances.
		 * -# The type may be a data type, indicating that is will not have properties.
		 * The Type may inherit from another type, but that is specified later with a
		 * call to setBaseType().
		 }
    procedure AddType(
      const AUri,
            ATypeName : string;
      const AFlags    : TTypeFlags
    );

		{
		 *  DataFactory::setBaseType allows inheritance
		 *
		 * The type specified second in the parameters becomes the basetype
		 * of the first parameter.
		 * The second type will have all the properties of its baser type, followed
		 * by any additional properties of its own. The property indices of the properties
		 * of the subclass will begin at one more than the total number of
		 * properties of the supertype
		 * This relationship is fixed when the first data object of any type is
		 * created by the factory. Up to that point the base type may be changed.
		 *
		 }
    procedure setBaseType(const AType, ABase : ISDOType);overload;
    procedure setBaseType(
      const ATypeURI, ATypeName,
            ABaseURI, ABaseName : string
    );overload;

		{
		 *  DataFactory::setAlias sets an alternative name
		 *
		 * A Type may be known to the data factory by several different names.
		 * This method adds a new name for an existing property.
		 *
		 }
    procedure setAlias(const ATypeURI, ATypeName, AAlias : string);overload;

		{
		 *  DataFactory::addPropertyToType adds properties
		 *
		 * The various addPropertyToType methods add a property to an
		 * existing type in the factory, specifying the name of the new property,
		 * and the type of the new property - which must also be an existing type
		 * in this factory.
		 * -# The new property may be many-valued - so it will be a list of values.
		 * -# The property may be read-only , and may not be altered by user code -
		 * However the value may be changed by data access service code.
		 * -# The property may be containment.
		 * The type of a property may be DataType, or DataObjectType (see Types).
		 * If the property is a DataType, then the actual value of the property is held
		 * within the data object containing the property.
		 * If the property is a DataObjectType, it may be containment, or reference.
		 * Containment indicates that the value of the property is contained in the
		 * data object, whilst reference indicates that the property is only a pointer to
		 * a value somewhere else in the data graph.
		 *
		 }
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
      const AType     : ISDOType;
      const APropName : string;
      const APropType : ISDOType;
      const AFlags : TPropertyFlags
    );overload;
    procedure addProperty(
      const AType : ISDOType;
      const APropName, APropTypeUri, APropTypeName : string;
      const AFlags : TPropertyFlags
    );overload;


		{
		 *  DataFactory::setAlias sets a property alias name
		 *
		 * A property, like a type, may be known to the factory by several
		 * names.
		 }
    procedure setAlias(const ATypeUri, ATypeName, APropName, AAlias : string);overload;

    { for open Type support - this is an implementation extension!
        the "ADataObject" parameter must be of open type
    }
    procedure addProperty(
            ADataObject : ISDODataObject;
      const APropName : string;
      const APropType : ISDOType;
      const AFlags : TPropertyFlags
    );overload;

    //Implementation extension
    function CreateList(AType : ISDOType) : ISDODataObjectList;overload;
    function CreateList(const AUri, ATypeName : string) : ISDODataObjectList;overload;
  end;


  ISDODataObject = interface
    ['{74EFD05B-438F-4193-8AF0-3E28195A034B}']

    {  getPropertyIndex gets the unique index of a property
     *
       * A property of a data object has a unique index associated with it.
     * This method gets a property index for this object from the property,
     * or throw SDOPropertyNotFoundException if the property is not part
     * of this data object.
     }
    function getPropertyIndex(const AProperty : ISDOProperty) : PtrInt;

      {  getInstanceProperties gets the props of the current object.
     *
     * Returns a read-only List of the Properties currently used in this DataObject.
       * This list will contain all of the properties in getType().getProperties()
       * and any properties where isSet(property) is true.
       * For example, properties resulting from the use of
       * open or mixed XML content are present if allowed by the Type.
       * The list does not contain duplicates.
       * The order of the properties in the list begins with getType().getProperties()
       * and the order of the remaining properties is determined by the implementation.
       * The same list will be returned unless the DataObject is updated so that
       * the contents of the list change
       * Returns the list of Properties currently used in this DataObject.
     }
    function getInstanceProperties() : ISDOPropertyList;

    {
     * These are just like getType().getProperty(), but may return
     * values other than the property list for open types.
     }
    function getProperty(const AIndex : PtrUInt) : ISDOProperty;overload;
    function getProperty(const AProp : string) : ISDOProperty;overload;

    {  getContainer get the containing object
     *
     * Returns the containing data object
     * or 0 if there is no container.
     }
    function getContainer() : ISDODataObject;

    {  getContainmentProperty returns the property containing this object
     *
     *  Return the Property of the data object containing this data object
     *  or throw an SDOPropertyNotFoundException if there is no container.
     }
    function getContainmentProperty() : ISDOProperty;

    {  getType  returns the data object's type.
     *
     * getType returns the data object's type.
     * The type defines the properties available for reflective access.
       }
    function getType() : ISDOType;

    {  getTypeEnum returns an enumerator for the type
     *
     * Returns an enumerator for the type for easy switching on basic types.
     * The enumerator is part of the Type class
     }
    function getTypeEnum() : TSDOTypeKind;

    {  getDataObject returns a data object by path, index or property
     *
     * Returns the value of a property of either this object or an object 
       * reachable from it, as identified by the specified path.
     }
    function getDataObject(const APath : string) : ISDODataObject; overload;
    function getDataObject(const APropertyIndex : PtrUInt) : ISDODataObject; overload;
    function getDataObject(const AProperty : ISDOProperty) : ISDODataObject; overload;

    {  setDataObject sets a value by path, index or property
     *
     * Sets a property of either this object or an object reachable from it,
     * as identified by the specified path,
     * to the specified value.
     }
    procedure setDataObject(const APath : string; AValue : ISDODataObject); overload;
    procedure setDataObject(const APropertyIndex : PtrUInt; AValue : ISDODataObject); overload;
    procedure setDataObject(const AProperty : ISDOProperty; AValue : ISDODataObject); overload;

    {  getBoolean returns a TSDOBoolean by path, index or property
     *
     * Returns the value of a property of either this object or an object 
       * reachable from it, as identified by the specified path.
     }
    function getBoolean(const APath : string) : TSDOBoolean; overload;
    function getBoolean(const APropertyIndex : PtrUInt) : TSDOBoolean; overload;
    function getBoolean(const AProperty : ISDOProperty) : TSDOBoolean; overload;

    procedure setBoolean(const APath : string; const AValue : TSDOBoolean); overload;
    procedure setBoolean(const APropertyIndex : PtrUInt; const AValue : TSDOBoolean); overload;
    procedure setBoolean(const AProperty : ISDOProperty; const AValue : TSDOBoolean); overload;

    {  getByte returns a char by path, index or property
     *
     * Returns the value of a property of either this object or an object 
       * reachable from it, as identified by the specified path.
     }
    function getByte(const APath : string) : TSDOByte;overload;
    function getByte(const APropertyIndex : PtrUInt) : TSDOByte;overload;
    function getByte(const AProperty : ISDOProperty) : TSDOByte;overload;

    procedure setByte(const APath : string; const AValue : TSDOByte);overload;
    procedure setByte(const APropertyIndex : PtrUInt; const AValue : TSDOByte);overload;
    procedure setByte(const AProperty : ISDOProperty; const AValue : TSDOByte);overload;

{$IFDEF HAS_SDO_CHAR}
    {  getCharacter returns a wchar_t by path, index or property
     *
     * Returns the value of a property of either this object or an object
       * reachable from it, as identified by the specified path.
     }
    function getCharacter(const APath : string) : TSDOChar;overload;
    function getCharacter(const APropertyIndex : PtrUInt) : TSDOChar;overload;
    function getCharacter(const AProperty : ISDOProperty) : TSDOChar;overload;

    procedure setCharacter(const APath : string; const AValue : TSDOChar);overload;
    procedure setCharacter(const APropertyIndex : PtrUInt; const AValue : TSDOChar);overload;
    procedure setCharacter(const AProperty : ISDOProperty; const AValue : TSDOChar);overload;
{$ENDIF HAS_SDO_CHAR}

{$IFDEF HAS_SDO_CURRENCY}
    function getCurrency(const APath : string) : TSDOCurrency;overload;
    function getCurrency(const APropertyIndex : PtrUInt) : TSDOCurrency;overload;
    function getCurrency(const AProperty : ISDOProperty) : TSDOCurrency;overload;

    procedure setCurrency(const APath : string; const AValue : TSDOCurrency);overload;
    procedure setCurrency(const APropertyIndex : PtrUInt; const AValue : TSDOCurrency);overload;
    procedure setCurrency(const AProperty : ISDOProperty; const AValue : TSDOCurrency);overload;
{$ENDIF HAS_SDO_CURRENCY}

{$IFDEF HAS_SDO_BYTES}
    {  getBytes returns a byte buffer
     *
     * A DataObject of type Bytes holds an array of bytes as its value. These
     * methods transfer the contents of that buffer into an array of chars allocated
     * by the users program. The return value is the number of bytes actually 
     * copied.
     * The byte array is not necessarily null terminated. If a null terminated
     * C style string is required, then getCString is an alternative.
     * The third paarameter is the length of the allocated buffer, which may be more
     * than the length of the byte array. If the length specified is less than the
     * length of the byte array, then only a portion of the 
     * byte array is returned.
       }
    function getBytes(const APath : string) : TSDOBytes;overload;
    function getBytes(const APropertyIndex : PtrUInt) : TSDOBytes;overload;
    function getBytes(const AProperty : ISDOProperty) : TSDOBytes;overload;

    procedure setBytes(const APath : string; AValue : TSDOBytes);overload;
    procedure setBytes(const APropertyIndex : PtrUInt; AValue : TSDOBytes);overload;
    procedure setBytes(const AProperty : ISDOProperty; AValue : TSDOBytes);overload;
{$ENDIF HAS_SDO_BYTES}

    {  getString returns a wide char buffer
     *
     * A DataObject of type String holds an array of wide characters as its value. These
     * methods transfer the contents of that buffer into an array of wchar_t allocated
     * by the users program. The return value is the number of wchar_t actually 
     * copied.
     * The array is not necessarily null terminated. 
     * The third paarameter is the length of the allocated buffer, which may be more
     * than the length of the array. If the length specified is less than the
     * length of the array, then only a portion of the array is returned.
       }
    function getString(const APath : string) : TSDOString;overload;
    function getString(const APropertyIndex : PtrUInt) : TSDOString;overload;
    function getString(const AProperty : ISDOProperty) : TSDOString;overload;

    procedure setString(const APath : string; const AValue : TSDOString);overload;
    procedure setString(const APropertyIndex : PtrUInt; const AValue : TSDOString);overload;
    procedure setString(const AProperty : ISDOProperty; const AValue : TSDOString);overload;

    { Extension : Variant support }
    function getVariant(const APath : string) : TSDOVariant;overload;
    function getVariant(const APropertyIndex : PtrUInt) : TSDOVariant;overload;
    function getVariant(const AProperty : ISDOProperty) : TSDOVariant;overload;

    procedure setVariant(const APath : string; const AValue : TSDOVariant);overload;
    procedure setVariant(const APropertyIndex : PtrUInt; const AValue : TSDOVariant);overload;
    procedure setVariant(const AProperty : ISDOProperty; const AValue : TSDOVariant);overload;


    {  getDate returns an SDODate by path, index or property
     *
     * Returns the value of a property of either this object or an object 
       * reachable from it, as identified by the specified path.
     }
    function getDate(const APath : string) : TSDODate;overload;
    function getDate(const APropertyIndex : PtrUInt) : TSDODate;overload;
    function getDate(const AProperty : ISDOProperty) : TSDODate;overload;

    procedure setDate(const APath : string; const AValue : TSDODate);overload;
    procedure setDate(const APropertyIndex : PtrUInt; const AValue : TSDODate);overload;
    procedure setDate(const AProperty : ISDOProperty; const AValue : TSDODate);overload;

{$IFDEF HAS_SDO_DOUBLE}
    {  getDouble returns a long double by path, index or property
     *
     * Returns the value of a property of either this object or an object 
       * reachable from it, as identified by the specified path.
     }
    function getDouble(const APath : string) : TSDODouble;overload;
    function getDouble(const APropertyIndex : PtrUInt) : TSDODouble;overload;
    function getDouble(const AProperty : ISDOProperty) : TSDODouble;overload;

    procedure setDouble(const APath : string; const AValue : TSDODouble);overload;
    procedure setDouble(const APropertyIndex : PtrUInt; const AValue : TSDODouble);overload;
    procedure setDouble(const AProperty : ISDOProperty; const AValue : TSDODouble);overload;
{$ENDIF HAS_SDO_DOUBLE}

{$IFDEF HAS_SDO_FLOAT}
    {  getFloat returns a float by path, index or property
     *
     * Returns the value of a property of either this object or an object 
       * reachable from it, as identified by the specified path.
     }
    function getFloat(const APath : string) : TSDOFloat;overload;
    function getFloat(const APropertyIndex : PtrUInt) : TSDOFloat;overload;
    function getFloat(const AProperty : ISDOProperty) : TSDOFloat;overload;

    procedure setFloat(const APath : string; const AValue : TSDOFloat);overload;
    procedure setFloat(const APropertyIndex : PtrUInt; const AValue : TSDOFloat);overload;
    procedure setFloat(const AProperty : ISDOProperty; const AValue : TSDOFloat);overload;
{$ENDIF HAS_SDO_FLOAT}

    {  getInteger returns a long by path, index or property
     *
     * Returns the value of a property of either this object or an object 
       * reachable from it, as identified by the specified path.
     }
    function getInteger(const APath : string) : TSDOInteger;overload;
    function getInteger(const APropertyIndex : PtrUInt) : TSDOInteger;overload;
    function getInteger(const AProperty : ISDOProperty) : TSDOInteger;overload;

    procedure setInteger(const APath : string; const AValue : TSDOInteger);overload;
    procedure setInteger(const APropertyIndex : PtrUInt; const AValue : TSDOInteger);overload;
    procedure setInteger(const AProperty : ISDOProperty; const AValue : TSDOInteger);overload;

    {  getLong returns a int64_t by path, index or property
     *
     * Returns the value of a property of either this object or an object 
       * reachable from it, as identified by the specified path.
     }
    function getLong(const APath : string) : TSDOLong;overload;
    function getLong(const APropertyIndex : PtrUInt) : TSDOLong;overload;
    function getLong(const AProperty : ISDOProperty) : TSDOLong;overload;

    procedure setLong(const APath : string; const AValue : TSDOLong);overload;
    procedure setLong(const APropertyIndex : PtrUInt; const AValue : TSDOLong);overload;
    procedure setLong(const AProperty : ISDOProperty; const AValue : TSDOLong);overload;

    {  getShort returns a short by path, index or property
     *
     * Returns the value of a property of either this object or an object 
       * reachable from it, as identified by the specified path.
     }
    function getShort(const APath : string) : TSDOShort;overload;
    function getShort(const APropertyIndex : PtrUInt) : TSDOShort;overload;
    function getShort(const AProperty : ISDOProperty) : TSDOShort;overload;

    procedure setShort(const APath : string; const AValue : TSDOShort);overload;
    procedure setShort(const APropertyIndex : PtrUInt; const AValue : TSDOShort);overload;
    procedure setShort(const AProperty : ISDOProperty; const AValue : TSDOShort);overload;

    {  setNull sets a data object value to null.
     *
     * A DataObjectType or DataType value may be set or unset. If it is set, then
     * it may have a value, or it may be set to null. A distinction is drawn between
     * being unset, having the default value, being set and being null.
     * When the value of an integer (for example) is returned as zero, it could have
     * been set to zero, or it could be null. Use isNull() to verify.
       }
    procedure setNull(const APath : string);overload;
    procedure setNull(const APropertyIndex : PtrUInt);overload;
    procedure setNull(const AProperty : ISDOProperty);overload;

    function isNull(const APath : string) : Boolean;overload;
    function isNull(const APropertyIndex : PtrUInt) : Boolean;overload;
    function isNull(const AProperty : ISDOProperty) : Boolean;overload;


    {  isSet test whether the value has been set
     *
     * Returns whether a property of either this object or an object reachable 
     * from it, as identified by the specified path,
     * is considered to be set.
     }
    function isSet(const APath : string) : Boolean;overload;
    function isSet(const APropertyIndex : PtrUInt) : Boolean;overload;
    function isSet(const AProperty : ISDOProperty) : Boolean;overload;


    { unset unsets a value previously set.
     *
     * unsets a property of either this object or an object reachable 
     * from it, as identified by the specified path.
     }

    procedure unset(const APath : string);overload;
    procedure unset(const APropertyIndex : PtrUInt);overload;
    procedure unset(const AProperty : ISDOProperty);overload;

    { setUserData sets a reserved field in the data object.
     *
     * Each data object has precisely one 32 bit slot available to 
     * be used by applications. This is not part of the data, its
     * just a place to store anything for later retrieval. 
     }

    {virtual SDO_API void setUserData(const char* path,void* value) = 0;
    virtual SDO_API void setUserData(unsigned int propertyIndex, void* value) = 0;
    virtual SDO_API void setUserData(const Property& property, void* value) = 0;
    virtual SDO_API void setUserData(void* value) = 0;
    virtual SDO_API void* getUserData(const char* path) = 0;
    virtual SDO_API void* getUserData(unsigned int propertyIndex) = 0;
    virtual SDO_API void* getUserData(const Property& property) = 0;
    virtual SDO_API void* getUserData() = 0;
    }

    {  getSequence returns the sequence for a data object
     *
     * Returns the value of a Sequence property identified by 
     * the specified path. See Sequence.
     }

    {virtual SDO_API SequencePtr getSequence() = 0;
    virtual SDO_API SequencePtr getSequence(const char* path) = 0;
    virtual SDO_API SequencePtr getSequence(unsigned int propertyIndex) = 0;
    virtual SDO_API SequencePtr getSequence(const Property& property) = 0;
    }


    {  createDataObject creates a data object value
     *
     * Returns a new data object contained by this object using the 
     * specified property,which must be a containment property.
     * The type of the created object is the declared type
     * of the specified property.
     * If the property is many valued, this method adds an element to the
     * list, otherwise it sets the value, removing any old value.
     }
    function createDataObject(const APath : string) : ISDODataObject; overload;
    function createDataObject(const APropertyIndex : PtrUInt) : ISDODataObject; overload;
    function createDataObject(const AProperty : ISDOProperty) : ISDODataObject; overload;

    {  detach detaches an object from the graph
     *
     * This method removes the current data object from the graph, but does
     * not destroy it. The DataObject can be re-attached to the graph later.
     }

    //procedure detach();

    {  clear unsets all the properties
     *
     * This method unsets all the properties, and deletes all the data object 
     * propertiy values from this data object.
     }
 
    procedure clear();


      {  getList gets the value of a many-valued property
     *
     * Many valued properties are returned as lists of DataObjects.
     * These lists may contain primitives or data objects, but they behave
     * like data objects.
     * Getting a many valued integer consists of getting the list, then
     * using the DataObjectList API to getInteger() for each list element.
     }
    function getList(const APath : string) : ISDODataObjectList; overload;
    function getList(const APropertyIndex : PtrUInt) : ISDODataObjectList; overload;
    function getList(const AProperty : ISDOProperty) : ISDODataObjectList; overload;
    {
    virtual DataObjectList& getList() = 0;
    }

    {  getChangeSummary get the applicable change summary
     *
     * This method gets the applicable change summary for a data object.
     * The summary is not necessarily attached to the data object, it may be
     * the summary for a parent data object. No object with a summary attached
     * may be a child of another object with a summary attached.
     * See the ChangeSummary API for details of using the change sumamry.
     }
    function getChangeSummary() : ISDOChangeSummary;overload;
    {function getChangeSummary(const APath : string) : ISDOChangeSummary;overload;
    function getChangeSummary(const APropIndex : PtrUInt) : ISDOChangeSummary;overload;
    function getChangeSummary(const AProp : ISDOProperty ) : ISDOChangeSummary;overload;}

    {  objectToXPath - utility to find the xpath from the root.
     *
     * objectToXPath returns a string which could be used to locate this data
     * object from the root data object of the graph.
     }

    //virtual SDO_SPI const char* objectToXPath() = 0;

  end;


  ISDOCursorBookmark = TLinkedListBookmark;
  ISDOCursor = ILinkedListCursor;
  ISDODataObjectList = interface
    ['{54C2BFCE-60B4-492B-A7EF-0B43BC994840}']

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

    {These operations use the cursor location}
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

    {These operations use the cursor location}
    procedure delete();overload;
    procedure delete(const AIndex : PtrInt);overload;

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
    procedure setDataObject(const AIndex : PtrInt; AValue : ISDODataObject);overload;
    procedure setVariant(const AIndex : PtrInt; const AValue : TSDOVariant);overload;

    procedure insert(const AIndex : PtrInt; AValue : ISDODataObject);overload;
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

    procedure append(AValue : ISDODataObject);overload;
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
  end;

  IXSDHelper = interface
    ['{D8E3D8F1-E8AE-4EE8-B60E-1235C72DD532}']
    procedure LoadFromStream(AStream : TStream);
    procedure LoadFromFile(const AFileName : string);
    procedure LoadFromString(const ASchemaString : string);

    procedure Generate(
            ATypeList : ISDOTypeList;
            ADestStream : TStream;
      const ATargetNamespace : string
    );overload;
    procedure Generate(
            ATypeList : ISDOTypeList;
      const ATargetNamespace : string;
      const AFileName : string
    );overload;
    function Generate(
            ATypeList : ISDOTypeList;
      const ATargetNamespace : string
    ) : string;overload;
    procedure GenerateCode(
            ATypeList : ISDOTypeList;
            ADestStream : TStream;
      const ATargetNamespace : string
    );overload;
    function GenerateCode(
            ATypeList : ISDOTypeList;
      const ATargetNamespace : string
    ) : string;overload;
    procedure GenerateCode(
            ATypeList : ISDOTypeList;
      const ATargetNamespace : string;
      const AFileName : string
    );overload;
    function  GetDataFactory() : ISDODataFactory;
  end;
  
  ISDOSerializer = interface
    ['{AE41D827-E606-43A4-A56C-22CBFCA5F197}']
    procedure save(
      const AName : string;
            AObject : ISDODataObject;
      const ADestStream : TStream
    );overload;
    procedure save(
            AObject : ISDODataObject;
      const ADestStream : TStream
    );overload;
    procedure save(
      const AName : string;
            AObject : ISDODataObject;
      const AFileName : string
    );overload;
    procedure save(
            AObject : ISDODataObject;
      const AFileName : string
    );overload;
    procedure load(
      const AStream : TStream;
            ADestList : ISDODataObjectList
    );overload;
    procedure load(
      const AFileName : string;
            ADestList : ISDODataObjectList
    );overload;
    function load(const AStream : TStream) : ISDODataObject;overload;
    function load(const AFileName : string) : ISDODataObject;overload;

    // these are the implementation's extensions
    function getOptions() : TSerializerOptions;
    procedure setOptions(const AValue : TSerializerOptions);
    property Options : TSerializerOptions read getOptions write setOptions;
  end;

  ISDOChangedDataObjectList = interface
    ['{E4E4A9F1-5287-44B6-876B-1A128E70BDA9}']
    function size() : PtrInt;
    function getType(const AIndex : PtrInt) : TChangeType;
    function getDataObject(const AIndex : PtrInt) : ISDODataObject;
  end;

  TValueBuffer = packed record
    case TSDOTypeKind of
      BooleanType     : ( BooleanValue : TSDOBoolean );
      ByteType        : ( ByteValue : TSDOByte );
{$IFDEF HAS_SDO_BYTES}
      BytesType       : ( BytesValue : PSDOBytes );
{$ENDIF HAS_SDO_BYTES}
{$IFDEF HAS_SDO_CHAR}
      CharacterType   : ( CharValue : TSDOChar );
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
      CurrencyType    : ( CurrencyValue : TSDOCurrency );
{$ENDIF HAS_SDO_CURRENCY}
      DateTimeType    : ( DateValue : TSDODateTime );
{$IFDEF HAS_SDO_DOUBLE}
      DoubleType      : ( DoubleValue : TSDODouble );
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
      FloatType       : ( FloatValue : TSDOFloat );
{$ENDIF HAS_SDO_FLOAT}
      IntegerType     : ( IntegerValue : TSDOInteger );
{$IFDEF HAS_SDO_LONG}
      LongType        : ( LongValue : TSDOLong );
{$ENDIF HAS_SDO_LONG}      
      ObjectType      : ( ObjectValue : PSDODataObject );
{$IFDEF HAS_SDO_SHORT}
      ShortType       : ( ShortValue : TSDOShort );
{$ENDIF HAS_SDO_SHORT}      
      StringType      : ( StringValue : PSDOString ); 
  end;
  TValueSetting = class
  private
    FSet : Boolean;
    FNull : Boolean;
    FProperty : ISDOProperty;
    FIndex : PtrInt;
    FValue : TValueBuffer;
  private
    procedure PrepareBuffer();{$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure FreeBuffer();{$IFDEF USE_INLINE}inline;{$ENDIF}
  protected
    procedure SetValue(const ABuffer);{$IFDEF USE_INLINE}inline;{$ENDIF}
  public
    constructor Create(
      const ASet, ANull : Boolean;
      const AValue;
      const AProperty   : ISDOProperty;
			const AIndex      : PtrInt
    );
    destructor Destroy();override;
    function getProperty() : ISDOProperty;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function isSet() : Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function isNull() : Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function getIndex() : PtrInt;{$IFDEF USE_INLINE}inline;{$ENDIF}

    function getBooleanValue() : TSDOBoolean;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function getByteValue() : TSDOByte;{$IFDEF USE_INLINE}inline;{$ENDIF}
{$IFDEF HAS_SDO_CHAR}
    function getCharacterValue() : TSDOChar;{$IFDEF USE_INLINE}inline;{$ENDIF}
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
    function getCurrencyValue() : TSDOCurrency;{$IFDEF USE_INLINE}inline;{$ENDIF}
{$ENDIF HAS_SDO_CURRENCY}
{$IFDEF HAS_SDO_BYTES}
    function getBytesValue() : TSDOBytes;{$IFDEF USE_INLINE}inline;{$ENDIF}
{$ENDIF HAS_SDO_BYTES}
    function getStringValue() : TSDOString;{$IFDEF USE_INLINE}inline;{$ENDIF}
{$IFDEF HAS_SDO_SHORT}
    function getShortValue() : TSDOShort;{$IFDEF USE_INLINE}inline;{$ENDIF}
{$ENDIF HAS_SDO_SHORT}
    function getIntegerValue() : TSDOInteger;{$IFDEF USE_INLINE}inline;{$ENDIF}
{$IFDEF HAS_SDO_LONG}
    function getLongValue() : TSDOLong;{$IFDEF USE_INLINE}inline;{$ENDIF}
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_FLOAT}
    function getFloatValue() : TSDOFloat;{$IFDEF USE_INLINE}inline;{$ENDIF}
{$ENDIF HAS_SDO_FLOAT}
{$IFDEF HAS_SDO_DOUBLE}
    function getDoubleValue() : TSDODouble;{$IFDEF USE_INLINE}inline;{$ENDIF}
{$ENDIF HAS_SDO_DOUBLE}
    function getDateValue() : TSDODate;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function getDataObjectValue() : ISDODataObject;{$IFDEF USE_INLINE}inline;{$ENDIF}
  end;

  ISDOSettingList = interface
    ['{502BE24D-17C7-4537-883C-E213E4714852}']
    function size() : PtrInt;
    function getItem(const AIndex : PtrInt) : TValueSetting;
    procedure insert (const AIndex : PtrInt; const ASetting : TValueSetting);
    procedure append (const ASetting : TValueSetting);
    procedure remove (const AIndex : PtrInt);
  end;

  ISDOChangeSummary = interface
    ['{5DB0D74B-A1D8-446A-83A4-98CB35D446F2}']
    function getChangedDataObjects() : ISDOChangedDataObjectList;
    function getOldValues(const ADataObject : ISDODataObject) : ISDOSettingList;
    function getOldXpath(const ADataObject : ISDODataObject) : string;
    procedure beginLogging();
    procedure endLogging();
    function isLogging() : Boolean;
    function isCreated(const ADataObject : ISDODataObject) : Boolean;
    function isDeleted(const ADataObject : ISDODataObject) : Boolean;
    function isModified(const ADataObject : ISDODataObject) : Boolean;
    function getOldValue(const ADataObject : ISDODataObject; const AProperty : ISDOProperty) : TValueSetting;
    function getOldContainer(const ADataObject : ISDODataObject) : ISDODataObject;
    function getOldContainmentProperty(const ADataObject : ISDODataObject) : ISDOProperty;
    procedure undoChanges() ;
    //SequencePtr getOldSequence(DataObjectPtr dataObject);
  end;

  TSDOConvertHelper = class
  public
    class function BoolToByte(const AValue : TSDOBoolean) : TSDOByte;{$IFDEF USE_INLINE}inline;{$ENDIF}
{$IFDEF HAS_SDO_BYTES}
    class function BoolToBytes(const AValue : TSDOBoolean) : TSDOBytes;{$IFDEF USE_INLINE}inline;{$ENDIF}
{$ENDIF HAS_SDO_BYTES}
{$IFDEF HAS_SDO_CHAR}
    class function BoolToChar(const AValue : TSDOBoolean) : TSDOChar;{$IFDEF USE_INLINE}inline;{$ENDIF}
{$ENDIF HAS_SDO_CHAR}
    class function BoolToInteger(const AValue : TSDOBoolean) : TSDOInteger;{$IFDEF USE_INLINE}inline;{$ENDIF}
{$IFDEF HAS_SDO_LONG}
    class function BoolToLong(const AValue : TSDOBoolean) : TSDOLong;{$IFDEF USE_INLINE}inline;{$ENDIF}
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
    class function BoolToShort(const AValue : TSDOBoolean) : TSDOShort;{$IFDEF USE_INLINE}inline;{$ENDIF}
    class function BoolToString(const AValue : TSDOBoolean) : TSDOString;{$IFDEF USE_INLINE}inline;{$ENDIF}
{$ENDIF HAS_SDO_SHORT}

    class function IntegerToBool(const AValue : TSDOInteger) : TSDOBoolean; {$IFDEF USE_INLINE}inline;{$ENDIF}
{$IFDEF HAS_SDO_CHAR}
    class function IntegerToChar(const AValue : TSDOInteger) : TSDOChar; {$IFDEF USE_INLINE}inline;{$ENDIF}
{$ENDIF HAS_SDO_CHAR}
    class function IntegerToString(const AValue : TSDOInteger) : TSDOString; {$IFDEF USE_INLINE}inline;{$ENDIF}

    class function ByteToBool(const AValue : TSDOByte) : TSDOBoolean; {$IFDEF USE_INLINE}inline;{$ENDIF}
{$IFDEF HAS_SDO_CHAR}
    class function ByteToChar(const AValue : TSDOByte) : TSDOChar; {$IFDEF USE_INLINE}inline;{$ENDIF}
{$ENDIF HAS_SDO_CHAR}
    class function ByteToString(const AValue : TSDOByte) : TSDOString; {$IFDEF USE_INLINE}inline;{$ENDIF}

    class function StringToBool(const AValue : TSDOString) : TSDOBoolean; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function StringToByte(const AValue : TSDOString) : TSDOByte; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function StringToBytes(const AValue : TSDOString) : TSDOBytes; {$IFDEF USE_INLINE}inline;{$ENDIF}
{$IFDEF HAS_SDO_CHAR}
    class function StringToChar(const AValue : TSDOString) : TSDOChar; {$IFDEF USE_INLINE}inline;{$ENDIF}
{$ENDIF HAS_SDO_CHAR}
    class function StringToDate(const AValue : TSDOString) : TSDODate; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function StringToInteger(const AValue : TSDOString) : TSDOInteger; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ $IFDEF HAS_SDO_FLOAT}
    class function StringToFloat(const AValue : TSDOString) : Extended; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ $ENDIF HAS_SDO_FLOAT}
{$IFDEF HAS_SDO_LONG}
    class function StringToLong(const AValue : TSDOString) : TSDOLong; {$IFDEF USE_INLINE}inline;{$ENDIF}
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
    class function StringToShort(const AValue : TSDOString) : TSDOShort; {$IFDEF USE_INLINE}inline;{$ENDIF}
{$ENDIF HAS_SDO_SHORT}

{ $IFDEF HAS_SDO_FLOAT}
    class function FloatToString(const AValue : Extended) : TSDOString; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function FloatToBool(const AValue : Extended) : TSDOBoolean; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ $ENDIF HAS_SDO_FLOAT}

    class function DateToString(const AValue : TSDODate) : TSDOString; {$IFDEF USE_INLINE}inline;{$ENDIF}

{$IFDEF HAS_SDO_CHAR}
    class function CharToBool(const AValue : TSDOChar) : TSDOBoolean; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function CharToByte(const AValue : TSDOChar) : TSDOByte; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function CharToInteger(const AValue : TSDOChar) : TSDOInteger; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function CharToLong(const AValue : TSDOChar) : TSDOLong; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function CharToShort(const AValue : TSDOChar) : TSDOShort; {$IFDEF USE_INLINE}inline;{$ENDIF}
{$ENDIF HAS_SDO_CHAR}

{$IFDEF HAS_SDO_LONG}
    class function LongToBool(const AValue : TSDOLong) : TSDOBoolean; {$IFDEF USE_INLINE}inline;{$ENDIF}
  {$IFDEF HAS_SDO_CHAR}
    class function LongToChar(const AValue : TSDOLong) : TSDOChar; {$IFDEF USE_INLINE}inline;{$ENDIF}
  {$ENDIF HAS_SDO_CHAR}
    class function LongToString(const AValue : TSDOLong) : TSDOString; {$IFDEF USE_INLINE}inline;{$ENDIF}
{$ENDIF HAS_SDO_LONG}

{$IFDEF HAS_SDO_SHORT}
    class function ShortToBool(const AValue : TSDOShort) : TSDOBoolean; {$IFDEF USE_INLINE}inline;{$ENDIF}
  {$IFDEF HAS_SDO_CHAR}
    class function ShortToChar(const AValue : TSDOShort) : TSDOChar; {$IFDEF USE_INLINE}inline;{$ENDIF}
  {$ENDIF HAS_SDO_CHAR}
    class function ShortToString(const AValue : TSDOShort) : TSDOString; {$IFDEF USE_INLINE}inline;{$ENDIF}
{$ENDIF HAS_SDO_SHORT}

    class function BytesToString(const AValue : TSDOBytes) : TSDOString; {$IFDEF USE_INLINE}inline;{$ENDIF}
{$IFDEF HAS_SDO_CURRENCY}
    class function CurrencyToString(const AValue : TSDOCurrency) : TSDOString; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function StringToCurrency(const AValue : TSDOString) : TSDOCurrency; {$IFDEF USE_INLINE}inline;{$ENDIF}
{$ENDIF HAS_SDO_CURRENCY}
  end;
  TSDOConvertHelperClass = class of TSDOConvertHelper;

  TSDOCopyHelper = class
  private
    class procedure copyProperty(const A, B : ISDODataObject; const AProp : ISDOProperty);{$IFDEF USE_INLINE}inline;{$ENDIF}
    class procedure copyPropertyList(const A, B : ISDODataObjectList; const AType : ISDOType; const ADeepCopy : Boolean);{$IFDEF USE_INLINE}inline;{$ENDIF}
    class function internalCopy(const ADataObject : ISDODataObject; const ADeepCopy : Boolean) : ISDODataObject;
  public
    class function copyShallow(const ADataObject : ISDODataObject) : ISDODataObject;{$IFDEF USE_INLINE}inline;{$ENDIF}
    class function copy(const ADataObject : ISDODataObject) : ISDODataObject;{$IFDEF USE_INLINE}inline;{$ENDIF}
  end;
  TSDOCopyHelperClass = class of TSDOCopyHelper;

  //implementation extension
  procedure CopySimpleList(
    const ASource, ADest : ISDODataObjectList;
    const AType : TSDOTypeKind
  );

type

  TSDOEqualityHelper = class
  private
    class function CompareProperty(const A,B : ISDODataObject; const AProp : ISDOProperty) : Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}
    class function CompareList(const A,B : ISDODataObjectList; const AType : ISDOType) : Boolean;
    class function InternalEqual(const A,B : ISDODataObject; const AFullCompare : Boolean) : Boolean;
  public
    class function equalShallow(const A,B : ISDODataObject) : Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}
    class function equal(const A,B : ISDODataObject) : Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}
  end;
  TSDOEqualityHelperClass = class of TSDOEqualityHelper;

const
  NIL_OBJECT : ISDODataObject = nil;

  procedure InitBufferResources(const ADataType : TSDOTypeKind; var ABuffer : TValueBuffer);{$IFDEF USE_INLINE}inline;{$ENDIF}
  procedure FreeBufferResources(const ADataType : TSDOTypeKind; var ABuffer : TValueBuffer);{$IFDEF USE_INLINE}inline;{$ENDIF}

implementation

uses
  StrUtils;

procedure InitBufferResources(const ADataType : TSDOTypeKind; var ABuffer : TValueBuffer);
begin
  case ADataType of
{$IFDEF HAS_SDO_BYTES}
    BytesType     : New(ABuffer.BytesValue);
{$ENDIF HAS_SDO_BYTES}
    ObjectType    : New(ABuffer.ObjectValue);
    StringType   : New(ABuffer.StringValue);
  end;
end;

procedure FreeBufferResources(const ADataType : TSDOTypeKind; var ABuffer : TValueBuffer);
begin
  case ADataType of
{$IFDEF HAS_SDO_BYTES}
    BytesType     :
      begin
        ABuffer.BytesValue^ := nil;
        Dispose(ABuffer.BytesValue);
        ABuffer.BytesValue := nil;
      end;
{$ENDIF HAS_SDO_BYTES}
    ObjectType    :
      begin
        ABuffer.ObjectValue^ := nil;
        Dispose(ABuffer.ObjectValue);
        ABuffer.ObjectValue := nil;
      end;
    StringType   :
      begin
        ABuffer.StringValue^ := '';
        Dispose(ABuffer.StringValue);
        ABuffer.StringValue := nil;
      end;
  end;
end;

{ ESDOPropertyNotFoundException }

constructor ESDOPropertyNotFoundException.Create(const APropName: string);
begin
  inherited CreateFmt('Property not found : %s.',[APropName]);
  FPropertyName := APropName;
end;

{ ESDOUnsupportedOperationException }

constructor ESDOUnsupportedOperationException.Create(const AOperation: string);
begin
  inherited CreateFmt('Unsupported Operation : %s.',[AOperation]);
  FOperation := AOperation;
end;

{ ESDOIllegalArgumentException }

constructor ESDOIllegalArgumentException.Create(const AArgumentName: string);
begin
  inherited CreateFmt('Illegal Argument : %s.',[AArgumentName]);
  FArgumentName := AArgumentName;
end;

{ ESDOIndexOutOfRangeException }

constructor ESDOIndexOutOfRangeException.Create(const AValue: Integer);
begin
  inherited CreateFmt('Index out of range : %d',[AValue]);
  FValue := AValue;
end;

{ ESDOTypeNotFoundException }

constructor ESDOTypeNotFoundException.Create(const AName: string);
begin
  inherited CreateFmt('Type not found : %s.',[AName]);
  FName := AName;
end;

{ ESDOIncompleteTypeException }

constructor ESDOIncompleteTypeException.Create(const ATypeName: string);
begin
  inherited CreateFmt('Incomplete Type definition : %s.',[ATypeName]);
  FTypeName := ATypeName;
end;

{ ESDODuplicatedItemException }

constructor ESDODuplicatedItemException.Create(const AName: string);
begin
  inherited CreateFmt('Duplicated item : %s.',[AName]);
  FName := AName;
end;

{ ESDOInvalidConversionException }

constructor ESDOInvalidConversionException.Create(const AName: string);
begin
  inherited CreateFmt('Invalid conversion : %s.',[AName]);
  FName := AName;
end;

{ TValueSetting }

constructor TValueSetting.Create(
  const ASet, ANull : Boolean;
  const AValue;
  const AProperty: ISDOProperty;
  const AIndex: PtrInt
);
begin
  if ( AProperty = nil ) then
    raise ESDOIllegalArgumentException.Create('AProperty');
  FSet := ASet;
  FNull := ANull;
  FProperty := AProperty;
  FIndex := AIndex;
  PrepareBuffer();
  if FSet and ( not FNull ) then
    SetValue(AValue);
end;

destructor TValueSetting.Destroy();
begin
  if Assigned(FProperty) then begin
    FreeBuffer();
  end;
  inherited;
end;

procedure TValueSetting.FreeBuffer();
begin
  FreeBufferResources(FProperty.getTypeEnum(), FValue);
end;

function TValueSetting.getBooleanValue() : TSDOBoolean;
begin
  case FProperty.getTypeEnum() of
    BooleanType   : Result := FValue.BooleanValue;
    ByteType      : Result := TSDOConvertHelper.ByteToBool(FValue.ByteValue);
{$IFDEF HAS_SDO_CHAR}
    CharacterType : Result := TSDOConvertHelper.CharToBool(FValue.CharValue);
{$ENDIF HAS_SDO_CHAR}
    IntegerType   : Result := TSDOConvertHelper.IntegerToBool(FValue.IntegerValue);
{$IFDEF HAS_SDO_LONG}
    LongType      : Result := TSDOConvertHelper.LongToBool(FValue.LongValue);
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
    ShortType     : Result := TSDOConvertHelper.ShortToBool(FValue.ShortValue);
{$ENDIF HAS_SDO_SHORT}
    StringType   : Result := TSDOConvertHelper.StringToBool(FValue.StringValue^);
    else
      raise ESDONotImplementedException.Create('getBooleanValue() this with DataType.');
  end;
end;

{$IFDEF HAS_SDO_BYTES}
function TValueSetting.getBytesValue() : TSDOBytes;
begin
  case FProperty.getTypeEnum() of
    BooleanType   : Result := TSDOConvertHelper.BoolToBytes(FValue.BooleanValue);
    BytesType     : Result := FValue.BytesValue^;
    StringType    : Result := TSDOConvertHelper.StringToBytes(FValue.StringValue^);
    else
      raise ESDONotImplementedException.Create('getBytesValue() this with DataType.');
  end;
end;
{$ENDIF HAS_SDO_BYTES}

function TValueSetting.getByteValue() : TSDOByte;
begin
  case FProperty.getTypeEnum() of
    BooleanType   : Result := TSDOConvertHelper.BoolToByte(FValue.BooleanValue);
    ByteType      : Result := FValue.ByteValue;
{$IFDEF HAS_SDO_CHAR}
    CharacterType : Result := TSDOConvertHelper.CharToByte(FValue.CharValue);
{$ENDIF HAS_SDO_CHAR}
    IntegerType   : Result := FValue.IntegerValue;
{$IFDEF HAS_SDO_LONG}
    LongType      : Result := FValue.LongValue;
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
    ShortType     : Result := FValue.ShortValue;
{$ENDIF HAS_SDO_SHORT}
    StringType   : Result := TSDOConvertHelper.StringToByte(FValue.StringValue^);
    else
      raise ESDONotImplementedException.Create('getByteValue() this with DataType.');
  end;
end;

{$IFDEF HAS_SDO_CHAR}
function TValueSetting.getCharacterValue() : TSDOChar;
begin
  case FProperty.getTypeEnum() of
    BooleanType   : Result := TSDOConvertHelper.BoolToChar(FValue.BooleanValue);
    ByteType      : Result := TSDOConvertHelper.ByteToChar(FValue.ByteValue);
{$IFDEF HAS_SDO_CHAR}
    CharacterType : Result := FValue.CharValue;
{$ENDIF HAS_SDO_CHAR}
    IntegerType   : Result := TSDOConvertHelper.IntegerToChar(FValue.IntegerValue);
{$IFDEF HAS_SDO_LONG}
    LongType      : Result := TSDOConvertHelper.LongToChar(FValue.LongValue);
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
    ShortType     : Result := TSDOConvertHelper.ShortToChar(FValue.ShortValue);
{$ENDIF HAS_SDO_SHORT}
    StringType   : Result := TSDOConvertHelper.StringToChar(FValue.StringValue^);
    else
      raise ESDONotImplementedException.Create('getCharacterValue() this with DataType.');
  end;
end;
{$ENDIF HAS_SDO_CHAR}

{$IFDEF HAS_SDO_CURRENCY }
function TValueSetting.getCurrencyValue() : TSDOCurrency;
begin
  case FProperty.getTypeEnum() of
    ByteType      : Result := FValue.ByteValue;
    CurrencyType  : Result := FValue.CurrencyValue;
{$IFDEF HAS_SDO_DOUBLE}
    DoubleType    : Result := FValue.DoubleValue;
{$ENDIF HAS_SDO_DOUBLE}
    IntegerType   : Result := FValue.IntegerValue;
{$IFDEF HAS_SDO_FLOAT}
    FloatType     : Result := FValue.FloatValue;
{$ENDIF HAS_SDO_FLOAT}
{$IFDEF HAS_SDO_LONG}
    LongType      : Result := FValue.LongValue;
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
    ShortType     : Result := FValue.ShortValue;
{$ENDIF HAS_SDO_SHORT}
    StringType    : Result := TSDOConvertHelper.StringToInteger(FValue.StringValue^);
    else
      raise ESDONotImplementedException.Create('getFloatValue() this with DataType.');
  end;
end;
{$ENDIF HAS_SDO_CURRENCY }

function TValueSetting.getDataObjectValue() : ISDODataObject;
begin
  if ( FProperty.getTypeEnum() = ObjectType ) then
    Result := FValue.ObjectValue^
  else
    raise ESDONotImplementedException.Create('getDataObjectValue() this with DataType.');;
end;

function TValueSetting.getDateValue() : TSDODate;
begin
  case FProperty.getTypeEnum() of
    DateTimeType  : Result := FValue.DateValue;
    StringType    : Result := TSDOConvertHelper.StringToDate(FValue.StringValue^);
    else
      raise ESDONotImplementedException.Create('getDateValue() this with DataType.');
  end;
end;

{$IFDEF HAS_SDO_DOUBLE}
function TValueSetting.getDoubleValue() : TSDODouble;
begin
  case FProperty.getTypeEnum() of
    ByteType      : Result := FValue.ByteValue;
{$IFDEF HAS_SDO_CURRENCY }
    CurrencyType  : Result := FValue.CurrencyValue;
{$ENDIF HAS_SDO_CURRENCY }
    DoubleType    : Result := FValue.DoubleValue;
    IntegerType   : Result := FValue.IntegerValue;
{$IFDEF HAS_SDO_FLOAT}
    FloatType     : Result := FValue.FloatValue;
{$ENDIF HAS_SDO_FLOAT}
{$IFDEF HAS_SDO_LONG}
    LongType      : Result := FValue.LongValue;
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
    ShortType     : Result := FValue.ShortValue;
{$ENDIF HAS_SDO_SHORT}
    StringType    : Result := TSDOConvertHelper.StringToInteger(FValue.StringValue^);
    else
      raise ESDONotImplementedException.Create('getFloatValue() this with DataType.');
  end;
end;
{$ENDIF HAS_SDO_DOUBLE}

{$IFDEF HAS_SDO_FLOAT}
function TValueSetting.getFloatValue() : TSDOFloat;
begin
  case FProperty.getTypeEnum() of
    ByteType      : Result := FValue.ByteValue;
{$IFDEF HAS_SDO_CURRENCY }
    CurrencyType  : Result := FValue.CurrencyValue;
{$ENDIF HAS_SDO_CURRENCY }
{$IFDEF HAS_SDO_DOUBLE}
    DoubleType    : Result := FValue.DoubleValue;
{$ENDIF HAS_SDO_DOUBLE}
    IntegerType   : Result := FValue.IntegerValue;
    FloatType     : Result := FValue.FloatValue;
{$IFDEF HAS_SDO_LONG}
    LongType      : Result := FValue.LongValue;
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
    ShortType     : Result := FValue.ShortValue;
{$ENDIF HAS_SDO_SHORT}
    StringType    : Result := TSDOConvertHelper.StringToInteger(FValue.StringValue^);
    else
      raise ESDONotImplementedException.Create('getFloatValue() this with DataType.');
  end;
end;
{$ENDIF HAS_SDO_FLOAT}

function TValueSetting.getIndex() : PtrInt;
begin
  Result := FIndex;
end;

function TValueSetting.getIntegerValue() : TSDOInteger;
begin
  case FProperty.getTypeEnum() of
    BooleanType   : Result := TSDOConvertHelper.BoolToInteger(FValue.BooleanValue);
    ByteType      : Result := FValue.ByteValue;
{$IFDEF HAS_SDO_CHAR}
    CharacterType : Result := TSDOConvertHelper.CharToInteger(FValue.CharValue);
{$ENDIF HAS_SDO_CHAR}
    IntegerType   : Result := FValue.IntegerValue;
{$IFDEF HAS_SDO_LONG}
    LongType      : Result := FValue.LongValue;
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
    ShortType     : Result := FValue.ShortValue;
{$ENDIF HAS_SDO_SHORT}
    StringType    : Result := TSDOConvertHelper.StringToInteger(FValue.StringValue^);
    else
      raise ESDONotImplementedException.Create('getIntegerValue() this with DataType.');
  end;
end;

{$IFDEF HAS_SDO_LONG}
function TValueSetting.getLongValue() : TSDOLong;
begin
  case FProperty.getTypeEnum() of
    BooleanType   : Result := TSDOConvertHelper.BoolToLong(FValue.BooleanValue);
    ByteType      : Result := FValue.ByteValue;
{$IFDEF HAS_SDO_CHAR}
    CharacterType : Result := TSDOConvertHelper.CharToLong(FValue.CharValue);
{$ENDIF HAS_SDO_CHAR}
    IntegerType   : Result := FValue.IntegerValue;
{$IFDEF HAS_SDO_LONG}
    LongType      : Result := FValue.LongValue;
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
    ShortType     : Result := FValue.ShortValue;
{$ENDIF HAS_SDO_SHORT}
    StringType    : Result := TSDOConvertHelper.StringToLong(FValue.StringValue^);
    else
      raise ESDONotImplementedException.Create('getLongValue() this with DataType.');
  end;
end;
{$ENDIF HAS_SDO_LONG}

function TValueSetting.getProperty() : ISDOProperty;
begin
  Result := FProperty;
end;

{$IFDEF HAS_SDO_SHORT}
function TValueSetting.getShortValue() : TSDOShort;
begin
  case FProperty.getTypeEnum() of
    BooleanType   : Result := TSDOConvertHelper.BoolToShort(FValue.BooleanValue);
    ByteType      : Result := FValue.ByteValue;
{$IFDEF HAS_SDO_CHAR}
    CharacterType : Result := TSDOConvertHelper.CharToShort(FValue.CharValue);
{$ENDIF HAS_SDO_CHAR}
    IntegerType   : Result := FValue.IntegerValue;
{$IFDEF HAS_SDO_LONG}
    LongType      : Result := FValue.LongValue;
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
    ShortType     : Result := FValue.ShortValue;
{$ENDIF HAS_SDO_SHORT}
    StringType    : Result := TSDOConvertHelper.StringToShort(FValue.StringValue^);
    else
      raise ESDONotImplementedException.Create('getShortValue() this with DataType.');
  end;
end;
{$ENDIF HAS_SDO_SHORT}

function TValueSetting.getStringValue() : TSDOString;
begin
  case FProperty.getTypeEnum() of
    BooleanType   : Result := TSDOConvertHelper.BoolToString(FValue.BooleanValue);
    ByteType      : Result := TSDOConvertHelper.ByteToString(FValue.ByteValue);
{$IFDEF HAS_SDO_BYTES}
    BytesType     : Result := TSDOConvertHelper.BytesToString(FValue.BytesValue^);
{$ENDIF HAS_SDO_BYTES}
{$IFDEF HAS_SDO_CHAR}
    CharacterType : Result := FValue.CharValue;
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
    CurrencyType  : Result := TSDOConvertHelper.CurrencyToString(FValue.CurrencyValue);
{$ENDIF HAS_SDO_CURRENCY}
    DateTimeType  : Result := TSDOConvertHelper.DateToString(FValue.DateValue);
{$IFDEF HAS_SDO_DOUBLE}
    DoubleType    : Result := TSDOConvertHelper.FloatToString(FValue.DoubleValue);
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
    FloatType     : Result := TSDOConvertHelper.FloatToString(FValue.FloatValue);
{$ENDIF HAS_SDO_FLOAT}
    IntegerType   : Result := TSDOConvertHelper.IntegerToString(FValue.IntegerValue);
{$IFDEF HAS_SDO_LONG}
    LongType      : Result := TSDOConvertHelper.LongToString(FValue.LongValue);
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
    ShortType     : Result := TSDOConvertHelper.ShortToString(FValue.ShortValue);
{$ENDIF HAS_SDO_SHORT}
    StringType    : Result := FValue.StringValue^;
    else
      raise ESDONotImplementedException.Create('getStringValue() this with DataType.');
  end;
end;

function TValueSetting.isNull() : Boolean;
begin
  Result := FNull;
end;

function TValueSetting.isSet() : Boolean;
begin
  Result := FSet;
end;

procedure TValueSetting.PrepareBuffer();
begin
  case FProperty.getTypeEnum() of
{$IFDEF HAS_SDO_BYTES}  
    BytesType     : New(FValue.BytesValue);
{$ENDIF HAS_SDO_BYTES}
    ObjectType    : New(FValue.ObjectValue);
    StringType    : New(FValue.StringValue);
  end;
end;

procedure TValueSetting.SetValue(const ABuffer);
begin
  case FProperty.getTypeEnum() of
    BooleanType   : FValue.BooleanValue := TSDOBoolean(ABuffer);
    ByteType      : FValue.ByteValue := TSDOByte(ABuffer);
{$IFDEF HAS_SDO_BYTES}
    BytesType     : FValue.BytesValue^ := TSDOBytes(ABuffer);
{$ENDIF HAS_SDO_BYTES}
{$IFDEF HAS_SDO_CHAR}
    CharacterType : FValue.CharValue := TSDOChar(ABuffer);
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
    CurrencyType  : FValue.CurrencyValue := TSDOCurrency(ABuffer);
{$ENDIF HAS_SDO_CURRENCY}
    DateTimeType  : FValue.DateValue := TSDODate(ABuffer);
{$IFDEF HAS_SDO_DOUBLE}
    DoubleType    : FValue.DoubleValue := TSDODouble(ABuffer);
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
    FloatType     : FValue.FloatValue := TSDOFloat(ABuffer);
{$ENDIF HAS_SDO_FLOAT}
    IntegerType   : FValue.IntegerValue := TSDOInteger(ABuffer);
{$IFDEF HAS_SDO_LONG}
    LongType      : FValue.LongValue := TSDOLong(ABuffer);
{$ENDIF HAS_SDO_LONG}
    ObjectType    : FValue.ObjectValue^ := ISDODataObject(ABuffer);
{$IFDEF HAS_SDO_SHORT}
    ShortType     : FValue.ShortValue := TSDOShort(ABuffer);
{$ENDIF HAS_SDO_SHORT}    
    StringType    : FValue.StringValue^ := TSDOString(ABuffer);
  end;
end;

{ ESDOCircularDependencyTypeException }

constructor ESDOCircularDependencyTypeException.Create(
  const ATypeName,
        APropertyName,
        APropertyType: string
);
begin
  inherited Create(APropertyName);
  Message := Format(
               'A circular dependendy has been found the type definition ' + sLineBreak +
               '  Type name : "%s"' + sLineBreak +
               '  Property name : "%s"' + sLineBreak +
               '  Property type : "%s"',
               [ATypeName,APropertyName,APropertyType]
             );
  FTypeName := ATypeName;
  FPropertyType := APropertyType;
  FPropertyName := APropertyName; 
end;

{ ESDOInvalidPathException }

constructor ESDOInvalidPathException.Create(const APath: string);
begin
  FPath := APath;
end;

var
  ConverterFormatSetting : TFormatSettings;
function sdo_TryStrToInt(const s: string; out i : Int64) : Boolean;overload;{$IFDEF USE_INLINE}inline;{$ENDIF}
var
  e: Integer;
begin
  Val(s,i,e);
  Result := ( e = 0 );
end;

function sdo_TryStrToInt(const s: string; out i : Integer) : Boolean;overload;{$IFDEF USE_INLINE}inline;{$ENDIF}
var
  e: Integer;
begin
  Val(s,i,e);
  Result := ( e = 0 );
end;

function sdo_TryStrToInt(const s: string; out i : Smallint) : Boolean;overload;{$IFDEF USE_INLINE}inline;{$ENDIF}
var
  e: Integer;
begin
  Val(s,i,e);
  Result := ( e = 0 );
end;

function sdo_TryStrToInt(const s: string; out i : TSDOByte) : Boolean;overload;{$IFDEF USE_INLINE}inline;{$ENDIF}
var
  e: Integer;
begin
  Val(s,i,e);
  Result := ( e = 0 );
end;

{ TSDOConvertHelper }

class function TSDOConvertHelper.BoolToByte(const AValue: TSDOBoolean): TSDOByte;
begin
  if AValue then
    Result := 1
  else
    Result := 0;
end;

{$IFDEF HAS_SDO_BYTES}
class function TSDOConvertHelper.BoolToBytes(const AValue: TSDOBoolean): TSDOBytes;
const
  ITEM_LENGTH = 5;
  STR_BOOL : array[TSDOBoolean] of String[ITEM_LENGTH] = ( 'false', 'true' );
begin
  SetLength(Result,Length(STR_BOOL[AValue]));
  Move(STR_BOOL[AValue][0],Result[0],ITEM_LENGTH);
end;
{$ENDIF HAS_SDO_BYTES}

{$IFDEF HAS_SDO_CHAR}
class function TSDOConvertHelper.BoolToChar(const AValue: TSDOBoolean) : TSDOChar;
begin
  if AValue then
    Result := '1'
  else
    Result := '0';
end;
{$ENDIF HAS_SDO_CHAR}

class function TSDOConvertHelper.BoolToInteger(const AValue: TSDOBoolean): TSDOInteger;
begin
  Result := BoolToByte(AValue)
end;

class function TSDOConvertHelper.BoolToLong(const AValue: TSDOBoolean): TSDOLong;
begin
  Result := BoolToByte(AValue);
end;

class function TSDOConvertHelper.BoolToShort(const AValue: TSDOBoolean): TSDOShort;
begin
  Result := BoolToByte(AValue);
end;

class function TSDOConvertHelper.BoolToString(const AValue: TSDOBoolean): TSDOString;
begin
  if AValue then
    Result := '1'
  else
    Result := '0';
end;

class function TSDOConvertHelper.BytesToString(const AValue: TSDOBytes): TSDOString;
var
  locRes : AnsiString;
begin
  if (  Length(AValue) > 0 ) then begin
    SetLength(locRes, ( 2 * Length(AValue) ) );
    BinToHex(PAnsiChar(@(AValue[0])),PAnsiChar(@(locRes[1])),Length(AValue));
    Result := locRes;
  end else begin
    Result := '';
  end;
end;

class function TSDOConvertHelper.ByteToBool(const AValue: TSDOByte): TSDOBoolean;
begin
  Result := ( AValue <> 0 );
end;

{$IFDEF HAS_SDO_CHAR}
class function TSDOConvertHelper.ByteToChar(const AValue: TSDOByte): TSDOChar;
begin
  Result := TSDOChar(AValue);
end;
{$ENDIF HAS_SDO_CHAR}

class function TSDOConvertHelper.ByteToString(const AValue: TSDOByte): TSDOString;
begin
  Result := IntToStr(AValue);
end;

{$IFDEF HAS_SDO_CHAR}
class function TSDOConvertHelper.CharToBool(const AValue: TSDOChar): TSDOBoolean;
begin
  Result := ( Ord(AValue) <> 0 );
end;

class function TSDOConvertHelper.CharToByte(const AValue: TSDOChar): TSDOByte;
begin
{$IF SizeOf(TSDOChar) = SizeOf(Byte)}
  Result := Ord(AValue);
{$ELSE}
  case Ord(AValue) of
    Low(Byte)..High(Byte) : Result := Ord(AValue);
    else
      Result := 0;
  end;
{$IFEND}
end;

class function TSDOConvertHelper.CharToInteger(const AValue: TSDOChar): TSDOInteger;
begin
  Result := Ord(AValue);
end;

class function TSDOConvertHelper.CharToLong(const AValue: TSDOChar): TSDOLong;
begin
  Result := Ord(AValue);
end;

class function TSDOConvertHelper.CharToShort(const AValue: TSDOChar): TSDOShort;
begin
  Result := Ord(AValue);
end;
{$ENDIF HAS_SDO_CHAR}

class function TSDOConvertHelper.DateToString(const AValue: TSDODate): TSDOString;
begin
  Result := xsd_DateTimeToStr(AValue,xdkDateTime);
end;

{ $IFDEF HAS_SDO_FLOAT}
class function TSDOConvertHelper.FloatToBool(const AValue: Extended): TSDOBoolean;
begin
  Result := ( AValue <> 0 );
end;

class function TSDOConvertHelper.FloatToString(const AValue: Extended) : TSDOString;
begin
  Result := FloatToStrF(AValue,ffGeneral,18,18,ConverterFormatSetting);
end;
{ $ENDIF HAS_SDO_FLOAT}

class function TSDOConvertHelper.IntegerToBool(const AValue: TSDOInteger): TSDOBoolean;
begin
  Result := ( AValue <> 0 );
end;

{$IFDEF HAS_SDO_CHAR}
class function TSDOConvertHelper.IntegerToChar(const AValue: TSDOInteger): TSDOChar;
begin
  Result := TSDOChar(AValue);
end;
{$ENDIF HAS_SDO_CHAR}

class function TSDOConvertHelper.IntegerToString(const AValue: TSDOInteger): TSDOString;
begin
  Result := IntToStr(AValue);
end;

class function TSDOConvertHelper.LongToBool(const AValue: TSDOLong): TSDOBoolean;
begin
  Result := ( AValue <> 0 );
end;

{$IFDEF HAS_SDO_LONG}
  {$IFDEF HAS_SDO_CHAR}
class function TSDOConvertHelper.LongToChar(const AValue: TSDOLong): TSDOChar;
begin
  Result := TSDOChar(AValue);
end;
  {$ENDIF HAS_SDO_CHAR}

class function TSDOConvertHelper.LongToString(const AValue: TSDOLong): TSDOString;
begin
  Result := IntToStr(AValue);
end;
{$ENDIF HAS_SDO_LONG}

class function TSDOConvertHelper.ShortToBool(const AValue: TSDOShort): TSDOBoolean;
begin
  Result := ( AValue <> 0 );
end;

{$IFDEF HAS_SDO_SHORT}
  {$IFDEF HAS_SDO_CHAR}
class function TSDOConvertHelper.ShortToChar(const AValue: TSDOShort): TSDOChar;
begin
  Result := TSDOChar(AValue);
end;
  {$ENDIF HAS_SDO_CHAR}

class function TSDOConvertHelper.ShortToString(const AValue: TSDOShort): TSDOString;
begin
  Result := IntToStr(AValue);
end;
{$ENDIF HAS_SDO_SHORT}

class function TSDOConvertHelper.StringToBool(const AValue: TSDOString): TSDOBoolean;
begin
  case AnsiIndexStr(LowerCase(AValue),['0', '1', 'false', 'true'] ) of
    0, 2 : Result := False;
    1, 3 : Result := True;
    else
      raise ESDOInvalidConversionException.Create('TSDOConvertHelper.StringToBool');
  end;
end;

class function TSDOConvertHelper.StringToByte(const AValue: TSDOString): TSDOByte;
begin
  if not sdo_TryStrToInt(AValue, Result) then
    raise ESDOInvalidConversionException.Create('TSDOConvertHelper.StringToInteger');
end;

class function TSDOConvertHelper.StringToBytes(const AValue: TSDOString): TSDOBytes;
var
  locValue : AnsiString;
begin
  if ( Length(AValue) > 0 ) then begin
    locValue := AValue;
    SetLength(Result,( Length(locValue) div 2 ));
    HexToBin(PAnsiChar(locValue),PAnsiChar(@(Result[0])),Length(Result));
  end else begin
    Result := nil;
  end;
end;

{$IFDEF HAS_SDO_CHAR}
class function TSDOConvertHelper.StringToChar(const AValue: TSDOString): TSDOChar;
begin
  if ( Length(AValue) > 0 ) then
    Result := AValue[1]
  else
    raise ESDOInvalidConversionException.Create('TSDOConvertHelper.StringToChar');
end;
{$ENDIF HAS_SDO_ENDIF}

class function TSDOConvertHelper.StringToDate(const AValue: TSDOString): TSDODate;
begin
  if not xsd_TryStrToDate(AValue,Result,xdkDateTime) then
    raise ESDOInvalidConversionException.Create('TSDOConvertHelper.StringToDate');
end;

{ $IFDEF HAS_SDO_FLOAT}
class function TSDOConvertHelper.StringToFloat(const AValue: TSDOString): Extended;
begin
  if not TryStrToFloat(AValue,Result,ConverterFormatSetting) then
    raise ESDOInvalidConversionException.Create('TSDOConvertHelper.StringToFloat');
end;
{ $ENDIF HAS_SDO_FLOAT}

class function TSDOConvertHelper.StringToInteger(const AValue: TSDOString): TSDOInteger;
begin
  if not sdo_TryStrToInt(AValue, Result) then
    raise ESDOInvalidConversionException.Create('TSDOConvertHelper.StringToInteger');
end;

{$IFDEF HAS_SDO_LONG}
class function TSDOConvertHelper.StringToLong(const AValue: TSDOString): TSDOLong;
begin
  if not sdo_TryStrToInt(AValue, Result) then
    raise ESDOInvalidConversionException.Create('TSDOConvertHelper.StringToLong');
end;
{$ENDIF HAS_SDO_LONG}

{$IFDEF HAS_SDO_SHORT}
class function TSDOConvertHelper.StringToShort(const AValue: TSDOString): TSDOShort;
begin
  if not sdo_TryStrToInt(AValue, Result) then
    raise ESDOInvalidConversionException.Create('TSDOConvertHelper.StringToShort');
end;
{$ENDIF HAS_SDO_SHORT}

{$IFDEF HAS_SDO_CURRENCY}
class function TSDOConvertHelper.CurrencyToString(const AValue: TSDOCurrency): TSDOString;
begin
  Result := CurrToStrF(AValue,ffFixed,4,ConverterFormatSetting);
end;

class function TSDOConvertHelper.StringToCurrency(const AValue: TSDOString): TSDOCurrency;
begin
  if not TryStrToCurr(AValue,Result,ConverterFormatSetting) then
    raise ESDOInvalidConversionException.Create('TSDOConvertHelper.StringToCurrency');
end;
{$ENDIF HAS_SDO_CURRENCY}



type
  TSimpleTypeCopyProc = procedure (const A,B : ISDODataObject; const AProp : ISDOProperty);
  TListTypeCopyProc = procedure (const A,B : ISDODataObjectList);
  TCopyProcRecord = record
    Simple : TSimpleTypeCopyProc;
    List   : TListTypeCopyProc;
  end;

procedure NotImplementedSimplePROC(const A,B : ISDODataObject; const AProp : ISDOProperty);
begin
  raise ESDONotImplementedException.Create('');
end;

procedure NotImplementedListPROC(const A,B : ISDODataObjectList);
begin
  raise ESDONotImplementedException.Create('');
end;

procedure CopyBoolProperty(const A,B : ISDODataObject; const AProp : ISDOProperty);
begin
  if A.isSet(AProp) then begin
    if A.isNull(AProp) then
      B.setNull(AProp)
    else
      B.setBoolean(AProp,A.getBoolean(AProp));
  end else begin
    B.unset(AProp);
  end;
end;

procedure CopyByteProperty(const A,B : ISDODataObject; const AProp : ISDOProperty);
begin
  if A.isSet(AProp) then begin
    if A.isNull(AProp) then
      B.setNull(AProp)
    else
      B.setByte(AProp,A.getByte(AProp));
  end else begin
    B.unset(AProp);
  end;
end;

{$IFDEF HAS_SDO_BYTES}
procedure CopyBytesProperty(const A,B : ISDODataObject; const AProp : ISDOProperty);
begin
  if A.isSet(AProp) then begin
    if A.isNull(AProp) then
      B.setNull(AProp)
    else
      B.setBytes(AProp,Copy(A.getBytes(AProp)));
  end else begin
    B.unset(AProp);
  end;
end;

procedure CopyListBytes(const A,B : ISDODataObjectList);
begin
  B.appendBytes(Copy(A.getBytes()));
end;
{$ENDIF HAS_SDO_BYTES}

{$IFDEF HAS_SDO_CHAR}
procedure CopyCharProperty(const A,B : ISDODataObject; const AProp : ISDOProperty);
begin
  if A.isSet(AProp) then begin
    if A.isNull(AProp) then
      B.setNull(AProp)
    else
      B.setCharacter(AProp,A.getCharacter(AProp));
  end else begin
    B.unset(AProp);
  end;
end;

procedure CopyListChar(const A,B : ISDODataObjectList);
begin
  B.append(A.getCharacter());
end;
{$ENDIF HAS_SDO_CHAR}

{$IFDEF HAS_SDO_CURRENCY}
procedure CopyCurrencyProperty(const A,B : ISDODataObject; const AProp : ISDOProperty);
begin
  if A.isSet(AProp) then begin
    if A.isNull(AProp) then
      B.setNull(AProp)
    else
      B.setCurrency(AProp,A.getCurrency(AProp));
  end else begin
    B.unset(AProp);
  end;
end;

procedure CopyListCurrency(const A,B : ISDODataObjectList);
begin
  B.appendCurrency(A.getCurrency());
end;
{$ENDIF HAS_SDO_CURRENCY}

{$IFDEF HAS_SDO_DOUBLE}
procedure CopyDoubleProperty(const A,B : ISDODataObject; const AProp : ISDOProperty);
begin
  if A.isSet(AProp) then begin
    if A.isNull(AProp) then
      B.setNull(AProp)
    else
      B.setDouble(AProp,A.getDouble(AProp));
  end else begin
    B.unset(AProp);
  end;
end;

procedure CopyListDouble(const A,B : ISDODataObjectList);
begin
  B.append(A.getDouble());
end;
{$ENDIF HAS_SDO_DOUBLE}

procedure CopyDateProperty(const A,B : ISDODataObject; const AProp : ISDOProperty);
begin
  if A.isSet(AProp) then begin
    if A.isNull(AProp) then
      B.setNull(AProp)
    else
      B.setDate(AProp,A.getDate(AProp));
  end else begin
    B.unset(AProp);
  end;
end;

{$IFDEF HAS_SDO_FLOAT}
procedure CopyFloatProperty(const A,B : ISDODataObject; const AProp : ISDOProperty);
begin
  if A.isSet(AProp) then begin
    if A.isNull(AProp) then
      B.setNull(AProp)
    else
      B.setFloat(AProp,A.getFloat(AProp));
  end else begin
    B.unset(AProp);
  end;
end;

procedure CopyListFloat(const A,B : ISDODataObjectList);
begin
  B.append(A.getFloat());
end;
{$ENDIF HAS_SDO_FLOAT}

procedure CopyIntegerProperty(const A,B : ISDODataObject; const AProp : ISDOProperty);
begin
  if A.isSet(AProp) then begin
    if A.isNull(AProp) then
      B.setNull(AProp)
    else
      B.setInteger(AProp,A.getInteger(AProp));
  end else begin
    B.unset(AProp);
  end;
end;

{$IFDEF HAS_SDO_LONG}
procedure CopyLongProperty(const A,B : ISDODataObject; const AProp : ISDOProperty);
begin
  if A.isSet(AProp) then begin
    if A.isNull(AProp) then
      B.setNull(AProp)
    else
      B.setLong(AProp,A.getLong(AProp));
  end else begin
    B.unset(AProp);
  end;
end;
{$ENDIF HAS_SDO_LONG}

{$IFDEF HAS_SDO_SHORT}
procedure CopyShortProperty(const A,B : ISDODataObject; const AProp : ISDOProperty);
begin
  if A.isSet(AProp) then begin
    if A.isNull(AProp) then
      B.setNull(AProp)
    else
      B.setShort(AProp,A.getShort(AProp));
  end else begin
    B.unset(AProp);
  end;
end;
{$ENDIF HAS_SDO_SHORT}

procedure CopyStringProperty(const A,B : ISDODataObject; const AProp : ISDOProperty);
begin
  if A.isSet(AProp) then begin
    if A.isNull(AProp) then
      B.setNull(AProp)
    else
      B.setString(AProp,A.getString(AProp));
  end else begin
    B.unset(AProp);
  end;
end;

procedure CopyListBool(const A,B : ISDODataObjectList);
begin
  B.append(A.getBoolean());
end;

procedure CopyListByte(const A,B : ISDODataObjectList);
begin
  B.append(A.getByte());
end;

procedure CopyListDate(const A,B : ISDODataObjectList);
begin
  B.append(A.getDate());
end;

procedure CopyListInteger(const A,B : ISDODataObjectList);
begin
  B.append(A.getInteger());
end;

{$IFDEF HAS_SDO_LONG}
procedure CopyListLong(const A,B : ISDODataObjectList);
begin
  B.append(A.getLong());
end;
{$ENDIF HAS_SDO_LONG}

{$IFDEF HAS_SDO_SHORT}
procedure CopyListShort(const A,B : ISDODataObjectList);
begin
  B.append(A.getShort());
end;
{$ENDIF HAS_SDO_SHORT}

procedure CopyListObjectRef(const A,B : ISDODataObjectList);
begin
  B.append(A.getDataObject());
end;

procedure CopyListString(const A,B : ISDODataObjectList);
begin
  B.append(A.getString());
end;

var
  ValueCopyFunctions : array[TSDOTypeKind] of TCopyProcRecord =(
    ( Simple : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}CopyBoolProperty; List : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}CopyListBool ), // BooleanType,
    ( Simple : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}CopyByteProperty; List : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}CopyListByte ), //ByteType,
{$IFDEF HAS_SDO_BYTES}
    ( Simple : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}CopyBytesProperty; List : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}CopyListBytes ), //BytesType,
{$ENDIF HAS_SDO_BYTES}
    ( Simple : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}NotImplementedSimplePROC; List : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}NotImplementedListPROC ), //ChangeSummaryType,
{$IFDEF HAS_SDO_CHAR}
    ( Simple : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}CopyCharProperty; List : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}CopyListChar ), //CharacterType,
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
    ( Simple : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}CopyCurrencyProperty; List : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}CopyListCurrency ), //CurrencyType,
{$ENDIF HAS_SDO_CURRENCY}
    ( Simple : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}CopyDateProperty; List : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}CopyListDate ), //DateTimeType,
{$IFDEF HAS_SDO_DOUBLE}
    ( Simple : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}CopyDoubleProperty; List : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}CopyListDouble ), //DoubleType,
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
    ( Simple : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}CopyFloatProperty; List : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}CopyListFloat ), //FloatType,
{$ENDIF HAS_SDO_FLOAT}
    ( Simple : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}CopyIntegerProperty; List : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}CopyListInteger ), //IntegerType,
{$IFDEF HAS_SDO_LONG}
    ( Simple : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}CopyLongProperty; List : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}CopyListLong ), //LongType,
{$ENDIF HAS_SDO_LONG}
    ( Simple : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}NotImplementedSimplePROC; List : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}CopyListObjectRef ), //ObjectType,
{$IFDEF HAS_SDO_SHORT}
    ( Simple : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}CopyShortProperty; List : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}CopyListShort ), //ShortType,
{$ENDIF HAS_SDO_SHORT}
    ( Simple : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}CopyStringProperty; List : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}CopyListString ) //StringType,

  );

procedure CopySimpleList(
  const ASource, ADest : ISDODataObjectList;
  const AType : TSDOTypeKind
);
var
  crsA : ILinkedListCursor;
  func : TListTypeCopyProc;
  bmkA : TLinkedListBookmark;
begin
//  if not ( AType in SDODataTypeKinds ) then
  //  raise ESDOIllegalArgumentException.Create('AType');
  func := ValueCopyFunctions[AType].List;
  crsA := ASource.getCursor();
  bmkA := crsA.GetBookmark();
  try
    crsA.Reset();
    while crsA.MoveNext() do begin
      func(ASource,ADest);
    end;
  finally
    crsA.GotoBookmark(bmkA);
  end;
end;

{ TSDOCopyHelper }

class function TSDOCopyHelper.copy(const ADataObject: ISDODataObject): ISDODataObject;
begin
  Result := internalCopy(ADataObject,True);
end;

class procedure TSDOCopyHelper.copyProperty(const A, B: ISDODataObject; const AProp: ISDOProperty);
begin
  ValueCopyFunctions[AProp.getTypeEnum()].Simple(A,B,AProp);
end;

class procedure TSDOCopyHelper.copyPropertyList(
  const A, B : ISDODataObjectList;
  const AType : ISDOType;
  const ADeepCopy : Boolean
);
var
  crsA : ILinkedListCursor;
  func : TListTypeCopyProc;
  bmkA : TLinkedListBookmark;
begin
  func := ValueCopyFunctions[AType.getTypeEnum()].List;
  crsA := A.getCursor();
  bmkA := crsA.GetBookmark();
  try
    crsA.Reset();
    if AType.isDataType() then begin
      while crsA.MoveNext() do begin
        func(A,B);
      end;
    end else begin
      while crsA.MoveNext() do begin
        B.append(internalCopy(A.getDataObject(),ADeepCopy));
      end;
    end;
  finally
    crsA.GotoBookmark(bmkA);
  end;
end;

class function TSDOCopyHelper.copyShallow(const ADataObject: ISDODataObject): ISDODataObject;
begin
  Result := internalCopy(ADataObject,False);
end;

class function TSDOCopyHelper.internalCopy(
  const ADataObject: ISDODataObject;
  const ADeepCopy: Boolean
) : ISDODataObject;
var
  locType, locPropType : ISDOType;
  a, b : ISDODataObject;
  al, bl : ISDODataObjectList;
  locProps : ISDOPropertyList;
  locProp : ISDOProperty;
  i, c : PtrInt;
begin
  if ( ADataObject = nil ) then begin
    b := nil;
  end else begin
    a := ADataObject;
    locType := ADataObject.getType();
    locProps := locType.getProperties();
    b := locType.getOwner().CreateNew(locType);
    c := locProps.getCount();
    for i := 0 to Pred(c) do begin
      locProp := locProps.getItem(i);
      if not locProp.isReadOnly() then begin
        locPropType := locProp.getType();
        if locProp.isMany() then begin
          al := a.getList(locProp);
          bl := b.getList(locProp);
          copyPropertyList(al,bl,locPropType,ADeepCopy);
        end else begin
          if locPropType.isDataType() then
            copyProperty(a,b,locProp)
          else if locPropType.isDataObjectType() then
            b.setDataObject(locProp,internalCopy(a.getDataObject(locProp),ADeepCopy));
        end;
      end;
    end;
  end;
  Result := b;
end;


type
  TSimpleTypeCompareFunction = function (const A,B : ISDODataObject; const AProp : ISDOProperty) : Boolean;
  TListTypeCompareFunction = function (const A,B : ISDODataObjectList) : Boolean;
  TCompareFunctionRecord = record
    SimpleFunction : TSimpleTypeCompareFunction;
    ListFunction   : TListTypeCompareFunction;
  end;

{$WARNINGS OFF}
function NotImplementedSimpleFUNC(const A,B : ISDODataObject; const AProp : ISDOProperty) : Boolean;
begin
  raise ESDONotImplementedException.Create('');
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function NotImplementedListFUNC(const A,B : ISDODataObjectList) : Boolean;
begin
  raise ESDONotImplementedException.Create('');
end;
{$WARNINGS ON}

{$IFNDEF EQUALITY_USE_SET_NULL}
function CompareBoolProperty(const A,B : ISDODataObject; const AProp : ISDOProperty) : Boolean;
begin
  Result := ( A.getBoolean(AProp.getName()) = B.getBoolean(AProp.getName()) );
end;

function CompareByteProperty(const A,B : ISDODataObject; const AProp : ISDOProperty) : Boolean;
begin
  Result := ( A.getByte(AProp.getName()) = B.getByte(AProp.getName()) );
end;

function CompareDateProperty(const A,B : ISDODataObject; const AProp : ISDOProperty) : Boolean;
begin
  Result := ValueEquals(A.getDate(AProp.getName()),B.getDate(AProp.getName()));
end;

function CompareIntegerProperty(const A,B : ISDODataObject; const AProp : ISDOProperty) : Boolean;
begin
  Result := ( A.getInteger(AProp.getName()) = B.getInteger(AProp.getName()) );
end;

function CompareStringProperty(const A,B : ISDODataObject; const AProp : ISDOProperty) : Boolean;
begin
  Result := ( A.getString(AProp.getName()) = B.getString(AProp.getName()) );
end;
{$ENDIF EQUALITY_USE_SET_NULL}

{$IFDEF EQUALITY_USE_SET_NULL}
function CompareBoolProperty(const A,B : ISDODataObject; const AProp : ISDOProperty) : Boolean;
begin
  Result := ( ( A.isSet(AProp.getName()) = False ) and
              ( B.isSet(AProp.getName()) = False )
            ) or
            ( ( ( A.isSet(AProp.getName()) = True ) and
                ( B.isSet(AProp.getName()) = True )
              ) and
              ( A.getBoolean(AProp.getName()) = B.getBoolean(AProp.getName()) )
            ) ;
end;

function CompareByteProperty(const A,B : ISDODataObject; const AProp : ISDOProperty) : Boolean;
begin
  Result := ( ( A.isSet(AProp.getName()) = False ) and
              ( B.isSet(AProp.getName()) = False )
            ) or
            ( ( ( A.isSet(AProp.getName()) = True ) and
                ( B.isSet(AProp.getName()) = True )
              ) and
              ( A.getByte(AProp.getName()) = B.getByte(AProp.getName()) )
            ) ;
end;

function CompareIntegerProperty(const A,B : ISDODataObject; const AProp : ISDOProperty) : Boolean;
begin
  Result := ( ( A.isSet(AProp.getName()) = False ) and
              ( B.isSet(AProp.getName()) = False )
            ) or
            ( ( ( A.isSet(AProp.getName()) = True ) and
                ( B.isSet(AProp.getName()) = True )
              ) and
              ( A.getInteger(AProp.getName()) = B.getInteger(AProp.getName()) )
            ) ;
end;

function CompareStringProperty(const A,B : ISDODataObject; const AProp : ISDOProperty) : Boolean;
begin
  Result := ( ( A.isSet(AProp.getName()) = False ) and
              ( B.isSet(AProp.getName()) = False )
            ) or
            ( ( ( A.isSet(AProp.getName()) = True ) and
                ( B.isSet(AProp.getName()) = True )
              ) and
              ( A.getString(AProp.getName()) = B.getString(AProp.getName()) )
            ) ;
end;

{$ENDIF EQUALITY_USE_SET_NULL}

function CompareListChangeSummary(const A,B : ISDODataObjectList) : Boolean;
begin
  Result := True;
end;

function CompareChangeSummaryProperty(const A,B : ISDODataObject; const AProp : ISDOProperty) : Boolean;
begin
  Result := True;
end;

function CompareListBool(const A,B : ISDODataObjectList) : Boolean;
begin
  Result := ( A.getBoolean() = B.getBoolean() );
end;

function CompareListByte(const A,B : ISDODataObjectList) : Boolean;
begin
  Result := ( A.getByte() = B.getByte() );
end;

function CompareListDate(const A,B : ISDODataObjectList) : Boolean;
begin
  Result := ValueEquals(A.getDate(),B.getDate());
end;

function CompareListInteger(const A,B : ISDODataObjectList) : Boolean;
begin
  Result := ( A.getInteger() = B.getInteger() );
end;

function CompareListString(const A,B : ISDODataObjectList) : Boolean;
begin
  Result := ( A.getString() = B.getString() );
end;

{$IFDEF HAS_SDO_BYTES}
function CompareBytesProperty(const A,B : ISDODataObject; const AProp : ISDOProperty) : Boolean;
var
  av, bv : TSDOBytes;
begin
  av := A.getBytes(AProp.getName());
  bv := B.getBytes(AProp.getName());
  Result := ( ( av = nil ) and ( bv = nil ) ) or
            ( ( av <> nil ) and ( bv <> nil) and ( Length(av) = Length(bv) ) and CompareMem(Pointer(av),Pointer(av),Length(av)) );
end;

function CompareListBytes(const A,B : ISDODataObjectList) : Boolean;
var
  av, bv : TSDOBytes;
begin
  av := A.getBytes();
  bv := B.getBytes();
  Result := ( ( av = nil ) and ( bv = nil ) ) or
            ( ( av <> nil ) and ( bv <> nil) and ( Length(av) = Length(bv) ) and CompareMem(Pointer(av),Pointer(av),Length(av)) );
end;
{$ENDIF HAS_SDO_BYTES}

{$IFDEF HAS_SDO_CHAR}
function CompareCharProperty(const A,B : ISDODataObject; const AProp : ISDOProperty) : Boolean;
begin
  Result := ( A.getCharacter(AProp.getName()) = B.getCharacter(AProp.getName()) );
end;

function CompareListChar(const A,B : ISDODataObjectList) : Boolean;
begin
  Result := ( A.getCharacter() = B.getCharacter() );
end;
{$ENDIF HAS_SDO_CHAR}

{$IFDEF HAS_SDO_CURRENCY}
function CompareCurrencyProperty(const A,B : ISDODataObject; const AProp : ISDOProperty) : Boolean;
begin
  Result := ( A.getCurrency(AProp.getName()) = B.getCurrency(AProp.getName()) );
end;

function CompareListCurrency(const A,B : ISDODataObjectList) : Boolean;
begin
  Result := ( A.getCurrency() = B.getCurrency() );
end;
{$ENDIF HAS_SDO_CURRENCY}

{$IFDEF HAS_SDO_DOUBLE}
function CompareDoubleProperty(const A,B : ISDODataObject; const AProp : ISDOProperty) : Boolean;
begin
  Result := ( A.getDouble(AProp.getName()) = B.getDouble(AProp.getName()) );
end;

function CompareListDouble(const A,B : ISDODataObjectList) : Boolean;
begin
  Result := ( A.getDouble() = B.getDouble() );
end;
{$ENDIF HAS_SDO_DOUBLE}

{$IFDEF HAS_SDO_FLOAT}
function CompareFloatProperty(const A,B : ISDODataObject; const AProp : ISDOProperty) : Boolean;
begin
  Result := ( A.getFloat(AProp.getName()) = B.getFloat(AProp.getName()) );
end;

function CompareListFloat(const A,B : ISDODataObjectList) : Boolean;
begin
  Result := ( A.getFloat() = B.getFloat() );
end;
{$ENDIF HAS_SDO_FLOAT}

{$IFDEF HAS_SDO_LONG}
function CompareLongProperty(const A,B : ISDODataObject; const AProp : ISDOProperty) : Boolean;
begin
  Result := ( A.getLong(AProp.getName()) = B.getLong(AProp.getName()) );
end;

function CompareListLong(const A,B : ISDODataObjectList) : Boolean;
begin
  Result := ( A.getLong() = B.getLong() );
end;
{$ENDIF HAS_SDO_LONG}

{$IFDEF HAS_SDO_SHORT}
function CompareShortProperty(const A,B : ISDODataObject; const AProp : ISDOProperty) : Boolean;
begin
  Result := ( A.getShort(AProp.getName()) = B.getShort(AProp.getName()) );
end;

function CompareListShort(const A,B : ISDODataObjectList) : Boolean;
begin
  Result := ( A.getShort() = B.getShort() );
end;
{$ENDIF HAS_SDO_SHORT}

var
  ValueCompareFunctions : array[TSDOTypeKind] of TCompareFunctionRecord =(
    ( SimpleFunction : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}CompareBoolProperty; ListFunction : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}CompareListBool ), // BooleanType,
    ( SimpleFunction : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}CompareByteProperty; ListFunction : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}CompareListByte ), //ByteType,
{$IFDEF HAS_SDO_BYTES}
    ( SimpleFunction : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}CompareBytesProperty; ListFunction : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}CompareListBytes ), //BytesType,
{$ENDIF HAS_SDO_BYTES}
    ( SimpleFunction : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}CompareChangeSummaryProperty; ListFunction : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}CompareListChangeSummary ), //ChangeSummaryType,
{$IFDEF HAS_SDO_CHAR}
    ( SimpleFunction : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}CompareCharProperty; ListFunction : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}CompareListChar ), //CharacterType,
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
    ( SimpleFunction : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}CompareCurrencyProperty; ListFunction : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}CompareListCurrency ), //CurrencyType,
{$ENDIF HAS_SDO_CURRENCY}
    ( SimpleFunction : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}CompareDateProperty; ListFunction : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}CompareListDate ), //DateTimeType,
{$IFDEF HAS_SDO_DOUBLE}
    ( SimpleFunction : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}CompareDoubleProperty; ListFunction : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}CompareListDouble ), //DoubleType,
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
    ( SimpleFunction : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}CompareFloatProperty; ListFunction : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}CompareListFloat ), //FloatType,
{$ENDIF HAS_SDO_FLOAT}
    ( SimpleFunction : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}CompareIntegerProperty; ListFunction : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}CompareListInteger ), //IntegerType,
{$IFDEF HAS_SDO_LONG}
    ( SimpleFunction : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}CompareLongProperty; ListFunction : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}CompareListLong ), //LongType,
{$ENDIF HAS_SDO_LONG}
    ( SimpleFunction : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}NotImplementedSimpleFUNC; ListFunction : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}NotImplementedListFUNC ), //ObjectType,
{$IFDEF HAS_SDO_SHORT}
    ( SimpleFunction : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}CompareShortProperty; ListFunction : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}CompareListShort ), //ShortType,
{$ENDIF HAS_SDO_SHORT}
    ( SimpleFunction : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}CompareStringProperty; ListFunction : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}CompareListString ) //StringType,

  );

(*procedure Init();
begin
  FillChar(ValueCompareFunctions,SizeOf(ValueCompareFunctions),#0);
  ValueCompareFunctions[BooleanType].SimpleFunction := {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}CompareBoolProperty;
    ValueCompareFunctions[BooleanType].ListFunction := {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}CompareListBool;
  ValueCompareFunctions[IntegerType].SimpleFunction := {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}CompareIntegerProperty;
    ValueCompareFunctions[IntegerType].ListFunction := {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}CompareListInteger;
  ValueCompareFunctions[StringType].SimpleFunction  := {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}CompareStringProperty;
    ValueCompareFunctions[StringType].ListFunction  := {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}CompareListString;
end;
*)

{ TSDOEqualityHelper }

class function TSDOEqualityHelper.CompareList(
  const A,B: ISDODataObjectList;
  const AType : ISDOType
) : Boolean;
var
  c : PtrInt;
  crsA, crsB : ISDOCursor;
  ok : Boolean;
  func : TListTypeCompareFunction;
  bmkA, bmkB : ISDOCursorBookmark;
begin
  if ( A = nil ) and ( B = nil ) then begin
    Result := True;
  end else if ( A <> nil ) and ( B <> nil ) then begin
    Result := False;
    c := A.size();
    if ( c = B.size() ) then begin
      ok := True;
      if ( c > 0 ) then begin
        func := ValueCompareFunctions[AType.getTypeEnum()].ListFunction;
        crsA := A.getCursor();
        crsB := B.getCursor();
        bmkA := crsA.GetBookmark();
        bmkB := crsB.GetBookmark();
        try
          crsA.Reset();
          crsB.Reset();
          if AType.isDataType() then begin
            while crsA.MoveNext() do begin
              if not ( crsB.MoveNext() and func(A,B) ) then begin
                ok := False;
                Break;
              end;
            end;
          end else begin
            while crsA.MoveNext() do begin
              if not ( crsB.MoveNext() and InternalEqual(A.getDataObject(),B.getDataObject(),True) ) then begin
                ok := False;
                Break;
              end;
            end;
          end;
        finally
          crsB.GotoBookmark(bmkB);
          crsA.GotoBookmark(bmkA);
        end;
      end;
      Result := ok;
    end;
  end else begin
    Result := False;
  end;
end;

class function TSDOEqualityHelper.CompareProperty(
  const A, B : ISDODataObject;
  const AProp : ISDOProperty
) : Boolean;
begin
  Result := ValueCompareFunctions[AProp.getTypeEnum()].SimpleFunction(A,B,AProp);
end;

class function TSDOEqualityHelper.equal(const A, B: ISDODataObject): Boolean;
begin
  Result := InternalEqual(A,B,True);
end;

class function TSDOEqualityHelper.equalShallow(const A,B: ISDODataObject): Boolean;
begin
  Result := InternalEqual(A,B,False);
end;

class function TSDOEqualityHelper.InternalEqual(
  const A, B: ISDODataObject;
  const AFullCompare: Boolean
) : Boolean;
var
  i, c : PtrInt;
  plsA, plsB : ISDOPropertyList;
  ok : Boolean;
  pA, pB : ISDOProperty;
begin
  if ( A = nil ) and ( B = nil ) then begin
    Result := True;
  end else if ( A <> nil ) and ( B <> nil ) then begin
    Result := False;
    if A.getType().equals(B.getType()) then begin
      plsA := A.getInstanceProperties();
      plsB := B.getInstanceProperties();
      c := plsA.getCount();
      if ( c = plsB.getCount() ) then begin
        ok := True;
        if ( c > 0 ) then begin
          for i := 0 to Pred(c) do begin
            pA := plsA.getItem(i);
            pB := plsB.find(pA.getName());
            if ( pB = nil ) then begin
              ok := False;
              Break;
            end;
            if ( pA.getType().isDataType() <> pB.getType().isDataType() ) then begin
              ok := False;
              Break;
            end;
            if pA.getType().isDataType() then begin
              if pA.isMany() then
                ok := CompareList(A.getList(pA),B.getList(pB),pA.getType())
              else
                ok := CompareProperty(A,B,pA);
              if not ok then
                Break;
            end else if AFullCompare then begin
              if pA.isMany() then
                ok := CompareList(A.getList(pA),B.getList(pB),pA.getType())
              else
                ok := InternalEqual(A.getDataObject(pA.getName()),B.getDataObject(pA.getName()),True);
              if not ok then
                Break;
            end;
          end;
        end;
        if ok then
          Result := True;
      end;
    end;
  end else begin
    Result := False;
  end;
end;


initialization
{$IFDEF DELPHI}
  GetLocaleFormatSettings(GetThreadLocale(),ConverterFormatSetting);
{$ENDIF}
{$IFDEF FPC}
  ConverterFormatSetting := DefaultFormatSettings;
{$ENDIF}
  ConverterFormatSetting.DecimalSeparator := '.';

end.
