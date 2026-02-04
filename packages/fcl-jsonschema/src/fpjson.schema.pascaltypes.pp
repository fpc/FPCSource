{
    This file is part of the Free Component Library
    Copyright (c) 2024 by Michael Van Canneyt michael@freepascal.org

    JSON Schema - pascal types and helpers

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpjson.schema.pascaltypes;

{$mode ObjFPC}{$H+}

interface

uses
  {$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, System.Contnrs, System.StrUtils,
  {$ELSE}
  Classes, SysUtils, contnrs, StrUtils,
  {$ENDIF}
  fpjson.schema.types,
  fpjson.schema.schema;

Type
  ESchemaData = Class(EJSONSchema);

  TPascalTypeData = Class;

  TSchemaCodeGenLogEvent = Procedure (aType : TEventType; const Msg : String) of object;

  TDependencyType = (dtNone,dtDirect,dtIndirect);
  TDependencyTypes = set of TDependencyType;

  TNameType = (ntSchema,ntPascal,ntInterface,ntImplementation,ntSerializer);
  TNameTypes = set of TNameType;

  TSerializeType = (stSerialize,stDeserialize);
  TSerializeTypes = set of TSerializeType;

  TPascalType = (ptUnknown,
                   ptBoolean,      // Boolean
                   ptInteger,      // 32-bit integer
                   ptInt64,        // 64-bit integer
                   ptDateTime,     // TDateTime
                   ptFloat32,      // Single
                   ptFloat64,      // Double
                   ptString,       // String
                   ptEnum,         // Enumerated
                   ptJSON,         // TJSONData (empty schema object)
                   ptAnonStruct,   // Anonymous Class/Record  (schema object with properties)
                   ptSchemaStruct, // Named Class/Record
                   ptArray         // Array of...
                   );

  TPascalTypes = Set of TPascalType;
  // Aliases
  TPropertyType = TPascalType;
  TPropertyTypes = TPascalTypes;

  { TPascalPropertyData }

  TPascalPropertyData = class(TObject)
  private
    FSchemaName: string;
    FElementType: TPropertyType;
    FEnumValues: TStrings;
    FPascalName: string;
    FSchema: TJSONSchema;
    FTypeNames: Array[TNameType] of string ;
    FElementTypeNames: Array[TNameType] of string ;
    FPropertyType: TPropertyType;
    FTypeData: TPascalTypeData;
    function GetElementTypeNames(aType : TNameType): String;
    function GetFallBackTypeName(aPropertyType: TPropertyType): string;
    function GetPascalTypeName: String;
    procedure SetElementTypeNames(aType : TNameType; AValue: String);
    procedure SetEnumValues(AValue: TStrings);
    Function GetTypeName(aType: TNameType) : String;
    procedure SetTypeData(AValue: TPascalTypeData);
    procedure SetTypeName(aType: TNameType; aValue : String);
  Public
    Constructor Create(const aSchemaName, aPascalName : string);
    Destructor Destroy; override;
    // schema Name of the property
    Property SchemaName : string Read FSchemaName Write FSchemaName;
    // Pascal Name of the property
    Property PascalName : string Read FPascalName Write FPascalName;
    // Indexed access to all kind of type names
    Property TypeNames [aType : TNameType] : String Read GetTypeName Write SetTypeName;
    // Type of the property
    Property PropertyType : TPropertyType Read FPropertyType Write FPropertyType;
    // If Type is ptEnum, the values. Without _empty_.
    Property EnumValues : TStrings Read FEnumValues Write SetEnumValues;
    // Pascal type name for the property (same as TypeNames[ntPascal])
    Property PascalTypeName : String Index ntPascal Read GetTypeName Write SetTypeName;
    // PropertyType = ptArray : The array element type
    Property ElementType : TPropertyType Read FElementType Write FElementType;
    // PropertyType = ptArray : The array element type name (same as ElementTypeNames[ntPascal])
    Property ElementTypeName : String Index ntPascal Read GetElementTypeNames Write SetElementTypeNames;
    // PropertyType = ptArray : The array element type names
    Property ElementTypeNames[aType : TNameType] : String Read GetElementTypeNames Write SetElementTypeNames;
    // PropertyType = ptSchemaStruct: The type data for that component.
    // PropertyType = ptArray and elType=ptSchemaStruct
    Property TypeData : TPascalTypeData Read FTypeData Write SetTypeData;
    // The JSON Schema for this property
    Property Schema : TJSONSchema Read FSchema Write FSchema;
  end;

  { TPascalTypeData }

  TPascalTypeData = class(TObject)
  private
    FElementTypeData: TPascalTypeData;
    FSchemaName: String;
    FImplementationName: String;
    FIndex: Integer;
    FInterfaceName: String;
    FInterfaceUUID: String;
    FPascalName: String;
    FSchema: TJSONSChema;
    FDependencies : TFPObjectList;
    FSerializerName: String;
    FSerializeTypes: TSerializeTypes;
    FSorted : Boolean;
    FProperties : TFPObjectList;
    FType: TPascalType;
    function GetDependency(aIndex : Integer): TPascalTypeData;
    function GetDependencyCount: Integer;
    function GetImplementationName: String;
    function GetInterfaceName: String;
    function GetInterfaceUUID: String;
    function GetProperty(aIndex : Integer): TPascalPropertyData;
    function GetPropertyCount: Integer;
    function GetSerializerName: String;
  Protected
    function CreateProperty(const aSchemaName, aPascalName: string): TPascalPropertyData; virtual;
  Public
    class function ExtractFirstType(aSchema: TJSONSchema): TSchemaSimpleType;
  Public
    Constructor Create(aIndex : integer; aType : TPascalType; const aSchemaName,aPascalName : String; aSchema : TJSONSchema);
    destructor Destroy; override;
    // Sort the properties.
    Procedure SortProperties;
    // Index of property using schema name
    Function IndexOfProperty(const aSchemaName: string) : Integer;
    // Index of property using Pascal name
    Function IndexOfPascalProperty(const aPascalName: string) : Integer;
    // Find property by schema name.
    Function FindProperty(const aName: string) : TPascalPropertyData;
    // Add a property. The pascal name must not yet exist.
    Function AddProperty(const aSchemaName,aPascalName : String) : TPascalPropertyData;
    // Return the requested name
    function GetTypeName(aNameType : TNameType) : string;
    // Check whether this component depends on given component. If recurse is true, check all intermediary structures as well.
    function DependsOn(aData : TPascalTypeData; Recurse : Boolean) : TDependencyType;
    // Add aData as a type this type depends on.
    Procedure AddDependency(aData : TPascalTypeData);
    // Component has array-typed property ?
    Function HasArrayProperty : Boolean;
    // Component has object-typed property ? (SchemaComponentsonly = False -> also return array of string etc.)
    function HasObjectProperty(aSchemaComponentsOnly: Boolean): Boolean;
    // Components his component depends on
    Property Dependency[aIndex : Integer] : TPascalTypeData Read GetDependency;
    // Number of Components his component depends on
    Property DependencyCount : Integer Read GetDependencyCount;
    // Indexed access to Properties
    Property Properties[aIndex : Integer] : TPascalPropertyData Read GetProperty; default;
    // Number of properties
    Property PropertyCount : Integer Read GetPropertyCount;
    // Pascal type name for DTO (can be influenced by map). Default is schema name with prefix/suffix
    Property PascalName : String Read FPascalName;
    // Schema name.
    Property SchemaName : String Read FSchemaName;
    // Interface name. Default Pascal name + 'Intf'
    Property InterfaceName : String Read GetInterfaceName Write FInterfaceName;
    // Interface UUID.
    Property InterfaceUUID : String Read GetInterfaceUUID Write FInterfaceUUID;
    // Implemention class name. Default Pascal name + 'Obj'
    Property ImplementationName : String Read GetImplementationName  write FImplementationName;
    // Name of serialized helper. Default is Pascal name + 'Serializer'
    Property SerializerName : String Read GetSerializerName Write FSerializerName;
    // Do we need to serialize/deserialize ?
    Property SerializeTypes : TSerializeTypes Read FSerializeTypes Write FSerializeTypes;
    // Schema of this component.
    Property Schema: TJSONSChema Read FSchema;
    // Was this element sorted ?
    Property Sorted : Boolean Read FSorted Write FSorted;
    // PascalType
    Property Pascaltype : TPascalType Read FType;
    // For arrays, a pointer to the element type
    Property ElementTypeData : TPascalTypeData Read FElementTypeData Write FElementTypeData;
  end;

  { TPascalTypeDataList }

  TPascalTypeDataList = Class(TFPObjectList)
  private
    function GetTypes(aIndex : Integer): TPascalTypeData;
  Public
    Procedure Add(aItem : TPascalTypeData); reintroduce;
    Property Types[aIndex : Integer] : TPascalTypeData Read GetTypes; default;
  end;

  TKeywordEscapeMode = (kemAmpersand,kemSuffix,kemPrefix);

  // How to handle reserved type names that conflict with standard library types
  TReservedTypeBehaviour = (
    rtbEscape,   // Escape the type name (e.g., TTimeZone -> TTimeZone_)
    rtbQualify   // Use fully qualified name in serializer references
  );

  { TSchemaData }

  TSchemaData = class(TObject)
  private
    FKeywordEscapeMode: TKeywordEscapeMode;
    FReservedTypeBehaviour: TReservedTypeBehaviour;
    FReservedTypes: TStrings;
    FTypeAliases: TStrings;
    FMaxIdentifierLength: Integer;
    FTypeList : TPascalTypeDataList;
    FAliasList : TPascalTypeDataList;
    FTypeMap : TFPObjectHashTable;
    FArrayTypePrefix: string;
    FArrayTypeSuffix: string;
    FDelphiTypes: Boolean;
    FInterfaceTypePrefix: String;
    FObjectTypePrefix: string;
    FObjectTypeSuffix: string;
    FOnLog: TSchemaCodeGenLogEvent;
    FUseEnums: Boolean;
    procedure SetReservedTypes(AValue: TStrings);
    procedure SetTypeAliases(AValue: TStrings);
    function GetSchemaType(aIndex : Integer): TPascalTypeData;
    function GetSchemaTypeCount: Integer;
  protected
    // Logging
    procedure DoLog(Const aType : TEventType; const aMessage : String);
    procedure DoLog(Const aType : TEventType; const aFmt : String; aArgs : Array of const);
    // Override this to finish creating a type.
    procedure FinishAutoCreatedType(aName: string; aType: TPascalTypeData; lElementTypeData: TPascalTypeData); virtual;
    // Override this to determine the type name of a pascal property
    function GetSchemaTypeAndName(aType: TPascalTypeData; aSchema: TJSONSchema; out aPropType: TPascalType; aNameType: TNameType=ntPascal): String; virtual;
    // Add a new type to the type map.
    procedure AddToTypeMap(const aSchemaName: String; aData : TPascalTypeData); virtual; overload;
    // Get pascal type name based on schema name
    function SchemaNameToNameType(const aName: string; aNameType: TNameType): string; virtual;
    // Take JSONPointer reference and find pascal type data for it.
    function GetPascalTypeDataFromRef(const aRef: String): TPascalTypeData; virtual;
    // Find schema pascal type data. If AllowCreate is true, type data for Enum,Array and object properties will be created.
    function GetSchemaTypeData(aType: TPascalTypeData; lSchema: TJSONSchema; AllowCreate: Boolean=False): TPascalTypeData;
    // Add a type to the alias list
    Procedure AddAliasType(aType : TPascalTypeData); virtual;
    // Sanitize identifier
    function Sanitize(const aName : string) : String;
    // Sort types in dependency order
    procedure SortTypes;
    // Define default reserved type names.
    procedure DefineDefaultReservedTypes; virtual;
  Public
    Constructor Create; virtual;
    Destructor Destroy; override;
    // Create aliases for known simple types
    procedure DefineStandardPascalTypes;
    // Is the word a pascal keyword ?
    class function IsKeyWord(const aWord : String) : Boolean;
    // Is the type name a reserved type that conflicts with standard library types ?
    function IsReservedTypeName(const aTypeName : String) : Boolean;
    // Escape the word if it is a pascal keyword ?
    function EscapeKeyWord(const aWord : string) : string;
    // Handle reserved type name - escape or return as-is based on ReservedTypeBehaviour
    function HandleReservedTypeName(const aTypeName : string) : string;
    // Load type aliases from file (format: SchemaTypeName=AliasName per line)
    procedure LoadTypeAliases(const aFileName : string);
    // Apply type name shortening: first check aliases, then auto-shorten if too long
    function ApplyTypeNameShortening(const aSchemaName : string) : string;
    // Get qualified type name for serializer use (returns UnitName.TypeName for reserved types when rtbQualify)
    function GetQualifiedTypeName(const aTypeName, aUnitName : string) : string;
    // Ensure Pascal type name is unique (case-insensitive check), append suffix if needed
    function EnsureUniquePascalName(const aPascalName : string) : string;
    // Get the pascal name based on schema name
    function GetTypeMap(const aName : string): String;
    // Return index of named schema type (name as in OpenApi). Return -1 if not found.
    function IndexOfSchemaType(const aSchemaName: String): integer;
    // Find Pascal type data based on schema type name.
    function FindSchemaTypeData(const aSchemaName: String; aFormat : String = ''): TPascalTypeData;
    // Extract simple type from schema
    Function GetSchemaType(aSchema : TJSONSchema) : TSchemaSimpleType;
    // Extract element type from schema
    Function GetArrayElementType(aSchema : TJSONSchema) : TSchemaSimpleType;
    // Used when creating a new type. Override to create a descendant;
    function CreatePascalType(aIndex: integer; aType : TPascalType; const aSchemaName, aPascalName: String; aSchema: TJSONSchema): TPascalTypeData; virtual;
    // Add a type to the list
    Procedure AddType(const aSchemaName: String; aType : TPascalTypeData); virtual;
    // Add a type definition to the type map.
    function AddAliasToTypeMap(aType: TPascalType; const aAlias, aSchemaTypeName, aPascalTypeName: String; aSchema: TJSONSchema) : TPascalTypeData; overload;
    // Add a property to a type
    function AddTypeProperty(aType: TPascalTypeData; lProp: TJSONSchema; aName : string = ''; Recurse : Boolean = True): TPascalPropertyData;
    // Add properties to structured pascal type from aSchema. if aSchema = nil then use aType.Schema
    Procedure AddPropertiesToType(aType : TPascalTypeData; aSchema: TJSONSchema = Nil; Recurse : Boolean = True);
    // For all types, fill the dependency list: contains all structured types on which the type depends (recursively).
    procedure CheckDependencies; virtual;
    // Number of types
    Property TypeCount : Integer Read GetSchemaTypeCount;
    // Indexed access to all types.
    Property Types[aIndex : Integer] : TPascalTypeData Read GetSchemaType; default;
    // Map schema type to pascal type.
    Property TypeMap[aSchemaName : string] : String Read GetTypeMap;
    // prefix for object definitions. Default T
    Property ObjectTypePrefix : string Read FObjectTypePrefix Write FObjectTypePrefix;
    // prefix for object definitions. Default empty
    Property ObjectTypeSuffix : string Read FObjectTypeSuffix Write FObjectTypeSuffix;
    // Prefix for Dto Objects
    Property InterfaceTypePrefix : String Read FInterfaceTypePrefix Write FInterfaceTypePrefix;
    // Prefix for array types
    Property ArrayTypePrefix : string Read FArrayTypePrefix Write FArrayTypePrefix;
    // Suffix for array types. Default Array
    Property ArrayTypeSuffix : string Read FArrayTypeSuffix Write FArrayTypeSuffix;
    // Use delphi types: TArray<X> instead of Array of X
    Property DelphiTypes : Boolean Read FDelphiTypes Write FDelphiTypes;
    // Use enums for enumerateds (default is to keep them as strings)
    Property UseEnums : Boolean Read FUseEnums Write FUseEnums;
    // Log callback
    Property OnLog : TSchemaCodeGenLogEvent Read FOnLog Write FOnLog;
    // how to escape keywords
    Property KeywordEscapeMode : TKeywordEscapeMode Read FKeywordEscapeMode Write FKeywordEscapeMode;
    // List of reserved type names that conflict with standard library types (one per line, without T prefix)
    Property ReservedTypes : TStrings Read FReservedTypes Write SetReservedTypes;
    // How to handle reserved type names: escape them or use qualified names
    Property ReservedTypeBehaviour : TReservedTypeBehaviour Read FReservedTypeBehaviour Write FReservedTypeBehaviour;
    // Type aliases for shortening long type names (format: SchemaTypeName=AliasName)
    Property TypeAliases : TStrings Read FTypeAliases Write SetTypeAliases;
    // Maximum identifier length (default 120, to leave room for Array suffix under FPC's 127 limit)
    Property MaxIdentifierLength : Integer Read FMaxIdentifierLength Write FMaxIdentifierLength;
  end;

implementation


function CompareTypeDataOnName(Item1, Item2: Pointer): Integer;

var
  lType1 : TPascalTypeData absolute Item1;
  lType2 : TPascalTypeData absolute Item2;

begin
  Result:=CompareText(lType1.SchemaName,lType2.SchemaName);
end;


function CompareProperties(Item1, Item2: Pointer): Integer;

var
  lParam1 : TPascalPropertyData absolute Item1;
  lParam2 : TPascalPropertyData absolute Item2;

begin
  Result:=CompareText(lParam1.PascalName,lParam2.PascalName);
end;


{ TPascalPropertyData }

procedure TPascalPropertyData.SetEnumValues(AValue: TStrings);

begin
  if FEnumValues=AValue then Exit;
  FEnumValues.Assign(AValue);
end;


function TPascalPropertyData.GetPascalTypeName: String;

begin
 Result:=GetTypeName(ntPascal);
end;


function TPascalPropertyData.GetElementTypeNames(aType : TNameType): String;

begin
  Result:=FElementTypeNames[aType];
  if Result<>'' then
    exit;
  if (PropertyType=ptArray) then
    begin
    if (ElementType=ptSchemaStruct) then
      Exit(TypeData.GetTypeName(aType));
    Result:=GetFallBackTypeName(ElementType);
    end;
end;

procedure TPascalPropertyData.SetElementTypeNames(aType : TNameType; AValue: String);

begin
  FElementTypeNames[aType]:=aValue;
end;


constructor TPascalPropertyData.Create(const aSchemaName, aPascalName: string);

begin
  FSchemaName:=aSchemaName;
  FPascalName:=aPascalName;
  FEnumValues:=TStringList.Create;
end;

destructor TPascalPropertyData.Destroy;

begin
  FreeAndNil(FEnumValues);
  inherited Destroy;
end;

function TPascalPropertyData.GetTypeName(aType: TNameType): String;

begin
  Result:=FTypeNames[aType];
  if Result<>'' then
    exit;
  if Assigned(FTypeData) then
    Exit(FTypeData.GetTypeName(aType));
  // Fallback
  Result:=GetFallBackTypeName(FPropertyType);
end;

procedure TPascalPropertyData.SetTypeData(AValue: TPascalTypeData);
begin
  if FTypeData=AValue then Exit;
  FTypeData:=AValue;
  if Assigned(FTypeData) then
    FElementType:=FTypeData.Pascaltype;
end;

function TPascalPropertyData.GetFallBackTypeName(aPropertyType: TPropertyType): string;

begin
  Case aPropertyType of
    ptUnknown      : Raise ESchemaData.CreateFmt('Unknown property type for property "%s"',[PascalName]);
    ptBoolean      : Result:='boolean';
    ptInteger      : Result:='integer';
    ptInt64        : Result:='Int64';
    ptDateTime     : Result:='TDateTime';
    ptFloat32      : Result:='single';
    ptFloat64      : Result:='double';
    ptString       : Result:='string';
    ptEnum         : Raise ESchemaData.CreateFmt('Unknown name for enumerated property "%s"',[PascalName]);
    ptJSON         : Result := 'string';
    ptAnonStruct   : Raise ESchemaData.CreateFmt('Unknown name for structured property "%s"',[PascalName]);
    ptSchemaStruct : Raise ESchemaData.CreateFmt('Unknown name for schema-typed property "%s"',[PascalName]);
  end;
end;

procedure TPascalPropertyData.SetTypeName(aType: TNameType; aValue: String);

begin
  FTypeNames[aType]:=aValue;
end;


function TPascalTypeData.GetDependencyCount: Integer;

begin
  Result:=0;
  if Assigned(FDependencies) then
    Result:=FDependencies.Count;
end;


function TPascalTypeData.GetImplementationName: String;

begin
  Result:=FImplementationName;
  if Result='' then
    begin
    if PascalType in [ptSchemaStruct,ptAnonStruct] then
      begin
      Result:='T'+StringReplace(SchemaName,'Dto','',[rfIgnoreCase]);
      Result:=Result+'Obj';
      end
    else
      Result:=PascalName;
    end;
end;


function TPascalTypeData.GetInterfaceName: String;

begin
  Result:=FInterfaceName;
  if Result='' then
    begin
    if Pascaltype in [ptAnonStruct,ptSchemaStruct] then
      Result:='I'+SchemaName
    else
      Result:=PascalName;
    end;
end;


function TPascalTypeData.GetInterfaceUUID: String;

begin
  if FInterfaceUUID='' then
    FInterfaceUUID:=TGUID.NewGuid.ToString(False);
  Result:=FInterfaceUUID;
end;


function TPascalTypeData.GetProperty(aIndex : Integer): TPascalPropertyData;

begin
  Result:=TPascalPropertyData(FProperties[aIndex]);
end;


function TPascalTypeData.GetPropertyCount: Integer;

begin
  Result:=FProperties.Count;
end;


function TPascalTypeData.GetSerializerName: String;

begin
  Result:=FSerializerName;
  If Result='' then
    Result:=PascalName+'Serializer';
end;


function TPascalTypeData.CreateProperty(const aSchemaName,aPascalName: string): TPascalPropertyData;

begin
  Result:=TPascalPropertyData.Create(aSchemaName,aPascalName);
end;


procedure TPascalTypeData.SortProperties;

begin
  FProperties.Sort(@CompareProperties);
end;


function TPascalTypeData.GetDependency(aIndex : Integer): TPascalTypeData;

begin
  if Assigned(FDependencies) then
    Result:=TPascalTypeData(FDependencies[aIndex])
  else
    Raise EListError.CreateFmt('List index out of bounds: %d',[aIndex]);
end;


constructor TPascalTypeData.Create(aIndex: integer; aType: TPascalType; const aSchemaName, aPascalName: String; aSchema: TJSONSchema
  );

begin
  FIndex:=aIndex;
  FSchema:=ASchema;
  FSchemaName:=aSchemaName;
  FPascalName:=aPascalName;
  FSerializeTypes:=[stSerialize,stDeserialize];
  FProperties:=TFPObjectList.Create(True);
  FType:=aType;
end;


destructor TPascalTypeData.Destroy;

begin
  FreeAndNil(FProperties);
  FreeAndNil(FDependencies);
  Inherited;
end;


function TPascalTypeData.IndexOfProperty(const aSchemaName: string): Integer;

begin
  Result:=FProperties.Count-1;
  While (Result>=0) and Not SameText(GetProperty(Result).SchemaName,aSchemaName) do
    Dec(Result);
end;


function TPascalTypeData.IndexOfPascalProperty(const aPascalName: string): Integer;

begin
  Result:=FProperties.Count-1;
  While (Result>=0) and Not SameText(GetProperty(Result).PascalName,aPascalName) do
    Dec(Result);
end;


function TPascalTypeData.FindProperty(const aName: string): TPascalPropertyData;

var
  Idx : Integer;

begin
  Idx:=IndexOfProperty(aName);
  If Idx=-1 then
    Result:=Nil
  else
    Result:=GetProperty(Idx);
end;


function TPascalTypeData.AddProperty(const aSchemaName, aPascalName: String): TPascalPropertyData;

begin
  if IndexOfPascalProperty(aPascalName)<>-1 then
    Raise ESchemaData.CreateFmt('Duplicate property name : %s',[aPascalName]);
  Result:=CreateProperty(aSchemaName,aPascalName);
  FProperties.Add(Result);
end;


function TPascalTypeData.GetTypeName(aNameType: TNameType): string;

begin
  Case aNameType of
    ntSchema: Result:=SchemaName;
    ntPascal: Result:=PascalName;
    ntInterface : Result:=InterfaceName;
    ntImplementation : Result:=ImplementationName;
    ntSerializer : Result:=SerializerName
  end;
end;


function TPascalTypeData.DependsOn(aData: TPascalTypeData; Recurse: Boolean): TDependencyType;

  function DoCheck(aType: TPascalTypeData; aVisited: TFPObjectList): TDependencyType;
  var
    I: Integer;
  begin
    Result := dtNone;
    if Not Assigned(aType.FDependencies) then
      exit;
    // Check if already visited to prevent infinite recursion on circular dependencies
    if aVisited.IndexOf(aType) >= 0 then
      exit;
    aVisited.Add(aType);
    For I := 0 to aType.DependencyCount-1 do
      if (aType.Dependency[i] = aData) then
        exit(dtDirect);
    if not Recurse then
      exit;
    For I := 0 to aType.DependencyCount-1 do
      if (DoCheck(aType.Dependency[i], aVisited) <> dtNone) then
        Exit(dtIndirect);
  end;

var
  lVisited: TFPObjectList;

begin
  lVisited := TFPObjectList.Create(False);
  try
    Result := DoCheck(Self, lVisited);
  finally
    lVisited.Free;
  end;
end;


procedure TPascalTypeData.AddDependency(aData: TPascalTypeData);

begin
  if FDependencies=Nil then
     FDependencies:=TFPObjectList.Create(False);
  FDependencies.Add(aData);
end;

procedure TSchemaData.CheckDependencies;

  procedure CheckProps(lTop,aData : TPascalTypeData);

  var
    lProp : TPascalPropertyData;
    lPropData : TPascalTypeData;
    I : Integer;
  begin
    For I:=0 to aData.PropertyCount-1 do
      begin
      lProp:=aData.Properties[I];
      lPropData:=lProp.TypeData; // Array element or struct depending on type.
      if Assigned(lPropData) then
        begin
        Case lPropData.Pascaltype of
        ptAnonStruct,ptSchemaStruct:
          begin
          if lTop.DependsOn(lPropData, False) = dtNone then
            begin
            lTop.AddDependency(lPropData);
            CheckProps(lTop,lPropData);
            end;
          end;
        ptArray:
          begin
          lPropData:=lPropData.ElementTypeData;
          if assigned(lPropData) and (lPropData.PascalType in [ptAnonStruct,ptSchemaStruct]) then
            if lTop.DependsOn(lPropData, False) = dtNone then
              begin
              lTop.AddDependency(lPropData);
              CheckProps(lTop,lPropData);
              end;
          end
        else
          ;
        end;
        end;
      end;
  end;

var
  I : Integer;
  lData : TPascalTypeData;
  lName : string;

begin
  For I:=0 to TypeCount-1 do
    begin
    lData:=Types[I];
    Case lData.Pascaltype of
      ptAnonStruct,
      ptSchemaStruct:
        CheckProps(lData,lData) ;
      ptArray:
        begin
        // Resolve element type ref
        if (lData.Schema.Items.Count=1) then
          begin
          lName:=lData.Schema.Items[0].Ref;
          if lName<>'' then
            lData.ElementTypeData:=GetPascalTypeDataFromRef(lName);
          end;
        end;
    end;
    end;
end;


class function TPascalTypeData.ExtractFirstType(aSchema : TJSONSchema): TSchemaSimpleType;

begin
  Result:=aSchema.Validations.GetFirstType;
end;


function TPascalTypeData.HasArrayProperty: Boolean;

var
  I : integer;

begin
  Result:=False;
  if not Assigned(FSchema) then exit;
  For I:=0 to Schema.Properties.Count-1 do
    if (ExtractFirstType(Schema.Properties[i])=sstArray) then
      exit(True);
end;


function TPascalTypeData.HasObjectProperty(aSchemaComponentsOnly : Boolean): Boolean;

var
  I : integer;
  lProp : TJSONSchema;

begin
  Result:=False;
  if not Assigned(FSchema) then exit;
  For I:=0 to Schema.Properties.Count-1 do
    begin
    lProp:=Schema.Properties[i];
    if (lProp.Ref<>'') then
      exit(True);
    if (ExtractFirstType(lProp)=sstObject) and not aSchemaComponentsOnly then
      exit(True);
    end;
end;

{ TPascalTypeDataList }

function TPascalTypeDataList.GetTypes(aIndex : Integer): TPascalTypeData;

begin
  Result:=TPascalTypeData(Items[aIndex]);
end;


procedure TPascalTypeDataList.Add(aItem: TPascalTypeData);

begin
  Inherited Add(aItem);
end;


{ TSchemaData }

function TSchemaData.GetSchemaTypeCount: Integer;

begin
  Result:=FTypeList.Count;
end;


function TSchemaData.GetSchemaType(aIndex : Integer): TPascalTypeData;

begin
  Result:=FTypeList[aIndex];
end;


procedure TSchemaData.DoLog(const aType: TEventType; const aMessage: String);

begin
  If Assigned(FOnLog) then
    FOnLog(aType,aMessage);
end;

procedure TSchemaData.DoLog(const aType: TEventType; const aFmt: String;  aArgs: array of const);

begin
  If Assigned(FOnLog) then
    FOnLog(aType,Format(aFmt,aArgs));
end;

// Find requested name type in API types, based on openAPI name.
function TSchemaData.SchemaNameToNameType(const aName: string; aNameType: TNameType): string;

var
  lType : TPascalTypeData;

begin
  lType:=FindSchemaTypeData(aName);
  if Assigned(lType) then
    Result:=lType.GetTypeName(aNameType)
  else
    Result:=aName;
end;

function TSchemaData.GetPascalTypeDataFromRef(const aRef : String): TPascalTypeData;

var
  P : Integer;
  lName : String;
begin
  P:=RPos('/',aRef);
  if P=0 then
    P:=RPos('#',aRef);
  if p=0 then
    lName:=aRef
  else
    lName:=Copy(aRef,P+1,Length(aRef)-P);
  Result:=FindSchemaTypeData(lName);
end;

procedure TSchemaData.AddAliasType(aType: TPascalTypeData);
begin
  FAliasList.Add(aType);
end;

function TSchemaData.Sanitize(const aName: string): String;
var
  i : integer;
  lRes : string;
begin
  lRes:=aName;
  UniqueString(lRes);
  For I:=1 to Length(lRes) do
    if not (lRes[i] in ['a'..'z','A'..'Z','0'..'9','_']) then
      lRes[i]:='_';
  Result:=lRes;
end;


// Determine the PascalType and pascal type name of the given schema

function TSchemaData.GetSchemaTypeAndName(aType: TPascalTypeData; aSchema: TJSONSchema; out aPropType: TPascalType; aNameType : TNameType = ntPascal): String;

var
  lTypeData : TPascalTypeData;

begin
  lTypeData:=GetSchemaTypeData(aType,aSchema);
  if lTypeData=Nil then
    begin
    aPropType:=ptUnknown;
    Result:='';
    end
  else
    begin
    aPropType:=lTypeData.Pascaltype;
    Result:=lTypeData.GetTypeName(aNameType);
    end;
end;

procedure TSchemaData.FinishAutoCreatedType(aName: string; aType: TPascalTypeData; lElementTypeData: TPascalTypeData);

begin
  AddType(aName,aType);
  Case aType.Pascaltype of
  ptAnonStruct:
    AddPropertiesToType(aType,aType.Schema,True);
  ptArray:
    aType.FElementTypeData:=lElementTypeData;
  end;
end;

function TSchemaData.GetSchemaTypeData(aType: TPascalTypeData; lSchema: TJSONSchema; AllowCreate : Boolean = False) : TPascalTypeData;

var
  lType : TSchemaSimpleType;
  lName,lBaseName,lPascalName : string;
  lFormat : String;
  lTmp : TJSONSchema;
  lElTypeData : TPascalTypeData;

begin
  LType:=lSchema.Validations.GetFirstType;
  Result:=Nil;
  if lSchema.Ref<>'' then
    Result:=GetPascalTypeDataFromRef(lSchema.Ref)
  else
    begin
    lName:='';
    lFormat:='';
    Case lType of
      sstNone: ;
      sstNull: ;
      sstBoolean :
        lName:='boolean';
      sstInteger :
        begin
        lName:='integer';
        lFormat:=lSchema.Validations.Format;
        end;
      sstNumber:
        begin
        lName:='number';
        end;
      sstString:
        begin
        if IndexText(lSchema.Validations.Format,['date','time','date-time'])>=0 then
          begin
          lName:='string';
          lFormat:=lSchema.Validations.Format;
          end
        else if UseEnums and lSchema.Validations.HasKeywordData(jskEnum) and (lSchema.Validations.Enum.Count>0) then
          begin
          if assigned(aType) then
            lBaseName:=aType.GetTypeName(ntSchema)+'_'+lSchema.Name
          else
            lBaseName:='T'+lSchema.Name;
          lName:='('+lBaseName+')';
          Result:=FindSchemaTypeData(lName);
          if (Result=Nil) and allowCreate then
            begin
            Result:=CreatePascalType(-1,ptEnum,lName,'T'+lBaseName,lSchema);
            FinishAutoCreatedType(lName,Result,Nil);
            end;
          end
        else
          begin
          lName:='string';
          end;
        end;
      sstArray:
        begin
        lElTypeData:=GetSchemaTypeData(Nil,lSchema.Items[0],True);
        lPascalName:=Sanitize(ArrayTypePrefix+lElTypeData.PascalName+ArrayTypeSuffix);
        lName:='['+lElTypeData.SchemaName;
        if lSchema.Items[0].Validations.HasKeywordData(jskformat) then
          lName:=lName+'--'+lSchema.Items[0].Validations.Format;
        lName:=LName+']';
        Result:=FindSchemaTypeData(lName);
        if Result<>Nil then
          lName:='';
        if (Result=Nil) and AllowCreate then
          begin
          Result:=CreatePascalType(-1,ptArray,lName,lPascalName,lSchema);
          FinishAutoCreatedType(lName,Result,lElTypeData);
          lName:='';
          end;
        end;
      sstObject:
        begin
        if lSchema.Properties.Count=0 then
          lName:='JSON'
        else
          begin
          if assigned(aType) then
            lBaseName:=aType.GetTypeName(ntSchema)+'_'+lSchema.Name
          else
            begin
            lBaseName:=lSchema.Name;
            lTmp:=lSchema.Parent;
            While lTmp<>Nil do
              begin
              if lTmp.Name<>'' then
                lBaseName:=lTmp.Name+'_'+lBaseName
              else
                lBaseName:='Nested_'+lBaseName;
              lTmp:=lTmp.Parent;
              end;
            end;
          lName:='{'+lBaseName+'}';
          lPascalName:=HandleReservedTypeName(ObjectTypePrefix+Sanitize(lBaseName));
          Result:=FindSchemaTypeData(lName);
          if (Result=Nil) and AllowCreate then
            begin
            Result:=CreatePascalType(-1,ptAnonStruct,lName,lPascalName,lSchema);
            FinishAutoCreatedType(lName,Result,lElTypeData);
            lName:='';
            end;
          end;
        end;
      sstAny:
        lname:='any';
    end;
    if lName<>'' then
      Result:=FindSchemaTypeData(lName,lFormat);
    end;
end;

// Add a property to the type using the schema
function TSchemaData.AddTypeProperty(aType: TPascalTypeData; lProp: TJSONSchema; aName: string; Recurse: Boolean
  ): TPascalPropertyData;

var
  lTypeName, lName : string;
  lType,lEltype : TPropertyType;
  I : Integer;
  lPropTypeData : TPascaltypeData;


begin
  lName:=aName;
  if lName='' then
    lName:=EscapeKeyWord(Sanitize(lProp.Name));
  DoLog(etInfo,'Adding property name %s to %s',[lName,aType.PascalName]);
  if lProp.Validations.TypesCount>1 then
    Raise ESchemaData.CreateFmt('Creating property for schema with multiple types ("%s") is not supported',[lName]);
  if (lProp.Validations.GetFirstType=sstArray) then
    if (lProp.Items.Count<>1) then
      Raise ESchemaData.CreateFmt('Creating array property for schema with multiple item types ("%s") is not supported',[lName])
    else if (lProp.Items.Count<1) then
      Raise ESchemaData.CreateFmt('Creating array property for schema without item types ("%s") is not supported',[lName]);
  lPropTypeData:=GetSchemaTypeData(aType,lProp,Recurse);
  if lPropTypeData=Nil then
    Raise ESchemaData.CreateFmt('Unknown property type for property %s',[lName]);
  lType:=lPropTypeData.Pascaltype;
  lTypeName:=lPropTypeData.GetTypeName(ntPascal);
  Result:=aType.AddProperty(lProp.Name,lName);
  Result.Schema:=lProp;
  Result.PropertyType:=lType;
  Result.TypeData:=lPropTypeData;
  Result.PascalTypeName:=lPropTypeData.GetTypeName(ntPascal);
  if (lType=ptEnum) then
    begin
    for I:=0 to lProp.Validations.Enum.Count-1 do
      Result.EnumValues.Add(EscapeKeyWord(lProp.Validations.Enum.Items[I].AsString));
    end;
  if (lType=ptArray) then
    begin
    Result.PascalTypeName:=lTypeName;
    if (lProp.Items[0].Ref<>'') then
      begin
      Result.ElementType:=ptSchemaStruct;
      Result.TypeData:=GetPascalTypeDataFromRef(lProp.Items[0].Ref);
      if Result.TypeData=Nil then
        Raise ESchemaData.CreateFmt('No typedata for property %s element type (Ref: %s)',[Result.PascalName,lProp.Items[0].Ref]);
      Result.ElementTypeName:=Result.TypeData.PascalName;
      end
    else
      begin
      Result.ElementTypeName:=GetSchemaTypeAndName(Nil,lProp.Items[0],lEltype);
      Result.ElementType:=lElType;
      end;
    Result.TypeNames[ntInterface]:=GetSchemaTypeAndName(Nil,lProp,lelType,ntInterface);
    Result.TypeNames[ntImplementation]:=GetSchemaTypeAndName(Nil,lProp,lElType,ntImplementation);
    end;
end;

procedure TSchemaData.AddPropertiesToType(aType: TPascalTypeData; aSchema: TJSONSchema; Recurse: Boolean);

var
  I : Integer;
  lSchema : TJSONSchema;
begin
  lSchema:=aSchema;
  if lSchema=Nil then
    lSchema:=aType.Schema;
  for I:=0 to lSchema.Properties.Count-1 do
    AddTypeProperty(aType,lSchema.Properties[i],'',Recurse);
end;

function TSchemaData.CreatePascalType(aIndex: integer; aType : TPascalType; const aSchemaName, aPascalName: String; aSchema: TJSONSchema): TPascalTypeData;
begin
  Result:=TPascalTypeData.Create(aIndex,aType,aSchemaName,aPascalName,aSchema);
end;


function TSchemaData.AddAliasToTypeMap(aType: TPascalType; const aAlias, aSchemaTypeName, aPascalTypeName: String;
  aSchema: TJSONSchema): TPascalTypeData;

var
  lType : TPascalTypeData;

begin
  lType:=CreatePascalType(-1,aType,aSchemaTypeName,aPascalTypeName,aSchema);
  if not (aType in [ptSchemaStruct,ptAnonStruct,ptArray]) then
    lType.InterfaceName:=aPascalTypeName;
  AddToTypeMap(aAlias,lType);
  AddAliasType(lType);
  Result:=lType;
end;

procedure TSchemaData.DefineDefaultReservedTypes;
begin
  With FReservedTypes do
    begin
    // Default reserved types that conflict with FPC/Delphi standard library
    Add('TimeStamp');     // DateUtils.TTimeStamp
    Add('TimeZone');      // DateUtils.TTimeZone
    Add('Date');          // Common type name
    Add('Time');          // Common type name
    Add('DateTime');      // SysUtils.TDateTime
    Add('Stream');        // Classes.TStream
    Add('List');          // Contnrs/Generics.TList
    Add('StringList');    // Classes.TStringList
    Add('Strings');       // Classes.TStrings
    Add('Thread');        // Classes.TThread
    Add('Component');     // Classes.TComponent
    Add('Collection');    // Classes.TCollection
    Add('Object');        // System.TObject
    Add('Word');          // SysUtils.TWordArray conflicts
    Add('JsonObject');    // fpJson.TJSONObject conflicts
    Add('EventType');     // SysUtils.TEventType conflicts
    end;
end;

 
constructor TSchemaData.Create;

begin
  FTypeMap:=TFPObjectHashTable.Create(False);
  FTypeList:=TPascalTypeDataList.Create(True);
  FAliasList:=TPascalTypeDataList.Create(True);
  FReservedTypes:=TStringList.Create;
  DefineDefaultReservedTypes;
  TStringList(FReservedTypes).Duplicates:=dupIgnore;
  TStringList(FReservedTypes).Sorted:=True;
  FTypeAliases:=TStringList.Create;
  TStringList(FTypeAliases).Duplicates:=dupIgnore;
  FMaxIdentifierLength:=120;
  FObjectTypePrefix:='T';
  FObjectTypeSuffix:='';
  FInterfaceTypePrefix:='I';
  FArrayTypeSuffix:='Array';
  FArrayTypePrefix:='';
  FKeywordEscapeMode:=kemSuffix;
  FReservedTypeBehaviour:=rtbEscape;
end;


destructor TSchemaData.Destroy;

begin
  FreeAndNil(FTypeList);
  FreeAndNil(FAliasList);
  FreeAndNil(FTypeMap);
  FreeAndNil(FReservedTypes);
  FreeAndNil(FTypeAliases);
  inherited Destroy;
end;

procedure TSchemaData.DefineStandardPascalTypes;
var
  lArr,lElem : TPascalTypeData;

begin
  // typename--format
  lElem:=AddAliasToTypeMap(ptInteger,'integer','integer','integer',Nil);
  lArr:=AddAliasToTypeMap(ptArray,'[integer]','[integer]','TIntegerDynArray',Nil);
  lArr.ElementTypeData:=lElem;

  AddAliasToTypeMap(ptInteger,'integer--int32','integer','integer',Nil);

  lElem:=AddAliasToTypeMap(ptInt64,'integer--int64','integer','int64',Nil);
  lArr:=AddAliasToTypeMap(ptArray,'[integer--int64]','[integer--int64]','TInt64DynArray',Nil);
  lArr.ElementTypeData:=lElem;

  // Google Discovery uint32/uint64 formats
  AddAliasToTypeMap(ptInteger,'integer--uint32','integer','Cardinal',Nil);
  lElem:=AddAliasToTypeMap(ptInt64,'integer--uint64','integer','QWord',Nil);
  lArr:=AddAliasToTypeMap(ptArray,'[integer--uint64]','[integer--uint64]','TQWordDynArray',Nil);
  lArr.ElementTypeData:=lElem;

  lElem:=AddAliasToTypeMap(ptString,'string','string','string',Nil);
  lArr:=AddAliasToTypeMap(ptArray,'[string]','[string]','TStringDynArray',Nil);
  lArr.ElementTypeData:=lElem;

  AddAliasToTypeMap(ptDateTime,'string--date','string','TDateTime',Nil);
  AddAliasToTypeMap(ptDateTime,'string--time','string','TDateTime',Nil);
  AddAliasToTypeMap(ptDateTime,'string--date-time','string','TDateTime',Nil);

  // Google Discovery uses strings for large integers (JSON/JavaScript limitation)
  AddAliasToTypeMap(ptString,'string--int64','string','string',Nil);
  AddAliasToTypeMap(ptString,'string--uint64','string','string',Nil);

  lElem:=AddAliasToTypeMap(ptFloat64,'number','number','double',Nil);
  lArr:=AddAliasToTypeMap(ptArray,'[number]','[number]','TDoubleDynArray',Nil);
  lArr.ElementTypeData:=lElem;

  AddAliasToTypeMap(ptJSON,'JSON','object','string',Nil);
  AddAliasToTypeMap(ptJSON,'any','object','string',Nil);

  lElem:=AddAliasToTypeMap(ptBoolean,'boolean','boolean','boolean',Nil);
  lArr:=AddAliasToTypeMap(ptArray,'[boolean]','[boolean]','TBooleanDynArray',Nil);
  lArr.ElementTypeData:=lElem;

end;


class function TSchemaData.IsKeyWord(const aWord: String): Boolean;

Const
  KW=';absolute;and;array;asm;begin;case;const;constructor;destructor;div;do;'+
      'downto;else;end;file;for;function;goto;if;implementation;in;inherited;'+
      'inline;interface;label;mod;nil;not;object;of;on;operator;or;packed;'+
      'procedure;program;record;reintroduce;repeat;self;set;shl;shr;string;then;'+
      'to;type;unit;until;uses;var;while;with;xor;dispose;exit;false;new;true;'+
      'as;class;dispinterface;except;exports;finalization;finally;initialization;'+
      'inline;is;library;on;out;packed;property;raise;resourcestring;threadvar;try;'+
      'private;published;length;setlength;result;create;destroy;free;methodname;'+
      'default;classname;strict;instancesize;classtype;';

begin
  Result:=Pos(';'+lowercase(aWord)+';',KW)<>0;
end;


function TSchemaData.EscapeKeyWord(const aWord: string): string;

begin
  Result:=aWord;
  if IsKeyWord(Result) then
    case KeywordEscapeMode of
      kemSuffix : Result:=Result+'_';
      kemPrefix : Result:='_'+Result;
      kemAmpersand : Result:='&'+Result;
    end;
end;


procedure TSchemaData.SetReservedTypes(AValue: TStrings);

begin
  if FReservedTypes = AValue then Exit;
  FReservedTypes.Clear;
  FReservedTypes.AddStrings(AValue);
end;


procedure TSchemaData.SetTypeAliases(AValue: TStrings);

begin
  if FTypeAliases = AValue then Exit;
  FTypeAliases.Clear;
  FTypeAliases.AddStrings(AValue);
end;


procedure TSchemaData.LoadTypeAliases(const aFileName: string);

begin
  if FileExists(aFileName) then
    FTypeAliases.LoadFromFile(aFileName);
end;


function TSchemaData.ApplyTypeNameShortening(const aSchemaName: string): string;

var
  lIdx: Integer;
  lFullName: string;
  lHash: Cardinal;
  lHashStr: string;
  lMaxNameLen: Integer;

  function SimpleHash(const S: string): Cardinal;
  var
    I: Integer;
  begin
    Result := 0;
    for I := 1 to Length(S) do
      Result := ((Result shl 5) + Result) + Ord(S[I]);
  end;

begin
  // Tier 1: Check user-defined aliases first
  lIdx := FTypeAliases.IndexOfName(aSchemaName);
  if lIdx >= 0 then
  begin
    Result := FTypeAliases.ValueFromIndex[lIdx];
    DoLog(etInfo, 'Using alias for type %s -> %s', [aSchemaName, Result]);
    Exit;
  end;

  // Tier 2: Check if shortening is needed
  lFullName := ObjectTypePrefix + Sanitize(aSchemaName) + ObjectTypeSuffix;

  if Length(lFullName) <= FMaxIdentifierLength then
  begin
    Result := aSchemaName;
    Exit;
  end;

  // Fallback: truncate and add hash to ensure uniqueness
  lHash := SimpleHash(aSchemaName);
  lHashStr := '_' + IntToHex(lHash, 4);
  // Calculate max name length excluding prefix, suffix, and hash
  lMaxNameLen := FMaxIdentifierLength - Length(ObjectTypePrefix) - Length(ObjectTypeSuffix) - Length(lHashStr);
  Result := Copy(aSchemaName, 1, lMaxNameLen) + lHashStr;
  DoLog(etWarning, 'Type name too long, truncated with hash: %s -> %s', [aSchemaName, Result]);
end;


function TSchemaData.IsReservedTypeName(const aTypeName: String): Boolean;

var
  lName: string;
  I: Integer;

begin
  Result := False;
  // Check if the type name (without prefix) matches any reserved type
  lName := aTypeName;
  // Remove common prefixes for comparison
  if (Length(lName) > 1) and (lName[1] = 'T') then
    lName := Copy(lName, 2, Length(lName) - 1);
  for I := 0 to FReservedTypes.Count - 1 do
    if SameText(lName, FReservedTypes[I]) then
      Exit(True);
end;


function TSchemaData.HandleReservedTypeName(const aTypeName: string): string;

begin
  Result := aTypeName;
  if not IsReservedTypeName(aTypeName) then
    Exit;
  case ReservedTypeBehaviour of
    rtbEscape:
      // Escape by adding suffix (consistent with keyword escaping)
      case KeywordEscapeMode of
        kemSuffix : Result := Result + '_';
        kemPrefix : Result := '_' + Result;
        kemAmpersand : Result := '&' + Result;
      end;
    rtbQualify:
      { For rtbQualify, do not modify the name here - the code generator
        will use the fully qualified name when using the type using 
        GetQualifiedTypeName}
      ;
  end;
end;


function TSchemaData.GetQualifiedTypeName(const aTypeName, aUnitName: string): string;

begin
  Result := aTypeName;
  // Only qualify if rtbQualify is set and type is reserved
  if (ReservedTypeBehaviour = rtbQualify) and IsReservedTypeName(aTypeName) and (aUnitName <> '') then
    Result := aUnitName + '.' + aTypeName;
end;


function TSchemaData.EnsureUniquePascalName(const aPascalName: string): string;

var
  I, Suffix: Integer;
  Found: Boolean;

begin
  Result := aPascalName;
  Suffix := 2;

  // Check if any existing type has the same Pascal name (case-insensitive)
  repeat
    Found := False;
    for I := 0 to FTypeList.Count - 1 do
    begin
      if SameText(TPascalTypeData(FTypeList[I]).PascalName, Result) then
      begin
        Found := True;
        Result := aPascalName + '_' + IntToStr(Suffix);
        Inc(Suffix);
        DoLog(etWarning, 'Type name conflict (case-insensitive): %s renamed to %s', [aPascalName, Result]);
        Break;
      end;
    end;
  until not Found;
end;


function TSchemaData.GetTypeMap(const aName: string): String;

begin
  Result:=SchemaNameToNameType(aName,ntPascal);
end;


// Find Pascal type data based on schema name
function TSchemaData.FindSchemaTypeData(const aSchemaName: String; aFormat: String): TPascalTypeData;

var
  lName : string;

begin
  lName:=aSchemaName;
  if aFormat<>'' then
    lName:=lName+'--'+aFormat;
  Result:=TPascalTypeData(FTypeMap.Items[lName]);
end;

function TSchemaData.IndexOfSchemaType(const aSchemaName: String): integer;

begin
  Result:=FTypeList.Count-1;
  While (Result>=0) and (GetSchemaType(Result).SchemaName<>aSchemaName) do
    Dec(Result);
end;

function TSchemaData.GetSchemaType(aSchema: TJSONSchema): TSchemaSimpleType;

begin
  if aSchema=Nil then
    Result:=sstNone
  else
    Result:=TPascalTypeData.ExtractFirstType(aSchema);
end;

function TSchemaData.GetArrayElementType(aSchema: TJSONSchema): TSchemaSimpleType;
begin
  Result:=sstNone;
  if GetSchemaType(aSchema)=sstArray then
    Result:=GetSchemaType(aSchema.Items[0]);
end;


procedure TSchemaData.AddType(const aSchemaName: String; aType: TPascalTypeData);

begin
  FTypeList.Add(aType);
  addToTypeMap(aSchemaName,aType);
end;


procedure TSchemaData.AddToTypeMap(const aSchemaName: String; aData: TPascalTypeData);

begin
  if FTypeMap.Items[aSchemaName]=Nil then
    FTypeMap.Add(aSchemaName,aData);
end;


procedure TSchemaData.SortTypes;

  Procedure AddToList(aList : TPascalTypeDataList; aType : TPascalTypeData);

  var
    I : integer;

  begin
    if aType.Sorted then
      exit;
    aType.Sorted:=True;  // Mark before recursing to prevent infinite recursion on circular dependencies
    for I:=0 to aType.DependencyCount-1 do
      AddToList(aList,aType.Dependency[i]);
    aList.Add(aType);
  end;

var
  lTmpList,lSortedList : TPascalTypeDataList;
  i : integer;

begin
  FTypeList.Sort(@CompareTypeDataOnName);
  lSortedList:=TPascalTypeDataList.Create(False);
  try
    lTmpList:=lSortedList;
    For I:=0 to FTypeList.Count-1 do
      AddToList(lSortedList,TPascalTypeData(FTypeList[i]));
    lTmpList:=FTypeList;
    FTypeList:=lSortedList;
    FTypeList.OwnsObjects:=True;
    lSortedList:=lTmpList;
    lSortedList.OwnsObjects:=False;
  finally
    lSortedList.Free;
  end;
end;

end.

