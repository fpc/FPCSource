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
  System.Classes, System.SysUtils, System.Contnrs,
  {$ELSE}
  Classes, SysUtils, contnrs,
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
                   ptStructure,    // Class/Record  (schema object with properties)
                   ptSchemaStruct, // Def/APcomponent
                   ptArray         // Array of...
                   );

  TPascalTypes = Set of TPascalType;
  // Aliases
  TPropertyType = TPascalType;
  TPropertyTypes = TPascalTypes;

  { TPascalProperty }

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
    Property TypeData : TPascalTypeData Read FTypeData Write FTypeData;
    // The JSON Schema for this property
    Property Schema : TJSONSchema Read FSchema Write FSchema;
  end;

  { TPascalTypeData }

  TPascalTypeData = class(TObject)
  private
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
    Procedure SortProperties;
  Public
    class function ExtractFirstType(aSchema: TJSONSchema): TSchemaSimpleType;
  Public
    Constructor Create(aIndex : integer; const aSchemaName,aPascalName : String; aSchema : TJSONSchema);
    destructor Destroy; override;
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
    //
    Property Sorted : Boolean Read FSorted Write FSorted;
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

  { TSchemaData }

  TSchemaData = class(TObject)
  private
    FKeywordEscapeMode: TKeywordEscapeMode;
    FTypeList : TPascalTypeDataList;
    FTypeMap : TFPObjectHashTable;
    FArrayTypePrefix: string;
    FArrayTypeSuffix: string;
    FDelphiTypes: Boolean;
    FInterfaceTypePrefix: String;
    FObjectTypePrefix: string;
    FObjectTypeSuffix: string;
    FOnLog: TSchemaCodeGenLogEvent;
    FUseEnums: Boolean;
    function GetSchemaType(aIndex : Integer): TPascalTypeData;
    function GetSchemaTypeCount: Integer;
  protected
    // Logging
    procedure DoLog(Const aType : TEventType; const aMessage : String);
    procedure DoLog(Const aType : TEventType; const aFmt : String; aArgs : Array of const);
    // Add a new type to the type map.
    procedure AddToTypeMap(const aSchemaName: String; aData : TPascalTypeData); virtual; overload;

    procedure SortTypes;
  Public
    Constructor Create; virtual;
    Destructor Destroy; override;
    // Is the word a pascal keyword ?
    class function IsKeyWord(const aWord : String) : Boolean;
    // Escape the word if it is a pascal keyword ?
    function EscapeKeyWord(const aWord : string) : string;
    // Get the pascal name based on schema name
    function GetTypeMap(const aName : string): String;
    // Return index of named schema type (name as in OpenApi). Return -1 if not found.
    function IndexOfSchemaType(const aSchemaName: String): integer;
    // Extract simple type from schema
    Function GetSchemaType(aSchema : TJSONSchema) : TSchemaSimpleType;
    // Extract element type from schema
    Function GetArrayElementType(aSchema : TJSONSchema) : TSchemaSimpleType;
    // Add a type to the list
    Procedure AddType(const aSchemaName: String; aType : TPascalTypeData); virtual;
    // Add a type definition to the type map.
    procedure AddAliasToTypeMap(const aSchemaTypeName,aPascalTypeName : String; aSchema : TJSONSchema = Nil); overload;

    Property TypeCount : Integer Read GetSchemaTypeCount;
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
    ptStructure    : Raise ESchemaData.CreateFmt('Unknown name for structured property "%s"',[PascalName]);
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
    Result:='T'+StringReplace(SchemaName,'Dto','',[rfIgnoreCase]);
    Result:=Result+'Obj';
    end;
end;


function TPascalTypeData.GetInterfaceName: String;

begin
  Result:=FInterfaceName;
  if Result='' then
    Result:='I'+SchemaName;
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


constructor TPascalTypeData.Create(aIndex: integer; const aSchemaName, aPascalName: String; aSchema: TJSONSchema);

begin
  FIndex:=aIndex;
  FSchema:=ASchema;
  FSchemaName:=aSchemaName;
  FPascalName:=aPascalName;
  FSerializeTypes:=[stSerialize,stDeserialize];
  FProperties:=TFPObjectList.Create(True);
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

var
  I : Integer;

begin
  Result:=dtNone;
  if Not Assigned(FDependencies) then
    exit;
  For I:=0 to DependencyCount-1 do
    if (Dependency[i]=aData) then
      exit(dtDirect);
  if not Recurse then
    exit;
  For I:=0 to DependencyCount-1 do
    if (Dependency[i].DependsOn(aData,True)<>dtNone) then
      Exit(dtIndirect);
end;


procedure TPascalTypeData.AddDependency(aData: TPascalTypeData);

begin
  if FDependencies=Nil then
     FDependencies:=TFPObjectList.Create(False);
  FDependencies.Add(aData);
end;


class function TPascalTypeData.ExtractFirstType(aSchema : TJSONSchema): TSchemaSimpleType;

var
  types : TSchemaSimpleTypes;
  t : TSchemaSimpleType;

begin
  result:=sstNone;
  types:=aSchema.Validations.Types;
  for T in TSchemaSimpleType do
    if T in Types then
      Exit(T);
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


procedure TSchemaData.AddAliasToTypeMap(const aSchemaTypeName, aPascalTypeName: String; aSchema: TJSONSchema);

begin
  AddToTypeMap(aSchemaTypeName,TPascalTypeData.Create(-1,aSchemaTypeName,aPascalTypeName,aSchema));
end;


constructor TSchemaData.Create;

begin
  FTypeMap:=TFPObjectHashTable.Create(False);
  FTypeList:=TPascalTypeDataList.Create(True);
  FObjectTypePrefix:='T';
  FObjectTypeSuffix:='';
  FInterfaceTypePrefix:='I';
  FKeywordEscapeMode:=kemSuffix;
end;


destructor TSchemaData.Destroy;

begin
  FreeAndNil(FTypeList);
  FreeAndNil(FTypeMap);
  inherited Destroy;
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
      'private;published;length;setlength;';

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


function TSchemaData.GetTypeMap(const aName: string): String;

var
  Obj : TPascalTypeData;

begin
  Obj:=TPascalTypeData(FTypeMap.Items[aName]);
  if Assigned(Obj) then
    Result:=Obj.PascalName
  else
    Result:=aName;
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
    for I:=0 to aType.DependencyCount-1 do
      AddToList(aList,aType.Dependency[i]);
    aList.Add(aType);
    aType.Sorted:=True;
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

