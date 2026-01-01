{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2024 by Michael Van Canneyt (michael@freepascal.org)

    Classes describing OpenAPI structures

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit fpopenapi.objects;

{$mode ObjFPC}{$H+}

interface

uses
  {$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, System.Contnrs, FpJson.Data,
  {$ELSE}
  Classes, SysUtils, contnrs, fpJSON,
  {$ENDIF}
  fpopenapi.Types, fpjson.schema.schema;

Type
  EOpenAPi = Class(Exception);

  // Forward definitions
  TOpenAPI = class;
  TInfo = class;
  TContact = class;
  TLicense = class;
  TServer = class;
  TServerList = class;
  TServerVariable = class;
  TServerVariableMap = class;
  TComponents = class;
  TPathItem = class;
  TPathItemOrReference = class;
  TPathItemOrReferenceMap = class;
  TApiOperation = class;
  TExternalDocumentation = class;
  TParameter = class;
  TParameterOrReference = class;
  TParameterOrReferenceMap = class;
  TParameterStyle = class;
  TRequestBody = class;
  TRequestBodyOrReference = class;
  TRequestBodyOrReferenceMap = class;
  TMediaType = class;
  TMediaTypeMap = class;
  TEncoding = class;
  TEncodingMap = class;
  TResponses = class;
  TResponse = class;
  TResponseOrReference = class;
  TResponseOrReferenceMap = class;
  TExample = class;
  TExampleOrReference = class;
  TExampleOrReferenceMap = class;
  TLink = class;
  TLinkOrReference = class;
  TLinkOrReferenceMap = class;
  TTag = class;
  TReference = class;
  TSchema = class;
  TDiscriminator = class;
  TXML = class;
  TSecurityScheme = class;
  TSecuritySchemeOrReference = class;
  TSecuritySchemeOrReferenceMap = class;
  TOAuthFlows = class;
  TOauthFlow = class;
  THeader = class;
  THeaderOrReference = class;
  THeaderOrReferenceMap = class;
  TCallback = class;
  TCallbackOrReference = class;
  TCallbackOrReferenceMap = class;
  TSecurityRequirement = class;
  TSecurityRequirementList = class;
  TBaseOpenAPIObjectList = class;

  { TBaseOpenAPIObject }

  TBaseOpenAPIObject = class(TObject)
  private
    FParent: TBaseOpenAPIObject;
    FPathComponent : TJSONStringType;
    FChildren : TBaseOpenAPIObjectList;
  protected
    function CreateChildrenList : TBaseOpenAPIObjectList; virtual;
    procedure AddChild(aChild : TBaseOpenAPIObject);
    procedure RemoveChild(aChild : TBaseOpenAPIObject);
    procedure SetPathComponent(const aValue : TJSONStringType);
  Public
    constructor create(aParent : TBaseOpenAPIObject; aPathComponent : TJSONStringType); virtual;
    destructor destroy; override;
    Function GetChildByPathComponent(const aName : TJSONStringType): TBaseOpenAPIObject;
    Function Find(const aPath : TJSONStringType): TBaseOpenAPIObject;
    function PathComponent : TJSONStringType;
    function Path : TJSONStringType;
    property Parent : TBaseOpenAPIObject read FParent;
  end;
  TOpenAPIObjectClass = Class of TBaseOpenAPIObject;

  { TBaseOpenAPIObjectList }

  TBaseOpenAPIObjectList = Class(TFPObjectList)
  private
    function GetObject(aIndex : Integer): TBaseOpenAPIObject;
  Public
    Property Objects[aIndex : Integer] : TBaseOpenAPIObject Read GetObject; default;
  end;


  TExtendableObject = class(TBaseOpenAPIObject)
  private
    FExtensions : TJSONObject;
    function GetExtensions: TJSONObject;
    procedure SetExtensions(AValue: TJSONObject);
  protected
    Function CreateExtensions : TJSONObject; virtual;
  Public
    Function HasExtensions : Boolean;
    Property Extensions : TJSONObject Read GetExtensions Write SetExtensions;
  end;



  { TNamedOpenAPiObject }

  { TNamedObject }

  TNamedObject = Class(TObject)
  private
    FName: TJSONStringType;
    FObject: TObject;
  Public
    Constructor Create(aName: TJSONStringType; aObject : TObject);
    Destructor Destroy; override;
    Property Name : TJSONStringType Read FName;
    Property Object_ : TObject Read FObject;
  end;

  { TNamedObjectList }

  TNamedObjectList = class(TBaseOpenAPIObject)
  Private
    FList : TFPObjectList;
    FHash : TFPObjectHashTable; //
    FType : TClass;
    function GetCount: Integer;
    function GetName(aIndex : Integer): TJSONStringType;
    function GetNamedObject(aIndex : Integer): TNamedObject;
    procedure SetName(aIndex : Integer; AValue: TJSONStringType);
    procedure SetNamedObject(aIndex : Integer; AValue: TNamedObject);
  Protected
    Function GetObjectByName(const aName : String): TObject;
    Procedure Add(const AName : TJSONStringType; aObject : TObject);
  Public
    Constructor Create(aParent: TBaseOpenAPIObject; const aPathComponent : TJSONStringType; aType : TClass); virtual; reintroduce;
    Destructor Destroy; override;
    Function CreateNew(const AName : TJSONStringType) : TObject; virtual; abstract;
    Procedure Delete(aName : TJSONStringType);
    Procedure Delete(aIndex : Integer);
    Function IndexOfName(aName : TJSONStringType) : Integer;
    Property Names[aIndex : Integer] : TJSONStringType Read GetName Write SetName;
    Property NamedObjects[aIndex : Integer] : TNamedObject Read GetNamedObject;
    property Count : Integer Read GetCount;
  end;
  TNamedObjectListClass = Class of TNamedObjectList;

  { TNamedOpenAPIObjectList }

  TNamedOpenAPIObjectList = Class(TNamedObjectList)
  private
    function GetApiObject(aIndex : Integer): TBaseOpenAPIObject;
  Public
    constructor Create(aParent : TBaseOpenAPIObject; const aPathComponent : TJSONStringType; aType: TClass); override;
    Function CreateNew(const AName : TJSONStringType) : TObject; override;
    Property APIObjects[aIndex : Integer] : TBaseOpenAPIObject Read GetApiObject;
  end;

  { TNamedOpenAPiObject }


  { TJSONSchemaMap }

  TJSONSchemaMap = Class(TNamedObjectList)
  private
    function GetNamedSchema(aName: TJSONStringType): TJSONSchema;
  public
    function Add(const AName : TJSONStringType): TJSONSchema; reintroduce;
    Function CreateNew(const AName : TJSONStringType) : TObject; override;
    Property Schemas[aName: TJSONStringType] : TJSONSchema Read GetNamedSchema; default;
  end;

  { TPathsList }

  TPathsList = class(TNamedOpenAPIObjectList)
  Private
    function GetObject(const aName : TJSONStringType) : TPathItem;
    function GetObjectByIndex(aIndex : Integer): TPathItem;
  Public
    constructor Create(aParent : TBaseOpenAPIObject; Const aPathComponent: TJSONStringType); reintroduce;
    function AddItem(const aName : TJSONStringType) : TPathItem;
    property Paths[aName : TJSONStringType] : TPathItem Read GetObject; default;
    property PathByIndex[aIndex : Integer] : TPathItem Read GetObjectByIndex;
  end;

  { TTagList }

  TTagList = class(TBaseOpenAPIObjectList)
  private
    function GetTag(aIndex : Integer): TTag;
  Public
    procedure AddTag(aTag : TTag);
    property Tags [aIndex : Integer] : TTag Read GetTag; default;
  end;

  { TParameterOrReferenceList }

  TParameterOrReferenceList = class(TBaseOpenAPIObjectList)
  private
    function GetParam(aIndex : Integer): TParameterOrReference;
  Public
    Function AddParam(aParent : TBaseOpenAPIObject) : TParameterOrReference;
    Property Params[aIndex : Integer] : TParameterOrReference Read GetParam; default;
  end;

  TJSONDataMap = class(TObject);


  { TOpenAPI }

  TOpenAPI = Class(TExtendableObject)
  Private
    FKeywords : TOpenAPIKeywords;
    FOpenApi : string;
    FInfo : TInfo;
    FJSONSchemaDialect : string;
    FServers : TServerList;
    FPaths : TPathsList;
    FWebHooks : TPathItemOrReferenceMap;
    FComponents : TComponents;
    FSecurity : TSecurityRequirementList;
    FTags : TTagList;
    FExternalDocs : TExternalDocumentation;
    Function Get_Info : TInfo;
    Function Get_Servers : TServerList;
    Function Get_Paths : TPathsList;
    Function Get_WebHooks : TPathItemOrReferenceMap;
    Function Get_Components : TComponents;
    Function Get_Security : TSecurityRequirementList;
    Function Get_Tags : TTagList;
    Function Get_ExternalDocs : TExternalDocumentation;
    procedure Set_OpenApi(const aValue : string);
    procedure Set_Info(const aValue : TInfo);
    procedure Set_JSONSchemaDialect(const aValue : string);
    procedure Set_Servers(const aValue : TServerList);
    procedure Set_Paths(const aValue : TPathsList);
    procedure Set_WebHooks(const aValue : TPathItemOrReferenceMap);
    procedure Set_Components(const aValue : TComponents);
    procedure Set_Security(const aValue : TSecurityRequirementList);
    procedure Set_Tags(const aValue : TTagList);
    procedure Set_ExternalDocs(const aValue : TExternalDocumentation);
  Protected
    procedure AddKeyword(aKeyword : TOpenAPIKeyword); virtual;
    procedure RemoveKeyword(aKeyword : TOpenAPIKeyword); virtual;
  public
    Constructor Create; reintroduce;
    destructor destroy; override;
    function HasKeyWord(aKeyword :TOpenAPIKeyword) : Boolean;
    property OpenApi: string Read FOpenApi write Set_OpenApi;
    property Info: TInfo Read Get_Info write Set_Info;
    property JSONSchemaDialect: string Read FJSONSchemaDialect write Set_JSONSchemaDialect;
    property Servers: TServerList Read Get_Servers write Set_Servers;
    property Paths: TPathsList Read Get_Paths write Set_Paths;
    property WebHooks: TPathItemOrReferenceMap Read Get_WebHooks write Set_WebHooks;
    property Components: TComponents Read Get_Components write Set_Components;
    property Security: TSecurityRequirementList Read Get_Security write Set_Security;
    property Tags: TTagList Read Get_Tags write Set_Tags;
    property ExternalDocs: TExternalDocumentation Read Get_ExternalDocs write Set_ExternalDocs;
  end;
  TOpenAPIArray = array of TOpenAPI;

  { TInfo }

  TInfo = Class(TExtendableObject)
  Private
    FKeywords : TInfoKeywords;
    FTitle : string;
    FSummary : string;
    FDescription : string;
    FTermsOfService : string;
    FContact : TContact;
    FLicense : TLicense;
    FVersion : string;
    Function Get_Contact : TContact;
    Function Get_License : TLicense;
    procedure Set_Title(const aValue : string);
    procedure Set_Summary(const aValue : string);
    procedure Set_Description(const aValue : string);
    procedure Set_TermsOfService(const aValue : string);
    procedure Set_Contact(const aValue : TContact);
    procedure Set_License(const aValue : TLicense);
    procedure Set_Version(const aValue : string);
  Protected
    procedure AddKeyword(aKeyword : TInfoKeyword); virtual;
    procedure RemoveKeyword(aKeyword : TInfoKeyword); virtual;
  public
    destructor destroy; override;
    function HasKeyWord(aKeyword :TInfoKeyword) : Boolean;
    property Title: string Read FTitle write Set_Title;
    property Summary: string Read FSummary write Set_Summary;
    property Description: string Read FDescription write Set_Description;
    property TermsOfService: string Read FTermsOfService write Set_TermsOfService;
    property Contact: TContact Read Get_Contact write Set_Contact;
    property License: TLicense Read Get_License write Set_License;
    property Version: string Read FVersion write Set_Version;
  end;
  TInfoArray = array of TInfo;


  { TContact }

  TContact = Class(TExtendableObject)
  Private
    FKeywords : TContactKeywords;
    FName : string;
    FUrl : string;
    FEmail : string;
    procedure Set_Name(const aValue : string);
    procedure Set_Url(const aValue : string);
    procedure Set_Email(const aValue : string);
  Protected
    procedure AddKeyword(aKeyword : TContactKeyword); virtual;
    procedure RemoveKeyword(aKeyword : TContactKeyword); virtual;
  public
    destructor destroy; override;
    function HasKeyWord(aKeyword :TContactKeyword) : Boolean;
    property Name: string Read FName write Set_Name;
    property Url: string Read FUrl write Set_Url;
    property Email: string Read FEmail write Set_Email;
  end;
  TContactArray = array of TContact;

  { TLicense }

  TLicense = Class(TExtendableObject)
  Private
    FKeywords : TLicenseKeywords;
    FName : string;
    FIdentifier : string;
    FUrl : string;
    procedure Set_Name(const aValue : string);
    procedure Set_Identifier(const aValue : string);
    procedure Set_Url(const aValue : string);
  Protected
    procedure AddKeyword(aKeyword : TLicenseKeyword); virtual;
    procedure RemoveKeyword(aKeyword : TLicenseKeyword); virtual;
  public
    destructor destroy; override;
    function HasKeyWord(aKeyword :TLicenseKeyword) : Boolean;
    property Name: string Read FName write Set_Name;
    property Identifier: string Read FIdentifier write Set_Identifier;
    property Url: string Read FUrl write Set_Url;
  end;
  TLicenseArray = array of TLicense;

  { TServer }

  TServer = Class(TExtendableObject)
  Private
    FKeywords : TServerKeywords;
    FServerUrl : string;
    FDescription : string;
    FVariables : TServerVariableMap;
    Function Get_Variables : TServerVariableMap;
    procedure Set_ServerUrl(const aValue : string);
    procedure Set_Description(const aValue : string);
    procedure Set_Variables(const aValue : TServerVariableMap);
  Protected
    procedure AddKeyword(aKeyword : TServerKeyword); virtual;
    procedure RemoveKeyword(aKeyword : TServerKeyword); virtual;
  public
    destructor destroy; override;
    function HasKeyWord(aKeyword :TServerKeyword) : Boolean;
    property Url: string Read FServerUrl write Set_ServerUrl;
    property Description: string Read FDescription write Set_Description;
    property Variables: TServerVariableMap Read Get_Variables write Set_Variables;
  end;
  TServerArray = array of TServer;

  { TServerList }

  TServerList = class(TFPObjectList)
  Private
    function GetObj(aIndex : Integer): TServer;
    procedure SetObj(aIndex : Integer; aValue : TServer);
  Public
    function ToArray : TServerArray;
    property Servers[aIndex : integer] : TServer Read GetObj Write SetObj; default;
  end;

  { TServerVariable }

  TServerVariable = Class(TExtendableObject)
  Private
    FKeywords : TServerVariableKeywords;
    FEnum : TStrings;
    FDefault : string;
    FDescription : string;
    Function Get_Enum : TStrings;
    procedure Set_Enum(const aValue : TStrings);
    procedure Set_Default(const aValue : string);
    procedure Set_Description(const aValue : string);
  Protected
    procedure AddKeyword(aKeyword : TServerVariableKeyword); virtual;
    procedure RemoveKeyword(aKeyword : TServerVariableKeyword); virtual;
  public
    destructor destroy; override;
    function HasKeyWord(aKeyword :TServerVariableKeyword) : Boolean;
    property Enum: TStrings Read Get_Enum write Set_Enum;
    property Default: string Read FDefault write Set_Default;
    property Description: string Read FDescription write Set_Description;
  end;
  TServerVariableArray = array of TServerVariable;

  { TServerVariableMap }

  TServerVariableMap = Class(TNamedOpenAPIObjectList)
  Private
    function GetObject(const aName : TJSONStringType) : TServerVariable;
  Public
    constructor Create(aParent : TBaseOpenAPIObject; const aPathComponent : TJSONStringType); reintroduce;
    function AddItem(const aName : TJSONStringType) : TServerVariable;
    property ServerVariables[aName : TJSONStringType] : TServerVariable Read GetObject; default;
  end;

  { TComponents }

  TComponents = Class(TExtendableObject)
  Private
    FKeywords : TComponentsKeywords;
    FSchemas : TJSONSchemaMap;
    FResponses : TResponseOrReferenceMap;
    FParameters : TParameterOrReferenceMap;
    FExamples : TExampleOrReferenceMap;
    FRequestBodies : TRequestBodyOrReferenceMap;
    FHeaders : THeaderOrReferenceMap;
    FSecuritySchemes : TSecuritySchemeOrReferenceMap;
    FLinks : TLinkOrReferenceMap;
    FCallbacks : TLinkOrReferenceMap;
    FPathItems : TPathItemOrReferenceMap;
    Function Get_Schemas : TJSONSchemaMap;
    Function Get_Responses : TResponseOrReferenceMap;
    Function Get_Parameters : TParameterOrReferenceMap;
    Function Get_Examples : TExampleOrReferenceMap;
    Function Get_RequestBodies : TRequestBodyOrReferenceMap;
    Function Get_Headers : THeaderOrReferenceMap;
    Function Get_SecuritySchemes : TSecuritySchemeOrReferenceMap;
    Function Get_Links : TLinkOrReferenceMap;
    Function Get_Callbacks : TLinkOrReferenceMap;
    Function Get_PathItems : TPathItemOrReferenceMap;
    procedure Set_Schemas(const aValue : TJSONSchemaMap);
    procedure Set_Responses(const aValue : TResponseOrReferenceMap);
    procedure Set_Parameters(const aValue : TParameterOrReferenceMap);
    procedure Set_Examples(const aValue : TExampleOrReferenceMap);
    procedure Set_RequestBodies(const aValue : TRequestBodyOrReferenceMap);
    procedure Set_Headers(const aValue : THeaderOrReferenceMap);
    procedure Set_SecuritySchemes(const aValue : TSecuritySchemeOrReferenceMap);
    procedure Set_Links(const aValue : TLinkOrReferenceMap);
    procedure Set_Callbacks(const aValue : TLinkOrReferenceMap);
    procedure Set_PathItems(const aValue : TPathItemOrReferenceMap);
  Protected
    procedure AddKeyword(aKeyword : TComponentsKeyword); virtual;
    procedure RemoveKeyword(aKeyword : TComponentsKeyword); virtual;
  public
    destructor destroy; override;
    function HasKeyWord(aKeyword :TComponentsKeyword) : Boolean;
    property Schemas: TJSONSchemaMap Read Get_Schemas write Set_Schemas;
    property Responses: TResponseOrReferenceMap Read Get_Responses write Set_Responses;
    property Parameters: TParameterOrReferenceMap Read Get_Parameters write Set_Parameters;
    property Examples: TExampleOrReferenceMap Read Get_Examples write Set_Examples;
    property RequestBodies: TRequestBodyOrReferenceMap Read Get_RequestBodies write Set_RequestBodies;
    property Headers: THeaderOrReferenceMap Read Get_Headers write Set_Headers;
    property SecuritySchemes: TSecuritySchemeOrReferenceMap Read Get_SecuritySchemes write Set_SecuritySchemes;
    property Links: TLinkOrReferenceMap Read Get_Links write Set_Links;
    property Callbacks: TLinkOrReferenceMap Read Get_Callbacks write Set_Callbacks;
    property PathItems: TPathItemOrReferenceMap Read Get_PathItems write Set_PathItems;
  end;
  TComponentsArray = array of TComponents;

  { TPathItem }

  TPathItem = Class(TExtendableObject)
  Private
    FKeywords : TPathItemKeywords;
    FRef : string;
    FSummary : string;
    FDescription : string;
    FGet : TApiOperation;
    FPut : TApiOperation;
    FPost : TApiOperation;
    FDelete : TApiOperation;
    FOptions : TApiOperation;
    FHead : TApiOperation;
    FPatch : TApiOperation;
    FTrace : TApiOperation;
    FServers : TServerList;
    FParameters : TParameterOrReferenceList;
    Function Get_Get : TApiOperation;
    Function Get_Put : TApiOperation;
    Function Get_Post : TApiOperation;
    Function Get_Delete : TApiOperation;
    Function Get_Options : TApiOperation;
    Function Get_Head : TApiOperation;
    Function Get_Patch : TApiOperation;
    Function Get_Trace : TApiOperation;
    Function Get_Servers : TServerList;
    Function Get_Parameters : TParameterOrReferenceList;
    procedure Set_Ref(const aValue : string);
    procedure Set_Summary(const aValue : string);
    procedure Set_Description(const aValue : string);
    procedure Set_Get(const aValue : TApiOperation);
    procedure Set_Put(const aValue : TApiOperation);
    procedure Set_Post(const aValue : TApiOperation);
    procedure Set_Delete(const aValue : TApiOperation);
    procedure Set_Options(const aValue : TApiOperation);
    procedure Set_Head(const aValue : TApiOperation);
    procedure Set_Patch(const aValue : TApiOperation);
    procedure Set_Trace(const aValue : TApiOperation);
    procedure Set_Servers(const aValue : TServerList);
    procedure Set_Parameters(const aValue : TParameterOrReferenceList);
  Protected
    procedure AddKeyword(aKeyword : TPathItemKeyword); virtual;
    procedure RemoveKeyword(aKeyword : TPathItemKeyword); virtual;
  public
    destructor destroy; override;
    function HasKeyWord(aKeyword :TPathItemKeyword) : Boolean;
    function GetOperation(aKeyword : TPathItemOperationKeyword) : TAPIOperation;
    property Ref: string Read FRef write Set_Ref;
    property Summary: string Read FSummary write Set_Summary;
    property Description: string Read FDescription write Set_Description;
    property Get: TApiOperation Read Get_Get write Set_Get;
    property Put: TApiOperation Read Get_Put write Set_Put;
    property Post: TApiOperation Read Get_Post write Set_Post;
    property Delete: TApiOperation Read Get_Delete write Set_Delete;
    property Options: TApiOperation Read Get_Options write Set_Options;
    property Head: TApiOperation Read Get_Head write Set_Head;
    property Patch: TApiOperation Read Get_Patch write Set_Patch;
    property Trace: TApiOperation Read Get_Trace write Set_Trace;
    property Servers: TServerList Read Get_Servers write Set_Servers;
    property Parameters: TParameterOrReferenceList Read Get_Parameters write Set_Parameters;
  end;
  TPathItemArray = array of TPathItem;

  TPathItemOrReference = Class(TPathItem)
  Private
    FReference : TReference;
    function GetReference : TReference;
  Public
    destructor Destroy; override;
    function HasReference : Boolean;
    property Reference : TReference Read GetReference;
  end;

  { TPathItemOrReferenceMap }

  TPathItemOrReferenceMap = Class(TNamedOpenAPIObjectList)
  Private
    function GetObject(const aName : TJSONStringType) : TPathItemOrReference;
  Public
    constructor Create(aParent : TBaseOpenAPIObject; aPathComponent : TJSONStringType); reintroduce;
    function AddItem(const aName : TJSONStringType) : TPathItemOrReference;
    property PathItemOrReferences[aName : TJSONStringType] : TPathItemOrReference Read GetObject; default;
  end;

  { TApiOperation }

  TApiOperation = Class(TExtendableObject)
  Private
    FKeywords : TApiOperationKeywords;
    FTags : TStrings;
    FSummary : string;
    FDescription : string;
    FExternalDocs : TExternalDocumentation;
    FOperationId : string;
    FParameters : TParameterOrReferenceList;
    FRequestBody : TRequestBodyOrReference;
    FResponses : TResponses;
    FCallbacks : TCallBackOrReferenceMap;
    FDeprecated : Boolean;
    FSecurity : TSecurityRequirementList;
    FServers : TServerList;
    Function Get_Tags : TStrings;
    Function Get_ExternalDocs : TExternalDocumentation;
    Function Get_Parameters : TParameterOrReferenceList;
    Function Get_RequestBody : TRequestBodyOrReference;
    Function Get_Responses : TResponses;
    Function Get_Callbacks : TCallBackOrReferenceMap;
    Function Get_Security : TSecurityRequirementList;
    Function Get_Servers : TServerList;
    procedure Set_Tags(const aValue : TStrings);
    procedure Set_Summary(const aValue : string);
    procedure Set_Description(const aValue : string);
    procedure Set_ExternalDocs(const aValue : TExternalDocumentation);
    procedure Set_OperationId(const aValue : string);
    procedure Set_Parameters(const aValue : TParameterOrReferenceList);
    procedure Set_RequestBody(const aValue : TRequestBodyOrReference);
    procedure Set_Responses(const aValue : TResponses);
    procedure Set_Callbacks(const aValue : TCallBackOrReferenceMap);
    procedure Set_Deprecated(const aValue : Boolean);
    procedure Set_Security(const aValue : TSecurityRequirementList);
    procedure Set_Servers(const aValue : TServerList);
  Protected
    procedure AddKeyword(aKeyword : TApiOperationKeyword); virtual;
    procedure RemoveKeyword(aKeyword : TApiOperationKeyword); virtual;
  public
    destructor destroy; override;
    function HasKeyWord(aKeyword :TApiOperationKeyword) : Boolean;
    property Tags: TStrings Read Get_Tags write Set_Tags;
    property Summary: string Read FSummary write Set_Summary;
    property Description: string Read FDescription write Set_Description;
    property ExternalDocs: TExternalDocumentation Read Get_ExternalDocs write Set_ExternalDocs;
    property OperationId: string Read FOperationId write Set_OperationId;
    property Parameters: TParameterOrReferenceList Read Get_Parameters write Set_Parameters;
    property RequestBody: TRequestBodyOrReference Read Get_RequestBody write Set_RequestBody;
    property Responses: TResponses Read Get_Responses write Set_Responses;
    property Callbacks: TCallBackOrReferenceMap Read Get_Callbacks write Set_Callbacks;
    property Deprecated: Boolean Read FDeprecated write Set_Deprecated;
    property Security: TSecurityRequirementList Read Get_Security write Set_Security;
    property Servers: TServerList Read Get_Servers write Set_Servers;
  end;
  TApiOperationArray = array of TApiOperation;

  { TExternalDocumentation }

  TExternalDocumentation = Class(TExtendableObject)
  Private
    FKeywords : TExternalDocumentationKeywords;
    FDescription : string;
    FUrl : string;
    procedure Set_Description(const aValue : string);
    procedure Set_Url(const aValue : string);
  Protected
    procedure AddKeyword(aKeyword : TExternalDocumentationKeyword); virtual;
    procedure RemoveKeyword(aKeyword : TExternalDocumentationKeyword); virtual;
  public
    destructor destroy; override;
    function HasKeyWord(aKeyword :TExternalDocumentationKeyword) : Boolean;
    property Description: string Read FDescription write Set_Description;
    property Url: string Read FUrl write Set_Url;
  end;
  TExternalDocumentationArray = array of TExternalDocumentation;

  { TParameter }

  TParameterOrHeader = Class(TExtendableObject)
  Private
    FKeywords : TParameterKeywords;
    FName : string;
    FIn_ : string;
    FDescription : string;
    FRequired : Boolean;
    FDeprecated : Boolean;
    FAllowEmptyValue : Boolean;
    FStyle : string;
    FExplode : Boolean;
    FAllowReserved : Boolean;
    FSchema : TJSONSchema;
    FExample : TJSONData;
    FExamples : TExampleOrReferenceMap;
    FContent : TMediaTypeMap;
    Function Get_Schema : TJSONSchema;
    Function Get_Examples : TExampleOrReferenceMap;
    Function Get_Content : TMediaTypeMap;
    procedure Set_Name(const aValue : string); virtual;
    procedure Set_In_(const aValue : string); virtual;
    procedure Set_Description(const aValue : string);
    procedure Set_Required(const aValue : Boolean);
    procedure Set_Deprecated(const aValue : Boolean);
    procedure Set_AllowEmptyValue(const aValue : Boolean);
    procedure Set_Style(const aValue : string);
    procedure Set_Explode(const aValue : Boolean);
    procedure Set_AllowReserved(const aValue : Boolean);
    procedure Set_Schema(const aValue : TJSONSchema);
    procedure Set_Example(const aValue : TJSONData);
    procedure Set_Examples(const aValue : TExampleOrReferenceMap);
    procedure Set_Content(const aValue : TMediaTypeMap);
  Protected
    procedure AddKeyword(aKeyword : TParameterKeyword); virtual;
    procedure RemoveKeyword(aKeyword : TParameterKeyword); virtual;
    property Name: string Read FName write Set_Name;
    property In_: string Read FIn_ write Set_In_;
  public
    destructor destroy; override;
    function HasKeyWord(aKeyword :TParameterKeyword) : Boolean;
    property Description: string Read FDescription write Set_Description;
    property Required: Boolean Read FRequired write Set_Required;
    property Deprecated: Boolean Read FDeprecated write Set_Deprecated;
    property AllowEmptyValue: Boolean Read FAllowEmptyValue write Set_AllowEmptyValue;
    property Style: string Read FStyle write Set_Style;
    property Explode: Boolean Read FExplode write Set_Explode;
    property AllowReserved: Boolean Read FAllowReserved write Set_AllowReserved;
    property Schema: TJSONSchema Read Get_Schema write Set_Schema;
    property Example: TJSONData Read FExample write Set_Example;
    property Examples: TExampleOrReferenceMap Read Get_Examples write Set_Examples;
    property Content: TMediaTypeMap Read Get_Content write Set_Content;
  end;

  TParameter = Class(TParameterOrHeader)
  Public
    Property Name;
    property In_;
  end;
  TParameterArray = array of TParameter;

  THeader = Class(TParameterOrHeader)
  end;
  THeaderArray = array of THeader;


  TParameterOrReference = Class(TParameter)
  Private
    FReference : TReference;
    function GetReference : TReference;
  Public
    destructor Destroy; override;
    function HasReference : Boolean;
    property Reference : TReference Read GetReference;
  end;

  { TParameterOrReferenceMap }

  TParameterOrReferenceMap = Class(TNamedOpenAPIObjectList)
  Private
    function GetObject(const aName : TJSONStringType) : TParameterOrReference;
  Public
    constructor Create(aParent : TBaseOpenAPIObject; const aPathComponent : TJSONStringType); reintroduce;
    function AddItem(const aName : TJSONStringType) : TParameterOrReference;
    property ParameterOrReferences[aName : TJSONStringType] : TParameterOrReference Read GetObject; default;
  end;

  { TParameterStyle }

  TParameterStyle = Class(TExtendableObject)
  Private
    FKeywords : TParameterStyleKeywords;
    FMatrix : string;
    FLabel_ : string;
    FForm : string;
    FSimple : string;
    FSpaceDelimited : string;
    FPipeDelimited : string;
    FDeepObject : string;
    procedure Set_Matrix(const aValue : string);
    procedure Set_Label_(const aValue : string);
    procedure Set_Form(const aValue : string);
    procedure Set_Simple(const aValue : string);
    procedure Set_SpaceDelimited(const aValue : string);
    procedure Set_PipeDelimited(const aValue : string);
    procedure Set_DeepObject(const aValue : string);
  Protected
    procedure AddKeyword(aKeyword : TParameterStyleKeyword); virtual;
    procedure RemoveKeyword(aKeyword : TParameterStyleKeyword); virtual;
  public
    destructor destroy; override;
    function HasKeyWord(aKeyword :TParameterStyleKeyword) : Boolean;
    property Matrix: string Read FMatrix write Set_Matrix;
    property Label_: string Read FLabel_ write Set_Label_;
    property Form: string Read FForm write Set_Form;
    property Simple: string Read FSimple write Set_Simple;
    property SpaceDelimited: string Read FSpaceDelimited write Set_SpaceDelimited;
    property PipeDelimited: string Read FPipeDelimited write Set_PipeDelimited;
    property DeepObject: string Read FDeepObject write Set_DeepObject;
  end;
  TParameterStyleArray = array of TParameterStyle;

  { TRequestBody }

  TRequestBody = Class(TExtendableObject)
  Private
    FKeywords : TRequestBodyKeywords;
    FDescription : string;
    FContent : TMediaTypeMap;
    FRequired : Boolean;
    Function Get_Content : TMediaTypeMap;
    procedure Set_Description(const aValue : string);
    procedure Set_Content(const aValue : TMediaTypeMap);
    procedure Set_Required(const aValue : Boolean);
  Protected
    procedure AddKeyword(aKeyword : TRequestBodyKeyword); virtual;
    procedure RemoveKeyword(aKeyword : TRequestBodyKeyword); virtual;
  public
    destructor destroy; override;
    function HasKeyWord(aKeyword :TRequestBodyKeyword) : Boolean;
    property Description: string Read FDescription write Set_Description;
    property Content: TMediaTypeMap Read Get_Content write Set_Content;
    property Required: Boolean Read FRequired write Set_Required;
  end;
  TRequestBodyArray = array of TRequestBody;


  TRequestBodyOrReference = Class(TRequestBody)
  Private
    FReference : TReference;
    function GetReference : TReference;
  Public
    destructor Destroy; override;
    function HasReference : Boolean;
    property Reference : TReference Read GetReference;
  end;

  { TRequestBodyOrReferenceMap }

  TRequestBodyOrReferenceMap = Class(TNamedOpenAPIObjectList)
  Private
    function GetObject(const aName : TJSONStringType) : TRequestBodyOrReference;
  Public
    constructor Create(aParent : TBaseOpenAPIObject; const aPathComponent : TJSONStringType); reintroduce;
    function AddItem(const aName : TJSONStringType) : TRequestBodyOrReference;
    property RequestBodyOrReferences[aName : TJSONStringType] : TRequestBodyOrReference Read GetObject; default;
  end;

  { TMediaType }

  TMediaType = Class(TExtendableObject)
  Private
    FKeywords : TMediaTypeKeywords;
    FSchema : TJSONSchema;
    FExample : TJSONData;
    FExamples : TExampleOrReferenceMap;
    FEncoding : TEncodingMap;
    Function Get_Schema : TJSONSchema;
    Function Get_Examples : TExampleOrReferenceMap;
    Function Get_Encoding : TEncodingMap;
    procedure Set_Schema(const aValue : TJSONSchema);
    procedure Set_Example(const aValue : TJSONData);
    procedure Set_Examples(const aValue : TExampleOrReferenceMap);
    procedure Set_Encoding(const aValue : TEncodingMap);
  Protected
    procedure AddKeyword(aKeyword : TMediaTypeKeyword); virtual;
    procedure RemoveKeyword(aKeyword : TMediaTypeKeyword); virtual;
  public
    destructor destroy; override;
    function HasKeyWord(aKeyword :TMediaTypeKeyword) : Boolean;
    property Schema: TJSONSchema Read Get_Schema write Set_Schema;
    property Example: TJSONData Read FExample write Set_Example;
    property Examples: TExampleOrReferenceMap Read Get_Examples write Set_Examples;
    property Encoding: TEncodingMap Read Get_Encoding write Set_Encoding;
  end;
  TMediaTypeArray = array of TMediaType;

  { TMediaTypeMap }

  TMediaTypeMap = Class(TNamedOpenAPIObjectList)
  Private
    function GetObject(const aName : TJSONStringType) : TMediaType;
  Public
    constructor Create(aParent : TBaseOpenAPIObject; const aPathcomponent : TJSONStringType); reintroduce;
    function AddItem(const aName : TJSONStringType) : TMediaType;
    property MediaTypes[aName : TJSONStringType] : TMediaType Read GetObject; default;
  end;

  { TEncoding }

  TEncoding = Class(TExtendableObject)
  Private
    FKeywords : TEncodingKeywords;
    FContentType : string;
    FHeaders : THeaderOrReferenceMap;
    FStyle : string;
    FExplode : Boolean;
    FAllowReserved : Boolean;
    Function Get_Headers : THeaderOrReferenceMap;
    procedure Set_ContentType(const aValue : string);
    procedure Set_Headers(const aValue : THeaderOrReferenceMap);
    procedure Set_Style(const aValue : string);
    procedure Set_Explode(const aValue : Boolean);
    procedure Set_AllowReserved(const aValue : Boolean);
  Protected
    procedure AddKeyword(aKeyword : TEncodingKeyword); virtual;
    procedure RemoveKeyword(aKeyword : TEncodingKeyword); virtual;
  public
    destructor destroy; override;
    function HasKeyWord(aKeyword :TEncodingKeyword) : Boolean;
    property ContentType: string Read FContentType write Set_ContentType;
    property Headers: THeaderOrReferenceMap Read Get_Headers write Set_Headers;
    property Style: string Read FStyle write Set_Style;
    property Explode: Boolean Read FExplode write Set_Explode;
    property AllowReserved: Boolean Read FAllowReserved write Set_AllowReserved;
  end;
  TEncodingArray = array of TEncoding;

  { TEncodingMap }

  TEncodingMap = Class(TNamedOpenAPIObjectList)
  Private
    function GetObject(const aName : TJSONStringType) : TEncoding;
  Public
    constructor Create(aParent : TBaseOpenAPIObject; const aPathComponent : TJSONStringType); reintroduce;
    function AddItem(const aName : TJSONStringType) : TEncoding;
    property Encodings[aName : TJSONStringType] : TEncoding Read GetObject; default;
  end;

  { TResponses }

  TResponses = Class(TNamedOpenAPIObjectList)
  Private
    function GetObject(const aName : TJSONStringType) : TResponse;
    function GetResponseByIndex(aIndex : Integer): TResponse;
  Public
    constructor Create(aParent : TBaseOpenAPIObject; const aPathComponent : TJSONStringType); reintroduce;
    function AddItem(const aName : TJSONStringType) : TResponse;
    property ResponseByIndex[aIndex : Integer] : TResponse Read GetResponseByIndex;
    property Encodings[aName : TJSONStringType] : TResponse Read GetObject; default;
  end;
  TResponsesArray = array of TResponses;

  { TResponse }

  TResponse = Class(TExtendableObject)
  Private
    FKeywords : TResponseKeywords;
    FDescription : string;
    FHeaders : THeaderOrReferenceMap;
    FContent : TMediaTypeMap;
    FLinks : TLinkOrReferenceMap;
    Function Get_Headers : THeaderOrReferenceMap;
    Function Get_Content : TMediaTypeMap;
    Function Get_Links : TLinkOrReferenceMap;
    procedure Set_Description(const aValue : string);
    procedure Set_Headers(const aValue : THeaderOrReferenceMap);
    procedure Set_Content(const aValue : TMediaTypeMap);
    procedure Set_Links(const aValue : TLinkOrReferenceMap);
  Protected
    procedure AddKeyword(aKeyword : TResponseKeyword); virtual;
    procedure RemoveKeyword(aKeyword : TResponseKeyword); virtual;
  public
    destructor destroy; override;
    function HasKeyWord(aKeyword :TResponseKeyword) : Boolean;
    property Description: string Read FDescription write Set_Description;
    property Headers: THeaderOrReferenceMap Read Get_Headers write Set_Headers;
    property Content: TMediaTypeMap Read Get_Content write Set_Content;
    property Links: TLinkOrReferenceMap Read Get_Links write Set_Links;
  end;
  TResponseArray = array of TResponse;

  TResponseOrReference = Class(TResponse)
  Private
    FReference : TReference;
    function GetReference : TReference;
  Public
    destructor Destroy; override;
    function HasReference : Boolean;
    property Reference : TReference Read GetReference;
  end;

  { TResponseOrReferenceMap }

  TResponseOrReferenceMap = Class(TNamedOpenAPIObjectList)
  Private
    function GetObject(const aName : TJSONStringType) : TResponseOrReference;
  Public
    constructor Create(aParent : TBaseOpenAPIObject; const aPathComponent : TJSONStringType); reintroduce;
    function AddItem(const aName : TJSONStringType) : TResponseOrReference;
    property ResponseOrReferences[aName : TJSONStringType] : TResponseOrReference Read GetObject; default;
  end;

  { TExample }

  TExample = Class(TExtendableObject)
  Private
    FKeywords : TExampleKeywords;
    FSummary : string;
    FDescription : string;
    FValue : TJSONData;
    FExternalValue : string;
    procedure Set_Summary(const aValue : string);
    procedure Set_Description(const aValue : string);
    procedure Set_Value(const aValue : TJSONData);
    procedure Set_ExternalValue(const aValue : string);
  Protected
    procedure AddKeyword(aKeyword : TExampleKeyword); virtual;
    procedure RemoveKeyword(aKeyword : TExampleKeyword); virtual;
  public
    destructor destroy; override;
    function HasKeyWord(aKeyword :TExampleKeyword) : Boolean;
    property Summary: string Read FSummary write Set_Summary;
    property Description: string Read FDescription write Set_Description;
    property Value: TJSONData Read FValue write Set_Value;
    property ExternalValue: string Read FExternalValue write Set_ExternalValue;
  end;
  TExampleArray = array of TExample;

  TExampleOrReference = Class(TExample)
  Private
    FReference : TReference;
    function GetReference : TReference;
  Public
    destructor Destroy; override;
    function HasReference : Boolean;
    property Reference : TReference Read GetReference;
  end;

  { TExampleOrReferenceMap }

  TExampleOrReferenceMap = Class(TNamedOpenAPIObjectList)
  Private
    function GetObject(const aName : TJSONStringType) : TExampleOrReference;
  Public
    constructor Create(aParent : TBaseOpenAPIObject; const aPathComponent : TJSONStringType); reintroduce;
    function AddItem(const aName : TJSONStringType) : TExampleOrReference;
    property ExampleOrReferences[aName : TJSONStringType] : TExampleOrReference Read GetObject; default;
  end;

  { TLink }

  TLink = Class(TExtendableObject)
  Private
    FKeywords : TLinkKeywords;
    FOperationRef : string;
    FOperationId : string;
    FParameters : TJSONObject;
    FRequestBody : TJSONData;
    FDescription : string;
    FServer : TServer;
    Function Get_Parameters : TJSONObject;
    Function Get_Server : TServer;
    procedure Set_OperationRef(const aValue : string);
    procedure Set_OperationId(const aValue : string);
    procedure Set_Parameters(const aValue : TJSONObject);
    procedure Set_RequestBody(const aValue : TJSONData);
    procedure Set_Description(const aValue : string);
    procedure Set_Server(const aValue : TServer);
  Protected
    procedure AddKeyword(aKeyword : TLinkKeyword); virtual;
    procedure RemoveKeyword(aKeyword : TLinkKeyword); virtual;
  public
    destructor destroy; override;
    function HasKeyWord(aKeyword :TLinkKeyword) : Boolean;
    property OperationRef: string Read FOperationRef write Set_OperationRef;
    property OperationId: string Read FOperationId write Set_OperationId;
    property Parameters: TJSONObject Read Get_Parameters write Set_Parameters;
    property RequestBody: TJSONData Read FRequestBody write Set_RequestBody;
    property Description: string Read FDescription write Set_Description;
    property Server: TServer Read Get_Server write Set_Server;
  end;
  TLinkArray = array of TLink;

  TLinkOrReference = Class(TLink)
  Private
    FReference : TReference;
    function GetReference : TReference;
  Public
    destructor Destroy; override;
    function HasReference : Boolean;
    property Reference : TReference Read GetReference;
  end;

  { TLinkOrReferenceMap }

  TLinkOrReferenceMap = Class(TNamedOpenAPIObjectList)
  Private
    function GetObject(const aName : TJSONStringType) : TLinkOrReference;
  Public
    constructor Create(aParent : TBaseOpenAPIObject; const aPathComponent : TJSONStringType); reintroduce;
    function AddItem(const aName : TJSONStringType) : TLinkOrReference;
    property LinkOrReferences[aName : TJSONStringType] : TLinkOrReference Read GetObject; default;
  end;

  { TTag }

  TTag = Class(TExtendableObject)
  Private
    FKeywords : TTagKeywords;
    FName : string;
    FDescription : string;
    FExternalDocs : TExternalDocumentation;
    Function Get_ExternalDocs : TExternalDocumentation;
    procedure Set_Name(const aValue : string);
    procedure Set_Description(const aValue : string);
    procedure Set_ExternalDocs(const aValue : TExternalDocumentation);
  Protected
    procedure AddKeyword(aKeyword : TTagKeyword); virtual;
    procedure RemoveKeyword(aKeyword : TTagKeyword); virtual;
  public
    destructor destroy; override;
    function HasKeyWord(aKeyword :TTagKeyword) : Boolean;
    property Name: string Read FName write Set_Name;
    property Description: string Read FDescription write Set_Description;
    property ExternalDocs: TExternalDocumentation Read Get_ExternalDocs write Set_ExternalDocs;
  end;
  TTagArray = array of TTag;


  { TReference }

  TReference = Class(TExtendableObject)
  Private
    FKeywords : TReferenceKeywords;
    FRef : string;
    FSummary : string;
    FDescription : string;
    procedure Set_Ref(const aValue : string);
    procedure Set_Summary(const aValue : string);
    procedure Set_Description(const aValue : string);
  Protected
    procedure AddKeyword(aKeyword : TReferenceKeyword); virtual;
    procedure RemoveKeyword(aKeyword : TReferenceKeyword); virtual;
  public
    destructor destroy; override;
    function HasKeyWord(aKeyword :TReferenceKeyword) : Boolean;
    property Ref: string Read FRef write Set_Ref;
    property Summary: string Read FSummary write Set_Summary;
    property Description: string Read FDescription write Set_Description;
  end;
  TReferenceArray = array of TReference;

  { TSchema }

  TSchema = Class(TExtendableObject)
  Private
    FKeywords : TSchemaKeywords;
    FDiscriminator : TDiscriminator;
    FXML : TXML;
    FExternalDocs : TExternalDocumentation;
    FExample : TJSONData;
    Function Get_Discriminator : TDiscriminator;
    Function Get_XML : TXML;
    Function Get_ExternalDocs : TExternalDocumentation;
    procedure Set_Discriminator(const aValue : TDiscriminator);
    procedure Set_XML(const aValue : TXML);
    procedure Set_ExternalDocs(const aValue : TExternalDocumentation);
    procedure Set_Example(const aValue : TJSONData);
  Protected
    procedure AddKeyword(aKeyword : TSchemaKeyword); virtual;
    procedure RemoveKeyword(aKeyword : TSchemaKeyword); virtual;
  public
    destructor destroy; override;
    function HasKeyWord(aKeyword :TSchemaKeyword) : Boolean;
    property Discriminator: TDiscriminator Read Get_Discriminator write Set_Discriminator;
    property XML: TXML Read Get_XML write Set_XML;
    property ExternalDocs: TExternalDocumentation Read Get_ExternalDocs write Set_ExternalDocs;
    property Example: TJSONData Read FExample write Set_Example;
  end;
  TSchemaArray = array of TSchema;

  { TDiscriminator }

  TDiscriminator = Class(TExtendableObject)
  Private
    FKeywords : TDiscriminatorKeywords;
    FPropertyName : string;
    FMapping : TStrings;
    Function Get_Mapping : TStrings;
    procedure Set_PropertyName(const aValue : string);
    procedure Set_Mapping(const aValue : TStrings);
  Protected
    procedure AddKeyword(aKeyword : TDiscriminatorKeyword); virtual;
    procedure RemoveKeyword(aKeyword : TDiscriminatorKeyword); virtual;
  public
    destructor destroy; override;
    function HasKeyWord(aKeyword :TDiscriminatorKeyword) : Boolean;
    property PropertyName: string Read FPropertyName write Set_PropertyName;
    property Mapping: TStrings Read Get_Mapping write Set_Mapping;
  end;
  TDiscriminatorArray = array of TDiscriminator;

  { TXML }

  TXML = Class(TExtendableObject)
  Private
    FKeywords : TXMLKeywords;
    FName : string;
    FNamespace : string;
    FPrefix : string;
    FAttribute : Boolean;
    FWrapped : Boolean;
    procedure Set_Name(const aValue : string);
    procedure Set_Namespace(const aValue : string);
    procedure Set_Prefix(const aValue : string);
    procedure Set_Attribute(const aValue : Boolean);
    procedure Set_Wrapped(const aValue : Boolean);
  Protected
    procedure AddKeyword(aKeyword : TXMLKeyword); virtual;
    procedure RemoveKeyword(aKeyword : TXMLKeyword); virtual;
  public
    destructor destroy; override;
    function HasKeyWord(aKeyword :TXMLKeyword) : Boolean;
    property Name: string Read FName write Set_Name;
    property Namespace: string Read FNamespace write Set_Namespace;
    property Prefix: string Read FPrefix write Set_Prefix;
    property Attribute: Boolean Read FAttribute write Set_Attribute;
    property Wrapped: Boolean Read FWrapped write Set_Wrapped;
  end;
  TXMLArray = array of TXML;

  { TSecurityScheme }

  TSecurityScheme = Class(TExtendableObject)
  Private
    FKeywords : TSecuritySchemeKeywords;
    FType_ : string;
    FDescription : string;
    FName : string;
    FIn_ : string;
    FScheme : string;
    FBearerFormat : string;
    FFlows : TOAuthFlows;
    FOpenIdConnectUrl : string;
    Function Get_Flows : TOAuthFlows;
    procedure Set_Type_(const aValue : string);
    procedure Set_Description(const aValue : string);
    procedure Set_Name(const aValue : string);
    procedure Set_In_(const aValue : string);
    procedure Set_Scheme(const aValue : string);
    procedure Set_BearerFormat(const aValue : string);
    procedure Set_Flows(const aValue : TOAuthFlows);
    procedure Set_OpenIdConnectUrl(const aValue : string);
  Protected
    procedure AddKeyword(aKeyword : TSecuritySchemeKeyword); virtual;
    procedure RemoveKeyword(aKeyword : TSecuritySchemeKeyword); virtual;
  public
    destructor destroy; override;
    function HasKeyWord(aKeyword :TSecuritySchemeKeyword) : Boolean;
    property Type_: string Read FType_ write Set_Type_;
    property Description: string Read FDescription write Set_Description;
    property Name: string Read FName write Set_Name;
    property In_: string Read FIn_ write Set_In_;
    property Scheme: string Read FScheme write Set_Scheme;
    property BearerFormat: string Read FBearerFormat write Set_BearerFormat;
    property Flows: TOAuthFlows Read Get_Flows write Set_Flows;
    property OpenIdConnectUrl: string Read FOpenIdConnectUrl write Set_OpenIdConnectUrl;
  end;
  TSecuritySchemeArray = array of TSecurityScheme;

  TSecuritySchemeOrReference = Class(TSecurityScheme)
  Private
    FReference : TReference;
    function GetReference : TReference;
  Public
    destructor Destroy; override;
    function HasReference : Boolean;
    property Reference : TReference Read GetReference;
  end;

  { TSecuritySchemeOrReferenceMap }

  TSecuritySchemeOrReferenceMap = Class(TNamedOpenAPIObjectList)
  Private
    function GetObject(const aName : TJSONStringType) : TSecuritySchemeOrReference;
  Public
    constructor Create(aParent : TBaseOpenAPIObject; const aPathComponent : TJSONStringType); reintroduce;
    function AddItem(const aName : TJSONStringType) : TSecuritySchemeOrReference;
    property SecuritySchemeOrReferences[aName : TJSONStringType] : TSecuritySchemeOrReference Read GetObject; default;
  end;

  { TOAuthFlows }

  TOAuthFlows = Class(TExtendableObject)
  Private
    FKeywords : TOAuthFlowsKeywords;
    FImplicit : TOAuthFlow;
    FPassword : TOAuthFlow;
    FClientCredentials : TOAuthFlow;
    FClientAuthorizationCode : TOAuthFlow;
    Function Get_Implicit : TOAuthFlow;
    Function Get_Password : TOAuthFlow;
    Function Get_ClientCredentials : TOAuthFlow;
    Function Get_ClientAuthorizationCode : TOAuthFlow;
    procedure Set_Implicit(const aValue : TOAuthFlow);
    procedure Set_Password(const aValue : TOAuthFlow);
    procedure Set_ClientCredentials(const aValue : TOAuthFlow);
    procedure Set_ClientAuthorizationCode(const aValue : TOAuthFlow);
  Protected
    procedure AddKeyword(aKeyword : TOAuthFlowsKeyword); virtual;
    procedure RemoveKeyword(aKeyword : TOAuthFlowsKeyword); virtual;
  public
    destructor destroy; override;
    function HasKeyWord(aKeyword :TOAuthFlowsKeyword) : Boolean;
    property Implicit: TOAuthFlow Read Get_Implicit write Set_Implicit;
    property Password: TOAuthFlow Read Get_Password write Set_Password;
    property ClientCredentials: TOAuthFlow Read Get_ClientCredentials write Set_ClientCredentials;
    property ClientAuthorizationCode: TOAuthFlow Read Get_ClientAuthorizationCode write Set_ClientAuthorizationCode;
  end;
  TOAuthFlowsArray = array of TOAuthFlows;

  { TOauthFlow }

  TOauthFlow = Class(TExtendableObject)
  Private
    FKeywords : TOauthFlowKeywords;
    FAuthorizationUrl : string;
    FTokenURL : string;
    FRefreshURL : string;
    FScopes : TStrings;
    Function Get_Scopes : TStrings;
    procedure Set_AuthorizationUrl(const aValue : string);
    procedure Set_TokenURL(const aValue : string);
    procedure Set_RefreshURL(const aValue : string);
    procedure Set_Scopes(const aValue : TStrings);
  Protected
    procedure AddKeyword(aKeyword : TOauthFlowKeyword); virtual;
    procedure RemoveKeyword(aKeyword : TOauthFlowKeyword); virtual;
  public
    destructor destroy; override;
    function HasKeyWord(aKeyword :TOauthFlowKeyword) : Boolean;
    property AuthorizationUrl: string Read FAuthorizationUrl write Set_AuthorizationUrl;
    property TokenURL: string Read FTokenURL write Set_TokenURL;
    property RefreshURL: string Read FRefreshURL write Set_RefreshURL;
    property Scopes: TStrings Read Get_Scopes write Set_Scopes;
  end;
  TOauthFlowArray = array of TOauthFlow;

  { THeader }


  THeaderOrReference = Class(THeader)
  Private
    FReference : TReference;
    function GetReference : TReference;
  Public
    destructor Destroy; override;
    function HasReference : Boolean;
    property Reference : TReference Read GetReference;
  end;

  { THeaderOrReferenceMap }

  THeaderOrReferenceMap = Class(TNamedOpenAPIObjectList)
  Private
    function GetObject(const aName : TJSONStringType) : THeaderOrReference;
  Public
    constructor Create(aParent : TBaseOpenAPIObject; const aPathComponent : TJSONStringType); reintroduce;
    function AddItem(const aName : TJSONStringType) : THeaderOrReference;
    property HeaderOrReferences[aName : TJSONStringType] : THeaderOrReference Read GetObject; default;
  end;

  { TCallback }

  TCallback = Class(TPathItemOrReferenceMap);

  TCallbackArray = array of TCallback;

  TCallbackOrReference = Class(TCallback)
  Private
    FReference : TReference;
    function GetReference : TReference;
  Public
    destructor Destroy; override;
    function HasReference : Boolean;
    property Reference : TReference Read GetReference;
  end;

  { TCallbackOrReferenceMap }

  TCallbackOrReferenceMap = Class(TNamedOpenAPIObjectList)
  Private
    function GetObject(const aName : TJSONStringType) : TCallbackOrReference;
  Public
    constructor Create(aParent : TBaseOpenAPIObject; const aPathComponent : TJSONStringType); reintroduce;
    function AddItem(const aName : TJSONStringType) : TCallbackOrReference;
    property CallbackOrReferences[aName : TJSONStringType] : TCallbackOrReference Read GetObject; default;
  end;

  { TNamedStringList }

  TNamedStringList = class(TNamedObjectList)
  Public
    Function CreateNew(const aName : TJSONStringType) : TObject; override;
  end;

  { TSecurityRequirement }

  TSecurityRequirement = Class(TBaseOpenAPIObject)
  Private
    FList : TNamedStringList;
    function GetCount: Integer;
    function GetList(aIndex : Integer): TStrings;
    function GetName(aIndex : Integer): String;
    function GetRequirements(aName : TJSONStringType): TStrings;
  public
    constructor Create (aParent : TBaseOpenAPIObject; const aPathComponent : TJSONStringType); reintroduce;
    destructor destroy; override;
    Function AddItem(const AName : TJSONStringType) : TStrings;
    property Requirements[aName : TJSONStringType] : TStrings Read GetRequirements;
    property Names[aIndex : Integer] : String Read GetName;
    property Lists[aIndex : Integer] : TStrings Read GetList;
    Property Count : Integer Read GetCount;
  end;
  TSecurityRequirementArray = array of TSecurityRequirement;

  { TSecurityRequirementList }

  TSecurityRequirementList = class(TFPObjectList)
  Private
    function GetObj(aIndex : Integer): TSecurityRequirement;
    procedure SetObj(aIndex : Integer; aValue : TSecurityRequirement);
  Public
    function ToArray : TSecurityRequirementArray;
    Procedure AddSecurity(aSecurity : TSecurityRequirement);
    function AddSecurity(aParent : TBaseOpenAPIObject) : TSecurityRequirement;
    property SecurityRequirements[aIndex : integer] : TSecurityRequirement Read GetObj Write SetObj; default;
  end;


function EncodeJSONPointer(S : TJSONStringType) : TJSONStringType;
function DecodeJSONPointer(S : TJSONStringType) : TJSONStringType;

implementation

function EncodeJSONPointer(S : TJSONStringType) : TJSONStringType;

Const ZeroOne : Array[Boolean] of char = ('0','1');

var
  I,Count : Integer;
  C : TJSONCharType;

begin
  Count:=0;
  For I:=1 to Length(S) do
    if S[i] in ['~','/'] then
      Inc(Count);
  if Count=0 then
    Exit(S);
  SetLength(Result,Length(S)+Count);
  Count:=1;
  For I:=1 to Length(S) do
    begin
    C:=S[I];
    if Not (C in ['~','/']) then
      Result[Count]:=C
    else
      begin
      Result[Count]:='~';
      Inc(Count);
      Result[Count]:=ZeroOne[C='/'];
      end;
    inc(Count);
    end;
end;

function DecodeJSONPointer(S : TJSONStringType) : TJSONStringType;

begin
  Result:=StringReplace(S,'~1','/',[rfReplaceAll]);
  Result:=StringReplace(Result,'~0','~',[rfReplaceAll]);
end;

{ TExtendableObject }

function TExtendableObject.GetExtensions: TJSONObject;
begin
  if FExtensions=Nil then
    FExtensions:=TJSONObject.Create;
  Result:=FExtensions;
end;

procedure TExtendableObject.SetExtensions(AValue: TJSONObject);
begin
  If (FExtensions=aValue) then exit;
  FreeAndNil(FExtensions);
  FExtensions:=aValue;
end;

function TExtendableObject.CreateExtensions: TJSONObject;
begin
  Result:=TJSONObject.Create;
end;

function TExtendableObject.HasExtensions: Boolean;
begin
  Result:=Assigned(FExtensions) and (FExtensions.Count>0);
end;

{ TNamedObject }

constructor TNamedObject.Create(aName: TJSONStringType; aObject: TObject);
begin
  FName:=aName;
  FObject:=aObject;
end;

destructor TNamedObject.Destroy;
begin
  FreeAndNil(FObject);
  inherited Destroy;
end;


{ TNamedOpenAPIObjectList }

function TNamedObjectList.GetName(aIndex : Integer): TJSONStringType;
begin
  Result:=NamedObjects[aIndex].Name;
end;

function TNamedObjectList.GetCount: Integer;
begin
  Result:=FList.Count;
end;

function TNamedObjectList.GetNamedObject(aIndex: Integer): TNamedObject;
begin
  Result:=TNamedObject(FList[aIndex]);
end;


procedure TNamedObjectList.SetName(aIndex : Integer; AValue: TJSONStringType);
begin
  NamedObjects[aIndex].FName:=aValue;
end;

procedure TNamedObjectList.SetNamedObject(aIndex: Integer; AValue: TNamedObject);
begin
  FList[aIndex]:=aValue;
end;


function TNamedObjectList.GetObjectByName(const aName: String): TObject;

var
  Obj : TNamedObject;
begin
  Result:=Nil;
  Obj:=TNamedObject(FHash.Items[aName]);
  If Assigned(Obj) then
    Result:=Obj.Object_;
end;

procedure TNamedObjectList.Add(const AName: TJSONStringType; aObject: TObject);

var
  Itm : TNamedObject;

begin
  if not aObject.InheritsFrom(FType) then
    Raise EListError.CreateFmt('%s is not of type %s',[aObject.ClassName,FType.ClassName]);
  Itm:=TNamedObject.Create(aName,aObject);
  FList.Add(Itm);
  FHash.Add(aName,Itm);
end;


constructor TNamedObjectList.Create(aParent: TBaseOpenAPIObject; const aPathComponent : TJSONStringType; aType: TClass);
begin
  Inherited Create(aParent,aPathComponent);
  FType:=aType;
  FList:=TFPObjectList.Create;
  FHash:=TFPObjectHashTable.Create(False);
end;

function TNamedOpenAPIObjectList.GetApiObject(aIndex : Integer): TBaseOpenAPIObject;
begin
  Result:=NamedObjects[aIndex].Object_ as TBaseOpenAPIObject;
end;

constructor TNamedOpenAPIObjectList.Create(aParent: TBaseOpenAPIObject; const aPathComponent : TJSONStringType; aType: TClass);
begin
  if not aType.InheritsFrom(TBaseOpenAPIObject) then
    Raise EOpenAPi.CreateFmt('Class %s does not inherit from TOpenAPIObject',[aType.ClassName]);
  Inherited Create(aParent,aPathComponent,aType);
end;

function TNamedOpenAPIObjectList.CreateNew(const AName: TJSONStringType): TObject;
begin
  Result:=TOpenAPIObjectClass(FType).create(Self,aName);
  Add(aName,Result);
end;

{ TJSONSchemaMap }

function TJSONSchemaMap.GetNamedSchema(aName: TJSONStringType): TJSONSchema;
begin
  Result:=TJSONSchema(GetObjectByName(aName));
end;

function TJSONSchemaMap.CreateNew(const AName: TJSONStringType): TObject;
begin
  if aName<>'' then ;
  Result:=TJSONSchema.Create;
end;

function TJSONSchemaMap.Add(const AName: TJSONStringType): TJSONSchema;
begin
  Result:=TJSONSchema(CreateNew(aName));
  Inherited Add(aName,Result);
end;

destructor TNamedObjectList.Destroy;
begin
  FreeAndNil(FList);
  FreeAndNil(FHash);
  inherited Destroy;
end;

procedure TNamedObjectList.Delete(aName: TJSONStringType);

var
  Idx : Integer;
begin
  Idx:=IndexOfName(aName);
  if Idx<>-1 then
    Delete(Idx);
end;

procedure TNamedObjectList.Delete(aIndex: Integer);
begin
  FList.Delete(aIndex);
end;

function TNamedObjectList.IndexOfName(aName: TJSONStringType): Integer;
begin
  Result:=Count;
  While (Result>=0) and Not SameText(Names[Result],aName) do
    Dec(Result);
end;

{ TPathsList }

function TPathsList.GetObject(const aName: TJSONStringType): TPathItem;
begin
  Result:=TPathItem(GetObjectByName(aName));
end;

function TPathsList.GetObjectByIndex(aIndex : Integer): TPathItem;
begin
  Result:=TPathItem(NamedObjects[aIndex].Object_);
end;

constructor TPathsList.Create(aParent: TBaseOpenAPIObject;
  const aPathComponent: TJSONStringType);
begin
  Inherited Create(aParent,aPathComponent,TPathItem);
end;

function TPathsList.AddItem(const aName: TJSONStringType): TPathItem;
begin
  Result:=CreateNew(aName) as TPathItem;
  //  Add(aName,Result);
end;

{ TTagList }

function TTagList.GetTag(aIndex : Integer): TTag;
begin
  Result:=Items[aIndex] as TTag;
end;

procedure TTagList.AddTag(aTag: TTag);
begin
  Inherited Add(aTag);
end;

{ TParameterOrReferenceList }

function TParameterOrReferenceList.GetParam(aIndex : Integer
  ): TParameterOrReference;
begin
  Result:=TParameterOrReference(Items[aIndex]);
end;

function TParameterOrReferenceList.AddParam(aParent: TBaseOpenAPIObject): TParameterOrReference;
begin
  Result:=TParameterOrReference.Create(aParent,Format('[%d]',[Count]));
  Add(Result);
end;


function TOpenAPI.Get_Info : TInfo;

begin
  if Not assigned(FInfo) then
    begin
    FInfo:=TInfo.Create(self,'info');
    AddKeyWord(oakInfo);
    end;
  Result:=FInfo;
end;


function TOpenAPI.Get_Servers : TServerList;

begin
  if Not assigned(FServers) then
    begin
    FServers:=TServerList.Create;
    AddKeyWord(oakServers);
    end;
  Result:=FServers;
end;


function TOpenAPI.Get_Paths : TPathsList;

begin
  if Not assigned(FPaths) then
    begin
    FPaths:=TPathsList.Create(Self,'paths');
    AddKeyWord(oakPaths);
    end;
  Result:=FPaths;
end;


function TOpenAPI.Get_WebHooks : TPathItemOrReferenceMap;

begin
  if Not assigned(FWebHooks) then
    begin
    FWebHooks:=TPathItemOrReferenceMap.Create(Self,'webhooks');
    AddKeyWord(oakWebHooks);
    end;
  Result:=FWebHooks;
end;


function TOpenAPI.Get_Components : TComponents;

begin
  if Not assigned(FComponents) then
    begin
    FComponents:=TComponents.Create(Self,'components');
    AddKeyWord(oakComponents);
    end;
  Result:=FComponents;
end;


function TOpenAPI.Get_Security : TSecurityRequirementList;

begin
  if Not assigned(FSecurity) then
    begin
    FSecurity:=TSecurityRequirementList.Create; // (Self,'security');
    AddKeyWord(oakSecurity);
    end;
  Result:=FSecurity;
end;


function TOpenAPI.Get_Tags : TTagList;

begin
  if Not assigned(FTags) then
    begin
    FTags:=TTagList.Create;
    AddKeyWord(oakTags);
    end;
  Result:=FTags;
end;


function TOpenAPI.Get_ExternalDocs : TExternalDocumentation;

begin
  if Not assigned(FExternalDocs) then
    begin
    FExternalDocs:=TExternalDocumentation.Create(Self,'externalDocs');
    AddKeyWord(oakExternalDocs);
    end;
  Result:=FExternalDocs;
end;


procedure TOpenAPI.Set_OpenApi(const aValue : string);

begin
  if (FOpenApi=aValue) then exit;
  FOpenApi:=aValue;
  AddKeyWord(oakOpenApi);
end;


procedure TOpenAPI.Set_Info(const aValue : TInfo);

begin
  if (FInfo=aValue) then exit;
  FreeAndNil(FInfo);
  FInfo:=aValue;
  AddKeyWord(oakInfo);
end;


procedure TOpenAPI.Set_JSONSchemaDialect(const aValue : string);

begin
  if (FJSONSchemaDialect=aValue) then exit;
  FJSONSchemaDialect:=aValue;
  AddKeyWord(oakJSONSchemaDialect);
end;


procedure TOpenAPI.Set_Servers(const aValue : TServerList);

begin
  if (FServers=aValue) then exit;
  FreeAndNil(FServers);
  FServers:=aValue;
  AddKeyWord(oakServers);
end;


procedure TOpenAPI.Set_Paths(const aValue : TPathsList);

begin
  if (FPaths=aValue) then exit;
  FreeAndNil(FPaths);
  FPaths:=aValue;
  AddKeyWord(oakPaths);
end;


procedure TOpenAPI.Set_WebHooks(const aValue : TPathItemOrReferenceMap);

begin
  if (FWebHooks=aValue) then exit;
  FreeAndNil(FWebHooks);
  FWebHooks:=aValue;
  AddKeyWord(oakWebHooks);
end;


procedure TOpenAPI.Set_Components(const aValue : TComponents);

begin
  if (FComponents=aValue) then exit;
  FreeAndNil(FComponents);
  FComponents:=aValue;
  AddKeyWord(oakComponents);
end;


procedure TOpenAPI.Set_Security(const aValue : TSecurityRequirementList);

begin
  if (FSecurity=aValue) then exit;
  FreeAndNil(FSecurity);
  FSecurity:=aValue;
  AddKeyWord(oakSecurity);
end;


procedure TOpenAPI.Set_Tags(const aValue : TTagList);

begin
  if (FTags=aValue) then exit;
  FreeAndNil(FTags);
  FTags:=aValue;
  AddKeyWord(oakTags);
end;


procedure TOpenAPI.Set_ExternalDocs(const aValue : TExternalDocumentation);

begin
  if (FExternalDocs=aValue) then exit;
  FreeAndNil(FExternalDocs);
  FExternalDocs:=aValue;
  AddKeyWord(oakExternalDocs);
end;


procedure TOpenAPI.AddKeyword(aKeyword : TOpenAPIKeyword);

begin
  Include(FKeywords,aKeyword);
end;


procedure TOpenAPI.RemoveKeyword(aKeyword : TOpenAPIKeyword);

begin
  Exclude(FKeywords,aKeyword);
end;


function TOpenAPI.HasKeyWord(aKeyword: TOpenAPIKeyword): Boolean;

begin
  Result:=aKeyword in FKeywords;
end;


destructor TOpenAPI.destroy;

begin
  FreeAndNil(FInfo);
  FreeAndNil(FServers);
  FreeAndNil(FPaths);
  FreeAndNil(FWebHooks);
  FreeAndNil(FComponents);
  FreeAndNil(FSecurity);
  FreeAndNil(FTags);
  FreeAndNil(FExternalDocs);
  Inherited;
end;

constructor TOpenAPI.Create;
begin
  Inherited Create(Nil,'');
end;



function TInfo.Get_Contact : TContact;

begin
  if Not assigned(FContact) then
    begin
    FContact:=TContact.Create(Self,'contact');
    AddKeyWord(ikContact);
    end;
  Result:=FContact;
end;


function TInfo.Get_License : TLicense;

begin
  if Not assigned(FLicense) then
    begin
    FLicense:=TLicense.Create(Self,'license');
    AddKeyWord(ikLicense);
    end;
  Result:=FLicense;
end;


procedure TInfo.Set_Title(const aValue : string);

begin
  if (FTitle=aValue) then exit;
  FTitle:=aValue;
  AddKeyWord(ikTitle);
end;


procedure TInfo.Set_Summary(const aValue : string);

begin
  if (FSummary=aValue) then exit;
  FSummary:=aValue;
  AddKeyWord(ikSummary);
end;


procedure TInfo.Set_Description(const aValue : string);

begin
  if (FDescription=aValue) then exit;
  FDescription:=aValue;
  AddKeyWord(ikDescription);
end;


procedure TInfo.Set_TermsOfService(const aValue : string);

begin
  if (FTermsOfService=aValue) then exit;
  FTermsOfService:=aValue;
  AddKeyWord(ikTermsOfService);
end;


procedure TInfo.Set_Contact(const aValue : TContact);

begin
  if (FContact=aValue) then exit;
  FreeAndNil(FContact);
  FContact:=aValue;
  AddKeyWord(ikContact);
end;


procedure TInfo.Set_License(const aValue : TLicense);

begin
  if (FLicense=aValue) then exit;
  FreeAndNil(FLicense);
  FLicense:=aValue;
  AddKeyWord(ikLicense);
end;


procedure TInfo.Set_Version(const aValue : string);

begin
  if (FVersion=aValue) then exit;
  FVersion:=aValue;
  AddKeyWord(ikVersion);
end;


procedure TInfo.AddKeyword(aKeyword : TInfoKeyword);

begin
  Include(FKeywords,aKeyword);
end;


procedure TInfo.RemoveKeyword(aKeyword : TInfoKeyword);

begin
  Exclude(FKeywords,aKeyword);
end;


Function TInfo.HasKeyword(aKeyword : TInfoKeyword) : Boolean;

begin
  Result:=aKeyword in FKeywords;
end;


destructor TInfo.destroy;

begin
  FreeAndNil(FContact);
  FreeAndNil(FLicense);
  Inherited;
end;




procedure TContact.Set_Name(const aValue : string);

begin
  if (FName=aValue) then exit;
  FName:=aValue;
  AddKeyWord(cokName);
end;


procedure TContact.Set_Url(const aValue : string);

begin
  if (FUrl=aValue) then exit;
  FUrl:=aValue;
  AddKeyWord(cokUrl);
end;


procedure TContact.Set_Email(const aValue : string);

begin
  if (FEmail=aValue) then exit;
  FEmail:=aValue;
  AddKeyWord(cokEmail);
end;


procedure TContact.AddKeyword(aKeyword : TContactKeyword);

begin
  Include(FKeywords,aKeyword);
end;


procedure TContact.RemoveKeyword(aKeyword : TContactKeyword);

begin
  Exclude(FKeywords,aKeyword);
end;


Function TContact.HasKeyword(aKeyword : TContactKeyword) : Boolean;

begin
  Result:=aKeyword in FKeywords;
end;


destructor TContact.destroy;

begin
  Inherited;
end;


procedure TLicense.Set_Name(const aValue : string);

begin
  if (FName=aValue) then exit;
  FName:=aValue;
  AddKeyWord(lkName);
end;


procedure TLicense.Set_Identifier(const aValue : string);

begin
  if (FIdentifier=aValue) then exit;
  FIdentifier:=aValue;
  AddKeyWord(lkIdentifier);
end;


procedure TLicense.Set_Url(const aValue : string);

begin
  if (FUrl=aValue) then exit;
  FUrl:=aValue;
  AddKeyWord(lkUrl);
end;


procedure TLicense.AddKeyword(aKeyword : TLicenseKeyword);

begin
  Include(FKeywords,aKeyword);
end;


procedure TLicense.RemoveKeyword(aKeyword : TLicenseKeyword);

begin
  Exclude(FKeywords,aKeyword);
end;


Function TLicense.HasKeyword(aKeyword : TLicenseKeyword) : Boolean;

begin
  Result:=aKeyword in FKeywords;
end;


destructor TLicense.destroy;

begin
  Inherited;
end;


function TServer.Get_Variables : TServerVariableMap;

begin
  if Not assigned(FVariables) then
    begin
    FVariables:=TServerVariableMap.Create(Self,'variables');
    AddKeyWord(skVariables);
    end;
  Result:=FVariables;
end;


procedure TServer.Set_ServerUrl(const aValue : string);

begin
  if (FServerUrl=aValue) then exit;
  FServerUrl:=aValue;
  AddKeyWord(skServerUrl);
end;


procedure TServer.Set_Description(const aValue : string);

begin
  if (FDescription=aValue) then exit;
  FDescription:=aValue;
  AddKeyWord(skDescription);
end;


procedure TServer.Set_Variables(const aValue : TServerVariableMap);

begin
  if (FVariables=aValue) then exit;
  FreeAndNil(FVariables);
  FVariables:=aValue;
  AddKeyWord(skVariables);
end;


procedure TServer.AddKeyword(aKeyword : TServerKeyword);

begin
  Include(FKeywords,aKeyword);
end;


procedure TServer.RemoveKeyword(aKeyword : TServerKeyword);

begin
  Exclude(FKeywords,aKeyword);
end;


Function TServer.HasKeyword(aKeyword : TServerKeyword) : Boolean;

begin
  Result:=aKeyword in FKeywords;
end;


destructor TServer.destroy;

begin
  FreeAndNil(FVariables);
  Inherited;
end;


function TServerList.GetObj(aIndex : Integer): TServer;

begin
  Result:=TServer(Items[aIndex]);
end;


procedure TServerList.SetObj(aIndex : Integer; aValue : TServer);

begin
  Items[aIndex]:=aValue;
end;


function TServerList.ToArray : TServerArray;

var
  I : Integer;

begin
  Result:=[];
  SetLength(Result,Count);
  For I:=0 to Count-1 do
    Result[I]:=GetObj(I);
end;


function TServerVariable.Get_Enum : TStrings;

begin
  if Not assigned(FEnum) then
    begin
    FEnum:=TStringList.Create;
    AddKeyWord(svkEnum);
    end;
  Result:=FEnum;
end;


procedure TServerVariable.Set_Enum(const aValue : TStrings);

begin
  if (FEnum=aValue) then exit;
  FreeAndNil(FEnum);
  FEnum:=aValue;
  AddKeyWord(svkEnum);
end;


procedure TServerVariable.Set_Default(const aValue : string);

begin
  if (FDefault=aValue) then exit;
  FDefault:=aValue;
  AddKeyWord(svkDefault);
end;


procedure TServerVariable.Set_Description(const aValue : string);

begin
  if (FDescription=aValue) then exit;
  FDescription:=aValue;
  AddKeyWord(svkDescription);
end;


procedure TServerVariable.AddKeyword(aKeyword : TServerVariableKeyword);

begin
  Include(FKeywords,aKeyword);
end;


procedure TServerVariable.RemoveKeyword(aKeyword : TServerVariableKeyword);

begin
  Exclude(FKeywords,aKeyword);
end;


Function TServerVariable.HasKeyword(aKeyword : TServerVariableKeyword) : Boolean;

begin
  Result:=aKeyword in FKeywords;
end;


destructor TServerVariable.destroy;

begin
  FreeAndNil(FEnum);
  Inherited;
end;



function TServerVariableMap.GetObject(const aName : TJSONStringType) : TServerVariable;

begin
  Result:=TServerVariable(GetObjectByName(aName));
end;


constructor TServerVariableMap.Create(aParent: TBaseOpenAPIObject; const aPathComponent: TJSONStringType);

begin
  Inherited Create(aParent,aPathComponent,TServerVariable);
end;


function TServerVariableMap.AddItem(const aName : TJSONStringType) : TServerVariable;

begin
  Result:=CreateNew(aName) as TServerVariable;
//  Add(aName,Result);
end;


function TComponents.Get_Schemas : TJSONSchemaMap;

begin
  if Not assigned(FSchemas) then
    begin
    FSchemas:=TJSONSchemaMap.Create(Self,'schemas',TJSONSchema);
    AddKeyWord(ckSchemas);
    end;
  Result:=FSchemas;
end;


function TComponents.Get_Responses : TResponseOrReferenceMap;

begin
  if Not assigned(FResponses) then
    begin
    FResponses:=TResponseOrReferenceMap.Create(Self,'responses');
    AddKeyWord(ckResponses);
    end;
  Result:=FResponses;
end;


function TComponents.Get_Parameters : TParameterOrReferenceMap;

begin
  if Not assigned(FParameters) then
    begin
    FParameters:=TParameterOrReferenceMap.Create(Self,'parameters');
    AddKeyWord(ckParameters);
    end;
  Result:=FParameters;
end;


function TComponents.Get_Examples : TExampleOrReferenceMap;

begin
  if Not assigned(FExamples) then
    begin
    FExamples:=TExampleOrReferenceMap.Create(Self,'examples');
    AddKeyWord(ckExamples);
    end;
  Result:=FExamples;
end;


function TComponents.Get_RequestBodies : TRequestBodyOrReferenceMap;

begin
  if Not assigned(FRequestBodies) then
    begin
    FRequestBodies:=TRequestBodyOrReferenceMap.Create(Self,'requestBodies');
    AddKeyWord(ckRequestBodies);
    end;
  Result:=FRequestBodies;
end;


function TComponents.Get_Headers : THeaderOrReferenceMap;

begin
  if Not assigned(FHeaders) then
    begin
    FHeaders:=THeaderOrReferenceMap.Create(Self,'headers');
    AddKeyWord(ckHeaders);
    end;
  Result:=FHeaders;
end;


function TComponents.Get_SecuritySchemes : TSecuritySchemeOrReferenceMap;

begin
  if Not assigned(FSecuritySchemes) then
    begin
    FSecuritySchemes:=TSecuritySchemeOrReferenceMap.Create(Self,'securitySchemes');
    AddKeyWord(ckSecuritySchemes);
    end;
  Result:=FSecuritySchemes;
end;


function TComponents.Get_Links : TLinkOrReferenceMap;

begin
  if Not assigned(FLinks) then
    begin
    FLinks:=TLinkOrReferenceMap.Create(Self,'links');
    AddKeyWord(ckLinks);
    end;
  Result:=FLinks;
end;


function TComponents.Get_Callbacks : TLinkOrReferenceMap;

begin
  if Not assigned(FCallbacks) then
    begin
    FCallbacks:=TLinkOrReferenceMap.Create(Self,'callbacks');
    AddKeyWord(ckCallbacks);
    end;
  Result:=FCallbacks;
end;


function TComponents.Get_PathItems : TPathItemOrReferenceMap;

begin
  if Not assigned(FPathItems) then
    begin
    FPathItems:=TPathItemOrReferenceMap.Create(Self,'pathitems');
    AddKeyWord(ckPathItems);
    end;
  Result:=FPathItems;
end;


procedure TComponents.Set_Schemas(const aValue : TJSONSchemaMap);

begin
  if (FSchemas=aValue) then exit;
  FreeAndNil(FSchemas);
  FSchemas:=aValue;
  AddKeyWord(ckSchemas);
end;


procedure TComponents.Set_Responses(const aValue : TResponseOrReferenceMap);

begin
  if (FResponses=aValue) then exit;
  FreeAndNil(FResponses);
  FResponses:=aValue;
  AddKeyWord(ckResponses);
end;


procedure TComponents.Set_Parameters(const aValue : TParameterOrReferenceMap);

begin
  if (FParameters=aValue) then exit;
  FreeAndNil(FParameters);
  FParameters:=aValue;
  AddKeyWord(ckParameters);
end;


procedure TComponents.Set_Examples(const aValue : TExampleOrReferenceMap);

begin
  if (FExamples=aValue) then exit;
  FreeAndNil(FExamples);
  FExamples:=aValue;
  AddKeyWord(ckExamples);
end;


procedure TComponents.Set_RequestBodies(const aValue : TRequestBodyOrReferenceMap);

begin
  if (FRequestBodies=aValue) then exit;
  FreeAndNil(FRequestBodies);
  FRequestBodies:=aValue;
  AddKeyWord(ckRequestBodies);
end;


procedure TComponents.Set_Headers(const aValue : THeaderOrReferenceMap);

begin
  if (FHeaders=aValue) then exit;
  FreeAndNil(FHeaders);
  FHeaders:=aValue;
  AddKeyWord(ckHeaders);
end;


procedure TComponents.Set_SecuritySchemes(const aValue : TSecuritySchemeOrReferenceMap);

begin
  if (FSecuritySchemes=aValue) then exit;
  FreeAndNil(FSecuritySchemes);
  FSecuritySchemes:=aValue;
  AddKeyWord(ckSecuritySchemes);
end;


procedure TComponents.Set_Links(const aValue : TLinkOrReferenceMap);

begin
  if (FLinks=aValue) then exit;
  FreeAndNil(FLinks);
  FLinks:=aValue;
  AddKeyWord(ckLinks);
end;


procedure TComponents.Set_Callbacks(const aValue : TLinkOrReferenceMap);

begin
  if (FCallbacks=aValue) then exit;
  FreeAndNil(FCallbacks);
  FCallbacks:=aValue;
  AddKeyWord(ckCallbacks);
end;


procedure TComponents.Set_PathItems(const aValue : TPathItemOrReferenceMap);

begin
  if (FPathItems=aValue) then exit;
  FreeAndNil(FPathItems);
  FPathItems:=aValue;
  AddKeyWord(ckPathItems);
end;


procedure TComponents.AddKeyword(aKeyword : TComponentsKeyword);

begin
  Include(FKeywords,aKeyword);
end;


procedure TComponents.RemoveKeyword(aKeyword : TComponentsKeyword);

begin
  Exclude(FKeywords,aKeyword);
end;


Function TComponents.HasKeyword(aKeyword : TComponentsKeyword) : Boolean;

begin
  Result:=aKeyword in FKeywords;
end;


destructor TComponents.destroy;

begin
  FreeAndNil(FSchemas);
  FreeAndNil(FResponses);
  FreeAndNil(FParameters);
  FreeAndNil(FExamples);
  FreeAndNil(FRequestBodies);
  FreeAndNil(FHeaders);
  FreeAndNil(FSecuritySchemes);
  FreeAndNil(FLinks);
  FreeAndNil(FCallbacks);
  FreeAndNil(FPathItems);
  Inherited;
end;



function TPathItem.Get_Get : TApiOperation;

begin
  if Not assigned(FGet) then
    begin
    FGet:=TApiOperation.Create(Self,'get');
    AddKeyWord(pkGet);
    end;
  Result:=FGet;
end;


function TPathItem.Get_Put : TApiOperation;

begin
  if Not assigned(FPut) then
    begin
    FPut:=TApiOperation.Create(Self,'put');
    AddKeyWord(pkPut);
    end;
  Result:=FPut;
end;


function TPathItem.Get_Post : TApiOperation;

begin
  if Not assigned(FPost) then
    begin
    FPost:=TApiOperation.Create(Self,'post');
    AddKeyWord(pkPost);
    end;
  Result:=FPost;
end;


function TPathItem.Get_Delete : TApiOperation;

begin
  if Not assigned(FDelete) then
    begin
    FDelete:=TApiOperation.Create(Self,'delete');
    AddKeyWord(pkDelete);
    end;
  Result:=FDelete;
end;


function TPathItem.Get_Options : TApiOperation;

begin
  if Not assigned(FOptions) then
    begin
    FOptions:=TApiOperation.Create(Self,'options');
    AddKeyWord(pkOptions);
    end;
  Result:=FOptions;
end;


function TPathItem.Get_Head : TApiOperation;

begin
  if Not assigned(FHead) then
    begin
    FHead:=TApiOperation.Create(Self,'head');
    AddKeyWord(pkHead);
    end;
  Result:=FHead;
end;


function TPathItem.Get_Patch : TApiOperation;

begin
  if Not assigned(FPatch) then
    begin
    FPatch:=TApiOperation.Create(Self,'patch');
    AddKeyWord(pkPatch);
    end;
  Result:=FPatch;
end;


function TPathItem.Get_Trace : TApiOperation;

begin
  if Not assigned(FTrace) then
    begin
    FTrace:=TApiOperation.Create(Self,'trace');
    AddKeyWord(pkTrace);
    end;
  Result:=FTrace;
end;


function TPathItem.Get_Servers : TServerList;

begin
  if Not assigned(FServers) then
    begin
    FServers:=TServerList.Create;
    AddKeyWord(pkServers);
    end;
  Result:=FServers;
end;


function TPathItem.Get_Parameters : TParameterOrReferenceList;

begin
  if Not assigned(FParameters) then
    begin
    FParameters:=TParameterOrReferenceList.Create;
    AddKeyWord(pkParameters);
    end;
  Result:=FParameters;
end;


procedure TPathItem.Set_Ref(const aValue : string);

begin
  if (FRef=aValue) then exit;
  FRef:=aValue;
  AddKeyWord(pkRef);
end;


procedure TPathItem.Set_Summary(const aValue : string);

begin
  if (FSummary=aValue) then exit;
  FSummary:=aValue;
  AddKeyWord(pkSummary);
end;


procedure TPathItem.Set_Description(const aValue : string);

begin
  if (FDescription=aValue) then exit;
  FDescription:=aValue;
  AddKeyWord(pkDescription);
end;


procedure TPathItem.Set_Get(const aValue : TApiOperation);

begin
  if (FGet=aValue) then exit;
  FreeAndNil(FGet);
  FGet:=aValue;
  AddKeyWord(pkGet);
end;


procedure TPathItem.Set_Put(const aValue : TApiOperation);

begin
  if (FPut=aValue) then exit;
  FreeAndNil(FPut);
  FPut:=aValue;
  AddKeyWord(pkPut);
end;


procedure TPathItem.Set_Post(const aValue : TApiOperation);

begin
  if (FPost=aValue) then exit;
  FreeAndNil(FPost);
  FPost:=aValue;
  AddKeyWord(pkPost);
end;


procedure TPathItem.Set_Delete(const aValue : TApiOperation);

begin
  if (FDelete=aValue) then exit;
  FreeAndNil(FDelete);
  FDelete:=aValue;
  AddKeyWord(pkDelete);
end;


procedure TPathItem.Set_Options(const aValue : TApiOperation);

begin
  if (FOptions=aValue) then exit;
  FreeAndNil(FOptions);
  FOptions:=aValue;
  AddKeyWord(pkOptions);
end;


procedure TPathItem.Set_Head(const aValue : TApiOperation);

begin
  if (FHead=aValue) then exit;
  FreeAndNil(FHead);
  FHead:=aValue;
  AddKeyWord(pkHead);
end;


procedure TPathItem.Set_Patch(const aValue : TApiOperation);

begin
  if (FPatch=aValue) then exit;
  FreeAndNil(FPatch);
  FPatch:=aValue;
  AddKeyWord(pkPatch);
end;


procedure TPathItem.Set_Trace(const aValue : TApiOperation);

begin
  if (FTrace=aValue) then exit;
  FreeAndNil(FTrace);
  FTrace:=aValue;
  AddKeyWord(pkTrace);
end;


procedure TPathItem.Set_Servers(const aValue : TServerList);

begin
  if (FServers=aValue) then exit;
  FreeAndNil(FServers);
  FServers:=aValue;
  AddKeyWord(pkServers);
end;


procedure TPathItem.Set_Parameters(const aValue : TParameterOrReferenceList);

begin
  if (FParameters=aValue) then exit;
  FreeAndNil(FParameters);
  FParameters:=aValue;
  AddKeyWord(pkParameters);
end;


procedure TPathItem.AddKeyword(aKeyword : TPathItemKeyword);

begin
  Include(FKeywords,aKeyword);
end;


procedure TPathItem.RemoveKeyword(aKeyword : TPathItemKeyword);

begin
  Exclude(FKeywords,aKeyword);
end;


function TPathItem.HasKeyWord(aKeyword: TPathItemKeyword): Boolean;

begin
  Result:=aKeyword in FKeywords;
end;

function TPathItem.GetOperation(aKeyword: TPathItemOperationKeyword
  ): TAPIOperation;
begin
  Case aKeyword of
    pkGet : Result:=FGet;
    pkPut : Result:=FPut;
    pkPost : Result:=FPost;
    pkDelete : Result:=FDelete;
    pkOptions : Result:=FOptions;
    pkHead : Result:=FHead;
    pkPatch : Result:=FPatch;
    pkTrace : Result:=FTrace;
  else
    Result:=Nil;
  end;
end;


destructor TPathItem.destroy;

begin
  FreeAndNil(FGet);
  FreeAndNil(FPut);
  FreeAndNil(FPost);
  FreeAndNil(FDelete);
  FreeAndNil(FOptions);
  FreeAndNil(FHead);
  FreeAndNil(FPatch);
  FreeAndNil(FTrace);
  FreeAndNil(FServers);
  FreeAndNil(FParameters);
  Inherited;
end;



function TPathItemOrReference.GetReference : TReference;

begin
  if Not Assigned(FReference) then
    FReference:=TReference.Create(Self,'reference');
  Result:=FReference;
end;


destructor TPathItemOrReference.destroy;

begin
  FreeAndNil(FReference);
  Inherited;
end;


function TPathItemOrReference.HasReference : Boolean;

begin
  Result:=Assigned(FReference);
end;


function TPathItemOrReferenceMap.GetObject(const aName : TJSONStringType) : TPathItemOrReference;

begin
  Result:=TPathItemOrReference(GetObjectByName(aName));
end;


constructor TPathItemOrReferenceMap.Create(aParent: TBaseOpenAPIObject; aPathComponent: TJSONStringType);

begin
  Inherited Create(aParent,aPathComponent,TPathItemOrReference);
end;


function TPathItemOrReferenceMap.AddItem(const aName : TJSONStringType) : TPathItemOrReference;

begin
  Result:=CreateNew(aName) as TPathItemOrReference;
  //Add(aName,Result);
end;


function TApiOperation.Get_Tags : TStrings;

begin
  if Not assigned(FTags) then
    begin
    FTags:=TStringList.Create;
    AddKeyWord(okTags);
    end;
  Result:=FTags;
end;


function TApiOperation.Get_ExternalDocs : TExternalDocumentation;

begin
  if Not assigned(FExternalDocs) then
    begin
    FExternalDocs:=TExternalDocumentation.Create(Self,'externalDocs');
    AddKeyWord(okExternalDocs);
    end;
  Result:=FExternalDocs;
end;


function TApiOperation.Get_Parameters : TParameterOrReferenceList;

begin
  if Not assigned(FParameters) then
    begin
    FParameters:=TParameterOrReferenceList.Create;
    AddKeyWord(okParameters);
    end;
  Result:=FParameters;
end;


function TApiOperation.Get_RequestBody : TRequestBodyOrReference;

begin
  if Not assigned(FRequestBody) then
    begin
    FRequestBody:=TRequestBodyOrReference.Create(Self,'requestBody');
    AddKeyWord(okRequestBody);
    end;
  Result:=FRequestBody;
end;


function TApiOperation.Get_Responses : TResponses;

begin
  if Not assigned(FResponses) then
    begin
    FResponses:=TResponses.Create(Self,'responses');
    AddKeyWord(okResponses);
    end;
  Result:=FResponses;
end;


function TApiOperation.Get_Callbacks : TCallBackOrReferenceMap;

begin
  if Not assigned(FCallbacks) then
    begin
    FCallbacks:=TCallBackOrReferenceMap.Create(Self,'callbacks');
    AddKeyWord(okCallbacks);
    end;
  Result:=FCallbacks;
end;


function TApiOperation.Get_Security : TSecurityRequirementList;

begin
  if Not assigned(FSecurity) then
    begin
    FSecurity:=TSecurityRequirementList.Create;
    AddKeyWord(okSecurity);
    end;
  Result:=FSecurity;
end;


function TApiOperation.Get_Servers : TServerList;

begin
  if Not assigned(FServers) then
    begin
    FServers:=TServerList.Create;
    AddKeyWord(okServers);
    end;
  Result:=FServers;
end;


procedure TApiOperation.Set_Tags(const aValue : TStrings);

begin
  if (FTags=aValue) then exit;
  FreeAndNil(FTags);
  FTags:=aValue;
  AddKeyWord(okTags);
end;


procedure TApiOperation.Set_Summary(const aValue : string);

begin
  if (FSummary=aValue) then exit;
  FSummary:=aValue;
  AddKeyWord(okSummary);
end;


procedure TApiOperation.Set_Description(const aValue : string);

begin
  if (FDescription=aValue) then exit;
  FDescription:=aValue;
  AddKeyWord(okDescription);
end;


procedure TApiOperation.Set_ExternalDocs(const aValue : TExternalDocumentation);

begin
  if (FExternalDocs=aValue) then exit;
  FreeAndNil(FExternalDocs);
  FExternalDocs:=aValue;
  AddKeyWord(okExternalDocs);
end;


procedure TApiOperation.Set_OperationId(const aValue : string);

begin
  if (FOperationId=aValue) then exit;
  FOperationId:=aValue;
  AddKeyWord(okOperationId);
end;


procedure TApiOperation.Set_Parameters(const aValue : TParameterOrReferenceList);

begin
  if (FParameters=aValue) then exit;
  FreeAndNil(FParameters);
  FParameters:=aValue;
  AddKeyWord(okParameters);
end;


procedure TApiOperation.Set_RequestBody(const aValue : TRequestBodyOrReference);

begin
  if (FRequestBody=aValue) then exit;
  FreeAndNil(FRequestBody);
  FRequestBody:=aValue;
  AddKeyWord(okRequestBody);
end;


procedure TApiOperation.Set_Responses(const aValue : TResponses);

begin
  if (FResponses=aValue) then exit;
  FreeAndNil(FResponses);
  FResponses:=aValue;
  AddKeyWord(okResponses);
end;


procedure TApiOperation.Set_Callbacks(const aValue : TCallBackOrReferenceMap);

begin
  if (FCallbacks=aValue) then exit;
  FreeAndNil(FCallbacks);
  FCallbacks:=aValue;
  AddKeyWord(okCallbacks);
end;


procedure TApiOperation.Set_Deprecated(const aValue : Boolean);

begin
  if (FDeprecated=aValue) then exit;
  FDeprecated:=aValue;
  AddKeyWord(okDeprecated);
end;


procedure TApiOperation.Set_Security(const aValue : TSecurityRequirementList);

begin
  if (FSecurity=aValue) then exit;
  FreeAndNil(FSecurity);
  FSecurity:=aValue;
  AddKeyWord(okSecurity);
end;


procedure TApiOperation.Set_Servers(const aValue : TServerList);

begin
  if (FServers=aValue) then exit;
  FreeAndNil(FServers);
  FServers:=aValue;
  AddKeyWord(okServers);
end;


procedure TApiOperation.AddKeyword(aKeyword : TApiOperationKeyword);

begin
  Include(FKeywords,aKeyword);
end;


procedure TApiOperation.RemoveKeyword(aKeyword : TApiOperationKeyword);

begin
  Exclude(FKeywords,aKeyword);
end;


Function TApiOperation.HasKeyword(aKeyword : TApiOperationKeyword) : Boolean;

begin
  Result:=aKeyword in FKeywords;
end;


destructor TApiOperation.destroy;

begin
  FreeAndNil(FTags);
  FreeAndNil(FExternalDocs);
  FreeAndNil(FParameters);
  FreeAndNil(FRequestBody);
  FreeAndNil(FResponses);
  FreeAndNil(FCallbacks);
  FreeAndNil(FSecurity);
  FreeAndNil(FServers);
  Inherited;
end;



procedure TExternalDocumentation.Set_Description(const aValue : string);

begin
  if (FDescription=aValue) then exit;
  FDescription:=aValue;
  AddKeyWord(edkDescription);
end;


procedure TExternalDocumentation.Set_Url(const aValue : string);

begin
  if (FUrl=aValue) then exit;
  FUrl:=aValue;
  AddKeyWord(edkUrl);
end;


procedure TExternalDocumentation.AddKeyword(aKeyword : TExternalDocumentationKeyword);

begin
  Include(FKeywords,aKeyword);
end;


procedure TExternalDocumentation.RemoveKeyword(aKeyword : TExternalDocumentationKeyword);

begin
  Exclude(FKeywords,aKeyword);
end;


Function TExternalDocumentation.HasKeyword(aKeyword : TExternalDocumentationKeyword) : Boolean;

begin
  Result:=aKeyword in FKeywords;
end;


destructor TExternalDocumentation.destroy;

begin
  Inherited;
end;


function TParameterOrHeader.Get_Schema : TJSONSchema;

begin
  if Not assigned(FSchema) then
    begin
    FSchema:=TJSONSchema.Create();
    AddKeyWord(pakSchema);
    end;
  Result:=FSchema;
end;


function TParameterOrHeader.Get_Examples : TExampleOrReferenceMap;

begin
  if Not assigned(FExamples) then
    begin
    FExamples:=TExampleOrReferenceMap.Create(Self,'examples');
    AddKeyWord(pakExamples);
    end;
  Result:=FExamples;
end;


function TParameterOrHeader.Get_Content : TMediaTypeMap;

begin
  if Not assigned(FContent) then
    begin
    FContent:=TMediaTypeMap.Create(Self,'content');
    AddKeyWord(pakContent);
    end;
  Result:=FContent;
end;


procedure TParameterOrHeader.Set_Name(const aValue : string);

begin
  if (FName=aValue) then exit;
  FName:=aValue;
  AddKeyWord(pakName);
end;


procedure TParameterOrHeader.Set_In_(const aValue : string);

begin
  if (FIn_=aValue) then exit;
  FIn_:=aValue;
  AddKeyWord(pakIn);
end;


procedure TParameterOrHeader.Set_Description(const aValue : string);

begin
  if (FDescription=aValue) then exit;
  FDescription:=aValue;
  AddKeyWord(pakDescription);
end;


procedure TParameterOrHeader.Set_Required(const aValue : Boolean);

begin
  if (FRequired=aValue) then exit;
  FRequired:=aValue;
  AddKeyWord(pakRequired);
end;


procedure TParameterOrHeader.Set_Deprecated(const aValue : Boolean);

begin
  if (FDeprecated=aValue) then exit;
  FDeprecated:=aValue;
  AddKeyWord(pakDeprecated);
end;


procedure TParameterOrHeader.Set_AllowEmptyValue(const aValue : Boolean);

begin
  if (FAllowEmptyValue=aValue) then exit;
  FAllowEmptyValue:=aValue;
  AddKeyWord(pakAllowEmptyValue);
end;


procedure TParameterOrHeader.Set_Style(const aValue : string);

begin
  if (FStyle=aValue) then exit;
  FStyle:=aValue;
  AddKeyWord(pakStyle);
end;


procedure TParameterOrHeader.Set_Explode(const aValue : Boolean);

begin
  if (FExplode=aValue) then exit;
  FExplode:=aValue;
  AddKeyWord(pakExplode);
end;


procedure TParameterOrHeader.Set_AllowReserved(const aValue : Boolean);

begin
  if (FAllowReserved=aValue) then exit;
  FAllowReserved:=aValue;
  AddKeyWord(pakAllowReserved);
end;


procedure TParameterOrHeader.Set_Schema(const aValue : TJSONSchema);

begin
  if (FSchema=aValue) then exit;
  FreeAndNil(FSchema);
  FSchema:=aValue;
  AddKeyWord(pakSchema);
end;


procedure TParameterOrHeader.Set_Example(const aValue : TJSONData);

begin
  if (FExample=aValue) then exit;
  FreeAndNil(FExample);
  FExample:=aValue;
  AddKeyWord(pakExample);
end;


procedure TParameterOrHeader.Set_Examples(const aValue : TExampleOrReferenceMap);

begin
  if (FExamples=aValue) then exit;
  FreeAndNil(FExamples);
  FExamples:=aValue;
  AddKeyWord(pakExamples);
end;


procedure TParameterOrHeader.Set_Content(const aValue : TMediaTypeMap);

begin
  if (FContent=aValue) then exit;
  FreeAndNil(FContent);
  FContent:=aValue;
  AddKeyWord(pakContent);
end;


procedure TParameterOrHeader.AddKeyword(aKeyword : TParameterKeyword);

begin
  Include(FKeywords,aKeyword);
end;


procedure TParameterOrHeader.RemoveKeyword(aKeyword : TParameterKeyword);

begin
  Exclude(FKeywords,aKeyword);
end;


Function TParameterOrHeader.HasKeyword(aKeyword : TParameterKeyword) : Boolean;

begin
  Result:=aKeyword in FKeywords;
end;


destructor TParameterOrHeader.destroy;

begin
  FreeAndNil(FSchema);
  FreeAndNil(FExample);
  FreeAndNil(FExamples);
  FreeAndNil(FContent);
  Inherited;
end;


function TParameterOrReference.GetReference : TReference;

begin
  if Not Assigned(FReference) then
    FReference:=TReference.Create(Self,'reference');
  Result:=FReference;
end;


destructor TParameterOrReference.destroy;

begin
  FreeAndNil(FReference);
  Inherited;
end;


function TParameterOrReference.HasReference : Boolean;

begin
  Result:=Assigned(FReference);
end;


function TParameterOrReferenceMap.GetObject(const aName : TJSONStringType) : TParameterOrReference;

begin
  Result:=TParameterOrReference(GetObjectByName(aName));
end;


constructor TParameterOrReferenceMap.Create(aParent: TBaseOpenAPIObject; const aPathComponent: TJSONStringType);

begin
  Inherited Create(aParent,aPathComponent,TParameterOrReference);
end;


function TParameterOrReferenceMap.AddItem(const aName : TJSONStringType) : TParameterOrReference;

begin
  Result:=CreateNew(aName) as TParameterOrReference;
  // Add(aName,Result);
end;


procedure TParameterStyle.Set_Matrix(const aValue : string);

begin
  if (FMatrix=aValue) then exit;
  FMatrix:=aValue;
  AddKeyWord(pskMatrix);
end;


procedure TParameterStyle.Set_Label_(const aValue : string);

begin
  if (FLabel_=aValue) then exit;
  FLabel_:=aValue;
  AddKeyWord(pskLabel);
end;


procedure TParameterStyle.Set_Form(const aValue : string);

begin
  if (FForm=aValue) then exit;
  FForm:=aValue;
  AddKeyWord(pskForm);
end;


procedure TParameterStyle.Set_Simple(const aValue : string);

begin
  if (FSimple=aValue) then exit;
  FSimple:=aValue;
  AddKeyWord(pskSimple);
end;


procedure TParameterStyle.Set_SpaceDelimited(const aValue : string);

begin
  if (FSpaceDelimited=aValue) then exit;
  FSpaceDelimited:=aValue;
  AddKeyWord(pskSpaceDelimited);
end;


procedure TParameterStyle.Set_PipeDelimited(const aValue : string);

begin
  if (FPipeDelimited=aValue) then exit;
  FPipeDelimited:=aValue;
  AddKeyWord(pskPipeDelimited);
end;


procedure TParameterStyle.Set_DeepObject(const aValue : string);

begin
  if (FDeepObject=aValue) then exit;
  FDeepObject:=aValue;
  AddKeyWord(pskDeepObject);
end;


procedure TParameterStyle.AddKeyword(aKeyword : TParameterStyleKeyword);

begin
  Include(FKeywords,aKeyword);
end;


procedure TParameterStyle.RemoveKeyword(aKeyword : TParameterStyleKeyword);

begin
  Exclude(FKeywords,aKeyword);
end;


Function TParameterStyle.HasKeyword(aKeyword : TParameterStyleKeyword) : Boolean;

begin
  Result:=aKeyword in FKeywords;
end;


destructor TParameterStyle.destroy;

begin
  Inherited;
end;


function TRequestBody.Get_Content : TMediaTypeMap;

begin
  if Not assigned(FContent) then
    begin
    FContent:=TMediaTypeMap.Create(Self,'content');
    AddKeyWord(rbkContent);
    end;
  Result:=FContent;
end;


procedure TRequestBody.Set_Description(const aValue : string);

begin
  if (FDescription=aValue) then exit;
  FDescription:=aValue;
  AddKeyWord(rbkDescription);
end;


procedure TRequestBody.Set_Content(const aValue : TMediaTypeMap);

begin
  if (FContent=aValue) then exit;
  FreeAndNil(FContent);
  FContent:=aValue;
  AddKeyWord(rbkContent);
end;


procedure TRequestBody.Set_Required(const aValue : Boolean);

begin
  if (FRequired=aValue) then exit;
  FRequired:=aValue;
  AddKeyWord(rbkRequired);
end;


procedure TRequestBody.AddKeyword(aKeyword : TRequestBodyKeyword);

begin
  Include(FKeywords,aKeyword);
end;


procedure TRequestBody.RemoveKeyword(aKeyword : TRequestBodyKeyword);

begin
  Exclude(FKeywords,aKeyword);
end;


Function TRequestBody.HasKeyword(aKeyword : TRequestBodyKeyword) : Boolean;

begin
  Result:=aKeyword in FKeywords;
end;


destructor TRequestBody.destroy;

begin
  FreeAndNil(FContent);
  Inherited;
end;

function TRequestBodyOrReference.GetReference : TReference;

begin
  if Not Assigned(FReference) then
    FReference:=TReference.Create(Self,'reference');
  Result:=FReference;
end;


destructor TRequestBodyOrReference.destroy;

begin
  FreeAndNil(FReference);
  Inherited;
end;


function TRequestBodyOrReference.HasReference : Boolean;

begin
  Result:=Assigned(FReference);
end;


function TRequestBodyOrReferenceMap.GetObject(const aName : TJSONStringType) : TRequestBodyOrReference;

begin
  Result:=TRequestBodyOrReference(GetObjectByName(aName));
end;


constructor TRequestBodyOrReferenceMap.Create(aParent: TBaseOpenAPIObject; const aPathComponent: TJSONStringType);

begin
  Inherited Create(aParent,aPathComponent,TRequestBodyOrReference);
end;


function TRequestBodyOrReferenceMap.AddItem(const aName : TJSONStringType) : TRequestBodyOrReference;

begin
  Result:=CreateNew(aName) as TRequestBodyOrReference;
//  Add(aName,Result);
end;


function TMediaType.Get_Schema : TJSONSchema;

begin
  if Not assigned(FSchema) then
    begin
    FSchema:=TJSONSchema.Create();
    AddKeyWord(mtkSchema);
    end;
  Result:=FSchema;
end;


function TMediaType.Get_Examples : TExampleOrReferenceMap;

begin
  if Not assigned(FExamples) then
    begin
    FExamples:=TExampleOrReferenceMap.Create(Self,'examples');
    AddKeyWord(mtkExamples);
    end;
  Result:=FExamples;
end;


function TMediaType.Get_Encoding : TEncodingMap;

begin
  if Not assigned(FEncoding) then
    begin
    FEncoding:=TEncodingMap.Create(Self,'encoding');
    AddKeyWord(mtkEncoding);
    end;
  Result:=FEncoding;
end;


procedure TMediaType.Set_Schema(const aValue : TJSONSchema);

begin
  if (FSchema=aValue) then exit;
  FreeAndNil(FSchema);
  FSchema:=aValue;
  AddKeyWord(mtkSchema);
end;


procedure TMediaType.Set_Example(const aValue : TJSONData);

begin
  if (FExample=aValue) then exit;
  FreeAndNil(FExample);
  FExample:=aValue;
  AddKeyWord(mtkExample);
end;


procedure TMediaType.Set_Examples(const aValue : TExampleOrReferenceMap);

begin
  if (FExamples=aValue) then exit;
  FreeAndNil(FExamples);
  FExamples:=aValue;
  AddKeyWord(mtkExamples);
end;


procedure TMediaType.Set_Encoding(const aValue : TEncodingMap);

begin
  if (FEncoding=aValue) then exit;
  FreeAndNil(FEncoding);
  FEncoding:=aValue;
  AddKeyWord(mtkEncoding);
end;


procedure TMediaType.AddKeyword(aKeyword : TMediaTypeKeyword);

begin
  Include(FKeywords,aKeyword);
end;


procedure TMediaType.RemoveKeyword(aKeyword : TMediaTypeKeyword);

begin
  Exclude(FKeywords,aKeyword);
end;


Function TMediaType.HasKeyword(aKeyword : TMediaTypeKeyword) : Boolean;

begin
  Result:=aKeyword in FKeywords;
end;


destructor TMediaType.destroy;

begin
  FreeAndNil(FSchema);
  FreeAndNil(FExample);
  FreeAndNil(FExamples);
  FreeAndNil(FEncoding);
  Inherited;
end;




function TMediaTypeMap.GetObject(const aName : TJSONStringType) : TMediaType;

begin
  Result:=TMediaType(GetObjectByName(aName));
end;


constructor TMediaTypeMap.Create(aParent : TBaseOpenAPIObject; const aPathcomponent : TJSONStringType);

begin
  Inherited Create(aParent,aPathComponent,TMediaType);
end;


function TMediaTypeMap.AddItem(const aName : TJSONStringType) : TMediaType;

begin
  Result:=CreateNew(aName) as TMediaType;
//  Add(aName,Result);
end;


function TEncoding.Get_Headers : THeaderOrReferenceMap;

begin
  if Not assigned(FHeaders) then
    begin
    FHeaders:=THeaderOrReferenceMap.Create(Self,'headers');
    AddKeyWord(eckHeaders);
    end;
  Result:=FHeaders;
end;


procedure TEncoding.Set_ContentType(const aValue : string);

begin
  if (FContentType=aValue) then exit;
  FContentType:=aValue;
  AddKeyWord(eckContentType);
end;


procedure TEncoding.Set_Headers(const aValue : THeaderOrReferenceMap);

begin
  if (FHeaders=aValue) then exit;
  FreeAndNil(FHeaders);
  FHeaders:=aValue;
  AddKeyWord(eckHeaders);
end;


procedure TEncoding.Set_Style(const aValue : string);

begin
  if (FStyle=aValue) then exit;
  FStyle:=aValue;
  AddKeyWord(eckStyle);
end;


procedure TEncoding.Set_Explode(const aValue : Boolean);

begin
  if (FExplode=aValue) then exit;
  FExplode:=aValue;
  AddKeyWord(eckExplode);
end;


procedure TEncoding.Set_AllowReserved(const aValue : Boolean);

begin
  if (FAllowReserved=aValue) then exit;
  FAllowReserved:=aValue;
  AddKeyWord(eckAllowReserved);
end;


procedure TEncoding.AddKeyword(aKeyword : TEncodingKeyword);

begin
  Include(FKeywords,aKeyword);
end;


procedure TEncoding.RemoveKeyword(aKeyword : TEncodingKeyword);

begin
  Exclude(FKeywords,aKeyword);
end;


Function TEncoding.HasKeyword(aKeyword : TEncodingKeyword) : Boolean;

begin
  Result:=aKeyword in FKeywords;
end;


destructor TEncoding.destroy;

begin
  FreeAndNil(FHeaders);
  Inherited;
end;


function TEncodingMap.GetObject(const aName : TJSONStringType) : TEncoding;

begin
  Result:=TEncoding(GetObjectByName(aName));
end;


constructor TEncodingMap.Create(aParent : TBaseOpenAPIObject; const aPathComponent : TJSONStringType);

begin
  Inherited Create(aParent,aPathComponent,TEncoding);
end;


function TEncodingMap.AddItem(const aName : TJSONStringType) : TEncoding;

begin
  Result:=CreateNew(aName) as TEncoding;
//  Add(aName,Result);
end;

{ TResponses }

function TResponses.GetObject(const aName: TJSONStringType): TResponse;
begin
  Result:=TResponse(GetObjectByName(aName));
end;

function TResponses.GetResponseByIndex(aIndex : Integer): TResponse;
begin
  Result:=TResponse(NamedObjects[aIndex].Object_);
end;


constructor TResponses.Create(aParent: TBaseOpenAPIObject; const aPathComponent : TJSONStringType);
begin
  Inherited Create(aParent,aPathComponent,TResponse);
end;

function TResponses.AddItem(const aName: TJSONStringType): TResponse;
begin
  Result:=CreateNew(aName) as TResponse;
//  Add(aName,Result);
end;




function TResponse.Get_Headers : THeaderOrReferenceMap;

begin
  if Not assigned(FHeaders) then
    begin
    FHeaders:=THeaderOrReferenceMap.Create(Self,'headers');
    AddKeyWord(rkHeaders);
    end;
  Result:=FHeaders;
end;


function TResponse.Get_Content : TMediaTypeMap;

begin
  if Not assigned(FContent) then
    begin
    FContent:=TMediaTypeMap.Create(Self,'content');
    AddKeyWord(rkContent);
    end;
  Result:=FContent;
end;


function TResponse.Get_Links : TLinkOrReferenceMap;

begin
  if Not assigned(FLinks) then
    begin
    FLinks:=TLinkOrReferenceMap.Create(Self,'links');
    AddKeyWord(rkLinks);
    end;
  Result:=FLinks;
end;


procedure TResponse.Set_Description(const aValue : string);

begin
  if (FDescription=aValue) then exit;
  FDescription:=aValue;
  AddKeyWord(rkDescription);
end;


procedure TResponse.Set_Headers(const aValue : THeaderOrReferenceMap);

begin
  if (FHeaders=aValue) then exit;
  FreeAndNil(FHeaders);
  FHeaders:=aValue;
  AddKeyWord(rkHeaders);
end;


procedure TResponse.Set_Content(const aValue : TMediaTypeMap);

begin
  if (FContent=aValue) then exit;
  FreeAndNil(FContent);
  FContent:=aValue;
  AddKeyWord(rkContent);
end;


procedure TResponse.Set_Links(const aValue : TLinkOrReferenceMap);

begin
  if (FLinks=aValue) then exit;
  FreeAndNil(FLinks);
  FLinks:=aValue;
  AddKeyWord(rkLinks);
end;


procedure TResponse.AddKeyword(aKeyword : TResponseKeyword);

begin
  Include(FKeywords,aKeyword);
end;


procedure TResponse.RemoveKeyword(aKeyword : TResponseKeyword);

begin
  Exclude(FKeywords,aKeyword);
end;


Function TResponse.HasKeyword(aKeyword : TResponseKeyword) : Boolean;

begin
  Result:=aKeyword in FKeywords;
end;


destructor TResponse.destroy;

begin
  FreeAndNil(FHeaders);
  FreeAndNil(FContent);
  FreeAndNil(FLinks);
  Inherited;
end;


function TResponseOrReference.GetReference : TReference;

begin
  if Not Assigned(FReference) then
    FReference:=TReference.Create(Self,'');
  Result:=FReference;
end;


destructor TResponseOrReference.destroy;

begin
  FreeAndNil(FReference);
  Inherited;
end;


function TResponseOrReference.HasReference : Boolean;

begin
  Result:=Assigned(FReference);
end;


function TResponseOrReferenceMap.GetObject(const aName : TJSONStringType) : TResponseOrReference;

begin
  Result:=TResponseOrReference(GetObjectByName(aName));
end;


constructor TResponseOrReferenceMap.Create(aParent : TBaseOpenAPIObject; const aPathComponent : TJSONStringType);

begin
  Inherited Create(aParent,aPathComponent,TResponseOrReference);
end;


function TResponseOrReferenceMap.AddItem(const aName : TJSONStringType) : TResponseOrReference;

begin
  Result:=CreateNew(aName) as TResponseOrReference;
//  Add(aName,Result);
end;


procedure TExample.Set_Summary(const aValue : string);

begin
  if (FSummary=aValue) then exit;
  FSummary:=aValue;
  AddKeyWord(exkSummary);
end;


procedure TExample.Set_Description(const aValue : string);

begin
  if (FDescription=aValue) then exit;
  FDescription:=aValue;
  AddKeyWord(exkDescription);
end;


procedure TExample.Set_Value(const aValue : TJSONData);

begin
  if (FValue=aValue) then exit;
  FreeAndNil(FValue);
  FValue:=aValue;
  AddKeyWord(exkValue);
end;


procedure TExample.Set_ExternalValue(const aValue : string);

begin
  if (FExternalValue=aValue) then exit;
  FExternalValue:=aValue;
  AddKeyWord(exkExternalValue);
end;


procedure TExample.AddKeyword(aKeyword : TExampleKeyword);

begin
  Include(FKeywords,aKeyword);
end;


procedure TExample.RemoveKeyword(aKeyword : TExampleKeyword);

begin
  Exclude(FKeywords,aKeyword);
end;


Function TExample.HasKeyword(aKeyword : TExampleKeyword) : Boolean;

begin
  Result:=aKeyword in FKeywords;
end;


destructor TExample.destroy;

begin
  FreeAndNil(FValue);
  Inherited;
end;


function TExampleOrReference.GetReference : TReference;

begin
  if Not Assigned(FReference) then
    FReference:=TReference.Create(Self,'');
  Result:=FReference;
end;


destructor TExampleOrReference.destroy;

begin
  FreeAndNil(FReference);
  Inherited;
end;


function TExampleOrReference.HasReference : Boolean;

begin
  Result:=Assigned(FReference);
end;


function TExampleOrReferenceMap.GetObject(const aName : TJSONStringType) : TExampleOrReference;

begin
  Result:=TExampleOrReference(GetObjectByName(aName));
end;


constructor TExampleOrReferenceMap.Create(aParent: TBaseOpenAPIObject; const aPathComponent : TJSONStringType);

begin
  Inherited Create(aParent,aPathComponent,TExampleOrReference);
end;


function TExampleOrReferenceMap.AddItem(const aName : TJSONStringType) : TExampleOrReference;

begin
  Result:=CreateNew(aName) as TExampleOrReference;
//  Add(aName,Result);
end;


function TLink.Get_Parameters : TJSONObject;

begin
  if Not assigned(FParameters) then
    begin
    FParameters:=TJSONObject.Create();
    AddKeyWord(likParameters);
    end;
  Result:=FParameters;
end;


function TLink.Get_Server : TServer;

begin
  if Not assigned(FServer) then
    begin
    FServer:=TServer.Create(Self,'server');
    AddKeyWord(likServer);
    end;
  Result:=FServer;
end;


procedure TLink.Set_OperationRef(const aValue : string);

begin
  if (FOperationRef=aValue) then exit;
  FOperationRef:=aValue;
  AddKeyWord(likOperationRef);
end;


procedure TLink.Set_OperationId(const aValue : string);

begin
  if (FOperationId=aValue) then exit;
  FOperationId:=aValue;
  AddKeyWord(likOperationId);
end;


procedure TLink.Set_Parameters(const aValue : TJSONObject);

begin
  if (FParameters=aValue) then exit;
  FreeAndNil(FParameters);
  FParameters:=aValue;
  AddKeyWord(likParameters);
end;


procedure TLink.Set_RequestBody(const aValue : TJSONData);

begin
  if (FRequestBody=aValue) then exit;
  FreeAndNil(FRequestBody);
  FRequestBody:=aValue;
  AddKeyWord(likRequestBody);
end;


procedure TLink.Set_Description(const aValue : string);

begin
  if (FDescription=aValue) then exit;
  FDescription:=aValue;
  AddKeyWord(likDescription);
end;


procedure TLink.Set_Server(const aValue : TServer);

begin
  if (FServer=aValue) then exit;
  FreeAndNil(FServer);
  FServer:=aValue;
  AddKeyWord(likServer);
end;


procedure TLink.AddKeyword(aKeyword : TLinkKeyword);

begin
  Include(FKeywords,aKeyword);
end;


procedure TLink.RemoveKeyword(aKeyword : TLinkKeyword);

begin
  Exclude(FKeywords,aKeyword);
end;


Function TLink.HasKeyword(aKeyword : TLinkKeyword) : Boolean;

begin
  Result:=aKeyword in FKeywords;
end;


destructor TLink.destroy;

begin
  FreeAndNil(FParameters);
  FreeAndNil(FRequestBody);
  FreeAndNil(FServer);
  Inherited;
end;


function TLinkOrReference.GetReference : TReference;

begin
  if Not Assigned(FReference) then
    FReference:=TReference.Create(Self,'');
  Result:=FReference;
end;


destructor TLinkOrReference.destroy;

begin
  FreeAndNil(FReference);
  Inherited;
end;


function TLinkOrReference.HasReference : Boolean;

begin
  Result:=Assigned(FReference);
end;


function TLinkOrReferenceMap.GetObject(const aName : TJSONStringType) : TLinkOrReference;

begin
  Result:=TLinkOrReference(GetObjectByName(aName));
end;


constructor TLinkOrReferenceMap.Create(aParent : TBaseOpenAPIObject; const aPathComponent : TJSONStringType);

begin
  Inherited Create(aParent,aPathComponent,TLinkOrReference);
end;


function TLinkOrReferenceMap.AddItem(const aName : TJSONStringType) : TLinkOrReference;

begin
  Result:=CreateNew(aName) as TLinkOrReference;
//  Add(aName,Result);
end;


function TTag.Get_ExternalDocs : TExternalDocumentation;

begin
  if Not assigned(FExternalDocs) then
    begin
    FExternalDocs:=TExternalDocumentation.Create(Self,'externalDocs');
    AddKeyWord(tkExternalDocs);
    end;
  Result:=FExternalDocs;
end;


procedure TTag.Set_Name(const aValue : string);

begin
  if (FName=aValue) then exit;
  FName:=aValue;
  AddKeyWord(tkName);
end;


procedure TTag.Set_Description(const aValue : string);

begin
  if (FDescription=aValue) then exit;
  FDescription:=aValue;
  AddKeyWord(tkDescription);
end;


procedure TTag.Set_ExternalDocs(const aValue : TExternalDocumentation);

begin
  if (FExternalDocs=aValue) then exit;
  FreeAndNil(FExternalDocs);
  FExternalDocs:=aValue;
  AddKeyWord(tkExternalDocs);
end;


procedure TTag.AddKeyword(aKeyword : TTagKeyword);

begin
  Include(FKeywords,aKeyword);
end;


procedure TTag.RemoveKeyword(aKeyword : TTagKeyword);

begin
  Exclude(FKeywords,aKeyword);
end;


Function TTag.HasKeyword(aKeyword : TTagKeyword) : Boolean;

begin
  Result:=aKeyword in FKeywords;
end;


destructor TTag.destroy;

begin
  FreeAndNil(FExternalDocs);
  Inherited;
end;


procedure TReference.Set_Ref(const aValue : string);

begin
  if (FRef=aValue) then exit;
  FRef:=aValue;
  AddKeyWord(rfkRef);
end;


procedure TReference.Set_Summary(const aValue : string);

begin
  if (FSummary=aValue) then exit;
  FSummary:=aValue;
  AddKeyWord(rfkSummary);
end;


procedure TReference.Set_Description(const aValue : string);

begin
  if (FDescription=aValue) then exit;
  FDescription:=aValue;
  AddKeyWord(rfkDescription);
end;


procedure TReference.AddKeyword(aKeyword : TReferenceKeyword);

begin
  Include(FKeywords,aKeyword);
end;


procedure TReference.RemoveKeyword(aKeyword : TReferenceKeyword);

begin
  Exclude(FKeywords,aKeyword);
end;


Function TReference.HasKeyword(aKeyword : TReferenceKeyword) : Boolean;

begin
  Result:=aKeyword in FKeywords;
end;


destructor TReference.destroy;

begin
  Inherited;
end;



function TSchema.Get_Discriminator : TDiscriminator;

begin
  if Not assigned(FDiscriminator) then
    begin
    FDiscriminator:=TDiscriminator.Create(Self,'discriminator');
    AddKeyWord(sckDiscriminator);
    end;
  Result:=FDiscriminator;
end;


function TSchema.Get_XML : TXML;

begin
  if Not assigned(FXML) then
    begin
    FXML:=TXML.Create(Self,'XML');
    AddKeyWord(sckXML);
    end;
  Result:=FXML;
end;


function TSchema.Get_ExternalDocs : TExternalDocumentation;

begin
  if Not assigned(FExternalDocs) then
    begin
    FExternalDocs:=TExternalDocumentation.Create(Self,'externaldocs');
    AddKeyWord(sckExternalDocs);
    end;
  Result:=FExternalDocs;
end;


procedure TSchema.Set_Discriminator(const aValue : TDiscriminator);

begin
  if (FDiscriminator=aValue) then exit;
  FreeAndNil(FDiscriminator);
  FDiscriminator:=aValue;
  AddKeyWord(sckDiscriminator);
end;


procedure TSchema.Set_XML(const aValue : TXML);

begin
  if (FXML=aValue) then exit;
  FreeAndNil(FXML);
  FXML:=aValue;
  AddKeyWord(sckXML);
end;


procedure TSchema.Set_ExternalDocs(const aValue : TExternalDocumentation);

begin
  if (FExternalDocs=aValue) then exit;
  FreeAndNil(FExternalDocs);
  FExternalDocs:=aValue;
  AddKeyWord(sckExternalDocs);
end;


procedure TSchema.Set_Example(const aValue : TJSONData);

begin
  if (FExample=aValue) then exit;
  FreeAndNil(FExample);
  FExample:=aValue;
  AddKeyWord(sckExample);
end;


procedure TSchema.AddKeyword(aKeyword : TSchemaKeyword);

begin
  Include(FKeywords,aKeyword);
end;


procedure TSchema.RemoveKeyword(aKeyword : TSchemaKeyword);

begin
  Exclude(FKeywords,aKeyword);
end;


Function TSchema.HasKeyword(aKeyword : TSchemaKeyword) : Boolean;

begin
  Result:=aKeyword in FKeywords;
end;


destructor TSchema.destroy;

begin
  FreeAndNil(FDiscriminator);
  FreeAndNil(FXML);
  FreeAndNil(FExternalDocs);
  FreeAndNil(FExample);
  Inherited;
end;


function TDiscriminator.Get_Mapping : TStrings;

begin
  if Not assigned(FMapping) then
    begin
    FMapping:=TStringList.Create;
    AddKeyWord(dikMapping);
    end;
  Result:=FMapping;
end;


procedure TDiscriminator.Set_PropertyName(const aValue : string);

begin
  if (FPropertyName=aValue) then exit;
  FPropertyName:=aValue;
  AddKeyWord(dikPropertyName);
end;


procedure TDiscriminator.Set_Mapping(const aValue : TStrings);

begin
  if (FMapping=aValue) then exit;
  FreeAndNil(FMapping);
  FMapping:=aValue;
  AddKeyWord(dikMapping);
end;


procedure TDiscriminator.AddKeyword(aKeyword : TDiscriminatorKeyword);

begin
  Include(FKeywords,aKeyword);
end;


procedure TDiscriminator.RemoveKeyword(aKeyword : TDiscriminatorKeyword);

begin
  Exclude(FKeywords,aKeyword);
end;


Function TDiscriminator.HasKeyword(aKeyword : TDiscriminatorKeyword) : Boolean;

begin
  Result:=aKeyword in FKeywords;
end;


destructor TDiscriminator.destroy;

begin
  FreeAndNil(FMapping);
  Inherited;
end;


procedure TXML.Set_Name(const aValue : string);

begin
  if (FName=aValue) then exit;
  FName:=aValue;
  AddKeyWord(xmkName);
end;


procedure TXML.Set_Namespace(const aValue : string);

begin
  if (FNamespace=aValue) then exit;
  FNamespace:=aValue;
  AddKeyWord(xmkNamespace);
end;


procedure TXML.Set_Prefix(const aValue : string);

begin
  if (FPrefix=aValue) then exit;
  FPrefix:=aValue;
  AddKeyWord(xmkPrefix);
end;


procedure TXML.Set_Attribute(const aValue : Boolean);

begin
  if (FAttribute=aValue) then exit;
  FAttribute:=aValue;
  AddKeyWord(xmkAttribute);
end;


procedure TXML.Set_Wrapped(const aValue : Boolean);

begin
  if (FWrapped=aValue) then exit;
  FWrapped:=aValue;
  AddKeyWord(xmkWrapped);
end;


procedure TXML.AddKeyword(aKeyword : TXMLKeyword);

begin
  Include(FKeywords,aKeyword);
end;


procedure TXML.RemoveKeyword(aKeyword : TXMLKeyword);

begin
  Exclude(FKeywords,aKeyword);
end;


Function TXML.HasKeyword(aKeyword : TXMLKeyword) : Boolean;

begin
  Result:=aKeyword in FKeywords;
end;


destructor TXML.destroy;

begin
  Inherited;
end;


function TSecurityScheme.Get_Flows : TOAuthFlows;

begin
  if Not assigned(FFlows) then
    begin
    FFlows:=TOAuthFlows.Create(Self,'flows');
    AddKeyWord(sskFlows);
    end;
  Result:=FFlows;
end;


procedure TSecurityScheme.Set_Type_(const aValue : string);

begin
  if (FType_=aValue) then exit;
  FType_:=aValue;
  AddKeyWord(sskType);
end;


procedure TSecurityScheme.Set_Description(const aValue : string);

begin
  if (FDescription=aValue) then exit;
  FDescription:=aValue;
  AddKeyWord(sskDescription);
end;


procedure TSecurityScheme.Set_Name(const aValue : string);

begin
  if (FName=aValue) then exit;
  FName:=aValue;
  AddKeyWord(sskName);
end;


procedure TSecurityScheme.Set_In_(const aValue : string);

begin
  if (FIn_=aValue) then exit;
  FIn_:=aValue;
  AddKeyWord(sskIn);
end;


procedure TSecurityScheme.Set_Scheme(const aValue : string);

begin
  if (FScheme=aValue) then exit;
  FScheme:=aValue;
  AddKeyWord(sskScheme);
end;


procedure TSecurityScheme.Set_BearerFormat(const aValue : string);

begin
  if (FBearerFormat=aValue) then exit;
  FBearerFormat:=aValue;
  AddKeyWord(sskBearerFormat);
end;


procedure TSecurityScheme.Set_Flows(const aValue : TOAuthFlows);

begin
  if (FFlows=aValue) then exit;
  FreeAndNil(FFlows);
  FFlows:=aValue;
  AddKeyWord(sskFlows);
end;


procedure TSecurityScheme.Set_OpenIdConnectUrl(const aValue : string);

begin
  if (FOpenIdConnectUrl=aValue) then exit;
  FOpenIdConnectUrl:=aValue;
  AddKeyWord(sskOpenIdConnectUrl);
end;


procedure TSecurityScheme.AddKeyword(aKeyword : TSecuritySchemeKeyword);

begin
  Include(FKeywords,aKeyword);
end;


procedure TSecurityScheme.RemoveKeyword(aKeyword : TSecuritySchemeKeyword);

begin
  Exclude(FKeywords,aKeyword);
end;


Function TSecurityScheme.HasKeyword(aKeyword : TSecuritySchemeKeyword) : Boolean;

begin
  Result:=aKeyword in FKeywords;
end;


destructor TSecurityScheme.destroy;

begin
  FreeAndNil(FFlows);
  Inherited;
end;


function TSecuritySchemeOrReference.GetReference : TReference;

begin
  if Not Assigned(FReference) then
    FReference:=TReference.Create(Self,'');
  Result:=FReference;
end;


destructor TSecuritySchemeOrReference.destroy;

begin
  FreeAndNil(FReference);
  Inherited;
end;


function TSecuritySchemeOrReference.HasReference : Boolean;

begin
  Result:=Assigned(FReference);
end;


function TSecuritySchemeOrReferenceMap.GetObject(const aName : TJSONStringType) : TSecuritySchemeOrReference;

begin
  Result:=TSecuritySchemeOrReference(GetObjectByName(aName));
end;


constructor TSecuritySchemeOrReferenceMap.Create(aParent : TBaseOpenAPIObject; const aPathComponent : TJSONStringType);

begin
  Inherited Create(aParent,aPathComponent,TSecuritySchemeOrReference);
end;


function TSecuritySchemeOrReferenceMap.AddItem(const aName : TJSONStringType) : TSecuritySchemeOrReference;

begin
  Result:=CreateNew(aName) as TSecuritySchemeOrReference;
//  Add(aName,Result);
end;


function TOAuthFlows.Get_Implicit : TOAuthFlow;

begin
  if Not assigned(FImplicit) then
    begin
    FImplicit:=TOAuthFlow.Create(Self,'implicit');
    AddKeyWord(ofskImplicit);
    end;
  Result:=FImplicit;
end;


function TOAuthFlows.Get_Password : TOAuthFlow;

begin
  if Not assigned(FPassword) then
    begin
    FPassword:=TOAuthFlow.Create(Self,'password');
    AddKeyWord(ofskPassword);
    end;
  Result:=FPassword;
end;


function TOAuthFlows.Get_ClientCredentials : TOAuthFlow;

begin
  if Not assigned(FClientCredentials) then
    begin
    FClientCredentials:=TOAuthFlow.Create(Self,'clientCredentials');
    AddKeyWord(ofskClientCredentials);
    end;
  Result:=FClientCredentials;
end;


function TOAuthFlows.Get_ClientAuthorizationCode : TOAuthFlow;

begin
  if Not assigned(FClientAuthorizationCode) then
    begin
    FClientAuthorizationCode:=TOAuthFlow.Create(Self,'clientAuthorizationCode');
    AddKeyWord(ofskClientAuthorizationCode);
    end;
  Result:=FClientAuthorizationCode;
end;


procedure TOAuthFlows.Set_Implicit(const aValue : TOAuthFlow);

begin
  if (FImplicit=aValue) then exit;
  FreeAndNil(FImplicit);
  FImplicit:=aValue;
  AddKeyWord(ofskImplicit);
end;


procedure TOAuthFlows.Set_Password(const aValue : TOAuthFlow);

begin
  if (FPassword=aValue) then exit;
  FreeAndNil(FPassword);
  FPassword:=aValue;
  AddKeyWord(ofskPassword);
end;


procedure TOAuthFlows.Set_ClientCredentials(const aValue : TOAuthFlow);

begin
  if (FClientCredentials=aValue) then exit;
  FreeAndNil(FClientCredentials);
  FClientCredentials:=aValue;
  AddKeyWord(ofskClientCredentials);
end;


procedure TOAuthFlows.Set_ClientAuthorizationCode(const aValue : TOAuthFlow);

begin
  if (FClientAuthorizationCode=aValue) then exit;
  FreeAndNil(FClientAuthorizationCode);
  FClientAuthorizationCode:=aValue;
  AddKeyWord(ofskClientAuthorizationCode);
end;


procedure TOAuthFlows.AddKeyword(aKeyword : TOAuthFlowsKeyword);

begin
  Include(FKeywords,aKeyword);
end;


procedure TOAuthFlows.RemoveKeyword(aKeyword : TOAuthFlowsKeyword);

begin
  Exclude(FKeywords,aKeyword);
end;


Function TOAuthFlows.HasKeyword(aKeyword : TOAuthFlowsKeyword) : Boolean;

begin
  Result:=aKeyword in FKeywords;
end;


destructor TOAuthFlows.destroy;

begin
  FreeAndNil(FImplicit);
  FreeAndNil(FPassword);
  FreeAndNil(FClientCredentials);
  FreeAndNil(FClientAuthorizationCode);
  Inherited;
end;


function TOauthFlow.Get_Scopes : TStrings;

begin
  if Not assigned(FScopes) then
    begin
    FScopes:=TStringList.Create();
    AddKeyWord(ofkScopes);
    end;
  Result:=FScopes;
end;


procedure TOauthFlow.Set_AuthorizationUrl(const aValue : string);

begin
  if (FAuthorizationUrl=aValue) then exit;
  FAuthorizationUrl:=aValue;
  AddKeyWord(ofkAuthorizationUrl);
end;


procedure TOauthFlow.Set_TokenURL(const aValue : string);

begin
  if (FTokenURL=aValue) then exit;
  FTokenURL:=aValue;
  AddKeyWord(ofkTokenURL);
end;


procedure TOauthFlow.Set_RefreshURL(const aValue : string);

begin
  if (FRefreshURL=aValue) then exit;
  FRefreshURL:=aValue;
  AddKeyWord(ofkRefreshURL);
end;


procedure TOauthFlow.Set_Scopes(const aValue : TStrings);

begin
  if (FScopes=aValue) then exit;
  FreeAndNil(FScopes);
  FScopes:=aValue;
  AddKeyWord(ofkScopes);
end;


procedure TOauthFlow.AddKeyword(aKeyword : TOauthFlowKeyword);

begin
  Include(FKeywords,aKeyword);
end;


procedure TOauthFlow.RemoveKeyword(aKeyword : TOauthFlowKeyword);

begin
  Exclude(FKeywords,aKeyword);
end;


Function TOauthFlow.HasKeyword(aKeyword : TOauthFlowKeyword) : Boolean;

begin
  Result:=aKeyword in FKeywords;
end;


destructor TOauthFlow.destroy;

begin
  FreeAndNil(FScopes);
  Inherited;
end;




function THeaderOrReference.GetReference : TReference;

begin
  if Not Assigned(FReference) then
    FReference:=TReference.Create(Self,'');
  Result:=FReference;
end;


destructor THeaderOrReference.destroy;

begin
  FreeAndNil(FReference);
  Inherited;
end;


function THeaderOrReference.HasReference : Boolean;

begin
  Result:=Assigned(FReference);
end;


function THeaderOrReferenceMap.GetObject(const aName : TJSONStringType) : THeaderOrReference;

begin
  Result:=THeaderOrReference(GetObjectByName(aName));
end;


constructor THeaderOrReferenceMap.Create(aParent : TBaseOpenAPIObject; const aPathComponent : TJSONStringType);

begin
  Inherited Create(aParent,aPathComponent,THeaderOrReference);
end;


function THeaderOrReferenceMap.AddItem(const aName : TJSONStringType) : THeaderOrReference;

begin
  Result:=CreateNew(aName) as THeaderOrReference;
//  Add(aName,Result);
end;



function TCallbackOrReference.GetReference : TReference;

begin
  if Not Assigned(FReference) then
    FReference:=TReference.Create(Self,'');
  Result:=FReference;
end;


destructor TCallbackOrReference.destroy;

begin
  FreeAndNil(FReference);
  Inherited;
end;


function TCallbackOrReference.HasReference : Boolean;

begin
  Result:=Assigned(FReference);
end;


function TCallbackOrReferenceMap.GetObject(const aName : TJSONStringType) : TCallbackOrReference;

begin
  Result:=TCallbackOrReference(GetObjectByName(aName));
end;


constructor TCallbackOrReferenceMap.Create(aParent: TBaseOpenAPIObject; const aPathComponent : TJSONStringType);

begin
  Inherited Create(aParent,aPathComponent,TCallbackOrReference);
end;


function TCallbackOrReferenceMap.AddItem(const aName : TJSONStringType) : TCallbackOrReference;

begin
  Result:=CreateNew(aName) as TCallbackOrReference;
//  Add(aName,Result);
end;

{ TNamedStringList }

function TNamedStringList.CreateNew(const aName: TJSONStringType): TObject;
begin
  Result:=TStringList.Create;
end;

function TSecurityRequirement.GetRequirements(aName : TJSONStringType): TStrings;
begin
  Result:=TStrings(FList.GetObjectByName(aName))
end;

function TSecurityRequirement.GetCount: Integer;
begin
  Result:=FList.Count;
end;

function TSecurityRequirement.GetList(aIndex : Integer): TStrings;
begin
  Result:=FList.NamedObjects[aIndex].Object_ as TStrings;
end;

function TSecurityRequirement.GetName(aIndex : Integer): String;
begin
  Result:=FList.Names[aIndex];
end;

constructor TSecurityRequirement.Create(aParent : TBaseOpenAPIObject; const aPathComponent : TJSONStringType);
begin
  Inherited Create(aParent,aPathComponent);
  FList:=TNamedStringList.Create(aParent,aPathComponent,TStringList);
end;

destructor TSecurityRequirement.destroy;
begin
  FreeAndNil(FList);
  Inherited;
end;

function TSecurityRequirement.AddItem(const AName: TJSONStringType): TStrings;
begin
  Result:=TStringList.Create();
  Flist.Add(aName,Result);
end;


function TSecurityRequirementList.GetObj(aIndex : Integer): TSecurityRequirement;

begin
  Result:=TSecurityRequirement(Items[aIndex]);
end;


procedure TSecurityRequirementList.SetObj(aIndex : Integer; aValue : TSecurityRequirement);

begin
  Items[aIndex]:=aValue;
end;


function TSecurityRequirementList.ToArray : TSecurityRequirementArray;

var
  I : Integer;

begin
  Result:=[];
  SetLength(Result,Count);
  For I:=0 to Count-1 do
    Result[I]:=GetObj(I);
end;

procedure TSecurityRequirementList.AddSecurity(aSecurity: TSecurityRequirement);
begin
  Add(aSecurity);
end;

function TSecurityRequirementList.AddSecurity(aParent: TBaseOpenAPIObject): TSecurityRequirement;
begin
  Result:=TSecurityRequirement.Create(aParent,Format('[%d]',[count]));
  AddSecurity(Result);
end;

{ TBaseOpenAPIObject }

function TBaseOpenAPIObject.CreateChildrenList: TBaseOpenAPIObjectList;
begin
  Result:=TBaseOpenAPIObjectList.Create(False);
end;

procedure TBaseOpenAPIObject.AddChild(aChild: TBaseOpenAPIObject);
begin
  if not Assigned(FChildren) then
    FChildren:=CreateChildrenList;
  FChildren.Add(aChild);
end;

procedure TBaseOpenAPIObject.RemoveChild(aChild: TBaseOpenAPIObject);
begin
  if Assigned(FChildren) then
    FChildren.Remove(aChild);
end;

procedure TBaseOpenAPIObject.SetPathComponent(const aValue: TJSONStringType);
begin
  FPathComponent:=aValue;
end;

constructor TBaseOpenAPIObject.create(aParent: TBaseOpenAPIObject; aPathComponent : TJSONStringType);
begin
  FParent:=aParent;
  FPathComponent:=aPathComponent;
  if assigned(FParent) then
    FParent.AddChild(Self);
end;

destructor TBaseOpenAPIObject.destroy;

begin
  if Assigned(FParent) then
     FParent.RemoveChild(Self);
  FreeAndNil(FChildren);
  inherited destroy;
end;

function TBaseOpenAPIObject.GetChildByPathComponent(const aName: TJSONStringType): TBaseOpenAPIObject;

var
  I : Integer;
begin
  Result:=Nil;
  if Not Assigned(FChildren) then
    exit;
  I:=FChildren.Count-1;
  While (I>=0) and (FChildren[I].PathComponent<>aName) do
    Dec(I);
  if I>=0 then
    Result:=FChildren[I];
end;

function TBaseOpenAPIObject.Find(const aPath: TJSONStringType): TBaseOpenAPIObject;

var
  Child,Sub : TJSONStringType;
  P : Integer;

begin
  if aPath='' then
    Exit(Self);
  P:=Pos('/',aPath);
  If P=0 then
    P:=Length(aPath)+1;
  Child:=Copy(aPath,1,P-1);
  Sub:=Copy(aPath,P+1,Length(aPath)-P);
  Result:=GetChildByPathComponent(Child);
  if Assigned(Result) then
    Result:=Result.Find(Sub);
end;

function TBaseOpenAPIObject.PathComponent: TJSONStringType;
begin
  Result:=FPathComponent;
end;

function TBaseOpenAPIObject.Path: TJSONStringType;

var
  O : TBaseOpenAPIObject;

begin
  Result:=EncodeJSONPointer(PathComponent);
  O:=Parent;
  While Assigned(O) do
    begin
    if (O.PathComponent<>'') then
      Result:=O.PathComponent+'/'+Result;
    O:=O.Parent;
    end;
end;

{ TBaseOpenAPIObjectList }

function TBaseOpenAPIObjectList.GetObject(aIndex : Integer): TBaseOpenAPIObject;
begin
  Result:=Items[aIndex] as TBaseOpenAPIObject;
end;


end.

