{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2024 by Michael Van Canneyt (michael@freepascal.org)

    Classes for writing a OpenAPI document.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit fpopenapi.writer;

{$mode objfpc}
{$h+}

interface

 uses
  {$IFDEF FPC_DOTTEDUNITS}
  System.SysUtils, System.Classes, System.Contnrs, FpJson.Data, FpJson.Writer,
  {$ELSE}
  sysutils, classes, contnrs, fpjson, jsonwriter,
  {$ENDIF}
  fpopenapi.types, fpopenapi.objects, fpjson.schema.schema, fpjson.schema.Writer;

Type
  EOpenAPIWriter = Class(EOpenAPi);

  { TOpenAPIReader }
  TWriteObjectFunc = Procedure(aObject : TBaseOpenAPIObject) of object;

  { TOpenAPIWriter }

  TOpenAPIWriter = Class(TComponent)
  Private
    FWriter : TAbstractJSONWriter;
    FSchemaWriter : TJSONSchemaWriter;
    // Untyped callbacks
    procedure WriteServerVariableUntyped(aObj: TBaseOpenAPIObject);
    procedure WritePathItemUntyped(aObj: TBaseOpenAPIObject);
    procedure WritePathItemOrReferenceUnTyped(aObj: TBaseOpenAPIObject);
    procedure WriteParameterOrReferenceUntyped(aObj: TBaseOpenAPIObject);
    procedure WriteRequestBodyOrReferenceUnTyped(aObj: TBaseOpenAPIObject);
    procedure WriteMediaTypeUnTyped(aObj: TBaseOpenAPIObject);
    procedure WriteEncodingUntyped(aObj: TBaseOpenAPIObject);
    procedure WriteResponseOrReferenceUnTyped(aObj: TBaseOpenAPIObject);
    procedure WriteExampleOrReferenceUnTyped(aObj: TBaseOpenAPIObject);
    procedure WriteLinkOrReferenceUntyped(aObj: TBaseOpenAPIObject);
    procedure WriteHeaderOrReferenceUnTyped(aObj: TBaseOpenAPIObject);
    procedure WriteSecuritySchemeOrReferenceUnTyped(aObj: TBaseOpenAPIObject);
    procedure WriteCallbackOrReferenceUnTyped(aObj: TBaseOpenAPIObject);
    procedure WriteTagUnTyped(aObj: TBaseOpenAPIObject);
    procedure WriteSecurityRequirementUnTyped(aObj: TBaseOpenAPIObject);
    procedure WriteResponseUntyped(aObj: TBaseOpenAPIObject);
//  Protected
    // utility
    Procedure StartObjectProp(const aName : String); inline;
    Procedure EndObjectProp; inline;
    Procedure StartArrayProp(const aName : String); inline;
    Procedure EndArrayProp; inline;
    Procedure WriteProperty(const aName : String; const aValue : String); inline;
    Procedure WriteProperty(const aName : String; const aValue : Boolean); inline;
    Procedure WriteProperty(const aName : String; const aValue : Integer); inline;
    Procedure WriteProperty(const aName : String; const aValue : Int64); inline;
    Procedure WriteProperty(const aName : String; const aValue : Double); inline;
    Procedure WriteProperty(const aName : String; const aValue : TJSONSchema); inline;
    Procedure WriteProperty(const aName : String; const aValue : TJSONData); inline;
    procedure WriteStrings(const aKey : String; aList : TStrings) ;
    procedure WriteStringMap(const aKey : String; aList : TStrings) ;
    procedure WriteExtensions(aObj : TJSONObject);
    procedure WriteObjectArray(const aKey: String; aList: TFPObjectList; aWriteObject: TWriteObjectFunc); overload;
    procedure WriteMapObject(const aKey : String; aList: TNamedOpenAPIObjectList; aObjectWriter : TWriteObjectFunc);
    procedure WriteMapObject(aList: TNamedOpenAPIObjectList; aObjectWriter : TWriteObjectFunc);
    // objects
    procedure WriteOpenAPI(aObj : TOpenAPI); virtual; overload;
    procedure WriteInfo(const aKey: string; aObj: TInfo);
    procedure WriteInfo(aObj: TInfo); virtual; overload;
    procedure WriteContact(const aKey: string; aObj: TContact);
    procedure WriteContact(aObj: TContact); virtual; overload;
    procedure WriteLicense(const aKey: string; aObj: TLicense);
    procedure WriteLicense(aObj: TLicense); virtual; overload;
    procedure WriteServer(aObj: TServer); virtual; overload;
    procedure WriteServer(const aKey: string; aObj: TServer); virtual; overload;
    procedure WriteServerList(const aKey : string; aObj : TServerList); virtual; overload;
    procedure WritePathsList(const aKey : string; aObj : TPathsList); virtual; overload;
    procedure WriteServerVariable(aObj : TServerVariable); virtual; overload;
    procedure WriteServerVariableMap(const aKey: string; aObj: TServerVariableMap); virtual; overload;
    procedure WriteJSONSchemaMap(const aKey: String; aObj: TJSONSchemaMap); virtual; overload;
    procedure WriteComponents(aObj : TComponents); virtual; overload;
    procedure WriteComponents(const aKey : String; aObj : TComponents);
    procedure WritePathItem(aObj : TPathItem); virtual; overload;
    procedure WritePathItemOrReference(aObj : TPathItemOrReference); virtual; overload;
    procedure WritePathItemOrReferenceMap(const aKey : string; aObj : TPathItemOrReferenceMap); virtual; overload;
    procedure WriteApiOperation(aObj: TApiOperation);
    procedure WriteApiOperation(aKey : string; aObj : TApiOperation); virtual; overload;
    procedure WriteExternalDocumentation(aObj : TExternalDocumentation); virtual; overload;
    procedure WriteExternalDocumentation(const aKey : String; aObj : TExternalDocumentation); virtual; overload;
    procedure WriteParameterOrHeader(aObj: TParameterOrHeader);
    procedure WriteParameter(aObj : TParameter); virtual; overload;
    procedure WriteParameterOrReference(aObj : TParameterOrReference); virtual; overload;
    procedure WriteParameterOrReferenceList(const aKey : string; aObj : TParameterOrReferenceList); virtual; overload;
    procedure WriteParameterOrReferenceMap(const aKey : String; aObj : TParameterOrReferenceMap); virtual; overload;
    procedure WriteParameterStyle(aObj : TParameterStyle); virtual; overload;
    procedure WriteRequestBody(aObj: TRequestBody); virtual; overload;
    procedure WriteRequestBody(const aKey : string; aObj : TRequestBody);
    procedure WriteRequestBodyOrReference(aObj : TRequestBodyOrReference); virtual; overload;
    procedure WriteRequestBodyOrReferenceMap(const aKey : string; aObj : TRequestBodyOrReferenceMap); virtual; overload;
    procedure WriteMediaType(aObj : TMediaType); virtual; overload;
    procedure WriteMediaTypeMap(const aKey : String; aObj : TMediaTypeMap); virtual; overload;
    procedure WriteEncoding(aObj : TEncoding); virtual; overload;
    procedure WriteEncodingMap(const aKey : string; aObj : TEncodingMap); virtual; overload;
    procedure WriteResponse(aObj : TResponse); virtual; overload;
    procedure WriteResponses(const aKey : String; aObj : TResponses); virtual; overload;
    procedure WriteResponseOrReference(aObj : TResponseOrReference); virtual; overload;
    procedure WriteResponseOrReferenceMap(const aKey : String; aObj : TResponseOrReferenceMap); virtual; overload;
    procedure WriteExample(aObj : TExample); virtual; overload;
    procedure WriteExampleOrReference(aObj : TExampleOrReference); virtual; overload;
    procedure WriteExampleOrReferenceMap(const aKey : String; aObj : TExampleOrReferenceMap); virtual; overload;
    procedure WriteLink(aObj : TLink); virtual; overload;
    procedure WriteLinkOrReference(aObj : TLinkOrReference); virtual; overload;
    procedure WriteLinkOrReferenceMap(const aKey : string; aObj : TLinkOrReferenceMap); virtual; overload;
    procedure WriteReference(aObj : TReference); virtual; overload;
    procedure WriteSchema(aObj : TSchema); virtual; overload;
    procedure WriteXML(aObj : TXML); virtual; overload;
    procedure WriteSecurityScheme(aObj : TSecurityScheme); virtual; overload;
    procedure WriteSecuritySchemeOrReference(aObj : TSecuritySchemeOrReference); virtual; overload;
    procedure WriteSecuritySchemeOrReferenceMap(const aKey : string; aObj : TSecuritySchemeOrReferenceMap); virtual; overload;
    procedure WriteOAuthFlow(aObj : TOauthFlow); virtual; overload;
    procedure WriteOAuthFlow(const aKey : String; aObj : TOauthFlow); virtual; overload;
    procedure WriteOAuthFlows(aObj : TOAuthFlows); virtual; overload;
    procedure WriteOAuthFlows(const aKey : String; aObj : TOAuthFlows); virtual; overload;
    procedure WriteHeader(aObj : THeader); virtual; overload;
    procedure WriteHeaderOrReference(aObj : THeaderOrReference); virtual; overload;
    procedure WriteHeaderOrReferenceMap(const aKey : string; aObj : THeaderOrReferenceMap); virtual; overload;
    procedure WriteCallbackOrReference(aObj : TCallbackOrReference); virtual; overload;
    procedure WriteCallbackOrReferenceMap(const aKey : string; aObj : TCallbackOrReferenceMap); virtual; overload;
    procedure WriteCallBack(aObj: TCallBack);
    procedure WriteDiscriminator(aObj : TDiscriminator); virtual; overload;
    procedure WriteDiscriminator(const aKey: string; aObj: TDiscriminator);
    procedure WriteSecurityRequirement(aObj : TSecurityRequirement); virtual; overload;
    procedure WriteSecurityRequirement(const aKey: String; aObj: TSecurityRequirement);
    procedure WriteSecurityRequirementList(const aKey : String; aObj : TSecurityRequirementList); virtual; overload;
    procedure WriteTag(aObj : TTag); virtual; overload;
    procedure WriteTagList(const aKey : string; aObj : TTagList); virtual; overload;
    property Writer : TAbstractJSONWriter Read FWriter;
 Public
   procedure Write(aOpenAPI: TOpenAPI; AWriter: TAbstractJSONWriter);
   procedure WriteToStream(aOpenAPI: TOpenAPI; AStream: TStream);
   function WriteToJSON(aOpenAPI: TOpenAPI) : TJSONObject;
   function WriteToString(aOpenAPI: TOpenAPI) : TJSONStringType;
 end;

implementation

{ ---------------------------------------------------------------------
  Public routines
  ---------------------------------------------------------------------}

procedure TOpenAPIWriter.Write(aOpenAPI: TOpenAPI; AWriter: TAbstractJSONWriter);
begin
  FWriter:=aWriter;
  try
    FSchemaWriter:=TJSONSchemaWriter.Create(Self);
    WriteOpenAPI(aOpenAPI);
  finally
    FreeAndNil(FSchemaWriter);
    FWriter:=Nil;
  end;
end;

procedure TOpenAPIWriter.WriteToStream(aOpenAPI: TOpenAPI; AStream: TStream);

var
  lWriter : TJSONStreamWriter;

begin
  lWriter:=TJSONStreamWriter.Create(aStream);
  try
    Write(aOpenApi,lWriter);
  finally
    lWriter.Free;
  end;

end;

function TOpenAPIWriter.WriteToJSON(aOpenAPI: TOpenAPI): TJSONObject;
var
  lWriter : TJSONDataWriter;
  lData : TJSONData;

begin
  lWriter:=TJSONDataWriter.Create;
  try
    Write(aOpenApi,lWriter);
    lData:=lWriter.ExtractData;
    if lData is TJSONObject then
      Result:=TJSONObject(lData)
    else
      begin
      lData.Free;
      Result:=nil;
      end;
  finally
    lWriter.Free;
  end;
end;

function TOpenAPIWriter.WriteToString(aOpenAPI: TOpenAPI) : TJSONStringType;
var
  S: TStringStream;
begin
  S:=TStringStream.Create('');
  try
    WriteToStream(aOpenAPI,S);
    Result:=S.DataString;
  finally
    S.Free;
  end;
end;

{ ---------------------------------------------------------------------
  Utility routines
  ---------------------------------------------------------------------}

procedure TOpenAPIWriter.WriteProperty(const aName: String; const aValue: String);

begin
  Writer.WriteProperty(aName,aValue);
end;

procedure TOpenAPIWriter.WriteProperty(const aName: String; const aValue: Boolean);

begin
  Writer.WriteProperty(aName,aValue);
end;

procedure TOpenAPIWriter.WriteProperty(const aName: String; const aValue: Integer);

begin
  Writer.WriteProperty(aName,aValue);
end;

procedure TOpenAPIWriter.WriteProperty(const aName: String; const aValue: Int64);

begin
  Writer.WriteProperty(aName,aValue);
end;

procedure TOpenAPIWriter.WriteProperty(const aName: String; const aValue: Double);

begin
  Writer.WriteProperty(aName,aValue);
end;

procedure TOpenAPIWriter.WriteProperty(const aName: String; const aValue: TJSONSchema);

begin
  Writer.StartProperty(aName);
  FSchemaWriter.Writeschema(aValue,Writer);
  Writer.EndProperty;
end;

procedure TOpenAPIWriter.WriteProperty(const aName: String; const aValue: TJSONData);

begin
  Writer.WriteProperty(aName,aValue);
end;

procedure TOpenAPIWriter.StartObjectProp(const aName: String);
begin
  Writer.StartProperty(aName);
  Writer.StartObject;
end;

procedure TOpenAPIWriter.EndObjectProp;

begin
  Writer.EndObject;
  Writer.EndProperty;
end;

procedure TOpenAPIWriter.StartArrayProp(const aName: String);
begin
  Writer.StartProperty(aName);
  Writer.StartArray;
end;

procedure TOpenAPIWriter.EndArrayProp;
begin
  Writer.EndArray;
  Writer.EndProperty;
end;

procedure TOpenAPIWriter.WriteExtensions(aObj : TJSONObject);

var
  I : Integer;

begin
  For I:=0 to aObj.Count-1 do
    WriteProperty(aObj.Names[i],aObj.Items[i]);
end;

procedure TOpenAPIWriter.WriteMapObject(aList: TNamedOpenAPIObjectList; aObjectWriter: TWriteObjectFunc);
var
  I : Integer;

begin
  For I:=0 to aList.Count-1 do
    begin
    StartObjectProp(aList.Names[i]);
    aObjectWriter(aList.APIObjects[i]);
    EndObjectProp;
    end;
end;

procedure TOpenAPIWriter.WriteStrings(const aKey: String; aList: TStrings);

var
  S : String;

begin
  Writer.StartProperty(aKey);
  Writer.StartArray;
  For S in aList do
    begin
    Writer.NextElement;
    Writer.WriteValue(S);
    end;
  Writer.EndArray;
  Writer.EndProperty();
end;

procedure TOpenAPIWriter.WriteStringMap(const aKey: String; aList: TStrings);
// Writes TStrings as a JSON object with key-value pairs (Name=Value format)
var
  I: Integer;
begin
  Writer.StartProperty(aKey);
  Writer.StartObject;
  For I := 0 to aList.Count - 1 do
    begin
    Writer.NextElement;
    Writer.WriteProperty(aList.Names[I], aList.ValueFromIndex[I]);
    end;
  Writer.EndObject;
  Writer.EndProperty();
end;

procedure TOpenAPIWriter.WriteObjectArray(const aKey: String; aList: TFPObjectList; aWriteObject: TWriteObjectFunc);

var
  i : integer;

begin
  StartArrayProp(aKey);
  for I:=0 to aList.count-1 do
    begin
    Writer.NextElement;
    Writer.StartObject;
    aWriteObject(TBaseOpenAPIObject(aList[i]));
    Writer.EndObject;
    end;
  EndArrayProp;
end;

procedure TOpenAPIWriter.WriteMapObject(const aKey: String; aList: TNamedOpenAPIObjectList; aObjectWriter: TWriteObjectFunc);
begin
  StartObjectProp(aKey);
  WriteMapObject(aList,aObjectWriter);
  EndObjectProp;
end;

{ ---------------------------------------------------------------------
  Objects
  ---------------------------------------------------------------------}

// OpenAPI

procedure TOpenAPIWriter.WriteOpenAPI(aObj: TOpenAPI);

var
  lName : String;
  lKeyword : TOpenAPIKeyword;

begin
  Writer.StartObject;
  for lKeyword in TOPenAPIKeyword do
    if aObj.HasKeyWord(lKeyword) then
      begin
      lName:=lKeyword.ToString;
      case lKeyword of
      oakOpenApi:
        WriteProperty(lName,aObj.OpenApi);
      oakInfo:
        WriteInfo(lName, aObj.Info);
      oakJSONSchemaDialect:
        WriteProperty(lName,aObj.JSONSchemaDialect);
      oakServers:
        WriteServerList(lName,aObj.Servers);
      oakPaths:
        WritePathsList(lName,aObj.Paths);
      oakWebHooks:
        WritePathItemOrReferenceMap(lName,aObj.WebHooks);
      oakComponents:
        WriteComponents(lName,aObj.Components);
      oakSecurity:
        WriteSecurityRequirementList(lName,aObj.Security);
      oakTags:
        WriteTagList(lName,aObj.Tags);
      oakExternalDocs:
        WriteExternalDocumentation(lName,aObj.ExternalDocs);
      end;
    end;
  if aObj.HasExtensions then
    WriteExtensions(aObj.Extensions);
  Writer.endObject;
end;

// Info

procedure TOpenAPIWriter.WriteInfo(const aKey : string; aObj: TInfo);

begin
  StartObjectProp(aKey);
  WriteInfo(aObj);
  EndObjectProp;
end;

procedure TOpenAPIWriter.WriteInfo(aObj: TInfo);

var
  lName : String;
  lKeyword : TInfoKeyword;

begin
  for lKeyword in TInfoKeyword do
    if aObj.HasKeyWord(lKeyword) then
      begin
      lName:=lKeyword.ToString;
      case lKeyword of
      ikTitle:
        WriteProperty(lName,aObj.Title);
      ikSummary:
        WriteProperty(lName,aObj.Summary);
      ikDescription:
        WriteProperty(lName,aObj.Description);
      ikTermsOfService:
        WriteProperty(lName,aObj.TermsOfService);
      ikContact:
        WriteContact(lName,aObj.Contact);
      ikLicense:
        WriteLicense(lName,aObj.License);
      ikVersion:
        WriteProperty(lName,aObj.Version);
      end;
      end;
  if aObj.HasExtensions then
    WriteExtensions(aObj.Extensions);
end;

// Contact

procedure TOpenAPIWriter.WriteContact(const aKey : string; aObj: TContact);

begin
  StartObjectProp(aKey);
  WriteContact(aObj);
  EndObjectProp;
end;

procedure TOpenAPIWriter.WriteContact(aObj: TContact);

var
  lName : String;
  lKeyword : TContactKeyword;

begin
  For lKeyword in TContactKeyword do
    if aObj.HasKeyWord(lkeyword) then
      begin
      lName:=lKeyword.ToString;
      case lKeyword of
      cokName:
        WriteProperty(lName,aObj.Name);
      cokUrl:
        WriteProperty(lName,aObj.Url);
      cokEmail:
        WriteProperty(lName,aObj.Email);
      end;
      end;
  if aObj.HasExtensions then
    WriteExtensions(aObj.Extensions);
end;

// License

procedure TOpenAPIWriter.WriteLicense(const aKey : string; aObj: TLicense);

begin
  StartObjectProp(aKey);
  WriteLicense(aObj);
  EndObjectProp;
end;

procedure TOpenAPIWriter.WriteLicense(aObj: TLicense);
var
  lName : String;
  lKeyword : TLicenseKeyword;
begin
  for lKeyword in TLicenseKeyword do
    if aObj.HasKeyWord(lKeyword) then
    begin
    lName:=lKeyword.ToString;
    case lKeyword of
    lkName:
      WriteProperty(lName,aObj.Name);
    lkIdentifier:
      WriteProperty(lName,aObj.Identifier);
    lkUrl:
      WriteProperty(lName,aObj.Url);
    end;
    end;
  if aObj.HasExtensions then
    WriteExtensions(aObj.Extensions);
end;

// Server

procedure TOpenAPIWriter.WriteServer(aObj: TServer);

var
  lName : String;
  lKeyword : TServerKeyword;
begin
  For lKeyword in TServerKeyword do
    if aObj.HasKeyWord(lKeyword) then
      begin
      lName:=lKeyword.ToString;
      case lKeyword of
      skServerUrl:
        WriteProperty(lName,aObj.Url);
      skDescription:
        WriteProperty(lName,aObj.Description);
      skVariables:
        WriteServerVariableMap(skVariables.ToString, aObj.Variables);
      end;
      end;
  if aObj.HasExtensions then
    WriteExtensions(aObj.Extensions);
end;

procedure TOpenAPIWriter.WriteServer(const aKey: string; aObj: TServer);
begin
  StartObjectProp(aKey);
  WriteServer(aObj);
  EndObjectProp;
end;

procedure TOpenAPIWriter.WriteServerList(const aKey: string; aObj: TServerList);

var
  I : Integer;

begin
  StartArrayProp(aKey);
  For I:=0 to aObj.Count-1 do
    begin
    Writer.NextElement;
    Writer.StartObject;
    WriteServer(aObj.Servers[i]);
    Writer.EndObject;
    end;
  EndArrayProp;
end;

// Server variable

procedure TOpenAPIWriter.WriteServerVariable(aObj: TServerVariable);

var
  lName : String;
  lKeyword : TServerVariableKeyword;

begin
  for lKeyword in TServerVariableKeyword do
    if aObj.HasKeyword(lKeyword) then
      begin
      lName:=lKeyWord.ToString;
      case lKeyword of
      svkEnum:
        WriteStrings(lName,aObj.Enum);
      svkDefault:
        WriteProperty(lName,aObj.Default);
      svkDescription:
        WriteProperty(lName,aObj.Description);
      end;
      end;
  if aObj.HasExtensions then
    WriteExtensions(aObj.Extensions);
end;

procedure TOpenAPIWriter.WriteServerVariableUntyped(aObj: TBaseOpenAPIObject);
begin
  WriteServerVariable(aObj as TServerVariable);
end;

procedure TOpenAPIWriter.WriteServerVariableMap(const aKey : string; aObj: TServerVariableMap);

begin
  StartObjectProp(aKey);
  WriteMapObject(aObj,@WriteServerVariableUntyped);
  EndObjectProp;
end;

// Components

procedure TOpenAPIWriter.WriteComponents(aObj: TComponents);

var
  lName : String;
  lKeyword : TComponentsKeyword;
begin
  for lKeyword in TComponentsKeyword do
    if aObj.HasKeyWord(lKeyword) then
      begin
      lName:=lKeyword.ToString;
      case lKeyword of
      ckSchemas:
        WriteJSONSchemaMap(lName,aObj.Schemas);
      ckResponses:
        WriteResponseOrReferenceMap(lName,aObj.Responses);
      ckParameters:
        WriteParameterOrReferenceMap(lName,aObj.Parameters);
      ckExamples:
        WriteExampleOrReferenceMap(lName,aObj.Examples);
      ckRequestBodies:
        WriteRequestBodyOrReferenceMap(lName,aObj.RequestBodies);
      ckHeaders:
        WriteHeaderOrReferenceMap(lName,aObj.Headers);
      ckSecuritySchemes:
        WriteSecuritySchemeOrReferenceMap(lName,aObj.SecuritySchemes);
      ckLinks:
        WriteLinkOrReferenceMap(lName,aObj.Links);
      ckCallbacks:
        WriteLinkOrReferenceMap(lName,aObj.Callbacks);
      ckPathItems:
        WritePathItemOrReferenceMap(lName,aObj.PathItems);
      end;
      end;
  if aObj.HasExtensions then
    WriteExtensions(aObj.Extensions);
end;

procedure TOpenAPIWriter.WriteComponents(const aKey: String; aObj: TComponents);
begin
  StartObjectProp(aKey);
  WriteComponents(aObj);
  EndObjectProp;
end;

procedure TOpenAPIWriter.WriteJSONSchemaMap(const aKey : String; aObj: TJSONSchemaMap);
var
  lName : String;
  lSchema : TJSONSchema;
  I : Integer;

begin
  StartObjectProp(aKey);
  For I:=0 to aObj.Count-1 do
    begin
    lName:=aObj.Names[i];
    lSchema:=aObj.NamedObjects[i].Object_ as TJSONSchema;
    WriteProperty(lName,lSchema);
    end;
  EndObjectProp();
end;

// Paths

procedure TOpenAPIWriter.WritePathsList(const aKey: string; aObj: TPathsList);

begin
  WriteMapObject(aKey,aObj,@WritePathItemUnTyped);
end;

procedure TOpenAPIWriter.WritePathItemUntyped(aObj: TBaseOpenAPIObject);

begin
  WritePathItem(aObj as TPathItem);
end;

procedure TOpenAPIWriter.WritePathItem(aObj: TPathItem);

var
  lName : String;
  lKeyword : TPathItemKeyword;

begin
  for lKeyword in TPathItemKeyword do
    if aObj.HasKeyword(lKeyword) then
      begin
      lName:=lKeyword.AsString;
      case lKeyword of
      pkRef:
        WriteProperty(lName,aObj.Ref);
      pkSummary:
        WriteProperty(lName,aObj.Summary);
      pkDescription:
        WriteProperty(lName,aObj.Description);
      pkGet:
        WriteApiOperation(lName,aObj.Get);
      pkPut:
        WriteApiOperation(lName,aObj.Put);
      pkPost:
        WriteApiOperation(lName,aObj.Post);
      pkDelete:
        WriteApiOperation(lName,aObj.Delete);
      pkOptions:
        WriteApiOperation(lName,aObj.Options);
      pkHead:
        WriteApiOperation(lName,aObj.Head);
      pkPatch:
        WriteApiOperation(lName,aObj.Patch);
      pkTrace:
        WriteApiOperation(lName,aObj.Trace);
      pkServers:
        WriteServerList(lName,aObj.Servers);
      pkParameters:
        WriteParameterOrReferenceList(lName,aObj.Parameters);
      end;
      end;
  if aObj.HasExtensions then
    WriteExtensions(aObj.Extensions);
end;


procedure TOpenAPIWriter.WritePathItemOrReference(aObj: TPathItemOrReference);

begin
  if aObj.HasReference then
    WriteReference(aObj.Reference)
  else
    WritePathItem(aObj);
end;

procedure TOpenAPIWriter.WritePathItemOrReferenceUnTyped(aObj: TBaseOpenAPIObject);

begin
  WritePathItemOrReference(aObj as TPathItemOrReference);
end;

procedure TOpenAPIWriter.WritePathItemOrReferenceMap(const aKey: string; aObj: TPathItemOrReferenceMap);

begin
  WriteMapObject(aKey,aObj,@WritePathItemOrReferenceUnTyped);
end;

// Operations

procedure TOpenAPIWriter.WriteApiOperation(aKey: string; aObj: TApiOperation);

begin
  StartObjectProp(aKey);
  WriteAPIoperation(aObj);
  EndObjectProp;
end;

procedure TOpenAPIWriter.WriteApiOperation(aObj: TApiOperation);

var
  lName : String;
  lKeyword : TApiOperationKeyword;

begin
  for lKeyword in TApiOperationKeyword do
    if aObj.HasKeyWord(lKeyword) then
      begin
      lName:=lKeyWord.AsString;
      case lKeyword of
      okTags:
        WriteStrings(lName,aObj.Tags);
      okSummary:
        WriteProperty(lName,aObj.Summary);
      okDescription:
        WriteProperty(lName,aObj.Description);
      okExternalDocs:
        WriteExternalDocumentation(lName,aObj.ExternalDocs);
      okOperationId:
        WriteProperty(lName,aObj.OperationId);
      okParameters:
        WriteParameterOrReferenceList(lName,aObj.Parameters);
      okRequestBody:
        WriteRequestBody(lName,aObj.RequestBody);
      okResponses:
        WriteResponses(lName,aObj.Responses);
      okCallbacks:
        WriteCallBackOrReferenceMap(lName,aObj.Callbacks);
      okDeprecated:
        WriteProperty(lName,aObj.Deprecated);
      okSecurity:
        WriteSecurityRequirementList(lName,aObj.Security);
      okServers:
        WriteServerList(lName,aObj.Servers);
      end;
      end;
  if aObj.HasExtensions then
    WriteExtensions(aObj.Extensions);
end;

// External docs

procedure TOpenAPIWriter.WriteExternalDocumentation(const aKey: String; aObj: TExternalDocumentation);

begin
  StartObjectProp(aKey);
  WriteExternalDocumentation(aObj);
  EndObjectProp;
end;

procedure TOpenAPIWriter.WriteExternalDocumentation(aObj: TExternalDocumentation);

var
  lName : String;
  lKeyword : TExternalDocumentationKeyword;

begin
  for lKeyword in TExternalDocumentationKeyword do
    if aObj.HasKeyWord(lKeyword) then
      begin
      lName:=lKeyWord.AsString;
      case lKeyword of
      edkDescription:
        WriteProperty(lName,aObj.Description);
      edkUrl:
        WriteProperty(lName,aObj.Url);
      end;
      end;
  if aObj.HasExtensions then
    WriteExtensions(aObj.Extensions);
end;

// Parameters

procedure TOpenAPIWriter.WriteParameter(aObj: TParameter);

begin
  WriteParameterOrHeader(aObj);
end;

procedure TOpenAPIWriter.WriteParameterOrHeader(aObj: TParameterOrHeader);

var
  lName : string;
  lKeyword : TParameterKeyword;

begin
  for lKeyword in TParameterKeyword do
    if aObj.HasKeyWord(LKeyword) then
      begin
      lName:=lKeyword.AsString;
      Case lKeyword of
      pakName:
        WriteProperty(lName,TParameter(aObj).Name);
      pakIn:
        WriteProperty(lName,TParameter(aObj).In_);
      pakDescription:
        WriteProperty(lName,aObj.Description);
      pakRequired:
        WriteProperty(lName,aObj.Required);
      pakDeprecated:
        WriteProperty(lName,aObj.Deprecated);
      pakAllowEmptyValue:
        WriteProperty(lName,aObj.AllowEmptyValue);
      pakStyle:
        WriteProperty(lName,aObj.Style);
      pakExplode:
        WriteProperty(lName,aObj.Explode);
      pakAllowReserved:
        WriteProperty(lName,aObj.AllowReserved);
      pakSchema:
        WriteProperty(lName,aObj.Schema);
      pakExample:
        WriteProperty(lName,aObj.Example);
      pakExamples:
        WriteExampleOrReferenceMap(lName,aObj.Examples);
      pakContent:
        WriteMediaTypeMap(lName,aObj.Content);
      end;
      end;
  if aObj.HasExtensions then
     WriteExtensions(aObj.Extensions);
end;


procedure TOpenAPIWriter.WriteParameterOrReference(aObj: TParameterOrReference);

begin
  if aObj.HasReference then
    WriteReference(aObj.Reference)
  else
    WriteParameter(aObj)
end;


procedure TOpenAPIWriter.WriteParameterOrReferenceList(const aKey: string; aObj: TParameterOrReferenceList);

begin
  WriteObjectArray(aKey,aObj,@WriteParameterOrReferenceUntyped);
end;

procedure TOpenAPIWriter.WriteParameterOrReferenceUntyped(aObj: TBaseOpenAPIObject);

begin
  WriteParameterOrReference(aObj as TParameterOrReference);
end;

procedure TOpenAPIWriter.WriteParameterOrReferenceMap(const aKey: String; aObj: TParameterOrReferenceMap);

begin
  WriteMapObject(aKey,aObj,@WriteParameterOrReferenceUnTyped);
end;


procedure TOpenAPIWriter.WriteParameterStyle(aObj: TParameterStyle);

var
  lName : String;
  lKeyword : TParameterStyleKeyword;

begin
  for lKeyword in TParameterStyleKeyword do
    if aObj.HasKeyWord(lKeyword) then
      begin
      lName:=lKeyWord.AsString;
      case lKeyword of
      pskMatrix:
        WriteProperty(lName,aObj.Matrix);
      pskLabel:
        WriteProperty(lName,aObj.Label_);
      pskForm:
        WriteProperty(lName,aObj.Form);
      pskSimple:
        WriteProperty(lName,aObj.Simple);
      pskSpaceDelimited:
        WriteProperty(lName,aObj.SpaceDelimited);
      pskPipeDelimited:
        WriteProperty(lName,aObj.PipeDelimited);
      pskDeepObject:
        WriteProperty(lName,aObj.DeepObject);
      end;
      end;
  if aObj.HasExtensions then
    WriteExtensions(aObj.Extensions);
end;

// Request body

procedure TOpenAPIWriter.WriteRequestBody(const aKey: string; aObj: TRequestBody);

begin
  StartObjectProp(aKey);
  WriteRequestBody(aObj);
  EndObjectProp;
end;

procedure TOpenAPIWriter.WriteRequestBody(aObj: TRequestBody);

var
  lName : String;
  lKeyword : TRequestBodyKeyword;

begin
  for lKeyword in TRequestBodyKeyword do
    if aObj.HasKeyWord(lKeyword) then
      begin
      lName:=lKeyWord.AsString;
      case lKeyword of
      rbkDescription:
        WriteProperty(lName,aObj.Description);
      rbkContent:
        WriteMediaTypeMap(lName,aObj.Content);
      rbkRequired:
        WriteProperty(lName,aObj.Required);
      end;
      end;
  if aObj.HasExtensions then
    WriteExtensions(aObj.Extensions);
end;


procedure TOpenAPIWriter.WriteRequestBodyOrReference(aObj: TRequestBodyOrReference);

begin
  if aObj.HasReference then
    WriteReference(aObj.Reference)
  else
    WriteRequestBody(aObj);
end;

procedure TOpenAPIWriter.WriteRequestBodyOrReferenceUnTyped(aObj: TBaseOpenAPIObject);

begin
  WriteRequestBodyOrReference(aObj as TRequestBodyOrReference);
end;

procedure TOpenAPIWriter.WriteRequestBodyOrReferenceMap(const aKey: string; aObj: TRequestBodyOrReferenceMap);

begin
  WriteMapObject(aKey,aObj,@WriteRequestBodyOrReferenceUnTyped);
end;

// Media type

procedure TOpenAPIWriter.WriteMediaType(aObj: TMediaType);

var
  lName : String;
  lKeyword : TMediaTypeKeyword;

begin
  for lKeyword in TMediaTypeKeyword do
    if aObj.HasKeyWord(lKeyword) then
      begin
      lName:=lKeyWord.AsString;
      case lKeyword of
      mtkSchema:
        WriteProperty(lName,aObj.Schema);
      mtkExample:
        WriteProperty(lName,aObj.Example);
      mtkExamples:
        WriteExampleOrReferenceMap(lName,aObj.Examples);
      mtkEncoding:
        WriteEncodingMap(lName,aObj.Encoding);
      end;
      end;
  if aObj.HasExtensions then
    WriteExtensions(aObj.Extensions);
end;

procedure TOpenAPIWriter.WriteMediaTypeUnTyped(aObj: TBaseOpenAPIObject);
begin
  WriteMediaType(aObj as TMediaType);
end;

procedure TOpenAPIWriter.WriteMediaTypeMap(const aKey : String; aObj: TMediaTypeMap);

begin
  WriteMapObject(aKey,aObj,@WriteMediaTypeUntyped);
end;

// Encoding

procedure TOpenAPIWriter.WriteEncoding(aObj: TEncoding);

var
  lName : String;
  lKeyword : TEncodingKeyword;
begin
  for lKeyword in TEncodingKeyword do
    if aObj.HasKeyWord(lKeyword) then
      begin
      lName:=lKeyWord.AsString;
      case lKeyword of
      eckContentType:
        WriteProperty(lName,aObj.ContentType);
      eckHeaders:
        WriteHeaderOrReferenceMap(lName,aObj.Headers);
      eckStyle:
        WriteProperty(lName,aObj.Style);
      eckExplode:
        WriteProperty(lName,aObj.Explode);
      eckAllowReserved:
        WriteProperty(lName,aObj.AllowReserved);
      end;
      end;
  if aObj.HasExtensions then
    WriteExtensions(aObj.Extensions);
end;

procedure TOpenAPIWriter.WriteEncodingUntyped(aObj: TBaseOpenAPIObject);
begin
  WriteEncoding(aObj as TEncoding);
end;

procedure TOpenAPIWriter.WriteEncodingMap(const aKey : string; aObj: TEncodingMap);

begin
  WriteMapObject(aKey,aObj,@WriteEncodingUntyped);
end;

// Responses

procedure TOpenAPIWriter.WriteResponses(const aKey: String; aObj: TResponses);

begin
  WriteMapObject(aKey, aObj, @WriteResponseUntyped)
end;

procedure TOpenAPIWriter.WriteResponseUntyped(aObj: TBaseOpenAPIObject);

begin
  WriteResponse(aObj as TResponse);
end;

procedure TOpenAPIWriter.WriteResponse(aObj: TResponse);

var
  lName : String;
  lKeyword : TResponseKeyword;
begin
  for lKeyword in TResponseKeyword do
    if aObj.HasKeyWord(lKeyword) then
      begin
      lName:=lKeyWord.AsString;
      case lKeyword of
      rkDescription:
        WriteProperty(lName,aObj.Description);
      rkHeaders:
        WriteHeaderOrReferenceMap(lName,aObj.Headers);
      rkContent:
        WriteMediaTypeMap(lName,aObj.Content);
      rkLinks:
        WriteLinkOrReferenceMap(lName,aObj.Links);
      end;
      end;
  if aObj.HasExtensions then
    WriteExtensions(aObj.Extensions);
end;


procedure TOpenAPIWriter.WriteResponseOrReference(aObj: TResponseOrReference);

begin
  if aObj.HasReference then
    WriteReference(aObj.Reference)
  else
    WriteResponse(aObj);
end;

procedure TOpenAPIWriter.WriteResponseOrReferenceUnTyped(aObj: TBaseOpenAPIObject);
begin
  WriteResponseOrReference(aObj as TResponseOrReference);
end;

procedure TOpenAPIWriter.WriteResponseOrReferenceMap(const aKey: String; aObj: TResponseOrReferenceMap);

begin
  WriteMapObject(aKey,aObj,@WriteResponseOrReferenceUnTyped);
end;

// Example

procedure TOpenAPIWriter.WriteExample(aObj: TExample);

var
  lName : String;
  lKeyword : TExampleKeyword;
begin
  for lKeyword in TExampleKeyword do
    if aObj.HasKeyWord(lKeyword) then
      begin
      lName:=lKeyWord.AsString;
      case lKeyword of
      exkSummary:
        WriteProperty(lName,aObj.Summary);
      exkDescription:
        WriteProperty(lName,aObj.Description);
      exkValue:
        WriteProperty(lName,aObj.Value);
      exkExternalValue:
        WriteProperty(lName,aObj.ExternalValue);
      end;
      end;
  if aObj.HasExtensions then
    WriteExtensions(aObj.Extensions);
end;


procedure TOpenAPIWriter.WriteExampleOrReference(aObj: TExampleOrReference);

begin
  if aObj.HasReference then
    WriteReference(aObj.Reference)
  else
    WriteExample(aObj);
end;

procedure TOpenAPIWriter.WriteExampleOrReferenceUnTyped(aObj: TBaseOpenAPIObject);

begin
  WriteExampleOrReference(aObj as TExampleOrReference);
end;

procedure TOpenAPIWriter.WriteExampleOrReferenceMap(const aKey: String; aObj: TExampleOrReferenceMap);

begin
  WriteMapObject(aKey,aObj,@WriteExampleOrReferenceUnTyped);
end;

// Link

procedure TOpenAPIWriter.WriteLink(aObj: TLink);

var
  lName : String;
  lKeyword : TLinkKeyword;
begin
  for lKeyword in TLinkKeyword do
    if aObj.HasKeyWord(lKeyword) then
      begin
      lName:=lKeyWord.AsString;
      case lKeyword of
      likOperationRef:
        WriteProperty(lName,aObj.OperationRef);
      likOperationId:
        WriteProperty(lName,aObj.OperationId);
      likParameters:
        WriteProperty(lName,aObj.Parameters);
      likRequestBody:
        WriteProperty(lName,aObj.RequestBody);
      likDescription:
        WriteProperty(lName,aObj.Description);
      likServer:
        WriteServer(lName,aObj.Server);
      end;
      end;
  if aObj.HasExtensions then
    WriteExtensions(aObj.Extensions);
end;


procedure TOpenAPIWriter.WriteLinkOrReference(aObj: TLinkOrReference);

begin
  if aObj.HasReference then
    WriteReference(aObj.Reference)
  else
    WriteLink(aObj);
end;

procedure TOpenAPIWriter.WriteLinkOrReferenceUntyped(aObj: TBaseOpenAPIObject);

begin
  WriteLinkOrReference(aObj as TLinkOrReference)
end;

procedure TOpenAPIWriter.WriteLinkOrReferenceMap(const aKey: string; aObj: TLinkOrReferenceMap);

begin
  WriteMapObject(aKey,aObj,@WriteLinkOrReferenceUntyped);
end;

// Reference

procedure TOpenAPIWriter.WriteReference(aObj: TReference);

var
  lName : String;
  lKeyword : TReferenceKeyword;
begin
  for lKeyword in TReferenceKeyword do
    if aObj.HasKeyWord(lKeyword) then
      begin
      lName:=lKeyWord.AsString;
      case lKeyword of
      rfkRef:
        WriteProperty(lName,aObj.Ref);
      rfkSummary:
        WriteProperty(lName,aObj.Summary);
      rfkDescription:
        WriteProperty(lName,aObj.Description);
      end;
      end;
  if aObj.HasExtensions then
    WriteExtensions(aObj.Extensions);
end;

// Schema

procedure TOpenAPIWriter.WriteSchema(aObj: TSchema);

var
  lName : String;
  lKeyword : TSchemaKeyword;

begin
  for lKeyword in TSchemaKeyword do
    if aObj.HasKeyWord(lKeyword) then
      begin
      lName:=lKeyWord.AsString;
      case lKeyword of
      sckDiscriminator:
        WriteDiscriminator(lName,aObj.Discriminator);
      sckXML:
        WriteXML(aObj.XML);
      sckExternalDocs:
        WriteExternalDocumentation(lName,aObj.ExternalDocs);
      sckExample:
        WriteProperty(lName,aObj.Example);
      end;
      end;
  if aObj.HasExtensions then
    WriteExtensions(aObj.Extensions);
end;

// Discriminator

procedure TOpenAPIWriter.WriteDiscriminator(const aKey : string; aObj: TDiscriminator);

begin
  StartObjectProp(aKey);
  WriteDiscriminator(aObj);
  EndObjectProp;
end;

procedure TOpenAPIWriter.WriteDiscriminator(aObj: TDiscriminator);
var
  lName : String;
  lKeyword : TDiscriminatorKeyword;
begin
  for lKeyword in TDiscriminatorKeyword do
    if aObj.HasKeyWord(lKeyword) then
      begin
      lName:=lKeyWord.AsString;
      case lKeyword of
      dikPropertyName:
        WriteProperty(lName,aObj.PropertyName);
      dikMapping:
        WriteStringMap(lName,aObj.Mapping);
      end;
      end;
  if aObj.HasExtensions then
    WriteExtensions(aObj.Extensions);
end;

// XML

procedure TOpenAPIWriter.WriteXML(aObj: TXML);

var
  lName : String;
  lKeyword : TXMLKeyword;
begin
  for lKeyword in TXMLKeyword do
    if aObj.HasKeyWord(lKeyword) then
      begin
      lName:=lKeyWord.AsString;
      case lKeyword of
      xmkName:
        WriteProperty(lName,aObj.Name);
      xmkNamespace:
        WriteProperty(lName,aObj.Namespace);
      xmkPrefix:
        WriteProperty(lName,aObj.Prefix);
      xmkAttribute:
        WriteProperty(lName,aObj.Attribute);
      xmkWrapped:
        WriteProperty(lName,aObj.Wrapped);
      end;
      end;
  if aObj.HasExtensions then
    WriteExtensions(aObj.Extensions);
end;

// Security scheme

procedure TOpenAPIWriter.WriteSecurityScheme(aObj: TSecurityScheme);

var
  lName : String;
  lKeyword : TSecuritySchemeKeyword;
begin
  for lKeyword in TSecuritySchemeKeyword do
    if aObj.HasKeyWord(lKeyword) then
      begin
      lName:=lKeyWord.AsString;
      case lKeyword of
      sskType:
        WriteProperty(lName,aObj.Type_);
      sskDescription:
        WriteProperty(lName,aObj.Description);
      sskName:
        WriteProperty(lName,aObj.Name);
      sskIn:
        WriteProperty(lName,aObj.In_);
      sskScheme:
        WriteProperty(lName,aObj.Scheme);
      sskBearerFormat:
        WriteProperty(lName,aObj.BearerFormat);
      sskFlows:
        WriteOAuthFlows(lName, aObj.Flows);
      sskOpenIdConnectUrl:
        WriteProperty(lName,aObj.OpenIdConnectUrl);
      end;
      end;
  if aObj.HasExtensions then
    WriteExtensions(aObj.Extensions);
end;


procedure TOpenAPIWriter.WriteSecuritySchemeOrReference(aObj: TSecuritySchemeOrReference);

begin
  if aObj.HasReference then
    WriteReference(aObj.Reference)
  else
    WriteSecurityScheme(aObj);
end;

procedure TOpenAPIWriter.WriteSecuritySchemeOrReferenceUnTyped(aObj: TBaseOpenAPIObject);

begin
  WriteSecuritySchemeOrReference(aObj as TSecuritySchemeOrReference);
end;

procedure TOpenAPIWriter.WriteSecuritySchemeOrReferenceMap(const aKey: string; aObj: TSecuritySchemeOrReferenceMap);

begin
  WriteMapObject(aKey,aObj,@WriteSecuritySchemeOrReferenceUntyped);
end;

// OAuth flows

procedure TOpenAPIWriter.WriteOAuthFlows(aObj: TOAuthFlows);

var
  lName : String;
  lKeyword : TOAuthFlowsKeyword;
begin
  for lKeyword in TOAuthFlowsKeyword do
    if aObj.HasKeyWord(lKeyword) then
      begin
      lName:=lKeyWord.AsString;
    case lKeyword of
    ofskImplicit:
      WriteOauthFlow(lName,aObj.Implicit);
    ofskPassword:
      WriteOauthFlow(lName,aObj.Password);
    ofskClientCredentials:
      WriteOauthFlow(lName,aObj.ClientCredentials);
    ofskClientAuthorizationCode:
      WriteOauthFlow(lName,aObj.ClientAuthorizationCode);
    end;
    end;
  if aObj.HasExtensions then
    WriteExtensions(aObj.Extensions);
end;

procedure TOpenAPIWriter.WriteOAuthFlows(const aKey: String; aObj: TOAuthFlows);
begin
  StartObjectProp(aKey);
  WriteOAuthFlows(aObj);
  EndObjectProp;
end;

// OAuth flow

procedure TOpenAPIWriter.WriteOAuthFlow(aObj: TOauthFlow);
var
  lName : String;
  lKeyword : TOauthFlowKeyword;
begin
  for lKeyword in TOauthFlowKeyword do
    if aObj.HasKeyWord(lKeyword) then
      begin
      lName:=lKeyWord.AsString;
    case lKeyword of
    ofkAuthorizationUrl:
      WriteProperty(lName,aObj.AuthorizationUrl);
    ofkTokenURL:
      WriteProperty(lName,aObj.TokenURL);
    ofkRefreshURL:
      WriteProperty(lName,aObj.RefreshURL);
    ofkScopes:
      WriteStringMap(lName,aObj.Scopes);
    end;
    end;
  if aObj.HasExtensions then
    WriteExtensions(aObj.Extensions);
end;

procedure TOpenAPIWriter.WriteOAuthFlow(const aKey: String; aObj: TOauthFlow);
begin
  StartObjectProp(aKey);
  WriteOauthFlow(aObj);
  EndObjectProp;
end;

// Header

procedure TOpenAPIWriter.WriteHeader(aObj: THeader);

begin
  WriteParameterOrHeader(aObj);
end;


procedure TOpenAPIWriter.WriteHeaderOrReference(aObj: THeaderOrReference);

begin
  if aObj.HasReference then
    WriteReference(aObj.Reference)
  else
    WriteHeader(aObj)
end;


procedure TOpenAPIWriter.WriteHeaderOrReferenceUnTyped(aObj: TBaseOpenAPIObject);

begin
  WriteHeaderOrReference(aObj as THeaderOrReference)
end;

procedure TOpenAPIWriter.WriteHeaderOrReferenceMap(const aKey: string; aObj: THeaderOrReferenceMap);

begin
  WriteMapObject(aKey,aObj,@WriteHeaderOrReferenceUnTyped);
end;

// Callback

procedure TOpenAPIWriter.WriteCallBack(aObj: TCallBack);

begin
  WriteMapObject(aObj,@WritePathItemOrReferenceUnTyped);
end;

procedure TOpenAPIWriter.WriteCallbackOrReference(aObj: TCallbackOrReference);

begin
  if aObj.HasReference then
    WriteReference(aObj.Reference)
  else
    WriteCallBack(aObj);
end;

procedure TOpenAPIWriter.WriteCallbackOrReferenceUnTyped(aObj: TBaseOpenAPIObject);
begin
  WriteCallbackOrReference(aObj as TCallbackOrReference);
end;

procedure TOpenAPIWriter.WriteCallbackOrReferenceMap(const aKey: string; aObj: TCallbackOrReferenceMap);

begin
  WriteMapObject(aKey,aObj,@WriteCallbackOrReferenceUnTyped);
end;

// Security requirement

procedure TOpenAPIWriter.WriteSecurityRequirement(aObj: TSecurityRequirement);

var
  i: Integer;
  lName : String;

begin
  for I:=0 to aObj.Count-1 do
    begin
    lName:=aObj.Names[i];
    WriteStrings(lName,aObj.Lists[i]);
    end;
end;

procedure TOpenAPIWriter.WriteSecurityRequirement(const aKey: String; aObj: TSecurityRequirement);

begin
  StartObjectProp(aKey);
  WriteSecurityRequirement(aObj);
  EndObjectProp;
end;

procedure TOpenAPIWriter.WriteSecurityRequirementUnTyped(aObj: TBaseOpenAPIObject);

begin
  WriteSecurityRequirement(aObj as TSecurityRequirement);
end;

procedure TOpenAPIWriter.WriteSecurityRequirementList(const aKey: String; aObj: TSecurityRequirementList);

begin
  WriteObjectArray(aKey,aObj,@WriteSecurityRequirementUntyped);
end;

// Tag

procedure TOpenAPIWriter.WriteTag(aObj: TTag);

var
  lName : String;
  lKeyword : TTagKeyword;

begin
  for lKeyword in TTagKeyword do
    if aObj.HasKeyWord(lKeyword) then
      begin
      lName:=lKeyWord.AsString;
      case lKeyword of
      tkName:
        WriteProperty(lName,aObj.Name);
      tkDescription:
        WriteProperty(lName,aObj.Description);
      tkExternalDocs:
        WriteExternalDocumentation(lName,aObj.ExternalDocs);
      end;
      end;
  if aObj.HasExtensions then
    WriteExtensions(aObj.Extensions);
end;

procedure TOpenAPIWriter.WriteTagUnTyped(aObj: TBaseOpenAPIObject);

begin
  WriteTag(aObj as TTag);
end;

procedure TOpenAPIWriter.WriteTagList(const aKey : string; aObj: TTagList);
begin
  WriteObjectArray(aKey,aObj,@WriteTagUnTyped);
end;

end.
