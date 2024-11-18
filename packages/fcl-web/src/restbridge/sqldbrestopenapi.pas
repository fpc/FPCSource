unit sqldbrestopenapi;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, httpdefs, sqldbrestbridge, sqldbrestschema, fpopenapi.objects, fpjson.schema.types,
  fpjson, fpjson.schema.schema, jsonwriter, fpopenapi.writer, sqldbrestio;

Const
  DefaultContentType = 'application/json';

type

  { TSLQDBRestSchemaToOpenAPI }

  TSLQDBRestSchemaToOpenAPI = class(TComponent)
  private
    FBasePath: String;
    FContentType: String;
    FListPrefix: String;
    FListSuffix: String;
    FOperationIDDeletePrefix: String;
    FOperationIDGetPrefix: String;
    FOperationIDListPrefix: String;
    FOperationIDPatchPrefix: String;
    FOperationIDPostPrefix: String;
    FOperationIDPutPrefix: String;
    procedure ConvertResourceToPathItemID(aResource: TSQLDBRestResource; aOpenAPI: TOpenAPI);
    function GetComponentName(aResource: TSQLDBRestResource; aList: Boolean): string;
    function HasKeyField(aResource: TSQLDBRestResource): Boolean;
    procedure SetRequestBody(aAPIOperation: TAPIOperation; aResource: TSQLDBRestResource);
  Protected
    procedure ConvertResourceToListSchema(aResource: TSQLDBRestResource; aSchema: TJSONSchema);
    procedure ConvertResourceToPathItem(aResource: TSQLDBRestResource; aOpenAPI: TOpenAPI);
    procedure SetResponse(aAPIOperation: TAPIoperation; aOperationPrefix: String; aResource: TSQLDBRestResource; aList: Boolean);
    procedure ConvertFieldToProperty(aField: TSQLDBRestField; aSchema: TJSONSchema); virtual;
    function ConvertFieldTypeToSimpleType(aType: TRestFieldType): TSchemaSimpleType; virtual;
    procedure ConvertResourceToComponents(aResource: TSQLDBRestResource; aOpenAPI: TOpenAPI); virtual;
    procedure ConvertResourceToSchema(aResource: TSQLDBRestResource; aSchema: TJSONSchema); virtual;
    function FieldTypeHasStringFormat(aType: TRestFieldType; out aFormat: TStringFormatValidator): Boolean; virtual;
  Public
    Procedure InitDefaults;
  Public
    Constructor Create(aOwner : TComponent); override;
    Procedure Convert(aSchema : TSQLDBRestSchema; aOpenAPI : TOpenAPI);
    Property ListPrefix : String Read FListPrefix Write FListPrefix;
    Property ListSuffix : String Read FListSuffix Write FListSuffix;
    Property BasePath : String Read FBasePath Write FBasePath;
    Property ContentType : String Read FContentType Write FContentType;
    Property OperationIDGetPrefix : String Read FOperationIDGetPrefix Write FOperationIDGetPrefix;
    Property OperationIDListPrefix : String Read FOperationIDListPrefix Write FOperationIDListPrefix;
    Property OperationIDPostPrefix : String Read FOperationIDPostPrefix Write FOperationIDPostPrefix;
    Property OperationIDPutPrefix : String Read FOperationIDPutPrefix Write FOperationIDPutPrefix;
    Property OperationIDPatchPrefix : String Read FOperationIDPatchPrefix Write FOperationIDPatchPrefix;
    Property OperationIDDeletePrefix : String Read FOperationIDDeletePrefix Write FOperationIDDeletePrefix;
  end;



implementation

{ TSLQDBRestSchemaToOpenAPI }


constructor TSLQDBRestSchemaToOpenAPI.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  InitDefaults;
end;


procedure TSLQDBRestSchemaToOpenAPI.InitDefaults;
begin
  ContentType:=DefaultContentType;
  ListSuffix:='List';
  BasePath:='/REST/';
  OperationIDGetPrefix:='Get';
  OperationIDListPrefix:='List';
  OperationIDPostPrefix:='Create';
  OperationIDPutPrefix:='Replace';
  OperationIDPatchPrefix:='Update';
  OperationIDDeletePrefix:='Delete';
end;

function TSLQDBRestSchemaToOpenAPI.FieldTypeHasStringFormat(aType : TRestFieldType; out aFormat: TStringFormatValidator) : Boolean;

begin
  Result:=True;
  case aType of
    rftDate      : aFormat:=sfvDate;
    rftTime      : aFormat:=sfvTime;
    rftDateTime  : aFormat:=sfvDatetime;
    rftBlob      : aFormat:=sfvCustom;
  else
    Result:=False
  end;
end;

function TSLQDBRestSchemaToOpenAPI.ConvertFieldTypeToSimpleType(aType : TRestFieldType) : TSchemaSimpleType;

begin
  case aType of
    rftUnknown : Result:=sstAny;
    rftInteger : Result:=sstInteger;
    rftLargeInt : Result:=sstInteger;
    rftFloat : Result:=sstNumber;
    rftDate : Result:=sstString;
    rftTime  : Result:=sstString;
    rftDateTime  : Result:=sstString;
    rftString  : Result:=sstString;
    rftBoolean : Result:=sstBoolean;
    rftBlob: Result:=sstString;
  end;
end;

function TSLQDBRestSchemaToOpenAPI.HasKeyField(aResource : TSQLDBRestResource) : Boolean;

var
  I : Integer;

begin
  Result:=False;
  I:=0;
  While not Result and (I<aResource.Fields.Count) do
    begin
    Result:=foInKey in aResource.Fields[0].Options;
    Inc(I);
    end;
end;

function TSLQDBRestSchemaToOpenAPI.GetComponentName(aResource : TSQLDBRestResource; aList : Boolean) : string;

begin
  Result:=aResource.ResourceName;
  if aList then
    Result:=ListPrefix+Result+ListSuffix;
end;


procedure TSLQDBRestSchemaToOpenAPI.ConvertFieldToProperty(aField : TSQLDBRestField; aSchema : TJSONSchema);

var
  sst : TSchemaSimpleType;
  fvt : TStringFormatValidator;
  isActualString : Boolean;


begin
  sst:=ConvertFieldTypeToSimpleType(aField.FieldType);
  aSchema.Validations.Types:=[sst];
  if (sst=sstString) then
    begin
    isActualString:=not FieldTypeHasStringFormat(aField.FieldType,fvt);
    if not IsActualString then
      aSchema.Validations.FormatValidator:=fvt
    else
      begin
      if aField.MaxLen>0 then
        aSchema.Validations.MaxLength:=aField.MaxLen;
      end;
    end;
end;

procedure TSLQDBRestSchemaToOpenAPI.ConvertResourceToSchema(aResource: TSQLDBRestResource; aSchema : TJSONSchema);

var
  I : Integer;
  lField : TSQLDBRestField;
  lFieldSchema: TJSONSchema;

begin
  aSchema.Validations.Types:=[sstObject];
  For I:=0 to aResource.Fields.Count-1 do
    begin
    lField:=aResource.Fields[I];
    lFieldSchema:=aSchema.Properties.Add(lField.PublicName);
    ConvertFieldToProperty(lField,lFieldSchema);
    end;
end;

procedure TSLQDBRestSchemaToOpenAPI.ConvertResourceToListSchema(aResource: TSQLDBRestResource; aSchema : TJSONSchema);

var
  lSchema: TJSONSChema;

begin
  aSchema.Validations.Types:=[sstArray];
  lSchema:=TJSONSChema.Create(aSchema);
  lSchema.Ref:='#/components/schemas/'+GetComponentName(aResource,False);
  aSchema.Items.Add(lSchema);
end;


procedure TSLQDBRestSchemaToOpenAPI.ConvertResourceToComponents(aResource: TSQLDBRestResource; aOpenAPI : TOpenAPI);

var
  lSchema : TJSONSchema;

begin
  lSchema:=aOpenAPI.Components.Schemas.Add(GetComponentName(aResource,False));
  ConvertResourceToSchema(aResource,lSchema);
  if roGet in aResource.AllowedOperations then
    begin
    lSchema:=aOpenAPI.Components.Schemas.Add(GetComponentName(aResource,True));
    ConvertResourceToListSchema(aResource,lSchema);
    end;
end;

procedure TSLQDBRestSchemaToOpenAPI.SetResponse(aAPIOperation : TAPIoperation; aOperationPrefix : String; aResource : TSQLDBRestResource; aList : Boolean);

var
  lResponse : TResponse;
  lMedia : TMediaType;

begin
  aAPIOperation.OperationId:=aOperationPrefix+aResource.ResourceName;
  lResponse:=aAPIOperation.Responses.AddItem('default');
  lMedia:=lResponse.Content.AddItem(ContentType);
  lMedia.Schema.Ref:='#/components/schemas/'+GetComponentName(aResource,aList);
end;

procedure TSLQDBRestSchemaToOpenAPI.SetRequestBody(aAPIOperation : TAPIOperation; aResource :TSQLDBRestResource);

var
  lMedia : TMediaType;

begin
  lMedia:=aAPIOperation.RequestBody.Content.AddItem(ContentType);
  lMedia.Schema.Ref:='#/components/schemas/'+GetComponentName(aResource,False);
end;

procedure TSLQDBRestSchemaToOpenAPI.ConvertResourceToPathItem(aResource: TSQLDBRestResource; aOpenAPI : TOpenAPI);

var
  aPathItem : TPathItem;

begin
  aPathItem:=aOpenAPI.Paths.AddItem(aResource.ResourceName);
  if roGet in aResource.AllowedOperations then
    SetResponse(aPathItem.Get,OperationIDListPrefix,aResource,True);
  if roPost in aResource.AllowedOperations then
    begin
    SetResponse(aPathItem.Post,OperationIDPostPrefix,aResource,False);
    SetRequestBody(aPathItem.Post,aResource);
    end;
end;

procedure TSLQDBRestSchemaToOpenAPI.ConvertResourceToPathItemID(aResource: TSQLDBRestResource; aOpenAPI : TOpenAPI);

var
  aPathItem : TPathItem;

begin
  if ([roGet,roPut,roPatch,roDelete] * aResource.AllowedOperations) = [] then
    exit;
  aPathItem:=aOpenAPI.Paths.AddItem(aResource.ResourceName+'/{ID}');
  if roGet in aResource.AllowedOperations then
    SetResponse(aPathItem.Get,OperationIDGetPrefix,aResource,False);
  if roPut in aResource.AllowedOperations then
    begin
    SetResponse(aPathItem.Put,OperationIDPutPrefix,aResource,False);
    SetRequestBody(aPathItem.Put,aResource);
    end;
  if roPatch in aResource.AllowedOperations then
    begin
    SetResponse(aPathItem.Patch,OperationIDPatchPrefix,aResource,False);
    SetRequestBody(aPathItem.Patch,aResource);
    end;
  if roDelete in aResource.AllowedOperations then
    SetResponse(aPathItem.Delete,OperationIDDeletePrefix,aResource,False);
//  if not aResource.AllowedOperations then
end;


procedure TSLQDBRestSchemaToOpenAPI.Convert(aSchema: TSQLDBRestSchema; aOpenAPI: TOpenAPI);

var
  I : Integer;
  lResource : TSQLDBRestResource;

begin
  For I:=0 to aSchema.Resources.Count-1 do
    begin
    lResource:=aSchema.Resources[i];
    ConvertResourceToComponents(lResource,aOpenAPI);
    ConvertResourceToPathItem(lResource,aOpenAPI);
    if HasKeyField(lResource) then
      ConvertResourceToPathItemID(lResource,aOpenAPI);
    end;
end;

procedure HandleOpenAPIRoute(aDispatcher : TSQLDBRestDispatcher; aRequest : TRequest; aResponse : httpdefs.TResponse);

var
  Converter : TSLQDBRestSchemaToOpenAPI;
  OpenAPI : TOpenAPI;
  Writer : TOpenAPIWriter;
  J : TJSONDataWriter;
  D : TJSONData;
  Schema : TSQLDBRestSchema;
  I : Integer;
  S : TJSONStringType;

begin
  J:=Nil;
  D:=NIl;
  OpenAPI:=Nil;
  Writer:=Nil;
  Converter:=TSLQDBRestSchemaToOpenAPI.Create(Nil);
  try
    OpenAPI:=TOpenAPI.Create;
    OpenAPI.OpenApi:='3.1.1';
    OpenAPI.Info.Title:='SQLDBRest interface '+aDispatcher.Name;
    OpenAPI.Info.Version:='1';
    For I:=0 to aDispatcher.Schemas.Count-1 do
      begin
      Schema:=aDispatcher.Schemas[i].Schema;
      Converter.Convert(Schema,OpenAPI);
      end;
    Writer:=TOpenAPIWriter.Create(Nil);
    J:=TJSONDataWriter.Create;
    Writer.Write(OpenAPI,J);
    D:=J.ExtractData;
    S:=aRequest.QueryFields.Values[aDispatcher.Strings.HumanReadableParam];
    if TRestIO.StrToNullBoolean(S,false)=nbTrue then
      S:=D.FormatJSON
    else
      S:=D.AsJSON;
    aResponse.Content:=S;
    aResponse.ContentType:='application/json';
  finally
    D.Free;
    J.Free;
    Writer.Free;
    Converter.Free;
    OpenAPI.Free;
  end;
end;

initialization
  TSQLDBRestDispatcher.SetOpenAPIRequestHandler(@HandleOpenAPIRoute);
end.

