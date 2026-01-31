unit utOpenApiReader;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, fpjson, testregistry, fpopenapi.objects, fpopenapi.reader;

Type

  { TTestOpenApiReader }

  TTestOpenApiReader = class(TTestCase)
  private
    FOpenAPI: TOpenAPi;
    FReader: TOpenAPIReader;
  Public
    Procedure Setup; override;
    Procedure TearDown; override;
    procedure LoadJSON(aJSON: TJSONStringType);
    procedure LoadJSONFile(aJSONFileName: TJSONStringType);
    Procedure TestRead(const aJSON : String);
    Property Reader : TOpenAPIReader Read FReader;
    Property OpenAPI : TOpenAPi Read FOpenAPI;
  Published
    Procedure TestHookup;
  end;

  TTestOpenApiReaderLarge = class (TTestOpenApiReader)
  Published
    procedure TestTitle;
    Procedure TestLarge;
  end;

//  TTestOpenAPIReader =

  TTestOpenApiReaderOpenAPI = Class(TTestOpenAPIReader)
    Procedure TestOpenAPI;
    Procedure TestJSONSchemaDialect;
  end;

  TTestOpenApiReaderInfo = Class(TTestOpenAPIReader)
    Procedure TestVersion;
    Procedure TestTitle;
    Procedure TestSummary;
    Procedure TestDescription;
    Procedure TestTermsOfService;
    Procedure TestContactEmail;
    Procedure TestContactName;
    Procedure TestContactURL;
    Procedure TestLicenseUrl;
    Procedure TestLicenseIdentifier;
    Procedure TestLicenseName;
  end;

  TTestOpenApiReaderTags = Class(TTestOpenAPIReader)
    Procedure TestName;
    Procedure TestDescription;
    Procedure TestExternalDocs;
    Procedure Test2Tags;
  end;

  TTestOpenApiReaderExternalDocs = Class(TTestOpenAPIReader)
    Procedure TestUrl;
    Procedure TestDescription;
  end;

  TTestOpenApiReaderServers = Class(TTestOpenAPIReader)
    Procedure TestUrl;
    Procedure TestDescription;
    Procedure TestVariablesDefault;
    Procedure TestVariablesDescription;
    Procedure TestVariablesEnum;
    Procedure Test2Url;
  end;

  TTestOpenApiReaderPaths = Class(TTestOpenAPIReader)
    Procedure TestEmpty;
    Procedure TestRef;
    Procedure TestSummary;
    Procedure TestDescription;
    Procedure TestGetEmpty;
    Procedure TestGetSummary;
    Procedure TestGetDescription;
    Procedure TestGetOperationId;
    Procedure TestGetDeprecated;
    Procedure TestGetExternalDocsEmpty;
    Procedure TestGetExternalDocsURL;
    Procedure TestGetServers;
  end;

  TTestOpenApiReaderPathRequestBody = Class(TTestOpenAPIReader)
    Procedure TestEmpty;
    Procedure TestDescription;
    Procedure TestRequired;
    Procedure TestContent;
    Procedure TestContentSchema;
    Procedure TestContentExample;
    Procedure TestContentExamples;
    Procedure TestContentEncodingContentType;
    Procedure TestContentEncodingExplode;
    Procedure TestContentEncodingStyle;
    Procedure TestContentEncodingAllowReserved;
    Procedure TestContentEncodingHeaders;
  end;

  TTestOpenApiReaderPathParameters = Class(TTestOpenAPIReader)
    Procedure TestGetParametersEmpty;
  end;

  TTestOpenApiReaderPathResponses = Class(TTestOpenAPIReader)
    Procedure TestEmpty;
    Procedure TestHeaders;
    Procedure TestDescription;
    Procedure TestContentEmpty;
    Procedure TestContentSchema;
    Procedure TestContentExample;
    Procedure TestContentExamples;
    Procedure TestContentEncodingEmpty;
    Procedure TestContentEncodingContentType;
  end;

  TTestOpenApiReaderPathSecurity = Class(TTestOpenAPIReader)
    Procedure TestEmpty;
    Procedure TestAPIKey;
  end;

  TTestOpenApiReaderPathResponsesLinks = Class(TTestOpenAPIReader)
    Procedure TestEmpty;
    Procedure TestOperatonRef;
    Procedure TestOperatonId;
    Procedure TestParameters;
    Procedure TestRequestBody;
    Procedure TestDescription;
    Procedure TestServer;
  end;

  TTestOpenApiReaderWebHooks = Class(TTestOpenAPIReader)
    Procedure TestEmpty;
    Procedure TestSummary;
  end;

  TTestOpenApiReaderComponents = Class(TTestOpenAPIReader)
    Procedure TestSchemaEmpty;
    Procedure TestSchemaContentEncoding;
  end;

  TTestOpenApiReaderOAuth2Scopes = Class(TTestOpenAPIReader)
    Procedure TestOAuth2ScopesAsObject;
    Procedure TestOAuth2ScopesEmpty;
  end;

  TTestOpenApiReaderSecurity = Class(TTestOpenAPIReader)
    Procedure TestOne;
  end;

implementation

{ TTestOpenApiReader }

procedure TTestOpenApiReader.Setup;
begin
  Inherited;
  FReader:=TOpenAPIReader.Create(Nil);
end;

procedure TTestOpenApiReader.TearDown;
begin
  FreeAndNil(FOpenAPI);
  FreeAndNil(FReader);
  Inherited;
end;

procedure TTestOpenApiReader.LoadJSON(aJSON: TJSONStringType);
begin
  FreeAndNil(FOpenAPI);
  FOpenAPI:=TOpenAPI.Create;
  Reader.ReadFromString(OpenAPI,aJSON);
end;

procedure TTestOpenApiReader.LoadJSONFile(aJSONFileName: TJSONStringType);

var
  F : TFileStream;
  aJSON : TJSONStringType;

begin
  aJSON:='';
  F:=TFileStream.Create(aJSONFileName,fmOpenRead or fmShareDenyWrite);
  try
    SetLength(aJSON,F.Size);
    F.ReadBuffer(aJSON[1],Length(aJSON));
    LoadJSON(aJSON);
  finally
    F.Free;
  end;
end;

procedure TTestOpenApiReader.TestRead(const aJSON: String);
begin
  LoadJSON(aJSON);
end;

procedure TTestOpenApiReader.TestHookup;
begin
  AssertNotNull('Have reader',FReader);
end;

procedure TTestOpenApiReaderLarge.TestTitle;

const
  JSON =
    '  {' + sLineBreak+
    '    "openapi": "3.0.0",' + sLineBreak+
    '    "info": {' + sLineBreak+
    '      "version": "1.0.0",' + sLineBreak+
    '      "title": "Sample API",' + sLineBreak+
    '      "description": "A sample API to illustrate OpenAPI concepts"' + sLineBreak+
    '    }' + sLineBreak+
    '  }';

begin
  LoadJSON(JSON);
  AssertEquals('OpenApi','3.0.0',OpenAPI.OpenApi);
  AssertEquals('OpenAPI.Info.Version','1.0.0',OpenAPI.Info.Version);
  AssertEquals('OpenAPI.Info.title','Sample API',OpenAPI.Info.Title);
  AssertEquals('OpenAPI.Info.description','A sample API to illustrate OpenAPI concepts',OpenAPI.Info.Description);
end;

procedure TTestOpenApiReaderLarge.TestLarge;
begin
  LoadJSONFile('openapi-all.json');
end;

procedure TTestOpenApiReaderOpenAPI.TestOpenAPI;

begin
  TestRead('{ "openapi" : "123" }');
  AssertEquals('OpenAPI.openapi','123',OpenAPI.openapi);
end;


procedure TTestOpenApiReaderOpenAPI.TestJSONSchemaDialect;

begin
  TestRead('{ "jsonSchemaDialect" : "123" }');
  AssertEquals('OpenAPI.jsonSchemaDialect','123',OpenAPI.jsonSchemaDialect);
end;


procedure TTestOpenApiReaderInfo.TestVersion;

begin
  TestRead('{ "info" : { "version" : "123" } }');
  AssertNotNull('OpenAPI.info',OpenAPI.info);
  AssertEquals('OpenAPI.info.version','123',OpenAPI.info.version);
end;


procedure TTestOpenApiReaderInfo.TestTitle;

begin
  TestRead('{ "info" : { "title" : "123" } }');
  AssertNotNull('OpenAPI.info',OpenAPI.info);
  AssertEquals('OpenAPI.info.title','123',OpenAPI.info.title);
end;


procedure TTestOpenApiReaderInfo.TestSummary;

begin
  TestRead('{ "info" : { "summary" : "123" } }');
  AssertNotNull('OpenAPI.info',OpenAPI.info);
  AssertEquals('OpenAPI.info.summary','123',OpenAPI.info.summary);
end;


procedure TTestOpenApiReaderInfo.TestDescription;

begin
  TestRead('{ "info" : { "description" : "123" } }');
  AssertNotNull('OpenAPI.info',OpenAPI.info);
  AssertEquals('OpenAPI.info.description','123',OpenAPI.info.description);
end;


procedure TTestOpenApiReaderInfo.TestTermsOfService;

begin
  TestRead('{ "info" : { "termsOfService" : "123" } }');
  AssertNotNull('OpenAPI.info',OpenAPI.info);
  AssertEquals('OpenAPI.info.termsOfService','123',OpenAPI.info.termsOfService);
end;


procedure TTestOpenApiReaderInfo.TestContactEmail;

begin
  TestRead('{ "info" : { "contact" : { "email" : "123" } } }');
  AssertNotNull('OpenAPI.info',OpenAPI.info);
  AssertNotNull('OpenAPI.info.contact',OpenAPI.info.contact);
  AssertEquals('OpenAPI.info.contact.email','123',OpenAPI.info.contact.email);
end;


procedure TTestOpenApiReaderInfo.TestContactName;

begin
  TestRead('{ "info" : { "contact" : { "name" : "123" } } }');
  AssertNotNull('OpenAPI.info',OpenAPI.info);
  AssertNotNull('OpenAPI.info.contact',OpenAPI.info.contact);
  AssertEquals('OpenAPI.info.contact.name','123',OpenAPI.info.contact.name);
end;


procedure TTestOpenApiReaderInfo.TestContactURL;

begin
  TestRead('{ "info" : { "contact" : { "url" : "123" } } }');
  AssertNotNull('OpenAPI.info',OpenAPI.info);
  AssertNotNull('OpenAPI.info.contact',OpenAPI.info.contact);
  AssertEquals('OpenAPI.info.contact.url','123',OpenAPI.info.contact.url);
end;


procedure TTestOpenApiReaderInfo.TestLicenseUrl;

begin
  TestRead('{ "info" : { "license" : { "url" : "123" } } }');
  AssertNotNull('OpenAPI.info',OpenAPI.info);
  AssertNotNull('OpenAPI.info.license',OpenAPI.info.license);
  AssertEquals('OpenAPI.info.license.url','123',OpenAPI.info.license.url);
end;


procedure TTestOpenApiReaderInfo.TestLicenseIdentifier;

begin
  TestRead('{ "info" : { "license" : { "identifier" : "123" } } }');
  AssertNotNull('OpenAPI.info',OpenAPI.info);
  AssertNotNull('OpenAPI.info.license',OpenAPI.info.license);
  AssertEquals('OpenAPI.info.license.identifier','123',OpenAPI.info.license.identifier);
end;


procedure TTestOpenApiReaderInfo.TestLicenseName;

begin
  TestRead('{ "info" : { "license" : { "name" : "123" } } }');
  AssertNotNull('OpenAPI.info',OpenAPI.info);
  AssertNotNull('OpenAPI.info.license',OpenAPI.info.license);
  AssertEquals('OpenAPI.info.license.name','123',OpenAPI.info.license.name);
end;


procedure TTestOpenApiReaderTags.TestName;

begin
  TestRead('{ "tags" : [ { "name" : "123" } ] }');
end;


procedure TTestOpenApiReaderTags.TestDescription;

begin
  TestRead('{ "tags" : [ { "description" : "123" } ] }');
end;


procedure TTestOpenApiReaderTags.TestExternalDocs;

begin
  TestRead('{ "tags" : [ { "externalDocs" : { "url" :"123" } } ] }');
end;


procedure TTestOpenApiReaderTags.Test2Tags;

begin
  TestRead('{ "tags" : [ { "name" : "123" },  { "name" : "456" } ] }');
end;


procedure TTestOpenApiReaderExternalDocs.TestUrl;

begin
  TestRead('{ "externalDocs" : { "url" :"123" } }');
  AssertNotNull('OpenAPI.externalDocs',OpenAPI.externalDocs);
  AssertEquals('OpenAPI.externalDocs.url','123',OpenAPI.externalDocs.url);
end;


procedure TTestOpenApiReaderExternalDocs.TestDescription;

begin
  TestRead('{ "externalDocs" : { "description" :"123" } }');
  AssertNotNull('OpenAPI.externalDocs',OpenAPI.externalDocs);
  AssertEquals('OpenAPI.externalDocs.description','123',OpenAPI.externalDocs.description);
end;


procedure TTestOpenApiReaderServers.TestUrl;

begin
  TestRead('{ "servers" : [ { "url" :"123" } ] }');
end;


procedure TTestOpenApiReaderServers.TestDescription;

begin
  TestRead('{ "servers" : [ { "description" :"123" } ] }');
end;


procedure TTestOpenApiReaderServers.TestVariablesDefault;

begin
  TestRead('{ "servers" : [ { "variables" : { "user": { "default": "123" } } } ] }');
end;


procedure TTestOpenApiReaderServers.TestVariablesDescription;

begin
  TestRead('{ "servers" : [ { "variables" : { "user": { "description": "123" } } } ] }');
end;


procedure TTestOpenApiReaderServers.TestVariablesEnum;

begin
  TestRead('{ "servers" : [ { "variables" : { "user": { "enum": [ "123", "456" ] } } } ] }');
end;


procedure TTestOpenApiReaderServers.Test2Url;

begin
  TestRead('{ "servers" : [ { "url" :"123" }, { "url" :"456" } ] }');
end;


procedure TTestOpenApiReaderPaths.TestEmpty;

begin
  TestRead('{ "paths" : { "api" : {} } }');
  AssertNotNull('OpenAPI.paths',OpenAPI.paths);
  AssertNotNull('OpenAPI.paths.api',OpenAPI.paths['api']);
end;


procedure TTestOpenApiReaderPaths.TestRef;

begin
  TestRead('{ "paths" : { "api" : { "$ref": "abc"} } }');
  AssertNotNull('OpenAPI.paths',OpenAPI.paths);
  AssertNotNull('OpenAPI.paths.api',OpenAPI.paths['api']);
  AssertEquals('OpenAPI.paths.api.$ref','abc',OpenAPI.Paths['api'].ref);
end;


procedure TTestOpenApiReaderPaths.TestSummary;

begin
  TestRead('{ "paths" : { "api" : { "summary": "abc"} } }');
  AssertNotNull('OpenAPI.paths',OpenAPI.paths);
  AssertNotNull('OpenAPI.paths.api',OpenAPI.Paths['api']);
  AssertEquals('OpenAPI.paths.api.summary','abc',OpenAPI.Paths['api'].summary);
end;


procedure TTestOpenApiReaderPaths.TestDescription;

begin
  TestRead('{ "paths" : { "api" : { "description": "abc"} } }');
  AssertNotNull('OpenAPI.paths',OpenAPI.paths);
  AssertNotNull('OpenAPI.paths.api',OpenAPI.Paths['api']);
  AssertEquals('OpenAPI.paths.api.description','abc',OpenAPI.Paths['api'].description);
end;


procedure TTestOpenApiReaderPaths.TestGetEmpty;

begin
  TestRead('{ "paths" : { "api" : { "get": { } } } }');
  AssertNotNull('OpenAPI.paths',OpenAPI.paths);
  AssertNotNull('OpenAPI.paths.api',OpenAPI.Paths['api']);
  AssertNotNull('OpenAPI.paths.api.get',OpenAPI.Paths['api'].get);
end;


procedure TTestOpenApiReaderPaths.TestGetSummary;

begin
  TestRead('{ "paths" : { "api" : { "get": { "summary" : "123" } } } }');
  AssertNotNull('OpenAPI.paths',OpenAPI.paths);
  AssertNotNull('OpenAPI.paths.api',OpenAPI.Paths['api']);
  AssertNotNull('OpenAPI.paths.api.get',OpenAPI.Paths['api'].get);
  AssertEquals('OpenAPI.paths.api.get.summary','123',OpenAPI.Paths['api'].get.summary);
end;


procedure TTestOpenApiReaderPaths.TestGetDescription;

begin
  TestRead('{ "paths" : { "api" : { "get": { "description" : "123" } } } }');
  AssertNotNull('OpenAPI.paths',OpenAPI.paths);
  AssertNotNull('OpenAPI.paths.api',OpenAPI.Paths['api']);
  AssertNotNull('OpenAPI.paths.api.get',OpenAPI.Paths['api'].get);
  AssertEquals('OpenAPI.paths.api.get.description','123',OpenAPI.Paths['api'].get.description);
end;


procedure TTestOpenApiReaderPaths.TestGetOperationId;

begin
  TestRead('{ "paths" : { "api" : { "get": { "operationId" : "123" } } } }');
  AssertNotNull('OpenAPI.paths',OpenAPI.paths);
  AssertNotNull('OpenAPI.paths.api',OpenAPI.Paths['api']);
  AssertNotNull('OpenAPI.paths.api.get',OpenAPI.Paths['api'].get);
  AssertEquals('OpenAPI.paths.api.get.operationId','123',OpenAPI.Paths['api'].get.operationId);
end;


procedure TTestOpenApiReaderPaths.TestGetDeprecated;

begin
  TestRead('{ "paths" : { "api" : { "get": { "deprecated" : true } } } }');
  AssertNotNull('OpenAPI.paths',OpenAPI.paths);
  AssertNotNull('OpenAPI.paths.api',OpenAPI.Paths['api']);
  AssertNotNull('OpenAPI.paths.api.get',OpenAPI.Paths['api'].get);
  AssertEquals('OpenAPI.paths.api.get.deprecated',True,OpenAPI.Paths['api'].get.deprecated);
end;


procedure TTestOpenApiReaderPaths.TestGetExternalDocsEmpty;

begin
  TestRead('{ "paths" : { "api" : { "get": { "externalDocs" : {  } } } } }');
  AssertNotNull('OpenAPI.paths',OpenAPI.paths);
  AssertNotNull('OpenAPI.paths.api',OpenAPI.Paths['api']);
  AssertNotNull('OpenAPI.paths.api.get',OpenAPI.Paths['api'].get);
  AssertNotNull('OpenAPI.paths.api.get.externalDocs',OpenAPI.Paths['api'].get.externalDocs);
end;


procedure TTestOpenApiReaderPaths.TestGetExternalDocsURL;

begin
  TestRead('{ "paths" : { "api" : { "get": { "externalDocs" : { "url" : "123" } } } } }');
  AssertNotNull('OpenAPI.paths',OpenAPI.paths);
  AssertNotNull('OpenAPI.paths.api',OpenAPI.Paths['api']);
  AssertNotNull('OpenAPI.paths.api.get',OpenAPI.Paths['api'].get);
  AssertNotNull('OpenAPI.paths.api.get.externalDocs',OpenAPI.Paths['api'].get.externalDocs);
  AssertEquals('OpenAPI.paths.api.get.externalDocs.url','123',OpenAPI.Paths['api'].get.externalDocs.url);
end;


procedure TTestOpenApiReaderPaths.TestGetServers;

begin
  TestRead('{ "paths" : { "api" : { "servers": [ {} ] } } }');
  AssertNotNull('OpenAPI.paths',OpenAPI.paths);
  AssertNotNull('OpenAPI.paths.api',OpenAPI.Paths['api']);
  AssertNotNull('OpenAPI.paths.api.servers[0]',OpenAPI.Paths['api'].servers[0]);
end;


procedure TTestOpenApiReaderPathRequestBody.TestEmpty;

begin
  TestRead('{ "paths" : { "api" : { "get": { "requestBody" : {  } } } } }');
  AssertNotNull('OpenAPI.paths',OpenAPI.paths);
  AssertNotNull('OpenAPI.paths.api',OpenAPI.Paths['api']);
  AssertNotNull('OpenAPI.paths.api.get',OpenAPI.Paths['api'].get);
  AssertNotNull('OpenAPI.paths.api.get.requestBody',OpenAPI.Paths['api'].get.requestBody);
end;


procedure TTestOpenApiReaderPathRequestBody.TestDescription;

begin
  TestRead('{ "paths" : { "api" : { "get": { "requestBody" : { "description" : "123" } } } } }');
  AssertNotNull('OpenAPI.paths',OpenAPI.paths);
  AssertNotNull('OpenAPI.paths.api',OpenAPI.Paths['api']);
  AssertNotNull('OpenAPI.paths.api.get',OpenAPI.Paths['api'].get);
  AssertNotNull('OpenAPI.paths.api.get.requestBody',OpenAPI.Paths['api'].get.requestBody);
  AssertEquals('OpenAPI.paths.api.get.requestBody.description','123',OpenAPI.Paths['api'].get.requestBody.description);
end;


procedure TTestOpenApiReaderPathRequestBody.TestRequired;

begin
  TestRead('{ "paths" : { "api" : { "get": { "requestBody" : { "required" : true} } } } }');
  AssertNotNull('OpenAPI.paths',OpenAPI.paths);
  AssertNotNull('OpenAPI.paths.api',OpenAPI.Paths['api']);
  AssertNotNull('OpenAPI.paths.api.get',OpenAPI.Paths['api'].get);
  AssertNotNull('OpenAPI.paths.api.get.requestBody',OpenAPI.Paths['api'].get.requestBody);
  AssertEquals('OpenAPI.paths.api.get.requestBody.required',True,OpenAPI.Paths['api'].get.requestBody.required);
end;


procedure TTestOpenApiReaderPathRequestBody.TestContent;

begin
  TestRead('{ "paths" : { "api" : { "get": { "requestBody" : { "content" : { "123" : { } } } } } } }');
  AssertNotNull('OpenAPI.paths',OpenAPI.paths);
  AssertNotNull('OpenAPI.paths.api',OpenAPI.Paths['api']);
  AssertNotNull('OpenAPI.paths.api.get',OpenAPI.Paths['api'].get);
  AssertNotNull('OpenAPI.paths.api.get.requestBody',OpenAPI.Paths['api'].get.requestBody);
  AssertNotNull('OpenAPI.paths.api.get.requestBody.content',OpenAPI.Paths['api'].get.requestBody.content);
  AssertNotNull('OpenAPI.paths.api.get.requestBody.content.123',OpenAPI.Paths['api'].get.requestBody.content['123']);
end;


procedure TTestOpenApiReaderPathRequestBody.TestContentSchema;

begin
  TestRead('{ "paths" : { "api" : { "get": { "requestBody" : { "content" : { "123" : { "schema" : true } } } } } } }');
  AssertNotNull('OpenAPI.paths',OpenAPI.paths);
  AssertNotNull('OpenAPI.paths.api',OpenAPI.Paths['api']);
  AssertNotNull('OpenAPI.paths.api.get',OpenAPI.Paths['api'].get);
  AssertNotNull('OpenAPI.paths.api.get.requestBody',OpenAPI.Paths['api'].get.requestBody);
  AssertNotNull('OpenAPI.paths.api.get.requestBody.content',OpenAPI.Paths['api'].get.requestBody.content);
  AssertNotNull('OpenAPI.paths.api.get.requestBody.content.123',OpenAPI.Paths['api'].get.requestBody.content['123']);
  AssertNotNull('OpenAPI.paths.api.get.requestBody.content.123.schema',OpenAPI.Paths['api'].get.requestBody.content['123'].schema);
end;


procedure TTestOpenApiReaderPathRequestBody.TestContentExample;

begin
  TestRead('{ "paths" : { "api" : { "get": { "requestBody" : { "content" : { "123" : { "example" : { } } } } } } } }');
  AssertNotNull('OpenAPI.paths',OpenAPI.paths);
  AssertNotNull('OpenAPI.paths.api',OpenAPI.Paths['api']);
  AssertNotNull('OpenAPI.paths.api.get',OpenAPI.Paths['api'].get);
  AssertNotNull('OpenAPI.paths.api.get.requestBody',OpenAPI.Paths['api'].get.requestBody);
  AssertNotNull('OpenAPI.paths.api.get.requestBody.content',OpenAPI.Paths['api'].get.requestBody.content);
  AssertNotNull('OpenAPI.paths.api.get.requestBody.content.123',OpenAPI.Paths['api'].get.requestBody.content['123']);
  AssertNotNull('OpenAPI.paths.api.get.requestBody.content.123.example',OpenAPI.Paths['api'].get.requestBody.content['123'].example);
end;


procedure TTestOpenApiReaderPathRequestBody.TestContentExamples;

begin
  TestRead('{ "paths" : { "api" : { "get": { "requestBody" : { "content" : { "123" : { "examples" : { "abc" : { "summary": "x"} } } } } } } } }');
  AssertNotNull('OpenAPI.paths',OpenAPI.paths);
  AssertNotNull('OpenAPI.paths.api',OpenAPI.Paths['api']);
  AssertNotNull('OpenAPI.paths.api.get',OpenAPI.Paths['api'].get);
  AssertNotNull('OpenAPI.paths.api.get.requestBody',OpenAPI.Paths['api'].get.requestBody);
  AssertNotNull('OpenAPI.paths.api.get.requestBody.content',OpenAPI.Paths['api'].get.requestBody.content);
  AssertNotNull('OpenAPI.paths.api.get.requestBody.content.123',OpenAPI.Paths['api'].get.requestBody.content['123']);
  AssertNotNull('OpenAPI.paths.api.get.requestBody.content.123.examples',OpenAPI.Paths['api'].get.requestBody.content['123'].examples);
  AssertNotNull('OpenAPI.paths.api.get.requestBody.content.123.examples.abc',OpenAPI.Paths['api'].get.requestBody.content['123'].examples['abc']);
  AssertEquals('OpenAPI.paths.api.get.requestBody.content.123.examples.abc.summary','x',OpenAPI.Paths['api'].get.requestBody.content['123'].examples['abc'].summary);
end;


procedure TTestOpenApiReaderPathRequestBody.TestContentEncodingContentType;

begin
  TestRead('{ "paths" : { "api" : { "get": { "requestBody" : { "content" : { "123" : { "encoding" : { "abc" : { "contentType": "def"} } } } } } } } }');
  AssertNotNull('OpenAPI.paths',OpenAPI.paths);
  AssertNotNull('OpenAPI.paths.api',OpenAPI.Paths['api']);
  AssertNotNull('OpenAPI.paths.api.get',OpenAPI.Paths['api'].get);
  AssertNotNull('OpenAPI.paths.api.get.requestBody',OpenAPI.Paths['api'].get.requestBody);
  AssertNotNull('OpenAPI.paths.api.get.requestBody.content',OpenAPI.Paths['api'].get.requestBody.content);
  AssertNotNull('OpenAPI.paths.api.get.requestBody.content.[123]',OpenAPI.Paths['api'].get.requestBody.content['123']);
  AssertNotNull('OpenAPI.paths.api.get.requestBody.content.123.encoding',OpenAPI.Paths['api'].get.requestBody.content['123'].encoding);
  AssertNotNull('OpenAPI.paths.api.get.requestBody.content.123.encoding.abc',OpenAPI.Paths['api'].get.requestBody.content['123'].encoding['abc']);
  AssertEquals('OpenAPI.paths.api.get.requestBody.content.123.encoding.abc.contentType','def',OpenAPI.Paths['api'].get.requestBody.content['123'].encoding['abc'].contentType);
end;


procedure TTestOpenApiReaderPathRequestBody.TestContentEncodingExplode;

begin
  TestRead('{ "paths" : { "api" : { "get": { "requestBody" : { "content" : { "123" : { "encoding" : { "abc" : { "explode": true} } } } } } } } }');
  AssertNotNull('OpenAPI.paths',OpenAPI.paths);
  AssertNotNull('OpenAPI.paths.api',OpenAPI.Paths['api']);
  AssertNotNull('OpenAPI.paths.api.get',OpenAPI.Paths['api'].get);
  AssertNotNull('OpenAPI.paths.api.get.requestBody',OpenAPI.Paths['api'].get.requestBody);
  AssertNotNull('OpenAPI.paths.api.get.requestBody.content',OpenAPI.Paths['api'].get.requestBody.content);
  AssertNotNull('OpenAPI.paths.api.get.requestBody.content.123',OpenAPI.Paths['api'].get.requestBody.content['123']);
  AssertNotNull('OpenAPI.paths.api.get.requestBody.content.123.encoding',OpenAPI.Paths['api'].get.requestBody.content['123'].encoding);
  AssertNotNull('OpenAPI.paths.api.get.requestBody.content.123.encoding.abc',OpenAPI.Paths['api'].get.requestBody.content['123'].encoding['abc']);
  AssertEquals('OpenAPI.paths.api.get.requestBody.content.123.encoding.abc.explode',True,OpenAPI.Paths['api'].get.requestBody.content['123'].encoding['abc'].explode);
end;


procedure TTestOpenApiReaderPathRequestBody.TestContentEncodingStyle;

begin
  TestRead('{ "paths" : { "api" : { "get": { "requestBody" : { "content" : { "123" : { "encoding" : { "abc" : { "style": "def"} } } } } } } } }');
  AssertNotNull('OpenAPI.paths',OpenAPI.paths);
  AssertNotNull('OpenAPI.paths.api',OpenAPI.Paths['api']);
  AssertNotNull('OpenAPI.paths.api.get',OpenAPI.Paths['api'].get);
  AssertNotNull('OpenAPI.paths.api.get.requestBody',OpenAPI.Paths['api'].get.requestBody);
  AssertNotNull('OpenAPI.paths.api.get.requestBody.content',OpenAPI.Paths['api'].get.requestBody.content);
  AssertNotNull('OpenAPI.paths.api.get.requestBody.content.123',OpenAPI.Paths['api'].get.requestBody.content['123']);
  AssertNotNull('OpenAPI.paths.api.get.requestBody.content.123.encoding',OpenAPI.Paths['api'].get.requestBody.content['123'].encoding);
  AssertNotNull('OpenAPI.paths.api.get.requestBody.content.123.encoding.abc',OpenAPI.Paths['api'].get.requestBody.content['123'].encoding['abc']);
  AssertEquals('OpenAPI.paths.api.get.requestBody.content.123.encoding.abc.style','def',OpenAPI.Paths['api'].get.requestBody.content['123'].encoding['abc'].style);
end;


procedure TTestOpenApiReaderPathRequestBody.TestContentEncodingAllowReserved;

begin
  TestRead('{ "paths" : { "api" : { "get": { "requestBody" : { "content" : { "123" : { "encoding" : { "abc" : { "allowReserved": true} } } } } } } } }');
  AssertNotNull('OpenAPI.paths',OpenAPI.paths);
  AssertNotNull('OpenAPI.paths.api',OpenAPI.Paths['api']);
  AssertNotNull('OpenAPI.paths.api.get',OpenAPI.Paths['api'].get);
  AssertNotNull('OpenAPI.paths.api.get.requestBody',OpenAPI.Paths['api'].get.requestBody);
  AssertNotNull('OpenAPI.paths.api.get.requestBody.content',OpenAPI.Paths['api'].get.requestBody.content);
  AssertNotNull('OpenAPI.paths.api.get.requestBody.content.123',OpenAPI.Paths['api'].get.requestBody.content['123']);
  AssertNotNull('OpenAPI.paths.api.get.requestBody.content.123.encoding',OpenAPI.Paths['api'].get.requestBody.content['123'].encoding);
  AssertNotNull('OpenAPI.paths.api.get.requestBody.content.123.encoding.abc',OpenAPI.Paths['api'].get.requestBody.content['123'].encoding['abc']);
  AssertEquals('OpenAPI.paths.api.get.requestBody.content.123.encoding.abc.allowReserved',True,OpenAPI.Paths['api'].get.requestBody.content['123'].encoding['abc'].allowReserved);
end;


procedure TTestOpenApiReaderPathRequestBody.TestContentEncodingHeaders;

begin
  TestRead('{ "paths" : { "api" : { "get": { "requestBody" : { "content" : { "123" : { "encoding" : { "abc" : { "headers": { "def" : { "explode" : true } } } } } } } } } } }');
  AssertNotNull('OpenAPI.paths',OpenAPI.paths);
  AssertNotNull('OpenAPI.paths.api',OpenAPI.Paths['api']);
  AssertNotNull('OpenAPI.paths.api.get',OpenAPI.Paths['api'].get);
  AssertNotNull('OpenAPI.paths.api.get.requestBody',OpenAPI.Paths['api'].get.requestBody);
  AssertNotNull('OpenAPI.paths.api.get.requestBody.content',OpenAPI.Paths['api'].get.requestBody.content);
  AssertNotNull('OpenAPI.paths.api.get.requestBody.content.123',OpenAPI.Paths['api'].get.requestBody.content['123']);
  AssertNotNull('OpenAPI.paths.api.get.requestBody.content.123.encoding',OpenAPI.Paths['api'].get.requestBody.content['123'].encoding);
  AssertNotNull('OpenAPI.paths.api.get.requestBody.content.123.encoding.abc',OpenAPI.Paths['api'].get.requestBody.content['123'].encoding['abc']);
  AssertNotNull('OpenAPI.paths.api.get.requestBody.content.123.encoding.abc.headers',OpenAPI.Paths['api'].get.requestBody.content['123'].encoding['abc'].headers);
  AssertNotNull('OpenAPI.paths.api.get.requestBody.content.123.encoding.abc.headers.def',OpenAPI.Paths['api'].get.requestBody.content['123'].encoding['abc'].headers['def']);
  AssertEquals('OpenAPI.paths.api.get.requestBody.content.123.encoding.abc.headers.def.explode',True,OpenAPI.Paths['api'].get.requestBody.content['123'].encoding['abc'].headers['def'].explode);
end;


procedure TTestOpenApiReaderPathParameters.TestGetParametersEmpty;

begin
  TestRead('{ "paths" : { "api" : { "parameters": [ {} ] } } }');
  AssertNotNull('OpenAPI.paths',OpenAPI.paths);
  AssertNotNull('OpenAPI.paths.api',OpenAPI.Paths['api']);
  AssertNotNull('OpenAPI.paths.api.parameters[0]',OpenAPI.Paths['api'].parameters[0]);
end;


procedure TTestOpenApiReaderPathResponses.TestEmpty;

begin
  TestRead('{ "paths" : { "api" : { "get" : { "responses": { "default" : {} } } } } }');
  AssertNotNull('OpenAPI.paths',OpenAPI.paths);
  AssertNotNull('OpenAPI.paths.api',OpenAPI.Paths['api']);
  AssertNotNull('OpenAPI.paths.api.get',OpenAPI.Paths['api'].get);
  AssertNotNull('OpenAPI.paths.api.get.responses',OpenAPI.Paths['api'].get.responses);
  AssertNotNull('OpenAPI.paths.api.get.responses.default',OpenAPI.Paths['api'].get.responses['default']);
end;


procedure TTestOpenApiReaderPathResponses.TestHeaders;

begin
  TestRead('{ "paths" : { "api" : { "get" : { "responses": { "default" : { "headers" : { "x-content" : { } } } } } } } }');
  AssertNotNull('OpenAPI.paths',OpenAPI.paths);
  AssertNotNull('OpenAPI.paths.api',OpenAPI.Paths['api']);
  AssertNotNull('OpenAPI.paths.api.get',OpenAPI.Paths['api'].get);
  AssertNotNull('OpenAPI.paths.api.get.responses',OpenAPI.Paths['api'].get.responses);
  AssertNotNull('OpenAPI.paths.api.get.responses[default]',OpenAPI.Paths['api'].get.responses['default']);
  AssertNotNull('OpenAPI.paths.api.get.responses.default.headers',OpenAPI.Paths['api'].get.responses['default'].headers);
  AssertNotNull('OpenAPI.paths.api.get.responses.default.headers.x-content',OpenAPI.Paths['api'].get.responses['default'].headers['x-content']);
end;


procedure TTestOpenApiReaderPathResponses.TestDescription;

begin
  TestRead('{ "paths" : { "api" : { "get" : { "responses": { "default" : { "description" : "abc" } } } } } }');
  AssertNotNull('OpenAPI.paths',OpenAPI.paths);
  AssertNotNull('OpenAPI.paths.api',OpenAPI.Paths['api']);
  AssertNotNull('OpenAPI.paths.api.get',OpenAPI.Paths['api'].get);
  AssertNotNull('OpenAPI.paths.api.get.responses',OpenAPI.Paths['api'].get.responses);
  AssertNotNull('OpenAPI.paths.api.get.responses.default',OpenAPI.Paths['api'].get.responses['default']);
  AssertEquals('OpenAPI.paths.api.get.responses.default.description','abc',OpenAPI.Paths['api'].get.responses['default'].description);
end;


procedure TTestOpenApiReaderPathResponses.TestContentEmpty;

begin
  TestRead('{ "paths" : { "api" : { "get" : { "responses": { "default" : { "content" : { "application/json" : { } } } } } } } }');
  AssertNotNull('OpenAPI.paths',OpenAPI.paths);
  AssertNotNull('OpenAPI.paths.api',OpenAPI.Paths['api']);
  AssertNotNull('OpenAPI.paths.api.get',OpenAPI.Paths['api'].get);
  AssertNotNull('OpenAPI.paths.api.get.responses',OpenAPI.Paths['api'].get.responses);
  AssertNotNull('OpenAPI.paths.api.get.responses.default',OpenAPI.Paths['api'].get.responses['default']);
  AssertNotNull('OpenAPI.paths.api.get.responses.default.content',OpenAPI.Paths['api'].get.responses['default'].content);
  AssertNotNull('OpenAPI.paths.api.get.responses.default.content.application/json',OpenAPI.Paths['api'].get.responses['default'].content['application/json']);
end;


procedure TTestOpenApiReaderPathResponses.TestContentSchema;

begin
  TestRead('{ "paths" : { "api" : { "get" : { "responses": { "default" : { "content" : { "application/json" : { "schema" : true } } } } } } } }');
  AssertNotNull('OpenAPI.paths',OpenAPI.paths);
  AssertNotNull('OpenAPI.paths.api',OpenAPI.Paths['api']);
  AssertNotNull('OpenAPI.paths.api.get',OpenAPI.Paths['api'].get);
  AssertNotNull('OpenAPI.paths.api.get.responses',OpenAPI.Paths['api'].get.responses);
  AssertNotNull('OpenAPI.paths.api.get.responses.default',OpenAPI.Paths['api'].get.responses['default']);
  AssertNotNull('OpenAPI.paths.api.get.responses.default.content',OpenAPI.Paths['api'].get.responses['default'].content);
  AssertNotNull('OpenAPI.paths.api.get.responses.default.content.application/json',OpenAPI.Paths['api'].get.responses['default'].content['application/json']);
  AssertNotNull('OpenAPI.paths.api.get.responses.default.content.application/json.schema',OpenAPI.Paths['api'].get.responses['default'].content['application/json'].schema);
end;


procedure TTestOpenApiReaderPathResponses.TestContentExample;

begin
  TestRead('{ "paths" : { "api" : { "get" : { "responses": { "default" : { "content" : { "application/json" : { "example" : {} } } } } } } } }');
  AssertNotNull('OpenAPI.paths',OpenAPI.paths);
  AssertNotNull('OpenAPI.paths.api',OpenAPI.Paths['api']);
  AssertNotNull('OpenAPI.paths.api.get',OpenAPI.Paths['api'].get);
  AssertNotNull('OpenAPI.paths.api.get.responses',OpenAPI.Paths['api'].get.responses);
  AssertNotNull('OpenAPI.paths.api.get.responses.default',OpenAPI.Paths['api'].get.responses['default']);
  AssertNotNull('OpenAPI.paths.api.get.responses.default.content',OpenAPI.Paths['api'].get.responses['default'].content);
  AssertNotNull('OpenAPI.paths.api.get.responses.default.content.application/json',OpenAPI.Paths['api'].get.responses['default'].content['application/json']);
  AssertNotNull('OpenAPI.paths.api.get.responses.default.content.application/json.example',OpenAPI.Paths['api'].get.responses['default'].content['application/json'].example);
end;


procedure TTestOpenApiReaderPathResponses.TestContentExamples;

begin
  TestRead('{ "paths" : { "api" : { "get" : { "responses": { "default" : { "content" : { "application/json" : { "examples" : { "abc" : { "summary": "x"} } } } } } } } } }');
  AssertNotNull('OpenAPI.paths',OpenAPI.paths);
  AssertNotNull('OpenAPI.paths.api',OpenAPI.Paths['api']);
  AssertNotNull('OpenAPI.paths.api.get',OpenAPI.Paths['api'].get);
  AssertNotNull('OpenAPI.paths.api.get.responses',OpenAPI.Paths['api'].get.responses);
  AssertNotNull('OpenAPI.paths.api.get.responses.default',OpenAPI.Paths['api'].get.responses['default']);
  AssertNotNull('OpenAPI.paths.api.get.responses.default.content',OpenAPI.Paths['api'].get.responses['default'].content);
  AssertNotNull('OpenAPI.paths.api.get.responses.default.content.application/json',OpenAPI.Paths['api'].get.responses['default'].content['application/json']);
  AssertNotNull('OpenAPI.paths.api.get.responses.default.content.application/json.examples',OpenAPI.Paths['api'].get.responses['default'].content['application/json'].examples);
  AssertNotNull('OpenAPI.paths.api.get.responses.default.content.application/json.examples.abc',OpenAPI.Paths['api'].get.responses['default'].content['application/json'].examples['abc']);
  AssertEquals('OpenAPI.paths.api.get.responses.default.content.application/json.examples.abc.summary','x',OpenAPI.Paths['api'].get.responses['default'].content['application/json'].examples['abc'].summary);
end;


procedure TTestOpenApiReaderPathResponses.TestContentEncodingEmpty;

begin
  TestRead('{ "paths" : { "api" : { "get" : { "responses": { "default" : { "content" : { "application/json" : { "encoding" : { "abc" : { } } } } } } } } } }');
  AssertNotNull('OpenAPI.paths',OpenAPI.paths);
  AssertNotNull('OpenAPI.paths.api',OpenAPI.Paths['api']);
  AssertNotNull('OpenAPI.paths.api.get',OpenAPI.Paths['api'].get);
  AssertNotNull('OpenAPI.paths.api.get.responses',OpenAPI.Paths['api'].get.responses);
  AssertNotNull('OpenAPI.paths.api.get.responses.default',OpenAPI.Paths['api'].get.responses['default']);
  AssertNotNull('OpenAPI.paths.api.get.responses.default.content',OpenAPI.Paths['api'].get.responses['default'].content);
  AssertNotNull('OpenAPI.paths.api.get.responses.default.content.application/json',OpenAPI.Paths['api'].get.responses['default'].content['application/json']);
  AssertNotNull('OpenAPI.paths.api.get.responses.default.content.application/json.encoding',OpenAPI.Paths['api'].get.responses['default'].content['application/json'].encoding);
  AssertNotNull('OpenAPI.paths.api.get.responses.default.content.application/json.encoding.abc',OpenAPI.Paths['api'].get.responses['default'].content['application/json'].encoding['abc']);
end;


procedure TTestOpenApiReaderPathResponses.TestContentEncodingContentType;

begin
  TestRead('{ "paths" : { "api" : { "get" : { "responses": { "default" : { "content" : { "application/json" : { "encoding" : { "abc" : { "contentType" : "application/json" } } } } } } } } } }');
  AssertNotNull('OpenAPI.paths',OpenAPI.paths);
  AssertNotNull('OpenAPI.paths.api',OpenAPI.Paths['api']);
  AssertNotNull('OpenAPI.paths.api.get',OpenAPI.Paths['api'].get);
  AssertNotNull('OpenAPI.paths.api.get.responses',OpenAPI.Paths['api'].get.responses);
  AssertNotNull('OpenAPI.paths.api.get.responses.default',OpenAPI.Paths['api'].get.responses['default']);
  AssertNotNull('OpenAPI.paths.api.get.responses.default.content',OpenAPI.Paths['api'].get.responses['default'].content);
  AssertNotNull('OpenAPI.paths.api.get.responses.default.content.application/json',OpenAPI.Paths['api'].get.responses['default'].content['application/json']);
  AssertNotNull('OpenAPI.paths.api.get.responses.default.content.application/json.encoding',OpenAPI.Paths['api'].get.responses['default'].content['application/json'].encoding);
  AssertNotNull('OpenAPI.paths.api.get.responses.default.content.application/json.encoding.abc',OpenAPI.Paths['api'].get.responses['default'].content['application/json'].encoding['abc']);
  AssertEquals('OpenAPI.paths.api.get.responses.default.content.application/json.encoding.abc.contentType','application/json',OpenAPI.Paths['api'].get.responses['default'].content['application/json'].encoding['abc'].contentType);
end;


procedure TTestOpenApiReaderPathSecurity.TestEmpty;

begin
  TestRead('{ "paths" : { "api" : { "get" : { "security" : [ {} ] } } } }');
  AssertNotNull('OpenAPI.paths',OpenAPI.paths);
  AssertNotNull('OpenAPI.paths.api',OpenAPI.Paths['api']);
  AssertNotNull('OpenAPI.paths.api.get',OpenAPI.Paths['api'].get);
  AssertNotNull('OpenAPI.paths.api.get.security[0]',OpenAPI.Paths['api'].get.security[0]);
end;


procedure TTestOpenApiReaderPathSecurity.TestAPIKey;

begin
  TestRead('{ "paths" : { "api" : { "get" : { "security" : [ { "api_key" : ["akey"] } ] } } } }');
  AssertNotNull('OpenAPI.paths',OpenAPI.paths);
  AssertNotNull('OpenAPI.paths.api',OpenAPI.Paths['api']);
  AssertNotNull('OpenAPI.paths.api.get',OpenAPI.Paths['api'].get);
  AssertNotNull('OpenAPI.paths.api.get.security[0]',OpenAPI.Paths['api'].get.security[0]);
  AssertEquals('OpenAPI.paths.api.get.security[0].api_key[0]','akey',OpenAPI.Paths['api'].get.security[0].Requirements['api_key'].Strings[0]);
end;


procedure TTestOpenApiReaderPathResponsesLinks.TestEmpty;

begin
  TestRead('{ "paths" : { "api" : { "get" : { "responses" : { "default" : { "links": { "abc" : { } } } } } } } }');
  AssertNotNull('OpenAPI.paths',OpenAPI.paths);
  AssertNotNull('OpenAPI.paths.api',OpenAPI.Paths['api']);
  AssertNotNull('OpenAPI.paths.api.get',OpenAPI.Paths['api'].get);
  AssertNotNull('OpenAPI.paths.api.get.responses',OpenAPI.Paths['api'].get.responses);
  AssertNotNull('OpenAPI.paths.api.get.responses.default',OpenAPI.Paths['api'].get.responses['default']);
  AssertNotNull('OpenAPI.paths.api.get.responses.default.links',OpenAPI.Paths['api'].get.responses['default'].links);
  AssertNotNull('OpenAPI.paths.api.get.responses.default.links.abc',OpenAPI.Paths['api'].get.responses['default'].links['abc']);
end;


procedure TTestOpenApiReaderPathResponsesLinks.TestOperatonRef;

begin
  TestRead('{ "paths" : { "api" : { "get" : { "responses" : { "default" : { "links": { "abc" : { "operationRef" : "123" } } } } } } } }');
  AssertNotNull('OpenAPI.paths',OpenAPI.paths);
  AssertNotNull('OpenAPI.paths.api',OpenAPI.Paths['api']);
  AssertNotNull('OpenAPI.paths.api.get',OpenAPI.Paths['api'].get);
  AssertNotNull('OpenAPI.paths.api.get.responses',OpenAPI.Paths['api'].get.responses);
  AssertNotNull('OpenAPI.paths.api.get.responses.default',OpenAPI.Paths['api'].get.responses['default']);
  AssertNotNull('OpenAPI.paths.api.get.responses.default.links',OpenAPI.Paths['api'].get.responses['default'].links);
  AssertNotNull('OpenAPI.paths.api.get.responses.default.links.abc',OpenAPI.Paths['api'].get.responses['default'].links['abc']);
  AssertEquals('OpenAPI.paths.api.get.responses.default.links.abc.operationRef','123',OpenAPI.Paths['api'].get.responses['default'].links['abc'].operationRef);
end;


procedure TTestOpenApiReaderPathResponsesLinks.TestOperatonId;

begin
  TestRead('{ "paths" : { "api" : { "get" : { "responses" : { "default" : { "links": { "abc" : { "operationId" : "123" } } } } } } } }');
  AssertNotNull('OpenAPI.paths',OpenAPI.paths);
  AssertNotNull('OpenAPI.paths.api',OpenAPI.Paths['api']);
  AssertNotNull('OpenAPI.paths.api.get',OpenAPI.Paths['api'].get);
  AssertNotNull('OpenAPI.paths.api.get.responses',OpenAPI.Paths['api'].get.responses);
  AssertNotNull('OpenAPI.paths.api.get.responses.default',OpenAPI.Paths['api'].get.responses['default']);
  AssertNotNull('OpenAPI.paths.api.get.responses.default.links',OpenAPI.Paths['api'].get.responses['default'].links);
  AssertNotNull('OpenAPI.paths.api.get.responses.default.links.abc',OpenAPI.Paths['api'].get.responses['default'].links['abc']);
  AssertEquals('OpenAPI.paths.api.get.responses.default.links.abc.operationId','123',OpenAPI.Paths['api'].get.responses['default'].links['abc'].operationId);
end;


procedure TTestOpenApiReaderPathResponsesLinks.TestParameters;

begin
  TestRead('{ "paths" : { "api" : { "get" : { "responses" : { "default" : { "links": { "abc" : { "parameters" : { "x" : "y" } } } } } } } } }');
  AssertNotNull('OpenAPI.paths',OpenAPI.paths);
  AssertNotNull('OpenAPI.paths.api',OpenAPI.Paths['api']);
  AssertNotNull('OpenAPI.paths.api.get',OpenAPI.Paths['api'].get);
  AssertNotNull('OpenAPI.paths.api.get.responses',OpenAPI.Paths['api'].get.responses);
  AssertNotNull('OpenAPI.paths.api.get.responses.default',OpenAPI.Paths['api'].get.responses['default']);
  AssertNotNull('OpenAPI.paths.api.get.responses.default.links',OpenAPI.Paths['api'].get.responses['default'].links);
  AssertNotNull('OpenAPI.paths.api.get.responses.default.links.abc',OpenAPI.Paths['api'].get.responses['default'].links['abc']);
  AssertNotNull('OpenAPI.paths.api.get.responses.default.links.abc.parameters',OpenAPI.Paths['api'].get.responses['default'].links['abc'].parameters);
  AssertEquals('OpenAPI.paths.api.get.responses.default.links.abc.parameters.x','y',OpenAPI.Paths['api'].get.responses['default'].links['abc'].parameters['x'].AsString);
end;


procedure TTestOpenApiReaderPathResponsesLinks.TestRequestBody;

begin
  TestRead('{ "paths" : { "api" : { "get" : { "responses" : { "default" : { "links": { "abc" : { "requestBody" : {} } } } } } } } }');
  AssertNotNull('OpenAPI.paths',OpenAPI.paths);
  AssertNotNull('OpenAPI.paths.api',OpenAPI.Paths['api']);
  AssertNotNull('OpenAPI.paths.api.get',OpenAPI.Paths['api'].get);
  AssertNotNull('OpenAPI.paths.api.get.responses',OpenAPI.Paths['api'].get.responses);
  AssertNotNull('OpenAPI.paths.api.get.responses.default',OpenAPI.Paths['api'].get.responses['default']);
  AssertNotNull('OpenAPI.paths.api.get.responses[default].links',OpenAPI.Paths['api'].get.responses['default'].links);
  AssertNotNull('OpenAPI.paths.api.get.responses[default].links.abc',OpenAPI.Paths['api'].get.responses['default'].links['abc']);
  AssertNotNull('OpenAPI.paths.api.get.responses[default].links.abc.requestBody',OpenAPI.Paths['api'].get.responses['default'].links['abc'].requestBody);
end;


procedure TTestOpenApiReaderPathResponsesLinks.TestDescription;

begin
  TestRead('{ "paths" : { "api" : { "get" : { "responses" : { "default" : { "links": { "abc" : { "description" : "123" } } } } } } } }');
  AssertNotNull('OpenAPI.paths',OpenAPI.paths);
  AssertNotNull('OpenAPI.paths.api',OpenAPI.Paths['api']);
  AssertNotNull('OpenAPI.paths.api.get',OpenAPI.Paths['api'].get);
  AssertNotNull('OpenAPI.paths.api.get.responses',OpenAPI.Paths['api'].get.responses);
  AssertNotNull('OpenAPI.paths.api.get.responses[default]',OpenAPI.Paths['api'].get.responses['default']);
  AssertNotNull('OpenAPI.paths.api.get.responses[default].links',OpenAPI.Paths['api'].get.responses['default'].links);
  AssertNotNull('OpenAPI.paths.api.get.responses[default].links.abc',OpenAPI.Paths['api'].get.responses['default'].links['abc']);
  AssertEquals('OpenAPI.paths.api.get.responses[default].links.abc.description','123',OpenAPI.Paths['api'].get.responses['default'].links['abc'].description);
end;


procedure TTestOpenApiReaderPathResponsesLinks.TestServer;

begin
  TestRead('{ "paths" : { "api" : { "get" : { "responses" : { "default" : { "links": { "abc" : { "server" : { "url" : "123" } } } } } } } } }');
  AssertNotNull('OpenAPI.paths',OpenAPI.paths);
  AssertNotNull('OpenAPI.paths.api',OpenAPI.Paths['api']);
  AssertNotNull('OpenAPI.paths.api.get',OpenAPI.Paths['api'].get);
  AssertNotNull('OpenAPI.paths.api.get.responses',OpenAPI.Paths['api'].get.responses);
  AssertNotNull('OpenAPI.paths.api.get.responses[default]',OpenAPI.Paths['api'].get.responses['default']);
  AssertNotNull('OpenAPI.paths.api.get.responses[default].links',OpenAPI.Paths['api'].get.responses['default'].links);
  AssertNotNull('OpenAPI.paths.api.get.responses[default].links.abc',OpenAPI.Paths['api'].get.responses['default'].links['abc']);
  AssertNotNull('OpenAPI.paths.api.get.responses[default].links.abc.server',OpenAPI.Paths['api'].get.responses['default'].links['abc'].server);
  AssertEquals('OpenAPI.paths.api.get.responses[default].links.abc.server.url','123',OpenAPI.Paths['api'].get.responses['default'].links['abc'].server.url);
end;


procedure TTestOpenApiReaderWebHooks.TestEmpty;

begin
  TestRead('{ "webhooks" : { "newitem" : { } } }');
  AssertNotNull('OpenAPI.webhooks',OpenAPI.webhooks);
  AssertNotNull('OpenAPI.webhooks.newitem',OpenAPI.webhooks['newitem']);
end;


procedure TTestOpenApiReaderWebHooks.TestSummary;

begin
  TestRead('{ "webhooks" : { "newitem" : { "summary" : "abc" } } }');
  AssertNotNull('OpenAPI.webhooks',OpenAPI.webhooks);
  AssertNotNull('OpenAPI.webhooks.newitem',OpenAPI.webhooks['newitem']);
  AssertEquals('OpenAPI.webhooks.newitem.summary','abc',OpenAPI.webhooks['newitem'].summary);
end;


procedure TTestOpenApiReaderComponents.TestSchemaEmpty;

begin
  TestRead('{ "components" : { "schemas" : { "abc" : true } } }');
  AssertNotNull('OpenAPI.components',OpenAPI.components);
  AssertNotNull('OpenAPI.components.schemas',OpenAPI.components.schemas);
  AssertNotNUll('OpenAPI.components.schemas.abc',OpenAPI.components.schemas['abc']);
end;


procedure TTestOpenApiReaderComponents.TestSchemaContentEncoding;

begin
  TestRead('{ "components" : { "schemas" : { "abc" : { "contentEncoding" : "utf8" } } } }');
  AssertNotNull('OpenAPI.components',OpenAPI.components);
  AssertNotNull('OpenAPI.components.schemas',OpenAPI.components.schemas);
  AssertNotNull('OpenAPI.components.schemas.abc',OpenAPI.components.schemas['abc']);
  AssertEquals('OpenAPI.components.schemas.abc.contentEncoding','utf8',OpenAPI.components.schemas['abc'].Validations.contentEncoding);
end;

{ TTestOpenApiReaderOAuth2Scopes }

procedure TTestOpenApiReaderOAuth2Scopes.TestOAuth2ScopesAsObject;

begin
  TestRead('{ "components" : { "securitySchemes" : { "oauth2" : { "type" : "oauth2", "flows" : { "authorizationCode" : { "authorizationUrl" : "https://example.com/auth", "tokenUrl" : "https://example.com/token", "scopes" : { "read:user" : "Read user data", "write:user" : "Write user data" } } } } } } }');
  AssertNotNull('OpenAPI.components',OpenAPI.components);
  AssertNotNull('OpenAPI.components.securitySchemes',OpenAPI.components.securitySchemes);
  AssertNotNull('OpenAPI.components.securitySchemes.oauth2',OpenAPI.components.securitySchemes['oauth2']);
  AssertNotNull('OpenAPI.components.securitySchemes.oauth2.flows',OpenAPI.components.securitySchemes['oauth2'].flows);
  AssertNotNull('OpenAPI.components.securitySchemes.oauth2.flows.authorizationCode',OpenAPI.components.securitySchemes['oauth2'].flows.ClientAuthorizationCode);
  AssertEquals('scopes count', 2, OpenAPI.components.securitySchemes['oauth2'].flows.ClientAuthorizationCode.Scopes.Count);
  AssertEquals('scope read:user value', 'Read user data', OpenAPI.components.securitySchemes['oauth2'].flows.ClientAuthorizationCode.Scopes.Values['read:user']);
  AssertEquals('scope write:user value', 'Write user data', OpenAPI.components.securitySchemes['oauth2'].flows.ClientAuthorizationCode.Scopes.Values['write:user']);
end;

procedure TTestOpenApiReaderOAuth2Scopes.TestOAuth2ScopesEmpty;

begin
  TestRead('{ "components" : { "securitySchemes" : { "oauth2" : { "type" : "oauth2", "flows" : { "authorizationCode" : { "authorizationUrl" : "https://example.com/auth", "tokenUrl" : "https://example.com/token", "scopes" : { } } } } } } }');
  AssertNotNull('OpenAPI.components',OpenAPI.components);
  AssertNotNull('OpenAPI.components.securitySchemes',OpenAPI.components.securitySchemes);
  AssertNotNull('OpenAPI.components.securitySchemes.oauth2',OpenAPI.components.securitySchemes['oauth2']);
  AssertEquals('scopes count', 0, OpenAPI.components.securitySchemes['oauth2'].flows.ClientAuthorizationCode.Scopes.Count);
end;


procedure TTestOpenApiReaderSecurity.TestOne;

begin
  TestRead('{ "security" : [ { "abc" : [] } ] }');
end;


initialization
  registertests('Reader',[
    TTestOpenApiReaderLarge,
    TTestopenApiReaderOpenAPI,
    TTestopenApiReaderInfo,
    TTestopenApiReaderTags,
    TTestOpenApiReaderExternalDocs,
    TTestOpenApiReaderServers,
    TTestOpenApiReaderPaths,
    TTestOpenApiReaderPathRequestBody,
    TTestOpenApiReaderPathParameters,
    TTestOpenApiReaderPathResponses,
    TTestOpenApiReaderPathSecurity,
    TTestOpenApiReaderPathResponsesLinks,
    TTestOpenApiReaderWebHooks,
    TTestOpenApiReaderComponents,
    TTestOpenApiReaderOAuth2Scopes,
    TTestOpenApiReaderSecurity
  ]);

end.

