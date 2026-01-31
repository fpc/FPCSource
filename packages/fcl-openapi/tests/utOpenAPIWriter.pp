unit utOpenAPIWriter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, fpjson, fpjson.schema.schema, fpopenapi.objects, fpopenapi.writer;

type

  { TTestOpenApiWriterBase }

  TTestOpenApiWriterBase = class(TTestCase)
  private
    FDocument: TJSONObject;
    FOpenAPI: TOpenAPI;
    procedure AddSource(Const aFmt : string; aArgs : Array of const);
    Procedure AddSource(Const aLine : string);
    procedure AddDecl(Const aFmt : string; aArgs : Array of const);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    Procedure WriteOpenAPI;
    procedure CheckJSON(const Msg: String; aJSON: TJSONStringType);
    procedure TestWrite(const Msg: String; aJSON: TJSONStringType);
  Public
    Property OpenApi : TOpenAPI Read FOpenAPI;
    Property Document : TJSONObject Read FDocument;
  published
    procedure TestHookUp;
  end;

  { TTestOpenApiWriter }

  TTestOpenApiWriterOpenAPI = class(TTestOpenApiWriterBase)
  Published
    procedure TestEmpty;
    Procedure TestOpenAPI;
    Procedure TestJSONSchemaDialect;
  end;

  TTestOpenApiWriterInfo = class(TTestOpenApiWriterBase)
  Published
    procedure TestVersion;
    procedure TestTitle;
    procedure TestSummary;
    procedure TestDescription;
    procedure TestTermsOfService;
    procedure TestContactEmail;
    procedure TestContactName;
    procedure TestContactURL;
    procedure TestLicenseUrl;
    procedure TestLicenseIdentifier;
    procedure TestLicenseName;
  end;

  TTestOpenApiWriterTags = class(TTestOpenApiWriterBase)
  Published
    Procedure TestName;
    Procedure TestDescription;
    Procedure TestExternalDocs;
    Procedure Test2Tags;
  end;

  TTestOpenApiWriterExternalDocs = class(TTestOpenApiWriterBase)
  Published
    procedure TestUrl;
    procedure TestDescription;
  end;

  { TTestOpenApiWriterServers }

  TTestOpenApiWriterServers = class(TTestOpenApiWriterBase)
  Published
    Procedure TestUrl;
    Procedure TestDescription;
    procedure TestVariablesDefault;
    procedure TestVariablesDescription;
    procedure TestVariablesEnum;
    Procedure Test2Url;
  end;

  { TTestOpenApiWriterPaths }
  TTestOpenApiWriterPathBase = class(TTestOpenApiWriterBase)
  Public
    Function AddPath(aPath : String) : TPathItem;
  end;

  TTestOpenApiWriterPaths = class(TTestOpenApiWriterPathBase)
  Published
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

  { TTestOpenApiWriterPathRequestBody }

  TTestOpenApiWriterPathRequestBody = class(TTestOpenApiWriterPathBase)
  Public
    Function AddBody(aPath : String) : TRequestBody;
  Published
    procedure TestEmpty;
    procedure TestDescription;
    procedure TestRequired;
    procedure TestContent;
    procedure TestContentSchema;
    procedure TestContentExample;
    procedure TestContentExamples;
    procedure TestContentEncodingContentType;
    procedure TestContentEncodingExplode;
    procedure TestContentEncodingStyle;
    procedure TestContentEncodingAllowReserved;
    procedure TestContentEncodingHeaders;
  end;

  { TTestOpenApiWriterPathParameters }

  TTestOpenApiWriterPathParameters = class(TTestOpenApiWriterPathBase)
  Public
    Function AddParam(aPath : String) : TParameter;
  Published
    Procedure TestGetParametersEmpty;
  end;

  { TTestOpenApiWriterPathResponses }

  TTestOpenApiWriterPathResponses = class(TTestOpenApiWriterPathBase)
  Public
    Function AddResponse(aPath : String) : TResponse;
  Published
    procedure TestEmpty;
    procedure TestHeaders;
    procedure TestDescription;
    procedure TestContentEmpty;
    procedure TestContentSchema;
    procedure TestContentExample;
    procedure TestContentExamples;
    procedure TestContentEncodingEmpty;
    procedure TestContentEncodingContentType;
  end;

  { TTestOpenApiWriterPathResponsesLinks }

  TTestOpenApiWriterPathResponsesLinks = class(TTestOpenApiWriterPathBase)
  Public
    Function AddLink(aPath : String) : TLink;
  Published
    procedure TestEmpty;
    Procedure TestOperatonRef;
    Procedure TestOperatonId;
    Procedure TestParameters;
    Procedure TestRequestBody;
    Procedure TestDescription;
    Procedure TestServer;
  end;

  { TTestOpenApiWriterPathSecurity }

  TTestOpenApiWriterPathSecurity = class(TTestOpenApiWriterPathBase)
  Published
    Procedure TestEmpty;
    Procedure TestAPIKey;
  end;

  { TTestOpenApiWriterWebHooks }

  TTestOpenApiWriterWebHooks = class(TTestOpenApiWriterBase)
  Public
     function AddWebHook(const aname : string) : TPathItem;
  Published
    procedure TestEmpty;
    procedure TestSummary;
    // At the moment, no more tests needed: the items are identical to path items.
  end;

  { TTestOpenApiWriterComponents }

  TTestOpenApiWriterComponents = class(TTestOpenApiWriterBase)
  Public
     function AddComponentSchema(const aname: string): TJSONSchema;
  Published
    procedure TestSchemaEmpty;
    procedure TestSchemaContentEncoding;
  end;

  { TTestOpenApiWriterOAuth2Scopes }

  TTestOpenApiWriterOAuth2Scopes = class(TTestOpenApiWriterBase)
  Published
    procedure TestOAuth2ScopesAsObject;
    procedure TestOAuth2ScopesEmpty;
  end;

  { TTestOpenApiWriterSecurity }

  TTestOpenApiWriterSecurity = class(TTestOpenApiWriterBase)
  Published
    procedure TestOne;
  end;

  (*

property Components: TComponents Read Get_Components write Set_Components;
property Security: TSecurityRequirementList Read Get_Security write Set_Security;

*)

implementation

uses jsoncomparer;

var
  _Sources : TStrings;
  _Decl : TStrings;
  _LastClass : String;

function  ChangeRead(S : String) : String;
begin
  Result:=StringReplace(S,'Write','Reade',[rfReplaceAll,rfIgnoreCase]);
end;

procedure StartSources; forward;


Procedure AddMethod(aClassName,aTestName : string; const aJSON : String);

var
  lClass,lTest : String;
  J,S : TJSONObject;
  D : TJSONData;
  P,N : String;


begin
  lClass:=ChangeRead(aClassName);
  lTest:=ChangeRead(aTestName);
  if lClass<>_LastClass then
    begin
    if _LastClass<>'' then
      _Decl.Add('  end;');
    _Decl.Add('');
    _Decl.Add('  %s = Class(TOpenAPITestRead)',[lClass]);
    _LastClass:=lClass;
    end;
  _Decl.Add('    Procedure %s;',[lTest]);
  _Sources.Add('');
  _Sources.Add('procedure %s.%s;',[lClass,lTest]);
  _Sources.Add('');
  _Sources.Add('begin');
  _Sources.Add('  TestRead(''%s'');',[aJSON]);
  J:=GetJSON(aJSON) as TJSONObject;
  P:='OpenAPI';
  S:=Nil;
  try
    if J.Count>0 then
      begin
      D:=J.Items[0];
      if D is TJSONObject then
        S:=TJSONObject(D);
      N:=J.Names[0];
      While S is TJSONObject do
        begin
        P:=P+'.'+N;
        _Sources.Add('  AssertNotNull(''%s'',%s);',[P,P]);
        if S.Count=0 then
          begin
          D:=Nil;
          S:=Nil;
          Continue;
          end;
        D:=S.Items[0];
        N:=S.Names[0];
        if (D is TJSONArray) and (TJSONArray(D).Count>0) then
          begin
          D:=TJSONArray(D)[0];
          N:=N+'[0]';
          end;
        if D is TJSONObject then
          S:=TJSONObject(D)
        else
          S:=Nil;
        end;
      P:=P+'.'+N;
      if assigned(D) then
        Case D.JSONType of
          jtString : _Sources.Add('  AssertEquals(''%s'',''%s'',%s);',[P,D.AsString,P]);
          jtBoolean : _Sources.Add('  AssertEquals(''%s'',%s,%s);',[P,D.AsString,P]);
          jtNumber : _Sources.Add('  AssertEquals(''%s'',%s,%s);',[P,D.AsString,P]);
        end;
      end;
  finally
    J.Free;
  end;
  _Sources.Add('end;');
  _Sources.Add('');
end;


procedure TTestOpenApiWriterBase.AddSource(const aFmt: string; aArgs: array of const);
begin
  AddSource(Format(aFmt,aArgs));
end;

procedure TTestOpenApiWriterBase.AddSource(const aLine: string);
begin
  _Sources.Add(aLine);
end;

procedure TTestOpenApiWriterBase.AddDecl(const aFmt: string; aArgs: array of const);
begin
  _Decl.Add(aFmt,aArgs);
end;

procedure TTestOpenApiWriterBase.SetUp;
begin
  FOpenAPI:=TOpenAPI.Create;
  if _Decl=Nil then
    StartSources;
end;

procedure TTestOpenApiWriterBase.TearDown;
begin
  FreeAndNil(FOpenAPI);
  FreeAndNil(FDocument);
end;

procedure TTestOpenApiWriterBase.WriteOpenAPI;

var
  W : TOpenAPIWriter;

begin
  W:=TOpenAPIWriter.Create(Nil);
  try
    FDocument:=W.WriteToJSON(OpenAPI);
  finally
    W.Free;
  end;
end;


procedure TTestOpenApiWriterBase.CheckJSON(const Msg: String; aJSON: TJSONStringType);

var
  Data : TJSONData;
  Obj : TJSONObject absolute data;
  Errors : TStrings;

begin
  Errors:=Nil;
  Data:=GetJSON(aJSON,true) as TJSONObject;
  try
    AssertEquals('Json object',TJSONObject,Data.ClassType);
    Errors:=TStringList.Create;
    // Writeln('Comparing ',Obj.AsJSON,' === ',Document.AsJSON);
    TJSONComparer.Compare(Obj,Document,Errors);
    if (Errors.Count<>0) then
      Fail(Msg+':'#10+Errors.Text);
  finally
    Errors.Free;
    Data.Free;
  end;
end;

procedure TTestOpenApiWriterBase.TestWrite(const Msg: String; aJSON: TJSONStringType);
begin
  AddMethod(ClassName,TestName,aJSON);
  WriteOpenAPI;
  CheckJSON(msg,aJSON);
end;


procedure TTestOpenApiWriterBase.TestHookUp;
begin
  AssertNotNull('have openapi',OpenAPI);
  AssertNull('have no document',Document);
end;

procedure TTestopenApiWriterOpenApi.TestEmpty;
begin
  WriteOpenApi;
  CheckJSON('Empty','{}');
end;


procedure TTestopenApiWriterOpenAPI.TestOpenAPI;
begin
  OpenAPI.OpenAPI:='123';
  TestWrite('openapi version','{ "openapi" : "123" }');
end;

procedure TTestopenApiWriterOpenAPI.TestJSONSchemaDialect;
begin
  OpenAPI.JSONSchemaDialect:='123';
  TestWrite('json schema dialect','{ "jsonSchemaDialect" : "123" }');
end;

procedure TTestopenApiWriterInfo.TestVersion;
begin
  OpenAPI.Info.Version:='123';
  TestWrite('Version','{ "info" : { "version" : "123" } }');
end;

procedure TTestopenApiWriterInfo.TestTitle;
begin
  OpenAPI.Info.title:='123';
  TestWrite('Title','{ "info" : { "title" : "123" } }');
end;

procedure TTestopenApiWriterInfo.TestSummary;
begin
  OpenAPI.Info.summary:='123';
  TestWrite('Summary','{ "info" : { "summary" : "123" } }');
end;

procedure TTestopenApiWriterInfo.TestDescription;
begin
  OpenAPI.Info.Description:='123';
  TestWrite('Description','{ "info" : { "description" : "123" } }');
end;

procedure TTestopenApiWriterInfo.TestTermsOfService;
begin
  OpenAPI.Info.TermsOfService:='123';
  TestWrite('TermsOfService','{ "info" : { "termsOfService" : "123" } }');
end;

procedure TTestopenApiWriterInfo.TestContactEmail;
begin
  OpenAPI.Info.Contact.Email:='123';
  TestWrite('Contact mail','{ "info" : { "contact" : { "email" : "123" } } }');
end;

procedure TTestopenApiWriterInfo.TestContactName;
begin
  OpenAPI.Info.Contact.Name:='123';
  TestWrite('Contact name','{ "info" : { "contact" : { "name" : "123" } } }');
end;

procedure TTestopenApiWriterInfo.TestContactURL;
begin
  OpenAPI.Info.Contact.URL:='123';
  TestWrite('Contact url','{ "info" : { "contact" : { "url" : "123" } } }');
end;

procedure TTestopenApiWriterInfo.TestLicenseUrl;
begin
  OpenAPI.Info.License.Url:='123';
  TestWrite('license url','{ "info" : { "license" : { "url" : "123" } } }');
end;

procedure TTestopenApiWriterInfo.TestLicenseName;
begin
  OpenAPI.Info.License.Name:='123';
  TestWrite('license name','{ "info" : { "license" : { "name" : "123" } } }');
end;


procedure TTestopenApiWriterInfo.TestLicenseIdentifier;
begin
  OpenAPI.Info.License.Identifier:='123';
  TestWrite('license name','{ "info" : { "license" : { "identifier" : "123" } } }');
end;

procedure TTestopenApiWriterTags.TestName;

var
  T : TTag;

begin
  T:=TTag.Create(OpenAPI,'tags[0]');
  T.Name:='123';
  OpenAPI.Tags.Add(T);
  TestWrite('name','{ "tags" : [ { "name" : "123" } ] }');
end;

procedure TTestopenApiWriterTags.TestDescription;
var
  T : TTag;

begin
  T:=TTag.Create(OpenAPI,'tags[1]');
  T.Description:='123';
  OpenAPI.Tags.Add(T);
  TestWrite('description','{ "tags" : [ { "description" : "123" } ] }');
end;

procedure TTestopenApiWriterTags.TestExternalDocs;
var
  T : TTag;

begin
  T:=TTag.Create(OpenAPI,'tags[1]');
  T.ExternalDocs.Url:='123';
  OpenAPI.Tags.Add(T);
  TestWrite('externaldocs','{ "tags" : [ { "externalDocs" : { "url" :"123" } } ] }');
end;

procedure TTestopenApiWriterTags.Test2Tags;
var
  T : TTag;

begin
  T:=TTag.Create(OpenAPI,'tags[0]');
  T.Name:='123';
  OpenAPI.Tags.Add(T);
  T:=TTag.Create(OpenAPI,'tags[1]');
  T.Name:='456';
  OpenAPI.Tags.Add(T);
  TestWrite('multi','{ "tags" : [ { "name" : "123" },  { "name" : "456" } ] }');
end;

procedure TTestopenApiWriterExternalDocs.TestUrl;

begin
  OpenAPI.ExternalDocs.Url:='123';
  TestWrite('Url','{ "externalDocs" : { "url" :"123" } }');
end;

procedure TTestopenApiWriterExternalDocs.TestDescription;
begin
  OpenAPI.ExternalDocs.Description:='123';
  TestWrite('description','{ "externalDocs" : { "description" :"123" } }');
end;

{ TTestOpenApiWriterServers }

procedure TTestOpenApiWriterServers.TestUrl;

var
  Server : TServer;

begin
  Server:=TServer.Create(OpenAPI,'servers[0]');
  Server.Url:='123';
  OpenAPI.Servers.Add(Server);
  TestWrite('url','{ "servers" : [ { "url" :"123" } ] }');
end;

procedure TTestOpenApiWriterServers.TestDescription;

var
  Server : TServer;

begin
  Server:=TServer.Create(OpenAPI,'servers[0]');
  Server.description:='123';
  OpenAPI.Servers.Add(Server);
  TestWrite('url','{ "servers" : [ { "description" :"123" } ] }');
end;

procedure TTestOpenApiWriterServers.TestVariablesDefault;
var
  Server : TServer;
  ServerVar : TServerVariable;
begin
  Server:=TServer.Create(OpenAPI,'servers[0]');
  ServerVar:=Server.Variables.AddItem('user');
  ServerVar.Default:='123';
  OpenAPI.Servers.Add(Server);
  TestWrite('var.default','{ "servers" : [ { "variables" : { "user": { "default": "123" } } } ] }');
end;

procedure TTestOpenApiWriterServers.TestVariablesDescription;
var
  Server : TServer;
  ServerVar : TServerVariable;
begin
  Server:=TServer.Create(OpenAPI,'servers[0]');
  ServerVar:=Server.Variables.AddItem('user');
  ServerVar.Description:='123';
  OpenAPI.Servers.Add(Server);
  TestWrite('var.description','{ "servers" : [ { "variables" : { "user": { "description": "123" } } } ] }');
end;

procedure TTestOpenApiWriterServers.TestVariablesEnum;
var
  Server : TServer;
  ServerVar : TServerVariable;
begin
  Server:=TServer.Create(OpenAPI,'servers[0]');
  ServerVar:=Server.Variables.AddItem('user');
  ServerVar.Enum.Add('123');
  ServerVar.Enum.Add('456');
  OpenAPI.Servers.Add(Server);
  TestWrite('var.enum','{ "servers" : [ { "variables" : { "user": { "enum": [ "123", "456" ] } } } ] }');
end;

procedure TTestOpenApiWriterServers.Test2Url;
var
  Server : TServer;

begin
  Server:=TServer.Create(OpenAPI,'servers[0]');
  Server.Url:='123';
  OpenAPI.Servers.Add(Server);
  Server:=TServer.Create(OpenAPI,'servers[0]');
  Server.Url:='456';
  OpenAPI.Servers.Add(Server);
  TestWrite('url','{ "servers" : [ { "url" :"123" }, { "url" :"456" } ] }');
end;

{ TTestOpenApiWriterPaths }

function TTestOpenApiWriterPathBase.AddPath(aPath: String): TPathItem;
begin
  Result:=OpenAPI.Paths.AddItem(aPath);
end;

procedure TTestOpenApiWriterPaths.TestEmpty;

var
  P : TPathItem;
begin
  P:=AddPath('api');
  AssertNotNull('Have path',P);
  TestWrite('Path','{ "paths" : { "api" : {} } }');
end;

procedure TTestOpenApiWriterPaths.TestRef;
var
  P : TPathItem;
begin
  P:=AddPath('api');
  P.Ref:='abc';
  TestWrite('Path ref','{ "paths" : { "api" : { "$ref": "abc"} } }');
end;

procedure TTestOpenApiWriterPaths.TestSummary;
var
  P : TPathItem;
begin
  P:=AddPath('api');
  P.Summary:='abc';
  TestWrite('Path summary','{ "paths" : { "api" : { "summary": "abc"} } }');
end;

procedure TTestOpenApiWriterPaths.TestDescription;
var
  P : TPathItem;
begin
  P:=AddPath('api');
  P.description:='abc';
  TestWrite('Path description','{ "paths" : { "api" : { "description": "abc"} } }');
end;

procedure TTestOpenApiWriterPaths.TestGetServers;
var
  P : TPathItem;
  S : TServer;
begin
  P:=AddPath('api');
  S:=TServer.Create(P,'servers[0]');
  P.Servers.Add(S);
  TestWrite('Path description','{ "paths" : { "api" : { "servers": [ {} ] } } }');
end;

procedure TTestOpenApiWriterPaths.TestGetEmpty;
var
  P : TPathItem;
begin
  P:=AddPath('api');
  AssertEquals('Empty','',P.Get.Summary);
  TestWrite('Path get','{ "paths" : { "api" : { "get": { } } } }');
end;

procedure TTestOpenApiWriterPaths.TestGetSummary;
var
  P : TPathItem;
begin
  P:=AddPath('api');
  P.Get.Summary:='123';
  TestWrite('Path get summary','{ "paths" : { "api" : { "get": { "summary" : "123" } } } }');
end;

procedure TTestOpenApiWriterPaths.TestGetDescription;
var
  P : TPathItem;
begin
  P:=AddPath('api');
  P.Get.Description:='123';
  TestWrite('Path get description','{ "paths" : { "api" : { "get": { "description" : "123" } } } }');
end;

procedure TTestOpenApiWriterPaths.TestGetOperationId;
var
  P : TPathItem;
begin
  P:=AddPath('api');
  P.Get.operationId:='123';
  TestWrite('Path get operationId','{ "paths" : { "api" : { "get": { "operationId" : "123" } } } }');
end;

procedure TTestOpenApiWriterPaths.TestGetDeprecated;
var
  P : TPathItem;
begin
  P:=AddPath('api');
  P.Get.Deprecated:=True;
  TestWrite('Path get operationId','{ "paths" : { "api" : { "get": { "deprecated" : true } } } }');
end;

procedure TTestOpenApiWriterPaths.TestGetExternalDocsEmpty;
var
  P : TPathItem;
begin
  P:=AddPath('api');
  AssertNotNull('Get creates',P.Get.ExternalDocs);
  TestWrite('Path get operationId','{ "paths" : { "api" : { "get": { "externalDocs" : {  } } } } }');
end;

procedure TTestOpenApiWriterPaths.TestGetExternalDocsURL;
var
  P : TPathItem;
begin
  P:=AddPath('api');
  P.Get.ExternalDocs.Url:='123';
  TestWrite('Path get externaldocs','{ "paths" : { "api" : { "get": { "externalDocs" : { "url" : "123" } } } } }');
end;

{ TTestOpenApiWriterPathRequestBody }

function TTestOpenApiWriterPathRequestBody.AddBody(aPath: String): TRequestBody;

begin
  Result:=AddPath(aPath).Get.RequestBody;
end;

procedure TTestOpenApiWriterPathRequestBody.TestEmpty;

var
  R : TRequestBody;
begin
  R:=AddBody('api');
  AssertNotNull('Get creates',R);
  TestWrite('Path get requestbody','{ "paths" : { "api" : { "get": { "requestBody" : {  } } } } }');
end;

procedure TTestOpenApiWriterPathRequestBody.TestDescription;

var
  R : TRequestBody;
begin
  R:=AddBody('api');
  R.Description:='123';
  TestWrite('Path get requestbody','{ "paths" : { "api" : { "get": { "requestBody" : { "description" : "123" } } } } }');
end;

procedure TTestOpenApiWriterPathRequestBody.TestRequired;
var
  R : TRequestBody;
begin
  R:=AddBody('api');
  R.Required:=True;
  TestWrite('Path get requestbody','{ "paths" : { "api" : { "get": { "requestBody" : { "required" : true} } } } }');
end;



procedure TTestOpenApiWriterPathRequestBody.TestContent;

var
  R : TRequestBody;
begin
  R:=AddBody('api');
  R.Content.AddItem('123');
  TestWrite('Path get operationId','{ "paths" : { "api" : { "get": { "requestBody" : { "content" : { "123" : { } } } } } } }');
end;

procedure TTestOpenApiWriterPathRequestBody.TestContentSchema;

var
  R : TRequestBody;
begin
  R:=AddBody('api');
  R.Content.AddItem('123').Schema:=TJSONSchema.Create();
  TestWrite('Path get operationId','{ "paths" : { "api" : { "get": { "requestBody" : { "content" : { "123" : { "schema" : true } } } } } } }');
end;

procedure TTestOpenApiWriterPathRequestBody.TestContentExample;
var
  R : TRequestBody;
begin
  R:=AddBody('api');
  R.Content.AddItem('123').example:=TJSONObject.Create;
  TestWrite('Path get operationId','{ "paths" : { "api" : { "get": { "requestBody" : { "content" : { "123" : { "example" : { } } } } } } } }');
end;

procedure TTestOpenApiWriterPathRequestBody.TestContentExamples;
var
  R : TRequestBody;
begin
  R:=AddBody('api');
  R.Content.AddItem('123').examples.AddItem('abc').Summary:='x';
  TestWrite('Path get operationId','{ "paths" : { "api" : { "get": { "requestBody" : { "content" : { "123" : { "examples" : { "abc" : { "summary": "x"} } } } } } } } }');
end;

procedure TTestOpenApiWriterPathRequestBody.TestContentEncodingContentType;
var
  R : TRequestBody;
begin
  R:=AddBody('api');
  R.Content.AddItem('123').Encoding.AddItem('abc').ContentType:='def';
  TestWrite('Path get operationId','{ "paths" : { "api" : { "get": { "requestBody" : { "content" : { "123" : { "encoding" : { "abc" : { "contentType": "def"} } } } } } } } }');
end;

procedure TTestOpenApiWriterPathRequestBody.TestContentEncodingExplode;
var
  R : TRequestBody;
begin
  R:=AddBody('api');
  R.Content.AddItem('123').Encoding.AddItem('abc').Explode:=True;
  TestWrite('Path get operationId','{ "paths" : { "api" : { "get": { "requestBody" : { "content" : { "123" : { "encoding" : { "abc" : { "explode": true} } } } } } } } }');
end;

procedure TTestOpenApiWriterPathRequestBody.TestContentEncodingStyle;
var
  R : TRequestBody;
begin
  R:=AddBody('api');
  R.Content.AddItem('123').Encoding.AddItem('abc').Style:='def';
  TestWrite('Path get operationId','{ "paths" : { "api" : { "get": { "requestBody" : { "content" : { "123" : { "encoding" : { "abc" : { "style": "def"} } } } } } } } }');
end;

procedure TTestOpenApiWriterPathRequestBody.TestContentEncodingAllowReserved;
var
  R : TRequestBody;
begin
  R:=AddBody('api');
  R.Content.AddItem('123').Encoding.AddItem('abc').AllowReserved:=True;
  TestWrite('Path get operationId','{ "paths" : { "api" : { "get": { "requestBody" : { "content" : { "123" : { "encoding" : { "abc" : { "allowReserved": true} } } } } } } } }');
end;

procedure TTestOpenApiWriterPathRequestBody.TestContentEncodingHeaders;
var
  R : TRequestBody;
begin
  R:=AddBody('api');
  R.Content.AddItem('123').Encoding.AddItem('abc').Headers.AddItem('def').Explode:=True;
  TestWrite('Path get operationId','{ "paths" : { "api" : { "get": { "requestBody" : { "content" : { "123" : { "encoding" : { "abc" : { "headers": { "def" : { "explode" : true } } } } } } } } } } }');
end;

function TTestOpenApiWriterPathParameters.AddParam(aPath: String): TParameter;

var
  P : TPathItem;

begin
  P:=addPath(aPath);
  Result:=P.Parameters.AddParam(P);
end;

procedure TTestOpenApiWriterPathParameters.TestGetParametersEmpty;

var
  P : TParameter;

begin
  P:=AddParam('api');
  AssertNotNull('have p',p);
  TestWrite('Path param','{ "paths" : { "api" : { "parameters": [ {} ] } } }');
end;

function TTestOpenApiWriterPathResponses.AddResponse(aPath: String): TResponse;
begin
  Result:=AddPath(aPath).Get.Responses.AddItem('default');
end;

procedure TTestOpenApiWriterPathResponses.TestEmpty;
begin
  AssertNotNull('Have response',AddResponse('api'));
  TestWrite('Path param','{ "paths" : { "api" : { "get" : { "responses": { "default" : {} } } } } }');
end;

procedure TTestOpenApiWriterPathResponses.TestHeaders;
begin
  AddResponse('api').Headers.AddItem('x-content');
  TestWrite('Path param','{ "paths" : { "api" : { "get" : { "responses": { "default" : { "headers" : { "x-content" : { } } } } } } } }');
end;

procedure TTestOpenApiWriterPathResponses.TestDescription;
begin
  AddResponse('api').Description:='abc';
  TestWrite('Path param','{ "paths" : { "api" : { "get" : { "responses": { "default" : { "description" : "abc" } } } } } }');
end;

procedure TTestOpenApiWriterPathResponses.TestContentEmpty;
begin
  AddResponse('api').content.AddItem('application/json');
  TestWrite('Path param','{ "paths" : { "api" : { "get" : { "responses": { "default" : { "content" : { "application/json" : { } } } } } } } }');
end;

procedure TTestOpenApiWriterPathResponses.TestContentSchema;
begin
  AssertNotNull('Have schema',AddResponse('api').content.AddItem('application/json').Schema);
  TestWrite('Path param','{ "paths" : { "api" : { "get" : { "responses": { "default" : { "content" : { "application/json" : { "schema" : true } } } } } } } }');
end;

procedure TTestOpenApiWriterPathResponses.TestContentExample;
begin
  AddResponse('api').content.AddItem('application/json').Example:=TJSONObject.Create;
  TestWrite('Path param','{ "paths" : { "api" : { "get" : { "responses": { "default" : { "content" : { "application/json" : { "example" : {} } } } } } } } }');
end;

procedure TTestOpenApiWriterPathResponses.TestContentExamples;
begin
  AddResponse('api').content.AddItem('application/json').Examples.AddItem('abc').Summary:='x';
  TestWrite('Path param','{ "paths" : { "api" : { "get" : { "responses": { "default" : { "content" : { "application/json" : { "examples" : { "abc" : { "summary": "x"} } } } } } } } } }');
end;

procedure TTestOpenApiWriterPathResponses.TestContentEncodingEmpty;
begin
  AddResponse('api').content.AddItem('application/json').Encoding.AddItem('abc'); //.ContentType:='application/json';
  TestWrite('Path param','{ "paths" : { "api" : { "get" : { "responses": { "default" : { "content" : { "application/json" : { "encoding" : { "abc" : { } } } } } } } } } }');

end;

procedure TTestOpenApiWriterPathResponses.TestContentEncodingContentType;
begin
  AddResponse('api').content.AddItem('application/json').Encoding.AddItem('abc').ContentType:='application/json';
  TestWrite('Path param','{ "paths" : { "api" : { "get" : { "responses": { "default" : { "content" : { "application/json" : { "encoding" : { "abc" : { "contentType" : "application/json" } } } } } } } } } }');
end;

{ TTestOpenApiWriterPathResponsesLinks }

function TTestOpenApiWriterPathResponsesLinks.AddLink(aPath: String): TLink;
begin
  Result:=AddPath(aPath).Get.Responses.AddItem('default').links.AddItem('abc');
end;

procedure TTestOpenApiWriterPathResponsesLinks.TestEmpty;
begin
  AddLink('api');
  TestWrite('Path param','{ "paths" : { "api" : { "get" : { "responses" : { "default" : { "links": { "abc" : { } } } } } } } }');
end;

procedure TTestOpenApiWriterPathResponsesLinks.TestOperatonRef;
begin
  AddLink('api').OperationRef:='123';
  TestWrite('Path param','{ "paths" : { "api" : { "get" : { "responses" : { "default" : { "links": { "abc" : { "operationRef" : "123" } } } } } } } }');
end;

procedure TTestOpenApiWriterPathResponsesLinks.TestOperatonId;
begin
  AddLink('api').OperationId:='123';
  TestWrite('Path param','{ "paths" : { "api" : { "get" : { "responses" : { "default" : { "links": { "abc" : { "operationId" : "123" } } } } } } } }');
end;

procedure TTestOpenApiWriterPathResponsesLinks.TestParameters;
begin
  AddLink('api').Parameters.Add('x','y');
  TestWrite('Path param','{ "paths" : { "api" : { "get" : { "responses" : { "default" : { "links": { "abc" : { "parameters" : { "x" : "y" } } } } } } } } }');
end;

procedure TTestOpenApiWriterPathResponsesLinks.TestRequestBody;
begin
  AddLink('api').RequestBody:=TJSONObject.Create();
  TestWrite('Path param','{ "paths" : { "api" : { "get" : { "responses" : { "default" : { "links": { "abc" : { "requestBody" : {} } } } } } } } }');
end;

procedure TTestOpenApiWriterPathResponsesLinks.TestDescription;
begin
  AddLink('api').Description:='123';
  TestWrite('Path param','{ "paths" : { "api" : { "get" : { "responses" : { "default" : { "links": { "abc" : { "description" : "123" } } } } } } } }');
end;

procedure TTestOpenApiWriterPathResponsesLinks.TestServer;
begin
  AddLink('api').Server.Url:='123';
  TestWrite('Path param','{ "paths" : { "api" : { "get" : { "responses" : { "default" : { "links": { "abc" : { "server" : { "url" : "123" } } } } } } } } }');
end;

procedure TTestOpenApiWriterPathSecurity.TestEmpty;

var
  O : TAPIOperation;

begin
  O:=AddPath('api').Get;
  O.Security.AddSecurity(O);
  TestWrite('Path param','{ "paths" : { "api" : { "get" : { "security" : [ {} ] } } } }');
end;

procedure TTestOpenApiWriterPathSecurity.TestAPIKey;
var
  O : TAPIOperation;

begin
  O:=AddPath('api').Get;
  O.Security.AddSecurity(O).AddItem('api_key').Add('akey');
  TestWrite('Path param','{ "paths" : { "api" : { "get" : { "security" : [ { "api_key" : ["akey"] } ] } } } }');
end;

{ TTestOpenApiWriterWebHooks }

function TTestOpenApiWriterWebHooks.AddWebHook(const aname: string): TPathItem;
begin
  Result:=OpenAPI.WebHooks.AddItem(aName);
end;

procedure TTestOpenApiWriterWebHooks.TestEmpty;
begin
  AssertNotNull(OpenAPI.WebHooks.AddItem('newitem'));
  TestWrite('Path param','{ "webhooks" : { "newitem" : { } } }');
end;

procedure TTestOpenApiWriterWebHooks.TestSummary;
begin
  OpenAPI.WebHooks.AddItem('newitem').Summary:='abc';
  TestWrite('Path param','{ "webhooks" : { "newitem" : { "summary" : "abc" } } }');
end;

{ TTestOpenApiWriterComponents }

function TTestOpenApiWriterComponents.AddComponentSchema(const aname: string): TJSONSchema;
begin
  Result:=OpenAPI.Components.Schemas.Add(aName);
end;

procedure TTestOpenApiWriterComponents.TestSchemaEmpty;
begin
  AssertNotNull('Schema',AddComponentSchema('abc'));
  TestWrite('Path componentschema','{ "components" : { "schemas" : { "abc" : true } } }');
end;

procedure TTestOpenApiWriterComponents.TestSchemaContentEncoding;
begin
  AddComponentSchema('abc').Validations.contentEncoding:='utf8';
  TestWrite('Path componentschema','{ "components" : { "schemas" : { "abc" : { "contentEncoding" : "utf8" } } } }');
end;

{ TTestOpenApiWriterOAuth2Scopes }

procedure TTestOpenApiWriterOAuth2Scopes.TestOAuth2ScopesAsObject;
var
  Scheme: TSecuritySchemeOrReference;
  Flow: TOAuthFlow;
begin
  Scheme := OpenAPI.Components.SecuritySchemes.AddItem('oauth2');
  Scheme.Type_ := 'oauth2';
  Flow := Scheme.Flows.ClientAuthorizationCode;
  Flow.AuthorizationUrl := 'https://example.com/auth';
  Flow.TokenURL := 'https://example.com/token';
  Flow.Scopes.Add('read:user=Read user data');
  Flow.Scopes.Add('write:user=Write user data');
  TestWrite('OAuth2 scopes as object','{ "components" : { "securitySchemes" : { "oauth2" : { "type" : "oauth2", "flows" : { "authorizationCode" : { "authorizationUrl" : "https://example.com/auth", "tokenUrl" : "https://example.com/token", "scopes" : { "read:user" : "Read user data", "write:user" : "Write user data" } } } } } } }');
end;

procedure TTestOpenApiWriterOAuth2Scopes.TestOAuth2ScopesEmpty;
var
  Scheme: TSecuritySchemeOrReference;
  Flow: TOAuthFlow;
begin
  Scheme := OpenAPI.Components.SecuritySchemes.AddItem('oauth2');
  Scheme.Type_ := 'oauth2';
  Flow := Scheme.Flows.ClientAuthorizationCode;
  Flow.AuthorizationUrl := 'https://example.com/auth';
  Flow.TokenURL := 'https://example.com/token';
  // Access Scopes to ensure it's initialized but leave it empty
  AssertEquals('Scopes count', 0, Flow.Scopes.Count);
  TestWrite('OAuth2 empty scopes','{ "components" : { "securitySchemes" : { "oauth2" : { "type" : "oauth2", "flows" : { "authorizationCode" : { "authorizationUrl" : "https://example.com/auth", "tokenUrl" : "https://example.com/token", "scopes" : { } } } } } } }');
end;

{ TTestOpenApiWriterSecurity }

procedure TTestOpenApiWriterSecurity.TestOne;
begin
  OpenAPI.Security.AddSecurity(OpenAPI).AddItem('abc');
  TestWrite('Security','{ "security" : [ { "abc" : [] } ] }');
end;

procedure StartSources;

begin
  _Sources:=TStringList.Create;
  _Decl:=TStringList.Create;
  _Decl.Add('unit ReadTests;');
  _Decl.Add('');
  _Decl.Add('interface');
  _Decl.Add('');
  _Decl.Add('uses fpcunit;');
  _Decl.Add('');
  _Decl.Add('type');
  _Decl.Add('');
end;

procedure WriteSources;

begin
  _Decl.Add('  end;');
  _Decl.Add('');
  _Decl.Add('implementation');
  _Decl.Add('');
  _Decl.AddStrings(_Sources);
  _Decl.Add('');
  _Decl.Add('end.');
  _Decl.SaveToFile('ReadTests.pp');
  _Sources.Free;
  _Decl.Free;
end;

initialization
//  StartSources;
  RegisterTests('Writer',[
    TTestopenApiWriterOpenAPI,
    TTestopenApiWriterInfo,
    TTestopenApiWriterTags,
    TTestOpenApiWriterExternalDocs,
    TTestOpenApiWriterServers,
    TTestOpenApiWriterPaths,
    TTestOpenApiWriterPathRequestBody,
    TTestOpenApiWriterPathParameters,
    TTestOpenApiWriterPathResponses,
    TTestOpenApiWriterPathSecurity,
    TTestOpenApiWriterPathResponsesLinks,
    TTestOpenApiWriterWebHooks,
    TTestOpenApiWriterComponents,
    TTestOpenApiWriterOAuth2Scopes,
    TTestOpenApiWriterSecurity
  ]);

finalization
  if assigned(_Decl) then
    WriteSources;
end.

