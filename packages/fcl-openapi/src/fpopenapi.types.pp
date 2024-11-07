{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2024 by Michael Van Canneyt (michael@freepascal.org)

    Basic OpenAPI types

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit fpopenapi.types;

{$mode ObjFPC}{$H+}
{$modeswitch typehelpers}

interface

uses
  {$IFDEF FPC_DOTTEDUNITS}
  System.SysUtils, System.Classes;
  {$ELSE}
  sysutils, classes;
  {$ENDIF}

Type
  // OpenApi object
  TOpenAPIKeyword = (oakUnknown, oakOpenApi, oakInfo, oakJSONSchemaDialect, oakServers, oakPaths,
                     oakWebHooks, oakComponents, oakSecurity, oakTags, oakExternalDocs);
  // Info Object
  TInfoKeyword = (ikUnknown, ikTitle, ikSummary, ikDescription, ikTermsOfService,
                  ikContact,ikLicense, ikVersion);

  // Contact object
  TContactKeyword = (cokUnknown, cokName,cokUrl,cokEmail);

  // License Object
  TLicenseKeyword = (lkUnknown, lkName, lkIdentifier, lkUrl);

  // Server object
  TServerKeyword = (skUnknown, skServerUrl,skDescription,skVariables);

  // Server Variable object
  TServerVariableKeyword = (svkUnknown, svkEnum,svkDefault,svkDescription);

  // Components object
  TComponentsKeyword = (ckUnknown, ckSchemas, ckResponses, ckParameters, ckExamples, ckRequestBodies, ckHeaders,
                        ckSecuritySchemes,ckLinks,ckCallbacks,ckPathItems);

  // Path item object
  TPathItemKeyword = (pkUnknown, pkRef, pkSummary, pkDescription, pkGet, pkPut, pkPost, pkDelete, pkOptions,
                      pkHead, pkPatch, pkTrace, pkServers, pkParameters);

  TPathItemOperationKeyword = pkGet..pkTrace;

  // Operation object
  TApiOperationKeyword = (okUnknown, okTags, okSummary, okDescription, okExternalDocs, okOperationId, okParameters, okRequestBody,
                       okResponses, okCallbacks, okDeprecated, okSecurity, okServers);
  // External documentaton object
  TExternalDocumentationKeyword = (edkUnknown, edkDescription, edkUrl);

  // Parameter object
  TParameterKeyword = (pakUnknown, pakName, pakIn, pakDescription, pakRequired, pakDeprecated, pakAllowEmptyValue,
                       pakStyle, pakExplode, pakAllowReserved, pakSchema, pakExample, pakExamples,
                       pakContent);

  // ParameterStyle
  TParameterStyleKeyword = (pskUnknown, pskMatrix, pskLabel, pskForm, pskSimple, pskSpaceDelimited, pskPipeDelimited, pskDeepObject);

  // Request body object
  TRequestBodyKeyword = (rbkUnknown, rbkDescription, rbkContent, rbkRequired);

  // Media Type Object
  TMediaTypeKeyword = (mtkUnknown, mtkSchema, mtkExample, mtkExamples, mtkEncoding);

  // Encoding object
  TEncodingKeyword = (eckUnknown, eckContentType, eckHeaders, eckStyle, eckExplode, eckAllowReserved);

  // Responses Object
  TResponsesKeyword = (rskUnknown, rskDefault);

  // Response Object
  TResponseKeyword = (rkUnknown, rkDescription, rkHeaders, rkContent, rkLinks);

  // Example object
  TExampleKeyword = (exkUnknown, exkSummary, exkDescription, exkValue, exkExternalValue);

  // Link Object
  TLinkKeyword = (likUnknown, likOperationRef, likOperationId, likParameters, likRequestBody, likDescription, likServer);

  // Tag Object
  TTagKeyword = (tkUnknown, tkName, tkDescription, tkExternalDocs);

  // Reference Object
  TReferenceKeyword = (rfkUnknown, rfkRef, rfkSummary, rfkDescription);

  // Schema object
  TSchemaKeyword = (sckUnknown, sckDiscriminator, sckXML, sckExternalDocs, sckExample);

  TDiscriminatorKeyword = (dikUnknown, dikPropertyName, dikMapping);

  // XML Object
  TXMLKeyword = (xmkUnknown, xmkName, xmkNamespace, xmkPrefix, xmkAttribute, xmkWrapped);

  // Security scheme object
  TSecuritySchemeKeyword = (sskUnknown, sskType, sskDescription, sskName, sskIn, sskScheme, sskBearerFormat, sskFlows, sskOpenIdConnectUrl);

  // Oauth Flows object
  TOAuthFlowsKeyword = (ofskUnknown, ofskImplicit, ofskPassword, ofskClientCredentials, ofskClientAuthorizationCode);

  // Oauth Flow object
  TOauthFlowKeyword = (ofkUnknown, ofkAuthorizationUrl, ofkTokenURL, ofkRefreshURL, ofkScopes);

  // Pseudo objects
{  THeaderKeyword = (hekUnknown, hekHeader);
  TCallbackKeyword = (cbkUnknown, cbkCallback);}
  TSecurityRequirementKeyword = (srkUnknown, srkSecurityRequirement);

  TOpenAPIKeywords = set of TOpenAPIKeyword;
  TInfoKeywords = set of TInfoKeyword;
  TContactKeywords = set of TContactKeyword;
  TLicenseKeywords = set of TLicenseKeyword;
  TServerKeywords = set of TServerKeyword;
  TServerVariableKeywords = set of TServerVariableKeyword;
  TComponentsKeywords = set of TComponentsKeyword;
  TPathItemKeywords = set of TPathItemKeyword;
  TOperationKeywords = set of TAPiOperationKeyword;
  TApiOperationKeywords = TOperationKeywords;
  TExternalDocumentationKeywords = set of TExternalDocumentationKeyword;
  TParameterKeywords = set of TParameterKeyword;
  TParameterStyleKeywords = set of TParameterStyleKeyword;
  TRequestBodyKeywords = set of TRequestBodyKeyword;
  TMediaTypeKeywords = set of TMediaTypeKeyword;
  TEncodingKeywords = set of TEncodingKeyword;
  TResponsesKeywords = set of TResponsesKeyword;
  TResponseKeywords = set of TResponseKeyword;
  TExampleKeywords = set of TExampleKeyword;
  TLinkKeywords = set of TLinkKeyword;
  TTagKeywords = set of TTagKeyword;
  TReferenceKeywords = set of TReferenceKeyword;
  TSchemaKeywords = set of TSchemaKeyword;
  TDiscriminatorKeywords = set of TDiscriminatorKeyword;
  TXMLKeywords = set of TXMLKeyword;
  TSecuritySchemeKeywords = set of TSecuritySchemeKeyword;
  TOAuthFlowsKeywords = set of TOAuthFlowsKeyword;
  TOauthFlowKeywords = set of TOauthFlowKeyword;
  TSecurityRequirementKeywords = set of TSecurityRequirementKeyword;
//  THeaderKeywords = set of THeaderKeyword;
//  TCallbackKeywords = set of TCallbackKeyword;

  TOpenAPIKeywordHelper = type helper for TOpenAPIKeyword
  Private
    procedure SetAsString(const aValue : string);
  public
    function ToString : String;
    class function fromString(const aValue : string) : TOpenAPIKeyword; static;
    property AsString : string read toString Write SetAsString;
  end;

  TInfoKeywordHelper = type helper for TInfoKeyword
  Private
    procedure SetAsString(const aValue : string);
  public
    function ToString : String;
    class function fromString(const aValue : string) : TInfoKeyword; static;
    property AsString : string read toString Write SetAsString;
  end;

  TContactKeywordHelper = type helper for TContactKeyword
  Private
    procedure SetAsString(const aValue : string);
  public
    function ToString : String;
    class function fromString(const aValue : string) : TContactKeyword; static;
    property AsString : string read toString Write SetAsString;
  end;

  TLicenseKeywordHelper = type helper for TLicenseKeyword
  Private
    procedure SetAsString(const aValue : string);
  public
    function ToString : String;
    class function fromString(const aValue : string) : TLicenseKeyword; static;
    property AsString : string read toString Write SetAsString;
  end;

  TServerKeywordHelper = type helper for TServerKeyword
  Private
    procedure SetAsString(const aValue : string);
  public
    function ToString : String;
    class function fromString(const aValue : string) : TServerKeyword; static;
    property AsString : string read toString Write SetAsString;
  end;

  TServerVariableKeywordHelper = type helper for TServerVariableKeyword
  Private
    procedure SetAsString(const aValue : string);
  public
    function ToString : String;
    class function fromString(const aValue : string) : TServerVariableKeyword; static;
    property AsString : string read toString Write SetAsString;
  end;

  TComponentsKeywordHelper = type helper for TComponentsKeyword
  Private
    procedure SetAsString(const aValue : string);
  public
    function ToString : String;
    class function fromString(const aValue : string) : TComponentsKeyword; static;
    property AsString : string read toString Write SetAsString;
  end;

  TPathItemKeywordHelper = type helper for TPathItemKeyword
  Private
    procedure SetAsString(const aValue : string);
  public
    function ToString : String;
    class function fromString(const aValue : string) : TPathItemKeyword; static;
    property AsString : string read toString Write SetAsString;
  end;

  TOperationKeywordHelper = type helper for TApiOperationKeyword
  Private
    procedure SetAsString(const aValue : string);
  public
    function ToString : String;
    class function fromString(const aValue : string) : TApiOperationKeyword; static;
    property AsString : string read toString Write SetAsString;
  end;

  TExternalDocumentationKeywordHelper = type helper for TExternalDocumentationKeyword
  Private
    procedure SetAsString(const aValue : string);
  public
    function ToString : String;
    class function fromString(const aValue : string) : TExternalDocumentationKeyword; static;
    property AsString : string read toString Write SetAsString;
  end;

  TParameterKeywordHelper = type helper for TParameterKeyword
  Private
    procedure SetAsString(const aValue : string);
  public
    function ToString : String;
    class function fromString(const aValue : string) : TParameterKeyword; static;
    property AsString : string read toString Write SetAsString;
  end;

  TParameterStyleKeywordHelper = type helper for TParameterStyleKeyword
  Private
    procedure SetAsString(const aValue : string);
  public
    function ToString : String;
    class function fromString(const aValue : string) : TParameterStyleKeyword; static;
    property AsString : string read toString Write SetAsString;
  end;

  TRequestBodyKeywordHelper = type helper for TRequestBodyKeyword
  Private
    procedure SetAsString(const aValue : string);
  public
    function ToString : String;
    class function fromString(const aValue : string) : TRequestBodyKeyword; static;
    property AsString : string read toString Write SetAsString;
  end;

  TMediaTypeKeywordHelper = type helper for TMediaTypeKeyword
  Private
    procedure SetAsString(const aValue : string);
  public
    function ToString : String;
    class function fromString(const aValue : string) : TMediaTypeKeyword; static;
    property AsString : string read toString Write SetAsString;
  end;

  TEncodingKeywordHelper = type helper for TEncodingKeyword
  Private
    procedure SetAsString(const aValue : string);
  public
    function ToString : String;
    class function fromString(const aValue : string) : TEncodingKeyword; static;
    property AsString : string read toString Write SetAsString;
  end;

  TResponsesKeywordHelper = type helper for TResponsesKeyword
  Private
    procedure SetAsString(const aValue : string);
  public
    function ToString : String;
    class function fromString(const aValue : string) : TResponsesKeyword; static;
    property AsString : string read toString Write SetAsString;
  end;

  TResponseKeywordHelper = type helper for TResponseKeyword
  Private
    procedure SetAsString(const aValue : string);
  public
    function ToString : String;
    class function fromString(const aValue : string) : TResponseKeyword; static;
    property AsString : string read toString Write SetAsString;
  end;

  TExampleKeywordHelper = type helper for TExampleKeyword
  Private
    procedure SetAsString(const aValue : string);
  public
    function ToString : String;
    class function fromString(const aValue : string) : TExampleKeyword; static;
    property AsString : string read toString Write SetAsString;
  end;

  TLinkKeywordHelper = type helper for TLinkKeyword
  Private
    procedure SetAsString(const aValue : string);
  public
    function ToString : String;
    class function fromString(const aValue : string) : TLinkKeyword; static;
    property AsString : string read toString Write SetAsString;
  end;

  TTagKeywordHelper = type helper for TTagKeyword
  Private
    procedure SetAsString(const aValue : string);
  public
    function ToString : String;
    class function fromString(const aValue : string) : TTagKeyword; static;
    property AsString : string read toString Write SetAsString;
  end;

  TReferenceKeywordHelper = type helper for TReferenceKeyword
  Private
    procedure SetAsString(const aValue : string);
  public
    function ToString : String;
    class function fromString(const aValue : string) : TReferenceKeyword; static;
    property AsString : string read toString Write SetAsString;
  end;

  TSchemaKeywordHelper = type helper for TSchemaKeyword
  Private
    procedure SetAsString(const aValue : string);
  public
    function ToString : String;
    class function fromString(const aValue : string) : TSchemaKeyword; static;
    property AsString : string read toString Write SetAsString;
  end;

  TDiscriminatorKeywordHelper = type helper for TDiscriminatorKeyword
  Private
    procedure SetAsString(const aValue : string);
  public
    function ToString : String;
    class function fromString(const aValue : string) : TDiscriminatorKeyword; static;
    property AsString : string read toString Write SetAsString;
  end;

  TXMLKeywordHelper = type helper for TXMLKeyword
  Private
    procedure SetAsString(const aValue : string);
  public
    function ToString : String;
    class function fromString(const aValue : string) : TXMLKeyword; static;
    property AsString : string read toString Write SetAsString;
  end;

  TSecuritySchemeKeywordHelper = type helper for TSecuritySchemeKeyword
  Private
    procedure SetAsString(const aValue : string);
  public
    function ToString : String;
    class function fromString(const aValue : string) : TSecuritySchemeKeyword; static;
    property AsString : string read toString Write SetAsString;
  end;

  TOAuthFlowsKeywordHelper = type helper for TOAuthFlowsKeyword
  Private
    procedure SetAsString(const aValue : string);
  public
    function ToString : String;
    class function fromString(const aValue : string) : TOAuthFlowsKeyword; static;
    property AsString : string read toString Write SetAsString;
  end;

  TOauthFlowKeywordHelper = type helper for TOauthFlowKeyword
  Private
    procedure SetAsString(const aValue : string);
  public
    function ToString : String;
    class function fromString(const aValue : string) : TOauthFlowKeyword; static;
    property AsString : string read toString Write SetAsString;
  end;


implementation

uses fpopenapi.consts;

Const
  OpenAPIKeywordNames : Array[TOpenAPIKeyword] of string = (
    '?',
    KWOpenAPIOpenApi,
    KWOpenAPIInfo,
    KWOpenAPIJSONSchemaDialect,
    KWOpenAPIServers,
    KWOpenAPIPaths,
    KWOpenAPIWebHooks,
    KWOpenAPIComponents,
    KWOpenAPISecurity,
    KWOpenAPITags,
    KWOpenAPIExternalDocs
  );

  InfoKeywordNames : Array[TInfoKeyword] of string = (
    '?',
    KWInfoTitle,
    KWInfoSummary,
    KWInfoDescription,
    KWInfoTermsOfService,
    KWInfoContact,
    KWInfoLicense,
    KWInfoVersion
  );

  ContactKeywordNames : Array[TContactKeyword] of string = (
    '?',
    KWContactName,
    KWContactUrl,
    KWContactEmail
  );

  LicenseKeywordNames : Array[TLicenseKeyword] of string = (
    '?',
    KWLicenseName,
    KWLicenseIdentifier,
    KWLicenseUrl
  );

  ServerKeywordNames : Array[TServerKeyword] of string = (
    '?',
    KWServerUrl,
    KWServerDescription,
    KWServerVariables
  );

  ServerVariableKeywordNames : Array[TServerVariableKeyword] of string = (
    '?',
    KWServerVariableEnum,
    KWServerVariableDefault,
    KWServerVariableDescription
  );

  ComponentsKeywordNames : Array[TComponentsKeyword] of string = (
    '?',
    KWComponentsSchemas,
    KWComponentsResponses,
    KWComponentsParameters,
    KWComponentsExamples,
    KWComponentsRequestBodies,
    KWComponentsHeaders,
    KWComponentsSecuritySchemes,
    KWComponentsLinks,
    KWComponentsCallbacks,
    KWComponentsPathItems
  );

  PathItemKeywordNames : Array[TPathItemKeyword] of string = (
    '?',
    KWPathItemRef,
    KWPathItemSummary,
    KWPathItemDescription,
    KWPathItemGet,
    KWPathItemPut,
    KWPathItemPost,
    KWPathItemDelete,
    KWPathItemOptions,
    KWPathItemHead,
    KWPathItemPatch,
    KWPathItemTrace,
    KWPathItemServers,
    KWPathItemParameters
  );

  OperationKeywordNames : Array[TAPiOperationKeyword] of string = (
    '?',
    KWOperationTags,
    KWOperationSummary,
    KWOperationDescription,
    KWOperationExternalDocs,
    KWOperationOperationId,
    KWOperationParameters,
    KWOperationRequestBody,
    KWOperationResponses,
    KWOperationCallbacks,
    KWOperationDeprecated,
    KWOperationSecurity,
    KWOperationServers
  );

  ExternalDocsKeywordNames : Array[TExternalDocumentationKeyword] of string = (
    '?',
    KWExternalDocsDescription,
    KWExternalDocsUrl
  );

  ParameterKeywordNames : Array[TParameterKeyword] of string = (
    '?',
    KWParameterName,
    KWParameterIn,
    KWParameterDescription,
    KWParameterRequired,
    KWParameterDeprecated,
    KWParameterAllowEmptyValue,
    KWParameterStyle,
    KWParameterExplode,
    KWParameterAllowReserved,
    KWParameterSchema,
    KWParameterExample,
    KWParameterExamples,
    KWParameterContent
  );

  ParameterStyleNames : Array[TParameterStyleKeyword] of string = (
    '?',
    KWParameterStyleMatrix,
    KWParameterStyleLabel,
    KWParameterStyleForm,
    KWParameterStyleSimple,
    KWParameterStyleSpaceDelimited,
    KWParameterStylePipeDelimited,
    KWParameterStyleDeepObject
  );

  RequestBodyKeywordNames : Array[TRequestBodyKeyword] of string = (
    '?',
    KWRequestBodyDescription,
    KWRequestBodyContent,
    KWRequestBodyRequired
  );

  MediaTypeKeywordNames : Array[TMediaTypeKeyword] of string = (
    '?',
    KWMediaTypeSchema,
    KWMediaTypeExample,
    KWMediaTypeExamples,
    KWMediaTypeEncoding
  );

  EncodingKeywordNames : Array[TEncodingKeyword] of string = (
    '?',
    KWEncodingContentType,
    KWEncodingHeaders,
    KWEncodingStyle,
    KWEncodingExplode,
    KWEncodingAllowReserved
  );

  ResponsesKeywordNames : Array[TResponsesKeyword] of string = (
    '?',
    KWResponsesDefault
  );

  ResponseKeywordNames : Array[TResponseKeyword] of string = (
    '?',
    KWResponseDescription,
    KWResponseHeaders,
    KWResponseContent,
    KWResponseLinks
  );

  ExampleKeywordNames : Array[TExampleKeyword] of string = (
    '?',
    KWExampleSummary,
    KWExampleDescription,
    KWExampleValue,
    KWExampleExternalValue
  );

  LinkKeywordNames : Array[TLinkKeyword] of string = (
    '?',
    KWLinkOperationRef,
    KWLinkOperationId,
    KWLinkParameters,
    KWLinkRequestBody,
    KWLinkDescription,
    KWLinkServer
  );

  TagKeywordNames : Array[TTagKeyword] of string = (
    '?',
    KWTagName,
    KWTagDescription,
    KWTagExternalDocs
  );

  ReferenceKeywordNames : Array[TReferenceKeyword] of string = (
    '?',
    KWReferenceRef,
    KWReferenceSummary,
    KWReferenceDescription
  );

  SchemaKeywordNames : Array[TSchemaKeyword] of string = (
    '?',
    KWSchemaDiscriminator,
    KWSchemaXML,
    KWSchemaExternalDocs,
    KWSchemaExample
  );

  DiscriminatorKeywordNames : Array[TDiscriminatorKeyword] of string = (
    '?',
    KWDiscriminatorPropertyName,
    KWDiscriminatorMapping
  );

  XMLKeywordNames : Array[TXMLKeyword] of string = (
    '?',
    KWXMLName,
    KWXMLNamespace,
    KWXMLPrefix,
    KWXMLAttribute,
    KWXMLWrapped
  );

  SecuritySchemeKeywordNames : Array[TSecuritySchemeKeyword] of string = (
    '?',
    KWSecuritySchemeType,
    KWSecuritySchemeDescription,
    KWSecuritySchemeName,
    KWSecuritySchemeIn,
    KWSecuritySchemeScheme,
    KWSecuritySchemeBearerFormat,
    KWSecuritySchemeFlows,
    KWSecuritySchemeOpenIdConnectUrl
  );

  OAuthFlowsKeywordNames : Array[TOAuthFlowsKeyword] of string = (
    '?',
    KWOAuthFlowsImplicit,
    KWOAuthFlowsPassword,
    KWOAuthFlowsClientCredentials,
    KWOAuthFlowsClientAuthorizationCode
  );

  OauthFlowKeywordNames : Array[TOauthFlowKeyword] of string = (
    '?',
    KWOauthFlowAuthorizationUrl,
    KWOauthFlowTokenURL,
    KWOauthFlowRefreshURL,
    KWOauthFlowScopes
  );

procedure TOpenAPIKeywordHelper.SetAsString(const aValue : string);

var
  T : TOpenAPIKeyword;

begin
  Self:=Default(TOpenAPIKeyword);
  For T in TOpenAPIKeyword do
    if (T.AsString=aValue) then
      begin
      self:=T;
      exit;
      end;
end;


function TOpenAPIKeywordHelper.ToString : String;

begin
  Result:=OpenAPIKeywordNames[Self];
end;


class function TOpenAPIKeywordHelper.fromString(const aValue : string) : TOpenAPIKeyword;

begin
  Result:=Default(TOpenAPIKeyword);
  Result.AsString:=aValue;
end;


procedure TInfoKeywordHelper.SetAsString(const aValue : string);

var
  T : TInfoKeyword;

begin
  Self:=Default(TInfoKeyword);
  For T in TInfoKeyword do
    if (T.AsString=aValue) then
      begin
      self:=T;
      exit;
      end;
end;


function TInfoKeywordHelper.ToString : String;

begin
  Result:=InfoKeywordNames[Self];
end;


class function TInfoKeywordHelper.fromString(const aValue : string) : TInfoKeyword;

begin
  Result:=Default(TInfoKeyword);
  Result.AsString:=aValue;
end;


procedure TContactKeywordHelper.SetAsString(const aValue : string);

var
  T : TContactKeyword;

begin
  Self:=Default(TContactKeyword);
  For T in TContactKeyword do
    if (T.AsString=aValue) then
      begin
      self:=T;
      exit;
      end;
end;


function TContactKeywordHelper.ToString : String;

begin
  Result:=ContactKeywordNames[Self];
end;


class function TContactKeywordHelper.fromString(const aValue : string) : TContactKeyword;

begin
  Result:=Default(TContactKeyword);
  Result.AsString:=aValue;
end;


procedure TLicenseKeywordHelper.SetAsString(const aValue : string);

var
  T : TLicenseKeyword;

begin
  Self:=Default(TLicenseKeyword);
  For T in TLicenseKeyword do
    if (T.AsString=aValue) then
      begin
      self:=T;
      exit;
      end;
end;


function TLicenseKeywordHelper.ToString : String;

begin
  Result:=LicenseKeywordNames[Self];
end;


class function TLicenseKeywordHelper.fromString(const aValue : string) : TLicenseKeyword;

begin
  Result:=Default(TLicenseKeyword);
  Result.AsString:=aValue;
end;


procedure TServerKeywordHelper.SetAsString(const aValue : string);

var
  T : TServerKeyword;

begin
  Self:=Default(TServerKeyword);
  For T in TServerKeyword do
    if (T.AsString=aValue) then
      begin
      self:=T;
      exit;
      end;
end;


function TServerKeywordHelper.ToString : String;

begin
  Result:=ServerKeywordNames[Self];
end;


class function TServerKeywordHelper.fromString(const aValue : string) : TServerKeyword;

begin
  Result:=Default(TServerKeyword);
  Result.AsString:=aValue;
end;


procedure TServerVariableKeywordHelper.SetAsString(const aValue : string);

var
  T : TServerVariableKeyword;

begin
  Self:=Default(TServerVariableKeyword);
  For T in TServerVariableKeyword do
    if (T.AsString=aValue) then
      begin
      self:=T;
      exit;
      end;
end;


function TServerVariableKeywordHelper.ToString : String;

begin
  Result:=ServerVariableKeywordNames[Self];
end;


class function TServerVariableKeywordHelper.fromString(const aValue : string) : TServerVariableKeyword;

begin
  Result:=Default(TServerVariableKeyword);
  Result.AsString:=aValue;
end;


procedure TComponentsKeywordHelper.SetAsString(const aValue : string);

var
  T : TComponentsKeyword;

begin
  Self:=Default(TComponentsKeyword);
  For T in TComponentsKeyword do
    if (T.AsString=aValue) then
      begin
      self:=T;
      exit;
      end;
end;


function TComponentsKeywordHelper.ToString : String;

begin
  Result:=ComponentsKeywordNames[Self];
end;


class function TComponentsKeywordHelper.fromString(const aValue : string) : TComponentsKeyword;

begin
  Result:=Default(TComponentsKeyword);
  Result.AsString:=aValue;
end;


procedure TPathItemKeywordHelper.SetAsString(const aValue : string);

var
  T : TPathItemKeyword;

begin
  Self:=Default(TPathItemKeyword);
  For T in TPathItemKeyword do
    if (T.AsString=aValue) then
      begin
      self:=T;
      exit;
      end;
end;


function TPathItemKeywordHelper.ToString : String;

begin
  Result:=PathItemKeywordNames[Self];
end;


class function TPathItemKeywordHelper.fromString(const aValue : string) : TPathItemKeyword;

begin
  Result:=Default(TPathItemKeyword);
  Result.AsString:=aValue;
end;


procedure TOperationKeywordHelper.SetAsString(const aValue : string);

var
  T : TAPiOperationKeyword;

begin
  Self:=Default(TApiOperationKeyword);
  For T in TAPiOperationKeyword do
    if (T.AsString=aValue) then
      begin
      self:=T;
      exit;
      end;
end;


function TOperationKeywordHelper.ToString : String;

begin
  Result:=OperationKeywordNames[Self];
end;


class function TOperationKeywordHelper.fromString(const aValue : string) : TApiOperationKeyword;

begin
  Result:=Default(TAPiOperationKeyword);
  Result.AsString:=aValue;
end;


procedure TExternalDocumentationKeywordHelper.SetAsString(const aValue : string);

var
  T : TExternalDocumentationKeyword;

begin
  Self:=Default(TExternalDocumentationKeyword);
  For T in TExternalDocumentationKeyword do
    if (T.AsString=aValue) then
      begin
      self:=T;
      exit;
      end;
end;


function TExternalDocumentationKeywordHelper.ToString : String;

begin
  Result:=ExternalDocsKeywordNames[Self];
end;


class function TExternalDocumentationKeywordHelper.fromString(const aValue : string) : TExternalDocumentationKeyword;

begin
  Result:=Default(TExternalDocumentationKeyword);
  Result.AsString:=aValue;
end;


procedure TParameterKeywordHelper.SetAsString(const aValue : string);

var
  T : TParameterKeyword;

begin
  Self:=Default(TParameterKeyword);
  For T in TParameterKeyword do
    if (T.AsString=aValue) then
      begin
      self:=T;
      exit;
      end;
end;


function TParameterKeywordHelper.ToString : String;

begin
  Result:=ParameterKeywordNames[Self];
end;


class function TParameterKeywordHelper.fromString(const aValue : string) : TParameterKeyword;

begin
  Result:=Default(TParameterKeyword);
  Result.AsString:=aValue;
end;


procedure TParameterStyleKeywordHelper.SetAsString(const aValue : string);

var
  T : TParameterStyleKeyword;

begin
  Self:=Default(TParameterStyleKeyword);
  For T in TParameterStyleKeyword do
    if (T.AsString=aValue) then
      begin
      self:=T;
      exit;
      end;
end;


function TParameterStyleKeywordHelper.ToString : String;

begin
  Result:=ParameterStyleNames[Self];
end;


class function TParameterStyleKeywordHelper.fromString(const aValue : string) : TParameterStyleKeyword;

begin
  Result:=Default(TParameterStyleKeyword);
  Result.AsString:=aValue;
end;


procedure TRequestBodyKeywordHelper.SetAsString(const aValue : string);

var
  T : TRequestBodyKeyword;

begin
  Self:=Default(TRequestBodyKeyword);
  For T in TRequestBodyKeyword do
    if (T.AsString=aValue) then
      begin
      self:=T;
      exit;
      end;
end;


function TRequestBodyKeywordHelper.ToString : String;

begin
  Result:=RequestBodyKeywordNames[Self];
end;


class function TRequestBodyKeywordHelper.fromString(const aValue : string) : TRequestBodyKeyword;

begin
  Result:=Default(TRequestBodyKeyword);
  Result.AsString:=aValue;
end;


procedure TMediaTypeKeywordHelper.SetAsString(const aValue : string);

var
  T : TMediaTypeKeyword;

begin
  Self:=Default(TMediaTypeKeyword);
  For T in TMediaTypeKeyword do
    if (T.AsString=aValue) then
      begin
      self:=T;
      exit;
      end;
end;


function TMediaTypeKeywordHelper.ToString : String;

begin
  Result:=MediaTypeKeywordNames[Self];
end;


class function TMediaTypeKeywordHelper.fromString(const aValue : string) : TMediaTypeKeyword;

begin
  Result:=Default(TMediaTypeKeyword);
  Result.AsString:=aValue;
end;


procedure TEncodingKeywordHelper.SetAsString(const aValue : string);

var
  T : TEncodingKeyword;

begin
  Self:=Default(TEncodingKeyword);
  For T in TEncodingKeyword do
    if (T.AsString=aValue) then
      begin
      self:=T;
      exit;
      end;
end;


function TEncodingKeywordHelper.ToString : String;

begin
  Result:=EncodingKeywordNames[Self];
end;


class function TEncodingKeywordHelper.fromString(const aValue : string) : TEncodingKeyword;

begin
  Result:=Default(TEncodingKeyword);
  Result.AsString:=aValue;
end;


procedure TResponsesKeywordHelper.SetAsString(const aValue : string);

var
  T : TResponsesKeyword;

begin
  Self:=Default(TResponsesKeyword);
  For T in TResponsesKeyword do
    if (T.AsString=aValue) then
      begin
      self:=T;
      exit;
      end;
end;


function TResponsesKeywordHelper.ToString : String;

begin
  Result:=ResponsesKeywordNames[Self];
end;


class function TResponsesKeywordHelper.fromString(const aValue : string) : TResponsesKeyword;

begin
  Result:=Default(TResponsesKeyword);
  Result.AsString:=aValue;
end;


procedure TResponseKeywordHelper.SetAsString(const aValue : string);

var
  T : TResponseKeyword;

begin
  Self:=Default(TResponseKeyword);
  For T in TResponseKeyword do
    if (T.AsString=aValue) then
      begin
      self:=T;
      exit;
      end;
end;


function TResponseKeywordHelper.ToString : String;

begin
  Result:=ResponseKeywordNames[Self];
end;


class function TResponseKeywordHelper.fromString(const aValue : string) : TResponseKeyword;

begin
  Result:=Default(TResponseKeyword);
  Result.AsString:=aValue;
end;


procedure TExampleKeywordHelper.SetAsString(const aValue : string);

var
  T : TExampleKeyword;

begin
  Self:=Default(TExampleKeyword);
  For T in TExampleKeyword do
    if (T.AsString=aValue) then
      begin
      self:=T;
      exit;
      end;
end;


function TExampleKeywordHelper.ToString : String;

begin
  Result:=ExampleKeywordNames[Self];
end;


class function TExampleKeywordHelper.fromString(const aValue : string) : TExampleKeyword;

begin
  Result:=Default(TExampleKeyword);
  Result.AsString:=aValue;
end;


procedure TLinkKeywordHelper.SetAsString(const aValue : string);

var
  T : TLinkKeyword;

begin
  Self:=Default(TLinkKeyword);
  For T in TLinkKeyword do
    if (T.AsString=aValue) then
      begin
      self:=T;
      exit;
      end;
end;


function TLinkKeywordHelper.ToString : String;

begin
  Result:=LinkKeywordNames[Self];
end;


class function TLinkKeywordHelper.fromString(const aValue : string) : TLinkKeyword;

begin
  Result:=Default(TLinkKeyword);
  Result.AsString:=aValue;
end;


procedure TTagKeywordHelper.SetAsString(const aValue : string);

var
  T : TTagKeyword;

begin
  Self:=Default(TTagKeyword);
  For T in TTagKeyword do
    if (T.AsString=aValue) then
      begin
      self:=T;
      exit;
      end;
end;


function TTagKeywordHelper.ToString : String;

begin
  Result:=TagKeywordNames[Self];
end;


class function TTagKeywordHelper.fromString(const aValue : string) : TTagKeyword;

begin
  Result:=Default(TTagKeyword);
  Result.AsString:=aValue;
end;


procedure TReferenceKeywordHelper.SetAsString(const aValue : string);

var
  T : TReferenceKeyword;

begin
  Self:=Default(TReferenceKeyword);
  For T in TReferenceKeyword do
    if (T.AsString=aValue) then
      begin
      self:=T;
      exit;
      end;
end;


function TReferenceKeywordHelper.ToString : String;

begin
  Result:=ReferenceKeywordNames[Self];
end;


class function TReferenceKeywordHelper.fromString(const aValue : string) : TReferenceKeyword;

begin
  Result:=Default(TReferenceKeyword);
  Result.AsString:=aValue;
end;


procedure TSchemaKeywordHelper.SetAsString(const aValue : string);

var
  T : TSchemaKeyword;

begin
  Self:=Default(TSchemaKeyword);
  For T in TSchemaKeyword do
    if (T.AsString=aValue) then
      begin
      self:=T;
      exit;
      end;
end;


function TSchemaKeywordHelper.ToString : String;

begin
  Result:=SchemaKeywordNames[Self];
end;


class function TSchemaKeywordHelper.fromString(const aValue : string) : TSchemaKeyword;

begin
  Result:=Default(TSchemaKeyword);
  Result.AsString:=aValue;
end;


procedure TDiscriminatorKeywordHelper.SetAsString(const aValue : string);

var
  T : TDiscriminatorKeyword;

begin
  Self:=Default(TDiscriminatorKeyword);
  For T in TDiscriminatorKeyword do
    if (T.AsString=aValue) then
      begin
      self:=T;
      exit;
      end;
end;


function TDiscriminatorKeywordHelper.ToString : String;

begin
  Result:=DiscriminatorKeywordNames[Self];
end;


class function TDiscriminatorKeywordHelper.fromString(const aValue : string) : TDiscriminatorKeyword;

begin
  Result:=Default(TDiscriminatorKeyword);
  Result.AsString:=aValue;
end;


procedure TXMLKeywordHelper.SetAsString(const aValue : string);

var
  T : TXMLKeyword;

begin
  Self:=Default(TXMLKeyword);
  For T in TXMLKeyword do
    if (T.AsString=aValue) then
      begin
      self:=T;
      exit;
      end;
end;


function TXMLKeywordHelper.ToString : String;

begin
  Result:=XMLKeywordNames[Self];
end;


class function TXMLKeywordHelper.fromString(const aValue : string) : TXMLKeyword;

begin
  Result:=Default(TXMLKeyword);
  Result.AsString:=aValue;
end;


procedure TSecuritySchemeKeywordHelper.SetAsString(const aValue : string);

var
  T : TSecuritySchemeKeyword;

begin
  Self:=Default(TSecuritySchemeKeyword);
  For T in TSecuritySchemeKeyword do
    if (T.AsString=aValue) then
      begin
      self:=T;
      exit;
      end;
end;


function TSecuritySchemeKeywordHelper.ToString : String;

begin
  Result:=SecuritySchemeKeywordNames[Self];
end;


class function TSecuritySchemeKeywordHelper.fromString(const aValue : string) : TSecuritySchemeKeyword;

begin
  Result:=Default(TSecuritySchemeKeyword);
  Result.AsString:=aValue;
end;


procedure TOAuthFlowsKeywordHelper.SetAsString(const aValue : string);

var
  T : TOAuthFlowsKeyword;

begin
  Self:=Default(TOAuthFlowsKeyword);
  For T in TOAuthFlowsKeyword do
    if (T.AsString=aValue) then
      begin
      self:=T;
      exit;
      end;
end;


function TOAuthFlowsKeywordHelper.ToString : String;

begin
  Result:=OAuthFlowsKeywordNames[Self];
end;


class function TOAuthFlowsKeywordHelper.fromString(const aValue : string) : TOAuthFlowsKeyword;

begin
  Result:=Default(TOAuthFlowsKeyword);
  Result.AsString:=aValue;
end;


procedure TOauthFlowKeywordHelper.SetAsString(const aValue : string);

var
  T : TOauthFlowKeyword;

begin
  Self:=Default(TOauthFlowKeyword);
  For T in TOauthFlowKeyword do
    if (T.AsString=aValue) then
      begin
      self:=T;
      exit;
      end;
end;


function TOauthFlowKeywordHelper.ToString : String;

begin
  Result:=OauthFlowKeywordNames[Self];
end;


class function TOauthFlowKeywordHelper.fromString(const aValue : string) : TOauthFlowKeyword;

begin
  Result:=Default(TOauthFlowKeyword);
  Result.AsString:=aValue;
end;


end.

