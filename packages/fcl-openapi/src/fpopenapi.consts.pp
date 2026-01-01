{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2024 by Michael Van Canneyt (michael@freepascal.org)

    Basic OpenAPI constants

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit fpopenapi.consts;

{$mode ObjFPC}{$H+}

interface

Const
  SOpenAPiVersion3_1_0 = '3.1.0';

  KWExtensionPrefix = 'x-';

  KWOpenAPIOpenApi = 'openapi';
  KWOpenAPIInfo = 'info';
  KWOpenAPIJSONSchemaDialect = 'jsonSchemaDialect';
  KWOpenAPIServers= 'servers';
  KWOpenAPIPaths = 'paths';
  KWOpenAPIWebHooks = 'webhooks';
  KWOpenAPIComponents = 'components';
  KWOpenAPISecurity = 'security';
  KWOpenAPITags = 'tags';
  KWOpenAPIExternalDocs = 'externalDocs';

  // Info
  KWInfoTitle = 'title';
  KWInfoSummary = 'summary';
  KWInfoDescription = 'description';
  KWInfoTermsOfService = 'termsOfService';
  KWInfoContact = 'contact';
  KWInfoLicense = 'license';
  KWInfoVersion = 'version';

  // Contact object
  KWContactName = 'name';
  KWContactUrl = 'url';
  KWContactEmail = 'email';

  // Licence Object
  KWLicenseName = 'name';
  KWLicenseIdentifier = 'identifier';
  KWLicenseUrl = 'url';

  // Server object
  KWServerUrl = 'url';
  KWServerDescription = 'description';
  KWServerVariables = 'variables';

  // Server Variable object
  KWServerVariableEnum = 'enum';
  KWServerVariableDefault = 'default';
  KWServerVariableDescription = 'description';

  // Components object
  KWComponentsSchemas = 'schemas';
  KWComponentsResponses = 'responses';
  KWComponentsParameters = 'parameters';
  KWComponentsExamples = 'examples';
  KWComponentsRequestBodies = 'requestBodies';
  KWComponentsHeaders = 'headers';
  KWComponentsSecuritySchemes = 'securitySchemes';
  KWComponentsLinks = 'securitySchemes';
  KWComponentsCallbacks = 'callbacks';
  KWComponentsPathItems = 'pathItems';

  // Path item object
  KWPathItemRef = '$ref';
  KWPathItemSummary = 'summary';
  KWPathItemDescription = 'description';
  KWPathItemGet = 'get';
  KWPathItemPut = 'put';
  KWPathItemPost = 'post';
  KWPathItemDelete = 'delete';
  KWPathItemOptions = 'options';
  KWPathItemHead = 'head';
  KWPathItemPatch = 'patch';
  KWPathItemTrace = 'trace';
  KWPathItemServers = 'servers';
  KWPathItemParameters = 'parameters';

  // Operation object
  KWOperationTags = 'tags';
  KWOperationSummary = 'summary';
  KWOperationDescription = 'description';
  KWOperationExternalDocs = 'externalDocs';
  KWOperationOperationId = 'operationId';
  KWOperationParameters = 'parameters';
  KWOperationRequestBody = 'requestBody';
  KWOperationResponses = 'responses';
  KWOperationCallbacks = 'callbacks';
  KWOperationDeprecated = 'deprecated';
  KWOperationSecurity = 'security';
  KWOperationServers = 'servers';

  // External documentation object
  KWExternalDocsDescription = 'description';
  KWExternalDocsUrl = 'url';

  // Parameter object
  KWParameterName = 'name';
  KWParameterIn = 'in';
  KWParameterDescription = 'description';
  KWParameterRequired = 'required';
  KWParameterDeprecated = 'deprecated';
  KWParameterAllowEmptyValue = 'allowEmptyValue';
  KWParameterStyle = 'style';
  KWParameterExplode = 'explode';
  KWParameterAllowReserved = 'allowReserved';
  KWParameterSchema = 'schema';
  KWParameterExample = 'example';
  KWParameterExamples = 'examples';
  KWParameterContent = 'content';

  // ParameterStyle
  KWParameterStyleMatrix = 'matrix';
  KWParameterStyleLabel = 'matrix';
  KWParameterStyleForm = 'form';
  KWParameterStyleSimple = 'simple';
  KWParameterStyleSpaceDelimited = 'spaceDelimited';
  KWParameterStylePipeDelimited = 'pipeDelimited';
  KWParameterStyleDeepObject = 'deepObject';

  // Request body object
  KWRequestBodyDescription = 'description';
  KWRequestBodyContent = 'content';
  KWRequestBodyRequired = 'required';

  // Media Type Object
  KWMediaTypeSchema = 'schema';
  KWMediaTypeExample = 'example';
  KWMediaTypeExamples = 'examples';
  KWMediaTypeEncoding = 'encoding';

  // Encoding object
  KWEncodingContentType = 'contentType';
  KWEncodingHeaders = 'headers';
  KWEncodingStyle = 'style';
  KWEncodingExplode = 'explode';
  KWEncodingAllowReserved = 'allowReserved';

  // Responses Object
  KWResponsesDefault = 'default';

  // Response Object
  KWResponseDescription = 'description';
  KWResponseHeaders = 'headers';
  KWResponseContent = 'content';
  KWResponseLinks = 'links';

  // Example object
  KWExampleSummary = 'summary';
  KWExampleDescription= 'description';
  KWExampleValue = 'value';
  KWExampleExternalValue = 'externalValue';

  // Link Object
  KWLinkOperationRef = 'operationRef';
  KWLinkOperationId = 'operationId';
  KWLinkParameters = 'parameters';
  KWLinkRequestBody = 'requestBody';
  KWLinkDescription = 'description';
  KWLinkServer = 'server';

  // Tag Object
  KWTagName = 'name';
  KWTagDescription = 'description';
  KWTagExternalDocs = 'externalDocs';

  // Reference Object
  KWReferenceRef = '$ref';
  KWReferenceSummary = 'summary';
  KWReferenceDescription = 'description';

  // Schema object
  KWSchemaDiscriminator = 'discriminator';
  KWSchemaXML = 'xml';
  KWSchemaExternalDocs = 'externalDocs';
  KWSchemaExample = 'example';

  KWDiscriminatorPropertyName = 'propertyName';
  KWDiscriminatorMapping = 'mapping';

  // XML Object
  KWXMLName = 'name';
  KWXMLNamespace = 'namespace';
  KWXMLPrefix = 'prefix';
  KWXMLAttribute = 'attribute';
  KWXMLWrapped = 'wrapped';

  // Security scheme object
  KWSecuritySchemeType = 'type';
  KWSecuritySchemeDescription = 'description';
  KWSecuritySchemeName = 'name';
  KWSecuritySchemeIn = 'in';
  KWSecuritySchemeScheme = 'scheme';
  KWSecuritySchemeBearerFormat = 'bearerFormat';
  KWSecuritySchemeFlows = 'flows';
  KWSecuritySchemeOpenIdConnectUrl = 'openIdConnectUrl';

  // Oauth Flows object
  KWOAuthFlowsImplicit = 'implicit';
  KWOAuthFlowsPassword= 'password';
  KWOAuthFlowsClientCredentials = 'credentials';
  KWOAuthFlowsClientAuthorizationCode = 'authorizationCode';

  // Oauth Flow object
  KWOAuthFlowAuthorizationUrl = 'authorizationUrl';
  KWOAuthFlowTokenURL = 'tokenUrl';
  KWOAuthFlowRefreshURL = 'refreshUrl';
  KWOAuthFlowScopes = 'scopes';


implementation

end.

