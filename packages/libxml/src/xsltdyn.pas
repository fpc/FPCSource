{$IFNDEF FPC_DOTTEDUNITS}
unit xsltdyn;
{$ENDIF FPC_DOTTEDUNITS}

{$mode ObjFPC}
{$H+}
{$macro on}

{$ALIGN 8}
{$MINENUMSIZE 4}

{$DEFINE NIL_FUNCVARS_ON_FREE}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  Api.XML.Xml2Dyn,
  System.CTypes,
  System.DynLibs
  {$IFDEF WINDOWS}
  , WinApi.Windows
  {$ENDIF}
  ;
{$ELSE FPC_DOTTEDUNITS}
uses
  xml2dyn, ctypes, dynlibs
  {$IFDEF WINDOWS}
  , Windows
  {$ENDIF}
  ;
{$ENDIF}

{.$DEFINE XSLT_REFACTORED}
{$DEFINE HAVE_STRXFRM_L}

const
{$IF Defined(WINDOWS)}
  xsltlib = 'libxslt.'+sharedsuffix;
  {$DEFINE EXTDECL := cdecl}
  {$DEFINE NO_EXTERNAL_VARS}
{$ELSEIF Defined(UNIX)}
  xsltlib = 'libxslt.'+sharedsuffix;
  {$DEFINE EXTDECL := cdecl}
{$ELSE}
  {$MESSAGE ERROR 'Platform not supported right now'}
{$IFEND}

{$IFNDEF NO_EXTERNAL_VARS}
{$DEFINE NO_EXTERNAL_VARS}
{$ENDIF}

type
{$DEFINE POINTER}
{$I xsltlocale.inc}
{$I xsltInternals.inc}
{$UNDEF POINTER}

{$DEFINE TYPE}
{$I xsltpattern.inc}
{$I numbersInternals.inc}
{$I xsltlocale.inc}
{$I xsltInternals.inc}
{$I documents.inc}
{$I extensions.inc}
{$I security.inc}
{$I xsltutils.inc}
{$UNDEF TYPE}

const
{$DEFINE CONST}
{$I xsltlocale.inc}
{$I xsltInternals.inc}
{$I documents.inc}
{$I extra.inc}
{$I keys.inc}
{$I namespaces.inc}
{$I variables.inc}
{$I xslt.inc}
{$I xsltconfig.inc}
{$I xsltutils.inc}
{$UNDEF CONST}


{$DEFINE FUNCTIONVAR}
var
{$I xsltpattern.inc}
{$I xsltlocale.inc}
{$I xsltInternals.inc}
{$I attributes.inc}
{$I documents.inc}
{$I extensions.inc}
{$I extra.inc}
{$I functions.inc}
{$I imports.inc}
{$I keys.inc}
{$I namespaces.inc}
{$I preproc.inc}
{$I security.inc}
{$I templates.inc}
{$I transform.inc}
{$I variables.inc}
{$I xslt.inc}
{$I xsltutils.inc}
{$UNDEF FUNCTION}

{$IFNDEF NO_EXTERNAL_VARS}
{$DEFINE EXTVAR}
{$I documents.inc}
{$I preproc.inc}
{$I xslt.inc}
{$I xsltutils.inc}
{$UNDEF EXTVAR}
{$ENDIF}

var
  LibXsltHandle: TLibHandle = NilHandle;

function LoadLibXslt(ALibName: String = ''): Boolean;
procedure FreeLibXslt;

implementation

function LoadLibXslt(ALibName: String): Boolean;
begin
  if ALibName = '' then
    ALibName := xsltlib;
  LibXsltHandle := dynlibs.LoadLibrary(ALibName);
  if LibXsltHandle <> NilHandle then
  begin
    { xsltpattern.inc }
    Pointer(xsltCompilePattern) := GetProcAddress(LibXsltHandle, 'xsltCompilePattern');
    Pointer(xsltFreeCompMatchList) := GetProcAddress(LibXsltHandle, 'xsltFreeCompMatchList');
    Pointer(xsltTestCompMatchList) := GetProcAddress(LibXsltHandle, 'xsltTestCompMatchList');
    Pointer(xsltCompMatchClearCache) := GetProcAddress(LibXsltHandle, 'xsltCompMatchClearCache');
    Pointer(xsltNormalizeCompSteps) := GetProcAddress(LibXsltHandle, 'xsltNormalizeCompSteps');
    Pointer(xsltAddTemplate) := GetProcAddress(LibXsltHandle, 'xsltAddTemplate');
    Pointer(xsltGetTemplate) := GetProcAddress(LibXsltHandle, 'xsltGetTemplate');
    Pointer(xsltFreeTemplateHashes) := GetProcAddress(LibXsltHandle, 'xsltFreeTemplateHashes');
    Pointer(xsltCleanupTemplates) := GetProcAddress(LibXsltHandle, 'xsltCleanupTemplates');

    { xsltlocale.inc }
  {$IFDEF HAVE_STRXFRM_L}
    Pointer(xsltNewLocale) := GetProcAddress(LibXsltHandle, 'xsltNewLocale');
    Pointer(xsltFreeLocale) := GetProcAddress(LibXsltHandle, 'xsltFreeLocale');
    Pointer(xsltStrxfrm) := GetProcAddress(LibXsltHandle, 'xsltStrxfrm');
    Pointer(xsltLocaleStrcmp) := GetProcAddress(LibXsltHandle, 'xsltLocaleStrcmp');
    Pointer(xsltFreeLocales) := GetProcAddress(LibXsltHandle, 'xsltFreeLocales');
  {$ENDIF} {HAVE_STRXFRM_L}

    { xsltInternals.inc }
  {$IFDEF XSLT_REFACTORED}
    Pointer(xsltPointerListCreate) := GetProcAddress(LibXsltHandle, 'xsltPointerListCreate');
    Pointer(xsltPointerListFree) := GetProcAddress(LibXsltHandle, 'xsltPointerListFree');
    Pointer(xsltPointerListClear) := GetProcAddress(LibXsltHandle, 'xsltPointerListClear');
    Pointer(xsltPointerListAddSize) := GetProcAddress(LibXsltHandle, 'xsltPointerListAddSize');
  {$ELSE} (* XSLT_REFACTORED *)
    Pointer(xsltNewStylesheet) := GetProcAddress(LibXsltHandle, 'xsltNewStylesheet');
    Pointer(xsltParseStylesheetFile) := GetProcAddress(LibXsltHandle, 'xsltParseStylesheetFile');
    Pointer(xsltFreeStylesheet) := GetProcAddress(LibXsltHandle, 'xsltFreeStylesheet');
    Pointer(xsltIsBlank) := GetProcAddress(LibXsltHandle, 'xsltIsBlank');
    Pointer(xsltFreeStackElemList) := GetProcAddress(LibXsltHandle, 'xsltFreeStackElemList');
    Pointer(xsltDecimalFormatGetByName) := GetProcAddress(LibXsltHandle, 'xsltDecimalFormatGetByName');
    Pointer(xsltDecimalFormatGetByQName) := GetProcAddress(LibXsltHandle, 'xsltDecimalFormatGetByQName');
    Pointer(xsltParseStylesheetProcess) := GetProcAddress(LibXsltHandle, 'xsltParseStylesheetProcess');
    Pointer(xsltParseStylesheetOutput) := GetProcAddress(LibXsltHandle, 'xsltParseStylesheetOutput');
    Pointer(xsltParseStylesheetDoc) := GetProcAddress(LibXsltHandle, 'xsltParseStylesheetDoc');
    Pointer(xsltParseStylesheetImportedDoc) := GetProcAddress(LibXsltHandle, 'xsltParseStylesheetImportedDoc');
    Pointer(xsltParseStylesheetUser) := GetProcAddress(LibXsltHandle, 'xsltParseStylesheetUser');
    Pointer(xsltLoadStylesheetPI) := GetProcAddress(LibXsltHandle, 'xsltLoadStylesheetPI');
    Pointer(xsltNumberFormat) := GetProcAddress(LibXsltHandle, 'xsltNumberFormat');
    Pointer(xsltFormatNumberConversion) := GetProcAddress(LibXsltHandle, 'xsltFormatNumberConversion');
    Pointer(xsltParseTemplateContent) := GetProcAddress(LibXsltHandle, 'xsltParseTemplateContent');
    Pointer(xsltAllocateExtra) := GetProcAddress(LibXsltHandle, 'xsltAllocateExtra');
    Pointer(xsltAllocateExtraCtxt) := GetProcAddress(LibXsltHandle, 'xsltAllocateExtraCtxt');
    Pointer(xsltCreateRVT) := GetProcAddress(LibXsltHandle, 'xsltCreateRVT');
    Pointer(xsltRegisterTmpRVT) := GetProcAddress(LibXsltHandle, 'xsltRegisterTmpRVT');
    Pointer(xsltRegisterLocalRVT) := GetProcAddress(LibXsltHandle, 'xsltRegisterLocalRVT');
    Pointer(xsltRegisterPersistRVT) := GetProcAddress(LibXsltHandle, 'xsltRegisterPersistRVT');
    Pointer(xsltExtensionInstructionResultRegister) := GetProcAddress(LibXsltHandle, 'xsltExtensionInstructionResultRegister');
    Pointer(xsltExtensionInstructionResultFinalize) := GetProcAddress(LibXsltHandle, 'xsltExtensionInstructionResultFinalize');
    Pointer(xsltFlagRVTs) := GetProcAddress(LibXsltHandle, 'xsltFlagRVTs');
    Pointer(xsltFreeRVTs) := GetProcAddress(LibXsltHandle, 'xsltFreeRVTs');
    Pointer(xsltReleaseRVT) := GetProcAddress(LibXsltHandle, 'xsltReleaseRVT');
    Pointer(xsltCompileAttr) := GetProcAddress(LibXsltHandle, 'xsltCompileAttr');
    Pointer(xsltEvalAVT) := GetProcAddress(LibXsltHandle, 'xsltEvalAVT');
    Pointer(xsltFreeAVTList) := GetProcAddress(LibXsltHandle, 'xsltFreeAVTList');
    Pointer(xsltUninit) := GetProcAddress(LibXsltHandle, 'xsltUninit');
  {$ENDIF}
  {$IFDEF XSLT_REFACTORED}
    Pointer(xsltParseSequenceConstructor) := GetProcAddress(LibXsltHandle, 'xsltParseSequenceConstructor');
    Pointer(xsltParseAnyXSLTElem) := GetProcAddress(LibXsltHandle, 'xsltParseAnyXSLTElem');
  {$IFDEF XSLT_REFACTORED_XSLT_NSCOMP}
    Pointer(xsltRestoreDocumentNamespaces) := GetProcAddress(LibXsltHandle, 'xsltRestoreDocumentNamespaces');
  {$ENDIF}
  {$ENDIF} (* XSLT_REFACTORED *)
    Pointer(xsltInitCtxtKey) := GetProcAddress(LibXsltHandle, 'xsltInitCtxtKey');
    Pointer(xsltInitAllDocKeys) := GetProcAddress(LibXsltHandle, 'xsltInitAllDocKeys');

    { attributes.inc }
    Pointer(xsltParseStylesheetAttributeSet) := GetProcAddress(LibXsltHandle, 'xsltParseStylesheetAttributeSet');
    Pointer(xsltFreeAttributeSetsHashes) := GetProcAddress(LibXsltHandle, 'xsltFreeAttributeSetsHashes');
    Pointer(xsltApplyAttributeSet) := GetProcAddress(LibXsltHandle, 'xsltApplyAttributeSet');
    Pointer(xsltResolveStylesheetAttributeSet) := GetProcAddress(LibXsltHandle, 'xsltResolveStylesheetAttributeSet');

    { documents.inc }
    Pointer(xsltNewDocument) := GetProcAddress(LibXsltHandle, 'xsltNewDocument');
    Pointer(xsltLoadDocument) := GetProcAddress(LibXsltHandle, 'xsltLoadDocument');
    Pointer(xsltFindDocument) := GetProcAddress(LibXsltHandle, 'xsltFindDocument');
    Pointer(xsltFreeDocuments) := GetProcAddress(LibXsltHandle, 'xsltFreeDocuments');
    Pointer(xsltLoadStyleDocument) := GetProcAddress(LibXsltHandle, 'xsltLoadStyleDocument');
    Pointer(xsltNewStyleDocument) := GetProcAddress(LibXsltHandle, 'xsltNewStyleDocument');
    Pointer(xsltFreeStyleDocuments) := GetProcAddress(LibXsltHandle, 'xsltFreeStyleDocuments');
    Pointer(xsltSetLoaderFunc) := GetProcAddress(LibXsltHandle, 'xsltSetLoaderFunc');

    { extensions.inc }
    Pointer(xsltInitGlobals) := GetProcAddress(LibXsltHandle, 'xsltInitGlobals');
    Pointer(xsltRegisterExtModule) := GetProcAddress(LibXsltHandle, 'xsltRegisterExtModule');
    Pointer(xsltRegisterExtModuleFull) := GetProcAddress(LibXsltHandle, 'xsltRegisterExtModuleFull');
    Pointer(xsltUnregisterExtModule) := GetProcAddress(LibXsltHandle, 'xsltUnregisterExtModule');
    Pointer(xsltGetExtData) := GetProcAddress(LibXsltHandle, 'xsltGetExtData');
    Pointer(xsltStyleGetExtData) := GetProcAddress(LibXsltHandle, 'xsltStyleGetExtData');
  {$IFDEF XSLT_REFACTORED}
    Pointer(xsltStyleStylesheetLevelGetExtData) := GetProcAddress(LibXsltHandle, 'xsltStyleStylesheetLevelGetExtData');
  {$ENDIF}
    Pointer(xsltShutdownCtxtExts) := GetProcAddress(LibXsltHandle, 'xsltShutdownCtxtExts');
    Pointer(xsltShutdownExts) := GetProcAddress(LibXsltHandle, 'xsltShutdownExts');
    Pointer(xsltXPathGetTransformContext) := GetProcAddress(LibXsltHandle, 'xsltXPathGetTransformContext');
    Pointer(xsltRegisterExtModuleFunction) := GetProcAddress(LibXsltHandle, 'xsltRegisterExtModuleFunction');
    Pointer(xsltExtModuleFunctionLookup) := GetProcAddress(LibXsltHandle, 'xsltExtModuleFunctionLookup');
    Pointer(xsltUnregisterExtModuleFunction) := GetProcAddress(LibXsltHandle, 'xsltUnregisterExtModuleFunction');
    Pointer(xsltNewElemPreComp) := GetProcAddress(LibXsltHandle, 'xsltNewElemPreComp');
    Pointer(xsltInitElemPreComp) := GetProcAddress(LibXsltHandle, 'xsltInitElemPreComp');
    Pointer(xsltRegisterExtModuleElement) := GetProcAddress(LibXsltHandle, 'xsltRegisterExtModuleElement');
    Pointer(xsltExtElementLookup) := GetProcAddress(LibXsltHandle, 'xsltExtElementLookup');
    Pointer(xsltExtModuleElementLookup) := GetProcAddress(LibXsltHandle, 'xsltExtModuleElementLookup');
    Pointer(xsltExtModuleElementPreComputeLookup) := GetProcAddress(LibXsltHandle, 'xsltExtModuleElementPreComputeLookup');
    Pointer(xsltUnregisterExtModuleElement) := GetProcAddress(LibXsltHandle, 'xsltUnregisterExtModuleElement');
    Pointer(xsltRegisterExtModuleTopLevel) := GetProcAddress(LibXsltHandle, 'xsltRegisterExtModuleTopLevel');
    Pointer(xsltExtModuleTopLevelLookup) := GetProcAddress(LibXsltHandle, 'xsltExtModuleTopLevelLookup');
    Pointer(xsltUnregisterExtModuleTopLevel) := GetProcAddress(LibXsltHandle, 'xsltUnregisterExtModuleTopLevel');
    Pointer(xsltRegisterExtFunction) := GetProcAddress(LibXsltHandle, 'xsltRegisterExtFunction');
    Pointer(xsltRegisterExtElement) := GetProcAddress(LibXsltHandle, 'xsltRegisterExtElement');
    Pointer(xsltRegisterExtPrefix) := GetProcAddress(LibXsltHandle, 'xsltRegisterExtPrefix');
    Pointer(xsltCheckExtPrefix) := GetProcAddress(LibXsltHandle, 'xsltCheckExtPrefix');
    Pointer(xsltCheckExtURI) := GetProcAddress(LibXsltHandle, 'xsltCheckExtURI');
    Pointer(xsltInitCtxtExts) := GetProcAddress(LibXsltHandle, 'xsltInitCtxtExts');
    Pointer(xsltFreeCtxtExts) := GetProcAddress(LibXsltHandle, 'xsltFreeCtxtExts');
    Pointer(xsltFreeExts) := GetProcAddress(LibXsltHandle, 'xsltFreeExts');
    Pointer(xsltPreComputeExtModuleElement) := GetProcAddress(LibXsltHandle, 'xsltPreComputeExtModuleElement');
    Pointer(xsltGetExtInfo) := GetProcAddress(LibXsltHandle, 'xsltGetExtInfo');
    Pointer(xsltRegisterTestModule) := GetProcAddress(LibXsltHandle, 'xsltRegisterTestModule');
    Pointer(xsltDebugDumpExtensions) := GetProcAddress(LibXsltHandle, 'xsltDebugDumpExtensions');

    { extra.inc }
    Pointer(xsltFunctionNodeSet) := GetProcAddress(LibXsltHandle, 'xsltFunctionNodeSet');
    Pointer(xsltDebug) := GetProcAddress(LibXsltHandle, 'xsltDebug');
    Pointer(xsltRegisterExtras) := GetProcAddress(LibXsltHandle, 'xsltRegisterExtras');
    Pointer(xsltRegisterAllExtras) := GetProcAddress(LibXsltHandle, 'xsltRegisterAllExtras');

    { functions.inc }
    Pointer(xsltXPathFunctionLookup) := GetProcAddress(LibXsltHandle, 'xsltXPathFunctionLookup');
    Pointer(xsltDocumentFunction) := GetProcAddress(LibXsltHandle, 'xsltDocumentFunction');
    Pointer(xsltKeyFunction) := GetProcAddress(LibXsltHandle, 'xsltKeyFunction');
    Pointer(xsltUnparsedEntityURIFunction) := GetProcAddress(LibXsltHandle, 'xsltUnparsedEntityURIFunction');
    Pointer(xsltFormatNumberFunction) := GetProcAddress(LibXsltHandle, 'xsltFormatNumberFunction');
    Pointer(xsltGenerateIdFunction) := GetProcAddress(LibXsltHandle, 'xsltGenerateIdFunction');
    Pointer(xsltSystemPropertyFunction) := GetProcAddress(LibXsltHandle, 'xsltSystemPropertyFunction');
    Pointer(xsltElementAvailableFunction) := GetProcAddress(LibXsltHandle, 'xsltElementAvailableFunction');
    Pointer(xsltFunctionAvailableFunction) := GetProcAddress(LibXsltHandle, 'xsltFunctionAvailableFunction');
    Pointer(xsltRegisterAllFunctions) := GetProcAddress(LibXsltHandle, 'xsltRegisterAllFunctions');

    { imports.inc }
    Pointer(xsltParseStylesheetImport) := GetProcAddress(LibXsltHandle, 'xsltParseStylesheetImport');
    Pointer(xsltParseStylesheetInclude) := GetProcAddress(LibXsltHandle, 'xsltParseStylesheetInclude');
    Pointer(xsltNextImport) := GetProcAddress(LibXsltHandle, 'xsltNextImport');
    Pointer(xsltNeedElemSpaceHandling) := GetProcAddress(LibXsltHandle, 'xsltNeedElemSpaceHandling');
    Pointer(xsltFindElemSpaceHandling) := GetProcAddress(LibXsltHandle, 'xsltFindElemSpaceHandling');
    Pointer(xsltFindTemplate) := GetProcAddress(LibXsltHandle, 'xsltFindTemplate');

    { keys.inc }
    Pointer(xsltAddKey) := GetProcAddress(LibXsltHandle, 'xsltAddKey');
    Pointer(xsltGetKey) := GetProcAddress(LibXsltHandle, 'xsltGetKey');
    Pointer(xsltInitCtxtKeys) := GetProcAddress(LibXsltHandle, 'xsltInitCtxtKeys');
    Pointer(xsltFreeKeys) := GetProcAddress(LibXsltHandle, 'xsltFreeKeys');
    Pointer(xsltFreeDocumentKeys) := GetProcAddress(LibXsltHandle, 'xsltFreeDocumentKeys');

    { namespaces.inc }
    Pointer(xsltNamespaceAlias) := GetProcAddress(LibXsltHandle, 'xsltNamespaceAlias');
    Pointer(xsltGetNamespace) := GetProcAddress(LibXsltHandle, 'xsltGetNamespace');
    Pointer(xsltGetPlainNamespace) := GetProcAddress(LibXsltHandle, 'xsltGetPlainNamespace');
    Pointer(xsltGetSpecialNamespace) := GetProcAddress(LibXsltHandle, 'xsltGetSpecialNamespace');
    Pointer(xsltCopyNamespace) := GetProcAddress(LibXsltHandle, 'xsltCopyNamespace');
    Pointer(xsltCopyNamespaceList) := GetProcAddress(LibXsltHandle, 'xsltCopyNamespaceList');
    Pointer(xsltFreeNamespaceAliasHashes) := GetProcAddress(LibXsltHandle, 'xsltFreeNamespaceAliasHashes');

    { preproc.inc }
    Pointer(xsltDocumentComp) := GetProcAddress(LibXsltHandle, 'xsltDocumentComp');
    Pointer(xsltStylePreCompute) := GetProcAddress(LibXsltHandle, 'xsltStylePreCompute');
    Pointer(xsltFreeStylePreComps) := GetProcAddress(LibXsltHandle, 'xsltFreeStylePreComps');

    { security.inc }
    Pointer(xsltNewSecurityPrefs) := GetProcAddress(LibXsltHandle, 'xsltNewSecurityPrefs');
    Pointer(xsltFreeSecurityPrefs) := GetProcAddress(LibXsltHandle, 'xsltFreeSecurityPrefs');
    Pointer(xsltSetSecurityPrefs) := GetProcAddress(LibXsltHandle, 'xsltSetSecurityPrefs');
    Pointer(xsltGetSecurityPrefs) := GetProcAddress(LibXsltHandle, 'xsltGetSecurityPrefs');
    Pointer(xsltSetDefaultSecurityPrefs) := GetProcAddress(LibXsltHandle, 'xsltSetDefaultSecurityPrefs');
    Pointer(xsltGetDefaultSecurityPrefs) := GetProcAddress(LibXsltHandle, 'xsltGetDefaultSecurityPrefs');
    Pointer(xsltSetCtxtSecurityPrefs) := GetProcAddress(LibXsltHandle, 'xsltSetCtxtSecurityPrefs');
    Pointer(xsltSecurityAllow) := GetProcAddress(LibXsltHandle, 'xsltSecurityAllow');
    Pointer(xsltSecurityForbid) := GetProcAddress(LibXsltHandle, 'xsltSecurityForbid');
    Pointer(xsltCheckWrite) := GetProcAddress(LibXsltHandle, 'xsltCheckWrite');
    Pointer(xsltCheckRead) := GetProcAddress(LibXsltHandle, 'xsltCheckRead');

    { templates.inc }
    Pointer(xsltEvalXPathPredicate) := GetProcAddress(LibXsltHandle, 'xsltEvalXPathPredicate');
    Pointer(xsltEvalTemplateString) := GetProcAddress(LibXsltHandle, 'xsltEvalTemplateString');
    Pointer(xsltEvalAttrValueTemplate) := GetProcAddress(LibXsltHandle, 'xsltEvalAttrValueTemplate');
    Pointer(xsltEvalStaticAttrValueTemplate) := GetProcAddress(LibXsltHandle, 'xsltEvalStaticAttrValueTemplate');
    Pointer(xsltEvalXPathString) := GetProcAddress(LibXsltHandle, 'xsltEvalXPathString');
    Pointer(xsltEvalXPathStringNs) := GetProcAddress(LibXsltHandle, 'xsltEvalXPathStringNs');
    Pointer(xsltTemplateProcess) := GetProcAddress(LibXsltHandle, 'xsltTemplateProcess');
    Pointer(xsltAttrListTemplateProcess) := GetProcAddress(LibXsltHandle, 'xsltAttrListTemplateProcess');
    Pointer(xsltAttrTemplateProcess) := GetProcAddress(LibXsltHandle, 'xsltAttrTemplateProcess');
    Pointer(xsltAttrTemplateValueProcess) := GetProcAddress(LibXsltHandle, 'xsltAttrTemplateValueProcess');
    Pointer(xsltAttrTemplateValueProcessNode) := GetProcAddress(LibXsltHandle, 'xsltAttrTemplateValueProcessNode');

    { transform.inc }
    Pointer(xsltSetXIncludeDefault) := GetProcAddress(LibXsltHandle, 'xsltSetXIncludeDefault');
    Pointer(xsltGetXIncludeDefault) := GetProcAddress(LibXsltHandle, 'xsltGetXIncludeDefault');
    Pointer(xsltNewTransformContext) := GetProcAddress(LibXsltHandle, 'xsltNewTransformContext');
    Pointer(xsltFreeTransformContext) := GetProcAddress(LibXsltHandle, 'xsltFreeTransformContext');
    Pointer(xsltApplyStylesheetUser) := GetProcAddress(LibXsltHandle, 'xsltApplyStylesheetUser');
    Pointer(xsltProcessOneNode) := GetProcAddress(LibXsltHandle, 'xsltProcessOneNode');
    Pointer(xsltApplyStripSpaces) := GetProcAddress(LibXsltHandle, 'xsltApplyStripSpaces');
    Pointer(xsltApplyStylesheet) := GetProcAddress(LibXsltHandle, 'xsltApplyStylesheet');
    Pointer(xsltProfileStylesheet) := GetProcAddress(LibXsltHandle, 'xsltProfileStylesheet');
    Pointer(xsltRunStylesheet) := GetProcAddress(LibXsltHandle, 'xsltRunStylesheet');
    Pointer(xsltRunStylesheetUser) := GetProcAddress(LibXsltHandle, 'xsltRunStylesheetUser');
    Pointer(xsltApplyOneTemplate) := GetProcAddress(LibXsltHandle, 'xsltApplyOneTemplate');
    Pointer(xsltDocumentElem) := GetProcAddress(LibXsltHandle, 'xsltDocumentElem');
    Pointer(xsltSort) := GetProcAddress(LibXsltHandle, 'xsltSort');
    Pointer(xsltCopy) := GetProcAddress(LibXsltHandle, 'xsltCopy');
    Pointer(xsltText) := GetProcAddress(LibXsltHandle, 'xsltText');
    Pointer(xsltElement) := GetProcAddress(LibXsltHandle, 'xsltElement');
    Pointer(xsltComment) := GetProcAddress(LibXsltHandle, 'xsltComment');
    Pointer(xsltAttribute) := GetProcAddress(LibXsltHandle, 'xsltAttribute');
    Pointer(xsltProcessingInstruction) := GetProcAddress(LibXsltHandle, 'xsltProcessingInstruction');
    Pointer(xsltCopyOf) := GetProcAddress(LibXsltHandle, 'xsltCopyOf');
    Pointer(xsltValueOf) := GetProcAddress(LibXsltHandle, 'xsltValueOf');
    Pointer(xsltNumber) := GetProcAddress(LibXsltHandle, 'xsltNumber');
    Pointer(xsltApplyImports) := GetProcAddress(LibXsltHandle, 'xsltApplyImports');
    Pointer(xsltCallTemplate) := GetProcAddress(LibXsltHandle, 'xsltCallTemplate');
    Pointer(xsltApplyTemplates) := GetProcAddress(LibXsltHandle, 'xsltApplyTemplates');
    Pointer(xsltChoose) := GetProcAddress(LibXsltHandle, 'xsltChoose');
    Pointer(xsltIf) := GetProcAddress(LibXsltHandle, 'xsltIf');
    Pointer(xsltForEach) := GetProcAddress(LibXsltHandle, 'xsltForEach');
    Pointer(xsltRegisterAllElement) := GetProcAddress(LibXsltHandle, 'xsltRegisterAllElement');
    Pointer(xsltCopyTextString) := GetProcAddress(LibXsltHandle, 'xsltCopyTextString');
    Pointer(xsltLocalVariablePop) := GetProcAddress(LibXsltHandle, 'xsltLocalVariablePop');
    Pointer(xsltLocalVariablePush) := GetProcAddress(LibXsltHandle, 'xsltLocalVariablePush');
    Pointer(xslHandleDebugger) := GetProcAddress(LibXsltHandle, 'xslHandleDebugger');

    { variables.inc }
    Pointer(xsltEvalGlobalVariables) := GetProcAddress(LibXsltHandle, 'xsltEvalGlobalVariables');
    Pointer(xsltEvalUserParams) := GetProcAddress(LibXsltHandle, 'xsltEvalUserParams');
    Pointer(xsltQuoteUserParams) := GetProcAddress(LibXsltHandle, 'xsltQuoteUserParams');
    Pointer(xsltEvalOneUserParam) := GetProcAddress(LibXsltHandle, 'xsltEvalOneUserParam');
    Pointer(xsltQuoteOneUserParam) := GetProcAddress(LibXsltHandle, 'xsltQuoteOneUserParam');
    Pointer(xsltParseGlobalVariable) := GetProcAddress(LibXsltHandle, 'xsltParseGlobalVariable');
    Pointer(xsltParseGlobalParam) := GetProcAddress(LibXsltHandle, 'xsltParseGlobalParam');
    Pointer(xsltParseStylesheetVariable) := GetProcAddress(LibXsltHandle, 'xsltParseStylesheetVariable');
    Pointer(xsltParseStylesheetParam) := GetProcAddress(LibXsltHandle, 'xsltParseStylesheetParam');
    Pointer(xsltParseStylesheetCallerParam) := GetProcAddress(LibXsltHandle, 'xsltParseStylesheetCallerParam');
    Pointer(xsltAddStackElemList) := GetProcAddress(LibXsltHandle, 'xsltAddStackElemList');
    Pointer(xsltFreeGlobalVariables) := GetProcAddress(LibXsltHandle, 'xsltFreeGlobalVariables');
    Pointer(xsltVariableLookup) := GetProcAddress(LibXsltHandle, 'xsltVariableLookup');
    Pointer(xsltXPathVariableLookup) := GetProcAddress(LibXsltHandle, 'xsltXPathVariableLookup');

    { xslt.inc }
    Pointer(xsltInit) := GetProcAddress(LibXsltHandle, 'xsltInit');
    Pointer(xsltCleanupGlobals) := GetProcAddress(LibXsltHandle, 'xsltCleanupGlobals');

    { xsltutils.inc }
    Pointer(xsltGetNsProp) := GetProcAddress(LibXsltHandle, 'xsltGetNsProp');
    Pointer(xsltGetCNsProp) := GetProcAddress(LibXsltHandle, 'xsltGetCNsProp');
    Pointer(xsltGetUTF8Char) := GetProcAddress(LibXsltHandle, 'xsltGetUTF8Char');
    Pointer(xsltDebugSetDefaultTrace) := GetProcAddress(LibXsltHandle, 'xsltDebugSetDefaultTrace');
    Pointer(xsltDebugGetDefaultTrace) := GetProcAddress(LibXsltHandle, 'xsltDebugGetDefaultTrace');
    Pointer(xsltPrintErrorContext) := GetProcAddress(LibXsltHandle, 'xsltPrintErrorContext');
    Pointer(xsltMessage) := GetProcAddress(LibXsltHandle, 'xsltMessage');
    Pointer(xsltSetGenericErrorFunc) := GetProcAddress(LibXsltHandle, 'xsltSetGenericErrorFunc');
    Pointer(xsltSetGenericDebugFunc) := GetProcAddress(LibXsltHandle, 'xsltSetGenericDebugFunc');
    Pointer(xsltSetTransformErrorFunc) := GetProcAddress(LibXsltHandle, 'xsltSetTransformErrorFunc');
    Pointer(xsltTransformError) := GetProcAddress(LibXsltHandle, 'xsltTransformError');
    Pointer(xsltSetCtxtParseOptions) := GetProcAddress(LibXsltHandle, 'xsltSetCtxtParseOptions');
    Pointer(xsltDocumentSortFunction) := GetProcAddress(LibXsltHandle, 'xsltDocumentSortFunction');
    Pointer(xsltSetSortFunc) := GetProcAddress(LibXsltHandle, 'xsltSetSortFunc');
    Pointer(xsltSetCtxtSortFunc) := GetProcAddress(LibXsltHandle, 'xsltSetCtxtSortFunc');
    Pointer(xsltDefaultSortFunction) := GetProcAddress(LibXsltHandle, 'xsltDefaultSortFunction');
    Pointer(xsltDoSortFunction) := GetProcAddress(LibXsltHandle, 'xsltDoSortFunction');
    Pointer(xsltComputeSortResult) := GetProcAddress(LibXsltHandle, 'xsltComputeSortResult');
    Pointer(xsltSplitQName) := GetProcAddress(LibXsltHandle, 'xsltSplitQName');
    Pointer(xsltGetQNameURI) := GetProcAddress(LibXsltHandle, 'xsltGetQNameURI');
    Pointer(xsltGetQNameURI2) := GetProcAddress(LibXsltHandle, 'xsltGetQNameURI2');
    Pointer(xsltSaveResultTo) := GetProcAddress(LibXsltHandle, 'xsltSaveResultTo');
    Pointer(xsltSaveResultToFilename) := GetProcAddress(LibXsltHandle, 'xsltSaveResultToFilename');
    Pointer(xsltSaveResultToFile) := GetProcAddress(LibXsltHandle, 'xsltSaveResultToFile');
    Pointer(xsltSaveResultToFd) := GetProcAddress(LibXsltHandle, 'xsltSaveResultToFd');
    Pointer(xsltSaveResultToString) := GetProcAddress(LibXsltHandle, 'xsltSaveResultToString');
    Pointer(xsltXPathCompile) := GetProcAddress(LibXsltHandle, 'xsltXPathCompile');
    Pointer(xsltXPathCompileFlags) := GetProcAddress(LibXsltHandle, 'xsltXPathCompileFlags');
    Pointer(xsltSaveProfiling) := GetProcAddress(LibXsltHandle, 'xsltSaveProfiling');
    Pointer(xsltGetProfileInformation) := GetProcAddress(LibXsltHandle, 'xsltGetProfileInformation');
    Pointer(xsltTimestamp) := GetProcAddress(LibXsltHandle, 'xsltTimestamp');
    Pointer(xsltCalibrateAdjust) := GetProcAddress(LibXsltHandle, 'xsltCalibrateAdjust');
    Pointer(xsltSetDebuggerStatus) := GetProcAddress(LibXsltHandle, 'xsltSetDebuggerStatus');
    Pointer(xsltGetDebuggerStatus) := GetProcAddress(LibXsltHandle, 'xsltGetDebuggerStatus');
    Pointer(xsltSetDebuggerCallbacks) := GetProcAddress(LibXsltHandle, 'xsltSetDebuggerCallbacks');
    Pointer(xslAddCall) := GetProcAddress(LibXsltHandle, 'xslAddCall');
    Pointer(xslDropCall) := GetProcAddress(LibXsltHandle, 'xslDropCall');

    Result := True;
  end
  else
    Result := False;
end;

procedure FreeLibXslt;
begin
  if LibXsltHandle = NilHandle then
    Exit;

  dynlibs.FreeLibrary(LibXsltHandle);
  LibXsltHandle := NilHandle;

{$IFDEF NIL_FUNCVARS_ON_FREE}
  { xsltpattern.inc }
  xsltCompilePattern := nil;
  xsltFreeCompMatchList := nil;
  xsltTestCompMatchList := nil;
  xsltCompMatchClearCache := nil;
  xsltNormalizeCompSteps := nil;
  xsltAddTemplate := nil;
  xsltGetTemplate := nil;
  xsltFreeTemplateHashes := nil;
  xsltCleanupTemplates := nil;

  { xsltlocale.inc }
{$IFDEF HAVE_STRXFRM_L}
  xsltNewLocale := nil;
  xsltFreeLocale := nil;
  xsltStrxfrm := nil;
  xsltLocaleStrcmp := nil;
  xsltFreeLocales := nil;
{$ENDIF} {HAVE_STRXFRM_L}

  { xsltInternals.inc }
{$IFDEF XSLT_REFACTORED}
  xsltPointerListCreate := nil;
  xsltPointerListFree := nil;
  xsltPointerListClear := nil;
  xsltPointerListAddSize := nil;
{$ELSE} (* XSLT_REFACTORED *)
  xsltNewStylesheet := nil;
  xsltParseStylesheetFile := nil;
  xsltFreeStylesheet := nil;
  xsltIsBlank := nil;
  xsltFreeStackElemList := nil;
  xsltDecimalFormatGetByName := nil;
  xsltDecimalFormatGetByQName := nil;
  xsltParseStylesheetProcess := nil;
  xsltParseStylesheetOutput := nil;
  xsltParseStylesheetDoc := nil;
  xsltParseStylesheetImportedDoc := nil;
  xsltParseStylesheetUser := nil;
  xsltLoadStylesheetPI := nil;
  xsltNumberFormat := nil;
  xsltFormatNumberConversion := nil;
  xsltParseTemplateContent := nil;
  xsltAllocateExtra := nil;
  xsltAllocateExtraCtxt := nil;
  xsltCreateRVT := nil;
  xsltRegisterTmpRVT := nil;
  xsltRegisterLocalRVT := nil;
  xsltRegisterPersistRVT := nil;
  xsltExtensionInstructionResultRegister := nil;
  xsltExtensionInstructionResultFinalize := nil;
  xsltFlagRVTs := nil;
  xsltFreeRVTs := nil;
  xsltReleaseRVT := nil;
  xsltCompileAttr := nil;
  xsltEvalAVT := nil;
  xsltFreeAVTList := nil;
  xsltUninit := nil;
{$ENDIF}
{$IFDEF XSLT_REFACTORED}
  xsltParseSequenceConstructor := nil;
  xsltParseAnyXSLTElem := nil;
{$IFDEF XSLT_REFACTORED_XSLT_NSCOMP}
  xsltRestoreDocumentNamespaces := nil;
{$ENDIF}
{$ENDIF} (* XSLT_REFACTORED *)
  xsltInitCtxtKey := nil;
  xsltInitAllDocKeys := nil;

  { attributes.inc }
  xsltParseStylesheetAttributeSet := nil;
  xsltFreeAttributeSetsHashes := nil;
  xsltApplyAttributeSet := nil;
  xsltResolveStylesheetAttributeSet := nil;

  { documents.inc }
  xsltNewDocument := nil;
  xsltLoadDocument := nil;
  xsltFindDocument := nil;
  xsltFreeDocuments := nil;
  xsltLoadStyleDocument := nil;
  xsltNewStyleDocument := nil;
  xsltFreeStyleDocuments := nil;
  xsltSetLoaderFunc := nil;

  { extensions.inc }
  xsltInitGlobals := nil;
  xsltRegisterExtModule := nil;
  xsltRegisterExtModuleFull := nil;
  xsltUnregisterExtModule := nil;
  xsltGetExtData := nil;
  xsltStyleGetExtData := nil;
{$IFDEF XSLT_REFACTORED}
  xsltStyleStylesheetLevelGetExtData := nil;
{$ENDIF}
  xsltShutdownCtxtExts := nil;
  xsltShutdownExts := nil;
  xsltXPathGetTransformContext := nil;
  xsltRegisterExtModuleFunction := nil;
  xsltExtModuleFunctionLookup := nil;
  xsltUnregisterExtModuleFunction := nil;
  xsltNewElemPreComp := nil;
  xsltInitElemPreComp := nil;
  xsltRegisterExtModuleElement := nil;
  xsltExtElementLookup := nil;
  xsltExtModuleElementLookup := nil;
  xsltExtModuleElementPreComputeLookup := nil;
  xsltUnregisterExtModuleElement := nil;
  xsltRegisterExtModuleTopLevel := nil;
  xsltExtModuleTopLevelLookup := nil;
  xsltUnregisterExtModuleTopLevel := nil;
  xsltRegisterExtFunction := nil;
  xsltRegisterExtElement := nil;
  xsltRegisterExtPrefix := nil;
  xsltCheckExtPrefix := nil;
  xsltCheckExtURI := nil;
  xsltInitCtxtExts := nil;
  xsltFreeCtxtExts := nil;
  xsltFreeExts := nil;
  xsltPreComputeExtModuleElement := nil;
  xsltGetExtInfo := nil;
  xsltRegisterTestModule := nil;
  xsltDebugDumpExtensions := nil;

  { extra.inc }
  xsltFunctionNodeSet := nil;
  xsltDebug := nil;
  xsltRegisterExtras := nil;
  xsltRegisterAllExtras := nil;

  { functions.inc }
  xsltXPathFunctionLookup := nil;
  xsltDocumentFunction := nil;
  xsltKeyFunction := nil;
  xsltUnparsedEntityURIFunction := nil;
  xsltFormatNumberFunction := nil;
  xsltGenerateIdFunction := nil;
  xsltSystemPropertyFunction := nil;
  xsltElementAvailableFunction := nil;
  xsltFunctionAvailableFunction := nil;
  xsltRegisterAllFunctions := nil;

  { imports.inc }
  xsltParseStylesheetImport := nil;
  xsltParseStylesheetInclude := nil;
  xsltNextImport := nil;
  xsltNeedElemSpaceHandling := nil;
  xsltFindElemSpaceHandling := nil;
  xsltFindTemplate := nil;

  { keys.inc }
  xsltAddKey := nil;
  xsltGetKey := nil;
  xsltInitCtxtKeys := nil;
  xsltFreeKeys := nil;
  xsltFreeDocumentKeys := nil;

  { namespaces.inc }
  xsltNamespaceAlias := nil;
  xsltGetNamespace := nil;
  xsltGetPlainNamespace := nil;
  xsltGetSpecialNamespace := nil;
  xsltCopyNamespace := nil;
  xsltCopyNamespaceList := nil;
  xsltFreeNamespaceAliasHashes := nil;

  { preproc.inc }
  xsltDocumentComp := nil;
  xsltStylePreCompute := nil;
  xsltFreeStylePreComps := nil;

  { security.inc }
  xsltNewSecurityPrefs := nil;
  xsltFreeSecurityPrefs := nil;
  xsltSetSecurityPrefs := nil;
  xsltGetSecurityPrefs := nil;
  xsltSetDefaultSecurityPrefs := nil;
  xsltGetDefaultSecurityPrefs := nil;
  xsltSetCtxtSecurityPrefs := nil;
  xsltSecurityAllow := nil;
  xsltSecurityForbid := nil;
  xsltCheckWrite := nil;
  xsltCheckRead := nil;

  { templates.inc }
  xsltEvalXPathPredicate := nil;
  xsltEvalTemplateString := nil;
  xsltEvalAttrValueTemplate := nil;
  xsltEvalStaticAttrValueTemplate := nil;
  xsltEvalXPathString := nil;
  xsltEvalXPathStringNs := nil;
  xsltTemplateProcess := nil;
  xsltAttrListTemplateProcess := nil;
  xsltAttrTemplateProcess := nil;
  xsltAttrTemplateValueProcess := nil;
  xsltAttrTemplateValueProcessNode := nil;

  { transform.inc }
  xsltSetXIncludeDefault := nil;
  xsltGetXIncludeDefault := nil;
  xsltNewTransformContext := nil;
  xsltFreeTransformContext := nil;
  xsltApplyStylesheetUser := nil;
  xsltProcessOneNode := nil;
  xsltApplyStripSpaces := nil;
  xsltApplyStylesheet := nil;
  xsltProfileStylesheet := nil;
  xsltRunStylesheet := nil;
  xsltRunStylesheetUser := nil;
  xsltApplyOneTemplate := nil;
  xsltDocumentElem := nil;
  xsltSort := nil;
  xsltCopy := nil;
  xsltText := nil;
  xsltElement := nil;
  xsltComment := nil;
  xsltAttribute := nil;
  xsltProcessingInstruction := nil;
  xsltCopyOf := nil;
  xsltValueOf := nil;
  xsltNumber := nil;
  xsltApplyImports := nil;
  xsltCallTemplate := nil;
  xsltApplyTemplates := nil;
  xsltChoose := nil;
  xsltIf := nil;
  xsltForEach := nil;
  xsltRegisterAllElement := nil;
  xsltCopyTextString := nil;
  xsltLocalVariablePop := nil;
  xsltLocalVariablePush := nil;
  xslHandleDebugger := nil;

  { variables.inc }
  xsltEvalGlobalVariables := nil;
  xsltEvalUserParams := nil;
  xsltQuoteUserParams := nil;
  xsltEvalOneUserParam := nil;
  xsltQuoteOneUserParam := nil;
  xsltParseGlobalVariable := nil;
  xsltParseGlobalParam := nil;
  xsltParseStylesheetVariable := nil;
  xsltParseStylesheetParam := nil;
  xsltParseStylesheetCallerParam := nil;
  xsltAddStackElemList := nil;
  xsltFreeGlobalVariables := nil;
  xsltVariableLookup := nil;
  xsltXPathVariableLookup := nil;

  { xslt.inc }
  xsltInit := nil;
  xsltCleanupGlobals := nil;

  { xsltutils.inc }
  xsltGetNsProp := nil;
  xsltGetCNsProp := nil;
  xsltGetUTF8Char := nil;
  xsltDebugSetDefaultTrace := nil;
  xsltDebugGetDefaultTrace := nil;
  xsltPrintErrorContext := nil;
  xsltMessage := nil;
  xsltSetGenericErrorFunc := nil;
  xsltSetGenericDebugFunc := nil;
  xsltSetTransformErrorFunc := nil;
  xsltTransformError := nil;
  xsltSetCtxtParseOptions := nil;
  xsltDocumentSortFunction := nil;
  xsltSetSortFunc := nil;
  xsltSetCtxtSortFunc := nil;
  xsltDefaultSortFunction := nil;
  xsltDoSortFunction := nil;
  xsltComputeSortResult := nil;
  xsltSplitQName := nil;
  xsltGetQNameURI := nil;
  xsltGetQNameURI2 := nil;
  xsltSaveResultTo := nil;
  xsltSaveResultToFilename := nil;
  xsltSaveResultToFile := nil;
  xsltSaveResultToFd := nil;
  xsltSaveResultToString := nil;
  xsltXPathCompile := nil;
  xsltXPathCompileFlags := nil;
  xsltSaveProfiling := nil;
  xsltGetProfileInformation := nil;
  xsltTimestamp := nil;
  xsltCalibrateAdjust := nil;
  xsltSetDebuggerStatus := nil;
  xsltGetDebuggerStatus := nil;
  xsltSetDebuggerCallbacks := nil;
  xslAddCall := nil;
  xslDropCall := nil;
{$ENDIF} {NIL_FUNCVARS_ON_FREE}
end;

end.

