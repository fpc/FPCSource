{
  Translation of the libxml2 headers for FreePascal
  Copyright (C) 2008 by Ivo Steinmann
}

{$IFNDEF FPC_DOTTEDUNITS}
unit xml2dyn;
{$ENDIF FPC_DOTTEDUNITS}

{$DEFINE LIBXML_DYN}
{$DEFINE NIL_FUNCVARS_ON_FREE}

{$i xml2h.inc}

{$IFNDEF NO_EXTERNAL_VARS}
  {$DEFINE NO_EXTERNAL_VARS}
{$ENDIF}

{$i xml2.inc}

var
  libXmlHandle: TLibHandle = NilHandle;

function LoadLibXML(AFileName: String = ''; InstallFpcMemFunc: Boolean = True): Boolean;
procedure FreeLibXML;

implementation

{$i fpcfunctions.inc}

{$i macros.inc}

function LoadLibXML(AFileName: String; InstallFpcMemFunc: Boolean): Boolean;
var
  mask : TFPUExceptionMask;
begin
  if AFileName = '' then
    AFileName := xml2lib;
  libXmlHandle := LoadLibrary(AFileName);
  if libXmlHandle <> NilHandle then
  begin
  { xmlregexp.inc }
   {__emptyExp := xmlExpNodePtrPtr(GetProcAddress(libXmlHandle, 'emptyExp'));
    __forbiddenExp := xmlExpNodePtrPtr(GetProcAddress(libXmlHandle, 'forbiddenExp'));}

  { paserInternals.inc }
    //__xmlParserMaxDepth := PCardinal(GetProcAddress(libXmlHandle, 'xmlParserMaxDepth'));

  {  }
   {xmlStringComment := PAnsiChar(GetProcAddress(libXmlHandle, 'xmlStringComment'));
    xmlStringText := PAnsiChar(GetProcAddress(libXmlHandle, 'xmlStringText'));
    xmlStringTextNoenc := PAnsiChar(GetProcAddress(libXmlHandle, 'xmlStringTextNoenc'));}

  { chvalid.inc }
    __xmlIsBaseCharGroup := xmlChRangeGroupPtr(GetProcAddress(libXmlHandle, 'xmlIsBaseCharGroup'));
    __xmlIsCharGroup := xmlChRangeGroupPtr(GetProcAddress(libXmlHandle, 'xmlIsCharGroup'));
    __xmlIsCombiningGroup := xmlChRangeGroupPtr(GetProcAddress(libXmlHandle, 'xmlIsCombiningGroup'));
    __xmlIsDigitGroup := xmlChRangeGroupPtr(GetProcAddress(libXmlHandle, 'xmlIsDigitGroup'));
    __xmlIsExtenderGroup := xmlChRangeGroupPtr(GetProcAddress(libXmlHandle, 'xmlIsExtenderGroup'));
    __xmlIsIdeographicGroup := xmlChRangeGroupPtr(GetProcAddress(libXmlHandle, 'xmlIsIdeographicGroup'));
    __xmlIsPubidChar_tab := GetProcAddress(libXmlHandle, 'xmlIsPubidChar_tab');

  { globals.inc }
    varxmlMalloc := PxmlMallocFunc(GetProcAddress(libXmlHandle, 'xmlMalloc'));
    varxmlMallocAtomic := PxmlMallocFunc(GetProcAddress(libXmlHandle, 'xmlMallocAtomic'));
    varxmlRealloc := PxmlReallocFunc(GetProcAddress(libXmlHandle, 'xmlRealloc'));
    varxmlFree := PxmlFreeFunc(GetProcAddress(libXmlHandle, 'xmlFree'));
    varxmlMemStrdup := PxmlStrdupFunc(GetProcAddress(libXmlHandle, 'xmlMemStrdup'));

  { xpath.inc }
   {__xmlXPathNAN := PDouble(GetProcAddress(libXmlHandle, 'xmlXPathNAN'));
    __xmlXPathNINF := PDouble(GetProcAddress(libXmlHandle, 'xmlXPathNINF'));
    __xmlXPathPINF := PDouble(GetProcAddress(libXmlHandle, 'xmlXPathPINF'));}


    { xmlversion.inc }
    Pointer(xmlCheckVersion) := GetProcAddress(libXmlHandle, 'xmlCheckVersion');

    { catalog.inc }
  {$IFDEF LIBXML_CATALOG_ENABLED}
    Pointer(xmlNewCatalog) := GetProcAddress(libXmlHandle, 'xmlNewCatalog');
    Pointer(xmlLoadACatalog) := GetProcAddress(libXmlHandle, 'xmlLoadACatalog');
    Pointer(xmlLoadSGMLSuperCatalog) := GetProcAddress(libXmlHandle, 'xmlLoadSGMLSuperCatalog');
    Pointer(xmlConvertSGMLCatalog) := GetProcAddress(libXmlHandle, 'xmlConvertSGMLCatalog');
    //xmlLoadACatalog
    Pointer(xmlACatalogAdd) := GetProcAddress(libXmlHandle, 'xmlACatalogAdd');
    Pointer(xmlACatalogRemove) := GetProcAddress(libXmlHandle, 'xmlACatalogRemove');
    Pointer(xmlACatalogResolve) := GetProcAddress(libXmlHandle, 'xmlACatalogResolve');
    Pointer(xmlACatalogResolveSystem) := GetProcAddress(libXmlHandle, 'xmlACatalogResolveSystem');
    Pointer(xmlACatalogResolvePublic) := GetProcAddress(libXmlHandle, 'xmlACatalogResolvePublic');
    Pointer(xmlACatalogResolveURI) := GetProcAddress(libXmlHandle, 'xmlACatalogResolveURI');
  {$IFDEF LIBXML_OUTPUT_ENABLED}
    Pointer(xmlACatalogDump) := GetProcAddress(libXmlHandle, 'xmlACatalogDump');
  {$ENDIF} (* LIBXML_OUTPUT_ENABLED *)
    Pointer(xmlFreeCatalog) := GetProcAddress(libXmlHandle, 'xmlFreeCatalog');
    Pointer(xmlCatalogIsEmpty) := GetProcAddress(libXmlHandle, 'xmlCatalogIsEmpty');
    Pointer(xmlInitializeCatalog) := GetProcAddress(libXmlHandle, 'xmlInitializeCatalog');
    Pointer(xmlLoadCatalog) := GetProcAddress(libXmlHandle, 'xmlLoadCatalog');
    Pointer(xmlLoadCatalogs) := GetProcAddress(libXmlHandle, 'xmlLoadCatalogs');
    Pointer(xmlCatalogCleanup) := GetProcAddress(libXmlHandle, 'xmlCatalogCleanup');
  {$IFDEF LIBXML_OUTPUT_ENABLED}
    Pointer(xmlCatalogDump) := GetProcAddress(libXmlHandle, 'xmlCatalogDump');
  {$ENDIF} (* LIBXML_OUTPUT_ENABLED *)
    Pointer(xmlCatalogResolve) := GetProcAddress(libXmlHandle, 'xmlCatalogResolve');
    Pointer(xmlCatalogResolveSystem) := GetProcAddress(libXmlHandle, 'xmlCatalogResolveSystem');
    Pointer(xmlCatalogResolvePublic) := GetProcAddress(libXmlHandle, 'xmlCatalogResolvePublic');
    Pointer(xmlCatalogResolveURI) := GetProcAddress(libXmlHandle, 'xmlCatalogResolveURI');
    Pointer(xmlCatalogAdd) := GetProcAddress(libXmlHandle, 'xmlCatalogAdd');
    Pointer(xmlCatalogRemove) := GetProcAddress(libXmlHandle, 'xmlCatalogRemove');
    Pointer(xmlParseCatalogFile) := GetProcAddress(libXmlHandle, 'xmlParseCatalogFile');
    Pointer(xmlCatalogConvert) := GetProcAddress(libXmlHandle, 'xmlCatalogConvert');
    Pointer(xmlCatalogFreeLocal) := GetProcAddress(libXmlHandle, 'xmlCatalogFreeLocal');
    Pointer(xmlCatalogAddLocal) := GetProcAddress(libXmlHandle, 'xmlCatalogAddLocal');
    Pointer(xmlCatalogLocalResolve) := GetProcAddress(libXmlHandle, 'xmlCatalogLocalResolve');
    Pointer(xmlCatalogLocalResolveURI) := GetProcAddress(libXmlHandle, 'xmlCatalogLocalResolveURI');
    Pointer(xmlCatalogSetDebug) := GetProcAddress(libXmlHandle, 'xmlCatalogSetDebug');
    Pointer(xmlCatalogSetDefaultPrefer) := GetProcAddress(libXmlHandle, 'xmlCatalogSetDefaultPrefer');
    Pointer(xmlCatalogSetDefaults) := GetProcAddress(libXmlHandle, 'xmlCatalogSetDefaults');
    Pointer(xmlCatalogGetDefaults) := GetProcAddress(libXmlHandle, 'xmlCatalogGetDefaults');
  {$ENDIF} (* LIBXML_CATALOG_ENABLED *)

    { chvalid.inc }
    Pointer(xmlCharInRange) := GetProcAddress(libXmlHandle, 'xmlCharInRange');
    Pointer(xmlIsBaseChar) := GetProcAddress(libXmlHandle, 'xmlIsBaseChar');
    Pointer(xmlIsBlank) := GetProcAddress(libXmlHandle, 'xmlIsBlank');
    Pointer(xmlIsChar) := GetProcAddress(libXmlHandle, 'xmlIsChar');
    Pointer(xmlIsCombining) := GetProcAddress(libXmlHandle, 'xmlIsCombining');
    Pointer(xmlIsDigit) := GetProcAddress(libXmlHandle, 'xmlIsDigit');
    Pointer(xmlIsExtender) := GetProcAddress(libXmlHandle, 'xmlIsExtender');
    Pointer(xmlIsIdeographic) := GetProcAddress(libXmlHandle, 'xmlIsIdeographic');
    Pointer(xmlIsPubidChar) := GetProcAddress(libXmlHandle, 'xmlIsPubidChar');

    { dict.inc }
    Pointer(xmlInitializeDict) := GetProcAddress(libXmlHandle, 'xmlInitializeDict');
    Pointer(xmlDictCreate) := GetProcAddress(libXmlHandle, 'xmlDictCreate');
    Pointer(xmlDictSetLimit) := GetProcAddress(libXmlHandle, 'xmlDictSetLimit');
    Pointer(xmlDictGetUsage) := GetProcAddress(libXmlHandle, 'xmlDictGetUsage');
    Pointer(xmlDictCreateSub) := GetProcAddress(libXmlHandle, 'xmlDictCreateSub');
    Pointer(xmlDictReference) := GetProcAddress(libXmlHandle, 'xmlDictReference');
    Pointer(xmlDictFree) := GetProcAddress(libXmlHandle, 'xmlDictFree');
    Pointer(xmlDictLookup) := GetProcAddress(libXmlHandle, 'xmlDictLookup');
    Pointer(xmlDictExists) := GetProcAddress(libXmlHandle, 'xmlDictExists');
    Pointer(xmlDictQLookup) := GetProcAddress(libXmlHandle, 'xmlDictQLookup');
    Pointer(xmlDictOwns) := GetProcAddress(libXmlHandle, 'xmlDictOwns');
    Pointer(xmlDictSize) := GetProcAddress(libXmlHandle, 'xmlDictSize');
    Pointer(xmlDictCleanup) := GetProcAddress(libXmlHandle, 'xmlDictCleanup');

    { encoding.inc }
    Pointer(xmlInitCharEncodingHandlers) := GetProcAddress(libXmlHandle, 'xmlInitCharEncodingHandlers');
    Pointer(xmlCleanupCharEncodingHandlers) := GetProcAddress(libXmlHandle, 'xmlCleanupCharEncodingHandlers');
    Pointer(xmlRegisterCharEncodingHandler) := GetProcAddress(libXmlHandle, 'xmlRegisterCharEncodingHandler');
    Pointer(xmlGetCharEncodingHandler) := GetProcAddress(libXmlHandle, 'xmlGetCharEncodingHandler');
    Pointer(xmlFindCharEncodingHandler) := GetProcAddress(libXmlHandle, 'xmlFindCharEncodingHandler');
    Pointer(xmlNewCharEncodingHandler) := GetProcAddress(libXmlHandle, 'xmlNewCharEncodingHandler');
    Pointer(xmlAddEncodingAlias) := GetProcAddress(libXmlHandle, 'xmlAddEncodingAlias');
    Pointer(xmlDelEncodingAlias) := GetProcAddress(libXmlHandle, 'xmlDelEncodingAlias');
    Pointer(xmlGetEncodingAlias) := GetProcAddress(libXmlHandle, 'xmlGetEncodingAlias');
    Pointer(xmlCleanupEncodingAliases) := GetProcAddress(libXmlHandle, 'xmlCleanupEncodingAliases');
    Pointer(xmlParseCharEncoding) := GetProcAddress(libXmlHandle, 'xmlParseCharEncoding');
    Pointer(xmlGetCharEncodingName) := GetProcAddress(libXmlHandle, 'xmlGetCharEncodingName');
    Pointer(xmlDetectCharEncoding) := GetProcAddress(libXmlHandle, 'xmlDetectCharEncoding');
    Pointer(xmlCharEncOutFunc) := GetProcAddress(libXmlHandle, 'xmlCharEncOutFunc');
    Pointer(xmlCharEncInFunc) := GetProcAddress(libXmlHandle, 'xmlCharEncInFunc');
    Pointer(xmlCharEncFirstLine) := GetProcAddress(libXmlHandle, 'xmlCharEncFirstLine');
    Pointer(xmlCharEncCloseFunc) := GetProcAddress(libXmlHandle, 'xmlCharEncCloseFunc');
  {$IFDEF LIBXML_OUTPUT_ENABLED}
    Pointer(UTF8Toisolat1) := GetProcAddress(libXmlHandle, 'UTF8Toisolat1');
  {$ENDIF} (* LIBXML_OUTPUT_ENABLED *)
    Pointer(isolat1ToUTF8) := GetProcAddress(libXmlHandle, 'isolat1ToUTF8');

    { tree.inc }
  {$IF defined(LIBXML_TREE_ENABLED) or defined(LIBXML_XPATH_ENABLED) or defined(LIBXML_SCHEMAS_ENABLED) or
    defined(LIBXML_DEBUG_ENABLED) or defined (LIBXML_HTML_ENABLED) or defined(LIBXML_SAX1_ENABLED) or
    defined(LIBXML_HTML_ENABLED) or defined(LIBXML_WRITER_ENABLED) or defined(LIBXML_DOCB_ENABLED)}
    Pointer(xmlValidateNCName) := GetProcAddress(libXmlHandle, 'xmlValidateNCName');
  {$ENDIF}
  {$IF defined(LIBXML_TREE_ENABLED) or defined(LIBXML_SCHEMAS_ENABLED)}
    Pointer(xmlValidateQName) := GetProcAddress(libXmlHandle, 'xmlValidateQName');
    Pointer(xmlValidateName) := GetProcAddress(libXmlHandle, 'xmlValidateName');
    Pointer(xmlValidateNMToken) := GetProcAddress(libXmlHandle, 'xmlValidateNMToken');
  {$ENDIF}
    Pointer(xmlBuildQName) := GetProcAddress(libXmlHandle, 'xmlBuildQName');
    Pointer(xmlSplitQName2) := GetProcAddress(libXmlHandle, 'xmlSplitQName2');
    Pointer(xmlSplitQName3) := GetProcAddress(libXmlHandle, 'xmlSplitQName3');
    Pointer(xmlSetBufferAllocationScheme) := GetProcAddress(libXmlHandle, 'xmlSetBufferAllocationScheme');
    Pointer(xmlGetBufferAllocationScheme) := GetProcAddress(libXmlHandle, 'xmlGetBufferAllocationScheme');
    Pointer(xmlBufferCreate) := GetProcAddress(libXmlHandle, 'xmlBufferCreate');
    Pointer(xmlBufferCreateSize) := GetProcAddress(libXmlHandle, 'xmlBufferCreateSize');
    Pointer(xmlBufferCreateStatic) := GetProcAddress(libXmlHandle, 'xmlBufferCreateStatic');
    Pointer(xmlBufferResize) := GetProcAddress(libXmlHandle, 'xmlBufferResize');
    Pointer(xmlBufferFree) := GetProcAddress(libXmlHandle, 'xmlBufferFree');
    Pointer(xmlBufferDump) := GetProcAddress(libXmlHandle, 'xmlBufferDump');
    Pointer(xmlBufferAdd) := GetProcAddress(libXmlHandle, 'xmlBufferAdd');
    Pointer(xmlBufferAddHead) := GetProcAddress(libXmlHandle, 'xmlBufferAddHead');
    Pointer(xmlBufferCat) := GetProcAddress(libXmlHandle, 'xmlBufferCat');
    Pointer(xmlBufferCCat) := GetProcAddress(libXmlHandle, 'xmlBufferCCat');
    Pointer(xmlBufferShrink) := GetProcAddress(libXmlHandle, 'xmlBufferShrink');
    Pointer(xmlBufferGrow) := GetProcAddress(libXmlHandle, 'xmlBufferGrow');
    Pointer(xmlBufferEmpty) := GetProcAddress(libXmlHandle, 'xmlBufferEmpty');
    Pointer(xmlBufferContent) := GetProcAddress(libXmlHandle, 'xmlBufferContent');
    Pointer(xmlBufferDetach) := GetProcAddress(libXmlHandle, 'xmlBufferDetach');
    Pointer(xmlBufferSetAllocationScheme) := GetProcAddress(libXmlHandle, 'xmlBufferSetAllocationScheme');
    Pointer(xmlBufferLength) := GetProcAddress(libXmlHandle, 'xmlBufferLength');
    Pointer(xmlCreateIntSubset) := GetProcAddress(libXmlHandle, 'xmlCreateIntSubset');
    Pointer(xmlNewDtd) := GetProcAddress(libXmlHandle, 'xmlNewDtd');
    Pointer(xmlGetIntSubset) := GetProcAddress(libXmlHandle, 'xmlGetIntSubset');
    Pointer(xmlFreeDtd) := GetProcAddress(libXmlHandle, 'xmlFreeDtd');
  {$IFDEF LIBXML_LEGACY_ENABLED}
    Pointer(xmlNewGlobalNs) := GetProcAddress(libHandle, 'xmlNewGlobalNs');
  {$ENDIF} (* LIBXML_LEGACY_ENABLED *)
    Pointer(xmlNewNs) := GetProcAddress(libXmlHandle, 'xmlNewNs');
    Pointer(xmlFreeNs) := GetProcAddress(libXmlHandle, 'xmlFreeNs');
    Pointer(xmlFreeNsList) := GetProcAddress(libXmlHandle, 'xmlFreeNsList');
    Pointer(xmlNewDoc) := GetProcAddress(libXmlHandle, 'xmlNewDoc');
    Pointer(xmlFreeDoc) := GetProcAddress(libXmlHandle, 'xmlFreeDoc');
    Pointer(xmlNewDocProp) := GetProcAddress(libXmlHandle, 'xmlNewDocProp');
  {$IF defined(LIBXML_TREE_ENABLED) or defined(LIBXML_HTML_ENABLED) or defined(LIBXML_SCHEMAS_ENABLED)}
    Pointer(xmlNewProp) := GetProcAddress(libXmlHandle, 'xmlNewProp');
  {$ENDIF}
    Pointer(xmlNewNsProp) := GetProcAddress(libXmlHandle, 'xmlNewNsProp');
    Pointer(xmlNewNsPropEatName) := GetProcAddress(libXmlHandle, 'xmlNewNsPropEatName');
    Pointer(xmlFreePropList) := GetProcAddress(libXmlHandle, 'xmlFreePropList');
    Pointer(xmlFreeProp) := GetProcAddress(libXmlHandle, 'xmlFreeProp');
    Pointer(xmlCopyProp) := GetProcAddress(libXmlHandle, 'xmlCopyProp');
    Pointer(xmlCopyPropList) := GetProcAddress(libXmlHandle, 'xmlCopyPropList');
  {$IFDEF LIBXML_TREE_ENABLED}
    Pointer(xmlCopyDtd) := GetProcAddress(libXmlHandle, 'xmlCopyDtd');
  {$ENDIF} (* LIBXML_TREE_ENABLED *)
  {$IF defined(LIBXML_TREE_ENABLED) or defined(LIBXML_SCHEMAS_ENABLED)}
    Pointer(xmlCopyDoc) := GetProcAddress(libXmlHandle, 'xmlCopyDoc');
  {$ENDIF} (* defined(LIBXML_TREE_ENABLED) || defined(LIBXML_SCHEMAS_ENABLED) *)
    Pointer(xmlNewDocNode) := GetProcAddress(libXmlHandle, 'xmlNewDocNode');
    Pointer(xmlNewDocNodeEatName) := GetProcAddress(libXmlHandle, 'xmlNewDocNodeEatName');
    Pointer(xmlNewNode) := GetProcAddress(libXmlHandle, 'xmlNewNode');
    Pointer(xmlNewNodeEatName) := GetProcAddress(libXmlHandle, 'xmlNewNodeEatName');
  {$IF defined(LIBXML_TREE_ENABLED) or defined(LIBXML_SCHEMAS_ENABLED)}
    Pointer(xmlNewChild) := GetProcAddress(libXmlHandle, 'xmlNewChild');
  {$ENDIF}
    Pointer(xmlNewDocText) := GetProcAddress(libXmlHandle, 'xmlNewDocText');
    Pointer(xmlNewText) := GetProcAddress(libXmlHandle, 'xmlNewText');
    Pointer(xmlNewDocPI) := GetProcAddress(libXmlHandle, 'xmlNewDocPI');
    Pointer(xmlNewPI) := GetProcAddress(libXmlHandle, 'xmlNewPI');
    Pointer(xmlNewDocTextLen) := GetProcAddress(libXmlHandle, 'xmlNewDocTextLen');
    Pointer(xmlNewTextLen) := GetProcAddress(libXmlHandle, 'xmlNewTextLen');
    Pointer(xmlNewDocComment) := GetProcAddress(libXmlHandle, 'xmlNewDocComment');
    Pointer(xmlNewComment) := GetProcAddress(libXmlHandle, 'xmlNewComment');
    Pointer(xmlNewCDataBlock) := GetProcAddress(libXmlHandle, 'xmlNewCDataBlock');
    Pointer(xmlNewCharRef) := GetProcAddress(libXmlHandle, 'xmlNewCharRef');
    Pointer(xmlNewReference) := GetProcAddress(libXmlHandle, 'xmlNewReference');
    Pointer(xmlCopyNode) := GetProcAddress(libXmlHandle, 'xmlCopyNode');
    Pointer(xmlDocCopyNode) := GetProcAddress(libXmlHandle, 'xmlDocCopyNode');
    Pointer(xmlDocCopyNodeList) := GetProcAddress(libXmlHandle, 'xmlDocCopyNodeList');
    Pointer(xmlCopyNodeList) := GetProcAddress(libXmlHandle, 'xmlCopyNodeList');
  {$IFDEF LIBXML_TREE_ENABLED}
    Pointer(xmlNewTextChild) := GetProcAddress(libXmlHandle, 'xmlNewTextChild');
    Pointer(xmlNewDocRawNode) := GetProcAddress(libXmlHandle, 'xmlNewDocRawNode');
    Pointer(xmlNewDocFragment) := GetProcAddress(libXmlHandle, 'xmlNewDocFragment');
  {$ENDIF} (* LIBXML_TREE_ENABLED *)
    Pointer(xmlGetLineNo) := GetProcAddress(libXmlHandle, 'xmlGetLineNo');
  {$IF defined(LIBXML_TREE_ENABLED) or defined(LIBXML_DEBUG_ENABLED)}
    Pointer(xmlGetNodePath) := GetProcAddress(libXmlHandle, 'xmlGetNodePath');
  {$ENDIF} (* defined(LIBXML_TREE_ENABLED) || defined(LIBXML_DEBUG_ENABLED) *)
    Pointer(xmlDocGetRootElement) := GetProcAddress(libXmlHandle, 'xmlDocGetRootElement');
    Pointer(xmlGetLastChild) := GetProcAddress(libXmlHandle, 'xmlGetLastChild');
    Pointer(xmlNodeIsText) := GetProcAddress(libXmlHandle, 'xmlNodeIsText');
    Pointer(xmlIsBlankNode) := GetProcAddress(libXmlHandle, 'xmlIsBlankNode');
  {$IF defined(LIBXML_TREE_ENABLED) or defined(LIBXML_WRITER_ENABLED)}
    Pointer(xmlDocSetRootElement) := GetProcAddress(libXmlHandle, 'xmlDocSetRootElement');
  {$ENDIF} (* defined(LIBXML_TREE_ENABLED) || defined(LIBXML_WRITER_ENABLED) *)
  {$IFDEF LIBXML_TREE_ENABLED}
    Pointer(xmlNodeSetName) := GetProcAddress(libXmlHandle, 'xmlNodeSetName');
  {$ENDIF} (* LIBXML_TREE_ENABLED *)
    Pointer(xmlAddChild) := GetProcAddress(libXmlHandle, 'xmlAddChild');
    Pointer(xmlAddChildList) := GetProcAddress(libXmlHandle, 'xmlAddChildList');
  {$IF defined(LIBXML_TREE_ENABLED) or defined(LIBXML_WRITER_ENABLED)}
    Pointer(xmlReplaceNode) := GetProcAddress(libXmlHandle, 'xmlReplaceNode');
  {$ENDIF} (* defined(LIBXML_TREE_ENABLED) || defined(LIBXML_WRITER_ENABLED) *)
  {$IF defined(LIBXML_TREE_ENABLED) or defined(LIBXML_HTML_ENABLED) or defined(LIBXML_SCHEMAS_ENABLED)}
    Pointer(xmlAddPrevSibling) := GetProcAddress(libXmlHandle, 'xmlAddPrevSibling');
  {$ENDIF} (* LIBXML_TREE_ENABLED || LIBXML_HTML_ENABLED || LIBXML_SCHEMAS_ENABLED *)
    Pointer(xmlAddSibling) := GetProcAddress(libXmlHandle, 'xmlAddSibling');
    Pointer(xmlAddNextSibling) := GetProcAddress(libXmlHandle, 'xmlAddNextSibling');
    Pointer(xmlUnlinkNode) := GetProcAddress(libXmlHandle, 'xmlUnlinkNode');
    Pointer(xmlTextMerge) := GetProcAddress(libXmlHandle, 'xmlTextMerge');
    Pointer(xmlTextConcat) := GetProcAddress(libXmlHandle, 'xmlTextConcat');
    Pointer(xmlFreeNodeList) := GetProcAddress(libXmlHandle, 'xmlFreeNodeList');
    Pointer(xmlFreeNode) := GetProcAddress(libXmlHandle, 'xmlFreeNode');
    Pointer(xmlSetTreeDoc) := GetProcAddress(libXmlHandle, 'xmlSetTreeDoc');
    Pointer(xmlSetListDoc) := GetProcAddress(libXmlHandle, 'xmlSetListDoc');
    Pointer(xmlSearchNs) := GetProcAddress(libXmlHandle, 'xmlSearchNs');
    Pointer(xmlSearchNsByHref) := GetProcAddress(libXmlHandle, 'xmlSearchNsByHref');
  {$IF defined(LIBXML_TREE_ENABLED) or defined(LIBXML_XPATH_ENABLED) or defined(LIBXML_SCHEMAS_ENABLED)}
    Pointer(xmlGetNsList) := GetProcAddress(libXmlHandle, 'xmlGetNsList');
  {$ENDIF} (* defined(LIBXML_TREE_ENABLED) || defined(LIBXML_XPATH_ENABLED) *)
    Pointer(xmlSetNs) := GetProcAddress(libXmlHandle, 'xmlSetNs');
    Pointer(xmlCopyNamespace) := GetProcAddress(libXmlHandle, 'xmlCopyNamespace');
    Pointer(xmlCopyNamespaceList) := GetProcAddress(libXmlHandle, 'xmlCopyNamespaceList');
  {$IF defined(LIBXML_TREE_ENABLED) or defined(LIBXML_XINCLUDE_ENABLED) or defined(LIBXML_SCHEMAS_ENABLED) or defined(LIBXML_HTML_ENABLED)}
    Pointer(xmlSetProp) := GetProcAddress(libXmlHandle, 'xmlSetProp');
    Pointer(xmlSetNsProp) := GetProcAddress(libXmlHandle, 'xmlSetNsProp');
  {$ENDIF} (* defined(LIBXML_TREE_ENABLED) || defined(LIBXML_XINCLUDE_ENABLED) || defined(LIBXML_SCHEMAS_ENABLED) || defined(LIBXML_HTML_ENABLED) *)
    Pointer(xmlGetNoNsProp) := GetProcAddress(libXmlHandle, 'xmlGetNoNsProp');
    Pointer(xmlGetProp) := GetProcAddress(libXmlHandle, 'xmlGetProp');
    Pointer(xmlHasProp) := GetProcAddress(libXmlHandle, 'xmlHasProp');
    Pointer(xmlHasNsProp) := GetProcAddress(libXmlHandle, 'xmlHasNsProp');
    Pointer(xmlGetNsProp) := GetProcAddress(libXmlHandle, 'xmlGetNsProp');
    Pointer(xmlStringGetNodeList) := GetProcAddress(libXmlHandle, 'xmlStringGetNodeList');
    Pointer(xmlStringLenGetNodeList) := GetProcAddress(libXmlHandle, 'xmlStringLenGetNodeList');
    Pointer(xmlNodeListGetString) := GetProcAddress(libXmlHandle, 'xmlNodeListGetString');
  {$IFDEF LIBXML_TREE_ENABLED}
    Pointer(xmlNodeListGetRawString) := GetProcAddress(libXmlHandle, 'xmlNodeListGetRawString');
  {$ENDIF} (* LIBXML_TREE_ENABLED *)
    Pointer(xmlNodeSetContent) := GetProcAddress(libXmlHandle, 'xmlNodeSetContent');
  {$IFDEF LIBXML_TREE_ENABLED}
    Pointer(xmlNodeSetContentLen) := GetProcAddress(libXmlHandle, 'xmlNodeSetContentLen');
  {$ENDIF} (* LIBXML_TREE_ENABLED *)
    Pointer(xmlNodeAddContent) := GetProcAddress(libXmlHandle, 'xmlNodeAddContent');
    Pointer(xmlNodeAddContentLen) := GetProcAddress(libXmlHandle, 'xmlNodeAddContentLen');
    Pointer(xmlNodeGetContent) := GetProcAddress(libXmlHandle, 'xmlNodeGetContent');
    Pointer(xmlNodeBufGetContent) := GetProcAddress(libXmlHandle, 'xmlNodeBufGetContent');
    Pointer(xmlNodeGetLang) := GetProcAddress(libXmlHandle, 'xmlNodeGetLang');
    Pointer(xmlNodeGetSpacePreserve) := GetProcAddress(libXmlHandle, 'xmlNodeGetSpacePreserve');
  {$IFDEF LIBXML_TREE_ENABLED}
    Pointer(xmlNodeSetLang) := GetProcAddress(libXmlHandle, 'xmlNodeSetLang');
    Pointer(xmlNodeSetSpacePreserve) := GetProcAddress(libXmlHandle, 'xmlNodeSetSpacePreserve');
  {$ENDIF} (* LIBXML_TREE_ENABLED *)
    Pointer(xmlNodeGetBase) := GetProcAddress(libXmlHandle, 'xmlNodeGetBase');
  {$IF defined(LIBXML_TREE_ENABLED) or defined(LIBXML_XINCLUDE_ENABLED)}
    Pointer(xmlNodeSetBase) := GetProcAddress(libXmlHandle, 'xmlNodeSetBase');
  {$ENDIF}
    Pointer(xmlRemoveProp) := GetProcAddress(libXmlHandle, 'xmlRemoveProp');
  {$IF defined(LIBXML_TREE_ENABLED) or defined(LIBXML_SCHEMAS_ENABLED)}
    Pointer(xmlUnsetNsProp) := GetProcAddress(libXmlHandle, 'xmlUnsetNsProp');
    Pointer(xmlUnsetProp) := GetProcAddress(libXmlHandle, 'xmlUnsetProp');
  {$ENDIF} (* defined(LIBXML_TREE_ENABLED) || defined(LIBXML_SCHEMAS_ENABLED) *)
  {$IFDEF LIBXML_TREE_ENABLED}
    Pointer(xmlReconciliateNs) := GetProcAddress(libXmlHandle, 'xmlReconciliateNs');
  {$ENDIF}
  {$IFDEF LIBXML_OUTPUT_ENABLED}
    Pointer(xmlDocDumpFormatMemory) := GetProcAddress(libXmlHandle, 'xmlDocDumpFormatMemory');
    Pointer(xmlDocDumpMemory) := GetProcAddress(libXmlHandle, 'xmlDocDumpMemory');
    Pointer(xmlDocDumpMemoryEnc) := GetProcAddress(libXmlHandle, 'xmlDocDumpMemoryEnc');
    Pointer(xmlDocDumpFormatMemoryEnc) := GetProcAddress(libXmlHandle, 'xmlDocDumpFormatMemoryEnc');
    Pointer(xmlDocFormatDump) := GetProcAddress(libXmlHandle, 'xmlDocFormatDump');
    Pointer(xmlDocDump) := GetProcAddress(libXmlHandle, 'xmlDocDump');
    Pointer(xmlElemDump) := GetProcAddress(libXmlHandle, 'xmlElemDump');
    Pointer(xmlSaveFormatFile) := GetProcAddress(libXmlHandle, 'xmlSaveFormatFile');
    Pointer(xmlSaveFile) := GetProcAddress(libXmlHandle, 'xmlSaveFile');
    Pointer(xmlBufNodeDump) := GetProcAddress(libXmlHandle, 'xmlBufNodeDump');
    Pointer(xmlNodeDump) := GetProcAddress(libXmlHandle, 'xmlNodeDump');
    Pointer(xmlSaveFileTo) := GetProcAddress(libXmlHandle, 'xmlSaveFileTo');
    Pointer(xmlSaveFormatFileTo) := GetProcAddress(libXmlHandle, 'xmlSaveFormatFileTo');
    Pointer(xmlNodeDumpOutput) := GetProcAddress(libXmlHandle, 'xmlNodeDumpOutput');
    Pointer(xmlSaveFormatFileEnc) := GetProcAddress(libXmlHandle, 'xmlSaveFormatFileEnc');
    Pointer(xmlSaveFileEnc) := GetProcAddress(libXmlHandle, 'xmlSaveFileEnc');
  {$ENDIF} (* LIBXML_OUTPUT_ENABLED *)
    Pointer(xmlIsXHTML) := GetProcAddress(libXmlHandle, 'xmlIsXHTML');
    Pointer(xmlGetDocCompressMode) := GetProcAddress(libXmlHandle, 'xmlGetDocCompressMode');
    Pointer(xmlSetDocCompressMode) := GetProcAddress(libXmlHandle, 'xmlSetDocCompressMode');
    Pointer(xmlGetCompressMode) := GetProcAddress(libXmlHandle, 'xmlGetCompressMode');
    Pointer(xmlSetCompressMode) := GetProcAddress(libXmlHandle, 'xmlSetCompressMode');
    Pointer(xmlDOMWrapNewCtxt) := GetProcAddress(libXmlHandle, 'xmlDOMWrapNewCtxt');
    Pointer(xmlDOMWrapFreeCtxt) := GetProcAddress(libXmlHandle, 'xmlDOMWrapFreeCtxt');
    Pointer(xmlDOMWrapReconcileNamespaces) := GetProcAddress(libXmlHandle, 'xmlDOMWrapReconcileNamespaces');
    Pointer(xmlDOMWrapAdoptNode) := GetProcAddress(libXmlHandle, 'xmlDOMWrapAdoptNode');
    Pointer(xmlDOMWrapRemoveNode) := GetProcAddress(libXmlHandle, 'xmlDOMWrapRemoveNode');
    Pointer(xmlDOMWrapCloneNode) := GetProcAddress(libXmlHandle, 'xmlDOMWrapCloneNode');
  {$IFDEF LIBXML_TREE_ENABLED}
    Pointer(xmlChildElementCount) := GetProcAddress(libXmlHandle, 'xmlChildElementCount');
    Pointer(xmlNextElementSibling) := GetProcAddress(libXmlHandle, 'xmlNextElementSibling');
    Pointer(xmlFirstElementChild) := GetProcAddress(libXmlHandle, 'xmlFirstElementChild');
    Pointer(xmlLastElementChild) := GetProcAddress(libXmlHandle, 'xmlLastElementChild');
    Pointer(xmlPreviousElementSibling) := GetProcAddress(libXmlHandle, 'xmlPreviousElementSibling');
  {$ENDIF} (* LIBXML_TREE_ENABLED *)

    { list.inc }
    Pointer(xmlListCreate) := GetProcAddress(libXmlHandle, 'xmlListCreate');
    Pointer(xmlListDelete) := GetProcAddress(libXmlHandle, 'xmlListDelete');
    Pointer(xmlListSearch) := GetProcAddress(libXmlHandle, 'xmlListSearch');
    Pointer(xmlListReverseSearch) := GetProcAddress(libXmlHandle, 'xmlListReverseSearch');
    Pointer(xmlListInsert) := GetProcAddress(libXmlHandle, 'xmlListInsert');
    Pointer(xmlListAppend) := GetProcAddress(libXmlHandle, 'xmlListAppend');
    Pointer(xmlListRemoveFirst) := GetProcAddress(libXmlHandle, 'xmlListRemoveFirst');
    Pointer(xmlListRemoveLast) := GetProcAddress(libXmlHandle, 'xmlListRemoveLast');
    Pointer(xmlListRemoveAll) := GetProcAddress(libXmlHandle, 'xmlListRemoveAll');
    Pointer(xmlListClear) := GetProcAddress(libXmlHandle, 'xmlListClear');
    Pointer(xmlListEmpty) := GetProcAddress(libXmlHandle, 'xmlListEmpty');
    Pointer(xmlListFront) := GetProcAddress(libXmlHandle, 'xmlListFront');
    Pointer(xmlListEnd) := GetProcAddress(libXmlHandle, 'xmlListEnd');
    Pointer(xmlListSize) := GetProcAddress(libXmlHandle, 'xmlListSize');
    Pointer(xmlListPopFront) := GetProcAddress(libXmlHandle, 'xmlListPopFront');
    Pointer(xmlListPopBack) := GetProcAddress(libXmlHandle, 'xmlListPopBack');
    Pointer(xmlListPushFront) := GetProcAddress(libXmlHandle, 'xmlListPushFront');
    Pointer(xmlListPushBack) := GetProcAddress(libXmlHandle, 'xmlListPushBack');
    Pointer(xmlListReverse) := GetProcAddress(libXmlHandle, 'xmlListReverse');
    Pointer(xmlListSort) := GetProcAddress(libXmlHandle, 'xmlListSort');
    Pointer(xmlListWalk) := GetProcAddress(libXmlHandle, 'xmlListWalk');
    Pointer(xmlListReverseWalk) := GetProcAddress(libXmlHandle, 'xmlListReverseWalk');
    Pointer(xmlListMerge) := GetProcAddress(libXmlHandle, 'xmlListMerge');
    Pointer(xmlListDup) := GetProcAddress(libXmlHandle, 'xmlListDup');
    Pointer(xmlListCopy) := GetProcAddress(libXmlHandle, 'xmlListCopy');
    Pointer(xmlLinkGetData) := GetProcAddress(libXmlHandle, 'xmlLinkGetData');

    { entities.inc }
  {$IFDEF LIBXML_LEGACY_ENABLED}
    Pointer(xmlInitializePredefinedEntities) := GetProcAddress(libHandle, 'xmlInitializePredefinedEntities');
  {$ENDIF} (* LIBXML_LEGACY_ENABLED *)
    Pointer(xmlNewEntity) := GetProcAddress(libXmlHandle, 'xmlNewEntity');
    Pointer(xmlAddDocEntity) := GetProcAddress(libXmlHandle, 'xmlAddDocEntity');
    Pointer(xmlAddDtdEntity) := GetProcAddress(libXmlHandle, 'xmlAddDtdEntity');
    Pointer(xmlGetPredefinedEntity) := GetProcAddress(libXmlHandle, 'xmlGetPredefinedEntity');
    Pointer(xmlGetDocEntity) := GetProcAddress(libXmlHandle, 'xmlGetDocEntity');
    Pointer(xmlGetDtdEntity) := GetProcAddress(libXmlHandle, 'xmlGetDtdEntity');
    Pointer(xmlGetParameterEntity) := GetProcAddress(libXmlHandle, 'xmlGetParameterEntity');
  {$IFDEF LIBXML_LEGACY_ENABLED}
    Pointer(xmlEncodeEntities) := GetProcAddress(libHandle, 'xmlEncodeEntities');
  {$ENDIF} (* LIBXML_LEGACY_ENABLED *)
    Pointer(xmlEncodeEntitiesReentrant) := GetProcAddress(libXmlHandle, 'xmlEncodeEntitiesReentrant');
    Pointer(xmlEncodeSpecialChars) := GetProcAddress(libXmlHandle, 'xmlEncodeSpecialChars');
    Pointer(xmlCreateEntitiesTable) := GetProcAddress(libXmlHandle, 'xmlCreateEntitiesTable');
  {$IFDEF LIBXML_TREE_ENABLED}
    Pointer(xmlCopyEntitiesTable) := GetProcAddress(libXmlHandle, 'xmlCopyEntitiesTable');
  {$ENDIF} (* LIBXML_TREE_ENABLED *)
    Pointer(xmlFreeEntitiesTable) := GetProcAddress(libXmlHandle, 'xmlFreeEntitiesTable');
  {$IFDEF LIBXML_OUTPUT_ENABLED}
    Pointer(xmlDumpEntitiesTable) := GetProcAddress(libXmlHandle, 'xmlDumpEntitiesTable');
    Pointer(xmlDumpEntityDecl) := GetProcAddress(libXmlHandle, 'xmlDumpEntityDecl');
  {$ENDIF} (* LIBXML_OUTPUT_ENABLED *)
  {$IFDEF LIBXML_LEGACY_ENABLED}
    Pointer(xmlCleanupPredefinedEntities) := GetProcAddress(libHandle, 'xmlCleanupPredefinedEntities');
  {$ENDIF} (* LIBXML_LEGACY_ENABLED *)

    { xmlerror.inc }
    Pointer(xmlSetGenericErrorFunc) := GetProcAddress(libXmlHandle, 'xmlSetGenericErrorFunc');
    Pointer(initGenericErrorDefaultFunc) := GetProcAddress(libXmlHandle, 'initGenericErrorDefaultFunc');
    Pointer(xmlSetStructuredErrorFunc) := GetProcAddress(libXmlHandle, 'xmlSetStructuredErrorFunc');
    Pointer(xmlParserError) := GetProcAddress(libXmlHandle, 'xmlParserError');
    Pointer(xmlParserWarning) := GetProcAddress(libXmlHandle, 'xmlParserWarning');
    Pointer(xmlParserValidityError) := GetProcAddress(libXmlHandle, 'xmlParserValidityError');
    Pointer(xmlParserValidityWarning) := GetProcAddress(libXmlHandle, 'xmlParserValidityWarning');
    Pointer(xmlParserPrintFileInfo) := GetProcAddress(libXmlHandle, 'xmlParserPrintFileInfo');
    Pointer(xmlParserPrintFileContext) := GetProcAddress(libXmlHandle, 'xmlParserPrintFileContext');
    Pointer(xmlGetLastError) := GetProcAddress(libXmlHandle, 'xmlGetLastError');
    Pointer(xmlResetLastError) := GetProcAddress(libXmlHandle, 'xmlResetLastError');
    Pointer(xmlCtxtGetLastError) := GetProcAddress(libXmlHandle, 'xmlCtxtGetLastError');
    Pointer(xmlCtxtResetLastError) := GetProcAddress(libXmlHandle, 'xmlCtxtResetLastError');
    Pointer(xmlResetError) := GetProcAddress(libXmlHandle, 'xmlResetError');
    Pointer(xmlCopyError) := GetProcAddress(libXmlHandle, 'xmlCopyError');
    Pointer(__xmlRaiseError) := GetProcAddress(libXmlHandle, '__xmlRaiseError');
    Pointer(__xmlSimpleError) := GetProcAddress(libXmlHandle, '__xmlSimpleError');

    { xmlmemory.inc }
    Pointer(xmlMemSetup) := GetProcAddress(libXmlHandle, 'xmlMemSetup');
    Pointer(xmlMemGet) := GetProcAddress(libXmlHandle, 'xmlMemGet');
    Pointer(xmlGcMemSetup) := GetProcAddress(libXmlHandle, 'xmlGcMemSetup');
    Pointer(xmlGcMemGet) := GetProcAddress(libXmlHandle, 'xmlGcMemGet');
    Pointer(xmlInitMemory) := GetProcAddress(libXmlHandle, 'xmlInitMemory');
    Pointer(xmlCleanupMemory) := GetProcAddress(libXmlHandle, 'xmlCleanupMemory');
    Pointer(xmlMemUsed) := GetProcAddress(libXmlHandle, 'xmlMemUsed');
    Pointer(xmlMemBlocks) := GetProcAddress(libXmlHandle, 'xmlMemBlocks');
    Pointer(xmlMemDisplay) := GetProcAddress(libXmlHandle, 'xmlMemDisplay');
    Pointer(xmlMemDisplayLast) := GetProcAddress(libXmlHandle, 'xmlMemDisplayLast');
    Pointer(xmlMemShow) := GetProcAddress(libXmlHandle, 'xmlMemShow');
    Pointer(xmlMemoryDump) := GetProcAddress(libXmlHandle, 'xmlMemoryDump');
    Pointer(xmlMemMalloc) := GetProcAddress(libXmlHandle, 'xmlMemMalloc');
    Pointer(xmlMemRealloc) := GetProcAddress(libXmlHandle, 'xmlMemRealloc');
    Pointer(xmlMemFree) := GetProcAddress(libXmlHandle, 'xmlMemFree');
    Pointer(xmlMemoryStrdup) := GetProcAddress(libXmlHandle, 'xmlMemoryStrdup');
    Pointer(xmlMallocLoc) := GetProcAddress(libXmlHandle, 'xmlMallocLoc');
    Pointer(xmlReallocLoc) := GetProcAddress(libXmlHandle, 'xmlReallocLoc');
    Pointer(xmlMallocAtomicLoc) := GetProcAddress(libXmlHandle, 'xmlMallocAtomicLoc');
    Pointer(xmlMemStrdupLoc) := GetProcAddress(libXmlHandle, 'xmlMemStrdupLoc');

    { pattern.inc }
  {$IFDEF LIBXML_PATTERN_ENABLED}
    Pointer(xmlFreePattern) := GetProcAddress(libXmlHandle, 'xmlFreePattern');
    Pointer(xmlFreePatternList) := GetProcAddress(libXmlHandle, 'xmlFreePatternList');
    Pointer(xmlPatterncompile) := GetProcAddress(libXmlHandle, 'xmlPatterncompile');
    Pointer(xmlPatternMatch) := GetProcAddress(libXmlHandle, 'xmlPatternMatch');
    Pointer(xmlPatternStreamable) := GetProcAddress(libXmlHandle, 'xmlPatternStreamable');
    Pointer(xmlPatternMaxDepth) := GetProcAddress(libXmlHandle, 'xmlPatternMaxDepth');
    Pointer(xmlPatternMinDepth) := GetProcAddress(libXmlHandle, 'xmlPatternMinDepth');
    Pointer(xmlPatternFromRoot) := GetProcAddress(libXmlHandle, 'xmlPatternFromRoot');
    Pointer(xmlPatternGetStreamCtxt) := GetProcAddress(libXmlHandle, 'xmlPatternGetStreamCtxt');
    Pointer(xmlFreeStreamCtxt) := GetProcAddress(libXmlHandle, 'xmlFreeStreamCtxt');
    Pointer(xmlStreamPushNode) := GetProcAddress(libXmlHandle, 'xmlStreamPushNode');
    Pointer(xmlStreamPush) := GetProcAddress(libXmlHandle, 'xmlStreamPush');
    Pointer(xmlStreamPushAttr) := GetProcAddress(libXmlHandle, 'xmlStreamPushAttr');
    Pointer(xmlStreamPop) := GetProcAddress(libXmlHandle, 'xmlStreamPop');
    Pointer(xmlStreamWantsAnyNode) := GetProcAddress(libXmlHandle, 'xmlStreamWantsAnyNode');
  {$ENDIF} (* LIBXML_PATTERN_ENABLED *)

    { schemasInternals.inc }
  {$IFDEF LIBXML_SCHEMAS_ENABLED}
    Pointer(xmlSchemaFreeType) := GetProcAddress(libXmlHandle, 'xmlSchemaFreeType');
    Pointer(xmlSchemaFreeWildcard) := GetProcAddress(libXmlHandle, 'xmlSchemaFreeWildcard');
  {$ENDIF} (* LIBXML_SCHEMAS_ENABLED *)

    { hash.inc }
    Pointer(xmlHashCreate) := GetProcAddress(libXmlHandle, 'xmlHashCreate');
    Pointer(xmlHashCreateDict) := GetProcAddress(libXmlHandle, 'xmlHashCreateDict');
    Pointer(xmlHashFree) := GetProcAddress(libXmlHandle, 'xmlHashFree');
    Pointer(xmlHashDefaultDeallocator) := GetProcAddress(libXmlHandle, 'xmlHashDefaultDeallocator');
    Pointer(xmlHashAddEntry) := GetProcAddress(libXmlHandle, 'xmlHashAddEntry');
    Pointer(xmlHashUpdateEntry) := GetProcAddress(libXmlHandle, 'xmlHashUpdateEntry');
    Pointer(xmlHashAddEntry2) := GetProcAddress(libXmlHandle, 'xmlHashAddEntry2');
    Pointer(xmlHashUpdateEntry2) := GetProcAddress(libXmlHandle, 'xmlHashUpdateEntry2');
    Pointer(xmlHashAddEntry3) := GetProcAddress(libXmlHandle, 'xmlHashAddEntry3');
    Pointer(xmlHashUpdateEntry3) := GetProcAddress(libXmlHandle, 'xmlHashUpdateEntry3');
    Pointer(xmlHashRemoveEntry) := GetProcAddress(libXmlHandle, 'xmlHashRemoveEntry');
    Pointer(xmlHashRemoveEntry2) := GetProcAddress(libXmlHandle, 'xmlHashRemoveEntry2');
    Pointer(xmlHashRemoveEntry3) := GetProcAddress(libXmlHandle, 'xmlHashRemoveEntry3');
    Pointer(xmlHashLookup) := GetProcAddress(libXmlHandle, 'xmlHashLookup');
    Pointer(xmlHashLookup2) := GetProcAddress(libXmlHandle, 'xmlHashLookup2');
    Pointer(xmlHashLookup3) := GetProcAddress(libXmlHandle, 'xmlHashLookup3');
    Pointer(xmlHashQLookup) := GetProcAddress(libXmlHandle, 'xmlHashQLookup');
    Pointer(xmlHashQLookup2) := GetProcAddress(libXmlHandle, 'xmlHashQLookup2');
    Pointer(xmlHashQLookup3) := GetProcAddress(libXmlHandle, 'xmlHashQLookup3');
    Pointer(xmlHashCopy) := GetProcAddress(libXmlHandle, 'xmlHashCopy');
    Pointer(xmlHashSize) := GetProcAddress(libXmlHandle, 'xmlHashSize');
    Pointer(xmlHashScan) := GetProcAddress(libXmlHandle, 'xmlHashScan');
    Pointer(xmlHashScan3) := GetProcAddress(libXmlHandle, 'xmlHashScan3');
    Pointer(xmlHashScanFull) := GetProcAddress(libXmlHandle, 'xmlHashScanFull');
    Pointer(xmlHashScanFull3) := GetProcAddress(libXmlHandle, 'xmlHashScanFull3');

    { valid.inc }
    Pointer(xmlAddNotationDecl) := GetProcAddress(libXmlHandle, 'xmlAddNotationDecl');
  {$IFDEF LIBXML_TREE_ENABLED}
    Pointer(xmlCopyNotationTable) := GetProcAddress(libXmlHandle, 'xmlCopyNotationTable');
  {$ENDIF} (* LIBXML_TREE_ENABLED *)
    Pointer(xmlFreeNotationTable) := GetProcAddress(libXmlHandle, 'xmlFreeNotationTable');
  {$IFDEF LIBXML_OUTPUT_ENABLED}
    Pointer(xmlDumpNotationDecl) := GetProcAddress(libXmlHandle, 'xmlDumpNotationDecl');
    Pointer(xmlDumpNotationTable) := GetProcAddress(libXmlHandle, 'xmlDumpNotationTable');
  {$ENDIF} (* LIBXML_OUTPUT_ENABLED *)
    Pointer(xmlNewDocElementContent) := GetProcAddress(libXmlHandle, 'xmlNewDocElementContent');
    Pointer(xmlCopyDocElementContent) := GetProcAddress(libXmlHandle, 'xmlCopyDocElementContent');
    Pointer(xmlFreeDocElementContent) := GetProcAddress(libXmlHandle, 'xmlFreeDocElementContent');
    Pointer(xmlSnprintfElementContent) := GetProcAddress(libXmlHandle, 'xmlSnprintfElementContent');
    Pointer(xmlAddElementDecl) := GetProcAddress(libXmlHandle, 'xmlAddElementDecl');
  {$IFDEF LIBXML_TREE_ENABLED}
    Pointer(xmlCopyElementTable) := GetProcAddress(libXmlHandle, 'xmlCopyElementTable');
  {$ENDIF} (* LIBXML_TREE_ENABLED *)
    Pointer(xmlFreeElementTable) := GetProcAddress(libXmlHandle, 'xmlFreeElementTable');
  {$IFDEF LIBXML_OUTPUT_ENABLED}
    Pointer(xmlDumpElementTable) := GetProcAddress(libXmlHandle, 'xmlDumpElementTable');
    Pointer(xmlDumpElementDecl) := GetProcAddress(libXmlHandle, 'xmlDumpElementDecl');
  {$ENDIF} (* LIBXML_OUTPUT_ENABLED *)
    Pointer(xmlCreateEnumeration) := GetProcAddress(libXmlHandle, 'xmlCreateEnumeration');
    Pointer(xmlFreeEnumeration) := GetProcAddress(libXmlHandle, 'xmlFreeEnumeration');
  {$IFDEF LIBXML_TREE_ENABLED}
    Pointer(xmlCopyEnumeration) := GetProcAddress(libXmlHandle, 'xmlCopyEnumeration');
  {$ENDIF} (* LIBXML_TREE_ENABLED *)
    Pointer(xmlAddAttributeDecl) := GetProcAddress(libXmlHandle, 'xmlAddAttributeDecl');
  {$IFDEF LIBXML_TREE_ENABLED}
    Pointer(xmlCopyAttributeTable) := GetProcAddress(libXmlHandle, 'xmlCopyAttributeTable');
  {$ENDIF} (* LIBXML_TREE_ENABLED *)
    Pointer(xmlFreeAttributeTable) := GetProcAddress(libXmlHandle, 'xmlFreeAttributeTable');
  {$IFDEF LIBXML_OUTPUT_ENABLED}
    Pointer(xmlDumpAttributeTable) := GetProcAddress(libXmlHandle, 'xmlDumpAttributeTable');
    Pointer(xmlDumpAttributeDecl) := GetProcAddress(libXmlHandle, 'xmlDumpAttributeDecl');
  {$ENDIF} (* LIBXML_OUTPUT_ENABLED *)
    Pointer(xmlAddID) := GetProcAddress(libXmlHandle, 'xmlAddID');
    Pointer(xmlFreeIDTable) := GetProcAddress(libXmlHandle, 'xmlFreeIDTable');
    Pointer(xmlGetID) := GetProcAddress(libXmlHandle, 'xmlGetID');
    Pointer(xmlIsID) := GetProcAddress(libXmlHandle, 'xmlIsID');
    Pointer(xmlRemoveID) := GetProcAddress(libXmlHandle, 'xmlRemoveID');
    Pointer(xmlAddRef) := GetProcAddress(libXmlHandle, 'xmlAddRef');
    Pointer(xmlFreeRefTable) := GetProcAddress(libXmlHandle, 'xmlFreeRefTable');
    Pointer(xmlIsRef) := GetProcAddress(libXmlHandle, 'xmlIsRef');
    Pointer(xmlRemoveRef) := GetProcAddress(libXmlHandle, 'xmlRemoveRef');
    Pointer(xmlGetRefs) := GetProcAddress(libXmlHandle, 'xmlGetRefs');
  {$IFDEF LIBXML_VALID_ENABLED}
    Pointer(xmlNewValidCtxt) := GetProcAddress(libXmlHandle, 'xmlNewValidCtxt');
    Pointer(xmlFreeValidCtxt) := GetProcAddress(libXmlHandle, 'xmlFreeValidCtxt');
    Pointer(xmlValidateRoot) := GetProcAddress(libXmlHandle, 'xmlValidateRoot');
    Pointer(xmlValidateElementDecl) := GetProcAddress(libXmlHandle, 'xmlValidateElementDecl');
    Pointer(xmlValidNormalizeAttributeValue) := GetProcAddress(libXmlHandle, 'xmlValidNormalizeAttributeValue');
    Pointer(xmlValidCtxtNormalizeAttributeValue) := GetProcAddress(libXmlHandle, 'xmlValidCtxtNormalizeAttributeValue');
    Pointer(xmlValidateAttributeDecl) := GetProcAddress(libXmlHandle, 'xmlValidateAttributeDecl');
    Pointer(xmlValidateAttributeValue) := GetProcAddress(libXmlHandle, 'xmlValidateAttributeValue');
    Pointer(xmlValidateNotationDecl) := GetProcAddress(libXmlHandle, 'xmlValidateNotationDecl');
    Pointer(xmlValidateDtd) := GetProcAddress(libXmlHandle, 'xmlValidateDtd');
    Pointer(xmlValidateDtdFinal) := GetProcAddress(libXmlHandle, 'xmlValidateDtdFinal');
    Pointer(xmlValidateDocument) := GetProcAddress(libXmlHandle, 'xmlValidateDocument');
    Pointer(xmlValidateElement) := GetProcAddress(libXmlHandle, 'xmlValidateElement');
    Pointer(xmlValidateOneElement) := GetProcAddress(libXmlHandle, 'xmlValidateOneElement');
    Pointer(xmlValidateOneAttribute) := GetProcAddress(libXmlHandle, 'xmlValidateOneAttribute');
    Pointer(xmlValidateOneNamespace) := GetProcAddress(libXmlHandle, 'xmlValidateOneNamespace');
    Pointer(xmlValidateDocumentFinal) := GetProcAddress(libXmlHandle, 'xmlValidateDocumentFinal');
  {$ENDIF} (* LIBXML_VALID_ENABLED *)
  {$IF defined(LIBXML_VALID_ENABLED) or defined(LIBXML_SCHEMAS_ENABLED)}
    Pointer(xmlValidateNotationUse) := GetProcAddress(libXmlHandle, 'xmlValidateNotationUse');
  {$ENDIF} (* LIBXML_VALID_ENABLED or LIBXML_SCHEMAS_ENABLED *)
    Pointer(xmlIsMixedElement) := GetProcAddress(libXmlHandle, 'xmlIsMixedElement');
    Pointer(xmlGetDtdAttrDesc) := GetProcAddress(libXmlHandle, 'xmlGetDtdAttrDesc');
    Pointer(xmlGetDtdQAttrDesc) := GetProcAddress(libXmlHandle, 'xmlGetDtdQAttrDesc');
    Pointer(xmlGetDtdNotationDesc) := GetProcAddress(libXmlHandle, 'xmlGetDtdNotationDesc');
    Pointer(xmlGetDtdQElementDesc) := GetProcAddress(libXmlHandle, 'xmlGetDtdQElementDesc');
    Pointer(xmlGetDtdElementDesc) := GetProcAddress(libXmlHandle, 'xmlGetDtdElementDesc');
  {$IFDEF LIBXML_VALID_ENABLED}
    Pointer(xmlValidGetPotentialChildren) := GetProcAddress(libXmlHandle, 'xmlValidGetPotentialChildren');
    Pointer(xmlValidGetValidElements) := GetProcAddress(libXmlHandle, 'xmlValidGetValidElements');
    Pointer(xmlValidateNameValue) := GetProcAddress(libXmlHandle, 'xmlValidateNameValue');
    Pointer(xmlValidateNamesValue) := GetProcAddress(libXmlHandle, 'xmlValidateNamesValue');
    Pointer(xmlValidateNmtokenValue) := GetProcAddress(libXmlHandle, 'xmlValidateNmtokenValue');
    Pointer(xmlValidateNmtokensValue) := GetProcAddress(libXmlHandle, 'xmlValidateNmtokensValue');
  {$IFDEF LIBXML_REGEXP_ENABLED}
    Pointer(xmlValidBuildContentModel) := GetProcAddress(libXmlHandle, 'xmlValidBuildContentModel');
    Pointer(xmlValidatePushElement) := GetProcAddress(libXmlHandle, 'xmlValidatePushElement');
    Pointer(xmlValidatePushCData) := GetProcAddress(libXmlHandle, 'xmlValidatePushCData');
    Pointer(xmlValidatePopElement) := GetProcAddress(libXmlHandle, 'xmlValidatePopElement');
  {$ENDIF} (* LIBXML_REGEXP_ENABLED *)
  {$ENDIF} (* LIBXML_VALID_ENABLED *)

    { libxmlparser.inc }
    Pointer(xmlInitParser) := GetProcAddress(libXmlHandle, 'xmlInitParser');
    Pointer(xmlCleanupParser) := GetProcAddress(libXmlHandle, 'xmlCleanupParser');
    Pointer(xmlParserInputRead) := GetProcAddress(libXmlHandle, 'xmlParserInputRead');
    Pointer(xmlParserInputGrow) := GetProcAddress(libXmlHandle, 'xmlParserInputGrow');
  {$IFDEF LIBXML_SAX1_ENABLED}
    Pointer(xmlParseDoc) := GetProcAddress(libXmlHandle, 'xmlParseDoc');
    Pointer(xmlParseFile) := GetProcAddress(libXmlHandle, 'xmlParseFile');
    Pointer(xmlParseMemory) := GetProcAddress(libXmlHandle, 'xmlParseMemory');
  {$ENDIF} (* LIBXML_SAX1_ENABLED *)
    Pointer(xmlSubstituteEntitiesDefault) := GetProcAddress(libXmlHandle, 'xmlSubstituteEntitiesDefault');
    Pointer(xmlKeepBlanksDefault) := GetProcAddress(libXmlHandle, 'xmlKeepBlanksDefault');
    Pointer(xmlStopParser) := GetProcAddress(libXmlHandle, 'xmlStopParser');
    Pointer(xmlPedanticParserDefault) := GetProcAddress(libXmlHandle, 'xmlPedanticParserDefault');
    Pointer(xmlLineNumbersDefault) := GetProcAddress(libXmlHandle, 'xmlLineNumbersDefault');
  {$IFDEF LIBXML_SAX1_ENABLED}
    Pointer(xmlRecoverDoc) := GetProcAddress(libXmlHandle, 'xmlRecoverDoc');
    Pointer(xmlRecoverMemory) := GetProcAddress(libXmlHandle, 'xmlRecoverMemory');
    Pointer(xmlRecoverFile) := GetProcAddress(libXmlHandle, 'xmlRecoverFile');
  {$ENDIF} (* LIBXML_SAX1_ENABLED *)
    Pointer(xmlParseDocument) := GetProcAddress(libXmlHandle, 'xmlParseDocument');
    Pointer(xmlParseExtParsedEnt) := GetProcAddress(libXmlHandle, 'xmlParseExtParsedEnt');
  {$IFDEF LIBXML_SAX1_ENABLED}
    Pointer(xmlSAXUserParseFile) := GetProcAddress(libXmlHandle, 'xmlSAXUserParseFile');
    Pointer(xmlSAXUserParseMemory) := GetProcAddress(libXmlHandle, 'xmlSAXUserParseMemory');
    Pointer(xmlSAXParseDoc) := GetProcAddress(libXmlHandle, 'xmlSAXParseDoc');
    Pointer(xmlSAXParseMemory) := GetProcAddress(libXmlHandle, 'xmlSAXParseMemory');
    Pointer(xmlSAXParseMemoryWithData) := GetProcAddress(libXmlHandle, 'xmlSAXParseMemoryWithData');
    Pointer(xmlSAXParseFile) := GetProcAddress(libXmlHandle, 'xmlSAXParseFile');
    Pointer(xmlSAXParseFileWithData) := GetProcAddress(libXmlHandle, 'xmlSAXParseFileWithData');
    Pointer(xmlSAXParseEntity) := GetProcAddress(libXmlHandle, 'xmlSAXParseEntity');
    Pointer(xmlParseEntity) := GetProcAddress(libXmlHandle, 'xmlParseEntity');
  {$ENDIF} (* LIBXML_SAX1_ENABLED *)
  {$IFDEF LIBXML_VALID_ENABLED}
    Pointer(xmlSAXParseDTD) := GetProcAddress(libXmlHandle, 'xmlSAXParseDTD');
    Pointer(xmlParseDTD) := GetProcAddress(libXmlHandle, 'xmlParseDTD');
    Pointer(xmlIOParseDTD) := GetProcAddress(libXmlHandle, 'xmlIOParseDTD');
  {$ENDIF} (* LIBXML_VALID_ENABLE *)
  {$IFDEF LIBXML_SAX1_ENABLED}
    Pointer(xmlParseBalancedChunkMemory) := GetProcAddress(libXmlHandle, 'xmlParseBalancedChunkMemory');
  {$ENDIF} (* LIBXML_SAX1_ENABLED *)
    Pointer(xmlParseInNodeContext) := GetProcAddress(libXmlHandle, 'xmlParseInNodeContext');
  {$IFDEF LIBXML_SAX1_ENABLED}
    Pointer(xmlParseBalancedChunkMemoryRecover) := GetProcAddress(libXmlHandle, 'xmlParseBalancedChunkMemoryRecover');
    Pointer(xmlParseExternalEntity) := GetProcAddress(libXmlHandle, 'xmlParseExternalEntity');
  {$ENDIF} (* LIBXML_SAX1_ENABLED *)
    Pointer(xmlParseCtxtExternalEntity) := GetProcAddress(libXmlHandle, 'xmlParseCtxtExternalEntity');
    Pointer(xmlNewParserCtxt) := GetProcAddress(libXmlHandle, 'xmlNewParserCtxt');
    Pointer(xmlInitParserCtxt) := GetProcAddress(libXmlHandle, 'xmlInitParserCtxt');
    Pointer(xmlClearParserCtxt) := GetProcAddress(libXmlHandle, 'xmlClearParserCtxt');
    Pointer(xmlFreeParserCtxt) := GetProcAddress(libXmlHandle, 'xmlFreeParserCtxt');
  {$IFDEF LIBXML_SAX1_ENABLED}
    Pointer(xmlSetupParserForBuffer) := GetProcAddress(libXmlHandle, 'xmlSetupParserForBuffer');
  {$ENDIF} (* LIBXML_SAX1_ENABLED *)
    Pointer(xmlCreateDocParserCtxt) := GetProcAddress(libXmlHandle, 'xmlCreateDocParserCtxt');
  {$IFDEF LIBXML_LEGACY_ENABLED}
    Pointer(xmlGetFeaturesList) := GetProcAddress(libHandle, 'xmlGetFeaturesList');
    Pointer(xmlGetFeature) := GetProcAddress(libHandle, 'xmlGetFeature');
    Pointer(xmlSetFeature) := GetProcAddress(libHandle, 'xmlSetFeature');
  {$ENDIF} (* LIBXML_LEGACY_ENABLED *)
  {$IFDEF LIBXML_PUSH_ENABLED}
    Pointer(xmlCreatePushParserCtxt) := GetProcAddress(libXmlHandle, 'xmlCreatePushParserCtxt');
    Pointer(xmlParseChunk) := GetProcAddress(libXmlHandle, 'xmlParseChunk');
  {$ENDIF} (* LIBXML_PUSH_ENABLED *)
    Pointer(xmlCreateIOParserCtxt) := GetProcAddress(libXmlHandle, 'xmlCreateIOParserCtxt');
    Pointer(xmlNewIOInputStream) := GetProcAddress(libXmlHandle, 'xmlNewIOInputStream');
    Pointer(xmlParserFindNodeInfo) := GetProcAddress(libXmlHandle, 'xmlParserFindNodeInfo');
    Pointer(xmlInitNodeInfoSeq) := GetProcAddress(libXmlHandle, 'xmlInitNodeInfoSeq');
    Pointer(xmlClearNodeInfoSeq) := GetProcAddress(libXmlHandle, 'xmlClearNodeInfoSeq');
    Pointer(xmlParserFindNodeInfoIndex) := GetProcAddress(libXmlHandle, 'xmlParserFindNodeInfoIndex');
    Pointer(xmlParserAddNodeInfo) := GetProcAddress(libXmlHandle, 'xmlParserAddNodeInfo');
    Pointer(xmlSetExternalEntityLoader) := GetProcAddress(libXmlHandle, 'xmlSetExternalEntityLoader');
    Pointer(xmlGetExternalEntityLoader) := GetProcAddress(libXmlHandle, 'xmlGetExternalEntityLoader');
    Pointer(xmlLoadExternalEntity) := GetProcAddress(libXmlHandle, 'xmlLoadExternalEntity');
    Pointer(xmlByteConsumed) := GetProcAddress(libXmlHandle, 'xmlByteConsumed');
    Pointer(xmlCtxtReset) := GetProcAddress(libXmlHandle, 'xmlCtxtReset');
    Pointer(xmlCtxtResetPush) := GetProcAddress(libXmlHandle, 'xmlCtxtResetPush');
    Pointer(xmlCtxtUseOptions) := GetProcAddress(libXmlHandle, 'xmlCtxtUseOptions');
    Pointer(xmlReadDoc) := GetProcAddress(libXmlHandle, 'xmlReadDoc');
    Pointer(xmlReadFile) := GetProcAddress(libXmlHandle, 'xmlReadFile');
    Pointer(xmlReadMemory) := GetProcAddress(libXmlHandle, 'xmlReadMemory');
    Pointer(xmlReadFd) := GetProcAddress(libXmlHandle, 'xmlReadFd');
    Pointer(xmlReadIO) := GetProcAddress(libXmlHandle, 'xmlReadIO');
    Pointer(xmlCtxtReadDoc) := GetProcAddress(libXmlHandle, 'xmlCtxtReadDoc');
    Pointer(xmlCtxtReadFile) := GetProcAddress(libXmlHandle, 'xmlCtxtReadFile');
    Pointer(xmlCtxtReadMemory) := GetProcAddress(libXmlHandle, 'xmlCtxtReadMemory');
    Pointer(xmlCtxtReadFd) := GetProcAddress(libXmlHandle, 'xmlCtxtReadFd');
    Pointer(xmlCtxtReadIO) := GetProcAddress(libXmlHandle, 'xmlCtxtReadIO');
    Pointer(xmlHasFeature) := GetProcAddress(libXmlHandle, 'xmlHasFeature');

    { schematron.inc }
  {$IFDEF LIBXML_SCHEMATRON_ENABLED}
    Pointer(xmlSchematronNewParserCtxt) := GetProcAddress(libXmlHandle, 'xmlSchematronNewParserCtxt');
    Pointer(xmlSchematronNewMemParserCtxt) := GetProcAddress(libXmlHandle, 'xmlSchematronNewMemParserCtxt');
    Pointer(xmlSchematronNewDocParserCtxt) := GetProcAddress(libXmlHandle, 'xmlSchematronNewDocParserCtxt');
    Pointer(xmlSchematronFreeParserCtxt) := GetProcAddress(libXmlHandle, 'xmlSchematronFreeParserCtxt');
    Pointer(xmlSchematronParse) := GetProcAddress(libXmlHandle, 'xmlSchematronParse');
    Pointer(xmlSchematronFree) := GetProcAddress(libXmlHandle, 'xmlSchematronFree');
    Pointer(xmlSchematronSetValidStructuredErrors) := GetProcAddress(libXmlHandle, 'xmlSchematronSetValidStructuredErrors');
    Pointer(xmlSchematronNewValidCtxt) := GetProcAddress(libXmlHandle, 'xmlSchematronNewValidCtxt');
    Pointer(xmlSchematronFreeValidCtxt) := GetProcAddress(libXmlHandle, 'xmlSchematronFreeValidCtxt');
    Pointer(xmlSchematronValidateDoc) := GetProcAddress(libXmlHandle, 'xmlSchematronValidateDoc');
  {$ENDIF} (* LIBXML_SCHEMATRON_ENABLED *)

    { threads.inc }
    Pointer(xmlNewMutex) := GetProcAddress(libXmlHandle, 'xmlNewMutex');
    Pointer(xmlMutexLock) := GetProcAddress(libXmlHandle, 'xmlMutexLock');
    Pointer(xmlMutexUnlock) := GetProcAddress(libXmlHandle, 'xmlMutexUnlock');
    Pointer(xmlFreeMutex) := GetProcAddress(libXmlHandle, 'xmlFreeMutex');
    Pointer(xmlNewRMutex) := GetProcAddress(libXmlHandle, 'xmlNewRMutex');
    Pointer(xmlRMutexLock) := GetProcAddress(libXmlHandle, 'xmlRMutexLock');
    Pointer(xmlRMutexUnlock) := GetProcAddress(libXmlHandle, 'xmlRMutexUnlock');
    Pointer(xmlFreeRMutex) := GetProcAddress(libXmlHandle, 'xmlFreeRMutex');
    Pointer(xmlInitThreads) := GetProcAddress(libXmlHandle, 'xmlInitThreads');
    Pointer(xmlLockLibrary) := GetProcAddress(libXmlHandle, 'xmlLockLibrary');
    Pointer(xmlUnlockLibrary) := GetProcAddress(libXmlHandle, 'xmlUnlockLibrary');
    Pointer(xmlGetThreadId) := GetProcAddress(libXmlHandle, 'xmlGetThreadId');
    Pointer(xmlIsMainThread) := GetProcAddress(libXmlHandle, 'xmlIsMainThread');
    Pointer(xmlCleanupThreads) := GetProcAddress(libXmlHandle, 'xmlCleanupThreads');
    Pointer(xmlGetGlobalState) := GetProcAddress(libXmlHandle, 'xmlGetGlobalState');

    { uri.inc }
    Pointer(xmlCreateURI) := GetProcAddress(libXmlHandle, 'xmlCreateURI');
    Pointer(xmlBuildURI) := GetProcAddress(libXmlHandle, 'xmlBuildURI');
    Pointer(xmlBuildRelativeURI) := GetProcAddress(libXmlHandle, 'xmlBuildRelativeURI');
    Pointer(xmlParseURI) := GetProcAddress(libXmlHandle, 'xmlParseURI');
    Pointer(xmlParseURIRaw) := GetProcAddress(libXmlHandle, 'xmlParseURIRaw');
    Pointer(xmlParseURIReference) := GetProcAddress(libXmlHandle, 'xmlParseURIReference');
    Pointer(xmlSaveUri) := GetProcAddress(libXmlHandle, 'xmlSaveUri');
    Pointer(xmlPrintURI) := GetProcAddress(libXmlHandle, 'xmlPrintURI');
    Pointer(xmlURIEscapeStr) := GetProcAddress(libXmlHandle, 'xmlURIEscapeStr');
    Pointer(xmlURIUnescapeString) := GetProcAddress(libXmlHandle, 'xmlURIUnescapeString');
    Pointer(xmlNormalizeURIPath) := GetProcAddress(libXmlHandle, 'xmlNormalizeURIPath');
    Pointer(xmlURIEscape) := GetProcAddress(libXmlHandle, 'xmlURIEscape');
    Pointer(xmlFreeURI) := GetProcAddress(libXmlHandle, 'xmlFreeURI');
    Pointer(xmlCanonicPath) := GetProcAddress(libXmlHandle, 'xmlCanonicPath');
    Pointer(xmlPathToURI) := GetProcAddress(libXmlHandle, 'xmlPathToURI');

    { relaxng.inc }
  {$IFDEF LIBXML_SCHEMAS_ENABLED}
    Pointer(xmlRelaxNGInitTypes) := GetProcAddress(libXmlHandle, 'xmlRelaxNGInitTypes');
    Pointer(xmlRelaxNGCleanupTypes) := GetProcAddress(libXmlHandle, 'xmlRelaxNGCleanupTypes');
    Pointer(xmlRelaxNGNewParserCtxt) := GetProcAddress(libXmlHandle, 'xmlRelaxNGNewParserCtxt');
    Pointer(xmlRelaxNGNewMemParserCtxt) := GetProcAddress(libXmlHandle, 'xmlRelaxNGNewMemParserCtxt');
    Pointer(xmlRelaxNGNewDocParserCtxt) := GetProcAddress(libXmlHandle, 'xmlRelaxNGNewDocParserCtxt');
    Pointer(xmlRelaxParserSetFlag) := GetProcAddress(libXmlHandle, 'xmlRelaxParserSetFlag');
    Pointer(xmlRelaxNGFreeParserCtxt) := GetProcAddress(libXmlHandle, 'xmlRelaxNGFreeParserCtxt');
    Pointer(xmlRelaxNGSetParserErrors) := GetProcAddress(libXmlHandle, 'xmlRelaxNGSetParserErrors');
    Pointer(xmlRelaxNGGetParserErrors) := GetProcAddress(libXmlHandle, 'xmlRelaxNGGetParserErrors');
    Pointer(xmlRelaxNGSetParserStructuredErrors) := GetProcAddress(libXmlHandle, 'xmlRelaxNGSetParserStructuredErrors');
    Pointer(xmlRelaxNGParse) := GetProcAddress(libXmlHandle, 'xmlRelaxNGParse');
    Pointer(xmlRelaxNGFree) := GetProcAddress(libXmlHandle, 'xmlRelaxNGFree');
  {$IFDEF LIBXML_OUTPUT_ENABLED}
    Pointer(xmlRelaxNGDump) := GetProcAddress(libXmlHandle, 'xmlRelaxNGDump');
    Pointer(xmlRelaxNGDumpTree) := GetProcAddress(libXmlHandle, 'xmlRelaxNGDumpTree');
  {$ENDIF} (* LIBXML_OUTPUT_ENABLED *)
    Pointer(xmlRelaxNGSetValidErrors) := GetProcAddress(libXmlHandle, 'xmlRelaxNGSetValidErrors');
    Pointer(xmlRelaxNGGetValidErrors) := GetProcAddress(libXmlHandle, 'xmlRelaxNGGetValidErrors');
    Pointer(xmlRelaxNGSetValidStructuredErrors) := GetProcAddress(libXmlHandle, 'xmlRelaxNGSetValidStructuredErrors');
    Pointer(xmlRelaxNGNewValidCtxt) := GetProcAddress(libXmlHandle, 'xmlRelaxNGNewValidCtxt');
    Pointer(xmlRelaxNGFreeValidCtxt) := GetProcAddress(libXmlHandle, 'xmlRelaxNGFreeValidCtxt');
    Pointer(xmlRelaxNGValidateDoc) := GetProcAddress(libXmlHandle, 'xmlRelaxNGValidateDoc');
    Pointer(xmlRelaxNGValidatePushElement) := GetProcAddress(libXmlHandle, 'xmlRelaxNGValidatePushElement');
    Pointer(xmlRelaxNGValidatePushCData) := GetProcAddress(libXmlHandle, 'xmlRelaxNGValidatePushCData');
    Pointer(xmlRelaxNGValidatePopElement) := GetProcAddress(libXmlHandle, 'xmlRelaxNGValidatePopElement');
    Pointer(xmlRelaxNGValidateFullElement) := GetProcAddress(libXmlHandle, 'xmlRelaxNGValidateFullElement');
  {$ENDIF} (* LIBXML_SCHEMAS_ENABLED *)

    { globals.inc }
    Pointer(xmlInitGlobals) := GetProcAddress(libXmlHandle, 'xmlInitGlobals');
    Pointer(xmlCleanupGlobals) := GetProcAddress(libXmlHandle, 'xmlCleanupGlobals');
    Pointer(xmlParserInputBufferCreateFilenameDefault) := GetProcAddress(libXmlHandle, 'xmlParserInputBufferCreateFilenameDefault');
    Pointer(xmlOutputBufferCreateFilenameDefault) := GetProcAddress(libXmlHandle, 'xmlOutputBufferCreateFilenameDefault');
    Pointer(xmlInitializeGlobalState) := GetProcAddress(libXmlHandle, 'xmlInitializeGlobalState');
    Pointer(xmlThrDefSetGenericErrorFunc) := GetProcAddress(libXmlHandle, 'xmlThrDefSetGenericErrorFunc');
    Pointer(xmlThrDefSetStructuredErrorFunc) := GetProcAddress(libXmlHandle, 'xmlThrDefSetStructuredErrorFunc');
    Pointer(xmlRegisterNodeDefault) := GetProcAddress(libXmlHandle, 'xmlRegisterNodeDefault');
    Pointer(xmlThrDefRegisterNodeDefault) := GetProcAddress(libXmlHandle, 'xmlThrDefRegisterNodeDefault');
    Pointer(xmlDeregisterNodeDefault) := GetProcAddress(libXmlHandle, 'xmlDeregisterNodeDefault');
    Pointer(xmlThrDefDeregisterNodeDefault) := GetProcAddress(libXmlHandle, 'xmlThrDefDeregisterNodeDefault');
    Pointer(xmlThrDefOutputBufferCreateFilenameDefault) := GetProcAddress(libXmlHandle, 'xmlThrDefOutputBufferCreateFilenameDefault');
    Pointer(xmlThrDefParserInputBufferCreateFilenameDefault) := GetProcAddress(libXmlHandle, 'xmlThrDefParserInputBufferCreateFilenameDefault');
    Pointer(__docbDefaultSAXHandler) := GetProcAddress(libXmlHandle, '__docbDefaultSAXHandler');
    Pointer(__htmlDefaultSAXHandler) := GetProcAddress(libXmlHandle, '__htmlDefaultSAXHandler');
    Pointer(__xmlLastError) := GetProcAddress(libXmlHandle, '__xmlLastError');
    Pointer(__oldXMLWDcompatibility) := GetProcAddress(libXmlHandle, '__oldXMLWDcompatibility');
    Pointer(__xmlBufferAllocScheme) := GetProcAddress(libXmlHandle, '__xmlBufferAllocScheme');
    Pointer(xmlThrDefBufferAllocScheme) := GetProcAddress(libXmlHandle, 'xmlThrDefBufferAllocScheme');
    Pointer(__xmlDefaultBufferSize) := GetProcAddress(libXmlHandle, '__xmlDefaultBufferSize');
    Pointer(xmlThrDefDefaultBufferSize) := GetProcAddress(libXmlHandle, 'xmlThrDefDefaultBufferSize');
    Pointer(__xmlDefaultSAXHandler) := GetProcAddress(libXmlHandle, '__xmlDefaultSAXHandler');
    Pointer(__xmlDefaultSAXLocator) := GetProcAddress(libXmlHandle, '__xmlDefaultSAXLocator');
    Pointer(__xmlDoValidityCheckingDefaultValue) := GetProcAddress(libXmlHandle, '__xmlDoValidityCheckingDefaultValue');
    Pointer(xmlThrDefDoValidityCheckingDefaultValue) := GetProcAddress(libXmlHandle, 'xmlThrDefDoValidityCheckingDefaultValue');
    Pointer(__xmlGenericError) := GetProcAddress(libXmlHandle, '__xmlGenericError');
    Pointer(__xmlStructuredError) := GetProcAddress(libXmlHandle, '__xmlStructuredError');
    Pointer(__xmlGenericErrorContext) := GetProcAddress(libXmlHandle, '__xmlGenericErrorContext');
    Pointer(__xmlGetWarningsDefaultValue) := GetProcAddress(libXmlHandle, '__xmlGetWarningsDefaultValue');
    Pointer(xmlThrDefGetWarningsDefaultValue) := GetProcAddress(libXmlHandle, 'xmlThrDefGetWarningsDefaultValue');
    Pointer(__xmlIndentTreeOutput) := GetProcAddress(libXmlHandle, '__xmlIndentTreeOutput');
    Pointer(xmlThrDefIndentTreeOutput) := GetProcAddress(libXmlHandle, 'xmlThrDefIndentTreeOutput');
    Pointer(__xmlTreeIndentString) := GetProcAddress(libXmlHandle, '__xmlTreeIndentString');
    Pointer(xmlThrDefTreeIndentString) := GetProcAddress(libXmlHandle, 'xmlThrDefTreeIndentString');
    Pointer(__xmlKeepBlanksDefaultValue) := GetProcAddress(libXmlHandle, '__xmlKeepBlanksDefaultValue');
    Pointer(xmlThrDefKeepBlanksDefaultValue) := GetProcAddress(libXmlHandle, 'xmlThrDefKeepBlanksDefaultValue');
    Pointer(__xmlLineNumbersDefaultValue) := GetProcAddress(libXmlHandle, '__xmlLineNumbersDefaultValue');
    Pointer(xmlThrDefLineNumbersDefaultValue) := GetProcAddress(libXmlHandle, 'xmlThrDefLineNumbersDefaultValue');
    Pointer(__xmlLoadExtDtdDefaultValue) := GetProcAddress(libXmlHandle, '__xmlLoadExtDtdDefaultValue');
    Pointer(xmlThrDefLoadExtDtdDefaultValue) := GetProcAddress(libXmlHandle, 'xmlThrDefLoadExtDtdDefaultValue');
    Pointer(__xmlParserDebugEntities) := GetProcAddress(libXmlHandle, '__xmlParserDebugEntities');
    Pointer(xmlThrDefParserDebugEntities) := GetProcAddress(libXmlHandle, 'xmlThrDefParserDebugEntities');
    Pointer(__xmlParserVersion) := GetProcAddress(libXmlHandle, '__xmlParserVersion');
    Pointer(__xmlPedanticParserDefaultValue) := GetProcAddress(libXmlHandle, '__xmlPedanticParserDefaultValue');
    Pointer(xmlThrDefPedanticParserDefaultValue) := GetProcAddress(libXmlHandle, 'xmlThrDefPedanticParserDefaultValue');
    Pointer(__xmlSaveNoEmptyTags) := GetProcAddress(libXmlHandle, '__xmlSaveNoEmptyTags');
    Pointer(xmlThrDefSaveNoEmptyTags) := GetProcAddress(libXmlHandle, 'xmlThrDefSaveNoEmptyTags');
    Pointer(__xmlSubstituteEntitiesDefaultValue) := GetProcAddress(libXmlHandle, '__xmlSubstituteEntitiesDefaultValue');
    Pointer(xmlThrDefSubstituteEntitiesDefaultValue) := GetProcAddress(libXmlHandle, 'xmlThrDefSubstituteEntitiesDefaultValue');
    Pointer(__xmlRegisterNodeDefaultValue) := GetProcAddress(libXmlHandle, '__xmlRegisterNodeDefaultValue');
    Pointer(__xmlDeregisterNodeDefaultValue) := GetProcAddress(libXmlHandle, '__xmlDeregisterNodeDefaultValue');
    Pointer(__xmlParserInputBufferCreateFilenameValue) := GetProcAddress(libXmlHandle, '__xmlParserInputBufferCreateFilenameValue');
    Pointer(__xmlOutputBufferCreateFilenameValue) := GetProcAddress(libXmlHandle, '__xmlOutputBufferCreateFilenameValue');

    { nanoftp.inc }
  {$IFDEF LIBXML_FTP_ENABLED}
    Pointer(xmlNanoFTPInit) := GetProcAddress(libXmlHandle, 'xmlNanoFTPInit');
    Pointer(xmlNanoFTPCleanup) := GetProcAddress(libXmlHandle, 'xmlNanoFTPCleanup');
    Pointer(xmlNanoFTPNewCtxt) := GetProcAddress(libXmlHandle, 'xmlNanoFTPNewCtxt');
    Pointer(xmlNanoFTPFreeCtxt) := GetProcAddress(libXmlHandle, 'xmlNanoFTPFreeCtxt');
    Pointer(xmlNanoFTPConnectTo) := GetProcAddress(libXmlHandle, 'xmlNanoFTPConnectTo');
    Pointer(xmlNanoFTPOpen) := GetProcAddress(libXmlHandle, 'xmlNanoFTPOpen');
    Pointer(xmlNanoFTPConnect) := GetProcAddress(libXmlHandle, 'xmlNanoFTPConnect');
    Pointer(xmlNanoFTPClose) := GetProcAddress(libXmlHandle, 'xmlNanoFTPClose');
    Pointer(xmlNanoFTPQuit) := GetProcAddress(libXmlHandle, 'xmlNanoFTPQuit');
    Pointer(xmlNanoFTPScanProxy) := GetProcAddress(libXmlHandle, 'xmlNanoFTPScanProxy');
    Pointer(xmlNanoFTPProxy) := GetProcAddress(libXmlHandle, 'xmlNanoFTPProxy');
    Pointer(xmlNanoFTPUpdateURL) := GetProcAddress(libXmlHandle, 'xmlNanoFTPUpdateURL');
    Pointer(xmlNanoFTPGetResponse) := GetProcAddress(libXmlHandle, 'xmlNanoFTPGetResponse');
    Pointer(xmlNanoFTPCheckResponse) := GetProcAddress(libXmlHandle, 'xmlNanoFTPCheckResponse');
    Pointer(xmlNanoFTPCwd) := GetProcAddress(libXmlHandle, 'xmlNanoFTPCwd');
    Pointer(xmlNanoFTPDele) := GetProcAddress(libXmlHandle, 'xmlNanoFTPDele');
    Pointer(xmlNanoFTPGetConnection) := GetProcAddress(libXmlHandle, 'xmlNanoFTPGetConnection');
    Pointer(xmlNanoFTPCloseConnection) := GetProcAddress(libXmlHandle, 'xmlNanoFTPCloseConnection');
    Pointer(xmlNanoFTPList) := GetProcAddress(libXmlHandle, 'xmlNanoFTPList');
    Pointer(xmlNanoFTPGetSocket) := GetProcAddress(libXmlHandle, 'xmlNanoFTPGetSocket');
    Pointer(xmlNanoFTPGet) := GetProcAddress(libXmlHandle, 'xmlNanoFTPGet');
    Pointer(xmlNanoFTPRead) := GetProcAddress(libXmlHandle, 'xmlNanoFTPRead');
  {$ENDIF} (* LIBXML_FTP_ENABLED *)

    { nanohttp.inc }
  {$IFDEF LIBXML_HTTP_ENABLED}
    Pointer(xmlNanoHTTPInit) := GetProcAddress(libXmlHandle, 'xmlNanoHTTPInit');
    Pointer(xmlNanoHTTPCleanup) := GetProcAddress(libXmlHandle, 'xmlNanoHTTPCleanup');
    Pointer(xmlNanoHTTPScanProxy) := GetProcAddress(libXmlHandle, 'xmlNanoHTTPScanProxy');
    Pointer(xmlNanoHTTPFetch) := GetProcAddress(libXmlHandle, 'xmlNanoHTTPFetch');
    Pointer(xmlNanoHTTPMethod) := GetProcAddress(libXmlHandle, 'xmlNanoHTTPMethod');
    Pointer(xmlNanoHTTPMethodRedir) := GetProcAddress(libXmlHandle, 'xmlNanoHTTPMethodRedir');
    Pointer(xmlNanoHTTPOpen) := GetProcAddress(libXmlHandle, 'xmlNanoHTTPOpen');
    Pointer(xmlNanoHTTPOpenRedir) := GetProcAddress(libXmlHandle, 'xmlNanoHTTPOpenRedir');
    Pointer(xmlNanoHTTPReturnCode) := GetProcAddress(libXmlHandle, 'xmlNanoHTTPReturnCode');
    Pointer(xmlNanoHTTPAuthHeader) := GetProcAddress(libXmlHandle, 'xmlNanoHTTPAuthHeader');
    Pointer(xmlNanoHTTPRedir) := GetProcAddress(libXmlHandle, 'xmlNanoHTTPRedir');
    Pointer(xmlNanoHTTPContentLength) := GetProcAddress(libXmlHandle, 'xmlNanoHTTPContentLength');
    Pointer(xmlNanoHTTPEncoding) := GetProcAddress(libXmlHandle, 'xmlNanoHTTPEncoding');
    Pointer(xmlNanoHTTPMimeType) := GetProcAddress(libXmlHandle, 'xmlNanoHTTPMimeType');
    Pointer(xmlNanoHTTPRead) := GetProcAddress(libXmlHandle, 'xmlNanoHTTPRead');
  {$IFDEF LIBXML_OUTPUT_ENABLED}
    Pointer(xmlNanoHTTPSave) := GetProcAddress(libXmlHandle, 'xmlNanoHTTPSave');
  {$ENDIF} (* LIBXML_OUTPUT_ENABLED *)
    Pointer(xmlNanoHTTPClose) := GetProcAddress(libXmlHandle, 'xmlNanoHTTPClose');
  {$ENDIF} (* LIBXML_HTTP_ENABLED *)

    { SAX.inc }
  {$IFDEF LIBXML_LEGACY_ENABLED}
    Pointer(getPublicId) := GetProcAddress(libHandle, 'getPublicId');
    Pointer(getSystemId) := GetProcAddress(libHandle, 'getSystemId');
    Pointer(setDocumentLocator) := GetProcAddress(libHandle, 'setDocumentLocator');
    Pointer(getLineNumber) := GetProcAddress(libHandle, 'getLineNumber');
    Pointer(getColumnNumber) := GetProcAddress(libHandle, 'getColumnNumber');
    Pointer(isStandalone) := GetProcAddress(libHandle, 'isStandalone');
    Pointer(hasInternalSubset) := GetProcAddress(libHandle, 'hasInternalSubset');
    Pointer(hasExternalSubset) := GetProcAddress(libHandle, 'hasExternalSubset');
    Pointer(internalSubset) := GetProcAddress(libHandle, 'internalSubset');
    Pointer(externalSubset) := GetProcAddress(libHandle, 'externalSubset');
    Pointer(getEntity) := GetProcAddress(libHandle, 'getEntity');
    Pointer(getParameterEntity) := GetProcAddress(libHandle, 'getParameterEntity');
    Pointer(resolveEntity) := GetProcAddress(libHandle, 'resolveEntity');
    Pointer(entityDecl) := GetProcAddress(libHandle, 'entityDecl');
    Pointer(attributeDecl) := GetProcAddress(libHandle, 'attributeDecl');
    Pointer(elementDecl) := GetProcAddress(libHandle, 'elementDecl');
    Pointer(notationDecl) := GetProcAddress(libHandle, 'notationDecl');
    Pointer(unparsedEntityDecl) := GetProcAddress(libHandle, 'unparsedEntityDecl');
    Pointer(startDocument) := GetProcAddress(libHandle, 'startDocument');
    Pointer(endDocument) := GetProcAddress(libHandle, 'endDocument');
    Pointer(attribute) := GetProcAddress(libHandle, 'attribute');
    Pointer(startElement) := GetProcAddress(libHandle, 'startElement');
    Pointer(endElement) := GetProcAddress(libHandle, 'endElement');
    Pointer(reference) := GetProcAddress(libHandle, 'reference');
    Pointer(characters) := GetProcAddress(libHandle, 'characters');
    Pointer(ignorableWhitespace) := GetProcAddress(libHandle, 'ignorableWhitespace');
    Pointer(processingInstruction) := GetProcAddress(libHandle, 'processingInstruction');
    Pointer(globalNamespace) := GetProcAddress(libHandle, 'globalNamespace');
    Pointer(setNamespace) := GetProcAddress(libHandle, 'setNamespace');
    Pointer(getNamespace) := GetProcAddress(libHandle, 'getNamespace');
    Pointer(checkNamespace) := GetProcAddress(libHandle, 'checkNamespace');
    Pointer(namespaceDecl) := GetProcAddress(libHandle, 'namespaceDecl');
    Pointer(comment) := GetProcAddress(libHandle, 'comment');
    Pointer(cdataBlock) := GetProcAddress(libHandle, 'cdataBlock');
  {$IFDEF LIBXML_SAX1_ENABLED}
    Pointer(initxmlDefaultSAXHandler) := GetProcAddress(libHandle, 'initxmlDefaultSAXHandler');
  {$IFDEF LIBXML_HTML_ENABLED}
    Pointer(inithtmlDefaultSAXHandler) := GetProcAddress(libHandle, 'inithtmlDefaultSAXHandler');
  {$ENDIF}
  {$IFDEF LIBXML_DOCB_ENABLED}
    Pointer(initdocbDefaultSAXHandler) := GetProcAddress(libHandle, 'initdocbDefaultSAXHandler');
  {$ENDIF}
  {$ENDIF} (* LIBXML_SAX1_ENABLED *)
  {$ENDIF} (* LIBXML_LEGACY_ENABLED *)

    { SAX2.inc }
    Pointer(xmlSAX2GetPublicId) := GetProcAddress(libXmlHandle, 'xmlSAX2GetPublicId');
    Pointer(xmlSAX2GetSystemId) := GetProcAddress(libXmlHandle, 'xmlSAX2GetSystemId');
    Pointer(xmlSAX2SetDocumentLocator) := GetProcAddress(libXmlHandle, 'xmlSAX2SetDocumentLocator');
    Pointer(xmlSAX2GetLineNumber) := GetProcAddress(libXmlHandle, 'xmlSAX2GetLineNumber');
    Pointer(xmlSAX2GetColumnNumber) := GetProcAddress(libXmlHandle, 'xmlSAX2GetColumnNumber');
    Pointer(xmlSAX2IsStandalone) := GetProcAddress(libXmlHandle, 'xmlSAX2IsStandalone');
    Pointer(xmlSAX2HasInternalSubset) := GetProcAddress(libXmlHandle, 'xmlSAX2HasInternalSubset');
    Pointer(xmlSAX2HasExternalSubset) := GetProcAddress(libXmlHandle, 'xmlSAX2HasExternalSubset');
    Pointer(xmlSAX2InternalSubset) := GetProcAddress(libXmlHandle, 'xmlSAX2InternalSubset');
    Pointer(xmlSAX2ExternalSubset) := GetProcAddress(libXmlHandle, 'xmlSAX2ExternalSubset');
    Pointer(xmlSAX2GetEntity) := GetProcAddress(libXmlHandle, 'xmlSAX2GetEntity');
    Pointer(xmlSAX2GetParameterEntity) := GetProcAddress(libXmlHandle, 'xmlSAX2GetParameterEntity');
    Pointer(xmlSAX2ResolveEntity) := GetProcAddress(libXmlHandle, 'xmlSAX2ResolveEntity');
    Pointer(xmlSAX2EntityDecl) := GetProcAddress(libXmlHandle, 'xmlSAX2EntityDecl');
    Pointer(xmlSAX2AttributeDecl) := GetProcAddress(libXmlHandle, 'xmlSAX2AttributeDecl');
    Pointer(xmlSAX2ElementDecl) := GetProcAddress(libXmlHandle, 'xmlSAX2ElementDecl');
    Pointer(xmlSAX2NotationDecl) := GetProcAddress(libXmlHandle, 'xmlSAX2NotationDecl');
    Pointer(xmlSAX2UnparsedEntityDecl) := GetProcAddress(libXmlHandle, 'xmlSAX2UnparsedEntityDecl');
    Pointer(xmlSAX2StartDocument) := GetProcAddress(libXmlHandle, 'xmlSAX2StartDocument');
    Pointer(xmlSAX2EndDocument) := GetProcAddress(libXmlHandle, 'xmlSAX2EndDocument');
  {$IF defined(LIBXML_SAX1_ENABLED) or defined(LIBXML_HTML_ENABLED) or defined(LIBXML_WRITER_ENABLED) or defined(LIBXML_DOCB_ENABLED)}
    Pointer(xmlSAX2StartElement) := GetProcAddress(libXmlHandle, 'xmlSAX2StartElement');
    Pointer(xmlSAX2EndElement) := GetProcAddress(libXmlHandle, 'xmlSAX2EndElement');
  {$ENDIF} (* LIBXML_SAX1_ENABLED or LIBXML_HTML_ENABLED *)
    Pointer(xmlSAX2StartElementNs) := GetProcAddress(libXmlHandle, 'xmlSAX2StartElementNs');
    Pointer(xmlSAX2EndElementNs) := GetProcAddress(libXmlHandle, 'xmlSAX2EndElementNs');
    Pointer(xmlSAX2Reference) := GetProcAddress(libXmlHandle, 'xmlSAX2Reference');
    Pointer(xmlSAX2Characters) := GetProcAddress(libXmlHandle, 'xmlSAX2Characters');
    Pointer(xmlSAX2IgnorableWhitespace) := GetProcAddress(libXmlHandle, 'xmlSAX2IgnorableWhitespace');
    Pointer(xmlSAX2ProcessingInstruction) := GetProcAddress(libXmlHandle, 'xmlSAX2ProcessingInstruction');
    Pointer(xmlSAX2Comment) := GetProcAddress(libXmlHandle, 'xmlSAX2Comment');
    Pointer(xmlSAX2CDataBlock) := GetProcAddress(libXmlHandle, 'xmlSAX2CDataBlock');
  {$IFDEF LIBXML_SAX1_ENABLED}
    Pointer(xmlSAXDefaultVersion) := GetProcAddress(libXmlHandle, 'xmlSAXDefaultVersion');
  {$ENDIF} (* LIBXML_SAX1_ENABLED *)
    Pointer(xmlSAXVersion) := GetProcAddress(libXmlHandle, 'xmlSAXVersion');
    Pointer(xmlSAX2InitDefaultSAXHandler) := GetProcAddress(libXmlHandle, 'xmlSAX2InitDefaultSAXHandler');
  {$IFDEF LIBXML_HTML_ENABLED}
    Pointer(xmlSAX2InitHtmlDefaultSAXHandler) := GetProcAddress(libXmlHandle, 'xmlSAX2InitHtmlDefaultSAXHandler');
    Pointer(htmlDefaultSAXHandlerInit) := GetProcAddress(libXmlHandle, 'htmlDefaultSAXHandlerInit');
  {$ENDIF}
  {$IFDEF LIBXML_DOCB_ENABLED}
    Pointer(xmlSAX2InitDocbDefaultSAXHandler) := GetProcAddress(libXmlHandle, 'xmlSAX2InitDocbDefaultSAXHandler');
    Pointer(docbDefaultSAXHandlerInit) := GetProcAddress(libXmlHandle, 'docbDefaultSAXHandlerInit');
  {$ENDIF}
    Pointer(xmlDefaultSAXHandlerInit) := GetProcAddress(libXmlHandle, 'xmlDefaultSAXHandlerInit');

    { HTMLtree.inc }
  {$IFDEF LIBXML_HTML_ENABLED}
    Pointer(htmlNewDoc) := GetProcAddress(libXmlHandle, 'htmlNewDoc');
    Pointer(htmlNewDocNoDtD) := GetProcAddress(libXmlHandle, 'htmlNewDocNoDtD');
    Pointer(htmlGetMetaEncoding) := GetProcAddress(libXmlHandle, 'htmlGetMetaEncoding');
    Pointer(htmlSetMetaEncoding) := GetProcAddress(libXmlHandle, 'htmlSetMetaEncoding');
  {$IFDEF LIBXML_OUTPUT_ENABLED}
    Pointer(htmlDocDumpMemory) := GetProcAddress(libXmlHandle, 'htmlDocDumpMemory');
    Pointer(htmlDocDumpMemoryFormat) := GetProcAddress(libXmlHandle, 'htmlDocDumpMemoryFormat');
    Pointer(htmlDocDump) := GetProcAddress(libXmlHandle, 'htmlDocDump');
    Pointer(htmlSaveFile) := GetProcAddress(libXmlHandle, 'htmlSaveFile');
    Pointer(htmlNodeDump) := GetProcAddress(libXmlHandle, 'htmlNodeDump');
    Pointer(htmlNodeDumpFile) := GetProcAddress(libXmlHandle, 'htmlNodeDumpFile');
    Pointer(htmlNodeDumpFileFormat) := GetProcAddress(libXmlHandle, 'htmlNodeDumpFileFormat');
    Pointer(htmlSaveFileEnc) := GetProcAddress(libXmlHandle, 'htmlSaveFileEnc');
    Pointer(htmlSaveFileFormat) := GetProcAddress(libXmlHandle, 'htmlSaveFileFormat');
    Pointer(htmlNodeDumpFormatOutput) := GetProcAddress(libXmlHandle, 'htmlNodeDumpFormatOutput');
    Pointer(htmlDocContentDumpOutput) := GetProcAddress(libXmlHandle, 'htmlDocContentDumpOutput');
    Pointer(htmlDocContentDumpFormatOutput) := GetProcAddress(libXmlHandle, 'htmlDocContentDumpFormatOutput');
    Pointer(htmlNodeDumpOutput) := GetProcAddress(libXmlHandle, 'htmlNodeDumpOutput');
  {$ENDIF} (* LIBXML_OUTPUT_ENABLED *)
    Pointer(htmlIsBooleanAttr) := GetProcAddress(libXmlHandle, 'htmlIsBooleanAttr');
  {$ENDIF} (* LIBXML_HTML_ENABLED *)

    { HTMLparser.inc }
  {$IFDEF LIBXML_HTML_ENABLED}
    Pointer(htmlTagLookup) := GetProcAddress(libXmlHandle, 'htmlTagLookup');
    Pointer(htmlEntityLookup) := GetProcAddress(libXmlHandle, 'htmlEntityLookup');
    Pointer(htmlEntityValueLookup) := GetProcAddress(libXmlHandle, 'htmlEntityValueLookup');
    Pointer(htmlIsAutoClosed) := GetProcAddress(libXmlHandle, 'htmlIsAutoClosed');
    Pointer(htmlAutoCloseTag) := GetProcAddress(libXmlHandle, 'htmlAutoCloseTag');
    Pointer(htmlParseEntityRef) := GetProcAddress(libXmlHandle, 'htmlParseEntityRef');
    Pointer(htmlParseCharRef) := GetProcAddress(libXmlHandle, 'htmlParseCharRef');
    Pointer(htmlParseElement) := GetProcAddress(libXmlHandle, 'htmlParseElement');
    Pointer(htmlNewParserCtxt) := GetProcAddress(libXmlHandle, 'htmlNewParserCtxt');
    Pointer(htmlCreateMemoryParserCtxt) := GetProcAddress(libXmlHandle, 'htmlCreateMemoryParserCtxt');
    Pointer(htmlParseDocument) := GetProcAddress(libXmlHandle, 'htmlParseDocument');
    Pointer(htmlSAXParseDoc) := GetProcAddress(libXmlHandle, 'htmlSAXParseDoc');
    Pointer(htmlParseDoc) := GetProcAddress(libXmlHandle, 'htmlParseDoc');
    Pointer(htmlSAXParseFile) := GetProcAddress(libXmlHandle, 'htmlSAXParseFile');
    Pointer(htmlParseFile) := GetProcAddress(libXmlHandle, 'htmlParseFile');
    Pointer(UTF8ToHtml) := GetProcAddress(libXmlHandle, 'UTF8ToHtml');
    Pointer(htmlEncodeEntities) := GetProcAddress(libXmlHandle, 'htmlEncodeEntities');
    Pointer(htmlIsScriptAttribute) := GetProcAddress(libXmlHandle, 'htmlIsScriptAttribute');
    Pointer(htmlHandleOmittedElem) := GetProcAddress(libXmlHandle, 'htmlHandleOmittedElem');
  {$IFDEF LIBXML_PUSH_ENABLED}
    Pointer(htmlCreatePushParserCtxt) := GetProcAddress(libXmlHandle, 'htmlCreatePushParserCtxt');
    Pointer(htmlParseChunk) := GetProcAddress(libXmlHandle, 'htmlParseChunk');
  {$ENDIF} (* LIBXML_PUSH_ENABLED *)
    Pointer(htmlFreeParserCtxt) := GetProcAddress(libXmlHandle, 'htmlFreeParserCtxt');
  {$ENDIF} (* LIBXML_HTML_ENABLED *)

    { xmlautomata.inc }
  {$IFDEF LIBXML_REGEXP_ENABLED}
  {$IFDEF LIBXML_AUTOMATA_ENABLED}
    Pointer(xmlNewAutomata) := GetProcAddress(libXmlHandle, 'xmlNewAutomata');
    Pointer(xmlFreeAutomata) := GetProcAddress(libXmlHandle, 'xmlFreeAutomata');
    Pointer(xmlAutomataGetInitState) := GetProcAddress(libXmlHandle, 'xmlAutomataGetInitState');
    Pointer(xmlAutomataSetFinalState) := GetProcAddress(libXmlHandle, 'xmlAutomataSetFinalState');
    Pointer(xmlAutomataNewState) := GetProcAddress(libXmlHandle, 'xmlAutomataNewState');
    Pointer(xmlAutomataNewTransition) := GetProcAddress(libXmlHandle, 'xmlAutomataNewTransition');
    Pointer(xmlAutomataNewTransition2) := GetProcAddress(libXmlHandle, 'xmlAutomataNewTransition2');
    Pointer(xmlAutomataNewNegTrans) := GetProcAddress(libXmlHandle, 'xmlAutomataNewNegTrans');
    Pointer(xmlAutomataNewCountTrans) := GetProcAddress(libXmlHandle, 'xmlAutomataNewCountTrans');
    Pointer(xmlAutomataNewCountTrans2) := GetProcAddress(libXmlHandle, 'xmlAutomataNewCountTrans2');
    Pointer(xmlAutomataNewOnceTrans) := GetProcAddress(libXmlHandle, 'xmlAutomataNewOnceTrans');
    Pointer(xmlAutomataNewOnceTrans2) := GetProcAddress(libXmlHandle, 'xmlAutomataNewOnceTrans2');
    Pointer(xmlAutomataNewAllTrans) := GetProcAddress(libXmlHandle, 'xmlAutomataNewAllTrans');
    Pointer(xmlAutomataNewEpsilon) := GetProcAddress(libXmlHandle, 'xmlAutomataNewEpsilon');
    Pointer(xmlAutomataNewCountedTrans) := GetProcAddress(libXmlHandle, 'xmlAutomataNewCountedTrans');
    Pointer(xmlAutomataNewCounterTrans) := GetProcAddress(libXmlHandle, 'xmlAutomataNewCounterTrans');
    Pointer(xmlAutomataNewCounter) := GetProcAddress(libXmlHandle, 'xmlAutomataNewCounter');
    Pointer(xmlAutomataCompile) := GetProcAddress(libXmlHandle, 'xmlAutomataCompile');
    Pointer(xmlAutomataIsDeterminist) := GetProcAddress(libXmlHandle, 'xmlAutomataIsDeterminist');
  {$ENDIF} (* LIBXML_AUTOMATA_ENABLED *)
  {$ENDIF} (* LIBXML_REGEXP_ENABLED *)

    { xmlIO.inc }
    Pointer(xmlCleanupInputCallbacks) := GetProcAddress(libXmlHandle, 'xmlCleanupInputCallbacks');
    Pointer(xmlPopInputCallbacks) := GetProcAddress(libXmlHandle, 'xmlPopInputCallbacks');
    Pointer(xmlRegisterDefaultInputCallbacks) := GetProcAddress(libXmlHandle, 'xmlRegisterDefaultInputCallbacks');
    Pointer(xmlAllocParserInputBuffer) := GetProcAddress(libXmlHandle, 'xmlAllocParserInputBuffer');
    Pointer(xmlParserInputBufferCreateFilename) := GetProcAddress(libXmlHandle, 'xmlParserInputBufferCreateFilename');
    Pointer(xmlParserInputBufferCreateFile) := GetProcAddress(libXmlHandle, 'xmlParserInputBufferCreateFile');
    Pointer(xmlParserInputBufferCreateFd) := GetProcAddress(libXmlHandle, 'xmlParserInputBufferCreateFd');
    Pointer(xmlParserInputBufferCreateMem) := GetProcAddress(libXmlHandle, 'xmlParserInputBufferCreateMem');
    Pointer(xmlParserInputBufferCreateStatic) := GetProcAddress(libXmlHandle, 'xmlParserInputBufferCreateStatic');
    Pointer(xmlParserInputBufferCreateIO) := GetProcAddress(libXmlHandle, 'xmlParserInputBufferCreateIO');
    Pointer(xmlParserInputBufferRead) := GetProcAddress(libXmlHandle, 'xmlParserInputBufferRead');
    Pointer(xmlParserInputBufferGrow) := GetProcAddress(libXmlHandle, 'xmlParserInputBufferGrow');
    Pointer(xmlParserInputBufferPush) := GetProcAddress(libXmlHandle, 'xmlParserInputBufferPush');
    Pointer(xmlFreeParserInputBuffer) := GetProcAddress(libXmlHandle, 'xmlFreeParserInputBuffer');
    Pointer(xmlParserGetDirectory) := GetProcAddress(libXmlHandle, 'xmlParserGetDirectory');
    Pointer(xmlRegisterInputCallbacks) := GetProcAddress(libXmlHandle, 'xmlRegisterInputCallbacks');
    Pointer(__xmlParserInputBufferCreateFilename) := GetProcAddress(libXmlHandle, '__xmlParserInputBufferCreateFilename');
  {$IFDEF LIBXML_OUTPUT_ENABLED}
    Pointer(xmlCleanupOutputCallbacks) := GetProcAddress(libXmlHandle, 'xmlCleanupOutputCallbacks');
    Pointer(xmlRegisterDefaultOutputCallbacks) := GetProcAddress(libXmlHandle, 'xmlRegisterDefaultOutputCallbacks');
    Pointer(xmlAllocOutputBuffer) := GetProcAddress(libXmlHandle, 'xmlAllocOutputBuffer');
    Pointer(xmlOutputBufferCreateFilename) := GetProcAddress(libXmlHandle, 'xmlOutputBufferCreateFilename');
    Pointer(xmlOutputBufferCreateFile) := GetProcAddress(libXmlHandle, 'xmlOutputBufferCreateFile');
    Pointer(xmlOutputBufferCreateBuffer) := GetProcAddress(libXmlHandle, 'xmlOutputBufferCreateBuffer');
    Pointer(xmlOutputBufferCreateFd) := GetProcAddress(libXmlHandle, 'xmlOutputBufferCreateFd');
    Pointer(xmlOutputBufferCreateIO) := GetProcAddress(libXmlHandle, 'xmlOutputBufferCreateIO');
    Pointer(xmlOutputBufferGetContent) := GetProcAddress(libXmlHandle, 'xmlOutputBufferGetContent');
    Pointer(xmlOutputBufferGetSize) := GetProcAddress(libXmlHandle, 'xmlOutputBufferGetSize');
    Pointer(xmlOutputBufferWrite) := GetProcAddress(libXmlHandle, 'xmlOutputBufferWrite');
    Pointer(xmlOutputBufferWriteString) := GetProcAddress(libXmlHandle, 'xmlOutputBufferWriteString');
    Pointer(xmlOutputBufferWriteEscape) := GetProcAddress(libXmlHandle, 'xmlOutputBufferWriteEscape');
    Pointer(xmlOutputBufferFlush) := GetProcAddress(libXmlHandle, 'xmlOutputBufferFlush');
    Pointer(xmlOutputBufferClose) := GetProcAddress(libXmlHandle, 'xmlOutputBufferClose');
    Pointer(xmlRegisterOutputCallbacks) := GetProcAddress(libXmlHandle, 'xmlRegisterOutputCallbacks');
    Pointer(__xmlOutputBufferCreateFilename) := GetProcAddress(libXmlHandle, '__xmlOutputBufferCreateFilename');
  {$IFDEF LIBXML_HTTP_ENABLED}
    Pointer(xmlRegisterHTTPPostCallbacks) := GetProcAddress(libXmlHandle, 'xmlRegisterHTTPPostCallbacks');
  {$ENDIF} (* LIBXML_HTTP_ENABLED *)
  {$ENDIF} (* LIBXML_OUTPUT_ENABLED *)
    Pointer(xmlCheckHTTPInput) := GetProcAddress(libXmlHandle, 'xmlCheckHTTPInput');
    Pointer(xmlNoNetExternalEntityLoader) := GetProcAddress(libXmlHandle, 'xmlNoNetExternalEntityLoader');
    Pointer(xmlCheckFilename) := GetProcAddress(libXmlHandle, 'xmlCheckFilename');
    Pointer(xmlFileMatch) := GetProcAddress(libXmlHandle, 'xmlFileMatch');
    Pointer(xmlFileOpen) := GetProcAddress(libXmlHandle, 'xmlFileOpen');
    Pointer(xmlFileRead) := GetProcAddress(libXmlHandle, 'xmlFileRead');
    Pointer(xmlFileClose) := GetProcAddress(libXmlHandle, 'xmlFileClose');
  {$IFDEF LIBXML_HTTP_ENABLED}
    Pointer(xmlIOHTTPMatch) := GetProcAddress(libXmlHandle, 'xmlIOHTTPMatch');
    Pointer(xmlIOHTTPOpen) := GetProcAddress(libXmlHandle, 'xmlIOHTTPOpen');
  {$IFDEF LIBXML_OUTPUT_ENABLED}
    Pointer(xmlIOHTTPOpenW) := GetProcAddress(libXmlHandle, 'xmlIOHTTPOpenW');
  {$ENDIF} (* LIBXML_OUTPUT_ENABLED *)
    Pointer(xmlIOHTTPRead) := GetProcAddress(libXmlHandle, 'xmlIOHTTPRead');
    Pointer(xmlIOHTTPClose) := GetProcAddress(libXmlHandle, 'xmlIOHTTPClose');
  {$ENDIF} (* LIBXML_HTTP_ENABLED *)
  {$IFDEF LIBXML_FTP_ENABLED}
    Pointer(xmlIOFTPMatch) := GetProcAddress(libXmlHandle, 'xmlIOFTPMatch');
    Pointer(xmlIOFTPOpen) := GetProcAddress(libXmlHandle, 'xmlIOFTPOpen');
    Pointer(xmlIOFTPRead) := GetProcAddress(libXmlHandle, 'xmlIOFTPRead');
    Pointer(xmlIOFTPClose) := GetProcAddress(libXmlHandle, 'xmlIOFTPClose');
  {$ENDIF} (* LIBXML_FTP_ENABLED *)

    { xmlmodule.inc }
  {$IFDEF LIBXML_MODULES_ENABLED}
    Pointer(xmlModuleOpen) := GetProcAddress(libXmlHandle, 'xmlModuleOpen');
    Pointer(xmlModuleSymbol) := GetProcAddress(libXmlHandle, 'xmlModuleSymbol');
    Pointer(xmlModuleClose) := GetProcAddress(libXmlHandle, 'xmlModuleClose');
    Pointer(xmlModuleFree) := GetProcAddress(libXmlHandle, 'xmlModuleFree');
  {$ENDIF} (* LIBXML_MODULES_ENABLED *)

    { xmlreader.inc }
  {$IFDEF LIBXML_SCHEMAS_ENABLED}
  {$IFDEF LIBXML_READER_ENABLED}
    Pointer(xmlNewTextReader) := GetProcAddress(libXmlHandle, 'xmlNewTextReader');
    Pointer(xmlNewTextReaderFilename) := GetProcAddress(libXmlHandle, 'xmlNewTextReaderFilename');
    Pointer(xmlFreeTextReader) := GetProcAddress(libXmlHandle, 'xmlFreeTextReader');
    Pointer(xmlTextReaderSetup) := GetProcAddress(libXmlHandle, 'xmlTextReaderSetup');
    Pointer(xmlTextReaderRead) := GetProcAddress(libXmlHandle, 'xmlTextReaderRead');
  {$IFDEF LIBXML_WRITER_ENABLED}
    Pointer(xmlTextReaderReadInnerXml) := GetProcAddress(libXmlHandle, 'xmlTextReaderReadInnerXml');
    Pointer(xmlTextReaderReadOuterXml) := GetProcAddress(libXmlHandle, 'xmlTextReaderReadOuterXml');
  {$ENDIF}
    Pointer(xmlTextReaderReadString) := GetProcAddress(libXmlHandle, 'xmlTextReaderReadString');
    Pointer(xmlTextReaderReadAttributeValue) := GetProcAddress(libXmlHandle, 'xmlTextReaderReadAttributeValue');
    Pointer(xmlTextReaderAttributeCount) := GetProcAddress(libXmlHandle, 'xmlTextReaderAttributeCount');
    Pointer(xmlTextReaderDepth) := GetProcAddress(libXmlHandle, 'xmlTextReaderDepth');
    Pointer(xmlTextReaderHasAttributes) := GetProcAddress(libXmlHandle, 'xmlTextReaderHasAttributes');
    Pointer(xmlTextReaderHasValue) := GetProcAddress(libXmlHandle, 'xmlTextReaderHasValue');
    Pointer(xmlTextReaderIsDefault) := GetProcAddress(libXmlHandle, 'xmlTextReaderIsDefault');
    Pointer(xmlTextReaderIsEmptyElement) := GetProcAddress(libXmlHandle, 'xmlTextReaderIsEmptyElement');
    Pointer(xmlTextReaderNodeType) := GetProcAddress(libXmlHandle, 'xmlTextReaderNodeType');
    Pointer(xmlTextReaderQuoteChar) := GetProcAddress(libXmlHandle, 'xmlTextReaderQuoteChar');
    Pointer(xmlTextReaderReadState) := GetProcAddress(libXmlHandle, 'xmlTextReaderReadState');
    Pointer(xmlTextReaderIsNamespaceDecl) := GetProcAddress(libXmlHandle, 'xmlTextReaderIsNamespaceDecl');
    Pointer(xmlTextReaderConstBaseUri) := GetProcAddress(libXmlHandle, 'xmlTextReaderConstBaseUri');
    Pointer(xmlTextReaderConstLocalName) := GetProcAddress(libXmlHandle, 'xmlTextReaderConstLocalName');
    Pointer(xmlTextReaderConstName) := GetProcAddress(libXmlHandle, 'xmlTextReaderConstName');
    Pointer(xmlTextReaderConstNamespaceUri) := GetProcAddress(libXmlHandle, 'xmlTextReaderConstNamespaceUri');
    Pointer(xmlTextReaderConstPrefix) := GetProcAddress(libXmlHandle, 'xmlTextReaderConstPrefix');
    Pointer(xmlTextReaderConstXmlLang) := GetProcAddress(libXmlHandle, 'xmlTextReaderConstXmlLang');
    Pointer(xmlTextReaderConstString) := GetProcAddress(libXmlHandle, 'xmlTextReaderConstString');
    Pointer(xmlTextReaderConstValue) := GetProcAddress(libXmlHandle, 'xmlTextReaderConstValue');
    Pointer(xmlTextReaderBaseUri) := GetProcAddress(libXmlHandle, 'xmlTextReaderBaseUri');
    Pointer(xmlTextReaderLocalName) := GetProcAddress(libXmlHandle, 'xmlTextReaderLocalName');
    Pointer(xmlTextReaderName) := GetProcAddress(libXmlHandle, 'xmlTextReaderName');
    Pointer(xmlTextReaderNamespaceUri) := GetProcAddress(libXmlHandle, 'xmlTextReaderNamespaceUri');
    Pointer(xmlTextReaderPrefix) := GetProcAddress(libXmlHandle, 'xmlTextReaderPrefix');
    Pointer(xmlTextReaderXmlLang) := GetProcAddress(libXmlHandle, 'xmlTextReaderXmlLang');
    Pointer(xmlTextReaderValue) := GetProcAddress(libXmlHandle, 'xmlTextReaderValue');
    Pointer(xmlTextReaderClose) := GetProcAddress(libXmlHandle, 'xmlTextReaderClose');
    Pointer(xmlTextReaderGetAttributeNo) := GetProcAddress(libXmlHandle, 'xmlTextReaderGetAttributeNo');
    Pointer(xmlTextReaderGetAttribute) := GetProcAddress(libXmlHandle, 'xmlTextReaderGetAttribute');
    Pointer(xmlTextReaderGetAttributeNs) := GetProcAddress(libXmlHandle, 'xmlTextReaderGetAttributeNs');
    Pointer(xmlTextReaderGetRemainder) := GetProcAddress(libXmlHandle, 'xmlTextReaderGetRemainder');
    Pointer(xmlTextReaderLookupNamespace) := GetProcAddress(libXmlHandle, 'xmlTextReaderLookupNamespace');
    Pointer(xmlTextReaderMoveToAttributeNo) := GetProcAddress(libXmlHandle, 'xmlTextReaderMoveToAttributeNo');
    Pointer(xmlTextReaderMoveToAttribute) := GetProcAddress(libXmlHandle, 'xmlTextReaderMoveToAttribute');
    Pointer(xmlTextReaderMoveToAttributeNs) := GetProcAddress(libXmlHandle, 'xmlTextReaderMoveToAttributeNs');
    Pointer(xmlTextReaderMoveToFirstAttribute) := GetProcAddress(libXmlHandle, 'xmlTextReaderMoveToFirstAttribute');
    Pointer(xmlTextReaderMoveToNextAttribute) := GetProcAddress(libXmlHandle, 'xmlTextReaderMoveToNextAttribute');
    Pointer(xmlTextReaderMoveToElement) := GetProcAddress(libXmlHandle, 'xmlTextReaderMoveToElement');
    Pointer(xmlTextReaderNormalization) := GetProcAddress(libXmlHandle, 'xmlTextReaderNormalization');
    Pointer(xmlTextReaderConstEncoding) := GetProcAddress(libXmlHandle, 'xmlTextReaderConstEncoding');
    Pointer(xmlTextReaderSetParserProp) := GetProcAddress(libXmlHandle, 'xmlTextReaderSetParserProp');
    Pointer(xmlTextReaderGetParserProp) := GetProcAddress(libXmlHandle, 'xmlTextReaderGetParserProp');
    Pointer(xmlTextReaderCurrentNode) := GetProcAddress(libXmlHandle, 'xmlTextReaderCurrentNode');
    Pointer(xmlTextReaderGetParserLineNumber) := GetProcAddress(libXmlHandle, 'xmlTextReaderGetParserLineNumber');
    Pointer(xmlTextReaderGetParserColumnNumber) := GetProcAddress(libXmlHandle, 'xmlTextReaderGetParserColumnNumber');
    Pointer(xmlTextReaderPreserve) := GetProcAddress(libXmlHandle, 'xmlTextReaderPreserve');
  {$IFDEF LIBXML_PATTERN_ENABLED}
    Pointer(xmlTextReaderPreservePattern) := GetProcAddress(libXmlHandle, 'xmlTextReaderPreservePattern');
  {$ENDIF} (* LIBXML_PATTERN_ENABLED *)
    Pointer(xmlTextReaderCurrentDoc) := GetProcAddress(libXmlHandle, 'xmlTextReaderCurrentDoc');
    Pointer(xmlTextReaderExpand) := GetProcAddress(libXmlHandle, 'xmlTextReaderExpand');
    Pointer(xmlTextReaderNext) := GetProcAddress(libXmlHandle, 'xmlTextReaderNext');
    Pointer(xmlTextReaderNextSibling) := GetProcAddress(libXmlHandle, 'xmlTextReaderNextSibling');
    Pointer(xmlTextReaderIsValid) := GetProcAddress(libXmlHandle, 'xmlTextReaderIsValid');
  {$IFDEF LIBXML_SCHEMAS_ENABLED}
    Pointer(xmlTextReaderRelaxNGValidate) := GetProcAddress(libXmlHandle, 'xmlTextReaderRelaxNGValidate');
    Pointer(xmlTextReaderRelaxNGSetSchema) := GetProcAddress(libXmlHandle, 'xmlTextReaderRelaxNGSetSchema');
    Pointer(xmlTextReaderSchemaValidate) := GetProcAddress(libXmlHandle, 'xmlTextReaderSchemaValidate');
    Pointer(xmlTextReaderSchemaValidateCtxt) := GetProcAddress(libXmlHandle, 'xmlTextReaderSchemaValidateCtxt');
    Pointer(xmlTextReaderSetSchema) := GetProcAddress(libXmlHandle, 'xmlTextReaderSetSchema');
  {$ENDIF}
    Pointer(xmlTextReaderConstXmlVersion) := GetProcAddress(libXmlHandle, 'xmlTextReaderConstXmlVersion');
    Pointer(xmlTextReaderStandalone) := GetProcAddress(libXmlHandle, 'xmlTextReaderStandalone');
    Pointer(xmlTextReaderByteConsumed) := GetProcAddress(libXmlHandle, 'xmlTextReaderByteConsumed');
    Pointer(xmlReaderWalker) := GetProcAddress(libXmlHandle, 'xmlReaderWalker');
    Pointer(xmlReaderForDoc) := GetProcAddress(libXmlHandle, 'xmlReaderForDoc');
    Pointer(xmlReaderForFile) := GetProcAddress(libXmlHandle, 'xmlReaderForFile');
    Pointer(xmlReaderForMemory) := GetProcAddress(libXmlHandle, 'xmlReaderForMemory');
    Pointer(xmlReaderForFd) := GetProcAddress(libXmlHandle, 'xmlReaderForFd');
    Pointer(xmlReaderForIO) := GetProcAddress(libXmlHandle, 'xmlReaderForIO');
    Pointer(xmlReaderNewWalker) := GetProcAddress(libXmlHandle, 'xmlReaderNewWalker');
    Pointer(xmlReaderNewDoc) := GetProcAddress(libXmlHandle, 'xmlReaderNewDoc');
    Pointer(xmlReaderNewFile) := GetProcAddress(libXmlHandle, 'xmlReaderNewFile');
    Pointer(xmlReaderNewMemory) := GetProcAddress(libXmlHandle, 'xmlReaderNewMemory');
    Pointer(xmlReaderNewFd) := GetProcAddress(libXmlHandle, 'xmlReaderNewFd');
    Pointer(xmlReaderNewIO) := GetProcAddress(libXmlHandle, 'xmlReaderNewIO');
    Pointer(xmlTextReaderLocatorLineNumber) := GetProcAddress(libXmlHandle, 'xmlTextReaderLocatorLineNumber');
    Pointer(xmlTextReaderLocatorBaseURI) := GetProcAddress(libXmlHandle, 'xmlTextReaderLocatorBaseURI');
    Pointer(xmlTextReaderSetErrorHandler) := GetProcAddress(libXmlHandle, 'xmlTextReaderSetErrorHandler');
    Pointer(xmlTextReaderSetStructuredErrorHandler) := GetProcAddress(libXmlHandle, 'xmlTextReaderSetStructuredErrorHandler');
    Pointer(xmlTextReaderGetErrorHandler) := GetProcAddress(libXmlHandle, 'xmlTextReaderGetErrorHandler');
  {$ENDIF} (* LIBXML_READER_ENABLED *)
  {$ENDIF} (* LIBXML_SCHEMAS_ENABLED *)

    { xmlregexp.inc }
  {$IFDEF LIBXML_REGEXP_ENABLED}
    Pointer(xmlRegexpCompile) := GetProcAddress(libXmlHandle, 'xmlRegexpCompile');
    Pointer(xmlRegFreeRegexp) := GetProcAddress(libXmlHandle, 'xmlRegFreeRegexp');
    Pointer(xmlRegexpExec) := GetProcAddress(libXmlHandle, 'xmlRegexpExec');
    Pointer(xmlRegexpPrint) := GetProcAddress(libXmlHandle, 'xmlRegexpPrint');
    Pointer(xmlRegexpIsDeterminist) := GetProcAddress(libXmlHandle, 'xmlRegexpIsDeterminist');
    Pointer(xmlRegNewExecCtxt) := GetProcAddress(libXmlHandle, 'xmlRegNewExecCtxt');
    Pointer(xmlRegFreeExecCtxt) := GetProcAddress(libXmlHandle, 'xmlRegFreeExecCtxt');
    Pointer(xmlRegExecPushString) := GetProcAddress(libXmlHandle, 'xmlRegExecPushString');
    Pointer(xmlRegExecPushString2) := GetProcAddress(libXmlHandle, 'xmlRegExecPushString2');
    Pointer(xmlRegExecNextValues) := GetProcAddress(libXmlHandle, 'xmlRegExecNextValues');
    Pointer(xmlRegExecErrInfo) := GetProcAddress(libXmlHandle, 'xmlRegExecErrInfo');
  {$IFDEF LIBXML_EXPR_ENABLED}
    Pointer(xmlExpFreeCtxt) := GetProcAddress(libXmlHandle, 'xmlExpFreeCtxt');
    Pointer(xmlExpNewCtxt) := GetProcAddress(libXmlHandle, 'xmlExpNewCtxt');
    Pointer(xmlExpCtxtNbNodes) := GetProcAddress(libXmlHandle, 'xmlExpCtxtNbNodes');
    Pointer(xmlExpCtxtNbCons) := GetProcAddress(libXmlHandle, 'xmlExpCtxtNbCons');
    Pointer(xmlExpFree) := GetProcAddress(libXmlHandle, 'xmlExpFree');
    Pointer(xmlExpRef) := GetProcAddress(libXmlHandle, 'xmlExpRef');
    Pointer(xmlExpParse) := GetProcAddress(libXmlHandle, 'xmlExpParse');
    Pointer(xmlExpNewAtom) := GetProcAddress(libXmlHandle, 'xmlExpNewAtom');
    Pointer(xmlExpNewOr) := GetProcAddress(libXmlHandle, 'xmlExpNewOr');
    Pointer(xmlExpNewSeq) := GetProcAddress(libXmlHandle, 'xmlExpNewSeq');
    Pointer(xmlExpNewRange) := GetProcAddress(libXmlHandle, 'xmlExpNewRange');
    Pointer(xmlExpIsNillable) := GetProcAddress(libXmlHandle, 'xmlExpIsNillable');
    Pointer(xmlExpMaxToken) := GetProcAddress(libXmlHandle, 'xmlExpMaxToken');
    Pointer(xmlExpGetLanguage) := GetProcAddress(libXmlHandle, 'xmlExpGetLanguage');
    Pointer(xmlExpGetStart) := GetProcAddress(libXmlHandle, 'xmlExpGetStart');
    Pointer(xmlExpStringDerive) := GetProcAddress(libXmlHandle, 'xmlExpStringDerive');
    Pointer(xmlExpExpDerive) := GetProcAddress(libXmlHandle, 'xmlExpExpDerive');
    Pointer(xmlExpSubsume) := GetProcAddress(libXmlHandle, 'xmlExpSubsume');
    Pointer(xmlExpDump) := GetProcAddress(libXmlHandle, 'xmlExpDump');
  {$ENDIF} (* LIBXML_EXPR_ENABLED *)
  {$ENDIF} (* LIBXML_REGEXP_ENABLED *)

    { xmlsave.inc }
  {$IFDEF LIBXML_OUTPUT_ENABLED}
    Pointer(xmlSaveToFd) := GetProcAddress(libXmlHandle, 'xmlSaveToFd');
    Pointer(xmlSaveToFilename) := GetProcAddress(libXmlHandle, 'xmlSaveToFilename');
    Pointer(xmlSaveToBuffer) := GetProcAddress(libXmlHandle, 'xmlSaveToBuffer');
    Pointer(xmlSaveToIO) := GetProcAddress(libXmlHandle, 'xmlSaveToIO');
    Pointer(xmlSaveDoc) := GetProcAddress(libXmlHandle, 'xmlSaveDoc');
    Pointer(xmlSaveTree) := GetProcAddress(libXmlHandle, 'xmlSaveTree');
    Pointer(xmlSaveFlush) := GetProcAddress(libXmlHandle, 'xmlSaveFlush');
    Pointer(xmlSaveClose) := GetProcAddress(libXmlHandle, 'xmlSaveClose');
    Pointer(xmlSaveSetEscape) := GetProcAddress(libXmlHandle, 'xmlSaveSetEscape');
    Pointer(xmlSaveSetAttrEscape) := GetProcAddress(libXmlHandle, 'xmlSaveSetAttrEscape');
  {$ENDIF} (* LIBXML_OUTPUT_ENABLED *)

    { xmlschemas.inc }
  {$IFDEF LIBXML_SCHEMAS_ENABLED}
    Pointer(xmlSchemaNewParserCtxt) := GetProcAddress(libXmlHandle, 'xmlSchemaNewParserCtxt');
    Pointer(xmlSchemaNewMemParserCtxt) := GetProcAddress(libXmlHandle, 'xmlSchemaNewMemParserCtxt');
    Pointer(xmlSchemaNewDocParserCtxt) := GetProcAddress(libXmlHandle, 'xmlSchemaNewDocParserCtxt');
    Pointer(xmlSchemaFreeParserCtxt) := GetProcAddress(libXmlHandle, 'xmlSchemaFreeParserCtxt');
    Pointer(xmlSchemaSetParserErrors) := GetProcAddress(libXmlHandle, 'xmlSchemaSetParserErrors');
    Pointer(xmlSchemaSetParserStructuredErrors) := GetProcAddress(libXmlHandle, 'xmlSchemaSetParserStructuredErrors');
    Pointer(xmlSchemaGetParserErrors) := GetProcAddress(libXmlHandle, 'xmlSchemaGetParserErrors');
    Pointer(xmlSchemaIsValid) := GetProcAddress(libXmlHandle, 'xmlSchemaIsValid');
    Pointer(xmlSchemaParse) := GetProcAddress(libXmlHandle, 'xmlSchemaParse');
    Pointer(xmlSchemaFree) := GetProcAddress(libXmlHandle, 'xmlSchemaFree');
  {$IFDEF LIBXML_OUTPUT_ENABLED}
    Pointer(xmlSchemaDump) := GetProcAddress(libXmlHandle, 'xmlSchemaDump');
  {$ENDIF} (* LIBXML_OUTPUT_ENABLED *)
    Pointer(xmlSchemaSetValidErrors) := GetProcAddress(libXmlHandle, 'xmlSchemaSetValidErrors');
    Pointer(xmlSchemaSetValidStructuredErrors) := GetProcAddress(libXmlHandle, 'xmlSchemaSetValidStructuredErrors');
    Pointer(xmlSchemaGetValidErrors) := GetProcAddress(libXmlHandle, 'xmlSchemaGetValidErrors');
    Pointer(xmlSchemaSetValidOptions) := GetProcAddress(libXmlHandle, 'xmlSchemaSetValidOptions');
    Pointer(xmlSchemaValidCtxtGetOptions) := GetProcAddress(libXmlHandle, 'xmlSchemaValidCtxtGetOptions');
    Pointer(xmlSchemaNewValidCtxt) := GetProcAddress(libXmlHandle, 'xmlSchemaNewValidCtxt');
    Pointer(xmlSchemaValidCtxtGetParserCtxt) := GetProcAddress(libXmlHandle, 'xmlSchemaValidCtxtGetParserCtxt');
    Pointer(xmlSchemaFreeValidCtxt) := GetProcAddress(libXmlHandle, 'xmlSchemaFreeValidCtxt');
    Pointer(xmlSchemaValidateDoc) := GetProcAddress(libXmlHandle, 'xmlSchemaValidateDoc');
    Pointer(xmlSchemaValidateOneElement) := GetProcAddress(libXmlHandle, 'xmlSchemaValidateOneElement');
    Pointer(xmlSchemaValidateStream) := GetProcAddress(libXmlHandle, 'xmlSchemaValidateStream');
    Pointer(xmlSchemaValidateFile) := GetProcAddress(libXmlHandle, 'xmlSchemaValidateFile');
    Pointer(xmlSchemaValidateSetFilename) := GetProcAddress(libXmlHandle, 'xmlSchemaValidateSetFilename');
    Pointer(xmlSchemaValidateSetLocator) := GetProcAddress(libXmlHandle, 'xmlSchemaValidateSetLocator');
    Pointer(xmlSchemaSAXPlug) := GetProcAddress(libXmlHandle, 'xmlSchemaSAXPlug');
    Pointer(xmlSchemaSAXUnplug) := GetProcAddress(libXmlHandle, 'xmlSchemaSAXUnplug');
  {$ENDIF} (* LIBXML_SCHEMAS_ENABLED *)

    { xmlschemastypes.inc }
  {$IFDEF LIBXML_SCHEMAS_ENABLED}
    Pointer(xmlSchemaInitTypes) := GetProcAddress(libXmlHandle, 'xmlSchemaInitTypes');
    Pointer(xmlSchemaCleanupTypes) := GetProcAddress(libXmlHandle, 'xmlSchemaCleanupTypes');
    Pointer(xmlSchemaGetPredefinedType) := GetProcAddress(libXmlHandle, 'xmlSchemaGetPredefinedType');
    Pointer(xmlSchemaValidatePredefinedType) := GetProcAddress(libXmlHandle, 'xmlSchemaValidatePredefinedType');
    Pointer(xmlSchemaValPredefTypeNode) := GetProcAddress(libXmlHandle, 'xmlSchemaValPredefTypeNode');
    Pointer(xmlSchemaValidateFacet) := GetProcAddress(libXmlHandle, 'xmlSchemaValidateFacet');
    Pointer(xmlSchemaValidateFacetWhtsp) := GetProcAddress(libXmlHandle, 'xmlSchemaValidateFacetWhtsp');
    Pointer(xmlSchemaFreeValue) := GetProcAddress(libXmlHandle, 'xmlSchemaFreeValue');
    Pointer(xmlSchemaNewFacet) := GetProcAddress(libXmlHandle, 'xmlSchemaNewFacet');
    Pointer(xmlSchemaCheckFacet) := GetProcAddress(libXmlHandle, 'xmlSchemaCheckFacet');
    Pointer(xmlSchemaFreeFacet) := GetProcAddress(libXmlHandle, 'xmlSchemaFreeFacet');
    Pointer(xmlSchemaCompareValues) := GetProcAddress(libXmlHandle, 'xmlSchemaCompareValues');
    Pointer(xmlSchemaGetBuiltInListSimpleTypeItemType) := GetProcAddress(libXmlHandle, 'xmlSchemaGetBuiltInListSimpleTypeItemType');
    Pointer(xmlSchemaValidateListSimpleTypeFacet) := GetProcAddress(libXmlHandle, 'xmlSchemaValidateListSimpleTypeFacet');
    Pointer(xmlSchemaGetBuiltInType) := GetProcAddress(libXmlHandle, 'xmlSchemaGetBuiltInType');
    Pointer(xmlSchemaIsBuiltInTypeFacet) := GetProcAddress(libXmlHandle, 'xmlSchemaIsBuiltInTypeFacet');
    Pointer(xmlSchemaCollapseString) := GetProcAddress(libXmlHandle, 'xmlSchemaCollapseString');
    Pointer(xmlSchemaWhiteSpaceReplace) := GetProcAddress(libXmlHandle, 'xmlSchemaWhiteSpaceReplace');
    Pointer(xmlSchemaGetFacetValueAsULong) := GetProcAddress(libXmlHandle, 'xmlSchemaGetFacetValueAsULong');
    Pointer(xmlSchemaValidateLengthFacet) := GetProcAddress(libXmlHandle, 'xmlSchemaValidateLengthFacet');
    Pointer(xmlSchemaValidateLengthFacetWhtsp) := GetProcAddress(libXmlHandle, 'xmlSchemaValidateLengthFacetWhtsp');
    Pointer(xmlSchemaValPredefTypeNodeNoNorm) := GetProcAddress(libXmlHandle, 'xmlSchemaValPredefTypeNodeNoNorm');
    Pointer(xmlSchemaGetCanonValue) := GetProcAddress(libXmlHandle, 'xmlSchemaGetCanonValue');
    Pointer(xmlSchemaGetCanonValueWhtsp) := GetProcAddress(libXmlHandle, 'xmlSchemaGetCanonValueWhtsp');
    Pointer(xmlSchemaValueAppend) := GetProcAddress(libXmlHandle, 'xmlSchemaValueAppend');
    Pointer(xmlSchemaValueGetNext) := GetProcAddress(libXmlHandle, 'xmlSchemaValueGetNext');
    Pointer(xmlSchemaValueGetAsString) := GetProcAddress(libXmlHandle, 'xmlSchemaValueGetAsString');
    Pointer(xmlSchemaValueGetAsBoolean) := GetProcAddress(libXmlHandle, 'xmlSchemaValueGetAsBoolean');
    Pointer(xmlSchemaNewStringValue) := GetProcAddress(libXmlHandle, 'xmlSchemaNewStringValue');
    Pointer(xmlSchemaNewNOTATIONValue) := GetProcAddress(libXmlHandle, 'xmlSchemaNewNOTATIONValue');
    Pointer(xmlSchemaNewQNameValue) := GetProcAddress(libXmlHandle, 'xmlSchemaNewQNameValue');
    Pointer(xmlSchemaCompareValuesWhtsp) := GetProcAddress(libXmlHandle, 'xmlSchemaCompareValuesWhtsp');
    Pointer(xmlSchemaCopyValue) := GetProcAddress(libXmlHandle, 'xmlSchemaCopyValue');
    Pointer(xmlSchemaGetValType) := GetProcAddress(libXmlHandle, 'xmlSchemaGetValType');
  {$ENDIF} (* LIBXML_SCHEMAS_ENABLED *)

    { xmlstring.inc }
    Pointer(xmlStrdup) := GetProcAddress(libXmlHandle, 'xmlStrdup');
    Pointer(xmlStrndup) := GetProcAddress(libXmlHandle, 'xmlStrndup');
    Pointer(xmlCharStrndup) := GetProcAddress(libXmlHandle, 'xmlCharStrndup');
    Pointer(xmlCharStrdup) := GetProcAddress(libXmlHandle, 'xmlCharStrdup');
    Pointer(xmlStrsub) := GetProcAddress(libXmlHandle, 'xmlStrsub');
    Pointer(xmlStrchr) := GetProcAddress(libXmlHandle, 'xmlStrchr');
    Pointer(xmlStrstr) := GetProcAddress(libXmlHandle, 'xmlStrstr');
    Pointer(xmlStrcasestr) := GetProcAddress(libXmlHandle, 'xmlStrcasestr');
    Pointer(xmlStrcmp) := GetProcAddress(libXmlHandle, 'xmlStrcmp');
    Pointer(xmlStrncmp) := GetProcAddress(libXmlHandle, 'xmlStrncmp');
    Pointer(xmlStrcasecmp) := GetProcAddress(libXmlHandle, 'xmlStrcasecmp');
    Pointer(xmlStrncasecmp) := GetProcAddress(libXmlHandle, 'xmlStrncasecmp');
    Pointer(xmlStrEqual) := GetProcAddress(libXmlHandle, 'xmlStrEqual');
    Pointer(xmlStrQEqual) := GetProcAddress(libXmlHandle, 'xmlStrQEqual');
    Pointer(xmlStrlen) := GetProcAddress(libXmlHandle, 'xmlStrlen');
    Pointer(xmlStrcat) := GetProcAddress(libXmlHandle, 'xmlStrcat');
    Pointer(xmlStrncat) := GetProcAddress(libXmlHandle, 'xmlStrncat');
    Pointer(xmlStrncatNew) := GetProcAddress(libXmlHandle, 'xmlStrncatNew');
    Pointer(xmlStrPrintf) := GetProcAddress(libXmlHandle, 'xmlStrPrintf');
    Pointer(xmlStrVPrintf) := GetProcAddress(libXmlHandle, 'xmlStrVPrintf');
    Pointer(xmlGetUTF8Char) := GetProcAddress(libXmlHandle, 'xmlGetUTF8Char');
    Pointer(xmlCheckUTF8) := GetProcAddress(libXmlHandle, 'xmlCheckUTF8');
    Pointer(xmlUTF8Strsize) := GetProcAddress(libXmlHandle, 'xmlUTF8Strsize');
    Pointer(xmlUTF8Strndup) := GetProcAddress(libXmlHandle, 'xmlUTF8Strndup');
    Pointer(xmlUTF8Strpos) := GetProcAddress(libXmlHandle, 'xmlUTF8Strpos');
    Pointer(xmlUTF8Strloc) := GetProcAddress(libXmlHandle, 'xmlUTF8Strloc');
    Pointer(xmlUTF8Strsub) := GetProcAddress(libXmlHandle, 'xmlUTF8Strsub');
    Pointer(xmlUTF8Strlen) := GetProcAddress(libXmlHandle, 'xmlUTF8Strlen');
    Pointer(xmlUTF8Size) := GetProcAddress(libXmlHandle, 'xmlUTF8Size');
    Pointer(xmlUTF8Charcmp) := GetProcAddress(libXmlHandle, 'xmlUTF8Charcmp');

    { xmlunicode.inc }
  {$IFDEF LIBXML_UNICODE_ENABLED}
    Pointer(xmlUCSIsAegeanNumbers) := GetProcAddress(libXmlHandle, 'xmlUCSIsAegeanNumbers');
    Pointer(xmlUCSIsAlphabeticPresentationForms) := GetProcAddress(libXmlHandle, 'xmlUCSIsAlphabeticPresentationForms');
    Pointer(xmlUCSIsArabic) := GetProcAddress(libXmlHandle, 'xmlUCSIsArabic');
    Pointer(xmlUCSIsArabicPresentationFormsA) := GetProcAddress(libXmlHandle, 'xmlUCSIsArabicPresentationFormsA');
    Pointer(xmlUCSIsArabicPresentationFormsB) := GetProcAddress(libXmlHandle, 'xmlUCSIsArabicPresentationFormsB');
    Pointer(xmlUCSIsArmenian) := GetProcAddress(libXmlHandle, 'xmlUCSIsArmenian');
    Pointer(xmlUCSIsArrows) := GetProcAddress(libXmlHandle, 'xmlUCSIsArrows');
    Pointer(xmlUCSIsBasicLatin) := GetProcAddress(libXmlHandle, 'xmlUCSIsBasicLatin');
    Pointer(xmlUCSIsBengali) := GetProcAddress(libXmlHandle, 'xmlUCSIsBengali');
    Pointer(xmlUCSIsBlockElements) := GetProcAddress(libXmlHandle, 'xmlUCSIsBlockElements');
    Pointer(xmlUCSIsBopomofo) := GetProcAddress(libXmlHandle, 'xmlUCSIsBopomofo');
    Pointer(xmlUCSIsBopomofoExtended) := GetProcAddress(libXmlHandle, 'xmlUCSIsBopomofoExtended');
    Pointer(xmlUCSIsBoxDrawing) := GetProcAddress(libXmlHandle, 'xmlUCSIsBoxDrawing');
    Pointer(xmlUCSIsBraillePatterns) := GetProcAddress(libXmlHandle, 'xmlUCSIsBraillePatterns');
    Pointer(xmlUCSIsBuhid) := GetProcAddress(libXmlHandle, 'xmlUCSIsBuhid');
    Pointer(xmlUCSIsByzantineMusicalSymbols) := GetProcAddress(libXmlHandle, 'xmlUCSIsByzantineMusicalSymbols');
    Pointer(xmlUCSIsCJKCompatibility) := GetProcAddress(libXmlHandle, 'xmlUCSIsCJKCompatibility');
    Pointer(xmlUCSIsCJKCompatibilityForms) := GetProcAddress(libXmlHandle, 'xmlUCSIsCJKCompatibilityForms');
    Pointer(xmlUCSIsCJKCompatibilityIdeographs) := GetProcAddress(libXmlHandle, 'xmlUCSIsCJKCompatibilityIdeographs');
    Pointer(xmlUCSIsCJKCompatibilityIdeographsSupplement) := GetProcAddress(libXmlHandle, 'xmlUCSIsCJKCompatibilityIdeographsSupplement');
    Pointer(xmlUCSIsCJKRadicalsSupplement) := GetProcAddress(libXmlHandle, 'xmlUCSIsCJKRadicalsSupplement');
    Pointer(xmlUCSIsCJKSymbolsandPunctuation) := GetProcAddress(libXmlHandle, 'xmlUCSIsCJKSymbolsandPunctuation');
    Pointer(xmlUCSIsCJKUnifiedIdeographs) := GetProcAddress(libXmlHandle, 'xmlUCSIsCJKUnifiedIdeographs');
    Pointer(xmlUCSIsCJKUnifiedIdeographsExtensionA) := GetProcAddress(libXmlHandle, 'xmlUCSIsCJKUnifiedIdeographsExtensionA');
    Pointer(xmlUCSIsCJKUnifiedIdeographsExtensionB) := GetProcAddress(libXmlHandle, 'xmlUCSIsCJKUnifiedIdeographsExtensionB');
    Pointer(xmlUCSIsCherokee) := GetProcAddress(libXmlHandle, 'xmlUCSIsCherokee');
    Pointer(xmlUCSIsCombiningDiacriticalMarks) := GetProcAddress(libXmlHandle, 'xmlUCSIsCombiningDiacriticalMarks');
    Pointer(xmlUCSIsCombiningDiacriticalMarksforSymbols) := GetProcAddress(libXmlHandle, 'xmlUCSIsCombiningDiacriticalMarksforSymbols');
    Pointer(xmlUCSIsCombiningHalfMarks) := GetProcAddress(libXmlHandle, 'xmlUCSIsCombiningHalfMarks');
    Pointer(xmlUCSIsCombiningMarksforSymbols) := GetProcAddress(libXmlHandle, 'xmlUCSIsCombiningMarksforSymbols');
    Pointer(xmlUCSIsControlPictures) := GetProcAddress(libXmlHandle, 'xmlUCSIsControlPictures');
    Pointer(xmlUCSIsCurrencySymbols) := GetProcAddress(libXmlHandle, 'xmlUCSIsCurrencySymbols');
    Pointer(xmlUCSIsCypriotSyllabary) := GetProcAddress(libXmlHandle, 'xmlUCSIsCypriotSyllabary');
    Pointer(xmlUCSIsCyrillic) := GetProcAddress(libXmlHandle, 'xmlUCSIsCyrillic');
    Pointer(xmlUCSIsCyrillicSupplement) := GetProcAddress(libXmlHandle, 'xmlUCSIsCyrillicSupplement');
    Pointer(xmlUCSIsDeseret) := GetProcAddress(libXmlHandle, 'xmlUCSIsDeseret');
    Pointer(xmlUCSIsDevanagari) := GetProcAddress(libXmlHandle, 'xmlUCSIsDevanagari');
    Pointer(xmlUCSIsDingbats) := GetProcAddress(libXmlHandle, 'xmlUCSIsDingbats');
    Pointer(xmlUCSIsEnclosedAlphanumerics) := GetProcAddress(libXmlHandle, 'xmlUCSIsEnclosedAlphanumerics');
    Pointer(xmlUCSIsEnclosedCJKLettersandMonths) := GetProcAddress(libXmlHandle, 'xmlUCSIsEnclosedCJKLettersandMonths');
    Pointer(xmlUCSIsEthiopic) := GetProcAddress(libXmlHandle, 'xmlUCSIsEthiopic');
    Pointer(xmlUCSIsGeneralPunctuation) := GetProcAddress(libXmlHandle, 'xmlUCSIsGeneralPunctuation');
    Pointer(xmlUCSIsGeometricShapes) := GetProcAddress(libXmlHandle, 'xmlUCSIsGeometricShapes');
    Pointer(xmlUCSIsGeorgian) := GetProcAddress(libXmlHandle, 'xmlUCSIsGeorgian');
    Pointer(xmlUCSIsGothic) := GetProcAddress(libXmlHandle, 'xmlUCSIsGothic');
    Pointer(xmlUCSIsGreek) := GetProcAddress(libXmlHandle, 'xmlUCSIsGreek');
    Pointer(xmlUCSIsGreekExtended) := GetProcAddress(libXmlHandle, 'xmlUCSIsGreekExtended');
    Pointer(xmlUCSIsGreekandCoptic) := GetProcAddress(libXmlHandle, 'xmlUCSIsGreekandCoptic');
    Pointer(xmlUCSIsGujarati) := GetProcAddress(libXmlHandle, 'xmlUCSIsGujarati');
    Pointer(xmlUCSIsGurmukhi) := GetProcAddress(libXmlHandle, 'xmlUCSIsGurmukhi');
    Pointer(xmlUCSIsHalfwidthandFullwidthForms) := GetProcAddress(libXmlHandle, 'xmlUCSIsHalfwidthandFullwidthForms');
    Pointer(xmlUCSIsHangulCompatibilityJamo) := GetProcAddress(libXmlHandle, 'xmlUCSIsHangulCompatibilityJamo');
    Pointer(xmlUCSIsHangulJamo) := GetProcAddress(libXmlHandle, 'xmlUCSIsHangulJamo');
    Pointer(xmlUCSIsHangulSyllables) := GetProcAddress(libXmlHandle, 'xmlUCSIsHangulSyllables');
    Pointer(xmlUCSIsHanunoo) := GetProcAddress(libXmlHandle, 'xmlUCSIsHanunoo');
    Pointer(xmlUCSIsHebrew) := GetProcAddress(libXmlHandle, 'xmlUCSIsHebrew');
    Pointer(xmlUCSIsHighPrivateUseSurrogates) := GetProcAddress(libXmlHandle, 'xmlUCSIsHighPrivateUseSurrogates');
    Pointer(xmlUCSIsHighSurrogates) := GetProcAddress(libXmlHandle, 'xmlUCSIsHighSurrogates');
    Pointer(xmlUCSIsHiragana) := GetProcAddress(libXmlHandle, 'xmlUCSIsHiragana');
    Pointer(xmlUCSIsIPAExtensions) := GetProcAddress(libXmlHandle, 'xmlUCSIsIPAExtensions');
    Pointer(xmlUCSIsIdeographicDescriptionCharacters) := GetProcAddress(libXmlHandle, 'xmlUCSIsIdeographicDescriptionCharacters');
    Pointer(xmlUCSIsKanbun) := GetProcAddress(libXmlHandle, 'xmlUCSIsKanbun');
    Pointer(xmlUCSIsKangxiRadicals) := GetProcAddress(libXmlHandle, 'xmlUCSIsKangxiRadicals');
    Pointer(xmlUCSIsKannada) := GetProcAddress(libXmlHandle, 'xmlUCSIsKannada');
    Pointer(xmlUCSIsKatakana) := GetProcAddress(libXmlHandle, 'xmlUCSIsKatakana');
    Pointer(xmlUCSIsKatakanaPhoneticExtensions) := GetProcAddress(libXmlHandle, 'xmlUCSIsKatakanaPhoneticExtensions');
    Pointer(xmlUCSIsKhmer) := GetProcAddress(libXmlHandle, 'xmlUCSIsKhmer');
    Pointer(xmlUCSIsKhmerSymbols) := GetProcAddress(libXmlHandle, 'xmlUCSIsKhmerSymbols');
    Pointer(xmlUCSIsLao) := GetProcAddress(libXmlHandle, 'xmlUCSIsLao');
    Pointer(xmlUCSIsLatin1Supplement) := GetProcAddress(libXmlHandle, 'xmlUCSIsLatin1Supplement');
    Pointer(xmlUCSIsLatinExtendedA) := GetProcAddress(libXmlHandle, 'xmlUCSIsLatinExtendedA');
    Pointer(xmlUCSIsLatinExtendedB) := GetProcAddress(libXmlHandle, 'xmlUCSIsLatinExtendedB');
    Pointer(xmlUCSIsLatinExtendedAdditional) := GetProcAddress(libXmlHandle, 'xmlUCSIsLatinExtendedAdditional');
    Pointer(xmlUCSIsLetterlikeSymbols) := GetProcAddress(libXmlHandle, 'xmlUCSIsLetterlikeSymbols');
    Pointer(xmlUCSIsLimbu) := GetProcAddress(libXmlHandle, 'xmlUCSIsLimbu');
    Pointer(xmlUCSIsLinearBIdeograms) := GetProcAddress(libXmlHandle, 'xmlUCSIsLinearBIdeograms');
    Pointer(xmlUCSIsLinearBSyllabary) := GetProcAddress(libXmlHandle, 'xmlUCSIsLinearBSyllabary');
    Pointer(xmlUCSIsLowSurrogates) := GetProcAddress(libXmlHandle, 'xmlUCSIsLowSurrogates');
    Pointer(xmlUCSIsMalayalam) := GetProcAddress(libXmlHandle, 'xmlUCSIsMalayalam');
    Pointer(xmlUCSIsMathematicalAlphanumericSymbols) := GetProcAddress(libXmlHandle, 'xmlUCSIsMathematicalAlphanumericSymbols');
    Pointer(xmlUCSIsMathematicalOperators) := GetProcAddress(libXmlHandle, 'xmlUCSIsMathematicalOperators');
    Pointer(xmlUCSIsMiscellaneousMathematicalSymbolsA) := GetProcAddress(libXmlHandle, 'xmlUCSIsMiscellaneousMathematicalSymbolsA');
    Pointer(xmlUCSIsMiscellaneousMathematicalSymbolsB) := GetProcAddress(libXmlHandle, 'xmlUCSIsMiscellaneousMathematicalSymbolsB');
    Pointer(xmlUCSIsMiscellaneousSymbols) := GetProcAddress(libXmlHandle, 'xmlUCSIsMiscellaneousSymbols');
    Pointer(xmlUCSIsMiscellaneousSymbolsandArrows) := GetProcAddress(libXmlHandle, 'xmlUCSIsMiscellaneousSymbolsandArrows');
    Pointer(xmlUCSIsMiscellaneousTechnical) := GetProcAddress(libXmlHandle, 'xmlUCSIsMiscellaneousTechnical');
    Pointer(xmlUCSIsMongolian) := GetProcAddress(libXmlHandle, 'xmlUCSIsMongolian');
    Pointer(xmlUCSIsMusicalSymbols) := GetProcAddress(libXmlHandle, 'xmlUCSIsMusicalSymbols');
    Pointer(xmlUCSIsMyanmar) := GetProcAddress(libXmlHandle, 'xmlUCSIsMyanmar');
    Pointer(xmlUCSIsNumberForms) := GetProcAddress(libXmlHandle, 'xmlUCSIsNumberForms');
    Pointer(xmlUCSIsOgham) := GetProcAddress(libXmlHandle, 'xmlUCSIsOgham');
    Pointer(xmlUCSIsOldItalic) := GetProcAddress(libXmlHandle, 'xmlUCSIsOldItalic');
    Pointer(xmlUCSIsOpticalCharacterRecognition) := GetProcAddress(libXmlHandle, 'xmlUCSIsOpticalCharacterRecognition');
    Pointer(xmlUCSIsOriya) := GetProcAddress(libXmlHandle, 'xmlUCSIsOriya');
    Pointer(xmlUCSIsOsmanya) := GetProcAddress(libXmlHandle, 'xmlUCSIsOsmanya');
    Pointer(xmlUCSIsPhoneticExtensions) := GetProcAddress(libXmlHandle, 'xmlUCSIsPhoneticExtensions');
    Pointer(xmlUCSIsPrivateUse) := GetProcAddress(libXmlHandle, 'xmlUCSIsPrivateUse');
    Pointer(xmlUCSIsPrivateUseArea) := GetProcAddress(libXmlHandle, 'xmlUCSIsPrivateUseArea');
    Pointer(xmlUCSIsRunic) := GetProcAddress(libXmlHandle, 'xmlUCSIsRunic');
    Pointer(xmlUCSIsShavian) := GetProcAddress(libXmlHandle, 'xmlUCSIsShavian');
    Pointer(xmlUCSIsSinhala) := GetProcAddress(libXmlHandle, 'xmlUCSIsSinhala');
    Pointer(xmlUCSIsSmallFormVariants) := GetProcAddress(libXmlHandle, 'xmlUCSIsSmallFormVariants');
    Pointer(xmlUCSIsSpacingModifierLetters) := GetProcAddress(libXmlHandle, 'xmlUCSIsSpacingModifierLetters');
    Pointer(xmlUCSIsSpecials) := GetProcAddress(libXmlHandle, 'xmlUCSIsSpecials');
    Pointer(xmlUCSIsSuperscriptsandSubscripts) := GetProcAddress(libXmlHandle, 'xmlUCSIsSuperscriptsandSubscripts');
    Pointer(xmlUCSIsSupplementalArrowsA) := GetProcAddress(libXmlHandle, 'xmlUCSIsSupplementalArrowsA');
    Pointer(xmlUCSIsSupplementalArrowsB) := GetProcAddress(libXmlHandle, 'xmlUCSIsSupplementalArrowsB');
    Pointer(xmlUCSIsSupplementalMathematicalOperators) := GetProcAddress(libXmlHandle, 'xmlUCSIsSupplementalMathematicalOperators');
    Pointer(xmlUCSIsSupplementaryPrivateUseAreaA) := GetProcAddress(libXmlHandle, 'xmlUCSIsSupplementaryPrivateUseAreaA');
    Pointer(xmlUCSIsSupplementaryPrivateUseAreaB) := GetProcAddress(libXmlHandle, 'xmlUCSIsSupplementaryPrivateUseAreaB');
    Pointer(xmlUCSIsSyriac) := GetProcAddress(libXmlHandle, 'xmlUCSIsSyriac');
    Pointer(xmlUCSIsTagalog) := GetProcAddress(libXmlHandle, 'xmlUCSIsTagalog');
    Pointer(xmlUCSIsTagbanwa) := GetProcAddress(libXmlHandle, 'xmlUCSIsTagbanwa');
    Pointer(xmlUCSIsTags) := GetProcAddress(libXmlHandle, 'xmlUCSIsTags');
    Pointer(xmlUCSIsTaiLe) := GetProcAddress(libXmlHandle, 'xmlUCSIsTaiLe');
    Pointer(xmlUCSIsTaiXuanJingSymbols) := GetProcAddress(libXmlHandle, 'xmlUCSIsTaiXuanJingSymbols');
    Pointer(xmlUCSIsTamil) := GetProcAddress(libXmlHandle, 'xmlUCSIsTamil');
    Pointer(xmlUCSIsTelugu) := GetProcAddress(libXmlHandle, 'xmlUCSIsTelugu');
    Pointer(xmlUCSIsThaana) := GetProcAddress(libXmlHandle, 'xmlUCSIsThaana');
    Pointer(xmlUCSIsThai) := GetProcAddress(libXmlHandle, 'xmlUCSIsThai');
    Pointer(xmlUCSIsTibetan) := GetProcAddress(libXmlHandle, 'xmlUCSIsTibetan');
    Pointer(xmlUCSIsUgaritic) := GetProcAddress(libXmlHandle, 'xmlUCSIsUgaritic');
    Pointer(xmlUCSIsUnifiedCanadianAboriginalSyllabics) := GetProcAddress(libXmlHandle, 'xmlUCSIsUnifiedCanadianAboriginalSyllabics');
    Pointer(xmlUCSIsVariationSelectors) := GetProcAddress(libXmlHandle, 'xmlUCSIsVariationSelectors');
    Pointer(xmlUCSIsVariationSelectorsSupplement) := GetProcAddress(libXmlHandle, 'xmlUCSIsVariationSelectorsSupplement');
    Pointer(xmlUCSIsYiRadicals) := GetProcAddress(libXmlHandle, 'xmlUCSIsYiRadicals');
    Pointer(xmlUCSIsYiSyllables) := GetProcAddress(libXmlHandle, 'xmlUCSIsYiSyllables');
    Pointer(xmlUCSIsYijingHexagramSymbols) := GetProcAddress(libXmlHandle, 'xmlUCSIsYijingHexagramSymbols');
    Pointer(xmlUCSIsBlock) := GetProcAddress(libXmlHandle, 'xmlUCSIsBlock');
    Pointer(xmlUCSIsCatC) := GetProcAddress(libXmlHandle, 'xmlUCSIsCatC');
    Pointer(xmlUCSIsCatCc) := GetProcAddress(libXmlHandle, 'xmlUCSIsCatCc');
    Pointer(xmlUCSIsCatCf) := GetProcAddress(libXmlHandle, 'xmlUCSIsCatCf');
    Pointer(xmlUCSIsCatCo) := GetProcAddress(libXmlHandle, 'xmlUCSIsCatCo');
    Pointer(xmlUCSIsCatCs) := GetProcAddress(libXmlHandle, 'xmlUCSIsCatCs');
    Pointer(xmlUCSIsCatL) := GetProcAddress(libXmlHandle, 'xmlUCSIsCatL');
    Pointer(xmlUCSIsCatLl) := GetProcAddress(libXmlHandle, 'xmlUCSIsCatLl');
    Pointer(xmlUCSIsCatLm) := GetProcAddress(libXmlHandle, 'xmlUCSIsCatLm');
    Pointer(xmlUCSIsCatLo) := GetProcAddress(libXmlHandle, 'xmlUCSIsCatLo');
    Pointer(xmlUCSIsCatLt) := GetProcAddress(libXmlHandle, 'xmlUCSIsCatLt');
    Pointer(xmlUCSIsCatLu) := GetProcAddress(libXmlHandle, 'xmlUCSIsCatLu');
    Pointer(xmlUCSIsCatM) := GetProcAddress(libXmlHandle, 'xmlUCSIsCatM');
    Pointer(xmlUCSIsCatMc) := GetProcAddress(libXmlHandle, 'xmlUCSIsCatMc');
    Pointer(xmlUCSIsCatMe) := GetProcAddress(libXmlHandle, 'xmlUCSIsCatMe');
    Pointer(xmlUCSIsCatMn) := GetProcAddress(libXmlHandle, 'xmlUCSIsCatMn');
    Pointer(xmlUCSIsCatN) := GetProcAddress(libXmlHandle, 'xmlUCSIsCatN');
    Pointer(xmlUCSIsCatNd) := GetProcAddress(libXmlHandle, 'xmlUCSIsCatNd');
    Pointer(xmlUCSIsCatNl) := GetProcAddress(libXmlHandle, 'xmlUCSIsCatNl');
    Pointer(xmlUCSIsCatNo) := GetProcAddress(libXmlHandle, 'xmlUCSIsCatNo');
    Pointer(xmlUCSIsCatP) := GetProcAddress(libXmlHandle, 'xmlUCSIsCatP');
    Pointer(xmlUCSIsCatPc) := GetProcAddress(libXmlHandle, 'xmlUCSIsCatPc');
    Pointer(xmlUCSIsCatPd) := GetProcAddress(libXmlHandle, 'xmlUCSIsCatPd');
    Pointer(xmlUCSIsCatPe) := GetProcAddress(libXmlHandle, 'xmlUCSIsCatPe');
    Pointer(xmlUCSIsCatPf) := GetProcAddress(libXmlHandle, 'xmlUCSIsCatPf');
    Pointer(xmlUCSIsCatPi) := GetProcAddress(libXmlHandle, 'xmlUCSIsCatPi');
    Pointer(xmlUCSIsCatPo) := GetProcAddress(libXmlHandle, 'xmlUCSIsCatPo');
    Pointer(xmlUCSIsCatPs) := GetProcAddress(libXmlHandle, 'xmlUCSIsCatPs');
    Pointer(xmlUCSIsCatS) := GetProcAddress(libXmlHandle, 'xmlUCSIsCatS');
    Pointer(xmlUCSIsCatSc) := GetProcAddress(libXmlHandle, 'xmlUCSIsCatSc');
    Pointer(xmlUCSIsCatSk) := GetProcAddress(libXmlHandle, 'xmlUCSIsCatSk');
    Pointer(xmlUCSIsCatSm) := GetProcAddress(libXmlHandle, 'xmlUCSIsCatSm');
    Pointer(xmlUCSIsCatSo) := GetProcAddress(libXmlHandle, 'xmlUCSIsCatSo');
    Pointer(xmlUCSIsCatZ) := GetProcAddress(libXmlHandle, 'xmlUCSIsCatZ');
    Pointer(xmlUCSIsCatZl) := GetProcAddress(libXmlHandle, 'xmlUCSIsCatZl');
    Pointer(xmlUCSIsCatZp) := GetProcAddress(libXmlHandle, 'xmlUCSIsCatZp');
    Pointer(xmlUCSIsCatZs) := GetProcAddress(libXmlHandle, 'xmlUCSIsCatZs');
    Pointer(xmlUCSIsCat) := GetProcAddress(libXmlHandle, 'xmlUCSIsCat');
  {$ENDIF} (* LIBXML_UNICODE_ENABLED *)

    { xmlwriter.inc }
  {$IFDEF LIBXML_WRITER_ENABLED}
    Pointer(xmlNewTextWriter) := GetProcAddress(libXmlHandle, 'xmlNewTextWriter');
    Pointer(xmlNewTextWriterFilename) := GetProcAddress(libXmlHandle, 'xmlNewTextWriterFilename');
    Pointer(xmlNewTextWriterMemory) := GetProcAddress(libXmlHandle, 'xmlNewTextWriterMemory');
    Pointer(xmlNewTextWriterPushParser) := GetProcAddress(libXmlHandle, 'xmlNewTextWriterPushParser');
    Pointer(xmlNewTextWriterDoc) := GetProcAddress(libXmlHandle, 'xmlNewTextWriterDoc');
    Pointer(xmlNewTextWriterTree) := GetProcAddress(libXmlHandle, 'xmlNewTextWriterTree');
    Pointer(xmlFreeTextWriter) := GetProcAddress(libXmlHandle, 'xmlFreeTextWriter');
    Pointer(xmlTextWriterStartDocument) := GetProcAddress(libXmlHandle, 'xmlTextWriterStartDocument');
    Pointer(xmlTextWriterEndDocument) := GetProcAddress(libXmlHandle, 'xmlTextWriterEndDocument');
    Pointer(xmlTextWriterStartComment) := GetProcAddress(libXmlHandle, 'xmlTextWriterStartComment');
    Pointer(xmlTextWriterEndComment) := GetProcAddress(libXmlHandle, 'xmlTextWriterEndComment');
    Pointer(xmlTextWriterWriteFormatComment) := GetProcAddress(libXmlHandle, 'xmlTextWriterWriteFormatComment');
    Pointer(xmlTextWriterWriteVFormatComment) := GetProcAddress(libXmlHandle, 'xmlTextWriterWriteVFormatComment');
    Pointer(xmlTextWriterWriteComment) := GetProcAddress(libXmlHandle, 'xmlTextWriterWriteComment');
    Pointer(xmlTextWriterStartElement) := GetProcAddress(libXmlHandle, 'xmlTextWriterStartElement');
    Pointer(xmlTextWriterStartElementNS) := GetProcAddress(libXmlHandle, 'xmlTextWriterStartElementNS');
    Pointer(xmlTextWriterEndElement) := GetProcAddress(libXmlHandle, 'xmlTextWriterEndElement');
    Pointer(xmlTextWriterFullEndElement) := GetProcAddress(libXmlHandle, 'xmlTextWriterFullEndElement');
    Pointer(xmlTextWriterWriteFormatElement) := GetProcAddress(libXmlHandle, 'xmlTextWriterWriteFormatElement');
    Pointer(xmlTextWriterWriteVFormatElement) := GetProcAddress(libXmlHandle, 'xmlTextWriterWriteVFormatElement');
    Pointer(xmlTextWriterWriteElement) := GetProcAddress(libXmlHandle, 'xmlTextWriterWriteElement');
    Pointer(xmlTextWriterWriteFormatElementNS) := GetProcAddress(libXmlHandle, 'xmlTextWriterWriteFormatElementNS');
    Pointer(xmlTextWriterWriteVFormatElementNS) := GetProcAddress(libXmlHandle, 'xmlTextWriterWriteVFormatElementNS');
    Pointer(xmlTextWriterWriteElementNS) := GetProcAddress(libXmlHandle, 'xmlTextWriterWriteElementNS');
    Pointer(xmlTextWriterWriteFormatRaw) := GetProcAddress(libXmlHandle, 'xmlTextWriterWriteFormatRaw');
    Pointer(xmlTextWriterWriteVFormatRaw) := GetProcAddress(libXmlHandle, 'xmlTextWriterWriteVFormatRaw');
    Pointer(xmlTextWriterWriteRawLen) := GetProcAddress(libXmlHandle, 'xmlTextWriterWriteRawLen');
    Pointer(xmlTextWriterWriteRaw) := GetProcAddress(libXmlHandle, 'xmlTextWriterWriteRaw');
    Pointer(xmlTextWriterWriteFormatString) := GetProcAddress(libXmlHandle, 'xmlTextWriterWriteFormatString');
    Pointer(xmlTextWriterWriteVFormatString) := GetProcAddress(libXmlHandle, 'xmlTextWriterWriteVFormatString');
    Pointer(xmlTextWriterWriteString) := GetProcAddress(libXmlHandle, 'xmlTextWriterWriteString');
    Pointer(xmlTextWriterWriteBase64) := GetProcAddress(libXmlHandle, 'xmlTextWriterWriteBase64');
    Pointer(xmlTextWriterWriteBinHex) := GetProcAddress(libXmlHandle, 'xmlTextWriterWriteBinHex');
    Pointer(xmlTextWriterStartAttribute) := GetProcAddress(libXmlHandle, 'xmlTextWriterStartAttribute');
    Pointer(xmlTextWriterStartAttributeNS) := GetProcAddress(libXmlHandle, 'xmlTextWriterStartAttributeNS');
    Pointer(xmlTextWriterEndAttribute) := GetProcAddress(libXmlHandle, 'xmlTextWriterEndAttribute');
    Pointer(xmlTextWriterWriteFormatAttribute) := GetProcAddress(libXmlHandle, 'xmlTextWriterWriteFormatAttribute');
    Pointer(xmlTextWriterWriteVFormatAttribute) := GetProcAddress(libXmlHandle, 'xmlTextWriterWriteVFormatAttribute');
    Pointer(xmlTextWriterWriteAttribute) := GetProcAddress(libXmlHandle, 'xmlTextWriterWriteAttribute');
    Pointer(xmlTextWriterWriteFormatAttributeNS) := GetProcAddress(libXmlHandle, 'xmlTextWriterWriteFormatAttributeNS');
    Pointer(xmlTextWriterWriteVFormatAttributeNS) := GetProcAddress(libXmlHandle, 'xmlTextWriterWriteVFormatAttributeNS');
    Pointer(xmlTextWriterWriteAttributeNS) := GetProcAddress(libXmlHandle, 'xmlTextWriterWriteAttributeNS');
    Pointer(xmlTextWriterStartPI) := GetProcAddress(libXmlHandle, 'xmlTextWriterStartPI');
    Pointer(xmlTextWriterEndPI) := GetProcAddress(libXmlHandle, 'xmlTextWriterEndPI');
    Pointer(xmlTextWriterWriteFormatPI) := GetProcAddress(libXmlHandle, 'xmlTextWriterWriteFormatPI');
    Pointer(xmlTextWriterWriteVFormatPI) := GetProcAddress(libXmlHandle, 'xmlTextWriterWriteVFormatPI');
    Pointer(xmlTextWriterWritePI) := GetProcAddress(libXmlHandle, 'xmlTextWriterWritePI');
    Pointer(xmlTextWriterStartCDATA) := GetProcAddress(libXmlHandle, 'xmlTextWriterStartCDATA');
    Pointer(xmlTextWriterEndCDATA) := GetProcAddress(libXmlHandle, 'xmlTextWriterEndCDATA');
    Pointer(xmlTextWriterWriteFormatCDATA) := GetProcAddress(libXmlHandle, 'xmlTextWriterWriteFormatCDATA');
    Pointer(xmlTextWriterWriteVFormatCDATA) := GetProcAddress(libXmlHandle, 'xmlTextWriterWriteVFormatCDATA');
    Pointer(xmlTextWriterWriteCDATA) := GetProcAddress(libXmlHandle, 'xmlTextWriterWriteCDATA');
    Pointer(xmlTextWriterStartDTD) := GetProcAddress(libXmlHandle, 'xmlTextWriterStartDTD');
    Pointer(xmlTextWriterEndDTD) := GetProcAddress(libXmlHandle, 'xmlTextWriterEndDTD');
    Pointer(xmlTextWriterWriteFormatDTD) := GetProcAddress(libXmlHandle, 'xmlTextWriterWriteFormatDTD');
    Pointer(xmlTextWriterWriteVFormatDTD) := GetProcAddress(libXmlHandle, 'xmlTextWriterWriteVFormatDTD');
    Pointer(xmlTextWriterWriteDTD) := GetProcAddress(libXmlHandle, 'xmlTextWriterWriteDTD');
    Pointer(xmlTextWriterStartDTDElement) := GetProcAddress(libXmlHandle, 'xmlTextWriterStartDTDElement');
    Pointer(xmlTextWriterEndDTDElement) := GetProcAddress(libXmlHandle, 'xmlTextWriterEndDTDElement');
    Pointer(xmlTextWriterWriteFormatDTDElement) := GetProcAddress(libXmlHandle, 'xmlTextWriterWriteFormatDTDElement');
    Pointer(xmlTextWriterWriteVFormatDTDElement) := GetProcAddress(libXmlHandle, 'xmlTextWriterWriteVFormatDTDElement');
    Pointer(xmlTextWriterWriteDTDElement) := GetProcAddress(libXmlHandle, 'xmlTextWriterWriteDTDElement');
    Pointer(xmlTextWriterStartDTDAttlist) := GetProcAddress(libXmlHandle, 'xmlTextWriterStartDTDAttlist');
    Pointer(xmlTextWriterEndDTDAttlist) := GetProcAddress(libXmlHandle, 'xmlTextWriterEndDTDAttlist');
    Pointer(xmlTextWriterWriteFormatDTDAttlist) := GetProcAddress(libXmlHandle, 'xmlTextWriterWriteFormatDTDAttlist');
    Pointer(xmlTextWriterWriteVFormatDTDAttlist) := GetProcAddress(libXmlHandle, 'xmlTextWriterWriteVFormatDTDAttlist');
    Pointer(xmlTextWriterWriteDTDAttlist) := GetProcAddress(libXmlHandle, 'xmlTextWriterWriteDTDAttlist');
    Pointer(xmlTextWriterStartDTDEntity) := GetProcAddress(libXmlHandle, 'xmlTextWriterStartDTDEntity');
    Pointer(xmlTextWriterEndDTDEntity) := GetProcAddress(libXmlHandle, 'xmlTextWriterEndDTDEntity');
    Pointer(xmlTextWriterWriteFormatDTDInternalEntity) := GetProcAddress(libXmlHandle, 'xmlTextWriterWriteFormatDTDInternalEntity');
    Pointer(xmlTextWriterWriteVFormatDTDInternalEntity) := GetProcAddress(libXmlHandle, 'xmlTextWriterWriteVFormatDTDInternalEntity');
    Pointer(xmlTextWriterWriteDTDInternalEntity) := GetProcAddress(libXmlHandle, 'xmlTextWriterWriteDTDInternalEntity');
    Pointer(xmlTextWriterWriteDTDExternalEntity) := GetProcAddress(libXmlHandle, 'xmlTextWriterWriteDTDExternalEntity');
    Pointer(xmlTextWriterWriteDTDExternalEntityContents) := GetProcAddress(libXmlHandle, 'xmlTextWriterWriteDTDExternalEntityContents');
    Pointer(xmlTextWriterWriteDTDEntity) := GetProcAddress(libXmlHandle, 'xmlTextWriterWriteDTDEntity');
    Pointer(xmlTextWriterWriteDTDNotation) := GetProcAddress(libXmlHandle, 'xmlTextWriterWriteDTDNotation');
    Pointer(xmlTextWriterSetIndent) := GetProcAddress(libXmlHandle, 'xmlTextWriterSetIndent');
    Pointer(xmlTextWriterSetIndentString) := GetProcAddress(libXmlHandle, 'xmlTextWriterSetIndentString');
    Pointer(xmlTextWriterSetQuoteChar) := GetProcAddress(libXmlHandle, 'xmlTextWriterSetQuoteChar');
    Pointer(xmlTextWriterFlush) := GetProcAddress(libXmlHandle, 'xmlTextWriterFlush');
    Pointer(xmlTextWriterClose) := GetProcAddress(libXmlHandle, 'xmlTextWriterClose');
  {$ENDIF} (* LIBXML_WRITER_ENABLED *)

    { c14n.inc }
  {$IFDEF LIBXML_C14N_ENABLED}
  {$IFDEF LIBXML_OUTPUT_ENABLED}
    Pointer(xmlC14NDocSaveTo) := GetProcAddress(libXmlHandle, 'xmlC14NDocSaveTo');
    Pointer(xmlC14NDocDumpMemory) := GetProcAddress(libXmlHandle, 'xmlC14NDocDumpMemory');
    Pointer(xmlC14NDocSave) := GetProcAddress(libXmlHandle, 'xmlC14NDocSave');
    Pointer(xmlC14NExecute) := GetProcAddress(libXmlHandle, 'xmlC14NExecute');
  {$ENDIF} (* LIBXML_OUTPUT_ENABLED *)
  {$ENDIF} (* LIBXML_C14N_ENABLED *)

    { xpath.inc }
  {$IFDEF LIBXML_XPATH_ENABLED}
    Pointer(xmlXPathFreeObject) := GetProcAddress(libXmlHandle, 'xmlXPathFreeObject');
    Pointer(xmlXPathNodeSetCreate) := GetProcAddress(libXmlHandle, 'xmlXPathNodeSetCreate');
    Pointer(xmlXPathFreeNodeSetList) := GetProcAddress(libXmlHandle, 'xmlXPathFreeNodeSetList');
    Pointer(xmlXPathFreeNodeSet) := GetProcAddress(libXmlHandle, 'xmlXPathFreeNodeSet');
    Pointer(xmlXPathObjectCopy) := GetProcAddress(libXmlHandle, 'xmlXPathObjectCopy');
    Pointer(xmlXPathCmpNodes) := GetProcAddress(libXmlHandle, 'xmlXPathCmpNodes');
    Pointer(xmlXPathCastNumberToBoolean) := GetProcAddress(libXmlHandle, 'xmlXPathCastNumberToBoolean');
    Pointer(xmlXPathCastStringToBoolean) := GetProcAddress(libXmlHandle, 'xmlXPathCastStringToBoolean');
    Pointer(xmlXPathCastNodeSetToBoolean) := GetProcAddress(libXmlHandle, 'xmlXPathCastNodeSetToBoolean');
    Pointer(xmlXPathCastToBoolean) := GetProcAddress(libXmlHandle, 'xmlXPathCastToBoolean');
    Pointer(xmlXPathCastBooleanToNumber) := GetProcAddress(libXmlHandle, 'xmlXPathCastBooleanToNumber');
    Pointer(xmlXPathCastStringToNumber) := GetProcAddress(libXmlHandle, 'xmlXPathCastStringToNumber');
    Pointer(xmlXPathCastNodeToNumber) := GetProcAddress(libXmlHandle, 'xmlXPathCastNodeToNumber');
    Pointer(xmlXPathCastNodeSetToNumber) := GetProcAddress(libXmlHandle, 'xmlXPathCastNodeSetToNumber');
    Pointer(xmlXPathCastToNumber) := GetProcAddress(libXmlHandle, 'xmlXPathCastToNumber');
    Pointer(xmlXPathCastBooleanToString) := GetProcAddress(libXmlHandle, 'xmlXPathCastBooleanToString');
    Pointer(xmlXPathCastNumberToString) := GetProcAddress(libXmlHandle, 'xmlXPathCastNumberToString');
    Pointer(xmlXPathCastNodeToString) := GetProcAddress(libXmlHandle, 'xmlXPathCastNodeToString');
    Pointer(xmlXPathCastNodeSetToString) := GetProcAddress(libXmlHandle, 'xmlXPathCastNodeSetToString');
    Pointer(xmlXPathCastToString) := GetProcAddress(libXmlHandle, 'xmlXPathCastToString');
    Pointer(xmlXPathConvertBoolean) := GetProcAddress(libXmlHandle, 'xmlXPathConvertBoolean');
    Pointer(xmlXPathConvertNumber) := GetProcAddress(libXmlHandle, 'xmlXPathConvertNumber');
    Pointer(xmlXPathConvertString) := GetProcAddress(libXmlHandle, 'xmlXPathConvertString');
    Pointer(xmlXPathNewContext) := GetProcAddress(libXmlHandle, 'xmlXPathNewContext');
    Pointer(xmlXPathFreeContext) := GetProcAddress(libXmlHandle, 'xmlXPathFreeContext');
    Pointer(xmlXPathContextSetCache) := GetProcAddress(libXmlHandle, 'xmlXPathContextSetCache');
    Pointer(xmlXPathOrderDocElems) := GetProcAddress(libXmlHandle, 'xmlXPathOrderDocElems');
    Pointer(xmlXPathSetContextNode) := GetProcAddress(libXmlHandle, 'xmlXPathSetContextNode');
    Pointer(xmlXPathNodeEval) := GetProcAddress(libXmlHandle, 'xmlXPathNodeEval');
    Pointer(xmlXPathEval) := GetProcAddress(libXmlHandle, 'xmlXPathEval');
    Pointer(xmlXPathEvalExpression) := GetProcAddress(libXmlHandle, 'xmlXPathEvalExpression');
    Pointer(xmlXPathEvalPredicate) := GetProcAddress(libXmlHandle, 'xmlXPathEvalPredicate');
    Pointer(xmlXPathCompile) := GetProcAddress(libXmlHandle, 'xmlXPathCompile');
    Pointer(xmlXPathCtxtCompile) := GetProcAddress(libXmlHandle, 'xmlXPathCtxtCompile');
    Pointer(xmlXPathCompiledEval) := GetProcAddress(libXmlHandle, 'xmlXPathCompiledEval');
    Pointer(xmlXPathCompiledEvalToBoolean) := GetProcAddress(libXmlHandle, 'xmlXPathCompiledEvalToBoolean');
    Pointer(xmlXPathFreeCompExpr) := GetProcAddress(libXmlHandle, 'xmlXPathFreeCompExpr');
  {$ENDIF} (* LIBXML_XPATH_ENABLED *)
  {$if defined(LIBXML_XPATH_ENABLED) or defined(LIBXML_SCHEMAS_ENABLED)}
    Pointer(xmlXPathInit) := GetProcAddress(libXmlHandle, 'xmlXPathInit');
    Pointer(xmlXPathIsNaN) := GetProcAddress(libXmlHandle, 'xmlXPathIsNaN');
    Pointer(xmlXPathIsInf) := GetProcAddress(libXmlHandle, 'xmlXPathIsInf');
  {$ENDIF} (* LIBXML_XPATH_ENABLED or LIBXML_SCHEMAS_ENABLED*)


    { xpathInternals.inc }
  {$IFDEF LIBXML_XPATH_ENABLED}
    Pointer(xmlXPathRegisterVariableLookup) := GetProcAddress(libXmlHandle, 'xmlXPathRegisterVariableLookup');
    Pointer(xmlXPathRegisterFuncLookup) := GetProcAddress(libXmlHandle, 'xmlXPathRegisterFuncLookup');
  //procedure __xmlXPatherror(ctxt
    Pointer(xmlXPathErr) := GetProcAddress(libXmlHandle, 'xmlXPathErr');
  {$IFDEF LIBXML_DEBUG_ENABLED}
    Pointer(xmlXPathDebugDumpObject) := GetProcAddress(libXmlHandle, 'xmlXPathDebugDumpObject');
    Pointer(xmlXPathDebugDumpCompExpr) := GetProcAddress(libXmlHandle, 'xmlXPathDebugDumpCompExpr');
  {$ENDIF}
    Pointer(xmlXPathNodeSetContains) := GetProcAddress(libXmlHandle, 'xmlXPathNodeSetContains');
    Pointer(xmlXPathDifference) := GetProcAddress(libXmlHandle, 'xmlXPathDifference');
    Pointer(xmlXPathIntersection) := GetProcAddress(libXmlHandle, 'xmlXPathIntersection');
    Pointer(xmlXPathDistinctSorted) := GetProcAddress(libXmlHandle, 'xmlXPathDistinctSorted');
    Pointer(xmlXPathDistinct) := GetProcAddress(libXmlHandle, 'xmlXPathDistinct');
    Pointer(xmlXPathHasSameNodes) := GetProcAddress(libXmlHandle, 'xmlXPathHasSameNodes');
    Pointer(xmlXPathNodeLeadingSorted) := GetProcAddress(libXmlHandle, 'xmlXPathNodeLeadingSorted');
    Pointer(xmlXPathLeadingSorted) := GetProcAddress(libXmlHandle, 'xmlXPathLeadingSorted');
    Pointer(xmlXPathNodeLeading) := GetProcAddress(libXmlHandle, 'xmlXPathNodeLeading');
    Pointer(xmlXPathLeading) := GetProcAddress(libXmlHandle, 'xmlXPathLeading');
    Pointer(xmlXPathNodeTrailingSorted) := GetProcAddress(libXmlHandle, 'xmlXPathNodeTrailingSorted');
    Pointer(xmlXPathTrailingSorted) := GetProcAddress(libXmlHandle, 'xmlXPathTrailingSorted');
    Pointer(xmlXPathNodeTrailing) := GetProcAddress(libXmlHandle, 'xmlXPathNodeTrailing');
    Pointer(xmlXPathTrailing) := GetProcAddress(libXmlHandle, 'xmlXPathTrailing');
    Pointer(xmlXPathRegisterNs) := GetProcAddress(libXmlHandle, 'xmlXPathRegisterNs');
    Pointer(xmlXPathNsLookup) := GetProcAddress(libXmlHandle, 'xmlXPathNsLookup');
    Pointer(xmlXPathRegisteredNsCleanup) := GetProcAddress(libXmlHandle, 'xmlXPathRegisteredNsCleanup');
    Pointer(xmlXPathRegisterFunc) := GetProcAddress(libXmlHandle, 'xmlXPathRegisterFunc');
    Pointer(xmlXPathRegisterFuncNS) := GetProcAddress(libXmlHandle, 'xmlXPathRegisterFuncNS');
    Pointer(xmlXPathRegisterVariable) := GetProcAddress(libXmlHandle, 'xmlXPathRegisterVariable');
    Pointer(xmlXPathRegisterVariableNS) := GetProcAddress(libXmlHandle, 'xmlXPathRegisterVariableNS');
    Pointer(xmlXPathFunctionLookup) := GetProcAddress(libXmlHandle, 'xmlXPathFunctionLookup');
    Pointer(xmlXPathFunctionLookupNS) := GetProcAddress(libXmlHandle, 'xmlXPathFunctionLookupNS');
    Pointer(xmlXPathRegisteredFuncsCleanup) := GetProcAddress(libXmlHandle, 'xmlXPathRegisteredFuncsCleanup');
    Pointer(xmlXPathVariableLookup) := GetProcAddress(libXmlHandle, 'xmlXPathVariableLookup');
    Pointer(xmlXPathVariableLookupNS) := GetProcAddress(libXmlHandle, 'xmlXPathVariableLookupNS');
    Pointer(xmlXPathRegisteredVariablesCleanup) := GetProcAddress(libXmlHandle, 'xmlXPathRegisteredVariablesCleanup');
    Pointer(xmlXPathNewParserContext) := GetProcAddress(libXmlHandle, 'xmlXPathNewParserContext');
    Pointer(xmlXPathFreeParserContext) := GetProcAddress(libXmlHandle, 'xmlXPathFreeParserContext');
    Pointer(valuePop) := GetProcAddress(libXmlHandle, 'valuePop');
    Pointer(valuePush) := GetProcAddress(libXmlHandle, 'valuePush');
    Pointer(xmlXPathNewString) := GetProcAddress(libXmlHandle, 'xmlXPathNewString');
    Pointer(xmlXPathNewCString) := GetProcAddress(libXmlHandle, 'xmlXPathNewCString');
    Pointer(xmlXPathWrapString) := GetProcAddress(libXmlHandle, 'xmlXPathWrapString');
    Pointer(xmlXPathWrapCString) := GetProcAddress(libXmlHandle, 'xmlXPathWrapCString');
    Pointer(xmlXPathNewFloat) := GetProcAddress(libXmlHandle, 'xmlXPathNewFloat');
    Pointer(xmlXPathNewBoolean) := GetProcAddress(libXmlHandle, 'xmlXPathNewBoolean');
    Pointer(xmlXPathNewNodeSet) := GetProcAddress(libXmlHandle, 'xmlXPathNewNodeSet');
    Pointer(xmlXPathNewValueTree) := GetProcAddress(libXmlHandle, 'xmlXPathNewValueTree');
    Pointer(xmlXPathNodeSetAddUnique) := GetProcAddress(libXmlHandle, 'xmlXPathNodeSetAddUnique');
    Pointer(xmlXPathNodeSetAdd) := GetProcAddress(libXmlHandle, 'xmlXPathNodeSetAdd');
    Pointer(xmlXPathNodeSetAddNs) := GetProcAddress(libXmlHandle, 'xmlXPathNodeSetAddNs');
    Pointer(xmlXPathNodeSetSort) := GetProcAddress(libXmlHandle, 'xmlXPathNodeSetSort');
    Pointer(xmlXPathRoot) := GetProcAddress(libXmlHandle, 'xmlXPathRoot');
    Pointer(xmlXPathEvalExpr) := GetProcAddress(libXmlHandle, 'xmlXPathEvalExpr');
    Pointer(xmlXPathParseName) := GetProcAddress(libXmlHandle, 'xmlXPathParseName');
    Pointer(xmlXPathParseNCName) := GetProcAddress(libXmlHandle, 'xmlXPathParseNCName');
    Pointer(xmlXPathEqualValues) := GetProcAddress(libXmlHandle, 'xmlXPathEqualValues');
    Pointer(xmlXPathNotEqualValues) := GetProcAddress(libXmlHandle, 'xmlXPathNotEqualValues');
    Pointer(xmlXPathCompareValues) := GetProcAddress(libXmlHandle, 'xmlXPathCompareValues');
    Pointer(xmlXPathValueFlipSign) := GetProcAddress(libXmlHandle, 'xmlXPathValueFlipSign');
    Pointer(xmlXPathAddValues) := GetProcAddress(libXmlHandle, 'xmlXPathAddValues');
    Pointer(xmlXPathSubValues) := GetProcAddress(libXmlHandle, 'xmlXPathSubValues');
    Pointer(xmlXPathMultValues) := GetProcAddress(libXmlHandle, 'xmlXPathMultValues');
    Pointer(xmlXPathDivValues) := GetProcAddress(libXmlHandle, 'xmlXPathDivValues');
    Pointer(xmlXPathModValues) := GetProcAddress(libXmlHandle, 'xmlXPathModValues');
    Pointer(xmlXPathIsNodeType) := GetProcAddress(libXmlHandle, 'xmlXPathIsNodeType');
    Pointer(xmlXPathNextSelf) := GetProcAddress(libXmlHandle, 'xmlXPathNextSelf');
    Pointer(xmlXPathNextChild) := GetProcAddress(libXmlHandle, 'xmlXPathNextChild');
    Pointer(xmlXPathNextDescendant) := GetProcAddress(libXmlHandle, 'xmlXPathNextDescendant');
    Pointer(xmlXPathNextDescendantOrSelf) := GetProcAddress(libXmlHandle, 'xmlXPathNextDescendantOrSelf');
    Pointer(xmlXPathNextParent) := GetProcAddress(libXmlHandle, 'xmlXPathNextParent');
    Pointer(xmlXPathNextAncestorOrSelf) := GetProcAddress(libXmlHandle, 'xmlXPathNextAncestorOrSelf');
    Pointer(xmlXPathNextFollowingSibling) := GetProcAddress(libXmlHandle, 'xmlXPathNextFollowingSibling');
    Pointer(xmlXPathNextFollowing) := GetProcAddress(libXmlHandle, 'xmlXPathNextFollowing');
    Pointer(xmlXPathNextNamespace) := GetProcAddress(libXmlHandle, 'xmlXPathNextNamespace');
    Pointer(xmlXPathNextAttribute) := GetProcAddress(libXmlHandle, 'xmlXPathNextAttribute');
    Pointer(xmlXPathNextPreceding) := GetProcAddress(libXmlHandle, 'xmlXPathNextPreceding');
    Pointer(xmlXPathNextAncestor) := GetProcAddress(libXmlHandle, 'xmlXPathNextAncestor');
    Pointer(xmlXPathNextPrecedingSibling) := GetProcAddress(libXmlHandle, 'xmlXPathNextPrecedingSibling');
    Pointer(xmlXPathLastFunction) := GetProcAddress(libXmlHandle, 'xmlXPathLastFunction');
    Pointer(xmlXPathPositionFunction) := GetProcAddress(libXmlHandle, 'xmlXPathPositionFunction');
    Pointer(xmlXPathCountFunction) := GetProcAddress(libXmlHandle, 'xmlXPathCountFunction');
    Pointer(xmlXPathIdFunction) := GetProcAddress(libXmlHandle, 'xmlXPathIdFunction');
    Pointer(xmlXPathLocalNameFunction) := GetProcAddress(libXmlHandle, 'xmlXPathLocalNameFunction');
    Pointer(xmlXPathNamespaceURIFunction) := GetProcAddress(libXmlHandle, 'xmlXPathNamespaceURIFunction');
    Pointer(xmlXPathStringFunction) := GetProcAddress(libXmlHandle, 'xmlXPathStringFunction');
    Pointer(xmlXPathStringLengthFunction) := GetProcAddress(libXmlHandle, 'xmlXPathStringLengthFunction');
    Pointer(xmlXPathConcatFunction) := GetProcAddress(libXmlHandle, 'xmlXPathConcatFunction');
    Pointer(xmlXPathContainsFunction) := GetProcAddress(libXmlHandle, 'xmlXPathContainsFunction');
    Pointer(xmlXPathStartsWithFunction) := GetProcAddress(libXmlHandle, 'xmlXPathStartsWithFunction');
    Pointer(xmlXPathSubstringFunction) := GetProcAddress(libXmlHandle, 'xmlXPathSubstringFunction');
    Pointer(xmlXPathSubstringBeforeFunction) := GetProcAddress(libXmlHandle, 'xmlXPathSubstringBeforeFunction');
    Pointer(xmlXPathSubstringAfterFunction) := GetProcAddress(libXmlHandle, 'xmlXPathSubstringAfterFunction');
    Pointer(xmlXPathNormalizeFunction) := GetProcAddress(libXmlHandle, 'xmlXPathNormalizeFunction');
    Pointer(xmlXPathTranslateFunction) := GetProcAddress(libXmlHandle, 'xmlXPathTranslateFunction');
    Pointer(xmlXPathNotFunction) := GetProcAddress(libXmlHandle, 'xmlXPathNotFunction');
    Pointer(xmlXPathTrueFunction) := GetProcAddress(libXmlHandle, 'xmlXPathTrueFunction');
    Pointer(xmlXPathFalseFunction) := GetProcAddress(libXmlHandle, 'xmlXPathFalseFunction');
    Pointer(xmlXPathLangFunction) := GetProcAddress(libXmlHandle, 'xmlXPathLangFunction');
    Pointer(xmlXPathNumberFunction) := GetProcAddress(libXmlHandle, 'xmlXPathNumberFunction');
    Pointer(xmlXPathSumFunction) := GetProcAddress(libXmlHandle, 'xmlXPathSumFunction');
    Pointer(xmlXPathFloorFunction) := GetProcAddress(libXmlHandle, 'xmlXPathFloorFunction');
    Pointer(xmlXPathCeilingFunction) := GetProcAddress(libXmlHandle, 'xmlXPathCeilingFunction');
    Pointer(xmlXPathRoundFunction) := GetProcAddress(libXmlHandle, 'xmlXPathRoundFunction');
    Pointer(xmlXPathBooleanFunction) := GetProcAddress(libXmlHandle, 'xmlXPathBooleanFunction');
    Pointer(xmlXPathNodeSetFreeNs) := GetProcAddress(libXmlHandle, 'xmlXPathNodeSetFreeNs');
  {$ENDIF} (* LIBXML_XPATH_ENABLED *)

    { xlink.inc }
  {$IFDEF LIBXML_XPTR_ENABLED}
    Pointer(xlinkGetDefaultDetect) := GetProcAddress(libXmlHandle, 'xlinkGetDefaultDetect');
    Pointer(xlinkSetDefaultDetect) := GetProcAddress(libXmlHandle, 'xlinkSetDefaultDetect');
    Pointer(xlinkGetDefaultHandler) := GetProcAddress(libXmlHandle, 'xlinkGetDefaultHandler');
    Pointer(xlinkSetDefaultHandler) := GetProcAddress(libXmlHandle, 'xlinkSetDefaultHandler');
    Pointer(xlinkIsLink) := GetProcAddress(libXmlHandle, 'xlinkIsLink');
  {$ENDIF} (* LIBXML_XPTR_ENABLED *)

    { xinclude.inc }
  {$IFDEF LIBXML_XINCLUDE_ENABLED}
    Pointer(xmlXIncludeProcess) := GetProcAddress(libXmlHandle, 'xmlXIncludeProcess');
    Pointer(xmlXIncludeProcessFlags) := GetProcAddress(libXmlHandle, 'xmlXIncludeProcessFlags');
    Pointer(xmlXIncludeProcessFlagsData) := GetProcAddress(libXmlHandle, 'xmlXIncludeProcessFlagsData');
    Pointer(xmlXIncludeProcessTreeFlagsData) := GetProcAddress(libXmlHandle, 'xmlXIncludeProcessTreeFlagsData');
    Pointer(xmlXIncludeProcessTree) := GetProcAddress(libXmlHandle, 'xmlXIncludeProcessTree');
    Pointer(xmlXIncludeProcessTreeFlags) := GetProcAddress(libXmlHandle, 'xmlXIncludeProcessTreeFlags');
    Pointer(xmlXIncludeNewContext) := GetProcAddress(libXmlHandle, 'xmlXIncludeNewContext');
    Pointer(xmlXIncludeSetFlags) := GetProcAddress(libXmlHandle, 'xmlXIncludeSetFlags');
    Pointer(xmlXIncludeFreeContext) := GetProcAddress(libXmlHandle, 'xmlXIncludeFreeContext');
    Pointer(xmlXIncludeProcessNode) := GetProcAddress(libXmlHandle, 'xmlXIncludeProcessNode');
  {$ENDIF} (* LIBXML_XINCLUDE_ENABLED *)

    { xpointer.inc }
  {$IFDEF LIBXML_XPTR_ENABLED}
    Pointer(xmlXPtrLocationSetCreate) := GetProcAddress(libXmlHandle, 'xmlXPtrLocationSetCreate');
    Pointer(xmlXPtrFreeLocationSet) := GetProcAddress(libXmlHandle, 'xmlXPtrFreeLocationSet');
    Pointer(xmlXPtrLocationSetMerge) := GetProcAddress(libXmlHandle, 'xmlXPtrLocationSetMerge');
    Pointer(xmlXPtrNewRange) := GetProcAddress(libXmlHandle, 'xmlXPtrNewRange');
    Pointer(xmlXPtrNewRangePoints) := GetProcAddress(libXmlHandle, 'xmlXPtrNewRangePoints');
    Pointer(xmlXPtrNewRangeNodePoint) := GetProcAddress(libXmlHandle, 'xmlXPtrNewRangeNodePoint');
    Pointer(xmlXPtrNewRangePointNode) := GetProcAddress(libXmlHandle, 'xmlXPtrNewRangePointNode');
    Pointer(xmlXPtrNewRangeNodes) := GetProcAddress(libXmlHandle, 'xmlXPtrNewRangeNodes');
    Pointer(xmlXPtrNewLocationSetNodes) := GetProcAddress(libXmlHandle, 'xmlXPtrNewLocationSetNodes');
    Pointer(xmlXPtrNewLocationSetNodeSet) := GetProcAddress(libXmlHandle, 'xmlXPtrNewLocationSetNodeSet');
    Pointer(xmlXPtrNewRangeNodeObject) := GetProcAddress(libXmlHandle, 'xmlXPtrNewRangeNodeObject');
    Pointer(xmlXPtrNewCollapsedRange) := GetProcAddress(libXmlHandle, 'xmlXPtrNewCollapsedRange');
    Pointer(xmlXPtrLocationSetAdd) := GetProcAddress(libXmlHandle, 'xmlXPtrLocationSetAdd');
    Pointer(xmlXPtrWrapLocationSet) := GetProcAddress(libXmlHandle, 'xmlXPtrWrapLocationSet');
    Pointer(xmlXPtrLocationSetDel) := GetProcAddress(libXmlHandle, 'xmlXPtrLocationSetDel');
    Pointer(xmlXPtrLocationSetRemove) := GetProcAddress(libXmlHandle, 'xmlXPtrLocationSetRemove');
    Pointer(xmlXPtrNewContext) := GetProcAddress(libXmlHandle, 'xmlXPtrNewContext');
    Pointer(xmlXPtrEval) := GetProcAddress(libXmlHandle, 'xmlXPtrEval');
    Pointer(xmlXPtrRangeToFunction) := GetProcAddress(libXmlHandle, 'xmlXPtrRangeToFunction');
    Pointer(xmlXPtrBuildNodeList) := GetProcAddress(libXmlHandle, 'xmlXPtrBuildNodeList');
    Pointer(xmlXPtrEvalRangePredicate) := GetProcAddress(libXmlHandle, 'xmlXPtrEvalRangePredicate');
  {$ENDIF} (* LIBXML_XPTR_ENABLED *)

    SetExceptionMask([exInvalidOp,exDenormalized,exZeroDivide,exOverflow,exUnderflow,exPrecision]);

  (*
   * overloading the memory functions
   *)
    if InstallFpcMemFunc then
      xmlMemSetup(@fpcxmlFree, @fpcxmlMalloc, @fpcxmlRealloc, @fpcxmlStrdup);

  (*
   * this initialize the library and check potential ABI mismatches
   * between the version it was compiled for and the actual shared
   * library used.
   *)
    mask:=GetExceptionMask;
    SetExceptionMask([exInvalidOp,exDenormalized,exZeroDivide,exOverflow,exUnderflow,exPrecision]);

    LIBXML_TEST_VERSION;

    SetExceptionMask(mask);

  (*
   * overloading the error functions
   *)
    //xmlSetGenericErrorFunc(nil, @fpcxmlGenericErrorHandler);
    //xmlSetStructuredErrorFunc(nil, @fpcxmlStructuredErrorHandler);
    Result := True;
  end
  else
    Result := False;
end;

procedure FreeLibXML;
begin
  if libXmlHandle = NilHandle then
    Exit;

  if xmlCleanupParser <> nil then
    xmlCleanupParser();

  FreeLibrary(libXmlHandle);
  libXmlHandle := NilHandle;

{$IFDEF NIL_FUNCVARS_ON_FREE}
  { xmlregexp.inc }
  {__emptyExp := nil;
  __forbiddenExp := nil;}

  { paserInternals.inc }
  //__xmlParserMaxDepth := nil;

  {  }
  {xmlStringComment := nil;
  xmlStringText := nil;
  xmlStringTextNoenc := nil;}

  { chvalid.inc }
  __xmlIsBaseCharGroup := nil;
  __xmlIsCharGroup := nil;
  __xmlIsCombiningGroup := nil;
  __xmlIsDigitGroup := nil;
  __xmlIsExtenderGroup := nil;
  __xmlIsIdeographicGroup := nil;
  __xmlIsPubidChar_tab := nil;

  { globals.inc }
  varxmlMalloc := nil;
  varxmlMallocAtomic := nil;
  varxmlRealloc := nil;
  varxmlFree := nil;
  varxmlMemStrdup := nil;

  { xpath.inc }
  {__xmlXPathNAN := nil;
  __xmlXPathNINF := nil;
  __xmlXPathPINF := nil;}


  { xmlversion.inc }
  xmlCheckVersion := nil;

  { catalog.inc }
{$IFDEF LIBXML_CATALOG_ENABLED}
  xmlNewCatalog := nil;
  xmlLoadACatalog := nil;
  xmlLoadSGMLSuperCatalog := nil;
  xmlConvertSGMLCatalog := nil;
  //xmlLoadACatalog
  xmlACatalogAdd := nil;
  xmlACatalogRemove := nil;
  xmlACatalogResolve := nil;
  xmlACatalogResolveSystem := nil;
  xmlACatalogResolvePublic := nil;
  xmlACatalogResolveURI := nil;
{$IFDEF LIBXML_OUTPUT_ENABLED}
  xmlACatalogDump := nil;
{$ENDIF} (* LIBXML_OUTPUT_ENABLED *)
  xmlFreeCatalog := nil;
  xmlCatalogIsEmpty := nil;
  xmlInitializeCatalog := nil;
  xmlLoadCatalog := nil;
  xmlLoadCatalogs := nil;
  xmlCatalogCleanup := nil;
{$IFDEF LIBXML_OUTPUT_ENABLED}
  xmlCatalogDump := nil;
{$ENDIF} (* LIBXML_OUTPUT_ENABLED *)
  xmlCatalogResolve := nil;
  xmlCatalogResolveSystem := nil;
  xmlCatalogResolvePublic := nil;
  xmlCatalogResolveURI := nil;
  xmlCatalogAdd := nil;
  xmlCatalogRemove := nil;
  xmlParseCatalogFile := nil;
  xmlCatalogConvert := nil;
  xmlCatalogFreeLocal := nil;
  xmlCatalogAddLocal := nil;
  xmlCatalogLocalResolve := nil;
  xmlCatalogLocalResolveURI := nil;
  xmlCatalogSetDebug := nil;
  xmlCatalogSetDefaultPrefer := nil;
  xmlCatalogSetDefaults := nil;
  xmlCatalogGetDefaults := nil;
{$ENDIF} (* LIBXML_CATALOG_ENABLED *)

  { chvalid.inc }
  xmlCharInRange := nil;
  xmlIsBaseChar := nil;
  xmlIsBlank := nil;
  xmlIsChar := nil;
  xmlIsCombining := nil;
  xmlIsDigit := nil;
  xmlIsExtender := nil;
  xmlIsIdeographic := nil;
  xmlIsPubidChar := nil;

  { dict.inc }
  xmlInitializeDict := nil;
  xmlDictCreate := nil;
  xmlDictSetLimit := nil;
  xmlDictGetUsage := nil;
  xmlDictCreateSub := nil;
  xmlDictReference := nil;
  xmlDictFree := nil;
  xmlDictLookup := nil;
  xmlDictExists := nil;
  xmlDictQLookup := nil;
  xmlDictOwns := nil;
  xmlDictSize := nil;
  xmlDictCleanup := nil;

  { encoding.inc }
  xmlInitCharEncodingHandlers := nil;
  xmlCleanupCharEncodingHandlers := nil;
  xmlRegisterCharEncodingHandler := nil;
  xmlGetCharEncodingHandler := nil;
  xmlFindCharEncodingHandler := nil;
  xmlNewCharEncodingHandler := nil;
  xmlAddEncodingAlias := nil;
  xmlDelEncodingAlias := nil;
  xmlGetEncodingAlias := nil;
  xmlCleanupEncodingAliases := nil;
  xmlParseCharEncoding := nil;
  xmlGetCharEncodingName := nil;
  xmlDetectCharEncoding := nil;
  xmlCharEncOutFunc := nil;
  xmlCharEncInFunc := nil;
  xmlCharEncFirstLine := nil;
  xmlCharEncCloseFunc := nil;
{$IFDEF LIBXML_OUTPUT_ENABLED}
  UTF8Toisolat1 := nil;
{$ENDIF} (* LIBXML_OUTPUT_ENABLED *)
  isolat1ToUTF8 := nil;

  { tree.inc }
{$IF defined(LIBXML_TREE_ENABLED) or defined(LIBXML_XPATH_ENABLED) or defined(LIBXML_SCHEMAS_ENABLED) or
  defined(LIBXML_DEBUG_ENABLED) or defined (LIBXML_HTML_ENABLED) or defined(LIBXML_SAX1_ENABLED) or
  defined(LIBXML_HTML_ENABLED) or defined(LIBXML_WRITER_ENABLED) or defined(LIBXML_DOCB_ENABLED)}
  xmlValidateNCName := nil;
{$ENDIF}
{$IF defined(LIBXML_TREE_ENABLED) or defined(LIBXML_SCHEMAS_ENABLED)}
  xmlValidateQName := nil;
  xmlValidateName := nil;
  xmlValidateNMToken := nil;
{$ENDIF}
  xmlBuildQName := nil;
  xmlSplitQName2 := nil;
  xmlSplitQName3 := nil;
  xmlSetBufferAllocationScheme := nil;
  xmlGetBufferAllocationScheme := nil;
  xmlBufferCreate := nil;
  xmlBufferCreateSize := nil;
  xmlBufferCreateStatic := nil;
  xmlBufferResize := nil;
  xmlBufferFree := nil;
  xmlBufferDump := nil;
  xmlBufferAdd := nil;
  xmlBufferAddHead := nil;
  xmlBufferCat := nil;
  xmlBufferCCat := nil;
  xmlBufferShrink := nil;
  xmlBufferGrow := nil;
  xmlBufferEmpty := nil;
  xmlBufferContent := nil;
  xmlBufferDetach := nil;
  xmlBufferSetAllocationScheme := nil;
  xmlBufferLength := nil;
  xmlCreateIntSubset := nil;
  xmlNewDtd := nil;
  xmlGetIntSubset := nil;
  xmlFreeDtd := nil;
{$IFDEF LIBXML_LEGACY_ENABLED}
  xmlNewGlobalNs := nil;
{$ENDIF} (* LIBXML_LEGACY_ENABLED *)
  xmlNewNs := nil;
  xmlFreeNs := nil;
  xmlFreeNsList := nil;
  xmlNewDoc := nil;
  xmlFreeDoc := nil;
  xmlNewDocProp := nil;
{$IF defined(LIBXML_TREE_ENABLED) or defined(LIBXML_HTML_ENABLED) or defined(LIBXML_SCHEMAS_ENABLED)}
  xmlNewProp := nil;
{$ENDIF}
  xmlNewNsProp := nil;
  xmlNewNsPropEatName := nil;
  xmlFreePropList := nil;
  xmlFreeProp := nil;
  xmlCopyProp := nil;
  xmlCopyPropList := nil;
{$IFDEF LIBXML_TREE_ENABLED}
  xmlCopyDtd := nil;
{$ENDIF} (* LIBXML_TREE_ENABLED *)
{$IF defined(LIBXML_TREE_ENABLED) or defined(LIBXML_SCHEMAS_ENABLED)}
  xmlCopyDoc := nil;
{$ENDIF} (* defined(LIBXML_TREE_ENABLED) || defined(LIBXML_SCHEMAS_ENABLED) *)
  xmlNewDocNode := nil;
  xmlNewDocNodeEatName := nil;
  xmlNewNode := nil;
  xmlNewNodeEatName := nil;
{$IF defined(LIBXML_TREE_ENABLED) or defined(LIBXML_SCHEMAS_ENABLED)}
  xmlNewChild := nil;
{$ENDIF}
  xmlNewDocText := nil;
  xmlNewText := nil;
  xmlNewDocPI := nil;
  xmlNewPI := nil;
  xmlNewDocTextLen := nil;
  xmlNewTextLen := nil;
  xmlNewDocComment := nil;
  xmlNewComment := nil;
  xmlNewCDataBlock := nil;
  xmlNewCharRef := nil;
  xmlNewReference := nil;
  xmlCopyNode := nil;
  xmlDocCopyNode := nil;
  xmlDocCopyNodeList := nil;
  xmlCopyNodeList := nil;
{$IFDEF LIBXML_TREE_ENABLED}
  xmlNewTextChild := nil;
  xmlNewDocRawNode := nil;
  xmlNewDocFragment := nil;
{$ENDIF} (* LIBXML_TREE_ENABLED *)
  xmlGetLineNo := nil;
{$IF defined(LIBXML_TREE_ENABLED) or defined(LIBXML_DEBUG_ENABLED)}
  xmlGetNodePath := nil;
{$ENDIF} (* defined(LIBXML_TREE_ENABLED) || defined(LIBXML_DEBUG_ENABLED) *)
  xmlDocGetRootElement := nil;
  xmlGetLastChild := nil;
  xmlNodeIsText := nil;
  xmlIsBlankNode := nil;
{$IF defined(LIBXML_TREE_ENABLED) or defined(LIBXML_WRITER_ENABLED)}
  xmlDocSetRootElement := nil;
{$ENDIF} (* defined(LIBXML_TREE_ENABLED) || defined(LIBXML_WRITER_ENABLED) *)
{$IFDEF LIBXML_TREE_ENABLED}
  xmlNodeSetName := nil;
{$ENDIF} (* LIBXML_TREE_ENABLED *)
  xmlAddChild := nil;
  xmlAddChildList := nil;
{$IF defined(LIBXML_TREE_ENABLED) or defined(LIBXML_WRITER_ENABLED)}
  xmlReplaceNode := nil;
{$ENDIF} (* defined(LIBXML_TREE_ENABLED) || defined(LIBXML_WRITER_ENABLED) *)
{$IF defined(LIBXML_TREE_ENABLED) or defined(LIBXML_HTML_ENABLED) or defined(LIBXML_SCHEMAS_ENABLED)}
  xmlAddPrevSibling := nil;
{$ENDIF} (* LIBXML_TREE_ENABLED || LIBXML_HTML_ENABLED || LIBXML_SCHEMAS_ENABLED *)
  xmlAddSibling := nil;
  xmlAddNextSibling := nil;
  xmlUnlinkNode := nil;
  xmlTextMerge := nil;
  xmlTextConcat := nil;
  xmlFreeNodeList := nil;
  xmlFreeNode := nil;
  xmlSetTreeDoc := nil;
  xmlSetListDoc := nil;
  xmlSearchNs := nil;
  xmlSearchNsByHref := nil;
{$IF defined(LIBXML_TREE_ENABLED) or defined(LIBXML_XPATH_ENABLED) or defined(LIBXML_SCHEMAS_ENABLED)}
  xmlGetNsList := nil;
{$ENDIF} (* defined(LIBXML_TREE_ENABLED) || defined(LIBXML_XPATH_ENABLED) *)
  xmlSetNs := nil;
  xmlCopyNamespace := nil;
  xmlCopyNamespaceList := nil;
{$IF defined(LIBXML_TREE_ENABLED) or defined(LIBXML_XINCLUDE_ENABLED) or defined(LIBXML_SCHEMAS_ENABLED) or defined(LIBXML_HTML_ENABLED)}
  xmlSetProp := nil;
  xmlSetNsProp := nil;
{$ENDIF} (* defined(LIBXML_TREE_ENABLED) || defined(LIBXML_XINCLUDE_ENABLED) || defined(LIBXML_SCHEMAS_ENABLED) || defined(LIBXML_HTML_ENABLED) *)
  xmlGetNoNsProp := nil;
  xmlGetProp := nil;
  xmlHasProp := nil;
  xmlHasNsProp := nil;
  xmlGetNsProp := nil;
  xmlStringGetNodeList := nil;
  xmlStringLenGetNodeList := nil;
  xmlNodeListGetString := nil;
{$IFDEF LIBXML_TREE_ENABLED}
  xmlNodeListGetRawString := nil;
{$ENDIF} (* LIBXML_TREE_ENABLED *)
  xmlNodeSetContent := nil;
{$IFDEF LIBXML_TREE_ENABLED}
  xmlNodeSetContentLen := nil;
{$ENDIF} (* LIBXML_TREE_ENABLED *)
  xmlNodeAddContent := nil;
  xmlNodeAddContentLen := nil;
  xmlNodeGetContent := nil;
  xmlNodeBufGetContent := nil;
  xmlNodeGetLang := nil;
  xmlNodeGetSpacePreserve := nil;
{$IFDEF LIBXML_TREE_ENABLED}
  xmlNodeSetLang := nil;
  xmlNodeSetSpacePreserve := nil;
{$ENDIF} (* LIBXML_TREE_ENABLED *)
  xmlNodeGetBase := nil;
{$IF defined(LIBXML_TREE_ENABLED) or defined(LIBXML_XINCLUDE_ENABLED)}
  xmlNodeSetBase := nil;
{$ENDIF}
  xmlRemoveProp := nil;
{$IF defined(LIBXML_TREE_ENABLED) or defined(LIBXML_SCHEMAS_ENABLED)}
  xmlUnsetNsProp := nil;
  xmlUnsetProp := nil;
{$ENDIF} (* defined(LIBXML_TREE_ENABLED) || defined(LIBXML_SCHEMAS_ENABLED) *)
{$IFDEF LIBXML_TREE_ENABLED}
  xmlReconciliateNs := nil;
{$ENDIF}
{$IFDEF LIBXML_OUTPUT_ENABLED}
  xmlDocDumpFormatMemory := nil;
  xmlDocDumpMemory := nil;
  xmlDocDumpMemoryEnc := nil;
  xmlDocDumpFormatMemoryEnc := nil;
  xmlDocFormatDump := nil;
  xmlDocDump := nil;
  xmlElemDump := nil;
  xmlSaveFormatFile := nil;
  xmlSaveFile := nil;
  xmlBufNodeDump := nil;
  xmlNodeDump := nil;
  xmlSaveFileTo := nil;
  xmlSaveFormatFileTo := nil;
  xmlNodeDumpOutput := nil;
  xmlSaveFormatFileEnc := nil;
  xmlSaveFileEnc := nil;
{$ENDIF} (* LIBXML_OUTPUT_ENABLED *)
  xmlIsXHTML := nil;
  xmlGetDocCompressMode := nil;
  xmlSetDocCompressMode := nil;
  xmlGetCompressMode := nil;
  xmlSetCompressMode := nil;
  xmlDOMWrapNewCtxt := nil;
  xmlDOMWrapFreeCtxt := nil;
  xmlDOMWrapReconcileNamespaces := nil;
  xmlDOMWrapAdoptNode := nil;
  xmlDOMWrapRemoveNode := nil;
  xmlDOMWrapCloneNode := nil;
{$IFDEF LIBXML_TREE_ENABLED}
  xmlChildElementCount := nil;
  xmlNextElementSibling := nil;
  xmlFirstElementChild := nil;
  xmlLastElementChild := nil;
  xmlPreviousElementSibling := nil;
{$ENDIF} (* LIBXML_TREE_ENABLED *)

  { list.inc }
  xmlListCreate := nil;
  xmlListDelete := nil;
  xmlListSearch := nil;
  xmlListReverseSearch := nil;
  xmlListInsert := nil;
  xmlListAppend := nil;
  xmlListRemoveFirst := nil;
  xmlListRemoveLast := nil;
  xmlListRemoveAll := nil;
  xmlListClear := nil;
  xmlListEmpty := nil;
  xmlListFront := nil;
  xmlListEnd := nil;
  xmlListSize := nil;
  xmlListPopFront := nil;
  xmlListPopBack := nil;
  xmlListPushFront := nil;
  xmlListPushBack := nil;
  xmlListReverse := nil;
  xmlListSort := nil;
  xmlListWalk := nil;
  xmlListReverseWalk := nil;
  xmlListMerge := nil;
  xmlListDup := nil;
  xmlListCopy := nil;
  xmlLinkGetData := nil;

  { entities.inc }
{$IFDEF LIBXML_LEGACY_ENABLED}
  xmlInitializePredefinedEntities := nil;
{$ENDIF} (* LIBXML_LEGACY_ENABLED *)
  xmlNewEntity := nil;
  xmlAddDocEntity := nil;
  xmlAddDtdEntity := nil;
  xmlGetPredefinedEntity := nil;
  xmlGetDocEntity := nil;
  xmlGetDtdEntity := nil;
  xmlGetParameterEntity := nil;
{$IFDEF LIBXML_LEGACY_ENABLED}
  xmlEncodeEntities := nil;
{$ENDIF} (* LIBXML_LEGACY_ENABLED *)
  xmlEncodeEntitiesReentrant := nil;
  xmlEncodeSpecialChars := nil;
  xmlCreateEntitiesTable := nil;
{$IFDEF LIBXML_TREE_ENABLED}
  xmlCopyEntitiesTable := nil;
{$ENDIF} (* LIBXML_TREE_ENABLED *)
  xmlFreeEntitiesTable := nil;
{$IFDEF LIBXML_OUTPUT_ENABLED}
  xmlDumpEntitiesTable := nil;
  xmlDumpEntityDecl := nil;
{$ENDIF} (* LIBXML_OUTPUT_ENABLED *)
{$IFDEF LIBXML_LEGACY_ENABLED}
  xmlCleanupPredefinedEntities := nil;
{$ENDIF} (* LIBXML_LEGACY_ENABLED *)

  { xmlerror.inc }
  xmlSetGenericErrorFunc := nil;
  initGenericErrorDefaultFunc := nil;
  xmlSetStructuredErrorFunc := nil;
  xmlParserError := nil;
  xmlParserWarning := nil;
  xmlParserValidityError := nil;
  xmlParserValidityWarning := nil;
  xmlParserPrintFileInfo := nil;
  xmlParserPrintFileContext := nil;
  xmlGetLastError := nil;
  xmlResetLastError := nil;
  xmlCtxtGetLastError := nil;
  xmlCtxtResetLastError := nil;
  xmlResetError := nil;
  xmlCopyError := nil;
  __xmlRaiseError := nil;
  __xmlSimpleError := nil;

  { xmlmemory.inc }
  xmlMemSetup := nil;
  xmlMemGet := nil;
  xmlGcMemSetup := nil;
  xmlGcMemGet := nil;
  xmlInitMemory := nil;
  xmlCleanupMemory := nil;
  xmlMemUsed := nil;
  xmlMemBlocks := nil;
  xmlMemDisplay := nil;
  xmlMemDisplayLast := nil;
  xmlMemShow := nil;
  xmlMemoryDump := nil;
  xmlMemMalloc := nil;
  xmlMemRealloc := nil;
  xmlMemFree := nil;
  xmlMemoryStrdup := nil;
  xmlMallocLoc := nil;
  xmlReallocLoc := nil;
  xmlMallocAtomicLoc := nil;
  xmlMemStrdupLoc := nil;

  { pattern.inc }
{$IFDEF LIBXML_PATTERN_ENABLED}
  xmlFreePattern := nil;
  xmlFreePatternList := nil;
  xmlPatterncompile := nil;
  xmlPatternMatch := nil;
  xmlPatternStreamable := nil;
  xmlPatternMaxDepth := nil;
  xmlPatternMinDepth := nil;
  xmlPatternFromRoot := nil;
  xmlPatternGetStreamCtxt := nil;
  xmlFreeStreamCtxt := nil;
  xmlStreamPushNode := nil;
  xmlStreamPush := nil;
  xmlStreamPushAttr := nil;
  xmlStreamPop := nil;
  xmlStreamWantsAnyNode := nil;
{$ENDIF} (* LIBXML_PATTERN_ENABLED *)

  { schemasInternals.inc }
{$IFDEF LIBXML_SCHEMAS_ENABLED}
  xmlSchemaFreeType := nil;
  xmlSchemaFreeWildcard := nil;
{$ENDIF} (* LIBXML_SCHEMAS_ENABLED *)

  { hash.inc }
  xmlHashCreate := nil;
  xmlHashCreateDict := nil;
  xmlHashFree := nil;
  xmlHashDefaultDeallocator := nil;
  xmlHashAddEntry := nil;
  xmlHashUpdateEntry := nil;
  xmlHashAddEntry2 := nil;
  xmlHashUpdateEntry2 := nil;
  xmlHashAddEntry3 := nil;
  xmlHashUpdateEntry3 := nil;
  xmlHashRemoveEntry := nil;
  xmlHashRemoveEntry2 := nil;
  xmlHashRemoveEntry3 := nil;
  xmlHashLookup := nil;
  xmlHashLookup2 := nil;
  xmlHashLookup3 := nil;
  xmlHashQLookup := nil;
  xmlHashQLookup2 := nil;
  xmlHashQLookup3 := nil;
  xmlHashCopy := nil;
  xmlHashSize := nil;
  xmlHashScan := nil;
  xmlHashScan3 := nil;
  xmlHashScanFull := nil;
  xmlHashScanFull3 := nil;

  { valid.inc }
  xmlAddNotationDecl := nil;
{$IFDEF LIBXML_TREE_ENABLED}
  xmlCopyNotationTable := nil;
{$ENDIF} (* LIBXML_TREE_ENABLED *)
  xmlFreeNotationTable := nil;
{$IFDEF LIBXML_OUTPUT_ENABLED}
  xmlDumpNotationDecl := nil;
  xmlDumpNotationTable := nil;
{$ENDIF} (* LIBXML_OUTPUT_ENABLED *)
  xmlNewDocElementContent := nil;
  xmlCopyDocElementContent := nil;
  xmlFreeDocElementContent := nil;
  xmlSnprintfElementContent := nil;
  xmlAddElementDecl := nil;
{$IFDEF LIBXML_TREE_ENABLED}
  xmlCopyElementTable := nil;
{$ENDIF} (* LIBXML_TREE_ENABLED *)
  xmlFreeElementTable := nil;
{$IFDEF LIBXML_OUTPUT_ENABLED}
  xmlDumpElementTable := nil;
  xmlDumpElementDecl := nil;
{$ENDIF} (* LIBXML_OUTPUT_ENABLED *)
  xmlCreateEnumeration := nil;
  xmlFreeEnumeration := nil;
{$IFDEF LIBXML_TREE_ENABLED}
  xmlCopyEnumeration := nil;
{$ENDIF} (* LIBXML_TREE_ENABLED *)
  xmlAddAttributeDecl := nil;
{$IFDEF LIBXML_TREE_ENABLED}
  xmlCopyAttributeTable := nil;
{$ENDIF} (* LIBXML_TREE_ENABLED *)
  xmlFreeAttributeTable := nil;
{$IFDEF LIBXML_OUTPUT_ENABLED}
  xmlDumpAttributeTable := nil;
  xmlDumpAttributeDecl := nil;
{$ENDIF} (* LIBXML_OUTPUT_ENABLED *)
  xmlAddID := nil;
  xmlFreeIDTable := nil;
  xmlGetID := nil;
  xmlIsID := nil;
  xmlRemoveID := nil;
  xmlAddRef := nil;
  xmlFreeRefTable := nil;
  xmlIsRef := nil;
  xmlRemoveRef := nil;
  xmlGetRefs := nil;
{$IFDEF LIBXML_VALID_ENABLED}
  xmlNewValidCtxt := nil;
  xmlFreeValidCtxt := nil;
  xmlValidateRoot := nil;
  xmlValidateElementDecl := nil;
  xmlValidNormalizeAttributeValue := nil;
  xmlValidCtxtNormalizeAttributeValue := nil;
  xmlValidateAttributeDecl := nil;
  xmlValidateAttributeValue := nil;
  xmlValidateNotationDecl := nil;
  xmlValidateDtd := nil;
  xmlValidateDtdFinal := nil;
  xmlValidateDocument := nil;
  xmlValidateElement := nil;
  xmlValidateOneElement := nil;
  xmlValidateOneAttribute := nil;
  xmlValidateOneNamespace := nil;
  xmlValidateDocumentFinal := nil;
{$ENDIF} (* LIBXML_VALID_ENABLED *)
{$IF defined(LIBXML_VALID_ENABLED) or defined(LIBXML_SCHEMAS_ENABLED)}
  xmlValidateNotationUse := nil;
{$ENDIF} (* LIBXML_VALID_ENABLED or LIBXML_SCHEMAS_ENABLED *)
  xmlIsMixedElement := nil;
  xmlGetDtdAttrDesc := nil;
  xmlGetDtdQAttrDesc := nil;
  xmlGetDtdNotationDesc := nil;
  xmlGetDtdQElementDesc := nil;
  xmlGetDtdElementDesc := nil;
{$IFDEF LIBXML_VALID_ENABLED}
  xmlValidGetPotentialChildren := nil;
  xmlValidGetValidElements := nil;
  xmlValidateNameValue := nil;
  xmlValidateNamesValue := nil;
  xmlValidateNmtokenValue := nil;
  xmlValidateNmtokensValue := nil;
{$IFDEF LIBXML_REGEXP_ENABLED}
  xmlValidBuildContentModel := nil;
  xmlValidatePushElement := nil;
  xmlValidatePushCData := nil;
  xmlValidatePopElement := nil;
{$ENDIF} (* LIBXML_REGEXP_ENABLED *)
{$ENDIF} (* LIBXML_VALID_ENABLED *)

  { libxmlparser.inc }
  xmlInitParser := nil;
  xmlCleanupParser := nil;
  xmlParserInputRead := nil;
  xmlParserInputGrow := nil;
{$IFDEF LIBXML_SAX1_ENABLED}
  xmlParseDoc := nil;
  xmlParseFile := nil;
  xmlParseMemory := nil;
{$ENDIF} (* LIBXML_SAX1_ENABLED *)
  xmlSubstituteEntitiesDefault := nil;
  xmlKeepBlanksDefault := nil;
  xmlStopParser := nil;
  xmlPedanticParserDefault := nil;
  xmlLineNumbersDefault := nil;
{$IFDEF LIBXML_SAX1_ENABLED}
  xmlRecoverDoc := nil;
  xmlRecoverMemory := nil;
  xmlRecoverFile := nil;
{$ENDIF} (* LIBXML_SAX1_ENABLED *)
  xmlParseDocument := nil;
  xmlParseExtParsedEnt := nil;
{$IFDEF LIBXML_SAX1_ENABLED}
  xmlSAXUserParseFile := nil;
  xmlSAXUserParseMemory := nil;
  xmlSAXParseDoc := nil;
  xmlSAXParseMemory := nil;
  xmlSAXParseMemoryWithData := nil;
  xmlSAXParseFile := nil;
  xmlSAXParseFileWithData := nil;
  xmlSAXParseEntity := nil;
  xmlParseEntity := nil;
{$ENDIF} (* LIBXML_SAX1_ENABLED *)
{$IFDEF LIBXML_VALID_ENABLED}
  xmlSAXParseDTD := nil;
  xmlParseDTD := nil;
  xmlIOParseDTD := nil;
{$ENDIF} (* LIBXML_VALID_ENABLE *)
{$IFDEF LIBXML_SAX1_ENABLED}
  xmlParseBalancedChunkMemory := nil;
{$ENDIF} (* LIBXML_SAX1_ENABLED *)
  xmlParseInNodeContext := nil;
{$IFDEF LIBXML_SAX1_ENABLED}
  xmlParseBalancedChunkMemoryRecover := nil;
  xmlParseExternalEntity := nil;
{$ENDIF} (* LIBXML_SAX1_ENABLED *)
  xmlParseCtxtExternalEntity := nil;
  xmlNewParserCtxt := nil;
  xmlInitParserCtxt := nil;
  xmlClearParserCtxt := nil;
  xmlFreeParserCtxt := nil;
{$IFDEF LIBXML_SAX1_ENABLED}
  xmlSetupParserForBuffer := nil;
{$ENDIF} (* LIBXML_SAX1_ENABLED *)
  xmlCreateDocParserCtxt := nil;
{$IFDEF LIBXML_LEGACY_ENABLED}
  xmlGetFeaturesList := nil;
  xmlGetFeature := nil;
  xmlSetFeature := nil;
{$ENDIF} (* LIBXML_LEGACY_ENABLED *)
{$IFDEF LIBXML_PUSH_ENABLED}
  xmlCreatePushParserCtxt := nil;
  xmlParseChunk := nil;
{$ENDIF} (* LIBXML_PUSH_ENABLED *)
  xmlCreateIOParserCtxt := nil;
  xmlNewIOInputStream := nil;
  xmlParserFindNodeInfo := nil;
  xmlInitNodeInfoSeq := nil;
  xmlClearNodeInfoSeq := nil;
  xmlParserFindNodeInfoIndex := nil;
  xmlParserAddNodeInfo := nil;
  xmlSetExternalEntityLoader := nil;
  xmlGetExternalEntityLoader := nil;
  xmlLoadExternalEntity := nil;
  xmlByteConsumed := nil;
  xmlCtxtReset := nil;
  xmlCtxtResetPush := nil;
  xmlCtxtUseOptions := nil;
  xmlReadDoc := nil;
  xmlReadFile := nil;
  xmlReadMemory := nil;
  xmlReadFd := nil;
  xmlReadIO := nil;
  xmlCtxtReadDoc := nil;
  xmlCtxtReadFile := nil;
  xmlCtxtReadMemory := nil;
  xmlCtxtReadFd := nil;
  xmlCtxtReadIO := nil;
  xmlHasFeature := nil;

  { schematron.inc }
{$IFDEF LIBXML_SCHEMATRON_ENABLED}
  xmlSchematronNewParserCtxt := nil;
  xmlSchematronNewMemParserCtxt := nil;
  xmlSchematronNewDocParserCtxt := nil;
  xmlSchematronFreeParserCtxt := nil;
  xmlSchematronParse := nil;
  xmlSchematronFree := nil;
  xmlSchematronSetValidStructuredErrors := nil;
  xmlSchematronNewValidCtxt := nil;
  xmlSchematronFreeValidCtxt := nil;
  xmlSchematronValidateDoc := nil;
{$ENDIF} (* LIBXML_SCHEMATRON_ENABLED *)

  { threads.inc }
  xmlNewMutex := nil;
  xmlMutexLock := nil;
  xmlMutexUnlock := nil;
  xmlFreeMutex := nil;
  xmlNewRMutex := nil;
  xmlRMutexLock := nil;
  xmlRMutexUnlock := nil;
  xmlFreeRMutex := nil;
  xmlInitThreads := nil;
  xmlLockLibrary := nil;
  xmlUnlockLibrary := nil;
  xmlGetThreadId := nil;
  xmlIsMainThread := nil;
  xmlCleanupThreads := nil;
  xmlGetGlobalState := nil;

  { uri.inc }
  xmlCreateURI := nil;
  xmlBuildURI := nil;
  xmlBuildRelativeURI := nil;
  xmlParseURI := nil;
  xmlParseURIRaw := nil;
  xmlParseURIReference := nil;
  xmlSaveUri := nil;
  xmlPrintURI := nil;
  xmlURIEscapeStr := nil;
  xmlURIUnescapeString := nil;
  xmlNormalizeURIPath := nil;
  xmlURIEscape := nil;
  xmlFreeURI := nil;
  xmlCanonicPath := nil;
  xmlPathToURI := nil;

  { relaxng.inc }
{$IFDEF LIBXML_SCHEMAS_ENABLED}
  xmlRelaxNGInitTypes := nil;
  xmlRelaxNGCleanupTypes := nil;
  xmlRelaxNGNewParserCtxt := nil;
  xmlRelaxNGNewMemParserCtxt := nil;
  xmlRelaxNGNewDocParserCtxt := nil;
  xmlRelaxParserSetFlag := nil;
  xmlRelaxNGFreeParserCtxt := nil;
  xmlRelaxNGSetParserErrors := nil;
  xmlRelaxNGGetParserErrors := nil;
  xmlRelaxNGSetParserStructuredErrors := nil;
  xmlRelaxNGParse := nil;
  xmlRelaxNGFree := nil;
{$IFDEF LIBXML_OUTPUT_ENABLED}
  xmlRelaxNGDump := nil;
  xmlRelaxNGDumpTree := nil;
{$ENDIF} (* LIBXML_OUTPUT_ENABLED *)
  xmlRelaxNGSetValidErrors := nil;
  xmlRelaxNGGetValidErrors := nil;
  xmlRelaxNGSetValidStructuredErrors := nil;
  xmlRelaxNGNewValidCtxt := nil;
  xmlRelaxNGFreeValidCtxt := nil;
  xmlRelaxNGValidateDoc := nil;
  xmlRelaxNGValidatePushElement := nil;
  xmlRelaxNGValidatePushCData := nil;
  xmlRelaxNGValidatePopElement := nil;
  xmlRelaxNGValidateFullElement := nil;
{$ENDIF} (* LIBXML_SCHEMAS_ENABLED *)

  { globals.inc }
  xmlInitGlobals := nil;
  xmlCleanupGlobals := nil;
  xmlParserInputBufferCreateFilenameDefault := nil;
  xmlOutputBufferCreateFilenameDefault := nil;
  xmlInitializeGlobalState := nil;
  xmlThrDefSetGenericErrorFunc := nil;
  xmlThrDefSetStructuredErrorFunc := nil;
  xmlRegisterNodeDefault := nil;
  xmlThrDefRegisterNodeDefault := nil;
  xmlDeregisterNodeDefault := nil;
  xmlThrDefDeregisterNodeDefault := nil;
  xmlThrDefOutputBufferCreateFilenameDefault := nil;
  xmlThrDefParserInputBufferCreateFilenameDefault := nil;
  __docbDefaultSAXHandler := nil;
  __htmlDefaultSAXHandler := nil;
  __xmlLastError := nil;
  __oldXMLWDcompatibility := nil;
  __xmlBufferAllocScheme := nil;
  xmlThrDefBufferAllocScheme := nil;
  __xmlDefaultBufferSize := nil;
  xmlThrDefDefaultBufferSize := nil;
  __xmlDefaultSAXHandler := nil;
  __xmlDefaultSAXLocator := nil;
  __xmlDoValidityCheckingDefaultValue := nil;
  xmlThrDefDoValidityCheckingDefaultValue := nil;
  __xmlGenericError := nil;
  __xmlStructuredError := nil;
  __xmlGenericErrorContext := nil;
  __xmlGetWarningsDefaultValue := nil;
  xmlThrDefGetWarningsDefaultValue := nil;
  __xmlIndentTreeOutput := nil;
  xmlThrDefIndentTreeOutput := nil;
  __xmlTreeIndentString := nil;
  xmlThrDefTreeIndentString := nil;
  __xmlKeepBlanksDefaultValue := nil;
  xmlThrDefKeepBlanksDefaultValue := nil;
  __xmlLineNumbersDefaultValue := nil;
  xmlThrDefLineNumbersDefaultValue := nil;
  __xmlLoadExtDtdDefaultValue := nil;
  xmlThrDefLoadExtDtdDefaultValue := nil;
  __xmlParserDebugEntities := nil;
  xmlThrDefParserDebugEntities := nil;
  __xmlParserVersion := nil;
  __xmlPedanticParserDefaultValue := nil;
  xmlThrDefPedanticParserDefaultValue := nil;
  __xmlSaveNoEmptyTags := nil;
  xmlThrDefSaveNoEmptyTags := nil;
  __xmlSubstituteEntitiesDefaultValue := nil;
  xmlThrDefSubstituteEntitiesDefaultValue := nil;
  __xmlRegisterNodeDefaultValue := nil;
  __xmlDeregisterNodeDefaultValue := nil;
  __xmlParserInputBufferCreateFilenameValue := nil;
  __xmlOutputBufferCreateFilenameValue := nil;

  { nanoftp.inc }
{$IFDEF LIBXML_FTP_ENABLED}
  xmlNanoFTPInit := nil;
  xmlNanoFTPCleanup := nil;
  xmlNanoFTPNewCtxt := nil;
  xmlNanoFTPFreeCtxt := nil;
  xmlNanoFTPConnectTo := nil;
  xmlNanoFTPOpen := nil;
  xmlNanoFTPConnect := nil;
  xmlNanoFTPClose := nil;
  xmlNanoFTPQuit := nil;
  xmlNanoFTPScanProxy := nil;
  xmlNanoFTPProxy := nil;
  xmlNanoFTPUpdateURL := nil;
  xmlNanoFTPGetResponse := nil;
  xmlNanoFTPCheckResponse := nil;
  xmlNanoFTPCwd := nil;
  xmlNanoFTPDele := nil;
  xmlNanoFTPGetConnection := nil;
  xmlNanoFTPCloseConnection := nil;
  xmlNanoFTPList := nil;
  xmlNanoFTPGetSocket := nil;
  xmlNanoFTPGet := nil;
  xmlNanoFTPRead := nil;
{$ENDIF} (* LIBXML_FTP_ENABLED *)

  { nanohttp.inc }
{$IFDEF LIBXML_HTTP_ENABLED}
  xmlNanoHTTPInit := nil;
  xmlNanoHTTPCleanup := nil;
  xmlNanoHTTPScanProxy := nil;
  xmlNanoHTTPFetch := nil;
  xmlNanoHTTPMethod := nil;
  xmlNanoHTTPMethodRedir := nil;
  xmlNanoHTTPOpen := nil;
  xmlNanoHTTPOpenRedir := nil;
  xmlNanoHTTPReturnCode := nil;
  xmlNanoHTTPAuthHeader := nil;
  xmlNanoHTTPRedir := nil;
  xmlNanoHTTPContentLength := nil;
  xmlNanoHTTPEncoding := nil;
  xmlNanoHTTPMimeType := nil;
  xmlNanoHTTPRead := nil;
{$IFDEF LIBXML_OUTPUT_ENABLED}
  xmlNanoHTTPSave := nil;
{$ENDIF} (* LIBXML_OUTPUT_ENABLED *)
  xmlNanoHTTPClose := nil;
{$ENDIF} (* LIBXML_HTTP_ENABLED *)

  { SAX.inc }
{$IFDEF LIBXML_LEGACY_ENABLED}
  getPublicId := nil;
  getSystemId := nil;
  setDocumentLocator := nil;
  getLineNumber := nil;
  getColumnNumber := nil;
  isStandalone := nil;
  hasInternalSubset := nil;
  hasExternalSubset := nil;
  internalSubset := nil;
  externalSubset := nil;
  getEntity := nil;
  getParameterEntity := nil;
  resolveEntity := nil;
  entityDecl := nil;
  attributeDecl := nil;
  elementDecl := nil;
  notationDecl := nil;
  unparsedEntityDecl := nil;
  startDocument := nil;
  endDocument := nil;
  attribute := nil;
  startElement := nil;
  endElement := nil;
  reference := nil;
  characters := nil;
  ignorableWhitespace := nil;
  processingInstruction := nil;
  globalNamespace := nil;
  setNamespace := nil;
  getNamespace := nil;
  checkNamespace := nil;
  namespaceDecl := nil;
  comment := nil;
  cdataBlock := nil;
{$IFDEF LIBXML_SAX1_ENABLED}
  initxmlDefaultSAXHandler := nil;
{$IFDEF LIBXML_HTML_ENABLED}
  inithtmlDefaultSAXHandler := nil;
{$ENDIF}
{$IFDEF LIBXML_DOCB_ENABLED}
  initdocbDefaultSAXHandler := nil;
{$ENDIF}
{$ENDIF} (* LIBXML_SAX1_ENABLED *)
{$ENDIF} (* LIBXML_LEGACY_ENABLED *)

  { SAX2.inc }
  xmlSAX2GetPublicId := nil;
  xmlSAX2GetSystemId := nil;
  xmlSAX2SetDocumentLocator := nil;
  xmlSAX2GetLineNumber := nil;
  xmlSAX2GetColumnNumber := nil;
  xmlSAX2IsStandalone := nil;
  xmlSAX2HasInternalSubset := nil;
  xmlSAX2HasExternalSubset := nil;
  xmlSAX2InternalSubset := nil;
  xmlSAX2ExternalSubset := nil;
  xmlSAX2GetEntity := nil;
  xmlSAX2GetParameterEntity := nil;
  xmlSAX2ResolveEntity := nil;
  xmlSAX2EntityDecl := nil;
  xmlSAX2AttributeDecl := nil;
  xmlSAX2ElementDecl := nil;
  xmlSAX2NotationDecl := nil;
  xmlSAX2UnparsedEntityDecl := nil;
  xmlSAX2StartDocument := nil;
  xmlSAX2EndDocument := nil;
{$IF defined(LIBXML_SAX1_ENABLED) or defined(LIBXML_HTML_ENABLED) or defined(LIBXML_WRITER_ENABLED) or defined(LIBXML_DOCB_ENABLED)}
  xmlSAX2StartElement := nil;
  xmlSAX2EndElement := nil;
{$ENDIF} (* LIBXML_SAX1_ENABLED or LIBXML_HTML_ENABLED *)
  xmlSAX2StartElementNs := nil;
  xmlSAX2EndElementNs := nil;
  xmlSAX2Reference := nil;
  xmlSAX2Characters := nil;
  xmlSAX2IgnorableWhitespace := nil;
  xmlSAX2ProcessingInstruction := nil;
  xmlSAX2Comment := nil;
  xmlSAX2CDataBlock := nil;
{$IFDEF LIBXML_SAX1_ENABLED}
  xmlSAXDefaultVersion := nil;
{$ENDIF} (* LIBXML_SAX1_ENABLED *)
  xmlSAXVersion := nil;
  xmlSAX2InitDefaultSAXHandler := nil;
{$IFDEF LIBXML_HTML_ENABLED}
  xmlSAX2InitHtmlDefaultSAXHandler := nil;
  htmlDefaultSAXHandlerInit := nil;
{$ENDIF}
{$IFDEF LIBXML_DOCB_ENABLED}
  xmlSAX2InitDocbDefaultSAXHandler := nil;
  docbDefaultSAXHandlerInit := nil;
{$ENDIF}
  xmlDefaultSAXHandlerInit := nil;

  { HTMLtree.inc }
{$IFDEF LIBXML_HTML_ENABLED}
  htmlNewDoc := nil;
  htmlNewDocNoDtD := nil;
  htmlGetMetaEncoding := nil;
  htmlSetMetaEncoding := nil;
{$IFDEF LIBXML_OUTPUT_ENABLED}
  htmlDocDumpMemory := nil;
  htmlDocDumpMemoryFormat := nil;
  htmlDocDump := nil;
  htmlSaveFile := nil;
  htmlNodeDump := nil;
  htmlNodeDumpFile := nil;
  htmlNodeDumpFileFormat := nil;
  htmlSaveFileEnc := nil;
  htmlSaveFileFormat := nil;
  htmlNodeDumpFormatOutput := nil;
  htmlDocContentDumpOutput := nil;
  htmlDocContentDumpFormatOutput := nil;
  htmlNodeDumpOutput := nil;
{$ENDIF} (* LIBXML_OUTPUT_ENABLED *)
  htmlIsBooleanAttr := nil;
{$ENDIF} (* LIBXML_HTML_ENABLED *)

  { HTMLparser.inc }
{$IFDEF LIBXML_HTML_ENABLED}
  htmlTagLookup := nil;
  htmlEntityLookup := nil;
  htmlEntityValueLookup := nil;
  htmlIsAutoClosed := nil;
  htmlAutoCloseTag := nil;
  htmlParseEntityRef := nil;
  htmlParseCharRef := nil;
  htmlParseElement := nil;
  htmlNewParserCtxt := nil;
  htmlCreateMemoryParserCtxt := nil;
  htmlParseDocument := nil;
  htmlSAXParseDoc := nil;
  htmlParseDoc := nil;
  htmlSAXParseFile := nil;
  htmlParseFile := nil;
  UTF8ToHtml := nil;
  htmlEncodeEntities := nil;
  htmlIsScriptAttribute := nil;
  htmlHandleOmittedElem := nil;
{$IFDEF LIBXML_PUSH_ENABLED}
  htmlCreatePushParserCtxt := nil;
  htmlParseChunk := nil;
{$ENDIF} (* LIBXML_PUSH_ENABLED *)
  htmlFreeParserCtxt := nil;
{$ENDIF} (* LIBXML_HTML_ENABLED *)

  { xmlautomata.inc }
{$IFDEF LIBXML_REGEXP_ENABLED}
{$IFDEF LIBXML_AUTOMATA_ENABLED}
  xmlNewAutomata := nil;
  xmlFreeAutomata := nil;
  xmlAutomataGetInitState := nil;
  xmlAutomataSetFinalState := nil;
  xmlAutomataNewState := nil;
  xmlAutomataNewTransition := nil;
  xmlAutomataNewTransition2 := nil;
  xmlAutomataNewNegTrans := nil;
  xmlAutomataNewCountTrans := nil;
  xmlAutomataNewCountTrans2 := nil;
  xmlAutomataNewOnceTrans := nil;
  xmlAutomataNewOnceTrans2 := nil;
  xmlAutomataNewAllTrans := nil;
  xmlAutomataNewEpsilon := nil;
  xmlAutomataNewCountedTrans := nil;
  xmlAutomataNewCounterTrans := nil;
  xmlAutomataNewCounter := nil;
  xmlAutomataCompile := nil;
  xmlAutomataIsDeterminist := nil;
{$ENDIF} (* LIBXML_AUTOMATA_ENABLED *)
{$ENDIF} (* LIBXML_REGEXP_ENABLED *)

  { xmlIO.inc }
  xmlCleanupInputCallbacks := nil;
  xmlPopInputCallbacks := nil;
  xmlRegisterDefaultInputCallbacks := nil;
  xmlAllocParserInputBuffer := nil;
  xmlParserInputBufferCreateFilename := nil;
  xmlParserInputBufferCreateFile := nil;
  xmlParserInputBufferCreateFd := nil;
  xmlParserInputBufferCreateMem := nil;
  xmlParserInputBufferCreateStatic := nil;
  xmlParserInputBufferCreateIO := nil;
  xmlParserInputBufferRead := nil;
  xmlParserInputBufferGrow := nil;
  xmlParserInputBufferPush := nil;
  xmlFreeParserInputBuffer := nil;
  xmlParserGetDirectory := nil;
  xmlRegisterInputCallbacks := nil;
  __xmlParserInputBufferCreateFilename := nil;
{$IFDEF LIBXML_OUTPUT_ENABLED}
  xmlCleanupOutputCallbacks := nil;
  xmlRegisterDefaultOutputCallbacks := nil;
  xmlAllocOutputBuffer := nil;
  xmlOutputBufferCreateFilename := nil;
  xmlOutputBufferCreateFile := nil;
  xmlOutputBufferCreateBuffer := nil;
  xmlOutputBufferCreateFd := nil;
  xmlOutputBufferCreateIO := nil;
  xmlOutputBufferGetContent := nil;
  xmlOutputBufferGetSize := nil;
  xmlOutputBufferWrite := nil;
  xmlOutputBufferWriteString := nil;
  xmlOutputBufferWriteEscape := nil;
  xmlOutputBufferFlush := nil;
  xmlOutputBufferClose := nil;
  xmlRegisterOutputCallbacks := nil;
  __xmlOutputBufferCreateFilename := nil;
{$IFDEF LIBXML_HTTP_ENABLED}
  xmlRegisterHTTPPostCallbacks := nil;
{$ENDIF} (* LIBXML_HTTP_ENABLED *)
{$ENDIF} (* LIBXML_OUTPUT_ENABLED *)
  xmlCheckHTTPInput := nil;
  xmlNoNetExternalEntityLoader := nil;
  xmlCheckFilename := nil;
  xmlFileMatch := nil;
  xmlFileOpen := nil;
  xmlFileRead := nil;
  xmlFileClose := nil;
{$IFDEF LIBXML_HTTP_ENABLED}
  xmlIOHTTPMatch := nil;
  xmlIOHTTPOpen := nil;
{$IFDEF LIBXML_OUTPUT_ENABLED}
  xmlIOHTTPOpenW := nil;
{$ENDIF} (* LIBXML_OUTPUT_ENABLED *)
  xmlIOHTTPRead := nil;
  xmlIOHTTPClose := nil;
{$ENDIF} (* LIBXML_HTTP_ENABLED *)
{$IFDEF LIBXML_FTP_ENABLED}
  xmlIOFTPMatch := nil;
  xmlIOFTPOpen := nil;
  xmlIOFTPRead := nil;
  xmlIOFTPClose := nil;
{$ENDIF} (* LIBXML_FTP_ENABLED *)

  { xmlmodule.inc }
{$IFDEF LIBXML_MODULES_ENABLED}
  xmlModuleOpen := nil;
  xmlModuleSymbol := nil;
  xmlModuleClose := nil;
  xmlModuleFree := nil;
{$ENDIF} (* LIBXML_MODULES_ENABLED *)

  { xmlreader.inc }
{$IFDEF LIBXML_SCHEMAS_ENABLED}
{$IFDEF LIBXML_READER_ENABLED}
  xmlNewTextReader := nil;
  xmlNewTextReaderFilename := nil;
  xmlFreeTextReader := nil;
  xmlTextReaderSetup := nil;
  xmlTextReaderRead := nil;
{$IFDEF LIBXML_WRITER_ENABLED}
  xmlTextReaderReadInnerXml := nil;
  xmlTextReaderReadOuterXml := nil;
{$ENDIF}
  xmlTextReaderReadString := nil;
  xmlTextReaderReadAttributeValue := nil;
  xmlTextReaderAttributeCount := nil;
  xmlTextReaderDepth := nil;
  xmlTextReaderHasAttributes := nil;
  xmlTextReaderHasValue := nil;
  xmlTextReaderIsDefault := nil;
  xmlTextReaderIsEmptyElement := nil;
  xmlTextReaderNodeType := nil;
  xmlTextReaderQuoteChar := nil;
  xmlTextReaderReadState := nil;
  xmlTextReaderIsNamespaceDecl := nil;
  xmlTextReaderConstBaseUri := nil;
  xmlTextReaderConstLocalName := nil;
  xmlTextReaderConstName := nil;
  xmlTextReaderConstNamespaceUri := nil;
  xmlTextReaderConstPrefix := nil;
  xmlTextReaderConstXmlLang := nil;
  xmlTextReaderConstString := nil;
  xmlTextReaderConstValue := nil;
  xmlTextReaderBaseUri := nil;
  xmlTextReaderLocalName := nil;
  xmlTextReaderName := nil;
  xmlTextReaderNamespaceUri := nil;
  xmlTextReaderPrefix := nil;
  xmlTextReaderXmlLang := nil;
  xmlTextReaderValue := nil;
  xmlTextReaderClose := nil;
  xmlTextReaderGetAttributeNo := nil;
  xmlTextReaderGetAttribute := nil;
  xmlTextReaderGetAttributeNs := nil;
  xmlTextReaderGetRemainder := nil;
  xmlTextReaderLookupNamespace := nil;
  xmlTextReaderMoveToAttributeNo := nil;
  xmlTextReaderMoveToAttribute := nil;
  xmlTextReaderMoveToAttributeNs := nil;
  xmlTextReaderMoveToFirstAttribute := nil;
  xmlTextReaderMoveToNextAttribute := nil;
  xmlTextReaderMoveToElement := nil;
  xmlTextReaderNormalization := nil;
  xmlTextReaderConstEncoding := nil;
  xmlTextReaderSetParserProp := nil;
  xmlTextReaderGetParserProp := nil;
  xmlTextReaderCurrentNode := nil;
  xmlTextReaderGetParserLineNumber := nil;
  xmlTextReaderGetParserColumnNumber := nil;
  xmlTextReaderPreserve := nil;
{$IFDEF LIBXML_PATTERN_ENABLED}
  xmlTextReaderPreservePattern := nil;
{$ENDIF} (* LIBXML_PATTERN_ENABLED *)
  xmlTextReaderCurrentDoc := nil;
  xmlTextReaderExpand := nil;
  xmlTextReaderNext := nil;
  xmlTextReaderNextSibling := nil;
  xmlTextReaderIsValid := nil;
{$IFDEF LIBXML_SCHEMAS_ENABLED}
  xmlTextReaderRelaxNGValidate := nil;
  xmlTextReaderRelaxNGSetSchema := nil;
  xmlTextReaderSchemaValidate := nil;
  xmlTextReaderSchemaValidateCtxt := nil;
  xmlTextReaderSetSchema := nil;
{$ENDIF}
  xmlTextReaderConstXmlVersion := nil;
  xmlTextReaderStandalone := nil;
  xmlTextReaderByteConsumed := nil;
  xmlReaderWalker := nil;
  xmlReaderForDoc := nil;
  xmlReaderForFile := nil;
  xmlReaderForMemory := nil;
  xmlReaderForFd := nil;
  xmlReaderForIO := nil;
  xmlReaderNewWalker := nil;
  xmlReaderNewDoc := nil;
  xmlReaderNewFile := nil;
  xmlReaderNewMemory := nil;
  xmlReaderNewFd := nil;
  xmlReaderNewIO := nil;
  xmlTextReaderLocatorLineNumber := nil;
  xmlTextReaderLocatorBaseURI := nil;
  xmlTextReaderSetErrorHandler := nil;
  xmlTextReaderSetStructuredErrorHandler := nil;
  xmlTextReaderGetErrorHandler := nil;
{$ENDIF} (* LIBXML_READER_ENABLED *)
{$ENDIF} (* LIBXML_SCHEMAS_ENABLED *)

  { xmlregexp.inc }
{$IFDEF LIBXML_REGEXP_ENABLED}
  xmlRegexpCompile := nil;
  xmlRegFreeRegexp := nil;
  xmlRegexpExec := nil;
  xmlRegexpPrint := nil;
  xmlRegexpIsDeterminist := nil;
  xmlRegNewExecCtxt := nil;
  xmlRegFreeExecCtxt := nil;
  xmlRegExecPushString := nil;
  xmlRegExecPushString2 := nil;
  xmlRegExecNextValues := nil;
  xmlRegExecErrInfo := nil;
{$IFDEF LIBXML_EXPR_ENABLED}
  xmlExpFreeCtxt := nil;
  xmlExpNewCtxt := nil;
  xmlExpCtxtNbNodes := nil;
  xmlExpCtxtNbCons := nil;
  xmlExpFree := nil;
  xmlExpRef := nil;
  xmlExpParse := nil;
  xmlExpNewAtom := nil;
  xmlExpNewOr := nil;
  xmlExpNewSeq := nil;
  xmlExpNewRange := nil;
  xmlExpIsNillable := nil;
  xmlExpMaxToken := nil;
  xmlExpGetLanguage := nil;
  xmlExpGetStart := nil;
  xmlExpStringDerive := nil;
  xmlExpExpDerive := nil;
  xmlExpSubsume := nil;
  xmlExpDump := nil;
{$ENDIF} (* LIBXML_EXPR_ENABLED *)
{$ENDIF} (* LIBXML_REGEXP_ENABLED *)

  { xmlsave.inc }
{$IFDEF LIBXML_OUTPUT_ENABLED}
  xmlSaveToFd := nil;
  xmlSaveToFilename := nil;
  xmlSaveToBuffer := nil;
  xmlSaveToIO := nil;
  xmlSaveDoc := nil;
  xmlSaveTree := nil;
  xmlSaveFlush := nil;
  xmlSaveClose := nil;
  xmlSaveSetEscape := nil;
  xmlSaveSetAttrEscape := nil;
{$ENDIF} (* LIBXML_OUTPUT_ENABLED *)

  { xmlschemas.inc }
{$IFDEF LIBXML_SCHEMAS_ENABLED}
  xmlSchemaNewParserCtxt := nil;
  xmlSchemaNewMemParserCtxt := nil;
  xmlSchemaNewDocParserCtxt := nil;
  xmlSchemaFreeParserCtxt := nil;
  xmlSchemaSetParserErrors := nil;
  xmlSchemaSetParserStructuredErrors := nil;
  xmlSchemaGetParserErrors := nil;
  xmlSchemaIsValid := nil;
  xmlSchemaParse := nil;
  xmlSchemaFree := nil;
{$IFDEF LIBXML_OUTPUT_ENABLED}
  xmlSchemaDump := nil;
{$ENDIF} (* LIBXML_OUTPUT_ENABLED *)
  xmlSchemaSetValidErrors := nil;
  xmlSchemaSetValidStructuredErrors := nil;
  xmlSchemaGetValidErrors := nil;
  xmlSchemaSetValidOptions := nil;
  xmlSchemaValidCtxtGetOptions := nil;
  xmlSchemaNewValidCtxt := nil;
  xmlSchemaValidCtxtGetParserCtxt := nil;
  xmlSchemaFreeValidCtxt := nil;
  xmlSchemaValidateDoc := nil;
  xmlSchemaValidateOneElement := nil;
  xmlSchemaValidateStream := nil;
  xmlSchemaValidateFile := nil;
  xmlSchemaValidateSetFilename := nil;
  xmlSchemaValidateSetLocator := nil;
  xmlSchemaSAXPlug := nil;
  xmlSchemaSAXUnplug := nil;
{$ENDIF} (* LIBXML_SCHEMAS_ENABLED *)

  { xmlschemastypes.inc }
{$IFDEF LIBXML_SCHEMAS_ENABLED}
  xmlSchemaInitTypes := nil;
  xmlSchemaCleanupTypes := nil;
  xmlSchemaGetPredefinedType := nil;
  xmlSchemaValidatePredefinedType := nil;
  xmlSchemaValPredefTypeNode := nil;
  xmlSchemaValidateFacet := nil;
  xmlSchemaValidateFacetWhtsp := nil;
  xmlSchemaFreeValue := nil;
  xmlSchemaNewFacet := nil;
  xmlSchemaCheckFacet := nil;
  xmlSchemaFreeFacet := nil;
  xmlSchemaCompareValues := nil;
  xmlSchemaGetBuiltInListSimpleTypeItemType := nil;
  xmlSchemaValidateListSimpleTypeFacet := nil;
  xmlSchemaGetBuiltInType := nil;
  xmlSchemaIsBuiltInTypeFacet := nil;
  xmlSchemaCollapseString := nil;
  xmlSchemaWhiteSpaceReplace := nil;
  xmlSchemaGetFacetValueAsULong := nil;
  xmlSchemaValidateLengthFacet := nil;
  xmlSchemaValidateLengthFacetWhtsp := nil;
  xmlSchemaValPredefTypeNodeNoNorm := nil;
  xmlSchemaGetCanonValue := nil;
  xmlSchemaGetCanonValueWhtsp := nil;
  xmlSchemaValueAppend := nil;
  xmlSchemaValueGetNext := nil;
  xmlSchemaValueGetAsString := nil;
  xmlSchemaValueGetAsBoolean := nil;
  xmlSchemaNewStringValue := nil;
  xmlSchemaNewNOTATIONValue := nil;
  xmlSchemaNewQNameValue := nil;
  xmlSchemaCompareValuesWhtsp := nil;
  xmlSchemaCopyValue := nil;
  xmlSchemaGetValType := nil;
{$ENDIF} (* LIBXML_SCHEMAS_ENABLED *)

  { xmlstring.inc }
  xmlStrdup := nil;
  xmlStrndup := nil;
  xmlCharStrndup := nil;
  xmlCharStrdup := nil;
  xmlStrsub := nil;
  xmlStrchr := nil;
  xmlStrstr := nil;
  xmlStrcasestr := nil;
  xmlStrcmp := nil;
  xmlStrncmp := nil;
  xmlStrcasecmp := nil;
  xmlStrncasecmp := nil;
  xmlStrEqual := nil;
  xmlStrQEqual := nil;
  xmlStrlen := nil;
  xmlStrcat := nil;
  xmlStrncat := nil;
  xmlStrncatNew := nil;
  xmlStrPrintf := nil;
  xmlStrVPrintf := nil;
  xmlGetUTF8Char := nil;
  xmlCheckUTF8 := nil;
  xmlUTF8Strsize := nil;
  xmlUTF8Strndup := nil;
  xmlUTF8Strpos := nil;
  xmlUTF8Strloc := nil;
  xmlUTF8Strsub := nil;
  xmlUTF8Strlen := nil;
  xmlUTF8Size := nil;
  xmlUTF8Charcmp := nil;

  { xmlunicode.inc }
{$IFDEF LIBXML_UNICODE_ENABLED}
  xmlUCSIsAegeanNumbers := nil;
  xmlUCSIsAlphabeticPresentationForms := nil;
  xmlUCSIsArabic := nil;
  xmlUCSIsArabicPresentationFormsA := nil;
  xmlUCSIsArabicPresentationFormsB := nil;
  xmlUCSIsArmenian := nil;
  xmlUCSIsArrows := nil;
  xmlUCSIsBasicLatin := nil;
  xmlUCSIsBengali := nil;
  xmlUCSIsBlockElements := nil;
  xmlUCSIsBopomofo := nil;
  xmlUCSIsBopomofoExtended := nil;
  xmlUCSIsBoxDrawing := nil;
  xmlUCSIsBraillePatterns := nil;
  xmlUCSIsBuhid := nil;
  xmlUCSIsByzantineMusicalSymbols := nil;
  xmlUCSIsCJKCompatibility := nil;
  xmlUCSIsCJKCompatibilityForms := nil;
  xmlUCSIsCJKCompatibilityIdeographs := nil;
  xmlUCSIsCJKCompatibilityIdeographsSupplement := nil;
  xmlUCSIsCJKRadicalsSupplement := nil;
  xmlUCSIsCJKSymbolsandPunctuation := nil;
  xmlUCSIsCJKUnifiedIdeographs := nil;
  xmlUCSIsCJKUnifiedIdeographsExtensionA := nil;
  xmlUCSIsCJKUnifiedIdeographsExtensionB := nil;
  xmlUCSIsCherokee := nil;
  xmlUCSIsCombiningDiacriticalMarks := nil;
  xmlUCSIsCombiningDiacriticalMarksforSymbols := nil;
  xmlUCSIsCombiningHalfMarks := nil;
  xmlUCSIsCombiningMarksforSymbols := nil;
  xmlUCSIsControlPictures := nil;
  xmlUCSIsCurrencySymbols := nil;
  xmlUCSIsCypriotSyllabary := nil;
  xmlUCSIsCyrillic := nil;
  xmlUCSIsCyrillicSupplement := nil;
  xmlUCSIsDeseret := nil;
  xmlUCSIsDevanagari := nil;
  xmlUCSIsDingbats := nil;
  xmlUCSIsEnclosedAlphanumerics := nil;
  xmlUCSIsEnclosedCJKLettersandMonths := nil;
  xmlUCSIsEthiopic := nil;
  xmlUCSIsGeneralPunctuation := nil;
  xmlUCSIsGeometricShapes := nil;
  xmlUCSIsGeorgian := nil;
  xmlUCSIsGothic := nil;
  xmlUCSIsGreek := nil;
  xmlUCSIsGreekExtended := nil;
  xmlUCSIsGreekandCoptic := nil;
  xmlUCSIsGujarati := nil;
  xmlUCSIsGurmukhi := nil;
  xmlUCSIsHalfwidthandFullwidthForms := nil;
  xmlUCSIsHangulCompatibilityJamo := nil;
  xmlUCSIsHangulJamo := nil;
  xmlUCSIsHangulSyllables := nil;
  xmlUCSIsHanunoo := nil;
  xmlUCSIsHebrew := nil;
  xmlUCSIsHighPrivateUseSurrogates := nil;
  xmlUCSIsHighSurrogates := nil;
  xmlUCSIsHiragana := nil;
  xmlUCSIsIPAExtensions := nil;
  xmlUCSIsIdeographicDescriptionCharacters := nil;
  xmlUCSIsKanbun := nil;
  xmlUCSIsKangxiRadicals := nil;
  xmlUCSIsKannada := nil;
  xmlUCSIsKatakana := nil;
  xmlUCSIsKatakanaPhoneticExtensions := nil;
  xmlUCSIsKhmer := nil;
  xmlUCSIsKhmerSymbols := nil;
  xmlUCSIsLao := nil;
  xmlUCSIsLatin1Supplement := nil;
  xmlUCSIsLatinExtendedA := nil;
  xmlUCSIsLatinExtendedB := nil;
  xmlUCSIsLatinExtendedAdditional := nil;
  xmlUCSIsLetterlikeSymbols := nil;
  xmlUCSIsLimbu := nil;
  xmlUCSIsLinearBIdeograms := nil;
  xmlUCSIsLinearBSyllabary := nil;
  xmlUCSIsLowSurrogates := nil;
  xmlUCSIsMalayalam := nil;
  xmlUCSIsMathematicalAlphanumericSymbols := nil;
  xmlUCSIsMathematicalOperators := nil;
  xmlUCSIsMiscellaneousMathematicalSymbolsA := nil;
  xmlUCSIsMiscellaneousMathematicalSymbolsB := nil;
  xmlUCSIsMiscellaneousSymbols := nil;
  xmlUCSIsMiscellaneousSymbolsandArrows := nil;
  xmlUCSIsMiscellaneousTechnical := nil;
  xmlUCSIsMongolian := nil;
  xmlUCSIsMusicalSymbols := nil;
  xmlUCSIsMyanmar := nil;
  xmlUCSIsNumberForms := nil;
  xmlUCSIsOgham := nil;
  xmlUCSIsOldItalic := nil;
  xmlUCSIsOpticalCharacterRecognition := nil;
  xmlUCSIsOriya := nil;
  xmlUCSIsOsmanya := nil;
  xmlUCSIsPhoneticExtensions := nil;
  xmlUCSIsPrivateUse := nil;
  xmlUCSIsPrivateUseArea := nil;
  xmlUCSIsRunic := nil;
  xmlUCSIsShavian := nil;
  xmlUCSIsSinhala := nil;
  xmlUCSIsSmallFormVariants := nil;
  xmlUCSIsSpacingModifierLetters := nil;
  xmlUCSIsSpecials := nil;
  xmlUCSIsSuperscriptsandSubscripts := nil;
  xmlUCSIsSupplementalArrowsA := nil;
  xmlUCSIsSupplementalArrowsB := nil;
  xmlUCSIsSupplementalMathematicalOperators := nil;
  xmlUCSIsSupplementaryPrivateUseAreaA := nil;
  xmlUCSIsSupplementaryPrivateUseAreaB := nil;
  xmlUCSIsSyriac := nil;
  xmlUCSIsTagalog := nil;
  xmlUCSIsTagbanwa := nil;
  xmlUCSIsTags := nil;
  xmlUCSIsTaiLe := nil;
  xmlUCSIsTaiXuanJingSymbols := nil;
  xmlUCSIsTamil := nil;
  xmlUCSIsTelugu := nil;
  xmlUCSIsThaana := nil;
  xmlUCSIsThai := nil;
  xmlUCSIsTibetan := nil;
  xmlUCSIsUgaritic := nil;
  xmlUCSIsUnifiedCanadianAboriginalSyllabics := nil;
  xmlUCSIsVariationSelectors := nil;
  xmlUCSIsVariationSelectorsSupplement := nil;
  xmlUCSIsYiRadicals := nil;
  xmlUCSIsYiSyllables := nil;
  xmlUCSIsYijingHexagramSymbols := nil;
  xmlUCSIsBlock := nil;
  xmlUCSIsCatC := nil;
  xmlUCSIsCatCc := nil;
  xmlUCSIsCatCf := nil;
  xmlUCSIsCatCo := nil;
  xmlUCSIsCatCs := nil;
  xmlUCSIsCatL := nil;
  xmlUCSIsCatLl := nil;
  xmlUCSIsCatLm := nil;
  xmlUCSIsCatLo := nil;
  xmlUCSIsCatLt := nil;
  xmlUCSIsCatLu := nil;
  xmlUCSIsCatM := nil;
  xmlUCSIsCatMc := nil;
  xmlUCSIsCatMe := nil;
  xmlUCSIsCatMn := nil;
  xmlUCSIsCatN := nil;
  xmlUCSIsCatNd := nil;
  xmlUCSIsCatNl := nil;
  xmlUCSIsCatNo := nil;
  xmlUCSIsCatP := nil;
  xmlUCSIsCatPc := nil;
  xmlUCSIsCatPd := nil;
  xmlUCSIsCatPe := nil;
  xmlUCSIsCatPf := nil;
  xmlUCSIsCatPi := nil;
  xmlUCSIsCatPo := nil;
  xmlUCSIsCatPs := nil;
  xmlUCSIsCatS := nil;
  xmlUCSIsCatSc := nil;
  xmlUCSIsCatSk := nil;
  xmlUCSIsCatSm := nil;
  xmlUCSIsCatSo := nil;
  xmlUCSIsCatZ := nil;
  xmlUCSIsCatZl := nil;
  xmlUCSIsCatZp := nil;
  xmlUCSIsCatZs := nil;
  xmlUCSIsCat := nil;
{$ENDIF} (* LIBXML_UNICODE_ENABLED *)

  { xmlwriter.inc }
{$IFDEF LIBXML_WRITER_ENABLED}
  xmlNewTextWriter := nil;
  xmlNewTextWriterFilename := nil;
  xmlNewTextWriterMemory := nil;
  xmlNewTextWriterPushParser := nil;
  xmlNewTextWriterDoc := nil;
  xmlNewTextWriterTree := nil;
  xmlFreeTextWriter := nil;
  xmlTextWriterStartDocument := nil;
  xmlTextWriterEndDocument := nil;
  xmlTextWriterStartComment := nil;
  xmlTextWriterEndComment := nil;
  xmlTextWriterWriteFormatComment := nil;
  xmlTextWriterWriteVFormatComment := nil;
  xmlTextWriterWriteComment := nil;
  xmlTextWriterStartElement := nil;
  xmlTextWriterStartElementNS := nil;
  xmlTextWriterEndElement := nil;
  xmlTextWriterFullEndElement := nil;
  xmlTextWriterWriteFormatElement := nil;
  xmlTextWriterWriteVFormatElement := nil;
  xmlTextWriterWriteElement := nil;
  xmlTextWriterWriteFormatElementNS := nil;
  xmlTextWriterWriteVFormatElementNS := nil;
  xmlTextWriterWriteElementNS := nil;
  xmlTextWriterWriteFormatRaw := nil;
  xmlTextWriterWriteVFormatRaw := nil;
  xmlTextWriterWriteRawLen := nil;
  xmlTextWriterWriteRaw := nil;
  xmlTextWriterWriteFormatString := nil;
  xmlTextWriterWriteVFormatString := nil;
  xmlTextWriterWriteString := nil;
  xmlTextWriterWriteBase64 := nil;
  xmlTextWriterWriteBinHex := nil;
  xmlTextWriterStartAttribute := nil;
  xmlTextWriterStartAttributeNS := nil;
  xmlTextWriterEndAttribute := nil;
  xmlTextWriterWriteFormatAttribute := nil;
  xmlTextWriterWriteVFormatAttribute := nil;
  xmlTextWriterWriteAttribute := nil;
  xmlTextWriterWriteFormatAttributeNS := nil;
  xmlTextWriterWriteVFormatAttributeNS := nil;
  xmlTextWriterWriteAttributeNS := nil;
  xmlTextWriterStartPI := nil;
  xmlTextWriterEndPI := nil;
  xmlTextWriterWriteFormatPI := nil;
  xmlTextWriterWriteVFormatPI := nil;
  xmlTextWriterWritePI := nil;
  xmlTextWriterStartCDATA := nil;
  xmlTextWriterEndCDATA := nil;
  xmlTextWriterWriteFormatCDATA := nil;
  xmlTextWriterWriteVFormatCDATA := nil;
  xmlTextWriterWriteCDATA := nil;
  xmlTextWriterStartDTD := nil;
  xmlTextWriterEndDTD := nil;
  xmlTextWriterWriteFormatDTD := nil;
  xmlTextWriterWriteVFormatDTD := nil;
  xmlTextWriterWriteDTD := nil;
  xmlTextWriterStartDTDElement := nil;
  xmlTextWriterEndDTDElement := nil;
  xmlTextWriterWriteFormatDTDElement := nil;
  xmlTextWriterWriteVFormatDTDElement := nil;
  xmlTextWriterWriteDTDElement := nil;
  xmlTextWriterStartDTDAttlist := nil;
  xmlTextWriterEndDTDAttlist := nil;
  xmlTextWriterWriteFormatDTDAttlist := nil;
  xmlTextWriterWriteVFormatDTDAttlist := nil;
  xmlTextWriterWriteDTDAttlist := nil;
  xmlTextWriterStartDTDEntity := nil;
  xmlTextWriterEndDTDEntity := nil;
  xmlTextWriterWriteFormatDTDInternalEntity := nil;
  xmlTextWriterWriteVFormatDTDInternalEntity := nil;
  xmlTextWriterWriteDTDInternalEntity := nil;
  xmlTextWriterWriteDTDExternalEntity := nil;
  xmlTextWriterWriteDTDExternalEntityContents := nil;
  xmlTextWriterWriteDTDEntity := nil;
  xmlTextWriterWriteDTDNotation := nil;
  xmlTextWriterSetIndent := nil;
  xmlTextWriterSetIndentString := nil;
  xmlTextWriterSetQuoteChar := nil;
  xmlTextWriterFlush := nil;
{$ENDIF} (* LIBXML_WRITER_ENABLED *)

  { c14n.inc }
{$IFDEF LIBXML_C14N_ENABLED}
{$IFDEF LIBXML_OUTPUT_ENABLED}
  xmlC14NDocSaveTo := nil;
  xmlC14NDocDumpMemory := nil;
  xmlC14NDocSave := nil;
  xmlC14NExecute := nil;
{$ENDIF} (* LIBXML_OUTPUT_ENABLED *)
{$ENDIF} (* LIBXML_C14N_ENABLED *)

  { xpath.inc }
{$IFDEF LIBXML_XPATH_ENABLED}
  xmlXPathFreeObject := nil;
  xmlXPathNodeSetCreate := nil;
  xmlXPathFreeNodeSetList := nil;
  xmlXPathFreeNodeSet := nil;
  xmlXPathObjectCopy := nil;
  xmlXPathCmpNodes := nil;
  xmlXPathCastNumberToBoolean := nil;
  xmlXPathCastStringToBoolean := nil;
  xmlXPathCastNodeSetToBoolean := nil;
  xmlXPathCastToBoolean := nil;
  xmlXPathCastBooleanToNumber := nil;
  xmlXPathCastStringToNumber := nil;
  xmlXPathCastNodeToNumber := nil;
  xmlXPathCastNodeSetToNumber := nil;
  xmlXPathCastToNumber := nil;
  xmlXPathCastBooleanToString := nil;
  xmlXPathCastNumberToString := nil;
  xmlXPathCastNodeToString := nil;
  xmlXPathCastNodeSetToString := nil;
  xmlXPathCastToString := nil;
  xmlXPathConvertBoolean := nil;
  xmlXPathConvertNumber := nil;
  xmlXPathConvertString := nil;
  xmlXPathNewContext := nil;
  xmlXPathFreeContext := nil;
  xmlXPathContextSetCache := nil;
  xmlXPathOrderDocElems := nil;
  xmlXPathSetContextNode := nil;
  xmlXPathNodeEval := nil;
  xmlXPathEval := nil;
  xmlXPathEvalExpression := nil;
  xmlXPathEvalPredicate := nil;
  xmlXPathCompile := nil;
  xmlXPathCtxtCompile := nil;
  xmlXPathCompiledEval := nil;
  xmlXPathCompiledEvalToBoolean := nil;
  xmlXPathFreeCompExpr := nil;
{$ENDIF} (* LIBXML_XPATH_ENABLED *)
{$if defined(LIBXML_XPATH_ENABLED) or defined(LIBXML_SCHEMAS_ENABLED)}
  xmlXPathInit := nil;
  xmlXPathIsNaN := nil;
  xmlXPathIsInf := nil;
{$ENDIF} (* LIBXML_XPATH_ENABLED or LIBXML_SCHEMAS_ENABLED*)


  { xpathInternals.inc }
{$IFDEF LIBXML_XPATH_ENABLED}
  xmlXPathRegisterVariableLookup := nil;
  xmlXPathRegisterFuncLookup := nil;
//procedure __xmlXPatherror(ctxt
  xmlXPathErr := nil;
{$IFDEF LIBXML_DEBUG_ENABLED}
  xmlXPathDebugDumpObject := nil;
  xmlXPathDebugDumpCompExpr := nil;
{$ENDIF}
  xmlXPathNodeSetContains := nil;
  xmlXPathDifference := nil;
  xmlXPathIntersection := nil;
  xmlXPathDistinctSorted := nil;
  xmlXPathDistinct := nil;
  xmlXPathHasSameNodes := nil;
  xmlXPathNodeLeadingSorted := nil;
  xmlXPathLeadingSorted := nil;
  xmlXPathNodeLeading := nil;
  xmlXPathLeading := nil;
  xmlXPathNodeTrailingSorted := nil;
  xmlXPathTrailingSorted := nil;
  xmlXPathNodeTrailing := nil;
  xmlXPathTrailing := nil;
  xmlXPathRegisterNs := nil;
  xmlXPathNsLookup := nil;
  xmlXPathRegisteredNsCleanup := nil;
  xmlXPathRegisterFunc := nil;
  xmlXPathRegisterFuncNS := nil;
  xmlXPathRegisterVariable := nil;
  xmlXPathRegisterVariableNS := nil;
  xmlXPathFunctionLookup := nil;
  xmlXPathFunctionLookupNS := nil;
  xmlXPathRegisteredFuncsCleanup := nil;
  xmlXPathVariableLookup := nil;
  xmlXPathVariableLookupNS := nil;
  xmlXPathRegisteredVariablesCleanup := nil;
  xmlXPathNewParserContext := nil;
  xmlXPathFreeParserContext := nil;
  valuePop := nil;
  valuePush := nil;
  xmlXPathNewString := nil;
  xmlXPathNewCString := nil;
  xmlXPathWrapString := nil;
  xmlXPathWrapCString := nil;
  xmlXPathNewFloat := nil;
  xmlXPathNewBoolean := nil;
  xmlXPathNewNodeSet := nil;
  xmlXPathNewValueTree := nil;
  xmlXPathNodeSetAddUnique := nil;
  xmlXPathNodeSetAdd := nil;
  xmlXPathNodeSetAddNs := nil;
  xmlXPathNodeSetSort := nil;
  xmlXPathRoot := nil;
  xmlXPathEvalExpr := nil;
  xmlXPathParseName := nil;
  xmlXPathParseNCName := nil;
  xmlXPathEqualValues := nil;
  xmlXPathNotEqualValues := nil;
  xmlXPathCompareValues := nil;
  xmlXPathValueFlipSign := nil;
  xmlXPathAddValues := nil;
  xmlXPathSubValues := nil;
  xmlXPathMultValues := nil;
  xmlXPathDivValues := nil;
  xmlXPathModValues := nil;
  xmlXPathIsNodeType := nil;
  xmlXPathNextSelf := nil;
  xmlXPathNextChild := nil;
  xmlXPathNextDescendant := nil;
  xmlXPathNextDescendantOrSelf := nil;
  xmlXPathNextParent := nil;
  xmlXPathNextAncestorOrSelf := nil;
  xmlXPathNextFollowingSibling := nil;
  xmlXPathNextFollowing := nil;
  xmlXPathNextNamespace := nil;
  xmlXPathNextAttribute := nil;
  xmlXPathNextPreceding := nil;
  xmlXPathNextAncestor := nil;
  xmlXPathNextPrecedingSibling := nil;
  xmlXPathLastFunction := nil;
  xmlXPathPositionFunction := nil;
  xmlXPathCountFunction := nil;
  xmlXPathIdFunction := nil;
  xmlXPathLocalNameFunction := nil;
  xmlXPathNamespaceURIFunction := nil;
  xmlXPathStringFunction := nil;
  xmlXPathStringLengthFunction := nil;
  xmlXPathConcatFunction := nil;
  xmlXPathContainsFunction := nil;
  xmlXPathStartsWithFunction := nil;
  xmlXPathSubstringFunction := nil;
  xmlXPathSubstringBeforeFunction := nil;
  xmlXPathSubstringAfterFunction := nil;
  xmlXPathNormalizeFunction := nil;
  xmlXPathTranslateFunction := nil;
  xmlXPathNotFunction := nil;
  xmlXPathTrueFunction := nil;
  xmlXPathFalseFunction := nil;
  xmlXPathLangFunction := nil;
  xmlXPathNumberFunction := nil;
  xmlXPathSumFunction := nil;
  xmlXPathFloorFunction := nil;
  xmlXPathCeilingFunction := nil;
  xmlXPathRoundFunction := nil;
  xmlXPathBooleanFunction := nil;
  xmlXPathNodeSetFreeNs := nil;
{$ENDIF} (* LIBXML_XPATH_ENABLED *)

  { xlink.inc }
{$IFDEF LIBXML_XPTR_ENABLED}
  xlinkGetDefaultDetect := nil;
  xlinkSetDefaultDetect := nil;
  xlinkGetDefaultHandler := nil;
  xlinkSetDefaultHandler := nil;
  xlinkIsLink := nil;
{$ENDIF} (* LIBXML_XPTR_ENABLED *)

  { xinclude.inc }
{$IFDEF LIBXML_XINCLUDE_ENABLED}
  xmlXIncludeProcess := nil;
  xmlXIncludeProcessFlags := nil;
  xmlXIncludeProcessFlagsData := nil;
  xmlXIncludeProcessTreeFlagsData := nil;
  xmlXIncludeProcessTree := nil;
  xmlXIncludeProcessTreeFlags := nil;
  xmlXIncludeNewContext := nil;
  xmlXIncludeSetFlags := nil;
  xmlXIncludeFreeContext := nil;
  xmlXIncludeProcessNode := nil;
{$ENDIF} (* LIBXML_XINCLUDE_ENABLED *)

  { xpointer.inc }
{$IFDEF LIBXML_XPTR_ENABLED}
  xmlXPtrLocationSetCreate := nil;
  xmlXPtrFreeLocationSet := nil;
  xmlXPtrLocationSetMerge := nil;
  xmlXPtrNewRange := nil;
  xmlXPtrNewRangePoints := nil;
  xmlXPtrNewRangeNodePoint := nil;
  xmlXPtrNewRangePointNode := nil;
  xmlXPtrNewRangeNodes := nil;
  xmlXPtrNewLocationSetNodes := nil;
  xmlXPtrNewLocationSetNodeSet := nil;
  xmlXPtrNewRangeNodeObject := nil;
  xmlXPtrNewCollapsedRange := nil;
  xmlXPtrLocationSetAdd := nil;
  xmlXPtrWrapLocationSet := nil;
  xmlXPtrLocationSetDel := nil;
  xmlXPtrLocationSetRemove := nil;
  xmlXPtrNewContext := nil;
  xmlXPtrEval := nil;
  xmlXPtrRangeToFunction := nil;
  xmlXPtrBuildNodeList := nil;
  xmlXPtrEvalRangePredicate := nil;
{$ENDIF} (* LIBXML_XPTR_ENABLED *)
{$ENDIF} (* NIL_FUNCVARS_ON_FREE *)
end;

finalization
  FreeLibXML;

end.

