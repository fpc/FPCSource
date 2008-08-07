unit libxml2;

{$mode objfpc}
{$macro on}

{$ALIGN 8}
{$MINENUMSIZE 4}

interface

uses
  ctypes;

{$IFDEF WINDOWS}
  {$DEFINE DYNLINK}
{$ENDIF}

{$IFDEF DYNLINK}
const
{$IF Defined(WINDOWS)}
  libxml2lib = 'libxml2.dll';
{$ELSEIF Defined(UNIX)}
  libxml2lib = 'libxml2.so';
{$ELSE}
  {$MESSAGE ERROR 'DYNLINK not supported'}
{$IFEND}
{$ELSE}
  {$LINKLIB libxml2}
{$ENDIF}

{$i xmlexports.inc}
{$i xmlversion.inc}

type
  iconv_t = pointer;
  PFILE = pointer;
  va_list = pointer;
  size_t = {$IF Sizeof(pointer) = 8}qword{$ELSE}longword{$IFEND};

(*
  pointer forwarders
*)
// dict.inc
  xmlDictPtr = ^xmlDict;

// encoding.inc
  xmlCharEncodingHandlerPtr = ^xmlCharEncodingHandler;

// entities.inc
  xmlEntityPtr = ^xmlEntity;
  xmlEntitiesTablePtr = ^xmlEntitiesTable;

// globals.inc
  xmlGlobalStatePtr = ^xmlGlobalState;
  xmlRegisterNodeFuncPtr = ^xmlRegisterNodeFunc;
  xmlDeregisterNodeFuncPtr = ^xmlDeregisterNodeFunc;
  xmlParserInputBufferCreateFilenameFuncPtr = ^xmlParserInputBufferCreateFilenameFunc;
  xmlOutputBufferCreateFilenameFuncPtr = ^xmlOutputBufferCreateFilenameFunc;

// hash.inc
  xmlHashTablePtr = ^xmlHashTable;

// parser.inc
  xmlParserInputPtr = ^xmlParserInput;
  xmlParserInputPtrPtr = ^xmlParserInputPtr;
  xmlParserNodeInfoPtr = ^xmlParserNodeInfo;
  xmlParserNodeInfoSeqPtr = ^xmlParserNodeInfoSeq;
  xmlParserCtxtPtr = ^xmlParserCtxt;
  xmlSAXLocatorPtr = ^xmlSAXLocator;
  xmlSAXHandlerPtr = ^xmlSAXHandler;
  xmlSAXHandlerV1Ptr = ^xmlSAXHandlerV1;

// pattern.inc
  xmlPatternPtr = ^xmlPattern;
  xmlStreamCtxtPtr = ^xmlStreamCtxt;

// schemasInternals.inc
  xmlSchemaValPtr = ^xmlSchemaVal;
  xmlSchemaValPtrPtr = ^xmlSchemaValPtr;
  xmlSchemaTypePtr = ^xmlSchemaType;
  xmlSchemaFacetPtr = ^xmlSchemaFacet;
  xmlSchemaAnnotPtr = ^xmlSchemaAnnot;
  xmlSchemaAttributePtr = ^xmlSchemaAttribute;
  xmlSchemaAttributeLinkPtr = ^xmlSchemaAttributeLink;
  xmlSchemaAttributeLinkPtrPtr = ^xmlSchemaAttributeLinkPtr;
  xmlSchemaWildcardNsPtr = ^xmlSchemaWildcardNs;
  xmlSchemaWildcardPtr = ^xmlSchemaWildcard;
  xmlSchemaAttributeGroupPtr = ^xmlSchemaAttributeGroup;
  xmlSchemaTypeLinkPtr = ^xmlSchemaTypeLink;
  xmlSchemaFacetLinkPtr = ^xmlSchemaFacetLink;
  xmlSchemaElementPtr = ^xmlSchemaElement;

// tree.inc
  xmlBufferPtr = ^xmlBuffer;
  xmlNotationPtr = ^xmlNotation;
  xmlEnumerationPtr = ^xmlEnumeration;
  xmlAttributePtr = ^xmlAttribute;
  xmlElementContentPtr = ^xmlElementContent;
  xmlElementPtr = ^xmlElement;
  xmlNsPtr = ^xmlNs;
  xmlNodePtr = ^xmlNode;
  xmlNodePtrPtr = ^xmlNodePtr;
  xmlDtdPtr = ^xmlDtd;
  xmlAttrPtr = ^xmlAttr;
  xmlIDPtr = ^xmlID;
  xmlRefPtr = ^xmlRef;
  xmlDocPtr = ^xmlDoc;
  xmlDOMWrapCtxtPtr = ^xmlDOMWrapCtxt;
  xmlBufferAllocationSchemePtr = ^xmlBufferAllocationScheme;

// list.inc
  xmlLinkPtr = ^xmlLink;
  xmlListPtr = ^xmlList;

// threads.inc
  xmlMutexPtr = ^xmlMutex;
  xmlRMutexPtr = ^xmlRMutex;

// uri.inc
  xmlURIPtr = ^xmlURI;

// relaxng.inc
  xmlRelaxNGPtr = ^xmlRelaxNG;
  xmlRelaxNGParserCtxtPtr = ^xmlRelaxNGParserCtxt;
  xmlRelaxNGValidCtxtPtr = ^xmlRelaxNGValidCtxt;

// valid.inc
  xmlValidStatePtr = ^xmlValidState;
  xmlValidCtxtPtr = ^xmlValidCtxt;
  xmlNotationTablePtr = ^xmlNotationTable;
  xmlElementTablePtr = ^xmlElementTable;
  xmlAttributeTablePtr = ^xmlAttributeTable;
  xmlIDTablePtr = ^xmlIDTable;
  xmlRefTablePtr = ^xmlRefTable;

// xmlautomata.inc
  xmlAutomataPtr = ^xmlAutomata;
  xmlAutomataStatePtr = ^xmlAutomataState;

// xmlerror.inc
  xmlErrorPtr = ^xmlError;
  xmlGenericErrorFuncPtr = ^xmlGenericErrorFunc;
  xmlStructuredErrorFuncPtr = ^xmlStructuredErrorFunc;

// xmlIO.inc
  xmlParserInputBufferPtr = ^xmlParserInputBuffer;
  xmlOutputBufferPtr = ^xmlOutputBuffer;

// xmlmodule.inc
  xmlModulePtr = ^xmlModule;

// xmlreader.inc
  xmlTextReaderPtr = ^xmlTextReader;

// xmlregexp.inc
  xmlRegexpPtr = ^xmlRegexp;
  xmlRegExecCtxtPtr = ^xmlRegExecCtxt;
  xmlExpCtxtPtr = ^xmlExpCtxt;

// xmlsave.inc
  xmlSaveCtxtPtr = ^xmlSaveCtxt;

// xmlschemas.inc
  xmlSchemaPtr = ^xmlSchema;
  xmlSchemaParserCtxtPtr = ^xmlSchemaParserCtxt;
  xmlSchemaValidCtxtPtr = ^xmlSchemaValidCtxt;
  xmlSchemaSAXPlugPtr = ^xmlSchemaSAXPlugStruct;

// xmlstring.inc
  xmlCharPtr = pchar;
  xmlCharPtrPtr = ^xmlCharPtr;

// xmlwriter.inc
  xmlTextWriterPtr = ^xmlTextWriter;

// xpath.inc
  xmlNodeSetPtr = ^xmlNodeSet;
  xmlNodeSet = record end;

(*
  include types
*)
{$DEFINE TYPE}
  {$i dict.inc}
  {$i encoding.inc}
  {$i tree.inc}
  {$i list.inc}
  {$i entities.inc}
  {$i xmlerror.inc}
  {$i xmlmemory.inc}
  {$i hash.inc}
  {$i pattern.inc}
  {$i schemasInternals.inc}
  {$i valid.inc}
  {$i parser.inc}
  {$i threads.inc}
  {$i uri.inc}
  {$i relaxng.inc}
  {$i globals.inc}
  {$i xmlautomata.inc}
  {$i xmlIO.inc}
  {$i xmlmodule.inc}
  {$i xmlreader.inc}
  {$i xmlregexp.inc}
  {$i xmlsave.inc}
  {$i xmlschemas.inc}
  {$i xmlschemastypes.inc}
  {$i xmlstring.inc}
  {$i xmlunicode.inc}
  {$i xmlwriter.inc}
  {.$i xpath.inc}
  {$i c14n.inc}
{$UNDEF TYPE}

(*
  include functions
*)
{$DEFINE FUNCTION}
  {$i dict.inc}
  {$i encoding.inc}
  {$i tree.inc}
  {$i list.inc}
  {$i entities.inc}
  {$i xmlerror.inc}
  {$i xmlmemory.inc}
  {$i pattern.inc}
  {$i schemasInternals.inc}
  {$i hash.inc}
  {$i valid.inc}
  {$i parser.inc}
  {$i threads.inc}
  {$i uri.inc}
  {$i relaxng.inc}
  {$i globals.inc}
  {$i xmlautomata.inc}
  {$i xmlIO.inc}
  {$i xmlmodule.inc}
  {$i xmlreader.inc}
  {$i xmlregexp.inc}
  {$i xmlsave.inc}
  {$i xmlschemas.inc}
  {$i xmlschemastypes.inc}
  {$i xmlstring.inc}
  {$i xmlunicode.inc}
  {$i xmlwriter.inc}
  {.$i xpath.inc}
  {$i c14n.inc}
{$UNDEF FUNCTION}

implementation

procedure fpcxmlFree(mem: pointer); XMLCALL;
begin
  FreeMem(mem);
end;

function fpcxmlMalloc(size: size_t): pointer; XMLCALL;
begin
  GetMem(Result, size);
end;

function fpcxmlRealloc(mem: pointer; size: size_t): pointer; XMLCALL;
begin
  Result := mem;
  ReallocMem(Result, size);
end;

procedure fpcxmlGenericErrorHandler(ctx: pointer; msg: pchar; args: array of const); XMLCDECL;
begin
  writeln(msg);
end;

procedure fpcxmlStructuredErrorHandler(userData: pointer; error: xmlErrorPtr); XMLCALL;
begin
  writeln('struct error');
end;


initialization
(*
 * this initialize the library and check potential ABI mismatches
 * between the version it was compiled for and the actual shared
 * library used.
 *)
  LIBXML_TEST_VERSION;

(*
 * overloading the memory functions
 *)
  xmlMemSetup(@fpcxmlFree, @fpcxmlMalloc, @fpcxmlRealloc, nil);

(*
 * overloading the error functions
 *)
  xmlSetGenericErrorFunc(nil, @fpcxmlGenericErrorHandler);
  xmlSetStructuredErrorFunc(nil, @fpcxmlStructuredErrorHandler);

finalization
(*
 * Cleanup function for the XML library.
 *)
  xmlCleanupParser();

(*
 * this is to debug memory for regression tests
 *)
  xmlMemoryDump();

end.