unit libxml2;

{$mode objfpc}
{$macro on}

interface

{$LINKLIB libxml2.so}

uses
  ctypes;

{$i xmlexports.h}
{$i xmlversion.h}

type
  iconv_t = pointer;
  va_list = pointer;
  size_t = {$IF Sizeof(pointer) = 8}qword{$ELSE}longword{$IFEND};

(*
  pointer forwarders
*)
// dict.h
  xmlDictPtr = ^xmlDict;

// encoding.h
  xmlCharEncodingHandlerPtr = ^xmlCharEncodingHandler;

// entities.h
  xmlEntityPtr = ^xmlEntity;
  xmlEntitiesTablePtr = ^xmlEntitiesTable;

// globals.h
  xmlGlobalStatePtr = ^xmlGlobalState;

// hash.h
  xmlHashTablePtr = ^xmlHashTable;

// parser.h
  xmlParserInputPtr = ^xmlParserInput;
  xmlParserInputPtrPtr = ^xmlParserInputPtr;
  xmlParserNodeInfoPtr = ^xmlParserNodeInfo;
  xmlParserNodeInfoSeqPtr = ^xmlParserNodeInfoSeq;
  xmlParserCtxtPtr = ^xmlParserCtxt;
  xmlSAXLocatorPtr = ^xmlSAXLocator;
  xmlSAXHandlerPtr = ^xmlSAXHandler;
  xmlSAXHandlerV1Ptr = ^xmlSAXHandlerV1;

// tree.h
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

// list.h
  xmlLinkPtr = ^xmlLink;
  xmlListPtr = ^xmlList;

// uri.h
  xmlURIPtr = ^xmlURI;

// relaxng.h
  xmlRelaxNGPtr = ^xmlRelaxNG;
  xmlRelaxNGParserCtxtPtr = ^xmlRelaxNGParserCtxt;
  xmlRelaxNGValidCtxtPtr = ^xmlRelaxNGValidCtxt;

// valid.h
  xmlValidStatePtr = ^xmlValidState;
  xmlValidCtxtPtr = ^xmlValidCtxt;
  xmlNotationTablePtr = ^xmlNotationTable;
  xmlElementTablePtr = ^xmlElementTable;
  xmlAttributeTablePtr = ^xmlAttributeTable;
  xmlIDTablePtr = ^xmlIDTable;
  xmlRefTablePtr = ^xmlRefTable;

// xmlautomata.h
  xmlAutomataPtr = ^xmlAutomata;
  xmlAutomataStatePtr = ^xmlAutomataState;

// xmlerror.h
  xmlErrorPtr = ^xmlError;

// xmlIO.h
  xmlParserInputBufferPtr = ^xmlParserInputBuffer;
  xmlOutputBufferPtr = ^xmlOutputBuffer;

// xmlmodule.h
  xmlModulePtr = ^xmlModule;

// xmlreader.h
  xmlTextReaderPtr = ^xmlTextReader;

// xmlregexp.h
  xmlRegexpPtr = ^xmlRegexp;
  xmlRegExecCtxtPtr = ^xmlRegExecCtxt;
  xmlExpCtxtPtr = ^xmlExpCtxt;

// xmlsave.h
  xmlSaveCtxtPtr = ^xmlSaveCtxt;

// xmlschemas.h
  xmlSchemaPtr = ^xmlSchema;
  xmlSchemaParserCtxtPtr = ^xmlSchemaParserCtxt;
  xmlSchemaValidCtxtPtr = ^xmlSchemaValidCtxt;
  xmlSchemaSAXPlugPtr = ^xmlSchemaSAXPlugStruct;

// xmlstring.h
  xmlCharPtr = pchar;
  xmlCharPtrPtr = ^xmlCharPtr;

// xmlwriter.h
  xmlTextWriterPtr = ^xmlTextWriter;

(*
  include types
*)
{$DEFINE TYPE}
  {$i dict.h}
  {$i encoding.h}
  {$i tree.h}
  {$i list.h}
  {$i entities.h}
  {$i xmlerror.h}
  {$i xmlmemory.h}
  {$i hash.h}
  {$i schemasInternals.h}
  {$i valid.h}
  {$i parser.h}
  {$i uri.h}
  {$i relaxng.h}
  {$i globals.h}
  {$i xmlautomata.h}
  {$i xmlIO.h}
  {$i xmlmodule.h}
  {$i xmlreader.h}
  {$i xmlregexp.h}
  {$i xmlsave.h}
  {$i xmlschemas.h}
  {$i xmlschemastypes.h}
  {$i xmlstring.h}
  {$i xmlunicode.h}
  {$i xmlwriter.h}
  {.$i xpath.h}
  {.$i c14n.h}
{$UNDEF TYPE}

const
{$DEFINE CONST}
  {$i dict.h}
  {$i encoding.h}
  {$i tree.h}
  {$i list.h}
  {$i entities.h}
  {$i xmlerror.h}
  {$i xmlmemory.h}
  {$i schemasInternals.h}
  {$i hash.h}
  {$i valid.h}
  {$i parser.h}
  {$i uri.h}
  {$i relaxng.h}
  {$i globals.h}
  {$i xmlautomata.h}
  {$i xmlIO.h}
  {$i xmlmodule.h}
  {$i xmlreader.h}
  {$i xmlregexp.h}
  {$i xmlsave.h}
  {$i xmlschemas.h}
  {$i xmlschemastypes.h}
  {$i xmlstring.h}
  {$i xmlunicode.h}
  {$i xmlwriter.h}
  {.$i xpath.h}
  {.$i c14n.h}
{$UNDEF CONST}

(*
  include functions
*)
{$DEFINE FUNCTION}
  {$i dict.h}
  {$i encoding.h}
  {$i tree.h}
  {$i list.h}
  {$i entities.h}
  {$i xmlerror.h}
  {$i xmlmemory.h}
  {$i schemasInternals.h}
  {$i hash.h}
  {$i valid.h}
  {$i parser.h}
  {$i uri.h}
  {$i relaxng.h}
  {$i globals.h}
  {$i xmlautomata.h}
  {$i xmlIO.h}
  {$i xmlmodule.h}
  {$i xmlreader.h}
  {$i xmlregexp.h}
  {$i xmlsave.h}
  {$i xmlschemas.h}
  {$i xmlschemastypes.h}
  {$i xmlstring.h}
  {$i xmlunicode.h}
  {$i xmlwriter.h}
  {.$i xpath.h}
  {.$i c14n.h}
{$UNDEF FUNCTION}

implementation

procedure fpcxmlFree(mem: pointer); cdecl;
begin
  FreeMem(mem);
end;

function fpcxmlMalloc(size: size_t): pointer; cdecl;
begin
  GetMem(Result, size);
end;

function fpcxmlRealloc(mem: pointer; size: size_t): pointer; cdecl;
begin
  Result := mem;
  ReallocMem(Result, size);
end;

{procedure fpcxmlErrorHandler(ctx: pointer; msg: pchar; args: array of const); cdecl;
begin
  writeln('abcd');
end;}

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
  //xmlSetGenericErrorFunc(nil, @fpcxmlErrorHandler);

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