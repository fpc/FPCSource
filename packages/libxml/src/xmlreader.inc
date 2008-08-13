(*
 * Summary: the XMLReader implementation
 * Description: API of the XML streaming API based on C# interfaces.
 *
 * Copy: See Copyright for the status of this software.
 *
 * Author: Daniel Veillard
 *)

{$IFDEF LIBXML_SCHEMAS_ENABLED}
{$IFDEF LIBXML_READER_ENABLED}

{$IFDEF POINTER}
  xmlTextReaderPtr = ^xmlTextReader;
{$ENDIF}

{$IFDEF TYPE}
(**
 * xmlTextReaderMode:
 *
 * Internal state values for the reader.
 *)
  xmlTextReaderMode = (
    XML_TEXTREADER_MODE_INITIAL = 0,
    XML_TEXTREADER_MODE_INTERACTIVE = 1,
    XML_TEXTREADER_MODE_ERROR = 2,
    XML_TEXTREADER_MODE_EOF =3,
    XML_TEXTREADER_MODE_CLOSED = 4,
    XML_TEXTREADER_MODE_READING = 5
  );

(**
 * xmlParserProperties:
 *
 * Some common options to use with xmlTextReaderSetParserProp, but it
 * is better to use xmlParserOption and the xmlReaderNewxxx and
 * xmlReaderForxxx APIs now.
 *)
  xmlParserProperties = (
    XML_PARSER_LOADDTD = 1,
    XML_PARSER_DEFAULTATTRS = 2,
    XML_PARSER_VALIDATE = 3,
    XML_PARSER_SUBST_ENTITIES = 4
  );

(**
 * xmlParserSeverities:
 *
 * How severe an error callback is when the per-reader error callback API
 * is used.
 *)
  xmlParserSeverities = (
    XML_PARSER_SEVERITY_VALIDITY_WARNING = 1,
    XML_PARSER_SEVERITY_VALIDITY_ERROR = 2,
    XML_PARSER_SEVERITY_WARNING = 3,
    XML_PARSER_SEVERITY_ERROR = 4
  );

(**
 * xmlReaderTypes:
 *
 * Predefined constants for the different types of nodes.
 *)
  xmlReaderTypes = (
    XML_READER_TYPE_NONE = 0,
    XML_READER_TYPE_ELEMENT = 1,
    XML_READER_TYPE_ATTRIBUTE = 2,
    XML_READER_TYPE_TEXT = 3,
    XML_READER_TYPE_CDATA = 4,
    XML_READER_TYPE_ENTITY_REFERENCE = 5,
    XML_READER_TYPE_ENTITY = 6,
    XML_READER_TYPE_PROCESSING_INSTRUCTION = 7,
    XML_READER_TYPE_COMMENT = 8,
    XML_READER_TYPE_DOCUMENT = 9,
    XML_READER_TYPE_DOCUMENT_TYPE = 10,
    XML_READER_TYPE_DOCUMENT_FRAGMENT = 11,
    XML_READER_TYPE_NOTATION = 12,
    XML_READER_TYPE_WHITESPACE = 13,
    XML_READER_TYPE_SIGNIFICANT_WHITESPACE = 14,
    XML_READER_TYPE_END_ELEMENT = 15,
    XML_READER_TYPE_END_ENTITY = 16,
    XML_READER_TYPE_XML_DECLARATION = 17
  );

(**
 * xmlTextReader:
 *
 * Structure for an xmlReader context.
 *)
  xmlTextReader = record end;

  xmlTextReaderLocatorPtr = pointer;

  xmlTextReaderErrorFunc = procedure(arg: pointer; msg: pchar; severity: xmlParserSeverities; locator: xmlTextReaderLocatorPtr); XMLCALL;
{$ENDIF}

{$IFDEF FUNCTION}
(*
 * Constructors & Destructor
 *)
function xmlNewTextReader(input: xmlParserInputBufferPtr; URI: pchar): xmlTextReaderPtr; XMLCALL; XMLPUBFUN;
function xmlNewTextReaderFilename(URI: pchar): xmlTextReaderPtr; XMLCALL; XMLPUBFUN;
procedure xmlFreeTextReader(reader: xmlTextReaderPtr); XMLCALL; XMLPUBFUN;
function xmlTextReaderSetup(reader: xmlTextReaderPtr; input: xmlParserInputBufferPtr; URL, encoding: pchar; options: cint): cint; XMLCALL; XMLPUBFUN;

(*
 * Iterators
 *)
function xmlTextReaderRead(reader: xmlTextReaderPtr): cint; XMLCALL; XMLPUBFUN;
{$IFDEF LIBXML_WRITER_ENABLED}
function xmlTextReaderReadInnerXml(reader: xmlTextReaderPtr): xmlCharPtr; XMLCALL; XMLPUBFUN;
function xmlTextReaderReadOuterXml(reader: xmlTextReaderPtr): xmlCharPtr; XMLCALL; XMLPUBFUN;
{$ENDIF}
function xmlTextReaderReadString(reader: xmlTextReaderPtr): xmlCharPtr; XMLCALL; XMLPUBFUN;
function xmlTextReaderReadAttributeValue(reader: xmlTextReaderPtr): cint; XMLCALL; XMLPUBFUN;

(*
 * Attributes of the node
 *)
function xmlTextReaderAttributeCount(reader: xmlTextReaderPtr): cint; XMLCALL; XMLPUBFUN;
function xmlTextReaderDepth(reader: xmlTextReaderPtr): cint; XMLCALL; XMLPUBFUN;
function xmlTextReaderHasAttributes(reader: xmlTextReaderPtr): cint; XMLCALL; XMLPUBFUN;
function xmlTextReaderHasValue(reader: xmlTextReaderPtr): cint; XMLCALL; XMLPUBFUN;
function xmlTextReaderIsDefault(reader: xmlTextReaderPtr): cint; XMLCALL; XMLPUBFUN;
function xmlTextReaderIsEmptyElement(reader: xmlTextReaderPtr): cint; XMLCALL; XMLPUBFUN;
function xmlTextReaderNodeType(reader: xmlTextReaderPtr): cint; XMLCALL; XMLPUBFUN;
function xmlTextReaderQuoteChar(reader: xmlTextReaderPtr): cint; XMLCALL; XMLPUBFUN;
function xmlTextReaderReadState(reader: xmlTextReaderPtr): cint; XMLCALL; XMLPUBFUN;
function xmlTextReaderIsNamespaceDecl(reader: xmlTextReaderPtr): cint; XMLCALL; XMLPUBFUN;

function xmlTextReaderConstBaseUri(reader: xmlTextReaderPtr): xmlCharPtr; XMLCALL; XMLPUBFUN;
function xmlTextReaderConstLocalName(reader: xmlTextReaderPtr): xmlCharPtr; XMLCALL; XMLPUBFUN;
function xmlTextReaderConstName(reader: xmlTextReaderPtr): xmlCharPtr; XMLCALL; XMLPUBFUN;
function xmlTextReaderConstNamespaceUri(reader: xmlTextReaderPtr): xmlCharPtr; XMLCALL; XMLPUBFUN;
function xmlTextReaderConstPrefix(reader: xmlTextReaderPtr): xmlCharPtr; XMLCALL; XMLPUBFUN;
function xmlTextReaderConstXmlLang(reader: xmlTextReaderPtr): xmlCharPtr; XMLCALL; XMLPUBFUN;
function xmlTextReaderConstString(reader: xmlTextReaderPtr; str: xmlCharPtr): xmlCharPtr; XMLCALL; XMLPUBFUN;
function xmlTextReaderConstValue(reader: xmlTextReaderPtr): xmlCharPtr; XMLCALL; XMLPUBFUN;

(*
 * use the Const version of the routine for
 * better performance and simpler code
 *)
function xmlTextReaderBaseUri(reader: xmlTextReaderPtr): xmlCharPtr; XMLCALL; XMLPUBFUN;
function xmlTextReaderLocalName(reader: xmlTextReaderPtr): xmlCharPtr; XMLCALL; XMLPUBFUN;
function xmlTextReaderName(reader: xmlTextReaderPtr): xmlCharPtr; XMLCALL; XMLPUBFUN;
function xmlTextReaderNamespaceUri(reader: xmlTextReaderPtr): xmlCharPtr; XMLCALL; XMLPUBFUN;
function xmlTextReaderPrefix(reader: xmlTextReaderPtr): xmlCharPtr; XMLCALL; XMLPUBFUN;
function xmlTextReaderXmlLang(reader: xmlTextReaderPtr): xmlCharPtr; XMLCALL; XMLPUBFUN;
function xmlTextReaderValue(reader: xmlTextReaderPtr): xmlCharPtr; XMLCALL; XMLPUBFUN;

(*
 * Methods of the XmlTextReader
 *)
function xmlTextReaderClose(reader: xmlTextReaderPtr): cint; XMLCALL; XMLPUBFUN;
function xmlTextReaderGetAttributeNo(reader: xmlTextReaderPtr; no: cint): xmlCharPtr; XMLCALL; XMLPUBFUN;
function xmlTextReaderGetAttribute(reader: xmlTextReaderPtr; name: xmlCharPtr): xmlCharPtr; XMLCALL; XMLPUBFUN;
function xmlTextReaderGetAttributeNs(reader: xmlTextReaderPtr; localName, namespaceURI: xmlCharPtr): xmlCharPtr; XMLCALL; XMLPUBFUN;
function xmlTextReaderGetRemainder(reader: xmlTextReaderPtr): xmlParserInputBufferPtr; XMLCALL; XMLPUBFUN;
function xmlTextReaderLookupNamespace(reader: xmlTextReaderPtr; prefix: xmlCharPtr): xmlCharPtr; XMLCALL; XMLPUBFUN;
function xmlTextReaderMoveToAttributeNo(reader: xmlTextReaderPtr; no: cint): cint; XMLCALL; XMLPUBFUN;
function xmlTextReaderMoveToAttribute(reader: xmlTextReaderPtr; name: xmlCharPtr): cint; XMLCALL; XMLPUBFUN;
function xmlTextReaderMoveToAttributeNs(reader: xmlTextReaderPtr; localName, namespaceURI: xmlCharPtr): cint; XMLCALL; XMLPUBFUN;
function xmlTextReaderMoveToFirstAttribute(reader: xmlTextReaderPtr): cint; XMLCALL; XMLPUBFUN;
function xmlTextReaderMoveToNextAttribute(reader: xmlTextReaderPtr): cint; XMLCALL; XMLPUBFUN;
function xmlTextReaderMoveToElement(reader: xmlTextReaderPtr): cint; XMLCALL; XMLPUBFUN;
function xmlTextReaderNormalization(reader: xmlTextReaderPtr): cint; XMLCALL; XMLPUBFUN;
function xmlTextReaderConstEncoding(reader: xmlTextReaderPtr): xmlCharPtr; XMLCALL; XMLPUBFUN;

(*
 * Extensions
 *)
function xmlTextReaderSetParserProp(reader: xmlTextReaderPtr; prop, value: cint): cint; XMLCALL; XMLPUBFUN;
function xmlTextReaderGetParserProp(reader: xmlTextReaderPtr; prop: cint): cint; XMLCALL; XMLPUBFUN;
function xmlTextReaderCurrentNode(reader: xmlTextReaderPtr): xmlNodePtr; XMLCALL; XMLPUBFUN;
function xmlTextReaderGetParserLineNumber(reader: xmlTextReaderPtr): cint; XMLCALL; XMLPUBFUN;
function xmlTextReaderGetParserColumnNumber(reader: xmlTextReaderPtr): cint; XMLCALL; XMLPUBFUN;
function xmlTextReaderPreserve(reader: xmlTextReaderPtr): xmlNodePtr; XMLCALL; XMLPUBFUN;
{$IFDEF LIBXML_PATTERN_ENABLED}
function xmlTextReaderPreservePattern(reader: xmlTextReaderPtr; pattern: xmlCharPtr; namespaces: xmlCharPtrPtr): cint; XMLCALL; XMLPUBFUN;
{$ENDIF} (* LIBXML_PATTERN_ENABLED *)
function xmlTextReaderCurrentDoc(reader: xmlTextReaderPtr): xmlDocPtr; XMLCALL; XMLPUBFUN;
function xmlTextReaderExpand(reader: xmlTextReaderPtr): xmlNodePtr; XMLCALL; XMLPUBFUN;
function xmlTextReaderNext(reader: xmlTextReaderPtr): cint; XMLCALL; XMLPUBFUN;
function xmlTextReaderNextSibling(reader: xmlTextReaderPtr): cint; XMLCALL; XMLPUBFUN;
function xmlTextReaderIsValid(reader: xmlTextReaderPtr): cint; XMLCALL; XMLPUBFUN;
{$IFDEF LIBXML_SCHEMAS_ENABLED}

function xmlTextReaderRelaxNGValidate(reader: xmlTextReaderPtr; rng: pchar): cint; XMLCALL; XMLPUBFUN;
function xmlTextReaderRelaxNGSetSchema(reader: xmlTextReaderPtr; schema: xmlRelaxNGPtr): cint; XMLCALL; XMLPUBFUN;
function xmlTextReaderSchemaValidate(reader: xmlTextReaderPtr; xsd: pchar): cint; XMLCALL; XMLPUBFUN;
function xmlTextReaderSchemaValidateCtxt(reader: xmlTextReaderPtr; ctxt: xmlSchemaValidCtxtPtr; options: cint): cint; XMLCALL; XMLPUBFUN;
function xmlTextReaderSetSchema(reader: xmlTextReaderPtr; schema: xmlSchemaPtr): cint; XMLCALL; XMLPUBFUN;
{$ENDIF}
function xmlTextReaderConstXmlVersion(reader: xmlTextReaderPtr): xmlCharPtr; XMLCALL; XMLPUBFUN;
function xmlTextReaderStandalone(reader: xmlTextReaderPtr): cint; XMLCALL; XMLPUBFUN;

(*
 * Index lookup
 *)
function xmlTextReaderByteConsumed(reader: xmlTextReaderPtr): clong; XMLCALL; XMLPUBFUN;

(*
 * New more complete APIs for simpler creation and reuse of readers
 *)
function xmlReaderWalker(doc: xmlDocPtr): xmlTextReaderPtr; XMLCALL; XMLPUBFUN;
function xmlReaderForDoc(cur: xmlCharPtr; URL, encoding: pchar; options: cint): xmlTextReaderPtr; XMLCALL; XMLPUBFUN;
function xmlReaderForFile(filename, encoding: pchar; options: cint): xmlTextReaderPtr; XMLCALL; XMLPUBFUN;
function xmlReaderForMemory(buffer: pchar; size: cint; URL, encoding: pchar; options: cint): xmlTextReaderPtr; XMLCALL; XMLPUBFUN;
function xmlReaderForFd(fd: cint; URL, encoding: pchar; options: cint): xmlTextReaderPtr; XMLCALL; XMLPUBFUN;
function xmlReaderForIO(ioread: xmlInputReadCallback; ioclose: xmlInputCloseCallback; ioctx: pointer; URL, encoding: pchar; options: cint): xmlTextReaderPtr; XMLCALL; XMLPUBFUN;

function xmlReaderNewWalker(reader: xmlTextReaderPtr; doc: xmlDocPtr): cint; XMLCALL; XMLPUBFUN;
function xmlReaderNewDoc(reader: xmlTextReaderPtr; cur: xmlCharPtr; URL, encoding: pchar; options: cint): cint; XMLCALL; XMLPUBFUN;
function xmlReaderNewFile(reader: xmlTextReaderPtr; filename, encoding: pchar; options: cint): cint; XMLCALL; XMLPUBFUN;
function xmlReaderNewMemory(reader: xmlTextReaderPtr; buffer: pchar; size: cint; URL, encoding: pchar; options: cint): cint; XMLCALL; XMLPUBFUN;
function xmlReaderNewFd(reader: xmlTextReaderPtr; fd: cint; URL, encoding: pchar; options: cint): cint; XMLCALL; XMLPUBFUN;
function xmlReaderNewIO(reader: xmlTextReaderPtr; ioread: xmlInputReadCallback; ioclose: xmlInputCloseCallback; ioctx: pointer; URL, encoding: pchar; options: cint): cint; XMLCALL; XMLPUBFUN;

(*
 * Error handling extensions
 *)
function xmlTextReaderLocatorLineNumber(locator: xmlTextReaderLocatorPtr): cint; XMLCALL; XMLPUBFUN;
function xmlTextReaderLocatorBaseURI(locator: xmlTextReaderLocatorPtr): xmlCharPtr; XMLCALL; XMLPUBFUN;
procedure xmlTextReaderSetErrorHandler(reader: xmlTextReaderPtr; f: xmlTextReaderErrorFunc; arg: pointer); XMLCALL; XMLPUBFUN;
procedure xmlTextReaderSetStructuredErrorHandler(reader: xmlTextReaderPtr; f: xmlTextReaderErrorFunc; arg: pointer); XMLCALL; XMLPUBFUN;
procedure xmlTextReaderGetErrorHandler(reader: xmlTextReaderPtr; var f: xmlTextReaderErrorFunc; var arg: pointer); XMLCALL; XMLPUBFUN;
{$ENDIF}

{$ENDIF} (* LIBXML_READER_ENABLED *)
{$ENDIF} (* LIBXML_SCHEMAS_ENABLED *)

