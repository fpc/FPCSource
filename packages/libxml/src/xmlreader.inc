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

  xmlTextReaderErrorFunc = procedure(arg: pointer; msg: pchar; severity: xmlParserSeverities; locator: xmlTextReaderLocatorPtr); EXTDECL;
{$ENDIF}

{$IFDEF FUNCTION}
(*
 * Constructors & Destructor
 *)
function xmlNewTextReader(input: xmlParserInputBufferPtr; URI: pchar): xmlTextReaderPtr; EXTDECL; external xml2lib;
function xmlNewTextReaderFilename(URI: pchar): xmlTextReaderPtr; EXTDECL; external xml2lib;
procedure xmlFreeTextReader(reader: xmlTextReaderPtr); EXTDECL; external xml2lib;
function xmlTextReaderSetup(reader: xmlTextReaderPtr; input: xmlParserInputBufferPtr; URL, encoding: pchar; options: cint): cint; EXTDECL; external xml2lib;

(*
 * Iterators
 *)
function xmlTextReaderRead(reader: xmlTextReaderPtr): cint; EXTDECL; external xml2lib;
{$IFDEF LIBXML_WRITER_ENABLED}
function xmlTextReaderReadInnerXml(reader: xmlTextReaderPtr): xmlCharPtr; EXTDECL; external xml2lib;
function xmlTextReaderReadOuterXml(reader: xmlTextReaderPtr): xmlCharPtr; EXTDECL; external xml2lib;
{$ENDIF}
function xmlTextReaderReadString(reader: xmlTextReaderPtr): xmlCharPtr; EXTDECL; external xml2lib;
function xmlTextReaderReadAttributeValue(reader: xmlTextReaderPtr): cint; EXTDECL; external xml2lib;

(*
 * Attributes of the node
 *)
function xmlTextReaderAttributeCount(reader: xmlTextReaderPtr): cint; EXTDECL; external xml2lib;
function xmlTextReaderDepth(reader: xmlTextReaderPtr): cint; EXTDECL; external xml2lib;
function xmlTextReaderHasAttributes(reader: xmlTextReaderPtr): cint; EXTDECL; external xml2lib;
function xmlTextReaderHasValue(reader: xmlTextReaderPtr): cint; EXTDECL; external xml2lib;
function xmlTextReaderIsDefault(reader: xmlTextReaderPtr): cint; EXTDECL; external xml2lib;
function xmlTextReaderIsEmptyElement(reader: xmlTextReaderPtr): cint; EXTDECL; external xml2lib;
function xmlTextReaderNodeType(reader: xmlTextReaderPtr): cint; EXTDECL; external xml2lib;
function xmlTextReaderQuoteChar(reader: xmlTextReaderPtr): cint; EXTDECL; external xml2lib;
function xmlTextReaderReadState(reader: xmlTextReaderPtr): cint; EXTDECL; external xml2lib;
function xmlTextReaderIsNamespaceDecl(reader: xmlTextReaderPtr): cint; EXTDECL; external xml2lib;

function xmlTextReaderConstBaseUri(reader: xmlTextReaderPtr): xmlCharPtr; EXTDECL; external xml2lib;
function xmlTextReaderConstLocalName(reader: xmlTextReaderPtr): xmlCharPtr; EXTDECL; external xml2lib;
function xmlTextReaderConstName(reader: xmlTextReaderPtr): xmlCharPtr; EXTDECL; external xml2lib;
function xmlTextReaderConstNamespaceUri(reader: xmlTextReaderPtr): xmlCharPtr; EXTDECL; external xml2lib;
function xmlTextReaderConstPrefix(reader: xmlTextReaderPtr): xmlCharPtr; EXTDECL; external xml2lib;
function xmlTextReaderConstXmlLang(reader: xmlTextReaderPtr): xmlCharPtr; EXTDECL; external xml2lib;
function xmlTextReaderConstString(reader: xmlTextReaderPtr; str: xmlCharPtr): xmlCharPtr; EXTDECL; external xml2lib;
function xmlTextReaderConstValue(reader: xmlTextReaderPtr): xmlCharPtr; EXTDECL; external xml2lib;

(*
 * use the Const version of the routine for
 * better performance and simpler code
 *)
function xmlTextReaderBaseUri(reader: xmlTextReaderPtr): xmlCharPtr; EXTDECL; external xml2lib;
function xmlTextReaderLocalName(reader: xmlTextReaderPtr): xmlCharPtr; EXTDECL; external xml2lib;
function xmlTextReaderName(reader: xmlTextReaderPtr): xmlCharPtr; EXTDECL; external xml2lib;
function xmlTextReaderNamespaceUri(reader: xmlTextReaderPtr): xmlCharPtr; EXTDECL; external xml2lib;
function xmlTextReaderPrefix(reader: xmlTextReaderPtr): xmlCharPtr; EXTDECL; external xml2lib;
function xmlTextReaderXmlLang(reader: xmlTextReaderPtr): xmlCharPtr; EXTDECL; external xml2lib;
function xmlTextReaderValue(reader: xmlTextReaderPtr): xmlCharPtr; EXTDECL; external xml2lib;

(*
 * Methods of the XmlTextReader
 *)
function xmlTextReaderClose(reader: xmlTextReaderPtr): cint; EXTDECL; external xml2lib;
function xmlTextReaderGetAttributeNo(reader: xmlTextReaderPtr; no: cint): xmlCharPtr; EXTDECL; external xml2lib;
function xmlTextReaderGetAttribute(reader: xmlTextReaderPtr; name: xmlCharPtr): xmlCharPtr; EXTDECL; external xml2lib;
function xmlTextReaderGetAttributeNs(reader: xmlTextReaderPtr; localName, namespaceURI: xmlCharPtr): xmlCharPtr; EXTDECL; external xml2lib;
function xmlTextReaderGetRemainder(reader: xmlTextReaderPtr): xmlParserInputBufferPtr; EXTDECL; external xml2lib;
function xmlTextReaderLookupNamespace(reader: xmlTextReaderPtr; prefix: xmlCharPtr): xmlCharPtr; EXTDECL; external xml2lib;
function xmlTextReaderMoveToAttributeNo(reader: xmlTextReaderPtr; no: cint): cint; EXTDECL; external xml2lib;
function xmlTextReaderMoveToAttribute(reader: xmlTextReaderPtr; name: xmlCharPtr): cint; EXTDECL; external xml2lib;
function xmlTextReaderMoveToAttributeNs(reader: xmlTextReaderPtr; localName, namespaceURI: xmlCharPtr): cint; EXTDECL; external xml2lib;
function xmlTextReaderMoveToFirstAttribute(reader: xmlTextReaderPtr): cint; EXTDECL; external xml2lib;
function xmlTextReaderMoveToNextAttribute(reader: xmlTextReaderPtr): cint; EXTDECL; external xml2lib;
function xmlTextReaderMoveToElement(reader: xmlTextReaderPtr): cint; EXTDECL; external xml2lib;
function xmlTextReaderNormalization(reader: xmlTextReaderPtr): cint; EXTDECL; external xml2lib;
function xmlTextReaderConstEncoding(reader: xmlTextReaderPtr): xmlCharPtr; EXTDECL; external xml2lib;

(*
 * Extensions
 *)
function xmlTextReaderSetParserProp(reader: xmlTextReaderPtr; prop, value: cint): cint; EXTDECL; external xml2lib;
function xmlTextReaderGetParserProp(reader: xmlTextReaderPtr; prop: cint): cint; EXTDECL; external xml2lib;
function xmlTextReaderCurrentNode(reader: xmlTextReaderPtr): xmlNodePtr; EXTDECL; external xml2lib;
function xmlTextReaderGetParserLineNumber(reader: xmlTextReaderPtr): cint; EXTDECL; external xml2lib;
function xmlTextReaderGetParserColumnNumber(reader: xmlTextReaderPtr): cint; EXTDECL; external xml2lib;
function xmlTextReaderPreserve(reader: xmlTextReaderPtr): xmlNodePtr; EXTDECL; external xml2lib;
{$IFDEF LIBXML_PATTERN_ENABLED}
function xmlTextReaderPreservePattern(reader: xmlTextReaderPtr; pattern: xmlCharPtr; namespaces: xmlCharPtrPtr): cint; EXTDECL; external xml2lib;
{$ENDIF} (* LIBXML_PATTERN_ENABLED *)
function xmlTextReaderCurrentDoc(reader: xmlTextReaderPtr): xmlDocPtr; EXTDECL; external xml2lib;
function xmlTextReaderExpand(reader: xmlTextReaderPtr): xmlNodePtr; EXTDECL; external xml2lib;
function xmlTextReaderNext(reader: xmlTextReaderPtr): cint; EXTDECL; external xml2lib;
function xmlTextReaderNextSibling(reader: xmlTextReaderPtr): cint; EXTDECL; external xml2lib;
function xmlTextReaderIsValid(reader: xmlTextReaderPtr): cint; EXTDECL; external xml2lib;
{$IFDEF LIBXML_SCHEMAS_ENABLED}

function xmlTextReaderRelaxNGValidate(reader: xmlTextReaderPtr; rng: pchar): cint; EXTDECL; external xml2lib;
function xmlTextReaderRelaxNGSetSchema(reader: xmlTextReaderPtr; schema: xmlRelaxNGPtr): cint; EXTDECL; external xml2lib;
function xmlTextReaderSchemaValidate(reader: xmlTextReaderPtr; xsd: pchar): cint; EXTDECL; external xml2lib;
function xmlTextReaderSchemaValidateCtxt(reader: xmlTextReaderPtr; ctxt: xmlSchemaValidCtxtPtr; options: cint): cint; EXTDECL; external xml2lib;
function xmlTextReaderSetSchema(reader: xmlTextReaderPtr; schema: xmlSchemaPtr): cint; EXTDECL; external xml2lib;
{$ENDIF}
function xmlTextReaderConstXmlVersion(reader: xmlTextReaderPtr): xmlCharPtr; EXTDECL; external xml2lib;
function xmlTextReaderStandalone(reader: xmlTextReaderPtr): cint; EXTDECL; external xml2lib;

(*
 * Index lookup
 *)
function xmlTextReaderByteConsumed(reader: xmlTextReaderPtr): clong; EXTDECL; external xml2lib;

(*
 * New more complete APIs for simpler creation and reuse of readers
 *)
function xmlReaderWalker(doc: xmlDocPtr): xmlTextReaderPtr; EXTDECL; external xml2lib;
function xmlReaderForDoc(cur: xmlCharPtr; URL, encoding: pchar; options: cint): xmlTextReaderPtr; EXTDECL; external xml2lib;
function xmlReaderForFile(filename, encoding: pchar; options: cint): xmlTextReaderPtr; EXTDECL; external xml2lib;
function xmlReaderForMemory(buffer: pchar; size: cint; URL, encoding: pchar; options: cint): xmlTextReaderPtr; EXTDECL; external xml2lib;
function xmlReaderForFd(fd: cint; URL, encoding: pchar; options: cint): xmlTextReaderPtr; EXTDECL; external xml2lib;
function xmlReaderForIO(ioread: xmlInputReadCallback; ioclose: xmlInputCloseCallback; ioctx: pointer; URL, encoding: pchar; options: cint): xmlTextReaderPtr; EXTDECL; external xml2lib;

function xmlReaderNewWalker(reader: xmlTextReaderPtr; doc: xmlDocPtr): cint; EXTDECL; external xml2lib;
function xmlReaderNewDoc(reader: xmlTextReaderPtr; cur: xmlCharPtr; URL, encoding: pchar; options: cint): cint; EXTDECL; external xml2lib;
function xmlReaderNewFile(reader: xmlTextReaderPtr; filename, encoding: pchar; options: cint): cint; EXTDECL; external xml2lib;
function xmlReaderNewMemory(reader: xmlTextReaderPtr; buffer: pchar; size: cint; URL, encoding: pchar; options: cint): cint; EXTDECL; external xml2lib;
function xmlReaderNewFd(reader: xmlTextReaderPtr; fd: cint; URL, encoding: pchar; options: cint): cint; EXTDECL; external xml2lib;
function xmlReaderNewIO(reader: xmlTextReaderPtr; ioread: xmlInputReadCallback; ioclose: xmlInputCloseCallback; ioctx: pointer; URL, encoding: pchar; options: cint): cint; EXTDECL; external xml2lib;

(*
 * Error handling extensions
 *)
function xmlTextReaderLocatorLineNumber(locator: xmlTextReaderLocatorPtr): cint; EXTDECL; external xml2lib;
function xmlTextReaderLocatorBaseURI(locator: xmlTextReaderLocatorPtr): xmlCharPtr; EXTDECL; external xml2lib;
procedure xmlTextReaderSetErrorHandler(reader: xmlTextReaderPtr; f: xmlTextReaderErrorFunc; arg: pointer); EXTDECL; external xml2lib;
procedure xmlTextReaderSetStructuredErrorHandler(reader: xmlTextReaderPtr; f: xmlTextReaderErrorFunc; arg: pointer); EXTDECL; external xml2lib;
procedure xmlTextReaderGetErrorHandler(reader: xmlTextReaderPtr; var f: xmlTextReaderErrorFunc; var arg: pointer); EXTDECL; external xml2lib;
{$ENDIF}

{$ENDIF} (* LIBXML_READER_ENABLED *)
{$ENDIF} (* LIBXML_SCHEMAS_ENABLED *)

