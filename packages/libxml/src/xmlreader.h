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
  xmlTextReader = record
  end;

  xmlTextReaderLocatorPtr = pointer;

  xmlTextReaderErrorFunc = procedure(arg: pointer; msg: pchar; severity: xmlParserSeverities; locator: xmlTextReaderLocatorPtr); cdecl;
{$ENDIF}

{$IFDEF FUNCTION}
(*
 * Constructors & Destructor
 *)
function xmlNewTextReader(input: xmlParserInputBufferPtr; URI: pchar): xmlTextReaderPtr; cdecl; external;
function xmlNewTextReaderFilename(URI: pchar): xmlTextReaderPtr; cdecl; external;
procedure xmlFreeTextReader(reader: xmlTextReaderPtr); cdecl; external;
function xmlTextReaderSetup(reader: xmlTextReaderPtr; input: xmlParserInputBufferPtr; URL, encoding: pchar; options: cint): cint; cdecl; external;

(*
 * Iterators
 *)
function xmlTextReaderRead(reader: xmlTextReaderPtr): cint; cdecl; external;
{$IFDEF LIBXML_WRITER_ENABLED}
function xmlTextReaderReadInnerXml(reader: xmlTextReaderPtr): xmlCharPtr; cdecl; external;
function xmlTextReaderReadOuterXml(reader: xmlTextReaderPtr): xmlCharPtr; cdecl; external;
{$ENDIF}
function xmlTextReaderReadString(reader: xmlTextReaderPtr): xmlCharPtr; cdecl; external;
function xmlTextReaderReadAttributeValue(reader: xmlTextReaderPtr): cint; cdecl; external;

(*
 * Attributes of the node
 *)
function xmlTextReaderAttributeCount(reader: xmlTextReaderPtr): cint; cdecl; external;
function xmlTextReaderDepth(reader: xmlTextReaderPtr): cint; cdecl; external;
function xmlTextReaderHasAttributes(reader: xmlTextReaderPtr): cint; cdecl; external;
function xmlTextReaderHasValue(reader: xmlTextReaderPtr): cint; cdecl; external;
function xmlTextReaderIsDefault(reader: xmlTextReaderPtr): cint; cdecl; external;
function xmlTextReaderIsEmptyElement(reader: xmlTextReaderPtr): cint; cdecl; external;
function xmlTextReaderNodeType(reader: xmlTextReaderPtr): cint; cdecl; external;
function xmlTextReaderQuoteChar(reader: xmlTextReaderPtr): cint; cdecl; external;
function xmlTextReaderReadState(reader: xmlTextReaderPtr): cint; cdecl; external;
function xmlTextReaderIsNamespaceDecl(reader: xmlTextReaderPtr): cint; cdecl; external;

function xmlTextReaderConstBaseUri(reader: xmlTextReaderPtr): xmlCharPtr; cdecl; external;
function xmlTextReaderConstLocalName(reader: xmlTextReaderPtr): xmlCharPtr; cdecl; external;
function xmlTextReaderConstName(reader: xmlTextReaderPtr): xmlCharPtr; cdecl; external;
function xmlTextReaderConstNamespaceUri(reader: xmlTextReaderPtr): xmlCharPtr; cdecl; external;
function xmlTextReaderConstPrefix(reader: xmlTextReaderPtr): xmlCharPtr; cdecl; external;
function xmlTextReaderConstXmlLang(reader: xmlTextReaderPtr): xmlCharPtr; cdecl; external;
function xmlTextReaderConstString(reader: xmlTextReaderPtr; str: xmlCharPtr): xmlCharPtr; cdecl; external;
function xmlTextReaderConstValue(reader: xmlTextReaderPtr): xmlCharPtr; cdecl; external;

(*
 * use the Const version of the routine for
 * better performance and simpler code
 *)
function xmlTextReaderBaseUri(reader: xmlTextReaderPtr): xmlCharPtr; cdecl; external;
function xmlTextReaderLocalName(reader: xmlTextReaderPtr): xmlCharPtr; cdecl; external;
function xmlTextReaderName(reader: xmlTextReaderPtr): xmlCharPtr; cdecl; external;
function xmlTextReaderNamespaceUri(reader: xmlTextReaderPtr): xmlCharPtr; cdecl; external;
function xmlTextReaderPrefix(reader: xmlTextReaderPtr): xmlCharPtr; cdecl; external;
function xmlTextReaderXmlLang(reader: xmlTextReaderPtr): xmlCharPtr; cdecl; external;
function xmlTextReaderValue(reader: xmlTextReaderPtr): xmlCharPtr; cdecl; external;

(*
 * Methods of the XmlTextReader
 *)
function xmlTextReaderClose(reader: xmlTextReaderPtr): cint; cdecl; external;
function xmlTextReaderGetAttributeNo(reader: xmlTextReaderPtr; no: cint): xmlCharPtr; cdecl; external;
function xmlTextReaderGetAttribute(reader: xmlTextReaderPtr; name: xmlCharPtr): xmlCharPtr; cdecl; external;
function xmlTextReaderGetAttributeNs(reader: xmlTextReaderPtr; localName, namespaceURI: xmlCharPtr): xmlCharPtr; cdecl; external;
function xmlTextReaderGetRemainder(reader: xmlTextReaderPtr): xmlParserInputBufferPtr; cdecl; external;
function xmlTextReaderLookupNamespace(reader: xmlTextReaderPtr; prefix: xmlCharPtr): xmlCharPtr; cdecl; external;
function xmlTextReaderMoveToAttributeNo(reader: xmlTextReaderPtr; no: cint): cint; cdecl; external;
function xmlTextReaderMoveToAttribute(reader: xmlTextReaderPtr; name: xmlCharPtr): cint; cdecl; external;
function xmlTextReaderMoveToAttributeNs(reader: xmlTextReaderPtr; localName, namespaceURI: xmlCharPtr): cint; cdecl; external;
function xmlTextReaderMoveToFirstAttribute(reader: xmlTextReaderPtr): cint; cdecl; external;
function xmlTextReaderMoveToNextAttribute(reader: xmlTextReaderPtr): cint; cdecl; external;
function xmlTextReaderMoveToElement(reader: xmlTextReaderPtr): cint; cdecl; external;
function xmlTextReaderNormalization(reader: xmlTextReaderPtr): cint; cdecl; external;
function xmlTextReaderConstEncoding(reader: xmlTextReaderPtr): xmlCharPtr; cdecl; external;

(*
 * Extensions
 *)
function xmlTextReaderSetParserProp(reader: xmlTextReaderPtr; prop, value: cint): cint; cdecl; external;
function xmlTextReaderGetParserProp(reader: xmlTextReaderPtr; prop: cint): cint; cdecl; external;
function xmlTextReaderCurrentNode(reader: xmlTextReaderPtr): xmlNodePtr; cdecl; external;
function xmlTextReaderGetParserLineNumber(reader: xmlTextReaderPtr): cint; cdecl; external;
function xmlTextReaderGetParserColumnNumber(reader: xmlTextReaderPtr): cint; cdecl; external;
function xmlTextReaderPreserve(reader: xmlTextReaderPtr): xmlNodePtr; cdecl; external;
{$IFDEF LIBXML_PATTERN_ENABLED}
function xmlTextReaderPreservePattern(reader: xmlTextReaderPtr; pattern: xmlCharPtr; namespaces: xmlCharPtrPtr): cint; cdecl; external;
{$ENDIF} (* LIBXML_PATTERN_ENABLED *)
function xmlTextReaderCurrentDoc(reader: xmlTextReaderPtr): xmlDocPtr; cdecl; external;
function xmlTextReaderExpand(reader: xmlTextReaderPtr): xmlNodePtr; cdecl; external;
function xmlTextReaderNext(reader: xmlTextReaderPtr): cint; cdecl; external;
function xmlTextReaderNextSibling(reader: xmlTextReaderPtr): cint; cdecl; external;
function xmlTextReaderIsValid(reader: xmlTextReaderPtr): cint; cdecl; external;
{$IFDEF LIBXML_SCHEMAS_ENABLED}

function xmlTextReaderRelaxNGValidate(reader: xmlTextReaderPtr; rng: pchar): cint; cdecl; external;
function xmlTextReaderRelaxNGSetSchema(reader: xmlTextReaderPtr; schema: xmlRelaxNGPtr): cint; cdecl; external;
function xmlTextReaderSchemaValidate(reader: xmlTextReaderPtr; xsd: pchar): cint; cdecl; external;
function xmlTextReaderSchemaValidateCtxt(reader: xmlTextReaderPtr; ctxt: xmlSchemaValidCtxtPtr; options: cint): cint; cdecl; external;
function xmlTextReaderSetSchema(reader: xmlTextReaderPtr; schema: xmlSchemaPtr): cint; cdecl; external;
{$ENDIF}
function xmlTextReaderConstXmlVersion(reader: xmlTextReaderPtr): xmlCharPtr; cdecl; external;
function xmlTextReaderStandalone(reader: xmlTextReaderPtr): cint; cdecl; external;

(*
 * Index lookup
 *)
function xmlTextReaderByteConsumed(reader: xmlTextReaderPtr): clong; cdecl; external;

(*
 * New more complete APIs for simpler creation and reuse of readers
 *)
function xmlReaderWalker(doc: xmlDocPtr): xmlTextReaderPtr; cdecl; external;
function xmlReaderForDoc(cur: xmlCharPtr; URL, encoding: pchar; options: cint): xmlTextReaderPtr; cdecl; external;
function xmlReaderForFile(filename, encoding: pchar; options: cint): xmlTextReaderPtr; cdecl; external;
function xmlReaderForMemory(buffer: pchar; size: cint; URL, encoding: pchar; options: cint): xmlTextReaderPtr; cdecl; external;
function xmlReaderForFd(fd: cint; URL, encoding: pchar; options: cint): xmlTextReaderPtr; cdecl; external;
function xmlReaderForIO(ioread: xmlInputReadCallback; ioclose: xmlInputCloseCallback; ioctx: pointer; URL, encoding: pchar; options: cint): xmlTextReaderPtr; cdecl; external;

function xmlReaderNewWalker(reader: xmlTextReaderPtr; doc: xmlDocPtr): cint; cdecl; external;
function xmlReaderNewDoc(reader: xmlTextReaderPtr; cur: xmlCharPtr; URL, encoding: pchar; options: cint): cint; cdecl; external;
function xmlReaderNewFile(reader: xmlTextReaderPtr; filename, encoding: pchar; options: cint): cint; cdecl; external;
function xmlReaderNewMemory(reader: xmlTextReaderPtr; buffer: pchar; size: cint; URL, encoding: pchar; options: cint): cint; cdecl; external;
function xmlReaderNewFd(reader: xmlTextReaderPtr; fd: cint; URL, encoding: pchar; options: cint): cint; cdecl; external;
function xmlReaderNewIO(reader: xmlTextReaderPtr; ioread: xmlInputReadCallback; ioclose: xmlInputCloseCallback; ioctx: pointer; URL, encoding: pchar; options: cint): cint; cdecl; external;

(*
 * Error handling extensions
 *)
function xmlTextReaderLocatorLineNumber(locator: xmlTextReaderLocatorPtr): cint; cdecl; external;
function xmlTextReaderLocatorBaseURI(locator: xmlTextReaderLocatorPtr): xmlCharPtr; cdecl; external;
procedure xmlTextReaderLocatorBaseURI(reader: xmlTextReaderPtr; f: xmlTextReaderErrorFunc; arg: pointer); cdecl; external;
procedure xmlTextReaderSetStructuredErrorHandler(reader: xmlTextReaderPtr; f: xmlTextReaderErrorFunc; arg: pointer); cdecl; external;
procedure xmlTextReaderGetErrorHandler(reader: xmlTextReaderPtr; var f: xmlTextReaderErrorFunc; var arg: pointer); cdecl; external;
{$ENDIF}

{$ENDIF} (* LIBXML_READER_ENABLED *)
{$ENDIF} (* __XML_XMLREADER_H__ *)

