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

  xmlTextReaderErrorFunc = procedure(arg: pointer; msg: PAnsiChar; severity: xmlParserSeverities; locator: xmlTextReaderLocatorPtr); EXTDECL;
{$ENDIF}

{$IFDEF FUNCTION}
(*
 * Constructors & Destructor
 *)
function xmlNewTextReader(input: xmlParserInputBufferPtr; URI: PAnsiChar): xmlTextReaderPtr; EXTDECL; external xml2lib;
function xmlNewTextReaderFilename(URI: PAnsiChar): xmlTextReaderPtr; EXTDECL; external xml2lib;
procedure xmlFreeTextReader(reader: xmlTextReaderPtr); EXTDECL; external xml2lib;
function xmlTextReaderSetup(reader: xmlTextReaderPtr; input: xmlParserInputBufferPtr; URL, encoding: PAnsiChar; options: cint): cint; EXTDECL; external xml2lib;

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

function xmlTextReaderRelaxNGValidate(reader: xmlTextReaderPtr; rng: PAnsiChar): cint; EXTDECL; external xml2lib;
function xmlTextReaderRelaxNGSetSchema(reader: xmlTextReaderPtr; schema: xmlRelaxNGPtr): cint; EXTDECL; external xml2lib;
function xmlTextReaderSchemaValidate(reader: xmlTextReaderPtr; xsd: PAnsiChar): cint; EXTDECL; external xml2lib;
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
function xmlReaderForDoc(cur: xmlCharPtr; URL, encoding: PAnsiChar; options: cint): xmlTextReaderPtr; EXTDECL; external xml2lib;
function xmlReaderForFile(filename, encoding: PAnsiChar; options: cint): xmlTextReaderPtr; EXTDECL; external xml2lib;
function xmlReaderForMemory(buffer: PAnsiChar; size: cint; URL, encoding: PAnsiChar; options: cint): xmlTextReaderPtr; EXTDECL; external xml2lib;
function xmlReaderForFd(fd: cint; URL, encoding: PAnsiChar; options: cint): xmlTextReaderPtr; EXTDECL; external xml2lib;
function xmlReaderForIO(ioread: xmlInputReadCallback; ioclose: xmlInputCloseCallback; ioctx: pointer; URL, encoding: PAnsiChar; options: cint): xmlTextReaderPtr; EXTDECL; external xml2lib;

function xmlReaderNewWalker(reader: xmlTextReaderPtr; doc: xmlDocPtr): cint; EXTDECL; external xml2lib;
function xmlReaderNewDoc(reader: xmlTextReaderPtr; cur: xmlCharPtr; URL, encoding: PAnsiChar; options: cint): cint; EXTDECL; external xml2lib;
function xmlReaderNewFile(reader: xmlTextReaderPtr; filename, encoding: PAnsiChar; options: cint): cint; EXTDECL; external xml2lib;
function xmlReaderNewMemory(reader: xmlTextReaderPtr; buffer: PAnsiChar; size: cint; URL, encoding: PAnsiChar; options: cint): cint; EXTDECL; external xml2lib;
function xmlReaderNewFd(reader: xmlTextReaderPtr; fd: cint; URL, encoding: PAnsiChar; options: cint): cint; EXTDECL; external xml2lib;
function xmlReaderNewIO(reader: xmlTextReaderPtr; ioread: xmlInputReadCallback; ioclose: xmlInputCloseCallback; ioctx: pointer; URL, encoding: PAnsiChar; options: cint): cint; EXTDECL; external xml2lib;

(*
 * Error handling extensions
 *)
function xmlTextReaderLocatorLineNumber(locator: xmlTextReaderLocatorPtr): cint; EXTDECL; external xml2lib;
function xmlTextReaderLocatorBaseURI(locator: xmlTextReaderLocatorPtr): xmlCharPtr; EXTDECL; external xml2lib;
procedure xmlTextReaderSetErrorHandler(reader: xmlTextReaderPtr; f: xmlTextReaderErrorFunc; arg: pointer); EXTDECL; external xml2lib;
procedure xmlTextReaderSetStructuredErrorHandler(reader: xmlTextReaderPtr; f: xmlTextReaderErrorFunc; arg: pointer); EXTDECL; external xml2lib;
procedure xmlTextReaderGetErrorHandler(reader: xmlTextReaderPtr; var f: xmlTextReaderErrorFunc; var arg: pointer); EXTDECL; external xml2lib;
{$ENDIF}

{$IFDEF FUNCTIONVAR}
(*
 * Constructors & Destructor
 *)
  xmlNewTextReader: function(input: xmlParserInputBufferPtr; URI: PAnsiChar): xmlTextReaderPtr; EXTDECL;
  xmlNewTextReaderFilename: function(URI: PAnsiChar): xmlTextReaderPtr; EXTDECL;
  xmlFreeTextReader: procedure(reader: xmlTextReaderPtr); EXTDECL;
  xmlTextReaderSetup: function(reader: xmlTextReaderPtr; input: xmlParserInputBufferPtr; URL, encoding: PAnsiChar; options: cint): cint; EXTDECL;

(*
 * Iterators
 *)
  xmlTextReaderRead: function(reader: xmlTextReaderPtr): cint; EXTDECL;
{$IFDEF LIBXML_WRITER_ENABLED}
  xmlTextReaderReadInnerXml: function(reader: xmlTextReaderPtr): xmlCharPtr; EXTDECL;
  xmlTextReaderReadOuterXml: function(reader: xmlTextReaderPtr): xmlCharPtr; EXTDECL;
{$ENDIF}
  xmlTextReaderReadString: function(reader: xmlTextReaderPtr): xmlCharPtr; EXTDECL;
  xmlTextReaderReadAttributeValue: function(reader: xmlTextReaderPtr): cint; EXTDECL;

(*
 * Attributes of the node
 *)
  xmlTextReaderAttributeCount: function(reader: xmlTextReaderPtr): cint; EXTDECL;
  xmlTextReaderDepth: function(reader: xmlTextReaderPtr): cint; EXTDECL;
  xmlTextReaderHasAttributes: function(reader: xmlTextReaderPtr): cint; EXTDECL;
  xmlTextReaderHasValue: function(reader: xmlTextReaderPtr): cint; EXTDECL;
  xmlTextReaderIsDefault: function(reader: xmlTextReaderPtr): cint; EXTDECL;
  xmlTextReaderIsEmptyElement: function(reader: xmlTextReaderPtr): cint; EXTDECL;
  xmlTextReaderNodeType: function(reader: xmlTextReaderPtr): cint; EXTDECL;
  xmlTextReaderQuoteChar: function(reader: xmlTextReaderPtr): cint; EXTDECL;
  xmlTextReaderReadState: function(reader: xmlTextReaderPtr): cint; EXTDECL;
  xmlTextReaderIsNamespaceDecl: function(reader: xmlTextReaderPtr): cint; EXTDECL;

  xmlTextReaderConstBaseUri: function(reader: xmlTextReaderPtr): xmlCharPtr; EXTDECL;
  xmlTextReaderConstLocalName: function(reader: xmlTextReaderPtr): xmlCharPtr; EXTDECL;
  xmlTextReaderConstName: function(reader: xmlTextReaderPtr): xmlCharPtr; EXTDECL;
  xmlTextReaderConstNamespaceUri: function(reader: xmlTextReaderPtr): xmlCharPtr; EXTDECL;
  xmlTextReaderConstPrefix: function(reader: xmlTextReaderPtr): xmlCharPtr; EXTDECL;
  xmlTextReaderConstXmlLang: function(reader: xmlTextReaderPtr): xmlCharPtr; EXTDECL;
  xmlTextReaderConstString: function(reader: xmlTextReaderPtr; str: xmlCharPtr): xmlCharPtr; EXTDECL;
  xmlTextReaderConstValue: function(reader: xmlTextReaderPtr): xmlCharPtr; EXTDECL;

(*
 * use the Const version of the routine for
 * better performance and simpler code
 *)
  xmlTextReaderBaseUri: function(reader: xmlTextReaderPtr): xmlCharPtr; EXTDECL;
  xmlTextReaderLocalName: function(reader: xmlTextReaderPtr): xmlCharPtr; EXTDECL;
  xmlTextReaderName: function(reader: xmlTextReaderPtr): xmlCharPtr; EXTDECL;
  xmlTextReaderNamespaceUri: function(reader: xmlTextReaderPtr): xmlCharPtr; EXTDECL;
  xmlTextReaderPrefix: function(reader: xmlTextReaderPtr): xmlCharPtr; EXTDECL;
  xmlTextReaderXmlLang: function(reader: xmlTextReaderPtr): xmlCharPtr; EXTDECL;
  xmlTextReaderValue: function(reader: xmlTextReaderPtr): xmlCharPtr; EXTDECL;

(*
 * Methods of the XmlTextReader
 *)
  xmlTextReaderClose: function(reader: xmlTextReaderPtr): cint; EXTDECL;
  xmlTextReaderGetAttributeNo: function(reader: xmlTextReaderPtr; no: cint): xmlCharPtr; EXTDECL;
  xmlTextReaderGetAttribute: function(reader: xmlTextReaderPtr; name: xmlCharPtr): xmlCharPtr; EXTDECL;
  xmlTextReaderGetAttributeNs: function(reader: xmlTextReaderPtr; localName, namespaceURI: xmlCharPtr): xmlCharPtr; EXTDECL;
  xmlTextReaderGetRemainder: function(reader: xmlTextReaderPtr): xmlParserInputBufferPtr; EXTDECL;
  xmlTextReaderLookupNamespace: function(reader: xmlTextReaderPtr; prefix: xmlCharPtr): xmlCharPtr; EXTDECL;
  xmlTextReaderMoveToAttributeNo: function(reader: xmlTextReaderPtr; no: cint): cint; EXTDECL;
  xmlTextReaderMoveToAttribute: function(reader: xmlTextReaderPtr; name: xmlCharPtr): cint; EXTDECL;
  xmlTextReaderMoveToAttributeNs: function(reader: xmlTextReaderPtr; localName, namespaceURI: xmlCharPtr): cint; EXTDECL;
  xmlTextReaderMoveToFirstAttribute: function(reader: xmlTextReaderPtr): cint; EXTDECL;
  xmlTextReaderMoveToNextAttribute: function(reader: xmlTextReaderPtr): cint; EXTDECL;
  xmlTextReaderMoveToElement: function(reader: xmlTextReaderPtr): cint; EXTDECL;
  xmlTextReaderNormalization: function(reader: xmlTextReaderPtr): cint; EXTDECL;
  xmlTextReaderConstEncoding: function (reader: xmlTextReaderPtr): xmlCharPtr; EXTDECL;

(*
 * Extensions
 *)
  xmlTextReaderSetParserProp: function(reader: xmlTextReaderPtr; prop, value: cint): cint; EXTDECL;
  xmlTextReaderGetParserProp: function(reader: xmlTextReaderPtr; prop: cint): cint; EXTDECL;
  xmlTextReaderCurrentNode: function(reader: xmlTextReaderPtr): xmlNodePtr; EXTDECL;
  xmlTextReaderGetParserLineNumber: function(reader: xmlTextReaderPtr): cint; EXTDECL;
  xmlTextReaderGetParserColumnNumber: function(reader: xmlTextReaderPtr): cint; EXTDECL;
  xmlTextReaderPreserve: function(reader: xmlTextReaderPtr): xmlNodePtr; EXTDECL;
{$IFDEF LIBXML_PATTERN_ENABLED}
  xmlTextReaderPreservePattern: function(reader: xmlTextReaderPtr; pattern: xmlCharPtr; namespaces: xmlCharPtrPtr): cint; EXTDECL;
{$ENDIF} (* LIBXML_PATTERN_ENABLED *)
  xmlTextReaderCurrentDoc: function(reader: xmlTextReaderPtr): xmlDocPtr; EXTDECL;
  xmlTextReaderExpand: function(reader: xmlTextReaderPtr): xmlNodePtr; EXTDECL;
  xmlTextReaderNext: function(reader: xmlTextReaderPtr): cint; EXTDECL;
  xmlTextReaderNextSibling: function(reader: xmlTextReaderPtr): cint; EXTDECL;
  xmlTextReaderIsValid: function(reader: xmlTextReaderPtr): cint; EXTDECL;
{$IFDEF LIBXML_SCHEMAS_ENABLED}

  xmlTextReaderRelaxNGValidate: function(reader: xmlTextReaderPtr; rng: PAnsiChar): cint; EXTDECL;
  xmlTextReaderRelaxNGSetSchema: function(reader: xmlTextReaderPtr; schema: xmlRelaxNGPtr): cint; EXTDECL;
  xmlTextReaderSchemaValidate: function(reader: xmlTextReaderPtr; xsd: PAnsiChar): cint; EXTDECL;
  xmlTextReaderSchemaValidateCtxt: function(reader: xmlTextReaderPtr; ctxt: xmlSchemaValidCtxtPtr; options: cint): cint; EXTDECL;
  xmlTextReaderSetSchema: function(reader: xmlTextReaderPtr; schema: xmlSchemaPtr): cint; EXTDECL;
{$ENDIF}
  xmlTextReaderConstXmlVersion: function(reader: xmlTextReaderPtr): xmlCharPtr; EXTDECL;
  xmlTextReaderStandalone: function(reader: xmlTextReaderPtr): cint; EXTDECL;

(*
 * Index lookup
 *)
  xmlTextReaderByteConsumed: function(reader: xmlTextReaderPtr): clong; EXTDECL;

(*
 * New more complete APIs for simpler creation and reuse of readers
 *)
  xmlReaderWalker: function(doc: xmlDocPtr): xmlTextReaderPtr; EXTDECL;
  xmlReaderForDoc: function(cur: xmlCharPtr; URL, encoding: PAnsiChar; options: cint): xmlTextReaderPtr; EXTDECL;
  xmlReaderForFile: function(filename, encoding: PAnsiChar; options: cint): xmlTextReaderPtr; EXTDECL;
  xmlReaderForMemory: function(buffer: PAnsiChar; size: cint; URL, encoding: PAnsiChar; options: cint): xmlTextReaderPtr; EXTDECL;
  xmlReaderForFd: function(fd: cint; URL, encoding: PAnsiChar; options: cint): xmlTextReaderPtr; EXTDECL;
  xmlReaderForIO: function(ioread: xmlInputReadCallback; ioclose: xmlInputCloseCallback; ioctx: pointer; URL, encoding: PAnsiChar; options: cint): xmlTextReaderPtr; EXTDECL;

  xmlReaderNewWalker: function(reader: xmlTextReaderPtr; doc: xmlDocPtr): cint; EXTDECL;
  xmlReaderNewDoc: function(reader: xmlTextReaderPtr; cur: xmlCharPtr; URL, encoding: PAnsiChar; options: cint): cint; EXTDECL;
  xmlReaderNewFile: function(reader: xmlTextReaderPtr; filename, encoding: PAnsiChar; options: cint): cint; EXTDECL;
  xmlReaderNewMemory: function(reader: xmlTextReaderPtr; buffer: PAnsiChar; size: cint; URL, encoding: PAnsiChar; options: cint): cint; EXTDECL;
  xmlReaderNewFd: function(reader: xmlTextReaderPtr; fd: cint; URL, encoding: PAnsiChar; options: cint): cint; EXTDECL;
  xmlReaderNewIO: function(reader: xmlTextReaderPtr; ioread: xmlInputReadCallback; ioclose: xmlInputCloseCallback; ioctx: pointer; URL, encoding: PAnsiChar; options: cint): cint; EXTDECL;

(*
 * Error handling extensions
 *)
  xmlTextReaderLocatorLineNumber: function(locator: xmlTextReaderLocatorPtr): cint; EXTDECL;
  xmlTextReaderLocatorBaseURI: function(locator: xmlTextReaderLocatorPtr): xmlCharPtr; EXTDECL;
  xmlTextReaderSetErrorHandler: procedure(reader: xmlTextReaderPtr; f: xmlTextReaderErrorFunc; arg: pointer); EXTDECL;
  xmlTextReaderSetStructuredErrorHandler: procedure(reader: xmlTextReaderPtr; f: xmlTextReaderErrorFunc; arg: pointer); EXTDECL;
  xmlTextReaderGetErrorHandler: procedure(reader: xmlTextReaderPtr; var f: xmlTextReaderErrorFunc; var arg: pointer); EXTDECL;
{$ENDIF}

{$ENDIF} (* LIBXML_READER_ENABLED *)
{$ENDIF} (* LIBXML_SCHEMAS_ENABLED *)

