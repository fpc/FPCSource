(*
 * Summary: Old SAX version 1 handler, deprecated
 * Description: DEPRECATED set of SAX version 1 interfaces used to
 *              build the DOM tree.
 *
 * Copy: See Copyright for the status of this software.
 *
 * Author: Daniel Veillard
 *)

{$IFDEF LIBXML_LEGACY_ENABLED}

{$IFDEF FUNCTION}
function getPublicId(ctx: pointer): xmlCharPtr; EXTDECL; external xml2lib;
function getSystemId(ctx: pointer): xmlCharPtr; EXTDECL; external xml2lib;
procedure setDocumentLocator(ctx: pointer; loc: xmlSAXLocatorPtr); EXTDECL; external xml2lib;

function getLineNumber(ctx: pointer): cint; EXTDECL; external xml2lib;
function getColumnNumber(ctx: pointer): cint; EXTDECL; external xml2lib;

function isStandalone(ctx: pointer): cint; EXTDECL; external xml2lib;
function hasInternalSubset(ctx: pointer): cint; EXTDECL; external xml2lib;
function hasExternalSubset(ctx: pointer): cint; EXTDECL; external xml2lib;

procedure internalSubset(ctx: pointer; name, ExternalID, SystemID: xmlCharPtr); EXTDECL; external xml2lib;
procedure externalSubset(ctx: pointer; name, ExternalID, SystemID: xmlCharPtr); EXTDECL; external xml2lib;
function getEntity(ctx: pointer; name: xmlCharPtr): xmlEntityPtr; EXTDECL; external xml2lib;
function getParameterEntity(ctx: pointer; name: xmlCharPtr): xmlEntityPtr; EXTDECL; external xml2lib;
function resolveEntity(ctx: pointer; publicId, systemId: xmlCharPtr): xmlParserInputPtr; EXTDECL; external xml2lib;

procedure entityDecl(ctx: pointer; name: xmlCharPtr; _type: cint; content, ExternalID, SystemID: xmlCharPtr); EXTDECL; external xml2lib;
procedure attributeDecl(ctx: pointer; elem, fullname: xmlCharPtr; _type, def: cint; defaultValue: xmlCharPtr; tree: xmlEnumerationPtr); EXTDECL; external xml2lib;
procedure elementDecl(ctx: pointer; name: xmlCharPtr; _type: cint; content: xmlElementContentPtr); EXTDECL; external xml2lib;
procedure notationDecl(ctx: pointer; name, ExternalID, SystemID: xmlCharPtr); EXTDECL; external xml2lib;
procedure unparsedEntityDecl(ctx: pointer; name, publicId, systemId, notationName: xmlCharPtr); EXTDECL; external xml2lib;

procedure startDocument(ctx: pointer); EXTDECL; external xml2lib;
procedure endDocument(ctx: pointer); EXTDECL; external xml2lib;
procedure attribute(ctx: pointer; fullname, value: xmlCharPtr); EXTDECL; external xml2lib;
procedure startElement(ctx: pointer; fullname: xmlCharPtr; atts: xmlCharPtrPtr); EXTDECL; external xml2lib;
procedure endElement(ctx: pointer; name: xmlCharPtr); EXTDECL; external xml2lib;
procedure reference(ctx: pointer; name: xmlCharPtr); EXTDECL; external xml2lib;
procedure characters(ctx: pointer; ch: xmlCharPtr; len: cint); EXTDECL; external xml2lib;
procedure ignorableWhitespace(ctx: pointer; ch: xmlCharPtr; len: cint); EXTDECL; external xml2lib;
procedure processingInstruction(ctx: pointer; target, data: xmlCharPtr); EXTDECL; external xml2lib;
procedure globalNamespace(ctx: pointer; href, prefix: xmlCharPtr); EXTDECL; external xml2lib;
procedure setNamespace(ctx: pointer; name: xmlCharPtr); EXTDECL; external xml2lib;
function getNamespace(ctx: pointer): xmlNsPtr; EXTDECL; external xml2lib;
function checkNamespace(ctx: pointer; nameSpace: xmlCharPtr): cint; EXTDECL; external xml2lib;
procedure namespaceDecl(ctx: pointer; href, prefix: xmlCharPtr); EXTDECL; external xml2lib;
procedure comment(ctx: pointer; value: xmlCharPtr); EXTDECL; external xml2lib;
procedure cdataBlock(ctx: pointer; value: xmlCharPtr; len: cint); EXTDECL; external xml2lib;

{$IFDEF LIBXML_SAX1_ENABLED}
procedure initxmlDefaultSAXHandler(hdlr: xmlSAXHandlerV1Ptr; warning: cint); EXTDECL; external xml2lib;
{$IFDEF LIBXML_HTML_ENABLED}
procedure inithtmlDefaultSAXHandler(hdlr: xmlSAXHandlerV1Ptr); EXTDECL; external xml2lib;
{$ENDIF}
{$IFDEF LIBXML_DOCB_ENABLED}
procedure initdocbDefaultSAXHandler(hdlr: xmlSAXHandlerV1Ptr); EXTDECL; external xml2lib;
{$ENDIF}
{$ENDIF} (* LIBXML_SAX1_ENABLED *)
{$ENDIF}

{$ENDIF} (* LIBXML_LEGACY_ENABLED *)
