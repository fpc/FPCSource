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
function getPublicId(ctx: pointer): xmlCharPtr; XMLCALL; XMLPUBFUN;
function getSystemId(ctx: pointer): xmlCharPtr; XMLCALL; XMLPUBFUN;
procedure setDocumentLocator(ctx: pointer; loc: xmlSAXLocatorPtr); XMLCALL; XMLPUBFUN;

function getLineNumber(ctx: pointer): cint; XMLCALL; XMLPUBFUN;
function getColumnNumber(ctx: pointer): cint; XMLCALL; XMLPUBFUN;

function isStandalone(ctx: pointer): cint; XMLCALL; XMLPUBFUN;
function hasInternalSubset(ctx: pointer): cint; XMLCALL; XMLPUBFUN;
function hasExternalSubset(ctx: pointer): cint; XMLCALL; XMLPUBFUN;

procedure internalSubset(ctx: pointer; name, ExternalID, SystemID: xmlCharPtr); XMLCALL; XMLPUBFUN;
procedure externalSubset(ctx: pointer; name, ExternalID, SystemID: xmlCharPtr); XMLCALL; XMLPUBFUN;
function getEntity(ctx: pointer; name: xmlCharPtr): xmlEntityPtr; XMLCALL; XMLPUBFUN;
function getParameterEntity(ctx: pointer; name: xmlCharPtr): xmlEntityPtr; XMLCALL; XMLPUBFUN;
function resolveEntity(ctx: pointer; publicId, systemId: xmlCharPtr): xmlParserInputPtr; XMLCALL; XMLPUBFUN;

procedure entityDecl(ctx: pointer; name: xmlCharPtr; _type: cint; content, ExternalID, SystemID: xmlCharPtr); XMLCALL; XMLPUBFUN;
procedure attributeDecl(ctx: pointer; elem, fullname: xmlCharPtr; _type, def: cint; defaultValue: xmlCharPtr; tree: xmlEnumerationPtr); XMLCALL; XMLPUBFUN;
procedure elementDecl(ctx: pointer; name: xmlCharPtr; _type: cint; content: xmlElementContentPtr); XMLCALL; XMLPUBFUN;
procedure notationDecl(ctx: pointer; name, ExternalID, SystemID: xmlCharPtr); XMLCALL; XMLPUBFUN;
procedure unparsedEntityDecl(ctx: pointer; name, publicId, systemId, notationName: xmlCharPtr); XMLCALL; XMLPUBFUN;

procedure startDocument(ctx: pointer); XMLCALL; XMLPUBFUN;
procedure endDocument(ctx: pointer); XMLCALL; XMLPUBFUN;
procedure attribute(ctx: pointer; fullname, value: xmlCharPtr); XMLCALL; XMLPUBFUN;
procedure startElement(ctx: pointer; fullname: xmlCharPtr; atts: xmlCharPtrPtr); XMLCALL; XMLPUBFUN;
procedure endElement(ctx: pointer; name: xmlCharPtr); XMLCALL; XMLPUBFUN;
procedure reference(ctx: pointer; name: xmlCharPtr); XMLCALL; XMLPUBFUN;
procedure characters(ctx: pointer; ch: xmlCharPtr; len: cint); XMLCALL; XMLPUBFUN;
procedure ignorableWhitespace(ctx: pointer; ch: xmlCharPtr; len: cint); XMLCALL; XMLPUBFUN;
procedure processingInstruction(ctx: pointer; target, data: xmlCharPtr); XMLCALL; XMLPUBFUN;
procedure globalNamespace(ctx: pointer; href, prefix: xmlCharPtr); XMLCALL; XMLPUBFUN;
procedure setNamespace(ctx: pointer; name: xmlCharPtr); XMLCALL; XMLPUBFUN;
function getNamespace(ctx: pointer): xmlNsPtr; XMLCALL; XMLPUBFUN;
function checkNamespace(ctx: pointer; nameSpace: xmlCharPtr): cint; XMLCALL; XMLPUBFUN;
procedure namespaceDecl(ctx: pointer; href, prefix: xmlCharPtr); XMLCALL; XMLPUBFUN;
procedure comment(ctx: pointer; value: xmlCharPtr); XMLCALL; XMLPUBFUN;
procedure cdataBlock(ctx: pointer; value: xmlCharPtr; len: cint); XMLCALL; XMLPUBFUN;

{$IFDEF LIBXML_SAX1_ENABLED}
procedure initxmlDefaultSAXHandler(hdlr: xmlSAXHandlerV1Ptr; warning: cint); XMLCALL; XMLPUBFUN;
{$IFDEF LIBXML_HTML_ENABLED}
procedure inithtmlDefaultSAXHandler(hdlr: xmlSAXHandlerV1Ptr); XMLCALL; XMLPUBFUN;
{$ENDIF}
{$IFDEF LIBXML_DOCB_ENABLED}
procedure initdocbDefaultSAXHandler(hdlr: xmlSAXHandlerV1Ptr); XMLCALL; XMLPUBFUN;
{$ENDIF}
{$ENDIF} (* LIBXML_SAX1_ENABLED *)
{$ENDIF}

{$ENDIF} (* LIBXML_LEGACY_ENABLED *)
