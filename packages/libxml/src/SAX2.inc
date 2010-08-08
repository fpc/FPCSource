(*
 * Summary: SAX2 parser interface used to build the DOM tree
 * Description: those are the default SAX2 interfaces used by
 *              the library when building DOM tree.
 *
 * Copy: See Copyright for the status of this software.
 *
 * Author: Daniel Veillard
 *)

{$IFDEF FUNCTION}
function xmlSAX2GetPublicId(ctx: pointer): xmlCharPtr; EXTDECL; external xml2lib;
function xmlSAX2GetSystemId(ctx: pointer): xmlCharPtr; EXTDECL; external xml2lib;
procedure xmlSAX2GetSystemId(ctx: pointer; loc: xmlSAXLocatorPtr); EXTDECL; external xml2lib;

function xmlSAX2GetLineNumber(ctx: pointer): cint; EXTDECL; external xml2lib;
function xmlSAX2GetColumnNumber(ctx: pointer): cint; EXTDECL; external xml2lib;

function xmlSAX2IsStandalone(ctx: pointer): cint; EXTDECL; external xml2lib;
function xmlSAX2HasInternalSubset(ctx: pointer): cint; EXTDECL; external xml2lib;
function xmlSAX2HasExternalSubset(ctx: pointer): cint; EXTDECL; external xml2lib;

procedure xmlSAX2InternalSubset(ctx: pointer; name, ExternalID, SystemID: xmlCharPtr); EXTDECL; external xml2lib;
procedure xmlSAX2ExternalSubset(ctx: pointer; name, ExternalID, SystemID: xmlCharPtr); EXTDECL; external xml2lib;
function xmlSAX2GetEntity(ctx: pointer; name: xmlCharPtr): xmlEntityPtr; EXTDECL; external xml2lib;
function xmlSAX2GetParameterEntity(ctx: pointer; name: xmlCharPtr): xmlEntityPtr; EXTDECL; external xml2lib;
function xmlSAX2ResolveEntity(ctx: pointer; publicId, systemId: xmlCharPtr): xmlParserInputPtr; EXTDECL; external xml2lib;

procedure xmlSAX2EntityDecl(ctx: pointer; name: xmlCharPtr; _type: cint; publicId, systemId, content: xmlCharPtr); EXTDECL; external xml2lib;
procedure xmlSAX2AttributeDecl(ctx: pointer; elem, fullname: xmlCharPtr; _type, def: cint; defaultValue: xmlCharPtr; tree: xmlEnumerationPtr); EXTDECL; external xml2lib;
procedure xmlSAX2ElementDecl(ctx: pointer; name: xmlCharPtr; _type: cint; content: xmlElementContentPtr); EXTDECL; external xml2lib;
procedure xmlSAX2NotationDecl(ctx: pointer; name, publicId, systemId: xmlCharPtr); EXTDECL; external xml2lib;
procedure xmlSAX2UnparsedEntityDecl(ctx: pointer; name, publicId, systemId, notationName: xmlCharPtr); EXTDECL; external xml2lib;

procedure xmlSAX2StartDocument(ctx: pointer); EXTDECL; external xml2lib;
procedure xmlSAX2EndDocument(ctx: pointer); EXTDECL; external xml2lib;

{$IF defined(LIBXML_SAX1_ENABLED) or defined(LIBXML_HTML_ENABLED) or defined(LIBXML_WRITER_ENABLED) or defined(LIBXML_DOCB_ENABLED)}
procedure xmlSAX2StartElement(ctx: pointer; filename: xmlCharPtr; atts: xmlCharPtrPtr); EXTDECL; external xml2lib;
procedure xmlSAX2EndElement(ctx: pointer; name: xmlCharPtr); EXTDECL; external xml2lib;
{$ENDIF} (* LIBXML_SAX1_ENABLED or LIBXML_HTML_ENABLED *)
procedure xmlSAX2StartElementNs(ctx: pointer; localname, prefix, URI: xmlCharPtr; nb_namespaces: cint; namespaces: xmlCharPtrPtr;
  nb_attributes, nb_defaulted: cint; attributes: xmlCharPtrPtr); EXTDECL; external xml2lib;
procedure xmlSAX2EndElementNs(ctx: pointer; localname, prefix, URI: xmlCharPtr); EXTDECL; external xml2lib;
procedure xmlSAX2Reference(ctx: pointer; name: xmlCharPtr); EXTDECL; external xml2lib;
procedure xmlSAX2Characters(ctx: pointer; name: xmlCharPtr; len: cint); EXTDECL; external xml2lib;
procedure xmlSAX2IgnorableWhitespace(ctx: pointer; name: xmlCharPtr; len: cint); EXTDECL; external xml2lib;
procedure xmlSAX2ProcessingInstruction(ctx: pointer; target, data: xmlCharPtr); EXTDECL; external xml2lib;
procedure xmlSAX2Comment(ctx: pointer; value: xmlCharPtr); EXTDECL; external xml2lib;
procedure xmlSAX2CDataBlock(ctx: pointer; value: xmlCharPtr; len: cint); EXTDECL; external xml2lib;
{$IFDEF LIBXML_SAX1_ENABLED}
function xmlSAXDefaultVersion(version: cint): cint; EXTDECL; external xml2lib;
{$ENDIF} (* LIBXML_SAX1_ENABLED *)

function xmlSAXVersion(hdlr: xmlSAXHandlerPtr; version: cint): cint; EXTDECL; external xml2lib;
procedure xmlSAX2InitDefaultSAXHandler(hdlr: xmlSAXHandlerPtr; warning: cint); EXTDECL; external xml2lib;
{$IFDEF LIBXML_HTML_ENABLED}
procedure xmlSAX2InitHtmlDefaultSAXHandler(hdlr: xmlSAXHandlerPtr); EXTDECL; external xml2lib;
procedure htmlDefaultSAXHandlerInit; EXTDECL; external xml2lib;
{$ENDIF}
{$IFDEF LIBXML_DOCB_ENABLED}
procedure xmlSAX2InitDocbDefaultSAXHandler(hdlr: xmlSAXHandlerPtr); EXTDECL; external xml2lib;
procedure docbDefaultSAXHandlerInit; EXTDECL; external xml2lib;
{$ENDIF}
procedure xmlDefaultSAXHandlerInit; EXTDECL; external xml2lib;
{$ENDIF}
