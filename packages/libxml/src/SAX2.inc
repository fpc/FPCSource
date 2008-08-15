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
function xmlSAX2GetPublicId(ctx: pointer): xmlCharPtr; XMLCALL; XMLPUBFUN;
function xmlSAX2GetSystemId(ctx: pointer): xmlCharPtr; XMLCALL; XMLPUBFUN;
procedure xmlSAX2GetSystemId(ctx: pointer; loc: xmlSAXLocatorPtr); XMLCALL; XMLPUBFUN;

function xmlSAX2GetLineNumber(ctx: pointer): cint; XMLCALL; XMLPUBFUN;
function xmlSAX2GetColumnNumber(ctx: pointer): cint; XMLCALL; XMLPUBFUN;

function xmlSAX2IsStandalone(ctx: pointer): cint; XMLCALL; XMLPUBFUN;
function xmlSAX2HasInternalSubset(ctx: pointer): cint; XMLCALL; XMLPUBFUN;
function xmlSAX2HasExternalSubset(ctx: pointer): cint; XMLCALL; XMLPUBFUN;

procedure xmlSAX2InternalSubset(ctx: pointer; name, ExternalID, SystemID: xmlCharPtr); XMLCALL; XMLPUBFUN;
procedure xmlSAX2ExternalSubset(ctx: pointer; name, ExternalID, SystemID: xmlCharPtr); XMLCALL; XMLPUBFUN;
function xmlSAX2GetEntity(ctx: pointer; name: xmlCharPtr): xmlEntityPtr; XMLCALL; XMLPUBFUN;
function xmlSAX2GetParameterEntity(ctx: pointer; name: xmlCharPtr): xmlEntityPtr; XMLCALL; XMLPUBFUN;
function xmlSAX2ResolveEntity(ctx: pointer; publicId, systemId: xmlCharPtr): xmlParserInputPtr; XMLCALL; XMLPUBFUN;

procedure xmlSAX2EntityDecl(ctx: pointer; name: xmlCharPtr; _type: cint; publicId, systemId, content: xmlCharPtr); XMLCALL; XMLPUBFUN;
procedure xmlSAX2AttributeDecl(ctx: pointer; elem, fullname: xmlCharPtr; _type, def: cint; defaultValue: xmlCharPtr; tree: xmlEnumerationPtr); XMLCALL; XMLPUBFUN;
procedure xmlSAX2ElementDecl(ctx: pointer; name: xmlCharPtr; _type: cint; content: xmlElementContentPtr); XMLCALL; XMLPUBFUN;
procedure xmlSAX2NotationDecl(ctx: pointer; name, publicId, systemId: xmlCharPtr); XMLCALL; XMLPUBFUN;
procedure xmlSAX2UnparsedEntityDecl(ctx: pointer; name, publicId, systemId, notationName: xmlCharPtr); XMLCALL; XMLPUBFUN;

procedure xmlSAX2StartDocument(ctx: pointer); XMLCALL; XMLPUBFUN;
procedure xmlSAX2EndDocument(ctx: pointer); XMLCALL; XMLPUBFUN;

{$IF defined(LIBXML_SAX1_ENABLED) or defined(LIBXML_HTML_ENABLED) or defined(LIBXML_WRITER_ENABLED) or defined(LIBXML_DOCB_ENABLED)}
procedure xmlSAX2StartElement(ctx: pointer; filename: xmlCharPtr; atts: xmlCharPtrPtr); XMLCALL; XMLPUBFUN;
procedure xmlSAX2EndElement(ctx: pointer; name: xmlCharPtr); XMLCALL; XMLPUBFUN;
{$ENDIF} (* LIBXML_SAX1_ENABLED or LIBXML_HTML_ENABLED *)
procedure xmlSAX2StartElementNs(ctx: pointer; localname, prefix, URI: xmlCharPtr; nb_namespaces: cint; namespaces: xmlCharPtrPtr;
  nb_attributes, nb_defaulted: cint; attributes: xmlCharPtrPtr); XMLCALL; XMLPUBFUN;
procedure xmlSAX2EndElementNs(ctx: pointer; localname, prefix, URI: xmlCharPtr); XMLCALL; XMLPUBFUN;
procedure xmlSAX2Reference(ctx: pointer; name: xmlCharPtr); XMLCALL; XMLPUBFUN;
procedure xmlSAX2Characters(ctx: pointer; name: xmlCharPtr; len: cint); XMLCALL; XMLPUBFUN;
procedure xmlSAX2IgnorableWhitespace(ctx: pointer; name: xmlCharPtr; len: cint); XMLCALL; XMLPUBFUN;
procedure xmlSAX2ProcessingInstruction(ctx: pointer; target, data: xmlCharPtr); XMLCALL; XMLPUBFUN;
procedure xmlSAX2Comment(ctx: pointer; value: xmlCharPtr); XMLCALL; XMLPUBFUN;
procedure xmlSAX2CDataBlock(ctx: pointer; value: xmlCharPtr; len: cint); XMLCALL; XMLPUBFUN;
{$IFDEF LIBXML_SAX1_ENABLED}
function xmlSAXDefaultVersion(version: cint): cint; XMLCALL; XMLPUBFUN;
{$ENDIF} (* LIBXML_SAX1_ENABLED *)

function xmlSAXVersion(hdlr: xmlSAXHandlerPtr; version: cint): cint; XMLCALL; XMLPUBFUN;
procedure xmlSAX2InitDefaultSAXHandler(hdlr: xmlSAXHandlerPtr; warning: cint); XMLCALL; XMLPUBFUN;
{$IFDEF LIBXML_HTML_ENABLED}
procedure xmlSAX2InitHtmlDefaultSAXHandler(hdlr: xmlSAXHandlerPtr); XMLCALL; XMLPUBFUN;
procedure htmlDefaultSAXHandlerInit; XMLCALL; XMLPUBFUN;
{$ENDIF}
{$IFDEF LIBXML_DOCB_ENABLED}
procedure xmlSAX2InitDocbDefaultSAXHandler(hdlr: xmlSAXHandlerPtr); XMLCALL; XMLPUBFUN;
procedure docbDefaultSAXHandlerInit; XMLCALL; XMLPUBFUN;
{$ENDIF}
procedure xmlDefaultSAXHandlerInit; XMLCALL; XMLPUBFUN;
{$ENDIF}
