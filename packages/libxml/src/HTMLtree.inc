(*
 * Summary: specific APIs to process HTML tree, especially serialization
 * Description: this module implements a few function needed to process
 *              tree in an HTML specific way.
 *
 * Copy: See Copyright for the status of this software.
 *
 * Author: Daniel Veillard
 *)

{$IFDEF LIBXML_HTML_ENABLED}

{$IFDEF CONST}
(**
 * HTML_TEXT_NODE:
 *
 * Macro. A text node in a HTML document is really implemented
 * the same way as a text node in an XML document.
 *)
  HTML_TEXT_NODE		= XML_TEXT_NODE;
(**
 * HTML_ENTITY_REF_NODE:
 *
 * Macro. An entity reference in a HTML document is really implemented
 * the same way as an entity reference in an XML document.
 *)
  HTML_ENTITY_REF_NODE	= XML_ENTITY_REF_NODE;
(**
 * HTML_COMMENT_NODE:
 *
 * Macro. A comment in a HTML document is really implemented
 * the same way as a comment in an XML document.
 *)
  HTML_COMMENT_NODE	= XML_COMMENT_NODE;
(**
 * HTML_PRESERVE_NODE:
 *
 * Macro. A preserved node in a HTML document is really implemented
 * the same way as a CDATA section in an XML document.
 *)
  HTML_PRESERVE_NODE	= XML_CDATA_SECTION_NODE;
(**
 * HTML_PI_NODE:
 *
 * Macro. A processing instruction in a HTML document is really implemented
 * the same way as a processing instruction in an XML document.
 *)
  HTML_PI_NODE		= XML_PI_NODE;
{$ENDIF}

{$IFDEF FUNCTION}
function htmlNewDoc(URI, ExternalID: xmlCharPtr): htmlDocPtr; XMLCALL; XMLPUBFUN;
function htmlNewDocNoDtD(URI, ExternalID: xmlCharPtr): htmlDocPtr; XMLCALL; XMLPUBFUN;
function htmlGetMetaEncoding(doc: htmlDocPtr): xmlCharPtr; XMLCALL; XMLPUBFUN;
function htmlSetMetaEncoding(doc: htmlDocPtr; encoding: xmlCharPtr): cint; XMLCALL; XMLPUBFUN;
{$IFDEF LIBXML_OUTPUT_ENABLED}
procedure htmlDocDumpMemory(cur: xmlDocPtr; mem: xmlCharPtrPtr; size: pcint); XMLCALL; XMLPUBFUN;
procedure htmlDocDumpMemoryFormat(cur: xmlDocPtr; mem: xmlCharPtrPtr; size: pcint; format: cint); XMLCALL; XMLPUBFUN;
procedure htmlDocDump(f: PFILE; cur: xmlDocPtr); XMLCALL; XMLPUBFUN;
function htmlSaveFile(filename: pchar; doc: htmlDocPtr): cint; XMLCALL; XMLPUBFUN;
function htmlNodeDump(buf: xmlBufferPtr; doc: htmlDocPtr; cur: xmlNodePtr): cint; XMLCALL; XMLPUBFUN;
procedure htmlNodeDumpFile(_out: PFILE; doc: xmlDocPtr; cur: xmlNodePtr); XMLCALL; XMLPUBFUN;
function htmlNodeDumpFileFormat(_out: PFILE; doc: htmlDocPtr; cur: xmlNodePtr; encoding: pchar; format: cint): cint; XMLCALL; XMLPUBFUN;
function htmlSaveFileEnc(filename: pchar; cur: htmlDocPtr; encoding: pchar): cint; XMLCALL; XMLPUBFUN;
function htmlSaveFileFormat(filename: pchar; cur: htmlDocPtr; encoding: pchar; format: cint): cint; XMLCALL; XMLPUBFUN;
procedure htmlNodeDumpFormatOutput(buf: xmlOutputBufferPtr; doc: xmlDocPtr; cur: xmlNodePtr; encoding: pchar; format: cint); XMLCALL; XMLPUBFUN;
procedure htmlDocContentDumpOutput(buf: xmlOutputBufferPtr; cur: xmlNodePtr; encoding: pchar); XMLCALL; XMLPUBFUN;
procedure htmlDocContentDumpFormatOutput(buf: xmlOutputBufferPtr; cur: xmlNodePtr; encoding: pchar; format: cint); XMLCALL; XMLPUBFUN;
procedure htmlNodeDumpOutput(buf: xmlOutputBufferPtr; doc: xmlDocPtr; cur: xmlNodePtr; encoding: pchar); XMLCALL; XMLPUBFUN;
{$ENDIF} (* LIBXML_OUTPUT_ENABLED *)
function htmlIsBooleanAttr(name: xmlCharPtr): cint; XMLCALL; XMLPUBFUN;
{$ENDIF}

{$ENDIF} (* LIBXML_HTML_ENABLED *)
