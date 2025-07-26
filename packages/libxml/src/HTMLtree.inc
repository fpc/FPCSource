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
function htmlNewDoc(URI, ExternalID: xmlCharPtr): htmlDocPtr; EXTDECL; external xml2lib;
function htmlNewDocNoDtD(URI, ExternalID: xmlCharPtr): htmlDocPtr; EXTDECL; external xml2lib;
function htmlGetMetaEncoding(doc: htmlDocPtr): xmlCharPtr; EXTDECL; external xml2lib;
function htmlSetMetaEncoding(doc: htmlDocPtr; encoding: xmlCharPtr): cint; EXTDECL; external xml2lib;
{$IFDEF LIBXML_OUTPUT_ENABLED}
procedure htmlDocDumpMemory(cur: xmlDocPtr; mem: xmlCharPtrPtr; size: pcint); EXTDECL; external xml2lib;
procedure htmlDocDumpMemoryFormat(cur: xmlDocPtr; mem: xmlCharPtrPtr; size: pcint; format: cint); EXTDECL; external xml2lib;
procedure htmlDocDump(f: PFILE; cur: xmlDocPtr); EXTDECL; external xml2lib;
function htmlSaveFile(filename: PAnsiChar; doc: htmlDocPtr): cint; EXTDECL; external xml2lib;
function htmlNodeDump(buf: xmlBufferPtr; doc: htmlDocPtr; cur: xmlNodePtr): cint; EXTDECL; external xml2lib;
procedure htmlNodeDumpFile(_out: PFILE; doc: xmlDocPtr; cur: xmlNodePtr); EXTDECL; external xml2lib;
function htmlNodeDumpFileFormat(_out: PFILE; doc: htmlDocPtr; cur: xmlNodePtr; encoding: PAnsiChar; format: cint): cint; EXTDECL; external xml2lib;
function htmlSaveFileEnc(filename: PAnsiChar; cur: htmlDocPtr; encoding: PAnsiChar): cint; EXTDECL; external xml2lib;
function htmlSaveFileFormat(filename: PAnsiChar; cur: htmlDocPtr; encoding: PAnsiChar; format: cint): cint; EXTDECL; external xml2lib;
procedure htmlNodeDumpFormatOutput(buf: xmlOutputBufferPtr; doc: xmlDocPtr; cur: xmlNodePtr; encoding: PAnsiChar; format: cint); EXTDECL; external xml2lib;
procedure htmlDocContentDumpOutput(buf: xmlOutputBufferPtr; cur: xmlNodePtr; encoding: PAnsiChar); EXTDECL; external xml2lib;
procedure htmlDocContentDumpFormatOutput(buf: xmlOutputBufferPtr; cur: xmlNodePtr; encoding: PAnsiChar; format: cint); EXTDECL; external xml2lib;
procedure htmlNodeDumpOutput(buf: xmlOutputBufferPtr; doc: xmlDocPtr; cur: xmlNodePtr; encoding: PAnsiChar); EXTDECL; external xml2lib;
{$ENDIF} (* LIBXML_OUTPUT_ENABLED *)
function htmlIsBooleanAttr(name: xmlCharPtr): cint; EXTDECL; external xml2lib;
{$ENDIF}

{$IFDEF FUNCTIONVAR}
  htmlNewDoc: function(URI, ExternalID: xmlCharPtr): htmlDocPtr; EXTDECL;
  htmlNewDocNoDtD: function(URI, ExternalID: xmlCharPtr): htmlDocPtr; EXTDECL;
  htmlGetMetaEncoding: function(doc: htmlDocPtr): xmlCharPtr; EXTDECL;
  htmlSetMetaEncoding: function(doc: htmlDocPtr; encoding: xmlCharPtr): cint; EXTDECL;
{$IFDEF LIBXML_OUTPUT_ENABLED}
  htmlDocDumpMemory: procedure(cur: xmlDocPtr; mem: xmlCharPtrPtr; size: pcint); EXTDECL;
  htmlDocDumpMemoryFormat: procedure(cur: xmlDocPtr; mem: xmlCharPtrPtr; size: pcint; format: cint); EXTDECL;
  htmlDocDump: procedure(f: PFILE; cur: xmlDocPtr); EXTDECL;
  htmlSaveFile: function(filename: PAnsiChar; doc: htmlDocPtr): cint; EXTDECL;
  htmlNodeDump: function(buf: xmlBufferPtr; doc: htmlDocPtr; cur: xmlNodePtr): cint; EXTDECL;
  htmlNodeDumpFile: procedure(_out: PFILE; doc: xmlDocPtr; cur: xmlNodePtr); EXTDECL;
  htmlNodeDumpFileFormat: function(_out: PFILE; doc: htmlDocPtr; cur: xmlNodePtr; encoding: PAnsiChar; format: cint): cint; EXTDECL;
  htmlSaveFileEnc: function(filename: PAnsiChar; cur: htmlDocPtr; encoding: PAnsiChar): cint; EXTDECL;
  htmlSaveFileFormat: function(filename: PAnsiChar; cur: htmlDocPtr; encoding: PAnsiChar; format: cint): cint; EXTDECL;
  htmlNodeDumpFormatOutput: procedure(buf: xmlOutputBufferPtr; doc: xmlDocPtr; cur: xmlNodePtr; encoding: PAnsiChar; format: cint); EXTDECL;
  htmlDocContentDumpOutput: procedure(buf: xmlOutputBufferPtr; cur: xmlNodePtr; encoding: PAnsiChar); EXTDECL;
  htmlDocContentDumpFormatOutput: procedure(buf: xmlOutputBufferPtr; cur: xmlNodePtr; encoding: PAnsiChar; format: cint); EXTDECL;
  htmlNodeDumpOutput: procedure(buf: xmlOutputBufferPtr; doc: xmlDocPtr; cur: xmlNodePtr; encoding: PAnsiChar); EXTDECL;
{$ENDIF} (* LIBXML_OUTPUT_ENABLED *)
  htmlIsBooleanAttr: function(name: xmlCharPtr): cint; EXTDECL;
{$ENDIF}

{$ENDIF} (* LIBXML_HTML_ENABLED *)
