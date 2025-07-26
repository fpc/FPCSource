(*
 * Summary: the XML document serializer
 * Description: API to save document or subtree of document
 *
 * Copy: See Copyright for the status of this software.
 *
 * Author: Daniel Veillard
 *)

{$IFDEF LIBXML_OUTPUT_ENABLED}

{$IFDEF POINTER}
  xmlSaveCtxtPtr = ^xmlSaveCtxt;
{$ENDIF}

(**
 * xmlSaveOption:
 *
 * This is the set of XML save options that can be passed down
 * to the xmlSaveToFd() and similar calls.
 *)
{$IFDEF TYPE}
  xmlSaveCtxt = record end;

  xmlSaveOption = type cint;
{$ENDIF}

{$IFDEF CONST}
  XML_SAVE_FORMAT     = (1 shl 0);   (* format save output *)
  XML_SAVE_NO_DECL    = (1 shl 1);   (* drop the xml declaration *)
  XML_SAVE_NO_EMPTY   = (1 shl 2);   (* no empty tags *)
  XML_SAVE_NO_XHTML   = (1 shl 3);   (* disable XHTML1 specific rules *)
  XML_SAVE_XHTML      = (1 shl 4);   (* force XHTML1 specific rules *)
  XML_SAVE_AS_XML     = (1 shl 5);   (* force XML serialization on HTML doc *)
  XML_SAVE_AS_HTML    = (1 shl 6);   (* force HTML serialization on XML doc *)
  XML_SAVE_WSNONSIG   = (1 shl 7);   (* format with non-significant whitespace *)
{$ENDIF}

{$IFDEF FUNCTION}
function xmlSaveToFd(fd: cint; encoding: PAnsiChar; options: cint): xmlSaveCtxtPtr; EXTDECL; external xml2lib;
function xmlSaveToFilename(filename: PAnsiChar; encoding: PAnsiChar; options: cint): xmlSaveCtxtPtr; EXTDECL; external xml2lib;
function xmlSaveToBuffer(buffer: xmlBufferPtr; encoding: PAnsiChar; options: cint): xmlSaveCtxtPtr; EXTDECL; external xml2lib;
function xmlSaveToIO(iowrite: xmlOutputWriteCallback; ioclose: xmlOutputCloseCallback; ioctx: pointer; encoding: PAnsiChar; options: cint): xmlSaveCtxtPtr; EXTDECL; external xml2lib;

function xmlSaveDoc(ctxt: xmlSaveCtxtPtr; doc: xmlDocPtr): clong; EXTDECL; external xml2lib;
function xmlSaveTree(ctxt: xmlSaveCtxtPtr; node: xmlNodePtr): clong; EXTDECL; external xml2lib;
function xmlSaveFlush(ctxt: xmlSaveCtxtPtr): cint; EXTDECL; external xml2lib;
function xmlSaveClose(ctxt: xmlSaveCtxtPtr): cint; EXTDECL; external xml2lib;
function xmlSaveSetEscape(ctxt: xmlSaveCtxtPtr; escape: xmlCharEncodingOutputFunc): cint; EXTDECL; external xml2lib;
function xmlSaveSetAttrEscape(ctxt: xmlSaveCtxtPtr; escape: xmlCharEncodingOutputFunc): cint; EXTDECL; external xml2lib;
{$ENDIF}
{$IFDEF FUNCTIONVAR}
  xmlSaveToFd: function(fd: cint; encoding: PAnsiChar; options: cint): xmlSaveCtxtPtr; EXTDECL;
  xmlSaveToFilename: function(filename: PAnsiChar; encoding: PAnsiChar; options: cint): xmlSaveCtxtPtr; EXTDECL;
  xmlSaveToBuffer: function(buffer: xmlBufferPtr; encoding: PAnsiChar; options: cint): xmlSaveCtxtPtr; EXTDECL;
  xmlSaveToIO: function(iowrite: xmlOutputWriteCallback; ioclose: xmlOutputCloseCallback; ioctx: pointer; encoding: PAnsiChar; options: cint): xmlSaveCtxtPtr; EXTDECL;

  xmlSaveDoc: function(ctxt: xmlSaveCtxtPtr; doc: xmlDocPtr): clong; EXTDECL;
  xmlSaveTree: function(ctxt: xmlSaveCtxtPtr; node: xmlNodePtr): clong; EXTDECL;
  xmlSaveFlush: function(ctxt: xmlSaveCtxtPtr): cint; EXTDECL;
  xmlSaveClose: function(ctxt: xmlSaveCtxtPtr): cint; EXTDECL;
  xmlSaveSetEscape: function(ctxt: xmlSaveCtxtPtr; escape: xmlCharEncodingOutputFunc): cint; EXTDECL;
  xmlSaveSetAttrEscape: function(ctxt: xmlSaveCtxtPtr; escape: xmlCharEncodingOutputFunc): cint; EXTDECL;
{$ENDIF}
{$ENDIF} (* LIBXML_OUTPUT_ENABLED *)



