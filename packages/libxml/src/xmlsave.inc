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
  XML_SAVE_FORMAT     = 1 shl 0;	(* format save output *)
  XML_SAVE_NO_DECL    = 1 shl 1;	(* drop the xml declaration *)
  XML_SAVE_NO_EMPTY	= 1 shl 2; (* no empty tags *)
  XML_SAVE_NO_XHTML	= 1 shl 3;  (* disable XHTML1 specific rules *)
{$ENDIF}

{$IFDEF FUNCTION}
function xmlSaveToFd(fd: cint; encoding: pchar; options: cint): xmlSaveCtxtPtr; EXTDECL; external xml2lib;
function xmlSaveToFilename(filename: pchar; encoding: pchar; options: cint): xmlSaveCtxtPtr; EXTDECL; external xml2lib;
function xmlSaveToBuffer(buffer: xmlBufferPtr; encoding: pchar; options: cint): xmlSaveCtxtPtr; EXTDECL; external xml2lib;
function xmlSaveToIO(iowrite: xmlOutputWriteCallback; ioclose: xmlOutputCloseCallback; ioctx: pointer; encoding: pchar; options: cint): xmlSaveCtxtPtr; EXTDECL; external xml2lib;

function xmlSaveDoc(ctxt: xmlSaveCtxtPtr; doc: xmlDocPtr): clong; EXTDECL; external xml2lib;
function xmlSaveTree(ctxt: xmlSaveCtxtPtr; node: xmlNodePtr): clong; EXTDECL; external xml2lib;
function xmlSaveFlush(ctxt: xmlSaveCtxtPtr): cint; EXTDECL; external xml2lib;
function xmlSaveClose(ctxt: xmlSaveCtxtPtr): cint; EXTDECL; external xml2lib;
function xmlSaveSetEscape(ctxt: xmlSaveCtxtPtr; escape: xmlCharEncodingOutputFunc): cint; EXTDECL; external xml2lib;
function xmlSaveSetAttrEscape(ctxt: xmlSaveCtxtPtr; escape: xmlCharEncodingOutputFunc): cint; EXTDECL; external xml2lib;
{$ENDIF}
{$ENDIF} (* LIBXML_OUTPUT_ENABLED *)



