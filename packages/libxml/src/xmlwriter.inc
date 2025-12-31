
(*
 * Summary: text writing API for XML
 * Description: text writing API for XML
 *
 * Copy: See Copyright for the status of this software.
 *
 * Author: Alfred Mickautsch <alfred@mickautsch.de>
 *)

{$IFDEF LIBXML_WRITER_ENABLED}

{$IFDEF POINTER}
  xmlTextWriterPtr = ^xmlTextWriter;
{$ENDIF}

{$IFDEF TYPE}
  xmlTextWriter = record end;
{$ENDIF}

{$IFDEF FUNCTION}
(*
 * Constructors & Destructor
 *)
function xmlNewTextWriter(_out: xmlOutputBufferPtr): xmlTextWriterPtr; EXTDECL; external xml2lib;
function xmlNewTextWriterFilename(uri: PAnsiChar; compression: cint): xmlTextWriterPtr; EXTDECL; external xml2lib;
function xmlNewTextWriterMemory(buf: xmlBufferPtr; compression: cint): xmlTextWriterPtr; EXTDECL; external xml2lib;
function xmlNewTextWriterPushParser(ctxt: xmlParserCtxtPtr; compression: cint): xmlTextWriterPtr; EXTDECL; external xml2lib;
function xmlNewTextWriterDoc(var doc: xmlDocPtr; compression: cint): xmlTextWriterPtr; EXTDECL; external xml2lib;
function xmlNewTextWriterTree(doc: xmlDocPtr; node: xmlNodePtr; compression: cint): xmlTextWriterPtr; EXTDECL; external xml2lib;
procedure xmlFreeTextWriter(writer: xmlTextWriterPtr); EXTDECL; external xml2lib;

(*
 * Functions
 *)

(*
 * Document
 *)
function xmlTextWriterStartDocument(writer: xmlTextWriterPtr; version, encoding, standalone: PAnsiChar): cint; EXTDECL; external xml2lib;
function xmlTextWriterEndDocument(writer: xmlTextWriterPtr): cint; EXTDECL; external xml2lib;

(*
 * Comments
 *)
function xmlTextWriterStartComment(writer: xmlTextWriterPtr): cint; EXTDECL; external xml2lib;
function xmlTextWriterEndComment(writer: xmlTextWriterPtr): cint; EXTDECL; external xml2lib;
function xmlTextWriterWriteFormatComment(writer: xmlTextWriterPtr; format: PAnsiChar; args: array of const): cint; EXTDECL; external xml2lib;
function xmlTextWriterWriteVFormatComment(writer: xmlTextWriterPtr; format: PAnsiChar; argptr: va_list): cint; EXTDECL; external xml2lib;
function xmlTextWriterWriteComment(writer: xmlTextWriterPtr; content: xmlCharPtr): cint; EXTDECL; external xml2lib;

(*
 * Elements
 *)
function xmlTextWriterStartElement(writer: xmlTextWriterPtr; name: xmlCharPtr): cint; EXTDECL; external xml2lib;
function xmlTextWriterStartElementNS(writer: xmlTextWriterPtr; prefix, name, namespaceURI: xmlCharPtr): cint; EXTDECL; external xml2lib;
function xmlTextWriterEndElement(writer: xmlTextWriterPtr): cint; EXTDECL; external xml2lib;
function xmlTextWriterFullEndElement(writer: xmlTextWriterPtr): cint; EXTDECL; external xml2lib;

(*
 * Elements convenience functions
 *)
function xmlTextWriterWriteFormatElement(writer: xmlTextWriterPtr; name: xmlCharPtr; format: PAnsiChar; args: array of const): cint; EXTDECL; external xml2lib;
function xmlTextWriterWriteVFormatElement(writer: xmlTextWriterPtr; name: xmlCharPtr; format: PAnsiChar; argptr: va_list): cint; EXTDECL; external xml2lib;
function xmlTextWriterWriteElement(writer: xmlTextWriterPtr; name, content: xmlCharPtr): cint; EXTDECL; external xml2lib;
function xmlTextWriterWriteFormatElementNS(writer: xmlTextWriterPtr; prefix, name, namespaceURI: xmlCharPtr; format: PAnsiChar; args: array of const): cint; EXTDECL; external xml2lib;
function xmlTextWriterWriteVFormatElementNS(writer: xmlTextWriterPtr; prefix, name, namespaceURI: xmlCharPtr; format: PAnsiChar; argptr: va_list): cint; EXTDECL; external xml2lib;
function xmlTextWriterWriteElementNS(writer: xmlTextWriterPtr; prefix, name, namespaceURI, content: xmlCharPtr): cint; EXTDECL; external xml2lib;

(*
 * Text
 *)
function xmlTextWriterWriteFormatRaw(writer: xmlTextWriterPtr; format: PAnsiChar; args: array of const): cint; EXTDECL; external xml2lib;
function xmlTextWriterWriteVFormatRaw(writer: xmlTextWriterPtr; format: PAnsiChar; argptr: va_list): cint; EXTDECL; external xml2lib;
function xmlTextWriterWriteRawLen(writer: xmlTextWriterPtr; content: xmlCharPtr; len: cint): cint; EXTDECL; external xml2lib;
function xmlTextWriterWriteRaw(writer: xmlTextWriterPtr; content: xmlCharPtr): cint; EXTDECL; external xml2lib;
function xmlTextWriterWriteFormatString(writer: xmlTextWriterPtr; format: PAnsiChar; args: array of const): cint; EXTDECL; external xml2lib;
function xmlTextWriterWriteVFormatString(writer: xmlTextWriterPtr; format: PAnsiChar; argptr: va_list): cint; EXTDECL; external xml2lib;
function xmlTextWriterWriteString(writer: xmlTextWriterPtr; content: xmlCharPtr): cint; EXTDECL; external xml2lib;
function xmlTextWriterWriteBase64(writer: xmlTextWriterPtr; data: PAnsiChar; start, len: cint): cint; EXTDECL; external xml2lib;
function xmlTextWriterWriteBinHex(writer: xmlTextWriterPtr; data: PAnsiChar; start, len: cint): cint; EXTDECL; external xml2lib;

(*
 * Attributes
 *)
function xmlTextWriterStartAttribute(writer: xmlTextWriterPtr; name: xmlCharPtr): cint; EXTDECL; external xml2lib;
function xmlTextWriterStartAttributeNS(writer: xmlTextWriterPtr; prefix, name, namespaceURI: xmlCharPtr): cint; EXTDECL; external xml2lib;
function xmlTextWriterEndAttribute(writer: xmlTextWriterPtr): cint; EXTDECL; external xml2lib;

(*
 * Attributes convenience functions
 *)
function xmlTextWriterWriteFormatAttribute(writer: xmlTextWriterPtr; name: xmlCharPtr; format: PAnsiChar; args: array of const): cint; EXTDECL; external xml2lib;
function xmlTextWriterWriteVFormatAttribute(writer: xmlTextWriterPtr; name: xmlCharPtr; format: PAnsiChar; argptr: va_list): cint; EXTDECL; external xml2lib;
function xmlTextWriterWriteAttribute(writer: xmlTextWriterPtr; name, content: xmlCharPtr): cint; EXTDECL; external xml2lib;
function xmlTextWriterWriteFormatAttributeNS(writer: xmlTextWriterPtr; prefix, name, namespaceURI: xmlCharPtr; format: PAnsiChar; args: array of const): cint; EXTDECL; external xml2lib;
function xmlTextWriterWriteVFormatAttributeNS(writer: xmlTextWriterPtr; prefix, name, namespaceURI: xmlCharPtr; format: PAnsiChar; argptr: va_list): cint; EXTDECL; external xml2lib;
function xmlTextWriterWriteAttributeNS(writer: xmlTextWriterPtr; prefix, name, namespaceURI, content: xmlCharPtr): cint; EXTDECL; external xml2lib;

(*
 * PI's
 *)
function xmlTextWriterStartPI(writer: xmlTextWriterPtr; target: xmlCharPtr): cint; EXTDECL; external xml2lib;
function xmlTextWriterEndPI(writer: xmlTextWriterPtr): cint; EXTDECL; external xml2lib;

(*
 * PI convenience functions
 *)
function xmlTextWriterWriteFormatPI(writer: xmlTextWriterPtr; target: xmlCharPtr; format: PAnsiChar; args: array of const): cint; EXTDECL; external xml2lib;
function xmlTextWriterWriteVFormatPI(writer: xmlTextWriterPtr; target: xmlCharPtr; format: PAnsiChar; argptr: va_list): cint; EXTDECL; external xml2lib;
function xmlTextWriterWritePI(writer: xmlTextWriterPtr; target, content: xmlCharPtr): cint; EXTDECL; external xml2lib;

(**
 * xmlTextWriterWriteProcessingInstruction:
 *
 * This macro maps to xmlTextWriterWritePI
 *)
//function xmlTextWriterWriteProcessingInstruction(writer: xmlTextWriterPtr; target, content: xmlCharPtr): cint; EXTDECL; external xml2lib 'xmlTextWriterWritePI';

(*
 * CDATA
 *)
function xmlTextWriterStartCDATA(writer: xmlTextWriterPtr): cint; EXTDECL; external xml2lib;
function xmlTextWriterEndCDATA(writer: xmlTextWriterPtr): cint; EXTDECL; external xml2lib;

(*
 * CDATA convenience functions
 *)
function xmlTextWriterWriteFormatCDATA(writer: xmlTextWriterPtr; format: PAnsiChar; args: array of const): cint; EXTDECL; external xml2lib;
function xmlTextWriterWriteVFormatCDATA(writer: xmlTextWriterPtr; format: PAnsiChar; argptr: va_list): cint; EXTDECL; external xml2lib;
function xmlTextWriterWriteCDATA(writer: xmlTextWriterPtr; content: xmlCharPtr): cint; EXTDECL; external xml2lib;

(*
 * DTD
 *)
function xmlTextWriterStartDTD(writer: xmlTextWriterPtr; name, pubid, sysid: xmlCharPtr): cint; EXTDECL; external xml2lib;
function xmlTextWriterEndDTD(writer: xmlTextWriterPtr): cint; EXTDECL; external xml2lib;

(*
 * DTD convenience functions
 *)
function xmlTextWriterWriteFormatDTD(writer: xmlTextWriterPtr; name, pubid, sysid: xmlCharPtr; format: PAnsiChar; args: array of const): cint; EXTDECL; external xml2lib;
function xmlTextWriterWriteVFormatDTD(writer: xmlTextWriterPtr; name, pubid, sysid: xmlCharPtr; format: PAnsiChar; argptr: va_list): cint; EXTDECL; external xml2lib;
function xmlTextWriterWriteDTD(writer: xmlTextWriterPtr; name, pubid, sysid, subset: xmlCharPtr): cint; EXTDECL; external xml2lib;

(**
 * xmlTextWriterWriteDocType:
 *
 * this macro maps to xmlTextWriterWriteDTD
 *)
//{$DEFINE xmlTextWriterWriteDocType := xmlTextWriterWriteDTD}

(*
 * DTD element definition
 *)
function xmlTextWriterStartDTDElement(writer: xmlTextWriterPtr; name: xmlCharPtr): cint; EXTDECL; external xml2lib;
function xmlTextWriterEndDTDElement(writer: xmlTextWriterPtr): cint; EXTDECL; external xml2lib;

(*
 * DTD element definition convenience functions
 *)
function xmlTextWriterWriteFormatDTDElement(writer: xmlTextWriterPtr; name: xmlCharPtr; format: PAnsiChar; args: array of const): cint; EXTDECL; external xml2lib;
function xmlTextWriterWriteVFormatDTDElement(writer: xmlTextWriterPtr; name: xmlCharPtr; format: PAnsiChar; argptr: va_list): cint; EXTDECL; external xml2lib;
function xmlTextWriterWriteDTDElement(writer: xmlTextWriterPtr; name, content: xmlCharPtr): cint; EXTDECL; external xml2lib;

(*
 * DTD attribute list definition
 *)
function xmlTextWriterStartDTDAttlist(writer: xmlTextWriterPtr; name: xmlCharPtr): cint; EXTDECL; external xml2lib;
function xmlTextWriterEndDTDAttlist(writer: xmlTextWriterPtr): cint; EXTDECL; external xml2lib;

(*
 * DTD attribute list definition convenience functions
 *)
function xmlTextWriterWriteFormatDTDAttlist(writer: xmlTextWriterPtr; name: xmlCharPtr; format: PAnsiChar; args: array of const): cint; EXTDECL; external xml2lib;
function xmlTextWriterWriteVFormatDTDAttlist(writer: xmlTextWriterPtr; name: xmlCharPtr; format: PAnsiChar; argptr: va_list): cint; EXTDECL; external xml2lib;
function xmlTextWriterWriteDTDAttlist(writer: xmlTextWriterPtr; name, content: xmlCharPtr): cint; EXTDECL; external xml2lib;

(*
 * DTD entity definition
 *)
function xmlTextWriterStartDTDEntity(writer: xmlTextWriterPtr; pe: cint; name: xmlCharPtr): cint; EXTDECL; external xml2lib;
function xmlTextWriterEndDTDEntity(writer: xmlTextWriterPtr): cint; EXTDECL; external xml2lib;

(*
 * DTD entity definition convenience functions
 *)
function xmlTextWriterWriteFormatDTDInternalEntity(writer: xmlTextWriterPtr; pe: cint; name: xmlCharPtr; format: PAnsiChar; args: array of const): cint; EXTDECL; external xml2lib;
function xmlTextWriterWriteVFormatDTDInternalEntity(writer: xmlTextWriterPtr; pe: cint; name: xmlCharPtr; format: PAnsiChar; argptr: va_list): cint; EXTDECL; external xml2lib;
function xmlTextWriterWriteDTDInternalEntity(writer: xmlTextWriterPtr; pe: cint; name, content: xmlCharPtr): cint; EXTDECL; external xml2lib;
function xmlTextWriterWriteDTDExternalEntity(writer: xmlTextWriterPtr; pe: cint; name, pubid, sysid, ndataid: xmlCharPtr): cint; EXTDECL; external xml2lib;
function xmlTextWriterWriteDTDExternalEntityContents(writer: xmlTextWriterPtr; pubid, sysid, ndataid: xmlCharPtr): cint; EXTDECL; external xml2lib;
function xmlTextWriterWriteDTDEntity(writer: xmlTextWriterPtr; pe: cint; name, pubid, sysid, ndataid, content: xmlCharPtr): cint; EXTDECL; external xml2lib;

(*
 * DTD notation definition
 *)
function xmlTextWriterWriteDTDNotation(writer: xmlTextWriterPtr; name, pubid, sysid: xmlCharPtr): cint; EXTDECL; external xml2lib;

(*
 * Indentation
 *)
function xmlTextWriterSetIndent(writer: xmlTextWriterPtr; indent: cint): cint; EXTDECL; external xml2lib;
function xmlTextWriterSetIndentString(writer: xmlTextWriterPtr; str: xmlCharPtr): cint; EXTDECL; external xml2lib;
function xmlTextWriterSetQuoteChar(writer: xmlTextWriterPtr; quotechar: xmlChar): cint; EXTDECL; external xml2lib;

(*
 * misc
 *)
function xmlTextWriterFlush(writer: xmlTextWriterPtr): cint; EXTDECL; external xml2lib;
function xmlTextWriterClose(writer: xmlTextWriterPtr): cint; EXTDECL; external xml2lib;

{$ENDIF}
{$IFDEF FUNCTIONVAR}
(*
 * Constructors & Destructor
 *)
  xmlNewTextWriter: function(_out: xmlOutputBufferPtr): xmlTextWriterPtr; EXTDECL;
  xmlNewTextWriterFilename: function(uri: PAnsiChar; compression: cint): xmlTextWriterPtr; EXTDECL;
  xmlNewTextWriterMemory: function(buf: xmlBufferPtr; compression: cint): xmlTextWriterPtr; EXTDECL;
  xmlNewTextWriterPushParser: function(ctxt: xmlParserCtxtPtr; compression: cint): xmlTextWriterPtr; EXTDECL;
  xmlNewTextWriterDoc: function(var doc: xmlDocPtr; compression: cint): xmlTextWriterPtr; EXTDECL;
  xmlNewTextWriterTree: function(doc: xmlDocPtr; node: xmlNodePtr; compression: cint): xmlTextWriterPtr; EXTDECL;
  xmlFreeTextWriter: procedure(writer: xmlTextWriterPtr); EXTDECL;

(*
 * Functions
 *)

(*
 * Document
 *)
  xmlTextWriterStartDocument: function(writer: xmlTextWriterPtr; version, encoding, standalone: PAnsiChar): cint; EXTDECL;
  xmlTextWriterEndDocument: function(writer: xmlTextWriterPtr): cint; EXTDECL;

(*
 * Comments
 *)
  xmlTextWriterStartComment: function(writer: xmlTextWriterPtr): cint; EXTDECL;
  xmlTextWriterEndComment: function(writer: xmlTextWriterPtr): cint; EXTDECL;
  xmlTextWriterWriteFormatComment: function(writer: xmlTextWriterPtr; format: PAnsiChar; args: array of const): cint; EXTDECL;
  xmlTextWriterWriteVFormatComment: function(writer: xmlTextWriterPtr; format: PAnsiChar; argptr: va_list): cint; EXTDECL;
  xmlTextWriterWriteComment: function(writer: xmlTextWriterPtr; content: xmlCharPtr): cint; EXTDECL;

(*
 * Elements
 *)
  xmlTextWriterStartElement: function(writer: xmlTextWriterPtr; name: xmlCharPtr): cint; EXTDECL;
  xmlTextWriterStartElementNS: function(writer: xmlTextWriterPtr; prefix, name, namespaceURI: xmlCharPtr): cint; EXTDECL;
  xmlTextWriterEndElement: function(writer: xmlTextWriterPtr): cint; EXTDECL;
  xmlTextWriterFullEndElement: function(writer: xmlTextWriterPtr): cint; EXTDECL;

(*
 * Elements convenience functions
 *)
  xmlTextWriterWriteFormatElement: function(writer: xmlTextWriterPtr; name: xmlCharPtr; format: PAnsiChar; args: array of const): cint; EXTDECL;
  xmlTextWriterWriteVFormatElement: function(writer: xmlTextWriterPtr; name: xmlCharPtr; format: PAnsiChar; argptr: va_list): cint; EXTDECL;
  xmlTextWriterWriteElement: function(writer: xmlTextWriterPtr; name, content: xmlCharPtr): cint; EXTDECL;
  xmlTextWriterWriteFormatElementNS: function(writer: xmlTextWriterPtr; prefix, name, namespaceURI: xmlCharPtr; format: PAnsiChar; args: array of const): cint; EXTDECL;
  xmlTextWriterWriteVFormatElementNS: function(writer: xmlTextWriterPtr; prefix, name, namespaceURI: xmlCharPtr; format: PAnsiChar; argptr: va_list): cint; EXTDECL;
  xmlTextWriterWriteElementNS: function(writer: xmlTextWriterPtr; prefix, name, namespaceURI, content: xmlCharPtr): cint; EXTDECL;

(*
 * Text
 *)
  xmlTextWriterWriteFormatRaw: function(writer: xmlTextWriterPtr; format: PAnsiChar; args: array of const): cint; EXTDECL;
  xmlTextWriterWriteVFormatRaw: function(writer: xmlTextWriterPtr; format: PAnsiChar; argptr: va_list): cint; EXTDECL;
  xmlTextWriterWriteRawLen: function(writer: xmlTextWriterPtr; content: xmlCharPtr; len: cint): cint; EXTDECL;
  xmlTextWriterWriteRaw: function(writer: xmlTextWriterPtr; content: xmlCharPtr): cint; EXTDECL;
  xmlTextWriterWriteFormatString: function(writer: xmlTextWriterPtr; format: PAnsiChar; args: array of const): cint; EXTDECL;
  xmlTextWriterWriteVFormatString: function(writer: xmlTextWriterPtr; format: PAnsiChar; argptr: va_list): cint; EXTDECL;
  xmlTextWriterWriteString: function(writer: xmlTextWriterPtr; content: xmlCharPtr): cint; EXTDECL;
  xmlTextWriterWriteBase64: function(writer: xmlTextWriterPtr; data: PAnsiChar; start, len: cint): cint; EXTDECL;
  xmlTextWriterWriteBinHex: function(writer: xmlTextWriterPtr; data: PAnsiChar; start, len: cint): cint; EXTDECL;

(*
 * Attributes
 *)
  xmlTextWriterStartAttribute: function(writer: xmlTextWriterPtr; name: xmlCharPtr): cint; EXTDECL;
  xmlTextWriterStartAttributeNS: function(writer: xmlTextWriterPtr; prefix, name, namespaceURI: xmlCharPtr): cint; EXTDECL;
  xmlTextWriterEndAttribute: function(writer: xmlTextWriterPtr): cint; EXTDECL;

(*
 * Attributes convenience functions
 *)
  xmlTextWriterWriteFormatAttribute: function(writer: xmlTextWriterPtr; name: xmlCharPtr; format: PAnsiChar; args: array of const): cint; EXTDECL;
  xmlTextWriterWriteVFormatAttribute: function(writer: xmlTextWriterPtr; name: xmlCharPtr; format: PAnsiChar; argptr: va_list): cint; EXTDECL;
  xmlTextWriterWriteAttribute: function(writer: xmlTextWriterPtr; name, content: xmlCharPtr): cint; EXTDECL;
  xmlTextWriterWriteFormatAttributeNS: function(writer: xmlTextWriterPtr; prefix, name, namespaceURI: xmlCharPtr; format: PAnsiChar; args: array of const): cint; EXTDECL;
  xmlTextWriterWriteVFormatAttributeNS: function(writer: xmlTextWriterPtr; prefix, name, namespaceURI: xmlCharPtr; format: PAnsiChar; argptr: va_list): cint; EXTDECL;
  xmlTextWriterWriteAttributeNS: function(writer: xmlTextWriterPtr; prefix, name, namespaceURI, content: xmlCharPtr): cint; EXTDECL;

(*
 * PI's
 *)
  xmlTextWriterStartPI: function(writer: xmlTextWriterPtr; target: xmlCharPtr): cint; EXTDECL;
  xmlTextWriterEndPI: function(writer: xmlTextWriterPtr): cint; EXTDECL;

(*
 * PI convenience functions
 *)
  xmlTextWriterWriteFormatPI: function(writer: xmlTextWriterPtr; target: xmlCharPtr; format: PAnsiChar; args: array of const): cint; EXTDECL;
  xmlTextWriterWriteVFormatPI: function(writer: xmlTextWriterPtr; target: xmlCharPtr; format: PAnsiChar; argptr: va_list): cint; EXTDECL;
  xmlTextWriterWritePI: function(writer: xmlTextWriterPtr; target, content: xmlCharPtr): cint; EXTDECL;

(**
 * xmlTextWriterWriteProcessingInstruction:
 *
 * This macro maps to xmlTextWriterWritePI
 *)
//function xmlTextWriterWriteProcessingInstruction(writer: xmlTextWriterPtr; target, content: xmlCharPtr): cint; EXTDECL; external xml2lib 'xmlTextWriterWritePI';

(*
 * CDATA
 *)
  xmlTextWriterStartCDATA: function(writer: xmlTextWriterPtr): cint; EXTDECL;
  xmlTextWriterEndCDATA: function(writer: xmlTextWriterPtr): cint; EXTDECL;

(*
 * CDATA convenience functions
 *)
  xmlTextWriterWriteFormatCDATA: function(writer: xmlTextWriterPtr; format: PAnsiChar; args: array of const): cint; EXTDECL;
  xmlTextWriterWriteVFormatCDATA: function(writer: xmlTextWriterPtr; format: PAnsiChar; argptr: va_list): cint; EXTDECL;
  xmlTextWriterWriteCDATA: function(writer: xmlTextWriterPtr; content: xmlCharPtr): cint; EXTDECL;

(*
 * DTD
 *)
  xmlTextWriterStartDTD: function(writer: xmlTextWriterPtr; name, pubid, sysid: xmlCharPtr): cint; EXTDECL;
  xmlTextWriterEndDTD: function(writer: xmlTextWriterPtr): cint; EXTDECL;

(*
 * DTD convenience functions
 *)
  xmlTextWriterWriteFormatDTD: function(writer: xmlTextWriterPtr; name, pubid, sysid: xmlCharPtr; format: PAnsiChar; args: array of const): cint; EXTDECL;
  xmlTextWriterWriteVFormatDTD: function(writer: xmlTextWriterPtr; name, pubid, sysid: xmlCharPtr; format: PAnsiChar; argptr: va_list): cint; EXTDECL;
  xmlTextWriterWriteDTD: function(writer: xmlTextWriterPtr; name, pubid, sysid, subset: xmlCharPtr): cint; EXTDECL;

(**
 * xmlTextWriterWriteDocType:
 *
 * this macro maps to xmlTextWriterWriteDTD
 *)
//{$DEFINE xmlTextWriterWriteDocType := xmlTextWriterWriteDTD}

(*
 * DTD element definition
 *)
  xmlTextWriterStartDTDElement: function(writer: xmlTextWriterPtr; name: xmlCharPtr): cint; EXTDECL;
  xmlTextWriterEndDTDElement: function(writer: xmlTextWriterPtr): cint; EXTDECL;

(*
 * DTD element definition convenience functions
 *)
  xmlTextWriterWriteFormatDTDElement: function(writer: xmlTextWriterPtr; name: xmlCharPtr; format: PAnsiChar; args: array of const): cint; EXTDECL;
  xmlTextWriterWriteVFormatDTDElement: function(writer: xmlTextWriterPtr; name: xmlCharPtr; format: PAnsiChar; argptr: va_list): cint; EXTDECL;
  xmlTextWriterWriteDTDElement: function(writer: xmlTextWriterPtr; name, content: xmlCharPtr): cint; EXTDECL;

(*
 * DTD attribute list definition
 *)
  xmlTextWriterStartDTDAttlist: function(writer: xmlTextWriterPtr; name: xmlCharPtr): cint; EXTDECL;
  xmlTextWriterEndDTDAttlist: function(writer: xmlTextWriterPtr): cint; EXTDECL;

(*
 * DTD attribute list definition convenience functions
 *)
  xmlTextWriterWriteFormatDTDAttlist: function(writer: xmlTextWriterPtr; name: xmlCharPtr; format: PAnsiChar; args: array of const): cint; EXTDECL;
  xmlTextWriterWriteVFormatDTDAttlist: function(writer: xmlTextWriterPtr; name: xmlCharPtr; format: PAnsiChar; argptr: va_list): cint; EXTDECL;
  xmlTextWriterWriteDTDAttlist: function(writer: xmlTextWriterPtr; name, content: xmlCharPtr): cint; EXTDECL;

(*
 * DTD entity definition
 *)
  xmlTextWriterStartDTDEntity: function(writer: xmlTextWriterPtr; pe: cint; name: xmlCharPtr): cint; EXTDECL;
  xmlTextWriterEndDTDEntity: function(writer: xmlTextWriterPtr): cint; EXTDECL;

(*
 * DTD entity definition convenience functions
 *)
  xmlTextWriterWriteFormatDTDInternalEntity: function(writer: xmlTextWriterPtr; pe: cint; name: xmlCharPtr; format: PAnsiChar; args: array of const): cint; EXTDECL;
  xmlTextWriterWriteVFormatDTDInternalEntity: function(writer: xmlTextWriterPtr; pe: cint; name: xmlCharPtr; format: PAnsiChar; argptr: va_list): cint; EXTDECL;
  xmlTextWriterWriteDTDInternalEntity: function(writer: xmlTextWriterPtr; pe: cint; name, content: xmlCharPtr): cint; EXTDECL;
  xmlTextWriterWriteDTDExternalEntity: function(writer: xmlTextWriterPtr; pe: cint; name, pubid, sysid, ndataid: xmlCharPtr): cint; EXTDECL;
  xmlTextWriterWriteDTDExternalEntityContents: function(writer: xmlTextWriterPtr; pubid, sysid, ndataid: xmlCharPtr): cint; EXTDECL;
  xmlTextWriterWriteDTDEntity: function(writer: xmlTextWriterPtr; pe: cint; name, pubid, sysid, ndataid, content: xmlCharPtr): cint; EXTDECL;

(*
 * DTD notation definition
 *)
  xmlTextWriterWriteDTDNotation: function(writer: xmlTextWriterPtr; name, pubid, sysid: xmlCharPtr): cint; EXTDECL;

(*
 * Indentation
 *)
  xmlTextWriterSetIndent: function(writer: xmlTextWriterPtr; indent: cint): cint; EXTDECL;
  xmlTextWriterSetIndentString: function(writer: xmlTextWriterPtr; str: xmlCharPtr): cint; EXTDECL;
  xmlTextWriterSetQuoteChar: function(writer: xmlTextWriterPtr; quotechar: xmlChar): cint; EXTDECL;

(*
 * misc
 *)
  xmlTextWriterFlush: function(writer: xmlTextWriterPtr): cint; EXTDECL;
  xmlTextWriterClose: function(writer: xmlTextWriterPtr): cint; EXTDECL;

{$ENDIF}
{$ENDIF} (* LIBXML_WRITER_ENABLED *)
