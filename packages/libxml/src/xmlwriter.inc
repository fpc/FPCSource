
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
function xmlNewTextWriterFilename(uri: pchar; compression: cint): xmlTextWriterPtr; EXTDECL; external xml2lib;
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
function xmlTextWriterStartDocument(writer: xmlTextWriterPtr; version, encoding, standalone: pchar): cint; EXTDECL; external xml2lib;
function xmlTextWriterEndDocument(writer: xmlTextWriterPtr): cint; EXTDECL; external xml2lib;

(*
 * Comments
 *)
function xmlTextWriterStartComment(writer: xmlTextWriterPtr): cint; EXTDECL; external xml2lib;
function xmlTextWriterEndComment(writer: xmlTextWriterPtr): cint; EXTDECL; external xml2lib;
function xmlTextWriterWriteFormatComment(writer: xmlTextWriterPtr; format: pchar; args: array of const): cint; EXTDECL; external xml2lib;
function xmlTextWriterWriteVFormatComment(writer: xmlTextWriterPtr; format: pchar; argptr: va_list): cint; EXTDECL; external xml2lib;
function xmlTextWriterWriteComment(writer: xmlTextWriterPtr; content: xmlCharPtr): cint; EXTDECL; external xml2lib;

(*
 * Elements
 *)
function xmlTextWriterStartElement(writer: xmlTextWriterPtr; name: xmlCharPtr): cint; EXTDECL; external xml2lib;
function xmlTextWriterStartElementNS(writer: xmlTextWriterPtr; prefix, name, namespaceURI: xmlCharPtr): cint; EXTDECL; external xml2lib;
function xmlTextWriterEndElement(writer: xmlTextWriterPtr): cint; EXTDECL; external xml2lib;
function xmlTextWriterFullEndElement(writer: xmlTextWriterPtr): cint; EXTDECL; external xml2lib;

(*
 * Elements conveniency functions
 *)
function xmlTextWriterWriteFormatElement(writer: xmlTextWriterPtr; name: xmlCharPtr; format: pchar; args: array of const): cint; EXTDECL; external xml2lib;
function xmlTextWriterWriteVFormatElement(writer: xmlTextWriterPtr; name: xmlCharPtr; format: pchar; argptr: va_list): cint; EXTDECL; external xml2lib;
function xmlTextWriterWriteElement(writer: xmlTextWriterPtr; name, content: xmlCharPtr): cint; EXTDECL; external xml2lib;
function xmlTextWriterWriteFormatElementNS(writer: xmlTextWriterPtr; prefix, name, namespaceURI: xmlCharPtr; format: pchar; args: array of const): cint; EXTDECL; external xml2lib;
function xmlTextWriterWriteVFormatElementNS(writer: xmlTextWriterPtr; prefix, name, namespaceURI: xmlCharPtr; format: pchar; argptr: va_list): cint; EXTDECL; external xml2lib;
function xmlTextWriterWriteElementNS(writer: xmlTextWriterPtr; prefix, name, namespaceURI, content: xmlCharPtr): cint; EXTDECL; external xml2lib;

(*
 * Text
 *)
function xmlTextWriterWriteFormatRaw(writer: xmlTextWriterPtr; format: pchar; args: array of const): cint; EXTDECL; external xml2lib;
function xmlTextWriterWriteVFormatRaw(writer: xmlTextWriterPtr; format: pchar; argptr: va_list): cint; EXTDECL; external xml2lib;
function xmlTextWriterWriteRawLen(writer: xmlTextWriterPtr; content: xmlCharPtr; len: cint): cint; EXTDECL; external xml2lib;
function xmlTextWriterWriteRaw(writer: xmlTextWriterPtr; content: xmlCharPtr): cint; EXTDECL; external xml2lib;
function xmlTextWriterWriteFormatString(writer: xmlTextWriterPtr; format: pchar; args: array of const): cint; EXTDECL; external xml2lib;
function xmlTextWriterWriteVFormatString(writer: xmlTextWriterPtr; format: pchar; argptr: va_list): cint; EXTDECL; external xml2lib;
function xmlTextWriterWriteString(writer: xmlTextWriterPtr; content: xmlCharPtr): cint; EXTDECL; external xml2lib;
function xmlTextWriterWriteBase64(writer: xmlTextWriterPtr; data: pchar; start, len: cint): cint; EXTDECL; external xml2lib;
function xmlTextWriterWriteBinHex(writer: xmlTextWriterPtr; data: pchar; start, len: cint): cint; EXTDECL; external xml2lib;

(*
 * Attributes
 *)
function xmlTextWriterStartAttribute(writer: xmlTextWriterPtr; name: xmlCharPtr): cint; EXTDECL; external xml2lib;
function xmlTextWriterStartAttributeNS(writer: xmlTextWriterPtr; prefix, name, namespaceURI: xmlCharPtr): cint; EXTDECL; external xml2lib;
function xmlTextWriterEndAttribute(writer: xmlTextWriterPtr): cint; EXTDECL; external xml2lib;

(*
 * Attributes conveniency functions
 *)
function xmlTextWriterWriteFormatAttribute(writer: xmlTextWriterPtr; name: xmlCharPtr; format: pchar; args: array of const): cint; EXTDECL; external xml2lib;
function xmlTextWriterWriteVFormatAttribute(writer: xmlTextWriterPtr; name: xmlCharPtr; format: pchar; argptr: va_list): cint; EXTDECL; external xml2lib;
function xmlTextWriterWriteAttribute(writer: xmlTextWriterPtr; name, content: xmlCharPtr): cint; EXTDECL; external xml2lib;
function xmlTextWriterWriteFormatAttributeNS(writer: xmlTextWriterPtr; prefix, name, namespaceURI: xmlCharPtr; format: pchar; args: array of const): cint; EXTDECL; external xml2lib;
function xmlTextWriterWriteVFormatAttributeNS(writer: xmlTextWriterPtr; prefix, name, namespaceURI: xmlCharPtr; format: pchar; argptr: va_list): cint; EXTDECL; external xml2lib;
function xmlTextWriterWriteAttributeNS(writer: xmlTextWriterPtr; prefix, name, namespaceURI, content: xmlCharPtr): cint; EXTDECL; external xml2lib;

(*
 * PI's
 *)
function xmlTextWriterStartPI(writer: xmlTextWriterPtr; target: xmlCharPtr): cint; EXTDECL; external xml2lib;
function xmlTextWriterEndPI(writer: xmlTextWriterPtr): cint; EXTDECL; external xml2lib;

(*
 * PI conveniency functions
 *)
function xmlTextWriterWriteFormatPI(writer: xmlTextWriterPtr; target: xmlCharPtr; format: pchar; args: array of const): cint; EXTDECL; external xml2lib;
function xmlTextWriterWriteVFormatPI(writer: xmlTextWriterPtr; target: xmlCharPtr; format: pchar; argptr: va_list): cint; EXTDECL; external xml2lib;
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
 * CDATA conveniency functions
 *)
function xmlTextWriterWriteFormatCDATA(writer: xmlTextWriterPtr; format: pchar; args: array of const): cint; EXTDECL; external xml2lib;
function xmlTextWriterWriteVFormatCDATA(writer: xmlTextWriterPtr; format: pchar; argptr: va_list): cint; EXTDECL; external xml2lib;
function xmlTextWriterWriteCDATA(writer: xmlTextWriterPtr; content: xmlCharPtr): cint; EXTDECL; external xml2lib;

(*
 * DTD
 *)
function xmlTextWriterStartDTD(writer: xmlTextWriterPtr; name, pubid, sysid: xmlCharPtr): cint; EXTDECL; external xml2lib;
function xmlTextWriterEndDTD(writer: xmlTextWriterPtr): cint; EXTDECL; external xml2lib;

(*
 * DTD conveniency functions
 *)
function xmlTextWriterWriteFormatDTD(writer: xmlTextWriterPtr; name, pubid, sysid: xmlCharPtr; format: pchar; args: array of const): cint; EXTDECL; external xml2lib;
function xmlTextWriterWriteVFormatDTD(writer: xmlTextWriterPtr; name, pubid, sysid: xmlCharPtr; format: pchar; argptr: va_list): cint; EXTDECL; external xml2lib;
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
 * DTD element definition conveniency functions
 *)
function xmlTextWriterWriteFormatDTDElement(writer: xmlTextWriterPtr; name: xmlCharPtr; format: pchar; args: array of const): cint; EXTDECL; external xml2lib;
function xmlTextWriterWriteVFormatDTDElement(writer: xmlTextWriterPtr; name: xmlCharPtr; format: pchar; argptr: va_list): cint; EXTDECL; external xml2lib;
function xmlTextWriterWriteDTDElement(writer: xmlTextWriterPtr; name, content: xmlCharPtr): cint; EXTDECL; external xml2lib;

(*
 * DTD attribute list definition
 *)
function xmlTextWriterStartDTDAttlist(writer: xmlTextWriterPtr; name: xmlCharPtr): cint; EXTDECL; external xml2lib;
function xmlTextWriterEndDTDAttlist(writer: xmlTextWriterPtr): cint; EXTDECL; external xml2lib;

(*
 * DTD attribute list definition conveniency functions
 *)
function xmlTextWriterWriteFormatDTDAttlist(writer: xmlTextWriterPtr; name: xmlCharPtr; format: pchar; args: array of const): cint; EXTDECL; external xml2lib;
function xmlTextWriterWriteVFormatDTDAttlist(writer: xmlTextWriterPtr; name: xmlCharPtr; format: pchar; argptr: va_list): cint; EXTDECL; external xml2lib;
function xmlTextWriterWriteDTDAttlist(writer: xmlTextWriterPtr; name, content: xmlCharPtr): cint; EXTDECL; external xml2lib;

(*
 * DTD entity definition
 *)
function xmlTextWriterStartDTDEntity(writer: xmlTextWriterPtr; pe: cint; name: xmlCharPtr): cint; EXTDECL; external xml2lib;
function xmlTextWriterEndDTDEntity(writer: xmlTextWriterPtr): cint; EXTDECL; external xml2lib;

(*
 * DTD entity definition conveniency functions
 *)
function xmlTextWriterWriteFormatDTDInternalEntity(writer: xmlTextWriterPtr; pe: cint; name: xmlCharPtr; format: pchar; args: array of const): cint; EXTDECL; external xml2lib;
function xmlTextWriterWriteVFormatDTDInternalEntity(writer: xmlTextWriterPtr; pe: cint; name: xmlCharPtr; format: pchar; argptr: va_list): cint; EXTDECL; external xml2lib;
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

(*
 * misc
 *)
function xmlTextWriterFlush(writer: xmlTextWriterPtr): cint; EXTDECL; external xml2lib;

{$ENDIF}
{$ENDIF} (* LIBXML_WRITER_ENABLED *)
