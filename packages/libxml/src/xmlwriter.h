
(*
 * Summary: text writing API for XML
 * Description: text writing API for XML
 *
 * Copy: See Copyright for the status of this software.
 *
 * Author: Alfred Mickautsch <alfred@mickautsch.de>
 *)

{$IFDEF LIBXML_WRITER_ENABLED}

{$IFDEF TYPE}
  xmlTextWriter = record
  end;
{$ENDIF}

{$IFDEF FUNCTION}
(*
 * Constructors & Destructor
 *)
function xmlNewTextWriter(_out: xmlOutputBufferPtr): xmlTextWriterPtr; cdecl; external;
function xmlNewTextWriterFilename(uri: pchar; compression: cint): xmlTextWriterPtr; cdecl; external;
function xmlNewTextWriterMemory(buf: xmlBufferPtr; compression: cint): xmlTextWriterPtr; cdecl; external;
function xmlNewTextWriterPushParser(ctxt: xmlParserCtxtPtr; compression: cint): xmlTextWriterPtr; cdecl; external;
function xmlNewTextWriterDoc(var doc: xmlDocPtr; compression: cint): xmlTextWriterPtr; cdecl; external;
function xmlNewTextWriterTree(doc: xmlDocPtr; node: xmlNodePtr; compression: cint): xmlTextWriterPtr; cdecl; external;
procedure xmlFreeTextWriter(writer: xmlTextWriterPtr); cdecl; external;

(*
 * Functions
 *)

(*
 * Document
 *)
function xmlTextWriterStartDocument(writer: xmlTextWriterPtr; version, encoding, standalone: pchar): cint; cdecl; external;
function xmlTextWriterEndDocument(writer: xmlTextWriterPtr): cint; cdecl; external;

(*
 * Comments
 *)
function xmlTextWriterStartComment(writer: xmlTextWriterPtr): cint; cdecl; external;
function xmlTextWriterEndComment(writer: xmlTextWriterPtr): cint; cdecl; external;
function xmlTextWriterWriteFormatComment(writer: xmlTextWriterPtr; format: pchar; args: array of const): cint; cdecl; external;
function xmlTextWriterWriteVFormatComment(writer: xmlTextWriterPtr; format: pchar; argptr: va_list): cint; cdecl; external;
function xmlTextWriterWriteComment(writer: xmlTextWriterPtr; content: xmlCharPtr): cint; cdecl; external;

(*
 * Elements
 *)
function xmlTextWriterStartElement(writer: xmlTextWriterPtr; name: xmlCharPtr): cint; cdecl; external;
function xmlTextWriterStartElementNS(writer: xmlTextWriterPtr; prefix, name, namespaceURI: xmlCharPtr): cint; cdecl; external;
function xmlTextWriterEndElement(writer: xmlTextWriterPtr): cint; cdecl; external;
function xmlTextWriterFullEndElement(writer: xmlTextWriterPtr): cint; cdecl; external;

(*
 * Elements conveniency functions
 *)
function xmlTextWriterWriteFormatElement(writer: xmlTextWriterPtr; name: xmlCharPtr; format: pchar; args: array of const): cint; cdecl; external;
function xmlTextWriterWriteVFormatElement(writer: xmlTextWriterPtr; name: xmlCharPtr; format: pchar; argptr: va_list): cint; cdecl; external;
function xmlTextWriterWriteElement(writer: xmlTextWriterPtr; name, content: xmlCharPtr): cint; cdecl; external;
function xmlTextWriterWriteFormatElementNS(writer: xmlTextWriterPtr; prefix, name, namespaceURI: xmlCharPtr; format: pchar; args: array of const): cint; cdecl; external;
function xmlTextWriterWriteVFormatElementNS(writer: xmlTextWriterPtr; prefix, name, namespaceURI: xmlCharPtr; format: pchar; argptr: va_list): cint; cdecl; external;
function xmlTextWriterWriteElementNS(writer: xmlTextWriterPtr; prefix, name, namespaceURI, content: xmlCharPtr): cint; cdecl; external;

(*
 * Text
 *)
function xmlTextWriterWriteFormatRaw(writer: xmlTextWriterPtr; format: pchar; args: array of const): cint; cdecl; external;
function xmlTextWriterWriteVFormatRaw(writer: xmlTextWriterPtr; format: pchar; argptr: va_list): cint; cdecl; external;
function xmlTextWriterWriteRawLen(writer: xmlTextWriterPtr; content: xmlCharPtr; len: cint): cint; cdecl; external;
function xmlTextWriterWriteRaw(writer: xmlTextWriterPtr; content: xmlCharPtr): cint; cdecl; external;
function xmlTextWriterWriteFormatString(writer: xmlTextWriterPtr; format: pchar; args: array of const): cint; cdecl; external;
function xmlTextWriterWriteVFormatString(writer: xmlTextWriterPtr; format: pchar; argptr: va_list): cint; cdecl; external;
function xmlTextWriterWriteString(writer: xmlTextWriterPtr; content: xmlCharPtr): cint; cdecl; external;
function xmlTextWriterWriteBase64(writer: xmlTextWriterPtr; data: pchar; start, len: cint): cint; cdecl; external;
function xmlTextWriterWriteBinHex(writer: xmlTextWriterPtr; data: pchar; start, len: cint): cint; cdecl; external;

(*
 * Attributes
 *)
function xmlTextWriterStartAttribute(writer: xmlTextWriterPtr; name: xmlCharPtr): cint; cdecl; external;
function xmlTextWriterStartAttributeNS(writer: xmlTextWriterPtr; prefix, name, namespaceURI: xmlCharPtr): cint; cdecl; external;
function xmlTextWriterEndAttribute(writer: xmlTextWriterPtr): cint; cdecl; external;

(*
 * Attributes conveniency functions
 *)
function xmlTextWriterWriteFormatAttribute(writer: xmlTextWriterPtr; name: xmlCharPtr; format: pchar; args: array of const): cint; cdecl; external;
function xmlTextWriterWriteVFormatAttribute(writer: xmlTextWriterPtr; name: xmlCharPtr; format: pchar; argptr: va_list): cint; cdecl; external;
function xmlTextWriterWriteAttribute(writer: xmlTextWriterPtr; name, content: xmlCharPtr): cint; cdecl; external;
function xmlTextWriterWriteFormatAttributeNS(writer: xmlTextWriterPtr; prefix, name, namespaceURI: xmlCharPtr; format: pchar; args: array of const): cint; cdecl; external;
function xmlTextWriterWriteVFormatAttributeNS(writer: xmlTextWriterPtr; prefix, name, namespaceURI: xmlCharPtr; format: pchar; argptr: va_list): cint; cdecl; external;
function xmlTextWriterWriteAttributeNS(writer: xmlTextWriterPtr; prefix, name, namespaceURI, content: xmlCharPtr): cint; cdecl; external;

(*
 * PI's
 *)
function xmlTextWriterStartPI(writer: xmlTextWriterPtr; target: xmlCharPtr): cint; cdecl; external;
function xmlTextWriterEndPI(writer: xmlTextWriterPtr): cint; cdecl; external;

(*
 * PI conveniency functions
 *)
function xmlTextWriterWriteFormatPI(writer: xmlTextWriterPtr; target: xmlCharPtr; format: pchar; args: array of const): cint; cdecl; external;
function xmlTextWriterWriteVFormatPI(writer: xmlTextWriterPtr; target: xmlCharPtr; format: pchar; argptr: va_list): cint; cdecl; external;
function xmlTextWriterWritePI(writer: xmlTextWriterPtr; target, content: xmlCharPtr): cint; cdecl; external;

(**
 * xmlTextWriterWriteProcessingInstruction:
 *
 * This macro maps to xmlTextWriterWritePI
 *)
{$DEFINE xmlTextWriterWriteProcessingInstruction := xmlTextWriterWritePI}

(*
 * CDATA
 *)
function xmlTextWriterStartCDATA(writer: xmlTextWriterPtr): cint; cdecl; external;
function xmlTextWriterEndCDATA(writer: xmlTextWriterPtr): cint; cdecl; external;

(*
 * CDATA conveniency functions
 *)
function xmlTextWriterWriteFormatCDATA(writer: xmlTextWriterPtr; format: pchar; args: array of const): cint; cdecl; external;
function xmlTextWriterWriteVFormatCDATA(writer: xmlTextWriterPtr; format: pchar; argptr: va_list): cint; cdecl; external;
function xmlTextWriterWriteCDATA(writer: xmlTextWriterPtr; content: xmlCharPtr): cint; cdecl; external;

(*
 * DTD
 *)
function xmlTextWriterStartDTD(writer: xmlTextWriterPtr; name, pubid, sysid: xmlCharPtr): cint; cdecl; external;
function xmlTextWriterEndDTD(writer: xmlTextWriterPtr): cint; cdecl; external;

(*
 * DTD conveniency functions
 *)
function xmlTextWriterWriteFormatDTD(writer: xmlTextWriterPtr; name, pubid, sysid: xmlCharPtr; format: pchar; args: array of const): cint; cdecl; external;
function xmlTextWriterWriteVFormatDTD(writer: xmlTextWriterPtr; name, pubid, sysid: xmlCharPtr; format: pchar; argptr: va_list): cint; cdecl; external;
function xmlTextWriterWriteDTD(writer: xmlTextWriterPtr; name, pubid, sysid, subset: xmlCharPtr): cint; cdecl; external;

(**
 * xmlTextWriterWriteDocType:
 *
 * this macro maps to xmlTextWriterWriteDTD
 *)
{$DEFINE xmlTextWriterWriteDocType := xmlTextWriterWriteDTD}

(*
 * DTD element definition
 *)
function xmlTextWriterStartDTDElement(writer: xmlTextWriterPtr; name: xmlCharPtr): cint; cdecl; external;
function xmlTextWriterEndDTDElement(writer: xmlTextWriterPtr): cint; cdecl; external;

(*
 * DTD element definition conveniency functions
 *)
function xmlTextWriterWriteFormatDTDElement(writer: xmlTextWriterPtr; name: xmlCharPtr; format: pchar; args: array of const): cint; cdecl; external;
function xmlTextWriterWriteVFormatDTDElement(writer: xmlTextWriterPtr; name: xmlCharPtr; format: pchar; argptr: va_list): cint; cdecl; external;
function xmlTextWriterWriteDTDElement(writer: xmlTextWriterPtr; name, content: xmlCharPtr): cint; cdecl; external;

(*
 * DTD attribute list definition
 *)
function xmlTextWriterStartDTDAttlist(writer: xmlTextWriterPtr; name: xmlCharPtr): cint; cdecl; external;
function xmlTextWriterEndDTDAttlist(writer: xmlTextWriterPtr): cint; cdecl; external;

(*
 * DTD attribute list definition conveniency functions
 *)
function xmlTextWriterWriteFormatDTDAttlist(writer: xmlTextWriterPtr; name: xmlCharPtr; format: pchar; args: array of const): cint; cdecl; external;
function xmlTextWriterWriteVFormatDTDAttlist(writer: xmlTextWriterPtr; name: xmlCharPtr; format: pchar; argptr: va_list): cint; cdecl; external;
function xmlTextWriterWriteDTDAttlist(writer: xmlTextWriterPtr; name, content: xmlCharPtr): cint; cdecl; external;

(*
 * DTD entity definition
 *)
function xmlTextWriterStartDTDEntity(writer: xmlTextWriterPtr; pe: cint; name: xmlCharPtr): cint; cdecl; external;
function xmlTextWriterEndDTDEntity(writer: xmlTextWriterPtr): cint; cdecl; external;

(*
 * DTD entity definition conveniency functions
 *)
function xmlTextWriterWriteFormatDTDInternalEntity(writer: xmlTextWriterPtr; pe: cint; name: xmlCharPtr; format: pchar; args: array of const): cint; cdecl; external;
function xmlTextWriterWriteVFormatDTDInternalEntity(writer: xmlTextWriterPtr; pe: cint; name: xmlCharPtr; format: pchar; argptr: va_list): cint; cdecl; external;
function xmlTextWriterWriteDTDInternalEntity(writer: xmlTextWriterPtr; pe: cint; name, content: xmlCharPtr): cint; cdecl; external;
function xmlTextWriterWriteDTDExternalEntity(writer: xmlTextWriterPtr; pe: cint; name, pubid, sysid, ndataid: xmlCharPtr): cint; cdecl; external;
function xmlTextWriterWriteDTDExternalEntityContents(writer: xmlTextWriterPtr; pubid, sysid, ndataid: xmlCharPtr): cint; cdecl; external;
function xmlTextWriterWriteDTDEntity(writer: xmlTextWriterPtr; pe: cint; name, pubid, sysid, ndataid, content: xmlCharPtr): cint; cdecl; external;

(*
 * DTD notation definition
 *)
function xmlTextWriterWriteDTDNotation(writer: xmlTextWriterPtr; name, pubid, sysid: xmlCharPtr): cint; cdecl; external;

(*
 * Indentation
 *)
function xmlTextWriterSetIndent(writer: xmlTextWriterPtr; indent: cint): cint; cdecl; external;
function xmlTextWriterSetIndentString(writer: xmlTextWriterPtr; str: xmlCharPtr): cint; cdecl; external;

(*
 * misc
 *)
function xmlTextWriterFlush(writer: xmlTextWriterPtr): cint; cdecl; external;

{$ENDIF}
{$ENDIF} (* LIBXML_WRITER_ENABLED *)
