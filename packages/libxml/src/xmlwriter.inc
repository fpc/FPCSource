
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
function xmlNewTextWriter(_out: xmlOutputBufferPtr): xmlTextWriterPtr; XMLCALL; XMLPUBFUN;
function xmlNewTextWriterFilename(uri: pchar; compression: cint): xmlTextWriterPtr; XMLCALL; XMLPUBFUN;
function xmlNewTextWriterMemory(buf: xmlBufferPtr; compression: cint): xmlTextWriterPtr; XMLCALL; XMLPUBFUN;
function xmlNewTextWriterPushParser(ctxt: xmlParserCtxtPtr; compression: cint): xmlTextWriterPtr; XMLCALL; XMLPUBFUN;
function xmlNewTextWriterDoc(var doc: xmlDocPtr; compression: cint): xmlTextWriterPtr; XMLCALL; XMLPUBFUN;
function xmlNewTextWriterTree(doc: xmlDocPtr; node: xmlNodePtr; compression: cint): xmlTextWriterPtr; XMLCALL; XMLPUBFUN;
procedure xmlFreeTextWriter(writer: xmlTextWriterPtr); XMLCALL; XMLPUBFUN;

(*
 * Functions
 *)

(*
 * Document
 *)
function xmlTextWriterStartDocument(writer: xmlTextWriterPtr; version, encoding, standalone: pchar): cint; XMLCALL; XMLPUBFUN;
function xmlTextWriterEndDocument(writer: xmlTextWriterPtr): cint; XMLCALL; XMLPUBFUN;

(*
 * Comments
 *)
function xmlTextWriterStartComment(writer: xmlTextWriterPtr): cint; XMLCALL; XMLPUBFUN;
function xmlTextWriterEndComment(writer: xmlTextWriterPtr): cint; XMLCALL; XMLPUBFUN;
function xmlTextWriterWriteFormatComment(writer: xmlTextWriterPtr; format: pchar; args: array of const): cint; XMLCALL; XMLPUBFUN;
function xmlTextWriterWriteVFormatComment(writer: xmlTextWriterPtr; format: pchar; argptr: va_list): cint; XMLCALL; XMLPUBFUN;
function xmlTextWriterWriteComment(writer: xmlTextWriterPtr; content: xmlCharPtr): cint; XMLCALL; XMLPUBFUN;

(*
 * Elements
 *)
function xmlTextWriterStartElement(writer: xmlTextWriterPtr; name: xmlCharPtr): cint; XMLCALL; XMLPUBFUN;
function xmlTextWriterStartElementNS(writer: xmlTextWriterPtr; prefix, name, namespaceURI: xmlCharPtr): cint; XMLCALL; XMLPUBFUN;
function xmlTextWriterEndElement(writer: xmlTextWriterPtr): cint; XMLCALL; XMLPUBFUN;
function xmlTextWriterFullEndElement(writer: xmlTextWriterPtr): cint; XMLCALL; XMLPUBFUN;

(*
 * Elements conveniency functions
 *)
function xmlTextWriterWriteFormatElement(writer: xmlTextWriterPtr; name: xmlCharPtr; format: pchar; args: array of const): cint; XMLCALL; XMLPUBFUN;
function xmlTextWriterWriteVFormatElement(writer: xmlTextWriterPtr; name: xmlCharPtr; format: pchar; argptr: va_list): cint; XMLCALL; XMLPUBFUN;
function xmlTextWriterWriteElement(writer: xmlTextWriterPtr; name, content: xmlCharPtr): cint; XMLCALL; XMLPUBFUN;
function xmlTextWriterWriteFormatElementNS(writer: xmlTextWriterPtr; prefix, name, namespaceURI: xmlCharPtr; format: pchar; args: array of const): cint; XMLCALL; XMLPUBFUN;
function xmlTextWriterWriteVFormatElementNS(writer: xmlTextWriterPtr; prefix, name, namespaceURI: xmlCharPtr; format: pchar; argptr: va_list): cint; XMLCALL; XMLPUBFUN;
function xmlTextWriterWriteElementNS(writer: xmlTextWriterPtr; prefix, name, namespaceURI, content: xmlCharPtr): cint; XMLCALL; XMLPUBFUN;

(*
 * Text
 *)
function xmlTextWriterWriteFormatRaw(writer: xmlTextWriterPtr; format: pchar; args: array of const): cint; XMLCALL; XMLPUBFUN;
function xmlTextWriterWriteVFormatRaw(writer: xmlTextWriterPtr; format: pchar; argptr: va_list): cint; XMLCALL; XMLPUBFUN;
function xmlTextWriterWriteRawLen(writer: xmlTextWriterPtr; content: xmlCharPtr; len: cint): cint; XMLCALL; XMLPUBFUN;
function xmlTextWriterWriteRaw(writer: xmlTextWriterPtr; content: xmlCharPtr): cint; XMLCALL; XMLPUBFUN;
function xmlTextWriterWriteFormatString(writer: xmlTextWriterPtr; format: pchar; args: array of const): cint; XMLCALL; XMLPUBFUN;
function xmlTextWriterWriteVFormatString(writer: xmlTextWriterPtr; format: pchar; argptr: va_list): cint; XMLCALL; XMLPUBFUN;
function xmlTextWriterWriteString(writer: xmlTextWriterPtr; content: xmlCharPtr): cint; XMLCALL; XMLPUBFUN;
function xmlTextWriterWriteBase64(writer: xmlTextWriterPtr; data: pchar; start, len: cint): cint; XMLCALL; XMLPUBFUN;
function xmlTextWriterWriteBinHex(writer: xmlTextWriterPtr; data: pchar; start, len: cint): cint; XMLCALL; XMLPUBFUN;

(*
 * Attributes
 *)
function xmlTextWriterStartAttribute(writer: xmlTextWriterPtr; name: xmlCharPtr): cint; XMLCALL; XMLPUBFUN;
function xmlTextWriterStartAttributeNS(writer: xmlTextWriterPtr; prefix, name, namespaceURI: xmlCharPtr): cint; XMLCALL; XMLPUBFUN;
function xmlTextWriterEndAttribute(writer: xmlTextWriterPtr): cint; XMLCALL; XMLPUBFUN;

(*
 * Attributes conveniency functions
 *)
function xmlTextWriterWriteFormatAttribute(writer: xmlTextWriterPtr; name: xmlCharPtr; format: pchar; args: array of const): cint; XMLCALL; XMLPUBFUN;
function xmlTextWriterWriteVFormatAttribute(writer: xmlTextWriterPtr; name: xmlCharPtr; format: pchar; argptr: va_list): cint; XMLCALL; XMLPUBFUN;
function xmlTextWriterWriteAttribute(writer: xmlTextWriterPtr; name, content: xmlCharPtr): cint; XMLCALL; XMLPUBFUN;
function xmlTextWriterWriteFormatAttributeNS(writer: xmlTextWriterPtr; prefix, name, namespaceURI: xmlCharPtr; format: pchar; args: array of const): cint; XMLCALL; XMLPUBFUN;
function xmlTextWriterWriteVFormatAttributeNS(writer: xmlTextWriterPtr; prefix, name, namespaceURI: xmlCharPtr; format: pchar; argptr: va_list): cint; XMLCALL; XMLPUBFUN;
function xmlTextWriterWriteAttributeNS(writer: xmlTextWriterPtr; prefix, name, namespaceURI, content: xmlCharPtr): cint; XMLCALL; XMLPUBFUN;

(*
 * PI's
 *)
function xmlTextWriterStartPI(writer: xmlTextWriterPtr; target: xmlCharPtr): cint; XMLCALL; XMLPUBFUN;
function xmlTextWriterEndPI(writer: xmlTextWriterPtr): cint; XMLCALL; XMLPUBFUN;

(*
 * PI conveniency functions
 *)
function xmlTextWriterWriteFormatPI(writer: xmlTextWriterPtr; target: xmlCharPtr; format: pchar; args: array of const): cint; XMLCALL; XMLPUBFUN;
function xmlTextWriterWriteVFormatPI(writer: xmlTextWriterPtr; target: xmlCharPtr; format: pchar; argptr: va_list): cint; XMLCALL; XMLPUBFUN;
function xmlTextWriterWritePI(writer: xmlTextWriterPtr; target, content: xmlCharPtr): cint; XMLCALL; XMLPUBFUN;

(**
 * xmlTextWriterWriteProcessingInstruction:
 *
 * This macro maps to xmlTextWriterWritePI
 *)
{$DEFINE xmlTextWriterWriteProcessingInstruction := xmlTextWriterWritePI}

(*
 * CDATA
 *)
function xmlTextWriterStartCDATA(writer: xmlTextWriterPtr): cint; XMLCALL; XMLPUBFUN;
function xmlTextWriterEndCDATA(writer: xmlTextWriterPtr): cint; XMLCALL; XMLPUBFUN;

(*
 * CDATA conveniency functions
 *)
function xmlTextWriterWriteFormatCDATA(writer: xmlTextWriterPtr; format: pchar; args: array of const): cint; XMLCALL; XMLPUBFUN;
function xmlTextWriterWriteVFormatCDATA(writer: xmlTextWriterPtr; format: pchar; argptr: va_list): cint; XMLCALL; XMLPUBFUN;
function xmlTextWriterWriteCDATA(writer: xmlTextWriterPtr; content: xmlCharPtr): cint; XMLCALL; XMLPUBFUN;

(*
 * DTD
 *)
function xmlTextWriterStartDTD(writer: xmlTextWriterPtr; name, pubid, sysid: xmlCharPtr): cint; XMLCALL; XMLPUBFUN;
function xmlTextWriterEndDTD(writer: xmlTextWriterPtr): cint; XMLCALL; XMLPUBFUN;

(*
 * DTD conveniency functions
 *)
function xmlTextWriterWriteFormatDTD(writer: xmlTextWriterPtr; name, pubid, sysid: xmlCharPtr; format: pchar; args: array of const): cint; XMLCALL; XMLPUBFUN;
function xmlTextWriterWriteVFormatDTD(writer: xmlTextWriterPtr; name, pubid, sysid: xmlCharPtr; format: pchar; argptr: va_list): cint; XMLCALL; XMLPUBFUN;
function xmlTextWriterWriteDTD(writer: xmlTextWriterPtr; name, pubid, sysid, subset: xmlCharPtr): cint; XMLCALL; XMLPUBFUN;

(**
 * xmlTextWriterWriteDocType:
 *
 * this macro maps to xmlTextWriterWriteDTD
 *)
{$DEFINE xmlTextWriterWriteDocType := xmlTextWriterWriteDTD}

(*
 * DTD element definition
 *)
function xmlTextWriterStartDTDElement(writer: xmlTextWriterPtr; name: xmlCharPtr): cint; XMLCALL; XMLPUBFUN;
function xmlTextWriterEndDTDElement(writer: xmlTextWriterPtr): cint; XMLCALL; XMLPUBFUN;

(*
 * DTD element definition conveniency functions
 *)
function xmlTextWriterWriteFormatDTDElement(writer: xmlTextWriterPtr; name: xmlCharPtr; format: pchar; args: array of const): cint; XMLCALL; XMLPUBFUN;
function xmlTextWriterWriteVFormatDTDElement(writer: xmlTextWriterPtr; name: xmlCharPtr; format: pchar; argptr: va_list): cint; XMLCALL; XMLPUBFUN;
function xmlTextWriterWriteDTDElement(writer: xmlTextWriterPtr; name, content: xmlCharPtr): cint; XMLCALL; XMLPUBFUN;

(*
 * DTD attribute list definition
 *)
function xmlTextWriterStartDTDAttlist(writer: xmlTextWriterPtr; name: xmlCharPtr): cint; XMLCALL; XMLPUBFUN;
function xmlTextWriterEndDTDAttlist(writer: xmlTextWriterPtr): cint; XMLCALL; XMLPUBFUN;

(*
 * DTD attribute list definition conveniency functions
 *)
function xmlTextWriterWriteFormatDTDAttlist(writer: xmlTextWriterPtr; name: xmlCharPtr; format: pchar; args: array of const): cint; XMLCALL; XMLPUBFUN;
function xmlTextWriterWriteVFormatDTDAttlist(writer: xmlTextWriterPtr; name: xmlCharPtr; format: pchar; argptr: va_list): cint; XMLCALL; XMLPUBFUN;
function xmlTextWriterWriteDTDAttlist(writer: xmlTextWriterPtr; name, content: xmlCharPtr): cint; XMLCALL; XMLPUBFUN;

(*
 * DTD entity definition
 *)
function xmlTextWriterStartDTDEntity(writer: xmlTextWriterPtr; pe: cint; name: xmlCharPtr): cint; XMLCALL; XMLPUBFUN;
function xmlTextWriterEndDTDEntity(writer: xmlTextWriterPtr): cint; XMLCALL; XMLPUBFUN;

(*
 * DTD entity definition conveniency functions
 *)
function xmlTextWriterWriteFormatDTDInternalEntity(writer: xmlTextWriterPtr; pe: cint; name: xmlCharPtr; format: pchar; args: array of const): cint; XMLCALL; XMLPUBFUN;
function xmlTextWriterWriteVFormatDTDInternalEntity(writer: xmlTextWriterPtr; pe: cint; name: xmlCharPtr; format: pchar; argptr: va_list): cint; XMLCALL; XMLPUBFUN;
function xmlTextWriterWriteDTDInternalEntity(writer: xmlTextWriterPtr; pe: cint; name, content: xmlCharPtr): cint; XMLCALL; XMLPUBFUN;
function xmlTextWriterWriteDTDExternalEntity(writer: xmlTextWriterPtr; pe: cint; name, pubid, sysid, ndataid: xmlCharPtr): cint; XMLCALL; XMLPUBFUN;
function xmlTextWriterWriteDTDExternalEntityContents(writer: xmlTextWriterPtr; pubid, sysid, ndataid: xmlCharPtr): cint; XMLCALL; XMLPUBFUN;
function xmlTextWriterWriteDTDEntity(writer: xmlTextWriterPtr; pe: cint; name, pubid, sysid, ndataid, content: xmlCharPtr): cint; XMLCALL; XMLPUBFUN;

(*
 * DTD notation definition
 *)
function xmlTextWriterWriteDTDNotation(writer: xmlTextWriterPtr; name, pubid, sysid: xmlCharPtr): cint; XMLCALL; XMLPUBFUN;

(*
 * Indentation
 *)
function xmlTextWriterSetIndent(writer: xmlTextWriterPtr; indent: cint): cint; XMLCALL; XMLPUBFUN;
function xmlTextWriterSetIndentString(writer: xmlTextWriterPtr; str: xmlCharPtr): cint; XMLCALL; XMLPUBFUN;

(*
 * misc
 *)
function xmlTextWriterFlush(writer: xmlTextWriterPtr): cint; XMLCALL; XMLPUBFUN;

{$ENDIF}
{$ENDIF} (* LIBXML_WRITER_ENABLED *)
