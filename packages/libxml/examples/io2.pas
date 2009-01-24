(**
 * section: InputOutput
 * synopsis: Output to char buffer
 * purpose: Demonstrate the use of xmlDocDumpMemory
 *          to output document to a character buffer
 * usage: io2
 * test: io2 > io2.tmp ; diff io2.tmp io2.res ; rm -f io2.tmp
 * author: John Fleck
 * copy: see Copyright for the status of this software.
 *)

program io2;

{$mode objfpc}

uses
  ctypes,
  xml2,
  exutils,
  SysUtils;

var
  n: xmlNodePtr;
  doc: xmlDocPtr;
  xmlbuff: xmlCharPtr;
  buffersize: cint;

begin
  (*
   * Create the document.
   *)
  doc := xmlNewDoc(BAD_CAST('1.0'));
  n := xmlNewNode(nil, BAD_CAST('root'));
  xmlNodeSetContent(n, BAD_CAST('content'));
  xmlDocSetRootElement(doc, n);

  (*
   * Dump the document to a buffer and print it
   * for demonstration purposes.
   *)
  xmlDocDumpFormatMemory(doc, xmlbuff, buffersize, 1);
  printfn(xmlbuff);

  (*
   * Free associated memory.
   *)
  xmlFree(xmlbuff);
  xmlFreeDoc(doc);
end.
