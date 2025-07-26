(**
 * section: Parsing
 * synopsis: Parse an XML document in memory to a tree and free it
 * purpose: Demonstrate the use of xmlReadMemory() to read an XML file
 *          into a tree and xmlFreeDoc() to free the resulting tree
 * usage: parse3
 * test: parse3
 * author: Daniel Veillard
 * copy: see Copyright for the status of this software.
 *)

program parse3;

{$mode objfpc}

uses
  xml2,
  exutils;

const
  document = '<doc/>';

(**
 * example3Func:
 * @content: the content of the document
 * @length: the length in bytes
 *
 * Parse the in memory document and free the resulting tree
 *)

procedure example3Func(const content: PAnsiChar; length: Integer);
var
  doc: xmlDocPtr; (* the resulting document tree *)
begin
  (*
   * The document being in memory, it have no base per RFC 2396,
   * and the "noname.xml" argument will serve as its base.
   *)
  doc := xmlReadMemory(content, length, 'noname.xml', Nil, 0);
  if doc = Nil then
  begin
    printfn('Failed to parse document');
    Exit;
  end;
  xmlFreeDoc(doc);
end;

begin
  (*
   * this initialize the library and check potential ABI mismatches
   * between the version it was compiled for and the actual shared
   * library used.
   *)
  LIBXML_TEST_VERSION;

  example3Func(document, 6);

  (*
   * Cleanup function for the XML library.
   *)
  xmlCleanupParser();
  (*
   * this is to debug memory for regression tests
   *)
  xmlMemoryDump();
end.
