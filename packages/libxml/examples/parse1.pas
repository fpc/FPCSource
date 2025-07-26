(**
 * section: Parsing
 * synopsis: Parse an XML file to a tree and free it
 * purpose: Demonstrate the use of xmlReadFile() to read an XML file
 *          into a tree and xmlFreeDoc() to free the resulting tree
 * usage: parse1 test1.xml
 * test: parse1 test1.xml
 * author: Daniel Veillard
 * copy: see Copyright for the status of this software.
 *)

program parse1;

{$mode objfpc}

uses
  xml2,
  exutils;

{**
 * example1Func:
 * @filename: a filename or an URL
 *
 * Parse the resource and free the resulting tree
 *}
procedure example1Func(const filename: PAnsiChar);
var
  doc: xmlDocPtr; (* the resulting document tree *)
begin
  doc := xmlReadFile(filename, nil, 0);
  if (doc = nil) then
  begin
    printfn('Failed to parse %s', [filename]);
    Exit;
  end;
  xmlFreeDoc(doc);
end;

begin
  if ParamCount <> 1 then
    Halt(1);

  (*
   * this initialize the library and check potential ABI mismatches
   * between the version it was compiled for and the actual shared
   * library used.
   *)
  LIBXML_TEST_VERSION;

  example1Func(PAnsiChar(ParamStr(1)));

  (*
   * Cleanup function for the XML library.
   *)
  xmlCleanupParser();
  (*
   * this is to debug memory for regression tests
   *)
  xmlMemoryDump();
end.

