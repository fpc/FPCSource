(**
 * section: Parsing
 * synopsis: Parse and validate an XML file to a tree and free the result
 * purpose: Create a parser context for an XML file, then parse and validate
 *          the file, creating a tree, check the validation result
 *          and xmlFreeDoc() to free the resulting tree.
 * usage: parse2 test2.xml
 * test: parse2 test2.xml
 * author: Daniel Veillard
 * copy: see Copyright for the status of this software.
 *)

program parse2;

{$mode objfpc}

uses
  xml2,
  exutils;

(**
 * exampleFunc:
 * @filename: a filename or an URL
 *
 * Parse and validate the resource and free the resulting tree
 *)
procedure exampleFunc(const filename: PAnsiChar);
var
  ctxt: xmlParserCtxtPtr; (* the parser context *)
  doc: xmlDocPtr; (* the resulting document tree *)
begin
  (* create a parser context *)
  ctxt := xmlNewParserCtxt();
  if ctxt = Nil then
  begin
    printfn('Failed to allocate parser context');
    Exit;
  end;
  (* parse the file, activating the DTD validation option *)
  doc := xmlCtxtReadFile(ctxt, filename, Nil, XML_PARSE_DTDVALID);
  (* check if parsing succeeded *)
  if doc = Nil then
    printfn('Failed to parse %s', [filename])
  else
  begin
    (* check if validation succeeded *)
    if ctxt^.valid = 0 then
      printfn('Failed to validate %s', [filename]);
    (* free up the resulting document *)
    xmlFreeDoc(doc);
  end;
  (* free up the parser context *)
  xmlFreeParserCtxt(ctxt);
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

  exampleFunc(PAnsiChar(ParamStr(1)));

  (*
   * Cleanup function for the XML library.
   *)
  xmlCleanupParser();
  (*
   * this is to debug memory for regression tests
   *)
  xmlMemoryDump();
end.
