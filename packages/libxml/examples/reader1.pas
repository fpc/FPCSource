(**
 * section: xmlReader
 * synopsis: Parse an XML file with an xmlReader
 * purpose: Demonstrate the use of xmlReaderForFile() to parse an XML file
 *          and dump the informations about the nodes found in the process.
 *          (Note that the XMLReader functions require libxml2 version later
 *          than 2.6.)
 * usage: reader1 <filename>
 * test: reader1 test2.xml > reader1.tmp ; diff reader1.tmp reader1.res ; rm reader1.tmp
 * author: Daniel Veillard
 * copy: see Copyright for the status of this software.
 *)

program reader1;

{$mode objfpc}

uses
  ctypes,
  xml2,
  exutils;

(**
 * processNode:
 * @reader: the xmlReader
 *
 * Dump information about the current node
 *)
procedure processNode(reader: xmlTextReaderPtr);
var
  value, name: xmlCharPtr;
begin
  name := xmlTextReaderConstName(reader);
  if not assigned(name) then
    name := BAD_CAST('--');

  value := xmlTextReaderConstValue(reader);

  printf('%d %d %s %d %d',
      [xmlTextReaderDepth(reader),
      xmlTextReaderNodeType(reader),
      name,
      xmlTextReaderIsEmptyElement(reader),
      xmlTextReaderHasValue(reader)]);

  if not assigned(value) then
    writeln
  else
    if (xmlStrlen(value) > 40) then
      printfn(' %.40s...\n', [value])
    else
      printfn(' %s\n', [value]);
end;

(**
 * streamFile:
 * @filename: the file name to parse
 *
 * Parse and print information about an XML file.
 *)
procedure streamFile(const filename: pchar);
var
  reader: xmlTextReaderPtr;
  ret: cint;
begin
  reader := xmlReaderForFile(filename, nil, 0);
  if assigned(reader) then
  begin
    ret := xmlTextReaderRead(reader);
    while ret = 1 do
    begin
      processNode(reader);
      ret := xmlTextReaderRead(reader);
    end;
    xmlFreeTextReader(reader);

    if ret <> 0 then
      printfn('%s : failed to parse', [filename]);
  end else
    printfn('Unable to open %s', [filename]);
end;

begin
  (*
   * this initialize the library and check potential ABI mismatches
   * between the version it was compiled for and the actual shared
   * library used.
   *)
  LIBXML_TEST_VERSION;

  streamFile(pchar(ParamStr(1)));

  (*
   * Cleanup function for the XML library.
   *)
  xmlCleanupParser();
end.

