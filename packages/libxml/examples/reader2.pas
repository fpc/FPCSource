(**
 * section: xmlReader
 * synopsis: Parse and validate an XML file with an xmlReader
 * purpose: Demonstrate the use of xmlReaderForFile() to parse an XML file
 *          validating the content in the process and activating options
 *          like entities substitution, and DTD attributes defaulting.
 *          (Note that the XMLReader functions require libxml2 version later
 *          than 2.6.)
 * usage: reader2 <valid_xml_filename>
 * test: reader2 test2.xml > reader1.tmp ; diff reader1.tmp reader1.res ; rm reader1.tmp
 * author: Daniel Veillard
 * copy: see Copyright for the status of this software.
 *)

program reader2;

{$mode objfpc}

uses
  ctypes,
  libxml2 in '../libxml2.pas',
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
    name := pchar('--'); // BAD_CAST !!!!

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
  (*
   * Pass some special parsing options to activate DTD attribute defaulting,
   * entities substitution and DTD validation
   *)
  reader := xmlReaderForFile(filename, nil,
    XML_PARSE_DTDATTR or  // default DTD attributes
    XML_PARSE_NOENT or    // substitute entities
    XML_PARSE_DTDVALID);  // validate with the DTD

  if assigned(reader) then
  begin
    ret := xmlTextReaderRead(reader);
    while ret = 1 do
    begin
      processNode(reader);
      ret := xmlTextReaderRead(reader);
    end;

    (*
     * Once the document has been fully parsed check the validation results
     *)
    if (xmlTextReaderIsValid(reader) <> 1) then
      printfn('Document %s does not validate', [filename])
    else begin
      xmlFreeTextReader(reader);
      if ret <> 0 then
        printfn('%s : failed to parse', [filename]);
    end;
  end else
    printfn('Unable to open %s', [filename]);
end;

begin
  streamFile(pchar(ParamStr(1)));
end.

