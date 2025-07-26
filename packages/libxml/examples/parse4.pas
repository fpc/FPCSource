(**
 * section: Parsing
 * synopsis: Parse an XML document chunk by chunk to a tree and free it
 * purpose: Demonstrate the use of xmlCreatePushParserCtxt() and
 *          xmlParseChunk() to read an XML file progressively
 *          into a tree and xmlFreeDoc() to free the resulting tree
 * usage: parse4 test3.xml
 * test: parse4 test3.xml
 * author: Daniel Veillard
 * copy: see Copyright for the status of this software.
 *)

program parse4;

{$mode objfpc}

uses
  xml2,
  exutils,
  ctypes;

procedure processDoc(readerPtr: xmlTextReaderPtr);
var
  ret: cint;
  docPtr: xmlDocPtr;
  URL: PAnsiChar;
begin
  ret := xmlTextReaderRead(readerPtr);
  while ret = 1 do
    ret := xmlTextReaderRead(readerPtr);

  (*
   * One can obtain the document pointer to get interesting
   * information about the document like the URL, but one must also
   * be sure to clean it up at the end (see below).
   *)
  docPtr := xmlTextReaderCurrentDoc(readerPtr);
  if docPtr = Nil then
  begin
    printfn('failed to obtain document');
    Exit;
  end;

  URL := docPtr^.URL;
  if URL = Nil then
    printfn('Failed to obtain URL');

  if ret <> 0 then
  begin
    printfn('%s: Failed to parse', [URL]);
    Exit;
  end;

  printfn('%s: Processed ok', [URL]);
end;

var
  readerPtr: xmlTextReaderPtr;
  i: cint;
  docPtr: xmlDocPtr;

begin
  if ParamCount < 1 then
    Halt(1);

  (*
   * this initialises the library and check potential ABI mismatches
   * between the version it was compiled for and the actual shared
   * library used.
   *)
  LIBXML_TEST_VERSION;

  (*
   * Create a new reader for the first file and process the
   * document.
   *)
  readerPtr := xmlReaderForFile(PAnsiChar(ParamStr(1)), Nil, 0);
  if readerPtr = Nil then
  begin
    printfn('%s: failed to create reader', [ParamStr(1)]);
    Halt(1);
  end;
  processDoc(readerPtr);

  (*
   * The reader can be reused for subsequent files.
   *)
  for i := 2 to ParamCount do
  begin
    xmlReaderNewFile(readerPtr, PAnsiChar(ParamStr(i)), Nil, 0);
    if readerPtr = Nil then
    begin
      printfn('%s: failed to create reader', [ParamStr(i)]);
      Halt(1);
    end;
    processDoc(readerPtr);
  end;

  (*
   * Since we've called xmlTextReaderCurrentDoc, we now have to
   * clean up after ourselves.  We only have to do this the last
   * time, because xmlReaderNewFile calls xmlCtxtReset which takes
   * care of it.
   *)
  docPtr := xmlTextReaderCurrentDoc(readerPtr);
  if docPtr <> Nil then
    xmlFreeDoc(docPtr);

  (*
   * Clean up the reader.
   *)
  xmlFreeTextReader(readerPtr);

  (*
   * Cleanup function for the XML library.
   *)
  xmlCleanupParser();
  (*
   * this is to debug memory for regression tests
   *)
  xmlMemoryDump();
end.
