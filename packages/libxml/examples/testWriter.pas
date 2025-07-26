(**
 * section: xmlWriter
 * synopsis: use various APIs for the xmlWriter
 * purpose: tests a number of APIs for the xmlWriter, especially
 *          the various methods to write to a filename, to a memory
 *          buffer, to a new document, or to a subtree. It shows how to
 *          do encoding string conversions too. The resulting
 *          documents are then serialized.
 * usage: testWriter
 * test: testWriter && for i in 1 2 3 4 ; do diff $(srcdir)/writer.xml writer$$i.tmp || break ; done
 * author: Alfred Mickautsch
 * copy: see Copyright for the status of this software.
 *)

program testWriter;

{$mode objfpc}

uses
  ctypes,
  xml2,
  exutils,
  SysUtils,
  Strings;

const
  MY_ENCODING = 'ISO-8859-1';

(**
 * ConvertInput:
 * @in: string in a given encoding
 * @encoding: the encoding used
 *
 * Converts @in into UTF-8 for processing with libxml2 APIs
 *
 * Returns the converted UTF-8 string, or NULL in case of error.
 *)
function ConvertInput(const inp, encoding: PAnsiChar): xmlCharPtr;
var
  ret: cint;
  size: cint;
  out_size: cint;
  temp: cint;
  handler: xmlCharEncodingHandlerPtr;
begin
  Result := Nil;

  if inp = Nil then
    Exit;

  handler := xmlFindCharEncodingHandler(encoding);

  if handler = Nil then
  begin
    printfn('ConvertInput: no encoding handler found for ''%s''',
      [specialize IfThen<PAnsiChar>(encoding <> Nil, encoding, '')]);
    Exit;
  end;

  size := strlen(inp) + 1;
  out_size := size * 2 - 1;
  Result := xmlCharPtr(xmlMalloc(out_size));

  if Result <> nil then
  begin
    temp := size - 1;
    ret := handler^.input(Result, @out_size, inp, @temp);
    if (ret < 0) or (temp - size + 1 <> 0) then
    begin
      if ret < 0 then
        printfn('ConvertInput: conversion wasn''t successful.')
      else
        printfn('ConvertInput: conversion wasn''t successful. converted: %i octets.',
          [temp]);

      xmlFree(Result);
      Result := Nil;
    end else
    begin
      Result := xmlCharPtr(xmlRealloc(Result, out_size + 1));
      Result[out_size] := #0;  (*null terminating out *)
    end;
  end else
    printf('ConvertInput: no mem');
end;

(**
 * testXmlwriterFilename:
 * @uri: the output URI
 *
 * test the xmlWriter interface when writing to a new file
 *)
procedure testXmlwriterFilename(const uri: PAnsiChar);
var
  rc: cint;
  writer: xmlTextWriterPtr;
  tmp: xmlCharPtr;
begin
  (* Create a new XmlWriter for uri, with no compression. *)
  writer := xmlNewTextWriterFilename(uri, 0);
  if writer = Nil then
  begin
    printfn('testXmlwriterFilename: Error creating the xml writer');
    Exit;
  end;

  (* Start the document with the xml default for the version,
   * encoding ISO 8859-1 and the default for the standalone
   * declaration. *)
  rc := xmlTextWriterStartDocument(writer, Nil, MY_ENCODING, Nil);
  if rc < 0 then
  begin
    printfn('testXmlwriterFilename: Error at xmlTextWriterStartDocument');
    Exit;
  end;

  (* Start an element named "EXAMPLE". Since thist is the first
   * element, this will be the root element of the document. *)
  rc := xmlTextWriterStartElement(writer, 'EXAMPLE');
  if rc < 0 then
  begin
    printfn('testXmlwriterFilename: Error at xmlTextWriterStartElement');
    Exit;
  end;

  (* Write a comment as child of EXAMPLE.
   * Please observe, that the input to the xmlTextWriter functions
   * HAS to be in UTF-8, even if the output XML is encoded
   * in iso-8859-1 *)
  tmp := ConvertInput('This is a comment with special chars: <'#$E4#$F6#$FC'>',
    MY_ENCODING);
  rc := xmlTextWriterWriteComment(writer, tmp);
  if rc < 0 then
  begin
    printfn('testXmlwriterFilename: Error at xmlTextWriterWriteComment');
    Exit;
  end;
  if tmp <> Nil then
    xmlFree(tmp);

  (* Start an element named "ORDER" as child of EXAMPLE. *)
  rc := xmlTextWriterStartElement(writer, 'ORDER');
  if rc < 0 then
  begin
    printfn('testXmlwriterFilename: Error at xmlTextWriterStartElement');
    Exit;
  end;

  (* Add an attribute with name "version" and value "1.0" to ORDER. *)
  rc := xmlTextWriterWriteAttribute(writer, 'version', '1.0');
  if rc < 0 then
  begin
    printfn('testXmlwriterFilename: Error at xmlTextWriterWriteAttribute');
    Exit;
  end;

  (* Add an attribute with name "xml:lang" and value "de" to ORDER. *)
  rc := xmlTextWriterWriteAttribute(writer, 'xml:lang', 'de');
  if rc < 0 then
  begin
    printfn('testXmlwriterFilename: Error at xmlTextWriterWriteAttribute');
    Exit;
  end;

  (* Write a comment as child of ORDER *)
  tmp := ConvertInput('<'#$E4#$F6#$FC'>', MY_ENCODING);
  rc := xmlTextWriterWriteFormatComment(writer,
    'This is another comment with special chars: %s', [tmp]);
  if (rc < 0) then
  begin
    printfn('testXmlwriterFilename: Error at xmlTextWriterWriteFormatComment');
    Exit;
  end;
  if tmp <> Nil then
    xmlFree(tmp);

  (* Start an element named "HEADER" as child of ORDER. *)
  rc := xmlTextWriterStartElement(writer, 'HEADER');
  if rc < 0 then
  begin
    printfn('testXmlwriterFilename: Error at xmlTextWriterStartElement');
    Exit;
  end;

  (* Write an element named "X_ORDER_ID" as child of HEADER. *)
  rc := xmlTextWriterWriteFormatElement(writer, 'X_ORDER_ID', '%010d', [53535]);
  if rc < 0 then
  begin
    printf('testXmlwriterFilename: Error at xmlTextWriterWriteFormatElement');
    Exit;
  end;

  (* Write an element named "CUSTOMER_ID" as child of HEADER. *)
  rc := xmlTextWriterWriteFormatElement(writer, 'CUSTOMER_ID', '%d', [1010]);
  if rc < 0 then
  begin
    printf('testXmlwriterFilename: Error at xmlTextWriterWriteFormatElement');
    Exit;
  end;

  (* Write an element named "NAME_1" as child of HEADER. *)
  tmp := ConvertInput('M'#$FC'ller', MY_ENCODING);
  rc := xmlTextWriterWriteElement(writer, 'NAME_1', tmp);
  if rc < 0 then
  begin
    printfn('testXmlwriterFilename: Error at xmlTextWriterWriteElement');
    Exit;
  end;
  if tmp <> Nil then
    xmlFree(tmp);

  (* Write an element named "NAME_2" as child of HEADER. *)
  tmp := ConvertInput('J'#$F6'rg', MY_ENCODING);
  rc := xmlTextWriterWriteElement(writer, 'NAME_2', tmp);
  if rc < 0 then
  begin
    printfn('testXmlwriterFilename: Error at xmlTextWriterWriteElement');
    Exit;
  end;
  if tmp <> Nil then
    xmlFree(tmp);

  (* Close the element named HEADER. *)
  rc := xmlTextWriterEndElement(writer);
  if rc < 0 then
  begin
    printfn('testXmlwriterFilename: Error at xmlTextWriterEndElement');
    Exit;
  end;

  (* Start an element named "ENTRIES" as child of ORDER. *)
  rc := xmlTextWriterStartElement(writer, 'ENTRIES');
  if rc < 0 then
  begin
    printfn('testXmlwriterFilename: Error at xmlTextWriterStartElement');
    Exit;
  end;

  (* Start an element named "ENTRY" as child of ENTRIES. *)
  rc := xmlTextWriterStartElement(writer, 'ENTRY');
  if rc < 0 then
  begin
    printfn('testXmlwriterFilename: Error at xmlTextWriterStartElement');
    Exit;
  end;

  (* Write an element named "ARTICLE" as child of ENTRY. *)
  rc := xmlTextWriterWriteElement(writer, 'ARTICLE', '<Test>');
  if rc < 0 then
  begin
    printfn('testXmlwriterFilename: Error at xmlTextWriterWriteElement');
    Exit;
  end;

  (* Write an element named "ENTRY_NO" as child of ENTRY. *)
  rc := xmlTextWriterWriteFormatElement(writer, 'ENTRY_NO', '%d', [10]);
  if rc < 0 then
  begin
    printfn('testXmlwriterFilename: Error at xmlTextWriterWriteFormatElement');
    Exit;
  end;

  (* Close the element named ENTRY. *)
  rc := xmlTextWriterEndElement(writer);
  if rc < 0 then
  begin
    printfn('testXmlwriterFilename: Error at xmlTextWriterEndElement');
    Exit;
  end;

  (* Start an element named "ENTRY" as child of ENTRIES. *)
  rc := xmlTextWriterStartElement(writer, 'ENTRY');
  if rc < 0 then
  begin
    printfn('testXmlwriterFilename: Error at xmlTextWriterStartElement');
    Exit;
  end;

  (* Write an element named "ARTICLE" as child of ENTRY. *)
  rc := xmlTextWriterWriteElement(writer, 'ARTICLE', '<Test 2>');
  if rc < 0 then
  begin
    printfn('testXmlwriterFilename: Error at xmlTextWriterWriteElement');
    Exit;
  end;

  (* Write an element named "ENTRY_NO" as child of ENTRY. *)
  rc := xmlTextWriterWriteFormatElement(writer, 'ENTRY_NO', '%d', [20]);
  if rc < 0 then
  begin
    printfn('testXmlwriterFilename: Error at xmlTextWriterWriteFormatElement');
    Exit;
  end;

  (* Close the element named ENTRY. *)
  rc := xmlTextWriterEndElement(writer);
  if rc < 0 then
  begin
    printfn('testXmlwriterFilename: Error at xmlTextWriterEndElement');
    Exit;
  end;

  (* Close the element named ENTRIES. *)
  rc := xmlTextWriterEndElement(writer);
  if (rc < 0) then
  begin
    printfn('testXmlwriterFilename: Error at xmlTextWriterEndElement');
    Exit;
  end;

  (* Start an element named "FOOTER" as child of ORDER. *)
  rc := xmlTextWriterStartElement(writer, 'FOOTER');
  if rc < 0 then
  begin
    printfn('testXmlwriterFilename: Error at xmlTextWriterStartElement');
    Exit;
  end;

  (* Write an element named "TEXT" as child of FOOTER. *)
  rc := xmlTextWriterWriteElement(writer, 'TEXT', 'This is a text.');
  if rc < 0 then
  begin
    printfn('testXmlwriterFilename: Error at xmlTextWriterWriteElement');
    Exit;
  end;

  (* Close the element named FOOTER. *)
  rc := xmlTextWriterEndElement(writer);
  if rc < 0 then
  begin
    printfn('testXmlwriterFilename: Error at xmlTextWriterEndElement');
    Exit;
  end;

  (* Here we could close the elements ORDER and EXAMPLE using the
   * function xmlTextWriterEndElement, but since we do not want to
   * write any other elements, we simply call xmlTextWriterEndDocument,
   * which will do all the work. *)
  rc := xmlTextWriterEndDocument(writer);
  if rc < 0 then
  begin
    printfn('testXmlwriterFilename: Error at xmlTextWriterEndDocument');
    Exit;
  end;

  xmlFreeTextWriter(writer);
end;

(**
 * testXmlwriterMemory:
 * @file: the output file
 *
 * test the xmlWriter interface when writing to memory
 *)
procedure testXmlwriterMemory(const _file: PAnsiChar);
var
  rc: cint;
  writer: xmlTextWriterPtr;
  buf: xmlBufferPtr;
  tmp: xmlCharPtr;
  fp: THandle;
begin
  (* Create a new XML buffer, to which the XML document will be
   * written *)
  buf := xmlBufferCreate();
  if buf = Nil then
  begin
    printfn('testXmlwriterMemory: Error creating the xml buffer');
    Exit;
  end;

  (* Create a new XmlWriter for memory, with no compression.
   * Remark: there is no compression for this kind of xmlTextWriter *)
  writer := xmlNewTextWriterMemory(buf, 0);
  if writer = Nil then
  begin
    printfn('testXmlwriterMemory: Error creating the xml writer');
    Exit;
  end;

  (* Start the document with the xml default for the version,
   * encoding ISO 8859-1 and the default for the standalone
   * declaration. *)
  rc := xmlTextWriterStartDocument(writer, Nil, MY_ENCODING, Nil);
  if rc < 0 then
  begin
    printfn('testXmlwriterMemory: Error at xmlTextWriterStartDocument');
    Exit;
  end;

  (* Start an element named "EXAMPLE". Since thist is the first
   * element, this will be the root element of the document. *)
  rc := xmlTextWriterStartElement(writer, 'EXAMPLE');
  if rc < 0 then
  begin
    printfn('testXmlwriterMemory: Error at xmlTextWriterStartElement');
    Exit;
  end;

  (* Write a comment as child of EXAMPLE.
   * Please observe, that the input to the xmlTextWriter functions
   * HAS to be in UTF-8, even if the output XML is encoded
   * in iso-8859-1 *)
  tmp := ConvertInput('This is a comment with special chars: <'#$E4#$F6#$FC'>',
    MY_ENCODING);
  rc := xmlTextWriterWriteComment(writer, tmp);
  if rc < 0 then
  begin
    printfn('testXmlwriterMemory: Error at xmlTextWriterWriteComment');
    Exit;
  end;
  if tmp <> Nil then
    xmlFree(tmp);

  (* Start an element named "ORDER" as child of EXAMPLE. *)
  rc := xmlTextWriterStartElement(writer, 'ORDER');
  if rc < 0 then
  begin
    printfn('testXmlwriterMemory: Error at xmlTextWriterStartElement');
    Exit;
  end;

  (* Add an attribute with name "version" and value "1.0" to ORDER. *)
  rc := xmlTextWriterWriteAttribute(writer, 'version', '1.0');
  if rc < 0 then
  begin
    printfn('testXmlwriterMemory: Error at xmlTextWriterWriteAttribute');
    Exit;
  end;

  (* Add an attribute with name "xml:lang" and value "de" to ORDER. *)
  rc := xmlTextWriterWriteAttribute(writer, 'xml:lang', 'de');
  if rc < 0 then
  begin
    printfn('testXmlwriterMemory: Error at xmlTextWriterWriteAttribute');
    Exit;
  end;

  (* Write a comment as child of ORDER *)
  tmp := ConvertInput('<'#$E4#$F6#$FC'>', MY_ENCODING);
  rc := xmlTextWriterWriteFormatComment(writer,
    'This is another comment with special chars: %s', [tmp]);
  if rc < 0 then
  begin
    printfn('testXmlwriterMemory: Error at xmlTextWriterWriteFormatComment');
    Exit;
  end;
  if tmp <> Nil then
    xmlFree(tmp);

  (* Start an element named "HEADER" as child of ORDER. *)
  rc := xmlTextWriterStartElement(writer, 'HEADER');
  if rc < 0 then
  begin
    printfn('testXmlwriterMemory: Error at xmlTextWriterStartElement\n');
    Exit;
  end;

  (* Write an element named "X_ORDER_ID" as child of HEADER. *)
  rc := xmlTextWriterWriteFormatElement(writer, 'X_ORDER_ID', '%010d', [53535]);
  if rc < 0 then
  begin
    printfn('testXmlwriterMemory: Error at xmlTextWriterWriteFormatElement');
    Exit;
  end;

  (* Write an element named "CUSTOMER_ID" as child of HEADER. *)
  rc := xmlTextWriterWriteFormatElement(writer, 'CUSTOMER_ID', '%d', [1010]);
  if rc < 0 then
  begin
    printfn('testXmlwriterMemory: Error at xmlTextWriterWriteFormatElement\n');
    Exit;
  end;

  (* Write an element named "NAME_1" as child of HEADER. *)
  tmp := ConvertInput('M'#$FC'ller', MY_ENCODING);
  rc := xmlTextWriterWriteElement(writer, 'NAME_1', tmp);
  if rc < 0 then
  begin
    printfn('testXmlwriterMemory: Error at xmlTextWriterWriteElement');
    Exit;
  end;
  if tmp <> Nil then
    xmlFree(tmp);

  (* Write an element named "NAME_2" as child of HEADER. *)
  tmp := ConvertInput('J'#$F6'rg', MY_ENCODING);
  rc := xmlTextWriterWriteElement(writer, 'NAME_2', tmp);
  if rc < 0 then
  begin
    printfn('testXmlwriterMemory: Error at xmlTextWriterWriteElement');
    Exit;
  end;
  if tmp <> Nil then
    xmlFree(tmp);

  (* Close the element named HEADER. *)
  rc := xmlTextWriterEndElement(writer);
  if rc < 0 then
  begin
    printfn('testXmlwriterMemory: Error at xmlTextWriterEndElement');
    Exit;
  end;

  (* Start an element named "ENTRIES" as child of ORDER. *)
  rc := xmlTextWriterStartElement(writer, 'ENTRIES');
  if rc < 0 then
  begin
    printfn('testXmlwriterMemory: Error at xmlTextWriterStartElement');
    Exit;
  end;

  (* Start an element named "ENTRY" as child of ENTRIES. *)
  rc := xmlTextWriterStartElement(writer, 'ENTRY');
  if rc < 0 then
  begin
    printfn('testXmlwriterMemory: Error at xmlTextWriterStartElement');
    Exit;
  end;

  (* Write an element named "ARTICLE" as child of ENTRY. *)
  rc := xmlTextWriterWriteElement(writer, 'ARTICLE', '<Test>');
  if rc < 0 then
  begin
    printfn('testXmlwriterMemory: Error at xmlTextWriterWriteElement');
    Exit;
  end;

  (* Write an element named "ENTRY_NO" as child of ENTRY. *)
  rc := xmlTextWriterWriteFormatElement(writer, 'ENTRY_NO', '%d', [10]);
  if rc < 0 then
  begin
    printfn('testXmlwriterMemory: Error at xmlTextWriterWriteFormatElement');
    Exit;
  end;

  (* Close the element named ENTRY. *)
  rc := xmlTextWriterEndElement(writer);
  if rc < 0 then
  begin
    printfn('testXmlwriterMemory: Error at xmlTextWriterEndElement');
    Exit;
  end;

  (* Start an element named "ENTRY" as child of ENTRIES. *)
  rc := xmlTextWriterStartElement(writer, 'ENTRY');
  if rc < 0 then
  begin
    printfn('testXmlwriterMemory: Error at xmlTextWriterStartElement');
    Exit;
  end;

  (* Write an element named "ARTICLE" as child of ENTRY. *)
  rc := xmlTextWriterWriteElement(writer, 'ARTICLE', '<Test 2>');
  if rc < 0 then
  begin
    printfn('testXmlwriterMemory: Error at xmlTextWriterWriteElement\n');
    Exit;
  end;

  (* Write an element named "ENTRY_NO" as child of ENTRY. *)
  rc := xmlTextWriterWriteFormatElement(writer, 'ENTRY_NO', '%d', [20]);
  if rc < 0 then
  begin
    printfn('testXmlwriterMemory: Error at xmlTextWriterWriteFormatElement');
    Exit;
  end;

  (* Close the element named ENTRY. *)
  rc := xmlTextWriterEndElement(writer);
  if rc < 0 then
  begin
    printfn('testXmlwriterMemory: Error at xmlTextWriterEndElement');
    Exit;
  end;

  (* Close the element named ENTRIES. *)
  rc := xmlTextWriterEndElement(writer);
  if rc < 0 then
  begin
    printfn('testXmlwriterMemory: Error at xmlTextWriterEndElement');
    Exit;
  end;

  (* Start an element named "FOOTER" as child of ORDER. *)
  rc := xmlTextWriterStartElement(writer, 'FOOTER');
  if rc < 0 then
  begin
    printfn('testXmlwriterMemory: Error at xmlTextWriterStartElement');
    Exit;
  end;

  (* Write an element named "TEXT" as child of FOOTER. *)
  rc := xmlTextWriterWriteElement(writer, 'TEXT', 'This is a text.');
  if rc < 0 then
  begin
    printfn('testXmlwriterMemory: Error at xmlTextWriterWriteElement');
    Exit;
  end;

  (* Close the element named FOOTER. *)
  rc := xmlTextWriterEndElement(writer);
  if rc < 0 then
  begin
    printfn('testXmlwriterMemory: Error at xmlTextWriterEndElement');
    Exit;
  end;

  (* Here we could close the elements ORDER and EXAMPLE using the
   * function xmlTextWriterEndElement, but since we do not want to
   * write any other elements, we simply call xmlTextWriterEndDocument,
   * which will do all the work. *)
  rc := xmlTextWriterEndDocument(writer);
  if rc < 0 then
  begin
    printfn('testXmlwriterMemory: Error at xmlTextWriterEndDocument');
    Exit;
  end;

  xmlFreeTextWriter(writer);

  fp := FileCreate(_file);
  FileWrite(fp, buf^.content^, strlen(buf^.content));
  FileClose(fp);

  xmlBufferFree(buf);
end;

(**
 * testXmlwriterDoc:
 * @file: the output file
 *
 * test the xmlWriter interface when creating a new document
 *)
procedure testXmlwriterDoc(const _file: PAnsiChar);
var
  rc: cint;
  writer: xmlTextWriterPtr;
  tmp: xmlCharPtr;
  doc: xmlDocPtr;
begin
  (* Create a new XmlWriter for DOM, with no compression. *)
  writer := xmlNewTextWriterDoc(doc, 0);
  if writer = Nil then
  begin
    printfn('testXmlwriterDoc: Error creating the xml writer');
    Exit;
  end;

  (* Start the document with the xml default for the version,
   * encoding ISO 8859-1 and the default for the standalone
   * declaration. *)
  rc := xmlTextWriterStartDocument(writer, Nil, MY_ENCODING, Nil);
  if rc < 0 then
  begin
    printfn('testXmlwriterDoc: Error at xmlTextWriterStartDocument');
    Exit;
  end;

  (* Start an element named 'EXAMPLE'. Since thist is the first
   * element, this will be the root element of the document. *)
  rc := xmlTextWriterStartElement(writer, 'EXAMPLE');
  if rc < 0 then
  begin
    printfn('testXmlwriterDoc: Error at xmlTextWriterStartElement');
    Exit;
  end;

  (* Write a comment as child of EXAMPLE.
   * Please observe, that the input to the xmlTextWriter functions
   * HAS to be in UTF-8, even if the output XML is encoded
   * in iso-8859-1 *)
  tmp := ConvertInput('This is a comment with special chars: <'#$E4#$F6#$FC'>',
    MY_ENCODING);
  rc := xmlTextWriterWriteComment(writer, tmp);
  if rc < 0 then
  begin
    printfn('testXmlwriterDoc: Error at xmlTextWriterWriteComment');
    Exit;
  end;
  if tmp <> Nil then
    xmlFree(tmp);

  (* Start an element named 'ORDER' as child of EXAMPLE. *)
  rc := xmlTextWriterStartElement(writer, 'ORDER');
  if rc < 0 then
  begin
    printfn('testXmlwriterDoc: Error at xmlTextWriterStartElement');
    Exit;
  end;

  (* Add an attribute with name 'version' and value '1.0' to ORDER. *)
  rc := xmlTextWriterWriteAttribute(writer, 'version', '1.0');
  if rc < 0 then
  begin
    printfn('testXmlwriterDoc: Error at xmlTextWriterWriteAttribute');
    Exit;
  end;

  (* Add an attribute with name 'xml:lang' and value 'de' to ORDER. *)
  rc := xmlTextWriterWriteAttribute(writer, 'xml:lang', 'de');
  if rc < 0 then
  begin
    printfn('testXmlwriterDoc: Error at xmlTextWriterWriteAttribute');
    Exit;
  end;

  (* Write a comment as child of ORDER *)
  tmp := ConvertInput('<'#$E4#$F6#$FC'>', MY_ENCODING);
  rc := xmlTextWriterWriteFormatComment(writer,
    'This is another comment with special chars: %s', [tmp]);
  if rc < 0 then
  begin
    printfn('testXmlwriterDoc: Error at xmlTextWriterWriteFormatComment');
    Exit;
  end;
  if tmp <> Nil then
    xmlFree(tmp);

  (* Start an element named 'HEADER' as child of ORDER. *)
  rc := xmlTextWriterStartElement(writer, 'HEADER');
  if rc < 0 then
  begin
    printfn('testXmlwriterDoc: Error at xmlTextWriterStartElement');
    Exit;
  end;

  (* Write an element named 'X_ORDER_ID' as child of HEADER. *)
  rc := xmlTextWriterWriteFormatElement(writer, 'X_ORDER_ID', '%010d', [53535]);
  if rc < 0 then
  begin
    printfn('testXmlwriterDoc: Error at xmlTextWriterWriteFormatElement');
    Exit;
  end;

  (* Write an element named 'CUSTOMER_ID' as child of HEADER. *)
  rc := xmlTextWriterWriteFormatElement(writer, 'CUSTOMER_ID', '%d', [1010]);
  if rc < 0 then
  begin
    printfn('testXmlwriterDoc: Error at xmlTextWriterWriteFormatElement');
    Exit;
  end;

  (* Write an element named 'NAME_1' as child of HEADER. *)
  tmp := ConvertInput('M'#$FC'ller', MY_ENCODING);
  rc := xmlTextWriterWriteElement(writer, 'NAME_1', tmp);
  if rc < 0 then
  begin
    printfn('testXmlwriterDoc: Error at xmlTextWriterWriteElement');
    Exit;
  end;
  if tmp <> Nil then
    xmlFree(tmp);

  (* Write an element named 'NAME_2' as child of HEADER. *)
  tmp := ConvertInput('J'#$F6'rg', MY_ENCODING);
  rc := xmlTextWriterWriteElement(writer, 'NAME_2', tmp);
  if rc < 0 then
  begin
    printfn('testXmlwriterDoc: Error at xmlTextWriterWriteElement');
    Exit;
  end;
  if tmp <> Nil then
    xmlFree(tmp);

  (* Close the element named HEADER. *)
  rc := xmlTextWriterEndElement(writer);
  if rc < 0 then
  begin
    printfn('testXmlwriterDoc: Error at xmlTextWriterEndElement');
    Exit;
  end;

  (* Start an element named 'ENTRIES' as child of ORDER. *)
  rc := xmlTextWriterStartElement(writer, 'ENTRIES');
  if rc < 0 then
  begin
    printfn('testXmlwriterDoc: Error at xmlTextWriterStartElement');
    Exit;
  end;

  (* Start an element named 'ENTRY' as child of ENTRIES. *)
  rc := xmlTextWriterStartElement(writer, 'ENTRY');
  if rc < 0 then
  begin
    printfn('testXmlwriterDoc: Error at xmlTextWriterStartElement');
    Exit;
  end;

  (* Write an element named 'ARTICLE' as child of ENTRY. *)
  rc := xmlTextWriterWriteElement(writer, 'ARTICLE', '<Test>');
  if rc < 0 then
  begin
    printfn('testXmlwriterDoc: Error at xmlTextWriterWriteElement');
    Exit;
  end;

  (* Write an element named 'ENTRY_NO' as child of ENTRY. *)
  rc := xmlTextWriterWriteFormatElement(writer, 'ENTRY_NO', '%d', [10]);
  if rc < 0 then
  begin
    printfn('testXmlwriterDoc: Error at xmlTextWriterWriteFormatElement');
    Exit;
  end;

  (* Close the element named ENTRY. *)
  rc := xmlTextWriterEndElement(writer);
  if rc < 0 then
  begin
    printfn('testXmlwriterDoc: Error at xmlTextWriterEndElement');
    Exit;
  end;

  (* Start an element named 'ENTRY' as child of ENTRIES. *)
  rc := xmlTextWriterStartElement(writer, 'ENTRY');
  if rc < 0 then
  begin
    printfn('testXmlwriterDoc: Error at xmlTextWriterStartElement');
    Exit;
  end;

  (* Write an element named 'ARTICLE' as child of ENTRY. *)
  rc := xmlTextWriterWriteElement(writer, 'ARTICLE', '<Test 2>');
  if rc < 0 then
  begin
    printfn('testXmlwriterDoc: Error at xmlTextWriterWriteElement');
    Exit;
  end;

  (* Write an element named 'ENTRY_NO' as child of ENTRY. *)
  rc := xmlTextWriterWriteFormatElement(writer, 'ENTRY_NO', '%d', [20]);
  if rc < 0 then
  begin
    printfn('testXmlwriterDoc: Error at xmlTextWriterWriteFormatElement');
    Exit;
  end;

  (* Close the element named ENTRY. *)
  rc := xmlTextWriterEndElement(writer);
  if rc < 0 then
  begin
    printfn('testXmlwriterDoc: Error at xmlTextWriterEndElement');
    Exit;
  end;

  (* Close the element named ENTRIES. *)
  rc := xmlTextWriterEndElement(writer);
  if rc < 0 then
  begin
    printfn('testXmlwriterDoc: Error at xmlTextWriterEndElement');
    Exit;
  end;

  (* Start an element named 'FOOTER' as child of ORDER. *)
  rc := xmlTextWriterStartElement(writer, 'FOOTER');
  if rc < 0 then
  begin
    printfn('testXmlwriterDoc: Error at xmlTextWriterStartElement');
    Exit;
  end;

  (* Write an element named 'TEXT' as child of FOOTER. *)
  rc := xmlTextWriterWriteElement(writer, 'TEXT', 'This is a text.');
  if rc < 0 then
  begin
    printfn('testXmlwriterDoc: Error at xmlTextWriterWriteElement');
    Exit;
  end;

  (* Close the element named FOOTER. *)
  rc := xmlTextWriterEndElement(writer);
  if rc < 0 then
  begin
    printfn('testXmlwriterDoc: Error at xmlTextWriterEndElement');
    Exit;
  end;

  (* Here we could close the elements ORDER and EXAMPLE using the
   * function xmlTextWriterEndElement, but since we do not want to
   * write any other elements, we simply call xmlTextWriterEndDocument,
   * which will do all the work. *)
  rc := xmlTextWriterEndDocument(writer);
  if rc < 0 then
  begin
    printfn('testXmlwriterDoc: Error at xmlTextWriterEndDocument');
    Exit;
  end;

  xmlFreeTextWriter(writer);

  xmlSaveFileEnc(_file, doc, MY_ENCODING);

  xmlFreeDoc(doc);
end;

(**
 * testXmlwriterTree:
 * @file: the output file
 *
 * test the xmlWriter interface when writing to a subtree
 *)
procedure testXmlwriterTree(const _file: PAnsiChar);
var
  rc: cint;
  writer: xmlTextWriterPtr;
  doc: xmlDocPtr;
  node: xmlNodePtr;
  tmp: xmlCharPtr;
begin
  (* Create a new XML DOM tree, to which the XML document will be
   * written *)
  doc := xmlNewDoc(XML_DEFAULT_VERSION);
  if doc = Nil then
  begin
    printf('testXmlwriterTree: Error creating the xml document tree');
    Exit;
  end;

  (* Create a new XML node, to which the XML document will be
   * appended *)
  node := xmlNewDocNode(doc, Nil, 'EXAMPLE', Nil);
  if node = Nil then
  begin
    printf('testXmlwriterTree: Error creating the xml node');
    Exit;
  end;

  (* Make ELEMENT the root node of the tree *)
  xmlDocSetRootElement(doc, node);

  (* Create a new XmlWriter for DOM tree, with no compression. *)
  writer := xmlNewTextWriterTree(doc, node, 0);
  if writer = Nil then
  begin
    printf('testXmlwriterTree: Error creating the xml writer');
    Exit;
  end;

  (* Start the document with the xml default for the version,
   * encoding ISO 8859-1 and the default for the standalone
   * declaration. *)
  rc := xmlTextWriterStartDocument(writer, Nil, MY_ENCODING, Nil);
  if rc < 0 then
  begin
    printf('testXmlwriterTree: Error at xmlTextWriterStartDocument');
    Exit;
  end;

  (* Write a comment as child of EXAMPLE.
   * Please observe, that the input to the xmlTextWriter functions
   * HAS to be in UTF-8, even if the output XML is encoded
   * in iso-8859-1 *)
  tmp := ConvertInput('This is a comment with special chars: <'#$E4#$F6#$FC'>',
    MY_ENCODING);
  rc := xmlTextWriterWriteComment(writer, tmp);
  if rc < 0 then
  begin
    printf('testXmlwriterTree: Error at xmlTextWriterWriteComment');
    Exit;
  end;
  if tmp <> Nil then
    xmlFree(tmp);

  (* Start an element named 'ORDER' as child of EXAMPLE. *)
  rc := xmlTextWriterStartElement(writer, 'ORDER');
  if rc < 0 then
  begin
    printf('testXmlwriterTree: Error at xmlTextWriterStartElement');
    Exit;
  end;

  (* Add an attribute with name 'version' and value '1.0' to ORDER. *)
  rc := xmlTextWriterWriteAttribute(writer, 'version', '1.0');
  if rc < 0 then
  begin
    printf('testXmlwriterTree: Error at xmlTextWriterWriteAttribute');
    Exit;
  end;

  (* Add an attribute with name 'xml:lang' and value 'de' to ORDER. *)
  rc := xmlTextWriterWriteAttribute(writer, 'xml:lang', 'de');
  if rc < 0 then
  begin
    printf('testXmlwriterTree: Error at xmlTextWriterWriteAttribute');
    Exit;
  end;

  (* Write a comment as child of ORDER *)
  tmp := ConvertInput('<'#$E4#$F6#$FC'>', MY_ENCODING);
  rc := xmlTextWriterWriteFormatComment(writer,
    'This is another comment with special chars: %s', [tmp]);
  if rc < 0 then
  begin
    printf('testXmlwriterTree: Error at xmlTextWriterWriteFormatComment');
    Exit;
  end;
  if tmp <> Nil then
    xmlFree(tmp);

  (* Start an element named 'HEADER' as child of ORDER. *)
  rc := xmlTextWriterStartElement(writer, 'HEADER');
  if rc < 0 then
  begin
    printf('testXmlwriterTree: Error at xmlTextWriterStartElement');
    Exit;
  end;

  (* Write an element named 'X_ORDER_ID' as child of HEADER. *)
  rc := xmlTextWriterWriteFormatElement(writer, 'X_ORDER_ID', '%010d', [53535]);
  if rc < 0 then
  begin
    printf('testXmlwriterTree: Error at xmlTextWriterWriteFormatElement');
    Exit;
  end;

  (* Write an element named 'CUSTOMER_ID' as child of HEADER. *)
  rc := xmlTextWriterWriteFormatElement(writer, 'CUSTOMER_ID', '%d', [1010]);
  if rc < 0 then
  begin
    printf('testXmlwriterTree: Error at xmlTextWriterWriteFormatElement');
    Exit;
  end;

  (* Write an element named 'NAME_1' as child of HEADER. *)
  tmp := ConvertInput('M'#$FC'ller', MY_ENCODING);
  rc := xmlTextWriterWriteElement(writer, 'NAME_1', tmp);
  if rc < 0 then
  begin
    printf('testXmlwriterTree: Error at xmlTextWriterWriteElement');
    Exit;
  end;
  if tmp <> Nil then
    xmlFree(tmp);

  (* Write an element named 'NAME_2' as child of HEADER. *)
  tmp := ConvertInput('J'#$F6'rg', MY_ENCODING);
  rc := xmlTextWriterWriteElement(writer, 'NAME_2', tmp);
  if rc < 0 then
  begin
    printf('testXmlwriterTree: Error at xmlTextWriterWriteElement');
    Exit;
  end;
  if tmp <> Nil then
    xmlFree(tmp);

  (* Close the element named HEADER. *)
  rc := xmlTextWriterEndElement(writer);
  if rc < 0 then
  begin
    printf('testXmlwriterTree: Error at xmlTextWriterEndElement');
    Exit;
  end;

  (* Start an element named 'ENTRIES' as child of ORDER. *)
  rc := xmlTextWriterStartElement(writer, 'ENTRIES');
  if rc < 0 then
  begin
    printf('testXmlwriterTree: Error at xmlTextWriterStartElement');
    Exit;
  end;

  (* Start an element named 'ENTRY' as child of ENTRIES. *)
  rc := xmlTextWriterStartElement(writer, 'ENTRY');
  if rc < 0 then
  begin
    printf('testXmlwriterTree: Error at xmlTextWriterStartElement');
    Exit;
  end;

  (* Write an element named 'ARTICLE' as child of ENTRY. *)
  rc := xmlTextWriterWriteElement(writer, 'ARTICLE', '<Test>');
  if rc < 0 then
  begin
    printf('testXmlwriterTree: Error at xmlTextWriterWriteElement');
    Exit;
  end;

  (* Write an element named 'ENTRY_NO' as child of ENTRY. *)
  rc := xmlTextWriterWriteFormatElement(writer, 'ENTRY_NO', '%d', [10]);
  if rc < 0 then
  begin
    printf('testXmlwriterTree: Error at xmlTextWriterWriteFormatElement');
    Exit;
  end;

  (* Close the element named ENTRY. *)
  rc := xmlTextWriterEndElement(writer);
  if rc < 0 then
  begin
    printf('testXmlwriterTree: Error at xmlTextWriterEndElement');
    Exit;
  end;

  (* Start an element named 'ENTRY' as child of ENTRIES. *)
  rc := xmlTextWriterStartElement(writer, 'ENTRY');
  if rc < 0 then
  begin
    printf('testXmlwriterTree: Error at xmlTextWriterStartElement');
    Exit;
  end;

  (* Write an element named 'ARTICLE' as child of ENTRY. *)
  rc := xmlTextWriterWriteElement(writer, 'ARTICLE', '<Test 2>');
  if rc < 0 then
  begin
    printf('testXmlwriterTree: Error at xmlTextWriterWriteElement');
    Exit;
  end;

  (* Write an element named 'ENTRY_NO' as child of ENTRY. *)
  rc := xmlTextWriterWriteFormatElement(writer, 'ENTRY_NO', '%d', [20]);
  if rc < 0 then
  begin
    printf('testXmlwriterTree: Error at xmlTextWriterWriteFormatElement');
    Exit;
  end;

  (* Close the element named ENTRY. *)
  rc := xmlTextWriterEndElement(writer);
  if rc < 0 then
  begin
    printf('testXmlwriterTree: Error at xmlTextWriterEndElement');
    Exit;
  end;

  (* Close the element named ENTRIES. *)
  rc := xmlTextWriterEndElement(writer);
  if rc < 0 then
  begin
    printf('testXmlwriterTree: Error at xmlTextWriterEndElement');
    Exit;
  end;

  (* Start an element named 'FOOTER' as child of ORDER. *)
  rc := xmlTextWriterStartElement(writer, 'FOOTER');
  if rc < 0 then
  begin
    printf('testXmlwriterTree: Error at xmlTextWriterStartElement');
    Exit;
  end;

  (* Write an element named 'TEXT' as child of FOOTER. *)
  rc := xmlTextWriterWriteElement(writer, 'TEXT', 'This is a text.');
  if rc < 0 then
  begin
    printf('testXmlwriterTree: Error at xmlTextWriterWriteElement');
    Exit;
  end;

  (* Close the element named FOOTER. *)
  rc := xmlTextWriterEndElement(writer);
  if rc < 0 then
  begin
    printf('testXmlwriterTree: Error at xmlTextWriterEndElement');
    Exit;
  end;

  (* Here we could close the elements ORDER and EXAMPLE using the
   * function xmlTextWriterEndElement, but since we do not want to
   * write any other elements, we simply call xmlTextWriterEndDocument,
   * which will do all the work. *)
  rc := xmlTextWriterEndDocument(writer);
  if rc < 0 then
  begin
    printf('testXmlwriterTree: Error at xmlTextWriterEndDocument');
    Exit;
  end;

  xmlFreeTextWriter(writer);

  xmlSaveFileEnc(_file, doc, MY_ENCODING);

  xmlFreeDoc(doc);
end;


begin
  (*
   * this initialize the library and check potential ABI mismatches
   * between the version it was compiled for and the actual shared
   * library used.
   *)
  LIBXML_TEST_VERSION;

  (* first, the file version *)
  testXmlwriterFilename('writer1.tmp');

  (* next, the memory version *)
  testXmlwriterMemory('writer2.tmp');

  (* next, the DOM version *)
  testXmlwriterDoc('writer3.tmp');

  (* next, the tree version *)
  testXmlwriterTree('writer4.tmp');

  (*
   * Cleanup function for the XML library.
   *)
  xmlCleanupParser();
  (*
   * this is to debug memory for regression tests
   *)
  xmlMemoryDump();
end.

