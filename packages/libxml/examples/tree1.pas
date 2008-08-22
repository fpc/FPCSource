(**
 * section: Tree
 * synopsis: Navigates a tree to print element names
 * purpose: Parse a file to a tree, use xmlDocGetRootElement() to
 *          get the root element, then walk the document and print
 *          all the element name in document order.
 * usage: tree1 filename_or_URL
 * test: tree1 test2.xml > tree1.tmp ; diff tree1.tmp tree1.res ; rm tree1.tmp
 * author: Dodji Seketeli
 * copy: see Copyright for the status of this software.
 *)

program tree1;

{$mode objfpc}

uses
  ctypes,
  libxml2,
  exutils,
  SysUtils;

procedure print_element_names(a_node: xmlNodePtr);
var
  cur_node: xmlNodePtr;
begin
  cur_node := a_node;
  while assigned(cur_node) do
  begin
    if cur_node^._type = XML_ELEMENT_NODE then
      printfn('node type: Element, name: %s', [cur_node^.name]);

    print_element_names(cur_node^.children);

    cur_node := cur_node^.next;
  end;
end;

var
  doc: xmlDocPtr;
  root_element: xmlNodePtr;

begin
  if paramCount <> 1 then
    halt(1);

  (*
   * this initialize the library and check potential ABI mismatches
   * between the version it was compiled for and the actual shared
   * library used.
   *)
  LIBXML_TEST_VERSION;

  (* parse the file and get the DOM *)
  doc := xmlReadFile(pchar(paramStr(1)), nil, 0);

  if not assigned(doc) then
  begin
    printfn('error: could not parse file %s', [paramStr(1)]);
    halt(1);
  end;

  (* Get the root element node *)
  root_element := xmlDocGetRootElement(doc);

  print_element_names(root_element);

  (* free the document *)
  xmlFreeDoc(doc);

  (*
   * Free the global variables that may
   * have been allocated by the parser.
   *)
  xmlCleanupParser();
end.
