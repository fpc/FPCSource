(*
 * section:  Tree
 * synopsis: Creates a tree
 * purpose:  Shows how to create document, nodes and dump it to stdout or file.
 * usage:    tree2 <filename>  -Default output: stdout
 * test:     tree2 > tree2.tmp ; diff tree2.tmp tree2.res ; rm tree2.tmp
 * author:   Lucas Brasilino <brasilino@recife.pe.gov.br>
 * copy:     see Copyright for the status of this software
 *)

program tree2;

{$mode objfpc}

uses
  ctypes,
  xml2,
  exutils,
  SysUtils;

var
  doc: xmlDocPtr;
  root_node, node, node1: xmlNodePtr;
  dtd: xmlDtdPtr;
  buff: array[0..255] of char;
  i, j: cint;

begin
  LIBXML_TEST_VERSION;

  (*
   * Creates a new document, a node and set it as a root node
   *)
  doc := xmlNewDoc('1.0');
  root_node := xmlNewNode(nil, 'root');
  xmlDocSetRootElement(doc, root_node);

  (*
   * Creates a DTD declaration. Isn't mandatory.
   *)
  dtd := xmlCreateIntSubset(doc, 'root', nil, 'tree2.dtd');

  (*
   * xmlNewChild() creates a new node, which is "attached" as child node
   * of root_node node.
   *)
  xmlNewChild(root_node, nil, 'node1', 'content of node 1');

  (*
   * The same as above, but the new child node doesn't have a content
   *)
  xmlNewChild(root_node, nil, 'node2', nil);

  (*
   * xmlNewProp() creates attributes, which is "attached" to an node.
   * It returns xmlAttrPtr, which isn't used here.
   *)
  node := xmlNewChild(root_node, nil, 'node3', 'this node has attributes');
  xmlNewProp(node, 'attribute', 'yes');
  xmlNewProp(node, 'foo', 'bar');

  (*
   * Here goes another way to create nodes. xmlNewNode() and xmlNewText
   * creates a node and a text node separately. They are "attached"
   * by xmlAddChild()
   *)
  node := xmlNewNode(nil, 'node4');
  node1 := xmlNewText('other way to create content (which is also a node)');
  xmlAddChild(node, node1);
  xmlAddChild(root_node, node);

  (*
   * A simple loop that "automates" nodes creation
   *)
  for i := 5 to 6 do
  begin
    buff := 'node'+inttostr(i);
    node := xmlNewChild(root_node, nil, buff, nil);

    for j := 1 to 3 do
    begin
      buff := 'node'+inttostr(i)+inttostr(j);
      node1 := xmlNewChild(node, nil, buff, nil);
      if j mod 2 = 0 then
        xmlNewProp(node1, 'odd', 'no')
      else
        xmlNewProp(node1, 'odd', 'yes');
    end;
  end;

  (*
   * Dumping document to stdio or file
   *)
  docdump(doc);
  //xmlSaveFormatFileEnc(argc > 1 ? argv[1] : "-", doc, "UTF-8", 1);

  (*free the document *)
  xmlFreeDoc(doc);

  (*
   * Free the global variables that may
   * have been allocated by the parser.
   *)
  xmlCleanupParser();

  (*
   * this is to debug memory for regression tests
   *)
  xmlMemoryDump();
end.
